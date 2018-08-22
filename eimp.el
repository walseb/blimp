;;; eimp.el --- Emacs Image Manipulation Package (Core version) -*- lexical-binding: t -*-

;;; Copyright (C) 2006, 2007 Matthew P. Hodges

;; Author: Matthew P. Hodges <MPHodges@member.fsf.org>
;; URL: http://mph-emacs-pkgs.alioth.debian.org/EimpEl.html
;; Version: 1.4.1
;; Package-Requires: ((emacs "24"))
;; Keywords: multimedia, unix

;; eimp.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; eimp.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;;; Commentary:
;;
;; This package allows interactive image manipulation from within
;; Emacs.  It uses the mogrify utility from ImageMagick to do the
;; actual transformations.
;;
;; This is the core version of eimp.el, which is eimp.el with features
;; provided by the package blimp removed

;;; Code:
(require 'image-mode)

(defconst eimp-version "1.4.1"
  "Version number of this package.")

;; Customizable variables

(defgroup eimp nil
  "Emacs Image Manipulation Package."
  :group 'tools
  :link '(url-link "http://mph-emacs-pkgs.alioth.debian.org/EimpEl.html"))

(defcustom eimp-mogrify-program "mogrify"
  "Name of mogrify program.
Should be in PATH."
  :group 'eimp
  :type 'string)

(defcustom eimp-mogrify-arguments
  (when (= (call-process eimp-mogrify-program nil nil nil "-monitor")
           0)
    '("-monitor"))
  "List of arguments for `eimp-mogrify-program'.
Should include -monitor if supported."
  :group 'eimp
  :type '(repeat string))

(defcustom eimp-max-concurrent-processes 1
  "Maximum number of concurrent EIMP processes.
This is only relevant if there are multiple images; queued
operations act sequentially on any given image."
  :group 'eimp
  :type 'integer)

(defcustom eimp-process-delay 1.0
  "Delay between running EIMP processes."
  :group 'eimp
  :type 'number)

(defcustom eimp-max-queued-processes 128
  "Maximum number of queued EIMP processes."
  :group 'eimp
  :type 'integer)

(defcustom eimp-ignore-read-only-modes '(gnus-article-mode
                                         puzzle-mode
                                         tumme-display-image-mode
                                         tumme-thumbnail-mode
                                         w3m-mode)
  "Major modes for which we ignore `buffer-read-only'."
  :group 'eimp
  :type '(repeat symbol))

(defcustom eimp-enable-undo nil
  "Enable undo for EIMP modifications."
  :group 'eimp
  :type 'boolean)

;; Mode settings
(defvar eimp-mode-string " EIMP"
  "String used to indicate EIMP status in mode line.")
(make-variable-buffer-local 'eimp-mode-string)

;;;###autoload
(define-minor-mode eimp-mode
  "Toggle Eimp mode."
  nil eimp-mode-string
  (when eimp-mode
    (setq eimp-mode-string " EIMP"))
  (if (and eimp-mode (eq major-mode 'image-mode))
      (progn
        (add-hook 'write-contents-functions #'eimp-update-buffer-contents nil t)
        (set (make-local-variable 'require-final-newline) nil))
    (remove-hook 'write-contents-functions #'eimp-update-buffer-contents t)))

;; Variables

(defvar eimp-process-queue nil
  "List of pending EIMP processes.")

(defvar eimp-process-list nil
  "List of running EIMP processes.")

;; This is really c-save-buffer-state
(defmacro eimp-save-buffer-state (varlist &rest body)
  "Bind variables according to VARLIST (in `let*' style) and eval BODY.
Then restore the buffer state under the assumption that no significant
modification has been made in BODY.  A change is considered
significant if it affects the buffer text in any way that isn't
completely restored again.  Changes in text properties like `face' or
`syntax-table' are considered insignificant.  This macro allows text
properties to be changed, even in a read-only buffer.

This macro should be placed around all calculations which set
\"insignificant\" text properties in a buffer, even when the buffer is
known to be writeable.  That way, these text properties remain set
even if the user undoes the command which set them.

This macro should ALWAYS be placed around \"temporary\" internal buffer
changes \(like adding a newline to calculate a text-property then
deleting it again\), so that the user never sees them on his
`buffer-undo-list'.

The return value is the value of the last form in BODY."
  `(let* ((modified (buffer-modified-p)) (buffer-undo-list t)
	  (inhibit-read-only t) (inhibit-point-motion-hooks t)
	  before-change-functions after-change-functions
	  deactivate-mark
	  ,@varlist)
     (unwind-protect
	 (progn ,@body)
       (and (not modified)
	    (buffer-modified-p)
	    (set-buffer-modified-p nil)))))
(put 'eimp-save-buffer-state 'lisp-indent-function 1)

(defun eimp-get-display-property (&optional posn)
  "Get display property at POSN (or point, if POSN is nil).
Return a list, where the car is the type of image, and the cdr is
the image data."
  (or posn (setq posn (point)))
  (let (display)
    (cond
     ((setq display (eimp-get-text-property-display-property posn))
      (if (and (listp (car display))
               (eq (caar display) 'slice))
          (list 'text-prop-sliced display)
        (list 'text-prop display)))
     ((setq display (eimp-get-overlay-display-property posn))
      (list 'overlay display)))))

(defun eimp-get-text-property-display-property (posn)
  "Get display text property at POSN."
  (let ((display (get-text-property posn 'display)))
    display))

(defun eimp-get-overlay-display-property (posn)
  "Get display overlay at POSN."
  (let ((overlay (car (overlays-in (1+ posn) (1+ posn))))
        before-string display)
    (when (and overlay
               (setq before-string (overlay-get overlay 'before-string)))
      (setq display
            (get-text-property 0 'display before-string)))
    display))

(defun eimp-get-image ()
  "Get image specification at point."
  (let ((display (cadr (eimp-get-display-property))))
    (cond
     ((eq 'image (car display))
      display)
     ((and (listp (cdr display))
           (eq 'image (car (cadr display))))
      (cadr display)))))

(defun eimp-get-image-data (&optional posn)
  "Get data for image at POSN (or point, if POSN is nil)."
  (save-excursion
    (goto-char (or posn (point)))
    (let ((image-spec (eimp-get-image)))
      (or (cadr (member :data image-spec))
          (let ((file (cadr (member :file image-spec))))
            (when (and file (file-readable-p file))
              (with-temp-buffer
                (insert-file-contents-literally file)
                (encode-coding-string (buffer-string) 'no-conversion-multibyte))))))))

(defun eimp-mogrify-image (args)
  "Transform image, passing ARGS to mogrify."
  (when (eq major-mode 'image-mode)
    (goto-char (point-min)))
  (let ((image-spec (eimp-get-display-property))
        (id (make-temp-name "eimp-")))
    (when (and (not (memq major-mode eimp-ignore-read-only-modes))
               (memq (car image-spec) '(text-prop text-prop-sliced)))
      (barf-if-buffer-read-only))
    (cond
     ((null image-spec)
      (error "EIMP: No image at point"))
     (t
      (eimp-queue-process (cons (current-buffer) id))
      (eimp-save-buffer-state nil
        (put-text-property (point) (1+ (point)) id
                           `(image-type ,(car image-spec)
                                        proc-args ,args)))
      (eimp-run-queued-processes)))))

(defun eimp-queue-process (specs)
  "Add process identified by SPECS to list.
Car of SPECS is a buffer, and cdr of SPECS is the process ID (a
string)."
  (when (>= (length eimp-process-queue) eimp-max-queued-processes)
    (error "EIMP: eimp-max-queued-processes exceeded %S" specs))
  (setq eimp-process-queue
        (nconc eimp-process-queue (list specs))))

(defun eimp-run-queued-processes ()
  "Run a queued EIMP process."
  (eimp-clean-process-queue)
  (let ((queue (copy-alist eimp-process-queue)))
    (while (and queue (< (length eimp-process-list) eimp-max-concurrent-processes))
      (when (eimp-start-process (car queue))
        (setq eimp-process-queue (delete (car queue) eimp-process-queue)))
      (setq queue (cdr queue)))))

(defun eimp-clean-process-queue ()
  "Remove unrunnable processes from `eimp-process-queue'."
  (setq eimp-process-queue
        (delq nil
              (mapcar (lambda (spec)
                        (when (buffer-live-p (car spec))
                          spec))
                      eimp-process-queue))))

(defun eimp-start-process (spec)
  "Start an EIMP process according to SPEC.
Car of SPEC is the image buffer, cdr of SPEC is the process ID (a
string).  Return the process, if any."
  (let ((buffer (car spec))
        (id (cdr spec))
        proc)
    (if (not (buffer-live-p buffer))
        (message "Buffer not live")
      (with-current-buffer buffer
        (let* ((posn (eimp-image-position-by-id id))
               (image-data (and posn (eimp-get-image-data posn))))
          (when posn
            (eimp-check-for-zombie))
          (cond
           ((or (not posn) (not image-data))
            ;; Maybe the image was deleted, or the display property
            ;; removed; remove this queued process, and carry on
            ;; regardless.
            (setq eimp-process-queue (delete spec eimp-process-queue)))
           ((get-text-property posn 'eimp-proc)
            ;; Process already running for image at point; do nothing.
            )
           (t
            (save-excursion
              (goto-char posn)
              (let* ((coding-system-for-write 'no-conversion)
                     (eimp-data (get-text-property (point) id))
                     (temp-file (expand-file-name (make-temp-name "eimp-")
                                                  temporary-file-directory))
                     (args (cadr (member 'proc-args eimp-data)))
                     (image-type (cadr (member 'image-type eimp-data))))
                (with-temp-file temp-file
                  (insert (decode-coding-string image-data 'no-conversion-multibyte)))
                (setq proc
                      (apply #'start-process id nil eimp-mogrify-program
                             ;; TODO: haven't understood why things
                             ;; are so much slower when
                             ;; eimp-mogrify-arguments is nil
                             `(,@eimp-mogrify-arguments ,@args ,temp-file)))
                (push proc eimp-process-list)
                (set-process-buffer proc (current-buffer))
                (set-process-filter proc #'eimp-mogrify-process-filter)
                (set-process-sentinel proc #'eimp-mogrify-process-sentinel)
                (eimp-save-buffer-state nil
		  (put-text-property (point) (1+ (point)) 'eimp-proc
				     `(proc ,proc
					    image-type ,image-type
					    temp-file ,temp-file))
		  (remove-text-properties (point) (1+ (point)) (list id))))))))))
    proc))

(defun eimp-check-for-zombie ()
  "Check for zombie eimp-proc text property."
  (let ((proc (cadr (member 'proc (get-text-property (point) 'eimp-proc)))))
    (when (and proc
               (not (member proc eimp-process-list)))
      (eimp-save-buffer-state nil
        (remove-text-properties (point) (1+ (point)) '(eimp-proc))))))

;;;###autoload
(defun eimp-stop-all (&optional error)
  "Stop all running processes; remove queued processes.
If ERROR, signal an error with this string."
  (interactive)
  (eimp-clear-process-list)
  (eimp-clear-process-queue)
  (eimp-reset-mode-strings)
  (when error
    (error error)))

(defun eimp-clear-process-list ()
  "Remove running EIMP objects."
  (let (buffer posn)
    (save-excursion
      (dolist (proc eimp-process-list)
        (setq buffer (process-buffer proc))
        (when (buffer-live-p buffer)
          (setq posn (eimp-image-position-by-proc proc))
          (when posn
            (eimp-save-buffer-state nil
              (remove-text-properties posn (1+ posn) (list proc))))))))
  (setq eimp-process-list nil))

(defun eimp-clear-process-queue ()
  "Remove queued EIMP objects."
  (let (buffer id posn)
    (save-excursion
      (dolist (spec eimp-process-queue)
        (setq buffer (car spec)
              id (cdr spec))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq posn (eimp-image-position-by-id id))
            (when posn
              (eimp-save-buffer-state nil
                (remove-text-properties posn (1+ posn) (list id)))))))))
  (setq eimp-process-queue nil))

(defun eimp-reset-mode-strings ()
  "Reset all EIMP mode strings."
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when eimp-mode
        (setq eimp-mode-string " EIMP")))))

(defun eimp-mogrify-process-filter (proc msg)
  "Process filter for mogrify.
Process PROC with message string MSG."
  (eimp-check-image-delete-process proc)
  (when (and (buffer-live-p (process-buffer proc))
             (eq (process-status proc) 'run))
    (let ((progress (car (reverse (delete "" (split-string msg "[\n\r]+"))))))
      (with-current-buffer (process-buffer proc)
        (eimp-message proc progress)))))

(defun eimp-mogrify-process-sentinel (proc msg)
  "Process sentinel for mogrify.
Process PROC with message string MSG."
  (let ((buffer (process-buffer proc))
        error-message stopped)
    (if (buffer-live-p buffer)
        (save-excursion
          (with-current-buffer buffer
            (let* ((image-posn (eimp-image-position-by-proc proc))
                   (display (and image-posn (eimp-get-display-property image-posn)))
                   ;; Could be nil if no image
                   (eimp-data (and image-posn (get-text-property image-posn 'eimp-proc)))
                   (image-type (cadr (member 'image-type eimp-data)))
                   (temp-file (cadr (member 'temp-file eimp-data))))
              (cond
               ((or (not display) (not eimp-data))
                (setq error-message "EIMP image not found"))
               ((string-equal msg "finished\n")
                (goto-char image-posn)
                (if eimp-enable-undo
                    (eimp-replace-image image-type temp-file)
                  (eimp-save-buffer-state nil
                    (eimp-replace-image image-type temp-file))
                  (when (eq major-mode 'image-mode)
                    (set-buffer-modified-p t))))
               ((string-equal msg "stopped (signal)\n")
                (setq stopped t))
               (t
                (setq error-message (format "EIMP process exited with error: %s (exit status = %S)" msg
                                            (process-exit-status proc)))))
              (unless stopped
                (when image-posn
                  (setq eimp-process-list (delq proc eimp-process-list))
                  (eimp-save-buffer-state nil
                    (remove-text-properties
                     image-posn (1+ image-posn) '(eimp-proc))))
                (when (and temp-file (file-exists-p temp-file))
                  (delete-file temp-file))))))
      (setq error-message "EIMP image buffer deleted"))
    ;; Run queued processes, if no error and there are any remaining
    (if error-message
        (progn
          (run-at-time 0 nil #'eimp-stop-all error-message))
      (run-at-time eimp-process-delay nil #'eimp-run-queued-processes)
      (eimp-message proc))))

(defun eimp-message (proc &optional progress)
  "Emit EIMP message showing the number of running/queued processes.
Here message is used in a general sense, i.e. the message is
communicated using the mode-line or the *Messages* buffer,
depending on the mode of the buffer associated with the EIMP
process.  PROC is the process associated with the message, if
any, and optional argument PROGRESS is appended to the message."
  (with-current-buffer (process-buffer proc)
    (when (or eimp-mode
              (not (minibuffer-window-active-p (selected-window))))
      (let ((buffer-processes (length (delq nil
                                            (mapcar (lambda (p)
                                                      (eq (current-buffer)
                                                          (process-buffer p)))
                                                    eimp-process-list))))
            (buffer-queued (length (delq nil
                                         (mapcar (lambda (q)
                                                   (eq (current-buffer)
                                                       (car q)))
                                                 eimp-process-queue))))
            (message "EIMP")
            message-log-max)
        (if (or (> buffer-processes 0) (> buffer-queued 0))
            (progn
              (setq message (concat message (format ": (r:%d/q:%d)"
                                                    buffer-processes buffer-queued)))
              (when progress
                (setq message (concat message " " progress)))
              (if eimp-mode
                  (progn
                    (setq eimp-mode-string (concat " " message))
                    (force-mode-line-update))
                (message "%s" message)))
          (if eimp-mode
              (setq eimp-mode-string " EIMP")
            (message nil)))))))

(defun eimp-image-position-by-id (id)
  "Return point for image associated with ID."
  (cond
   ((get-text-property (point) id)
    (point))
   (t
    (save-excursion
      (goto-char (point-min))
      (catch 'found
        (while (< (point) (point-max))
          (when (get-text-property (point) id)
            (throw 'found (point)))
          (goto-char (or (next-single-char-property-change (point) id) (point-max)))))))))

(defun eimp-image-position-by-proc (proc)
  "Return point for image associated with process PROC."
  (cond
   ((eq (cadr (member 'proc (get-text-property (point) 'eimp-proc))) proc)
    (point))
   (t
    (save-excursion
      (goto-char (point-min))
      (catch 'found
        (while (< (point) (point-max))
          (when (eq (cadr (member 'proc (get-text-property (point) 'eimp-proc))) proc)
            (throw 'found (point)))
          (goto-char (or (next-single-char-property-change (point) 'eimp-proc) (point-max)))))))))

(defun eimp-check-image-delete-process (proc)
  "Check image still exists for process PROC.
Delete process if it doesn't"
  (let ((buffer (process-buffer proc)))
    (when (or (not (buffer-live-p buffer))
              (with-current-buffer buffer
                (let* ((image-posn (eimp-image-position-by-proc proc))
                       (display (and image-posn (eimp-get-display-property image-posn))))
                  (or (not image-posn) (not display)))))
      (eimp-stop-all))))

(defun eimp-replace-image (type file)
  "Replace image at point of type TYPE from file FILE."
  (cond
   ((equal type 'text-prop)
    (eimp-replace-text-property-image file))
   ((equal type 'text-prop-sliced)
    (eimp-replace-text-property-sliced-image file))
   ((equal type 'overlay)
    (eimp-replace-overlay-image file))))

(defun eimp-replace-text-property-image (file)
  "Replace text property image using contents of FILE."
  (let ((inhibit-read-only t))
    (put-text-property (point)
                       (next-single-char-property-change (point) 'display)
                       'display
                       (create-image (with-temp-buffer
                                       (insert-file-contents-literally file)
                                       (encode-coding-string (buffer-string) 'no-conversion-multibyte))
				     nil t))))

(defun eimp-replace-text-property-sliced-image (file)
  "Replace text property image slices in region using contents of FILE."
  (let ((inhibit-read-only t)
        (image (create-image (with-temp-buffer
                               (insert-file-contents-literally file)
                               (encode-coding-string (buffer-string) 'no-conversion-multibyte))
			     nil t))
        (image-prop (cdr (get-text-property (point) 'display))))
    ;; The slices could be anywhere; unfortunately this will replace
    ;; all slices for multiple copies of the same image.
    (goto-char (point-min))
    (while (not (eobp))
      (when (equal image-prop (cdr (get-text-property (point) 'display)))
        (put-text-property (point)
                           (next-single-char-property-change (point) 'display)
                           'display
                           (list (car (cadr (eimp-get-display-property (point))))
                                 image)))
      (goto-char (next-single-char-property-change (point) 'display)))))


(defun eimp-replace-overlay-image (file)
  "Replace overlay image using contents of FILE."
  (let ((inhibit-read-only t)
        (before-string (overlay-get (car (overlays-in (1+ (point)) (1+ (point)))) 'before-string)))
    (put-text-property 0 (length before-string) 'display
                       (create-image (with-temp-buffer
                                       (insert-file-contents-literally file)
                                       (encode-coding-string (buffer-string) 'no-conversion-multibyte))
				     nil t)
                       before-string)))

(defun eimp-update-buffer-contents ()
  "Update buffer contents with image text property."
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t)
          (data (encode-coding-string (eimp-get-image-data) 'no-conversion-multibyte)))
      (if eimp-enable-undo
          (progn
            (erase-buffer)
            (insert data))
        (eimp-save-buffer-state nil
	  (erase-buffer)
	  (insert data))))
    (require 'image-mode)
    (image-toggle-display)
    ;; Return nil
    nil))

;;;###autoload
(defun eimp-fit-image-to-window (arg)
  "Scale image to fit in the current window.
With a prefix arg, ARG, don't preserve the aspect ratio."
  (interactive "P")
  (let* ((edges (window-inside-pixel-edges))
         (width (- (nth 2 edges) (nth 0 edges)))
         (height (- (nth 3 edges) (nth 1 edges))))
    (eimp-mogrify-image `("-resize" ,(concat (format "%dx%d" width height)
                                             (and arg "!"))))))

;;;###autoload
(defun eimp-fit-image-to-whole-window ()
  "Scale image to fit the whole of the current window.
The aspect ratio is not preserved."
  (interactive)
  (eimp-fit-image-to-window t))

;;;###autoload
(defun eimp-fit-image-height-to-window (arg)
  "Scale image height to fit in the current window.
With a prefix arg, ARG, don't preserve the aspect ratio."
  (interactive "P")
  (let* ((edges (window-inside-pixel-edges))
         (height (- (nth 3 edges) (nth 1 edges)))
         (image-size (image-size (eimp-get-image) t))
         (image-width (car image-size))
         (image-height (cdr image-size)))
    (eimp-mogrify-image
     (if arg
         `("-resize" ,(concat (format "%dx%d!" image-width height)))
       `("-resize" ,(format "%d%%" (* 100 (/ (float height) image-height))))))))

;;;###autoload
(defun eimp-fit-image-width-to-window (arg)
  "Scale image width to fit in the current window.
With a prefix arg, ARG, don't preserve the aspect ratio."
  (interactive "P")
  (let* ((edges (window-inside-pixel-edges))
         (width (- (nth 2 edges) (nth 0 edges)))
         (image-size (image-size (eimp-get-image) t))
         (image-width (car image-size))
         (image-height (cdr image-size)))
    (eimp-mogrify-image
     (if arg
         `("-resize" ,(concat (format "%dx%d!" width image-height)))
       `("-resize" ,(format "%d%%" (* 100 (/ (float width) image-width))))))))

;;;###autoload
(defun eimp-mouse-resize-image (event)
  "Resize image with mouse.
Argument EVENT is a mouse event."
  (interactive "e")
  (eimp-mouse-resize-image-1 event nil))

;;;###autoload
(defun eimp-mouse-resize-image-preserve-aspect (event)
  "Resize image with mouse, preserving aspect ratio.
Argument EVENT is a mouse event."
  (interactive "e")
  (eimp-mouse-resize-image-1 event t))

(defun eimp-mouse-resize-image-1 (event preserve-aspect)
  "Resize image with mouse.
Argument EVENT is a mouse event; with non-nil PRESERVE-ASPECT,
preserve the aspect ratio."
  (let* ((event-start (event-start event))
         end
         message-log-max
         image-size image-width image-height
         width-ratio height-ratio ratio
         dx dy dx-dy x-y start-x-y)
    (mouse-set-point event)
    ;; Image at or just before point
    (unless (eimp-get-display-property)
      (backward-char))
    (cond
     ((not (posn-image event-start))
      (message "No image at mouse"))
     (t
      (setq image-size (image-size (eimp-get-image) t)
            image-width (car image-size)
            image-height (cdr image-size))
      (setq start-x-y (eimp-frame-relative-coordinates event-start)
            dx-dy (posn-object-x-y event-start))
      (setq start-x-y (cons (- (car start-x-y) (car dx-dy))
                            (- (cdr start-x-y) (cdr dx-dy))))
      (track-mouse
        (while (progn
                 (setq event (read-event))
                 (or (mouse-movement-p event)
                     (memq (car-safe event) '(switch-frame select-window))))
          
          (if (memq (car-safe event) '(switch-frame select-window))
              nil
            (setq end (event-end event))
            (if (numberp (posn-point end))
                (progn
                  (setq x-y (eimp-frame-relative-coordinates end)
                        dx (- (car x-y) (car start-x-y))
                        dy (- (cdr x-y) (cdr start-x-y))))
              (setq dx -1 dy -1))
            (if (or (< dx 0) (< dy 0))
                (message "Not scaling image")
              (if preserve-aspect
                  (progn
                    (setq width-ratio (/ dx (float image-width))
                          height-ratio (/ dy (float image-height))
                          ratio (max width-ratio height-ratio))
                    (message "Resizing image from %dx%d to %dx%d"
                             image-width image-height
                             (* image-width ratio)
                             (* image-height ratio)))
                (message "Resizing image from %dx%d to %dx%d"
                         image-width image-height dx dy))))))
      (when (and (> dx 0) (> dy 0))
        (if preserve-aspect
            (eimp-mogrify-image
             `("-resize" ,(format "%d%%" (* 100 ratio))))
          (eimp-mogrify-image
           `("-resize" ,(concat (format "%dx%d!" dx dy))))))))))

(defun eimp-frame-relative-coordinates (position)
  "Return frame-relative coordinates from POSITION."
  (let* ((x-y (posn-x-y position))
         (window (posn-window position))
         (edges (window-inside-pixel-edges window)))
    (cons (+ (car x-y) (car edges))
          (+ (cdr x-y) (cadr edges)))))

(defun eimp-trace-all ()
  "Trace all `eimp' functions.  For debugging."
  (require 'trace)
  (let ((buffer (get-buffer-create "*EIMP Trace*")))
    (buffer-disable-undo buffer)
    (all-completions "eimp" obarray
                     (lambda (sym)
		       (and (fboundp sym)
                            (not (memq (car-safe (symbol-function sym))
				       '(autoload macro)))
                            (trace-function-background sym buffer))))))
(provide 'eimp)

;;; eimp.el ends here
