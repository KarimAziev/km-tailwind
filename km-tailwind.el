;;; km-tailwind.el --- Dumb completions for tailwindcss -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/km-tailwind
;; Version: 0.1.0
;; Keywords: convenience languages
;; Package-Requires: ((emacs "28.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Dumb completions for tailwindcss

;;; Code:

(require 'cl-lib)

(defcustom km-tailwind-use-padding nil
  "Whether to use horizontal padding in the preview window."
  :group 'km-tailwind
  :type 'boolean)

(defcustom km-tailwind-use-separator nil
  "Whether to draw a line between the preview window and the Echo Area."
  :group 'km-tailwind
  :type 'boolean)

(defvar km-tailwind--preview-message-force-update nil)
(defvar km-tailwind--preview-wnd nil)
(defvar km-tailwind--candidates nil)


(defun km-tailwind--minibuffer-get-metadata ()
  "Return current minibuffer completion metadata."
  (completion-metadata
   (buffer-substring-no-properties
    (minibuffer-prompt-end)
    (max (minibuffer-prompt-end)
         (point)))
   minibuffer-completion-table
   minibuffer-completion-predicate))

(defun km-tailwind--minibuffer-ivy-selected-cand ()
  "Return the currently selected item in Ivy."
  (when (and (memq 'ivy--queue-exhibit post-command-hook)
             (boundp 'ivy-text)
             (boundp 'ivy--length)
             (boundp 'ivy-last)
             (fboundp 'ivy--expand-file-name)
             (fboundp 'ivy-state-current))
    (cons
     (completion-metadata-get
      (ignore-errors (km-tailwind--minibuffer-get-metadata))
      'category)
     (ivy--expand-file-name
      (if (and (> ivy--length 0)
               (stringp (ivy-state-current ivy-last)))
          (ivy-state-current ivy-last)
        ivy-text)))))

(defun km-tailwind--minibuffer-get-default-candidates ()
  "Return all current completion candidates from the minibuffer."
  (when (minibufferp)
    (let* ((all (completion-all-completions
                 (minibuffer-contents)
                 minibuffer-completion-table
                 minibuffer-completion-predicate
                 (max 0 (- (point)
                           (minibuffer-prompt-end)))))
           (last (last all)))
      (when last (setcdr last nil))
      (cons
       (completion-metadata-get (km-tailwind--minibuffer-get-metadata)
                                'category)
       all))))

(defun km-tailwind--get-minibuffer-get-default-completion ()
  "Target the top completion candidate in the minibuffer.
Return the category metadatum as the type of the target."
  (when (and (minibufferp) minibuffer-completion-table)
    (pcase-let* ((`(,category . ,candidates)
                  (km-tailwind--minibuffer-get-default-candidates))
                 (contents (minibuffer-contents))
                 (top (if (test-completion contents
                                           minibuffer-completion-table
                                           minibuffer-completion-predicate)
                          contents
                        (let ((completions (completion-all-sorted-completions)))
                          (if (null completions)
                              contents
                            (concat
                             (substring contents
                                        0 (or (cdr (last completions)) 0))
                             (car completions)))))))
      (cons category (or (car (member top candidates)) top)))))

(declare-function vertico--candidate "ext:vertico")
(declare-function vertico--update "ext:vertico")

(defun km-tailwind--vertico-selected ()
  "Target the currently selected item in Vertico.
Return the category metadatum as the type of the target."
  (when (bound-and-true-p vertico--input)
    (vertico--update)
    (cons (completion-metadata-get (km-tailwind--minibuffer-get-metadata)
                                   'category)
          (vertico--candidate))))

(defvar km-tailwind--minibuffer-targets-finders
  '(km-tailwind--minibuffer-ivy-selected-cand
    km-tailwind--vertico-selected
    km-tailwind--get-minibuffer-get-default-completion))

(defun km-tailwind--minibuffer-get-current-candidate ()
  "Return cons filename for current completion candidate."
  (let (target)
    (run-hook-wrapped
     'km-tailwind--minibuffer-targets-finders
     (lambda (fun)
       (when-let* ((result (funcall fun)))
         (when (and (cdr-safe result)
                    (stringp (cdr-safe result))
                    (not (string-empty-p (cdr-safe result))))
           (setq target result)))
       (and target (minibufferp))))
    target))

(defun km-tailwind--minibuffer-web-restore-completions-wind ()
  "Restore *Completions* window height."
  (when (eq this-command 'minibuffer-next-completion)
    (remove-hook 'post-command-hook
                 #'km-tailwind--minibuffer-web-restore-completions-wind)
    (when-let* ((win (get-buffer-window "*Completions*" 0)))
      (fit-window-to-buffer win completions-max-height))))

(defun km-tailwind--minibuffer-action-no-exit (action)
  "Execute ACTION with minibuffer candidate in its original window."
  (pcase-let ((`(,_category . ,current)
               (km-tailwind--minibuffer-get-current-candidate)))
    (when-let* ((win (get-buffer-window "*Completions*" 0)))
      (minimize-window win)
      (add-hook 'post-command-hook
                #'km-tailwind--minibuffer-web-restore-completions-wind))
    (with-minibuffer-selected-window
      (funcall action current))))

(defun km-tailwind--fontify (content &optional mode-fn &rest args)
  "Fontify CONTENT according to MODE-FN called with ARGS.
If CONTENT is not a string, instead of MODE-FN emacs-lisp-mode will be used."
  (with-temp-buffer
    (delay-mode-hooks
      (when (fboundp mode-fn)
        (apply mode-fn args))
      (goto-char (point-min))
      (insert content)
      (font-lock-ensure)
      (buffer-string))))



(defvar km-tailwind-last-candidate nil)

(defun km-tailwind--minibuffer-preview (candidate)
  "Call ACTION with minibuffer CANDIDATE in its original window."
  (let* ((cell
          (assoc-string candidate km-tailwind--candidates))
         (doc (car (last cell))))
    (if (not (and (stringp doc)
                  (not (string-empty-p doc))))
        (km-tailwind-delete-window)
      (unless (and (equal km-tailwind-last-candidate candidate)
                   (window-live-p km-tailwind--preview-wnd))
        (setq doc (km-tailwind--fontify doc 'css-mode))
        (km-tailwind-preview-message "%s" doc)
        (setq km-tailwind-last-candidate candidate)))))


(defun km-tailwind--pad-to-center (str width)
  "Pad STR with spaces on the left to be centered to WIDTH."
  (let* ((strs (split-string str "\n"))
         (padding (make-string
                   (/ (- width (length (car strs))) 2)
                   ?\ )))
    (mapconcat (lambda (s)
                 (concat padding s))
               strs "\n")))

(defun km-tailwind-window ()
  "Ensure that preview window is live and return it."
  (if (window-live-p km-tailwind--preview-wnd)
      km-tailwind--preview-wnd
    (let ((ori (selected-window))
          buf)
      (prog1 (setq km-tailwind--preview-wnd
                   (select-window
                    (let ((ignore-window-parameters t))
                      (split-window
                       (frame-root-window) -1 'below))
                    'norecord))
        (if (setq buf (get-buffer " *km-tailwind-preview*"))
            (switch-to-buffer buf 'norecord)
          (switch-to-buffer " *km-tailwind-preview*" 'norecord)
          (fundamental-mode)
          (set-window-hscroll km-tailwind--preview-wnd 0)
          (setq window-size-fixed t)
          (setq mode-line-format nil)
          (setq header-line-format nil)
          (setq tab-line-format nil)
          (setq cursor-type nil)
          (setq display-line-numbers nil)
          (setq display-fill-column-indicator nil)
          (set-window-dedicated-p km-tailwind--preview-wnd t)
          (set-window-parameter km-tailwind--preview-wnd 'no-other-window t)
          (run-hooks 'km-tailwind-window-hook))
        (select-window ori 'norecord)))))


(defun km-tailwind-delete-window ()
  "Delete preview window and kill its buffer."
  (when (window-live-p km-tailwind--preview-wnd)
    (let ((buf (window-buffer km-tailwind--preview-wnd)))
      (delete-window km-tailwind--preview-wnd)
      (kill-buffer buf))))

(defvar golden-ratio-mode)

(defun km-tailwind-preview-message (format-string &rest args)
  "Set preview window contents to (`format' FORMAT-STRING ARGS)."
  (let* ((str (apply #'format format-string args))
         (n-lines (cl-count ?\n str))
         deactivate-mark
         golden-ratio-mode)
    (with-selected-window (km-tailwind-window)
      (when km-tailwind-use-padding
        (setq str (km-tailwind--pad-to-center str (window-width))))
      (unless (and (string= (buffer-string) str)
                   (null km-tailwind--preview-message-force-update))
        (delete-region (point-min)
                       (point-max))
        (insert str)
        (when (and (window-system) km-tailwind-use-separator)
          (unless (looking-back "\n" nil)
            (insert "\n"))
          (insert
           (propertize "__" 'face 'km-tailwind-separator
                       'display '(space
                                  :height (1)))
           (propertize "\n" 'face 'km-tailwind-separator 'line-height t)))
        (set (make-local-variable 'window-min-height) n-lines)
        (setq truncate-lines (> n-lines 1))
        (let ((window-resize-pixelwise t)
              (window-size-fixed nil))
          (fit-window-to-buffer nil nil 1)))
      (goto-char (point-min)))))

(defun km-tailwind-minibuffer-preview ()
  "Call ACTION with minibuffer candidate in its original window."
  (interactive)
  (km-tailwind--minibuffer-preview-doc))

(defun km-tailwind--minibuffer-preview-doc ()
  "Preview doc with minibuffer candidate in its original window."
  (km-tailwind--minibuffer-action-no-exit 'km-tailwind--minibuffer-preview))

(defvar km-tailwind-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-j")
                #'km-tailwind-minibuffer-preview)
    map))

(defun km-tailwind--completing-read-with-keymap (prompt collection &optional
                                                        keymap predicate
                                                        require-match
                                                        initial-input hist def
                                                        inherit-input-method)
  "Read COLLECTION in minibuffer with PROMPT and KEYMAP.
See `completing-read' for PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF
INHERIT-INPUT-METHOD."
  (let ((collection (if (stringp (car-safe collection))
                        (copy-tree collection)
                      collection)))
    (minibuffer-with-setup-hook
        (lambda ()
          (when (minibufferp)
            (when keymap
              (let ((map (make-composed-keymap keymap
                                               (current-local-map))))
                (use-local-map map)))))
      (completing-read prompt
                       collection
                       predicate
                       require-match initial-input hist
                       def inherit-input-method))))

(defun km-tailwind--get-completion-prefix (item)
  "Extract prefix for completion from ITEM at point.

Argument ITEM is a string representing the completion item."
  (let* ((pos (point))
         (item-chars (reverse (append item nil)))
         (char (char-before pos)))
    (catch 'found
      (while
          (when-let* ((chars (member char item-chars)))
            (setq item-chars (cdr chars))
            (let* ((str (mapconcat #'char-to-string (reverse chars) ""))
                   (beg (- pos
                           (length str)))
                   (prefix (and (>= beg (point-min))
                                (buffer-substring-no-properties beg pos))))
              (if (and prefix
                       (string-prefix-p prefix str))
                  (throw 'found (substring-no-properties item (length prefix)))
                t)))))))

(defun km-tailwind--insert (item)
  "Insert ITEM or its completion prefix into the buffer.

Argument ITEM is a string that will be inserted into the buffer."
  (when item
    (insert (or (km-tailwind--get-completion-prefix item) item))))

(defun km-tailwind--make-overlay (start end &optional buffer front-advance
                                        rear-advance &rest props)
  "Create an overlay in BUFFER between START and END, applying PROPS.

Argument START is the position at which the overlay begins.

Argument END is the position at which the overlay ends.

Optional argument BUFFER is the buffer in which to create the overlay; defaults
to the current buffer.

Optional argument FRONT-ADVANCE when non-nil, makes the START of the overlay
advance when text is inserted at the overlay's beginning.

Optional argument REAR-ADVANCE when non-nil, makes the END of the overlay
advance when text is inserted at the overlay's end.

Remaining arguments PROPS are properties to set on the overlay, provided as a
property list."
  (let ((overlay (make-overlay start end buffer front-advance
                               rear-advance)))
    (dotimes (idx (length props))
      (when (eq (logand idx 1) 0)
        (let* ((prop-name (nth idx props))
               (val (plist-get props prop-name)))
          (overlay-put overlay prop-name val))))
    overlay))

;;;###autoload
(defun km-tailwind-complete ()
  "Provide dumb completions for Tailwind CSS classes at point."
  (interactive)
  (unless km-tailwind--candidates
    (setq km-tailwind--candidates
          (progn (require 'km-tailwind-alist)
                 (when (boundp 'km-tailwind-alist)
                   (copy-tree km-tailwind-alist)))))
  (let* ((buff (current-buffer))
         (symb
          (when-let* ((sym (symbol-at-point)))
            (format "%s" sym)))
         (pos (point))
         (alist km-tailwind--candidates)
         (longest (apply #'max (mapcar #'length (mapcar #'car alist))))
         (annotfmt (concat
                    (propertize " " 'display
                                `(space :align-to ,(1+
                                                    longest)))
                    " %s"
                    (propertize " " 'display `(space :align-to ,(+
                                                                 longest
                                                                 10
                                                                 80)))
                    " %s"))
         (annotf (lambda (str)
                   (let ((item (cdr (assoc-string str alist))))
                     (format annotfmt
                             (truncate-string-to-width
                              (replace-regexp-in-string
                               "\n"
                               ""
                               (or (car
                                    item)
                                   ""))
                              80 nil nil t)
                             (cadr item)))))
         (prev)
         (ov))
    (unwind-protect
        (minibuffer-with-setup-hook
            (lambda ()
              (when (minibufferp)
                (add-hook 'post-command-hook
                          #'km-tailwind--minibuffer-preview-doc
                          nil t)
                (add-hook 'after-change-functions
                          (lambda (&rest _)
                            (km-tailwind--minibuffer-preview-doc)
                            (pcase-let
                                ((`(,_category . ,current)
                                  (km-tailwind--minibuffer-get-current-candidate)))
                              (with-minibuffer-selected-window
                                (cond ((or (not prev)
                                           (not (string=
                                                 prev
                                                 current)))
                                       (setq prev current)
                                       (when (overlayp ov)
                                         (delete-overlay ov))
                                       (when (buffer-live-p buff)
                                         (setq ov (km-tailwind--make-overlay
                                                   pos
                                                   pos nil nil nil 'after-string
                                                   (or (km-tailwind--get-completion-prefix current)
                                                       current)))))))))
                          nil t)))
          (km-tailwind--insert
           (km-tailwind--completing-read-with-keymap "Insert completion: "
                                                     (lambda (str pred action)
                                                       (if (eq action 'metadata)
                                                           `(metadata
                                                             (annotation-function
                                                              .
                                                              ,annotf))
                                                         (complete-with-action
                                                          action
                                                          alist
                                                          str
                                                          pred)))
                                                     km-tailwind-minibuffer-map
                                                     nil
                                                     nil
                                                     (when
                                                         (and symb
                                                              (try-completion
                                                               symb
                                                               km-tailwind--candidates))
                                                       symb))))
      (when (overlayp ov)
        (delete-overlay ov)))))

(provide 'km-tailwind)
;;; km-tailwind.el ends here