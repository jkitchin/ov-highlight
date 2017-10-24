;;; ov-highlighter.el --- Highlight text with overlays  -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2017  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; URL: https://github.com/jkitchin/ov-highlighter

;; Version: 0.1.0
;; Keywords:  faces
;; Package-Requires: ((emacs "25") (hydra "0.14.0") (dash "2.12.0") (s "1.12.0"))
;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; ov-highlighter provides a lightweight way to highlight text and put notes on
;; them using overlays that are locally stored.

;; There is a hydra menu to make accessing all the commands below convenient:
;; `ov-highlighter/body'. I suggest you bind it to a key like H-h.

;; You can select text, and run these commands to add highlighting to it:
;; `ov-highlighter-yellow'
;; `ov-highlighter-blue'
;; `ov-highlighter-green'
;; `ov-highlighter-pink'
;; `ov-highlighter-color' to choose a color for the background.
;; `ov-highlighter-foreground' to choose the color for the font.

;; There are also some markup overlays: `ov-highlighter-bold', and italic,
;; underline, strikethrough, and box overlays.

;; You can increase/decrease the font size with
;; `ov-highlighter-increase-font-size' and `ov-highlighter-decrease-font-size', and
;; you can also change the font `ov-highlighter-font'.

;; `ov-highlighter-comment' will highlight the text and add a note to it as a
;; tooltip. You can edit a note by clicking on it.

;; You can list all the highlights with `ov-highlighter-list' in a buffer that
;; you can easily navigate to each one from.

;; You can clear a single highlight under the cursor with `ov-highlighter-clear'.
;; Remove all the highlights with `ov-highlighter-clear-all'.

;; You can convert the highlights to html with `ov-highlighter-html' which will
;; convert the buffer to an html file and open it in a browser.

;; Define your own custom highlight functions with `ov-highlighter-make'. See the
;; predefined functions for examples.

;; ov-highlighter uses a local save-buffer-hook to update the data when you save
;; the buffer. It also uses local file variables to load the highlights when you
;; open the file. The data is saved in a single line. I don't know what the
;; limitation of this is.

;; Known issues: You cannot cut and paste these highlights. Fixing this would
;; involve changing the kill and yank functions to capture overlays in the kill
;; region, and remake them in the yanked region.

(require 'hydra)
(require 'ov)

;;; Code:
(defvar ov-highlighter-data nil
  "Contains highlight data. Normally set as a local file variable.")

(defmacro ov-highlighter-make (label face &rest properties)
  "Create a user-defined highlight function.
LABEL is the name of the highlight. The function will be called
`ov-highlighter-LABEL', and it will apply FACE to the selected
region. FACE can be an anonymous face, or a function that returns
one. PROPERTIES is a list of symbols and properties. If the
property is a function, it will be evaluated. The function takes
no arguments."
  `(defun ,(intern (format "ov-highlighter-%s" label)) (beg end)
     ,(format "Apply the face %S to the region selected by BEG and END" face)
     (interactive "r")
     (flyspell-delete-region-overlays beg end)
     ;; add a local hook to make sure it gets saved
     (add-hook 'before-save-hook 'ov-highlighter-save nil t)
     (let ((face ,face)
	   (properties (quote ,properties))
	   (bounds (bounds-of-thing-at-point 'word)))
       (if (and  (ov-at) (overlay-get (ov-at) 'ov-highlighter))
	   ;; add face properties to current overlay face properties.
	   (let* (;; (current-properties (overlay-properties (ov-at)))
		  (cf (overlay-get (ov-at) 'face))
		  (fp (if (facep cf) (loop for (p . v) in (face-all-attributes cf)
					   if v
					   append
					   (list p v))
			cf))
		  prop val)
	     (while face
	       (setq prop (pop face)
		     val (pop face))
	       (plist-put fp prop val))
	     (overlay-put (ov-at) 'face fp)
	     (while properties
	       (setq prop (pop properties)
		     val (pop properties))
	       (overlay-put (ov-at) prop val)))
	 ;; make new overlay
	 (let* ((beg (if (region-active-p) (region-beginning) (car bounds)))
		(end (if (region-active-p) (region-end) (cdr bounds)))
		(ov (make-overlay beg end)))
	   (overlay-put ov 'face (if (functionp face)
				     (funcall face)
				   face))
	   (while properties
	     (setq prop (pop properties)
		   val (pop properties))
	     
	     (overlay-put ov prop (if (functionp val)
				      (funcall val)
				    val)))
	   (overlay-put ov 'ov-highlighter t)
	   (overlay-put ov 'ov-type ,label)
	   (set-buffer-modified-p t)
	   (let ((p (point)))
	     (when (mark)
	       (deactivate-mark))
	     (goto-char p))
	   ;; refresh highlights if needed
	   (let ((buf (get-buffer "*ov-highlights*")))
	     (when (and ov-highlighter-source
			(buffer-live-p ov-highlighter-source)
			buf)
	       (with-current-buffer buf
		 (ov-highlighter-refresh-list))))
	   ov)))))


;; see
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html
;; for details on setting face attributes

;; ** highlighters font effects
(ov-highlighter-make "bold" '(:weight bold))
(ov-highlighter-make "italic" '(:slant italic))
(ov-highlighter-make "underline" '(:underline t))
(ov-highlighter-make "strikethrough" '(:strike-through t))
(ov-highlighter-make "box" '(:box t))
(ov-highlighter-make "redbox" '(:box (:line-width 2 :color "red" :style released-button)))
(ov-highlighter-make "comic-sans" '(:family "Comic Sans MS"))

(ov-highlighter-make "yellow" '(:background "Yellow"))
(ov-highlighter-make "blue" '(:background "LightBlue"))
(ov-highlighter-make "pink" '(:background "Pink"))
(ov-highlighter-make "green" '(:background "Darkolivegreen1"))

(ov-highlighter-make "delete" '(:foreground "red" :strike-through t))
(ov-highlighter-make "insert" '(:foreground "blue"))

;; A user-selected color
(ov-highlighter-make "color" (lambda ()
			       (list :background
				     (plist-get
				      (get-text-property
				       0 'face
				       (completing-read
					"Color: "
					(progn
					  (save-selected-window
					    (list-colors-display))
					  (prog1
					      (with-current-buffer (get-buffer "*Colors*")
						(mapcar (lambda (line)
							  (append (list line)
								  (s-split " " line t)))
							(s-split "\n" (buffer-string))))
					    (kill-buffer "*Colors*")))))
				      :background))))

;; Change font color
(ov-highlighter-make "foreground" (lambda ()
				    (list :foreground
					  (plist-get (get-text-property
						      0 'face
						      (completing-read
						       "Color: "
						       (progn
							 (save-window-excursion
							   (list-colors-display))
							 (prog1
							     (with-current-buffer
								 (get-buffer "*Colors*")
							       (mapcar (lambda (line)
									 (append
									  (list line)
									  (s-split " " line t)))
								       (s-split
									"\n"
									(buffer-string))))
							   (kill-buffer "*Colors*")))))
						     :background))))

(defvar ov-highlighter-window-configuration nil
  "Stores the window configuration so we can restore it.")

(ov-highlighter-make "comment" '(:background "Orange1")
		     mouse-face highlight
		     local-map (lambda ()
				 (let ((map (make-sparse-keymap))
				       (edit-func
					(lambda ()
					  (interactive)
					  (setq ov-highlighter-window-configuration
						(current-window-configuration))
					  (let ((cb (current-buffer))
						(current-note (overlay-get (ov-at) 'help-echo)))
					    (switch-to-buffer "*ov-note*")
					    (erase-buffer)
					    (org-mode)
					    (insert (or current-note ""))
					    (let ((map (make-sparse-keymap)))
					      (setq header-line-format
						    "Click here or type s-<return> to finish. C-x k to cancel."))
					    (local-set-key
					     (kbd "C-x k")
					     `(lambda ()
						(interactive)
						(kill-buffer)))
					    (local-set-key
					     (kbd "s-<return>")
					     `(lambda ()
						(interactive)
						(let ((tooltip (buffer-substring-no-properties
								(point-min) (point-max))))
						  (kill-buffer)
						  (set-window-configuration
						   ov-highlighter-window-configuration)
						  (setq ov-highlighter-window-configuration nil)
						  (overlay-put (ov-at) 'help-echo tooltip))))))))
				   (define-key map [mouse-1] edit-func)
				   map))
		     help-echo (lambda ()
				 (setq ov-highlighter-window-configuration (current-window-configuration))
				 (switch-to-buffer "*ov-note*")
				 (erase-buffer)
				 (org-mode)
				 (let ((map (make-sparse-keymap)))
				   (setq header-line-format
					 (propertize
					  "Enter comment. Type s-<return> to finish. C-x k to cancel."
					  'local-map map)))

				 (use-local-map (copy-keymap org-mode-map))
				 
				 ;; Cancel
				 (local-set-key
				  (kbd "C-x k")
				  `(lambda ()
				     (interactive)
				     (kill-buffer)))

				 ;; Finish comment
				 (local-set-key
				  (kbd "s-<return>")
				  `(lambda ()
				     (interactive)
				     (let ((tooltip (buffer-substring-no-properties
						     (point-min) (point-max))))
				       (kill-buffer)
				       (set-window-configuration ov-highlighter-window-configuration)
				       (setq ov-highlighter-window-configuration nil)
				       (overlay-put (if (ov-at)
							(ov-at)
						      (ov-at (- (point) 1)))
						    'help-echo tooltip))))))
;; font color
(ov-highlighter-make "red-fg" '(:foreground "red"))

(ov-highlighter-make "typo" '(:background "PaleVioletRed1") help-echo "tpyo")

(ov-highlighter-make "font" (lambda ()
			      (list :family
				    (completing-read "Font: " (font-family-list)))))

;; ** font size changes
(defun ov-highlighter-increase-font-size (&optional arg)
  "Increase the font size of the overlay at point.
When no overlay is at point make one for the region or word at
point. With numeric prefix ARG set font to that size."
  (interactive "P")
  (let ((bounds (if (region-active-p)
		    (cons (region-beginning) (region-end))
		  (bounds-of-thing-at-point 'word))))
    (flyspell-delete-region-overlays (car bounds) (cdr bounds))
    (add-hook 'before-save-hook 'ov-highlighter-save nil t)
    (if (ov-at)
	;; overlay at point, update it
	(progn
	  (let* (;; (current-properties (overlay-properties (ov-at)))
		 (cf (overlay-get (ov-at) 'face))
		 (fp (if (facep cf) (loop for (p . v) in (face-all-attributes cf)
					  if v
					  append
					  (list p v))
		       cf))
		 (ht (or arg (if (plist-get fp :height)
				 (floor (* 1.1 (plist-get fp :height)))
			       (* 1.1 (face-attribute 'default :height)))))
		 ;; prop val
		 )
	    (plist-put fp :height ht)
	    (message "Set height to %s." ht)
	    (overlay-put (ov-at) 'face fp)))
      (let ((ov (make-overlay (car bounds) (cdr bounds)))
	    (ht (or arg (floor (* 1.1 (face-attribute 'default :height))))))
	(overlay-put ov 'face (list :height ht))
	(message "Set height to %s." ht)
	(overlay-put ov 'ov-highlighter t)
	(set-buffer-modified-p t)
	(let ((p (point)))
	  (when (mark)
	    (deactivate-mark))
	  (goto-char p))

	(let ((buf (get-buffer "*ov-highlights*")))
	  (when (and ov-highlighter-source
		     (buffer-live-p ov-highlighter-source)
		     buf)
	    (with-current-buffer buf
	      (ov-highlighter-refresh-list))))
	ov))))


(defun ov-highlighter-decrease-font-size (&optional arg)
  "Decrease the font size of the overlay at point.
When no overlay is at the point make one for the region or word
at point. With numeric prefix ARG set font to that size."
  (interactive "P")
  (let ((bounds (if (region-active-p)
		    (cons (region-beginning) (region-end))
		  (bounds-of-thing-at-point 'word))))
    (flyspell-delete-region-overlays (car bounds) (cdr bounds))
    (add-hook 'before-save-hook 'ov-highlighter-save nil t)
    (if (ov-at)
	;; overlay at point, update it
	(progn
	  (let* (;; (current-properties (overlay-properties (ov-at)))
		 (cf (overlay-get (ov-at) 'face))
		 (fp (if (facep cf) (loop for (p . v) in (face-all-attributes cf)
					  if (not (eq 'unspecified v))
					  append
					  (list p v))
		       cf))
		 (ht (or arg (if (plist-get fp :height)
				 (floor (* 0.9 (plist-get fp :height)))
			       (* 0.9 (face-attribute 'default :height)))))
		 ;; prop val
		 )
	    (plist-put fp :height ht)
	    (overlay-put (ov-at) 'face fp)))
      (let ((ov (make-overlay (car bounds) (cdr bounds)))
	    (ht (or arg (floor (* 0.9 (face-attribute 'default :height))))))
	(overlay-put ov 'face (list :height ht))
	(message "Set :height to %s." ht)
	(overlay-put ov 'ov-highlighter t)
	(set-buffer-modified-p t)
	(let ((p (point)))
	  (when (mark)
	    (deactivate-mark))
	  (goto-char p))
	(let ((buf (get-buffer "*ov-highlights*")))
	  (when (and ov-highlighter-source
		     (buffer-live-p ov-highlighter-source)
		     buf)
	    (with-current-buffer buf
	      (ov-highlighter-refresh-list))))
	ov))))


;; * Clearing highlights
;;;###autoload
(defun ov-highlighter-clear ()
  "Clear highlight at point."
  (interactive)
  (when-let (ov (ov-at))
    (delete-overlay ov))

  (set-buffer-modified-p t)
  (let ((buf (get-buffer "*ov-highlights*")))
    (when buf
      (with-current-buffer buf
	(ov-highlighter-refresh-list)))))


;;;###autoload
(defun ov-highlighter-clear-all ()
  "Clear all highlighted text.
They are really deleted when you save the buffer."
  (interactive)
  (mapc 'delete-overlay (ov-highlighter-get-overlays))
  (set-buffer-modified-p t))


;; * List highlights

(defun ov-highlighter-get-overlays ()
  "Return a list of the highlight overlays.
The list is from first to last."
  (reverse (-filter (lambda (ov) (overlay-get ov 'ov-highlighter))
		    (overlays-in (point-min) (point-max)))))


(defvar ov-highlighter-source nil
  "A cons cell of the buffer to get highlights from.")


(defvar ov-highlighter-window-configuration nil
  "Stores the window configuration.")


(defun ov-highlighter-display ()
  "Display the current overlays in the current buffer in a tabulated list form."
  (interactive)
  (save-buffer)
  (let ((buf (current-buffer)))
    (setq ov-highlighter-window-configuration (current-window-configuration))
    (switch-to-buffer-other-window
     (get-buffer-create "*ov-highlights*"))
    (ov-highlighter-list-mode)
    (setq ov-highlighter-source buf)
    (ov-highlighter-refresh-list)))


(defun ov-highlighter-refresh-list ()
  "Refresh the list of overlays in the buffer."
  (let ((highlights) (entries))
    (with-current-buffer ov-highlighter-source
      (setq highlights (ov-highlighter-get-overlays))
      (setq entries (loop for ov in highlights
			  collect
			  (list
			   nil		;id
			   (vector
			    (cons (buffer-substring (ov-beg ov) (ov-end ov))
				  (list 'face (overlay-get ov 'face)
					'ov-position (ov-beg ov)))
			    ;; the help-echo
			    (replace-regexp-in-string
			     "\n" " "
			     (or (overlay-get ov 'help-echo) "")))))))
    (setq tabulated-list-entries entries
	  tabulated-list-format (vector '("Highlight" 40 t) '("Note" 40 t)))
    (tabulated-list-init-header)
    (tabulated-list-print)))


(defun ov-highlighter-jump ()
  "In list mode, jump to the highlight."
  (interactive)
  (let ((pos (get-text-property (line-beginning-position) 'ov-position)))
    (when pos
      (switch-to-buffer-other-window ov-highlighter-source)
      (goto-char pos)
      (org-show-entry))))


(defvar ov-highlighter-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key tabulated-list-mode-map (kbd "q")
      (lambda ()
	(interactive)
	(kill-buffer)
	(set-window-configuration
	 ov-highlighter-window-configuration)))
    (define-key map (kbd "r")
      (lambda () (interactive) (ov-highlighter-refresh-list)))
    (define-key map (kbd "o") 'ov-highlighter-jump)
    (define-key map (kbd "[mouse-1]") 'ov-highlighter-jump)
    (define-key map (kbd "<return>") 'ov-highlighter-jump)
    (define-key map (kbd "k")
      (lambda ()
	(interactive)
	(let ((p (point)))
	  (save-window-excursion
	    (ov-highlighter-jump)
	    (ov-highlighter-clear))
	  (goto-char p))))
    (define-key map (kbd "e")
      (lambda ()
	(interactive)
	(ov-highlighter-jump)
	(setq ov-highlighter-window-configuration (current-window-configuration))
	(let ( ;; (cb (current-buffer))
	      (current-note (overlay-get (ov-at) 'help-echo)))
	  (switch-to-buffer "*ov-note*")
	  (erase-buffer)
	  (org-mode)
	  (insert (or current-note ""))
	  ;; (let ((map (make-sparse-keymap)))
	  ;;   (setq header-line-format
	  ;; 	  "Click here or type s-<return> to finish. C-x k to cancel."))
	  (setq header-line-format
		"Click here or type s-<return> to finish. C-x k to cancel.")
	  (local-set-key
	   (kbd "C-x k")
	   `(lambda ()
	      (interactive)
	      (kill-buffer)))
	  (local-set-key
	   (kbd "s-<return>")
	   `(lambda ()
	      (interactive)
	      (let ((tooltip (buffer-substring-no-properties (point-min) (point-max))))
		(kill-buffer)
		(set-window-configuration ov-highlighter-window-configuration)
		(setq ov-highlighter-window-configuration nil)
		(overlay-put (ov-at) 'help-echo tooltip)
		(let ((buf (get-buffer "*ov-highlights*")))
		  (when (and ov-highlighter-source
			     (buffer-live-p ov-highlighter-source)
			     buf)
		    (with-current-buffer buf
		      (ov-highlighter-refresh-list))))))))))
    map)
  "Local keymap for `ov-highlighter-list-mode.")


(define-derived-mode ov-highlighter-list-mode
  tabulated-list-mode "ov-highlighters"
  "Mode for viewing ov-highlighters as a tabular list.
\\{ov-highlighter-list-mode-map}"
  (setq tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook
	    #'ov-highlighter-refresh-list))


;; * Save and load functions
(defun ov-highlighter-get-highlights ()
  "Return a list of (beg end color note) for the overlays."
  (mapcar (lambda (ov)
	    (list (overlay-start ov)
		  (overlay-end ov)
		  (overlay-properties ov)))
	  (ov-highlighter-get-overlays)))


(defun ov-highlighter-read-data ()
  "Read the data saved in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (format "^%s.*?ov-highlighter-data: \\(.*\\)" comment-start))
    (read (org-link-unescape (match-string 1)))))


(defun ov-highlighter-load ()
  "Load and apply highlighted text."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "#  ov-highlighter-data: \\(.*\\)" nil t)
      (setq ov-highlighter-data (match-string 1))
      (add-text-properties
       (match-beginning 1) (match-end 1)
       '(display "..." invisible 'ov-highlighter-data))
      (add-to-invisibility-spec 'ov-highlighter-data))
    (mapc
     (lambda (entry)
       (let ((beg (nth 0 entry))
	     (end (nth 1 entry))
	     (properties (nth 2 entry)))
	 (flyspell-delete-region-overlays beg end)
	 (let ((ov (make-overlay beg end)))
	   (apply 'ov-put ov properties)
	   (set-buffer-modified-p t)
	   (let ((p (point)))
	     (when (mark)
	       (deactivate-mark))
	     (goto-char p))
	   ov)))
     (when ov-highlighter-data
       (read (base64-decode-string (or ov-highlighter-data ""))))))
  (add-hook 'before-save-hook 'ov-highlighter-save nil t)
  ;; loading marks the buffer as modified, because the overlay functions mark
  ;; it, but it isn't. We mark it unmodified here.
  (set-buffer-modified-p nil))


(defun ov-highlighter-save ()
  "Save highlight information.
Data is saved in comment in the document."
  (let* ((data (ov-highlighter-get-highlights))
	 (data-b64 (propertize
		    (base64-encode-string (format "%S" data) t)
		    'display "..."
		    'invisible 'ov-highlighter-data)))
    (save-restriction
      (widen)
      (save-excursion
	(goto-char (point-min))
	(unless (re-search-forward "eval: (ov-highlighter-load)" nil 'mv)
	  (add-file-local-variable 'eval '(ov-highlighter-load)))

	(goto-char (point-min))
	(if (re-search-forward "#  ov-highlighter-data: \\(.*\\)" nil 'mv)
	    (progn
	      (setf (buffer-substring (match-beginning 0) (match-end 0)) "")
	      (insert (format "#  ov-highlighter-data: %s"
			      data-b64)))
	  (re-search-backward "Local Variables:")
	  (goto-char (line-beginning-position))
	  (insert (format
		   "#  ov-highlighter-data: %s\n\n"
		   data-b64)))))
    
    (unless data
      ;; cleanup if we have no highlights
      (remove-hook 'before-save-hook 'ov-highlighter-save t)
      (delete-file-local-variable 'ov-highlighter-data))
    (add-to-invisibility-spec 'ov-highlighter-data)))

;; add the local var we use as safe so we don't get annoyed by permission to run
;; it.
(add-to-list 'safe-local-eval-forms
	     '(ov-highlighter-load))


;; * The hydra menu

(defhydra ov-highlighter (:color blue :hint nil)
  "
^Highlight^       ^Markup^       ^Font^        ^Edit^       ^List^
_g_: green        _b_: bold      _[_: decrease  _t_: typo    _l_: list
_p_: pink         _i_: italic    _]_: increase  _m_: comment _k_: clear
_y_: yellow       _u_: underline _F_: Change    _d_: delete  _K_: clear all
_c_: choose       _s_: strike    ^ ^            _n_: insert
_f_: foreground   _x_: box
"
  ("g" ov-highlighter-green "green")
  ("p" ov-highlighter-pink "pink")
  ("y" ov-highlighter-yellow "yellow")
  ("c" ov-highlighter-color "Choose color")
  ("f" ov-highlighter-foreground "Foreground")
  ("d" ov-highlighter-delete)
  ("n" ov-highlighter-insert)

  ("[" ov-highlighter-decrease-font-size "Make size smaller" :color red)
  ("]" ov-highlighter-increase-font-size "Make size bigger" :color red)
  ("F" ov-highlighter-font "Font")

  ("b" ov-highlighter-bold "Bold")
  ("i" ov-highlighter-italic "Italic")
  ("u" ov-highlighter-underline "Underline")
  ("s" ov-highlighter-strikethrough "Strikethrough")
  ("x" ov-highlighter-box "Box")

  ;; editmark/feedback options
  ("t" ov-highlighter-typo "Typo")
  ("m" ov-highlighter-comment "Comment highlight")
  
  ("l" ov-highlighter-display "List highlights")
  ("k" ov-highlighter-clear "Clear highlight")
  ("K" ov-highlighter-clear-all "Clear all highlights")
  ("q" quit-window "quit"))


;; * Copy, cut and paste
(defvar ov-highlighter-copy-data  '()
  "A p-list of overlays to recreate on copying and pasting.")

(defun ov-highlighter-copy (beg end)
  "Copy the region from BEG to END with overlays for pasting.
Uses `ov-highlighter-paste'. Notes: we save the starts and ends
separately, because the ends get lost when you cut overlays."
  (interactive "r")
  (let ((ovs (loop for ov in (overlays-in beg end)
		   if (overlay-get ov 'ov-highlighter)
		   collect (copy-overlay ov))))
    (setq ov-highlighter-copy-data (list
				    :point beg
				    :text (buffer-substring beg end)
				    :overlays ovs
				    :starts (mapcar #'ov-beg ovs)
				    :ends (mapcar #'ov-end ovs)))))

(defun ov-highlighter-paste ()
  "Paste the data from `ov-highlighter-copy-data' at point."
  (interactive)
  (let ((p (point)))
    (let* ((text (plist-get ov-highlighter-copy-data :text))
	   (ovs (plist-get ov-highlighter-copy-data :overlays))
	   (starts (plist-get ov-highlighter-copy-data :starts))
	   (ends (plist-get ov-highlighter-copy-data :ends)))
      (save-excursion (insert text))
      (loop for ov in ovs for beg in starts for end in ends
	    if (overlay-get ov 'ov-highlighter)
	    do
	    (let* ((delta (- beg (plist-get ov-highlighter-copy-data :point)))
		   (nov (make-overlay (+ p delta) (+ p delta (- end beg)))))
	      (apply #'ov-put nov (overlay-properties ov))
	      (delete-overlay ov)))
      (forward-char (length text)))
    (setq ov-highlighter-copy-data nil)))


;; *** Advices - copy

(defun ov-highlighter-copy-advice (orig-func &rest args)
  "Advice function for copy to enable ov-highlighters to be copied.
ORIG-FUNC here will be `kill-ring-save'.
ARGS is the original arguments to that function."
  (let ((beg (nth 0 args))
	(end (nth 1 args)))
    (if (-any #'identity (mapcar (lambda (ov)
				   (overlay-get ov 'ov-highlighter))
				 (overlays-in beg end)))
	(ov-highlighter-copy beg end)
      (apply orig-func args))))


;; *** cut advice

(defun ov-highlighter-cut-advice (orig-func &rest args)
  "Advice function for cut so we get ov-highlighters.
ORIG-FUNC here will be `kill-region'.
ARGS is the original arguments to that function."
  (let ((beg (nth 0 args))
	(end (nth 1 args)))
    (if (-any #'identity (mapcar (lambda (ov)
				   (overlay-get ov 'ov-highlighter))
				 (overlays-in beg end)))
	(progn (ov-highlighter-copy beg end)
	       (ov-clear beg end)
	       (setf (buffer-substring beg end) ""))
      (apply orig-func args))))


;; *** Paste advice

(defun ov-highlighter-paste-advice (orig-func &rest args)
  "Advice function for paste so we can get ov-highlighters.
ORIG-FUNC will be `yank'.
ARGS is the original arguments to that function."
  (if (not (null ov-highlighter-copy-data))
      (ov-highlighter-paste)
    (apply orig-func args)))


;; *** kill line advice
(defun ov-highlighter-kill-line-advice (orig-func &rest args)
  "Advise kill line so we get ov-highlighters.
ORIG-FUNC will be `kill-visual-line'.
ARGS is the original arguments to that function."
  (if (-any #'identity (mapcar (lambda (ov)
				 (overlay-get ov 'ov-highlighter))
			       (overlays-in
				(line-beginning-position)
				(line-end-position))))
      (progn
	(ov-highlighter-copy (line-beginning-position) (line-end-position))
	(ov-clear (line-beginning-position) (line-end-position))
	(setf (buffer-substring (line-beginning-position) (line-end-position)) ""))
    (apply orig-func args)))


(defun ov-highlighter-cut-copy-paste-on ()
  "Turn advices for cut/copy/paste/kill on.
Note: if this causes bad behavior use
`ov-highlighter-cut-copy-paste-off' to remove the advices."
  (interactive)
  (advice-add 'kill-ring-save :around 'ov-highlighter-copy-advice)
  (advice-add 'kill-region :around 'ov-highlighter-cut-advice)
  (advice-add 'yank :around 'ov-highlighter-paste-advice)
  (advice-add 'kill-visual-line :around 'ov-highlighter-kill-line-advice))


(defun ov-highlighter-cut-copy-paste-off ()
  "Turn advices for cut/copy/paste/kill off."
  (interactive)
  (advice-remove 'kill-ring-save 'ov-highlighter-copy-advice)
  (advice-remove 'kill-region 'ov-highlighter-cut-advice)
  (advice-remove 'yank 'ov-highlighter-paste-advice)
  (advice-remove 'kill-visual-line 'ov-highlighter-kill-line-advice))

;; On by default
(ov-highlighter-cut-copy-paste-on)


;; * HTML

(defun ov-highlighter-html (file)
  "Convert the buffer to html using htmlize and write to FILE."
  (interactive (list (read-file-name
		      "File: " nil (concat (file-name-base) ".html"))))
  (let ((buf (htmlize-buffer)))
    (with-current-buffer buf
      (write-file file (buffer-string)))
    (kill-buffer buf)
    (browse-url file)))


;; * The End
(provide 'ov-highlighter)

;;; ov-highlighter.el ends here
