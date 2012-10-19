(provide 'my-keymaps)

;;disable suspending emacs on ctrl-z
(global-set-key (kbd "C-z") 'undo)
(global-unset-key (kbd "C-x C-z"))

;;use ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;use ace-jump-mode
(global-set-key (kbd "C-'") 'ace-jump-mode)

;;use idomenu to search for symbols
(global-set-key (kbd "C-o") 'idomenu)

(global-set-key (kbd "C-<tab>") 'yas-expand-from-trigger-key)

(defun goto-last-edit-point ()
  "Go to the last point where editing occurred."
  (interactive)
  (let ((undos buffer-undo-list))
    (when (listp undos)
      (while (and undos
		  (let ((pos (or (cdr-safe (car undos))
				 (car undos))))
		    (not (and (integerp pos)
			      (goto-char (abs pos))))))
	(setq undos (cdr undos))))))
(global-set-key (kbd "C-c SPC") 'goto-last-edit-point)
