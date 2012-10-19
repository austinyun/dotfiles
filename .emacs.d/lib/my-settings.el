(load-theme 'solarized-dark t)
(set-default-font "Dina-8")

(require 'smex)
(setq smex-save-file (concat user-emacs-directory ".cache/.smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commnads)

(require 'ido)
(setq ido-enable-flex-matching t
      ido-enable-prefix nil
      ido-max-prospects 10
      ido-save-directory-list-file "~/.emacs.d/.cache/.ido.last")
(ido-mode t)
(ido-ubiquitous t)

(require 'auto-complete-config)
(ac-config-default)

(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))
(yas-global-mode 1)

(when window-system
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

(setq visible-bell t
      make-backup-files nil
      inhibit-startup-message t
      sentence-end-double-space nil
      whitespace-line-column 80
      whitespace-style '(face trailing lines-tail tabs))

(show-paren-mode 1)

(provide 'my-settings)
