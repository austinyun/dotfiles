(require 'package)

(progn
  (dolist (mode '(tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1))))

(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(defvar my-packages
  '(solarized-theme
    paredit
    smex ;; ido mode for M-x basically
    ido-ubiquitous ;; use ido mode whenever possible
    yasnippet ;; Snippets
    yas-jit ;; load Yasnippets on demand
    clojure-mode
    clojure-project-mode
    clojure-test-mode
    elein ;; Leiningen support from emacs
    ;;durandel ;; Clojure stuff
    jade-mode ;; Major mode for jade templates
    markdown-mode))

;; Installs everything in the list my-packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; This seems like a good place to end the init file since everything after this
;; is a preference of some kind and those that depend on the packages have to
;; happen after here.

(load-theme 'solarized-dark t)
(set-default-font "Dina-12")

(require 'smex)
(setq smex-save-file (concat user-emacs-directory ".cache/.smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commnads)

;; ido mode
(require 'ido)
(setq ido-enable-flex-matching t
      ido-enable-prefix nil
      ido-max-prospects 10
      ido-save-directory-list-file "~/.emacs.d/.cache/.ido.last")
(ido-mode t)
(ido-ubiquitous t)

(when window-system
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

(setq visible-bell t
      inhibit-startup-message t
      sentence-end-double-space nil
      whitespace-line-column 80
      whitespace-style '(face trailing lines-tail tabs))

(defalias 'yes-or-no-p 'y-or-n-p)

(show-paren-mode 1)
