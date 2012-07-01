;; First, turn off toolbars and scrollbars so they don't flash
(progn (dolist (mode '(tool-bar-mode scroll-bar-mode))
	 (when (fboundp mode) (funcall mode -1))))

(add-to-list 'load-path "~/.emacs.d/lib/")

(defvar my-packages
  '(solarized-theme ;; teh best
    evil ;; Better Vim style modal editing
    evil-leader ;; Provides a Vim leader key
    paredit
    smex ;; ido mode for M-x basically
    ido-ubiquitous ;; use ido mode whenever possible
    yasnippet ;; Snippets
    yas-jit ;; load Yasnippets on demand
    clojure-mode
    clojure-project-mode
    clojure-test-mode
    elein ;; Leiningen support from emacs
    durendal ;; Clojure stuff
    jade-mode ;; Major mode for jade templates
    markdown-mode))

(require-and-exec 'package
		  (add-to-list 'package-archives
			       '("marmalade" . "http://marmalade-repo.org/packages/") t)
		  (package-initialize)
		  (dolist (package my-packages)
		    (unless (package-installed-p package)
		      (package-install package)))) 

(require 'my-keymaps)
(require 'my-settings)

(add-hook 'clojure-mode-hook 'pretty-fn)

(add-hook 'emacs-startup-hook (lambda ()
                                (message "Time needed to load: %s seconds."
                                         (emacs-uptime "%s"))) 'append)

(add-hook 'after-save-hook 'byte-compile-config-on-save)
