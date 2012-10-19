;; First, turn off toolbars and scrollbars so they don't flash
(progn (dolist (mode '(tool-bar-mode scroll-bar-mode))
	 (when (fboundp mode) (funcall mode -1))))

(add-to-list 'load-path "~/.emacs.d/lib/")

(defvar my-packages
  '(solarized-theme ;; teh best
    auto-complete
    ace-jump-mode
    paredit
    smex ;; ido mode for M-x basically
    idomenu
    ido-ubiquitous ;; use ido mode whenever possible
    yasnippet ;; Snippets
    yas-jit ;; load Yasnippets on demand
    nrepl
    ac-nrepl
    clojure-mode)) ;; Clojure stuff

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
(when (not package-archive-contents) (package-refresh-contents))
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

(require 'my-settings)
(require 'my-keymaps)
(require 'my-clojure)

(add-hook 'emacs-startup-hook (lambda ()
                                (message "Time needed to load: %s seconds."
                                         (emacs-uptime "%s"))) 'append)
