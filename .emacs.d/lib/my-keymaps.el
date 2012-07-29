;; Make ESC quit pretty much everything
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; Clojure
(evil-declare-key 'normal clojure-mode-map
                  ",k" 'slime-compile-and-load-file
                  ",K" 'slime-repl-compile-and-load
                  ",d" 'slime-documentation)

(provide 'my-keymaps)
