;;fun
;;(global-set-key (kbd "C-x C-s") 'spook)

;;(add-to-list 'load-path "~/.elisp")
;;(load "php-mode")
;;(load "lorem-ipsum.el")
;;(load "linum.el")
;;(load "word-count.el")

;; cedet
;(load-file "~/.elisp/cedet/common/cedet.el")
;(global-ede-mode 1)                      ; Enable the Project management system
;(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
;(global-srecode-minor-mode 1)            ; Enable template insertion menu
;(require 'semantic-ia)
;(require 'semantic-gcc)


;; lorem ipsum
(autoload 'Lorem-ipsum-insert-paragraphs "lorem-ipsum" "" t)
(autoload 'Lorem-ipsum-insert-sentences "lorem-ipsum" "" t)
(autoload 'Lorem-ipsum-insert-list "lorem-ipsum" "" t)


;; auto tag creation
;(defun create-tags (dir-name)
;  "Create tags file."
;  (interactive "DDirectory: ")
;  (eshell-command 
;   (format "find %s -type f -name '*.[ch]' -o -name '*.cpp' | etags -L -" dir-name)))


;; syntax highlighting
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size 262144)

;; show me line and column nos
(line-number-mode t)
(column-number-mode t)
;; show current function too
;;(show-func-mode t)

;; none of these please
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

;; save and restore my buffers every time
;;(desktop-save-mode 1)

;; different buff switcher, makes me nervous
;; (ido-mode t)

;; c mode customizations
(cwarn-mode t)
(setq c-default-style "linux")
(which-func-mode t)
;;(hide-ifdefs t)

;; show date and time in mode-line
(setq display-time-day-and-date t )
(setq display-time-24hr-format t)
(display-time)

;; line numbers
(global-linum-mode 1)

;; make it easy on eyes ...
(set-foreground-color "gray")
(set-background-color "black")

(set-face-foreground 'default "gray")
(set-face-background 'default "black")