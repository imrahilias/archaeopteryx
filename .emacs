;;  ___   ___  ___  ___   ___  ___               ___        ___ 
;; |   |=|_.' `._|=|   |=|_.' |   | |`.     .'|=|_.'   .'|=|_.' 
;; `.  |           |   |      |   | |  `. .'  |  ___ .'  |  ___ 
;;   `.|=|`.       |   |      |   | |   | |   |=|_.' |   |=|_.' 
;;  ___  |  `.     `.  |      `.  | |   | |   |      |   |      
;;  `._|=|___|       `.|        `.|=|___| |___|      |___|      


;; fun
;;(global-set-key (kbd "C-x C-s") 'spook)

(require 'package)

;; MELPA
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

;; Added by Package.el.  This must come before configurations of installed packages.  Don't delete
;; this line.  If you don't want it, just comment it out by adding a semicolon to the start of the
;; line.  You may delete these explanatory comments.
(package-initialize)

;; no welcome message please
(setq inhibit-startup-message 't)
;; inital scratch text
(setq initial-scratch-message "")

;; syntax highlighting
(global-font-lock-mode 't)
(setq font-lock-maximum-decoration 't)

;; set fill paragraph size.
(setq-default fill-column 100)

;; highlight symbol under point.
;(require 'symbol-overlay)
(symbol-overlay-mode 't)

;; show me line and column nos
;;(line-number-mode 't)
(column-number-mode 't)

;; none of these please
(scroll-bar-mode '0)
(tool-bar-mode '0)
(menu-bar-mode '0)

;; save and restore my buffers every time
;;(desktop-save-mode 1)

;; Show Date And time in mode-line
;;(setq display-time-day-and-date t )
;;(setq display-time-24hr-format t)
;;(display-time)

;; line numbers
(global-linum-mode 't)

;; fonts
;;(set-frame-font "Bitstream Vera Sans Mono Roman" nil t)
;;(set-frame-font "Inconsolata 12" nil t)
(set-face-attribute 'default nil :height 120)

;; make it easy on eyes ...
(set-foreground-color "#A89C8C")
(set-background-color "#202020")
(set-face-foreground 'default "#A89C8C")
(set-face-background 'default "#202020")

;; window modifications
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; scrolling
(setq
 scroll-margin 5 ;; when to start scrolling
 scroll-conservatively 0
 )
(setq-default
 scroll-up-aggressively 0.01
 scroll-down-aggressively 0.01
 )

(defun scroll-down-keep-cursor ()
  ;; Scroll the text one line down while keeping the cursor
  (interactive)
  (scroll-down 1)
  )

(defun scroll-up-keep-cursor ()
  ;; Scroll the text one line up while keeping the cursor
  (interactive)
  (scroll-up 1)
  )

(global-set-key (kbd "C-,") 'scroll-down-keep-cursor)
(global-set-key (kbd "C-.") 'scroll-up-keep-cursor)

;; scrolling mouse
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ;; two lines at a time
(setq Mouse-Wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; Interactively Do Things mode
(require 'ido)
(ido-mode t)

;; ;; fill column indicator
;; (add-to-list 'load-path "~/.emacs.d/fill-column-indicator.el")
;; (require 'fill-column-indicator)
;; (define-globalized-minor-mode
;;  global-fci-mode fci-mode (lambda() (fci-mode 0)))
;; (setq fci-rule-width 1)
;; (setq fci-rule-color "darkgrey")
;; (setq-default fci-rule-column 80)

;; ;; fci-mode sux in the terminal, i want it just in graphical mode
;; (if (display-graphic-p)
;;    (progn
;;      ;; if graphic
;;      (global-fci-mode 1))
;;      ;; else
;;  (global-fci-mode 0)
;; )

;; highlight the current line
;;(setq hl-line-face 'hl-line)
(global-hl-line-mode '0)
;;(setq highlight-current-line-globally t)
;;(setq highlight-current-line-high-faces nil)
;;(setq highlight-current-line-whole-line nil)
;;(setq hl-line-face (quote highlight))

;; custom keyboard shortcuts
(global-set-key (kbd "C-c m") 'compile)

;; current date
(defun insert-current-date() (interactive)
       (insert (shell-command-to-string "echo -n $(date '+%Y-%m-%d %k:%M')"))
       )
(global-set-key (kbd "C-c d") 'insert-current-date)

;; convenience
(global-set-key (kbd "C-c r") (lambda() (interactive) (load-file "~/.emacs")))
(defun em ()
  (interactive)
  (find-file "~/.emacs")
  )

;; emacs paste on line curser (not mouse)
(setq mouse-yank-at-point t)

;; printer command
(setq lpr-command "lp")
(setq lpr-add-switches nil)

;; switch window with tab
(global-set-key [C-tab] 'other-window)
(global-set-key [C-C-tab]
                (lambda ()
                  (interactive)
                  (other-window -1)
                  )
                )

(custom-set-variables
 ;; custom-set-variables was added by Custom.  If you edit it by hand, you could mess it up, so be
 ;; careful.  Your init file should contain only one such instance.  If there is more than one, they
 ;; won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(package-selected-packages
   '(symbol-overlay lua-mode flycheck arduino-cli-mode arduino-mode markdown-mode company auto-complete auctex matlab-mode live-py-mode rainbow-identifiers rainbow-mode ess auto-correct)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.  If you edit it by hand, you could mess it up, so be
 ;; careful.  Your init file should contain only one such instance.  If there is more than one, they
 ;; won't work right.
 '(symbol-overlay-default-face ((t (:inherit symbol-overlay-face-8)))))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; subword mode (camelcase mode)
(setq-default global-subword-mode 't)

;; i hate tabs!
;;(setq-default indent-tabs-mode nil)

;; refresh buffers on change
(setq-default global-auto-revert-mode 't)

;; el-get
;; (add-to-list 'load-path "~/.emacs.d/el-get/")
;;  (add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; (unless (require 'el-get nil 'noerror)
;;   (with-current-buffer
;;       (url-retrieve-synchronously "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
;;     (goto-char (point-max))
;;     (eval-print-last-sexp)
;;     )
;;   )

;; (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
;; (el-get 'sync)

;; auto break lines in paragraphs
;; add-hook 'text-mode-hook 'turn-on-auto-fill)

;;                               ___         ___   ___                           ___         ___  ___   ___ 
;;   .'|        .'|=|`.     .'| |   |   .'|=|_.'  |   | |`.     .'|=|`.     .'|=|_.'    .'|=|_.' |   |=|_.' 
;; .'  |      .'  | |  `. .'  |\|   | .'  |___    |   | |  `. .'  | |  `. .'  |___    .'  |  ___ `.  |      
;; |   |      |   |=|   | |   | |   | |   |`._|=. |   | |   | |   |=|   | |   |`._|=. |   |=|_.'   `.|=|`.  
;; |   |  ___ |   | |   | |   | |  .' `.  |  __|| `.  | |   | |   | |   | `.  |  __|| |   |  ___  ___  |  `.
;; |___|=|_.' |___| |___| |___| |.'     `.|=|_.''   `.|=|___| |___| |___|   `.|=|_.'' |___|=|_.'  `._|=|___|


;; start auto-complete-mode
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (auto-complete-mode 1))))

;; highlight parentheses when the cursor is next to them
(require 'paren)
(show-paren-mode t)

;; c mode customizations
(cwarn-mode t)
(setq c-default-style "linux")
(which-function-mode t)
(setq c-basic-offset 2)
(global-set-key (kbd "C-c p") 'compile)


;; lua-mode
;; This line is not necessary, if lua-mode.el is already on your load-path
                                        ;(add-to-list 'load-path "~/.emacs.d/el-get/lua-mode")
;; (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
;; (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
;; (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))


;; octave mode
(global-set-key (kbd "C-c C-c") 'octave-send-region)


;; Latex mode

(with-eval-after-load "tex"
  (add-to-list 'TeX-command-list
               `("Arara" "arara --verbose %s" TeX-run-TeX nil t :help "Run Arara") t)
  (add-to-list 'TeX-command-list
               `("Extex" "lualatex -synctex=1 -interaction=nonstopmode --shell-escape %s" TeX-run-TeX nil t :help "LuaLatex + SyncTex + ShellEscape + NonstopMode (no halt-on-error)") t)
  (tex-source-correlate-mode t) )
  
(with-eval-after-load "latex"
  (define-key LaTeX-mode-map (kbd "C-c C-a")
    (lambda ()
      (interactive)
      (TeX-command-sequence '("Arara" "Extex") t))))
;;        (TeX-command-sequence '("Arara" "View") t))))


;;  ___   ___        __          ___                        ___   ___ 
;; |   |=|_.'   .'|=|  |    .'|=|_.'   .'|        .'|      |   |=|_.' 
;; `.  |      .'  | |  |  .'  |  ___ .'  |      .'  |      `.  |      
;;   `.|=|`.  |   |=|.'   |   |=|_.' |   |      |   |        `.|=|`.  
;;  ___  |  `.|   |       |   |  ___ |   |  ___ |   |  ___  ___  |  `.
;;  `._|=|___||___|       |___|=|_.' |___|=|_.' |___|=|_.'  `._|=|___|



;; Flyspell mode
;(global-flycheck-mode 't)
 
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; If you’re using a Mac, you may need to add the following Elisp code to your config file as well in order for Flyspell to pick up the two-finger clicks (right-clicks):
;; (eval-after-load "flyspell"
;;   '(progn
;;      (define-key flyspell-mouse-map [mouse-3] #'flyspell-correct-word)))


;; Ispell
(with-eval-after-load "ispell"
  ;; Configure `LANG`, otherwise ispell.el cannot find a 'default
  ;; dictionary' even though multiple dictionaries will be configured
  ;; in next line.
  ;;(setenv "LANG" "en_GB")
  (setq ispell-program-name "hunspell")
  ;; Configure German, Swiss German, and two variants of English.
  (setq ispell-dictionary "de_DE,en_GB,en_US")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "de_DE,en_GB,en_US")
  ;; For saving words to the personal dictionary, don't infer it from
  ;; the locale, otherwise it would save to ~/.hunspell_de_DE.
  (setq ispell-personal-dictionary "~/.hunspell_personal"))

;; The personal dictionary file has to exist, otherwise hunspell will silently not use it.
;;(unless (file-exists-p ispell-personal-dictionary)
;;(write-region "" nil ispell-personal-dictionary nil 0))


;; (global-set-key (kbd "<C>-<mouse-8>") (kbd "C-v"))
;; (global-set-key (kbd "<C>-<mouse-9>") (kbd "M-v"))

;; (global-set-key (kbd "<M>-<left>") (kbd "C-v"))
;; (global-set-key (kbd "<M>-<right>") (kbd "M-v"))

;; (define-key (current-local-map) (kbd "mouse-8")
;;   (lookup-key (current-local-map) (kbd "C-v")))

(setenv "LANG" "en_US.UTF-8")
