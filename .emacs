;;  ___   ___  ___  ___   ___  ___               ___        ___ 
;; |   |=|_.' `._|=|   |=|_.' |   | |`.     .'|=|_.'   .'|=|_.' 
;; `.  |           |   |      |   | |  `. .'  |  ___ .'  |  ___ 
;;   `.|=|`.       |   |      |   | |   | |   |=|_.' |   |=|_.' 
;;  ___  |  `.     `.  |      `.  | |   | |   |      |   |      
;;  `._|=|___|       `.|        `.|=|___| |___|      |___|      


;; fun
;;(global-set-key (kbd "C-x C-s") 'spook)

(require 'package)

;; melpa
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

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; no welcome message please
(setq inhibit-startup-message t)

;; inital scratch text
(setq initial-scratch-message "")

;; UI and base colors
(setq theme-color-accent  "#ff6000")
(setq theme-color-level-1 "#1D1F21")
(setq theme-color-level-2 "#373B41")
(setq theme-color-level-3 "#C5C8C6")

;; common colors
(setq theme-color-red     "#A54242")
(setq theme-color-green   "#8C9440")
(setq theme-color-yellow  "#DE935F")
(setq theme-color-blue    "#5F819D")
(setq theme-color-magenta "#85678F")
(setq theme-color-cyan    "#5E8D87")
(setq theme-color-gray    "#707880")

;; syntax highlighting
(global-color-identifiers-mode 't)
(global-font-lock-mode 't)
(setq font-lock-maximum-decoration 't)
(setq font-lock-maximum-size '262144)
(global-hi-lock-mode 't)
(global-highlight-operators-mode 't)
(global-highlight-parentheses-mode 't)
(global-highlight-thing-mode 't)
(global-hl-line-mode 't)
;; makes ugly glyphs from greek letters!
;;(global-prettify-symbols-mode 't)
;; highlight the current line
;;(setq hl-line-face 'hl-line)
(global-hl-line-mode '0)
(setq highlight-current-line-globally t)
(setq highlight-current-line-high-faces nil)
(setq highlight-current-line-whole-line nil)
(setq hl-line-face (quote highlight))

;; highlighting lock
(custom-set-faces
 `(hi-black-b  ((t (:inherit (bold) :foreground ,theme-color-level-1 :background ,theme-color-gray))))
 `(hi-black-hb ((t (:inherit (bold) :foreground ,theme-color-level-3 :background ,theme-color-gray))))
 `(hi-blue     ((t (:foreground ,theme-color-level-1 :background ,theme-color-blue))))
 `(hi-blue-b   ((t (:inherit (hi-blue bold) :inverse-video t))))
 `(hi-green    ((t (:foreground ,theme-color-level-1 :background ,theme-color-green))))
 `(hi-green-b  ((t (:inherit (hi-green bold) :inverse-video t))))
 `(hi-pink     ((t (:foreground ,theme-color-level-1 :background ,theme-color-magenta))))
 `(hi-red-b    ((t (:inherit (bold) :foreground ,theme-color-red))))
 `(hi-yellow   ((t (:foreground ,theme-color-level-1 :background ,theme-color-yellow)))))

;; show me line and column nos
(line-number-mode 't)
(column-number-mode 't)
(global-linum-mode 't)

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
                                        ;(global-set-key (kbd "C-c d") 'insert-current-date)

;; scrolling mouse
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ;; two lines at a time
(setq Mouse-Wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; Interactively Do Things mode
(require 'ido)
(ido-mode t)

;; fill column indicator
;; (add-to-list 'load-path "~/.emacs.d/fill-column-indicator.el")
;; (require 'fill-column-indicator)
;; (define-globalized-minor-mode
;;  global-fci-mode fci-mode (lambda() (fci-mode 0)))
;; (setq fci-rule-width 1)
;; (setq fci-rule-color "darkgrey")
;; (setq-default fci-rule-column 80)

;; fci-mode sux in the terminal, i want it just in graphical mode
;; (if (display-graphic-p)
;;    (progn
;;      ;; if graphic
;;      (global-fci-mode 1))
;;      ;; else
;;  (global-fci-mode 0)
;; )

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
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(package-selected-packages
   '(ample-regexps fuzzy auto-complete-auctex luarocks highlight-unique-symbol highlight-defined highlight-function-calls highlight-thing highlight-symbol highlight-parentheses highlight-operators highlight highlight-blocks highlight-escape-sequences highlight-quoted highlight-numbers color-identifiers-mode lua-mode flycheck arduino-cli-mode arduino-mode markdown-mode company auto-complete auctex matlab-mode live-py-mode rainbow-identifiers rainbow-mode ess auto-correct)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; subword mode (camelcase mode)
(global-subword-mode 1)

;; i hate tabs!
(setq-default indent-tabs-mode nil)

;; refresh buffers on change
(global-auto-revert-mode t)

;; el-get
;; (add-to-list 'load-path "~/.emacs.d/el-get/")
;; (add-to-list 'load-path "~/.emacs.d/el-get/el-get")

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

;; un/compact block
(defun fill-or-unfill ()
  "Reformat current paragraph or region to `fill-column', like `fill-paragraph' or “unfill”.
When there is a text selection, act on the selection, else, act on a text block separated by blank lines.
URL `http://xahlee.info/emacs/emacs/modernization_fill-paragraph.html'
Version 2017-01-08"
  (interactive)
  ;; This command symbol has a property “'compact-p”, the possible values are t and nil. This property is used to easily determine whether to compact or uncompact, when this command is called again
  (let ( ($compact-p
          (if (eq last-command this-command)
              (get this-command 'compact-p)
            (> (- (line-end-position) (line-beginning-position)) fill-column)))
         (deactivate-mark nil)
         ($blanks-regex "\n[ \t]*\n")
         $p1 $p2
         )
    (if (use-region-p)
        (progn (setq $p1 (region-beginning))
               (setq $p2 (region-end)))
      (save-excursion
        (if (re-search-backward $blanks-regex nil "NOERROR")
            (progn (re-search-forward $blanks-regex)
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (if (re-search-forward $blanks-regex nil "NOERROR")
            (progn (re-search-backward $blanks-regex)
                   (setq $p2 (point)))
          (setq $p2 (point)))))
    (if $compact-p
        (fill-region $p1 $p2)
      (let ((fill-column most-positive-fixnum ))
        (fill-region $p1 $p2)))
    (put this-command 'compact-p (not $compact-p))))

;; now set it as default un/compact block function:
(global-set-key (kbd "M-q") 'fill-or-unfill)


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
;(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
;(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
;(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))


;; octave mode
;(global-set-key (kbd "C-c C-c") 'octave-send-region)


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
;        (TeX-command-sequence '("Arara" "View") t))))


;;  ___   ___        __          ___                        ___   ___ 
;; |   |=|_.'   .'|=|  |    .'|=|_.'   .'|        .'|      |   |=|_.' 
;; `.  |      .'  | |  |  .'  |  ___ .'  |      .'  |      `.  |      
;;   `.|=|`.  |   |=|.'   |   |=|_.' |   |      |   |        `.|=|`.  
;;  ___  |  `.|   |       |   |  ___ |   |  ___ |   |  ___  ___  |  `.
;;  `._|=|___||___|       |___|=|_.' |___|=|_.' |___|=|_.'  `._|=|___|


;; Flyspell mode
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

;; The personal dictionary file has to exist, otherwise hunspell will
;; silently not use it.
;;(unless (file-exists-p ispell-personal-dictionary)
;;(write-region "" nil ispell-personal-dictionary nil 0))


;; (global-set-key (kbd "<C>-<mouse-8>") (kbd "C-v"))
;; (global-set-key (kbd "<C>-<mouse-9>") (kbd "M-v"))

;; (global-set-key (kbd "<M>-<left>") (kbd "C-v"))
;; (global-set-key (kbd "<M>-<right>") (kbd "M-v"))

;; (define-key (current-local-map) (kbd "mouse-8")
;;   (lookup-key (current-local-map) (kbd "C-v")))

(setenv "LANG" "en_US.UTF-8")
