;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Standard functional
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'linum)

(global-linum-mode)

(desktop-save-mode 1)

(tool-bar-mode nil)

(menu-bar-mode -1)

(scroll-bar-mode nil)  

(setq x-select-enable-clipboard t)

(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
)

(toggle-fullscreen)  

(display-time)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Haskell 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/")

(load "/home/shk/.emacs.d/haskellmode-emacs/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
			      
(setq haskell-program-name "/usr/bin/ghci")

(defun substitute-pattern-with-unicode (pattern symbol)
    "Add a font lock hook to replace the matched part of PATTERN with the                                       
     Unicode symbol SYMBOL looked up with UNICODE-SYMBOL."
    (font-lock-add-keywords
    nil `((,pattern 
           (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                     ,(unicode-symbol symbol)
                                     'decompose-region)
                             nil))))))

(defun substitute-patterns-with-unicode (patterns)
   "Call SUBSTITUTE-PATTERN-WITH-UNICODE repeatedly."
   (mapcar #'(lambda (x)
               (substitute-pattern-with-unicode (car x)
                                                (cdr x)))
           patterns))
           
(add-hook 'haskell-mode-hook 'haskell-unicode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Erlang 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                       
(setq load-path (cons "/usr/lib/erlang/lib/tools-2.6.6.1/emacs" 
	load-path))
		
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))

(add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))

(setq erlang-root-dir "/usr/lib/erlang")
(add-to-list 'exec-path "/usr/lib/erlang/bin")
(setq erlang-man-root-dir "/usr/lib/erlang/man")

(defun my-erlang-mode-hook ()
        (setq inferior-erlang-machine-options '("-sname" "emacs"))
        (imenu-add-to-menubar "imenu")
        (local-set-key [return] 'newline-and-indent)
        (set-frame-height (selected-frame) 20))

(defun erl-shell (flags)
   "Start an erlang shell with flags"
   (interactive (list (read-string "Flags: ")))
   (set 'inferior-erlang-machine-options (split-string flags))
   (set-frame-height  5)
   (erlang-shell))
   
(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)
(require 'erlang-start)

;;(add-to-list 'load-path "~/.emacs.d/distel/elisp")
;;(require 'distel)
;;(distel-setup)

;; A number of the erlang-extended-mode key bindings are useful in the shell too
(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-?"      erl-complete)
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind)
    ("\M-*"      erl-find-source-unwind)
    )
  "Additional keys to bind when in Erlang shell.")

(add-hook 'erlang-shell-mode-hook (lambda ()
          (dolist (spec distel-shell-keys)
          (define-key erlang-shell-mode-map (car spec) (cadr spec)))))
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Tabs 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'tabbar)

(global-set-key [C-left] 'tabbar-backward-tab)
(global-set-key [C-right] 'tabbar-forward-tab)
(global-set-key [C-tab] 'tabbar-forward-tab)

(global-set-key (kbd "C-x C-<right>") 'tabbar-forward-group)
(global-set-key (kbd "C-x C-<left>") 'tabbar-backward-group)

(set-face-attribute 'tabbar-default-face    nil   :background "gray0")
(set-face-attribute 'tabbar-unselected-face nil   :background "gray5"   :foreground "white"   :box nil)
(set-face-attribute 'tabbar-selected-face   nil   :background "dim gray" :foreground "orange"   :box nil)
(set-face-attribute 'tabbar-button-face nil :box  '(:line-width 3 :color "gray100" :style released-button))
(set-face-attribute 'tabbar-separator-face  nil   :height 0.8)

(tabbar-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Windows navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(windmove-default-keybindings)

(defun my-make-three-windows () 
  "Make three windows"
  (interactive)
  (split-window-horizontally)
  (split-window-vertically)	
  )

;; C-tab switchs to a next window
;;(global-set-key [(control tab)] 'other-window)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(cursor-in-non-selected-windows nil)
 '(display-time-mode t)
 '(ede-auto-add-method (quote always))
 '(show-paren-mode t)
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify))))


;;|-----------+-----------|
;;|           |           |
;;|           |           |
;;|-----------+           |
;;|           |           |
;;|           |           |
;;|-----------+-----------|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Save buffer
(global-set-key [f2] 'save-buffer)

;; Shell start
(global-set-key [f4] 'ansi-term)

;; gdb
(global-set-key [f5] 'gdb)

;; Prev error
(global-set-key [f11] 'next-error)

;; Next error
(global-set-key [f10] 'previous-error)

;; Compile
(global-set-key [(f9)] 'compile)

;; Recompile
(global-set-key [f7] 'recompile)

;; Go to line
(global-set-key "\C-g" 'goto-line)
(global-set-key "\M-g" 'goto-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Cua mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cua-mode t)
    (setq cua-auto-tabify-rectangles nil) 
    (transient-mark-mode 1) ;; No region when it is not highlighted
    (setq cua-keep-region-after-copy t) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Emacs encoding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Emacs theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/color-theme")
(require 'color-theme)
(color-theme-initialize)
(color-theme-gnome2)

(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "microsoft" :family "Consolas")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Emacs custom variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 '(abbrev-mode t)
 '(user-full-name "Alexander Kuleshov")
 '(user-mail-address "kuleshovmail@gmail.com")
 )
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Emacs dir tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/dir-utils")
(require 'dirtree)
