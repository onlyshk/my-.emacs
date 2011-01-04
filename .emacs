;;
;; Emacs Config 
;;

(add-to-list 'load-path "~/.emacs.d/")

(require 'auto-complete)
(global-auto-complete-mode t)
(require 'yasnippet)
(require 'linum)
(global-linum-mode)

(load-file "/usr/share/emacs/site-lisp/cedet/common/cedet.el")
(global-ede-mode t)
(semantic-load-enable-excessive-code-helpers)
(require 'semantic-gcc)
(require 'semantic-ia)
(require 'semanticdb)
(global-semanticdb-minor-mode 1)

;;
;; Erlang
;;
;; This is needed for Erlang mode setup
(setq load-path (cons  "/usr/lib/erlang/lib/tools-2.6.6.1/emacs"
      load-path))

(setq load-path (cons "/usr/local/Cellar/erlang/R13B04/lib/erlang/lib/tools-2.6.6.1/emacs/erlang-skels.el"
	load-path))

(add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))


(setq erlang-root-dir "/usr/lib/erlang")
(add-to-list 'exec-path "/usr/lib/erlang/bin")
(setq erlang-man-root-dir "/usr/lib/erlang/man")

(defun my-erlang-mode-hook ()
        ;; when starting an Erlang shell in Emacs, default in the node name
        (setq inferior-erlang-machine-options '("-sname" "emacs"))
        ;; add Erlang functions to an imenu menu
        (imenu-add-to-menubar "imenu")
        ;; customize keys
        (local-set-key [return] 'newline-and-indent)

        (set-frame-height (selected-frame) 20)
        )

(defun erl-shell (flags)
   "Start an erlang shell with flags"
   (interactive (list (read-string "Flags: ")))
   (set 'inferior-erlang-machine-options (split-string flags))
   (erlang-shell))

;; Some Erlang customizations
(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)
(require 'erlang-start)

(let ((distel-dir "/home/shk/.emacs.d/distel/elisp"))
  (unless (member distel-dir load-path)
    ;; Add distel-dir to the end of load-path
    (setq load-path (append load-path (list distel-dir)))))


(require 'distel)
(distel-setup)

(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-?"      erl-complete)
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind)
    ("\M-*"      erl-find-source-unwind)
    )
  "Additional keys to bind when in Erlang shell.")

(add-hook 'erlang-shell-mode-hook
	  (lambda ()
	    ;; add some Distel bindings to the Erlang shell
	    (dolist (spec distel-shell-keys)
	      (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key "\C-a" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle))
(add-hook 'c-mode-common-hook 'my-cedet-hook)

(defun my-c-mode-cedet-hook ()
 (local-set-key "." 'semantic-complete-self-insert)
 (local-set-key ">" 'semantic-complete-self-insert))
(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

;;
;; semantic includes
;;
(semantic-add-system-include "/usr/include" 'c-mode)

(add-hook 'scala-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

;;;;;;;;;
;; Tabbar

(require 'tabbar)

(global-set-key [C-s-tab] 'tabbar-backward-tab)
(global-set-key [C-tab] 'tabbar-forward-tab)

(global-set-key (kbd "C-x C-<right>") 'tabbar-forward-group)
(global-set-key (kbd "C-x C-<left>") 'tabbar-backward-group)

(set-face-attribute
'tabbar-default-face nil
:background "gray0")
(set-face-attribute
'tabbar-unselected-face nil
:background "gray5"
:foreground "white"
:box nil)
(set-face-attribute
'tabbar-selected-face nil
:background "grey100"
:foreground "black"
:box nil)
(set-face-attribute
'tabbar-button-face nil
:box '(:line-width 1 :color "gray100" :style released-button))
(set-face-attribute
'tabbar-separator-face nil
:height 0.6)

(tabbar-mode)
;; tabbar end
;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; windows navigation Emacs <S-up>, <S-down>, <S-left>, <S-right>.
;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key bindings
;;

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)


;; Save buffer
(global-set-key [f2] 'save-buffer)

;; Shell start
(global-set-key [f4] 'shell)

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
(global-set-key "\M-g" 'goto-line)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs customization
;;

;; Toolbar hide
(tool-bar-mode nil)

;; Scrollbar hide
(scroll-bar-mode nil)  

;; emacs select
(setq x-select-enable-clipboard t)

;; Emacs colors
(defun good-colors ()
  (progn
	 (set-background-color "grey46")
	 (set-foreground-color "White")
	 (set-cursor-color "Black")
	 (set-border-color "dark orange")
	 (set-mouse-color "dark orange")
	 
	 (set-face-background 'default "grey46")
	 (set-face-background 'region "Orange")
	 (set-face-background 'highlight "white")
	 (set-face-background 'modeline "black") ;;; CornflowerBlue")
	 
	 (set-face-foreground 'default "LightGray")
	 (set-face-foreground 'region "dark orange")
	 (set-face-foreground 'highlight "LightGray")  ;;; DimGray")
	 (set-face-foreground 'modeline "LightGray")
	 ))

;; calls the previously-defined function
(good-colors)

;; Emacs fullscreen at startup
(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
)

;; calls the toggle-fullscreen
(toggle-fullscreen)  

;;to display time
(display-time)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;cc-mode
;;
(require 'cc-mode)
(global-font-lock-mode 1)

(setq tab-width 4)
(define-key c-mode-map "\C-m" 'reindent-then-newline-and-indent)
(define-key c-mode-map "\C-ce" 'c-comment-edit)
(setq c-auto-hungry-initial-state 'none)
(setq c-delete-function 'backward-delete-char)
(setq c-tab-always-indent t)

;; compilation-window-height
(setq compilation-window-height 14)

(add-hook 'c-mode-hook        
	  '(lambda ( ) 
	     (c-set-style "k&r")))

;; Auto font lock mode
(defvar font-lock-auto-mode-list 
        (list 'c-mode 'c++-mode 'c++-c-mode 'emacs-lisp-mode 'lisp-mode 'perl-mode 'scheme-mode)
         "List of modes to always start in font-lock-mode")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUA mode
;;
(cua-mode t)
    (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
    (transient-mark-mode 1) ;; No region when it is not highlighted
    (setq cua-keep-region-after-copy t) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs encodig
;;
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "grey46" :foreground "LightGray" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 127 :width normal :foundry "microsoft" :family "Consolas")))))
