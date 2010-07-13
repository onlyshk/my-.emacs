;;
;; Emacs Config 
;;

;; seems sensible for this to be the first line..
(add-to-list 'load-path "~/.emacs.d/")

(load-file "/home/shk/emacs-23.2/lisp/cedet/cedet.el")

(require 'auto-complete)
(global-auto-complete-mode t)
(require 'yasnippet)
;;(yas/initialize)
;;(yas/load-directory "/home/shk/.emacs.d/snippets")

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
(global-set-key [(control tab)] 'other-window)

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

;; Emacs scroll-bar
(setq scroll-bar-mode-explicit t) 
(set-scroll-bar-mode `right)

;;emacs font
;;(set-default-font "-*-consolas-*-16-*-*-*-*-*-*-cp1251")

;; emacs select
(setq x-select-enable-clipboard t)

;; Emacs colors
(defun good-colors ()
  (progn
	 (set-background-color "DimGray")
	 (set-foreground-color "White")
	 (set-cursor-color "Black")
	 (set-border-color "DimGray")
	 (set-mouse-color "Black")
	 
	 (set-face-background 'default "DimGray")
	 (set-face-background 'region "DarkSlateGray")
	 (set-face-background 'highlight "DarkSlateBlue")
	 (set-face-background 'modeline "Black") ;;; CornflowerBlue")
	 
	 (set-face-foreground 'default "LightGray")
	 (set-face-foreground 'region "Ivory")
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
(setq compilation-window-height 8)

;;
;; Hide compilation window if errors no
;; or <Ctrl>+x if errors detected
;;
(setq compilation-finish-function
      (lambda (buf str)

        (if (string-match "exited abnormally" str)

            ;;there were errors
            (message "compilation errors, press C-x ` to visit")

          ;;no errors, make the compilation window go away in 0.5 seconds
          (run-at-time 0.5 nil 'delete-windows-on buf)
          (message "NO COMPILATION ERRORS!"))))

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


