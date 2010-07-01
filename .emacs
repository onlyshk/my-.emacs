;;
;; Emacs Config 
;;

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

;; Spell
(global-set-key [f5] 'ispell)

;; Prev error
(global-set-key [f11] 'next-error)

;; Next error
(global-set-key [f10] 'previous-error)

;; Compile
(global-set-key [(f9)] 'compile)

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

;; scrollbar color
;;(scroll-bar-mode (background "Dark slate gray"))

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
