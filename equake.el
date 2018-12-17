;;; equake.el --- Quake-style drop-drop terminal using eshell.

;; Copyright (C) 2018 Benjamin Slade

;; Author: Benjamin Slade <slade@jnanam.net>
;; Maintainer: Benjamin Slade <slade@jnanam.net>
;; URL: TODO
;; Package-Version: 0.1
;; Version: 0.1
;; Created: 2018-12-12
;; Keywords: eshell, dropdown, terminal,

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; TODO

;; This library display remote user, remote host, python virtual
;; environment info, git branch, git dirty info and git unpushed
;; number for eshell prompt.

;; If you want to display the python virtual environment info, you
;; need to install `virtualenvwrapper' and `virtualenvwrapper.el'.
;; pip install virtualenvwrapper
;; M-x: package-install: virtualenvwrapper

;; Installation
;; TODO
;; It is recommended installed by the ELPA package system.
;; You could install it by M-x: with
;; package-install: eshell-prompt-extras.

;; Usage
;; TODO
;; before emacs24.4
;; (eval-after-load 'esh-opt
;;   (progn
;;     (autoload 'epe-theme-lambda "eshell-prompt-extras")
;;     (setq eshell-highlight-prompt nil
;;           eshell-prompt-function 'epe-theme-lambda)))
;;
;; If you want to display python virtual environment information:
;; (eval-after-load 'esh-opt
;;   (progn
;;     (require 'virtualenvwrapper)
;;     (venv-initialize-eshell)
;;     (autoload 'epe-theme-lambda "eshell-prompt-extras")
;;     (setq eshell-highlight-prompt nil
;;           eshell-prompt-function 'epe-theme-lambda)))

;; after emacs24.4
;; (with-eval-after-load "esh-opt"
;;   (autoload 'epe-theme-lambda "eshell-prompt-extras")
;;   (setq eshell-highlight-prompt nil
;;         eshell-prompt-function 'epe-theme-lambda))
;;
;; If you want to display python virtual environment information:
;; (with-eval-after-load "esh-opt"
;;   (require 'virtualenvwrapper)
;;   (venv-initialize-eshell)
;;   (autoload 'epe-theme-lambda "eshell-prompt-extras")
;;   (setq eshell-highlight-prompt nil
;;         eshell-prompt-function 'epe-theme-lambda))

;;; Code:
;; *EQUAKE* - eshell dropdown

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         _         ;;;
;;   ___  __ _ _   _  __ _| | _____  ;;;
;;  / _ \/ _` | | | |/ _` | |/ / _ \ ;;;
;; |  __/ (_| | |_| | (_| |   <  __/ ;;;
;;  \___|\__, |\__,_|\__,_|_|\_\___| ;;;
;;          |_|                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO:
;; 1. pull out hard-coded values and create defcustom's for them:
;;   (a) width & height percentages
;;   (b) foreground and background colours
;;   (c) shortcut keys
;; 2. Prevent last tab from being closed, or at least prompt.
;; 3. possibly improve invocation: currently two choices:
;;   (a) needs to have an emacsclient open already on the monitor in order to work
;;   (b) calls a transient frame on the relevant monitor, which launches equake;
;;       this has the draw back of being significantly slower, and sometimes
;;       brief artefacts can be seen (i.e. the transient frame).
;;   And (b) doesn't seem to work very well/consistently.
;; 4. Maybe do something to (optionally) silence the minibuffer?
;;    (setq inhibit-message t) doesn't seem to help
;;    But it's probably fine as is.
;; 5. Test on:
;;    (a) Wayland
;;    (b) MacOS
;;    (c) Windows
;;   Comments: In theory it should work on Mac & Windows, since frame.el.gz defines
;;             frame-types 'ns (=Next Step) and 'w32 (=Windows). Maybe even on
;;             Wayland via Xwayland?
;;   New comments: Doesn't really work on stumpwm, it seems. Trouble also on awesomewm.
;;               also trouble if monitor-attributes doesn't include name [as on TP X200]
;;               now falls back to using geometry field as name in that case, hopefully unique

(require 'cl-lib)
(require 'subr-x)

(setq equake/restore-mode-line mode-line-format)  ; store mode-line-format to be able to restore it

(setq equake/width-percentage 1.0)
(setq equake/height-percentage 0.4)
(setq equake/tab-list 'nil)

(setq equake/default-shell 'eshell)
(setq equake/default-sh-command "/bin/bash")

;; prevent accidental closure? - set for equake only
(defun ask-before-closing-equake ()
  "Make sure user really wants to close equake, ask again."
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to close the equake console frame? [Advice: Cancel and use `C-x k` to close the buffer instead.]"))
      (save-buffers-kill-terminal)
    ;; (save-buffers-kill-emacs)
    (message "Cancelling the closing of the equake console frame")))

(defun check-if-in-equake-frame-before-closing 
()
  "Check if we're in an equake frame."
  (interactive)
  (if (cl-search "*EQUAKE*[" (frame-parameter (selected-frame) 'name))
      (ask-before-closing-equake)
    (save-buffers-kill-terminal)))

(global-set-key (kbd "C-x C-c") 'check-if-in-equake-frame-before-closing)

(defun equake/key-bindings ()
  "Set tab movement bindings."
  (local-set-key (kbd "C-+") 'equake/new-tab)
  (local-set-key (kbd "C-M-+") 'equake/new-tab-different-shell)  
  (local-set-key (kbd "C-{") 'equake/prev-tab)
  (local-set-key (kbd "C-}") 'equake/next-tab)
  (local-set-key (kbd "C-M-{") 'equake/move-tab-left)
  (local-set-key (kbd "C-M-}") 'equake/move-tab-right)
  (local-set-key (kbd "C-|") 'equake/rename-etab)
  ;; (local-set-key (kbd "C-x C-c") 'ask-before-closing-equake)
  )

(add-hook 'eshell-mode-hook 'equake/key-bindings)
(add-hook 'term-mode-hook 'equake/key-bindings)
(add-hook 'shell-mode-hook 'equake/key-bindings)

(defun equake/equake-frame-p (monitor frames) ;;; TEST
  "Test if *EQUAKE* is an existing frame."
  (let ((frame (car frames)))
    (if frame
	(if (cl-search (concat "*EQUAKE*[" monitor "]") (frame-parameter frame 'name))
	    frame
	  (equake/equake-frame-p monitor (cdr frames))))))

(defun equake/orphan-tabs (monitor buffers)   ;;; TEST
  "Rename orphaned equake tabs."
  (let ((buff (car buffers)))
    (if buff
	  (if (string-match-p (concat "EQUAKE\\[" monitor) (buffer-name buff))   ; find and rename all stray EQUAKE tabs/buffers
	   (progn (switch-to-buffer buff)
		  (rename-buffer (concat "orphaned_etab==" (string-remove-prefix "EQUAKE" (buffer-name buff))) t)
		   (emacs-lock-mode -1) ; unlock buffer / allow kill
    		  (equake/orphan-tabs monitor (cdr buffers)))
	   (equake/orphan-tabs monitor (cdr buffers))))))

(defun equake/hide-orphaned-tab-frames (buffers)
  "Delete any stray frame associated with orphaned tabs."
  ;; now it may work *too* well...more testing required
  (interactive)
  (let ((buf (car buffers)))
    (if buf						    ; if buffer exists ...
 	(if (cl-search "orphaned_etab==" (buffer-name buf)) ; if it's named ...
	    (progn (delete-frame (window-frame (get-buffer-window buf))) ; delete all frames associated with all windows displaying the buffer
		   (equake/hide-orphaned-tab-frames (cdr buffers)))	 ; cycle on to next buffer in list
	  (equake/hide-orphaned-tab-frames (cdr buffers))))))		 ; likewise if no hit


(defun equake/remove-screen (monitorid tablist)
  "Remove stray screens from equake tab list."
  (let ((cur-tab (car tablist)))
    (if cur-tab
	(if (equal (car cur-tab) monitorid) ; if monitorid found in global equake-tablist
					;	   (remove (car cur-tab) equake/tab-list) ; remove matching list with monitor as car
	    (setq equake/tab-list (remove cur-tab equake/tab-list)) ; remove screen from equake-tab-list before proceeding
					; if monitorid not (yet) found in global equake-tablist
	  (equake/remove-screen monitorid (cdr tablist))))))

(defun equake/get-monitor-property (prop attributes)
  "Get a property of the current monitor/screen."
  (let ((field (car attributes)))
    (if field
	(if (equal (format "%s" (car field)) prop)
	    (cdr field)
	  (equake/get-monitor-property prop (cdr attributes))))))

(defun equake/get-monitor-name (attributes)
  "Get the name or another constant designator of the current monitor/screen."
  (let ((name (equake/get-monitor-property "name" (frame-monitor-attributes))))
    (if name
	name
      (let ((name (equake/get-monitor-property "geometry" (frame-monitor-attributes))))
	(if name
	    (format "%s" name)
	  (message "screen-id error!"))))))

(defun equake/get-monitor-width (attributes)
  "Get the width of the current monitor/screen."
  (let ((width (equake/get-monitor-property "geometry" (frame-monitor-attributes))))
    (car (cdr (cdr width)))))

(defun equake/get-monitor-height (attributes)
  "Get the height of the current monitor/screen."
  (let ((height (equake/get-monitor-property "geometry" (frame-monitor-attributes))))
    (car (cdr (cdr (cdr height))))))

(defun equake/get-monitor-xpos (attributes)
  "Get the x-position of the current monitor/screen."
  (let ((xpos (equake/get-monitor-property "geometry" (frame-monitor-attributes))))
    (car xpos)))

(defun equake/get-monitor-ypos (attributes)
  "Get the y-position of the current monitor/screen."
  (let ((ypos (equake/get-monitor-property "geometry" (frame-monitor-attributes))))
    (car (cdr ypos))))

(defun equake/emacs-dropdown-console ()
  "Set up an emacs drop-drop console. Run with \"emacsclient -c -e '(equake/emacs-dropdown-console)\"."
  (interactive)
  (let ((monitorid (equake/get-monitor-name (frame-monitor-attributes))))
    (if (equal (frame-parameter (selected-frame) 'name) "transientframe") ; for multi-monitor hack getting terminal on correct monitor (at least on KDE Plasma 5)
					; launch with: emacsclient -n -c -F '((name . "transientframe") (alpha . (0 . 0)) (width . (text-pixels . 0)) (height . (text-pixels . 0)))' -e '(equake/emacs-dropdown-console)'
	(progn 	       (setq inhibit-message t)
		       (delete-frame))) ; get rid of transient frame as soon as possible
    (let ((monwidth (equake/get-monitor-width (frame-monitor-attributes))) ; get monitor width
	  (monheight (equake/get-monitor-height (frame-monitor-attributes))) ; get monitor height
	  (mon-xpos (equake/get-monitor-xpos (frame-monitor-attributes))) ; get monitor relative x-position
	  (mon-ypos (equake/get-monitor-ypos (frame-monitor-attributes)))) ; get monitor relative y-position
      (if (equake/equake-frame-p monitorid (frame-list)) ; check if *EQUAKE* frame exists
	  (let ((frame-to-raise (equake/equake-frame-p monitorid (frame-list)))) ; if so, get frame id
	    (if (frame-visible-p frame-to-raise) ; then, if equake frame is already raised, make it invisible
		(progn (set-frame-parameter frame-to-raise 'fullscreen 'nil) ; un-fullscreen, in case it is fullscreened, so fullscreen doesn't get 'stuck'
		       (make-frame-invisible frame-to-raise) (make-frame-invisible frame-to-raise)) ; double-tap: one more makes 100% sure
	      (progn (raise-frame frame-to-raise) ; if equake frame is invisible, raise it
		     (select-frame frame-to-raise)) ; and select raised-frame
					;		     (setq inhibit-message t))	    ; no messages in buffer - not working
	      (set-frame-size (selected-frame) (truncate (* monwidth equake/width-percentage)) (truncate (* monheight equake/height-percentage)) t)	)) ; set size accordingly
					; OLD tdrop method: ;      (call-process "tdrop" nil 0 nil "current") ; if so, raise *EQUAKE* frame
					; if no monitor-relative *EQUAKE* frame exists, make a new frame, rename it, call startup function
	(progn (equake/orphan-tabs monitorid (buffer-list)) ; orphan any stray EQUAKE tabs/buffers before creating new frame
	       (equake/remove-screen monitorid equake/tab-list) ; remove stray orphaned tab frames
	       (select-frame (make-frame `((title . ,(concat "*EQUAKE*[" "]")))))
	       (set-frame-name (concat "*EQUAKE*[" monitorid "]")) ; set frame-name to *EQUAKE* + [monitor id]
	       (equake/launch-shell)				   ; launch new shell
	       (set-frame-size (selected-frame) (truncate (* monwidth equake/width-percentage)) (truncate (* monheight equake/height-percentage)) t) ; size again
	       (set-frame-position (selected-frame) mon-xpos mon-ypos) ; set position to top
  	       (equake/set-up-equake-frame)))))) ; execute start-up functions

(defun equake/launch-shell (&optional override)
  "Launch a new shell session."
  (interactive)
  (let ((launchshell equake/default-shell))
    (if override
	(setq launchshell override))
    (cond ((equal launchshell 'eshell)
	   (eshell 'N))
	  ((equal launchshell 'ansi-term)
	   (ansi-term equake/default-sh-command))
	  ((equal launchshell 'term)
	   (term equake/default-sh-command))
	  ((equal launchshell 'shell)
	   (shell)
	   (delete-other-windows)))))

(defun equake/set-up-equake-frame ()
  "Set-up new *EQUAKE* frame, including cosmetic changes."
  (interactive)
  (let ((monitorid (equake/get-monitor-name (frame-monitor-attributes)))) ; get current screen name
					; OLD tdrop method: ;    (call-process "tdrop" nil 0 nil "-m -y 15 -e '(equake/emacs-dropdown-console)'") ; invoke tdrop on emacsclient
					;    (eshell 'N)                         ; create new eshell sessions
    (set-background-color "#000022")    ; set background colour
    (set-foreground-color "#eeeeee")	; set foreground colour
    (rename-buffer (concat "EQUAKE[" monitorid "]0%") )	     ; set buffer/tab-name
    (set-frame-parameter (selected-frame) 'menu-bar-lines 0) ; no menu-bars      
    (setq equake/tab-list (append equake/tab-list (list (cons monitorid (list 0))))) ; set equake local tab-list to an initial singleton list
    (set-frame-size (selected-frame) (truncate (* monwidth equake/width-percentage)) (truncate (* monheight equake/height-percentage)) t)
    (setq inhibit-message t)			     ; no messages in buffer
    (equake/hide-orphaned-tab-frames (buffer-list))) ; hide any stray orphaned tab frames
  )

(defun equake/count-tabs (monitor buffers count)
  "Count current equake tabs on monitor."
  (let ((buffbeg (car buffers))
	(buffend (cdr buffers)))
    (cond ((equal buffbeg 'nil)
	   count)
	  ((string-match-p (concat "EQUAKE\\[" monitor) (buffer-name buffbeg))
	   (progn (setq count (+ 1 count))
		  (equake/count-tabs monitor buffend count)))
	  ((not (string-match-p (concat "EQUAKE\\[" monitor) (buffer-name buffbeg)))
	   (equake/count-tabs monitor buffend count)))))

;; MAKE OWN FUNCTION?
;; (string-to-number (replace-regexp-in-string "%[[:alnum:]]*" "" (string-remove-prefix (concat "EQUAKE[" monitor "]") (buffer-name buffbeg))))

(defun equake/highest-etab (monitor buffers highest)
  "Get highest etab number on monitor."
  (let ((buffbeg (car buffers))
	(buffend (cdr buffers)))
    (cond ((equal buffbeg 'nil)
	   highest)
	  ((string-match-p (concat "EQUAKE\\[" monitor) (buffer-name buffbeg)) ; only consider relevant buffers 
	   (progn (if  (> (string-to-number (replace-regexp-in-string "%[[:alnum:]]*" "" (string-remove-prefix (concat "EQUAKE[" monitor "]") (buffer-name buffbeg)))) highest)
		      (setq highest (string-to-number (replace-regexp-in-string "%[[:alnum:]]*" "" (string-remove-prefix (concat "EQUAKE[" monitor "]") (buffer-name buffbeg))))))
		  (equake/highest-etab monitor buffend highest)))
	  ((not (string-match-p (concat "EQUAKE\\[" monitor) (buffer-name buffbeg)))
	   (equake/highest-etab monitor buffend highest)))))

(defun equake/new-tab-different-shell ()
  "Open a new shell tab, but using a shell different from the default."
  (interactive)
  (let ((shells '("eshell" "ansi-term" "term" "shell")))
    (equake/new-tab (intern (message "%s" (ido-completing-read "Choose shell:" shells )))))
  )

(defun equake/new-tab (&optional override)
  "Open a new shell tab on monitor."
  (interactive)
  (if override
      (equake/launch-shell override) 	; launch with specified shell if set
    (equake/launch-shell)) 		; otherwise, launch shell normally
  (set-background-color "#000022")
  (set-foreground-color "#eeeeee")			   ; set foreground colour
  (set-frame-parameter (selected-frame) 'menu-bar-lines 0) ; no menu-bars  
  (setq inhibit-message t)
  (let ((monitor  (equake/get-monitor-name (frame-monitor-attributes))))
    (let ((newhighest (+ 1 (equake/highest-etab monitor (buffer-list) 0))) ; figure out number to be set for the new tab for the current monitor
	  (cur-monitor-tab-list (equake/find-monitor-list monitor equake/tab-list))) ; find the tab-list associated with the current monitor
      (rename-buffer (concat "EQUAKE[" monitor "]" (number-to-string newhighest) "%")) ; rename buffer with monitor id and new tab number
					;    (set-frame-parameter (selected-frame) 'menu-bar-lines 0)  ; cosmetic changes
					;    (modify-frame-parameters (selected-frame) '((vertical-scroll-bars . nil) (horizontal-scroll-bars . nil)))
      (setq equake/tab-list (remove cur-monitor-tab-list equake/tab-list)) ; remove old monitor tab-list from global equake tab list
      (setq cur-monitor-tab-list (cons (car cur-monitor-tab-list) (append (cdr cur-monitor-tab-list) (list newhighest)))) ; pull into car (=monitor name) and cdr (=tab list); append newhighest to tab list and then cons monitor name and tab list back together
      (setq equake/tab-list (append equake/tab-list (list cur-monitor-tab-list)))
      (if equake/show-monitor-in-mode-line ; show monitorid or not
	  (setq mode-line-format (list (equake/mode-line (concat monitor ": ") (equake/find-monitor-list monitor equake/tab-list))))
	(setq mode-line-format (list (equake/mode-line "" (equake/find-monitor-list monitor equake/tab-list))))))))

(defun equake/find-monitor-list (monitor tabs)
  "Return the relevant list member associated with monitor/screen."
  (if (not (equal tabs 'nil))
      (if (equal monitor (car (car tabs)))
	  (car tabs)
	(equake/find-monitor-list monitor (cdr tabs)))))

(setq equake/show-monitor-in-mode-line t) ; whether or not to prepend monitor id to mode-line before tabs

(defun equake/shell-after-buffer-change-hook ()
  "Things to do when in Equake when the buffer changes."
  (let ((monitorid (equake/get-monitor-name (frame-monitor-attributes))))
    (if (cl-search "EQUAKE[" (buffer-name (current-buffer)))
	(progn ; get monitor-local list of buffers and send it to be processed for the mode-line
	  (if equake/show-monitor-in-mode-line ; show monitorid or not
	      (setq mode-line-format (list (equake/mode-line (concat monitorid ": ") (equake/find-monitor-list monitorid equake/tab-list))))
	    	      (setq mode-line-format (list (equake/mode-line "" (equake/find-monitor-list monitorid equake/tab-list)))))
	  (force-mode-line-update)
      ;;  (set-frame-parameter (selected-frame) 'menu-bar-lines 0) ; no menu-bars
	  (modify-frame-parameters (selected-frame) '((vertical-scroll-bars . nil) (horizontal-scroll-bars . nil)))) ; no scrollbars
      (progn (setq inhibit-message 'nil)
	     ;; (set-frame-parameter (selected-frame) 'menu-bar-lines 1)
	     (setq mode-line-format equake/restore-mode-line)))))  ; restore 'real' mode-line to non-EQUAKE frames

(add-hook 'buffer-list-update-hook 'equake/shell-after-buffer-change-hook)

(defun equake/kill-etab-buffer-hook ()
  "Things to do when an Equake buffer is killed."  ; TODO: prevent last equake tab from being killed!
  (if (string-match-p "EQUAKE\\[" (buffer-name))
      (let ((monitor (substring (buffer-name) (+ 1 (search "[" (buffer-name))) (search "]" (buffer-name))))
	    (killed-tab (string-to-number (substring (buffer-name) (+ 1 (search "]" (buffer-name))) (length (buffer-name))))))
	(let ((cur-monitor-tab-list (equake/find-monitor-list monitor equake/tab-list)))
	  (setq equake/tab-list (remove cur-monitor-tab-list equake/tab-list)) ; remove old monitor tab-list member from global tab list
	  (setq cur-monitor-tab-list (cons (car cur-monitor-tab-list) (remove killed-tab (cdr cur-monitor-tab-list)))) ; edit current monitor tab list to remove tab
	  (setq equake/tab-list (append equake/tab-list (list cur-monitor-tab-list))) ; add edited current monitor tab list back to global tab list
	  (setq mode-line-format (list (equake/mode-line "" cur-monitor-tab-list)))))))

(add-hook 'kill-buffer-hook 'equake/kill-etab-buffer-hook)

(defun equake/find-next-etab (tablist tab)
  "Return the next etab."
  (cond ((equal (car tablist) 'nil) ; return 'nil if we're at the end of the list
	 'nil)
	((equal (car tablist) tab) ; if we find the current tab, cdr and then car the list to get the next tab
	 (car (cdr tablist)))
	((not (equal (car tablist) tab)) ; if not, then cdr the list and test again
	 (equake/find-next-etab (cdr tablist) tab))))

(defun equake/move-tab (monitor tablist moving-tab direction)
  "Move the current tab one position to the right (direction=1) or left (direction=-1) in the list."
  (let ((examined-tab (car tablist)))	; get entire local monitor tab-list
    (let ((monitor-tab-list (cdr examined-tab))) ; just the local monitor list of tabs
      (if tablist
	  (if (equal (car examined-tab) monitor) ; if the monitor label matches
	      (progn (if (< (length monitor-tab-list) 2) ; if only one tab, stop with message to user
			 (message "Only one tab")
		       (progn (let ((orig-pos (cl-position moving-tab monitor-tab-list))) ; find the original position of tab we're moving
				(let ((target-pos (+ orig-pos direction)) ; find the target position of the moving tab
				      (reconstructed-local-tab 'nil)) ; initialise local variable 
				  (cond ((< target-pos 0) ; if trying to move tab beyond left edge
					 (progn (setq monitor-tab-list (remove moving-tab monitor-tab-list)) ; delete tab 
						(setq monitor-tab-list (append monitor-tab-list (list moving-tab))))) ; add tab to right edge
					((> target-pos (- (length monitor-tab-list) 1))
					 (progn (setq monitor-tab-list (remove moving-tab monitor-tab-list)) ; delete tab
						(setq monitor-tab-list (append (list moving-tab) monitor-tab-list)))) ; add tab to left edge
					(t (progn (let ((target-temp-id (nth target-pos monitor-tab-list))) ; store the original content of target position
						    (setf (nth target-pos monitor-tab-list) moving-tab (nth orig-pos monitor-tab-list) target-temp-id ))))) ; modify the list positions accordingly)))
				  (setq reconstructed-local-tab (list (cons monitor monitor-tab-list))) ; cons monitor name with modified tab-list
				  (setq equake/tab-list (remove examined-tab equake/tab-list)) ; remove the entire local monitor tab-list from global etab-list
				  (setq equake/tab-list (append equake/tab-list reconstructed-local-tab)) ; append the named, modified monitor tab list to the global equake tab list
				  (if equake/show-monitor-in-mode-line ; show monitorid or not
				      (setq mode-line-format (list (equake/mode-line (concat monitorid ": ") (equake/find-monitor-list monitorid equake/tab-list))))
	    			    (setq mode-line-format (list (equake/mode-line "" (equake/find-monitor-list monitorid equake/tab-list)))))
	;			  (setq mode-line-format (list (equake/mode-line "" (equake/find-monitor-list monitor equake/tab-list)))) ; update mode-line
				  (force-mode-line-update)))))) ; force refresh mode-line
	    (equake/move-tab monitor (cdr tablist) moving-tab direction)) ; check next monitor-local tab-list
	(message (concat "error: no relevant monitor " monitor " in ")))))) ; error if for some reason no such-name monitor

(defun equake/move-tab-right ()
  "Move current tab one position to the right."
  (interactive)
  (let ((monitorid (equake/get-monitor-name (frame-monitor-attributes))))
    (let ((current-etab (string-to-number (replace-regexp-in-string "%[[:alnum:]]*" "" (string-remove-prefix (concat "EQUAKE[" monitorid "]") (buffer-name (current-buffer))))))) ; get local tab number
    (equake/move-tab monitorid equake/tab-list current-etab 1)))) ; call general tab move function

(defun equake/move-tab-left ()
  "Move current tab one position to the left."
  (interactive)
(let ((monitorid (equake/get-monitor-name (frame-monitor-attributes))))
  (let ((current-etab (string-to-number (replace-regexp-in-string "%[[:alnum:]]*" "" (string-remove-prefix (concat "EQUAKE[" monitorid "]") (buffer-name (current-buffer))))))) ; get local tab number
    (equake/move-tab monitorid equake/tab-list current-etab -1)))) ; call general tab move function

(defun equake/next-tab ()
  "Switch to the next tab."
  (interactive)
  (let ((monitorid (equake/get-monitor-name (frame-monitor-attributes))))
    (if (< (equake/count-tabs monitorid (buffer-list) 0) 2)
	(print "No other tab to switch to.")
      (progn
	(let  ((current-tab (string-to-number (substring (buffer-name) (+ 1 (search "]" (buffer-name))) (+ 1 (search "%" (buffer-name)))))))
	  (let ((next-tab (equake/find-next-etab (cdr (equake/find-monitor-list monitorid equake/tab-list)) current-tab)))
	    (if (equal next-tab 'nil)	; switch to first tab if at end of list
		(switch-to-buffer (equake/find-buffer-by-monitor-and-tabnumber monitorid (car (cdr (equake/find-monitor-list monitorid equake/tab-list))) (buffer-list)))
	      (switch-to-buffer (equake/find-buffer-by-monitor-and-tabnumber monitorid next-tab (buffer-list))))))))))

(defun equake/prev-tab ()
  "Switch to the previous tab."
  (interactive)
  (let ((monitorid (equake/get-monitor-name (frame-monitor-attributes))))
    (if (< (equake/count-tabs monitorid (buffer-list) 0) 2)
	(print "No other tab to switch to.")
      (progn	     ; re-use equake/find-next-tab function, first reversing the list
	(let ((current-tab (string-to-number (substring (buffer-name) (+ 1 (search "]" (buffer-name))) (+ 1 (search "%" (buffer-name)))))))
	  (let ((prev-tab (equake/find-next-etab (reverse (cdr (equake/find-monitor-list monitorid equake/tab-list))) current-tab)))
	    (if (equal prev-tab 'nil)	; switch to last tab if at beginning of list
	      	(switch-to-buffer (equake/find-buffer-by-monitor-and-tabnumber monitorid (car  (reverse (cdr (equake/find-monitor-list monitorid equake/tab-list)))) (buffer-list)))
	      (switch-to-buffer (equake/find-buffer-by-monitor-and-tabnumber monitorid prev-tab (buffer-list))))))))))


(defun equake/find-buffer-by-monitor-and-tabnumber (monitor tabnum buffers)
"Find the name of an equake buffer given a monitor/screen name and tab number."
 (let ((buffbeg (car buffers))
       (buffend (cdr buffers))
       (name-skeleton (concat "EQUAKE\\[" monitor "\\]" (number-to-string tabnum) "%"))) ; buffer template matching everything before %+following characters
   (cond ((equal buffbeg 'nil)				    ; if we're out of buffers
	  message "Error! No such screen-tag pair exists!")    ; in case of trouble, please panic
	 ((string-match-p name-skeleton (buffer-name buffbeg)) ; if buffer name matches matches skeleton, i.e. everything before %+following characters
	  buffbeg)					       ; then return that buffer
	 (t (equake/find-buffer-by-monitor-and-tabnumber monitor tabnum buffend))))) ; go on to next buffer

(defun equake/rename-etab ()
"Rename current equake tab."
(interactive)
(let ((buffer-prefix (replace-regexp-in-string "%\.*" "" (buffer-name (current-buffer))))) ; get everything before the '%' and any characters that follow it
  (let ((newname (read-string "Enter a new tab name: ")))
    ;; (message "New name is %s" newname)
    (rename-buffer (concat buffer-prefix "%" newname)))))
    

(defun equake/mode-line (modelinestring buffers)
  "Content of mode-line for equake (show tabs)."
  (let ((curtab (car (cdr buffers))))
    (if (equal curtab 'nil)
	(list modelinestring (concat "((" (format "%s" major-mode) ")) "))
      (progn (setq modelinestring (concat modelinestring (equake/extract-format-tab-name curtab))) ; get name/number for tab in mode-line format
	     (equake/mode-line modelinestring (cdr buffers)))))) ; go on to next tab

(defun equake/extract-format-tab-name  (tab)
  "Extract equake tab name and format it for the modeline."
  (let ((monitor (equake/get-monitor-name (frame-monitor-attributes))))
    (let ((current-etab  (string-to-number (string-remove-prefix (concat "EQUAKE[" monitor "]") (buffer-name (current-buffer)))))) ; get the tab number of the current buffer
      (let ((etab-name (string-remove-prefix (concat "EQUAKE[" monitor "]" (number-to-string tab) "%") (buffer-name (equake/find-buffer-by-monitor-and-tabnumber monitor tab (buffer-list)))))) ; find the name of the tab
	(if (equal etab-name "") 	; if the name is null string
	    (setq etab-name (number-to-string tab))) ; set name to tab number
	(if (equal tab current-etab)		     
	    (concat "[*" etab-name "*] ") ; 'highlight' current tab
	  (concat "[" etab-name "] "))))))

(provide 'equake)

;;; equake.el ends here
