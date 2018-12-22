;;; equake.el --- Quake-style drop-drop console

;; *EQUAKE* - emacs shell dropdown console

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         _         ;;;
;;   ___  __ _ _   _  __ _| | _____  ;;;
;;  / _ \/ _` | | | |/ _` | |/ / _ \ ;;;
;; |  __/ (_| | |_| | (_| |   <  __/ ;;;
;;  \___|\__, |\__,_|\__,_|_|\_\___| ;;;
;;          |_|                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Copyright (C) 2018 Benjamin Slade

;; Author: Benjamin Slade <slade@jnanam.net>
;; Maintainer: Benjamin Slade <slade@jnanam.net>
;; URL: https://gitlab.com/emacsomancer/equake
;; Package-Version: 0.3
;; Version: 0.3
;; Package-Requires: ((emacs "24.4"))
;; Created: 2018-12-12
;; Keywords: frames, terminals, tools

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

;; This package is designed to recreate a drop-down console fully
;; within Emacs, ideally using eshell. It has multi-tab functionality,
;; and the tabs can be moved and renamed. Different shells can be
;; opened and used in different tabs. It is designed to be sensitive
;; to which screen/monitor it is opened on and to maintain separate
;; tabs for separate screens (though this is still a bit beta-y).

;; Installation
;; Right now, clone the git repo somewhere and put it in your
;; load-path, e.g., add something like this to your init.el:
;; (add-to-list 'load-path
;;             "~/.emacs.d/equake/")
;;  (require 'equake)

;; Usage
;; Run with "emacsclient -e '(equake-emacs-dropdown-console)'",
;; after launched an Emacs daemon of course.
;; I recommend binding this command to a key like F12 in your DE/WM.
;; Executing this command will create a new equake console
;; on your screen the first time, and subsequently toggle
;; the console (i.e. hide or show it).
;;
;; It works with eshell, ansi-term, term, shell. But it was
;; really designed to work with eshell, which is the default.
;; New console tabs can be specified to open with a shell
;; other than the default shell.
;;
;; Equake is designed to work with multi-screen setups,
;; with a different set of tabs for each screen.
;;
;; You'll probably also want to configure your WM/DE to
;; ignore the window in the task manager etc. and
;; have no titlebar or frame

;; In KDE Plasma 5:
;; systemsettings > Window Management > Window Rules:
;; Button: New
;; 
;; In Window matching tab:
;; Description: equake rules
;; Window types: Normal Window
;; Window title: Substring Match : *EQUAKE*
;;
;; In Arrangement & Access tab:
;; Check: 'Keep above' - Force - Yes
;; Check: 'Skip taskbar' - Force - Yes
;; Check: 'Skip switcher' - Force - Yes
;;
;; In Appearance & Fixes tab:
;; Check: 'No titlebar and frame' - Force - Yes
;; 
;; In awesomewm, probably adding to your 'Rules' something
;; like this:
;; 
;; { rule = { instance = "*EQUAKE*", class = "Emacs" },      
;;    properties = { titlebars_enabled = false } },
;; 
;; In stumpwm, I'm not sure: it doesn't seem to respect
;; Emacs frame settings.

;; TODO
;; 1. defcustoms:
;;   (a) for keybindings
;;   (b) make shell choice into actual list
;; 2. Prevent last tab from being closed, or at least prompt.
;; 3. possibly improve invocation: a bit slow, and still inconsistent on multi-monitor
;; 4. Maybe do something to (optionally) silence the minibuffer?
;;    (setq inhibit-message t) doesn't seem to help
;;    But it's probably fine as is.
;; 5. Saving layouts/tabs? 
;; 6. Test on:
;;    (a) Wayland -- seems to work ok on Gnome Shell Wayland
;;    (b) MacOS -- ??
;;    (c) Windows -- ??
;;   Comments: In theory it should work on Mac & Windows, since frame.el defines
;;             frame-types 'ns (=Next Step) and 'w32 (=Windows). Maybe even on
;;             Wayland via Xwayland? Yep, seems to work fine via Xwayland
;;   New comments: Doesn't really work on stumpwm, it seems. Trouble also on awesomewm:
;;                   awesomewm erros:
;; [   340.103667 ] error    3 BadWindow    request    2 minor    0 serial  20360: "BadWindow (invalid Window parameter)"
;; [   340.103720 ] error    3 BadWindow    request    2 minor    0 serial  20360: "BadWindow (invalid Window parameter)"
;; [   340.103741 ] error    3 BadWindow    request  129 minor    6 serial  20360: "BadWindow (invalid Window parameter)"
;; [   340.119921 ] error    3 BadWindow    request    2 minor    0 serial  20365: "BadWindow (invalid Window parameter)"
;; [   340.119961 ] error    3 BadWindow    request    2 minor    0 serial  20365: "BadWindow (invalid Window parameter)"
;; [   340.119986 ] error    3 BadWindow    request  129 minor    6 serial  20365: "BadWindow (invalid Window parameter)"
;; [   340.202927 ] error  152 XCB_DAMAGE_BAD_DAMAGE request  143 minor    2 serial  20375: "152"
;; [   340.236201 ] error  152 XCB_DAMAGE_BAD_DAMAGE request  143 minor    2 serial  20407: "152"
;; 
;;   Other issues:  also trouble if monitor-attributes doesn't include name [as on TP X200]
;;       changed code so it now falls back to using geometry field as name in that case, hopefully unique
;;
;;; Code


(require 'cl-lib)
(require 'subr-x)

(defvar equake-restore-mode-line mode-line-format)  ; store mode-line-format to be able to restore it

(defvar equake-tab-list 'nil)           ; empty list of equake tabs to start

(defgroup equake ()
  "Equake - Emacs drop-down console."
  :group 'shell)

(defcustom equake-width-percentage 1.0
  "Percentage (.01-1.0) for width of equake console."
  :type 'float
  :group 'equake)

(defcustom equake-height-percentage 0.4
  "Percentage (.01-1.0) for height of equake console."
  :type 'float
  :group 'equake)

(defcustom equake-active-opacity 75
  "Amount of opacity of equake console when active."
  :type 'integer
  :group 'equake)

(defcustom equake-inactive-opacity 60
  "Amount of opacity of equake console when inactive."
  :type 'integer
  :group 'equake)

(defcustom equake-default-shell 'eshell
  "Default shell used by equake: choice are 'eshell, 'ansi-term, 'term, 'shell."
  :type  'const
  :group 'equake)

(defcustom equake-default-sh-command "/bin/bash"
  "Default shell command used by (ansi-)term."
  :type 'string
  :group 'equake)

(defcustom equake-unnamed-monitor 'nil
  "Set this to t if your monitor/screen lacks a 'name' property."
  :type 'boolean
  :group 'equake)               ; set this to t if your monitor/screen doesn't provide a name attribute (may cause issues otherwise)

(defcustom equake-show-monitor-in-mode-line 'nil
  "Toggle to show monitor id string as part of equake mode-line."
  :type 'boolean
  :group 'equake) ; whether or not to prepend monitor id to mode-line before tabs

(defcustom equake-console-foreground-colour "#eeeeee"
  "Background colour of equake console."
  :type 'string
  :group 'equake)

(defcustom equake-console-background-colour "#000022"
  "Background colour of equake console."
  :type 'string
  :group 'equake)

(defcustom equake-inactive-tab-foreground-colour "black"
  "Colour of foreground text of inactive equake tabs."
  :type 'string
  :group 'equake)

(defcustom equake-inactive-tab-background-colour "lightblue"
  "Background colour of inactive equake tabs."
  :type 'string
  :group 'equake)

(defcustom equake-active-tab-foreground-colour "lightblue"
  "Colour of foreground text of active equake tabs."
  :type 'string
  :group 'equake)

(defcustom equake-active-tab-background-colour "black"
  "Background colour of active equake tabs."
  :type 'string
  :group 'equake)

(defcustom equake-shell-type-eshell-background "midnight blue"
  "Background colour of shell-type indicator in mode-line when eshell is the underlying shell."
  :type 'string
  :group 'equake)

(defcustom equake-shell-type-term-background "midnight blue"
  "Background colour of shell-type indicator in mode-line when (ansi-)term is the underlying shell."
  :type 'string
  :group 'equake)

(defcustom equake-shell-type-shell-background "midnight blue"
  "Background colour of shell-type indicator in mode-line when shell) is the underlying shell."
  :type 'string
  :group 'equake)

(defcustom equake-shell-type-eshell-foreground "spring green"
  "Background colour of shell-type indicator in mode-line when eshell is the underlying shell."
  :type 'string
  :group 'equake)

(defcustom equake-shell-type-term-foreground "gold"
  "Background colour of shell-type indicator in mode-line when (ansi-)term is the underlying shell."
  :type 'string
  :group 'equake)

(defcustom equake-shell-type-shell-foreground "magenta"
  "Background colour of shell-type indicator in mode-line when shell) is the underlying shell."
  :type 'string
  :group 'equake)

;; prevent accidental closure? - set for equake only
(defun equake-ask-before-closing-equake ()
  "Make sure user really wants to close equake, ask again."
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to close the equake console frame? [Advice: Cancel and use `C-x k` to close the buffer instead, returning to your shell session.]"))
      (save-buffers-kill-terminal)
    ;; (save-buffers-kill-emacs)
    (message "Wisely cancelling the closing of the equake console frame...")))

(defun equake-check-if-in-equake-frame-before-closing ()
  "Check if we're in an equake frame."
    (interactive)
    (if (cl-search "*EQUAKE*[" (frame-parameter (selected-frame) 'name))
      (equake-ask-before-closing-equake)
    (save-buffers-kill-terminal)))

(global-set-key (kbd "C-x C-c") 'equake-check-if-in-equake-frame-before-closing)

(defun equake-key-bindings ()
  "Set tab movement bindings."
  (local-set-key (kbd "C-+") 'equake-new-tab)
  (local-set-key (kbd "C-M-+") 'equake-new-tab-different-shell)  
  (local-set-key (kbd "C-{") 'equake-prev-tab)
  (local-set-key (kbd "C-}") 'equake-next-tab)
  (local-set-key (kbd "C-M-{") 'equake-move-tab-left)
  (local-set-key (kbd "C-M-}") 'equake-move-tab-right)
  (local-set-key (kbd "C-|") 'equake-rename-etab)
  ;; (local-set-key (kbd "C-x C-c") 'equake-ask-before-closing-equake)
  )

;; TODO:
;; probably should make equake its own minor mode and tie the bindings to that
(add-hook 'eshell-mode-hook 'equake-key-bindings)
(add-hook 'term-mode-hook 'equake-key-bindings)
(add-hook 'shell-mode-hook 'equake-key-bindings)

(defun equake-equake-frame-p (monitor frames) ;;; TEST
  "Test if *EQUAKE* is an existing frame."
  (let ((frame (car frames)))
    (if frame
        (if (equal (concat "*EQUAKE*[" monitor "]") (frame-parameter frame 'name))
            frame
          (equake-equake-frame-p monitor (cdr frames))))))

(defun equake-orphan-tabs (monitor buffers)   ;;; TEST
  "Rename orphaned equake tabs."
  (let ((buff (car buffers)))
    (if buff
          (if (string-match-p (concat "EQUAKE\\[" monitor) (buffer-name buff))   ; find and rename all stray EQUAKE tabs/buffers
           (progn (switch-to-buffer buff)
                  (rename-buffer (concat "orphaned_etab==" (string-remove-prefix "EQUAKE" (buffer-name buff))) t)
                   (emacs-lock-mode -1) ; unlock buffer / allow kill
                  (equake-orphan-tabs monitor (cdr buffers)))
           (equake-orphan-tabs monitor (cdr buffers))))))

(defun equake-hide-orphaned-tab-frames (buffers)
  "Delete any stray frame associated with orphaned tabs."
  ;; now it may work *too* well...more testing required
  (interactive)
  (let ((buf (car buffers)))
    (if buf                                                 ; if buffer exists ...
        (if (cl-search "orphaned_etab==" (buffer-name buf)) ; if it's named ...
            (progn (delete-frame (window-frame (get-buffer-window buf))) ; delete all frames associated with all windows displaying the buffer
                   (equake-hide-orphaned-tab-frames (cdr buffers)))      ; cycle on to next buffer in list
          (equake-hide-orphaned-tab-frames (cdr buffers))))))            ; likewise if no hit


(defun equake-remove-screen (monitorid tablist)
  "Remove stray screens from equake tab list."
  (let ((cur-tab (car tablist)))
    (if cur-tab
        (if (equal (car cur-tab) monitorid) ; if monitorid found in global equake-tablist
                                        ;          (remove (car cur-tab) equake-tab-list) ; remove matching list with monitor as car
            (setq equake-tab-list (remove cur-tab equake-tab-list)) ; remove screen from equake-tab-list before proceeding
                                        ; if monitorid not (yet) found in global equake-tablist
          (equake-remove-screen monitorid (cdr tablist))))))

(defun equake-get-monitor-property (prop attributes)
  "Get a property of the current monitor/screen."
  (let ((field (car attributes)))
    (if field
        (if (equal (format "%s" (car field)) prop)
            (cdr field)
          (equake-get-monitor-property prop (cdr attributes))))))


(defun equake-get-monitor-name (attributes)
  "Get the name or another constant designator of the current monitor/screen."
  (let ((name (equake-get-monitor-property "name" (frame-monitor-attributes))))
    (if name
        name
      (if equake-unnamed-monitor
          (let ((name (equake-get-monitor-property "geometry" (frame-monitor-attributes))))
            (if name
                (format "%s" name))))
              ;; (message "screen-id error! %s" name)    ; seems to occur when mouse is on one desktop but focus is on the other
              )))

(defun equake-get-monitor-width (attributes)
  "Get the width of the current monitor/screen."
  (let ((width (equake-get-monitor-property "geometry" (frame-monitor-attributes))))
    (car (cdr (cdr width)))))

(defun equake-get-monitor-height (attributes)
  "Get the height of the current monitor/screen."
  (let ((height (equake-get-monitor-property "geometry" (frame-monitor-attributes))))
    (car (cdr (cdr (cdr height))))))

(defun equake-get-monitor-xpos (attributes)
  "Get the x-position of the current monitor/screen."
  (let ((xpos (equake-get-monitor-property "geometry" (frame-monitor-attributes))))
    (car xpos)))

(defun equake-get-monitor-ypos (attributes)
  "Get the y-position of the current monitor/screen."
  (let ((ypos (equake-get-monitor-property "geometry" (frame-monitor-attributes))))
    (car (cdr ypos))))

(defun equake-kill-stray-transient-frames (frames)
  "Destroy any stray transient frames."
  (interactive)
  (let ((frame (car frames)))
    (if frame
        (progn (if (cl-search "transientframe" (frame-parameter frame 'name))
                   (delete-frame frame))
               (equake-kill-stray-transient-frames (cdr frames))))))

(defun equake-emacs-dropdown-console ()
  "Set up an emacs drop-drop console. Run with \"emacsclient -e '(equake-emacs-dropdown-console)'\"."
  (interactive)
  (let ((mon-xpos (equake-get-monitor-xpos (frame-monitor-attributes))) ; get monitor relative x- & y-positions
        (mon-ypos (equake-get-monitor-ypos (frame-monitor-attributes))))
  (select-frame (make-frame `('(name . "transientframe") (alpha . (01 . 01)) (width . (text-pixels . 0)) (height . (text-pixels . 0)))))
  (set-frame-position (selected-frame) mon-xpos mon-ypos)) ; set to upper left-hand corner of current(!) monitor
  (let ((tranframe (selected-frame))
        (monitorid (equake-get-monitor-name (frame-monitor-attributes))))
    (set-frame-name "transientframe")
    (let ((monitorid (equake-get-monitor-name (frame-monitor-attributes))))
      (if (equake-equake-frame-p monitorid (frame-list)) ; check if *EQUAKE* frame exists
          (let ((frame-to-raise (equake-equake-frame-p monitorid (frame-list)))) ; if so, get frame id
            (delete-frame tranframe) 
            (equake-kill-stray-transient-frames (frame-list))
            (if (frame-visible-p frame-to-raise) ; then, if equake frame is already raised, make it invisible
                (progn (set-frame-parameter frame-to-raise 'fullscreen 'nil) ; un-fullscreen, in case it is fullscreened, so fullscreen doesn't get 'stuck'
                       (make-frame-invisible frame-to-raise) (make-frame-invisible frame-to-raise)) ; double-tap: one more makes 100% sure
              (progn (raise-frame frame-to-raise) ; if equake frame is invisible, raise it
                     (select-frame frame-to-raise) ; and select raised-frame
                     (let ((monwidth (equake-get-monitor-width (frame-monitor-attributes))) ; get monitor width
                           (monheight (equake-get-monitor-height (frame-monitor-attributes))) ; get monitor height
                           (mon-xpos (equake-get-monitor-xpos (frame-monitor-attributes))) ; get monitor relative x-position
                           (mon-ypos (equake-get-monitor-ypos (frame-monitor-attributes)))) ; get monitor relative y-position
                       ;; (set-frame-size (selected-frame) 1 1 t)
                       ;; (make-frame-visible frame-to-raise)
                       (set-frame-size (selected-frame) 1 1 t)                 
                       (set-frame-size (selected-frame) (truncate (* monwidth equake-width-percentage)) (truncate (* monheight equake-height-percentage)) t)    )))) ; set size accordingly
                                        ; OLD tdrop method: ;      (call-process "tdrop" nil 0 nil "current") ; if so, raise *EQUAKE* frame
                                        ; if no monitor-relative *EQUAKE* frame exists, make a new frame, rename it, call startup function
        (progn (setq equake-tab-list (remove (equake-find-monitor-list monitorid equake-tab-list) equake-tab-list))
               (equake-orphan-tabs monitorid (buffer-list)) 
               (set-frame-name (concat "*EQUAKE*[" monitorid "]")) ; set frame-name to *EQUAKE* + [monitor id]                 
               (let ((newequakeframe (selected-frame))) ; orphan any stray EQUAKE tabs/buffers before creating new frame
                 (equake-remove-screen monitorid equake-tab-list)
                 (equake-kill-stray-transient-frames (frame-list))
                 (let ((monwidth (equake-get-monitor-width (frame-monitor-attributes))) ; get monitor width
                       (monheight (equake-get-monitor-height (frame-monitor-attributes))) ; get monitor height
                       (mon-xpos (equake-get-monitor-xpos (frame-monitor-attributes))) ; get monitor relative x-position
                       (mon-ypos (equake-get-monitor-ypos (frame-monitor-attributes)))) ; get monitor relative y-position
                   (set-frame-parameter (selected-frame) 'alpha `(,equake-active-opacity ,equake-inactive-opacity))                      
                   (set-frame-size (selected-frame) (truncate (* monwidth equake-width-percentage)) (truncate (* monheight equake-height-percentage)) t) ; size again
                   (set-frame-position (selected-frame) mon-xpos mon-ypos) ; set position to top
                   (equake-launch-shell) ; launch new shell
                   (equake-set-up-equake-frame)
                   (equake-kill-stray-transient-frames (frame-list))
                   )))))))      ; execute start-up functions


(defun equake-launch-shell (&optional override)
  "Launch a new shell session."
  (interactive)
  (let ((launchshell equake-default-shell))
    (if override
        (setq launchshell override))
    (cond ((equal launchshell 'eshell)
           (eshell 'N))
          ((equal launchshell 'ansi-term)
           (ansi-term equake-default-sh-command))
          ((equal launchshell 'term)
           (term equake-default-sh-command))
          ((equal launchshell 'shell)
           (shell)
           (delete-other-windows)))))


(defun equake-set-up-equake-frame ()
  "Set-up new *EQUAKE* frame, including cosmetic changes."
  (interactive)
  (let ((monitorid (equake-get-monitor-name (frame-monitor-attributes)))) ; get current screen name
                                        ; OLD tdrop method: ;    (call-process "tdrop" nil 0 nil "-m -y 15 -e '(equake-emacs-dropdown-console)'") ; invoke tdrop on emacsclient
    (let ((monwidth (equake-get-monitor-width (frame-monitor-attributes))) ; get monitor width
          (monheight (equake-get-monitor-height (frame-monitor-attributes))) ; get monitor height
          (mon-xpos (equake-get-monitor-xpos (frame-monitor-attributes))) ; get monitor relative x-position
          (mon-ypos (equake-get-monitor-ypos (frame-monitor-attributes)))) ; get monitor relative y-position)
      (set-background-color equake-console-background-colour)                     ; set background colour
      (set-foreground-color equake-console-foreground-colour)                     ; set foreground colour
      (rename-buffer (concat "EQUAKE[" monitorid "]0%") ) ; set buffer/tab-name
      (set-frame-parameter (selected-frame) 'menu-bar-lines 0) ; no menu-bars
      (set-frame-parameter (selected-frame) 'alpha `(,equake-active-opacity ,equake-inactive-opacity))      
      (setq equake-tab-list (append equake-tab-list (list (cons monitorid (list 0))))) ; set equake local tab-list to an initial singleton list
      (set-frame-size (selected-frame) (truncate (* monwidth equake-width-percentage)) (truncate (* monheight equake-height-percentage)) t) ; size again
      (set-frame-position (selected-frame) mon-xpos mon-ypos) ; set position to top
      (setq inhibit-message t)          ; no messages in buffer
      ;; (equake-hide-orphaned-tab-frames (buffer-list))) ; hide any stray orphaned tab frames
      )))

(defun equake-count-tabs (monitor buffers count)
  "Count current equake tabs on monitor."
  (let ((buffbeg (car buffers))
        (buffend (cdr buffers)))
    (cond ((equal buffbeg 'nil)
           count)
          ((string-match-p (concat "EQUAKE\\[" monitor) (buffer-name buffbeg))
           (progn (setq count (+ 1 count))
                  (equake-count-tabs monitor buffend count)))
          ((not (string-match-p (concat "EQUAKE\\[" monitor) (buffer-name buffbeg)))
           (equake-count-tabs monitor buffend count)))))

;; MAKE OWN FUNCTION?
;; (string-to-number (replace-regexp-in-string "%[[:alnum:]]*" "" (string-remove-prefix (concat "EQUAKE[" monitor "]") (buffer-name buffbeg))))

(defun equake-highest-etab (monitor buffers highest)
  "Get highest etab number on monitor."
  (let ((buffbeg (car buffers))
        (buffend (cdr buffers)))
    (cond ((equal buffbeg 'nil)
           highest)
          ((string-match-p (concat "EQUAKE\\[" monitor) (buffer-name buffbeg)) ; only consider relevant buffers 
           (progn (if  (> (string-to-number (replace-regexp-in-string "%[[:alnum:]]*" "" (string-remove-prefix (concat "EQUAKE[" monitor "]") (buffer-name buffbeg)))) highest)
                      (setq highest (string-to-number (replace-regexp-in-string "%[[:alnum:]]*" "" (string-remove-prefix (concat "EQUAKE[" monitor "]") (buffer-name buffbeg))))))
                  (equake-highest-etab monitor buffend highest)))
          ((not (string-match-p (concat "EQUAKE\\[" monitor) (buffer-name buffbeg)))
           (equake-highest-etab monitor buffend highest)))))

(defun equake-new-tab-different-shell ()
  "Open a new shell tab, but using a shell different from the default."
  (interactive)
  (let ((shells '("eshell" "ansi-term" "term" "shell")))
    (equake-new-tab (intern (message "%s" (ido-completing-read "Choose shell:" shells )))))
  )

(defun equake-new-tab (&optional override)
  "Open a new shell tab on monitor."
  (interactive)
  (if override
      (equake-launch-shell override)    ; launch with specified shell if set
    (equake-launch-shell))              ; otherwise, launch shell normally
  (set-background-color equake-console-background-colour)                         ; set background colour
  (set-foreground-color equake-console-foreground-colour)                         ; set foreground colour
  ;; (set-frame-parameter (selected-frame) 'alpha `(,equake-active-opacity ,equake-inactive-opacity))  
  ;; (set-frame-parameter (selected-frame) 'menu-bar-lines 0) ; no menu-bars  
  (setq inhibit-message t)
  (let ((monitor  (equake-get-monitor-name (frame-monitor-attributes))))
    (let ((newhighest (+ 1 (equake-highest-etab monitor (buffer-list) 0))) ; figure out number to be set for the new tab for the current monitor
          (cur-monitor-tab-list (equake-find-monitor-list monitor equake-tab-list))) ; find the tab-list associated with the current monitor
      (rename-buffer (concat "EQUAKE[" monitor "]" (number-to-string newhighest) "%")) ; rename buffer with monitor id and new tab number
                                        ;    (set-frame-parameter (selected-frame) 'menu-bar-lines 0)  ; cosmetic changes
                                        ;    (modify-frame-parameters (selected-frame) '((vertical-scroll-bars . nil) (horizontal-scroll-bars . nil)))
      (setq equake-tab-list (remove cur-monitor-tab-list equake-tab-list)) ; remove old monitor tab-list from global equake tab list
      (setq cur-monitor-tab-list (cons (car cur-monitor-tab-list) (append (cdr cur-monitor-tab-list) (list newhighest)))) ; pull into car (=monitor name) and cdr (=tab list); append newhighest to tab list and then cons monitor name and tab list back together
      (setq equake-tab-list (append equake-tab-list (list cur-monitor-tab-list)))
      (if equake-show-monitor-in-mode-line ; show monitorid or not
          (setq mode-line-format (list (equake-mode-line (concat monitor ": ") (equake-find-monitor-list monitor equake-tab-list))))
        (setq mode-line-format (list (equake-mode-line "" (equake-find-monitor-list monitor equake-tab-list))))))))

(defun equake-find-monitor-list (monitor tabs)
  "Return the relevant list member associated with monitor/screen."
  (if (not (equal tabs 'nil))
      (if (equal monitor (car (car tabs)))
          (car tabs)
        (equake-find-monitor-list monitor (cdr tabs)))))


(defun equake-shell-after-buffer-change-hook ()
  "Things to do when in Equake when the buffer changes."
  (let ((monitorid (equake-get-monitor-name (frame-monitor-attributes))))
    (if (cl-search "EQUAKE[" (buffer-name (current-buffer)))
        (progn ; get monitor-local list of buffers and send it to be processed for the mode-line
          (if equake-show-monitor-in-mode-line ; show monitorid or not
              (setq mode-line-format (list (equake-mode-line (concat monitorid ": ") (equake-find-monitor-list monitorid equake-tab-list))))
                      (setq mode-line-format (list (equake-mode-line "" (equake-find-monitor-list monitorid equake-tab-list)))))
          (force-mode-line-update)
      ;;  (set-frame-parameter (selected-frame) 'menu-bar-lines 0) ; no menu-bars
          (modify-frame-parameters (selected-frame) '((vertical-scroll-bars . nil) (horizontal-scroll-bars . nil)))) ; no scrollbars
      (progn (setq inhibit-message 'nil)
             ;; (set-frame-parameter (selected-frame) 'menu-bar-lines 1)
             (setq mode-line-format equake-restore-mode-line)))))  ; restore 'real' mode-line to non-EQUAKE frames

(add-hook 'buffer-list-update-hook 'equake-shell-after-buffer-change-hook)

(defun equake-kill-etab-buffer-hook ()
  "Things to do when an Equake buffer is killed."  ; TODO: prevent last equake tab from being killed!
  (if (string-match-p "EQUAKE\\[" (buffer-name))
      (let ((monitor (substring (buffer-name) (+ 1 (search "[" (buffer-name))) (search "]" (buffer-name))))
            (killed-tab (string-to-number (substring (buffer-name) (+ 1 (search "]" (buffer-name))) (length (buffer-name))))))
        (let ((cur-monitor-tab-list (equake-find-monitor-list monitor equake-tab-list)))
          (setq equake-tab-list (remove cur-monitor-tab-list equake-tab-list)) ; remove old monitor tab-list member from global tab list
          (setq cur-monitor-tab-list (cons (car cur-monitor-tab-list) (remove killed-tab (cdr cur-monitor-tab-list)))) ; edit current monitor tab list to remove tab
          (setq equake-tab-list (append equake-tab-list (list cur-monitor-tab-list))) ; add edited current monitor tab list back to global tab list
          (setq mode-line-format (list (equake-mode-line "" cur-monitor-tab-list)))))))

(add-hook 'kill-buffer-hook 'equake-kill-etab-buffer-hook)

(defun equake-find-next-etab (tablist tab)
  "Return the next etab."
  (cond ((equal (car tablist) 'nil) ; return 'nil if we're at the end of the list
         'nil)
        ((equal (car tablist) tab) ; if we find the current tab, cdr and then car the list to get the next tab
         (car (cdr tablist)))
        ((not (equal (car tablist) tab)) ; if not, then cdr the list and test again
         (equake-find-next-etab (cdr tablist) tab))))

(defun equake-move-tab (monitor tablist moving-tab direction)
  "Move the current tab one position to the right (direction=1) or left (direction=-1) in the list."
  (let ((examined-tab (car tablist)))   ; get entire local monitor tab-list
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
                                  (setq equake-tab-list (remove examined-tab equake-tab-list)) ; remove the entire local monitor tab-list from global etab-list
                                  (setq equake-tab-list (append equake-tab-list reconstructed-local-tab)) ; append the named, modified monitor tab list to the global equake tab list
                                  (if equake-show-monitor-in-mode-line ; show monitorid or not
                                      (setq mode-line-format (list (equake-mode-line (concat monitorid ": ") (equake-find-monitor-list monitorid equake-tab-list))))
                                    (setq mode-line-format (list (equake-mode-line "" (equake-find-monitor-list monitorid equake-tab-list)))))
        ;                         (setq mode-line-format (list (equake-mode-line "" (equake-find-monitor-list monitor equake-tab-list)))) ; update mode-line
                                  (force-mode-line-update)))))) ; force refresh mode-line
            (equake-move-tab monitor (cdr tablist) moving-tab direction)) ; check next monitor-local tab-list
        (message (concat "error: no relevant monitor " monitor " in ")))))) ; error if for some reason no such-name monitor

(defun equake-move-tab-right ()
  "Move current tab one position to the right."
  (interactive)
  (let ((monitorid (equake-get-monitor-name (frame-monitor-attributes))))
    (let ((current-etab (string-to-number (replace-regexp-in-string "%[[:alnum:]]*" "" (string-remove-prefix (concat "EQUAKE[" monitorid "]") (buffer-name (current-buffer))))))) ; get local tab number
    (equake-move-tab monitorid equake-tab-list current-etab 1)))) ; call general tab move function

(defun equake-move-tab-left ()
  "Move current tab one position to the left."
  (interactive)
(let ((monitorid (equake-get-monitor-name (frame-monitor-attributes))))
  (let ((current-etab (string-to-number (replace-regexp-in-string "%[[:alnum:]]*" "" (string-remove-prefix (concat "EQUAKE[" monitorid "]") (buffer-name (current-buffer))))))) ; get local tab number
    (equake-move-tab monitorid equake-tab-list current-etab -1)))) ; call general tab move function

(defun equake-next-tab ()
  "Switch to the next tab."
  (interactive)
  (let ((monitorid (equake-get-monitor-name (frame-monitor-attributes))))
    (if (< (equake-count-tabs monitorid (buffer-list) 0) 2)
        (print "No other tab to switch to.")
      (progn
        (let  ((current-tab (string-to-number (substring (buffer-name) (+ 1 (search "]" (buffer-name))) (+ 1 (search "%" (buffer-name)))))))
          (let ((next-tab (equake-find-next-etab (cdr (equake-find-monitor-list monitorid equake-tab-list)) current-tab)))
            (if (equal next-tab 'nil)   ; switch to first tab if at end of list
                (switch-to-buffer (equake-find-buffer-by-monitor-and-tabnumber monitorid (car (cdr (equake-find-monitor-list monitorid equake-tab-list))) (buffer-list)))
              (switch-to-buffer (equake-find-buffer-by-monitor-and-tabnumber monitorid next-tab (buffer-list))))))))))

(defun equake-prev-tab ()
  "Switch to the previous tab."
  (interactive)
  (let ((monitorid (equake-get-monitor-name (frame-monitor-attributes))))
    (if (< (equake-count-tabs monitorid (buffer-list) 0) 2)
        (print "No other tab to switch to.")
      (progn         ; re-use equake-find-next-tab function, first reversing the list
        (let ((current-tab (string-to-number (substring (buffer-name) (+ 1 (search "]" (buffer-name))) (+ 1 (search "%" (buffer-name)))))))
          (let ((prev-tab (equake-find-next-etab (reverse (cdr (equake-find-monitor-list monitorid equake-tab-list))) current-tab)))
            (if (equal prev-tab 'nil)   ; switch to last tab if at beginning of list
                (switch-to-buffer (equake-find-buffer-by-monitor-and-tabnumber monitorid (car  (reverse (cdr (equake-find-monitor-list monitorid equake-tab-list)))) (buffer-list)))
              (switch-to-buffer (equake-find-buffer-by-monitor-and-tabnumber monitorid prev-tab (buffer-list))))))))))


(defun equake-find-buffer-by-monitor-and-tabnumber (monitor tabnum buffers)
"Find the name of an equake buffer given a monitor/screen name and tab number."
 (let ((buffbeg (car buffers))
       (buffend (cdr buffers))
       (name-skeleton (concat "EQUAKE\\[" monitor "\\]" (number-to-string tabnum) "%"))) ; buffer template matching everything before %+following characters
   (cond ((equal buffbeg 'nil)                              ; if we're out of buffers
          (message "Error! No such screen-tag pair exists! %s %s" monitor tabnum ))    ; in case of trouble, please panic
         ((string-match-p name-skeleton (buffer-name buffbeg)) ; if buffer name matches matches skeleton, i.e. everything before %+following characters
          buffbeg)                                             ; then return that buffer
         (t (equake-find-buffer-by-monitor-and-tabnumber monitor tabnum buffend))))) ; go on to next buffer

(defun equake-rename-etab ()
"Rename current equake tab."
(interactive)
(let ((buffer-prefix (replace-regexp-in-string "%\.*" "" (buffer-name (current-buffer))))) ; get everything before the '%' and any characters that follow it
  (let ((newname (read-string "Enter a new tab name: ")))
    ;; (message "New name is %s" newname)
    (rename-buffer (concat buffer-prefix "%" newname)))))

(defun equake-mode-line (modelinestring buffers)
  "Content of mode-line for equake (show tabs)."
  (let ((curtab (car (cdr buffers))))
    (if (equal curtab 'nil)
        (list modelinestring (equake-shell-type-styling major-mode))
      (progn (setq modelinestring (concat modelinestring (equake-extract-format-tab-name curtab))) ; get name/number for tab in mode-line format
             (equake-mode-line modelinestring (cdr buffers)))))) ; go on to next tab

(defun equake-shell-type-styling (mode)
  "Style the shell-type indicator." (cond ((equal (format "%s" mode) "eshell-mode")
         (propertize " ((eshell)) " 'font-lock-face `(:foreground ,equake-shell-type-eshell-foreground :background ,equake-shell-type-eshell-background)))
        ((equal (format "%s" mode) "term-mode")
         (propertize " ((term)) " 'font-lock-face `(:foreground ,equake-shell-type-term-foreground :background ,equake-shell-type-term-background)))
        ((equal (format "%s" mode) "shell-mode")
         (propertize " ((shell)) " 'font-lock-face `(:foreground ,equake-shell-type-shell-foreground :background ,equake-shell-type-shell-background)))))

(defun equake-extract-format-tab-name  (tab)
  "Extract equake tab name and format it for the modeline."
  (let ((monitor (equake-get-monitor-name (frame-monitor-attributes))))
    (let ((current-etab  (string-to-number (string-remove-prefix (concat "EQUAKE[" monitor "]") (buffer-name (current-buffer)))))) ; get the tab number of the current buffer
      (let ((etab-name (string-remove-prefix (concat "EQUAKE[" monitor "]" (number-to-string tab) "%") (buffer-name (equake-find-buffer-by-monitor-and-tabnumber monitor tab (buffer-list)))))) ; find the name of the tab
        (if (equal etab-name "")                     ; if the name is null string
            (setq etab-name (number-to-string tab))) ; set name to tab number
        (if (equal tab current-etab)                 
            (concat " " (propertize (concat "[ " etab-name " ]") 'font-lock-face `(:foreground ,equake-inactive-tab-foreground-colour :background ,equake-inactive-tab-background-colour)) " ") ; 'highlight' current tab
          (concat " " (propertize  (concat "[ " etab-name " ]") 'font-lock-face `(:foreground ,equake-active-tab-foreground-colour :background ,equake-active-tab-background-colour)) " "))))))

(provide 'equake)

;;; equake.el ends here
