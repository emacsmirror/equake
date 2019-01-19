;;; equake.el --- Quake-style drop-drop console -*- lexical-binding: t; -*-

;; *EQUAKE* - emacs shell dropdown console

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         _         ;;;
;;   ___  __ _ _   _  __ _| | _____  ;;;
;;  / _ \/ _` | | | |/ _` | |/ / _ \ ;;;
;; |  __/ (_| | |_| | (_| |   <  __/ ;;;
;;  \___|\__, |\__,_|\__,_|_|\_\___| ;;;
;;          |_|                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Copyright (C) 2018-2019 Benjamin Slade

;; Author: Benjamin Slade <slade@jnanam.net>
;; Maintainer: Benjamin Slade <slade@jnanam.net>
;; URL: https://gitlab.com/emacsomancer/equake
;; Package-Version: 0.73
;; Version: 0.73
;; Package-Requires: ((emacs "25") (dash "2.14.1") (tco "20160811.12"))
;; Created: 2018-12-12
;; Keywords: convenience, frames, terminals, tools, window-system

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

;; This package is designed to recreate a Quake-style drop-down console fully
;; within Emacs, compatible with 'eshell, 'term, 'ansi-term, and 'shell modes.
;; It has multi-tab functionality, and the tabs can be moved and renamed
;; (different shells can be opened and used in different tabs).  It is intended
;; to be bound to shortcut key like F12 to toggle it off-and-on.

;; Installation:
;; To install manually, clone the git repo somewhere and put it in your
;; load-path, e.g., add something like this to your init.el:
;; (add-to-list 'load-path
;;             "~/.emacs.d/equake/")
;;  (require 'equake)

;; Usage:
;; Run with:---
;; emacsclient -n -e '(equake-invoke)' ,
;; after launching an Emacs daemon of course.
;; Alternatively, on multi-monitor setup, launch:
;; emacsclient -n -c -e '(equake-invoke)' -F '((title . "*transient*") (alpha . (0 . 0)) (width . (text-pixels . 0)) (height . (text-pixels . 0)))'
;; 
;; I recommend binding the relevant command to a key like F12 in your DE/WM.
;; Executing this command will create a new equake console
;; on your screen the first time, and subsequently toggle
;; the console (i.e. hide or show it).
;;
;; It works with eshell, ansi-term, term, shell.  But it was
;; really designed to work with eshell, which is the default.
;; New console tabs can be specified to open with a shell
;; other than the default shell.
;;
;; Equake is designed to work with multi-screen setups,
;; with a different set of tabs for each screen.
;;
;; You'll probably also want to configure your WM/DE to
;; ignore the window in the task manager etc and
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
;; Check: Focus stealing prevention - Force - None
;; Check: Focus protection - Force - Normal
;; Check: Accept focus - Force - Yes
;; 
;; In awesomewm, probably adding to your 'Rules' something
;; like this:
;; 
;; { rule = { instance = "*EQUAKE*", class = "Emacs" },
;;    properties = { titlebars_enabled = false } },
;; 
;; In stumpwm, I'm not sure: it doesn't seem to respect
;; Emacs frame settings.
;;
;; Advice:
;; add (global-set-key (kbd "C-x C-c") 'equake-check-if-in-equake-frame-before-closing)
;; to your settings to prevent accidental closure of equake frames

;; TODO:
;; 1. defcustoms:
;;   (a) for keybindings
;;   (b) make shell choice into actual list, or else more flexible functions
;; 2. Prevent last tab from being closed, or at least prompt.
;; 3. Maybe do something to (optionally) silence the minibuffer?
;;    (setq inhibit-message t) doesn't seem to help
;;    But it's probably fine as is.
;; 4. Test on:
;;    (a) MacOS -- ??
;;    (b) Windows -- ??
;;   Comments: In theory it should work on Mac & Windows, since frame.el defines
;;             frame-types 'ns (=Next Step) and 'w32 (=Windows).

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'subr-x)
(require 'dash)                         ; for -let*
(require 'tco)                          ; tail-call optimisation
(setq lexical-binding 't)               ; redundant?

(defvar equake-tab-list ())             ; initialise empty list of equake tabs
(defvar equake-last-buffer-list ())     ; initialise empty list of equake last buffers
(defvar equake-last-etab-list ())       ; initialise empty list of equake etab buffers
(defvar equake-win-history ())          ; initialise empty list of equake window history

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
  :group 'equake)

(defcustom equake-show-monitor-in-mode-line 'nil
  "Toggle to show monitor id string as part of equake mode-line."
  :type 'boolean
  :group 'equake)

(defcustom equake-console-foreground-colour "#eeeeee"
  "Background colour of Equake console."
  :type 'string
  :group 'equake)

(defcustom equake-console-background-colour "#000022"
  "Background colour of Equake console."
  :type 'string
  :group 'equake)

(defcustom equake-inactive-tab-foreground-colour "black"
  "Colour of foreground text of inactive Equake tabs."
  :type 'string
  :group 'equake)

(defcustom equake-inactive-tab-background-colour "lightblue"
  "Background colour of inactive Equake tabs."
  :type 'string
  :group 'equake)

(defcustom equake-active-tab-foreground-colour "lightblue"
  "Colour of foreground text of active Equake tabs."
  :type 'string
  :group 'equake)

(defcustom equake-active-tab-background-colour "black"
  "Background colour of active Equake tabs."
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

;; TODO: add defcustom to set font, e.g.:
;; (face-remap-add-relative 'default '(:family "DejaVu Sans Mono" :height 108))

(defun equake-ask-before-closing-equake ()
  "Make sure user really wants to close Equake, ask again."
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to close the equake console frame? \n[Advice: Cancel and use `C-x k` to close the buffer instead, returning to your shell session.]? "))
      (save-buffers-kill-terminal)
    (message "Wisely cancelling the closing of the equake console frame...")))

(defun equake-check-if-in-equake-frame-before-closing ()
  "Check if we're in an Equake frame."
  (interactive)
  (if (cl-search "*EQUAKE*[" (frame-parameter (selected-frame) 'name))
      (equake-ask-before-closing-equake)
    (save-buffers-kill-terminal)))

(defun equake-key-bindings ()
  "Set tab movement bindings."
  (local-set-key (kbd "C-+") #'equake-new-tab)
  (local-set-key (kbd "C-M-+") #'equake-new-tab-different-shell)
  (local-set-key (kbd "C-{") #'equake-prev-tab)
  (local-set-key (kbd "C-}") #'equake-next-tab)
  (local-set-key (kbd "C-M-{") #'equake-move-tab-left)
  (local-set-key (kbd "C-M-}") #'equake-move-tab-right)
  (local-set-key (kbd "C-|") #'equake-rename-etab))

(defun-tco equake-equake-frame-p (monitor frames)
  "Test if *EQUAKE* is an existing frame in MONITOR given a list of FRAMES."
  (let ((frame (car frames)))
    (when frame
      (if (equal (concat "*EQUAKE*[" monitor "]") (frame-parameter frame 'name))
          frame
        (equake-equake-frame-p monitor (cdr frames))))))

(defun-tco equake-get-monitor-property (prop attributes)
  "Get a property PROP of the current monitor/screen ATTRIBUTES."
  (let ((field (car attributes)))
    (when field
      (if (equal (format "%s" (car field)) prop)
          (cdr field)
        (equake-get-monitor-property prop (cdr attributes))))))

(defun equake-get-monitor-name ()
  "Get the name or another constant designator of the current monitor/screen ATTRIBUTES."
  (let ((name (equake-get-monitor-property "name" (frame-monitor-attributes))))
    (if name
        name
      (when equake-unnamed-monitor
        (let ((name (equake-get-monitor-property "geometry" (frame-monitor-attributes))))
          (when name
              (format "%s" name)))))))

(defun-tco equake-kill-stray-transient-frames (frames)
  "Destroy any stray transient FRAMES."
  (interactive)
  (let ((frame (car frames)))
    (when frame
      (when (cl-search "*transient*" (frame-parameter frame 'title))
        (delete-frame frame))
      (equake-kill-stray-transient-frames (cdr frames)))))

(defun equake-invoke ()
  "Toggle Equake frames.
Run with \"emacsclient -n -e '(equake-invoke)'\".
On multi-monitor set-ups, run instead \"emacsclient -n -c -e '(equake-invoke)' -F '((title . \"*transient*\") (alpha . (0 . 0)) (width . (text-pixels . 0)) (height . (text-pixels . 0)))'\"."
  (interactive)
  (-let* ((monitorid (equake-get-monitor-name))
          (equake-current-frame (equake-equake-frame-p monitorid (frame-list)))
          ((mon-xpos mon-ypos monwidth monheight) (mapcar #'floor (alist-get 'workarea (frame-monitor-attributes))))
          (mod-mon-xpos (floor (+ mon-xpos (/ (- monwidth (* monwidth equake-width-percentage)) 2)))))
    (if equake-current-frame            ; if frame exists, destroy it.
        (progn  (select-frame equake-current-frame)
                (when (equal (buffer-name (current-buffer)) " *server*") ; if opened to " *server*" buffer
                  (switch-to-buffer (other-buffer (current-buffer) 1)))  ; switch to other buffer
                (equake-set-last-buffer)
                (equake-set-winhistory)
                (when (string-match-p "EQUAKE\\[" (buffer-name (current-buffer)))
                  (equake-set-last-etab))
                (delete-frame equake-current-frame)) ; destroy frame.
      ;; else, make it.
      (-let* ((new-frame (make-frame (list (cons 'name (concat "*EQUAKE*[" monitorid "]"))
                                           (cons 'alpha `(,equake-active-opacity ,equake-inactive-opacity))
                                           (cons 'left mod-mon-xpos)
                                           (cons 'top mon-ypos)
                                           (cons 'width (cons 'text-pixels (truncate (* monwidth equake-width-percentage))))
                                           (cons 'height (cons 'text-pixels (truncate (* monheight equake-height-percentage))))))))
        (select-frame new-frame)
        (set-window-prev-buffers nil (cdr (equake-find-monitor-list monitorid equake-win-history)))
        (let ((highest-montab (equake-highest-etab monitorid (buffer-list) -1)))
          (if (< highest-montab 0)
              (equake-new-tab)          ; launch new shell
            (switch-to-buffer (cdr (equake-find-monitor-list monitorid equake-last-etab-list)))
            (switch-to-buffer (cdr (equake-find-monitor-list monitorid equake-last-buffer-list))))
          (equake-set-up-equake-frame))
        (set-window-prev-buffers nil (equake-filter-history (window-prev-buffers) (window-prev-buffers)))))
        (equake-kill-stray-transient-frames (frame-list))))

(defun-tco equake-filter-history (winhist filtwinhist)
  "Filter window history (WINHIST) into FILTWINHIST."
  (if (not winhist)
      filtwinhist
    (if (listp (car winhist))
        (unless (string-match-p "EQUAKE\\[" (buffer-name (car (car winhist))))
          (setq filtwinhist (remove (car winhist) filtwinhist)))
      (if (bufferp (car winhist))
          (unless (string-match-p "EQUAKE\\[" (buffer-name (car winhist)))
            (setq filtwinhist (remove (car winhist) filtwinhist)))
        (setq filtwinhist (remove (car winhist) filtwinhist)))
      (equake-filter-history (cdr winhist) filtwinhist))))

(defun equake-launch-shell (&optional override)
  "Launch a new shell session, OVERRIDE will set non-default shell."
  (interactive)
  (let ((launchshell equake-default-shell))
    (when override
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
  "Set-up new *EQUAKE* frame, including cosmetic alterations."
  (interactive)
  (set-background-color equake-console-background-colour) ; set background colour
  (set-foreground-color equake-console-foreground-colour) ; set foreground colour
  (set-frame-parameter (selected-frame) 'alpha `(,equake-active-opacity ,equake-inactive-opacity))
  (setq inhibit-message t))

(defun-tco equake-count-tabs (monitor buffers count)
  "COUNT current Equake tab BUFFERS on MONITOR."
  (let ((buffbeg (car buffers))
        (buffend (cdr buffers)))
    (cond ((equal buffbeg 'nil)
           count)
          ((string-match-p (concat "EQUAKE\\[" monitor) (buffer-name buffbeg))
           (progn (setq count (1+ count))
                  (equake-count-tabs monitor buffend count)))
          ((not (string-match-p (concat "EQUAKE\\[" monitor) (buffer-name buffbeg)))
           (equake-count-tabs monitor buffend count)))))

(defun-tco equake-highest-etab (monitor buffers highest)
  "Get HIGHEST etab number among BUFFERS on MONITOR."
  (let ((buffbeg (car buffers))
        (buffend (cdr buffers)))
    (cond ((equal buffbeg 'nil)
           highest)
          ((string-match-p (concat "EQUAKE\\[" monitor) (buffer-name buffbeg)) ; only consider relevant buffers
           (progn (when (> (string-to-number (replace-regexp-in-string "%[[:alnum:]]*" "" (string-remove-prefix (concat "EQUAKE[" monitor "]") (buffer-name buffbeg)))) highest)
                    (setq highest (string-to-number (replace-regexp-in-string "%[[:alnum:]]*" "" (string-remove-prefix (concat "EQUAKE[" monitor "]") (buffer-name buffbeg))))))
                  (equake-highest-etab monitor buffend highest)))
          ((not (string-match-p (concat "EQUAKE\\[" monitor) (buffer-name buffbeg)))
           (equake-highest-etab monitor buffend highest)))))

(defun-tco equake-lowest-etab (monitor buffers lowest)
  "Get LOWEST etab number among BUFFERS on MONITOR."
  (let ((buffbeg (car buffers))
        (buffend (cdr buffers)))
    (cond ((equal buffbeg 'nil)
           lowest)
          ((string-match-p (concat "EQUAKE\\[" monitor) (buffer-name buffbeg)) ; only consider relevant buffers
           (progn (when (< (string-to-number (replace-regexp-in-string "%[[:alnum:]]*" "" (string-remove-prefix (concat "EQUAKE[" monitor "]") (buffer-name buffbeg)))) lowest)
                    (setq lowest (string-to-number (replace-regexp-in-string "%[[:alnum:]]*" "" (string-remove-prefix (concat "EQUAKE[" monitor "]") (buffer-name buffbeg))))))
                  (equake-lowest-etab monitor buffend lowest)))
          ((not (string-match-p (concat "EQUAKE\\[" monitor) (buffer-name buffbeg)))
           (equake-lowest-etab monitor buffend lowest)))))

(defun equake-new-tab-different-shell ()
  "Open a new shell tab, but using a shell different from the default."
  (interactive)
  (let ((shells '("eshell" "ansi-term" "term" "shell")))
    (equake-new-tab (intern (message "%s" (ido-completing-read "Choose shell:" shells ))))))

(defun equake-new-tab (&optional override)
  "Open a new shell tab on monitor, optionally OVERRIDE default shell."
  (interactive)
  (if override
      (equake-launch-shell override)    ; launch with specified shell if set
    (equake-launch-shell))              ; otherwise, launch shell normally
  (set-background-color equake-console-background-colour) ; set background colour
  (set-foreground-color equake-console-foreground-colour) ; set foreground colour
  (setq inhibit-message t)
  (let* ((monitor (equake-get-monitor-name))
         (newhighest (1+ (equake-highest-etab monitor (buffer-list) -1))) ; figure out number to be set for the new tab for the current monitor
         (cur-monitor-tab-list (equake-find-monitor-list monitor equake-tab-list))) ; find the tab-list associated with the current monitor
    (rename-buffer (concat "EQUAKE[" monitor "]" (number-to-string newhighest) "%")) ; rename buffer with monitor id and new tab number
    (if (equal equake-tab-list 'nil)
        (setq equake-tab-list (list (cons monitor (list newhighest))))
      (if (equal cur-monitor-tab-list 'nil)
          (setq equake-tab-list  (append equake-tab-list (list (cons monitor (list newhighest)))))
        (setq equake-tab-list (remove cur-monitor-tab-list equake-tab-list)) ; remove old monitor tab-list from global equake tab list
        ;; pull into car (=monitor name) and cdr (=tab list); append newhighest to tab list and then cons monitor name and tab list back together
        (setq cur-monitor-tab-list (cons (car cur-monitor-tab-list) (append (cdr cur-monitor-tab-list) (list newhighest))))
        (setq equake-tab-list (append equake-tab-list (list cur-monitor-tab-list)))))
    (equake-set-winhistory)
    (if equake-show-monitor-in-mode-line ; show monitorid or not
        (setq mode-line-format (list (equake-mode-line (concat monitor ": ") (equake-find-monitor-list monitor equake-tab-list))))
      (setq mode-line-format (list (equake-mode-line "" (equake-find-monitor-list monitor equake-tab-list)))))
    (force-mode-line-update)))

(defun-tco equake-find-monitor-list (monitor tabs)
  "Return the relevant list member associated with MONITOR/screen amongst TABS."
  (unless (equal tabs 'nil)
    (if (equal monitor (car (car tabs)))
        (car tabs)
      (equake-find-monitor-list monitor (cdr tabs)))))

(defun equake-shell-after-buffer-change-hook ()
  "Things to do when in Equake when the current buffer is changed."
  (let ((monitorid (equake-get-monitor-name)))
    (if (cl-search (concat "EQUAKE[" monitorid) (buffer-name (current-buffer)))
        (progn
          ;; get monitor-local list of buffers and send it to be processed for the mode-line
          (if equake-show-monitor-in-mode-line ; show monitorid or not
              (setq mode-line-format (list (equake-mode-line (concat monitorid ": ") (equake-find-monitor-list monitorid equake-tab-list))))
            (setq mode-line-format (list (equake-mode-line "" (equake-find-monitor-list monitorid equake-tab-list)))))
          (force-mode-line-update)
          (modify-frame-parameters (selected-frame) '((vertical-scroll-bars . nil) (horizontal-scroll-bars . nil))) ; no scroll-bars
          (when (cl-search "*EQUAKE*[" (frame-parameter (selected-frame) 'name)) ; if we're in an equake frame
            (set-frame-parameter (selected-frame) 'menu-bar-lines 0) ; no menu-bar
            (set-frame-parameter (selected-frame) 'tool-bar-lines 0) ; no tool-bar
            (equake-key-bindings))                                   ; set equake bindings
          (equake-set-last-etab)
          (equake-set-winhistory))
      (setq inhibit-message 'nil))))

(add-hook 'buffer-list-update-hook #'equake-shell-after-buffer-change-hook)

(defun equake-set-last-buffer ()
  "Set last seen buffer."
  (let* ((monitorid (equake-get-monitor-name))
         (cur-monitor-last-buffer (equake-find-monitor-list monitorid equake-last-buffer-list)))
    (if equake-last-buffer-list
        (if cur-monitor-last-buffer
            (progn (setq equake-last-buffer-list (remove cur-monitor-last-buffer equake-last-buffer-list)) ; remove old monitor tab-list member from current tabs
                   (setq equake-last-buffer-list (append equake-last-buffer-list (list (cons monitorid (current-buffer))))))
          (setq equake-last-buffer-list (append equake-last-buffer-list (list (cons monitorid (current-buffer))))))
      (setq equake-last-buffer-list (list (cons monitorid (current-buffer)))))))

(defun equake-set-last-etab ()
  "Set last seen etab."
  (let* ((monitorid (equake-get-monitor-name))
         (cur-monitor-last-buffer (equake-find-monitor-list monitorid equake-last-etab-list)))
    (if equake-last-etab-list
        (if cur-monitor-last-buffer
            (progn (setq equake-last-etab-list (remove cur-monitor-last-buffer equake-last-etab-list)) ; remove old monitor tab-list member from current tabs
                   (setq equake-last-etab-list (append equake-last-etab-list (list (cons monitorid (current-buffer))))))
          (setq equake-last-etab-list (append equake-last-etab-list (list (cons monitorid (current-buffer))))))
      (setq equake-last-etab-list (list (cons monitorid (current-buffer)))))))

(defun equake-set-winhistory ()
  "Remember the buffer history for window."
  (let* ((monitorid (equake-get-monitor-name))
         (cur-monitor-last-etab (equake-find-monitor-list monitorid equake-win-history)))
    (if equake-win-history
        (progn (when cur-monitor-last-etab
                 (setq equake-win-history (remove cur-monitor-last-etab equake-win-history))) ; remove old monitor tab-list member from current tabs
               (setq equake-win-history (append equake-win-history (list (cons monitorid (window-prev-buffers))))))
      (setq equake-win-history (list (cons monitorid (window-prev-buffers)))))))

(defun equake-kill-etab-buffer-hook ()
  "Things to do when an Equake buffer is killed." ; TODO: prevent last equake tab from being killed
  (when (string-match-p "EQUAKE\\[" (buffer-name))
    (let* ((monitor (substring (buffer-name) (1+ (cl-search "[" (buffer-name))) (cl-search "]" (buffer-name))))
           (killed-tab (string-to-number (substring (buffer-name) (1+ (cl-search "]" (buffer-name))) (length (buffer-name)))))
           (cur-monitor-tab-list (equake-find-monitor-list monitor equake-tab-list)))
      (when (cl-search "*EQUAKE*[" (frame-parameter (selected-frame) 'name)) ; if we're in an equake frame
          (if (equake-find-next-etab (cdr (equake-find-monitor-list monitor equake-tab-list)) killed-tab) ; switch to the next etab, if if exists
              (switch-to-buffer (equake-find-buffer-by-monitor-and-tabnumber monitor (equake-find-next-etab (cdr (equake-find-monitor-list monitor equake-tab-list)) killed-tab) (buffer-list)))
            (switch-to-buffer (equake-find-buffer-by-monitor-and-tabnumber monitor (car (cdr (equake-find-monitor-list monitor equake-tab-list))) (buffer-list))))) ;otherwise switch to last
      (setq equake-tab-list (remove cur-monitor-tab-list equake-tab-list)) ; remove old monitor tab-list member from global tab list
      (setq cur-monitor-tab-list (cons (car cur-monitor-tab-list) (remove killed-tab (cdr cur-monitor-tab-list)))) ; edit current monitor tab list to remove tab
      (setq equake-tab-list (append equake-tab-list (list cur-monitor-tab-list))) ; add edited current monitor tab list back to global tab list
      (when (cl-search (concat "EQUAKE[" monitor) (buffer-name (current-buffer)))
        (progn (setq mode-line-format (list (equake-mode-line "" cur-monitor-tab-list)))
               (force-mode-line-update))))))

(add-hook 'kill-buffer-hook #'equake-kill-etab-buffer-hook)

(defun-tco equake-find-next-etab (tablist tab)
  "Return the next TAB from TABLIST."
  (cond ((equal (car tablist) 'nil)  ; return 'nil if we're at the end of the list
         'nil)
        ((equal (car tablist) tab)  ; if we find the current tab, cdr and then car the list to get the next tab
         (car (cdr tablist)))
        ((not (equal (car tablist) tab)) ; if not, then cdr the list and test again
         (equake-find-next-etab (cdr tablist) tab))))

(defun-tco equake-move-tab (monitor tablist moving-tab direction)
  "Move the current MOVING-TAB in the TABLIST on MONITOR one position to the right (DIRECTION=1) or left (DIRECTION=-1) in the TABLIST."
  (let* ((examined-tab (car tablist))   ; get entire local monitor tab-list
         (monitor-tab-list (cdr examined-tab))) ; just the local monitor list of tabs
    (if (not tablist)
        (message (concat "error: no relevant monitor " monitor " in "))   ; error if for some reason no such-name monitor
      (if (equal (car examined-tab) monitor) ; if the monitor label matches
          (if (< (length monitor-tab-list) 2) ; if only one tab, stop with message to user
              (message "Only one tab")
            (let* ((orig-pos (cl-position moving-tab monitor-tab-list)) ; find the original position of tab we're moving
                   (target-pos (+ orig-pos direction)) ; find the target position of the moving tab
                   (reconstructed-local-tab 'nil))     ; initialise local variable
              (cond ((< target-pos 0) ; if trying to move tab beyond left edge
                     (progn (setq monitor-tab-list (remove moving-tab monitor-tab-list)) ; delete tab
                            (setq monitor-tab-list (append monitor-tab-list (list moving-tab))))) ; add tab to right edge
                    ((> target-pos (1- (length monitor-tab-list))) ; if trying to move tab beyond right edge
                     (progn (setq monitor-tab-list (remove moving-tab monitor-tab-list)) ; delete tab
                            (setq monitor-tab-list (append (list moving-tab) monitor-tab-list)))) ; add tab to left edge
                    (t (progn (let ((target-temp-id (nth target-pos monitor-tab-list))) ; else, store the original content of target position
                                (setf (nth target-pos monitor-tab-list) moving-tab (nth orig-pos monitor-tab-list) target-temp-id ))))) ; modify the list positions accordingly)))
              (setq reconstructed-local-tab (list (cons monitor monitor-tab-list))) ; cons monitor name with modified tab-list
              (setq equake-tab-list (remove examined-tab equake-tab-list)) ; remove the entire local monitor tab-list from global etab-list
              (setq equake-tab-list (append equake-tab-list reconstructed-local-tab)) ; append the named, modified monitor tab list to the global equake tab list
              (if equake-show-monitor-in-mode-line ; show monitorid or not
                  (setq mode-line-format (list (equake-mode-line (concat monitor ": ") (equake-find-monitor-list monitor equake-tab-list))))
                (setq mode-line-format (list (equake-mode-line "" (equake-find-monitor-list monitor equake-tab-list)))))
              (force-mode-line-update))) ; force refresh mode-line
        (equake-move-tab monitor (cdr tablist) moving-tab direction))))) ; check next monitor-local tab-list

(defun equake-move-tab-right ()
  "Move current tab one position to the right."
  (interactive)
  (let* ((monitorid (equake-get-monitor-name))
         (current-etab (string-to-number (replace-regexp-in-string "%[[:alnum:]]*" "" (string-remove-prefix (concat "EQUAKE[" monitorid "]") (buffer-name (current-buffer)))))))
     (equake-move-tab monitorid equake-tab-list current-etab 1)
    (equake-set-winhistory))) ; call general tab move function

(defun equake-move-tab-left ()
  "Move current tab one position to the left."
  (interactive)
  (let* ((monitorid (equake-get-monitor-name))
         (current-etab (string-to-number (replace-regexp-in-string "%[[:alnum:]]*" "" (string-remove-prefix (concat "EQUAKE[" monitorid "]") (buffer-name (current-buffer)))))))
    (equake-move-tab monitorid equake-tab-list current-etab -1)
    (equake-set-winhistory))) ; call general tab move function

(defun equake-next-tab ()
  "Switch to the next tab."
  (interactive)
  (let ((monitorid (equake-get-monitor-name)))
    (if (< (equake-count-tabs monitorid (buffer-list) 0) 2)
        (print "No other tab to switch to.")
      (let* ((current-tab (string-to-number (substring (buffer-name) (1+ (cl-search "]" (buffer-name))) (1+ (cl-search "%" (buffer-name))))))
             (next-tab (equake-find-next-etab (cdr (equake-find-monitor-list monitorid equake-tab-list)) current-tab)))
        (if (equal next-tab 'nil)  ; switch to first tab if at end of list
            (switch-to-buffer (equake-find-buffer-by-monitor-and-tabnumber monitorid (car (cdr (equake-find-monitor-list monitorid equake-tab-list))) (buffer-list)))
          (switch-to-buffer (equake-find-buffer-by-monitor-and-tabnumber monitorid next-tab (buffer-list))))))))

(defun equake-prev-tab ()
  "Switch to the previous tab."
  (interactive)
  (let ((monitorid (equake-get-monitor-name)))
    (if (< (equake-count-tabs monitorid (buffer-list) 0) 2)
        (print "No other tab to switch to.")
      ;; re-use equake-find-next-tab function, first reversing the list
      (let* ((current-tab (string-to-number (substring (buffer-name) (1+ (cl-search "]" (buffer-name))) (1+ (cl-search "%" (buffer-name))))))
             (prev-tab (equake-find-next-etab (reverse (cdr (equake-find-monitor-list monitorid equake-tab-list))) current-tab)))
        (if (equal prev-tab 'nil)   ; switch to last tab if at beginning of list
            (switch-to-buffer (equake-find-buffer-by-monitor-and-tabnumber monitorid (car  (reverse (cdr (equake-find-monitor-list monitorid equake-tab-list)))) (buffer-list)))
          (switch-to-buffer (equake-find-buffer-by-monitor-and-tabnumber monitorid prev-tab (buffer-list))))))))

(defun-tco equake-find-buffer-by-monitor-and-tabnumber (monitor tabnum buffers)
  "Return an Equake buffer from BUFFERS given a MONITOR/screen name and TABNUM."
  (let ((buffbeg (car buffers))
        (buffend (cdr buffers))
        (name-skeleton (concat "EQUAKE\\[" monitor "\\]" (number-to-string tabnum) "%"))) ; buffer template matching everything before %+following characters
    (cond ((equal buffbeg 'nil)          ; if we're out of buffers
           (equake-find-buffer-by-monitor-and-tabnumber monitor (equake-highest-etab monitor (buffer-list) -1) (buffer-list)))
          ((string-match-p name-skeleton (buffer-name buffbeg)) ; if buffer name matches matches skeleton, i.e. everything before %+following characters
           buffbeg)                                             ; then return that buffer
          (t (equake-find-buffer-by-monitor-and-tabnumber monitor tabnum buffend))))) ; go on to next buffer

(defun equake-rename-etab ()
  "Rename current Equake tab."
  (interactive)
  (let* ((buffer-prefix (replace-regexp-in-string "%\.*" "" (buffer-name (current-buffer)))) ; get everything before the '%' and any characters that follow it
         (newname (read-string "Enter a new tab name: ")))
    (rename-buffer (concat buffer-prefix "%" newname))))

(defun-tco equake-mode-line (modelinestring buffers)
  "Content of MODELINESTRING for equake (show tabs), showing BUFFERS."
  (let ((curtab (car (cdr buffers))))
    (if (equal curtab 'nil)
        (list modelinestring (equake-shell-type-styling major-mode))                        ; when tabs exhausted, return modelinestring
      (setq modelinestring (concat modelinestring (equake-extract-format-tab-name curtab))) ; get name/number for tab in mode-line format
      (equake-mode-line modelinestring (cdr buffers)))))                                    ; go on to next tab

(defun equake-shell-type-styling (mode)
  "Style the shell-type indicator as per MODE."
  (cond ((equal (format "%s" mode) "eshell-mode")
         (propertize " ((eshell)) " 'font-lock-face `(:foreground ,equake-shell-type-eshell-foreground :background ,equake-shell-type-eshell-background)))
        ((equal (format "%s" mode) "term-mode")
         (propertize " ((term)) " 'font-lock-face `(:foreground ,equake-shell-type-term-foreground :background ,equake-shell-type-term-background)))
        ((equal (format "%s" mode) "shell-mode")
         (propertize " ((shell)) " 'font-lock-face `(:foreground ,equake-shell-type-shell-foreground :background ,equake-shell-type-shell-background)))))

(defun equake-extract-format-tab-name  (tab)
  "Extract Equake TAB name and format it for the modeline."
  (let* ((monitor (equake-get-monitor-name))
         (current-etab  (string-to-number (string-remove-prefix (concat "EQUAKE[" monitor "]") (buffer-name (current-buffer)))))
         (etab-name (string-remove-prefix (concat "EQUAKE[" monitor "]" (number-to-string tab) "%") (buffer-name (equake-find-buffer-by-monitor-and-tabnumber monitor tab (buffer-list)))))) ; find the name of the tab
    (when (equal etab-name "")                     ; if the name is null string
      (setq etab-name (number-to-string tab)))   ; set name to tab number
    (if (equal tab current-etab)
        (concat " " (propertize (concat "[ " etab-name " ]") 'font-lock-face `(:foreground ,equake-inactive-tab-foreground-colour :background ,equake-inactive-tab-background-colour)) " ") ; 'highlight' current tab
      (concat " " (propertize  (concat "[ " etab-name " ]") 'font-lock-face `(:foreground ,equake-active-tab-foreground-colour :background ,equake-active-tab-background-colour)) " "))))

(provide 'equake)

;;; equake.el ends here
