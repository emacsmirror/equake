;;; equake.el --- drop-drop console for eshell & terminal emulation -*- lexical-binding: t; -*-

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
;; Package-Version: 0.90
;; Version: 0.90
;; Package-Requires: ((emacs "25") (dash "2.14.1") (tco "20190309.55"))
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

;;; Installation:
;; To install manually, clone the git repo somewhere and put it in your
;; load-path, e.g., add something like this to your init.el:
;; (add-to-list 'load-path
;;             "~/.emacs.d/equake/")
;;  (require 'equake)

;;; Usage:
;; Run with:---
;; emacsclient -n -e '(equake-invoke)' ,
;; after launching an Emacs daemon of course.
;;
;; For multimonitor use using X11, you can set
;; (setq equake-use-xdotool-probe 't) to use xdotool to
;; automatically detect which screen the Equake frame should open on.
;;
;; Alternatively, on a non-X11 multi-monitor setup, launch:
;; emacsclient -n -c -e '(equake-invoke)' -F '((title . "*transient*") (alpha . (0 . 0)) (width . (text-pixels . 0)) (height . (text-pixels . 0)))'
;; (although this may be noticably slower)
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
;; have no titlebar or frame. It's most throughly tested with
;; KDE Plasma 5 & StumpWM, but should be able to be made to work
;; well with most DEs/WMs (I welcome notes on other environments).
;; 
;;; In Stumpwm:
;; add the following to your .stumpwmrc or ~/.stumpwm.d/init.lisp or other
;; initialisation file (please adjust your mouse focus policy as below to
;; get optimal Equake behaviour:
;; ;; BEGIN COMMON LISP HERE;;
;; (defun calc-equake-width ()
;;   (let ((screen-width (caddr (with-input-from-stringp (s (run-shell-command (concat emacsclient-launch " -n -e '(equake-find-workarea-of-current-screen (equake-calculate-mouse-location (display-monitor-attributes-list)) (display-monitor-attributes-list))'") t)) (read s))))
;;         (desired-width-perc (read-from-string (run-shell-command (concat emacsclient-location " -n -e 'equake-size-width'") t))))
;;     (truncate (* screen-width desired-width-perc))))

;; (defun calc-equake-height ()
;;   (let ((screen-height (cadddr (with-input-from-string (s (run-shell-command (concat emacsclient-location " -n -e '(equake-find-workarea-of-current-screen (equake-calculate-mouse-location (display-monitor-attributes-list)) (display-monitor-attributes-list))'") t)) (read s))))
;;         (desired-height-perc (read-from-string (run-shell-command (concat emacsclient-location " -n -e 'equake-size-height'") t))))
;;     (truncate (* screen-height desired-height-perc))))

;; (setf *equake-width* 1368)
;; (setf *equake-height* 768)

;; (defcommand invoke-equake () ()
;;   "Raise/lower Equake drop-down console."
;;   (let* ((on-top-windows (group-on-top-windows (current-group)))
;;          (equake-on-top (find-equake-in-group on-top-windows)))
;;     (if equake-on-top
;;         (progn (setf (group-on-top-windows (current-group)) (remove equake-on-top on-top-windows))
;;                (hide-window equake-on-top)) ;; then hide Equake window via native Stumpwm method.)
;;       (let ((found-equake (find-equake-globally (screen-groups (current-screen))))) ; Otherwise, search all groups of current screen for Equake window:
;;         (if (not found-equake)          ; If Equake cannot be found,
;;             (progn
;;               (run-shell-command (concat emacsclient-location " -n -e '(equake-invoke)'")) ; then invoke Equake via emacs function.
;;               (setf *equake-height* (calc-equake-height)) ; delay calculation of height & width setting until 1st time equake invoked
;;               (setf *equake-width* (calc-equake-width))) ; (otherwise Emacs may not be fully loaded)
;;             ;; (toggle-always-on-top))   ; make on top
;;             (progn (raise-window found-equake)
;;                    (unless (equalp (current-group) (window-group found-equake)) ; But if Equake window is found, and if it's in a different group
;;                      (move-window-to-group found-equake (current-group)))   ; move it to the current group,
;;                    (unhide-window found-equake) ; unhide window, in case hidden
;;                    (float-window found-equake (current-group)) ; float window
;;                    (float-window-move-resize (find-equake-globally (screen-groups (current-screen))) :width *equake-width* :height *equake-height*) ; set size
;;                    (focus-window found-equake)
;;                    (toggle-always-on-top))))))) ; make on top

;; (defun find-equake-in-group (windows-list) 
;;   "Search through WINDOWS-LIST, i.e. all windows of a group, for an Equake window. Sub-component of '#find-equake-globally."
;;   (let ((current-searched-window (car windows-list)))
;;     (if (equal current-searched-window 'nil)
;;         'nil
;;         (if (search "*EQUAKE*[" (window-name current-searched-window))
;;             current-searched-window
;;             (find-equake-in-group (cdr windows-list))))))

;; (defun find-equake-globally (group-list)
;;   "Recursively search through GROUP-LIST, a list of all groups on current screen, for an Equake window."
;;   (if (equal (car group-list) 'nil)
;;       'nil
;;       (let ((equake-window (find-equake-in-group (list-windows (car group-list)))))
;;         (if equake-window
;;             equake-window               ; stop if found and return window
;;             (find-equake-globally (cdr group-list))))))

;; ;; Set the mouse focus policy; 
;; (setf *mouse-focus-policy* :click) ;; options: :click, :ignore, :sloppy
;; ;; END COMMON LISP HERE;;
;; And add an appropriate keybinding to your stumpwm init to toggle, e.g.:
;; (define-key *top-map* (kbd "F12") "invoke-equake")
;; 
;;; In KDE Plasma 5:
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
;;; Other environments:
;; In awesomewm, probably adding to your 'Rules' something
;; like this:
;; 
;; { rule = { instance = "*EQUAKE*", class = "Emacs" },
;;    properties = { titlebars_enabled = false } },
;; 
;;; Advice:
;; add (global-set-key (kbd "C-x C-c") 'equake-check-if-in-equake-frame-before-closing)
;; to your settings to prevent accidental closure of equake frames

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'subr-x)
(require 'dash)                         ; for -let*
(require 'tco)                          ; tail-call optimisation

;;;###autoload
(define-minor-mode equake-mode
  "Minor mode for drop-down consoles for eshell and terminal emulation."
  :lighter " equake"
  :keymap (let ((map (make-sparse-keymap)))
            map))

(define-minor-mode rash-mode
  "Minor mode for drop-down consoles for rash console."
  :lighter " rash"
  :keymap (let ((map (make-sparse-keymap)))
            map))

(setq equake-rash-installed
      (and (executable-find "raco")
           (not (equal " [none]" (shell-command-to-string "printf \"$(raco pkg show rash | tail -1)\"")))))

(defvar equake-tab-list ()
  "List of Equake tabs, sorted by numerical identifier and screen.")

(defvar equake-last-buffer-list ()
  "A list of the last visited buffers for Equake frames.")

(defvar equake-last-etab-list ()
  "A list of last visited Equake tab for Equake frames.")

(defvar equake-win-history ()
  "A list of buffer history for Equake frames.")

(defgroup equake ()
  "Equake, a drop-down console for eshell and terminal emulation."
  :group 'shell)

(defgroup equake-bindings ()
  "Keybindings for Equake drop-down console. "
  :group 'equake)

(defcustom equake-inhibit-message-choice nil
  "Whether or not messages are displayed in the echo area."
  :group 'equake
  :type 'boolean)

(defcustom equake-hide-from-taskbar-choice t
  "Whether or not to hide Equake from taskbar (may not work in all DEs or WMs)."
  :group 'equake
  :type 'boolean)

(add-hook 'equake-mode-hook #'equake-inhibit-message-locally)

(defun equake-inhibit-message-locally ()
  "Set `inhibit-message' buffer-locally."
  (if equake-inhibit-message-choice
      (setq-local inhibit-message t)
    (setq-local inhibit-message nil)))

(defcustom equake-new-tab-binding "C-+"
  "Keybinding for creating new Equake tab using default shell."
  :type 'string
  :set (lambda (sym defs)
         (custom-set-default sym defs)
         (define-key equake-mode-map (kbd equake-new-tab-binding) 'equake-new-tab))
  :group 'equake-bindings)

(defcustom equake-new-tab-different-shell-binding "C-M-+"
  "Keybinding for creating new Equake tab with ad hoc specified shell."
  :type 'string
  :set (lambda (sym defs)
         (custom-set-default sym defs)
         (define-key equake-mode-map (kbd equake-new-tab-different-shell-binding) 'equake-new-tab-different-shell))
  :group 'equake-bindings)

(defcustom equake-prev-tab-binding "C-{"
  "Keybinding for switching to previous Equake tab."
  :type 'string
  :set (lambda (sym defs)
         (custom-set-default sym defs)
         (define-key equake-mode-map (kbd equake-prev-tab-binding) 'equake-prev-tab))
  :group 'equake-bindings)

(defcustom equake-next-tab-binding "C-}"
  "Keybinding for switching to next Equake tab."
  :type 'string
  :set (lambda (sym defs)
         (custom-set-default sym defs)
         (define-key equake-mode-map (kbd equake-next-tab-binding) 'equake-next-tab))
  :group 'equake-bindings)

(defcustom equake-move-tab-left-binding "C-M-{"
  "Keybinding for moving current Equake tab left."
  :type 'string
  :set (lambda (sym defs)
         (custom-set-default sym defs)
         (define-key equake-mode-map (kbd equake-move-tab-left-binding) 'equake-move-tab-left))
  :group 'equake-bindings)

(defcustom equake-move-tab-right-binding "C-M-}"
  "Keybinding for moving current Equake tab right."
  :type 'string
  :set (lambda (sym defs)
         (custom-set-default sym defs)
         (define-key equake-mode-map (kbd equake-move-tab-right-binding) 'equake-move-tab-right))
  :group 'equake-bindings)

(defcustom equake-rename-etab-binding "C-|"
  "Keybinding for renaming current Equake tab."
  :type 'string
  :set (lambda (sym defs)
         (custom-set-default sym defs)
         (define-key equake-mode-map (kbd equake-rename-etab-binding) 'equake-rename-etab))
  :group 'equake-bindings)

(defcustom equake-available-shells
  '("eshell"
    "vterm"
    "rash"
    "ansi-term"
    "term"
    "shell")
  "List of available `shell' modes for Equake."
  :group 'equake
  :type 'sexp)

(defcustom equake-size-width 1.0
  "Fraction (.01-1.0) for width of Equake console."
  :type 'float
  :group 'equake)

(defcustom equake-size-height 0.4
  "Fraction (.01-1.0) for height of Equake console."
  :type 'float
  :group 'equake)

(defcustom equake-opacity-active 75
  "Amount of opacity of Equake console when active."
  :type 'integer
  :group 'equake)

(defcustom equake-opacity-inactive 60
  "Amount of opacity of Equake console when inactive."
  :type 'integer
  :group 'equake)

(defcustom equake-default-shell 'eshell
  "Default shell used by Equake, choices are `eshell', `vterm', `rash', `ansi-term', `term', `shell'."
  :type  'symbol
  :group 'equake)

(defcustom equake-default-sh-command ""
  "Default shell command used by `(ansi-)term' in Equake tab.
If set to empty string, Equake will fall back to system's default SHELL
environment variable."
  :type 'string
  :group 'equake)

(defcustom equake-show-monitor-in-mode-line 'nil
  "Toggle to show monitor id string as part of Equake mode-line."
  :type 'boolean
  :group 'equake)

(defcustom equake-use-frame-hide 't
  "Hide frames rather than destroying frames."
  :type 'boolean
  :group 'equake)

(defcustom equake-use-xdotool-probe 'nil
  "Use xdotool to probe for mouse location.
Only works on X11 systems; unnecessary for single-screen setups."
  :type 'boolean
  :group 'equake)

(defgroup equake-faces nil
  "Faces for the Equake drop-down console."
  :group 'equake
  :group 'faces)

(defface equake-buffer-face
  '((t (:inherit default)))
  "Face used for internal Equake buffer text, including font typeface and
background colour."
  :group 'equake-faces)

(defface equake-tab-inactive
  '((t (:background "black" :foreground "lightblue")))
  "Face used for inactive Equake tabs in the mode-line."
  :group 'equake-faces)

(defface equake-tab-active
  '((t (:background "lightblue" :foreground "black")))
  "Face used for active Equake tabs in the mode-line."
  :group 'equake-faces)

(defface equake-shell-type-eshell
  '((t (:background "midnight blue" :foreground "spring green")))
  "Face used for indicating `eshell' shell type in the mode-line."
  :group 'equake-faces)

(defface equake-shell-type-vterm
  '((t (:background "midnight blue" :foreground "lightsteelblue")))
  "Face used for indicating `vterm' shell type in the mode-line."
  :group 'equake-faces)

(defface equake-shell-type-term
  '((t (:background "midnight blue" :foreground "gold")))
  "Face used for indicating `(ansi-)term' shell type in the mode-line."
  :group 'equake-faces)

(defface equake-shell-type-rash
  '((t (:background "midnight blue" :foreground "red")))
  "Face used for indicating `(ansi-)term' shell type in the mode-line."
  :group 'equake-faces)

(defface equake-shell-type-shell
  '((t (:background "midnight blue" :foreground "cyan")))
  "Face used for indicating (inferior) `shell' shell type in the mode-line."
  :group 'equake-faces)

(defun equake-ask-before-closing-equake ()
  "Make sure user really wants to close Equake, ask again."
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to close the equake console frame? \n[Advice: Cancel and use `C-x k` to close the buffer or invoke 'bury-buffer' instead, returning to your shell session.]? "))
      (save-buffers-kill-terminal)
    (print "Wisely cancelling the closing of the equake console frame...")))

(defun equake-check-if-in-equake-frame-before-closing ()
  "Check if we're in an Equake frame."
  (interactive)
  (if (cl-search "*EQUAKE*[" (frame-parameter (selected-frame) 'name))
      (equake-ask-before-closing-equake)
    (save-buffers-kill-terminal)))

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
        ;; if monitor lacks name property, fallback to using `geometry' as unique(?) identifier
        (let ((name (equake-get-monitor-property "geometry" (frame-monitor-attributes))))
          (when name
            (format "%s" name))))));)

(defun equake-calculate-mouse-location (displaylist)
  "Use the xdotool to get the mouse location of DISPLAYLIST.
And check #'display-monitor-attributes-list to see which screen/monitor to
raise/lower equake on."
  (let* ((test-var-x (shell-command-to-string "xdotool getmouselocation --shell | head -n 1"))
         (test-var-y (shell-command-to-string "xdotool getmouselocation --shell | head -n 2 |tail -n 1"))
         (x-mouse-pos (string-to-number (substring test-var-x 2 (length test-var-x))))
         (y-mouse-pos (string-to-number (substring test-var-y 2 (length test-var-y)))))
    (if displaylist
        (let* ((mon-geometry (cadar displaylist))
               (x-limit (second mon-geometry))
               (x-size (fourth mon-geometry))
               (y-limit (third mon-geometry))
               (y-size (fifth mon-geometry)))
          (if (and (> (+ x-limit x-size) x-mouse-pos) (< x-limit x-mouse-pos) (> (+ y-limit y-size) y-mouse-pos))
              (cdaar displaylist)
            (equake-calculate-mouse-location (cdr displaylist))))
      (error "Mouse pointer location out of range"))))

(defun-tco equake-kill-stray-transient-frames (frames)
  "Destroy any stray transient FRAMES."
  (interactive)
  (let ((frame (car frames)))
    (when frame
      (when (cl-search "*transient*" (frame-parameter frame 'title))
        (delete-frame frame))
      (equake-kill-stray-transient-frames (cdr frames)))))

(defun equake-find-workarea-of-current-screen (monitorid mlist)
  "Find the workarea of the currently active screen MONITORID given a list of screens MLIST."
  (if mlist
      (if (equal (cdaar mlist) monitorid)
          (alist-get 'workarea (car mlist))
        (equake-find-workarea-of-current-screen monitorid (cdr mlist)))))

(defun equake-record-history (equake-current-frame)
  "Store the EQUAKE-CURRENT-FRAME for easier recovery of destroyed equake frames."
  (select-frame equake-current-frame)
  (when (equal (buffer-name (current-buffer)) " *server*") ; if opened to " *server*" buffer
    (switch-to-buffer (other-buffer (current-buffer) 1))) ; switch to other buffer
  (equake-set-last-buffer)
  (equake-set-winhistory)
  (when (buffer-local-value equake-mode (current-buffer))
    (equake-set-last-etab)))

(defun equake-hide-from-taskbar ()
  "Hide Equake from the taskbar."
  (let ((monitor-name (equake-get-monitor-name)))
    (shell-command (concat "xprop -name "
                           "*EQUAKE*[" monitor-name "]"
                           " -f _NET_WM_STATE 32a -set _NET_WM_STATE _NET_WM_STATE_SKIP_TASKBAR"))))

(defun equake-invoke ()
  "Toggle Equake frames.
Run with \"emacsclient -n -e '(equake-invoke)'\".
On multi-monitor set-ups, run instead \"emacsclient -n -c -e '(equake-invoke)' -F '((title . \"*transient*\") (alpha . (0 . 0)) (width . (text-pixels . 0)) (height . (text-pixels . 0)))'\"."
  (interactive)
  (-let* ((monitorid (if equake-use-xdotool-probe
                         (equake-calculate-mouse-location (display-monitor-attributes-list))
                       (equake-get-monitor-name)))
          (equake-current-frame (equake-equake-frame-p monitorid (frame-list)))
          (target-workarea (if equake-use-xdotool-probe
                               (equake-find-workarea-of-current-screen monitorid (display-monitor-attributes-list))
                             (alist-get 'workarea (frame-monitor-attributes equake-current-frame))))
          ((mon-xpos mon-ypos monwidth monheight) (mapcar #'floor target-workarea))
          (mod-mon-xpos (floor (+ mon-xpos (/ (- monwidth (* monwidth equake-size-width)) 2)))))
    (if equake-current-frame ; if frame exists, hide or destroy it (depending on user options).
        (if equake-use-frame-hide ; if user has make-frame-(in)visible option set
            (if (frame-visible-p equake-current-frame)
                (progn (equake-record-history equake-current-frame) ; record history
                       (make-frame-invisible equake-current-frame) (make-frame-invisible equake-current-frame)) ; double-tap, otherwise frame lands in limbo
              (make-frame-visible equake-current-frame)) ; make visible visible if invisible.
          (equake-record-history equake-current-frame)   ; record history
          (delete-frame equake-current-frame))           ; destroy frame.
      ;; else, make it.
      (-let* ((new-frame (make-frame (list (cons 'name (concat "*EQUAKE*[" monitorid "]"))
                                           (cons 'alpha `(,equake-opacity-active ,equake-opacity-inactive))
                                           (cons 'left mod-mon-xpos)
                                           (cons 'top mon-ypos)
                                           (cons 'width (cons 'text-pixels (truncate (* monwidth equake-size-width))))
                                           (cons 'height (cons 'text-pixels (truncate (* monheight equake-size-height))))))))
        (select-frame new-frame)
        (set-window-prev-buffers nil (cdr (equake-find-monitor-list monitorid equake-win-history)))
        (let ((highest-montab (equake-highest-etab monitorid (buffer-list) -1)))
          (if (< highest-montab 0) ; if no extant Equake tabs on current monitor,
              (equake-new-tab)     ; then launch new shell.
            (switch-to-buffer (cdr (equake-find-monitor-list monitorid equake-last-etab-list))) ; else, restore last Equake tab
            (switch-to-buffer (cdr (equake-find-monitor-list monitorid equake-last-buffer-list))) ; and then restore last buffer used in frame.
            (when (not (cl-search (concat "EQUAKE[" monitorid) (buffer-name (current-buffer)))) ; make sure to restore to an Equake buffer
              (bury-buffer)))
          (equake-set-up-equake-frame) ; set-up frame
          (when equake-hide-from-taskbar-choice
            (equake-hide-from-taskbar))) ; hide equake from taskbar 
        (set-window-prev-buffers nil (equake-filter-history (window-prev-buffers) (window-prev-buffers))))) ; filter out irrelevant buffers.
    (unless (equal equake-use-xdotool-probe 't) ; if not using xdotool-probe
      (equake-kill-stray-transient-frames (frame-list))))) ; get rid of any Equake multi-monitor launcher `probes'.

(defun-tco equake-filter-history (winhist filtwinhist)
  "Filter window history (WINHIST) into FILTWINHIST."
  (if (not winhist)
      filtwinhist
    (if (listp (car winhist))
        (unless (buffer-local-value equake-mode (caar winhist))
          (setq filtwinhist (remove (caar winhist) filtwinhist)))
      (if (bufferp (car winhist))
          (unless (buffer-local-value equake-mode (car winhist))
            (setq filtwinhist (remove (car winhist) filtwinhist)))
        (setq filtwinhist (remove (car winhist) filtwinhist)))
      (equake-filter-history (cdr winhist) filtwinhist))))

(defun equake-launch-shell (launchshell)
  "Launch a new shell session, OVERRIDE will set non-default shell."
  (interactive)
  (let ((sh-command equake-default-sh-command)
        (success 't))
    (when (equal sh-command "")
      (setq sh-command shell-file-name))
    (cond ((equal launchshell 'eshell)
           (eshell 'N))
          ((equal launchshell 'vterm)
           (if (require 'vterm nil 'noerror)
               (vterm)
             (setq success 'nil)))
          ((equal launchshell 'rash)
           (if (not equake-rash-installed)
               (setq success 'nil)
               (if (require 'vterm nil 'noerror)
                   (vterm)
                 (shell)
                 (delete-other-windows))
             (rash-mode)))
          ((equal launchshell 'ansi-term)
           (ansi-term sh-command))
          ((equal launchshell 'term)
           (term sh-command))
          ((equal launchshell 'shell)
           (shell)
           (delete-other-windows))
          ('t (setq success 'nil)))
    success))

(defun equake-set-up-equake-frame ()
  "Set-up new *EQUAKE* frame, including cosmetic alterations."
  (interactive)
  (buffer-face-set 'equake-buffer-face)
  (set-frame-parameter (selected-frame) 'alpha `(,equake-opacity-active ,equake-opacity-inactive)))

(defun-tco equake-count-tabs (monitor buffers count)
  "COUNT current Equake tab BUFFERS on MONITOR."
  (let ((buffbeg (car buffers))
        (buffend (cdr buffers)))
    (cond ((equal buffbeg 'nil)
           count)
          ((string-match-p (concat "EQUAKE\\[" monitor) (buffer-name buffbeg))
           (setq count (1+ count))
           (equake-count-tabs monitor buffend count))
          (t (equake-count-tabs monitor buffend count)))))

(defun-tco equake-highest-etab (monitor buffers highest)
  "Get HIGHEST etab number among BUFFERS on MONITOR."
  (let ((buffbeg (car buffers))
        (buffend (cdr buffers)))
    (cond ((equal buffbeg 'nil)
           highest)
          ((string-match-p (concat "EQUAKE\\[" monitor) (buffer-name buffbeg)) ; only consider relevant buffers
           (when (> (string-to-number (replace-regexp-in-string "%[[:alnum:]]*" "" (string-remove-prefix (concat "EQUAKE[" monitor "]") (buffer-name buffbeg)))) highest)
             (setq highest (string-to-number (replace-regexp-in-string "%[[:alnum:]]*" "" (string-remove-prefix (concat "EQUAKE[" monitor "]") (buffer-name buffbeg))))))
           (equake-highest-etab monitor buffend highest))
          (t (equake-highest-etab monitor buffend highest)))))

(defun-tco equake-lowest-etab (monitor buffers lowest)
  "Get LOWEST etab number among BUFFERS on MONITOR."
  (let ((buffbeg (car buffers))
        (buffend (cdr buffers)))
    (cond ((equal buffbeg 'nil)
           lowest)
          ((string-match-p (concat "EQUAKE\\[" monitor) (buffer-name buffbeg)) ; only consider relevant buffers
           (when (< (string-to-number (replace-regexp-in-string "%[[:alnum:]]*" "" (string-remove-prefix (concat "EQUAKE[" monitor "]") (buffer-name buffbeg)))) lowest)
             (setq lowest (string-to-number (replace-regexp-in-string "%[[:alnum:]]*" "" (string-remove-prefix (concat "EQUAKE[" monitor "]") (buffer-name buffbeg))))))
           (equake-lowest-etab monitor buffend lowest))
          (t (equake-lowest-etab monitor buffend lowest)))))

(defun equake-new-tab-different-shell ()
  "Open a new shell tab, but using a shell different from the default."
  (interactive)
    (equake-new-tab (intern (message "%s" (ido-completing-read "Choose shell:" equake-available-shells 'nil 't 'nil 'nil "eshell")))))

(defun equake-new-tab (&optional override)
  "Open a new shell tab on monitor, optionally OVERRIDE default shell."
  (interactive)
  (let ((launchshell (or override equake-default-shell)))
    (if (not (equake-launch-shell launchshell))
        (progn (if inhibit-message
                   (progn (setq-local inhibit-message 'nil)
                          (message "No such shell or relevant shell not installed.")
                          (setq-local inhibit-message 't))
                 (message "No such shell or relevant shell not installed.")))
      (buffer-face-set 'equake-buffer-face)
      (let* ((monitor (equake-get-monitor-name))
             (newhighest (1+ (equake-highest-etab monitor (buffer-list) -1))) ; figure out number to be set for the new tab for the current monitor
             (cur-monitor-tab-list (equake-find-monitor-list monitor equake-tab-list))
             (newbuffname (concat "EQUAKE[" monitor "]" (number-to-string newhighest) "%"))) ; find the tab-list associated with the current monitor
        (rename-buffer newbuffname) ; rename buffer with monitor id and new tab number
        (when (equal rash-mode 't)
          (comint-send-string newbuffname "racket -l rash/repl --\n"))
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
        (equake-mode)                         ; set Equake minor mode for buffer
        (force-mode-line-update)))))

(defun-tco equake-find-monitor-list (monitor tabs)
  "Return the relevant list member associated with MONITOR/screen amongst TABS."
  (unless (equal tabs 'nil)
    (if (equal monitor (caar tabs))
        (car tabs)
      (equake-find-monitor-list monitor (cdr tabs)))))

(defun equake-shell-after-buffer-change-hook ()
  "Things to do when in Equake when the current buffer is changed."
  (let ((monitorid (equake-get-monitor-name)))
    (when (cl-search (concat "EQUAKE[" monitorid) (buffer-name (current-buffer)))
      ;; get monitor-local list of buffers and send it to be processed for the mode-line
      (if equake-show-monitor-in-mode-line ; show monitorid or not
          (setq mode-line-format (list (equake-mode-line (concat monitorid ": ") (equake-find-monitor-list monitorid equake-tab-list))))
        (setq mode-line-format (list (equake-mode-line "" (equake-find-monitor-list monitorid equake-tab-list)))))
      (force-mode-line-update)
      (modify-frame-parameters (selected-frame) '((vertical-scroll-bars . nil) (horizontal-scroll-bars . nil))) ; no scroll-bars
      (when (cl-search "*EQUAKE*[" (frame-parameter (selected-frame) 'name)) ; if we're in an equake frame,
        (set-frame-parameter (selected-frame) 'menu-bar-lines 0) ; no menu-bar
        (set-frame-parameter (selected-frame) 'tool-bar-lines 0)) ; no tool-bar
      (equake-set-last-etab)
      (equake-set-winhistory))))

(add-hook 'buffer-list-update-hook #'equake-shell-after-buffer-change-hook)

(defun equake-set-last-buffer ()
  "Set last seen buffer for Equake frame."
  (let* ((monitorid (equake-get-monitor-name))
         (cur-monitor-last-buffer (equake-find-monitor-list monitorid equake-last-buffer-list)))
    (if equake-last-buffer-list
        (if cur-monitor-last-buffer
            (progn (setq equake-last-buffer-list (remove cur-monitor-last-buffer equake-last-buffer-list)) ; remove old monitor tab-list member from current tabs
                   (setq equake-last-buffer-list (append equake-last-buffer-list (list (cons monitorid (current-buffer))))))
          (setq equake-last-buffer-list (append equake-last-buffer-list (list (cons monitorid (current-buffer))))))
      (setq equake-last-buffer-list (list (cons monitorid (current-buffer)))))))

(defun equake-set-last-etab ()
  "Set last seen actual Equake tab for Equake frame."
  (let* ((monitorid (equake-get-monitor-name))
         (cur-monitor-last-buffer (equake-find-monitor-list monitorid equake-last-etab-list)))
    (if equake-last-etab-list
        (if cur-monitor-last-buffer
            (progn (setq equake-last-etab-list (remove cur-monitor-last-buffer equake-last-etab-list)) ; remove old monitor tab-list member from current tabs
                   (setq equake-last-etab-list (append equake-last-etab-list (list (cons monitorid (current-buffer))))))
          (setq equake-last-etab-list (append equake-last-etab-list (list (cons monitorid (current-buffer))))))
      (setq equake-last-etab-list (list (cons monitorid (current-buffer)))))))

(defun equake-set-winhistory ()
  "Remember the buffer history for window (i.e. for the Equake frame)."
  (let* ((monitorid (equake-get-monitor-name))
         (cur-monitor-last-etab (equake-find-monitor-list monitorid equake-win-history)))
    (if equake-win-history
        (progn (when cur-monitor-last-etab
                 (setq equake-win-history (remove cur-monitor-last-etab equake-win-history))) ; remove old monitor tab-list member from current tabs
               (setq equake-win-history (append equake-win-history (list (cons monitorid (window-prev-buffers))))))
      (setq equake-win-history (list (cons monitorid (window-prev-buffers)))))))

(defun equake-kill-etab-buffer-hook ()
  "Things to do when an Equake buffer is killed." ; TODO: prevent last equake tab from being killed?
  (when (buffer-local-value equake-mode (current-buffer))
    (let* ((monitor (substring (buffer-name) (1+ (cl-search "[" (buffer-name))) (cl-search "]" (buffer-name))))
           (killed-tab (string-to-number (substring (buffer-name) (1+ (cl-search "]" (buffer-name))) (length (buffer-name)))))
           (cur-monitor-tab-list (equake-find-monitor-list monitor equake-tab-list)))
      (when (cl-search "*EQUAKE*[" (frame-parameter (selected-frame) 'name)) ; if we're in an equake frame,
        (if (equake-find-next-etab (cdr (equake-find-monitor-list monitor equake-tab-list)) killed-tab) ; switch to the next etab, if if exists
            (switch-to-buffer (equake-find-buffer-by-monitor-and-tabnumber monitor (equake-find-next-etab (cdr (equake-find-monitor-list monitor equake-tab-list)) killed-tab) (buffer-list)))
          (switch-to-buffer (equake-find-buffer-by-monitor-and-tabnumber monitor (cadr (equake-find-monitor-list monitor equake-tab-list)) (buffer-list))))) ;otherwise switch to last
      (setq equake-tab-list (remove cur-monitor-tab-list equake-tab-list)) ; remove old monitor tab-list member from global tab list
      (setq cur-monitor-tab-list (cons (car cur-monitor-tab-list) (remove killed-tab (cdr cur-monitor-tab-list)))) ; edit current monitor tab list to remove tab
      (setq equake-tab-list (append equake-tab-list (list cur-monitor-tab-list))) ; add edited current monitor tab list back to global tab list
      (when (cl-search (concat "EQUAKE[" monitor) (buffer-name (current-buffer)))
        (setq mode-line-format (list (equake-mode-line "" cur-monitor-tab-list)))
        (force-mode-line-update)))))

(add-hook 'kill-buffer-hook #'equake-kill-etab-buffer-hook)

(defun-tco equake-find-next-etab (tablist tab)
  "Return the next TAB from TABLIST."
  (cond ((equal (car tablist) 'nil)  ; return 'nil if we're at the end of the list
         'nil)
        ((equal (car tablist) tab)  ; if we find the current tab, cddr the list to get the next tab
         (cadr tablist))
        (t (equake-find-next-etab (cdr tablist) tab)))) ; if not, then cdr the list and test again

(defun-tco equake-move-tab (monitor tablist moving-tab direction)
  "Move the current MOVING-TAB in the TABLIST on MONITOR one position to the right (DIRECTION=1) or left (DIRECTION=-1) in the TABLIST."
  (let* ((examined-tab (car tablist))   ; get entire local monitor tab-list
         (monitor-tab-list (cdr examined-tab))) ; just the local monitor list of tabs
    (if (not tablist)
        (error (concat "Error: no relevant monitor " monitor "."))   ; error if for some reason no such-named monitor
      (if (equal (car examined-tab) monitor) ; if the monitor label matches
          (if (< (length monitor-tab-list) 2) ; if only one tab, stop with message to user
              (print "Only one tab exists.")
            (let* ((orig-pos (cl-position moving-tab monitor-tab-list)) ; find the original position of tab we're moving
                   (target-pos (+ orig-pos direction)) ; find the target position of the moving tab
                   (reconstructed-local-tab 'nil))     ; initialise local variable
              (cond ((< target-pos 0) ; if trying to move tab beyond left edge
                     (setq monitor-tab-list (remove moving-tab monitor-tab-list)) ; delete tab
                     (setq monitor-tab-list (append monitor-tab-list (list moving-tab)))) ; add tab to right edge
                    ((> target-pos (1- (length monitor-tab-list))) ; if trying to move tab beyond right edge
                     (setq monitor-tab-list (remove moving-tab monitor-tab-list)) ; delete tab
                     (setq monitor-tab-list (append (list moving-tab) monitor-tab-list))) ; add tab to left edge
                    (t (let ((target-temp-id (nth target-pos monitor-tab-list))) ; else, store the original content of target position
                         (setf (nth target-pos monitor-tab-list) moving-tab (nth orig-pos monitor-tab-list) target-temp-id )))) ; modify the list positions accordingly)))
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
    (equake-move-tab monitorid equake-tab-list current-etab 1) ; call general tab move function
    (equake-set-winhistory)))

(defun equake-move-tab-left ()
  "Move current tab one position to the left."
  (interactive)
  (let* ((monitorid (equake-get-monitor-name))
         (current-etab (string-to-number (replace-regexp-in-string "%[[:alnum:]]*" "" (string-remove-prefix (concat "EQUAKE[" monitorid "]") (buffer-name (current-buffer)))))))
    (equake-move-tab monitorid equake-tab-list current-etab -1) ; call general tab move function
    (equake-set-winhistory)))

(defun equake-next-tab ()
  "Switch to the next tab."
  (interactive)
  (let ((monitorid (equake-get-monitor-name)))
    (if (< (equake-count-tabs monitorid (buffer-list) 0) 2)
        (print "No other tab to switch to.")
      (let* ((current-tab (string-to-number (substring (buffer-name) (1+ (cl-search "]" (buffer-name))) (1+ (cl-search "%" (buffer-name))))))
             (next-tab (equake-find-next-etab (cdr (equake-find-monitor-list monitorid equake-tab-list)) current-tab)))
        (if (equal next-tab 'nil)  ; switch to first tab if at end of list
            (switch-to-buffer (equake-find-buffer-by-monitor-and-tabnumber monitorid (cadr (equake-find-monitor-list monitorid equake-tab-list)) (buffer-list)))
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
            (switch-to-buffer (equake-find-buffer-by-monitor-and-tabnumber monitorid (car (reverse (cdr (equake-find-monitor-list monitorid equake-tab-list)))) (buffer-list)))
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
         (buffer-pretty-name (replace-regexp-in-string "\.*%" "" (buffer-name (current-buffer)))) ; strip everything before the '%'
         (newname (read-string "Enter a new tab name: " buffer-pretty-name)))
    (rename-buffer (concat buffer-prefix "%" newname))))

(defun-tco equake-mode-line (modelinestring buffers)
  "Content of MODELINESTRING for equake (show tabs), showing BUFFERS."
  (let ((curtab (cadr buffers)))
    (if (equal curtab 'nil)
        (list modelinestring (equake-shell-type-styling major-mode))                        ; when tabs exhausted, return modelinestring
      (setq modelinestring (concat modelinestring (equake-extract-format-tab-name curtab))) ; get name/number for tab in mode-line format
      (equake-mode-line modelinestring (cdr buffers)))))                                    ; go on to next tab

(defun equake-shell-type-styling (mode)
  "Style the shell-type indicator as per MODE."
  (cond ((equal rash-mode 't)
         (propertize " ((rash)) " 'font-lock-face 'equake-shell-type-rash))
        ((equal (format "%s" mode) "vterm-mode")
         (propertize " ((vterm)) " 'font-lock-face 'equake-shell-type-vterm))
        ((equal (format "%s" mode) "eshell-mode")
         (propertize " ((eshell)) " 'font-lock-face 'equake-shell-type-eshell))
        ((equal (format "%s" mode) "term-mode")
         (propertize " ((term)) " 'font-lock-face 'equake-shell-type-term))
        ((equal (format "%s" mode) "shell-mode")
         (propertize " ((shell)) " 'font-lock-face 'equake-shell-type-shell))))

(defun equake-extract-format-tab-name  (tab)
  "Extract Equake TAB name and format it for the modeline."
  (let* ((monitor (equake-get-monitor-name))
         (current-etab  (string-to-number (string-remove-prefix (concat "EQUAKE[" monitor "]") (buffer-name (current-buffer)))))
         (etab-name (string-remove-prefix (concat "EQUAKE[" monitor "]" (number-to-string tab) "%") (buffer-name (equake-find-buffer-by-monitor-and-tabnumber monitor tab (buffer-list)))))) ; find the name of the tab
    (when (equal etab-name "")                     ; if the name is null string
      (setq etab-name (number-to-string tab)))   ; set name to tab number
    (if (equal tab current-etab)
        (concat " " (propertize (concat "[ " etab-name " ]") 'font-lock-face 'equake-tab-active) " ") ; 'highlight' current tab
      (concat " " (propertize  (concat "[ " etab-name " ]") 'font-lock-face 'equake-tab-inactive) " "))))

(provide 'equake)

;;; equake.el ends here
