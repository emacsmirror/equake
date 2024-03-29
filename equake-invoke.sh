#!/bin/sh

#
# A utility to converse with emacs equake window.
# by Jeff Kowalski. (see https://gitlab.com/emacsomancer/equake/-/issues/23#note_971632336 )
#
# This script should be bound to a window-manager hotkey (e.g. <F12>).
# Pressing the hotkey in succession will make the equake window appear/disappear.
# We try to be intelligent about bringing a hidden window onto the current
# desktop and into focus.
#

_equake_window_exists () {
    wmctrl -lx 2> /dev/null | grep -F "*EQUAKE*" | awk '{print $1}'
}

_equake_window_focused () {
    xdotool getwindowfocus getwindowname | grep -F "*EQUAKE*"
}

_equake_current_workspace () {
    wmctrl -lx | grep -F "*EQUAKE*" | cut -d ' ' -f3
}

_user_current_workspace () {
    wmctrl -d | grep -F '*' | cut -d ' ' -f1
}

if [ "$(_equake_window_exists)" = "" ]; then
    # don't have the window, so make one and raise it
    # spd-say "creating"
    emacsclient -n -e '(progn (equake-mode t)(equake-invoke))'
    wmctrl -a "*EQUAKE*"
elif [ "$(_equake_window_focused)" = "" ] && [ "$(_equake_current_workspace)" -ne "$(_user_current_workspace)" ]; then
    # have the window but it's not focused, so bring it here and focus it, as long it's not on the current workspace
    # spd-say "focusing"
    wmctrl -R "*EQUAKE*"
else
    # it is focused, so dismiss it
    # spd-say "dismissing"
    emacsclient -n -e '(progn (equake-mode t)(equake-invoke))'
fi
