#!/bin/sh

equakestatus=$(emacsclient -n -e '(frame-live-p (alist-get (equake--get-monitor) equake--frame))')

if [ "$equakestatus" = "nil" ]; then
    emacsclient -c -e "(progn (select-frame-set-input-focus (selected-frame))
                              (equake--transform-existing-frame-into-equake-frame)
                              (goto-char (1- (point-max))))"
else
    emacsclient -n -e '(progn (setq equake-use-frame-hide nil)
                              (equake-invoke))'
fi
