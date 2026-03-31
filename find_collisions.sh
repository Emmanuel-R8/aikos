#!/bin/bash
# Exclude commented lines (starting with ;;) and only match actual defop forms
grep -r "^(defop " laiko/src/vm/*.lisp | grep ":hexcode" | awk -F':hexcode ' '{print $2}' | awk '{print $1}' | sort | uniq -d
