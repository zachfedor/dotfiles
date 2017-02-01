#!/bin/sh
# 
# Track Org Files Automatically With Git

DIRS="org"

for DIR in $DIRS
do
    cd ~/$DIR
    # remove deleted files
    git ls-files --deleted -z | xargs -0 git rm >/dev/null 2>&1
    # add new files
    git add . >/dev/null 2>&1
    git commit -m "$(date)"
done
