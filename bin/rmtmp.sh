#!/bin/zsh

find . \( -name "#*#" -o -name "*~" \) -print
echo -n "remove? [y/N] "
read answer

if [ "$answer" == "y" ]; then
    find . \( -name "#*#" -o -name "*~" \) -print0 | xargs -0 rm
    echo "done"
else
    echo "NOT deleting anything"
fi
