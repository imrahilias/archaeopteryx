#!/bin/zsh

# get primary selection
url=`xclip -out -selection primary`

echo $url

# start in browser
chromium "https://www.thesaurus.com/browse/$url"
