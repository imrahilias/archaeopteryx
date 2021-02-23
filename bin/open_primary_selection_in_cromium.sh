#!/bin/zsh

# get primary selection
url=`xclip -out -selection primary`

echo $url

# start in browser
chromium "https://www.google.com/search?q=$url"
