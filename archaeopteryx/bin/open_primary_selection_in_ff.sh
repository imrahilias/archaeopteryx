#!/bin/bash

# get primary selection
url=`xclip -out -selection primary`

echo $url

# start in browser
firefox "https://duckduckgo.com/?q=$url"
