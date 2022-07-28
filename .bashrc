#!/bin/false
#blabla

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# The PS1 variable may be unset or empty and the shell may still be
# interactive (but without a prompt).
for i in "$-"
do
    case "$-" in *i*) ;;
                  *) return ;;
    esac
done

#        |_)            
#   _` | | |  _` |  __| 
#  (   | | | (   |\__ \ 
# \__,_|_|_|\__,_|____/ 

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# enable programmable completion features
[ -r /usr/share/bash-completion/bash_completion   ] && . /usr/share/bash-completion/bash_completion

# alias

## list aliasas
alias l='ls -1Bhl --group-directories-first' # '1' one entry/line, 'B' ignores backups (~), 'h' 4 human readable (kiB, MiB, ...), 'n' numeric uid/gid, 'o' like 'l' without group 
alias ll='ls -1ABhl --group-directories-first' # 'A' almost all
alias d='dirs -v' # lists zsh directory stack (enter <cd +- tab>, plus & minus (reverse) literally, with completion!'
alias hist='fc -El 0 | grep '
alias lsa='lsarchives '
alias df='df -h'
alias dfa='df -ah'
alias cp='rsync -aP' # show percentage
alias e='emacs '

#              |  _)                  
#   _ \  __ \  __| |  _ \  __ \   __| 
#  (   | |   | |   | (   | |   |\__ \ 
# \___/  .__/ \__|_|\___/ _|  _|____/ 
#       _|                  

shopt -s autocd # auto enter
shopt -s checkwinsize # check the window size after each command and, if necessary,update the values of LINES and COLUMNS.
shopt -s histappend # append to the history file, don't overwrite it
shopt -s histverify # ask for submission if hist completing

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
HISTCONTROL=$HISTCONTROL${HISTCONTROL+:}ignoredups
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoreboth
# HISTFILE=~/.histfile
# HISTSIZE=10000
# SAVEHIST=10000

# make less more friendly for non-text input files, see lesspipe(1)
#[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

#   _|                  |  _)                  
#  |   |   | __ \   __| __| |  _ \  __ \   __| 
#  __| |   | |   | (    |   | (   | |   |\__ \ 
# _|  \__,_|_|  _|\___|\__|_|\___/ _|  _|____/ 

## This will print the readable ~/ and ./ when starting which from
## your prompt, while still printing the full path when used from a
## script
which ()
{
  (alias; declare -f) | /usr/bin/which --tty-only --read-alias --read-functions --show-tilde --show-dot $@
}
export -f which

## man coloring
man() {
    env LESS_TERMCAP_mb=$'\E[01;31m' \
    LESS_TERMCAP_md=$'\E[01;38;5;74m' \
    LESS_TERMCAP_me=$'\E[0m' \
    LESS_TERMCAP_se=$'\E[0m' \
    LESS_TERMCAP_so=$'\E[38;5;246m' \
    LESS_TERMCAP_ue=$'\E[0m' \
    LESS_TERMCAP_us=$'\E[04;38;5;146m' \
    man "$@"
}


## Alt-l does `ls`
bind -m vi-insert -x '"\el": ls -1Bhl --group-directories-first .'
bind -m emacs -x     '"\el": ls -1Bhl --group-directories-first .'


# Zsh can invoke the manual for the command preceding the cursor by
# pressing Alt+h. A similar behaviour is obtained in Bash using this
# Readline bind:
run-help() { help "$READLINE_LINE" 2>/dev/null || man "$READLINE_LINE"; }
bind -m vi-insert -x '"\eh": run-help'
bind -m emacs -x     '"\eh": run-help'


## cd + ls
function c() {
    if [[ -d "$@" ]]; then
        echo -e "\e[35m""stack:"
        builtin pushd "$@"
        echo -e "\e[0m"
        ls -1Bhl --group-directories-first --color=auto
    else
        builtin cd "$@"
    fi
}
function v() {
    echo -e "\e[35m""stack:"
    builtin popd "$@"
    echo -e "\e[0m"
    ls -1Bhl --group-directories-first --color=auto
}


## quick dir change
rationalize-dot() {
    if [[ $LBUFFER = *.. ]]; then
        LBUFFER+=/..
    else
        LBUFFER+=.
    fi
}
bind -m vi-insert -x '"\ed": rationalize-dot'
# bind -m emacs -x     '".": rationalize-dot'

#                                  |   
#  __ \   __| _ \  __ `__ \  __ \  __| 
#  |   | |   (   | |   |   | |   | |   
#  .__/ _|  \___/ _|  _|  _| .__/ \__| 
# _|                        _|         

# Colors
bb="\[\e[34;1m\]" # bold blue
b="\[\e[34m\]"    # blue
rb="\[\e[31;1m\]" # bold red
r="\[\e[31m\]"    # red
yb="\[\e[33;1m\]" # bold yellow
y="\[\e[33m\]"    # yellow
gb="\[\e[32;1m\]" # bold green
g="\[\e[32m\]"    # green
pb="\[\e[35;1m\]" # bold purple
p="\[\e[35m\]"    # purple
tp="\[\e[36;1m\]" # bold turquoise
t="\[\e[36m\]"    # turquoise
wb="\[\e[37;1m\]" # bold white
w="\[\e[37m\]"    # white
def="\[\e[0m\]"   # default color

# set prompt
PS1="$b\u$w@$t\h$def:\w\$ "
