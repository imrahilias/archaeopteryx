# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
HISTCONTROL=$HISTCONTROL${HISTCONTROL+:}ignoredups
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
#[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then

# Color Bash
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
#### CHANGE HERE####
#256COLORS
nc='#ff0000'

PS1="$b\u$w@$t\h: $w\w $b> $w"
#####

else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

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

# some more ls aliases
alias l='ls -lph'
alias ll='ls -alph'     # --> ls --help for more ('h' for human readable sizes (kiB, MiB, GiB...), 'p' for / for dirs) 

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

# misc aliases
#alias emacs='emacs -nw'
alias sudo='sudo '
alias psm='ps au'
alias s='sudo su -' #--> zum einfacher zu root zu kommen... siehe /etc/sudoers für details
alias xxx='sudo halt'
alias swapoffa='sudo swapoff -a'
alias scan='scanimage --format=tiff --mode=Color' #>http://lists.alioth.debian.org/pipermail/sane-devel/2001-December/001177.html
alias z='zsh'
alias am='alsamixer'

# edit aliases
alias editfstab='sudo emc /etc/fstab'
alias editf='sudo emc /etc/fstab'
alias editbashrc='emc $HOME/.bashrc'
alias editb='emc $HOME/.bashrc'
alias editzshrc='emc $HOME/.zshrcc'
alias editz='emc $HOME/.zshrc'
alias emc='emacsclient -c -a ""'

# mount aliases
alias uma='sudo umount -a'
alias mntz='sudo mount -L ZERO'
alias mnte='sudo mount -L EXIL'
alias mntwinssd='sudo mount -L WIN7SSD'
alias mntwinhdd='sudo mount -L WIN7HDD'
alias mntdebext='sudo mount -L deb_ext'
alias mntdebhdd='sudo mount -L deb_hdd'
alias mntr='mntz && mnte && mntwinssd && mntwinhdd && mntdebext && mntdebhdd'
alias mnta='sudo mount -a'
alias sansa='sudo mount UUID=0CAA-BE9D /media/sansa'

# apt aliases
alias ai='sudo apt-get install'
alias au='sudo apt-get update'
alias arem='sudo apt-get remove'

# set PATH
export PATH='/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games:/home/imrahil/bin:/sbin:/usr/local/sbin:/usr/sbin'
