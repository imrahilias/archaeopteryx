#=========================================
# Auto-completions
#=========================================
autoload -U compinit; compinit
autoload -U colors && colors
zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*' menu select
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.config/shell/zsh_cache
zstyle ':completion:*' completer _complete _match
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:match:*' original only
zstyle ':completion:*' verbose true
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:kill:*:processes' command 'ps haxopid:5,user:4,%cpu:4,ni:2,stat:3,etime:8,args'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' force-list always
zstyle ':completion:*:processes' command "ps -au${USER}"
zstyle ':completion:*:pacman:*' force-list always
zstyle ':completion:*:*:pacman:*' menu yes select
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin
[[ -a $(whence -p pacman-color) ]] && compdef _pacman pacman-color=pacman


#=========================================
# Options
#=========================================
setopt prompt_subst
unsetopt beep
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000


#=========================================
# Syntax highlighting (by nicoulaj@github)
#=========================================
if [[ ${TERM} != "linux" && -f /usr/share/zsh/plugins/zsh-syntax-highlighting.zsh ]]; then
	. /usr/share/zsh/plugins/zsh-syntax-highlighting.zsh
	ZSH_HIGHLIGHT_STYLES[default]='none'
	ZSH_HIGHLIGHT_STYLES[unknown-token]='fg=red'
	ZSH_HIGHLIGHT_STYLES[reserved-word]='fg=green'
	ZSH_HIGHLIGHT_STYLES[alias]='fg=green'
	ZSH_HIGHLIGHT_STYLES[builtin]='fg=green'
	ZSH_HIGHLIGHT_STYLES[function]='fg=green'
	ZSH_HIGHLIGHT_STYLES[command]='fg=green'
	ZSH_HIGHLIGHT_STYLES[hashed-command]='fg=green'
	ZSH_HIGHLIGHT_STYLES[path]='fg=white'
	ZSH_HIGHLIGHT_STYLES[globbing]='fg=white'
	ZSH_HIGHLIGHT_STYLES[history-expansion]='fg=green,'
	ZSH_HIGHLIGHT_STYLES[single-hyphen-option]='fg=cyan'
	ZSH_HIGHLIGHT_STYLES[double-hyphen-option]='fg=cyan'
	ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]='fg=red'
	ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]='fg=red'
	ZSH_HIGHLIGHT_STYLES[assign]='none'
	ZSH_HIGHLIGHT_STYLES[single-quoted-argument]='fg=yellow'
	ZSH_HIGHLIGHT_STYLES[double-quoted-argument]='fg=yellow'
fi


#=========================================
# Functions @jinn
#=========================================
# prompt_char
# changes the prompt char to ± if the current dir is a git repo
function prompt_char {
    git branch >/dev/null 2>/dev/null && echo '±' && return 
    echo '»'
}

# git_branch
# if the current dir is a git repo, it prints the current branch and a * if there is
# stuff to be commited.
function git_branch {
    git branch >/dev/null 2>/dev/null && echo -n "git:"$(git branch | grep "*" | sed 's/* //')
    git status >/dev/null 2>/dev/null | grep modified >/dev/null 2>/dev/null && echo "* " && return
    echo " "
}

function cmd_fail {
    if [ "`echo $?`" -ne "0" ]; then
	echo ":( "
    fi
}


#=========================================
# Prompt
#=========================================
BLACK="%{"$'\033[00;30m'"%}"
BBLACK="%{"$'\033[01;30m'"%}"
RED="%{"$'\033[00;31m'"%}"
BRED="%{"$'\033[01;31m'"%}"
GREEN="%{"$'\033[00;32m'"%}"
BGREEN="%{"$'\033[01;32m'"%}"
YELLOW="%{"$'\033[00;33m'"%}"
BYELLOW="%{"$'\033[01;33m'"%}"
BLUE="%{"$'\033[00;34m'"%}"
BBLUE="%{"$'\033[01;34m'"%}"
MAGENTA="%{"$'\033[00;35m'"%}"
BMAGENTA="%{"$'\033[01;35m'"%}"
CYAN="%{"$'\033[00;36m'"%}"
BCYAN="%{"$'\033[01;36m'"%}"
WHITE="%{"$'\033[00;37m'"%}"
BWHITE="%{"$'\033[01;37m'"%}"
NORM="%{"$'\033[00m'"%}"
PROMPT="${BBLACK}%n${YELLOW}@${BBLACK}%M ${WHITE}%~ ${BBLUE}"'$(prompt_char)'" ${WHITE}" # Vote Jungle;)
RPROMPT='$(cmd_fail)$(git_branch)%T'

#PROMPT='[%{$fg[blue]%}%n$white@$cyan%m$reset:%~]$(prompt_char) '
#RPROMPT='$(cmd_fail)$(git_branch)%T' 


#=========================================
# Evironment variables
#=========================================
#export HS='alsa_output.usb-047f_c001-00-U0x47f0xc001.analog-stereo'
#export SP='alsa_output.pci-0000_00_1b.0.analog-stereo'
export EDITOR='/home/imrahil/scripts/emc'
export PATH='/bin:/usr/bin:/usr/local/bin:/home/imrahil/scripts:/sbin:/usr/sbin:/usr/local/sbin:/usr/games:/usr/local/games'
#path+=/scripts #hängt zur $path eben was an...


#=========================================
# Aliases
#=========================================
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
alias l='ls -lph'
alias ll='ls -alph'     # --> ls --help for more ('h' for human readable sizes (kiB, MiB, GiB...), 'p' for / for dirs) 

# edit aliases
alias emc='emacsclient -c -a ""' #see ~/scripts/emc
alias editfstab='sudo emc /etc/fstab'
alias editf='sudo emc /etc/fstab'
alias editbashrc='emc $HOME/.bashrc'
alias editb='emc $HOME/.bashrc'
alias editzshrc='emc $HOME/.zshrcc'
alias editz='emc $HOME/.zshrc'
alias editx='emc $HOME/.xinitrc'
alias editxm='emc $HOME/.xmonad/xmonad.hs'

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
alias asr='apt-cache search'
alias arem='sudo apt-get remove'

#misc aliases
alias s='sudo su -' #--> zum einfacher zu root zu kommen... siehe /etc/sudoers für details
alias sudo='sudo '
alias psm='ps au'
alias xxx='sudo halt'
alias swapoffa='sudo swapoff -a'
alias scan='scanimage --format=tiff --mode=Color' #>http://lists.alioth.debian.org/pipermail/sane-devel/2001-December/001177.html
alias b='bash'
alias am='alsamixer'
alias tug_vpn='sudo vpnc-connect tug'
alias vpn0='sudo vpnc-disconnect'
alias tug_ssh='ssh imrahil@faepnx.tugraz.at'

#=========================================
# MISC
#=========================================
# turn off XOFF/XON
stty -ixon