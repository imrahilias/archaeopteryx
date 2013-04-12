# ~/.zshrc 4 imrahil@triceratops

#=========================================
# Auto-completions
#=========================================
autoload -Uz compinit; compinit
autoload -U colors && colors
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' completions 1
zstyle ':completion:*' file-sort name
zstyle ':completion:*' glob 1
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' max-errors 2
zstyle ':completion:*' original true
zstyle ':completion:*' substitute 1
zstyle ':completion:*' special-dirs true # tab-completion for .. and others
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
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin

[ -r /usr/share/doc/pkgfile/command-not-found.zsh ] && . /usr/share/doc/pkgfile/command-not-found.zsh # files just 4 arch...


#=========================================
# Options
#=========================================
setopt correct #correct mistakes
#setopt auto_list # list choice on ambiguous command
setopt listtypes # %1 killed. will show up exactly when it is killed.
setopt auto_cd # change dir by just typing its name wo cd
setopt auto_pushd # automatically adds dirs to stack
setopt prompt_subst
setopt no_beep #never ever beep ever (alt: unsetopt beep)
setopt rm_star_wait # Wait, and ask if the user is serious when doing rm *
#setopt completealiases # is enabled elsewhere/ otherwise no effect
setopt append_history # Don't overwrite history
#setopt inc_append_history # saves in chronological order, all sessions
setopt share_history # even more, sessioins share the same file!
#histfile=~/.histfile
#histsize=10000
#savehist=10000
HISTFILE=~/.histfile # CASE SENSITIVE?!
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
# Other Functions
#=========================================

function chpwd() {
    emulate -L zsh
    ls -1bh --group-directories-first --color=auto # runs ls (...) after typing cd!
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

#PROMPT='[%{$fg[blue]%}%n$white@$cyan%m$reset:%~]$(prompt_char) ' # @jinn
#RPROMPT='$(cmd_fail)$(git_branch)%T' 


#=========================================
# Evironment variables
#=========================================
#export HS='alsa_output.usb-047f_c001-00-U0x47f0xc001.analog-stereo'
#export SP='alsa_output.pci-0000_00_1b.0.analog-stereo'
export EDITOR='emacs'
#export EDITOR='emacsclient -c -a ""'
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
# list aliasas
alias ö='ls -1Bh --group-directories-first' # '1' 4 one entry/line, 'B' ignores backups (~), 'h' 4 human readable (kiB, MiB, ...)
alias öö='ls -1ABh --group-directories-first' # 'A' 4 almost all
alias l='ls -Bhno --group-directories-first' # 'n' 4 numeric uid/gid, 'o' like 'l' without group 
alias ll='ls -ABhno --group-directories-first'
#alias ll='ls -alph'     # --> ls --help for more ('h' for human readable sizes (kiB, MiB, GiB...), 'p' for / for dirs) 
alias d='dirs -v' # lists zsh directory stack (enter <cd +- tab>, plus & minus (reverse) literally, with completion!'

# edit aliases
alias emc='emacsclient -c -a ""' # > emacsclient --help or ~/scripts/emc
alias editf='sudo emc /etc/fstab'
alias editb='emc $HOME/.bashrc'
alias editz='emc $HOME/.zshrc'
alias editx='emc $HOME/.xinitrc'
alias editxm='emc $HOME/.xmonad/xmonad.hs'

# mount aliases
alias mnta='sudo mount -a'
alias uma='sudo umount -a'
alias mntz='sudo mount /media/zero'
alias mnte='sudo mount /media/exil'
alias mnt0='sudo mount /media/0k3'
alias mntwinssd='sudo mount /media/win7ssd'
alias mntwinhdd='sudo mount /media/win7hdd'
alias mntdebext='sudo mount /media/deb_ext'
alias mntdebhdd='sudo mount /media/deb_hdd'
alias mntr='mntz && mnte && mntwinssd && mntwinhdd && mntdebext && mntdebhdd'
alias sansa='sudo mount UUID=0CAA-BE9D /media/sansa'

# apt aliases
alias ai='sudo apt-get install'
alias au='sudo apt-get update'
alias asr='apt-cache search'
alias arm='sudo apt-get remove'

#misc aliases
alias s='sudo su -' #--> zum einfacher zu root zu kommen... siehe /etc/sudoers für details
alias sudo='sudo '
alias psm='ps au'
alias xxx='sudo halt'
alias swapoffa='sudo swapoff -a'
alias scan='scanimage --format=tiff --mode=Color' #>http://lists.alioth.debian.org/pipermail/sane-devel/2001-December/001177.html
alias am='alsamixer'
#alias tug='sudo vpnc-connect tug' # in jinns script implementiert
alias tug='sudo tug_connect'
alias vpn0='sudo vpnc-disconnect'
alias tug_ssh='ssh imrahil@faepnx.tugraz.at'
alias jdl='sudo jdownloader'
alias dh='dh -h'
alias dha='dh -ah'
alias df='df -h'
alias dfa='df -ah'
alias x='xinit'
alias pp='ping 8.8.8.8'
alias oe1='mplayer http://mp3stream3.apasf.apa.at:8000/listen.pls'
alias countf='find . -type f | wc -l' # number of all files in dir
alias countd='find . -type d | wc -l' # number of all subdirs in dir


#=========================================
# MISC
#=========================================

# turn off XOFF/XON
stty -ixon

#key setups
bindkey -e # emacs key bindings: yeiha:D
#bindkey ' ' magic-space # also do history expansion on space ... doesnt work
