#=========================================
# Launch tmux
#=========================================
#[[ $TERM != "linux" ]] && /usr/bin/tmux -q &>/dev/null && exit

#=========================================
# Source external configuration files
#=========================================
#eval `dircolors -b`
#[[ ${TERM} != "linux" && -f ${HOME}/.config/shell/dircolors ]] && eval `dircolors -b ${HOME}/.config/shell/dircolors`
#for i in ${HOME}/.config/shell/{exports,aliases,functions}; do
#  . $i || { print "$i: cannnot source file" && setopt warncreateglobal }
#done

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
PROMPT="${BBLACK}%n${YELLOW}@${BBLACK}%M ${WHITE}%~ ${BBLUE}» ${WHITE}"

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
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt inc_append_history
setopt interactive_comments
unsetopt beep
unsetopt hist_beep
unsetopt list_beep

#=========================================
# Key bindings
#=========================================
bindkey "\e[1~" beginning-of-line
bindkey "\e[4~" end-of-line
bindkey "\e[5~" beginning-of-history
bindkey "\e[6~" end-of-history
bindkey "\e[3~" delete-char
bindkey "\e[2~" quoted-insert
bindkey "\e[5C" forward-word
bindkey "\e[5D" backward-word
bindkey "\e[7~" beginning-of-line
bindkey "\e[8~" end-of-line

#=========================================
# Syntax highlighting (by nicoulaj@github)
#=========================================
if [[ ${TERM} != "linux" && -f /usr/share/zsh/plugins/zsh-syntax-highlighting.zsh ]]; then
	. /usr/share/zsh/plugins/zsh-syntax-highlighting.zsh
	ZSH_HIGHLIGHT_STYLES[default]='none'
	ZSH_HIGHLIGHT_STYLES[unknown-token]='fg=red,bold'
	ZSH_HIGHLIGHT_STYLES[reserved-word]='fg=green,bold'
	ZSH_HIGHLIGHT_STYLES[alias]='fg=green,bold'
	ZSH_HIGHLIGHT_STYLES[builtin]='fg=green,bold'
	ZSH_HIGHLIGHT_STYLES[function]='fg=green,bold'
	ZSH_HIGHLIGHT_STYLES[command]='fg=green,bold'
	ZSH_HIGHLIGHT_STYLES[hashed-command]='fg=green,bold'
	ZSH_HIGHLIGHT_STYLES[path]='fg=white,bold'
	ZSH_HIGHLIGHT_STYLES[globbing]='fg=white,bold'
	ZSH_HIGHLIGHT_STYLES[history-expansion]='fg=green,bold'
	ZSH_HIGHLIGHT_STYLES[single-hyphen-option]='fg=cyan'
	ZSH_HIGHLIGHT_STYLES[double-hyphen-option]='fg=cyan'
	ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]='fg=red,bold'
	ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]='fg=red,bold'
	ZSH_HIGHLIGHT_STYLES[assign]='none'
	ZSH_HIGHLIGHT_STYLES[single-quoted-argument]='fg=yellow,bold'
	ZSH_HIGHLIGHT_STYLES[double-quoted-argument]='fg=yellow,bold'
fi


#=========================================
# Aliases
#=========================================

#aliases jinn
#alias ai='sudo apt-get install'
#alias asr='apt-cache search'
#alias sudo='sudo '
#alias ll='ls -al'
#alias l='ls -l'
#alias s="sudo su -"
#alias mktags="cd $CODEDIR && etags `find $CODEDIR -name '*.cpp' -o -name '*.[c|h]'` && cd -"
#alias pn="ping 8.8.8.8"
#alias pu="ping web.de"
#alias am="alsamixer"
#alias tor="/usr/src/tor/tor-browser_en-US/start-tor-browser"
#alias m="mplayer"
#alias moc="mocp && ~/.moc/moc_clear_song"

#aliases M

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

#misc aliases
#alias emacs='emacs -nw'
alias sudo='sudo '
alias psm='ps au'
#alias s='sudo su -' #--> zum einfacher zu root zu kommen... siehe /etc/sudoers für details
alias xxx='sudo halt'
alias swapoffa='sudo swapoff -a'
alias scan='scanimage --format=tiff --mode=Color' #>http://lists.alioth.debian.org/pipermail/sane-devel/2001-December/001177.html
alias s='sudo su -' #--> zum einfacher zu root zu kommen... siehe /etc/sudoers für details
alias b='bash'
alias am='alsamixer'

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
alias arem='sudo apt-get remove'

# set PATH
export PATH='/bin:/usr/bin:/usr/local/bin:/home/imrahil/scripts:/sbin:/usr/sbin:/usr/local/sbin:/usr/games:/usr/local/games'