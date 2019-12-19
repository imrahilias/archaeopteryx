#=========================================
# Aliases
#=========================================

# colors
red="\e[31m"
blue="\e[34m"
default="\e[0m"

# systemd alias
user_commands=(
  list-units is-active status show help list-unit-files
  is-enabled list-jobs show-environment cat list-timers
)

sudo_commands=(
  start stop reload restart try-restart isolate kill
  reset-failed enable disable reenable preset mask unmask
  link load cancel set-environment unset-environment
  edit
)

for c in $user_commands; do; alias sc-$c="systemctl $c"; done
for c in $sudo_commands; do; alias sc-$c="sudo systemctl $c"; done
for c in $user_commands; do; alias scu-$c="systemctl --user $c"; done
for c in $sudo_commands; do; alias scu-$c="systemctl --user $c"; done

alias sc-enable-now="sc-enable --now"
alias sc-disable-now="sc-disable --now"
alias sc-mask-now="sc-mask --now"

alias scu-enable-now="scu-enable --now"
alias scu-disable-now="scu-disable --now"
alias scu-mask-now="scu-mask --now"

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
#alias l='ls -1Bh --group-directories-first' # '1' 4 one entry/line, 'B' ignores backups (~), 'h' 4 human readable (kiB, MiB, ...)
#alias ll='ls -1ABh --group-directories-first' # 'A' 4 almost all
alias l='ls -1Bhl --group-directories-first' # 'n' 4 numeric uid/gid, 'o' like 'l' without group 
alias ll='ls -1ABhl --group-directories-first'
alias d='dirs -v' # lists zsh directory stack (enter <cd +- tab>, plus & minus (reverse) literally, with completion!'
alias blk='sudo blkid -o list'
alias hist='fc -El 0 | grep'
alias lsa='lsarchives '

# edit aliases
#alias editf='sudo emc /etc/fstab'
alias editf='sudo cp /etc/fstab /etc/fstab.$(date +%y%m%d%H%S) && sudo emc /etc/fstab'
alias editb='emacsclient -c $HOME/.bashrc'
alias editz='emacsclient -c $HOME/.zshrc'
alias editx='emacsclient -c $HOME/.xinitrc'
alias edita='emacsclient -c $HOME/.config/awesome/rc.lua'

# mount aliases
alias mnta='sudo mount -a; echo -e $red"mounted:"$default; mount' # echo 4 color, semicolon 4 1. command, if ok, than 2. com
alias uma='sudo umount -a; echo -e $red"mounted:"$default; mount'

# pacman aliases
alias pi='sudo pacman -Syyu' # Do a full system upgrade
alias pq='bauerbill -Ss --aur' # Search for all repo and AUR packages
alias pp='sudo powerpill -Syyu' # Do a full system upgrade using pauerpill with rsync 
alias ppa='sudo bb-wrapper -Syyu --aur' # Do a full system upgrade with AUR support using bauerbill with rsync
alias pa='sudo bb-wrapper -Su --aur' # install from AUR using bauerbill with rsync
alias px='sudo pacman -R' # Remove package
alias pc='sudo pacman -Scc && sudo pacman-optimize' # remove all cached pkg! and defragment
alias reflect='sudo reflector -p https -f 10 -l 10 --sort rate --save /etc/pacman.d/mirrorlist' # save 10 fastest of the 10 recent mirrors using https

# non root aliases
alias s='sudo su -' #--> zum einfacher zu root zu kommen... siehe /etc/sudoers für details
alias sudo='sudo '

# file aliases
alias du='du -d 1 -h'
alias dua='du -d 1 -ah' # Display the size of files at depth 1 in current location in human-readable form
alias duf='du -sh' # Display the size of files in current location in human-readable form
alias df='df -h'
alias dfa='df -ah'
alias countf='find . -type f | wc -l' # number of all files in dir
alias countd='find . -type d | wc -l' # number of all subdirs in dir
#alias rmtmp='rm *\#; rm *~; rm .*~' # moved to script
alias x='extract '

# network alias
alias oe1='mplayer http://mp3stream3.apasf.apa.at:8000/listen.pls'

# launch alias
alias x='startx'
#alias evince='dbus-launch evince'
alias ee="emacsclient -ca \'\'" # > service moved to systemd
alias scan='scanimage --format=tiff --mode=Color' #>http://lists.alioth.debian.org/pipermail/sane-devel/2001-December/001177.html
alias am='alsamixer'
alias halt='systemctl poweroff'
#alias mm='udisksctl mount -b' # nemo mounts on click

# misc
alias u='urxvtc'
alias dark='razercfg -l all:off'
alias light='razercfg -l GlowingLogo:off -l Scrollwheel:on'
alias fast='razercfg -r 1:3'
alias slow='razercfg -r 1:2'
alias rename='perl-rename'
alias zephyr='/usr/bin/git --git-dir=$HOME/.zephyr --work-tree=$HOME'
alias rainbow='for (( i = 30; i < 38; i++ )); do echo -e "\033[0;"$i"m Normal: (0;$i); \033[1;"$i"m Light: (1;$i)"; done'

#=========================================
# Options
#=========================================

setopt correct                  #correct mistakes
setopt auto_list                # list choice on ambiguous command
setopt listtypes                # %1 killed. will show up exactly when it is killed.
setopt auto_cd                  # change dir by just typing its name wo cd
setopt auto_pushd               # automatically adds dirs to stack
setopt prompt_subst             # prompt more dynamic, allow function in prompt
setopt no_beep                  # never ever beep ever (alt: unsetopt beep)
setopt rm_star_wait             # Wait, and ask if the user is serious when doing rm *
#setopt completealiases          # is enabled elsewhere/ otherwise no effect
setopt append_history           # Don't overwrite history
#setopt inc_append_history       # saves in chronological order, all sessions
setopt share_history            # even more, sessioins share the same file!
setopt hist_ignore_all_dups     # when runing a command several times, only store one
setopt hist_reduce_blanks       # reduce whitespace in history
setopt hist_ignore_space        # do not remember commands starting with space
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000


#=========================================
# Functions
#=========================================

# archive extraction
extract() {
	local remove_archive
	local success
	local extract_dir

	if (( $# == 0 )); then
		cat <<-'EOF' >&2
			Usage: extract [-option] [file ...]
			Options:
			    -r, --remove    Remove archive after unpacking.
		EOF
	fi

	remove_archive=1
	if [[ "$1" == "-r" ]] || [[ "$1" == "--remove" ]]; then
		remove_archive=0
		shift
	fi

	while (( $# > 0 )); do
		if [[ ! -f "$1" ]]; then
			echo "extract: '$1' is not a valid file" >&2
			shift
			continue
		fi

		success=0
		extract_dir="${1:t:r}"
		case "${1:l}" in
			(*.tar.gz|*.tgz) (( $+commands[pigz] )) && { pigz -dc "$1" | tar xv } || tar zxvf "$1" ;;
			(*.tar.bz2|*.tbz|*.tbz2) tar xvjf "$1" ;;
			(*.tar.xz|*.txz)
				tar --xz --help &> /dev/null \
				&& tar --xz -xvf "$1" \
				|| xzcat "$1" | tar xvf - ;;
			(*.tar.zma|*.tlz)
				tar --lzma --help &> /dev/null \
				&& tar --lzma -xvf "$1" \
				|| lzcat "$1" | tar xvf - ;;
			(*.tar) tar xvf "$1" ;;
			(*.gz) (( $+commands[pigz] )) && pigz -d "$1" || gunzip "$1" ;;
			(*.bz2) bunzip2 "$1" ;;
			(*.xz) unxz "$1" ;;
			(*.lzma) unlzma "$1" ;;
			(*.z) uncompress "$1" ;;
			(*.zip|*.war|*.jar|*.sublime-package|*.ipsw|*.xpi|*.apk|*.aar|*.whl) unzip "$1" -d $extract_dir ;;
			(*.rar) unrar x -ad "$1" ;;
			(*.7z) 7za x "$1" ;;
			(*.deb)
				mkdir -p "$extract_dir/control"
				mkdir -p "$extract_dir/data"
				cd "$extract_dir"; ar vx "../${1}" > /dev/null
				cd control; tar xzvf ../control.tar.gz
				cd ../data; extract ../data.tar.*
				cd ..; rm *.tar.* debian-binary
				cd ..
			;;
			(*)
				echo "extract: '$1' cannot be extracted" >&2
				success=1
			;;
		esac

		(( success = $success > 0 ? $success : $? ))
		(( $success == 0 )) && (( $remove_archive == 0 )) && rm "$1"
		shift
	done
}

# list archives
lsarchive() {
    if [ -f $1 ]; then
        case $1 in
            *.tar.bz2)      tar jtf $1      ;;
            *.tar.gz)       tar ztf $1      ;;
            *.tar)          tar tf $1       ;;
            *.tgz)          tar ztf $1      ;;
            *.zip)          unzip -l $1     ;;
            *.rar)          rar vb $1       ;;
            *.7z)           7z l $1         ;;
            *)              echo"'$1' Error. I have no idea what to do with that";;
        esac
    else
        echo "'$1' is not a valid archive"
    fi
}

# man coloring
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


# Pressing enter in a git directory runs `git status`
# in other directories `ls`
magic-enter () {

  # If commands are not already set, use the defaults
  [ -z "$MAGIC_ENTER_GIT_COMMAND" ] && MAGIC_ENTER_GIT_COMMAND="git status -u ."
  [ -z "$MAGIC_ENTER_OTHER_COMMAND" ] && MAGIC_ENTER_OTHER_COMMAND="ls -1Bhl --group-directories-first ."

  if [[ -z $BUFFER ]]; then
    echo ""
    if git rev-parse --is-inside-work-tree &>/dev/null; then
      eval "$MAGIC_ENTER_GIT_COMMAND"
    else
      eval "$MAGIC_ENTER_OTHER_COMMAND"
    fi
    zle redisplay
  else
    zle accept-line
  fi
}
zle -N magic-enter
bindkey "^M" magic-enter


# cd + ls
function chpwd() {
    emulate -L zsh
    ls -1Bhl --group-directories-first --color=auto . # runs ls (...) after typing cd!
}


# anti tar-bomb
atb() { l=$(tar tf $1); if [ $(echo "$l" | wc -l) -eq $(echo "$l" | grep $(echo "$l" | head -n1) | wc -l) ]; then tar xf $1; else mkdir ${1%.t(ar.gz||ar.bz2||gz||bz||ar)} && tar xf $1 -C ${1%.t(ar.gz||ar.bz2||gz||bz||ar)}; fi ;}


# quick dir change
rationalize-dot() {
    if [[ $LBUFFER = *.. ]]; then
        LBUFFER+=/..
    else
        LBUFFER+=.
    fi
}
zle -N rationalize-dot
bindkey . rationalize-dot


# prompt_char
# changes the prompt char to 'g' if the current dir is a git repo
function prompt_char {
    git branch >/dev/null 2>/dev/null && echo ' + ' && return 
    echo ' '
}


function cmd_fail {
    if [ "`echo $?`" -ne "0" ]; then
	echo ":( "
    fi
}


# git_branch
# if the current dir is a git repo, it prints the current branch and a * if there is
# stuff to be commited.
function git_branch {
    git branch >/dev/null 2>/dev/null && echo -n "git:"$(git branch | grep "*" | sed 's/* //')
    git status >/dev/null 2>/dev/null | grep modified >/dev/null 2>/dev/null && echo "* " && return
    echo " "
}


# sudo or sudoedit will be inserted before the command @ Dongweiming <ciici123@gmail.com>
sudo-command-line() {
    [[ -z $BUFFER ]] && zle up-history
    if [[ $BUFFER == sudo\ * ]]; then
        LBUFFER="${LBUFFER#sudo }"
    elif [[ $BUFFER == $EDITOR\ * ]]; then
        LBUFFER="${LBUFFER#$EDITOR }"
        LBUFFER="sudoedit $LBUFFER"
    elif [[ $BUFFER == sudoedit\ * ]]; then
        LBUFFER="${LBUFFER#sudoedit }"
        LBUFFER="$EDITOR $LBUFFER"
    else
        LBUFFER="sudo $LBUFFER"
    fi
}
zle -N sudo-command-line
# Defined shortcut keys: [Esc] [Esc]
bindkey -M emacs '\e\e' sudo-command-line
bindkey -M vicmd '\e\e' sudo-command-line
bindkey -M viins '\e\e' sudo-command-line


# colors, a lot of colors!
function clicolors() {
    i=1
    for color in {000..255}; do;
        c=$c"$FG[$color]$color✔$reset_color  ";
        if [ `expr $i % 8` -eq 0 ]; then
            c=$c"\n"
        fi
        i=`expr $i + 1`
    done;
    echo $c | sed 's/%//g' | sed 's/{//g' | sed 's/}//g' | sed '$s/..$//';
    c=''
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
PROMPT="${BWHITE}%~${BCYAN} @${BWHITE}"'$(prompt_char)'"${WHITE}" # Vote Jungle;)
RPROMPT='$(cmd_fail)$(git_branch)%T'

#PROMPT="${BBLACK}%n${YELLOW}@${BBLACK}%M ${WHITE}%~ ${BBLUE}"'$(prompt_char)'" ${WHITE}" # Vote Jungle;)
#PROMPT='[%{$fg[blue]%}%n$white@$cyan%m$reset:%~]$(prompt_char) ' # @jinn
#RPROMPT='$(cmd_fail)$(git_branch)%T' 


#=========================================
# Evironment variables
#=========================================

#export HS='alsa_output.usb-047f_c001-00-U0x47f0xc001.analog-stereo'
#export SP='alsa_output.pci-0000_00_1b.0.analog-stereo'
export EDITOR='emacsclient -c -a ""'
export PATH='/bin:/usr/bin:/usr/local/bin:/home/m/bin:/sbin:/usr/sbin:/usr/local/sbin:/usr/games:/usr/local/games'
#path+=/scripts #hängt zur $path eben was an...


#=========================================
# MISC
#=========================================

# turn off XOFF/XON
stty -ixon

# turn off powersaver/screensaver/blanking/bell
#xset -dpms s off s noblank -b

#key setups
bindkey -e # emacs key bindings: yeeha:D
bindkey ' ' magic-space # also do history expansion on space, type '!!', then hit enter, watch


#=========================================
# PLUGINS
#=========================================

# auto suggestion
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=6,bg=grey"
ZSH_AUTOSUGGEST_STRATEGY=(match_prev_cmd completion) # will first try to find a suggestion from your history, but, if it can't find a match, will find a suggestion from the completion engine (experimental).
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
ZSH_AUTOSUGGEST_USE_ASYNC=1

# history-substring-search
source /usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh

# syntax highlighning has to be last
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)


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
#zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin
zstyle ':completion:*:sudo:*' command-path /bin /usr/bin /usr/local/bin /home/m/bin /sbin /usr/sbin /usr/local/sbin /usr/games /usr/local/games

