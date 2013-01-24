#!/bin/bash
# conf2bak.sh
# config saving algorithm
echo ''

# Set
DAT=$(date +"%y%m%d")

# check 3 arguments are given #
case $# in
    0) echo "usage: conf2bak [-h|-z|-e|-c location]"
	echo "try: -h for help"
	exit 0
	;;
    1) case "$1" in
        # display help message
	-h) echo "config backup editor @M"
	    echo "usage: conf2bak [--help|-z|-e|-c location]"
	    echo "       --help   displays this text"
	    echo "       -z       copies configs to zero (int hdd)"
	    echo "       -e       copies configs to exil (ext hdd)"
	    echo "       -c location     copies configs to specified location (see mount/blkid)"  
	    exit 0
	    ;;
        # loc = zero
	-z) LOC="/media/zero/projects/computer/linux/configs/"$USER"_$DAT" #$USER gets evaluated via shell later on...
	    echo "copy configs > zero..."
	    ;;
        # loc = exil
	-e) LOC="/media/exil/projects/computer/linux/configs/"$USER"_$DAT"
	    echo "copy configs > exil..."
	    ;;
        # error message
	*) echo "invalid option"
	   echo "usage: conf2bak [-h|-z|-e|-c location]"
	   echo "try: -h for help"
	   exit 0
	   ;;
       esac
       ;;
    2) #  loc = custom
       if [ $1 == "-c" ]; then
	   LOC="$2/configs_"$USER"_$DAT"
	   echo "copy configs > wherever..."
       fi
       ;;
    *) # error message
       echo "invalid option"
       echo "usage: conf2bak [-h|-z|-e|-c location]"
       echo "try: -h for help"
       exit 0
       ;;
esac
echo ''

# root backup -----------------------------------------------------------------------------ERROR
if [[ $UID == 0 ]]; then
    echo "enabling superpowers..."
    echo "dont try, it doesnt work. needs fix :/"; echo "...failed"; echo ''
    exit 0
    echo "copy fstab..."
    cp /etc/fstab $LOC; echo "   fstab >"
    echo "...done"
    echo "disabling superpowers..."
    echo "...done"; echo ''
fi

# init location
echo "setting up location..."
mkdir $LOC
echo "...done"; echo ''
echo "backup location: $LOC"
echo "...done"; echo ''

# copy scripts
echo "copy scripts..."
cp -Rf $HOME/scripts $LOC; echo "scripts/ >"
echo "{"; echo "$(ls $HOME/scripts)"; echo "}"
echo "...done"; echo ''

# copy xmonad
echo "copy xmonad..."
mkdir $LOC/.xmonad
cp -Rf $HOME/.xmonad/xmonad.hs* $LOC/.xmonad; echo ".xmonad/ >"
echo "{"; echo "$(ls $HOME/.xmonad | grep xmonad.hs*)"; echo "}"; echo ''

# copy rcs
echo "copy rcs..."
cp $HOME/.bashrc $LOC; echo "   .bashrc >"
cp $HOME/.background.* $LOC; echo "   .background.* >"
cp $HOME/.conkyrc $LOC; echo "   .conkyrc >"
cp $HOME/.dircolors $LOC; echo "   .dircolors >"
cp $HOME/.emacs $LOC; echo "   .emacs >"
cp $HOME/.xinitrc $LOC; echo "   .xinitrc >"
cp $HOME/.xresources $LOC; echo "   .xresources >"
cp $HOME/.zshrc $LOC; echo "   .zshrc >"
echo "...done"; echo ''

# reminder
echo "keep in mind, this are rcs,they are invisible!"
touch "$LOC/here_are_invisible_files_cant_you_see"
echo "added reminder..."; echo ''

# end
echo "nothin else to do ... so doin nothin..."
echo "...done :)"; echo ''
