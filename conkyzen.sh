#!/bin/bash
# conky > dzen2 > statusbar 
# ~/scripts/conkyzen

#Layout
BAR_H=9
BIGBAR_W=65
WIDTH_L=960
WIDTH_R=960 #WIDTH_L + WIDTH_R = 1920
HEIGHT=16
X_POS_L=0
X_POS_R=$WIDTH_L
Y_POS_D=1064
Y_POS_U=0

#rc
#CONKYFILE="${HOME}/.conkyrc"
BAR_TOP_R="${HOME}/scripts/conkybattoprrc"
BAR_BOT_R="${HOME}/scripts/conkybatbotrrc"
BAR_BOT_L="${HOME}/scripts/conkybatbotlrc"

#Colors and font
CRIT="#99cc66"
BAR_FG="#3955c4"
BAR_BG="#363636"
DZEN_FG="#9d9d9d"
DZEN_BG="#000000" #TEST
DZEN_FG2="#444444"
#DZEN_BG="#020202"
COLOR_SEP=$DZEN_FG2


#top right bar
conky -c $BAR_TOP_R | dzen2 -e '' -x $X_POS_R -y $Y_POS_U -h $HEIGHT -w $WIDTH_R -ta 'r' -fg $DZEN_FG -bg $DZEN_BG &

#bottom right bar
conky -c $BAR_BOT_R | dzen2 -e '' -x $X_POS_R -y $Y_POS_D -h $HEIGHT -w $WIDTH_R -ta 'r' -fg $DZEN_FG -bg $DZEN_BG &

#bottom left bar
conky -c $BAR_BOT_L | dzen2 -e '' -x $X_POS_L -y $Y_POS_D -h $HEIGHT -w $WIDTH_L -ta 'l' -fg $DZEN_FG -bg $DZEN_BG &
