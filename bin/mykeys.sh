#!/bin/zsh

sudo echo '
partial alphanumeric_keys
	xkb_symbols "m" {
	include "us(basic)"
	name[Group1]= "English (US, umlaute)";
	key <AD11> { [ bracketleft, braceleft,     udiaeresis,          Udiaeresis ] };
	key <AC10> { [ semicolon,       colon,     odiaeresis,          Odiaeresis ] };
	key <AC11> { [ apostrophe,   quotedbl,     adiaeresis,          Adiaeresis ] };
	key <AE11> { [ minus,      underscore,     ssharp                          ] };
	key <AD03> { [         e,           E,       EuroSign,            EuroSign ] };
	include "level3(ralt_switch)"
};
' >> /usr/share/X11/xkb/symbols/us

setxkbmap -layout "us(m),de"
