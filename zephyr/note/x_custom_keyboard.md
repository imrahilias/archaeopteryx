# my own keys

to enable umlauts as keys like AltGr+O, add the fallowing to /usr/share/X11/xkb/symbols/us

sudo emacs /usr/share/X11/xkb/symbols/us

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------


# switch to custom keyboard layout
enables multiple layouts, switch via awesome, us(m) is custom

## setxkbmap us -variant m # same as "us(m)"


# - or -

enable compose keycodes *e.g. ralt+"+a = adiaeresis, kills the variant (see above)!

## setxkbmap -option compose:ralt


# multiple layouts

enables multiple layouts, switch via awesome, us(m) is custom.
add this line to .xinitrc, as it resets on boot

## setxkbmap -layout "us(m),de"
