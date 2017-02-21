
-- ror.lua
-- This is the file goes in your ~/.config/awesome/ directory
-- It contains your table of 'run or raise' key bindings for aweror.lua
-- Table entry format: ["key"]={"function", "match string", "optional attribute to match"}
-- The "key" can include "Control-", "Shift-", and "Mod1-" modifiers (eg "Control-z")
-- The "key" will be bound as "modkey + key". (eg from above would end up as modkey+Control+z)
-- The "function" is what gets run if no matching client windows are found.
-- Usual attributes are "class","instance", or "name". If no attribute is given it defaults to "class".
-- The "match string"  will match substrings.  So "Firefox" will match "blah Firefox blah"  
-- Use xprop to get this info from a window.  WM_CLASS(STRING) gives you "instance", "class".
-- WM_NAME(STRING) gives you the name of the selected window (usually something like the web page title
-- for browsers, or the file name for emacs).

table5={
   ["i"]={"firefox", "Firefox", "class"},
   ["d"]={"nemo", "Nemo", "class"},
   ["e"]={"emacsclient -ca ''","Emacs"}, 
   -- ["e"]={"emacsclient -a emacs -n -c","Emacs"}, 
   -- ["Control-z"]={"google-chrome --app=http://www.rdio.com","www.rdio.com", "instance"},
   -- ["w"]={"firefox","Firefox"}, 
   -- ["v"]={"firefox -new-window 'http://www.evernote.com/Home.action?login=true#v=l&so=mn'","Evernote", "name"}, 
   -- ["g"]={"firefox -new-window 'http://mail.google.com/mail/'","Gmail","name"}, 
   --["t"]={"xterm","xterm", "instance"}, 
   -- ["f"]={"xterm -name mcTerm -e mc -d","mcTerm", "instance"}, 
   -- ["Shift-s"]={"xterm -name rootTerm -cr red -title rootTerm -e su","rootTerm", "instance"}, 
   -- ["t"]={"xterm -name htopTerm -e htop","htopTerm","instance"}, 
   --["Shift-b"]={"xterm -title rtorrentTerm -e rtorrent","rtorrentTerm","name"}, 
   -- ["z"]={"xterm -name mocpTerm -e mocp","mocpTerm", "instance"} 
   --["i"]={"luakit", "luakit", "class"},
   --["o"]={"emacsclient -ca ''", "Emacs", "class"},
   --["Shift-o"]={"xterm -title emacsTerm -e emacsclient -c -nw", "emacsTerm", "name"},
   --["Control-o"]={"sudo xterm -title emacsRootTerm -e emacs -nw", "emacsRootTerm", "name"},
   --['\\']={"sudo xterm -title rootTerm -class rootTerm", "rootTerm", "class"},
   --["g"]={'gimp', 'gimp', "class"},
   --["y"]={"evince", "Evince", "class"},
   --["s"]={"xterm -title Sup -e sup", "Sup", "name"},
   --["["]={"xterm -title irssi -e irssi", "irssi", "name"},
   --["q"]={"xterm -title raspberrypi -e ssh pi@pi", "raspberrypi", "name"},
   --["a"]={"xterm -title pavi -e ssh jinn@pavianpredator", "pavi", "name"},
   --["z"]={"xterm -title vivian -e ssh nemo@vivian", "vivian", "name"},
   --["Control-j"]={"xterm -title jabber -e mcabber", "jabber", "name"}
}
