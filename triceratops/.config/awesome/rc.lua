-- Standard awesome library
require("awful")
require("awful.autofocus")
require("awful.rules")
-- Theme handling library
require("beautiful")
-- Notification library
require("naughty")

-- Load Debian menu entries
require("debian.menu")

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.add_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = err })
        in_error = false
    end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers
beautiful.init(".config/awesome/themes/default_theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "urxvtc"
editor = os.getenv("EDITOR") or "nano"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
layouts =
{
    awful.layout.suit.tile,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.max,
}
-- }}}

-- {{{ Menu
-- Create a laucher widget and a main menu

mymainmenu = awful.menu({ items = { { "Programs", debian.menu.Debian_menu.Debian },
                                    { "Terminal", terminal }
                                  }
                        })

mylauncher = awful.widget.launcher({ image = image(beautiful.awesome_icon),
                                     menu = mymainmenu })
-- }}}

-- {{{ Wibox
-- Create a textclock widget
mytextclock = awful.widget.textclock({ align = "right" })

-- Create a systray
mysystray = widget({ type = "systray" })

-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly),
                    awful.button({ modkey }, 1, awful.client.movetotag),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, awful.client.toggletag),
                    awful.button({ }, 4, awful.tag.viewnext),
                    awful.button({ }, 5, awful.tag.viewprev)
                    )
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  if not c:isvisible() then
                                                      awful.tag.viewonly(c:tags()[1])
                                                  end
                                                  -- This will also un-minimize
                                                  -- the client, if needed
                                                  client.focus = c
                                                  c:raise()
                                              end
                                          end),
                     awful.button({ }, 3, function ()
                                              if instance then
                                                  instance:hide()
                                                  instance = nil
                                              else
                                                  instance = awful.menu.clients({ width=250 })
                                              end
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                              if client.focus then client.focus:raise() end
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                              if client.focus then client.focus:raise() end
                                          end))

for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt({ layout = awful.widget.layout.horizontal.leftright })
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.label.all, mytaglist.buttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(function(c)
                                              return awful.widget.tasklist.label.currenttags(c, s)
                                          end, mytasklist.buttons)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top", screen = s })
    -- Add widgets to the wibox - order matters
    mywibox[s].widgets = {
        {
            mylauncher,
            mytaglist[s],
            mypromptbox[s],
            layout = awful.widget.layout.horizontal.leftright
        },
        mylayoutbox[s],
        mytextclock,
        s == 1 and mysystray or nil,
        mytasklist[s],
        layout = awful.widget.layout.horizontal.rightleft
    }
end
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(

    -- Standard program
    awful.key({ modkey }, "q", awesome.restart),
--    awful.key({ modkey,"Shift" }, "q", awesome.quit),

    -- Layout
    awful.key({ modkey }, "l",      function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey }, "h",      function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey }, "space",  function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey }, "t", function () awful.layout.set(awful.layout.suit.tile) end),
    awful.key({ modkey }, "Left",   awful.tag.viewprev),
    awful.key({ modkey }, "Right",  awful.tag.viewnext),
    awful.key({ modkey }, "Escape", awful.tag.history.restore), -- switch to last workspace 
    awful.key({ modkey }, "Tab",
        function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end),
   awful.key({ modkey }, "w", function () mymainmenu:show({keygrabber=true}) end),
  
   -- Launch
    awful.key({ modkey }, "Return", function () awful.util.spawn(terminal) end),
    awful.key({ modkey }, "e", function () awful.util.spawn("emacsclient -c -a ''", false) end),
    awful.key({ modkey }, "i", function () awful.util.spawn("chromium", false) end),
    awful.key({ modkey }, "f", function () awful.util.spawn("nautilus", false) end),
    awful.key({ modkey,"Shift" }, "f", function () awful.util.spawn("sudo nautilus", false) end),
    awful.key({ modkey }, "o", function () awful.util.spawn("libreoffice") end),
    awful.key({ modkey }, "u", function () awful.util.spawn("stardict") end),
    awful.key({ modkey }, "a", function () awful.util.spawn_with_shell("feh -F ~/screens/xmonad_key_bindings.png") end),
    awful.key({ modkey }, "z", function () awful.util.spawn_with_shell("feh -F ~/screens/qwerty.png") end),
    awful.key({ }, "XF86Calculator", function () awful.util.spawn("xcalc", false) end),

    -- Audio
    awful.key({ }, "XF86AudioRaiseVolume", function () awful.util.spawn("amixer set Master 5%+", false) end),
    awful.key({ }, "XF86AudioLowerVolume", function () awful.util.spawn("amixer set Master 5%-", false) end),
--    awful.key({ }, "XF86AudioMute", function () awful.util.spawn('for x in {"Master","Headphone","Front","Surround","Center","LFE","Side"} ; do amixer -c 0 set "${x}" toggle; done', false) end),
    awful.key({ }, "XF86AudioMute", function () awful.util.spawn("amixer set Master toggle", false) end),
    awful.key({ }, "XF86Tools", function () awful.util.spawn(terminal .. " -e ncmpcpp", false) end),
    awful.key({ }, "XF86AudioPrev", function () awful.util.spawn("ncmpcpp prev", false) end),
    awful.key({ }, "XF86AudioPlay", function () awful.util.spawn("ncmpcpp toggle", false) end),
    awful.key({ }, "XF86AudioNext", function () awful.util.spawn("ncmpcpp next", false) end),

    -- Screenshot
    awful.key({ }, "Print",         function () awful.util.spawn("screen png", false) end),

    -- Prompt
    awful.key({ modkey }, "s",      function () mypromptbox[mouse.screen]:run() end),
    awful.key({ modkey }, "p",
	      function ()
		 awful.util.spawn("dmenu_run -i -p '' -fn 'xft:Bitstream Vera Sans Mono:pixelsize=18:autohint=true' -nb '" .. 
		beautiful.bg_normal .. "' -nf '" .. beautiful.fg_normal .. 
		"' -sb '" .. beautiful.bg_focus .. 
		"' -sf '" .. beautiful.fg_focus .. "'") 
	      end),
    awful.key({ modkey, "Shift" }, "p",
	      function ()
		 awful.util.spawn("sudo dmenu_run -i -p '' -fn 'xft:Bitstream Vera Sans Mono:pixelsize=18:autohint=true' -nb '" .. 
		beautiful.bg_normal .. "' -nf '" .. beautiful.fg_normal .. 
		"' -sb '" .. beautiful.bg_focus .. 
		"' -sf '#FF0000'") 
	      end),
    -- Run or raise applications with dmenu
    awful.key({ modkey }, "r",
	      function ()
		 local f_reader = io.popen( "dmenu_path | dmenu -i -fn 'xft:Bitstream Vera Sans Mono:pixelsize=18:autohint=true' -nb '" .. beautiful.bg_normal .. "' -nf '" .. beautiful.fg_normal .. 
		"' -sb '" .. beautiful.bg_focus .. 
		"' -sf '" .. beautiful.fg_focus .. "'")
		 local command = assert(f_reader:read('*a'))
		 f_reader:close()
		 if command == "" then return end	
	 	 -- Check throught the clients if the class match the command
		 local lower_command=string.lower(command)
		 for k, c in pairs(client.get()) do
		    local class=string.lower(c.class)
		    if string.match(class, lower_command) then
		       for i, v in ipairs(c:tags()) do
			  awful.tag.viewonly(v)
			  c:raise()
			  c.minimized = false
			  return
		       end
		    end
		 end
		 awful.util.spawn(command)
	      end)
)

clientkeys = awful.util.table.join(
    awful.key({ modkey, "Shift"   }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
    awful.key({ modkey, "Shift"   }, "Return",    function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey, "Shift"   }, "r",      function (c) c:redraw()                       end)
)

-- {{{ Tags
-- Define a tag table which hold all screen tags.
tags = {}
for s = 1, screen.count() do
    -- Each screen has its own tag table.
    tags[s] = awful.tag({ "∅", "@", "$", "⛁", "≣", "☻", "♬", "◎", "↯", "➊", "➋", "➌", "➍", "➎" }, s, layouts[1])
end
-- }}}

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
local keys = { "asciicircum", 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, "ssharp", "acute", "BackSpace" }
for i = 1,14 do
    globalkeys = awful.util.table.join(globalkeys,
        awful.key({ modkey }, keys[i],
                  function ()
                        local screen = mouse.screen
                        if tags[screen][i] then
                            awful.tag.viewonly(tags[screen][i])
                        end
                  end),
        awful.key({ modkey, "Control" }, keys[i],
                  function ()
                      local screen = mouse.screen
                      if tags[screen][i] then
                          awful.tag.viewtoggle(tags[screen][i])
                      end
                  end),
        awful.key({ modkey, "Shift" }, keys[i],
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.movetotag(tags[client.focus.screen][i])
                      end
                  end),
        awful.key({ modkey, "Control", "Shift" }, keys[i],
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.toggletag(tags[client.focus.screen][i])
                      end
                  end))
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

--[[
-- load the 'run or raise' function
local ror = require("aweror")

-- generate and add the 'run or raise' key bindings to the globalkeys table
globalkeys = awful.util.table.join(globalkeys, ror.genkeys(modkey))
--]]

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = true,
                     keys = clientkeys,
                     buttons = clientbuttons } },
    -- floats
    { rule = { class = "MPlayer" }, properties = { floating = true } },
    { rule = { class = "pinentry" }, properties = { floating = true } },
    { rule = { class = "gimp" }, properties = { floating = true } },
    { rule = { class = "XCalc" }, properties = { floating = true } },
    { rule = { class = "File-roller"}, properties = { floating = true } },
    { rule = { class = "Nvidia-settings"}, properties = { floating = true } },
    { rule = { class = "XFontSel"}, properties = { floating = true } },
    { rule = { class = "XCalc"}, properties = { floating = true } },
    { rule = { class = "XClock"}, properties = { floating = true } },
    { rule = { class = "Main","mplayer2"}, properties = { floating = true } },
    -- WEBS (Set Chromium to always map on tags number 2 of screen 1.)
    { rule = { class = "Chromium" }, properties = { tag = tags[1][2], switchtotag = true  } },
    -- FILE
    { rule = { class = "Thunar" }, properties = { tag = tags[1][4], switchtotag = true } },
    { rule = { class = "Nautilus" }, properties = { tag = tags[1][4], switchtotag = true } },
    -- DOCS
    { rule = { class = "Evince" }, properties = { tag = tags[1][5], switchtotag = true } },
    { rule = { class = "libreoffice-startcenter" }, properties = { tag = tags[1][5], switchtotag = true } },
    { rule = { class = "libreoffice-writer" }, properties = { tag = tags[1][5], switchtotag = true } },
    { rule = { class = "libreoffice-calc" }, properties = { tag = tags[1][5], switchtotag = true } },
    { rule = { class = "Qtiplot" }, properties = { tag = tags[1][5], switchtotag = true } },
    -- CHAT
    { rule = { class = "psi" }, properties = { tag = tags[1][5], switchtotag = true } },
    { rule = { class = "Pidgin" }, properties = { tag = tags[1][5], switchtotag = true } },
    { rule = { class = "Skype" }, properties = { tag = tags[1][5], switchtotag = true } },
    { rule = { class = "Ts3client_linux_amd64" }, properties = { tag = tags[1][5], switchtotag = true } },
    -- MOV
    { rule = { class = "Vlc" }, properties = { tag = tags[1][5], switchtotag = true } },
    { rule = { class = "mplayer2" }, properties = { tag = tags[1][5], switchtotag = true } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.add_signal("manage", function (c, startup)
    -- Add a titlebar
    -- awful.titlebar.add(c, { modkey = modkey })
    -- Enable sloppy focus
    c:add_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end
end)

client.add_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.add_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}
