-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup").widget

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
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = tostring(err) })
        in_error = false
    end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
--beautiful.init(".config/awesome/desert.lua")
beautiful.init(awful.util.getdir("config") .. "/themes/canyon.lua")

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
awful.layout.layouts = {
    awful.layout.suit.tile,
    --awful.layout.suit.tile.left,
    awful.layout.suit.max,
    awful.layout.suit.tile.bottom,
    --awful.layout.suit.tile.top,
    --awful.layout.suit.fair,
    --awful.layout.suit.fair.horizontal,
    --awful.layout.suit.spiral,
    --awful.layout.suit.spiral.dwindle,
    --awful.layout.suit.floating,
    --awful.layout.suit.max.fullscreen,
    --awful.layout.suit.magnifier,
    --awful.layout.suit.corner.nw,
    -- awful.layout.suit.corner.ne,
    -- awful.layout.suit.corner.sw,
    -- awful.layout.suit.corner.se,
}
-- }}}

-- {{{ Helper functions
local function client_menu_toggle_fn()
    local instance = nil

    return function ()
        if instance and instance.wibox.visible then
            instance:hide()
            instance = nil
        else
            instance = awful.menu.clients({ theme = { width = 250 } })
        end
    end
end
-- }}}

-- {{{ Menu
--[[
-- Create a launcher widget and a main menu
myawesomemenu = {
   { "hotkeys", function() return false, hotkeys_popup.show_help end},
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end}
}
--]]

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
--                                    { "open terminal", terminal }
                                  }
                        })

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- Keyboard map indicator and switcher
mykeyboardlayout = awful.widget.keyboardlayout()

-- {{{ Wibar
-- Create a textclock widget
--mytextclock = wibox.widget.textclock()
mytextclock = wibox.widget.textclock('%a %e %b <span color="#FF8E38"> %_H:%M </span> ', 5)  -- blue: #1793D1

-- Create a wibox for each screen and add it
local taglist_buttons = awful.util.table.join(
                    awful.button({ }, 1, function(t) t:view_only() end),
                    awful.button({ modkey }, 1, function(t)
                                              if client.focus then
                                                  client.focus:move_to_tag(t)
                                              end
                                          end),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, function(t)
                                              if client.focus then
                                                  client.focus:toggle_tag(t)
                                              end
                                          end),
                    awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
                    awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
                )

local tasklist_buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  -- Without this, the following
                                                  -- :isvisible() makes no sense
                                                  c.minimized = false
                                                  if not c:isvisible() and c.first_tag then
                                                      c.first_tag:view_only()
                                                  end
                                                  -- This will also un-minimize
                                                  -- the client, if needed
                                                  client.focus = c
                                                  c:raise()
                                              end
                                          end),
                     awful.button({ }, 3, client_menu_toggle_fn()),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                          end))

local function set_wallpaper(s)
    -- Wallpaper
    if beautiful.wallpaper then
        local wallpaper = beautiful.wallpaper
        -- If wallpaper is a function, call it with the screen
        if type(wallpaper) == "function" then
            wallpaper = wallpaper(s)
        end
        gears.wallpaper.maximized(wallpaper, s, true)
    end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(function(s)
    -- Wallpaper
    set_wallpaper(s)

    -- Each screen has its own tag table.
    awful.tag({ "∅", "@", "$", "⛁", "≣", "♬", "♻", "⚡", "✆","♞", "♠", "♥", "♦", "♣" }, s, awful.layout.layouts[1]) -- ↯ 𝄞♫ ♞♟ "♤", "♡", "♢", "♧" ⚛

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc( 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(-1) end),
                           awful.button({ }, 4, function () awful.layout.inc( 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(-1) end)))

    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, tasklist_buttons)
    
    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, taglist_buttons)

    -- Create the wibox
    s.mywibox = awful.wibar({ position = "top", screen = s, height = 30 })

    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
--            mylauncher,
            s.mytaglist,
            s.mypromptbox,
        },
        s.mytasklist, -- Middle widget
        { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            mykeyboardlayout,
            wibox.widget.systray(),
            mytextclock,
         --   s.mylayoutbox,
        },
    }
end)
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
   
    -- Awesome program
    awful.key({ modkey, "Shift" }, "q", awesome.restart, {description = "reload awesome", group = "awesome"}),
    awful.key({ modkey, "Shift", "Control" }, "q", awesome.quit, {description = "quit awesome", group = "awesome"}),

   -- Menubar
    awful.key({ modkey }, "p", function() menubar.show() end, {description = "show the menubar", group = "launcher"}),

   -- Navigation
--    awful.key({ modkey }, "z", hotkeys_popup.show_help, {description="show help", group="awesome"}),
    awful.key({ modkey }, "Left",  awful.tag.viewprev, {description = "view previous", group = "tag"}),
    awful.key({ modkey }, "Right",  awful.tag.viewnext, {description = "view next", group = "tag"}),
    awful.key({ modkey }, "Escape", awful.tag.history.restore, {description = "go back", group = "tag"}),
--    awful.key({ modkey }, "w", function () mymainmenu:show() end, {description = "show main menu", group = "awesome"}),
    awful.key({ modkey }, "u", awful.client.urgent.jumpto,  {description = "jump to urgent client", group = "client"}),
	      
    -- Layout manipulation
    awful.key({ modkey }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "go back", group = "client"}),

    awful.key({ modkey }, "l", function () awful.tag.incmwfact( 0.05) end, {description = "increase master width factor", group = "layout"}),
    awful.key({ modkey }, "h", function () awful.tag.incmwfact(-0.05) end, {description = "decrease master width factor", group = "layout"}),
    awful.key({ modkey }, "space", function () awful.layout.inc( 1) end, {description = "select next", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1) end, {description = "select previous", group = "layout"}),
    awful.key({ modkey }, m, awful.layout.suit.max.fullscreen, {description = "select previous", group = "layout"}),

    awful.key({ modkey, "Shift" }, "n",
              function ()
                  local c = awful.client.restore()
                  -- Focus restored client
                  if c then
                      client.focus = c
                      c:raise()
                  end
              end,
              {description = "restore minimized", group = "client"}),

   awful.key({ modkey, "Shift" }, "f",
          function ()
              myscreen = awful.screen.focused()
              myscreen.mywibox.visible = not myscreen.mywibox.visible
          end,
          {description = "toggle statusbar"}),
	  
    -- Prompt
    awful.key({ modkey }, "r", function () awful.screen.focused().mypromptbox:run() end, {description = "run prompt", group = "launcher"}),
    
    -- Launch
    awful.key({ modkey }, "Return", function () awful.util.spawn(terminal) end, {description = "terminate", group = "launcher"}),
    awful.key({ modkey }, "e", function () awful.util.spawn("emacsclient -c -a ''", false) end, {description = "emacs", group = "launcher"}),
    awful.key({ modkey }, "d", function () awful.util.spawn("nemo", false) end, {description = "nemo --no-desktop", group = "launcher"}),
    awful.key({ modkey, "Shift" }, "d", function () awful.util.spawn("sudo nemo --no-desktop", false) end, {description = "sudo nemo --no-desktop", group = "launcher"}),
--    awful.key({ modkey }, "z", function () awful.util.spawn_with_shell("feh -F ~/.config/awesome/us_keyboard_layout.png") end, {description = "show querty", group = "launcher"}),
    awful.key({ modkey, "Shift" }, "s", function () awful.util.spawn_with_shell("open_primary_selection_in_browser.sh") end, {description = "open primary selection in browser", group = "launcher"}),

    -- Audio
    awful.key({ }, "XF86AudioRaiseVolume", function () awful.util.spawn("amixer set Master 5%+", false) end),
    awful.key({ }, "XF86AudioLowerVolume", function () awful.util.spawn("amixer set Master 5%-", false) end),
--    awful.key({ }, "XF86AudioMute", function () awful.util.spawn('for x in {"Master","Headphone","Front","Surround","Center","LFE","Side"} ; do amixer -c 0 set "${x}" toggle; done', false) end),
    awful.key({ }, "XF86AudioMute", function () awful.util.spawn("amixer set Master toggle", false) end),
    awful.key({ }, "XF86Tools", function () awful.util.spawn(terminal .. " -e ncmpcpp", false) end),
--    awful.key({ }, "XF86Tools", function () awful.util.spawn("spotify", false) end, {description = "spotify", group = "launcher"}),
    awful.key({ }, "XF86AudioPrev", function () awful.util.spawn("playerctl previous", false) end, {description = "previous", group = "audio"}),
    awful.key({ }, "XF86AudioPlay", function () awful.util.spawn("playerctl play-pause", false) end, {description = "play-pause", group = "audio"}),
    awful.key({ }, "XF86AudioNext", function () awful.util.spawn("playerctl next", false) end, {description = "next", group = "audio"}),

    -- Screenshot
    awful.key({ }, "Print", function () awful.util.spawn("scrot -e 'mv $f ~/screens/ 2>/dev/null'") end),

    -- Killer
    awful.key({ modkey, "Shift" }, "k", function () awful.util.spawn("sudo xkill", false) end, {description = "xkill", group = "awesome"}),

    -- Razer
    awful.key({ modkey }, "#86", function () awful.util.spawn("razercfg -l GlowingLogo:off -l Scrollwheel:on", false) end, {description = "xkill", group = "awesome"}),
    awful.key({ modkey }, "#82", function () awful.util.spawn("razercfg -l all:off", false) end, {description = "xkill", group = "awesome"})
    
)

clientkeys = awful.util.table.join(
    awful.key({ modkey, "Shift"   }, "c", function (c) c:kill() end, {description = "close", group = "client"}),
    awful.key({ modkey, "Shift" }, "Return", function (c) c:swap(awful.client.getmaster()) end, {description = "move to master", group = "client"}),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle, {description = "toggle floating", group = "client"}),
    
    awful.key({ modkey }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end ,
        {description = "minimize", group = "client"}),

    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized = not c.maximized
            c:raise()
        end ,
        {description = "maximize", group = "client"}),

    awful.key({ modkey, "Control" }, "m",
       function (c)
          c.maximized_vertical = not c.maximized_vertical
          c:raise()
       end ,
       {description = "(un)maximize vertically", group = "client"}),
    awful.key({ modkey, "Shift"   }, "m",
       function (c)
          c.maximized_horizontal = not c.maximized_horizontal
          c:raise()
       end ,
       {description = "(un)maximize horizontally", group = "client"}),
    
    awful.key({ modkey }, "f",
       function (c)
          c.fullscreen = not c.fullscreen
          c:raise()
       end,
       {description = "toggle fullscreen", group = "client"})

)

-- {{{ run or raise
local ror = require("aweror")

-- generate and add the 'run or raise' key bindings to the globalkeys table
globalkeys = awful.util.table.join(globalkeys, ror.genkeys(modkey))

root.keys(globalkeys)
-- }}}

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
local keys = { "#49", 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, "#20", "#21", "#22" }
for i = 1, 14 do
    globalkeys = awful.util.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, keys[i],
                  function ()
                        local screen = awful.screen.focused()
                        local tag = screen.tags[i]
                        if tag then
                           tag:view_only()
                        end
                  end,
                  {description = "view tag #"..i, group = "tag"}),
        -- Toggle tag display.
        awful.key({ modkey, "Control" }, keys[i],
                  function ()
                      local screen = awful.screen.focused()
                      local tag = screen.tags[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end,
                  {description = "toggle tag #" .. i, group = "tag"}),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, keys[i],
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:move_to_tag(tag)
                          end
                     end
                  end,
                  {description = "move focused client to tag #"..i, group = "tag"}),
        -- Toggle tag on focused client.
        awful.key({ modkey, "Control", "Shift" }, keys[i],
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
                      end
                  end,
                  {description = "toggle focused client on tag #" .. i, group = "tag"})
    )
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     screen = awful.screen.preferred,
                     placement = awful.placement.no_overlap+awful.placement.no_offscreen,
		     size_hints_honor = false
     }
    },

--    -- Floating clients.
--    { rule_any = {
--        instance = {
--          "DTA",  -- Firefox addon DownThemAll.
--          "copyq",  -- Includes session name in class.
--        },
--        class = {
--	   "Vlc",
--	  "mplayer",
--	  "Mplayer",
--	  "mplayer2",
--	  "Main","mplayer2",
--	  "XFontSel",
--          "Arandr",
--          "Gpick",
--          "Kruler",
--          "MessageWin",  -- kalarm.
--          "Sxiv",
--          "Wpa_gui",
--          "pinentry",
--          "veromix",
--          "xtightvncviewer"},
--
--        name = {
--          "Event Tester",  -- xev.
--        },
--        role = {
--          "AlarmWindow",  -- Thunderbird's calendar.
--          "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
--        }
--      }, properties = { floating = true }},
--
--    -- Add titlebars to normal clients and dialogs SERIOUSELY?
--    { rule_any = {type = { "normal", "dialog" }
--      }, properties = { titlebars_enabled = true },
--    },

    { rule_any = {   	    -- INTERNET
        class = {
          "Chromium",
	  "Firefox",
	  }},
       properties = { tag = "@", switchtotag = true }},

    { rule_any = {   	    -- CODE
        class = {
          "Emacs",
	  }},
       properties = { tag = "$", switchtotag = true }},

    { rule_any = {   	    -- FILE
        class = {
          "Nemo",
	  }},
       properties = { tag = "⛁", switchtotag = true }},

    { rule_any = {   	    -- DOC
        class = {
          "Evince",
          "libreoffice",
	  "libreoffice-startcenter",
	  "libreoffice-writer",
	  "libreoffice-calc",
	  "Qtiplot",
	  }},
       properties = { tag =  "≣", switchtotag = true }},

    { rule_any = {   	    -- MUSIC
        class = {
          "spotify",
          "Spotify",
                   },
        name = {
          "spotify",
          "Spotify",
	  }},
       properties = { tag = "♬", switchtotag = true }},

    { rule_any = {   	    -- VIDEO
        class = {
          "Vlc",
          "MPlayer",
	  }},
       properties = { tag = "♻", switchtotag = true }},

    { rule_any = {   	    -- DOWNLOAD
        wm_name = {
           "htops",
           "JDownloader 2",
        },
        class = {
          "JDownloader",
    }},
       properties = { tag = "⚡", switchtotag = true }},

    { rule_any = {    	    -- COMMUNICATION
        class = {
          "psi",
	  "Pidgin",
	  "Skype",
	  "Ts3client_linux_amd64",
          "TelegramDesktop",
	  }},
       properties = { tag = "✆", switchtotag = true }},
       
    { rule_any = {    	    -- GAMES
        class = {
          "steam",
          "Steam",
	  }},
       properties = { tag = "♞", switchtotag = true }},
}

-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    -- if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup and
      not c.size_hints.user_position
      and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
    -- buttons for the titlebar
    local buttons = awful.util.table.join(
        awful.button({ }, 1, function()
            client.focus = c
            c:raise()
            awful.mouse.client.move(c)
        end),
        awful.button({ }, 3, function()
            client.focus = c
            c:raise()
            awful.mouse.client.resize(c)
        end)
    )

    awful.titlebar(c) : setup {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                align  = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
        },
        { -- Right
            awful.titlebar.widget.floatingbutton (c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.stickybutton   (c),
            awful.titlebar.widget.ontopbutton    (c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
    if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
        and awful.client.focus.filter(c) then
        client.focus = c
    end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}
