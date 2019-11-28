-- vim: ts=4 sw=4 noet ai cindent syntax=lua
--[[
Conky, a system monitor, based on torsmo

Any original torsmo code is licensed under the BSD license

All code written since the fork of torsmo is licensed under the GPL

Please see COPYING for details

Copyright (c) 2004, Hannu Saransaari and Lauri Hakkarainen
Copyright (c) 2005-2012 Brenden Matthews, Philip Kovacs, et. al. (see AUTHORS)
All rights reserved.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
]]

conky.config = {
    alignment = 'top_left',
    background = true,
    border_width = 1,
    color1 = 'FF8E38',
    cpu_avg_samples = 2,          -- set to 1 to disable averaging
    default_color = 'A89C8C',
    default_outline_color = 'white',
    default_shade_color = 'white',
    double_buffer = true,          -- double buffering (reduces flicker, may not work for everyone)
    draw_borders = false,
    draw_graph_borders = false,
    draw_outline = false,
    draw_shades = false,
    extra_newline = false,
    font = 'Bitstream Vera Sans Mono:size=13',
    gap_x = 0,
    gap_y = 0,
    lua_load = '~/.config/conky/rings.lua',
    lua_draw_hook_pre = 'conky_clock_rings',
    minimum_height = 1080,
    minimum_width = 1920,
    net_avg_samples = 2,
    no_buffers = true,
    net_avg_samples = 2,          -- set to 1 to disable averaging
    out_to_console = false,
    out_to_stderr = false,
    own_window = true,          -- Draws own window (prevent flickering with more than one conky running)
    own_window_transparent = true,
    own_window = true,
    own_window_class = 'Conky',
    own_window_hints = 'undecorated,below,sticky,skip_taskbar,skip_pager',
    own_window_type = 'override',
    show_graph_scale = true,
    show_graph_range = false,
    short_units = true,          -- Shortens units to a single character (kiB->k, GiB->G, etc.). Default is off.
    stippled_borders = 0,
    update_interval = 1.0,
    uppercase = false,
    use_spacer = 'none',
    use_xft = true,
    xftalpha = 0.8,
}

--[[ TEST
${font xft:Bitstream Vera Sans Mono:size=40}${time %H:%M}${font}

${color #A89C8C}${hr 2}${color}
]]


conky.text = [[
]]
