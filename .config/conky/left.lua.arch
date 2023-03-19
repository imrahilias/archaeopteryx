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
    font = 'DejaVu Sans Mono:size=13',
    gap_x = 30,
    gap_y = 50,
    minimum_height = 5,
    minimum_width = 5,
    maximum_width = 350,
    net_avg_samples = 2,
    no_buffers = true,
    net_avg_samples = 2,          -- set to 1 to disable averaging
    out_to_console = false,
    out_to_stderr = false,
    override_utf8_locale = true,
    own_window = true,          -- Draws own window (prevent flickering with more than one conky running)
    own_window_transparent = true,
    own_window = true,
    own_window_class = 'Conky',
    own_window_hints = 'undecorated,below,sticky,skip_taskbar,skip_pager',
    own_window_type = 'override',
    show_graph_scale = false,
    show_graph_range = false,
    short_units = true,          -- Shortens units to a single character (kiB->k, GiB->G, etc.). Default is off.
    stippled_borders = 0,
    update_interval = 1.0,
    uppercase = false,
    use_spacer = 'none',
    use_xft = true,
    xftalpha = 0.8,
}

conky.text = [[

$alignr}su mo tu we th fr sa 
${execpi 60 today=`date +%_d`; cal | sed -n '3,8 p' | sed 's/^/${alignr} /' | sed s/"\(^\|[^0-9]\)$today"'\b'/'\1${color1}'"$today"'${color}'/}

  cpu    $color1 ${cpu cpu0} ${alignr} ${color} ${cpu cpu1}/${cpu cpu2}/${cpu cpu3}/${cpu cpu4}
  └ft    $color1 $freq ${color} ${alignr} ${hwmon 2 temp 1}/${hwmon 2 temp 2}/${hwmon 2 temp 3}/${hwmon 2 temp 4} / $color1${hwmon 2 temp 5}${color}
  gpu    $color1 ${nvidia gpuutil} / ${nvidia memutil} ${color} ${alignr} ${nvidia videoutil} / ${nvidia pcieutil}${color}
  └ft    $color1 ${nvidia gpufreq} / ${nvidia memfreq} ${color} ${alignr} ${nvidia temp}
  ram    $color1 $memperc ${color} ${alignr} $mem
  avg    $color1 $loadavg ${color} ${alignr}   ${acpitemp}   
  prc    $color1 $running_processes ${color} / $processes ${alignr} $uptime
  dsk    $color1 $diskio ${color} ${alignr} ${execpi 1 nc localhost 7634 | awk '{print $4,"/",$9}'}
  ├─a    $color1 ${diskio_write /dev/sda} ▼ $alignr ${color} ${diskio_read /dev/sda} ▲
  ├─p    $color1 ${diskio_write /dev/sdc} ▼ $alignr ${color} ${diskio_read /dev/sdc} ▲
  └─t    $color1 ${diskio_write /dev/sdd} ▼ $alignr ${color} ${diskio_read /dev/sdd} ▲
  net    $color1 ${downspeed eno1} ▼ ${color} $alignr ${color} ${upspeed eno1} ▲

${color1}${top cpu 1}	  ${top pid 1}	   ${top name 1}${color}
${top cpu 2}	  ${top pid 2}	   ${top name 2}
${top cpu 3}	  ${top pid 3}	   ${top name 3}
${top cpu 4}	  ${top pid 4}	   ${top name 4}
${top cpu 5}	  ${top pid 5}	   ${top name 5}
${top cpu 7}	  ${top pid 7}	   ${top name 7}
${top cpu 8}	  ${top pid 8}	   ${top name 8}
${top cpu 9}	  ${top pid 9}	   ${top name 9}
${top cpu 10}	  ${top pid 10}	   ${top name 10}

${color1}${top_mem mem 1}	  ${top_mem pid 1}	   ${top_mem name 1}${color}
${top_mem mem 2}	  ${top_mem pid 2}	   ${top_mem name 2}
${top_mem mem 3}	  ${top_mem pid 3}	   ${top_mem name 3}
${top_mem mem 4}	  ${top_mem pid 4}	   ${top_mem name 4}
${top_mem mem 5}	  ${top_mem pid 5}	   ${top_mem name 5}
${top_mem mem 6}	  ${top_mem pid 6}	   ${top_mem name 6}
${top_mem mem 7}	  ${top_mem pid 7}	   ${top_mem name 7}
${top_mem mem 8}	  ${top_mem pid 8}	   ${top_mem name 8}
${top_mem mem 9}	  ${top_mem pid 9}	   ${top_mem name 9}
${top_mem mem 10}	  ${top_mem pid 10}	   ${top_mem name 10}

${execpi 1 lsblk -o NAME,FSAVAIL,LABEL,MOUNTPOINT | sed -e '1,1d' |  awk '{print " ",$1,$2,"$color1",$3,"$color","$alignr",$4,""}'}

]]
