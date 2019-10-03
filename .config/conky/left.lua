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
    maximum_width = 310,
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

--[[ TEST
${font xft:Bitstream Vera Sans Mono:size=40}${time %H:%M}${font}

${color #5C5449} #calendar

${color #A89C8C}${hr 2}${color}

  ◈           $color1 ${nvidia gpufreq}, ${nvidia memfreq} ${color} ${alignr} ${nvidia temp} 
  ▣           $color1 $freq ${color} ${alignr} ${execi 60 sensors | grep Physical | sed -e 's/+//' -e 's/\.0//' | awk '{print "",$4}'} 
  λ           $color1 $loadavg ${color} ${alignr} ${acpitemp} 
  ☭           $color1 $memperc ${color} ${alignr} $mem / $memmax 
  ⊙           $color1 $diskio ${color} ${alignr} ${execi 60 sudo hddtemp /dev/sdc | awk '{print $4}'} 
  ⇝           $color1 $running_processes ${color} $alignr $processes 
  τ           $color1 $uptime ${color} 

${cpugraph cpu0 30,310 FF8E38 FF8E38}

${downspeedgraph eno1 35,290 ffaf73 A89C8C}
  ▼ $color1 $alignr ${downspeed eno1}  ${color}

${upspeedgraph eno1 30,290 ffaf73 A89C8C}
  ▲ $alignr ${upspeed eno1}  

 prime $alignr ${diskio_read `blkid -o list | grep "prime" | awk "{print $1}"`} ${diskio_write `blkid -o list | grep "prime" | awk '{print $1}'`}  

$color1  ${execpi 1 dmesg -t --level err | tail -n 10 | perl -lpe's/\b(.{30,30})\b/\1\n  /g'}

]]

conky.text = [[

$alignr}su mo tu we th fr sa 
${execpi 60 today=`date +%_d`; cal | sed -n '3,8 p' | sed 's/^/${alignr} /' | sed s/"\(^\|[^0-9]\)$today"'\b'/'\1${color1}'"$today"'${color}'/}

  cpu           $color1 $cpu ${alignr} ${color} ${cpu cpu1}/${cpu cpu2}/${cpu cpu3}/${cpu cpu4}
  fre           $color1 $freq ${color} 
  tmp           $color1 ${hwmon 2 temp 5} ${alignr} ${color} ${hwmon 2 temp 1}/${hwmon 2 temp 2}/${hwmon 2 temp 3}/${hwmon 2 temp 4}
  gpu           $color1 ${nvidia gpuutil} / ${nvidia memutil} ${color} ${alignr} ${nvidia videoutil} / ${nvidia pcieutil}${color}
  f/t           $color1 ${nvidia gpufreq} / ${nvidia memfreq} ${color} ${alignr} ${nvidia temp}
  mem           $color1 $memperc ${color} ${alignr} $mem
  avg           $color1 $loadavg ${color} ${alignr} ${acpitemp}   
  prc           $color1 $running_processes ${color} $alignr $processes 
  upt           $color1 $uptime ${color} 
  dsk           $color1 $diskio ${color} ${alignr} ${execi 60 sudo hddtemp /dev/sdc | awk '{print $4}' | sed 's/°C//g'}/${execi 60 sudo hddtemp /dev/sdd | awk '{print $4}' | sed 's/°C//g'}
  net           $color1 ${downspeed eno1}d ${color} $alignr ${color} ${upspeed eno1}u
  arc           $color1 ${diskio_read /dev/sda}r $alignr ${color} ${diskio_write /dev/sda}w
  pri           $color1 ${diskio_read /dev/sdc}r $alignr ${color} ${diskio_write /dev/sdc}w
  tro           $color1 ${diskio_read /dev/sdd}r $alignr ${color} ${diskio_write /dev/sdd}w

${color1}${top cpu 1}	  ${top pid 1}	   ${top name 1}${color}
${top cpu 2}	  ${top pid 2}	   ${top name 2}
${top cpu 3}	  ${top pid 3}	   ${top name 3}
${top cpu 4}	  ${top pid 4}	   ${top name 4}
${top cpu 5}	  ${top pid 5}	   ${top name 5}

${color1}${top_mem mem 1}	  ${top_mem pid 1}	   ${top_mem name 1}${color}
${top_mem mem 2}	  ${top_mem pid 2}	   ${top_mem name 2}
${top_mem mem 3}	  ${top_mem pid 3}	   ${top_mem name 3}
${top_mem mem 4}	  ${top_mem pid 4}	   ${top_mem name 4}
${top_mem mem 5}	  ${top_mem pid 5}	   ${top_mem name 5}

${execpi 1 sudo blkid -o list | sed -e '1,2d' -e 's/(not mounted)/ [] /' -e 's./dev/..' |  awk '{print " ",$1,$2,"$color1",$3,"$color","$alignr",$4,""}'}

]]


