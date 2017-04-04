--[[
Clock Rings by londonali1010 (2009)

This script draws percentage meters as rings, and also draws clock hands if you want! It is fully customisable; all options are described in the script. This script is based off a combination of my clock.lua script and my rings.lua script.

IMPORTANT: if you are using the 'cpu' function, it will cause a segmentation fault if it tries to draw a ring straight away. The if statement near the end of the script uses a delay to make sure that this doesn't happen. It calculates the length of the delay by the number of updates since Conky started. Generally, a value of 5s is long enough, so if you update Conky every 1s, use update_num > 5 in that if statement (the default). If you only update Conky every 2s, you should change it to update_num > 3; conversely if you update Conky every 0.5s, you should use update_num > 10. ALSO, if you change your Conky, is it best to use "killall conky; conky" to update it, otherwise the update_num will not be reset and you will get an error.

To call this script in Conky, use the following (assuming that you save this script to ~/scripts/rings.lua):
  lua_load ~/scripts/rings.lua
  lua_draw_hook_pre clock_rings

]]

------------------------------ VARIABLES ------------------------------

-----color -----
default=0x5C5449
white=0xffffff
orange=0xFF8E38
blue=0x39aaff

----- clock ------
--Use these settings to define the origin and extent of your clock.
  clock_r=70

--Colour & alpha of the clock hands
  clock_colour=white
  clock_alpha=0

--Show the seconds hand ?
  show_seconds=true

--Coordinates of the centre of the clock (hands+rings) , in pixels, from the top left of the Conky window.
  clock_x=400
  clock_y=150

------ cpu% ------
  cpu_x=350
  cpu_y=570
  cpu_color=white

------ mem% ------
  mem_x=330
  mem_y=330

------ downspeed/KiB  ------
  down_x=120
  down_y=1000

------ uppeed/KiB  ------
  up_x=down_x
  up_y=down_y

------ /% ------
  fs0_x=340
  fs0_y=880
 
------ % ------
  fs1_x=fs0_x
  fs1_y=fs0_y

------ thr33% ------
  fs2_x=fs0_x
  fs2_y=fs0_y

-- optienal: battery, wireless (analog eth), link, mpd (mpd_percent/bar dont work)?

settings_table = {
------------------------------ SETINGS ------------------------------
   --[[
             -- Edit this table to customise your rings.
        -- You can create more rings simply by adding more elements to settings_table.
        -- "name" is the type of stat to display; you can choose from 'cpu', 'memperc', 'fs_used_perc', 'battery_used_perc'.
        name='time',
        -- "arg" is the argument to the stat type, e.g. if in Conky you would write ${cpu cpu0}, 'cpu0' would be the argument. If you would not use an argument in the Conky variable, use ''.
        arg='%I.%M',
        -- "max" is the maximum value of the ring. If the Conky variable outputs a percentage, use 100.
        max=12,
        -- "bg_colour" is the colour of the base ring.
        bg_colour=0xffffff,
        -- "bg_alpha" is the alpha value of the base ring.
        bg_alpha=0.1,
        -- "fg_colour" is the colour of the indicator part of the ring.
        fg_colour=0xFF6600,
        -- "fg_alpha" is the alpha value of the indicator part of the ring.
        fg_alpha=0.2,
        -- "x" and "y" are the x and y coordinates of the centre of the ring, relative to the top left corner of the Conky window.
        x=100, y=150,
        -- "radius" is the radius of the ring.
        radius=50,
        -- "thickness" is the thickness of the ring, centred around the radius.
        thickness=5,
        -- "start_angle" is the starting angle of the ring, in degrees, clockwise from top. Value can be either positive or negative.
        start_angle=0,
        -- "end_angle" is the ending angle of the ring, in degrees, clockwise from top. Value can be either positive or negative, but must be larger than start_angle.
        end_angle=360
--]]
------------------------------ CLOCK ------------------------------
{
    name='time',
    arg='%S',
    max=60,
    bg_colour=default,
    bg_alpha=0,
    fg_colour=white,
    fg_alpha=0.3,
    x=clock_x, y=clock_y,
    radius=80,
    thickness=3,
    start_angle=0,
    end_angle=360
  },
{
    name='time',
    arg='%M.%S',
    max=60,
    bg_colour=orange,
    bg_alpha=0,
    fg_colour=white,
    fg_alpha=0.3,
    x=clock_x, y=clock_y,
    radius=64,
    thickness=10,
    start_angle=0,
    end_angle=360
  },
{
    name='time',
    arg='%I.%M',
    max=12,
    bg_colour=blue,
    bg_alpha=0,
    fg_colour=blue,
    fg_alpha=0.4,
    x=clock_x, y=clock_y,
    radius=54,
    thickness=5,
    start_angle=0,
    end_angle=360
  },
------------------------------ CPU ------------------------------
{
    name='freq',
    arg='',
    max=4300,
    bg_colour=0xffffff,
    bg_alpha=0.1,
    fg_colour=orange,
    fg_alpha=0.3,
    x=cpu_x+80, y=cpu_y,
    radius=7,
    thickness=15,
    start_angle=0,
    end_angle=360
  },
{
    name='cpu',
    arg='cpu1',
    max=100,
    bg_colour=0xffffff,
    bg_alpha=0.1,
    fg_colour=cpu_color,
    fg_alpha=0.3,
    x=cpu_x+66, y=cpu_y,
    radius=35,
    thickness=4,
    start_angle=-90,
    end_angle=270
  },
{
    name='cpu',
    arg='cpu2',
    max=100,
    bg_colour=0xffffff,
    bg_alpha=0.1,
    fg_colour=cpu_color,
    fg_alpha=0.3,
    x=cpu_x+48, y=cpu_y,
    radius=60,
    thickness=3,
    start_angle=0,
    end_angle=360
  },
{
    name='cpu',
    arg='cpu3',
    max=100,
    bg_colour=0xffffff,
    bg_alpha=0.1,
    fg_colour=cpu_color,
    fg_alpha=0.3,
    x=cpu_x+24, y=cpu_y,
    radius=90,
    thickness=2,
    start_angle=90,
    end_angle=450
  },
{
    name='cpu',
    arg='cpu4',
    max=100,
    bg_colour=0xffffff,
    bg_alpha=0.1,
    fg_colour=cpu_color,
    fg_alpha=0.3,
    x=cpu_x, y=cpu_y,
    radius=120,
    thickness=1,
    start_angle=180,
    end_angle=540
},
--[[
{
    name='cpu',
    arg='cpu0',
    max=100,
    bg_colour=0xffffff,
    bg_alpha=0,
    fg_colour=cpu_color,
    fg_alpha=0.6,
    x=cpu_x, y=cpu_y,
    radius=50,
    thickness=100,
    start_angle=45,
    end_angle=135
  },
{
    name='cpu',
    arg='cpu1',
    max=100,
    bg_colour=0xffffff,
    bg_alpha=0,
    fg_colour=cpu_color,
    fg_alpha=0.6,
    x=cpu_x, y=cpu_y,
    radius=50,
    thickness=100,
    start_angle=135,
    end_angle=225
  },
{
    name='cpu',
    arg='cpu2',
    max=100,
    bg_colour=0xffffff,
    bg_alpha=0,
    fg_colour=cpu_color,
    fg_alpha=0.6,
    x=cpu_x, y=cpu_y,
    radius=50,
    thickness=100,
    start_angle=225,
    end_angle=315
},
{
    name='cpu',
    arg='cpu3',
    max=100,
    bg_colour=white,
    bg_alpha=0,
    fg_colour=cpu_color,
    fg_alpha=0.6,
    x=cpu_x, y=cpu_y,
    radius=50,
    thickness=100,
    start_angle=315,
    end_angle=405
},
--]]
------------------------------ MEM ------------------------------
{
    name='memperc',
    arg='/',
    max=100,
    bg_colour=white,
    bg_alpha=0.1,
    fg_colour=white,
    fg_alpha=0.6,
    x=mem_x, y=mem_y,
    radius=80,
    thickness=1,
    start_angle=180,
    end_angle=540
},
------------------------------ UP/DOWN ------------------------------
{
    name='downspeedf',
    arg='eno1',
    max=8000,
    bg_colour=white,
    bg_alpha=0.1,
    fg_colour=white,
    fg_alpha=0.2,
    x=down_x, y=down_y,
    radius=50,
    thickness=20,
    start_angle=0,
    end_angle=360
  },
{
    name='upspeedf',
    arg='eno1',
    max=800,
    bg_colour=white,
    bg_alpha=0.1,
    fg_colour=white,
    fg_alpha=0.6,
    x=up_x, y=up_y,
    radius=65,
    thickness=5,
    start_angle=0,
    end_angle=360
},
------------------------------ FS ------------------------------
{
    name='fs_used_perc',
    arg='/',
    max=100,
    bg_colour=white,
    bg_alpha=0.1,
    fg_colour=orange,
    fg_alpha=1,
    x=fs0_x, y=fs0_y,
    radius=100,
    thickness=84,
    start_angle=260,
    end_angle=270
  },
{
    name="fs_used_perc",
    arg="/media/prime",
    max=100,
    bg_colour=white,
    bg_alpha=0.1,
    fg_colour=white,
    fg_alpha=.4,
    x=fs1_x, y=fs1_y,
    radius=63,
    thickness=10,
    start_angle=270,
    end_angle=360
  },
{
    name="fs_used_perc",
    arg="/media/troika",
    max=100,
    bg_colour=white,
    bg_alpha=0.1,
    fg_colour=white,
    fg_alpha=0.4,
    x=fs2_x, y=fs2_y,
    radius=140,
    thickness=2,
    start_angle=0,
    end_angle=260
  },
}


----------- crazy shit -----------

require 'cairo'

function rgb_to_r_g_b(colour,alpha)
  return ((colour / 0x10000) % 0x100) / 255., ((colour / 0x100) % 0x100) / 255., (colour % 0x100) / 255., alpha
end

function draw_ring(cr,t,pt)
  local w,h=conky_window.width,conky_window.height

  local xc,yc,ring_r,ring_w,sa,ea=pt['x'],pt['y'],pt['radius'],pt['thickness'],pt['start_angle'],pt['end_angle']
  local bgc, bga, fgc, fga=pt['bg_colour'], pt['bg_alpha'], pt['fg_colour'], pt['fg_alpha']

  local angle_0=sa*(2*math.pi/360)-math.pi/2
  local angle_f=ea*(2*math.pi/360)-math.pi/2
  local t_arc=t*(angle_f-angle_0)

  --Draw background ring
  cairo_arc(cr,xc,yc,ring_r,angle_0,angle_f)
  cairo_set_source_rgba(cr,rgb_to_r_g_b(bgc,bga))
  cairo_set_line_width(cr,ring_w)
  cairo_stroke(cr)

  --Draw indicator ring
  cairo_arc(cr,xc,yc,ring_r,angle_0,angle_0+t_arc)
  cairo_set_source_rgba(cr,rgb_to_r_g_b(fgc,fga))
  cairo_stroke(cr)
end

function draw_clock_hands(cr,xc,yc)
  local secs,mins,hours,secs_arc,mins_arc,hours_arc
  local xh,yh,xm,ym,xs,ys

  secs=os.date("%S")
  mins=os.date("%M")
  hours=os.date("%I")

  secs_arc=(2*math.pi/60)*secs
  mins_arc=(2*math.pi/60)*mins+secs_arc/60
  hours_arc=(2*math.pi/12)*hours+mins_arc/12

  --Draw hour hand
  xh=xc+0.65*clock_r*math.sin(hours_arc)
  yh=yc-0.65*clock_r*math.cos(hours_arc)
  cairo_move_to(cr,xc,yc)
  cairo_line_to(cr,xh,yh)
  --
  cairo_set_line_cap(cr,CAIRO_LINE_CAP_ROUND)
  cairo_set_line_width(cr,5)
  cairo_set_source_rgba(cr,rgb_to_r_g_b(clock_colour,clock_alpha))
  cairo_stroke(cr)

  --Draw minute hand
  xm=xc+0.95*clock_r*math.sin(mins_arc)
  ym=yc-0.95*clock_r*math.cos(mins_arc)
  cairo_move_to(cr,xc,yc)
  cairo_line_to(cr,xm,ym)
  --
  cairo_set_line_width(cr,3)
  cairo_stroke(cr)

  -- Draw seconds hand
  if show_seconds then
    xs=xc+1.1*clock_r*math.sin(secs_arc)
    ys=yc-1.1*clock_r*math.cos(secs_arc)
    cairo_move_to(cr,xc,yc)
    cairo_line_to(cr,xs,ys)
    --
    cairo_set_line_width(cr,1)
    cairo_stroke(cr)
  end
end

function conky_clock_rings()
   local function setup_rings(cr,pt)
      local str=''
      local value=0

      str=string.format('${%s %s}',pt['name'],pt['arg'])
      str=conky_parse(str)
      
      value=tonumber(str)
      if value == nil then value = 0 end
      
      --Les ajouts suivants permettent de corriger le retard prit par les anneaux
      --Ajout wlourf : conversion des minutes en centièmes d'heures
      if pt['arg'] == "%I.%M"  then
         value=os.date("%I")+os.date("%M")/60
         if value>12 then value=value-12 end
      end
      
      --Ajout Fenouille84 : conversion des secondes en centièmes de minutes
      if pt['arg'] == "%M.%S"  then
         value=os.date("%M")+os.date("%S")/60
      end
      --Fin ajout
      
      pct=value/pt['max']
      draw_ring(cr,pct,pt)
   end

--Check that Conky has been running for at least 5s
--removed because everything is up in a split-second...
--otherwise just increase the number after 'if update_num>0 then' ten lines below.

  if conky_window==nil then return end
  local cs=cairo_xlib_surface_create(conky_window.display,conky_window.drawable,conky_window.visual, conky_window.width,conky_window.height)

  local cr=cairo_create(cs)  

  local updates=conky_parse('${updates}')
  update_num=tonumber(updates)

  if update_num>0 then
    for i in pairs(settings_table) do
      setup_rings(cr,settings_table[i])
    end
  end

  draw_clock_hands(cr,clock_x,clock_y)
end
