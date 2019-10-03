-- http://thepeachyblog.blogspot.co.at/2010/02/here-is-another-lua-script.html

require 'cairo'

--[[
function addzero100(num)
   if tonumber(num) <>
      return "00" .. num
      return "0" .. num 
      else return num 
   end 
end
--]]

function circlewriting(inum, text, ival, font, fsize, radi, barw, barh, horiz, verti, tred, tgreen, tblue, talp, var1, var2)
   --[[ Above is the function name followed by a long list of strings that the function needs to work.
      inum - "length" ie the number of instances that the data will move to. In the above image this number was 10.
      text - the actual data that is being represented, in this case cpu usage.
      ival - in the below function an array is used to generate the data and in that array we will be using the code line:
      for i = 1, tonumber(slots) do
      slots is the same number as "inum" above. So "i" is all the numbers between 1 and 10.
      font - to set the font for the text output
      fsize - sets the font size
      radi - sets the radius of the circle around which the bars or text will be displayed.
      horiz - horizontal, x, position.
      verti - vertical, y, position
      tred - output red color value
      tgreen - output green color value
      tblue - oputput blue color value
      talp - oputput alpha, set to ival/10 to enable fading
      var1 - variable 1 ...variables 1 and 2 are used to fine tune the position and orientation of the bars or text around the circle
      var2 - variable 2
   --]]
   
   deg = 360/inum
   
   text_arc = ((2*math.pi/inum)*ival+var1)
   txs = 0 + radi*(math.sin(text_arc))
   tys = 0 - radi*(math.cos(text_arc))

   cairo_select_font_face (cr, font, CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL);
   cairo_set_font_size (cr, 22);
   cairo_set_source_rgba (cr, tred, tgreen, tblue, talp); 
   
   cairo_translate (cr, txs+horiz, tys+verti)

   cairo_rotate (cr, (deg*(ival+var2)*(math.pi/180)))

   --cairo_rectangle (cr, 0, 0, 10, (text*-1))
   cairo_rectangle (cr, 0, 0, barw, (text*-1))
   cairo_fill (cr)

   cairo_rotate (cr, ((deg*(ival+var2)*(math.pi/180)*-1)))

   cairo_translate (cr, -1*(txs+horiz), -1*(tys+verti))

   --you can reinstate these lines to get a text output around the circle
   --cairo_move_to (cr, txs+horiz, tys+verti);
   --cairo_rotate (cr, (deg*(ival+var2)*(math.pi/180)))
   --cairo_show_text (cr, (text))
   --cairo_show_text (cr, (addzero100(text)))
   --cairo_rotate (cr, ((deg*(ival+var2)*(math.pi/180)*-1)))
end

function conky_ring_graph()
   if conky_window == nil then return end
   local cs = cairo_xlib_surface_create(conky_window.display, conky_window.drawable, conky_window.visual, conky_window.width, conky_window.height)
   cr = cairo_create(cs)
   
   local updates = tonumber(conky_parse('${updates}'))
   if updates == 1 then
      
      slots = 3600     
      cpu0 = {}
      cpu1 = {}
      cpu2 = {}
      cpu3 = {}
      cpu4 = {}
      cpufreq = {}
      gpu = {}
      gpufreq = {}
      gpumem = {}
      gpumemfreq = {}
      mem = {}
      down = {}
      up = {}
      
      for i = 1, tonumber(slots) do        
         if cpu0[i] == nil then cpu0[i] = 0 end
         if cpu1[i] == nil then cpu1[i] = 0 end
         if cpu2[i] == nil then cpu2[i] = 0 end
         if cpu3[i] == nil then cpu3[i] = 0 end
         if cpu4[i] == nil then cpu4[i] = 0 end
         if cpufreq[i] == nil then cpufreq[i] = 0 end
         if gpu[i] == nil then gpu[i] = 0 end
         if gpufreq[i] == nil then gpufreq[i] = 0 end
         if gpumem[i] == nil then gpumem[i] = 0 end
         if gpumemfreq[i] == nil then gpumemfreq[i] = 0 end
         if mem[i] == nil then mem[i] = 0 end
         if down[i] == nil then down[i] = 0 end
         if up[i] == nil then up[i] = 0 end
      end
      
   end

   cursec = tonumber( os.date("%S") + os.date("%M")*60 ) --cursec = updates % slots
      
   cpu0[cursec]       = tonumber(conky_parse('${cpu}'))
   cpu1[cursec]       = tonumber(conky_parse('${cpu cpu1}'))
   cpu2[cursec]       = tonumber(conky_parse('${cpu cpu2}'))
   cpu3[cursec]       = tonumber(conky_parse('${cpu cpu3}'))
   cpu4[cursec]       = tonumber(conky_parse('${cpu cpu4}'))
   cpufreq[cursec]    = tonumber(conky_parse('${freq}')) / 43 -- in % of 4300 mhz
   gpu[cursec]        = tonumber(conky_parse('${nvidia gpuutil}')) 
   gpufreq[cursec]    = tonumber(conky_parse('${nvidia gpufreq}')) / 8.22 -- in % of 822 mhz
   gpumem[cursec]     = tonumber(conky_parse('${nvidia memutil}'))
   gpumemfreq[cursec] = tonumber(conky_parse('${nvidia memfreq}'))/ 20.04 -- in % of 2004 mhz
   mem[cursec]        = tonumber(conky_parse('${memperc}'))
   down[cursec]       = tonumber(conky_parse('${downspeedf eno1}')) / 120 -- in % of 12000 kib
   up[cursec]         = tonumber(conky_parse('${upspeedf eno1}'))/ 120 -- in % of 1200 kib
      
   for i = 1, tonumber(slots) do

      -- fading from 100 to 30 perc
      if i < cursec then
         fade = (30 + slots - cursec + i)/slots
      else
         fade = (30 + i - cursec)/slots
      end
               
      --circlewriting (inum, text, ival, font, fsize, radi, horiz, verti, tred, tgreen, tblue, var1, var2)
      circlewriting(slots, gpumemfreq[i], i, "Mono", 12,   0, 1, 10,  960, 550, 255, 255, 255, .3*fade, 0, 0)     
      circlewriting(slots, gpufreq[i],    i, "Mono", 12,   0, 1, 10,  960, 550, 255, 255, 255, .3*fade, 0, 0)
      circlewriting(slots, gpu[i],        i, "Mono", 12, 110, 1, 10,  960, 550, 255, 255, 255, .5*fade, 0, 0)
      circlewriting(slots, cpufreq[i],    i, "Mono", 12, 220, 1, 10,  960, 550, 255, 255, 255, .5*fade, 0, 0)
      --circlewriting(slots, cpu0[i],       i, "Mono", 12, 400, 1, 10,  960, 550, 255, 255, 255, .4*fade, 0, 0)
      circlewriting(slots, cpu1[i],       i, "Mono", 12, 330, 1, 10,  960, 550, 255, 255, 255, .2*fade, 0, 0)
      circlewriting(slots, cpu2[i],       i, "Mono", 12, 330, 1, 10,  960, 550, 255, 255, 255, .2*fade, 0, 0)
      circlewriting(slots, cpu3[i],       i, "Mono", 12, 330, 1, 10,  960, 550, 255, 255, 255, .2*fade, 0, 0)
      circlewriting(slots, cpu4[i],       i, "Mono", 12, 330, 1, 10,  960, 550, 255, 255, 255, .2*fade, 0, 0)
      circlewriting(slots, down[i],       i, "Mono", 12, 100, 1, 10, 1700, 850, 255, 255, 255,    fade, 0, 0)
      circlewriting(slots, up[i],         i, "Mono", 12,   0, 1, 10, 1700, 850, 255, 255, 255,    fade, 0, 0)
      circlewriting(slots, gpumem[i],     i, "Mono", 12,   0, 1, 10, 1700, 280, 255, 255, 255, .5*fade, 0, 0)
      circlewriting(slots, mem[i],        i, "Mono", 12, 100, 1, 10, 1700, 280, 255, 255, 255, .3*fade, 0, 0)
      
   end
   
   cairo_destroy(cr)
   cairo_surface_destroy(cs)
   
end
