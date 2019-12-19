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
   
   angel = ((2*math.pi/inum)*ival+var1)
   txs = 0 + radi*(math.sin(angel))
   tys = 0 - radi*(math.cos(angel))

   cairo_select_font_face (cr, font, CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL);
   cairo_set_font_size (cr, 22);
   cairo_set_source_rgba (cr, tred, tgreen, tblue, talp); 
  
   cairo_translate (cr, txs+horiz, tys+verti)
   cairo_rotate (cr, (deg*(ival+var2)*(math.pi/180)))

   cairo_rectangle (cr, 0, 0, barw, -text)
   cairo_fill (cr)

   cairo_rotate (cr, -1*(deg*(ival+var2)*(math.pi/180)))
   cairo_translate (cr, -1*(txs+horiz), -1*(tys+verti))

   --you can reinstate these lines to get a text output around the circle
   --cairo_move_to (cr, txs+horiz, tys+verti);
   --cairo_rotate (cr, (deg*(ival+var2)*(math.pi/180)))
   --cairo_show_text (cr, (text))
   --cairo_show_text (cr, (addzero100(text)))
   --cairo_rotate (cr, ((deg*(ival+var2)*(math.pi/180)*-1)))
end

function dotwriting(inum, text, ival, font, fsize, radi, barw, barh, horiz, verti, tred, tgreen, tblue, talp, var1, var2)

   if text == 0 then
      return
   end  
   
   deg = 360/inum
   
   angel = ((2*math.pi/inum)*ival+var1)
   txs = 0 + radi*(math.sin(angel))
   tys = 0 - radi*(math.cos(angel))

   cairo_select_font_face (cr, font, CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL);
   cairo_set_font_size (cr, 22);
   cairo_set_source_rgba (cr, tred, tgreen, tblue, talp); 
  
   cairo_translate (cr, txs+horiz, tys+verti)
   cairo_rotate (cr, (deg*(ival+var2)*(math.pi/180)))

   cairo_rectangle (cr, 0, -text, barw, barh)
   cairo_fill (cr)

   cairo_rotate (cr, -1*(deg*(ival+var2)*(math.pi/180)))
   cairo_translate (cr, -1*(txs+horiz), -1*(tys+verti))

end


function linewriting( texts, radi, lwd, horiz, verti,   r,   g,   b, talp, falph, var1, var2 )
   
   cairo_set_line_width(cr, lwd)
   cairo_set_source_rgba (cr, r, g, b, talp);
   
   for k = 1, tonumber(slots) do

      if texts[k] == 0 then -- dont print zero circle
         goto ende
      end
      
      angel = (2*math.pi/tonumber(slots)) * k + var1
      x = horiz + (radi+texts[k]) * (math.sin(angel))
      y = verti - (radi+texts[k]) * (math.cos(angel))

      cairo_line_to(cr, x, y)

      ::ende::
   end
  
   -- now remove the inside of the circle
   cairo_stroke_preserve(cr);
   cairo_set_source_rgba (cr, r, g, b, falph);
   cairo_arc(cr, horiz, verti, radi, math.pi, 3*math.pi)
   cairo_fill(cr);
   
end


function conky_ring_graph()
   
   if conky_window == nil then return end
   
   local cs = cairo_xlib_surface_create(conky_window.display, conky_window.drawable, conky_window.visual, conky_window.width, conky_window.height)
   cr = cairo_create(cs)
   
   local updates = tonumber(conky_parse('${updates}'))
   if updates == 1 then

      -- init vars
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

      -- fill arrays with 0
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

   -- get vars from conky
   cursec = tonumber( os.date("%S") + os.date("%M")*60 ) --cursec = updates % slots
      
   cpu0[cursec]       = tonumber(conky_parse('${cpu cpu0}'))
   cpu1[cursec]       = tonumber(conky_parse('${cpu cpu1}'))
   cpu2[cursec]       = tonumber(conky_parse('${cpu cpu2}'))
   cpu3[cursec]       = tonumber(conky_parse('${cpu cpu3}'))
   cpu4[cursec]       = tonumber(conky_parse('${cpu cpu4}'))
   cpufreq[cursec]    = tonumber(conky_parse('${freq}')) / 40 -- in % of 4000 mhz
   gpu[cursec]        = tonumber(conky_parse('${nvidia gpuutil}')) 
   gpufreq[cursec]    = tonumber(conky_parse('${nvidia gpufreq}')) / 8.22 -- in % of 822 mhz
   gpumem[cursec]     = tonumber(conky_parse('${nvidia memutil}'))
   gpumemfreq[cursec] = tonumber(conky_parse('${nvidia memfreq}'))/ 20.04 -- in % of 2004 mhz
   mem[cursec]        = tonumber(conky_parse('${memperc}'))
   down[cursec]       = tonumber(conky_parse('${downspeedf eno1}')) / 120 -- in % of 12000 kib
   up[cursec]         = tonumber(conky_parse('${upspeedf eno1}'))/ 120 -- in % of 1200 kib

   --linewriting( texts, radi, lwd, horiz, verti,   r,   g,   b, talp, falph, var1, var2 )
   linewriting( cpufreq,  330,   1,   960,   550, 255, 255, 255,    1,     0,    0,    0 )
   linewriting( cpu0   ,  330,   1,   960,   550, 255,   0,   0,    1,     0,    0,    0 )
   
   for i = 1, tonumber(slots) do

      -- fading from 100 to 30 perc
      if i < cursec then
         fade = (30 - cursec + i + slots )/slots
      else
         fade = (30 + i - cursec)/slots
      end
      
      --circlewriting (inum, text, ival, font, fsize, radi, horiz, verti, tred, tgreen, tblue, var1, var2)
      circlewriting( slots, gpu[i],        i, "Mono", 12,   0, 1, 10,  960, 550, 255, 255, 255, .5*fade, 0, 0 )
      circlewriting( slots, cpu1[i],       i, "Mono", 12, 220, 1, 10,  960, 550, 255,   0,   0,    fade, 0, 0 )
      circlewriting( slots, cpu2[i],       i, "Mono", 12, 220, 1, 10,  960, 550, 255, 255, 255,    fade, 0, 0 )
      circlewriting( slots, cpu3[i],       i, "Mono", 12, 220, 1, 10,  960, 550, 255, 255, 255,    fade, 0, 0 )
      circlewriting( slots, cpu4[i],       i, "Mono", 12, 220, 1, 10,  960, 550, 255, 255, 255,    fade, 0, 0 )
      circlewriting( slots, down[i],       i, "Mono", 12, 110, 1, 10, 1700, 850, 255, 255, 255,    fade, 0, 0 )
      circlewriting( slots, up[i],         i, "Mono", 12,   0, 1, 10, 1700, 850, 255, 255, 255,    fade, 0, 0 )
      --circlewriting( slots, gpumem[i],     i, "Mono", 12,   0, 1, 10, 1700, 280, 255, 255, 255, .5*fade, 0, 0 ) -- dont need that
      circlewriting( slots, mem[i],        i, "Mono", 12, 110, 1, 10, 1700, 280, 255, 255, 255, .3*fade, 0, 0 )

      dotwriting(   slots, gpumemfreq[i], i, "Mono", 12,   0, 2,  2, 1700, 280, 255, 255, 255,    fade, 0, 0 )
      dotwriting(   slots, gpufreq[i],    i, "Mono", 12,   0, 2,  2, 1700, 280, 255, 255, 255,    fade, 0, 0 )
      dotwriting(   slots, cpufreq[i],    i, "Mono", 12, 120, 2,  2,  960, 550, 255, 255, 255,    fade, 0, 0 )

   end
   
   cairo_destroy(cr)
   cairo_surface_destroy(cs)
   
end
