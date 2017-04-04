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

function circlewriting(inum, text, ival, font, fsize, radi, barw, barh, horiz, verti, tred, tgreen, tblue, var1, var2)
   --[[ Above is the function name followed by a long list of strings that the function needs to work.
      inum - "length" ie the number of instances that the data will move to. In the above image this number was 10.
      text - the actual data that is being represented, in this case cpu usage.
      ival - in the below function an array is used to generate the data and in that array we will be using the code line:
      for i = 1, tonumber(len_t) do
      len_t is the same number as "inum" above. So "i" is all the numbers between 1 and 10.
      font - to set the font for the text output
      fsize - sets the font size
      radi - sets the radius of the circle around which the bars or text will be displayed.
      horiz - horizontal, x, position.
      verti - vertical, y, position
      tred - output red color value
      tgreen - output green color value
      tblue - oputput blue color value
      var1 - variable 1 ...variables 1 and 2 are used to fine tune the position and orientation of the bars or text around the circle
      var2 - variable 2
   --]]
   deg=360/inum
   
   text_arc=((2*math.pi/inum)*ival+var1)
   txs=0+radi*(math.sin(text_arc))
   tys=0-radi*(math.cos(text_arc))

   cairo_select_font_face (cr, font, CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL);
   cairo_set_font_size (cr, 22);
   cairo_set_source_rgba (cr, tred, tgreen, tblue, ival/360); -- last input is alpha, set to ival/10 to enable fading
   
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
   --cairo_show_text (cr, (text)) --cairo_show_text (cr, (addzero100(text)))
   --cairo_rotate (cr, ((deg*(ival+var2)*(math.pi/180)*-1)))
end

function conky_ring_graph()
   if conky_window == nil then return end
   local cs = cairo_xlib_surface_create(conky_window.display, conky_window.drawable, conky_window.visual, conky_window.width, conky_window.height)
   cr = cairo_create(cs)

   local updates=tonumber(conky_parse('${updates}'))
   if updates==1 then 
      len_t=360
      t1={} 
   end

   if updates> 3 then
      for i = 1, tonumber(len_t) do
         if t1[i+1]==nil then t1[i+1]=0 end
         t1[i]=t1[i+1]
         if i==len_t then
            t1[len_t]=tonumber(conky_parse('${cpu}'))
         end

         --circlewriting (inum, text, ival, font, fsize, radi, horiz, verti, tred, tgreen, tblue, var1, var2)
         circlewriting(len_t, t1[i], i, "Mono", 12, 300, 3, 10, 960, 540, 255, 255, 255, -0.05, 0)
      end
      cairo_destroy(cr)
      cairo_surface_destroy(cs)
   end
end
