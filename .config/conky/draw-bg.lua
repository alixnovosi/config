--[[Background originally by londonali1010 (2009)
    ability to set any size for background mrpeachy 2011
    ability to set variables for bg in conkyrc dk75

  the change is that if you set width and/or height to 0
  then it assumes the width and/or height of the conky window

  modified again by amichaud 2014
so:

OR Both above TEXT (No composite manager required - no blinking!)

 lua_load ~/path/to/draw_bg.lua
 lua_draw_hook_pre draw_bg 10 0 0 0 0 0x000000 0.5
 TEXT

Note
${lua conky_draw_bg 20 0x000000 0.4}
  See below:        1  2        3

${lua conky_draw_bg corner_radius color alpha}

1 = 20             corner_radius
2 = 0x000000      color  - make the background any colour you want.
3 = 0.4           alpha

]]

require 'cairo'
local    cs, cr = nil
function rgb_to_r_g_b(colour,alpha)
return ((colour / 0x10000) % 0x100) / 255., ((colour / 0x100) % 0x100) / 255., (colour % 0x100) / 255., alpha
end
function conky_draw_bg(r,color,alpha)
if conky_window == nil then return end
if cs == nil then cairo_surface_destroy(cs) end
if cr == nil then cairo_destroy(cr) end
local cs = cairo_xlib_surface_create(conky_window.display, conky_window.drawable, conky_window.visual, conky_window.width, conky_window.height)
local cr = cairo_create(cs)
local w=tonumber(conky_window.text_width + conky_window.border_inner_margin)
local h=tonumber(conky_window.text_height + conky_window.border_inner_margin)
local x=tonumber(conky_window.text_start_x - (conky_window.border_inner_margin / 2))
local y=tonumber(conky_window.text_start_y - (conky_window.border_inner_margin / 2))
cairo_set_source_rgba (cr,rgb_to_r_g_b(color,alpha))
--top left mid circle
local xtl=x+r
local ytl=y+r
--top right mid circle
local xtr=(x+r)+((w)-(2*r))
local ytr=y+r
--bottom right mid circle
local xbr=(x+r)+((w)-(2*r))
local ybr=(y+r)+((h)-(2*r))
--bottom right mid circle
local xbl=(x+r)
local ybl=(y+r)+((h)-(2*r))
-----------------------------
cairo_move_to (cr,xtl,ytl-r)
cairo_line_to (cr,xtr,ytr-r)
cairo_arc(cr,xtr,ytr,r,((2*math.pi/4)*3),((2*math.pi/4)*4))
cairo_line_to (cr,xbr+r,ybr)
cairo_arc(cr,xbr,ybr,r,((2*math.pi/4)*4),((2*math.pi/4)*1))
cairo_line_to (cr,xbl,ybl+r)
cairo_arc(cr,xbl,ybl,r,((2*math.pi/4)*1),((2*math.pi/4)*2))
cairo_line_to (cr,xtl-r,ytl)
cairo_arc(cr,xtl,ytl,r,((2*math.pi/4)*2),((2*math.pi/4)*3))
cairo_close_path(cr)
cairo_fill (cr)
------------------------------------------------------------
cairo_surface_destroy(cs)
cairo_destroy(cr)
return ""
end
