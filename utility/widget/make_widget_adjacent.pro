;+
; NAME:  
;
;    MAKE_WIDGET_ADJACENT
;
; DESCRIPTION:
;    
;    Place a widget base adjacent to another.
;    
; CATEGORY:
;
;    Widget Utility
;
; CALLING SEQUENCE:
;
;    make_widget_adjacent,widget,base
;
; INPUT PARAMETERS:
;
;    widget: The widget ID to move.
;
;    base: The widget base to which to make this widget adjacent to
;       its TLB.
;    
; MODIFICATION HISTORY:
;    
;    2004-02-23 (J.D. Smith): Written
;-
;    $Id$
;##############################################################################
;
; LICENSE
;
;  Copyright (C) 2004 J.D. Smith
;
;  This file is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published
;  by the Free Software Foundation; either version 2, or (at your
;  option) any later version.
;
;  This file is distributed in the hope that it will be useful, but
;  WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;  General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with this file; see the file COPYING.  If not, write to the
;  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;  Boston, MA 02110-1301, USA.
;
;##############################################################################
pro make_widget_adjacent,widget,base
  repeat begin
     tlb=base
     base=widget_info(base,/PARENT)
  endrep until base eq 0
  gb=widget_info(tlb,/GEOMETRY)
  ss=get_screen_size()
  
  gw=widget_info(widget,/GEOMETRY)
  
  left_side=(gb.xoffset+gb.scr_xsize/2) gt ss[0]/2
  xpos=left_side?gb.xoffset-2*gb.margin-gw.scr_xsize-6*gw.xpad: $
       gb.xoffset+2*gb.margin+gb.scr_xsize+2*gb.xpad
  ypos=gb.yoffset-(gb.scr_ysize-gb.ysize-3*gb.ypad)
  widget_control, widget,XOFFSET=xpos,YOFFSET=ypos
end
