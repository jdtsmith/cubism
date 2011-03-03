;+
; NAME:  
;
;    CW_BITMEN
;
; DESCRIPTION:
;    
;    Create a menu with a bitmap head.
;    
; CATEGORY:
;
;    Widget Utility
;
; CALLING SEQUENCE:
;
;    id=cw_bitmen(parent,bitmap,list,lim,[UVALUE=,XSIZE=,YSIZE=,IDS=]
;
; INPUT PARAMETERS:
;
;    parent: The parent widget ID in which to root the menu.
;
;    bitmap: The bitmap array to draw.  Anything WIDGET_BUTTON can
;       take as a value.
;
;    list: List of menu items to place beneath the bitmap.
;
;    lim: Maximum number of menu items to show.
;    
; INPUT KEYWORD PARAMETERS:
;
;    UVALUE: The uvalue to assign the widget.
;
;    (X|Y)SIZE: Size of the top widget button.
;
; OUTPUT KEYWORD PARAMETERS:
; 
;    IDS: The ids of all menu buttons.
;
; OUTPUT:
;
;    id: The id of the compound widget.
;    
; MODIFICATION HISTORY:
;    
;    2002-12-06 (J.D. Smith): Import from SCORE-era sources.
;-
;    $Id$
;##############################################################################
;
; LICENSE
;
;  Copyright (C) 2002 J.D. Smith
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

function cw_bitmen_event,ev
   widget_control,ev.id,get_value=value
   return, { ID:ev.handler, TOP:ev.top, HANDLER:0L, value:value }
end

function cw_bitmen_get_value,id
   stash = WIDGET_INFO(id, /CHILD) ; Retrieve the state
   WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY
   if state.num gt 0 then begin 
      widget_control, state.ids[0], get_value=ret ; Get the value 
      for i=1,state.num-1 do begin 
         widget_control, state.ids[i], get_value=tmp ; Get the value 
         ret=[ret,tmp]          ;put it in an array
      endfor 
   endif  
   WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
   RETURN, ret
end

pro cw_bitmen_set_value,id,value
   stash=widget_info(id,/CHILD)
   WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY
   nv=n_elements(value)
   ;;--- Set the number actually displayed (no greater than state.lim)
   state.num=nv < state.lim
   ;;--- set buttons' values
   for i=0,state.num-1 do begin
      widget_control, state.ids[i], set_value=value[i]
   endfor 
   ;;--- empty the rest
   for i=state.num,state.lim-1 do begin
      widget_control, state.ids[i], set_value=''
   endfor 
   ;;--- restore state
   widget_control, stash, set_uvalue=state, /NO_COPY  
   return
end 

function cw_bitmen,parent,bitmap,list,lim,UVALUE=uval,XSIZE=xs,YSIZE=ys,IDS=ids
   IF (N_ELEMENTS(uval) EQ 0)  THEN uval = 0L
   nl=n_elements(list)
   ids=lonarr(lim)
   base=widget_base(parent,EVENT_FUNC='cw_bitmen_event', $
                    FUNC_GET_VALUE='cw_bitmen_get_value',$
                    PRO_SET_VALUE='cw_bitmen_set_value',UVALUE=uval)
   ;;--- create the bitmap base button
   if n_elements(xs) ne 0 and n_elements(ys) ne 0 then  $
    butbase=widget_button(base,VALUE=bitmap,MENU=1,$
                          xsize=xs,ysize=ys) $
   else $
        butbase=widget_button(base,VALUE=bitmap,MENU=1)
   
   for i=0,nl-1 do begin
      ids[i]=widget_button(butbase,value=list[i],/DYNAMIC_RESIZE)
   endfor 
   for i=nl,lim-1 do begin
      ids[i]=widget_button(butbase,value='',/DYNAMIC_RESIZE)
   endfor 
   state={ids:ids,lim:lim,num:nl}
   vid=widget_info(ids[0],/valid_id)
   if vid ne 1 then message,ids[0]+'is not a valid id'
   ;;--- stash state in first child
   widget_control,butbase,SET_UVALUE=state,/NO_COPY
   return,base
end
