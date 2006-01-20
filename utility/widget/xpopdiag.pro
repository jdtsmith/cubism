;+
; NAME:  
;
;    XPOPDIAG
;
; DESCRIPTION:
;    
;    Widget for selecting text to display from droplist.
;    
; CATEGORY:
;
;    Widget Utility
;
; CALLING SEQUENCE:
;
;    xpopdiag, poplist, ptrlist, [PARENT_GROUP=,DEFAULT=,LABEL=,TOP_LABEL=,
;                                 BUTTON_TEXT=,MODL=,TSIZE=,FREE_POINTERS=]
;
; INPUT PARAMETERS:
;
;    poplist: List of popup choices to offer.
;
;    ptrlist: Corresponding list of pointers to string arrays to
;       display.
;    
; INPUT KEYWORD PARAMETERS:
;
;    PARENT_GROUP: The widget ID of the group leader.
;
;    DEFAULT: Which block to show by default.
;
;    LABEL: A text label to put next to the popup choice.
;
;    TOP_LABEL: A descriptive label to put at the top of the widget.
;
;    BUTTON_TEXT: The finished button text, defaults to "OK".
;
;    MODAL: Whether to make the popup modal.
;
;    TSIZE: Size of the text window, 2 element int array, in
;       characters (default [80,15]).
;
;    FREE_POINTERS: Free the textpointers when done.  By default, the
;       caller must do this.
;
; MODIFICATION HISTORY:
;    
;    2002-12-06 (J.D. Smith): Initial import from SCORE-era sources.
;-
;    $Id$
;##############################################################################
;
; LICENSE
;
;  Copyright (C) 2002-2004 J.D. Smith
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

pro xpopdiag_kill,id
  widget_control, id, get_uvalue=uv
  if uv.Free then ptr_free,uv.ptr
end

pro xpopdiag_event, ev
   evtype=tag_names(ev,/structure_name)
   if evtype eq 'WIDGET_BUTTON' then begin
      widget_control, ev.top,/DESTROY
      return
   endif 
   if evtype eq 'WIDGET_LIST' then return
   widget_control, ev.top, get_uvalue=uv
   top=widget_info(uv.wText,/LIST_TOP) 
   widget_control, uv.wText, set_value=*uv.Ptr[ev.index],SET_LIST_TOP=top
 end

pro xpopdiag, poplist, ptrlist, PARENT_GROUP=parent, _EXTRA=e, DEFAULT=def,  $
              LABEL=lbl,TOP_LABEL=tlbl,BUTTON_TEXT=bv, MODAL=md,TSIZE=ts, $
              FREE_POINTERS=fp
   if n_elements(ts) eq 0 then ts=[80,15]
   if n_elements(bv) eq 0 then bv='  OK  '
   if n_elements(def) eq 0 then def=0
   if keyword_set(parent) then begin 
      base=widget_base(/COLUMN,MODAL=md,GROUP_LEADER=parent,_EXTRA=e, $
                       /BASE_ALIGN_CENTER,SPACE=1)
   endif else begin 
      sc=get_screen_size()
      base=widget_base(/COLUMN,_EXTRA=e,/BASE_ALIGN_CENTER,  $
                       XOFFSET=sc[0]/2-20, YOFFSET=sc[1]/2-20)
   endelse 
   
   if n_elements(tlbl) ne 0 then l=widget_label(base,value=tlbl)
   if n_elements(lbl) ne 0 then begin 
      b2=widget_base(base,/ROW)
      l=widget_label(b2,value=lbl)
   endif else b2=base
   list=widget_combobox(b2,value=poplist)
   text=widget_list(base,value=*ptrlist[def],/FRAME,XSIZE=TS[0],YSIZE=TS[1])
   button=widget_button(base,value=bv)
   widget_control, list, set_combobox_select=def
   widget_control, base,set_uvalue={wText:text,Ptr:ptrlist, $
                                    Free:keyword_set(fp)},/REALIZE
   XManager, 'xpopdiag', base,CLEANUP='xpopdiag_kill',/NO_BLOCK
end

