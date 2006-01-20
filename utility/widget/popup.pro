;+
; NAME:  
;
;    POPUP
;
; DESCRIPTION:
;    
;    Widget for selecting items from a droplist.
;    
; CATEGORY:
;
;    Widget Utility
;
; CALLING SEQUENCE:
;
;    item=popup(label,list, [TITLE=,PARENT_GROUP=,MODAL=,SELECT=,
;                            COMBOBOX=,ENTRY=]
;
; INPUT PARAMETERS:
;
;    label: Label to place at the top.
;
;    list: List of items to choose among in a pop box.
;    
; INPUT KEYWORD PARAMETERS:
;
;    TITLE: The title to give the popped-up widget.
;
;    PARENT_GROUP: The widget ID of the group leader.
;
;    MODAL: Whether to make the popup modal.
;
;    SELECT: Index of item to select by default.
;
;    COMBOBOX: Use a combobox intead of a droplist (good for long lists).
;
;    ENTRY: Label to place aside the pop box.
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

pro popup_event,ev
   widget_control, ev.top, get_uvalue=state
   
   if ev.id eq state.popup then begin 
      *state.index=ev.index
   endif else begin 
      if widget_info(state.entry,/VALID_ID) then begin 
         widget_control, state.entry,GET_VALUE=val
         *state.text=val[0]
      endif 
      widget_control, ev.top, /DESTROY
   endelse 
end

function popup, label, list, TITLE=ttl, PARENT_GROUP=parent, MODAL=md, $
                SELECT=sel,COMBOBOX=cb,ENTRY=entry,_EXTRA=e
   if n_elements(ttl) eq 0 then ttl='Choose One'
   if n_elements(md) eq 0 then md=0
   if n_elements(sel) ne 0 then begin 
      sel=0>sel<n_elements(list)
   endif else sel=0
   
   if keyword_set(parent) then begin 
      base=widget_base(/COLUMN,MODAL=md,GROUP_LEADER=parent,_EXTRA=e, $
                       /BASE_ALIGN_CENTER,TITLE=ttl,SPACE=1)
   endif else begin 
      sc=get_screen_size()
      base=widget_base(/COLUMN,_EXTRA=e,/BASE_ALIGN_CENTER,  $
                       XOFFSET=sc[0]/2-20, YOFFSET=sc[1]/2-20, TITLE=ttl)
   endelse 

   b1=widget_base(base,/ROW)
   nul=widget_label(b1,value=label)
   
   state={popup:0L, entry:0L, OK:0L, index:ptr_new(sel), $
          text:ptr_new(/ALLOCATE_HEAP)}
   
   if keyword_set(cb) then $
      state.popup=widget_combobox(b1,value=list) $
   else state.popup=widget_droplist(b1,value=list)
   
   if keyword_set(entry)  ne 0 then begin 
      lab=widget_label(b1,VALUE=entry)
      state.entry=widget_text(b1,/EDITABLE)
   endif 
   
   widget_control,state.popup,SET_DROPLIST_SELECT=sel
   state.ok=widget_button(base,value='  OK  ')
   widget_control, base, /REALIZE, set_uvalue=state
   XManager, 'popup', base
   if keyword_set(entry) then $
      val={text:*state.text, select:list[*state.index]} $
   else val=list[*state.index]
   ptr_free,state.index,state.text
   return,val
end
