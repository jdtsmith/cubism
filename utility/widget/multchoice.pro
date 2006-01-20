;+
; NAME:  
;
;    MULTCHOICE
;
; DESCRIPTION:
;    
;    Widget for selecting among multiple choices.
;    
; CATEGORY:
;
;    Widget Utility
;
; CALLING SEQUENCE:
;
;    choice=multchoice(text,list,[TITLE=,PARENT_GROUP=,MODAL=,SELECT=,
;                                 NONEXCLUSIVE=]
;
; INPUT PARAMETERS:
;
;    text: Text to place at the top of the choice list.
;
;    list: A list of choices to offer.
;    
; INPUT KEYWORD PARAMETERS:
;
;    TITLE: The title to use for the popup.
;
;    PARENT_GROUP: The widget ID of the group leader.
;
;    MODAL: Whether to make the popup modal.
;
;    SELECT: Index of button to select initially.
;
;    NONEXCLUSIVE: Allow multiple choices to be selected.
;
; OUTPUT:
;
;    choice: The index (indices) selected, or -1 if cancelled.
;    
; MODIFICATION HISTORY:
;    
;    2004-01-05 (J.D. Smith): Initially written.
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

pro multchoice_event,ev
  widget_control, ev.top,GET_UVALUE=ptr
  widget_control, ev.id,GET_UVALUE=uv
  if size(uv,/TYPE) eq 7 then begin 
     if uv eq 'cancel' then *ptr=-1
     widget_control, ev.top,/DESTROY 
     return
  endif 
  (*ptr)[uv]=ev.select
end


function multchoice, text,list,TITLE=ttl,PARENT_GROUP=parent, MODAL=md, $
                     SELECT=sel,NONEXCLUSIVE=nexc,_EXTRA=e
  if n_elements(ttl) eq 0 then ttl='Choose One'
  if n_elements(md) eq 0 then md=0
  if n_elements(sel) ne 0 then sel=0>sel<n_elements(list)
  if keyword_set(nexc) then exc=0 else begin 
     exc=1 & nexc=0
  endelse
  
  if keyword_set(parent) then begin 
     base=widget_base(/COLUMN,MODAL=md,GROUP_LEADER=parent,_EXTRA=e, $
                      /BASE_ALIGN_CENTER,TITLE=ttl,SPACE=1)
  endif else begin 
     sc=get_screen_size()
     base=widget_base(/COLUMN,_EXTRA=e,/BASE_ALIGN_CENTER,  $
                      XOFFSET=sc[0]/2-20,YOFFSET=sc[1]/2-20,TITLE=ttl)
  endelse 
  
  lab=widget_label(base,value=text)
  subbase=widget_base(base,EXCLUSIVE=exc,NONEXCLUSIVE=nexc,/COLUMN,/FRAME) 
  
  p=ptr_new(bytarr(n_elements(list)))
  
  for i=0,n_elements(list)-1 do begin 
     b=widget_button(subbase,value=list[i],uvalue=i)
     if n_elements(sel) gt 0 &&  i eq sel[0] then begin 
        widget_control, b,/SET_BUTTON
        (*p)[i]=1b
     endif 
  endfor 
  row=widget_base(base,/ROW) 
  ok=widget_button(row,value='  OK  ',uvalue='ok')
  cncl=widget_button(row,value='Cancel',uvalue='cancel')
  widget_control, base,/REALIZE,SET_UVALUE=p
  XManager,'multchoice',base
  if (*p)[0] eq -1 then ret=-1 else ret=where(*p)
  ptr_free,p
  return,ret
end
