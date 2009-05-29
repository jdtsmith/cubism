;+
; NAME:  
;
;    MULTCHOICE
;
; DESCRIPTION:
;    
;    Widget for selecting among multiple choices, with scrolling.
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
;    2009-05-29 (J.D. Smith): Implemented scrolling. 
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
  widget_control, ev.top,GET_UVALUE=state
  if ev.id eq ev.top then begin 
     ;; Size events
     dsz=state.delta_size
     widget_control, state.base,SCR_XSIZE=ev.X-dsz[0],SCR_YSIZE=ev.Y-dsz[1]
     return
  endif 
  
  widget_control, ev.id,GET_UVALUE=uv
  if size(uv,/TYPE) eq 7 then begin 
     ;; Control Button
     if uv eq 'cancel' then (*state.set)=-1
     widget_control, ev.top,/DESTROY 
     return
  endif else begin 
     ;; Select Button
     (*state.set)[uv]=ev.select
  endelse 
end


function multchoice, text,list,TITLE=ttl,PARENT_GROUP=parent, MODAL=md, $
                     SELECT=sel,NONEXCLUSIVE=nexc,SCROLL=scroll, $
                     YSIZE=ys,_EXTRA=e
  if n_elements(ttl) eq 0 then ttl='Choose One'
  if n_elements(md) eq 0 then md=0
  if n_elements(sel) ne 0 then sel=0>sel<n_elements(list)
  if keyword_set(nexc) then exc=0 else begin 
     exc=1 & nexc=0
  endelse
  
  sc=get_screen_size()
  if keyword_set(parent) then begin 
     base=widget_base(/COLUMN,MODAL=md,GROUP_LEADER=parent,_EXTRA=e, $
                      /BASE_ALIGN_CENTER,TITLE=ttl,SPACE=1,/TLB_SIZE_EVENTS, $
                      UVALUE='tlb')
  endif else begin 
     base=widget_base(/COLUMN,_EXTRA=e,/BASE_ALIGN_CENTER,  $
                      XOFFSET=sc[0]/2-20,YOFFSET=sc[1]/2-20,TITLE=ttl, $
                      TLB_FRAME_ATTR=1,/TLB_SIZE_EVENTS,UVALUE='tlb')
  endelse 
  
  lab=widget_label(base,value=text)
  subbase=widget_base(base,EXCLUSIVE=exc,NONEXCLUSIVE=nexc,/COLUMN,/FRAME, $
                      SCROLL=keyword_set(scroll))
  p=ptr_new(bytarr(n_elements(list)))
  mxw=0
  for i=0,n_elements(list)-1 do begin 
     b=widget_button(subbase,value=list[i],uvalue=i,/DYNAMIC_RESIZE)
     if n_elements(sel) gt 0 &&  i eq sel[0] then begin 
        widget_control, b,/SET_BUTTON
        (*p)[i]=1b
     endif 
     mxw>=(widget_info(b,/GEOMETRY)).SCR_XSIZE
     if n_elements(mxh) eq 0 then mxh=(widget_info(b,/GEOMETRY)).SCR_YSIZE
  endfor 
  row=widget_base(base,/ROW) 
  ok=widget_button(row,value='  OK  ',uvalue='ok')
  cncl=widget_button(row,value='Cancel',uvalue='cancel')
  widget_control, base,/REALIZE
  if keyword_set(scroll) then $
     widget_control, subbase, SCR_XSIZE=mxw*1.1, $
                     SCR_YSIZE=(n_elements(list)<10)*1.3*mxh<(sc[1]/2)
  
  wib=widget_info(base,/GEOMETRY) 
  wisb=widget_info(subbase,/GEOMETRY) 
  widget_control, base,SET_UVALUE={base:subbase,set:p, $
                                   delta_size:[wib.scr_xsize-wisb.scr_xsize,$
                                               wib.scr_ysize-wisb.scr_ysize]}
  
  XManager,'multchoice',base
  if (*p)[0] eq -1 then ret=-1 else ret=where(*p)
  ptr_free,p
  return,ret
end
