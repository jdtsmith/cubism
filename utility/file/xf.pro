;+
; NAME:  
;
;    XF
;
; DESCRIPTION:
;    
;    Select files interactively, with recent directory/files.
;    
; CATEGORY:
;
;    File Utility
;
; CALLING SEQUENCE:
;
;    xf,file, [_EXTRA=, TITLE=, PARENT_GROUP=,GROUP=, $
;              xoffset=, yoffset=,CANCEL_TEXT=,MODAL=]
;    
; INPUT KEYWORD PARAMETERS:
;
;    TITLE: The title to give the selection window.
;
;    PARENT_GROUP: The 
;
;    HMS: If set, format with hms or dms.
;
; OUTPUT:
;
;    file: The selected file(s), or -1 if cancelled.
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
;  Copyright (C) 2001-2005 J.D. Smith
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

pro xf_event,ev
   widget_control,ev.id,get_uvalue=eventval
   ; get the fileptr
   widget_control,ev.top,get_uvalue=fileptr
   case eventval of
      "file": $
       begin 
         widget_control, ev.id, get_value=filt ;get the current filter
         *fileptr=*ev.file
         ptr_free,ev.file
         widget_control, ev.top, /DESTROY 
      end
      "cancel": $
       begin 
         *fileptr=-1
         widget_control, ev.top, /DESTROY 
      end
   endcase
end
   
;; see cw_xf for keywords-- note that FVAL is explicitly passed
pro xf,file, _EXTRA=e, TITLE=t, PARENT_GROUP=xfp,GROUP=group, $
       xoffset=xoff, yoffset=yoff,CANCEL_TEXT=cancel,MODAL=md

   if n_elements(xoff) eq 0 then begin
      device,get_screen_size=sc
      xoff=sc(0)/3
   endif 

   if n_elements(yoff) eq 0 then begin 
      if n_elements(sc) eq 0 then device,get_screen_size=sc
      yoff=sc(1)/5
   endif 
   
   if n_elements(md) eq 0 then md=0
   if n_elements(t) eq 0 then t='Select File'
   if n_elements(cancel) eq 0 then cancel="CANCEL"
   fileptr=ptr_new(-1)
   if keyword_set(xfp) then begin 
      if widget_info(xfp,/VALID) then begin 
         g=widget_info(xfp,/GEOMETRY) 
         xoff=g.xoffset+g.scr_xsize/4
         yoff=g.yoffset+g.scr_ysize/4
         base=widget_base(TITLE=t,/COLUMN,uvalue=fileptr,xoffset=xoff, $
                          yoffset=yoff,GROUP_LEADER=xfp,/FLOATING, $
                          MODAL=md)
      endif else md=0           ;no topbase? can't be modal
   endif else md=0
   if n_elements(base) eq 0 then $
    base=widget_base(TITLE = t,/COLUMN,uvalue=fileptr,xoffset=xoff, $
                     yoffset=yoff,MODAL=md)

   fileloader=cw_xf(base,UVALUE='file',_EXTRA=e)
   
   button=widget_button(base,value=cancel,uvalue="cancel")
   
   if keyword_set(xfp) then $
      widget_control, base, /REALIZE, CANCEL_BUTTON=button $
   else widget_control, base, /REALIZE
   
   XManager,'xf', base,GROUP=group,NO_BLOCK=0
   ;; get the value of the filename, and put into file
   file=*fileptr
   ptr_free,fileptr
end
