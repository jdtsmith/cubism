;+
; NAME:  
;
;    GETINP
;
; DESCRIPTION:
;    
;    Widget to prompt for input.
;    
; CATEGORY:
;
;    Widget Utility
;
; CALLING SEQUENCE:
;
;    ans=getinp(question,default,[TITLE=,MODAL=,PARENT_GROUP=]
;
; INPUT PARAMETERS:
;
;    question: The question for input.
;
;    default: The default input to enter.
;    
; INPUT KEYWORD PARAMETERS:
;
;    TITLE: The title to give the popped-up widget.
;
;    MODAL: Whether to make the popup modal.
;
;    PARENT_GROUP: The widget ID of the group leader.
;
; OUTPUT:
;
;    ans: The input given.
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

pro getinp_event, ev
   widget_control,ev.id,get_uvalue=inpid
   widget_control,ev.top,get_uvalue=val_ptr
   if inpid ne 0L then begin 
      widget_control, inpid, get_value=val
      *val_ptr=val
   endif else ptr_free,val_ptr
   widget_control, ev.top,/DESTROY
   return
end


function getinp, question, DEFAULT, TITLE=t,MODAL=md,PARENT_GROUP=group, $
                 _EXTRA=e
   if n_elements(t) eq 0 then t='Enter Value'
   if n_elements(md) eq 0 then md=1
   if n_elements(default) eq 0 then default=' '
   groupQ=n_elements(group) ne 0?widget_info(group,/VALID_ID):0
   if groupQ then begin 
      g=widget_info(group,/GEOMETRY) 
      xoff=g.xoffset+g.scr_xsize/4
      yoff=g.yoffset+g.scr_ysize/4
   endif else begin 
      device,get_screen_size=sc
      xoff=sc[0]/3 & yoff=sc[1]/3
   endelse 
   val_ptr=ptr_new(DEFAULT)
   if NOT groupQ then $
      base=widget_base(TITLE=t,/COLUMN,xoffset=xoff,yoffset=yoff, $
                       uvalue=val_ptr) $
   else base=widget_base(TITLE=t,/COLUMN,xoffset=xoff,yoffset=yoff, $
                         uvalue=val_ptr,MODAL=md, GROUP=group)
   rowbase=widget_base(base,/row)
   input=cw_field(rowbase,value=DEFAULT,TITLE=question,/RETURN_EVENTS,_EXTRA=e)
   widget_control, input, set_uvalue=input ;put the id in
   rbase=widget_base(base,/row) 
   cancel=widget_button(rbase,value='Cancel', uvalue=0L)
   ok=widget_button(rbase,value=' OK ',uvalue=input)
   widget_control, base,/realize,DEFAULT_BUTTON=ok
   widget_control, input,/INPUT_FOCUS
   XManager,'getinp',base
   if NOT ptr_valid(val_ptr) then return,''
   ret=*val_ptr
   ptr_free,val_ptr
   return, ret[0]
end




