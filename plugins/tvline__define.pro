;+
; NAME:
;
;    tvLine
;
; DESCRIPTION:
;
;    A tvTools plugin to display a status line with pixel coordinates
;    and value information.
;
; CATEGORY:
;
;    tvTools, Image Status
;
; METHODS:
;
;    Init:  
;
;       CALLING SEQUENCE:
;
;          obj=obj_new('tvLine',oDraw,parent,[FORMAT=,_EXTRA=e])
;          
;       INPUT PARAMETERS:
;
;	   oDraw: The tvDraw object.
;
;          parent: The widget ID where the line reporting label will
;            be placed.
;	   
;       INPUT KEYWORD PARAMETERS:
;
;          FORMAT: The format code for printing pixel coordinates and
;             value.  Defaults to: '("(",I4,",",I4,") ",G14.8)'
;
;          _EXTRA: Any other ObjMsg initialization keywords
;             (e.g. message list).
;          
; INHERITANCE TREE:
;
;    ObjMsg-->tvPlug-->tvLine
;
; MODIFICATION HISTORY:
;
;    2001-08-07 (J.D. Smith): Imported from SCORE-era source.
;       
;-
;    $Id$
;##############################################################################
;
; LICENSE
;
;  Copyright (C) 2001,2003,2005 J.D. Smith
;
;  This file is part of tvTools.
;
;  tvTools is free software; you can redistribute it and/or modify it
;  under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2, or (at your option)
;  any later version.
;
;  tvTools is distributed in the hope that it will be useful, but
;  WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;  General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with tvTools; see the file COPYING.  If not, write to the
;  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;  Boston, MA 02110-1301, USA.
;
;##############################################################################


;=============================================================================
;       Message - Display the vals.  We have signed up for motion and
;                 tracking messages
;=============================================================================
pro tvLine::Message,msg
  self->tvPlug_lite::Message,msg,TYPE=type ;pass it up
  case type of 
     'DRAW_MOTION': begin 
        self.oDraw->GetProperty,IMORIG=imorig
        if NOT ptr_valid(imorig) then return
        pt=self.oDraw->Convert([msg.X,msg.Y],/SHOWING)
        if pt[0] eq -1 then begin 
           widget_control, self.wLine,set_value=' '
           self.savpoint=[-1,-1] ;ensure rentry works
           return
        endif 
        if array_equal(pt,self.savpoint) then return
        widget_control, self.wLine,set_value=self->String(imorig,pt)
        self.savpoint=pt
     end
     
     'WIDGET_TRACKING': begin 
        if msg.enter eq 0 then begin ;just left window -- clear status line
           widget_control,self.wLine,set_value=' '
           self.savpoint=[-1,-1]
        endif 
     end
     
     'TVDRAW_POSTDRAW': begin 
        if self.savpoint[0] eq -1 then return ;not on a point
        self.oDraw->GetProperty,IMORIG=imorig
        widget_control, self.wLine,set_value=self->String(imorig,self.savpoint)
     end
  endcase 
end 

;=============================================================================
;       String - The string value associate with point X,Y
;=============================================================================
function tvLine::String, im,point
  return,string(FORMAT=self.form,point,(*im)[point[0],point[1]])
end

;=============================================================================
;       Init - Initialize the line.
;=============================================================================
function tvLine::Init,oDraw,parent,FORMAT=form,_EXTRA=e
  if (self->tvPlug_lite::Init(oDraw,_EXTRA=e) ne 1) then return,0 ;chain up
  ;; set up the format for printing x,y, value
  if n_elements(form) eq 0 then self.form='("(",I4,",",I4,") ",G14.8)' else $
     self.form=form
  self.wLine=widget_label(parent,value=' ',/dynamic_resize)
  
  ;; specify motion, tracking and postdraw events... we're always on
  self.oDraw->MsgSignup,self,/DRAW_MOTION,/WIDGET_TRACKING,/TVDRAW_POSTDRAW
  return,1
end 

;=============================================================================
;       tvLine__define - Prototype the tvLine class.
;=============================================================================
pro tvLine__define
  struct={tvLine, $ 
          INHERITS tvPlug_lite,$ ;make it a plug-in
          savpoint: [0,0], $    ;point to save
          form:'', $            ;format of printed text
          wLine:0L}             ;widget id of text line
  return
end
