;+
; NAME:
;
;    tvFixedRegion
;
; DESCRIPTION:
;
;    A tvTools plugin for drawing fixed polygonal regions.
;
; CATEGORY:
;
;    tvTools, Regions
;
; METHODS:
;
;    Init:  
;
;       CALLING SEQUENCE:
;
;          obj=obj_new('tvFixedRegion',oDraw,[COLOR=,THICK=,_EXTRA=])
;          
;       INPUT PARAMETERS:
;
;	   oDraw: The tvDraw object.
;	   
;       INPUT KEYWORD PARAMETERS:
;
;          COLOR: The color to use to draw the region.
;             
;          THICK: The thickness used for drawing the lines.
;
;          _EXTRA: Any other ObjMsg initialization keywords
;             (e.g. message list).
;             
;    SetProperty:
;  
;	DESCRIPTION:
;
;	   Set properties of the object.
;	
;       CALLING SEQUENCE:
;
;          obj->SetProperty,COLOR=col,REGION=reg
;          
;       INPPUT KEYWORD PARAMETERS:
;
;          COLOR:  The color to draw the region with.
;          
;          REGION: A 2xn list of pixel coordinates which defines the
;             region.  Does not need to be closed (it will be closed
;             automatically).
;          
; INHERITANCE TREE:
;
;    ObjMsg-->tvPlug-->tvFixedRegion
;
; MODIFICATION HISTORY:
;
;    2004-09-27 (J.D. Smith): Written.
;       
;-
;    $Id$
;##############################################################################
;
; LICENSE
;
;  Copyright (C) 2004 J.D. Smith
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

pro tvFixedRegion::Message,msg
  self->Draw
end

pro tvFixedRegion::On
  if self->On() then begin      ;if turned on *again* .. reset
     self->Reset
     return
  endif
  self->tvPlug::On
  ;; Button and redraw events, no more snapshots
  self.oDraw->MsgSignup,self,/TVDRAW_SNAPSHOT
  self.oDraw->ReDraw,/SNAPSHOT
end

pro tvFixedRegion::Reset
  self.oDraw->MsgSignup,self,/NONE
  if self.drawn then begin 
     self.drawn=0b
     self.oDraw->ReDraw,/SNAPSHOT
  endif 
  self->tvPlug::Off
end

pro tvFixedRegion::SetProperty, COLOR=col,REGION=reg
  if n_elements(col) ne 0 then self.color=col
  if n_elements(reg) ne 0 then self.region=ptr_new(reg)
  if self->On() then self.oDraw->ReDraw,/SNAPSHOT
end

pro tvFixedRegion::Draw
  if ~ptr_valid(self.region) then return
  self.oDraw->Setwin
  x=-1 & y=-1
  n=n_elements(*self.region)/2
  x=lonarr(n+1) & y=lonarr(n+1)
  for i=0,n-1 do begin 
     coords=self.oDraw->Convert((*self.region)[*,i],/DEVICE,/FRACTIONAL)
     x[i]=coords[0] & y[i]=coords[1]
  endfor 
  x[n]=x[0] & y[n]=y[0]
  plots,x,y,COLOR=self.color,THICK=self.thick,/DEVICE
  self.drawn=1
end

pro tvFixedRegion::Cleanup
  ptr_free,self.region
  self->tvPlug::Cleanup
end

;=============================================================================
;  Init 
;    OPTIONS:                                          -- Default 
;  	COLOR:     Color index with which to draw box  -- 0
;  	THICK:     Thickness of box band               -- 1.2
;=============================================================================
function tvFixedRegion::Init,oDraw,COLOR=color,THICK=thick,_EXTRA=e
  if (self->tvPlug::Init(oDraw,/NO_ON_OFF,_EXTRA=e) ne 1) then return,0
  if n_elements(color) eq 0 then self.color=0 else self.color=color
  if n_elements(thick) eq 0 then self.thick=1. else self.thick=thick
  return,1
end

pro tvFixedRegion__define
  struct={tvFixedRegion, $
          INHERITS tvPlug, $    ;make it a tvDraw plug-in
          region:ptr_new(), $   ;the region to show
          color:0, $            ;color to draw it.
          thick:0., $           ;thickness of box line
          drawn:0b}             ;whether we've drawn a box
  
end
