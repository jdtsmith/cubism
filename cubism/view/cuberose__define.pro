;+
; NAME:  
;
;    CubeRose
;
; CONTACT:
;
;    UPDATED VERSIONS of CUBISM and more information can be found at:
;       http://sings.stsci.edu/cubism
;
; DESCRIPTION:
;    
;    Display a compass rose on images and cubes.
;    
; CATEGORY:
;
;    CUBISM Spectral Reduction, Analysis and Processing.
;    WCS.
;
; METHODS:
;
;    Init:  
;
;       CALLING SEQUENCE:
;
;          obj=obj_new('CubeRose',oDraw,COLOR=)
;
;       INPUT PARAMETERS:
;
;          oDraw: The tvDraw object.
;
;       INPUT KEYWORD PARAMETERS:
;
;          COLOR: The color ID to use for drawing the rose.
;             
; INHERITANCE TREE:
;
;    ObjMsg-->tvPlug-->CubeBackTrack
;
; MODIFICATION HISTORY:
;    
;    2003-12-18 (J.D. Smith): Written
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2003-2006 J.D. Smith
;
;  This file is part of CUBISM.
;
;  CUBISM is free software; you can redistribute it and/or modify it
;  under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2, or (at your option)
;  any later version.
;  
;  CUBISM is distributed in the hope that it will be useful, but
;  WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;  General Public License for more details.
;  
;  You should have received a copy of the GNU General Public License
;  along with CUBISM; see the file COPYING.  If not, write to the Free
;  Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;  Boston, MA 02110-1301, USA.
;
;##############################################################################

;;**************************OverRiding methods********************************

;=============================================================================
;  Message - tvDraw and CubeRec messages
;=============================================================================
pro CubeRose::Message, msg
  self->tvPlug::Message,msg,TYPE=type
  case type of
     'TVDRAW_SNAPSHOT': self->DrawRose
     'CUBEREC_UPDATE': begin 
        if msg.bcd_mode then begin 
           self->Off,/DISABLE   ;no longer relevant
           return
        endif else self->Enable
        self.cube=msg.cube
        if ptr_valid(msg.astrometry) then astr=*msg.astrometry else $
           self.cube->GetProperty,ASTROMETRY=astr
        getrot,astr,rot
        self.angle=rot/!RADEG
        ;if self->On() then self.oDraw->ReDraw,/SNAPSHOT
     end
  endcase 
end

;=============================================================================
;  On - Dislay on
;============================================================================
pro CubeRose::On, NO_REDRAW=nrd
  self->tvPlug::On
  self.oDraw->MsgSignup,self,/TVDRAW_SNAPSHOT
  if obj_valid(self.cube) && ~keyword_set(nrd) then $
     self.oDraw->ReDraw,/SNAPSHOT     
end

;=============================================================================
;  Off - Display off
;============================================================================
pro CubeRose::Off,RESET=reset,NO_REDRAW=nrd,_EXTRA=e
  self.oDraw->MsgSignup,self,/NONE
  was_on=self->On()
  self->tvPlug::Off,_EXTRA=e
  if was_on && ~keyword_set(nrd) then self.oDraw->ReDraw,/SNAPSHOT,/ERASE
end

;=============================================================================
;  Icon
;=============================================================================
function CubeRose::Icon
  return, [[128B, 001B],[128B, 001B],[240B, 015B],[152B, 025B],$
           [188B, 061B],[052B, 044B],[004B, 032B],[159B, 249B],$
           [159B, 249B],[004B, 032B],[052B, 044B],[188B, 061B], $
           [152B, 025B],[240B, 015B],[128B, 001B],[128B, 001B]]
end

;=============================================================================
;  Description
;=============================================================================
function CubeRose::Description
  return,'Draw a Compass Rose'
end

;;*************************End OverRiding methods******************************


;=============================================================================
;  DrawRose
;=============================================================================
pro CubeRose::DrawRose
  if ~obj_valid(self.cube) then return
  x=!D.X_SIZE*4./5 & y=!D.Y_SIZE*4./5
  s=sin(self.angle) & c=cos(self.angle)
  len=(!D.X_SIZE<!D.Y_SIZE)/8
  deltaE=[-c,-s]*len
  deltaN=[-s, c]*len
  plots,[x,x+deltaE[0]],[y,y+deltaE[1]],THICK=2,COLOR=self.color,/DEVICE
  plots,[x,x+deltaN[0]],[y,y+deltaN[1]],THICK=2,COLOR=self.color,/DEVICE
  deltaE=[-c,-s]*(len+!D.X_CH_SIZE+4) & deltaN=[-s,c]*(len+4)
  xyouts,x+deltaN[0],y+deltaN[1],/DEVICE,'N', $
         ORIENTATION=self.angle*!radeg,COLOR=self.color,CHARSIZE=1.25
  xyouts,x+deltaE[0],y+deltaE[1],/DEVICE,'E', $
         ORIENTATION=self.angle*!radeg, COLOR=self.color,CHARSIZE=1.25
end

;=============================================================================
;  Init
;=============================================================================
function CubeRose::Init,oDraw,COLOR=color
  if (self->tvPlug::Init(oDraw,_EXTRA=e) ne 1) then return,0 
  if n_elements(color) ne 0 then self.color=color
  return,1
end

pro CubeRose__define
  st={CubeRose, $
      INHERITS tvPlug,$         ;it's a tvDraw plugin
      cube:obj_new(),$          ;the cube we're drawing for
      color:0, $                ;the color to use for ourselves
      angle:0.0}                ;N angle, CCW from +y
end
