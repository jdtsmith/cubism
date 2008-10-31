;+
; NAME:
;
;    tvplug
;
; DESCRIPTION:
;
;    An more capable prototype class for tvDraw plugins which have
;    state (either ON or OFF).  These plug-ins can receive ObjMsg
;    messages, and may or may not send messages themselves, but they
;    are restricted to using the OMArray message helper class.  This
;    means there message recipient lists are boolean values (either
;    send the message, or don't).  See OMArray.  tvPlug plugins
;    typically reconfigure their message sending/receiving state when
;    turned ON or OFF.  Often, they also send a message
;    (TVPLUG_ON_OFF) when this occurs (the default).
;
; CATEGORY: Object-Based Drawing Tool Plug-In
;
; NOTES:
;
;   By itself, a tvPlug object doesn't do much.  It is intended to
;   serve as a superclass to plugins which utilize its
;   functionalities.
;
;
; MODIFICATION HISTORY:
;
;    2001-11-01 (J.D. Smith): Split into tvPlug and tvPlug_list, to
;       accomodate plugins without need of the full OMArray, on/off,
;       etc.
; 
;    2001-08-01 (J.D. Smith): Initial import from SCORE-era viewer
;       component collection.
;    
;-
;##############################################################################
;
; LICENSE
;
;  Copyright (C) 2001-2004 J.D. Smith
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

;;**************************OverRiding methods********************************

;=============================================================================
;  Start - Finalize the initialization, which will occur after ; all
;          widget connections/object additions have been made.
;=============================================================================
pro tvPlug::Start
  case self.active of
     0: self->Off
     1: self->On
     2: self->Off,/DISABLE
     3: self->Disable
  endcase    
end

;=============================================================================
;  On
;=============================================================================
pro tvPlug::On
  active=self.active
  self.active=self.active OR 1b
  if self.on_off then self->MsgSend,{TVPLUG_ON_OFF,self,self.active, $
                                     active ne self.active}
end

;=============================================================================
;  Off 
;=============================================================================
pro tvPlug::Off,DISABLE=dis
  active=self.active
  self.active=self.active AND NOT 1b 
  ;; set the disable bit
  if keyword_set(dis) then self.active=self.active OR 2b 
  if self.on_off then self->MsgSend,{TVPLUG_ON_OFF,self,self.active, $
                                     active ne self.active}
end

;=============================================================================
;  Disable
;=============================================================================
pro tvPlug::Disable
  active=self.active
  self.active=self.active OR 2b
  if self.on_off then self->MsgSend,{TVPLUG_ENABLE_DISABLE,self,self.active,$
                                     active ne self.active}
end

;=============================================================================
;  Enable
;=============================================================================
pro tvPlug::Enable
  active=self.active
  self.active=self.active AND NOT 2b ;take off the disable bit
  if self.on_off then self->MsgSend,{TVPLUG_ENABLE_DISABLE,self,self.active,$ $
                                     active ne self.active}
end

;=============================================================================
;  ReportWidget - A widget over which to display errors, etc.
;=============================================================================
function tvPlug::ReportWidget
  if NOT obj_valid(self.oDraw) then return,-1L
  self.oDraw->GetProperty,DRAWWIDGET=dw
  return,dw
end

;;*************************End OverRiding methods******************************

function tvPlug::Status
  return,self.active            
end

function tvPlug::Enabled
  return,(self.active AND 2b) eq 0b
end

function tvPlug::On
  return,self.active AND 1b
end

pro tvPlug::Cleanup
  self->OMArray::Cleanup
  self->tvPlug_lite::Cleanup    ;cleans up all subscribers
end

;=============================================================================
;  Init - Initialize a tvPlug object.
;=============================================================================
function tvPlug::Init,oDraw,NO_ON_OFF=noo, _EXTRA=e
  if (self->tvPlug_lite::Init(oDraw,_EXTRA=e) ne 1) then return,0 ;chain up
  if NOT keyword_set(noo) then begin 
     self.on_off=1b
     self->MsgSetup,['TVPLUG_ON_OFF','TVPLUG_ENABLE_DISABLE']
  endif
  return,1
end

;=============================================================================
;  tvPlug__define.  Prototype the tvPlug class
;=============================================================================
pro tvPlug__define
  st={tvPlug, $
      INHERITS OMArray, $       ;helper class for ObjMsg for BOOLEAN
                                ;  message recpient lists
      INHERITS tvPlug_lite, $   ;we're extending the lite version 
      INHERITS ObjReport, $     ;make it a reporter
      on_off:0b, $              ;are we using on_off messages?
      active:0b}                ;flag: whether we're active (off/on,disabled)
  msg={TVPLUG_ON_OFF, Object:obj_new(), Status:0b, Changed:0b} 
  msg={TVPLUG_ENABLE_DISABLE, Object:obj_new(), Status:0b, Changed:0b}
end
