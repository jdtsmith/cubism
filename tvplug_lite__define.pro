;+
; NAME:
;
;    tvPlug_lite
;
; DESCRIPTION:
;
;    An light-weight prototype class for plugin's to a tvDraw
;    object. These plugins can receive ObjMsg messages, and may or may
;    not send messages themselves.  They are free to use whatever form
;    of messages and message lists they want.
;
; CATEGORY:
;
;    Object-Based Drawing Tool Plug-In
;
; NOTES:
;
;   You probaby want to use the full tvPlug, unless you have a reason
;   not to prefer OMArray-based messaging.  By itself, a tvPlug_lite
;   object doesn't do much.  It is intended to serve as a superclass
;   to plugins (and tvPlug-based plugins) which utilize its
;   functionalities.
;

;
; INHERITANCE TREE:
;
;    ObjMsg-->tvPlug_Lite
;    
; MODIFICATION HISTORY:
;
;    2001-11-01 (J.D. Smith): Initially written: split from tvPlug to
;       provide some functionality without imposing OMArray message
;       lists, and all the ON_OFF baggage.
;       
;-
;##############################################################################
;
; LICENSE
;
;  Copyright (C) 2001-2002 J.D. Smith
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
;  Message.  Just return the type for them.
;=============================================================================
pro tvPlug_lite::Message,msg, TYPE=type
  type=tag_names(msg,/STRUCTURE_NAME)
end

;;*************************End OverRiding methods******************************

;=============================================================================
;  Icon - Return the Icon (2d bitmap array) to use for this plug-in,
;         if appropriate.  To be overridden.
;=============================================================================
function tvPlug_lite::Icon
  ;; Default is no icon
  return,''                     ; obj_class(self)
end

;=============================================================================
;  Description - A concise description of the plug-in, suitable for a
;                tool-tip or menu item.  To be overridden. Defaults to
;                nothing.
;=============================================================================
function tvPlug_lite::Description
;  descrip=obj_class(self)
;  if strpos('tv',obj_class(self)) eq 0 then descrip=strmid(descrip,2)
  return,''                     ;descrip
end

;=============================================================================
;  MouseHelp - A short help on the mouse: three strings, for left,
;              middle, right mouse buttons.
;=============================================================================
function tvPlug_lite::MouseHelp
  return,['','','']
end


;=============================================================================
;  Start - Just a dummy in case a plug-in needs no Startup.
;=============================================================================
pro tvPlug_lite::Start
  return
end

;=============================================================================
;  Cleanup
;=============================================================================
pro tvPlug_lite::Cleanup
  obj_destroy,self.oDraw        ;all for one, and one for all
  self->ObjMsg::Cleanup         ;cleans up all subscribers
end

;=============================================================================
;  Init - Initialize a tvPlug_lite object.
;=============================================================================
function tvPlug_lite::Init,oDraw, _EXTRA=e
  if (self->ObjMsg::Init(_EXTRA=e) ne 1) then return,0 ;chain up
  if Obj_Isa(oDraw,'tvDraw') then begin 
     self.oDraw=oDraw 
     self.oDraw->Plug,self           ;register us as a plug-in with oDraw...
  endif else begin 
     message,string('Specified object is not valid tvDraw object: ',oDraw)
     return,0
  endelse
  return,1
end

;=============================================================================
;  tvPlug_lite__define.  Prototype the tvPlug_lite class
;=============================================================================
pro tvPlug_lite__define
  st={tvPlug_lite, $
      INHERITS ObjMsg, $
      oDraw:obj_new()}          ;the tvDraw object 
end
