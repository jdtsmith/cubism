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
  self.active=1b
  if self.on_off then self->MsgSend,{TVPLUG_ON_OFF,self,1b}
end

;=============================================================================
;  Off 
;=============================================================================
pro tvPlug::Off,DISABLE=dis
  self.active=0b+keyword_set(dis)*2b
  if self.on_off then self->MsgSend,{TVPLUG_ON_OFF,self,self.active}
end

;=============================================================================
;  Disable
;=============================================================================
pro tvPlug::Disable
  self.active=self.active OR 2b
  if self.on_off then self->MsgSend,{TVPLUG_ON_OFF,self,self.active}
end

;=============================================================================
;  Enable
;=============================================================================
pro tvPlug::Enable
  self.active=self.active AND NOT 2b
  if self.on_off then self->MsgSend,{TVPLUG_ON_OFF,self,self.active}
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
  obj_destroy,self.oDraw        ;all for one, and one for all
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
     self->MsgSetup,'TVPLUG_ON_OFF'
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
      on_off:0b, $              ;are we using on_off messages?
      active:0b}                ;flag: whether we're active (off,on,disabled)
  message={TVPLUG_ON_OFF, Object:obj_new(), Status:0b} ;an on or off message.
end
