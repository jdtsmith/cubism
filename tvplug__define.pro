;+
; NAME: tvPlug
;
; PURPOSE: An object interface for plug-in's to a draw widget.
; 	These plug-in's may or may not send messages (via events or
; 	otherwise
;
; CATEGORY: Object-Based Drawing Widget
;
; CALLING SEQUENCE:
; 
; INPUTS:
;
; OPTIONAL INPUTS:
;	
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; INHERITANCE TREE: ObjMsg --> tvPlug
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;-


;;**************************OverRiding methods********************************
;=============================================================================
;	Message.  Handle EXCLUSIVE messages for the plug-ins
;=============================================================================
pro tvPlug::Message,msg, TYPE=type
  type=tag_names(msg,/STRUCTURE_NAME)
  ;; if it's a change in exclusive message, which we'll handle for
  ;; every type of plug-in.
  if type eq 'TVDRAW_EXCLUSIVE' then begin 
     if msg.obj eq self then $  ;we are now the active exclusive
        self->On else if self.active then self->Off
  endif 
end

;=============================================================================
;	Start - Finalize the initialization, which will occur after
;		all widget connections/object additions have been made.
;=============================================================================
pro tvPlug::Start
  if self.active then self->On else self->Off
end

;=============================================================================
;	On - Turn ourselves on, signing up for those messages we want
;=============================================================================
pro tvPlug::On
  ;;self.oDraw->MsgList, self
;  help,/traceback
;  print,obj_class(self)+' On'
;  print, self
;  print,'(******************************)'
  self.active=1b
end

;=============================================================================
;       Off - Turn ourselves off, receiving only any background
;             messages
;=============================================================================
pro tvPlug::Off
;  print,obj_class(self)+' Off'
  self.active=0b
end

;;*************************End OverRiding methods******************************

;=============================================================================
;       Update - Update message receiving status with the tvDraw
;                object.  Pass keywords for message types.
;=============================================================================
pro tvPlug::Update,_EXTRA=e
  self.oDraw->MsgList,self,_EXTRA=e
end 

;=============================================================================
;       Icon - Return the Icon (2d bitmap array) to use for this
;              plug-in, if appropriate.  To be overridden.
;=============================================================================
function tvPlug::Icon
  ;; If no icon, use the class name instead
  return,obj_class(self)
end

;; Tells whether active or NOT
function tvPlug::Status
  return,self.active
end

;=============================================================================
;	Init - Initialize a tvPlug object.
;=============================================================================
function tvPlug::Init,oDraw, _EXTRA=e
  if (self->ObjMsg::Init(_EXTRA=e) ne 1) then return,0 ;chain up
  if Obj_Isa(oDraw,'tvDraw') then begin 
     self.oDraw=oDraw 
     oDraw->Plug,self           ;register as a plug-in with oDraw...
  endif else begin 
     print,oDraw
     wmessage,'Specified object is not valid tvDraw object',/INFO,_EXTRA=e
     return,0
  endelse
  return,1
end

pro tvPlug::Cleanup
  obj_destroy,self.oDraw
  self->ObjMsg::Cleanup         ;cleans up all subscribers
  return
end

;=============================================================================
;	tvPlug__define.  Prototype the tvPlug class
;=============================================================================
pro tvPlug__define
  struct={tvPlug, $
          INHERITS ObjMsg, $
          oDraw:obj_new(), $    ;the tvDraw object 
          active:0b}            ;whether we're active
end
