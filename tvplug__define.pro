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
;   if type eq 'TVDRAW_EXCLUSIVE' then begin
;      if msg.obj eq self then $  ;we are now the active exclusive
;         self->On else if self.active then self->Off
;   endif
end

;=============================================================================
;	Start - Finalize the initialization, which will occur after
;		all widget connections/object additions have been made.
;=============================================================================
pro tvPlug::Start
  if self.active then self->On else self->Off
end

;=============================================================================
;	On
;=============================================================================
pro tvPlug::On
  self.active=1b
end

;=============================================================================
;       Off 
;=============================================================================
pro tvPlug::Off
  self.active=0b
end

;;*************************End OverRiding methods******************************

;=============================================================================
;       Icon - Return the Icon (2d bitmap array) to use for this
;              plug-in, if appropriate, otherwise just the class name.
;              To be overridden.
;=============================================================================
function tvPlug::Icon
  ;; If no icon, use the class name instead
  return,obj_class(self)
end

function tvPlug::Status
  return,self.active
end

pro tvPlug::Cleanup
  obj_destroy,self.oDraw        ;all for one, and one for all
  self->ObjMsg::Cleanup         ;cleans up all subscribers
end

;=============================================================================
;	Init - Initialize a tvPlug object.
;=============================================================================
function tvPlug::Init,oDraw, _EXTRA=e
  if (self->ObjMsg::Init(_EXTRA=e) ne 1) then return,0 ;chain up
  if Obj_Isa(oDraw,'tvDraw') then begin 
     self.oDraw=oDraw 
     oDraw->Plug,self           ;register us as a plug-in with oDraw...
  endif else begin 
     print,oDraw
     message,'Specified object is not valid tvDraw object'
     return,0
  endelse
  return,1
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
