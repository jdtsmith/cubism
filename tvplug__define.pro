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
;	Message.  Override the ObjMsg Message.  The value ac, for active
;               changed,is passed automatically to Update, so overriding it is
;               a suitable way of handling changes in the active state.
;		Possible values are:
;		0: Turned Off
;		1: Turned On
;		2: On and Left Unchanged (Exclusive was already us!)
;=============================================================================
pro tvPlug::Message,msg, TYPE=type
   type=tag_names(msg,/STRUCTURE_NAME)
   ;; if it's a change in exclusive message
   if type eq 'TVDRAW_EXCLUSIVE' then begin 
      old=self.recip.ACTIVE
      if msg.obj eq self then begin ;we are now the active exclusive
         self.recip.ACTIVE=1b
      endif else self.recip.ACTIVE=0b ; somebody else is now the exclusive
      if (old ne self.recip.ACTIVE) then $
       self->Update,self.recip.ACTIVE else  $ ;status changed
       if self.recip.ACTIVE then self->Update,2b
   endif 
   ;; now our subclasses can simply override Update and check ac, to see
   ;; what must be done specific to that class.
end

;=============================================================================
;	SetProperty. only allows the ACTIVE state of the plug-in to be
;	changed... note another way to do this is via an
;	exclusive message (turning off if it's not ourself).
;=============================================================================
pro tvPlug::SetProperty, ACTIVE=act,_EXTRA=e
   if n_elements(act) ne 0 then self.recip.ACTIVE=act ne 0
  ; if n_elements(but) ne 0 then self.recip.BUTTON=but ne 0
;   if n_elements(mot) ne 0 then self.recip.MOTION=mot ne 0
;   if n_elements(view) ne 0 then self.recip.VIEWPORT=view ne 0
;   if n_elements(exp) ne 0 then self.recip.EXPOSE=exp ne 0
;   if n_elements(track) ne 0 then self.recip.TRACKING=track ne 0
;   if n_elements(red) ne 0 then self.recip.REDRAW=red ne 0
;   if n_elements(exc) ne 0 then begin 
;      self.recip.EXCLUSIVE=exc ne 0
;      self.oDraw->MsgList,self.recip ;register changes
;      msg={TVDRAW_EXCLUSIVE,Obj:Self}
;      self.oDraw->MsgSend, msg  ;broadcast change in exclusive
;      return
;   endif 
   self->Update ;register changes
end

;;*************************End OverRiding methods******************************
;=============================================================================
;	Update - Update message receiving status with the tvDraw object.
;	if ac is passed, it is used to evaluate whether to send the message.
;	Overriding methods should test ac and see whether to do things...
;	0: Turned Off, 1: Turned On., 2: On and Turned on again 
;	(e.g. toggle off).
;=============================================================================
pro tvPlug::Update,ac
   self.oDraw->MsgList,self.recip 
end 

;=============================================================================
;	Icon - Return the Icon (2d bitmap array) to use for this plug-in,
;		if appropriate.  To be overridden.  
;=============================================================================

function tvPlug::Icon
   return,-1
end

;=============================================================================
;	Start - Finalize the initialization, which will occur after
;		all widget connections/object additions have been made.
;=============================================================================
pro tvPlug::Start
;   message,'The Start method must be overridden in class '+obj_class(self)
end

;; Tells whether active or NOT
function tvPlug::Status
   return,self.recip.ACTIVE
end

;=============================================================================
;	Init - Initialize a tvPlug object.
;=============================================================================
function tvPlug::Init,oDraw, _EXTRA=e
   if (self->ObjMsg::Init(_EXTRA=e) ne 1) then return,0 ;chain up
   self.recip.Obj=self
   if Obj_Isa(oDraw,'tvDraw') then begin 
      self.oDraw=oDraw 
      oDraw->Plug,self          ;register as a plug-in with oDraw...
   endif else begin 
      print,oDraw
      wmessage,'Specified object is not valid tvDraw object',/INFO,_EXTRA=e
      return,0
   endelse
   return,1
end

pro tvPlug::Cleanup
   obj_destroy,self.oDraw
   self->ObjMsg::Cleanup     ;cleans up all subscribers
   return
end

;=============================================================================
;	tvPlug__define.  Prototype the tvPlug class
;=============================================================================
pro tvPlug__define
   struct={tvPlug, $
           INHERITS ObjMsg, $
           oDraw:obj_new(), $   ;the tvDraw object 
           recip:{TVDRAWRECIP,Obj:obj_new(),ACTIVE:0b, $
                  BUTTON:0b,MOTION:0b, VIEWPORT:0b,EXPOSE:0b, TRACKING:0b, $
                  REDRAW:0b, EXCLUSIVE:0b}}
   
   return
end
