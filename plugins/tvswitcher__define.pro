;+
; tvSwitcher - A tvPlug plug-in selector with hotkeys designed to work
;              with a tvdraw object.
;-

;;**************************OverRiding methods********************************
;=============================================================================
;       Message - Expecting only TVKEY_KEY messages.
;=============================================================================
pro tvSwitcher::Message, msg
  if NOT self.UseCase then char=strlowcase(msg.KEY) else char=msg.KEY
  wh=where((*self.MsgList).keys eq char,cnt)
  if cnt eq 0 then return
  self.cur=wh[0]                ;this is now the current one
  obj=self->GetObj((*self.MsgList)[self.cur])
  self.oDraw->MsgSend,EXCLUSIVE=obj
  self->Button,obj
end

function tvSwitcher::GetObj, ml
  return,ml.obj
end
;;************************End OverRiding methods*******************************

pro tvSwitcher_Event, ev
  widget_control, ev.handler, get_uvalue=self
  self->Event,ev
end

;=============================================================================
;       Event - Give the hidden widget_text focus.
;=============================================================================
pro tvSwitcher::Event, ev
  type=tag_names(ev,/STRUCTURE_NAME)
  if type eq 'WIDGET_TRACKING' then begin 
     if ev.enter eq 1 then self.Key->Focus
     return
  endif 
  
  ;; Button Clicked
  wh=(where(*self.wList eq ev.id))[0] ;which button

  ;; have the tvDraw object send out the exclusive message
  obj=self->GetObj((*self.MsgList)[wh])
  self.oDraw->MsgSend,EXCLUSIVE=obj
  
  self.cur=wh
  self->Button,obj
end

;=============================================================================
;       Button - Set the buttons correctly.
;=============================================================================
pro tvSwitcher::Button, obj
  if NOT ptr_valid(self.wList) then return
  ;; It's up to the object whether he's on or off...
  if obj->Status() then begin 
     ;; turn *on* the newly clicked one, and others off
     all=self->GetMsgObjs()
     for i=0,n_elements(all)-1 do begin 
        ic=all[i]->Icon()
        if all[i] eq obj then begin ;turned on
           if size(ic,/N_DIMEN) eq 2 then val=ic else val='*'+ic+'*'
        endif else begin        ;turned off
           if size(ic,/N_DIMEN) eq 2 then val=ic XOR 255b else val=ic
        endelse 
        widget_control, (*self.wList)[i],set_value=val
     endfor 
  endif else begin
     ;; turn *off* the current button, Status is down (inactive)
     if self.cur ne -1 then begin
        ic=obj->Icon()
        if size(ic,/N_DIMEN) eq 2 then val=ic XOR 255b else val=ic
        widget_control,(*self.wList)[self.cur],set_value=val
     endif
  endelse
end

;=============================================================================
;	Start - Setup the icons in the base, if necessary.
;=============================================================================
pro tvSwitcher::Start
  if widget_info(self.sBase,/VALID) eq 0 then return
  objs=self->GetMsgObjs()
  if obj_valid(objs[0]) then begin
     widget_control, self.sBase, EVENT_PRO='tvSwitcher_Event', $
                     set_uvalue=self, /TRACKING_EVENTS
     no=n_elements(objs) 
     self.wList=ptr_new(lonarr(no,/NOZERO))            
     for i=0,no-1 do begin
        ic=objs[i]->Icon()
        if size(ic,/N_DIMEN) eq 2 then val=ic XOR 255b else val=ic   
        (*self.wList)[i]= $
           widget_button(self.sBase, /NO_RELEASE, value=val,/DYNAMIC_RESIZE)
     endfor 
  endif 
end

;=============================================================================
;	Cleanup
;=============================================================================
pro tvSwitcher::Cleanup
  ptr_free,self.wList
  self->tvPlug::Cleanup
end

;=============================================================================
;       Init - Initialize the hotkey.  Object is the object to
;              activate on receiving any of the string keys.  If
;              USECASE is set, the case of the hotkey will be
;              considered, no case is considered by default.
;
;              Inherited keywords:
;              
;              MsgList: (up to ObjMsg) -- form {Obj:obj, keys:''}
;              where obj is the name of the object and keys are the
;              keys to toggle activity on.  Note that no actual
;              messages are sent, only the toggling is done.
;=============================================================================
function tvSwitcher::Init,oDraw,USECASE=uc, SELECT_BASE=sb, _EXTRA=e
  if (self->tvPlug::Init(oDraw,_EXTRA=e) ne 1) then return,0
  
  ;; Get our key object and sign us up.  Important Keywords get Passed On.
  self.Key=obj_new('tvKey',oDraw)
  self.Key->MsgSignup,self,/TVKEY_KEY
  
  self.UseCase=keyword_set(uc) 
  
  ;; see if a valid base was passed, if so make the buttons there
  if n_elements(sb) ne 0 then $
     if widget_info(sb,/VALID) then $
     if widget_info(sb,/TYPE) eq 0 then self.sBase=sb ;it's a good base
  
  self.cur=-1                   ;none yet active
  if self->MsgListLen() ne 0 and NOT self.UseCase then  $
     (*self.MsgList).keys=strlowcase((*self.MsgList).keys)
  return,1
end

;=============================================================================
;	tvSwitcher__define.  Prototype the tvSwitcher class.
;=============================================================================
pro tvSwitcher__define
  struct={tvSwitcher, $
          INHERITS tvPlug, $    ;make it a tvDraw plug-in
          Key:obj_new(), $      ;our key plug-in
          UseCase:0b, $         ;whether to consider case
          sBase: 0L, $          ;widget id of the button base
          wList: ptr_new(), $   ;a list of buttons
          cur:0}                ;which is currently the active one
  return
end
