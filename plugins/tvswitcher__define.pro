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
  self->Toggle,wh[0]
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
  wh=where(*self.wList eq ev.id) ;which button
  self->Toggle,wh[0]
end

;=============================================================================
;       Toggle - Turn object on or off, depending on status and type,
;                and set button.
;=============================================================================
pro tvSwitcher::Toggle, hit
  ;; Toggle On/Off, depending on type.
  if self->MsgListLen() eq 0 then return
  if (*self.MsgList)[hit].Exclusive then begin
     wh=where((*self.MsgList).Exclusive,cnt)
     for i=0,cnt-1 do begin
        obj=self->GetObj((*self.MsgList)[wh[i]])
        if wh[i] eq  hit then obj->On else if obj->Status() then obj->Off
     endfor
  endif else begin
     obj=self->GetObj((*self.MsgList)[hit])
     if obj->Status() then obj->Off else obj->On
  endelse
  self->UpdateButtons
end


;=============================================================================
;       UpdateButtons - Update the buttons' states
;=============================================================================
pro tvSwitcher::UpdateButtons
  if NOT ptr_valid(self.wList) then return
  objs=self->GetMsgObjs()
  for i=0,self->MsgListLen()-1 do begin 
     ic=objs[i]->Icon()
     if objs[i]->Status() then begin ;on
        if size(ic,/N_DIMEN) eq 2 then val=ic else val='>'+ic+'<'
     endif else begin           ;off
        if size(ic,/N_DIMEN) eq 2 then val=ic XOR 255b else val=ic
     endelse 
     widget_control, (*self.wList)[i],set_value=val
  endfor 
end

;=============================================================================
;	Start - Setup the icons in two bases, if necessary.
;=============================================================================
pro tvSwitcher::Start
  objs=self->GetMsgObjs()
  if obj_valid(objs[0]) eq 0 then return
  
  ;; Set up the base(s)' event handler
  for type=0,1 do begin 
     if widget_info(self.sBase[type],/VALID_ID) eq 0 then continue
     widget_control, self.sBase[type], EVENT_PRO='tvSwitcher_Event', $
                     set_uvalue=self, /TRACKING_EVENTS
  endfor 
  
  no=n_elements(objs) 
  self.wList=ptr_new(lonarr(no,/NOZERO))
  for i=0,no-1 do begin 
     ic=objs[i]->Icon()
     if size(ic,/N_DIMEN) eq 2 then val=ic XOR 255b else val=ic   
     (*self.wList)[i]= $
        widget_button(self.sBase[1b-(*self.MsgList)[i].Exclusive], $
                      /NO_RELEASE,value=val,xsize=70)
  endfor 
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
function tvSwitcher::Init,parent,oDraw,USECASE=uc,_EXTRA=e
  if (self->tvPlug::Init(oDraw,_EXTRA=e) ne 1) then return,0
  
  ;; Get our key object and sign us up.  Important Keywords get Passed On.
  self.Key=obj_new('tvKey',oDraw)
  self.Key->MsgSignup,self,/TVKEY_KEY
  self.UseCase=keyword_set(uc) 
  
  ;; see if a valid base was passed, if so make the buttons bases there
  if widget_info(parent,/VALID_ID) eq 0 then return,0
  
  ;; Set up the base(s) for the buttons (actually create them in Start)
  if self->MsgListLen() ne 0 then begin 
     if NOT self.UseCase then  $
        (*self.MsgList).keys=strlowcase((*self.MsgList).keys)
     exc=where((*self.MsgList).Exclusive,exc_cnt,COMPLEMENT=tog, $
               NCOMPLEMENT=tog_cnt)
     if exc_cnt gt 0 then $
        self.sBase[0]=widget_base(parent,/ROW,SPACE=1,/ALIGN_LEFT) 
     if tog_cnt gt 0 then $
        self.sBase[1]=widget_base(parent,/ROW,SPACE=1,/ALIGN_LEFT) 
  endif 
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
          sBase: [0L,0L], $     ;widget ids of the button bases
          wList: ptr_new(), $    ;list of buttons, for each on MsgList
          cur:0}                ;which is currently the active one
  return
end
