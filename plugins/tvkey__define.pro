;;**************************OverRiding methods********************************

;=============================================================================
;	Message.  Override the tvPlug Message.  We have
;	signed up for button and tracking messages from the tvDraw object.
;=============================================================================
pro tvKey::Message, msg
  self->tvPlug::Message,msg,TYPE=type ;pass it up to tvPlug
  ;; set the input focus on entering the draw window, and on button release
  ;; events.
  if type eq 'WIDGET_DRAW' then begin ;it's a button event
     if msg.type eq 1 then widget_control, self.wHid,/INPUT_FOCUS
  endif else if type eq 'WIDGET_TRACKING' then $
     if msg.enter eq 1 then self->Focus                
end 

;;************************End OverRiding methods*******************************

;=============================================================================
;       tvKey_Event and Event.  Handle the internal hotkey events...
;=============================================================================
pro tvKey_Event, ev
  widget_control, ev.handler, get_uvalue=self
  self->Event,ev
end

pro tvKey::Event, ev
  type=tag_names(ev,/STRUCTURE_NAME)
  self->MsgSend,{KEY,KEY:string(ev.ch)}
end

pro tvKey::Focus
  widget_control, self.wHid,/INPUT_FOCUS,set_value=''
end

;=============================================================================
;	Cleanup.  Clean self up
;=============================================================================
pro tvKey::Cleanup
  self->tvPlug::Cleanup
  return
end

;=============================================================================
;       NOTE: Key assumes the tvDraw widget you pass has as its
;       parent a widget base without the COLUMN or ROW keywords set.
;       This will hide the hotkey box behind the tvDraw draw widget.
;       If this is not desired, pass as INVBASE the base you would
;       like to place the hotkey text entry widget into. Make sure 
;       something else is place into the base before this one (the
;       first item placed goes on top) to hide the HotKey box.
;	
;	Relevant inherited keywords:
;       MsgList: (up to ObjMsg) -- form [Obj] (just the object) where obj
;	is the name of the object wishing to recieve the Messages,
;	{KEY,KEY:''}
;
;=============================================================================
function tvKey::Init,oDraw, INVBASE=ib, SELECT_BASE=sb,_EXTRA=e
  if (self->tvPlug::Init(oDraw,_EXTRA=e) ne 1) then return,0
  
  ;; see if we need to get the invisible base ourselves
  if n_elements(ib) ne 0 then begin 
     if widget_info(ib,/VALID) then begin 
        if widget_info(ib,/TYPE) ne 0 then needib=1 else needib=0
     endif else needib=1
  endif else needib=1
  
  if needib then begin
     oDraw->GetProperty,DRAWWIDGET=dw
     ib=widget_info(dw,/PARENT) ;we'll just hope it's not /col or /row'd
  endif 
  
  ;; make a hidden text widget for hotkey action
  self.wHid=widget_text(ib,/ALL,FRAME=0,xsize=1,EVENT_PRO='tvKey_Event', $
                        uvalue=self)
  widget_control, self.wHid, /INPUT_FOCUS
  self->Update,/BUTTON,/TRACKING
  return,1
end

;=============================================================================
;	tvKey__define.  Prototype the tvKey class.
;=============================================================================
pro tvKey__define
  struct={tvKey, $ 
          INHERITS tvPlug, $    ;make it a tvDraw plug-in
          wHid: 0L}             ;the widget id of the hidden text widget
end

