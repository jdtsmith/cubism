;;**************************OverRiding methods********************************

;=============================================================================
;       Message - We have signed up for button and tracking messages
;                 from the tvDraw object.
;=============================================================================
pro tvKey::Message, msg
  self->tvPlug::Message,msg,TYPE=type ;pass it up to tvPlug
  ;; set the input focus on entering the draw window, and on button release
  ;; events.
  if type eq 'DRAW_BUTTON' then begin ;it's a button event
     if msg.type eq 1 then self->Focus ; reset on release
  endif else if type eq 'WIDGET_TRACKING' then $
     if msg.enter eq 1 then self->Focus                
end 

;;************************End OverRiding methods*******************************

pro tvKey_Event, ev
  widget_control, ev.handler, get_uvalue=self
  self->Event,ev
end

;=============================================================================
;       Event - Handle the internal hotkey events.
;=============================================================================
pro tvKey::Event, ev
  self->Focus
  case ev.type of
     3: begin                   ;WIDGET_TEXT_SEL
        delta=(widget_info(self.wHid,TEXT_OFFSET_TO_XY=ev.offset)-[1,1])*[1,-1]
        if total(abs(delta)) gt 1. then break
        ;print,'ARROW: ', delta
        self->MsgSend, {TVKEY_ARROW, MOVE:delta}
     end
     0: begin                   ;WIDGET_TEXT_CH
        if ev.ch ge 1b AND ev.ch le 26b then str='C-'+string(ev.ch+96b) else $
           str=string(ev.ch)    
        self->MsgSend,{TVKEY_KEY,KEY:str}
     end 
     2: self->MsgSend,{TVKEY_KEY,KEY:'DEL'} ;WIDGET_TEXT_DEL
  endcase 
end

;=============================================================================
;       Focus - Give the text widget focus.
;=============================================================================
pro tvKey::Focus
  widget_control, self.wHid,/INPUT_FOCUS,set_value=['..','..','..'], $
                  SET_TEXT_SELECT=self.base_pos
end

;=============================================================================
;       Cleanup
;=============================================================================
pro tvKey::Cleanup
  self->tvPlug::Cleanup
end

;=============================================================================
;	Init 
;=============================================================================
;       NOTE: Key assumes the tvDraw widget passed has as its parent a
;       widget base without the COLUMN or ROW keywords set, where it
;       will hide the hotkey box behind the tvDraw draw widget.  If
;       this is not desired, pass as INVBASE the base you would like
;       to place the hotkey text entry widget into. Make sure
;       something else is place into the base before this one (the
;       first item placed goes on top) to hide the HotKey box.
;=============================================================================
function tvKey::Init,oDraw, INVBASE=ib,SELECT_BASE=sb,_EXTRA=e
  if (self->tvPlug::Init(oDraw,_EXTRA=e) ne 1) then return,0
  
  ;; see if we need to create the invisible base ourselves
  if n_elements(ib) ne 0 then begin 
     if widget_info(ib,/VALID) then begin 
        if widget_info(ib,/TYPE) ne 0 then needib=1 else needib=0
     endif else needib=1
  endif else needib=1
  
  if needib then begin
     oDraw->GetProperty,DRAWWIDGET=dw
     ib=widget_info(dw,/PARENT) ;we'll just hope it's not /col or /row'd
  endif 
  
  ;; make a hidden text widget for key events
  self.wHid=widget_text(ib,/ALL,FRAME=0,xsize=1,ysize=3, $
                        EVENT_PRO='tvKey_Event', uvalue=self)
  self->Focus
  self.base_pos=widget_info(self.wHid,TEXT_XY_TO_OFFSET=[1,1])
  self->MsgSetup,['TVKEY_ARROW','TVKEY_KEY']
  self.oDraw->MsgSignup,self,/DRAW_BUTTON,/WIDGET_TRACKING
  return,1
end

;=============================================================================
;	tvKey__define -  Prototype the tvKey class.
;=============================================================================
pro tvKey__define
  struct={tvKey, $ 
          INHERITS tvPlug, $    ;make it a tvDraw plug-in
          base_pos:0, $         ;which character is centered
          wHid: 0L}             ;the widget id of the hidden text widget
  st={TVKEY_ARROW, MOVE:[0,0]}
  st={TVKEY_KEY,    KEY:''}
end

