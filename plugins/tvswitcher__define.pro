;+
; tvSwitcher - A tvPlug_lite plugin selector with hotkeys and
;              (optionally) icons and menus, designed to work with a
;              tvdraw object and some tvPlug plugins.
;-
;;**************************OverRiding methods********************************
;=============================================================================
;  Message - Expecting TVKEY_KEY or TVPLUG_ON_OFF message
;=============================================================================
pro tvSwitcher::Message, msg
  self->tvPlug_lite::Message,msg,TYPE=type
  if type eq 'TVPLUG_ON_OFF' then begin 
     wh=where(self->GetMsgObjs() eq msg.Object,cnt)
     ;; If we're managing it, set the button (if there is one)
     if cnt gt 0 then self->SetButton,msg.Object,wh[0] else return
     ;; If an exclusive has reported turning on, turn all others off
     if (*self.MsgList)[wh[0]].Exclusive AND msg.Status then $
        self->Exclusive,wh[0]
     return
  endif 
  
  ;; A key press
  if NOT self.UseCase then char=strlowcase(msg.KEY) else char=msg.KEY
  wh=where((*self.MsgList).keys eq char,cnt)
  if cnt eq 0 then return
  self->Toggle,wh[0]
end

function tvSwitcher::GetMsgListObj, ml
  return,ml.obj
end
;;************************End OverRiding methods*******************************

pro tvSwitcher_Event, ev
  widget_control, ev.handler, get_uvalue=self
  self->Event,ev
end

;=============================================================================
;  Event - Give the hidden widget_text focus.
;=============================================================================
pro tvSwitcher::Event, ev
  type=tag_names(ev,/STRUCTURE_NAME)
  if type eq 'WIDGET_TRACKING' then begin 
     if ev.enter eq 1 then begin 
        self.Key->Focus
        if obj_valid(self.oColor) then self.oColor->SetColors,/NO_REDRAW
     endif 
     return
  endif 
  
  ;; Button Clicked
  widget_control, ev.id,get_uvalue=which
  self->Toggle,which
end

;=============================================================================
;  Toggle - Turn off or on.  For exclusives, this means: if off, turn
;           on, if on, turn on again (since some use this as a reset).
;           For non-exclusives, simply toggle.
;=============================================================================
pro tvSwitcher::Toggle, hit
  if self->MsgListLen() eq 0 then return
  list=*self.MsgList
  obj=self->GetMsgListObj(list[hit])
  if list[hit].Exclusive then obj->On else $
     if obj->On() then obj->Off else obj->On
end

;=============================================================================
;  Exclusive - Turn all other exclusive objects off, besides the one passed
;=============================================================================
pro tvSwitcher::Exclusive, keep
  if self->MsgListLen() eq 0 then return
  wh=where((*self.MsgList).Exclusive,cnt)
  if cnt eq 0 then return
  wh=where(wh ne keep,cnt)
  if cnt eq 0 then return
  for i=0,cnt-1 do (self->GetMsgListObj((*self.MsgList)[wh[i]]))->Off
end

;=============================================================================
;  SetButton - Set a button icon, and modify menu checked status, if
;              present.
;=============================================================================
pro tvSwitcher::SetButton,object,which
  if NOT ptr_valid(self.wList) then return
  ic=object->Icon()
  if NOT keyword_set(ic) then return
  
  bitmap=size(ic,/N_DIMENSIONS) eq 2
  
  if object->Enabled() eq 0b then begin 
     if widget_info((*self.wList)[which],/VALID_ID) then $
        widget_control, (*self.wList)[which],SENSITIVE=0
     if widget_info((*self.wTList)[which],/VALID_ID) then $
        widget_control, (*self.wTList)[which],SENSITIVE=0
     return
  endif 
  
  if object->On() then begin    ;on
     if size(ic,/N_DIMEN) eq 2 then val=ic else val='*'+ic+'*'
  endif else begin              ;off
     if size(ic,/N_DIMEN) eq 2 then val=ic XOR 255b else val=ic
  endelse 
  if widget_info((*self.wList)[which],/VALID_ID) then $
     widget_control, (*self.wList)[which],set_value=val,/SENSITIVE
  if widget_info((*self.wTList)[which],/VALID_ID) then $
     widget_control, (*self.wTList)[which],/SENSITIVE ;XXX checked menu status
;  if NOT ptr_valid(self.wTList) then return
;  widget_control, (*self.wTList)[which],SET_BUTTON=status
end

;=============================================================================
;  Start - Setup the icons in two bases, if necessary.
;=============================================================================
pro tvSwitcher::Start
  objs=self->GetMsgObjs()
  if obj_valid(objs[0]) eq 0 then return
  
  ;; Set up the base(s)' event handler
  for type=0,1 do begin 
     if widget_info(self.sBase[type],/VALID_ID) eq 0 then continue
     widget_control, self.sBase[type], EVENT_PRO='tvSwitcher_Event', $
                     SET_UVALUE=self, /TRACKING_EVENTS
  endfor 
  
  no=n_elements(objs) 
  self.wList=ptr_new(lonarr(no))
  
  for i=0,no-1 do begin 
     ;; Signup for on and off messages from these plug-ins
     objs[i]->MsgSignup,self,/TVPLUG_ON_OFF
     ;; Add the button
     ic=objs[i]->Icon()
     if NOT keyword_set(ic) then continue
     if size(ic,/N_DIMEN) eq 2 then val=ic XOR 255b else val=ic   
     (*self.wList)[i]= $
        widget_button(self.sBase[1b-(*self.MsgList)[i].Exclusive], $
                      /NO_RELEASE,value=val,UVALUE=i,/ALIGN_CENTER, $
                     SENSITIVE=objs[i]->Enabled()) 
     ;TOOLTIP=objs[i]->Description()
  endfor 
  
  ;; Get the Color object, if any
  oCol=self.oDraw->GetMsgObjs(CLASS='tvColor')
  if obj_valid(oCol[0]) then self.oColor=oCol[0]
  
  if widget_info(self.toolMenu,/VALID_ID) then begin 
     widget_control, self.toolMenu, EVENT_PRO='tvSwitcher_Event', $
                     SET_UVALUE=self
  endif else return
  
  self.wTList=ptr_new(lonarr(no))
  ;; Add them to a tool menu in two separate categories (if appropriate)
  exc=where((*self.MsgList).EXCLUSIVE,ecnt,COMPLEMENT=tog,NCOMPLEMENT=tcnt)
  for i=0,ecnt-1 do begin 
     desc=objs[exc[i]]->Description()
     if NOT keyword_set(desc) then continue
     (*self.wTList)[exc[i]]= $  ;/CHECKED_MENU
        widget_button(self.toolMenu,value=desc,UVALUE=exc[i], $
                      SENSITIVE=objs[exc[i]]->Enabled())
  endfor 
  
  for i=0,tcnt-1 do begin 
     desc=objs[exc[i]]->Description()
     if NOT keyword_set(desc) then continue
     (*self.wTList)[tog[i]]= $
        widget_button(self.toolMenu,value=desc,UVALUE=tog[i], $
                      SEPARATOR=i eq 0, SENSITIVE=objs[tog[i]]->Enabled()) 
                                ;/CHECKED_MENU
  endfor 
end

;=============================================================================
;  Cleanup
;=============================================================================
pro tvSwitcher::Cleanup
  ptr_free,self.wList,self.wTList
  self->tvPlug_lite::Cleanup
end

;=============================================================================
;  Init - Initialize the Switcher.  If USECASE is set, the case of the
;         hotkey will be considered; case is not considered by default.
;
;         Inherited keywords:
;         
;         MsgList(structure up to ObjMsg):
;
;              {Obj:obj, keys:'',Exclusive:0b}
;
;           where obj is the name of the object and keys are the keys
;           to toggle activity on.  Separate exclusive and
;           non-exclusive into separate bases.
;=============================================================================
function tvSwitcher::Init,parent,oDraw,TOOL_MENU=tm,USECASE=uc,_EXTRA=e
  if (self->tvPlug_lite::Init(oDraw,_EXTRA=e) ne 1) then return,0
  
  ;; Get our key object and sign us up.  Important Keywords get Passed On.
  self.Key=obj_new('tvKey',oDraw)
  self.Key->MsgSignup,self,/TVKEY_KEY
  self.UseCase=keyword_set(uc) 
  
  ;; see if a valid base was passed, if so make the buttons bases there
  if widget_info(parent,/VALID_ID) eq 0 then return,0
  
  ;; The tool menu
  if n_elements(tm) ne 0 then if widget_info(tm,/VALID_ID) then $
     self.toolMenu=tm
  
  ;; Set up the base(s) for the buttons (actually create them in Start)
  if self->MsgListLen() ne 0 then begin 
     if NOT self.UseCase then  $
        (*self.MsgList).keys=strlowcase((*self.MsgList).keys)
     exc=where((*self.MsgList).Exclusive,exc_cnt,COMPLEMENT=tog, $
               NCOMPLEMENT=tog_cnt)
     row_base=widget_base(parent,/FRAME,/ROW,YPAD=0,XPAD=0,SPACE=1, $
                          /BASE_ALIGN_TOP) 
     if exc_cnt gt 0 then $
        self.sBase[0]=widget_base(row_base,/ROW,SPACE=0,XPAD=0,YPAD=0,/FRAME)

     if tog_cnt gt 0 then $
        self.sBase[1]=widget_base(row_base,/ROW,SPACE=0,XPAD=0,YPAD=0,/FRAME) 
  endif 
  
  return,1
end

;=============================================================================
;	tvSwitcher__define.  Prototype the tvSwitcher class.
;=============================================================================
pro tvSwitcher__define
  st={tvSwitcher, $
      INHERITS tvPlug_lite, $   ;tvDraw_lite plugin (no on_off stuff needed)
      Key:obj_new(), $          ;our key plugin
      UseCase:0b, $             ;whether to consider case
      sBase: [0L,0L], $         ;widget ids of the button bases
      oColor:obj_new(), $       ;the color object (if any)
      toolMenu: 0L, $           ;the menu for the tools
      wList: ptr_new(), $       ;list of buttons, for each on MsgList
      wTList:ptr_new(), $       ;list of tool menu buttons
      cur:0}                    ;which is currently the active one
end
