;+
; NAME:
;
;    tvSwitcher
;
; DESCRIPTION:
;
;    A tvTools plugin to manage switching on and off other exclusive
;    and non-exlusive plugins via hotkeys, icons, and a menu.
;
; CATEGORY:
;
;    tvTools, Plugin control
;
; METHODS:
;
;    Init:  
;
;       CALLING SEQUENCE:
;
;          obj=obj_new('tvSwitcher',oDraw,parent,[TOOL_MENU=,/USECASE],
;                      _EXTRA=e)
;          
;       INPUT PARAMETERS:
;
;	   oDraw: The tvDraw object.
;
;          parent: The widget ID where the line reporting label will
;            be placed.
;	   
;       INPUT KEYWORD PARAMETERS:
;
;          TOOL_MENU: The widget ID of a menu to root the tool
;             selection menu in.
;
;          USECASE: If set, the case of th ehotkey will be considered.
;             Otherwise not.
;
;          _EXTRA: Any other ObjMsg initialization keywords
;             (e.g. message list).
;          
; NOTES
;
;    Takes a message signup list (via ObjMsg) of the form
;
;      {Obj:obj,keys:'',Exclusive:0b}
;
;    to control plugin object "obj" with hotkey keys, either
;    exclusively (i.e. shutting down other exlusive plugins), or
;    non-exclusively.
;
; INHERITANCE TREE:
;
;    ObjMsg-->tvPlug_lite-->tvSwitcher
;
; MODIFICATION HISTORY:
;
;    2001-09-28 (J.D. Smith): Renamed from SCORE-era sources tvHotKey.
;       
;-
;    $Id$
;##############################################################################
;
; LICENSE
;
;  Copyright (C) 2001-2005 J.D. Smith
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
;  Message - Expecting DRAW_KEY or TVPLUG_ON_OFF message
;=============================================================================
pro tvSwitcher::Message, msg
  self->tvPlug_lite::Message,msg,TYPE=type
  if type eq 'TVPLUG_ON_OFF' || type eq 'TVPLUG_ENABLE_DISABLE' then begin 
     wh=where(self->GetMsgObjs() eq msg.Object,cnt)
     ;; If we're managing it, set the button/cursor (if there is one)
     if cnt gt 0 then self->SetButton,msg.Object,wh[0] else return
     if type eq 'TVPLUG_ENABLE_DISABLE' then return
     ;; If an exclusive has reported turning on (from off), turn all others off
     if (*self.MsgList)[wh[0]].Exclusive && msg.Changed then begin 
        if (msg.Status AND 1b) then self->Exclusive,wh[0]
        self->CheckCursor,msg.Object
     endif 
  endif else if type eq 'WIDGET_TRACKING' then begin 
     self->SetCursor,REMOVE=~msg.enter
  endif else begin 
     ;; A key press
     if msg.release then return ; Press only 
     if msg.type eq 6 then return ; non-ASCII keys not allowed
     if (msg.modifiers AND (NOT 5L)) ne 0L then return ; only Shift/Caps Lock
  
     if ~self.UseCase then char=strlowcase(msg.CH) else char=string(msg.CH)
     wh=where((*self.MsgList).keys eq char,cnt)
     if cnt gt 0 then self->Toggle,wh[0]
  endelse 
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
;  Event - Give focus or respond to buttons
;=============================================================================
pro tvSwitcher::Event, ev
  type=tag_names(ev,/STRUCTURE_NAME)
  if type eq 'WIDGET_TRACKING' then begin 
     if ev.enter eq 1 then begin 
        if obj_valid(self.oColor) then self.oColor->SetColors,/NO_REDRAW
     endif 
     return
  endif 
  
  ;; Button Clicked
  widget_control, ev.id,get_uvalue=which
  self->Toggle,which
  self.oDraw->Focus
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
  if ~obj->Enabled() then return
  self.oDraw->QueueRedraws
  if list[hit].Exclusive then obj->On else $
     if obj->On() then obj->Off else obj->On
  self.oDraw->FlushRedraws
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
  self.inhibit_cursor=1b      ;we don't want forced offs to set the cursor
  for i=0,cnt-1 do begin 
     obj=self->GetMsgListObj((*self.MsgList)[wh[i]])
     if obj->On() then obj->Off
  endfor
  self.inhibit_cursor=0b
end

;=============================================================================
;  SetButton - Set a button icon, and modify menu checked status, if
;              present.  
;=============================================================================
pro tvSwitcher::SetButton,object,which
  if ~ptr_valid(self.wList) then return
  onQ=object->On() & enabledQ=object->Enabled()
  if widget_info((*self.wList)[which],/VALID_ID) then $
     widget_control, (*self.wList)[which], SET_BUTTON=onQ,SENSITIVE=enabledQ
    
  if ~ptr_valid(self.wTList) then return
  if widget_info((*self.wTList)[which],/VALID_ID) then $
     widget_control, (*self.wTList)[which],SET_BUTTON=onQ,SENSITIVE=enabledQ
end


;=============================================================================
;  CheckCursor -  Check for changes in the cursor
;=============================================================================
pro tvSwitcher::CheckCursor,object
  if self.inhibit_cursor then return
  if ~obj_valid(object) then return
  self.oDraw->GetProperty,FOCUSED=focused
  
  self.active_cursor=object     ;assume it will be picked up by the enter event
  if focused then self->SetCursor ;if we are inside, do the cursor change now
end

;=============================================================================
;  SetCursor -  Set or remove the custom cursor
;=============================================================================
pro tvSwitcher::SetCursor,object,REMOVE=remove
  if keyword_set(remove) then begin 
     device,/CURSOR_CROSSHAIR
     return
  endif 
  object=self.active_cursor
  if obj_valid(object) && object->On() && object->Enabled() then begin
     cursor=object->Cursor(mask,offset)
     if cursor[0] ne -1 then begin 
        device,CURSOR_IMAGE=cursor,CURSOR_MASK=mask,CURSOR_XY=offset 
     endif else device,/CURSOR_CROSSHAIR
  endif else begin 
     self.active_cursor=obj_new()
     device,/CURSOR_CROSSHAIR
  endelse 
end

;=============================================================================
;  Start - Setup the icons in two bases, if necessary.
;=============================================================================
pro tvSwitcher::Start
  objs=self->GetMsgObjs()
  if obj_valid(objs[0]) eq 0 then return
  
  widget_control, self.topmenu,UPDATE=0
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
     objs[i]->MsgSignup,self,/TVPLUG_ON_OFF,/TVPLUG_ENABLE_DISABLE
     ;; Add the button
     ic=objs[i]->Icon()
     if NOT keyword_set(ic) then continue
     if size(ic,/N_DIMEN) eq 2 then val=ic XOR 255b else val=ic   
     desc=objs[i]->Description()
     if (*self.MsgList)[i].keys then desc+=" ("+(*self.MsgList)[i].keys+")"
     mh=objs[i]->MouseHelp()
     if mh[0] then $
        desc+=' - [ '+mh[0]+' | '+mh[1]+' | '+mh[2]+' ]'
     (*self.wList)[i]= $
        widget_button(self.sBase[1b-(*self.MsgList)[i].Exclusive], $
                      value=val,UVALUE=i,/ALIGN_CENTER, $
                      SENSITIVE=objs[i]->Enabled(),TOOLTIP=desc)
  endfor 
  
  ;; Ask for key events:
  self.oDraw->MsgSignup,self,/DRAW_KEY,/WIDGET_TRACKING
  
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
  
  for t=0,1 do begin 
     list=t?tog:exc
     for i=0,(t?tcnt:ecnt)-1 do begin 
        desc=objs[list[i]]->Description()
        if (*self.MsgList)[list[i]].keys then $
           desc+=" ("+(*self.MsgList)[list[i]].keys+")"
        if ~keyword_set(desc) then continue
        (*self.wTList)[list[i]]= $  
           widget_button(self.toolMenu,value=desc,UVALUE=list[i], $
                         /CHECKED_MENU,SENSITIVE=objs[list[i]]->Enabled(), $
                         SEPARATOR=t eq 1 && i eq 0)
     endfor 
  endfor 
  
  widget_control, self.topmenu,/UPDATE
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
function tvSwitcher::Init,oDraw,parent,TOOL_MENU=tm,USECASE=uc,_EXTRA=e
  if (self->tvPlug_lite::Init(oDraw,_EXTRA=e) ne 1) then return,0
  
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
     self.topmenu=widget_base(parent,/ROW,/FRAME,SPACE=0,XPAD=0,YPAD=0)
     if exc_cnt gt 0 then $
        self.sBase[0]=widget_base(self.topmenu,/ROW,SPACE=0, $
                                  /TOOLBAR,/NONEXCLUSIVE,/ALIGN_LEFT)

     
     if tog_cnt gt 0 then $
        self.sBase[1]=widget_base(self.topmenu,/ROW,SPACE=0, $
                                  /TOOLBAR,/NONEXCLUSIVE,/ALIGN_RIGHT)
  endif 
  
  return,1
end

;=============================================================================
;	tvSwitcher__define.  Prototype the tvSwitcher class.
;=============================================================================
pro tvSwitcher__define
  st={tvSwitcher, $
      INHERITS tvPlug_lite, $   ;tvDraw_lite plugin (no on_off stuff needed)
      UseCase:0b, $             ;whether to consider case
      sBase: [0L,0L], $         ;widget ids of the button bases
      oColor:obj_new(), $       ;the color object (if any)
      toolMenu: 0L, $           ;the menu for the tools
      topmenu:0L, $
      active_cursor:obj_new(),$ ;the active object for cursor (if any)
      inhibit_cursor:0b,$       ;whether to inhibit cursor setting
      wList: ptr_new(), $       ;list of buttons, for each on MsgList
      wTList:ptr_new(), $       ;list of tool menu buttons
      cur:0}                    ;which is currently the active one
end
