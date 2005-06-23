;;**************************OverRiding methods********************************
;=============================================================================
;  Message - We'll hear from CubeRec
;=============================================================================
pro CubeBackTrack::Message, msg
  self->tvPlug::Message,msg,TYPE=type
  case type of
     'DRAW_MOTION': begin 
        pt=self.oDraw->Convert([msg.X,msg.Y],/SHOWING)
        if array_equal(pt,self.point) then return
        self.point=pt
        self->UpdateList
        return
     end 
     
     'DRAW_BUTTON': begin 
        if msg.type ne 0 then return ;presses only
        pt=self.oDraw->Convert([msg.X,msg.Y],/SHOWING)
        if msg.press eq 1b AND pt[0] eq -1 then return
        if NOT array_equal(pt,self.point) then begin 
           self.point=pt
           self->UpdateList
        endif 
        if self.lock then self->DrawMark,/ERASE ; erase the old one either way
        self.lock=msg.press eq 1b
        self.oDraw->MsgSignup,self,DRAW_MOTION=self.lock eq 0b, $
                              TVDRAW_REDRAW=self.lock
        if self.lock then begin 
           self.mark=pt
           self->DrawMark
        endif 
        return
     end        
     
     'TVDRAW_REDRAW': begin 
        if self.lock then self->DrawMark
        return
     end
     
     'CUBEREC_UPDATE': begin 
        if msg.FULL_MODE then begin ; Full cube mode
           if self.cube ne msg.CUBE && obj_valid(msg.CUBE) then begin 
              self.cube=msg.CUBE
              self.cube->GetProperty,BCD_SIZE=bcdsz,PROJECT_NAME=pn, $
                                     WAVELENGTH=wave
              if widget_info(self.wBase,/VALID_ID) then $
                 widget_control,BASE_SET_TITLE= $
                                string(FORMAT='(%"Backtracking: %s")',pn), $
                                self.wBase
              self.wavelength=wave[msg.plane]
              self.msg_base=string(FORMAT='(%"Cube: %s")',pn)
              if bcdsz[0] ne -1 then self.bcd_size=bcdsz
           endif 
           self->Enable 
        endif else begin 
           if widget_info(self.wBase,/VALID_ID) then $
              widget_control, self.wBase,/DESTROY
           self->Off,/DISABLE
           return
        endelse 
     end
     
     'CUBEREC_FULL': begin 
        self->Enable
        self.wavelength=msg.WAVELENGTH
     end
  endcase
  self.plane=msg.PLANE
  if widget_info(self.wBase,/VALID_ID) then self->UpdateList
end

;=============================================================================
;  Off - No more events needed
;=============================================================================
pro CubeBackTrack::Off,_EXTRA=e
  self->tvPlug::Off,_EXTRA=e
  if self.lock then begin 
     self->DrawMark,/ERASE
     self.lock=0b
  endif 
  self->wDestroy
  ;; still listen for cube mode changes
  self.oDraw->MsgSignup,self,DRAW_MOTION=0,DRAW_BUTTON=0,TVDRAW_REDRAW=0
end

;=============================================================================
;  On - Signup for all our messages.
;=============================================================================
pro CubeBackTrack::On
  if self->On() then begin 
     self->Off
     return
  endif 
  self->EnsureCube
  self->tvPlug::On
  self.cube->GetProperty,PROJECT_NAME=pn
  self.msg_base=string(FORMAT='(%"Cube: %s")',pn)
  title=string(FORMAT='(%"BackTracking: %s")',pn)
  self.msg_head= $
     "BCD                      Pix         Frac        Val       Back  " + $
     "(Val-Back)  Flag"
  msg=self.msg_base+string(10b)+self.msg_head
  self.wBase=widget_base(/COLUMN, SPACE=1,GROUP_LEADER=self.parent, $
                         TITLE=title, /TLB_SIZE_EVENTS,UVALUE=self)
  self.wLabel=widget_label(self.wBase,value=msg,/ALIGN_LEFT, $
                           /DYNAMIC_RESIZE)
  if self.list_size gt 0 then $
     self.wList=widget_list(self.wBase,XSIZE=87,SCR_YSIZE=self.list_size, $
                            /CONTEXT_EVENTS) $
  else self.wList=widget_list(self.wBase,XSIZE=87,YSIZE=8,/CONTEXT_EVENTS)
  
  self.wMenu=widget_base(self.wList,/CONTEXT_MENU)
  self.wCBut_mark=widget_button(self.wMenu,VALUE='Mark as Bad Pixel')
  self.wCBut_unmark=widget_button(self.wMenu,VALUE='Mark as Good Pixel')
  
  widget_control, self.wBase, SET_UVALUE=self,/REALIZE
  make_widget_adjacent,self.wBase,self.parent
  
  ;; Save geometry for re-sizing
  geom=widget_info(self.wList,/GEOMETRY)
  self.list_size_diff=geom.SCR_YSIZE- $
     (widget_info(self.wBase,/GEOMETRY)).SCR_YSIZE
  self.oDraw->MsgSignup,self,/DRAW_MOTION,/DRAW_BUTTON
  XManager,title, self.wBase,/NO_BLOCK, EVENT_HANDLER='CubeBackTrack_event', $
           CLEANUP='CubeBackTrack_kill'
end

;=============================================================================
;  Icon
;=============================================================================
function CubeBackTrack::Icon
  return,[[016B, 000B],[040B, 000B],[068B, 252B],[130B, 000B],$
          [017B, 241B],[131B, 001B],[069B, 241B],[041B, 001B],$
          [017B, 253B],[146B, 000B],[084B, 240B],[056B, 001B],$
          [016B, 242B],[224B, 007B],[000B, 242B],[000B, 001B]]
end

;=============================================================================
;  Description
;=============================================================================
function CubeBackTrack::Description
  return,'BackTrack Cube Pixels'
end

;=============================================================================
;  MouseHelp
;=============================================================================
function CubeBackTrack::MouseHelp
  return,['Lock/Unlock Pixel','','']
end

;=============================================================================
;  ReportWidget - Where to position our error and other messages
;=============================================================================
function CubeBackTrack::ReportWidget
  return,self.parent
end
;;*************************End OverRiding methods******************************

pro CubeBackTrack::DrawMark,ERASE=erase
  self.oDraw->GetProperty,ZOOM=zoom
  pt=self.oDraw->Convert(self.mark,/DEVICE,/SHOWING)
  if keyword_set(erase) then self.oDraw->Erase,pt-zoom/2-1,[zoom,zoom]+2 $
  else plots,pt[0],pt[1],/DEVICE,COLOR=self.color,PSYM=7,SYMSIZE=zoom/7, $
             THICK=zoom ge 4?2.:1.
end

pro CubeBackTrack_kill,id
  widget_control, id,get_uvalue=self
  if obj_valid(self) then self->Off
end

pro CubeBackTrack_event, ev
   widget_control, ev.top, get_uvalue=self
   self->Event,ev
end

;=============================================================================
;  Event
;=============================================================================
pro CubeBackTrack::Event,ev
  tn=tag_names(ev,/STRUCTURE_NAME)
  case tn of
     'WIDGET_BASE': begin       ;size
        self.list_size=ev.Y+self.list_size_diff
        widget_control, self.wList, SCR_YSIZE=self.list_size
     end 
     
     'WIDGET_CONTEXT': begin 
        if ~ptr_valid(self.list) then return
        item=widget_info(self.wList,/LIST_SELECT)
        if item lt 0 then return
        item=(*self.list)[item]
        pix=item.bcd_pix
        widget_control, self.wCBut_mark, SENSITIVE=item.bad?0:1
        widget_control, self.wCBut_unmark, SENSITIVE=item.bad?1:0
        widget_displaycontextmenu,ev.id,ev.x,ev.y,self.wMenu
     end
     
     'WIDGET_BUTTON': begin ;context menus
        item=widget_info(self.wList,/LIST_SELECT)
        if item lt 0 then return
        item=(*self.list)[item]
        case ev.id of
           self.wCBut_mark: $
              self.cube->ToggleBadPixel,item.bcd_pix,/SET,/UPDATE
           self.wCBut_unmark: $
              self.cube->ToggleBadPixel,item.bcd_pix,SET=0,/UPDATE
        endcase 
        self->UpdateList
     end 
     else: ; Flush all other events
  endcase 
end

;=============================================================================
;  UpdateList - Update the list of backtracked pixels
;=============================================================================
pro CubeBackTrack::UpdateList
  msg=self.msg_base+ $
      (self.point[0] eq -1?$
       string(FORMAT='(%" Pix: [--,--] %6.3f um")',self.wavelength): $
       string(FORMAT='(%" Pix: [%d,%d] %6.3f um")',self.point, $
              self.wavelength))+ $
      string(10b)+self.msg_head
  widget_control, self.wLabel,SET_VALUE=msg
  if self.point[0] eq -1 then begin 
     widget_control,self.wList,SET_VALUE='---'
     return
  endif 
  self->EnsureCube
  list=self.cube->BackTrackPix(self.point,self.plane,/FOLLOW)
  oldid=''
  if size(list,/N_DIMENSIONS) eq 0 then str='' else begin 
     str=strarr(n_elements(list))
     for i=0,n_elements(list)-1 do begin 
        str[i]=string(FORMAT='(" (",I3,",",I3,") ",G9.3,1X,G9.3,1X,' + $
                      'G9.3,1X,G9.3,2X,A)', $
                      list[i].BCD_PIX mod self.bcd_size[0], $
                      list[i].BCD_PIX/self.bcd_size[0], $
                      list[i].AREA,list[i].BCD_VAL,list[i].BACK_VAL, $
                      list[i].BCD_VAL-list[i].BACK_VAL,list[i].FLAGS)
        if list[i].ID eq oldid then begin 
           str[i]=string(FORMAT='(23X,A)',str[i])
        endif else begin 
           str[i]=string(FORMAT='(A21,2X,A)',list[i].ID, $
                         str[i])
           oldid=list[i].ID
        endelse 
     endfor
  endelse 
  widget_control, self.wList,SET_VALUE=str
  ptr_free,self.list
  self.list=ptr_new(list,/NO_COPY)
end

;=============================================================================
;  EnsureCube - Make sure the cube we have is still valid
;=============================================================================
pro CubeBackTrack::EnsureCube
  if ~obj_valid(self.cube) then begin 
     self->Off
     self->Error,'Cube no longer valid.'
  endif 
end

;=============================================================================
;       wDestroy - Destroy the Widget
;=============================================================================
pro CubeBackTrack::wDestroy
  if NOT widget_info(self.wBase, /VALID_ID) then return
  widget_control, self.wBase,/DESTROY
end

pro CubeBackTrack::Cleanup
  ptr_free,self.list
  self->tvPlug::Cleanup
end

;=============================================================================
;  Init -  Initialize the CubeBackTrack object
;=============================================================================
function CubeBackTrack::Init,parent,oDraw,COLOR=color,_EXTRA=e
  if (self->tvPlug::Init(oDraw,_EXTRA=e) ne 1) then return,0 
  self.parent=parent
  self.point=[-1,-1]
  if n_elements(color) ne 0 then self.color=color
  return,1
end

;=============================================================================
;  CubeBackTrack__define - Prototype the CubeBackTrack class
;=============================================================================
pro CubeBackTrack__define
  st={CubeBackTrack, $
      INHERITS tvPlug,$         ;it's a full tvDraw plugin
      cube:obj_new(), $         ;the cube project we're servicing
      bcd_size:[0,0], $         ;the size of the BCD being viewed
      plane:0L, $               ;which plane of the cube is showing
      wavelength:0.0, $         ;the WaveLength that corresponds to
      point: [0,0], $           ;the point being examined
      mark: [0,0], $            ;the point marked
      lock:0b, $                ;whether to lock onto a single point
      list:ptr_new(), $         ;the backtrack list
      msg_base:'', $            ;the base for the title
      msg_head:'', $            ;the header message
      ;; Widget 
      color:0, $                ;the color to draw ourselves with
      list_size:0, $            ;pixel size of the list
      list_size_diff:0, $       ;for resizing the list
      parent:0L, $
      wBase:0L, $
      wLabel:0L, $
      wList:0L, $               ;list widget
      wMenu:0L, $               ;context menu
      wCBut_mark: 0L, $         ;mark button
      wCBut_unmark: 0L}         ;unmark button
end
