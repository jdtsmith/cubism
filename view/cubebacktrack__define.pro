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
     'CUBEREC_UPDATE': begin 
        if msg.FULL_MODE then begin ; Full cube mode
           if self.cube ne msg.CUBE then begin 
              self.cube=msg.CUBE
              self.cube->GetProperty,BCD_SIZE=bcdsz,PROJECT_NAME=pn, $
                                     WAVELENGTH=wave
              if widget_info(self.wBase,/VALID_ID) then $
                 widget_control,BASE_SET_TITLE= $
                                string(FORMAT='(%"Backtracking: %s")',pn), $
                                self.wBase
              self.wavelength=wave[msg.plane]
              self.msg_base=string(FORMAT='(%"Cube: %s")',pn)
              if bcdsz[0] ne 0 then self.bcd_size=bcdsz
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
  self->EnsureCube
  if widget_info(self.wBase,/VALID_ID) then self->UpdateList
end

;=============================================================================
;  Off - No more events needed
;=============================================================================
pro CubeBackTrack::Off,_EXTRA=e
  self->tvPlug::Off,_EXTRA=e
  self->wDestroy
  self.oDraw->MsgSignup,self,DRAW_MOTION=0 ;still listen for cube mode changes
end

;=============================================================================
;  On - Signup for all our messages.
;=============================================================================
pro CubeBackTrack::On
  self->tvPlug::On
  self.cube->GetProperty,PROJECT_NAME=pn
  self.msg_base=string(FORMAT='(%"Cube: %s")',pn)
  self.wBase=widget_base(/COLUMN, SPACE=1,GROUP_LEADER=self.parent, $
                         TITLE=string(FORMAT='(%"BackTracking: %s")',pn), $
                         KILL_NOTIFY='cubebacktrack_kill', $
                         UVALUE=self)
  self.wLabel=widget_label(self.wBase,value=self.msg_base,/ALIGN_LEFT, $
                           /DYNAMIC_RESIZE)
  self.wList=widget_list(self.wBase,XSIZE=40,YSIZE=6)
  widget_control, self.wBase, /REALIZE
  self.oDraw->MsgSignup,self,/DRAW_MOTION
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
;  ReportWidget - Where to position our error and other messages
;=============================================================================
function CubeBackTrack::ReportWidget
  return,self.wBase
end
;;*************************End OverRiding methods******************************

;=============================================================================
;  UpdateList - Update the list of backtracked pixels
;=============================================================================
pro CubeBackTrack::UpdateList
  msg=self.msg_base+ $
      (self.point[0] eq -1?$
       string(FORMAT='(%" Pix: [--,--] %6.3f um")',self.wavelength): $
       string(FORMAT='(%" Pix: [%d,%d] %6.3f um")',self.point,self.wavelength))
  widget_control, self.wLabel,SET_VALUE=msg
  if self.point[0] eq -1 then begin 
     widget_control,self.wList,SET_VALUE='---'
     return
  endif 
  list=self.cube->BackTrackPix(self.point,self.plane)
  oldid=''
  str=strarr(n_elements(list))
  for i=0,n_elements(list)-1 do begin 
     str[i]=string(FORMAT='(" (",I3,",",I3,") ",G9.3)', $
                   list[i].BCD_PIX mod self.bcd_size[0], $
                   list[i].BCD_PIX/self.bcd_size[0],list[i].AREA)
     if list[i].ID eq oldid then begin 
        str[i]=string(FORMAT='(16X,A)',str[i])
     endif else begin 
        str[i]=string(FORMAT='(A14,2X,A)',list[i].ID, $
                      str[i])
        oldid=list[i].ID
     endelse 
  endfor
  widget_control, self.wList,SET_VALUE=str
  ;ptr_free,self.list
  ;self.list=ptr_new(list,/NO_COPY)
end

pro CubeBackTrack_kill,id
  widget_control, id,get_uvalue=self
  if obj_valid(self) then self->Off
end

;=============================================================================
;  EnsureCube - Make sure the cube we have is still valid
;=============================================================================
pro CubeBackTrack::EnsureCube
  if NOT obj_valid(self.cube) then begin 
     widget_control, self.wBase[self.mode],SENSITIVE=0
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

;=============================================================================
;  Init -  Initialize the CubeBackTrack object
;=============================================================================
function CubeBackTrack::Init,parent,oDraw,_EXTRA=e
  if (self->tvPlug::Init(oDraw,_EXTRA=e) ne 1) then return,0 
  self.parent=parent
  self.point=[-1,-1]
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
      list:ptr_new(), $         ;the backtrack list
      msg_base:'', $            ;the base for the title
      ;; Widget ID's
      parent:0L, $
      wBase:0L, $
      wLabel:0L, $
      wList:0L}
end
