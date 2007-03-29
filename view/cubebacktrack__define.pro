;+
; NAME:  
;
;    CubeBackTrack
;
; CONTACT:
;
;    UPDATED VERSIONS of CUBISM and more information can be found at:
;       http://sings.stsci.edu/cubism
;
; DESCRIPTION:
;    
;    Show backtracking information from individual cube planes.
;    
; CATEGORY:
;
;    CUBISM Spectral Reduction, Analysis and Processing.
;    BackTracking.
;
; METHODS:
;
;    Init:  
;
;       CALLING SEQUENCE:
;
;          obj=obj_new('CubeBackTrack',oDraw,parent,COLOR=)
;
;       INPUT PARAMETERS:
;
;          oDraw: The tvDraw object.
;
;          parent: The widget ID of the parent to place the controls into.
;
;
;       INPUT KEYWORD PARAMETERS:
;
;          COLOR: The color ID to use for drawing the backtrack mark.
;             
; NOTES:
;  
;    Left click to freeze back-tracking on an individual pixel, right
;    click to release the freeze.
;
; INHERITANCE TREE:
;
;    ObjMsg-->tvPlug-->CubeBackTrack
;
; MODIFICATION HISTORY:
;    
;    2003-03-31 (J.D. Smith): Written
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2003-2006 J.D. Smith
;
;  This file is part of CUBISM.
;
;  CUBISM is free software; you can redistribute it and/or modify it
;  under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2, or (at your option)
;  any later version.
;  
;  CUBISM is distributed in the hope that it will be useful, but
;  WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;  General Public License for more details.
;  
;  You should have received a copy of the GNU General Public License
;  along with CUBISM; see the file COPYING.  If not, write to the Free
;  Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;  Boston, MA 02110-1301, USA.
;
;##############################################################################

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
           if (self.cube ne msg.CUBE && obj_valid(msg.CUBE)) || $
              self.bcd_size[0] eq 0 then begin 
              self.cube=msg.CUBE
              self.cube->GetProperty,BCD_SIZE=bcdsz,PROJECT_NAME=pn, $
                                     WAVELENGTH=wave,ACCOUNTS_VALID=av
              if n_elements(av) eq 0 || array_equal(av,0b) then begin 
                 if widget_info(self.wBase,/VALID_ID) then $
                    widget_control, self.wBase,/DESTROY
                 self->Reset,/DISABLE
                 return
              endif 
              
              if widget_info(self.wBase,/VALID_ID) then $
                 widget_control,BASE_SET_TITLE= $
                                string(FORMAT='(%"Backtracking: %s")',pn), $
                                self.wBase
              self.wavelength=wave[msg.plane<(n_elements(wave)-1) ]
              self.msg_base=string(FORMAT='(%"Cube: %s")',pn)
              if bcdsz[0] ne -1 then self.bcd_size=bcdsz
           endif 
           self->Enable 
           self->UpdateList
        endif else begin 
           if widget_info(self.wBase,/VALID_ID) then $
              widget_control, self.wBase,/DESTROY
           self->Reset,/DISABLE
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
;  Reset - Close down the box
;=============================================================================
pro CubeBackTrack::Reset,_EXTRA=e
  self->wDestroy
  if self.lock then begin 
     self->DrawMark,/ERASE
     self.lock=0b
  endif 
  self.oDraw->MsgSignup,self,TVDRAW_REDRAW=0 ; not drawn any longer
  self->Off,_EXTRA=e
end

;=============================================================================
;  Off - No more events needed
;=============================================================================
pro CubeBackTrack::Off,_EXTRA=e
  self->tvPlug::Off,_EXTRA=e
  ;; disable events (still listen for cube mode changes)
  self.oDraw->MsgSignup,self,DRAW_MOTION=0,DRAW_BUTTON=0
end

;=============================================================================
;  On - Signup for all our messages.
;=============================================================================
pro CubeBackTrack::On
  if self->On() then begin 
     self->Reset
     return
  endif 
  self->EnsureCube
  self->tvPlug::On
  self.cube->GetProperty,PROJECT_NAME=pn
  self.msg_base=string(FORMAT='(%"Cube: %s")',pn)
  title=string(FORMAT='(%"BackTracking: %s")',pn)
  self.msg_head= $
     "BCD                      Pix          Frac       Val                 "+$
     "Back                (Val-Back)     Flag"
  msg=self.msg_base+string(10b)+self.msg_head
  
  if ~widget_info(self.wBase,/VALID_ID) then begin 
     self.wBase=widget_base(/COLUMN, SPACE=1,GROUP_LEADER=self.parent, $
                            TITLE=title, /TLB_SIZE_EVENTS,UVALUE=self)
     self.wLabel=widget_label(self.wBase,value=msg,/ALIGN_LEFT, $
                              /DYNAMIC_RESIZE)
     if self.list_size gt 0 then $
        self.wList=widget_list(self.wBase,XSIZE=111,SCR_YSIZE=self.list_size, $
                               /CONTEXT_EVENTS) $
     else self.wList=widget_list(self.wBase,XSIZE=111,YSIZE=8,/CONTEXT_EVENTS)
  
     self.wMenu=widget_base(self.wList,/CONTEXT_MENU)
     self.wCBut_global_mark=widget_button(self.wMenu, /CHECKED_MENU,$
                                          VALUE='Bad Pixel (Global)')
     self.wCBut_bcd_mark=widget_button(self.wMenu,/CHECKED_MENU, $
                                       VALUE='Bad Pixel (This Record)')
     self.wCBut_bcds_mark=widget_button(self.wMenu,/CHECKED_MENU, $
                                      VALUE='Bad Pixel (These Records)')

     
     widget_control, self.wBase, SET_UVALUE=self,/REALIZE
     make_widget_adjacent,self.wBase,self.parent
  
     ;; Save geometry for re-sizing
     geom=widget_info(self.wList,/GEOMETRY)
     self.list_size_diff=geom.SCR_YSIZE- $
                         (widget_info(self.wBase,/GEOMETRY)).SCR_YSIZE
     XManager,title, self.wBase,/NO_BLOCK, $
              EVENT_HANDLER='CubeBackTrack_event',CLEANUP='CubeBackTrack_kill'
  endif 
  self.oDraw->MsgSignup,self,DRAW_MOTION=~self.lock,/DRAW_BUTTON
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
;  Cursor
;=============================================================================
function CubeBackTrack::Cursor,mask,offset
  mask=[4096U,4096U,4096U,4096U,61185U,4096U,4096U,4096U, $
        4348U,255U,255U,252U,252U,255U,255U,252U]
  offset=[4,11]
  return,[4096U,4096U,0U,0U,33537U,0U,0U,4096U, $
          4096U,255U,0U,252U,0U,255U,0U,252U]
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
  return,['Lock Pixel','','Unlock Pixel']
end

;=============================================================================
;  ReportWidget - Where to position our error and other messages
;=============================================================================
function CubeBackTrack::ReportWidget
  return,self.parent
end
;;*************************End OverRiding methods******************************

pro CubeBackTrack::DrawMark,ERASE=erase
  pt=self.oDraw->Convert(self.mark,/DEVICE,/SHOWING)
  if pt[0] eq -1 then return
  self.oDraw->GetProperty,ZOOM=zoom
  if keyword_set(erase) then self.oDraw->Erase,pt-zoom/2-1,[zoom,zoom]+2 $
  else plots,pt[0],pt[1],/DEVICE,COLOR=self.color,PSYM=7,SYMSIZE=zoom/7, $
             THICK=zoom ge 4?2.:1.
end

pro CubeBackTrack_kill,id
  widget_control, id,get_uvalue=self
  if obj_valid(self) then self->Reset
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
        ;; Workaround v6.1 resize offset bug:
        widget_control, ev.top, TLB_GET_OFFSET = offset
        widget_control, ev.top, TLB_SET_XOFFSET = offset[0], $
                        TLB_SET_YOFFSET = offset[1]
     end 
     
     'WIDGET_CONTEXT': begin 
        if n_elements(*self.list) eq 0 then return
        item=widget_info(self.wList,/LIST_SELECT)
        if item lt 0 then return
        item=(*self.list)[item]
        pix=item.bcd_pix
        widget_control, self.wCBut_global_mark, SET_BUTTON=item.bad AND 1b
        widget_control, self.wCBut_bcd_mark, SET_BUTTON=(item.bad AND 2b) ne 0b
        got=where((*self.list).bcd_pix eq item.bcd_pix)
        widget_control, self.wCBut_bcds_mark, $
                        SET_BUTTON=array_equal((*self.list)[got].bad AND 2b,2b)
        widget_displaycontextmenu,ev.id,ev.x,ev.y,self.wMenu
     end
     
     'WIDGET_BUTTON': begin     ;context menu buttons clicked
        item=widget_info(self.wList,/LIST_SELECT)
        if item lt 0 then return
        item=(*self.list)[item] ;the actual backtrack record
        case ev.id of
           self.wCBut_global_mark: $
              self.cube->ToggleBadPixel,item.bcd_pix,/UPDATE
           self.wCBut_bcd_mark: $ 
              self.cube->ToggleBadPixel,item.bcd_pix,/UPDATE, $
                                        RECORD_SET=item.dceid
           self.wCBut_bcds_mark: begin 
              got=where((*self.list).bcd_pix eq item.bcd_pix)
              self.cube->ToggleBadPixel,item.bcd_pix,/UPDATE, $
                                        RECORD_SET=(*self.list)[got].dceid, $
                                        SET=~(item.bad AND 2b) 
           end
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
  if ~widget_info(self.wLabel,/VALID_ID) then return
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
     
  list=self.cube->BackTrackPix(self.point,self.plane,/FOLLOW,ERROR=err)
  if keyword_set(err) then begin 
     self->Reset
     return
  endif 
  oldid=''
  pm=string(177b)
  if size(list,/N_DIMENSIONS) eq 0 then str='' else begin 
     str=strarr(n_elements(list))
     for i=0,n_elements(list)-1 do begin 
        str[i]=string(FORMAT='(" (",I3,",",I3,") ",' + $
                      'G9.3,1X,3(G9.3,A0,G-9.3,1X),A)', $
                      list[i].BCD_PIX mod self.bcd_size[0], $
                      list[i].BCD_PIX/self.bcd_size[0], $
                      list[i].AREA,list[i].BCD_VAL,pm,list[i].BCD_UNC, $
                      list[i].BACK_VAL,pm,list[i].BACK_UNC, $
                      list[i].BCD_VAL-list[i].BACK_VAL, pm, $
                      sqrt(list[i].BCD_UNC^2+list[i].BACK_UNC^2), $
                      list[i].FLAGS)
        if list[i].ID eq oldid then begin 
           str[i]=string(FORMAT='(23X,A)',str[i])
        endif else begin 
           str[i]=string(FORMAT='(A21,2X,A)',list[i].ID, $
                         str[i])
           oldid=list[i].ID
        endelse 
     endfor
  endelse 
  top=widget_info(self.wList,/LIST_TOP)
  select=widget_info(self.wList,/LIST_SELECT)
  widget_control, self.wList,SET_VALUE=str
  widget_control, self.wList,SET_LIST_TOP=top,SET_LIST_SELECT=select
  *self.list=temporary(list)
end

;=============================================================================
;  EnsureCube - Make sure the cube we have is still valid
;=============================================================================
pro CubeBackTrack::EnsureCube
  if ~obj_valid(self.cube) then begin 
     self->Reset
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
function CubeBackTrack::Init,oDraw,parent,COLOR=color,_EXTRA=e
  if (self->tvPlug::Init(oDraw,_EXTRA=e) ne 1) then return,0 
  self.parent=parent
  self.point=[-1,-1]
  self.list=ptr_new(/ALLOCATE_HEAP)
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
      wCBut_global_mark: 0L, $  ;mark button (global)
      wCBut_bcd_mark: 0L, $     ;mark button (bcd)
      wCBut_bcds_mark: 0L}      ;mark button (these bcds)
end
