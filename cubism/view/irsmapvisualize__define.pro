;+
; NAME:  
;
;    IRSMapVisualize
;
; CONTACT:
;
;    UPDATED VERSIONS of CUBISM and more information can be found at:
;       http://sings.stsci.edu/cubism
;
; DESCRIPTION:
;    
;    Visualize IRS spectral mapping AORs on any WCS-enabled image.
;    
; CATEGORY:
;
;    CUBISM Spectral Reduction, Analysis and Processing.
;    AOR Visualization
;    	
; METHODS:
;
;    Init:  
;
;       CALLING SEQUENCE:
;
;          obj=obj_new('IRSMapVisualize',oDraw,parent,COLOR=)
;
;       INPUT PARAMETERS:
;
;          oDraw: The tvDraw object.
;
;          parent: The widget ID of the parent to place the selection
;             status label into.
;
;
;       INPUT KEYWORD PARAMETERS:
;
;          COLOR: A 4-element array, with color IDs to use for
;             normal,selected,disabled,select/disables records.
; NOTES:
;  
;    Supports selection of records on the image.  Click and drag to
;    select ranges, Control-click-drag to extend ranges.  Shift click
;    to extend selection serially from last selected.
;
; INHERITANCE TREE:
;
;    ObjMsg-->tvPlug-->IRSMapVisualize
;
; MODIFICATION HISTORY:
;    
;    2005-06-26 (J.D. Smith): Written
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2005, 2006 J.D. Smith
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
;  On - (Extraction Tool)
;=============================================================================
pro IRSMapVisualize::On
  if self->On() then begin 
     self->Off,/RESET           ;if turned on *again* 
     return
  endif 
  self->tvPlug::On
  self->UpdateMapRecords
  self.oDraw->MsgSignup,self,/DRAW_BUTTON,/TVDRAW_SNAPSHOT
  if obj_valid(self.cube) then $
     self.cube->MsgSignup,self,/CUBEPROJ_SELECT,/CUBEPROJ_RECORD_UPDATE
  if self.oDraw->IsDrawn() then self.oDraw->ReDraw,/SNAPSHOT
end

;=============================================================================
;  Off
;=============================================================================
pro IRSMapVisualize::Off,NO_REDRAW=nrd,RESET=reset,_EXTRA=e
  was_on=self->On() 
  self.oDraw->MsgSignup,self,/NONE, $
                        TVDRAW_SNAPSHOT=(was_on AND ~keyword_set(reset))
  if keyword_set(reset) && obj_valid(self.cube) then $
     self.cube->MsgSignup,self,/NONE ;we still get updates from the cuberec
  self->tvPlug::Off,_EXTRA=e
  if was_on && ~keyword_set(nrd) then self.oDraw->ReDraw,/SNAPSHOT
end
 
;=============================================================================
;  Icon
;=============================================================================
function IRSMapVisualize::Icon
  return,[[255B, 255B],[057B, 128B],[255B, 255B],[024B, 000B],$
          [255B, 255B],[241B, 129B],[255B, 255B],[224B, 007B],$
          [255B, 255B],[193B, 143B],[255B, 255B],[000B, 014B],$
          [255B, 255B],[001B, 152B],[255B, 255B],[000B, 014B]]
end

;=============================================================================
;  Cursor
;=============================================================================
function IRSMapVisualize::Cursor,mask,offset
  mask=[4096U,4096U,4096U,4096U,61185U,4096U,4120U,4108U, $
        4102U,26U,124U,120U,224U,192U,96U,48U]
  offset=[4,11]
  return,[4096U,4096U,0U,0U,33537U,0U,8U,4100U, $
          4098U,2U,12U,56U,96U,64U,32U,16U]
end

;=============================================================================
;  Description
;=============================================================================
function IRSMapVisualize::Description
  return,'Visualize Map AORs'
end


;=============================================================================
;  MouseHelp
;=============================================================================
function IRSMapVisualize::MouseHelp
  return,['Select BCD','','']
end

;=============================================================================
;  Message - Draw, CubeRec and CUBE
;=============================================================================
pro IRSMapVisualize::Message, msg
  self->tvPlug::Message,msg,TYPE=type
  case type of
     'DRAW_MOTION': begin 
        ;;we must be dragging: always append
        got=self->NearestRecord(msg.X,msg.Y)
        if got eq -1 then return ; no penalty for going off
        ;; If any were skipped, include them as well (up to a few)
        self->Select,got,/APPEND,RANGE=abs(self.last-got) le 5,/NO_UPDATE
     end
        
     'DRAW_BUTTON': begin 
        case msg.type of 
           0: begin             ;button press
              self.drawing=1b
              got=self->NearestRecord(msg.X,msg.Y)
              if got eq -1 then self.last=-1
              self.oDraw->MsgSignup,self,/DRAW_MOTION
              ;;Append with control key, range with Shift key
              self->Select,got,APPEND=logical_true(msg.modifiers AND 2b), $
                           RANGE=msg.modifiers AND 1b,/NO_DRAW,/NO_UPDATE
              self->DrawAllRecords,/DOUBLE_BUFFER
           end
           1: begin
              self.drawing=0b
              self.oDraw->MsgSignup,self,DRAW_MOTION=0 ;button release
           end 
        endcase 
        
     end 
     
     'TVDRAW_SNAPSHOT': self->DrawAllRecords ;already buffered for us
     
     'CUBEREC_UPDATE': begin 
        if ~msg.visualize_mode then begin 
           self->Off,/DISABLE   ;no longer relevant
           return
        endif else self->Enable
        ;; Cube or visualize mode
        if ptr_valid(msg.astrometry) then begin 
           self.astrometry=ptr_new(*msg.astrometry)
           self->UpdateMapRecords
        endif 
        if self.cube ne msg.cube then begin 
           self.cube=msg.cube
           if self->On() then $
              self.cube->MsgSignup,self,/CUBEPROJ_SELECT, $
                                   /CUBEPROJ_RECORD_UPDATE
           self->UpdateMapRecords
        endif else if msg.calib_update then begin 
           self->UpdateMapRecords
           self.oDraw->ReDraw,/SNAPSHOT
        endif 
     end
     
     'CUBEPROJ_SELECT': begin 
        if msg.single_select ne -1 then begin 
           self.oDraw->SetWin
           self->Select,msg.single_select,/DCEID,/NO_SET
        endif else begin        ; just redraw everything
           self->UpdateMapStatus
           self->DrawAllRecords,/DOUBLE_BUFFER
        endelse 
     end 
     
     'CUBEPROJ_RECORD_UPDATE': begin 
        if msg.deleted then begin 
           self->UpdateMapRecords
           self.oDraw->ReDraw,/SNAPSHOT 
        end else begin 
           if msg.disabled then self->UpdateMapStatus,/CHECK_DISABLED $
           else self->UpdateMapRecords 
           self->DrawAllRecords,/DOUBLE_BUFFER
        endelse 
     end 
  endcase 
end

;;*************************End OverRiding methods******************************

; pro IRSMapVisualize_Event,ev
;   widget_control, ev.id, GET_UVALUE=self
;   call_method,'Event',self,ev
; end

; pro IRSMapVisualize::Event,ev
;   if self.oDraw->IsDrawn() then self.oDraw->ReDraw,/SNAPSHOT
;end

;=============================================================================
;  Select - Select a Record
;=============================================================================
pro IRSMapVisualize::Select,which,APPEND=app,RANGE=range,NO_DRAW=nd, $
                            DCEID=dceid,_EXTRA=e
  if ~ptr_valid(self.recs) || ~obj_valid(self.cube) then return
  if ~keyword_set(app) then begin 
     selected=where((*self.recs).SELECTED,scnt)
     if scnt gt 0 then begin 
        (*self.recs).SELECTED=0b
        for i=0,scnt-1 do self->DrawOneRecord,selected[i]
     endif 
  endif 
  if which[0] eq -1 then return
  if keyword_set(range) && self.last ge 0L then begin
     low=which<self.last
     high=which>self.last
     (*self.recs)[low:high].SELECTED=1b
     if ~keyword_set(nd) then for i=low,high do self->DrawOneRecord,i
  endif else begin 
     if keyword_set(dceid) then begin 
        which=where_array([which],(*self.recs).DCEID,cnt)
        if cnt eq 0 then return
     endif 
     (*self.recs)[which].SELECTED=1b
     if ~keyword_set(nd) then self->DrawOneRecord,which
  endelse 
  ;; Sync up cube to our list selection
  self.last=which
  self->UpdateCubeSelect,_EXTRA=e
end

;=============================================================================
;  UpdateCubeSelect - Update the cube and our select status
;=============================================================================
pro IRSMapVisualize::UpdateCubeSelect,NO_SET=ns,_EXTRA=e
  if ~keyword_set(ns) then begin 
     recs=where((*self.recs).SELECTED,cnt)
     if cnt gt 0 then recs=(*self.recs)[recs].dceid
     self.cube->SetListSelect,recs,/DCEID,_EXTRA=e
  endif 
  self->UpdateSelectStatus
end

;=============================================================================
;  UpdateMapRecords - Get all BCD bound data
;=============================================================================
pro IRSMapVisualize::UpdateMapRecords
  if ~obj_valid(self.cube) || ~ptr_valid(self.astrometry) then return
  bounds=self.cube->BCDBounds(/ALL) ;ra,dec
  self.cube->GetProperty,/ALL_RECORDS,DCEID=dceid,DISABLED=disabled
  ad2xy,bounds[0:3,*],bounds[4:7,*],*self.astrometry,x,y
  if n_elements(dceid) eq 0 then return
  ptr_free,self.recs
  recs=replicate({MAP_VISUALIZE_RECORD},n_elements(dceid))
  recs.bounds=[x,y]
  xmin=min(x,DIMENSION=1,MAX=xmax)
  ymin=min(y,DIMENSION=1,MAX=ymax)
  recs.dceid=dceid
  recs.disabled=disabled
  recs.xmin=xmin
  recs.ymin=ymin
  recs.xmax=xmax
  recs.ymax=ymax
  recs=recs[sort(recs.dceid)]   ;keep them sorted
  self.recs=ptr_new(recs,/NO_COPY)
  self.last=-1L
  self->UpdateMapStatus
end

;=============================================================================
;  UpdateMapStatus - Update the status of BCD selection/disabling
;=============================================================================
pro IRSMapVisualize::UpdateMapStatus, CHECK_DISABLED=cd
  if ~obj_valid(self.cube) then return  
  if ~ptr_valid(self.recs) then return
  selected_recs=self.cube->CurrentSelect(/DCEID)
  (*self.recs).SELECTED=0b
  if selected_recs[0] eq -1 then return
  
  wh=where_array([selected_recs],(*self.recs).DCEID,cnt)
  if cnt eq 0 then return
  (*self.recs)[wh].SELECTED=1b
  
  if keyword_set(cd) then begin 
     self.cube->GetProperty,RECORD_SET=(*self.recs).DCEID,DISABLED=dis
     (*self.recs).DISABLED=dis
  endif 
  self->UpdateSelectStatus
end


;=============================================================================
;  UpdateSelectStatus - Update the selection report
;=============================================================================
pro IRSMapVisualize::UpdateSelectStatus
  if ~ptr_valid(self.recs) then begin 
     widget_control, self.wSelectStatus,SET_VALUE='None Selected'
     return
  endif 
  wh=where((*self.recs).SELECTED,scnt)
  widget_control, self.wSelectStatus,SET_VALUE=strtrim(scnt,2)+' Selected'  
end

;=============================================================================
;  NearestRecord - Select the nearest containing record
;=============================================================================
function IRSMapVisualize::NearestRecord,x,y
  c=self.oDraw->Convert([x,y],/FRACTIONAL) ;real pixel coordinates
  x=c[0] & y=c[1]
  wh=where(x ge (*self.recs).XMIN AND $
           y ge (*self.recs).YMIN AND $
           x le (*self.recs).XMAX AND $
           y le (*self.recs).YMAX,ocnt)
  if ocnt eq 0 then return,-1
  
  bounds=(*self.recs)[wh].bounds
  xps=bounds[0:3,*] & yps=bounds[4:7,*]
  
  if ocnt eq 1 then begin 
     y=rebin([y],4,/SAMPLE) & x=rebin([x],4,/SAMPLE)
     yps2=shift(yps,1) & xps2=shift(xps,1)
  endif else begin 
     y=rebin([y],4,ocnt,/SAMPLE) & x=rebin([x],4,ocnt,/SAMPLE)
     yps2=shift(yps,1,0) & xps2=shift(xps,1,0)
  endelse 
  
  ;; Check for odd number of crossings 
  ;; based on Randolph Franklin's page:
  ;; http://www.ecse.rpi.edu/Homepages/wrf/research/geom/pnpoly.html
  cross=where(fix(total(( (yps le y AND y lt yps2) OR $
                          (yps2 le y AND y lt yps) ) $
                        AND $
                        (x lt ((xps2 - xps)*(y - yps)/(yps2 - yps) + xps)), $
                        1)) AND 1,cnt)
  if cnt eq 0 then return,-1 
  return,wh[cross[0]]
end


;=============================================================================
;  DrawOneRecord
;=============================================================================
pro IRSMapVisualize::DrawOneRecord,which
  a=[0,1,2,3,0]
  b=(*self.recs)[which].bounds
  col=self.color[(*self.recs)[which].SELECTED?2:(*self.recs)[which].DISABLED]
  !X.S=self.csave[0:1] & !Y.S=self.csave[2:3] & ps=!P.NOCLIP & !P.NOCLIP=1
  oplot,b[a],b[a+4],COLOR=col,THICK=2
  !P.NOCLIP=ps
end

;=============================================================================
;  DrawAllRecords - Draw all the region records
;=============================================================================
pro IRSMapVisualize::DrawAllRecords,DOUBLE_BUFFER=db
  if ~ptr_valid(self.recs) then return
  ;; Setup Plot axes
  self.oDraw->GetProperty,OFFSET=off,DISPSIZE=ds,PAN=pan,ZOOM=zm
  ;; Draw with a double buffer
  db=keyword_set(db) 
  if db then $
     self.oDraw->Erase,/DOUBLE,/FULL,/FROM_SCREEN ;be sure to get other's snaps

  plot,[0],[0],XRANGE=[off[0],off[0]+ds[0]],YRANGE=[off[1],off[1]+ds[1]], $
       XSTYLE=5,YSTYLE=5,POSITION=[pan,pan+ds*zm],/DEVICE,/NODATA,/NOERASE
  self.csave=[!X.S,!Y.S]
  nrec=n_elements(*self.recs)
  selected=where((*self.recs).SELECTED,COMPLEMENT=not_selected, $
                 NCOMPLEMENT=ncnt,scnt)
  for i=0,ncnt-1 do self->DrawOneRecord,not_selected[i]
  for i=0,scnt-1 do self->DrawOneRecord,selected[i]
  if db then self.oDraw->DBRefresh,/FULL
end

;=============================================================================
;  Cleanup
;=============================================================================
pro IRSMapVisualize::Cleanup
  ptr_free,self.recs,self.astrometry
  self->tvPlug::Cleanup
end

;=============================================================================
;  Init
;=============================================================================
function IRSMapVisualize::Init,oDraw,parent,COLOR=color
  if (self->tvPlug::Init(oDraw,_EXTRA=e) ne 1) then return,0 
  if n_elements(color) ne 0 then self.color=color
  self.wSelectStatus=widget_label(parent,VALUE='---',/DYNAMIC_RESIZE)
  self.last=-1L
  return,1
end

pro IRSMapVisualize__define
  st={IRSMapVisualize, $
      INHERITS tvPlug,$         ;it's a tvDraw plugin
      color:bytarr(3), $        ;the color: normal,selected,disabled,select/dis
      cube:obj_new(), $         ;the cube we're dealing with
      astrometry: ptr_new(), $  ;our astrometry
      recs: ptr_new(), $        ;list of BCD outline regions
      drawing: 0b, $            ;whether we're presently drawing
      last: 0L, $               ;the last selected record
      csave: dblarr(4), $       ;coordinate system save
      wSelectStatus: 0L}
      
  st={MAP_VISUALIZE_RECORD, $
      bounds:fltarr(8), $       ; x,y bounds of slitlet
      xmin:0., $                ; range of bounds coords
      xmax:0., $
      ymin:0., $
      ymax:0., $
      DCEID: 0L, $
      SELECTED: 0b, $
      DISABLED: 0b}
end
