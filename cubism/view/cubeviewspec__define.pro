;+
; NAME:  
;
;    CubeViewSpec
;
; CONTACT:
;
;    UPDATED VERSIONS of CUBISM and more information can be found at:
;       http://sings.stsci.edu/cubism
;
; DESCRIPTION:
;    
;    Interface for displaying extracted spectra, and creating maps.
;    
; CATEGORY:
;
;    CUBISM Spectral Reduction, Analysis and Processing.
;    Spectral manipulation.
;
; METHODS:
;
;    Init:  
;
;       CALLING SEQUENCE:
;
;          obj=obj_new('CubeViewSpec',[XRANGE=,YRANGE=,PARENT_GROUP=]
;
;       INPUT KEYWORD PARAMETERS:
;
;          (X|Y)RANGE: The X,Y plot range to use, defaults to
;             auto-ranging.
;
;          PARENT_GROUP: The widget ID of the group leader.
;             
;
; INHERITANCE TREE:
;
;    OMArray--+
;              \      
;       ObjMsg-->CubeViewSpec
;              /
;  ObjReport--+
;    
;
; MODIFICATION HISTORY:
;    
;    2002-12-06 (J.D. Smith): Written
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2002-2006 J.D. Smith
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
;  Message - We'll hear about new spectra from CubeRec, and switching
;            to FULL mode
;=============================================================================
pro CubeViewSpec::Message, msg
  type=tag_names(msg,/STRUCTURE_NAME)
  if type eq 'CUBEREC_FULL' then begin 
     self.wavelength=msg.wavelength      
     self->SwitchMode,/FULL
     self->Plot
  endif else begin              ; A new spectrum
     self.Info=msg.Info
     widget_control, self.wBase,TLB_SET_TITLE='CubeSpec: '+self.Info
     *self.lam=*msg.wavelength
     self.upgoing=((*self.lam)[n_elements(*self.lam)-1]-(*self.lam)[0]) gt 0.0
     *self.sp=*msg.spec
     if ptr_valid(msg.spec_unc) then *self.sp_unc=*msg.spec_unc else $
        if n_elements(*self.sp_unc) gt 0 then void=temporary(*self.sp_unc)
     widget_control, self.wToggles,GET_VALUE=ren
     if ren[1] then begin 
        med=median(*self.sp)
     endif 
     widget_control, self.wDraw,/DRAW_BUTTON_EVENTS, /DRAW_MOTION_EVENTS
     self.movestart=-1
     self->Plot
  endelse
end 

;=============================================================================
;  ReportWidget - Where to position error and other messages
;=============================================================================
function CubeViewSpec::ReportWidget
  return,self.wBase
end

;;*************************End OverRiding methods******************************
  
pro CubeViewSpec_kill,id
  widget_control, id, get_uvalue=self
  obj_destroy,self
end

pro CubeViewSpec_event, ev
  widget_control,ev.top,get_uvalue=self
  self->Event,ev
end

pro CubeViewSpec_map_event, ev
  widget_control,ev.top,get_uvalue=self
  self->MapEvent,ev
end


pro CubeViewSpec::Quit
  widget_control, self.wBase,/DESTROY
end

;=============================================================================
;  Event - Handle internal events.
;=============================================================================
pro CubeViewSpec::Event,ev
  type=tag_names(ev,/STRUCTURE_NAME)
  if type eq 'WIDGET_TRACKING' then  begin 
     if ev.enter then begin 
        self->SetColors
        self->SetWin
        widget_control, self.wDraw,/INPUT_FOCUS
     endif 
     return
  endif else if type eq 'WIDGET_BASE' then begin 
     ev.X>=self.xsize
     new=[ev.X,ev.Y]+self.draw_size_diff
     widget_control, self.wDraw, SCR_XSIZE=new[0],SCR_YSIZE=new[1]
     ;; Workaround v6.1 resize offset bug:
     widget_control, ev.top, TLB_GET_OFFSET = offset
     widget_control, ev.top,TLB_SET_XOFFSET = offset[0], $
                     TLB_SET_YOFFSET = offset[1]
     wdelete,self.pixwin
     window,/FREE,/PIXMAP,XSIZE=new[0],YSIZE=new[1] ;to cache the image in
     self.pixwin=!D.WINDOW
     self->Plot
     return
  endif 
  
  catch,err
  if err ne 0b then begin
     catch,/cancel
     self->Error,!ERROR_STATE.MSG
  endif 
  
  case ev.id of 
     self.wDraw: begin          ;press and motion events
        if ev.type lt 5 then $
           c=(convert_coord(ev.X,ev.Y,/DEVICE,/TO_DATA))[0:1]
        case ev.type of
           5b: begin ;; ASCII keys
              if ev.release then return ;just press events
              if ev.ch eq 8 then begin ; Delete Key
                 self->Delete 
                 return
              endif 
              case strupcase(ev.ch) of 
                 'C': begin
                    self->SwitchMode,/MAP
                    widget_control, self.wDo, SET_VALUE=2
                    widget_control, self.wRType, SET_DROPLIST_SELECT=0, $
                                    /SENSITIVE
                 end
                 'U': self->SwitchMode,/FULL
                 'P': begin
                    self->SwitchMode,/MAP
                    widget_control, self.wDo, SET_VALUE=2
                    widget_control, self.wRType, SET_DROPLIST_SELECT=1, $
                                    /SENSITIVE
                 end
                 'G': begin 
                    widget_control, self.wToggles,GET_VALUE=tog
                    tog[0]=1-tog[0]
                    self.log_scale=tog[0]
                    widget_control, self.wToggles,SET_VALUE=tog
                    self->Plot
                 end 
                 'R': self->Reset
                 'L': if self.mode eq 0 then widget_control, self.wDo, $
                                                             SET_VALUE=2
                 'D': self->Delete
                 'M': self->SwitchMode,/MAP
                 'F': self->Fit
                 'V': begin     ;value line
                    widget_control, self.wToggles, GET_VALUE=tog
                    tog[1]=1-tog[1]
                    widget_control, self.wToggles, SET_VALUE=tog
                    self->Plot
                 end
                 'X': begin     ;XZoom
                    widget_control, self.wDo, SET_VALUE=0
                    widget_control, self.wRType, SENSITIVE=0
                 end
                 'Y': begin     ;YZoom
                    widget_control, self.wDo, SET_VALUE=1
                    widget_control, self.wRType, SENSITIVE=0
                 end
                 'Q': self->Quit
                 
                 ' ':  begin    ;Space, switch outlines...
                    if self.selected eq -1 then return
                    if NOT ptr_valid(self.reg[self.seltype]) then return
                    nreg=n_elements(*self.reg[self.seltype])/2
                    self.selected=self.selected+1
                    if self.selected ge nreg then begin 
                       self.selected=0
                       if ptr_valid(self.reg[1-self.seltype]) then $
                          self.seltype=1-self.seltype
                    endif 
                    self->Plot
                 end
                 else:          ;print,"Got: *"+strupcase(ev.ch[0])+"*"
              endcase 
           end
           
           6b: begin ;; Special keys
              if ev.release then return ; Just press events
              if ev.key lt 5 or ev.key gt 8 then return ; Only Arrow Keys
              del=(ev.modifiers AND 1)?((ev.modifiers AND 2) ne 0?10:5):1
              ;; Clear out keyboard events, since they can accumulate
              widget_control, self.wDraw, /CLEAR_EVENTS
              if self.mode eq 0 then begin ;; Change wavelengths
                 self.wav_ind=0>$
                              (self.wav_ind+ $
                               (ev.key eq 5 or ev.key eq 7?-del:del))<$
                              (n_elements(*self.lam)-1)
                 self.wavelength=(*self.lam)[self.wav_ind]
                 if self.map_name then self->ResetMap
                 self->Plot
                 self->Send
                 return
              endif 
              ;; Moving a selected region
              if self.selected eq -1 then return
              if NOT ptr_valid(self.reg[self.seltype]) then return
              range=(*self.reg[self.seltype])[*,self.selected]
              case ev.key of
                 7: begin       ;up
                    range[0]=range[0]-del & range[1]=range[1]+del
                 end
                 5: range=range+(self.upgoing?-del:del) ;left
                 6: range=range+(self.upgoing?del:-del) ;right
                 8: begin       ;down
                    range[0]=range[0]+del & range[1]=range[1]-del
                 end
              endcase
              nl=n_elements(*self.lam) 
              del=range[1]-range[0]
              range[0]= (nl-1-del) < range[0] > 0
              range[1]=del > range[1] < (nl-1)
              if range[1]-range[0] lt 0 then self->Delete else $
                 (*self.reg[self.seltype])[*,self.selected]=range
              self->MergeRegs
              self->Plot
              if n_elements(*self.fit) ne 0 then self->ResetFit
              if self.map_name then self->ResetMap
              self->Send
           end 
              
           0b: case ev.press of ;press events
              1b: begin         ;Left button
                 widget_control, self.wDo, GET_VALUE=doing
                 if self.got eq -1 then begin ;first in pair
                    if self.mode eq 0 AND doing eq 2 then begin ;full-WL
                       ;; just pick the wavelength
                       new_wav=(*self.lam)[self.movestart]
                       if self.wavelength ne new_wav then begin 
                          self.wav_ind=self.movestart
                          self.wavelength=new_wav
                          self->Plot ;show the selected WL
                          self->Send
                       endif 
                    endif else begin 
                       self.pressloc=c
                       self.got=doing
                       self->ShowRegionLine
                    endelse 
                 endif else begin ;second press in a defining pair
                    self.got=-1 ;got them both now, yeah
                    case doing of 
                       2: begin ;region
                          sel=widget_info(self.wRtype,/DROPLIST_SELECT) 
                          m=min(abs(*self.lam-c[0]),ind1)
                          m=min(abs(*self.lam-self.pressloc[0]),ind2)
                          if ind1 eq ind2 then return
                          r=[ind1<ind2,ind1>ind2]
                          if ~array_equal(r ne -1,1b) then return
                          ;; append the new region
                          if ptr_valid(self.reg[sel]) then $
                             *self.reg[sel]=[[*self.reg[sel]],[r]] $
                          else $
                             self.reg[sel]=ptr_new(reform(r,2,1))
                          
                          self.selected=n_elements(*self.reg[sel])/2-1
                          self.seltype=sel
                          self->MergeRegs
                          ;; XXX regs and weights presently incompatible
                          ptr_free,self.weights 
                          self.map_name=''
                          if ptr_valid(self.wMapSets) then $
                             for i=0,n_elements(*self.wMapSets)-1 do $
                                widget_control, (*self.wMapSets)[i], $
                                                SET_BUTTON=0
                          self->UpdateButtons
                       end
                       
                       0:  begin ;XClip
                          self.xr=[self.pressloc[0]<c[0], $
                                   self.pressloc[0]>c[0]]
                       end
                       
                       1: begin ;YClip
                          self.yr=[self.pressloc[1]<c[1], $
                                   self.pressloc[1]>c[1]]
                       end
                    endcase 
                    self->Plot
                    if doing eq 2 then self->Send ;new region stack
                 endelse 
              end
              
              2b: begin         ;Middle Button -- drag/select regions
                 self->FindReg,value_locate(*self.lam,c[0]),FOUND=f
                 if self.selected eq -1 or f eq 0 then return
                 self.press=1b
                 self.movestart=value_locate(*self.lam,c[0])
              end
              
              
              4b: begin         ;Right Button - Reset (no nuking regions)
                 if self.got ge 0 then begin ;if selecting range, cancel
                    self.got=-1 
                    self->Plot  
                    if self.map_name then self->ResetMap
                 endif else self->Reset,/KEEP
              end
              else:
           endcase 
           
           1b: self.press=0b    ;release
           
           2b: begin            ;motion
              m=min(abs(*self.lam-c[0]),ind)
              ;ind=value_locate(*self.lam,c[0])
              nl=n_elements(*self.lam)
              if ind lt 0 or ind ge nl then return
              moved=ind-self.movestart
              if moved eq 0 then return
              self.movestart=ind ;relative to here
              if n_elements(*self.sp_unc) ne 0 then begin 
                 widget_control, self.wLine,SET_VALUE= $
                                 string(FORMAT='("[",F7.3,":",G9.4,' + $
                                        'A0,G-0.4,"]")', $
                                        (*self.lam)[ind],(*self.sp)[ind], $
                                        string(177b),(*self.sp_unc)[ind])
              endif else $
                 widget_control, self.wLine,SET_VALUE= $
                                 string(FORMAT='("[",F7.3,":",G9.4,"]")', $
                                        (*self.lam)[ind],(*self.sp)[ind])
              ;; Maybe Moving a region
              if self.press then begin 
                 range=(*self.reg[self.seltype])[*,self.selected]
                 delta=range[1]-range[0]
                 range=range+moved
                 range[0]=range[0] > 0 < (nl-1-delta)
                 range[1]=range[1] < (nl-1) > delta
                 (*self.reg[self.seltype])[*,self.selected]=range
                 self->MergeRegs
                 if n_elements(*self.fit) ne 0 then self->ResetFit
                 if self.map_name then self->ResetMap
                 self->Plot
                 self->Send
              endif else if self->ShowingValueLine() then self->Plot
           end 
        endcase 
        return
     end
     
     self.wQuit: begin 
        widget_control, ev.top,/DESTROY
        return
     end 
     
     self.wSaveBut: self->MsgSend,{CUBEVIEWSPEC_SAVE,0}
     self.wExportBut: self->MsgSend,{CUBEVIEWSPEC_SAVE,1}
     
     self.wToggles: begin 
        widget_control, ev.id,GET_VALUE=tog
        self.log_scale=tog[0]
        self.show_error=tog[2]
        self->Plot
     end 
     
     self.wFit: $
        case ev.value of 
        2: self->Reset
        1: self->Reset,/KEEP
        0: self->Fit
     endcase 
     
     self.wDo: begin 
        if self.mode eq 0 then return
        if ev.value ne 2 then widget_control, self.wRType, SENSITIVE=0 else $
           widget_control, self.wRType, SENSITIVE=1
     end
     
     self.wWavWeightInt: begin 
        if ev.value eq self.wWavWeightInt_ids[0] then begin 
           self.weight_cont=widget_info(ev.value,/BUTTON_SET)
        endif else begin                       ;; Integrate
           self.integrate=widget_info(ev.value,/BUTTON_SET) 
        endelse 
        if self.mode eq 0 then return ; nothing to update for full mode
        self->Send
     end 
     
     self.wMode: self->SwitchMode,ev.value
     else: return               ; Just ignore any others
  endcase
  widget_control, self.wDraw,/INPUT_FOCUS
end

;=============================================================================
;  Map Event - Handle map selection events.
;=============================================================================
pro CubeViewSpec::MapEvent,ev
  widget_control, ev.id,get_uvalue=uval
  widget_control, self.wDraw,/INPUT_FOCUS
  
  if n_elements(uval) eq 0 then begin 
     ;; A specific map choice selected
     self->SwitchMode,/MAP
     if ptr_valid(self.wMapSets) then $
        for i=0,n_elements(*self.wMapSets)-1 do $
           widget_control, (*self.wMapSets)[i], $
                           SET_BUTTON=(*self.wMapSets)[i] eq ev.id
     widget_control, ev.id,get_value=name
     self->ApplyMap,name
     return
  endif 
  
  oMap=IRSMapSet(self)

  ;; Load & Save
  case uval of 
     'save': begin              ;save the current
        if NOT (ptr_valid(self.reg[1]) OR ptr_valid(self.weights)) then $
           self->Error,'No valid region specified.'
        replid=0L
        if ptr_valid(self.wMapSets) then begin 
           for i=0,n_elements(*self.wMapSets)-1 do begin 
              if widget_info((*self.wMapSets)[i],/BUTTON_SET) then begin 
                 replid=(*self.wMapSets)[i]
                 break
              endif 
           endfor 
        endif 
        if widget_info(replid,/VALID_ID) then $
           widget_control, replid, get_value=name
        oMap->SaveMap,name,CANCELED=cncld,WEIGHTS=self.weights, $
                      FORERANGES=self.reg[1], $
                      BACKRANGES=self.reg[0],WAVELENGTH_CONVERT=*self.lam
        if cncld then return
        if name then begin 
           self.map_name=name   ;it's now this name
           self->Send
        endif 
     end
     'load': oMap->LoadSets
     'reset': oMap->LoadDefaultSets
     'redshift': begin
        redshift=getinp('Redshift (cz -- km/s):', $
                        string(FORMAT='(F0.1)',self.redshift), $
                        TITLE='Set Redshift', $
                        PARENT_GROUP=self.wBase, /MODAL)
        if redshift then begin 
           redshift=float(redshift)
           if redshift ne self.redshift then begin 
              self.redshift=redshift
              self->ApplyMap
           endif 
        endif
     end 
     'clear-redshift': begin 
        self.redshift=0.0
        self->ApplyMap
     end 
  endcase 

  ;; Rebuild the menu
  if ptr_valid(self.wMapSets) then begin 
     menu=widget_info((*self.wMapSets)[0],/PARENT)
     for i=0,n_elements(*self.wMapSets)-1 do begin 
        if n_elements(name) eq 0 then $
           if widget_info((*self.wMapSets)[i],/BUTTON_SET) then $
              widget_control, (*self.wMapSets)[i],GET_VALUE=name
        widget_control, (*self.wMapSets)[i],/DESTROY
     endfor 
  endif 
  self->BuildMapMenu,menu,ACTIVATE=name
end

;=============================================================================
;  BuildMapMenu - Build the menu with all current maps
;=============================================================================
pro CubeViewSpec::BuildMapMenu,menu,ACTIVATE=act
  oMap=IRSMapSet(self)
  ptr_free,self.wMapSets
  names=oMap->Names()
  if size(names,/TYPE) ne 7 then return
  nn=n_elements(names) 
  if nn gt 0 then begin 
     buts=lonarr(nn)
     for i=0,nn-1 do begin 
        buts[i]=widget_button(menu,value=names[i],/CHECKED_MENU, $
                              EVENT_PRO='CubeViewSpec_map_event', $
                              SEPARATOR=i eq 0)
        if n_elements(act) ne 0 then $
           if strlowcase(act) eq strlowcase(names[i]) then $
           widget_control, buts[i], /SET_BUTTON
     endfor 
     self.wMapSets=ptr_new(buts,/NO_COPY)
  endif
end


;=============================================================================
;  ApplyMap - Reset the map set
;=============================================================================
pro CubeViewSpec::ApplyMap,map_name
  if n_elements(map_name) eq 0 then map_name=self.map_name
  if ~map_name then return
  
  oMap=IRSMapSet(self)

  oMap->GetMap,map_name,BACKRANGES=br,FORERANGES=fr,WEIGHTS=weights, $
               WAVELENGTH_CONVERT=*self.lam,/NO_WEIGHT_CONVERT, $
               REDSHIFT=self.redshift
  ptr_free,self.reg,self.weights
  if n_elements(br)/2 gt 0 then self.reg[0]=ptr_new(br,/NO_COPY)
  if n_elements(fr)/2 gt 0 then self.reg[1]=ptr_new(fr,/NO_COPY)
  if n_elements(weights)/2 gt 0 then $
     self.weights=ptr_new(weights,/NO_COPY)
  self.selected=-1
  self.map_name=map_name
  self->Plot
  self->UpdateButtons
  self->Send
end 


;=============================================================================
;  ResetMap - Reset the map set
;=============================================================================
pro CubeViewSpec::ResetMap
  self.map_name=""
  if ptr_valid(self.wMapSets) then $
     for i=0,n_elements(*self.wMapSets)-1  do $
        widget_control, (*self.wMapSets)[i],SET_BUTTON=0
end

;=============================================================================
;  Send - Send the currently selected set of regions as a stack, or
;         just the wavelength if in full mode.
;=============================================================================
pro CubeViewSpec::Send,MAP_NAME=mn,JUST_SEND=js
  if n_elements(mn) eq 0 then if self.map_name then mn=self.map_name
  if self.mode eq 0L then begin ;full mode
     if self.wavelength eq 0.0 then return
     msg={CUBEVIEWSPEC_FULL,self.wavelength}
  endif else begin ;; map mode
     msg={CUBEVIEWSPEC_STACK}
     
     msg.weight_cont=self.weight_cont
     msg.integrate=self.integrate
     
     if n_elements(mn) ne 0 then begin ;named set, just pass it along
        msg.name=mn
        msg.info=string(FORMAT='(%"Map Set: %s")',mn)
     endif else begin 
        if NOT ptr_valid(self.reg[1]) then return ;need a peak region
        lams=(*self.lam)[*self.reg[1]]
        s=sort(lams[0,*]) & lams=lams[*,s]
        if lams[0,0] gt lams[1,0] then lams=lams[[1,0],*]
        msg.info="Stack: "+strjoin(string(FORMAT='(F5.2,"-",F5.2)',lams),", ")
        msg.foreground=self.reg[1]
        msg.weights=self.weights
     endelse 
     
     if self.integrate then $
        msg.info+=ptr_valid(self.weights)?', Int-weight':', Integ'
     
     if ptr_valid(self.reg[0]) then begin 
        msg.info+=' (Cont'
        ;; Send either the background regions themselves or map name
        if self.weight_cont then msg.info+=' -- Lam-Weight'
     endif 
        
     if n_elements(mn) eq 0 then msg.background=self.reg[0]
     
     ;;if ptr_valid(self.weights) then msg.info=msg.info+', Wts.'
     
     if ptr_valid(self.reg[0]) then msg.info+=')'
     if n_elements(mn) ne 0 && self.redshift then $
        msg.info+=string(FORMAT='(" [cz=",F0.1,"km/s]")',self.redshift)
  endelse 
  
  
  self->MsgSend,msg
  if keyword_set(js) eq 0 then self->SetWin ;in case it was taken away
end

;=============================================================================
;  SwitchMode - Switch between full(0) and map(1) mode, and tell
;               subscribers about it.  Use FULL or MAP mode if these
;               keywords are set.
;=============================================================================
pro CubeViewSpec::SwitchMode,mode,FULL=full,MAP=map
  if keyword_set(full) then mode=0
  if keyword_set(map) then mode=1
  if n_elements(mode) ne 0 then begin 
     if mode eq self.mode then return
     self.mode=mode
  endif else self.mode=1-self.mode
  widget_control, self.wMode,SET_VALUE=self.mode ;de-sen in full mode
  self->UpdateButtons
  self->Plot
  self->Send
end


;=============================================================================
;  UpdateButtons - Update all buttons to reflect current modes
;=============================================================================
pro CubeViewSpec::UpdateButtons
  for i=0,n_elements(self.wGray)-1  do $
     widget_control, self.wGray[i],SENSITIVE=self.mode
  widget_control, self.wReg_or_Wav,SET_VALUE=(['Lambda','Region:'])[self.mode]
  widget_control, self.wWavWeightInt_ids[0], $
                  SENSITIVE=n_elements(*self.fit) eq 0 && $
                  ptr_valid(self.reg[0]) && ptr_valid(self.reg[1])
  widget_control, self.wWavWeightInt_ids[1], $
                  SENSITIVE=ptr_valid(self.reg[1]) || ptr_valid(self.weights) 
end

;=============================================================================
;  Fit - Fit the current continuum with a polynomial of specified
;        order.
;=============================================================================
pro CubeViewSpec::Fit
  if self.mode eq 0 then return ;full mode, no fitting
  self->MergeRegs               ;just to make sure
  if NOT ptr_valid(self.reg[0]) then return
  
  ;;collect the continuum bits
  whcont=self->RegionIndices(*self.reg[0])
  
  ;; Get the fit order
  d=widget_info(self.wOrder,/DROPLIST_SELECT) 
  
  ;; Do the fit -- Wants number of terms (not order)...2 minimum (a line)
  *self.fit=svdfit((*self.lam)[whcont],(*self.sp)[whcont],d+2) 
  if ptr_valid(self.reg[1]) then begin 
     ;;collect the peak bits (we allow overlap in continuum and peak)
     whpeak=self->RegionIndices(*self.reg[1])
     
     ;; Average continuum there
     cont_under=poly((*self.lam)[whpeak],*self.fit)
     self.avgcont=mean(cont_under)
     
     ;; Calculate cumulatives
     pcnt=n_elements(whpeak)
     if pcnt le 3 then self->Error,'Not enough points in peak.'
     
     ;; The line strength
     self.strength=int_tabulated(/SORT,(*self.lam)[whpeak], $
                            (*self.sp)[whpeak]-cont_under)
     
     ;; find location where trapezoidal flux integral is half the
     ;; cumulative total
     spav=(*self.sp)[whpeak+1]+(*self.sp)[whpeak[0:pcnt-2]]
     contav=shift(cont_under,-1)+(cont_under)[0:pcnt-2]
     spav=((spav-contav)/2.)>0.0
     dellam=(*self.lam)[whpeak+1]-(*self.lam)[whpeak[0:pcnt-2]]
     
     ;; The wavelength we consider are on the right edge of the regions
     ;; over which we are integrating.
     ;; We are in effect solving for x in the limits of the integral:
     ;;    Int(sp-cont,lam_peak_0,x)==.5*(Int(sp-cont,lam_peak_0,lam_peak_1)
     lamcen=(*self.lam)[whpeak[0:pcnt-2]+1] ;+dellam;/2.
;      for i=0,n_elements(lamcen)-1 do oplot,[lamcen[i],lamcen[i]],!Y.CRANGE
     cum=total(/CUMULATIVE,(spav*dellam))
;      cpr,lamcen,spav,dellam,cum
     self.medlam=(interpol(lamcen, cum, cum[pcnt-2]/2., $
                           /LSQUADRATIC))[0]
     self.medpeak=interpol((*self.sp)[whpeak[0:pcnt-2]], $
                           (*self.lam)[whpeak[0:pcnt-2]], $
                           self.medlam, /LSQUADRATIC)
     
     ;; Find max and position
     self.max=max((*self.sp)[whpeak],mpos)
     self.maxlam=(*self.lam)[whpeak[mpos]]
     
     ;; Solve for the E.W. in:
     ;;   integral(f(lam) dlam,lamcen-ew/2,lamcen+ew/2)=f_strength
     ;; for parameter "ew".  Note that the fit background is:
     ;;   f(lam)=f_0+f_1*lam+f_2*lam^2+...+f_order*lam^order
     ;; which translates to a LHS expansion in powers of ew.
     ;; The even orders vanish, leaving only odd powers of ew.
     ;;
     ;; The coefficients of ew are, on j=1,order+1, j odd:
     ;;
     ;;                      f_n*lamcen^(n+1-j)  / n+1 \
     ;;   Sum{n=j-1,order}(  ------------------  |     | )
     ;;                        2^(j-1) (n+1)     \  j  /
     ;;

     order=d+1                  ;The order of the fit... 1st,2nd,3rd,...
     odd_max=(order+2)/2*2-1    ;the closest odd number <= n+1
     params=fltarr(odd_max+1)   ;room for it
     params[0]=-self.strength        ;bring over the line strength 
     f=*self.fit
     for j=1,odd_max,2 do begin 
        for n=j-1,order do begin 
           params[j]=params[j]+f[n]*self.medlam^(n+1-j)/ $
                     (2.^(j-1)*(n+1.)) * $
                     factorial(n+1)/factorial(j)/factorial(n+1-j)
        endfor 
     endfor 

;        for m=1,odd_max,2 do begin 
;           for i=m-1,order do begin 
;              params[m]=params[m]+ $
;                        2.^(1-m)*(*self.fit)[i]* $
;                        ;; following is (i+1 choose m)/i+1
;                        (factorial(i)/factorial(m)/factorial(i+1-m))* $ 
;                        self.medlam^(i+1-m)
;           endfor 
;        endfor 
;       print,params
      
     roots=fz_roots(params)
     wh=where(imaginary(roots) eq 0. and float(roots) ge 0.,cnt)
     no_pos=0
     if cnt eq 0 then begin 
        self->Warning,'No positive real width found!'
        self.ew=-.999999999999999e6
     endif else if cnt gt 1 then begin 
        ;;find the closest one to our approximation
        approx=self.strength/poly(self.medlam,*self.fit) ;linear approximate
        mn=min(abs(float(roots[wh])-approx),pos)
        self.ew=float(roots[wh[pos]])
     endif else self.ew=float(roots[wh[0]])
     self->UpdateParams
  end
  
  ;; Show it under continuum and peak, along with width, if calculated
  self->UpdateButtons
  if self.map_name then self->ResetMap
  self->Plot
end

;=============================================================================
;  UpdateParams - Update the line fit parameters displayed.
;=============================================================================
pro CubeViewSpec::UpdateParams
  if self.medlam eq 0 then widget_control, self.wParams, SET_VALUE='' $
  else $
     widget_control, self.wParams,SET_VALUE= $
     string(FORMAT='(7A)',$
            string(FORMAT='(A13,":",F8.4)',$
                   "Avg. Cont",self.avgcont, $
                   "Equiv. Width",self.ew, $
                   "Max Flux",self.max, $
                   "Max Wave",self.maxlam, $
                   "Median Flux",self.medpeak, $
                   "Median Wave",self.medlam)+string(10b), $
            string(FORMAT='(A13,":",E10.4)', $
                   "Line Strength",self.strength))
end

;=============================================================================
;  ShowRegionLine - Draw the beginning of region selection line.
;=============================================================================
pro CubeViewSpec::ShowRegionLine
  if self.got eq -1 then return ; we aren't defining a region
  val=self.pressloc[self.got eq 1]
  y=self.log_scale?10.^!Y.CRANGE:!Y.CRANGE
  if self.got eq 1 then plots,!X.CRANGE,val,COLOR=self.colors_base,THICK=2 $
  else plots,val,y,COLOR=self.colors_base,THICK=2
end

;=============================================================================
;  ShowFit - Draw the continuum fit.
;=============================================================================
pro CubeViewSpec::ShowFit
  if NOT (ptr_valid(self.reg[0])) then return
  ;; mark the max and median positions
  if self.medlam ne 0 then begin 
     plots,self.medlam,self.medpeak,PSYM=7,SYMSIZE=2
     oplot,[self.medlam,self.medlam],self.log_scale?10.^!Y.CRANGE:!Y.CRANGE
     plots,self.maxlam,self.max,PSYM=1,SYMSIZE=2
  endif 
  
  if n_elements(*self.fit) eq 0 then return
  
  mx=0 & mn=n_elements(*self.lam) 
  for i=0,1 do begin 
     if ptr_valid(self.reg[i]) then begin 
        mn=mn<min((*self.reg[i])[0,*])
        mx=mx>max((*self.reg[i])[1,*])
     endif 
  endfor
  
  lam=(*self.lam)[mn:mx]
  lam=lam[sort(lam)]
  spfit=poly(lam,*self.fit)
  oplot,lam,spfit
  
  if ~ptr_valid(self.reg[1]) || self.ew le 0.0 then return
  ;; show the equivalent width.
  ew_left=self.medlam-self.ew/2.  & c_left= poly(ew_left,*self.fit)
  ew_right=self.medlam+self.ew/2. & c_right=poly(ew_right,*self.fit)
  l=!Y.CRANGE[0]
  if self.log_scale then l=10.^l
  plots,ew_left, [l,c_left], COLOR=self.colors_base+3
  plots,ew_right,[l,c_right],COLOR=self.colors_base+3
  plots,[ew_left,ew_right],[l,l],COLOR=self.colors_base+3
  
  low=value_locate(lam,ew_left)+1
  high=value_locate(lam,ew_right)
  if low gt high then begin 
     fillx=[ew_left,ew_left,ew_right,ew_right]
     filly=[l,c_left,c_right,l]
  endif else begin 
     fillx=[ew_left,ew_left,lam[low:high],ew_right,ew_right]
     filly=[l,c_left,spfit[low:high],c_right,l]
  endelse 
  polyfill,fillx,filly,/LINE_FILL,ORIENTATION=45
end

;=============================================================================
;  Delete - Delete the selected region.
;=============================================================================
pro CubeViewSpec::Delete
  if self.selected eq -1 then return
  if NOT ptr_valid(self.reg[self.seltype]) then return
  nreg=n_elements(*self.reg[self.seltype])/2
  wh=where(indgen(nreg) ne $
           self.selected,cnt)
  if cnt ne 0 then begin 
     *self.reg[self.seltype]=(*self.reg[self.seltype])[*,wh] 
     if self.selected ne -1 then begin 
        if self.selected eq 0 then self.selected=cnt-1 else $
           self.selected=self.selected-1
     endif 
  endif else begin 
     ptr_free,self.reg[self.seltype]
     self.selected=-1
  endelse 
  self->UpdateButtons
  if self.map_name then self->ResetMap
  self->Plot
  self->Send
end

;=============================================================================
;  Reset - Reset the plot ranges, keeping regions intact if KEEP is
;          set.
;=============================================================================
pro CubeViewSpec::Reset,KEEP=k
  self.xr=self.xr_def
  self.yr=self.yr_def
  if NOT keyword_set(k) then begin
     ptr_free,self.reg 
     ptr_free,self.weights
     self->ResetMap
     self.selected=-1
  endif                         ;Sets up axes for region drawing
  self.medlam=0                 ;This signals no fit.
  self.redshift=0.0
  self->ResetFit
  self->UpdateParams
  self->UpdateButtons
  self->Plot
end

;=============================================================================
;  ResetFit - Reset the fit.
;=============================================================================
pro CubeViewSpec::ResetFit
  ptr_free,self.fit & self.fit=ptr_new(/ALLOCATE_HEAP)
  self->UpdateButtons
end

; pro CubeViewSpec::Load, lam, sp
;   if n_elements(sp) eq 0 then begin 
;      xf,file,TITLE='Spectrum File',FILTERLIST=['*.dat','*.*','*'],/RECENT, $
;         PARENT_GROUP=self.wDraw
;      if size(file,/TYPE) ne 7 then return
;      self.file=file
;      self.ofile=''
;      catch,err
;      if err ne 0 then begin 
;         catch,/CANCEL
;         wmessage,"Error reading file "+file,PARENT_GROUP=self.wDraw
;         return
;      endif 
;      qsav=!QUIET                ;Yuck
;      !QUIET=1
;      rdfloat,file,lam,sp,err
;      !QUIET=qsav
;   endif 
  
;   widget_control, self.wToggles,GET_VALUE=ren
;   if n_elements(ren) eq 2 then begin ; if there auto-renorm
;      if ren[0] then self.renorm=round(alog10(median(sp)))
;   endif
  
;   *self.lam=lam & *self.sp=sp
;   if self.renorm ne 0. then *self.sp=*self.sp/10.0D^self.renorm

;   s=sort(*self.lam)
;   *self.lam=(*self.lam)[s] & *self.sp=(*self.sp)[s]
;   self->Plot

; end

; pro CubeViewSpec::Save
;   if self.ofile eq '' then begin 
;      xf,ofile,TITLE='Fit Output File',FILTERLIST=['*.fit','*.*','*'], $
;         /RECENT,/SAVEFILE,PARENT_GROUP=self.wDraw
;      if size(ofile,/TYPE) ne 7 then return else self.ofile=ofile
;   endif 
;   openw,un,/get_lun,self.ofile,/APPEND
;   printf,un,FORMAT='(6G13.6)', self.ew,self.max,self.maxlam, $
;          self.medpeak,self.medlam,self.avgcont
;   free_lun,un
;end 

;=============================================================================
;  Plot - Plot everything which needs plotting.
;=============================================================================
pro CubeViewSpec::Plot,NOOUTLINE=noo
  if n_elements(*self.lam) eq 0 then return
  old=!D.WINDOW
  wset,self.pixwin              ;Double buffering
  erase
  sp=*self.sp
  if self.log_scale then sp>=min(abs(sp)) ; avoid truncation
  plot,*self.lam,sp,XRANGE=self.xr,YRANGE=self.yr,XSTYLE=5,YSTYLE=5, $
       CHARSIZE=1.3,POSITION=[.06,.06,.99,.95],/NODATA,YLOG=self.log_scale
  self->ShowRegions
  plot,*self.lam,sp,XRANGE=self.xr,YRANGE=self.yr,XSTYLE=1,YSTYLE=1, $
       CHARSIZE=1.3,POSITION=[.06,.06,.99,.95],/NOERASE,YLOG=self.log_scale
  if self.show_error && n_elements(*self.sp_unc) gt 0 then $
     errplot,*self.lam,*self.sp-*self.sp_unc,*self.sp+*self.sp_unc

  self.x_s=!X.S & self.y_s=!Y.S
  
  if self.Info then xyouts,.06,.97,/NORMAL,self.Info,CHARSIZE=1.2
  
  if self.mode eq 1 && self.map_name then begin 
     add=' [Map: '+self.map_name+']'+ $
         (self.redshift?string(FORMAT='(" cz=",F0.1,"km/s")',self.redshift):"")
     xyouts,.99,.97,add,/NORMAL,ALIGNMENT=1.0,CHARSIZE=1.2, $
            COLOR=self.colors_base+3
  endif 
  if self.mode ne 0 then begin 
     self->HighlightPeak
     if NOT keyword_set(noo) then self->Outline
     self->ShowFit
  endif
  if self.mode ne 0 OR self.got eq 0 or self.got eq 1 then self->ShowRegionLine

  self->ShowValueLine
  wset,self.win
  device,COPY=[0,0,!D.X_SIZE,!D.Y_SIZE,0,0,self.pixwin]
  wset,old
end

;=============================================================================
;  ShowValueLine - Draw the moving current value line.
;=============================================================================
pro CubeViewSpec::ShowValueLine
  if self.press or self.movestart eq -1 then return
  if ~self->ShowingValueLine() then return
  x=(*self.lam)[self.movestart]
  plots,x,self.log_scale?10.^!Y.CRANGE:!Y.CRANGE
end

;=============================================================================
;  ShowingValueLine - Is the value line showing
;=============================================================================
function CubeViewSpec::ShowingValueLine
  widget_control, self.wToggles, GET_VALUE=tog
  return,tog[1] ne 0 
end

;=============================================================================
;  HighlightPeak - Color the spectrum inside of the peak region.
;=============================================================================
pro CubeViewSpec::HighlightPeak
  if NOT ptr_valid(self.reg[1]) then return
  for i=0,n_elements(*self.reg[1])/2-1 do begin 
     reg=(*self.reg[1])[*,i]
     reg[1]<=n_elements(*self.sp)-1 
     if ~array_equal(reg ge 0,1b) then return
     sp=(*self.sp)[reg[0]:reg[1]]
     oplot,(*self.lam)[reg[0]:reg[1]],sp,COLOR=self.colors_base+3
     if self.show_error && n_elements(*self.sp_unc) gt 0 then begin 
        err=(*self.sp_unc)[reg[0]:reg[1]]
        errplot,(*self.lam)[reg[0]:reg[1]],sp-err,sp+err, $
                COLOR=self.colors_base+3
     endif 
  endfor 
end

;=============================================================================
;  ShowRegions - Draw the peak and continuum regions with outlines.
;=============================================================================
pro CubeViewSpec::ShowRegions
  y=self.log_scale?10.^!Y.CRANGE:!Y.CRANGE
  ;;full mode, no region -- simply highlight the chosen wavelength
  if self.mode eq 0 AND self.wavelength ne 0.0 then begin 
     plots,self.wavelength,y,COLOR=self.colors_base+3,THICK=2
     return
  endif
  
  do_weights=ptr_valid(self.weights) AND not ptr_valid(self.reg[1])
  
  ;; Plot the weight vector instead of foreground region
  if do_weights then begin 
     wlam=(*self.weights)[0,*] 
     wght=y[0]+(*self.weights)[1,*]*(y[1]-y[0])
     polyfill,wlam,wght,COLOR=self.colors_base+2,NOCLIP=0
  endif 
  
  nlam=n_elements(*self.lam)
  ;; Plot the two region types
  for j=do_weights?0:1,0,-1 do begin
     ptr=self.reg[j]
     if ptr_valid(ptr) then begin
        for i=0,n_elements(*ptr)/2-1 do begin
           reg=(*ptr)[*,i]
           l=(*self.lam)[reg]
 ;          lout=(*self.lam)[(reg+[-1,1])>0<(nlam-1)]
 ;          l=(l+lout)/2.
           polyfill,[l[0],l[0],l[1],l[1]],[y[0],y[1],y[1],y[0]], $
                    COLOR=self.colors_base+([0,2])[j],/DATA
        endfor
     endif
  endfor
  over=self->FindOver()         ;color the overlapping areas an intermediate
  if over[0] eq -1 then return
  for i=0,n_elements(over)/2-1 do begin 
     l=(*self.lam)[over[*,i]]
;     lout=(*self.lam)[(over[*,i]+[-1,1])>0<(nlam-1)]
;     l=(l+lout)/2.
     polyfill,[l[0],l[0],l[1],l[1]],[y[0],y[1],y[1],y[0]], $
              COLOR=self.colors_base+1,/DATA
  endfor 
end


;=============================================================================
;  Outline - Draw outline highlight around the selected region.
;=============================================================================
pro CubeViewSpec::Outline
  if self.selected eq -1 then return
  range=(*self.reg[self.seltype])[*,self.selected]
  l=(*self.lam)[range]
;  lout=(*self.lam)[(range+[-1,1])>0<(n_elements(*self.lam)-1)]
;  l=(l+lout)/2.
  y=self.log_scale?10.^!Y.CRANGE:!Y.CRANGE
  plots,/DATA,[l[0],l[0],l[1],l[1],l[0]],[y[0],y[1],y[1],y[0],y[0]], $
        COLOR=self.colors_base+3
end

;=============================================================================
;  FindOver - Find regions where peak and continuum overlap.
;=============================================================================
function CubeViewSpec::FindOver
  if total(ptr_valid(self.reg)) ne 2. then return,-1
  for i=0,n_elements(*self.reg[1])/2-1 do begin 
     wh=where((*self.reg[0])[0,*] lt (*self.reg[1])[1,i] AND $
              (*self.reg[0])[1,*] gt (*self.reg[1])[0,i],cnt)
     for j=0,cnt-1 do begin 
        over=[(*self.reg[0])[0,wh[j]]>(*self.reg[1])[0,i], $
              (*self.reg[0])[1,wh[j]]<(*self.reg[1])[1,i]]
        if n_elements(range) eq 0 then range=[over] else $
           range=[[range],[over]] 
     endfor 
  endfor 
  if n_elements(range) eq 0 then return,-1 else return,range
end

;=============================================================================
;  MergeRegs - Merge overlapping selected regions in continuum/peak
;              locations, and sort the final regions.
;=============================================================================
pro CubeViewSpec::MergeRegs
  for j=0,1 do begin 
     ptr=self.reg[j]
     if ptr_valid(ptr) then begin 
        nreg=n_elements(*ptr)/2
        ;;print,'Merging:  Before: ',strtrim(nreg,2)
        unmerged=replicate(1b,nreg) ;keep track of who's been tested for merge
        kept=bytarr(nreg)
        mins=(*ptr)[0,*] & maxs=(*ptr)[1,*]
        while total(unmerged) gt 0.  do begin
           still=where(unmerged,scnt) ;remaining to be merged
           if scnt gt 1 then begin ;overlaps possible
              wh=where(maxs[still] ge mins[still[0]] AND $ ;overlappers
                       mins[still] le maxs[still[0]],cnt)
              if cnt ne 0 then begin ;some were found
                 wh=still[wh]
                 bot=min([mins[still[0]],mins[wh]]) 
                 top=max([maxs[still[0]],maxs[wh]])
                 (*ptr)[*,still[0]]=[bot,top] ;merge into the bottom one
                 unmerged[wh]=0 ;these were merged in
              endif 
           endif 
           unmerged[still[0]]=0b ;The one we worked on is always counted done
           kept[still[0]]=1b    ;  and we're keeping it
        endwhile
        if self.selected ne -1 AND self.seltype eq j then $
           if NOT kept[self.selected] then begin 
           wh=where(kept[0:self.selected-1],cnt)
           if cnt gt 0 then self.selected=wh[cnt-1] else self.selected=-1
        endif 
        kept=where(kept,cnt)
        if cnt gt 0 then *ptr=(*ptr)[*,kept] else ptr_free,ptr
        ;;print,'Merging:  After: ',strtrim(n_elements(*ptr)/2,2)
        if cnt gt 1 then begin
           s=sort((*ptr)[0,*])
           if self.selected ne -1 then self.selected=where(s eq self.selected)
           *ptr=(*ptr)[*,s]
        endif 
     endif 
  endfor 
end

;=============================================================================
;  FindReg - Find the region we've clicked in, if any, and outline it
;=============================================================================
pro CubeViewSpec::FindReg, xpos,FOUND=found
  for j=0,1 do begin 
     ptr=self.reg[j]
     if ptr_valid(ptr) then begin 
        for i=0,n_elements(*ptr)/2-1 do begin 
           if xpos ge (*ptr)[0,i] AND xpos le (*ptr)[1,i] then begin
              ;; Found a region ... add it to the list
              if n_elements(which) eq 0 then which=i else which=[which,i]
              if n_elements(type) eq 0 then type=j else type=[type,j]
              s=(*ptr)[1,i]-(*ptr)[0,i]+1
              if n_elements(sz) eq 0 then sz=s else sz=[sz,s]
           endif 
        endfor 
     endif 
  endfor 
  if ((found=n_elements(which))) eq 0 then return
  if self.selected ne -1 then self->Plot,/NOOUTLINE ; remove old outline
  ;; find the smallest region underneath that point
  dum=min(sz,pos)
  self.selected=which[pos]
  widget_control, self.wRType,SET_DROPLIST_SELECT=type[pos]
  self.seltype=type[pos]
  self->Outline                 ; let's see it
end

;=============================================================================
;  RegionIndices - Return a all indices corresponding to a passed
;                  region set
;=============================================================================
function CubeViewSpec::RegionIndices, regs
  for i=0,n_elements(regs)/2-1 do begin 
     range=regs[*,i]
     range=range[0]+indgen(range[1]-range[0]+1)
     if n_elements(ret) ne 0 then ret=[ret,range] else ret=range
  endfor 
  return, n_elements(ret) eq 0?-1:ret
end

;=============================================================================
;  SetColors - Give us a nice dull to bright red then white
;=============================================================================
pro CubeViewSpec::SetColors
  tvlct,[110b,147b,184b,255b,255b], $
        [0b,  0b,   13b,158b,255b], $
        [0b,  0b,    0b, 54b,255b], $
        self.colors_base
end

;=============================================================================
;  SetWin - Set the window and restore plot scalings
;=============================================================================
pro CubeViewSpec::SetWin
  wset,self.win & !X.S=self.x_s & !Y.S=self.y_s
end

;=============================================================================
;  Cleanup
;=============================================================================
pro CubeViewSpec::Cleanup
  self.mode=0 & self->Send,/JUST_SEND ;make sure everybody is in Full mode
  wdelete,self.pixwin
  ptr_free,self.lam,self.sp,self.sp_unc, $
           self.fit,self.reg,self.weights,self.wMapSets
  self->OMArray::Cleanup
  self->ObjMsg::Cleanup
end

;=============================================================================
;  Init
;=============================================================================
function CubeViewSpec::Init,XRANGE=xr,YRANGE=yr,PARENT_GROUP=grp
  if n_elements(xr) ne 0 then self.xr_def=xr else self.xr_def=[0,0]
  if n_elements(yr) ne 0 then self.yr_def=yr else self.yr_def=[0,0]
  self.xr=self.xr_def & self.yr=self.yr_def
  
  self.colors_base=!D.TABLE_SIZE-5 ;load colors in
  
  self.wBase=widget_base(/COLUMN,/TRACKING_EVENTS,SPACE=1, $
                         TITLE='CubeSpec: Extracted Spectrum',MBAR=mbar, $
                         /TLB_SIZE_EVENTS)
  
  file=widget_button(mbar,value='File',/MENU)
  self.wSaveBut=widget_button(file,value='Save Spectrum As...')
  self.wExportBut=widget_button(file,SENSITIVE=~LMGR(/VM,/RUNTIME),  $
                                value='Export Spectrum to Command Line...')
  self.wQuit=widget_button(file,value='Close')
  maps=widget_button(mbar,value='Maps',/MENU)
  but=widget_button(maps, value='Save Current Map...', $
                    EVENT_PRO='CubeViewSpec_map_event',UVALUE='save')
  self.wGray[0]=but

  but=widget_button(maps, value='Load Maps...', $
                    EVENT_PRO='CubeViewSpec_map_event',UVALUE='load')
  but=widget_button(maps,value='Reset to Default Maps', $
                    EVENT_PRO='CubeViewSpec_map_event',UVALUE='reset')
  
  but=widget_button(maps,value='Set Redshift...', $
                    EVENT_PRO='CubeViewSpec_map_event',UVALUE='redshift')
  but=widget_button(maps,value='Clear Redshift', $
                    EVENT_PRO='CubeViewSpec_map_event',UVALUE='clear-redshift')
 ;;sets=widget_button(maps,value='Map Sets',/MENU)
  self->BuildMapMenu,maps
  
  
  ;; Button layout
  controlbase=widget_base(self.wBase,COLUMN=2,SPACE=20)
  
  col1base=widget_base(controlbase,/COLUMN,/ALIGN_LEFT) 
  
  col1row1base=widget_base(col1base,/ROW,/ALIGN_LEFT, $
                           /BASE_ALIGN_TOP,SPACE=1) 
  
  self.wMode=cw_bgroup(col1row1base,/EXCLUSIVE,/COLUMN,['Full Cube','Map'], $
                       SET_VALUE=0,/NO_RELEASE,/FRAME)
  

  mousecolbase=widget_base(col1row1base,/COLUMN,SPACE=1,/FRAME)
  mousebaserow1=widget_base(mousecolbase,/ROW,SPACE=1)
  but=widget_label(mousebaserow1,VALUE='Mouse:')
  self.wDo=cw_bgroup(mousebaserow1,/EXCLUSIVE,/ROW,SET_VALUE=2,/NO_RELEASE,$
                     ['XZoom','YZoom','Lambda'],IDS=ids) ;
  self.wReg_or_Wav=ids[2]
  
  mousebaserow2=widget_base(mousecolbase,/ROW,SPACE=1)
  
  but=cw_bgroup(mousebaserow2,/NONEXCLUSIVE,SET_VALUE=[0], $
                ['Lam-Weight','Integrate'],IDS=ids,/RETURN_ID,/ROW)
  self.wWavWeightInt=but
  self.wWavWeightInt_ids=ids
                            
  self.wRType=widget_droplist(mousebaserow2,VALUE=['Continuum','Peak'])
  self.wGray[1]=self.wRType
  
  
  col1row2base=widget_base(col1base,/ROW,/ALIGN_LEFT, $
                           /BASE_ALIGN_CENTER,SPACE=1) 
  
  buttons=['Log','Val','Err']
  
  self.wToggles=cw_bgroup(col1row2base,buttons,/NONEXCLUSIVE,/ROW,$
                          SET_VALUE=[0,replicate(1,n_elements(buttons)-1)])
  self.show_error=1b
  self.wOrder=widget_droplist(col1row2base,TITLE='Fit: Order', $
                              VALUE=string(FORMAT='(I0)',indgen(5)+1))
  self.wGray[2]=self.wOrder

  self.wFit=cw_bgroup(col1row2base,IDS=ids,/ROW, $
                      ['Fit','Remove','Reset'],/NO_RELEASE)
  self.wGray[3:4]=ids[0:1]
  
  colbase=widget_base(controlbase,/COLUMN,/ALIGN_TOP)
  
  for i=0,n_elements(self.wGray)-1  do $
     widget_control, self.wGray[i],SENSITIVE=0

  self.wParams=widget_label(colbase,/FRAME,/ALIGN_LEFT,VALUE= $
                            string(FORMAT='(6A)', $
                                   replicate(string(replicate(32b,30)),6)+ $
                                   string(10b)))
  self.wLine=widget_label(colbase,/FRAME, /ALIGN_LEFT, $
                          VALUE=string(bytarr(30)+(byte('*'))[0]))

  self.wDraw=widget_draw(self.wBase,XSIZE=650,YSIZE=400)
  window,/FREE,/PIXMAP,XSIZE=650,YSIZE=400 ;to cache the image in
  self.pixwin=!D.WINDOW
  
  self.lam=ptr_new(/ALLOCATE_HEAP)
  self.sp=ptr_new(/ALLOCATE_HEAP)
  self.sp_unc=ptr_new(/ALLOCATE_HEAP)
  self.fit=ptr_new(/ALLOCATE_HEAP)
  
  self.selected=-1
  self.movestart=-1
  self.got=-1
  
  widget_control, self.wBase,set_uvalue=self, $
                  KILL_NOTIFY="CubeViewSpec_kill",/REALIZE
  
  dgeom=widget_info(self.wDraw,/GEOMETRY)
  bgeom=widget_info(self.wBase,/GEOMETRY)
  self.draw_size_diff=[dgeom.SCR_XSIZE-bgeom.SCR_XSIZE, $
                       dgeom.SCR_YSIZE-bgeom.SCR_YSIZE]
  self.xsize=bgeom.SCR_XSIZE
  
  self->SetColors
  XManager,'CubeViewSpec',self.wBase,/NO_BLOCK,GROUP_LEADER=grp
  widget_control, self.wDraw,GET_VALUE=win
  self.win=win

  ;if n_elements(lam) ne 0 AND n_elements(sp) ne 0 then self->Load,lam,sp

  widget_control,self.wDraw,/DRAW_KEYBOARD_EVENTS,/INPUT_FOCUS
  self->MsgSetup,['CUBEVIEWSPEC_STACK','CUBEVIEWSPEC_FULL','CUBEVIEWSPEC_SAVE']
  return,1
end

;=============================================================================
;  CubeViewSpec
;=============================================================================
pro CubeViewSpec__define
  st={CubeViewSpec, $
      INHERITS OMArray, $       ;Helper class for ObjMsg
      INHERITS ObjMsg, $        ;We can send and receive messages
      INHERITS ObjReport, $     ;For warning and errors
      ;; The spectrum/fit and regions
      lam:ptr_new(), $          ;The spectral data
      sp:ptr_new(), $
      sp_unc: ptr_new(), $      ;uncertainty in the spectrum
      fit:ptr_new(), $          ;the continuum fit
      reg:ptrarr(2),$           ;2 ptrs each to a 2xn array of
                                ; continuum([0])/peak([1]) index ranges
      weights:ptr_new(), $      ;the weights vector for the foreground
      redshift:0.0, $           ;any redshift as cz in km/s
      ;; Data of the fit and peak
      ew: 0.0, $                ;Equivalent Width
      max: 0.0, $               ;The max data value
      maxlam: 0.0, $            ;x value of max data
      medpeak: 0.0, $           ;Value where peak is half cumulative
      medlam: 0.0, $            ;place where ""
      avgcont: 0.0, $           ;average fit continuum under peak
      strength: 0.0, $          ;The line strength
      ;; Ranges for display
      xr:[0.,0.], $             ;xrange to display
      xr_def:[0.,0.], $         ;default (largest) xrange to display
      yr:[0.,0.], $             ;yrange to display
      yr_def:[0.,0.], $         ;default (largest) yrange to display
      ;; Widget State Data      
      x_s:[0.0d,0.0d], $        ;the saved !X.S after a plot
      y_s:[0.0d,0.0d], $        ;the saved !Y.S after a plot
      mode:0, $                 ;full (0) /  stack (1) mode
      file:'', $                ;file read 
      ofile:'', $               ;file written
      Info:'',$                 ;a message describing the spectrum to display
      selected: 0, $            ;which region is selected, if any
      seltype:0, $              ;type selected, continuum, or peak
      press:0b, $               ;whether we've pressed (certain buttons)
      pressloc:[0.0,0.0], $     ;where the press occured
      movestart:0, $            ;where a region move began
      got: 0, $                 ;how many of a click pair we've gotten 
      wav_ind:0, $              ;the index of the current wavelength
      wavelength:0.0, $         ;the current wavelength (if full mode)
      upgoing:0b, $             ;whether the wavelength is increasing or not
      weight_cont:0b, $         ;fit continuum lambda-weighted
      integrate: 0b, $          ;whether to integrate the foreground
      map_name:'', $            ;the map name, if any
      show_error:0b, $          ;whether to show the errors
      log_scale: 0b, $          ;whether to use log scaling for the flux
      xsize:0, $                ;xsize of displayed widget
      draw_size_diff:[0,0],$    ;diff beteen window and widget
      ;; Widget/Window/ColorMap ID's
      wBase:0L, $               ;the window base
      wMapSets:ptr_new(), $     ;point to map set buttons
      wQuit:0L, $               ;quit button
      wFit:0L, $                ;reset,remove fit, fit
      wDo:0L, $                 ;Exclusive action... region_select,xclip,yclip
      wReg_or_Wav:0L, $         ;button with either Regions: or Wave
      wMode:0L, $               ;The Mode selector
      wGray:lonarr(5), $        ;things to de-sensitize
      wWavWeightInt: 0L, $      ;wavelength weighting/integrate button
      wWavWeightInt_ids: [0L,0L], $ ;ids for the two buttons above
      wRType:0L, $              ;peak or continuum being selected
      wDraw:0L, $               ;drawing canvas
      wLine: 0L, $              ;the status line id
      pixwin: 0l, $             ;window id of double-buffer pixmap
      win: 0L, $                ;window id of the real window
      wOrder:0L, $              ;the continuum fit order widget
      wToggles:0L, $            ;the value-line/errors check box  
      wFull:0L, $               ;widget id for selecting full mode
      wParams:0L , $            ;widget where the fitted parameters are listed
      wSaveBut:0L, $            ;save to fits
      wExportBut:0L, $          ;export spectrum button
      colors_base:0L}           ;5 linear shades
  
  ;; The messages we send
  msg={CUBEVIEWSPEC_STACK, $    ;send a selected stack set
       info:'', $               ;the info on this map stack specification
       name:'', $               ;the map set name, if any
       foreground:ptr_new(), $  ;a 2xn list of foreground wavelength index
                                ; ranges over which to average
       weights:ptr_new(), $     ;a 2xn list of weights
       background:ptr_new(),$   ;a 2xn list of continuum wavelength ranges to
                                ; average, or a list of background values
       weight_cont: 0b,$        ;whether the continuum is to be weighted
       integrate: 0b}           ;whether to integrate the foreground

                                ; background over wavelength.
  msg={CUBEVIEWSPEC_FULL, $     ;switch to full mode
       wavelength:0.0}          ;the wavelength we're at
  
  msg={CUBEVIEWSPEC_SAVE, $     ;save the current extraction
       export:0}
end
