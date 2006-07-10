;+
; NAME:  
;
;    CubeAutoBadPix
;
; CONTACT:
;
;    UPDATED VERSIONS of CUBISM and more information can be found at:
;       http://ssc.spitzer.caltech.edu/cubism
;
; DESCRIPTION:
;    
;    GUI for selecting parameters for automatic pixel detection.
;    
; CATEGORY:
;
;    CUBISM Spectral ReductiC. B. Coderxel Detection;
;
; MODIFICATION HISTORY:
;    
;
;    2005-11-13 (J.D. Smith): Initially written.
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2005 J.D. Smith
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

pro cubeautobadpix_event,ev
  widget_control, ev.top,GET_UVALUE=self
  self->Event,ev
end

pro CubeAutoBadPix::Event,ev
  name=widget_info(ev.id,/UNAME)
  
  case name of
     'ok': begin 
        widget_control, self.wMaxVar,GET_VALUE=maxvar
        widget_control, self.wMinFrac,GET_VALUE=minfrac
        self.with_background=widget_info(self.wWB,/BUTTON_SET)
        self.with_unc=widget_info(self.wWU,/BUTTON_SET)
        self.maxvar[self.with_background,self.record_only]=float(maxvar)
        self.minfrac[self.with_background,self.record_only]=float(minfrac)
        widget_control, ev.top,/DESTROY
        return
     end 
     
     'cancel': begin 
        self.cancel=1b
        widget_control, ev.top,/DESTROY
        return
     end 
     
     'toggleback': begin 
        widget_control, self.wMaxVar,GET_VALUE=maxvar
        widget_control, self.wMinFrac,GET_VALUE=minfrac
        self.minfrac[self.with_background,self.record_only]=float(minfrac)
        self.maxvar[self.with_background,self.record_only]=float(maxvar)
     end 
     
     'reset': self->Defaults 
     
     else:return
  endcase
  
  self.with_background=widget_info(self.wWB,/BUTTON_SET)     
  widget_control, self.wMaxVar, $
                  SET_VALUE=string(FORMAT='(F0.2)', $
                                   self.maxvar[self.with_background, $
                                               self.record_only])
  widget_control, self.wMinFrac, $
                  SET_VALUE=string(FORMAT='(F0.2)', $
                                   self.minfrac[self.with_background, $
                                                self.record_only])
end

pro CubeAutoBadPix::Prompt,maxvar,minfrac,wb,wu, $
                           DISABLE_WITH_BACKGROUND=dwb, $
                           DISABLE_WITH_UNC=dwu, RECORD_ONLY=ro, $
                           _EXTRA=e
  b=widget_base(TITLE='AutoBadPix',/COLUMN,_EXTRA=e)
  self.record_only=keyword_set(ro)
  self->Defaults
  t=widget_label(b,VALUE=self.record_only? $
                 'Automatic Record Bad Pixels': $
                 'Automatic Global Bad Pixels')
  r=widget_base(b,/ROW)
  l=widget_label(r,VALUE='Sigma-Trim:')
  if keyword_set(dwb) then self.with_background=0b
  if keyword_set(dwu) then self.with_unc=0b

  self.wMaxVar=widget_text(r,VALUE=string(FORMAT='(F0.2)', $
                                          self.maxvar[self.with_background, $
                                                      self.record_only]), $
                           XSIZE=8,/EDITABLE,UNAME='maxvar')
  r=widget_base(b,/ROW)
  l=widget_label(r,VALUE='MinBad-Frac:')
  self.wMinFrac=widget_text(r,XSIZE=8,/EDITABLE,UNAME='minfrac',VALUE= $
                            string(FORMAT='(F0.2)', $
                                   self.minfrac[self.with_background, $
                                                self.record_only]))
  
  r=widget_base(b,/NONEXCLUSIVE,/ROW)
  self.wWB=widget_button(r,VALUE='BG',UNAME='toggleback')
  self.wWU=widget_button(r,VALUE='UNC',UNAME='toggleunc')
  
  widget_control, self.wWB,SET_BUTTON=self.with_background, $
                  SENSITIVE=~keyword_set(dwb)
  
  widget_control, self.wWU,SET_BUTTON=self.with_unc, $
                  SENSITIVE=~keyword_set(dwu)
  
  r=widget_base(b,/ROW)
  ok=widget_button(r,VALUE=' OK ',UNAME='ok')
  cancel=widget_button(r,VALUE='Cancel',UNAME='cancel')
  reset=widget_button(r,VALUE='Reset',UNAME='reset')
  
  self.cancel=0b
  widget_control, b,SET_UVALUE=self,/REALIZE
  
  if keyword_set(dwb) then widget_control, self.wWB,SENSITIVE=0
  if keyword_set(dwu) then widget_control, self.wWU,SENSITIVE=0
  
  XManager,'CubeAutoBadPix',b,EVENT_HANDLER='cubeautobadpix_event'
  if self.cancel then begin 
     maxvar=-1
     minfra=-1
     wb=-1
     wu=-1
     return
  endif 
  wb=self.with_background
  wu=self.with_unc
  maxvar=self.maxvar[wb,self.record_only]
  minfrac=self.minfrac[wb,self.record_only]
end

pro CubeAutoBadPix::Defaults
  if self.defaults_set then return
  self.maxvar=[[10.0,5.0],$     ;without/with background
               [10.0,5.0]]      ;record only version
  self.minfrac=[[0.5,0.1], $
                [0.75,0.75]]
  self.with_background=1b
  self.with_unc=0b              ; self.record_only?0b:1b
  self.defaults_set=1b
end


pro cubeautobadpix__define
  st={CubeAutoBadPix, $
      minfrac:fltarr(2,2), $
      maxvar: fltarr(2,2), $
      defaults_set:0b, $
      with_background:0b, $
      with_unc:0b, $
      record_only: 0b, $
      cancel: 0b, $             ;was the prompt cancelled
      wMinFrac:0L, $
      wMaxVar:0L, $
      wWB:0L, $
      wWU:0L}
end
