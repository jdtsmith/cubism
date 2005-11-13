
pro cubeautobadpix_event,ev
  widget_control, ev.top,GET_UVALUE=self
  self->Event,ev
end

pro CubeAutoBadPix::Event,ev
  name=widget_info(ev.id,/UNAME)
  if name eq 'ok' then begin 
     widget_control, self.wMaxVar,GET_VALUE=maxvar
     widget_control, self.wMinFrac,GET_VALUE=minfrac
     self.with_background=widget_info(self.wWB,/BUTTON_SET)
     self.maxvar[self.with_background]=float(maxvar)
     self.minfrac[self.with_background]=float(minfrac)
     widget_control, ev.top,/DESTROY
     return
  endif else if name eq 'cancel' then begin 
     self.cancel=1b
     widget_control, ev.top,/DESTROY
     return
  endif else if name eq 'toggleback' then begin 
     widget_control, self.wMaxVar,GET_VALUE=maxvar
     widget_control, self.wMinFrac,GET_VALUE=minfrac
     self.maxvar[self.with_background]=float(maxvar)
     self.minfrac[self.with_background]=float(minfrac)
  endif else if name eq 'reset' then self->Defaults $
  else if name eq 'minfrac' || name eq 'maxvar' then return
  
  self.with_background=widget_info(self.wWB,/BUTTON_SET)     
  widget_control, self.wMaxVar, $
                  SET_VALUE=string(FORMAT='(F0.2)', $
                                   self.maxvar[self.with_background])
  widget_control, self.wMinFrac, $
                  SET_VALUE=string(FORMAT='(F0.2)', $
                                   self.minfrac[self.with_background])
end

pro CubeAutoBadPix::Prompt,maxvar,minfrac,wb,DISABLE_WITH_BACKGROUND=dwb, $
                           _EXTRA=e
  b=widget_base(TITLE='AutoBadPix',/COLUMN,_EXTRA=e)
  t=widget_label(b,VALUE='Automatic Bad Pixels')
  r=widget_base(b,/ROW)
  l=widget_label(r,VALUE='Sig-Trim:')
  if keyword_set(dwb) then self.with_background=0b

  self.wMaxVar=widget_text(r,VALUE=string(FORMAT='(F0.2)', $
                                          self.maxvar[self.with_background]), $
                           XSIZE=8,/EDITABLE,UNAME='maxvar')
  r=widget_base(b,/ROW)
  l=widget_label(r,VALUE='Bad-Frac:')
  self.wMinFrac=widget_text(r,XSIZE=8,/EDITABLE,UNAME='minfrac',VALUE= $
                            string(FORMAT='(F0.2)', $
                                   self.minfrac[self.with_background]))
  r=widget_base(b,/NONEXCLUSIVE,/ROW)
  self.wWB=widget_button(r,VALUE='With BG',UNAME='toggleback')
  widget_control, self.wWB,SET_BUTTON=self.with_background, $
                  SENSITIVE=~keyword_set(dwb)
  r=widget_base(b,/ROW)
  ok=widget_button(r,VALUE=' OK ',UNAME='ok')
  cancel=widget_button(r,VALUE='Cancel',UNAME='cancel')
  reset=widget_button(r,VALUE='Reset',UNAME='reset')
  
  self.cancel=0b
  widget_control, b,SET_UVALUE=self,/REALIZE
  
  XManager,'CubeAutoBadPix',b,EVENT_HANDLER='cubeautobadpix_event'
  if self.cancel then begin 
     maxvar=-1
     minfra=-1
     wb=-1
     return
  endif 
  wb=self.with_background
  maxvar=self.maxvar[wb]
  minfrac=self.minfrac[wb]
end

pro CubeAutoBadPix::Defaults
  self.maxvar=[10.0,5.0]        ;without/with background
  self.minfrac=[0.5,0.1]
  self.with_background=0b
end

function CubeAutoBadPix::Init
  self->Defaults
  return,1
end

pro cubeautobadpix__define
  st={CubeAutoBadPix, $
      minfrac:[0.0,0.0], $
      maxvar: [0.0,0.0], $
      with_background:0b, $
      cancel: 0b, $             ;was the prompt cancelled
      wMinFrac:0L, $
      wMaxVar:0L, $
      wWB:0L}
end
