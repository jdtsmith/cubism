pro tvFixedRegion::Message,msg
  self->Draw
end

pro tvFixedRegion::On
  if self->On() then begin      ;if turned on *again* .. reset
     self->Reset
     return
  endif
  self->tvPlug::On
  ;; Button and redraw events, no more snapshots
  self.oDraw->MsgSignup,self,/TVDRAW_SNAPSHOT
  self.oDraw->ReDraw,/SNAPSHOT
end

pro tvFixedRegion::Reset
  self.oDraw->MsgSignup,self,/NONE
  if self.drawn then begin 
     self.drawn=0b
     self.oDraw->ReDraw,/SNAPSHOT
  endif 
  self->tvPlug::Off
end

pro tvFixedRegion::SetProperty, COLOR=col,REGION=reg
  if n_elements(col) ne 0 then self.color=col
  if n_elements(reg) ne 0 then self.region=ptr_new(reg)
  if self->On() then self.oDraw->ReDraw,/SNAPSHOT
end

pro tvFixedRegion::Draw
  if ~ptr_valid(self.region) then return
  self.oDraw->Setwin
  x=-1 & y=-1
  n=n_elements(*self.region)/2
  x=lonarr(n+1) & y=lonarr(n+1)
  for i=0,n-1 do begin 
     coords=self.oDraw->Convert((*self.region)[*,i],/DEVICE,/FRACTIONAL)
     x[i]=coords[0] & y[i]=coords[1]
  endfor 
  x[n]=x[0] & y[n]=y[0]
  plots,x,y,COLOR=self.color,THICK=self.thick,/DEVICE
  self.drawn=1
end

pro tvFixedRegion::Cleanup
  ptr_free,self.region
  self->tvPlug::Cleanup
end

;=============================================================================
;  Init 
;    OPTIONS:                                          -- Default 
;  	COLOR:     Color index with which to draw box  -- 0
;  	THICK:     Thickness of box band               -- 1.2
;=============================================================================
function tvFixedRegion::Init,oDraw,COLOR=color,THICK=thick,_EXTRA=e
  if (self->tvPlug::Init(oDraw,/NO_ON_OFF,_EXTRA=e) ne 1) then return,0
  if n_elements(color) eq 0 then self.color=0 else self.color=color
  if n_elements(thick) eq 0 then self.thick=1. else self.thick=thick
  return,1
end

pro tvFixedRegion__define
  struct={tvFixedRegion, $
          INHERITS tvPlug, $    ;make it a tvDraw plug-in
          region:ptr_new(), $   ;the region to show
          color:0, $            ;color to draw it.
          thick:0., $           ;thickness of box line
          drawn:0b}             ;whether we've drawn a box
  
end
