
;;**************************OverRiding methods********************************
;=============================================================================
;	Message. Only exclusive and box messages.
;=============================================================================
pro tvStats::Message, msg
  self->tvPlug::Message,msg,TYPE=type
  switch type of
     'BOX': self->wShow
     'TVDRAW_POSTDRAW':self->Stats ;for both types, we redo the stats
  endswitch
end 

function tvStats::Icon
  return,[[  0b,  0b],[254b,127b],[  6b, 64b],[242b,111b], $
          [ 54b, 72b],[ 98b, 96b],[198b, 64b],[130b, 97b], $
          [134b, 65b],[194b, 96b],[102b, 64b],[ 50b,104b], $
          [246b, 79b],[  2b, 96b],[254b,127b],[  0b,  0b]]
end
;;************************End OverRiding methods*******************************
;=============================================================================
;	Stats.  Compute the stats in the box.
;=============================================================================
pro tvStats::Stats
  self.Box->Getlrtb,l,r,t,b
  self.oDraw->GetProperty,imorig=io
  if NOT ((l le r) and (b le t)) then return
  take=(*io)[l:r,b:t]
  n=n_elements(take) 
  max=max(take,MIN=min)
  med=median(take)
  avg=total(take)/n
  std=sqrt(total((take-avg)^2)/(n-1))
  str=string(FORMAT=self.form,l,b,r-l+1,t-b+1,max,min,avg,med,std)
  widget_control, self.wSlab, set_value=str
end

;=============================================================================
;	wShow: Show the widgets.
;=============================================================================
pro tvStats::wShow
  if widget_info(self.wBase,/VALID) then return
  widget_control, self.parent,UPDATE=0
  self.wBase=widget_base(self.parent,/COLUMN,SPACE=1)
  ;; A cosmetic line...
  self.oDraw->GetProperty, WINSIZE=ws
  ln=widget_base(self.wBase,FRAME=2,ysize=0,xsize=ws[0]-5)
  title=widget_label(self.wBase,value='Box Statistics', $
                     FONT=sm_get_fonts(/BOLD,SIZE=14))
  self.wSlab=widget_text(self.wBase,value=' ',xsize=54,ysize=2,_EXTRA=e)
  widget_control, self.parent,UPDATE=1
end

;=============================================================================
;	wDestroy: Destroy the Widget
;=============================================================================
pro tvStats::wDestroy
  widget_control, self.wBase,/DESTROY
end

;=============================================================================
;	On:  Get all our messages.
;=============================================================================
pro tvStats::On
  if self.active then begin     ;if turned on *again* .. means Reset
     self.Box->Reset
     self->Off
     return
  endif
  self->tvPlug::On
  self.Box->On
  self->Update,/EXCLUSIVE,/POSTDRAW
  if self.box->IsDrawn() then begin 
     self->wShow
     self->Stats
  endif
end

;=============================================================================
;       Off - If we have a box drawn, keep processing.
;=============================================================================
pro tvStats::Off
  self->tvPlug::Off
  self.Box->Off
  self->Update,/ALL_OFF,/EXCLUSIVE
  if widget_info(self.wBase,/VALID_ID) then self->wDestroy
end

;=============================================================================
;   Init - Initialize the Stats object.  All tvRBox keywords are
;          relevant (see tvrbox). 
;=============================================================================
function tvStats::Init,parent,oDraw,FORMAT=form,EXCLUSIVE=exc, $
                       _EXTRA=e
  if (self->tvPlug::Init(oDraw,_EXTRA=e) ne 1) then return,0 ;chain up
  self.parent=parent
  if n_elements(form) eq 0 then $
     self.form='("[",I3,",",I3,"]","(",I3," x",I3,")"," MAX:",G11.5,' + $
     '" MIN:",G11.5,/,"AVG:",G11.5,"  MEDIAN:",G11.5, "  STDEV:",G10.5)'  $
  else self.form=form
  
  ;; Get a tvrbox object, signing *ourself* up for box messages.
  self.box=obj_new('tvrbox', oDraw, MsgList=[self],_EXTRA=e)
  return,1
end

;=============================================================================
;	tvStats__define.  Prototype the tvStats class.
;=============================================================================
pro tvStats__define
  struct={tvStats, $ 
          INHERITS tvPlug, $    ;make it a plug-in
          box:obj_new(), $      ;a tvRBox to use.
          parent:0L, $          ;the parent of the widget set
          wBase:0L, $           ;a base to put the test widget in
          form:'', $            ;the format to print in 
          wSlab:0L}             ;a text widget for the stats
  return
end
