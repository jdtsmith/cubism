
;;**************************OverRiding;;methods********************************
;=============================================================================
;	GetProperty.
;=============================================================================
pro tvStats::GetProperty, min=min, max=max, med=med, avg=avg, std=std, box=box
  if arg_present(min) then min=self.min
  if arg_present(max) then max=self.max
  if arg_present(med) then med=self.med
  if arg_present(avg) then avg=self.avg
  if arg_present(std) then std=self.std
  if arg_present(box) then box=self.Box
end

;=============================================================================
;	Message. Only box messages.
;=============================================================================
pro tvStats::Message, msg
  self->tvPlug::Message,msg,TYPE=type
  case type of
     'BOX': begin 
        self->wShow
        self->Stats
     end 
     'TVDRAW_POSTDRAW': self->Stats ;for both types, we redo the stats
     'TVDRAW_RESIZE': if self->On() then self->Off
  endcase
end 

function tvStats::Icon
  return,[[  0b,  0b],[254b,127b],[  6b, 64b],[242b,111b], $
          [ 54b, 72b],[ 98b, 96b],[198b, 64b],[130b, 97b], $
          [134b, 65b],[194b, 96b],[102b, 64b],[ 50b,104b], $
          [246b, 79b],[  2b, 96b],[254b,127b],[  0b,  0b]]
end

function tvStats::Description
  return,'Box Statistics'
end
;;************************End OverRiding methods*******************************

;=============================================================================
;  On:  Get all our messages.
;=============================================================================
pro tvStats::On
  if self->On() then begin      ;if turned on *again* .. means Reset
     self->Reset
     return
  endif
  self->tvPlug::On
  self.Box->On
  self.oDraw->MsgSignup,self,/TVDRAW_POSTDRAW,/TVDRAW_RESIZE
  if self.box->IsDrawn() then begin 
     self->wShow
     self->Stats
  endif
end

;=============================================================================
;  Off - If we have a box drawn, keep processing.
;=============================================================================
pro tvStats::Off
  self->tvPlug::Off
  self.Box->Off
  self.oDraw->MsgSignup,self,/NONE
  if widget_info(self.wBase,/VALID_ID) then self->wDestroy
end

;=============================================================================
;  Reset 
;=============================================================================
pro tvStats::Reset
  self.Box->Reset
  self->Off
end

;;************************End OverRiding methods*******************************

;=============================================================================
;	Stats.  Compute the stats in the box.
;=============================================================================
pro tvStats::Stats
  self.Box->Getlrtb,l,r,t,b
  self.oDraw->GetProperty,imorig=io,SIZE=sz
  if NOT ((l le r) and (b le t)) then return
  if r ge sz[0] OR t ge sz[1] then begin 
     self->Reset                  ;panic
     return
  endif
  take=(*io)[l:r,b:t]
  wh=where(finite(take),n)
  if n le 1 then return
  max=max(take,MIN=min)
  med=median(take)
  avg=total(take,/NAN)/n
  std=sqrt(total((take-avg)^2,/NAN)/(n-1))
  
  keep=where(abs(take-avg) le 3.*std,nclip)
  if nclip gt 1 then begin 
     clipped_avg=total(take[keep],/NAN)/nclip
     clipped_std=sqrt(total((take[keep]-clipped_avg)^2,/NAN)/(nclip-1))
  endif else nclip=0
  
  str=string(FORMAT=self.form,l,b,r-l+1,t-b+1,max,min,avg,med,std, $
             nclip, clipped_avg, clipped_std)
  widget_control, self.wSlab, set_value=str
  self.min=min & self.max=max & self.med=med & self.avg=avg & self.std=std
end

;=============================================================================
;	wShow - Show the widgets.
;=============================================================================
pro tvStats::wShow
  if widget_info(self.wBase,/VALID) then return
  widget_control, self.parent,UPDATE=0
  self.wBase=widget_base(self.parent,/COLUMN,SPACE=1)
  ;; A cosmetic line...
  self.oDraw->GetProperty, WINSIZE=ws
  ln=widget_base(self.wBase,FRAME=2,ysize=0,xsize=ws[0]-5)
  title=widget_label(self.wBase,value='Box Statistics', $
                     FONT=cu_get_fonts(/BOLD,SIZE=14))
  self.wSlab=widget_text(self.wBase,value=' ',xsize=54,ysize=3,_EXTRA=e)
  widget_control, self.parent,UPDATE=1
end

;=============================================================================
;	wDestroy - Destroy the Widget
;=============================================================================
pro tvStats::wDestroy
  widget_control, self.wBase,/DESTROY
end

;=============================================================================
;       Init - Initialize the Stats object.  All tvRBox keywords are
;              relevant (see tvrbox).
;=============================================================================
function tvStats::Init,parent,oDraw,FORMAT=form, _EXTRA=e
  if (self->tvPlug::Init(oDraw,_EXTRA=e) ne 1) then return,0 ;chain up
  self.parent=parent
  if n_elements(form) eq 0 then $
     self.form='("[",I3,",",I3,"]","(",I3," x",I3,")"," MAX:",G11.5,' + $
        '"  MIN:",G11.5,/,"AVG:",G11.5,"  MEDIAN:",G11.5, "  STDEV:",G10.5,' +$
        '/,"3SIG CLIPPED  CNT:",I5," AVG:",G11.5,"  STDEV:",G11.5)'  $
  else self.form=form
  
  ;; Get a tvrbox object, signing *ourself* up for box messages.
  self.box=obj_new('tvRBox', oDraw,/ON_MOTION,/CORNERS,/SNAP,_EXTRA=e)
  self.Box->MsgSignup,self,/BOX
  self->Off
  return,1
end

;=============================================================================
;	tvStats__define.  Prototype the tvStats class.
;=============================================================================
pro tvStats__define
  struct={tvStats, $ 
          INHERITS tvPlug, $    ;make it a plug-in
          box:obj_new(), $      ;a tvRBox to use.
          min:0.0, $             ;the last min
          max:0.0, $             ;the last max
          med:0.0,$             ;the last med
          avg:0.0,$             ;the last avg
          std:0.0,$             ;the last std
          parent:0L, $          ;the parent of the widget set
          wBase:0L, $           ;a base to put the test widget in
          form:'', $            ;the format to print in 
          wSlab:0L}             ;a text widget for the stats
  return
end
