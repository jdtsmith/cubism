
;;**************************OverRiding methods********************************
;=============================================================================
;	Message. Only exclusive and box messages.
;=============================================================================
pro tvHist::Message, msg
  self->tvPlug::Message,msg,TYPE=type
  case type of
     'BOX': self.oDraw->Draw    ;they moved the box!
     'TVDRAW_PREDRAW':  self->Histo,msg.im
     'TVDRAW_POSTDRAW': self->PlotHist,msg.im
     else:
  endcase 
end 

function tvHist::Icon
  return,[[  0b,  0b],[  0b,  0b],[  0b,  1b],[  0b,  1b], $
          [128b, 11b],[128b, 30b],[128b, 54b],[152b,100b], $
          [188b,192b],[198b,128b],[131b,128b],[  1b,  0b], $
          [  0b,  0b],[170b,170b],[ 85b, 85b],[170b,170b]]
end

;;************************End OverRiding methods*******************************

;=============================================================================
;	Histo.  Actually scale the display to data contained in box.
;=============================================================================
pro tvHist::Histo,im
  if NOT self.Box->IsDrawn() then return
  
  self.Box->Getlrtb,l,r,t,b     ;get box sides
  if NOT ((l lt r) and (b lt t)) then return
  
  take=(*im)[l:r,b:t]
  s=take(sort(take))
  self.max=s[n_elements(take)-3] ;cut out 4 extreme outliers
  self.min=s[2]
  
  ;; truncate the display image below min, above max for redraw.
  if self.max gt self.min then *im=(self.min>(*im)<self.max) 
end

pro tvHist::PlotHist,im
  if NOT self.Box->IsDrawn() then return
  
  self.colobj->DrawCbar, WIN=win
  self.colobj->GetProperty, BSIZE=bs,TOP=top,BOTTOM=bottom
  oldwin=!D.Window
  wset,win
  
  min=self.min & max=self.max
  if min eq max then begin 
     self.oDraw->GetProperty, DRAWWIDGET=dw
     wmessage,PARENT_GROUP=dw,'Undefined Histogram, Aborting!',/ERROR
     self->Reset
     return
  endif 
  
  h=histogram(*im,MIN=bottom,MAX=top,BINSIZE=float(top-bottom)/self.nbins>1) 
  nh=n_elements(h) 
  
  ;; renormalize histogram (excluding endpoints) to height of bar
  m=max(h[1:nh-2])
  h=h*float(bs[1]-1)/m
  
  ;;scale x to width of bar
  x=findgen(nh)/(nh-1)*(bs[0]-1)
  plots,x,h,THICK=1.5,COLOR=self.color,/DEVICE
  
  ;; show the maximum histogram value
  xyouts,2,bs[1]-!D.Y_CH_SIZE,strtrim(m,2),COLOR=self.color,/DEVICE
  
  wset,oldwin
end

;=============================================================================
;	Reset: Turn Histo off and revert to original scaling.
;=============================================================================
pro tvHist::Reset
  self.Box->Reset               ;reset to no box drawn
  self->Off
  self.oDraw->Draw
end

;=============================================================================
;	On:  Get all our messages.
;=============================================================================
pro tvHist::On
  if self.active then begin     ;if turned on *again* .. means Reset
     self->Reset            ;reset to no box drawn
     return
  end
  self->tvPlug::On
  self.box->On
  self->Update,/EXCLUSIVE,/PREDRAW,/POSTDRAW
end

;=============================================================================
;       Off: If we have a box drawn, keep processing.
;=============================================================================
pro tvHist::Off
  self->tvPlug::Off
  self.box->Off
  ;; If no box ever drawn, sign up for exclusives only, otherwise,
  ;; keep all the messages coming
  if self.box->IsDrawn() eq 0 then begin 
     self.colobj->DrawCbar
     self->Update,/ALL_OFF,/EXCLUSIVE
  endif 
end

;=============================================================================
;	Start:  Post-Initialization
;=============================================================================
pro tvHist::Start
  self->tvPlug::Start
  if NOT obj_valid(self.colobj) then begin 
     test=self.oDraw->GetMsgObjs(CLASS='tvcolor')
     if NOT obj_valid(test[0]) then begin 
        wmessage, PARENT_GROUP=self.wBASE,'No Color plug-in registered.'
        return
     endif 
     self.ColObj=test[0]
  endif 
  self.Box->GetProperty,COLOR=col
  self.color=col
  self->Histo
end

;=============================================================================
;	init.  Initialize the histogram with a tvDraw object.  All tvRBox
;	keywords are relevant (see tvrbox).
;	If COLOR is passed, and is a valid tvColor object, a histogram
;	curve will be drawn over the top.
;=============================================================================
function tvHist::Init,oDraw,EXCLUSIVE=exc,COLOBJ=col,NBINS=nb, $
                      _EXTRA=e
  if (self->tvPlug::Init(oDraw) ne 1) then return,0 ;chain up
  if n_elements(nb) eq 0 then self.nbins=200 else self.nbins=nb
  if obj_valid(col) then if obj_isa(col,'tvColor') then self.colobj=col
  
  ;; Get a tvrbox object, signing ourselves up for box messages from it.
  self.box=obj_new('tvrbox', oDraw, MsgList=[self],/NO_REDRAW,_EXTRA=e)
  return,1
end

;=============================================================================
;	tvHist__define.  Prototype the tvHist class.
;=============================================================================
pro tvHist__define
  struct={tvHist, $ 
          INHERITS tvPlug, $    ;make it a plug-in
          min:0.0, $            ;the minimum hist value
          max:0.0, $            ;the maximum hist value
          nbins:0, $            ;number of bins, defaults to 100
          colobj:obj_new(), $   ;a tvColor object we might be using
          color:0, $              ;the color to draw the histogram with
          box:obj_new()}        ;a tvRBox to use.
  return
end
