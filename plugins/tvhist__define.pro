
;;**************************OverRiding methods********************************
;=============================================================================
;	Message. Only exclusive and box messages.
;=============================================================================
pro tvHist::Message, msg
  ;; check for changes in active state -- Update override controls box
  self->tvPlug::Message,msg,TYPE=type
  if type eq 'BOX' then begin   ; a message from our box
     self->Histo
     return
  endif 
  if type eq 'TVDRAW_REDRAW' then begin 
     if (msg.type and 2b) ne 0 then begin ; if original image redrawn
        ;; if we have a box, redo scaling (and hist plot)
        if self.Box->IsDrawn() then self->Histo
     endif 
     self.Box->Message,msg      ;now alert our box
  endif 
end 

;=============================================================================
;	Update.  Update tvDraw recipient status... turn off and on box if
;	our active status has just changed... called automatically from
;	tvPlug::Message to catch TVDRAW_EXCLUSIVE messages with parameter
;	ac (see tvPlug).
;=============================================================================
pro tvHist::Update, ac
  ;; turn box on and off whenever we update
  if n_elements(ac) eq 0 then begin ;if no ac, just do it, and get out
     self->tvPlug::Update
     return
  endif 
  ;; otherwise only toggle box if our active status has actually changed.
  case ac of
     0b: begin               
        self.box->Off
     end
     
     1b: begin
        self.box->On
     end
     
     2b: begin                  ;if turned on *again* .. means turn *off*!
        self.box->Off
        self.recip.ACTIVE=0b   
        self->Reset             ;reset box and resume original scaling
     end
  endcase 
  self->tvPlug::Update,ac       ;chain up, sending change status 
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
pro tvHist::Histo
  if NOT self.Box->IsDrawn() then return
  
  self.Box->Getlrtb,l,r,t,b     ;get box sides
  ;; Get the Pointers to the images
  self.oDraw->GetProperty,imorig=io, image=i
  if NOT ((l lt r) and (b lt t)) then return
  take=(*io)[l:r,b:t]
  s=take(sort(take))
  n=n_elements(take) 
  self.max=s[n-3]               ;cut out 4 extreme outliers
  self.min=s[2]
  
  ;; truncate the display image below min, above max for redraw.
  if self.max gt self.min then *i=(self.min>(*io)<self.max) 
  
  self.oDraw->Draw,1            ;draw and send the redraw message
  self->PlotHist                ;plot the histogram in colorbar if available.
end

pro tvHist::PlotHist
  if NOT self.Box->IsDrawn() then return
  
  self.oDraw->GetProperty, image=image
  if NOT ptr_valid(image) then return
  self.colobj->DrawCbar, WIN=win
  
  self.colobj->GetProperty, BSIZE=bs
  oldwin=!D.Window
  wset,win
  
  min=self.min & max=self.max
  if min eq max then begin 
     self.oDraw->GetProperty, DRAWWIDGET=dw
     wmessage,PARENT_GROUP=dw,'Undefined Histogram, Aborting!',/ERROR
     self->Reset
     return
  endif 
  
  if self.div ne 1 then begin
     ;; divide out the granularity
     min=min/self.div & max=max/self.div
     h=histogram(*image/self.div,MIN=min,MAX=max, $
                 BINSIZE=(max-min)/self.nbins) 
  endif else begin
     h=histogram(*image,MIN=min,MAX=max,BINSIZE=(max-min)/self.nbins) 
  endelse 
  nh=n_elements(h) 
  h=h[1:nh-2]                   ;lop off the scaled to regions
  
  ;; renormalize histogram to height of bar
  m=max(h)
  h=h*float(bs[1]-1)/m
  
  ;;scale x to width of bar
  x=findgen(nh-2)/(nh-3)*(bs[0]-1)

                                ;self.colobj->GetProperty,Top=t,Bottom=b, NRESERVE=nr
                                ;col=t-nr-1
  col=self.Colobj->GetColor('Green')
  plots,x,h,THICK=1.5,COLOR=col,/DEVICE

  
  ;; show the maximum histogram value
  xyouts,2,bs[1]-!D.Y_CH_SIZE,strtrim(m,2),COLOR=col,/DEVICE
  
  wset,oldwin
end

;=============================================================================
;	Reset: Turn Histo off and revert to original scaling.
;=============================================================================
pro tvHist::Reset
  self.Box->Reset               ;reset to no box drawn
  self.oDraw->GetProperty,image=i,imorig=io
  *i=*io
  self.oDraw->Draw
  return
end

;=============================================================================
;	Cleanup.  Clean self up
;=============================================================================
pro tvHist::Cleanup
  self->tvPlug::Cleanup
  return
end

;=============================================================================
;	Start:  Post-Initialization
;=============================================================================
pro tvHist::Start
  if NOT obj_valid(self.colobj) then begin 
     test=self.oDraw->GetMsgObjs(CLASS='tvcolor')
     if NOT obj_valid(test[0]) then begin 
        wmessage, PARENT_GROUP=self.wBASE,'No Color plug-in registered.'
        return
     endif 
     self.ColObj=test[0]
  endif 
  
  self->Histo
end

;=============================================================================
;	init.  Initialize the histogram with a tvDraw object.  All tvRBox
;	keywords are relevant (see tvrbox).
;	If COLOR is passed, and is a valid tvColor object, a histogram
;	curve will be drawn over the top.
;=============================================================================
function tvHist::Init,oDraw,EXCLUSIVE=exc,COLOBJ=col,DIVISOR=div,NBINS=nb, $
               _EXTRA=e
  if (self->tvPlug::Init(oDraw) ne 1) then return,0 ;chain up
  if n_elements(div) eq 0 then self.div=1 else self.div=div
  if n_elements(nb) eq 0 then self.nbins=200 else self.nbins=nb
  
  if obj_valid(col) then if obj_isa(col,'tvColor') then self.colobj=col
 
  ;; Sign up with Draw object for exclusives only (we lead tvrbox)
  self.recip.EXCLUSIVE=keyword_set(exc)
  self.recip.ACTIVE=1b-self.recip.EXCLUSIVE
  self.recip.REDRAW=1b

  ;; Get a tvrbox object, signing ourselves up for box messages.
  self.box=obj_new('tvrbox', oDraw, MsgList=[self],/NO_REDRAW, $
                   ACTIVE=self.recip.ACTIVE,_EXTRA=e)

  self->Update               
  return,1
end

;=============================================================================
;	tvHist__define.  Prototype the tvHist class.
;=============================================================================
pro tvHist__define
  struct={tvHist, $ 
          INHERITS tvPlug, $    ;make it a plug-in
          div:0, $              ;the divisor for proper histogram plotting
          min:0.0, $            ;the minimum hist value
          max:0.0, $            ;the maximum hist value
          nbins:0, $            ;number of bins, defaults to 100
          colobj:obj_new(), $   ;a tvColor object we might be using
          box:obj_new()}        ;a tvRBox to use.
  return
end
