
;;**************************OverRiding methods********************************
;=============================================================================
;  Message - Only exclusive and box messages expected
;=============================================================================
pro tvHist::Message, msg
  self->tvPlug::Message,msg,TYPE=type
  case type of
     'BOX': self.oDraw->Draw    ; box moved!
     'TVDRAW_PREDRAW':  self->Histo,msg.im
     'TVDRAW_POSTDRAW': self->PlotHist,msg.im
     else:
  endcase
end

;=============================================================================
;  On - Signup for all our messages.
;=============================================================================
pro tvHist::On
  if self->On() then begin     ;if turned on *again* .. reset
     self->Reset
     return
  end
  self->tvPlug::On
  self.box->On
  self.oDraw->MsgSignup,self,/TVDRAW_PREDRAW,/TVDRAW_POSTDRAW
end


;=============================================================================
;  Off - If we have a box drawn, process both, or for a non-trival
;        scaling mode, keep processing predraws.
;=============================================================================
pro tvHist::Off
  self->tvPlug::Off
  self.box->Off
  ;; If we have no box ever drawn, shut down the message flow
  if self.box->IsDrawn() eq 0 then begin 
     if obj_valid(self.colobj) then self.colobj->DrawCbar
     if self.scale_mode eq 0 then self.oDraw->MsgSignup,self,/NONE $
     else self.oDraw->MsgSignup,self,/NONE,/TVDRAW_PREDRAW
  endif
end

function tvHist::Icon
  return,[[  0b,  0b],[  0b,  0b],[  0b,  1b],[  0b,  1b], $
          [128b, 11b],[128b, 30b],[128b, 54b],[152b,100b], $
          [188b,192b],[198b,128b],[131b,128b],[  1b,  0b], $
          [  0b,  0b],[170b,170b],[ 85b, 85b],[170b,170b]]
end

function tvHist::Description
  return,'Scale Image with Histogram'
end
;;************************End OverRiding methods*******************************

;=============================================================================
;  Reset - Turn Histo off and revert to original scaling.
;=============================================================================
pro tvHist::Reset
  self.Box->Reset               ;reset to "no box drawn"
  self->Off
  self.oDraw->Draw
end

pro tvHist_scale_event,ev
  widget_control, ev.handler,get_uvalue=self
  self->ScaleEvent,ev
end

pro tvHist::ScaleEvent, ev
  widget_control, ev.id,get_uvalue=scale_mode
  if scale_mode eq self.scale_mode then return
  self.scale_mode=scale_mode
  if scale_mode ne 0 then begin 
     self.oDraw->MsgSignup,self,/TVDRAW_PREDRAW ;make sure we're getting these
     self.oDraw->Draw,/PREDRAW  ;and redraw it!
  endif else begin              ;just linear
     self.oDraw->Draw,/PREDRAW  ;redraw it back to linear
     if self.Box->IsDrawn() eq 0 then self.oDraw->MsgSignup,self,/NONE
  endelse 
end

;=============================================================================
;  Histo - Actually scale the display to data contained in the box,
;          and/or the scaling mode present.
;=============================================================================
pro tvHist::Histo,im
  if NOT self.Box->IsDrawn() then begin 
     ;; sanity check: linear with no hist box: do nothing
     if self.scale_mode eq 0 then return 	
     boxon=0
     take=*im
  endif else begin 
     self.Box->GetLRTB,l,r,t,b  ;get box sides
     if NOT ((l lt r) and (b lt t)) then return
     self.oDraw->GetProperty,SIZE=sz
     if r ge sz[0] OR t ge sz[1] then begin 
        self->Reset             ;panic
        return
     endif 
     boxon=1
     take=(*im)[l:r,b:t]
  endelse 
  
  switch self.scale_mode of 
     0: begin                   ; linear
        mx=max(take,min=mn)
        if mx gt mn then *im=(mn>(*im)<mx)    
        break
     end
     
     1: 
     2: begin                   ;99%,95% linear
        nt=n_elements(take)
        s=take[sort(take)]
        mx=s[((nt-1)*([99,95])[self.scale_mode-1]/100)<(nt-2)]
        mn=s[((nt-1)*([1,5])[self.scale_mode-1]/100)>1]
        if mx gt mn then *im=mn>*im<mx
        break
     end
     
     3: begin                   ;sqrt
        mx=max(take,min=mn)
        *im=sqrt((boxon?(mn>*im<mx):*im)-mn)
        break
     end
     
     4: begin                   ;logarithm
        mx=max(take,min=mn)
        *im=alog10((boxon?(mn>*im<mx):*im)-mn+(mx-mn)*1.e-6)
        break
     end
     
     5: begin                   ;histeq
        mx=max(take,min=mn)
        if mx eq mn then return
        bs=(mx-mn)/500
        h=histogram(*im,BINSIZE=bs,MIN=mn,MAX=mx)
        h[0]=0.                 ;don't elevate the background
        h=total(temporary(h),/CUMULATIVE,/DOUBLE)
        *im=h[((mn>*im<mx)-mn)/bs]
        break
     end
  endswitch
end

;=============================================================================
;  PlotHist - Show the histogram line in the colorbar.
;=============================================================================
pro tvHist::PlotHist,im
  if NOT self.Box->IsDrawn() then return
  
  self.colobj->DrawCbar, WIN=win
  self.colobj->GetProperty, BSIZE=bs,TOP=top,BOTTOM=bottom
  oldwin=!D.Window
  wset,win
  
;   min=self.min & max=self.max
;   if min eq max then begin 
;      self.oDraw->GetProperty,DRAWWIDGET=dw 
;      self->Reset
;      message,'Undefined Histogram, Aborting!'
;     return
;  endif 
  
  h=histogram(*im,MIN=bottom,MAX=top,BINSIZE=float(top-bottom)/self.nbins>1) 
  nh=n_elements(h) 
  
  ;; renormalize histogram (excluding endpoints) to height of bar
  m=max(h[1:nh-2])
  h=(h<m)*float(bs[1]-1)/m
  
  ;; show the maximum histogram value
  xyouts,3,bs[1]-!D.Y_CH_SIZE,strtrim(m,2),COLOR=self.color,/DEVICE
  
  ;;scale x to width of bar
  x=findgen(nh)/(nh-1)*(bs[0]-1)
  plots,x,h,COLOR=self.color,/DEVICE
  wset,oldwin
end


;=============================================================================
;	Start  Find the color object if none passed
;=============================================================================
pro tvHist::Start
  self->tvPlug::Start
  if NOT obj_valid(self.colobj) then begin 
     test=self.oDraw->GetMsgObjs(CLASS='tvColor')
     if NOT obj_valid(test[0]) then begin
        message,'No Color plug-in registered.'
        return
     endif 
     self.ColObj=test[0]
  endif 
  self->Histo
end

;=============================================================================
;       Init - Initialize the histogram with a tvDraw object.  All
;              tvRBox keywords are relevant (see tvrbox).  
;=============================================================================
function tvHist::Init,oDraw,NBINS=nb,SCALING_MENU=smenu,_EXTRA=e
  if (self->tvPlug::Init(oDraw) ne 1) then return,0 ;chain up
  if n_elements(nb) eq 0 then self.nbins=200 else self.nbins=nb
  ;; Get a tvrbox object, signing ourselves up for box messages from it.
  self.box=obj_new('tvRBox', oDraw,/CORNERS,/SNAP,_EXTRA=e)
  self.Box->MsgSignup,self,/BOX
  self.Box->GetProperty,COLOR=col
  self.color=col                ;for drawing the histogram over the color-bar
  
  if keyword_set(smenu) then begin 
     scal_menu=widget_button(smenu,value='Scale Image',/MENU,uvalue=self, $
                             event_pro='tvHist_scale_event')
     item=widget_button(scal_menu,value='Linear',uvalue=0)
     item=widget_button(scal_menu,value='Linear 99%',uvalue=1)
     item=widget_button(scal_menu,value='Linear 95%',uvalue=2)
     item=widget_button(scal_menu,value='Square Root',uvalue=3)
     item=widget_button(scal_menu,value='Logarithmic',uvalue=4)
     item=widget_button(scal_menu,value='Histogram Equalization',uvalue=5)
  endif 
  self->Off
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
          scale_mode: 0, $      ;0:linear 1:99% 2:95% 3:sqrt 4:log 5:histeq
          colobj:obj_new(), $   ;a tvColor object to use
          color:0, $            ;the color to draw the histogram with
          box:obj_new()}        ;a tvRBox to use.
  return
end
