;+
; NAME:
;
;    tvHist
;
; DESCRIPTION:
;
;    A tvTools plugin for scaling images and tracking the resulting
;    color histogram.
;
; CATEGORY:
;
;    tvTools, Image Scaling, Histograms
;
; SIDE EFFECTS:
;
;    Draws a histogram in red on the first located tvColor object
;    window in the same tvDraw plugin group.
;
; METHODS:
;
;    Init:  
;
;       CALLING SEQUENCE:
;
;          obj=obj_new('tvHist',oDraw,[NBINS=,SCALING_MENU=,_EXTRA=e])
;          
;       INPUT PARAMETERS:
;
;	   oDraw: The tvDraw object.
;	   
;       INPUT KEYWORD PARAMETERS:
;
;          NBINS: The number of scaling bins to use for histogram
;            computation.  Default 200.
;             
;          SCALING_MENU: A menu widget id, which, if passed will root
;            the "Scale Image" menu group.
;
;          _EXTRA: Any other ObjMsg initialization keywords
;             (e.g. message list).
;             
;    Freeze:
;  
;	DESCRIPTION:
;
;	   Freeze the low and high scaling range values.
;	
;       CALLING SEQUENCE:
;
;          obj->Freeze
;          
; INHERITANCE TREE:
;
;    ObjMsg-->tvPlug-->tvHist
;
; MODIFICATION HISTORY:
;
;    2005-06-24 (J.D. Smith): Reworked all scaling logic.  Sparse
;       sampling for large regions.
;
;    2001-08-02 (J.D. Smith): Imported from SCORE-era source.
;       
;-
;    $Id$
;##############################################################################
;
; LICENSE
;
;  Copyright (C) 2001-2005 J.D. Smith
;
;  This file is part of tvTools.
;
;  tvTools is free software; you can redistribute it and/or modify it
;  under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2, or (at your option)
;  any later version.
;
;  tvTools is distributed in the hope that it will be useful, but
;  WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;  General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with tvTools; see the file COPYING.  If not, write to the
;  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;  Boston, MA 02110-1301, USA.
;
;##############################################################################

;;**************************OverRiding methods********************************
;=============================================================================
;  Message - Only exclusive and box messages expected
;=============================================================================
pro tvHist::Message, msg
  self->tvPlug::Message,msg,TYPE=type
  case type of
     'BOX': if self.freeze eq 0 then self.oDraw->Draw        ; box moved!

     'TVDRAW_PREDRAW':  begin
        ptr_free,self.finite
        self.finite=ptr_new(finite(*msg.im))
        self.non_finite=~array_equal(*self.finite,1b)
        self->Histo,msg.im
     end 
     'TVDRAW_POSTDRAW': begin 
        self->CalcHist,msg.im
        self.oCol->DrawCbar
     end 
     'DRAW_KEY': if msg.press gt 0 then if msg.type eq 5 then $
        if msg.ch eq 32b then self->Freeze
     'TVCOLOR_REDRAW': self->PlotHist
     else:
  endcase
end

;=============================================================================
;  On - Turn on Box
;=============================================================================
pro tvHist::On
  if self->On() then begin     ;if turned on *again* .. reset
     self->Reset
     return
  end
  self->tvPlug::On
  self.box->On
end

;=============================================================================
;  Off - Shut down box
;=============================================================================
pro tvHist::Off
  self->tvPlug::Off
  self.box->Off
end

function tvHist::Icon
  return,[[  0b,  0b],[  0b,  0b],[  0b,  1b],[  0b,  1b], $
          [128b, 11b],[128b, 30b],[128b, 54b],[152b,100b], $
          [188b,192b],[198b,128b],[131b,128b],[  1b,  0b], $
          [  0b,  0b],[170b,170b],[ 85b, 85b],[170b,170b]]
end

function tvHist::Cursor,mask,offset
  mask=[4096U,4096U,4096U,4096U,61185U,4096U,4096U,4096U, $
        4097U,32769U,32793U,32957U,33021U,32999U,33023U,33023U]
  offset=[4,11]
  return,[4096U,4096U,0U,0U,33537U,0U,0U,4096U, $
          4096U,32768U,32768U,32776U,32924U,32998U,32768U,33023U]
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

pro tvHist_event,ev
  widget_control, ev.handler,get_uvalue=o
  call_method,o.method,o.obj,ev
end

;=============================================================================
;  ScaleEvent - Scaling menu selection events
;=============================================================================
pro tvHist::ScaleEvent, ev
  widget_control, ev.id,get_uvalue=scale_mode,/SET_BUTTON
  ;; The scaling modes
  if scale_mode le 3 then begin 
     if scale_mode eq self.scale_mode then return
     widget_control, self.wScalButs[self.scale_mode],SET_BUTTON=0
     self.scale_mode=scale_mode
  endif else begin 
     trim=scale_mode-3b         ;1 or 2
     if self.trim_mode eq trim then begin 
        ;; It was already set, clear it
        widget_control, ev.id,SET_BUTTON=0
        self.trim_mode=0b
     endif else begin 
        ;; Not already set, clear the other, if it was set
        if self.trim_mode gt 0b then begin 
           ;; turn the other off
           widget_control, self.wTrimButs[self.trim_mode-1],SET_BUTTON=0
        endif 
        self.trim_mode=trim
     endelse 
  endelse 
  self.oDraw->Draw              ;redraw it 
end

;=============================================================================
;  Freeze - Freeze the scaling limits
;=============================================================================
pro tvHist::Freeze,ev
  self.freeze=1-self.freeze
  widget_control,self.wFreeze,SET_BUTTON=self.freeze
  if self.freeze then self->PlotFreezeLock else self.oDraw->Draw
end

;=============================================================================
;  SetScaleRange - Set the scaling range
;=============================================================================
pro tvHist::SetScaleRange, ev
  if self.freeze eq 0 then self->Freeze ;can only do it when frozen
  self.oDraw->GetProperty,DRAWWIDGET=dw
  inp=twoin(low,high, PARENT_GROUP=dw, TITLE='Scaling',$
            string(FORMAT='(G0.5)',self.min), $
            string(FORMAT='(G0.5)',self.max), $
            LABEL='Set scaling range:',TEXT1="Low Scale:", TEXT2="High Scale:")
  if inp eq 0 then return
  on_ioerror, BAD_DATA
  self.min=float(low) & self.max=float(high)
  self.oDraw->Draw
  return
  BAD_DATA:  self->Error,['Improper range limits: '+low+' - '+high, $
                          !ERROR_STATE.MSG]
end


;=============================================================================
;  Histo - Actually scale the display to data contained in the box,
;          and/or the scaling mode present.
;=============================================================================
pro tvHist::Histo,im
  boxon=self.Box->IsDrawn()
  widget_control, /HOURGLASS
  if boxon then begin 
     self.Box->GetLRTB,l,r,t,b  ;get box sides
     if ~((l le r) and (b le t)) then return
     self.oDraw->GetProperty,SIZE=sz
     if r ge sz[0] OR t ge sz[1] then begin 
        self->Reset             ;panic
        return
     endif   
  endif
  
  ;; Frozen scaling range
  if self.freeze then begin
     switch self.scale_mode of
        0:                      ; linear
        3: begin                ;histeq
           self.oDraw->SetDrawMinMax,MIN=self.min,MAX=self.max
           break
        end 
        1: begin                ;sqrt
           *im=sqrt(*im-self.min>0)
           self.oDraw->SetDrawMinMax,MIN=0.,MAX=sqrt(self.max-self.min)
           break
        end
        2: begin                ;logarithm
           *im=alog10((*im - self.min)>0. + (self.max-self.min)*.05)
           self.oDraw->SetDrawMinMax,MIN=alog10((self.max-self.min)*.05), $
                                     MAX=alog10((self.max-self.min)*(1.+.05))
           break
        end
     endswitch
     return
  endif   
  
  ;; Non-frozen scaling range
  if boxon then begin 
     take=(*im)[l:r,b:t] 
     if self.non_finite then finite=(*self.finite)[l:r,b:t]
  endif else begin 
     boxon=0
     take=*im
     if self.non_finite then finite=*self.finite
  endelse
  

  ;; Find maximum and minimum/Trimmed max/min
  if self.trim_mode eq 0b then begin 
     mx=max(take,min=mn,NAN=self.non_finite)
  endif else begin 
     ;; Dilutely sample the image portion if it is larger than 500x500
     s=size(take,/DIMENSIONS)
     n=500
     if ~array_equal(s le n,1b) then begin 
        keep_x=lindgen(n)*(s[0]-1)/(n-1)
        keep_y=lindgen(n)*(s[1]-1)/(n-1)
        case 1 of 
           s[0] gt n && s[1] le n: begin 
              take=take[keep_x,*]
              if self.non_finite then finite=finite[keep_x,*]
           end 

           s[0] le n && s[1] gt n: begin 
              take=take[*,keep_y]
              if self.non_finite then finite=finite[*,keep_y]
           end 
           
           else: begin 
              take=take[keep_x,keep_y]
              if self.non_finite then finite=finite[keep_x,keep_y]
           end 
        endcase 
     endif 
     
     if self.non_finite then begin 
        wh=where(finite,nt)
        if nt eq 0 then return
        ;; sort the values
        s=take[wh[sort(take[wh])]]
     endif else begin 
        s=take[sort(take)]
        nt=n_elements(take) 
     endelse 
     mx=s[((nt-1)*([99,95])[self.trim_mode-1]/100)<(nt-2)]
     mn=s[((nt-1)*([1,5])[self.trim_mode-1]/100)>1]
  endelse
  
  
  if self.non_finite then finite=where(*self.finite)
  switch self.scale_mode of 
     0: begin                   ; linear
        if mx gt mn then self.oDraw->SetDrawMinMax,MIN=mn,MAX=mx
        break
     end
     
     1: begin                   ;sqrt
        if self.non_finite then $
           (*im)[finite]=sqrt((mn>(*im)[finite])-mn) $
        else *im=sqrt((mn>*im)-mn)
        
        if mx gt mn then self.oDraw->SetDrawMinMax,MIN=0.,MAX=sqrt(mx-mn)
        break
     end
     
     2: begin                   ;logarithm
        if self.non_finite then $
           (*im)[finite]=alog10((mn>(*im)[finite])-mn+(mx-mn)*.05) $
        else *im=alog10((mn>*im)-mn+(mx-mn)*.05)
        if mx gt mn then $
           self.oDraw->SetDrawMinMax,MIN=alog10((mx-mn)*.05), $
                                     MAX=alog10((mx-mn)*(1.+.05))
        break
     end
     
     3: begin                   ;histeq
        if mx le mn then return
        bs=(mx-mn)/500
        h=histogram(*im,BINSIZE=bs,MIN=mn,MAX=mx,NAN=self.non_finite)
        h[0]=0.                 ;don't elevate the background
        h=total(temporary(h),/CUMULATIVE,/DOUBLE)
        self.oDraw->SetDrawMinMax,MIN=0,MAX=max(h)
        if self.non_finite then $
           (*im)[finite]=h[((mn>(*im)[finite]<mx)-mn)/bs] $
        else *im=h[((mn>*im<mx)-mn)/bs]
        break
     end
  endswitch
  self.min=mn & self.max=mx
end

;=============================================================================
;  PlotFreezeLock - Show the freeze lock line in the colorbar.
;=============================================================================
pro tvHist::PlotFreezeLock,NO_SET_WIN=nsw
  if keyword_set(nsw) eq 0 then begin 
     oldwin=!D.Window
     self.oCol->SetWin
  endif
  sym_x=[1,0,0,1,2,2,2,3,5,6,6,5,5,5,3,3,7,8,8,7,4,4,5,5,3,3,4,4,1]
  sym_y=[8,6,4,3,3,2,1,0,0,1,3,3,2,1,1,3,3,4,6,8,8,6,5,4,4,5,6,8,8]
  usersym,(4.-sym_x)/2.,(4.-sym_y)/2.,/FILL
  plots,!D.X_SIZE-10,!D.Y_SIZE-8,/DEVICE,COLOR=self.color,PSYM=8,SYMSIZE=1.2
  if keyword_set(nsw) eq 0 then wset,oldwin
end

;=============================================================================
;  CalcHist - Show the histogram line in the colorbar.
;=============================================================================
pro tvHist::CalcHist,im
  self.oCol->GetProperty,TOP=top,BOTTOM=bottom
  *self.imhist= $
     histogram(*im,MIN=bottom,MAX=top,BINSIZE=float(top-bottom)/self.nbins>1, $
               /NAN)
  nh=n_elements(*self.imhist)
  ;; plot scale neglects endpoints (which can have large counts).
  self.imhist_max=max((*self.imhist)[1:nh-2])
end

;=============================================================================
;  PlotHist - Plot the histogram computed
;=============================================================================
pro tvHist::PlotHist
  oldwin=!D.Window
  self.oCol->SetWin
  plot,*self.imhist,POSITION=[0.,0.,1.,1.-15./!D.Y_SIZE], $
       XRANGE=[0,n_elements(*self.imhist)-1],YRANGE=[0,self.imhist_max], $
       XSTYLE=1,XTICKLEN=0,YSTYLE=5,/NOERASE,PSYM=10,COLOR=self.color
  
  ;; show the maximum histogram value
  xyouts,2,!D.Y_SIZE-12,string(FORMAT='("Max: ",I0)',self.imhist_max), $
         COLOR=self.color,CHARSIZE=1.2,/DEVICE,FONT=-1
  xyouts,!D.X_SIZE-20,!D.Y_SIZE-12,ALIGNMENT=1.0, $
         string(FORMAT='(G0.4,"!96!X",G0.4)',self.min,self.max),/DEVICE, $
         COLOR=self.color,CHARSIZE=1.2,FONT=-1
  if self.freeze then self->PlotFreezeLock,/NO_SET_WIN 
  wset,oldwin  
end
  
;=============================================================================
;  Start - Find the color object if none passed
;=============================================================================
pro tvHist::Start
  self->tvPlug::Start
  if NOT obj_valid(self.oCol) then begin 
     test=self.oDraw->GetMsgObjs(CLASS='tvColor')
     if NOT obj_valid(test[0]) then begin
        message,'No Color plug-in registered.'
        return
     endif 
     self.OCol=test[0]
     self.oCol->MsgSignup,self,/TVCOLOR_REDRAW
  endif 
end

;=============================================================================
;  Cleanup
;=============================================================================
pro tvHist::Cleanup
  ptr_free,self.imhist,self.finite
  self->tvPlug::Cleanup
end

;=============================================================================
;  Init - Initialize the histogram with a tvDraw object.  All tvRBox
;         keywords are relevant (see tvrbox).
;=============================================================================
function tvHist::Init,oDraw,NBINS=nb,SCALING_MENU=smenu,_EXTRA=e
  if (self->tvPlug::Init(oDraw) ne 1) then return,0 ;chain up
  if n_elements(nb) eq 0 then self.nbins=200 else self.nbins=nb
  ;; Get a tvrbox object, signing ourselves up for box messages from it.
  self.box=obj_new('tvRBox', oDraw,/CORNERS,/SNAP,/NO_REDRAW,_EXTRA=e)
  self.Box->MsgSignup,self,/BOX
  self.Box->GetProperty,COLOR=col
  self.color=col                ;for drawing the histogram over the color-bar
  
  if keyword_set(smenu) then begin
     scal_menu=widget_button(smenu,VALUE='Scale Image',/MENU, $
                             UVALUE={Obj:self,Method:"ScaleEvent"}, $
                             EVENT_PRO='tvHist_event')
     self.wScalButs= $
        [widget_button(scal_menu,value='Linear',uvalue=0b,/CHECKED_MENU), $
         widget_button(scal_menu,value='Square Root',uvalue=1b,/CHECKED_MENU),$
         widget_button(scal_menu,value='Logarithmic',uvalue=2b,/CHECKED_MENU),$
         widget_button(scal_menu,value='Histogram Equalization',uvalue=3b, $
                       /CHECKED_MENU)]
     
     self.wTrimButs= $
        [widget_button(scal_menu,value='Trim 1%',uvalue=4b,/CHECKED_MENU, $
                       /SEPARATOR), $
         widget_button(scal_menu,value='Trim 5%',uvalue=5b,/CHECKED_MENU)]
     widget_control,self.wScalButs[0],/SET_BUTTON
     but=widget_button(smenu,VALUE='Set Scale Range...', $
                       UVALUE={Obj:self,Method:"SetScaleRange"}, $
                       EVENT_PRO='tvHist_event',/SEPARATOR)
     self.wFreeze=widget_button(smenu,VALUE='Freeze Scaling',/CHECKED_MENU, $
                                UVALUE={Obj:self,Method:"Freeze"},$
                                EVENT_PRO='tvHist_event')
  endif 
    
  self.imhist=ptr_new(/ALLOCATE_HEAP)
  self.oDraw->MsgSignup,self,/DRAW_KEY,/TVDRAW_PREDRAW,/TVDRAW_POSTDRAW
  self->Off
  return,1
end

;=============================================================================
;  tvHist__define -  Prototype the tvHist class.
;=============================================================================
pro tvHist__define
  struct={tvHist, $ 
          INHERITS tvPlug, $    ;make it a plug-in
          imhist:ptr_new(), $   ;the image histogram
          imhist_max:0L, $      ;the maximum histogram value
          min:0.0, $            ;the minimum scaling value
          max:0.0, $            ;the maximum scaling value
          finite:ptr_new(), $   ;finite map
          non_finite:0, $       ;efficiency flag if any non-finite
          nbins:0, $            ;number of bins, defaults to 100
          scale_mode: 0b, $     ;0:linear  1:sqrt 2:log 3:histeq
          trim_mode: 0b, $       ;0: no trim, 1:99% trim, 2:95% trim
          wScalButs:lonarr(4), $ ;the scaling widget buttons
          wTrimButs:lonarr(2), $ ;the trimming buttons
          freeze:0, $           ;whether to freeze the scaling
          wFreeze:0L, $         ;the freeze menu item
          oCol:obj_new(), $     ;a tvColor object to use
          color:0, $            ;the color to draw the histogram with
          box:obj_new()}        ;a tvRBox to use.
  return
end
