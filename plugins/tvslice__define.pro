;;**************************OverRiding methods********************************
;; Motion and button messages expected
pro tvSlice::Message,msg
  self->tvPlug::Message,msg,TYPE=type
  case type of 
     'DRAW_MOTION': begin 
        if self.buttondown ne 0b then begin 
           pt=self.oDraw->Convert([msg.X,msg.Y],/SHOWING)
           if pt[0] eq -1 then return
           if total(self.ept eq pt) eq 2. then return
           self->EraseLine
           if self.constrain then begin 
              if abs(pt[1]-self.opt[1]) gt abs(pt[0]-self.opt[0]) then $
                 pt[0]=self.opt[0] $ ; up-down 
              else pt[1]=self.opt[1] ;left-right
           endif 
           self.ept=pt
           self->DrawLine
        endif
     end 
     
     'DRAW_BUTTON': begin 
        case msg.type of
           0: $                 ;button press
              begin  
              if self.opt[0] ne -1 then self->EraseLine 
              self.constrain=(msg.press AND 4b) ne 0 ;only up-down left right
              pt=self.oDraw->Convert([msg.X,msg.Y],/SHOWING)
              if pt[0] eq -1 then return
              self.opt=pt       ;the original point
              self.oDraw->MsgSignup,self,/DRAW_MOTION ;ask for motion events
              self.buttondown=1b
           end
           
           1: $                 ;button release
              begin 
              if self.buttondown eq 0b then return
              self.buttondown=0b
              ;; we need no more motion events
              self.oDraw->MsgSignup,self,DRAW_MOTION=0 
              if self.constrain then begin 
                 self.constrain=0b ;reset for next round
              endif else begin 
                 pt=self.oDraw->Convert([msg.X,msg.Y],/SHOWING)
                 if pt[0] ne -1 then self.ept=pt ;otherwise just use last one 
              endelse             
              if total(self.opt eq self.ept) eq 2. then return
              self.oDraw->SendRedraw ;probably overwrote stuff
              self->PlotSlice
           end
        endcase 
        break
     end 
     
     'TVDRAW_POSTDRAW':  begin 
        if NOT ptr_valid(self.plotvec) then return
        self->PlotSlice         ;we'll draw it when the redraw arrives.
        break
     end
     
     'TVDRAW_REDRAW': if ptr_valid(self.plotvec) then self->DrawLine
     else:
  endcase
end

;=============================================================================
;	On:  Get all our messages.
;=============================================================================
pro tvSlice::On
  if self.active then begin     ;if turned on *again* .. means off
     self->Off                  
     return
  endif
  self->tvPlug::On
  self.oDraw->MsgSignup,self,/DRAW_BUTTON,/DRAW_MOTION,/TVDRAW_REDRAW, $
     /TVDRAW_POSTDRAW
end

;=============================================================================
;       Off: (But if we have a box drawn, keep processing).
;=============================================================================
pro tvSlice::Off
  self->tvPlug::Off
  if self.opt[0] ne -1 then self->EraseLine
  self.plotpt=[-1,-1]           ;no longer plotting it
  if widget_info(self.wBase,/VALID) then  $
     widget_control, self.wBase,/DESTROY
  self.oDraw->MsgSignup,self,/NONE
end


function tvSlice::Icon
  return,[[000B, 000B],[000B, 096B],[000B, 080B],[000B, 040B],$
          [000B, 020B],[000B, 010B],[000B, 005B],[128B, 002B],$
          [064B, 001B],[174B, 000B],[092B, 000B],[056B, 000B],$
          [124B, 000B],[110B, 000B],[070B, 000B],[000B, 000B]]
end

;;************************End OverRiding methods*******************************

pro tvslice_plot_event,ev
  widget_control, ev.top, get_uvalue=self
  self->PlotEvent,ev
end

pro tvSlice::PlotEvent,ev
  type=tag_names(ev,/STRUCTURE_NAME)
  if type eq 'WIDGET_TRACKING' then begin 
     if ev.enter eq 0 then begin ; Leaving
        self->EraseIndicator    ;erase
        self.oDraw->GetProperty,DRAWWIN=dw
        wset,dw
        self->DrawLine
        self.plotpt=[-1,-1]     ;indicate we're not plotting anymore
     endif 
     return
  endif 
  ;; Motion events -- convert to data coords, nearest index
  self.oDraw->GetProperty,IMORIG=io
  c=(convert_coord(ev.X,ev.Y,/DEVICE,/TO_DATA))[0:1]
  c[0]=round((0>c[0])<(n_elements(*self.plotvec)-1))
  if self.plotpt[0] eq c[0] then return
  c[1]=(*io)[(*self.plotvec)[c[0]]] ;the data value there
  if self.plotpt[0] ne -1 then self->EraseIndicator ;Erase old one
  self.plotpt=c
  self->ShowIndicator           ;Draw the symbols
end

pro tvSlice::ShowIndicator, IMONLY=il
  self.oDraw->GetProperty,DRAWWIN=win,SIZE=sz
  wset,win
  self->DrawLine                ;Some of it got erased probably
  indx=(*self.plotvec)[self.plotpt[0]]
  x=indx mod sz[0] & y=indx/sz[0]
  self.ImCoords=self.oDraw->Convert([x,y],/DEVICE)
  plots,self.ImCoords,/DEVICE,PSYM=1,SYMSIZE=1.5,THICK=1.5,COLOR=self.color
  if n_elements(il) eq 0 then begin 
     wset,self.plotwin
     plots,[self.plotpt[0],self.plotpt[0]],!Y.CRANGE,/DATA,COLOR=self.color
     plots,self.plotpt,PSYM=7,SYMSIZE=1.5,THICK=1.5,COLOR=self.color,/DATA
  endif else wset,self.plotwin
end

;=============================================================================
;       tvSlice::EraseIndicator -  Erase the plot point indicators
;=============================================================================
pro tvSlice::EraseIndicator
  ;; Erase indicator + vertical line on the plot window
  wset,self.plotwin
  low=(convert_coord([self.plotpt[0],!Y.CRANGE[0]],/DATA,/TO_DEVICE))[0:1]-6
  high=(convert_coord([self.plotpt[0],!Y.CRANGE[1]], $
                      /DATA,/TO_DEVICE))[0:1]+6
  dist=high-low+1
  device,copy=[low,dist,low,self.plotpixwin]
  
  ;; Erase indicator on the image window
  self.oDraw->GetProperty,WINSIZE=winsize
  low=self.ImCoords-6>0
  dist=[13,13]<(winsize-low)>1
  self.oDraw->Erase,low,dist
end

pro tvSlice::PlotWin
  self.oDraw->GetProperty,DRAWWIDGET=dw,DRAWWIN=save
  repeat begin
     tlb=dw
     dw=widget_info(dw,/PARENT) 
  endrep until dw eq 0
  geom=widget_info(tlb,/GEOMETRY) 
  ss=get_screen_size()
  
  xpos=(geom.xoffset+geom.scr_xsize/2+geom.margin) gt ss[0]/2?  $
     geom.xoffset-2*geom.margin-512-23: $
     geom.xoffset+2*geom.margin+geom.scr_xsize+5
  ypos=geom.yoffset
  
  self.wBase=widget_base(/COLUMN,TITLE='Slice',XOFFSET=xpos,YOFFSET=ypos)
  self.wPlot=widget_draw(self.wBase,XSIZE=512,YSIZE=384,/TRACKING,/MOTION)
  widget_control, self.wBase,set_uvalue=self,/REALIZE
  widget_control, self.wPlot,get_value=win
  self.plotwin=win
  wset,save
  XManager,'tvSlice_Plot',self.wBase,/NO_BLOCK, $
           EVENT_HANDLER='tvslice_plot_event',GROUP_LEADER=tlb
end


;=============================================================================
;       tvSlice::PlotSlice.  Plot the sliced line
;=============================================================================
pro tvSlice::PlotSlice
  ;; Start it up, if it's  not yet started.
  if widget_info(self.wBase,/VALID) eq 0 then self->PlotWin
  wset,self.plotwin
  self.oDraw->GetProperty,IMORIG=io,SIZE=sz,DRAWWIN=dwin
  d=float(self.ept-self.opt)
  n=floor(sqrt(d[0]^2+d[1]^2))

  x=round(findgen(n)/(n-1)*d[0]+self.opt[0])
  y=round(findgen(n)/(n-1)*d[1]+self.opt[1])
  wh=where(x ge 0 and x lt sz[0],cnt)
  if cnt eq 0 then begin
     wset,dwin
     return
  endif 
  x=x[wh] & y=y[wh]
  wh=where(y ge 0 and y lt sz[1],cnt)
  if cnt eq 0 then begin
     wset,dwin
     return
  endif 
  x=x[wh] & y=y[wh]
  n=n_elements(x) 
  if ptr_valid(self.plotvec) then *self.plotvec=x+y*sz[0] else  $
     self.plotvec=ptr_new(x+y*sz[0])
  ttl=string(FORMAT='("[",I3,",",I3,"] to [",I3,",",I3,"]")', $
             x[0],y[0],x[n-1],y[n-1])
  plot,indgen(n),(*io)[*self.plotvec],xtitle="Slice Index",ytitle='Value',$
       TITLE=ttl,xstyle=17,XMINOR=n lt 10?-1:0
  wset,self.plotpixwin
  device,copy=[0,0,512,384,0,0,self.plotwin] ;save it for redrawing
  wset,dwin
end

;=============================================================================
;       tvSlice::DrawLine. Draw the slice line
;=============================================================================
pro tvSlice::DrawLine
  ;; Pixel to device coordinates
  bg=self.oDraw->Convert(self.opt,/DEVICE)
  en=self.oDraw->Convert(self.ept,/DEVICE)
  plots,[bg[0],en[0]],[bg[1],en[1]],COLOR=self.color,$
        THICK=1.5,/DEVICE
end

;=============================================================================
;       tvSlice::EraseLine.  Erase the slice line
;=============================================================================
pro tvSlice::EraseLine
  bg=self.oDraw->Convert(self.opt,/DEVICE)
  en=self.oDraw->Convert(self.ept,/DEVICE)
  self.oDraw->GetProperty,WINSIZE=winsize
  high=bg>en+1
  low=(en<bg-1)>0
  dist=(high-low+1) < (winsize-low) > 1
  self.oDraw->Erase,low,dist
end

pro tvSlice::Cleanup
  if widget_info(self.wBase,/VALID) then  $
     widget_control, self.wBase,/DESTROY
  ptr_free,self.plotvec
  wdelete,self.plotpixwin
  self->tvPlug::Cleanup         ;Chain Up
end

;=============================================================================
;       tvSlice::Init.  Initialize the slicer object
;=============================================================================
function tvSlice::Init,oDraw,COLOR=color
  self.opt=[-1,-1]
  self.ept=[-1,-1]
  self.plotpt=[-1,-1]
  
  if (self->tvPlug::Init(oDraw) ne 1) then return,0 ;chain up
  
  if n_elements(color) eq 0 then self.color=!D.TABLE_SIZE/2 else  $
     self.color=color
  ;; Get a pixmap for the plot window
  window,/Free,XSIZE=512,YSIZE=384,/PIXMAP 
  self.plotpixwin=!D.Window
  self->Off
  return,1
end

;=============================================================================
;       tvSlice__define.  Prototype the tvSlice class.
;=============================================================================
pro tvSlice__define
  struct={tvSlice, $ 
          INHERITS tvPlug,$     ;make it a plug-in
          wBase:0L, $           ;the base for the plot window
          wPlot:0L, $           ;widget id of the draw widget
          plotwin:0L, $         ;window id of above
          plotpixwin:0L, $      ;the pixmap window for the plot
          plotpt:[0.0,0.0], $   ;x device coordinate of cursor on plot
          plotvec:ptr_new(), $  ;the indices into image plotted
          ImCoords:[0,0], $     ;Device coords on image of displayed symbol
          color:0, $            ;what color to draw in
          buttondown:0, $       ;is the button held down
          opt:[0,0],$           ;original point (pixel coordinates)
          ept:[0,0], $          ;end point of line (pixel coordinates)
          constrain:0b}         ;with right button, constrain to up-down/l-r
end
