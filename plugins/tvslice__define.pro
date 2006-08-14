;+
; NAME:
;
;    tvSlice
;
; DESCRIPTION:
;
;    A tvTools plugin to form image slices and plot the cuts.
;
; CATEGORY:
;
;    tvTools, Image Slices
;
; SIDE EFFECTS:
;
;    Draws a cut line, and pops up a plot widget to diplay the cut,
;    with motion-based feedback between the pixel position and cut
;    position.  Right-click to constrain to horizontal/vertical.
;
; METHODS:
;
;    Init:  
;
;       CALLING SEQUENCE:
;
;          obj=obj_new('tvSlice',oDraw,[COLOR=,_EXTRA=e])
;          
;       INPUT PARAMETERS:
;
;	   oDraw: The tvDraw object.
;
;       INPUT KEYWORD PARAMETERS:
;
;          COLOR: The color to draw the slice line with.
;
;          _EXTRA: Any other ObjMsg initialization keywords
;             (e.g. message list).
;          
; INHERITANCE TREE:
;
;    ObjMsg-->tvPlug-->tvSlice
;
; MODIFICATION HISTORY:
;
;    2001-08-07 (J.D. Smith): Imported from SCORE-era source.
;       
;-
;    $Id$
;##############################################################################
;
; LICENSE
;
;  Copyright (C) 2001,2003,2004,2005 J.D. Smith
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
;;***************************************************************************
pro tvSlice::GetProperty, opt=opt, ept=ept
  if arg_present(ept) then ept=self.ept
  if arg_present(opt) then opt=self.opt
end


;**************************OverRiding methods********************************
;; Motion and button messages expected
pro tvSlice::Message,msg
  self->tvPlug::Message,msg,TYPE=type
  case type of 
     'DRAW_MOTION': begin 
        if self.buttondown ne 0b then begin 
           pt=self.oDraw->Convert([msg.X,msg.Y],/SHOWING)
           if pt[0] eq -1 then return
           if array_equal(self.ept,pt) then return
           if self.widen then begin 
              width=self->Width(pt)
              if self.width eq width then return
              self->EraseLine              
              self.width=width
           endif else begin 
              self->EraseLine
              if self.constrain || (msg.modifiers AND 2b) ne 0b then begin 
                 delx=abs(pt[0]-self.opt[0])
                 dely=abs(pt[1]-self.opt[1])
                 del=round(0.5*(delx+dely))>1
                 if abs(delx-dely)/del lt .5 then begin 
                    pt[0]=self.opt[0]+del*(pt[0] gt self.opt[0]?1:-1)
                    pt[1]=self.opt[1]+del*(pt[1] gt self.opt[1]?1:-1)
                 endif else if dely gt delx then $
                    pt[0]=self.opt[0] $ ; up-down 
                 else pt[1]=self.opt[1] ;left-right
              endif 
              self.ept=pt
           endelse 
           self->DrawLine
           self->PlotSlice
        endif
     end 
     
     'DRAW_BUTTON': begin 
        case msg.type of
           0: $                 ;button press
              begin  
              if self.opt[0] ne -1 then self->EraseLine 
              self.constrain=(msg.press AND 4b) ne 0
              self.width=0.0
              pt=self.oDraw->Convert([msg.X,msg.Y],/SHOWING)
              if pt[0] eq -1 then return
              
              self.buttondown=1b
              
              ;; With shift or middle click, widen
              if ((msg.press AND 2b) ne 0 || msg.modifiers AND 1b) && $
                 self.ept[0] ne -1 then begin 
                 self.widen=1b
                 width=self->Width(pt)
                 if self.width ne width then begin 
                    self->EraseLine
                    self.width=width
                    self->DrawLine
                 endif 
              endif else self.opt=pt                  ;the original point
              self.oDraw->MsgSignup,self,/DRAW_MOTION ;ask for motion events
              self.oDraw->SendRedraw
           end
           
           1: $                 ;button release
              begin 
              if self.buttondown eq 0b then return
              self.buttondown=0b
              ;; we need no more motion events
              self.oDraw->MsgSignup,self,DRAW_MOTION=0 
              if ~self.constrain && ~self.widen && $
                 (msg.modifiers AND 3b) eq 0b then begin 
                 pt=self.oDraw->Convert([msg.X,msg.Y],/SHOWING)
                 if pt[0] ne -1 then self.ept=pt ;otherwise just use last one 
              endif   
              self.constrain=0b & self.widen=0b
              if array_equal(self.opt,self.ept) then return
              self.oDraw->SendRedraw ;probably overwrote stuff
              self->PlotSlice
           end
        endcase 
        break
     end 
     
     'TVDRAW_POSTDRAW':  begin 
        if ~ptr_valid(self.plotvec) then return
        self->PlotSlice         ;we'll draw it when the redraw arrives.
        widget_control, self.wBase,TLB_SET_TITLE=self.oDraw->Title()+' - Slice'
        break
     end
     
     'TVDRAW_REDRAW': begin 
        if ptr_valid(self.plotvec) AND self.buttondown eq 0b then $
           self->DrawLine
     end 
     
     'TVDRAW_SNAPSHOT': begin 
        if ptr_valid(self.plotvec) AND self.buttondown eq 0b then $
           self->DrawLine
     end 
     else:
  endcase
end

;=============================================================================
;	On:
;=============================================================================
pro tvSlice::On
  if self->On() then begin      ;if turned on *again*, reset
     self->Reset                  
     return
  endif
  self->tvPlug::On
  self.oDraw->MsgSignup,self,/DRAW_BUTTON,/TVDRAW_REDRAW,/TVDRAW_POSTDRAW, $
                        TVDRAW_SNAPSHOT=0
  self.oDraw->ReDraw,/SNAPSHOT
end

;=============================================================================
;       Off: 
;=============================================================================
pro tvSlice::Off
  self->tvPlug::Off
  self.oDraw->MsgSignup,self,/NONE,/TVDRAW_POSTDRAW,/TVDRAW_SNAPSHOT
  if self.oDraw->IsDrawn() then self.oDraw->ReDraw,/SNAPSHOT
end

;=============================================================================
;       Reset: 
;=============================================================================
pro tvSlice::Reset
  showing=self.oDraw->IsDrawn()
  if self.opt[0] ne -1 && showing then self->EraseLine
  self.plotpt=[-1,-1]           ;no longer plotting it
  self.opt=[-1,-1]
  self.ept=[-1,-1]
  ptr_free,self.plotvec
  if widget_info(self.wBase,/VALID) then widget_control, self.wBase,/DESTROY
  self->tvPlug::Off
  if showing then begin 
     self.oDraw->MsgSignup,self,/NONE
     self.oDraw->ReDraw,/SNAPSHOT
  endif 
end


function tvSlice::Icon
  return,[[000B, 000B],[000B, 096B],[000B, 080B],[000B, 040B],$
          [000B, 020B],[000B, 010B],[000B, 005B],[128B, 002B],$
          [064B, 001B],[174B, 000B],[092B, 000B],[056B, 000B],$
          [124B, 000B],[110B, 000B],[070B, 000B],[000B, 000B]]
end

function tvSlice::Description
  return,'Line Slicing'
end

function tvSlice::MouseHelp
  return,['Slice','SetWidth','XYConstrained']
end
;;************************End OverRiding methods*******************************

pro tvslice_plot_event,ev
  widget_control, ev.top, get_uvalue=self
  self->PlotEvent,ev
end

pro tvslice_plot_kill,id
  widget_control, id,GET_UVALUE=self
  self->Reset
end

;=============================================================================
;  PlotEvent -  Update indicator and overlay point
;=============================================================================
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
  self->ResetScale
  c=(convert_coord(ev.X,ev.Y,/DEVICE,/TO_DATA))[0:1]
  n=(size(*self.plotvec,/DIMENSIONS))[1]
  c[0]=round((0>c[0])<(n-1))
  if self.plotpt[0] eq c[0] || c[0] ge n then return
  c[1]=(*self.plotvec)[1,c[0]] ;the data value there
  if self.plotpt[0] ne -1 then self->EraseIndicator ;Erase old one
  self.plotpt=c
  self->ShowIndicator           ;Draw the symbols
end

;=============================================================================
;  ShowIndicator -  Show the overlay indicator
;=============================================================================
pro tvSlice::ShowIndicator, IMONLY=il
  self.oDraw->GetProperty,DRAWWIN=win,SIZE=sz
  wset,win
  self.oDraw->SendRedraw ;some of it got erased probably
  indx=long((*self.plotvec)[0,self.plotpt[0]])
  x=indx mod sz[0] & y=indx/sz[0]
     
  self.ImCoords=self.oDraw->Convert([x,y],/DEVICE)
  plots,self.ImCoords,/DEVICE,PSYM=1,SYMSIZE=1.5,THICK=1.5,COLOR=self.color
  
  if self.width ne 0.0 then begin 
     self->FlankingPoints,x,y,fx,fy
     nf=n_elements(fx) 
     c=lindgen(2,nf)
     for i=0,nf-1 do $
        c[0,i]=self.oDraw->Convert([fx[i],fy[i]],/DEVICE)
     plots,c,PSYM=3,COLOR=self.color,/DEVICE
  endif
  
  if n_elements(il) eq 0 then begin 
     wset,self.plotwin
     self->ResetScale
     plots,[self.plotpt[0],self.plotpt[0]],self.range,/DATA,COLOR=self.color
     plots,self.plotpt,PSYM=7,SYMSIZE=1.5,THICK=1.5,COLOR=self.color,/DATA
  endif else wset,self.plotwin
end

;=============================================================================
;  EraseIndicator -  Erase the plot point indicators
;=============================================================================
pro tvSlice::EraseIndicator
  ;; Erase indicator + vertical line on the plot window
  wset,self.plotwin
  self->ResetScale
  low=(convert_coord([self.plotpt[0],self.range[0]],/DATA,/TO_DEVICE))[0:1]-6
  high=(convert_coord([self.plotpt[0],self.range[1]], $
                       /DATA,/TO_DEVICE))[0:1]+6
  dist=high-low+1
  device,copy=[low,dist,low,self.plotpixwin]
  
  ;; Erase indicator on the image window
  self.oDraw->GetProperty,WINSIZE=winsize,ZOOM=zm
  low=self.ImCoords-6-self.width*zm>0
  dist=[13,13]+(2*zm*self.width)<(winsize-low)>1
  self.oDraw->Erase,low,dist
end

;=============================================================================
;  Width -  Determine width perpendicular to slice line of given point
;=============================================================================
function tvSlice::Width,pt
  width=abs(float(self.ept[0]-self.opt[0])*(self.opt[1]-pt[1])- $
            float(self.opt[0]-pt[0])*(self.ept[1]-self.opt[1]))/ $
             sqrt(total(float(self.ept-self.opt)^2))
  if width lt 0.5 then width=0.0
  return,width
end

;=============================================================================
;  FlankingPoints - Find the flanking points within width of the given
;                   line point(s).
;=============================================================================
pro tvSlice::FlankingPoints,x,y,fx,fy
  del=float(self.ept-self.opt)
  d=sqrt(total(del^2))
  ang=atan(abs(del[1]),abs(del[0]))*!RADEG
  if ang gt 45. then ang=90.-ang
  cos_term=cos(ang/!RADEG)
  nw=(2*ceil(self.width*cos_term)+1)>2
  n=n_elements(x) 
  off=float(lindgen(nw)-nw/2)/((nw-1)/2)*self.width
  offx=off*(-del[1]/d)
  offy=off*( del[0]/d)
  all=make_array(n,nw,VALUE=!VALUES.F_NAN)
  fx=round(rebin([x],n,nw)+rebin(transpose(offx),n,nw))
  fy=round(rebin([y],n,nw)+rebin(transpose(offy),n,nw))
end


pro tvSlice::ResetScale
  !X.S=self.scale[0:1] & !Y.S=self.scale[2:3]
end

pro tvSlice::PlotWin
  self.oDraw->GetProperty,DRAWWIDGET=dw,DRAWWIN=save
  repeat begin
     tlb=dw
     dw=widget_info(dw,/PARENT) 
  endrep until dw eq 0
  geom=widget_info(tlb,/GEOMETRY) 
  ss=get_screen_size()
  
  self.wBase=widget_base(/COLUMN,TITLE=self.oDraw->Title()+' - Slice')
  self.wPlot=widget_draw(self.wBase,XSIZE=512,YSIZE=384,/TRACKING,/MOTION)
  widget_control, self.wBase,set_uvalue=self,/REALIZE
  
  gb=widget_info(self.wBase,/GEOMETRY)
  xpos=(geom.xoffset+geom.scr_xsize/2) gt ss[0]/2?  $
       geom.xoffset-2*geom.margin-gb.scr_xsize-6*gb.xpad: $
       geom.xoffset+2*geom.margin+geom.scr_xsize+2*geom.xpad
  ypos=geom.yoffset-(geom.scr_ysize-geom.ysize+2*geom.ypad)
  widget_control, self.wBase,XOFFSET=xpos,YOFFSET=ypos
  widget_control, self.wPlot,get_value=win
  self.plotwin=win
  wset,save
  XManager,'tvSlice_Plot',self.wBase,/NO_BLOCK, $
           EVENT_HANDLER='tvslice_plot_event',GROUP_LEADER=tlb, $
           CLEANUP='tvslice_plot_kill'
end


;=============================================================================
;       tvSlice::PlotSlice.  Plot the sliced line
;=============================================================================
pro tvSlice::PlotSlice
  ;; Start it up, if it's  not yet started.
  if widget_info(self.wBase,/VALID) eq 0 then self->PlotWin
  wset,self.plotwin
  self.oDraw->GetProperty,IMORIG=io,SIZE=sz,DRAWWIN=dwin
  del=float(self.ept-self.opt)
  d=sqrt(total(del^2))
  n=floor(d)
  if n lt 2 then return
  x=round(findgen(n)/(n-1)*del[0]+self.opt[0])
  y=round(findgen(n)/(n-1)*del[1]+self.opt[1])

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
    
  ttl=string(FORMAT='("[",I4,",",I4,"] to [",I4,",",I4,"]")', $
             x[0],y[0],x[n-1],y[n-1])
  
  
  inds=x+y*sz[0]
  ;; At each position, span width
  if self.width ne 0.0 then begin 
     self->FlankingPoints,x,y,allx,ally
     all=(*io)[allx,ally]
     
     vec=total(all,2,/NAN)/(total(finite(all),2)>1.)
     ttl+=string(FORMAT='("  width: ",F0.1)',self.width)
  endif else vec=(*io)[inds]
  
  if ptr_valid(self.plotvec) then $
     *self.plotvec=transpose([[inds],[vec]]) else  $
        self.plotvec=ptr_new(transpose([[inds],[vec]]))

  plot,indgen(n),vec,xtitle="Slice Index",ytitle='Value',$
       TITLE=ttl,/XSTYLE,XMINOR=n lt 10?-1:0,XMARGIN=[8,2],YMARGIN=[3,2]
  self.scale=[!X.S,!Y.S] & self.range=!Y.CRANGE
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
  
  if self.width ne 0.0 then begin 
     del=self.opt-self.ept
     off=self.width*[-del[1],del[0]]/sqrt(total(float(del)^2))
     for s=-1,1,2 do begin 
        bg=self.oDraw->Convert(self.opt+s*off,/DEVICE)
        en=self.oDraw->Convert(self.ept+s*off,/DEVICE)
        plots,[bg[0],en[0]],[bg[1],en[1]],COLOR=self.color,$
              THICK=1.5,/DEVICE,LINESTYLE=2
     endfor 
  endif 
end

;=============================================================================
;       tvSlice::EraseLine.  Erase the slice line
;=============================================================================
pro tvSlice::EraseLine
  bg=self.oDraw->Convert(self.opt,/DEVICE)
  en=self.oDraw->Convert(self.ept,/DEVICE)
  high=bg>en+1
  low=(en<bg-1)>0
  if self.width ne 0.0 then begin 
     del=self.opt-self.ept
     off=self.width*[-del[1],del[0]]/sqrt(total(float(del)^2))
     for s=-1,1,2 do begin 
        bg=self.oDraw->Convert(self.opt+s*off,/DEVICE)
        en=self.oDraw->Convert(self.ept+s*off,/DEVICE)
        high>=bg>en+1
        low<=(en<bg-1)>0
     endfor
  endif
     
  self.oDraw->GetProperty,WINSIZE=winsize
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
  self.width=-1.
  
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
          scale:dblarr(4),$     ;the x,y data->device scaling parameters
          range:fltarr(2), $    ;the saved y range after plot
          width:0.0, $          ;width (in pixels) of the slice
          constrain:0b,$        ;with right button, constrain to up-down/l-r
          widen:0b}             ;with middle button, widen area of averaging
end
