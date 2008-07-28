;+
; NAME:
;
;    tvrBox
;
; DESCRIPTION:
;
;    A tvTools plugin to manage a resizeable, movable selection box.
;
; CATEGORY:
;
;    tvTools, Selection Box.
;
; METHODS:
;
;    Init:  
;
;       CALLING SEQUENCE:
;
;          obj=obj_new('tvrBox',oDraw,[COLOR=,THICK=,HANDLE=,
;                      KNOBRAD=,HSIZE=,CORLEN=,BUTTON=,/CORNERS,
;                      /HANDCURS,/ON_MOTION,/SNAP,/NO_REDRAW,_EXTRA=e])
;          
;       INPUT PARAMETERS:
;
;	   oDraw: The tvDraw object.
;
;          parent: The widget ID where the line reporting label will
;            be placed.
;	   
;       INPUT KEYWORD PARAMETERS:
;
;          COLOR: The color to use for drawing.
;
;          THICK: The line thickness used for drawing the box.
;
;          HANDLE: Type of handle to draw
;  	 	     0: circle (default)
;  		     1: triangle
;  		     2: square
;
;          KNOBRAD: Radius around lower right point which defines the
;                   resize knob if clicked within (default 5 device
;                   pixels).
;
;          HSIZE: Size of handle knob (SYMSIZE, default 1).
;
;          BUTTON: Which button to use to move/resize box either
;             0 (left, default), 1 (middle), 2 (right).
;
;          CORNERS: If set, display corners when turned off.
;
;          HANDCURS: If set display a hand cursor when moving the box.
;
;          ON_MOTION: If set, deliver messages when the box moves (not
;             just when it is released).
;
;          SNAP: If set, snap the box to pixel boundaries at any zoom.
;
;          NO_REDRAW: If set, don't attempt to redraw ourselves
;             (presumably because redraw is being done elsewhere).
;
;          _EXTRA: Any other ObjMsg initialization keywords
;             (e.g. message list).
;
; NOTES
;
;   The layout of the tvRBox is:
;  
;   Offset Corner  +----------+
;                  |          |
;                  |          |
;                  |          |
;                  |          |
;                  |          |
;                  |          |
;                  +----------o  Handle Corner
;
;
;
; INHERITANCE TREE:
;
;    ObjMsg-->tvPlug-->tvrBox
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
pro tvRBox::Message, msg
  type=tag_names(msg,/STRUCTURE_NAME)
  case type of
     'DRAW_MOTION': begin 
        if self.buttondwn ne 0b then self->MoveBox,msg.X,msg.Y
        self.oDraw->ClearEvents
     end 
        
     'DRAW_BUTTON': begin 
        case msg.type of
           0: begin             ;button press
              self.oDraw->SetWin
              ;;check if our button hit
              if (msg.press ne self.button) then return 
              ;; check and see if box drawn
              if self.boxflag ne -1 then begin ;if a box is already drawn
                 ;; check if "on knob" (flag=0b) or "in bounds" (flag=1b)
                 ;; or neither (simply return).
                 dist=float([msg.X,msg.Y])- $
                    (self.boxoff+[1,-1]*(self.boxsize-1))
                 if total(dist^2) le self.knobrad^2 then $
                    self.boxflag=0b $ ;on knob
                 else begin 
                    if dist[0] le 0 and dist[0] ge -self.boxsize[0] and $
                       dist[1] ge 0 and dist[1] le self.boxsize[1] then begin 
                       self.boxflag=1 ;in box
                       if self.handcurs then hand_cursor,/CENTER
                    endif else return
                 endelse 
                 if self.snap_mode then $
                    self.save_coords=self.oDraw->Convert([msg.X,msg.Y])
              endif else begin  ; box not yet drawn: drawing a new one
                 if self.snap_mode then begin 	
                    coords=self.oDraw->Convert([msg.X,msg.Y],/SHOWING)
                    if coords[0] eq -1 then return
                    self.coords=coords
                    self.save_coords=self.coords
                    self.size=[1,1]
                    self->DrawBox
                 endif else self.boxoff=[msg.X,msg.Y]
                 self.boxflag=0b ;drawing new box, just like on knob
              endelse
              
              ;;sign up for motion events.
              self.oDraw->MsgSignup,self,/DRAW_MOTION
              self.save=[msg.X,msg.Y] ;the save location
              
              ;; set the user symbol (in case it gets changed)
              usersym,*self.ux,*self.uy,/FILL
              self.buttondwn=1b
           end 
           
           1: begin             ;button release
              ;; do nothing if wrong button, no buttondown flagged, or no box
              ;; yet drawn
              if msg.release ne self.button or self.buttondwn eq 0b or  $
                 self.boxflag eq -1 then return
              self.oDraw->MsgSignup,self,DRAW_MOTION=0 ;turn off motion
              self.buttondwn=0b ;button is no longer down
              if self.boxflag eq 1 then $ ;full box move
                 if self.handcurs then device,/cursor_cross $
              else if self.boxflag eq 0 then $ ;box resize
                 self.boxsize=abs(self.save-self.boxoff)
              ;; A redraw message (since box might have erased other things)
              if ~self.no_redraw then self.oDraw->SendRedraw    
              self->SendBox     ;maybe send a box message
           end 
        endcase
     end
     
     'DRAW_KEY': begin 
        if self->IsDrawn() eq 0 then return
        if msg.type eq 5 then return ; only arrow keys of interest
        if msg.key lt 5 or msg.key gt 8 then return
        if msg.release then return ; press only
        move=([[-1,0],[1,0],[0,1],[0,-1]])[0:1,msg.key-5]
        if self.snap_mode then begin 
           self.oDraw->GetProperty,OFFSET=off, DISPSIZE=ds
           new_coords=[off[0],off[1]+self.size[1]-1] > $
                      (self.coords+move) < $
                      [off[0]+ds[0]-self.size[0],off[1]+ds[1]-1]
           if array_equal(new_coords,self.coords) then return
           self->EraseBox
           self.coords=new_coords
        endif else begin 
           self->EraseBox
           self.boxoff=[0,self.boxsize[1]] > $
                       (self.boxoff+self.zoom*move) < $
                       [self.winsize[0]-self.boxsize[0]-1,self.winsize[1]-1]
        endelse 
        self->DrawBox
        self->SendBox
     end

     'TVDRAW_POSTDRAW': begin
        ;;  a change of original image, or zoom, etc. occurred
        if self.boxflag eq -1 then return ; only if a box is here
        if NOT self.corners then self->SetUpDisplay
     end 
     
     'TVDRAW_REDRAW': begin     ; the screen was clobbered
        if self->IsDrawn() then begin ; only if a box here
           if self->On() then begin 
              usersym,*self.ux,*self.uy,/FILL
              self->DrawBox   
           endif else if self.corners then self->DrawCorners ;currently not on
        endif 
     end
     
     'TVDRAW_SNAPSHOT': begin 
        if self.corners then begin 
           self->SetUpDisplay
           self->DrawCorners
        endif 
     end
     
     'TVDRAW_RESIZE': begin 
        self.oDraw->GetProperty,WINSIZE=ws
        self.winsize=ws
     end 
  endcase 
end

pro tvRBox::On
  self->tvPlug::On
  ;; Button and redraw events, no more snapshots
  self.oDraw->MsgSignup,self,/DRAW_BUTTON,/DRAW_KEY,/TVDRAW_POSTDRAW, $
                        /TVDRAW_REDRAW,TVDRAW_SNAPSHOT=0,/TVDRAW_RESIZE
  usersym,*self.ux,*self.uy,/FILL
  if self->IsDrawn() then begin 
     if self.corners then self.oDraw->ReDraw,/SNAPSHOT $ ;remove bg corners
     else self->DrawBox         ;just turned on, no snapshot coming
  endif
  self->SetUpDisplay
end

pro tvRBox::Off 
  self.oDraw->MsgSignup,self,/NONE
  if self->On() && self->IsDrawn() then begin 
     if self.corners then begin 
        ;;corner redraws and new images needed
        self.oDraw->MsgSignup,self,/NONE,/TVDRAW_POSTDRAW,/TVDRAW_SNAPSHOT, $
                              /TVDRAW_RESIZE
        self.oDraw->ReDraw,/SNAPSHOT ;get our corners into the background
     endif else self->EraseBox
  endif 
  self->tvPlug::Off
end
;;************************End OverRiding methods*******************************

;; return status of box, -1 (never drawn) 0 (drawing new) or 1 (drawn)
function tvRBox::IsDrawn
  return, self.boxflag ne -1
end

;; Get the current box sides in pixel coordinates, truncated to actual sizes
pro tvRBox::GetLRTB, l, r, t, b
  if self.snap_mode eq 0 then begin 
     tmp=self.oDraw->Convert(self.boxoff,/TRUNCATE)
     l=tmp[0] & t=tmp[1]
     tmp=self.oDraw->Convert(self.boxoff+[1,-1]*self.boxsize,/TRUNCATE)
     r=tmp[0] & b=tmp[1]
  endif else begin              ; pixel bounding only
     l=self.coords[0] & r=l+self.size[0]-1
     t=self.coords[1] & b=t-(self.size[1]-1)
  endelse 
end

pro tvRBox::SetProperty, CORNERS=cor, COLOR=col, NO_REDRAW=nrd
  if n_elements(cor) ne 0 then self.corners=cor
  if n_elements(col) ne 0 then self.color=col
  if n_elements(nrd) ne 0 then self.no_redraw=keyword_set(nrd) 
end

pro tvRBox::GetProperty, OFFSET=off,SIZE=sz, THICK=th, COLOR=cl, _REF_EXTRA=re
  self->tvPlug::GetProperty, _EXTRA=re
  if arg_present(off) then off=self.boxoff
  if arg_present(sz) then sz=self.boxsize
  if arg_present(th) then th=self.thick
  if arg_present(cl) then  cl=self.color
end

;; Like Off, but don't even draw corners
pro tvRBox::Reset
  if self->IsDrawn() then begin ;we had a box to begin with
     self->tvPlug::Off
     self->EraseBox
     self.boxflag=-1
     if ~self.no_redraw then self.oDraw->SendRedraw
  endif 
end

;; Set the high and low limits (device pixels) for boxoff, to keep the
;; box on the array.  Also mirror the zoom and winsize. 
;; Boxoff is the upper left of the box.
pro tvRBox::SetUpDisplay
  self.oDraw->GetProperty,ZOOM=zoom,WINSIZE=winsize,SIZE=sz,DISPSIZE=ds
  ;; Update our internal mirror of tvDraw data
  self.zoom=zoom & self.winsize=winsize
  if self.snap_mode eq 0b then begin ;Draw does this for us in Snap mode
     ;; Convert the saved pixel coordinate to device units
     self.boxoff=self.oDraw->Convert(self.coords,/DEVICE,/SHOWING)
     if self.boxoff[0] eq -1 then self.boxoff=[0,0]
     ;; Scale the box size, if necessary.
     self.boxsize=fix(self.size*zoom)<winsize
     ;; Setup the box limits, which can be negative or greater than
     ;; winsize (i.e. offscreen).
     self.low_limit=self.oDraw->Convert([0,0],/DEVICE,/FRACTIONAL)
     self.high_limit=self.oDraw->Convert(sz,/DEVICE,/FRACTIONAL)
  endif else begin 
;     self.coords=self.coords<(ds-1)
     self.size=self.size<sz
  endelse 
end

pro tvRBox::MoveBox,X,Y
  X=0>X<(self.winsize[0]-1)
  Y=0>Y<(self.winsize[1]-1)
  if self.snap_mode then begin 
     new_coord=self.oDraw->Convert([X,Y],/FRACTIONAL)
     self.oDraw->GetProperty,OFFSET=off, DISPSIZE=ds,SIZE=sz
     
     case self.boxflag of
        1b: begin               ;inside of box, performing move     
           nc=floor(new_coord)
           if array_equal(nc,self.save_coords) then return
           self->Erasebox,/DOUBLE,CACHE=bounds_cache
           
           self.coords=self.coords+(nc-self.save_coords)
           self.coords=[0,self.size[1]-1] > $
                       self.coords < $
                       [sz[0]-self.size[0],sz[1]-1]
           self.save_coords=nc
        end
        
        0b: begin               ;on knob, or new box being drawn
           nc=round(new_coord)
           new_size=[off[0]+ds[0]-self.coords[0],self.coords[1]-off[1]+1] < $
                    [nc[0]-self.coords[0],self.coords[1]-nc[1]+1] $
                    > 1
           if array_equal(new_size,self.size) then return
           self->EraseBox,/DOUBLE,CACHE=bounds_cache
           self.size=new_size
        end
     endcase 
  endif else begin    
     self->EraseBox,/DOUBLE,CACHE=bounds_cache
     case self.boxflag of
        1b: begin               ;inside of box, perform move
           ;; we keep the box inside of the *array* not inside the window.
           self.boxoff=[self.low_limit[0],self.low_limit[1]+self.boxsize[1]] >$
                       (self.boxoff+([X,Y]-self.save)) <$
                       [self.high_limit[0]-self.boxsize[0],self.high_limit[1]]
        end
        
        0b: begin               ;on knob, or new box being drawn               
           self.boxsize=[X-self.boxoff[0],self.boxoff[1]-Y]>1
        end
     endcase 
     self.save=[X,Y]
  endelse 
  self->Drawbox,/DOUBLE,CACHE=bounds_cache,/EXPAND
  if self.on_motion then self->SendBox
end

pro tvRBox::SnapCoords
  self.boxoff=self.oDraw->Convert(self.coords+[0,1], $
                                  /DEVICE,/FRACTIONAL)
  self.boxsize=self.oDraw->Convert(self.size,/DEVICE,/DISTANCE)
end

pro tvRBox::DrawCorners
  if self.snap_mode then self->SnapCoords
  x=self.boxoff[0]+[0,0,self.corlen] ;ll
  y=self.boxoff[1]-self.boxsize[1]+[self.corlen,0,0]
  plots, x,y,COLOR=self.color, THICK=self.thick, /DEVICE
  
  x=self.boxoff[0]+self.boxsize[0]+[0,0,-self.corlen] ;lr
  plots, x,y,COLOR=self.color, THICK=self.thick, /DEVICE
  
  y=self.boxoff[1]+[-self.corlen,0,0] ;ur
  plots, x,y,COLOR=self.color, THICK=self.thick, /DEVICE
  
  x=self.boxoff[0]+[0,0,self.corlen] ;ul
  plots, x,y,COLOR=self.color, THICK=self.thick, /DEVICE
end

pro tvRBox::DrawBox,DOUBLE=dbl,_REF_EXTRA=e
  if self.snap_mode then self->SnapCoords
  if keyword_set(dbl) then self.oDraw->SetWin,/DOUBLE
  plots, $
        [self.boxoff[0],self.boxoff[0],self.boxoff[0]+self.boxsize[0], $
         self.boxoff[0]+self.boxsize[0], self.boxoff[0]], $
        [self.boxoff[1],self.boxoff[1]-self.boxsize[1],self.boxoff[1]- $
         self.boxsize[1], self.boxoff[1], self.boxoff[1]],  $
        COLOR=self.color,THICK=self.thick, /DEVICE
  ;;knob
  plots,self.boxoff[0]+self.boxsize[0],self.boxoff[1]-self.boxsize[1], $
        PSYM=8, SYMSIZE=self.hsize,color=self.color,/DEVICE
  if keyword_set(dbl) then begin 
     self->Bounds,ll,dist,_EXTRA=e
     self.oDraw->DBRefresh,ll,dist
  endif 
end

pro tvRBox::EraseBox,_REF_EXTRA=e
  ;if self.snap_mode then self->SnapCoords
  ;if NOT array_equal(self.boxoff ge 0,1b) then return
  self->Bounds,ll,dist,_EXTRA=e
  if dist[0] ne 0 then self.oDraw->Erase,ll,dist,_EXTRA=e
end

pro tvRBox::Bounds,ll,dist,CACHE=c,EXPAND=exp
  lr=self.boxoff+(self.boxsize)*[1,-1]
  ur=(self.boxoff>lr)+(self.thick+2.*self.hsize) > 0
  ll=(self.boxoff<lr)-(self.thick+2.*self.hsize) > 0
  
  if n_elements(exp) ne 0 then begin 
     ur=ur>c[2:3] & ll=ll<c[0:1]
  endif else if arg_present(c) then c=[ll,ur]
  
  if ~array_equal(ll lt self.winsize,1b) then begin 
     dist=0
     return
  endif 
  dist=fix(ur-ll+1) < (self.winsize-ll) > 1
end

pro tvRBox::SendBox
  if self.snap_mode eq 0 then begin 
     ;; set up the data coordinate, for zooms, etc.
     self->GetLRTB,l,r,t,b
     size=[r-l,t-b]
     if array_equal([l,t],self.coords) AND array_equal(size,self.size) $
        then return             ;no message necessary
     self.coords=[l,t]
     self.size=size
  endif ; we always send a message in snap mode.  
  ;; send out a BOX message to our recipients.
  self->MsgSend, {BOX, self.boxflag}
end

pro tvRBox::Start
  self->tvPlug::Start
  self.oDraw->GetProperty,ZOOM=zm
  if zm eq 0. then zm=1.        ;a good starting value.
  self.zoom=zm
end

pro tvRBox::Cleanup
  ptr_free,self.ux,self.uy
  self->tvPlug::Cleanup
end

;=============================================================================
;  Init -  Initialize the tvRBox object.
;    OPTIONS:                                          -- Default 
;  	COLOR:     Color index with which to draw box  -- 0
;  	THICK:     Thickness of box band               -- 1.2
;  	HANDLE:    Type of handle to draw              -- 0 (circle)
;  	 	     0: circle
;  		     1: triangle
;  		     2: square
;  	KNOBRAD:   Radius around lower right point     -- 5 (device units)
;  		   which defines the resize knob if  
;  		   clicked within.  
;  	HSIZE:     Size of handle knob (SYMSIZE)       -- 1.
;  	CORLEN:	   Length of Corner brackets           -- 5 (device units)
;  	BUTTON:    Which button to use to activate     -- 0b (left)
;  		   either 0(left), 1(middle), 2(right)
;       CORNERS:   Whether to display corners when     -- 0 (no)
;        	   turned off.  
;   	HANDCURS:  Whether to display a hand cursor    -- 0 (no)
;   		   when moving the box.  
;       ON_MOTION: Deliver box motion events           -- 0 (no)
;                  (not just release).  
;       SNAP:      Whether to snap to pixel            -- 0 (no)
;                  boundaries.
;=============================================================================
function tvRBox::Init,oDraw,COLOR=color,THICK=thick,HANDLE=handle,KNOBRAD=kr,$
                      HSIZE=hs,CORLEN=cl,BUTTON=but,CORNERS=co,HANDCURS=hc, $
                      ON_MOTION=om,SNAP=snap,NO_REDRAW=nrd,_EXTRA=e
  if (self->tvPlug::Init(oDraw,/NO_ON_OFF,_EXTRA=e) ne 1) then return,0
  if n_elements(but) eq 0 then but=0b
  self.button=2b^fix(but)       ;assign which button
  if n_elements(color) eq 0 then self.color=0 else self.color=color
  if n_elements(handle) eq 0 then self.handle=0 else self.handle=0>handle<3
  if n_elements(hs) eq 0 then self.hsize=1. else self.hsize=hs
  if n_elements(cl) eq 0 then self.corlen=5. else self.corlen=ol
  if n_elements(kr) eq 0 then self.knobrad=5 else self.knobrad=kr
  if keyword_set(snap) then self.snap_mode=1
  self.corners=keyword_set(co)  ;whether to draw outline corners when off
  self.handcurs=keyword_set(hc) ;whether to show a hand cursor on movement
  self.on_motion=keyword_set(om) ;whether to send messages for box moves.
  self.no_redraw=keyword_set(nrd) 
  
  ;; What type of corner knob to draw
  case self.handle of
     0: begin                   ;circle
        ang = 2*!PI*findgen(10)/9. 
        self.ux = ptr_new(cos(ang))  &  self.uy = ptr_new(sin(ang))
        usersym,*self.ux,*self.uy,/FILL
     end
     
     1: begin                   ;triangle
        self.ux = ptr_new([-1,0,1,-1]) & self.uy=ptr_new([-1,1,-1,-1])
        usersym,*self.ux,*self.uy,/FILL
     end
     
     2: begin                   ;square
        self.ux = ptr_new([-1,-1,1, 1,-1]) & self.uy=ptr_new([-1, 1,1,-1,-1])
        usersym,*self.ux,*self.uy,/FILL
     end
  endcase 
  
  self.boxflag=-1               ;default to undrawn
  
  ;; get properties from the Draw object, mirror them here.
  self.oDraw->GetProperty,WINSIZE=winsize
  self.winsize=winsize
  if n_elements(thick) eq 0 then $
     self.thick=1.5*float(max(winsize))/256<2 else self.thick=thick
  self->MsgSetup,'BOX'
  return,1
end

pro tvRBox__define
  struct={tvRBox, $
          INHERITS tvPlug, $    ;make it a tvDraw plug-in
          snap_mode:0, $        ;using snap mode?
          no_redraw:0b, $       ;whether to skip sending any redraws
          on_motion: 0, $       ;whether to send Box messages on box motion
          color:0, $            ;color to draw it.
          thick:0., $           ;thickness of box line
          button:0b, $          ;which button to activate on
          handle:0, $           ;type of handle (see init)
          hsize:0., $           ;size of handle
          corlen:0, $           ;length of corner brackets.
          boxsize:[0,0], $      ;size of box (device units)
          boxoff:[0,0], $       ;offset of top-left of box (device units)
          low_limit:[0,0], $    ;the lower limit for box
          high_limit:[0,0], $   ;the upper limit for box
          ;; For snap mode
          coords:[0,0], $       ;the saved upper left coord (data pixel units)
          size:[0,0], $         ;the saved box size (data pixel units)
          save:[0,0], $         ;a saved device coordinate
          save_coords:[0,0], $  ;a saved pixel coordinate (for snap mode)
          ;; Flags
          boxflag:0, $          ;whether box is drawn, -1=never drawn
          knobrad:0, $          ;radius around lr to count as knob
          corners:0b, $         ;whether to display corners when turned off
          handcurs:0b, $        ;whether to make a hand cursor when moving
          ux:ptr_new(), $       ;the x vector for the usersym 
          uy:ptr_new(), $       ;the y vector for the usersym
          zoom:0.0, $           ;the saved zoom state of the display
          buttondwn:0b, $       ;whether a button is yet pressed
          winsize:[0,0]}        ;size of the window displayed in
  st={BOX,flag:0}
end
 
