;
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


;;**************************OverRiding methods********************************
pro tvRBox::Message, msg
  type=tag_names(msg,/STRUCTURE_NAME)
  case type of
     'DRAW_MOTION': $
        if self.buttondwn ne 0b then self->MoveBox,msg.X,msg.Y
     
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
              endif else begin  ; box not yet drawn: drawing a new one
                 self.boxflag=0b ;drawing new box, just like on knob
                 self.boxoff=[msg.X,msg.Y]
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
              self->SendBox     ;maybe send a box message
              ;; A redraw message (since box might have erased other things)
              self.oDraw->SendRedraw    
           end 
        endcase
     end
     
     'TVDRAW_POSTDRAW': begin
        ;;  a change of original image, or zoom, etc. occurred
        if self.boxflag eq -1 then return ; only if a box is here
        self->SetUpDisplay
     end 
     
     'TVKEY_ARROW': begin 
        if self->IsDrawn() eq 0 then return
        self->EraseBox
        self.boxoff=[0,self.boxsize[1]]>(self.boxoff+self.zoom*msg.move)< $
           [self.winsize[0]-self.boxsize[0]-1,self.winsize[1]-1]
        ;self.save=self.boxoff
        self->SendBox
        self->DrawBox
     end
     
     'TVDRAW_REDRAW': begin     ; the screen was clobbered
        if self->IsDrawn() then begin ; only if a box here
           if self.active then self->DrawBox  $ ;currently on
           else if self.corners then self->drawcorners ;currently not on
        endif 
     end
  endcase 
end

pro tvRBox::On
  self->tvPlug::On
  key=self.oDraw->GetMsgObjs(CLASS='tvKey')
  if obj_valid(key[0]) then key[0]->MsgSignup,self,/TVKEY_ARROW
  if self->IsDrawn() then begin 
     self->EraseBox             ;removes any drawn corners
     self->DrawBox              ;just turned on
  endif 
  self->SetUpDisplay
  ;; Button and redraw events
  self.oDraw->MsgSignup,self,/DRAW_BUTTON,/TVDRAW_POSTDRAW,/TVDRAW_REDRAW
end

pro tvRBox::Off 
  self->tvPlug::Off
  self.oDraw->MsgSignup,self,/NONE
  key=self.oDraw->GetMsgObjs(CLASS='tvKey')
  if obj_valid(key[0]) then key[0]->MsgSignup,self,/NONE
  if self->IsDrawn() then begin 
     self->EraseBox
     if self.corners then begin 
        ;;corner redraws and box size changes needed
        self.oDraw->MsgSignup,self,/NONE,/TVDRAW_POSTDRAW,/TVDRAW_REDRAW 
        self->drawcorners
     endif 
  endif 
end
;;************************End OverRiding methods*******************************

;; return status of box, -1 (never drawn) 0 (drawing new) or 1 (drawn)
function tvRBox::IsDrawn
  return, self.boxflag ne -1
end

;; Get the current box sides in pixel coordinates, truncated to actual sizes
pro tvRBox::GetLRTB, l, r, t, b
  tmp=self.oDraw->Convert(self.boxoff,/TRUNCATE)
  l=tmp[0] & t=tmp[1]
  tmp=self.oDraw->Convert(self.boxoff+[1,-1]*self.boxsize,/TRUNCATE)
  r=tmp[0] & b=tmp[1]
end

pro tvRBox::SetProperty, CORNERS=cor, COLOR=col
  if n_elements(cor) ne 0 then self.corners=cor
  if n_elements(col) ne 0 then self.color=col
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
     self.oDraw->SendRedraw
  endif 
end

;; Set the high and low limits (device pixels) for boxoff, to keep the
;; box on the array.  Also mirror the zoom and winsize. 
;; Boxoff is the upper left of the box.
pro tvRBox::SetUpDisplay
  self.oDraw->GetProperty,ZOOM=zoom,WINSIZE=winsize,SIZE=sz
  ;; Convert the saved pixel coordinate to device
  self.boxoff=self.oDraw->Convert(self.coords,/DEVICE)
  ;; Scale the box size, if necessary.
  self.boxsize=fix(self.boxsize*(zoom/self.zoom))
  ;; Update our internal mirror of tvDraw data
  self.zoom=zoom & self.winsize=winsize
  ;; Setup the box limits, which can be negative or greater than
  ;; winsize (i.e. offscreen).
  self.low_limit=self.oDraw->Convert([0,0],/DEVICE,/FRACTIONAL)
  self.high_limit=self.oDraw->Convert(sz,/DEVICE,/FRACTIONAL)
end

pro tvRBox::MoveBox,X,Y
  X=0>X<(self.winsize[0]-1)
  Y=0>Y<(self.winsize[1]-1)
  self->erasebox 
  case self.boxflag of
     1b: begin                  ;inside of box, perform move
        ;; we keep the box inside of the *array* not inside the window.
        self.boxoff=[self.low_limit[0],self.low_limit[1]+self.boxsize[1]] > $
           (self.boxoff+([X,Y]-self.save)) < $
           [self.high_limit[0]-self.boxsize[0],self.high_limit[1]]
     end
     
     0b: $                      ;on knob, or new box being drawn               
        self.boxsize=[X-self.boxoff[0],self.boxoff[1]-Y]>1
  endcase 
  self->Drawbox
  self.save=[X,Y]
  if self.on_motion then self->SendBox
end

pro tvRBox::DrawCorners
  x=self.boxoff[0]+[0,0,self.corlen] ;ll
  y=self.boxoff[1]-self.boxsize[1]+1+[self.corlen,0,0]
  plots, x,y,COLOR=self.color, THICK=self.thick, /DEVICE
  
  x=self.boxoff[0]+self.boxsize[0]-1+[0,0,-self.corlen] ;lr
  plots, x,y,COLOR=self.color, THICK=self.thick, /DEVICE
  
  y=self.boxoff[1]+[-self.corlen,0,0] ;ur
  plots, x,y,COLOR=self.color, THICK=self.thick, /DEVICE
  
  x=self.boxoff[0]+[0,0,self.corlen] ;ul
  plots, x,y,COLOR=self.color, THICK=self.thick, /DEVICE
end

pro tvRBox::DrawBox
  plots, $
        [self.boxoff[0],self.boxoff[0],self.boxoff[0]+self.boxsize[0], $
         self.boxoff[0]+self.boxsize[0], self.boxoff[0]], $
        [self.boxoff[1],self.boxoff[1]-self.boxsize[1],self.boxoff[1]- $
         self.boxsize[1], self.boxoff[1], self.boxoff[1]],  $
        COLOR=self.color,THICK=self.thick, /DEVICE
  ;;knob
  plots,self.boxoff[0]+self.boxsize[0],self.boxoff[1]-self.boxsize[1], $
        PSYM=8, SYMSIZE=self.hsize,color=self.color,/DEVICE
end

pro tvRBox::EraseBox
  lr=self.boxoff+(self.boxsize)*[1,-1]
  ll=(self.boxoff<lr)-(self.thick+2.*self.hsize) > 0
  ur=(self.boxoff>lr)+(self.thick+2.*self.hsize) > 0
  dist=fix(ur-ll+1) < (self.winsize-ll) > 0 
  self.oDraw->Erase,ll,dist
end

pro tvRBox::SendBox
  ;; set up the data coordinate, for zooms, etc.
  self->GetLRTB,l,r,t,b
  size=[r-l,t-b]
  if array_equal([l,t],self.coords) AND array_equal(size,self.size) $
     then return                ;no message necessary
  self.coords=[l,t]
  self.size=size
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
;       Init.  Initialize the tvRBox object.
;         OPTIONS:                                         -- Default 
;         	COLOR:  Color index with which to draw box -- 0
;         	THICK:  Thickness of box band              -- 1.2
;         	HANDLE: Type of handle to draw             -- 0 (circle)
;         	 	 0: circle
;         		 1: triangle
;         		 2: square
;         	KNOBRAD: Radius around lower right point   -- 5 (device units)
;         		 which defines the resize knob if
;         		 clicked within.
;         	HSIZE:   Size of handle knob (SYMSIZE)     -- 1.
;         	CORLEN:	 Length of Corner brackets         -- 5 (device units)
;         	BUTTON:  Which button to use to activate   -- 0b (left)
;         		 either 0(left), 1(middle), 2(right)
;               CORNERS: Whether to display corners when   -- 0 (no)
;               	 turned off.
;	   	HANDCURS:Whether to display a hand cursor  -- 1 (yes)
;	   		 when moving the box.
;=============================================================================
function tvRBox::Init,oDraw,COLOR=color,THICK=thick,HANDLE=handle,KNOBRAD=kr,$
                      HSIZE=hs,CORLEN=cl,BUTTON=but,CORNERS=co,HANDCURS=hc, $
                      ON_MOTION=om,_EXTRA=e
  if (self->tvPlug::Init(oDraw,_EXTRA=e) ne 1) then return,0
  if n_elements(but) eq 0 then but=0b
  self.button=2b^fix(but)       ;assign which button
  if n_elements(color) eq 0 then self.color=0 else self.color=color
  if n_elements(handle) eq 0 then self.handle=0 else self.handle=0>handle<3
  if n_elements(hs) eq 0 then self.hsize=1. else self.hsize=hs
  if n_elements(cl) eq 0 then self.corlen=5. else self.corlen=ol
  if n_elements(kr) eq 0 then self.knobrad=5 else self.knobrad=kr
  self.corners=keyword_set(co)  ;whether to draw outline corners when off
  self.handcurs=keyword_set(hc) ;whether to show a hand cursor on movement
  self.on_motion=keyword_set(om) ;whether to send messages for box moves.
  
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
  self.oDraw->GetProperty,WINSIZE=winsize,PIXWIN=pixwin
  self.pixwin=pixwin & self.winsize=winsize
  if n_elements(thick) eq 0 then $
     self.thick=1.5*float(max(winsize))/256<2 else self.thick=thick
  return,1
end

pro tvrbox__define
  struct={tvRBox, $
          INHERITS tvPlug, $    ;make it a tvDraw plug-in
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
          coords:[0,0], $       ;the saved upper left coord (data pixel units)
          size:[0,0], $         ;the saved box size (data pixel units)
          save:[0,0], $         ;a saved device coordinate box       
          boxflag:0, $          ;whether box is drawn, -1=never drawn
          knobrad:0, $          ;radius around lr to count as knob
          corners:0b, $         ;whether to display corners when turned off
          handcurs:0b, $        ;whether to make a hand cursor when moving
          ux:ptr_new(), $       ;the x vector for the usersym 
          uy:ptr_new(), $       ;the y vector for the usersym
          zoom:0.0, $           ;the saved zoom state of the display
          pixwin:0, $           ;the pixwin id of the window 
          buttondwn:0b, $       ;whether a button is yet pressed
          winsize:[0,0]}        ;size of the window displayed in
  st={BOX,flag:0}
end
