
;;**************************OverRiding methods********************************
pro tvRBox::Message, msg
;   help,msg,/st
   type=tag_names(msg,/STRUCTURE_NAME)
   if type eq 'TVDRAW_REDRAW' then begin 
      if self.boxflag ne -1b then begin ; only if a box here
         ;;  a change of original image
         if (msg.type and 2b) ne 0b or (msg.type and 4b) ne 0b then begin 
            self.boxoff=(self.coords-msg.offset)*msg.zoom+msg.pan
            self.boxsize=fix(self.boxsize*msg.scale)
         endif  
         if self.recip.ACTIVE eq 1b then begin ;we're currently on
            self->DrawBox
         endif else begin       ;we're currently not on
            if self.corners then self->drawcorners
         endelse 
      endif 
      return
   endif 
   
   ;; fall through to widget_draw messages here
   case msg.type of
      2: $                      ;motion event
       begin 
         ;; draw zoom box
         if self.buttondwn ne 0b then self->movebox,msg.X,msg.Y
      end
      
      0: $                      ;button press
       begin  
         self.oDraw->SetWin
         if (msg.press ne self.button) then return ;check if our button hit
         ;; check and see if box drawn
         if self.boxflag ne -1b then begin ;if a box is already drawn
            ;; check if "on knob" (flag=2b) or "in bounds" (flag=1b)
            ;; or neither (simply return).
            dist=float([msg.X,msg.Y])- $
             [self.boxoff[0]+self.boxsize[0]-1,self.boxoff[1]]
            if total(dist^2) le self.knobrad^2 then begin
               self.boxflag=2b  ;on knob
               self.boxfix=self.boxoff[1]+self.boxsize[1]
            endif else begin 
               if dist[0] le 0 and dist[0] ge -self.boxsize[0] and $
                dist[1] ge 0 and dist[1] le self.boxsize[1] then begin 
                  self.boxflag=1b ;in box
                  if self.handcurs then hand_cursor,/CENTER
               endif else return
            endelse 
         endif else begin       ; box not yet drawn: drawing a new one
            self.boxflag=0b     ;new box draw (flag=0b)
            self.boxoff=[msg.X,msg.Y]
         endelse 
         
         ;;sign up for motion events.
         self.recip.MOTION=1b & self->Update
         self.save=[msg.X,msg.Y] ;the save location
         
         ;; set the user symbol (in case it gets changed)
         usersym,*self.ux,*self.uy,/FILL
         
         ;;get the draw window id and set it
         self.buttondwn=1b      ;we've pressed our button
      end 
      
      1: $                      ;button release
       begin
         ;; do nothing if wrong button, no buttondown flagged, or no box
         ;; yet drawn
         if msg.release ne self.button or self.buttondwn eq 0b or  $
          self.boxflag eq -1b then return
         self.recip.MOTION=0b & self->Update ;turn off motion 
         self.buttondwn=0b      ;not button down any longer
         case self.boxflag of
            1b: if self.handcurs then device,/cursor_cross 
            0b: begin
               self.boxsize=abs(self.save-self.boxoff)
               self.boxoff=(self.boxoff < self.save)
            end
            else:
         endcase 
        
         ;; send out a BOX message...
         bmsg={BOX, type:self.boxflag}
         self->MsgSend, bmsg
         
         if self.boxflag eq 0b then self->erasebox ; first box not right on
         
         ;; and a redraw message (since box might have erased other things)
         self.oDraw->SendRedraw
         
         ;; set up the old saved data coordinate
         self.oDraw->GetProperty,zoom=zoom,pan=pan,offset=offset
         self.coords=float(self.boxoff-pan)/zoom+offset

      end
   endcase 
   return
end

;;************************End OverRiding methods*******************************

;; return status of box, 0 (not drawn) or 1 (drawn)
function tvRBox::IsDrawn
   return, NOT (self.boxflag eq -1b)
end

;; Get the current box sides in pixel coordinates (may not be real, since
;; box may overrun the edges of the array)
pro tvRBox::GetLRTB, l, r, t, b, oDp
   self.oDraw->GetProperty,zoom=zoom,pan=pan,offset=offset,dispsize=dispsize
   l=offset[0]+(FIX((self.boxoff[0]-pan[0])/zoom) > 0)
   r=offset[0]+FIX((self.boxoff[0]+self.boxsize[0]-1-pan[0])/  $
                       zoom) < (dispsize[0]-1+offset[0])
   t=offset[1]+FIX((self.boxoff[1]+self.boxsize[1]-1-pan[1])/ $
                       zoom) < (dispsize[1]-1+offset[1])
   b=offset[1]+(FIX((self.boxoff[1]-pan[1])/zoom) > 0)
end

pro tvRBox::SetProperty, CORNERS=cor, COLOR=col
   if n_elements(cor) ne 0 then self.corners=cor
   if n_elements(col) ne 0 then self.color=col
end

pro tvRBox::GetProperty, OFFSET=off,SIZE=sz, THICK=th, COLOR=cl
   if arg_present(off) then off=self.boxoff
   if arg_present(sz) then sz=self.boxsize
   if arg_present(th) then th=self.thick
   if arg_present(cl) then  cl=self.color
end

pro tvRBox::On
   self.recip.ACTIVE=1b
   if self.boxflag ne -1b then self->drawbox ;just turned on
   self->Update                 ;tell the tvDraw object no more draw events.
end

pro tvRBox::Off, RESET=rs
   self.recip.ACTIVE=0b
   if self.boxflag ne -1b then begin ;we had a box to begin with
      self->erasebox 
      ;; go back to no box drawn, if requested.
      if keyword_set(rs) then self.boxflag=-1b else begin 
         if self.corners then self->drawcorners 
      endelse 
   endif 
   self->Update                 ;turn ourselves off
   self.oDraw->SendRedraw
end

pro tvRBox::Reset
   if self.boxflag ne -1b then begin ;we had a box to begin with
      self->erasebox 
      self.boxflag=-1b 
   endif 
   self.oDraw->SendRedraw
end

pro tvRBox::MoveBox,X,Y
   case self.boxflag of
      1b: $                     ;in bounds
       begin 
         self->erasebox
         self.boxoff=(self.boxoff+([X,Y]-self.save)) < (self.winsize-2) > $
          (2-self.boxsize)
         self.save=[X,Y]
         self->drawbox
      end
      
      2b: $                     ;on knob
       begin 
         self->erasebox
         self.boxoff[1]=0>(self.boxoff[1]+Y-self.save[1])<self.boxfix
         self.boxsize=0 > (self.boxsize-[-1,1]*([X,Y]-self.save)) <  $
          (self.winsize-self.boxoff-1)
         self.save=[X,Y]
         self->drawbox
      end
      
      0b: $                     ;special case: drawing a new box
       begin 
         if self.buttondwn eq 2b then begin ; a draw box drawn
            low=((self.boxoff < self.save)-1) > 0
            dist=((self.boxoff > self.save)+1 < (self.winsize-1))-low+1
            device, copy=[low,dist,low,self.pixwin]
         endif else self.buttondwn=2b ;we're about to draw a first box

         self.boxoff=[X,Y]      ;set old position (for next erase)
         ;; plot box
         plots,[self.save[0],self.save[0],X,X, self.save[0]], $
          [self.save[1],Y,Y, self.save[1], $
           self.save[1]], COLOR=self.color,THICK=self.thick, /DEVICE
      end
   endcase 

   return
end

pro tvRBox::DrawCorners
   x=self.boxoff[0]+[0,0,self.corlen]     ;ll
   y=self.boxoff[1]+[self.corlen,0,0]
   plots, x,y,COLOR=self.color, THICK=self.thick, /DEVICE
   x=self.boxoff[0]+self.boxsize[0]-1+[0,0,-self.corlen] ;lr
   plots, x,y,COLOR=self.color, THICK=self.thick, /DEVICE
   y=self.boxoff[1]+self.boxsize[1]-1+[-self.corlen,0,0] ;ur
   plots, x,y,COLOR=self.color, THICK=self.thick, /DEVICE
   x=self.boxoff[0]+[0,0,self.corlen]   ;ul
   y=self.boxoff[1]+self.boxsize[1]-1+[-self.corlen,0,0]
   plots, x,y,COLOR=self.color, THICK=self.thick, /DEVICE
   return
end

pro tvRBox::DrawBox
   ;;plot box
   plots, $
    [self.boxoff[0],self.boxoff[0],self.boxoff[0]+self.boxsize[0]-1, $
     self.boxoff[0]+self.boxsize[0]-1, self.boxoff[0]], $
    [self.boxoff[1],self.boxoff[1]+self.boxsize[1]-1,self.boxoff[1]+ $
     self.boxsize[1]-1, self.boxoff[1], self.boxoff[1]],  $
    COLOR=self.color,THICK=self.thick, /DEVICE
   ;;plot knob
   plots,self.boxoff[0]+self.boxsize[0]-1,self.boxoff[1],PSYM=8,  $
    SYMSIZE=self.hsize,color=self.color,/DEVICE
   return
end

pro tvRBox::EraseBox
   high=self.boxoff+self.boxsize
   if total((self.boxoff lt 0 or self.boxoff ge self.winsize) +  $
            (high lt 0 or high ge self.winsize)) ge 4 then return
   low=fix((self.boxoff-(self.thick+2.*self.hsize))) > 0
   dist=fix(self.boxsize+(2.*self.hsize+self.thick)*2) <  $
    (self.winsize-low) > 1 
   device,copy=[low,dist,low,self.pixwin]
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
;		NO_REDRAW_WITH_BOX: If set, the box will   -- 0 (no)
;			 *not* send a redraw through tvdraw
;			 when it sends a box message.  This
;			 assumes the owner of the box will
;			 handle the redraw.
;	   	ACTIVE:  If set, the box starts active     -- 0 (no)
;
;	 	RELEVANT ObjMsg INHERITED KEYWORDS:
;	 	
;	        MSGLIST: The recipient list for messages   -- (none)
;	        	 when box moved/created/resized.
;	        	 Format is: object id only.
;	        	 Message sent is:
;	        	  {TVRBOX, offset:[0,0], size:[0,0], type:0}
;	        	  where offset is the device offset of the box,
;	        	  size is the size of the box, and type is one of:
; 	        	    0: Newly Created Box. 
;	        	    1: Moved Box. 
;	        	    2: Resized Box. 
;=============================================================================
function tvRBox::Init,oDraw,COLOR=color,THICK=thick,Handle=handle,KNOBRAD=kr,$
               HSIZE=hs,CORLEN=cl,BUTTON=but,CORNERS=co,HANDCURS=hc, $
               ACTIVE=act, _EXTRA=e
   if (self->tvPlug::Init(oDraw,_EXTRA=e) ne 1) then return,0
   if n_elements(but) eq 0 then but=0b
   self.button=2b^fix(but)      ;assign which button
   if n_elements(color) eq 0 then self.color=0 else self.color=color
   if n_elements(handle) eq 0 then self.handle=0 else self.handle=0>handle<3
   if n_elements(hs) eq 0 then self.hsize=1.3 else self.hsize=hs
   if n_elements(cl) eq 0 then self.corlen=5. else self.corlen=ol
   if n_elements(kr) eq 0 then self.knobrad=5 else self.knobrad=kr
   self.corners=keyword_set(co) 
   self.handcurs=keyword_set(hc) 
   
   case self.handle of
      0: begin                  ;circle
         ang = 2*!PI*findgen(25)/24. 
         self.ux = ptr_new(cos(ang))  &  self.uy = ptr_new(sin(ang))
         usersym,*self.ux,*self.uy,/FILL
      end
      
      1: begin
         self.ux = ptr_new([-1,0,1,-1]) & self.uy=ptr_new([-1,1,-1,-1])
         usersym,*self.ux,*self.uy,/FILL
      end
      
      2: begin
         self.ux = ptr_new([-1,-1,1, 1,-1]) & self.uy=ptr_new([-1, 1,1,-1,-1])
         usersym,*self.ux,*self.uy,/FILL
      end
   endcase 
   
   self.boxflag=-1b             ;default to undrawn
   
   ;; get properties from the Draw object, mirror here.
   self.oDraw->GetProperty,WINSIZE=winsize,PIXWIN=pixwin
   self.pixwin=pixwin & self.winsize=winsize
   if n_elements(thick) eq 0 then  $
    self.thick=1.5*float(max(winsize))/256<2 else self.thick=thick
   
   ;; Sign up with Draw object
   self.recip.BUTTON=1b         ;get button events
   self.recip.ACTIVE=keyword_set(act)
   self->Update   
   return,1
end

pro tvrbox__define
   struct={tvRBox, $ 
           INHERITS tvPlug, $   ;make it a tvDraw plug-in
           color:0, $           ;color to draw it.
           thick:0., $          ;thickness of box line
           button:0b, $         ;which button to activate on
           handle:0, $          ;type of handle (see init)
           hsize:0., $          ;size of handle
           corlen:0, $          ;length of off corner brackets.
           boxsize:[0,0], $     ;size of box (device units)
           boxoff:[0,0], $      ;offset of box (device units)
           coords:[0,0], $      ;the saved lower left coords in data coords.
           save:[0,0], $        ;a saved coordinate box       
           boxfix:0, $          ;a fixed y location for box resizes
           boxflag:0b, $        ;whether box is drawn, -1=never drawn
           knobrad:0, $         ;radius around lr to count as knob
           corners:0b, $        ;whether to display corners when turned off
           handcurs:0b, $       ;whether to make a hand cursor when moving
           ux:ptr_new(), $      ;the x vector for the usersym 
           uy:ptr_new(), $      ;the y vector for the usersym
           pixwin:0, $          ;the pixwin id of the window 
           buttondwn:0b, $      ;whether a button is yet pressed
           winsize:[0,0]}       ;size of the window displayed in
end


