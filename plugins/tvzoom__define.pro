
;;**************************OverRiding methods********************************
;=============================================================================
;	Message.  Override the ObjMsg Message to do zooms.  We have
;	signed up for button messages from the tvDraw object.
;=============================================================================
pro tvZoom::Message, msg
   self->tvPlug::Message,msg,TYPE=type ;pass it up to tvPlug
   if type eq 'WIDGET_DRAW' then begin ;it's a button event
      case msg.type of
         2: $                   ;motion event
          begin 
            ;; draw zoom box
            if self.buttondwn ne 0b then self->tvzoombox,msg.X,msg.Y
         end
      
         0: $                   ;button press
          begin  
            if (msg.press and ((self.button and NOT 4b)+4b)) eq 0 then return
            if msg.press eq self.button then begin 
               self.buttondwn=1b ;we've pressed
               self.orig=[msg.X,msg.Y]
               ;;sign up for motion events.
               self.recip.MOTION=1b & self->Update
            endif else begin ;; it's a zoom out button press -- button 3
               if not ptr_valid(self.zoomlist) then return
               n=n_elements(*self.zoomlist) 
               if msg.clicks eq 2 or n lt 8 then begin ;it's out to the first
                  self.oDraw->GetProperty, SIZE=size
                  self.oDraw->SetProperty,offset=[0,0],dispsize=size   
                  ptr_free,self.zoomlist ;no zoom list left
               endif else begin ;a single click *and* 2 or more levels of zoom
                  off=(*self.zoomlist)[4:5] ; pop the top of the stack off.
                  ds=(*self.zoomlist)[6:7]
                  *self.zoomlist=(*self.zoomlist)[4:n-1] ;remove newest
                  self.oDraw->SetProperty,offset=off, dispsize=ds
               endelse 
            endelse
         end
      
         1: $
          begin                 ;button release
            if msg.release ne self.button then return
            self.recip.MOTION=0b & self->Update ;turn off motion 
            self.buttondwn=0b   ;not button down any longer
            self->tvzoomit,msg.X,msg.Y ;zoom in on it
         end
      endcase 
      return
   endif 
end 

function tvZoom::Icon
   return,[[  0b,  0b],[224b,  3b],[ 56b, 14b],[ 24b, 12b], $
           [ 12b, 24b],[ 12b, 24b],[ 12b, 25b],[140b, 26b], $
           [ 12b, 25b],[ 24b, 12b],[ 56b, 14b],[224b,  3b], $
           [192b,  1b],[192b,  1b],[192b,  1b],[192b,  1b]]
end
;;************************End OverRiding methods*******************************

pro tvZoom::tverasebox,X,Y
   low=(([X,Y] < self.orig)-1) > 0 ;start two to the left
   high=([X,Y] > self.orig) < self.winsize
   dist=(high-low+2>0) < (self.winsize-low) > 1 ;box sides
;   covlow=2<(self.winsize-1-low) > 1 ;rubber band coverage moves
;   covhigh=2<(self.winsize-1-high) > 1 ;avoiding leaving the box
;   boxes=[[low[0]-1>0,low[1],covlow[0],dist[1]], $ ;bottom
;          [low[0],low[1]-1>0,dist[0],covlow[1]], $ ;left
;          [high[0]-1<(self.winsize[0]-1),low[1],covhigh[0],dist[1]], $ ;right
;          [low[0],high[1]-1<(self.winsize[1]-1),dist[0],covhigh[1]]] ;top
;   for i=0,3 do begin
;      print,[boxes[*,i],boxes[0:1,i],self.pixwin]
;      device,copy=[boxes[*,i],boxes[0:1,i],self.pixwin]
;   endfor 
   device,copy=[low,dist,low,self.pixwin]
end
   
pro tvZoom::tvzoombox,X,Y
   if self.buttondwn eq 2b then begin ;erase old
      self->tverasebox,self.save[0],self.save[1]
      
      ;low=((self.save < self.orig)-2) > 0 ;start two to the left
      ; high=(self.save > self.orig) < self.winsize
      ; dist=(high-low+2>0) < (self.winsize-low) > 1 ;box sides
      ; device,copy=[low,dist,low,self.pixwin]
   endif else self.buttondwn=2b ;we're about to draw one
   plots, $                     ;plot the zoom box
    [self.orig[0],self.orig[0],X,X,  $
     self.orig[0]], $
    [self.orig[1],Y,Y, self.orig[1], $
     self.orig[1]], COLOR=self.color,THICK=self.thick, /DEVICE
   self.save=[X,Y]
end 
   
pro tvZoom::tvzoomit, X, Y
   self.oDraw->GetProperty,zoom=zoom,pan=pan,offset=offset,size=size
   left=FIX(((self.orig[0] < X)- $
             pan[0])/zoom) > 0
   right=FIX(((self.orig[0] > X)- $
              pan[0])/zoom) < (size[0]-1-offset[0])
   top=FIX(((self.orig[1] > Y)- $
            pan[1])/zoom) < (size[1]-1-offset[1])
   bottom=FIX(((self.orig[1] < Y )- $
               pan[1])/zoom) > 0
   
   validzoom=(left lt right) and (bottom lt top)
   
   if NOT validzoom then begin 
      self->tverasebox,X,Y      ;erase the current box
      return
   endif else begin ; a valid zoom occured
      ;; set the tvDraw parameters
      offset=offset+[left,bottom]
      dispsize=[right-left+1, top-bottom+1]
      ;; update the zoomlist
      if ptr_valid(self.zoomlist) then begin 
         *self.zoomlist=[offset, dispsize, *self.zoomlist]
      endif else  $
       self.zoomlist=ptr_new([offset, dispsize])
      ;; set and draw it
      self.oDraw->SetProperty,offset=offset, dispsize=dispsize
   endelse 
end

;=============================================================================
;	Cleanup.  Clean self up
;=============================================================================
pro tvZoom::Cleanup
   ptr_free,self.zoomlist
   self->tvPlug::Cleanup
   return
end

;=============================================================================
;	init.  Initialize the zoomer with a tvDraw object
;=============================================================================
function tvZoom::Init,oDraw,BUTTON=but,EXCLUSIVE=exc,COLOR=color,_EXTRA=e
   if (self->tvPlug::Init(oDraw,_EXTRA=e) ne 1) then return,0
   if n_elements(but) eq 0 then but=0b
   if n_elements(color) ne 0 then self.color=0>color<(!D.N_COLORS-1) else  $
    self.color=!D.N_COLORS/2
   self.button=2b^fix(but)      ;pick which button
   self.recip.BUTTON=1b
   self.recip.EXCLUSIVE=keyword_set(exc)
   self.recip.ACTIVE=1b-self.recip.EXCLUSIVE
   self->Update                 ;sign up with the draw widget
   self.oDraw->GetProperty,WINSIZE=winsize,PIXWIN=pixwin
   self.thick=1.5*float(max(winsize))/256.<2
   self.pixwin=pixwin & self.winsize=winsize
   return,1
end

;=============================================================================
;	tvZoom__define.  Prototype the tvZoom class.
;=============================================================================
pro tvZoom__define
   struct={tvZoom, $ 
           INHERITS tvPlug,$    ;make it a tvDraw plug-in
           button:0b, $         ;which button to activate on
           buttondwn:0b,$       ;whether a down button press was received
           orig:[0,0], $        ;original coordinates of a button press
           save:[0,0], $        ;the last saved coordinates
           thick:0.0, $         ;width of the drawing line
           color:0, $           ;the color index for the band  
           pixwin:0, $          ;the pixwin id of the tvDraw object
           winsize:[0,0], $     ;size of the window displayed
           zoomlist:ptr_new()}  ;a list of previous zooms and positions  
   return
end












