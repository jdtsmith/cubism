
;;**************************OverRiding methods********************************
;=============================================================================
;	Message.  Override the ObjMsg Message to do zooms.  We have
;	signed up for button messages from the tvDraw object.
;=============================================================================
pro tvZoom::Message, msg
  self->tvPlug::Message,msg,TYPE=type ;pass it up to tvPlug
  case type of 
     'TVDRAW_PREDRAW': begin 
        s=size(*msg.im,/DIMENSIONS)
        if ~array_equal(s,self.imsize) then begin 
           self.imsize=s
           ptr_free,self.zoomlist ;new image
        endif 
     end 
     'DRAW_MOTION': begin 
        if self.buttondwn eq 0b then return
        if self.buttondwn AND 1b then self->ZoomBox,msg.X,msg.Y $
        else self->Pan,msg.X,msg.Y,msg.modifiers
     end

     
     'DRAW_BUTTON': begin
        case msg.type of 
           0: begin             ;button press
              self->UpdateDrawSize
              if msg.press eq 4b then begin ;right, zoom out
                 n=ptr_valid(self.zoomlist)?n_elements(*self.zoomlist):0
                 if msg.clicks eq 2 || n lt 2 then begin ;all the way out
                    self.oDraw->GetProperty, SIZE=size
                    self.oDraw->SetProperty,offset=[0,0],dispsize=size   
                    ptr_free,self.zoomlist ;no zoom list left
                 endif else begin ;a single click, 2 or more levels of zoom
                    self.oDraw->SetProperty, $
                       DISPSIZE=(*self.zoomlist)[n-2].Size, $
                       OFFSET=(*self.zoomlist)[n-2].Off
                    *self.zoomlist=(*self.zoomlist)[0:n-2] ;remove newest
                 endelse 
              endif else begin  ; zoom or pan
                 self.orig=[msg.X,msg.Y]
                 ;;sign up for motion events.
                 self.oDraw->MsgSignup,self,/DRAW_MOTION
                 self.buttondwn=msg.press
                 if msg.press eq 2b then begin 
                    self.oDraw->GetProperty,offset=offset
                    self.save=offset
                 endif 
              endelse 
           end
           
           1: begin             ;button release
              if msg.release eq 4b then return ;nothing to do
              self.oDraw->MsgSignup,self,DRAW_MOTION=0 ;turn off motion 
              if self.buttondwn eq 1b then $
                 self->ZoomIt,msg.X,msg.Y ;zoom in on it
              self.buttondwn=0b ;no button down anymore
           end
        endcase
     end
     else:
  endcase
end 

pro tvZoom::On
  if self->On() then self->Off ;already on
  self->tvPlug::On
  self.oDraw->MsgSignup,self,/DRAW_BUTTON,/TVDRAW_PREDRAW
end

pro tvZoom::Off
  self->tvPlug::Off
  self.oDraw->MsgSignup,self,DRAW_BUTTON=0,/TVDRAW_PREDRAW
end

function tvZoom::Icon
  return,[[  0b,  0b],[224b,  3b],[ 56b, 14b],[ 24b, 12b], $
          [ 12b, 24b],[ 12b, 24b],[ 12b, 25b],[140b, 26b], $
          [ 12b, 25b],[ 24b, 12b],[ 56b, 14b],[224b,  3b], $
          [192b,  1b],[192b,  1b],[192b,  1b],[192b,  1b]]
end

function tvZoom::Description
  return,'Zoom/Pan Image'
end

function tvZoom::MouseHelp
  return,['Zoom In','Pan','Zoom Out']
end

;;************************End OverRiding methods*******************************

pro tvZoom::UpdateDrawSize
  self.oDraw->GetProperty,WINSIZE=ws
  self.winsize=ws
end

pro tvZoom::EraseBox,X,Y
  low=(([X,Y] < self.orig)-1) > 0 ;start two to the left
  high=([X,Y] > self.orig) < self.winsize
  dist=(high-low+2>0) < (self.winsize-low) > 1 ;box sides
  self.oDraw->Erase,low,dist
end

pro tvZoom::ZoomBox,X,Y
  if self.buttondwn AND 8b ne 0b then begin ;erase old
     self->EraseBox,self.save[0],self.save[1]
  endif else self.buttondwn=self.buttondwn OR 8b ;we're about to draw one
  plots, $                      ;plot the zoom box
         [self.orig[0],self.orig[0],X,X,  $
          self.orig[0]], $
         [self.orig[1],Y,Y, self.orig[1], $
          self.orig[1]], COLOR=self.color,THICK=self.thick, /DEVICE
  self.save=[X,Y]
end 

pro tvZoom::ZoomIt, X, Y
  self.oDraw->GetProperty,zoom=zoom,pan=pan,offset=offset,size=size, $
                          dispsize=ds
  left=FIX(((self.orig[0] < X)- $
            pan[0])/zoom) > 0
  right=FIX(((self.orig[0] > X)- $
             pan[0])/zoom) < (size[0]-1-offset[0])
  top=FIX(((self.orig[1] > Y)- $
           pan[1])/zoom) < (size[1]-1-offset[1])
  bottom=FIX(((self.orig[1] < Y )- $
              pan[1])/zoom) > 0
  
  ;; point-click: zoom in a factor of two
  if (left ge right) or (bottom ge top) then begin 
     pix=self.oDraw->Convert([X,Y],/SHOWING)
     if pix[0] eq -1 then begin ; it wasn't on a showing pixel
        self->EraseBox,X,Y
        return
     endif 
     ;; show as much as possible at 2x zoom, centered on pix
     max=self.winsize/(2*zoom)
     halfsize=floor(max/2-1)>1
     left=pix[0]-halfsize[0]>0
     right=pix[0]+halfsize[0]<(size[0]-1)
     bottom=pix[1]-halfsize[1]>0
     top=pix[1]+halfsize[1]<(size[1]-1)
     offset=[left,bottom]
  endif else offset=offset+[left,bottom]
  
  ;; set the tvDraw parameters
  dispsize=[right-left+1, top-bottom+1]
  
  ;; update the zoomlist
  if ptr_valid(self.zoomlist) then begin 
     n=n_elements(*self.zoomlist) 
     if array_equal((*self.zoomlist)[n-1].off, offset) AND $
        array_equal((*self.zoomlist)[n-1].size,dispsize) then return
     *self.zoomlist=[*self.zoomlist, {ZoomBox,offset, dispsize}]
  endif else  $
     self.zoomlist=ptr_new({ZoomBox,offset,dispsize})
  ;; set and draw it
  self.oDraw->SetProperty,offset=offset,dispsize=dispsize
end

pro tvZoom::Pan,X,Y,modifier
  self.oDraw->GetProperty,size=size,dispsize=ds
  dist=self.oDraw->Convert([X,Y]-self.orig,/DISTANCE)
  if array_equal(dist,0) then return
  if modifier eq 1b then begin 
     mn=min(abs(dist),pos)
     dist[pos]=0
  endif 
  offset=0>(self.save-dist)<(size-ds)
  ;; Update this zoom level
  if ptr_valid(self.zoomlist) then begin 
     n=n_elements(*self.zoomlist) 
     (*self.zoomlist)[n-1].off=offset
  endif 
  self.oDraw->SetProperty,OFFSET=offset,/DOUBLE_BUFFER
end

;=============================================================================
;	Cleanup.  Clean self up
;=============================================================================
pro tvZoom::Cleanup
  ptr_free,self.zoomlist
  self->tvPlug::Cleanup
end

;=============================================================================
;	init.  Initialize the zoomer with a tvDraw object
;=============================================================================
function tvZoom::Init,oDraw,COLOR=color,_EXTRA=e
  if (self->tvPlug::Init(oDraw,_EXTRA=e) ne 1) then return,0
  if n_elements(color) ne 0 then self.color=0>color<(!D.TABLE_SIZE-1) else  $
     self.color=!D.TABLE_SIZE/2
  self.oDraw->GetProperty,WINSIZE=winsize,PIXWIN=pixwin
  self.thick=1.5*float(max(winsize))/256.<2
  self.winsize=winsize
  self->Off
  return,1
end

;=============================================================================
;	tvZoom__define.  Prototype the tvZoom class.
;=============================================================================
pro tvZoom__define
  struct={tvZoom, $ 
          INHERITS tvPlug,$     ;make it a tvDraw plug-in
          buttondwn:0b,$        ;whether a down button press was received
          orig:[0,0], $         ;original coordinates of a button press
          save:[0,0], $         ;the last saved coordinates
          thick:0.0, $          ;width of the drawing line
          color:0, $            ;the color index for the band  
          winsize:[0L,0L], $    ;size of the window displayed
          imsize:[0L,0L], $     ;image size of displayed image
          zoomlist:ptr_new()}   ;a list of previous zooms and positions  
  struct={ZoomBox,Off:[0,0],Size:[0,0]}
  return
end
