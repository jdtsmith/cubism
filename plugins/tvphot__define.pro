
;;**************************OverRiding methods********************************
;=============================================================================
;	Message. Only exclusive,redraw, and box messages.
;=============================================================================
pro tvPhot::Message, msg
   ;; let tvPlug handle the exclusive messages, if any...
   self->tvPlug::Message,msg,TYPE=type
   if type eq 'BOX' then begin ; a message from our box
      self->Erase               ;Erase in case the centroid is gone
      self->Phot                ;calculate and display photometry
      return
   endif 
   
   ;; We'll always get REDRAW, since we might have corners during inactivity
   ;; but we'll only do anything if we're both on and our box is drawn.
   if type eq 'TVDRAW_REDRAW' then begin 
      if NOT self.Box->IsDrawn() then return
      self.Box->Message,msg     ;first alert our box, he'll redraw.
      if NOT self.recip.ACTIVE then return
      self->Erase
      self->Phot                ; original image redrawn
      self->Draw                ;Draw us to make sure our circles are visible
   endif 
end 

pro tvPhot::Update, ac
   if n_elements(ac) eq 0 then begin ;if no active change, just do the update
      self->tvPlug::Update
      return
   endif 
   ;; otherwise only toggle box and map if active status has actually changed.
   case ac of
      0b: begin                  ;just turned off
         self.box->Off
         if self.hide then self->wDestroy
      end
      1b: begin                  ;just turned on
         self.box->On
         if self.hide then self->wShow
         if self.Box->IsDrawn() then begin
            self->Draw
            self->Phot
         endif 
      end 
      
      2b: begin                 ;clicked or hit again
         self.Box->Reset        ;reset it
      end
   endcase
   self->tvPlug::Update,ac      ;chain up, sending only for change.
end
   
function tvPhot::Icon
   return,[[  0b,  0b],[224b,  7b],[248b, 31b],[252b, 63b], $
           [ 60b, 60b],[ 30b,120b],[142b,112b],[142b,115b], $
           [206b,113b],[ 14b,113b],[ 30b,120b],[ 60b, 60b], $
           [252b, 63b],[248b, 31b],[224b,  7b],[  0b,  0b]]
end

;;************************End OverRiding methods*******************************

pro tvPhot::GetProperty, Phot=phot, Rad=rad, SkyWidth=sw, SKY=sky
   if arg_present(phot) then phot=self.Phot
   if arg_present(rad) then rad=self.Rad
   if arg_present(srad) then sw=self.SkyWidth
   if arg_present(sky) then sky=self.Sky
end

pro tvPhot::Erase
   if self.rad eq 0. then return
   ;; get the clean pixwin and the size of the window
   self.oDraw->GetProperty,PIXWIN=pw,WINSIZE=ws
   rad=self.Rad+self.SkyWidth
   low=fix(self.cntrd-rad-3)
   low=(ws-1)<self.oDraw->Convert(low,/DEVICE)>0
   high=fix(self.cntrd+rad+3)
   high=0>self.oDraw->Convert(high,/DEVICE)<(ws-1)
   if (where((high-low) eq 0))[0] ne -1 then return
   device,copy=[low,high-low,low,pw]
end

pro tvPhot::Draw
   self.Box->drawbox
   if self.cntrd[0] ne -1. then begin 
      ;; draw a small circle on the centroid center.
      cendev=round(self.oDraw->Convert(self.cntrd,/FRAC,/DEVICE,ZOOM=zm))
      self.Box->GetProperty,COLOR=cl
      x=findgen(250)/249.*2*!PI & y=sin(x) & x=cos(temporary(x))
      plots,2*x+cendev[0],2*y+cendev[1], COLOR=cl,THICK=1.5,/DEVICE
      if (self.photgood and 2b) ne 0 then begin ;a standard only centroid
         xtmp=[-3,3] & ytmp=[3,-3] ;put an "x" through the circle
         plots,xtmp+cendev[0],ytmp+cendev[1], COLOR=cl,THICK=1.5,/DEVICE
         ytmp=[-3,3]
         plots,xtmp+cendev[0],ytmp+cendev[1], COLOR=cl,THICK=1.5,/DEVICE
      endif 
      if self.photgood and 1b then begin 
         ;; draw a larger circle of radius self.Rad centered on centroid
         plots,self.Rad*zm*x+cendev[0],self.Rad*zm*y+cendev[1],COLOR=cl, $
          THICK=1.5,/DEVICE
         rad2=self.Rad+self.SKyWidth
         plots,rad2*zm*x+cendev[0],rad2*zm*y+cendev[1], $
          COLOR=cl,THICK=1.5,/DEVICE
      endif 
   endif 
end

pro tvPhot::Phot
   if NOT self.Box->IsDrawn() then return
   self.Box->Getlrtb,l,r,t,b
   self.oDraw->GetProperty,imorig=io ;get the image pointer
   if NOT ((l le r) and (b le t)) then return
   take=(*io)[l:r,b:t]
   
   ;;take as fwhm either half the geometric mean of box sides or the 
   ;;radius of the circle, whichever is smaller, or 4 if both smaller
   fw=self.rad<sqrt((r-l+1.)*(t-b+1.))/2.> 4
   ;; the image centroid
   self.cntrd=cen(take,ERROR=cerr, FWHM=fw,TRUST=0.,/SILENT)
   if cerr eq 1b then begin     ;no good centroid
      str=string(FORMAT='("CENT:  [",2A6,"] PHOT:",A12," SKY:",A12)', $
                 '***','***', '***','***')
   endif else begin 
      self.cntrd=self.cntrd+[l,b] ;in real terms
      skyrad=self.Rad+self.SkyWidth
      low=(floor(self.cntrd-SkyRad)-1)>0
      high=(ceil(self.cntrd+SkyRad)+1)<(size(*io,/DIMENSIONS))[0]
      take=(*io)[low[0]:high[0],low[1]:high[1]]
      photcen=self.cntrd-low    ;in terms of our new take
      phot=aphot(take,photcen,ERROR=err,SKYRADIUS=SkyRad, $
                 RADIUS=self.rad,/SILENT,SKYVAL=sky)
      self.photgood=(1b-err)+2b*(cerr eq 2b)
      if self.photgood then begin 
         self.phot=phot
         self.sky=sky
         str=string(FORMAT='("CENT:[",2F6.2,"]  PHOT:",G12.6," SKY:",G12.6)', $
                    self.cntrd,self.phot,self.sky )
      endif else $ 
       str=string(FORMAT='("CENT:[",2F6.2,"]  PHOT:",A12," SKY:",A12)', $
                  self.cntrd,'***','***') 
   endelse 
   widget_control, self.wSlab, set_value=str
   ;; note we do not draw here, since our box sends a redraw just after the
   ;; BOX message.
end

pro tvphot_event, ev
   widget_control, ev.handler, get_uvalue=self
   self->Event,ev
end

pro tvPhot::Event,ev
   widget_control, ev.handler, get_value=tmp
   
   
   new=0
   if ev.handler eq self.wRad then begin 
      catch, converr
      if converr ne 0 then begin 
         catch,/CANCEL
         widget_control, ev.handler, set_value=string(self.rad,FORMAT='(F5.2)')
         return
      endif 
      newrad=float(tmp[0])
      catch,/CANCEL
      if newrad le 1.5 then begin
         widget_control, ev.handler, set_value=string(self.rad,FORMAT='(F5.2)')
         return
      endif else  $
       widget_control, ev.handler, set_value=string(newrad,FORMAT='(F5.2)')
      if newrad ne self.rad then new=1   
   endif else begin             ;the width
      catch, converr
      if converr ne 0 then begin 
         catch,/CANCEL
         widget_control, ev.handler, set_value=string(self.SkyWidth, $
                                                      FORMAT='(F5.2)')
         return
      endif 
      newwidth=float(tmp[0])
      catch,/CANCEL
      if newwidth lt 1.0 then begin 
         widget_control, ev.handler, set_value=string(self.SkyWidth, $
                                                      FORMAT='(F5.2)')
         return
      endif else $
       widget_control, ev.handler, set_value=string(newwidth,FORMAT='(F5.2)')
      if newwidth ne self.SkyWidth then new=2
   endelse 
   
   ;; do it again if changed
   if new ne 0 then begin 
      self->Erase               ;Erase the *old* Circles
      self.Box->EraseBox        ;Erase the Box
      self.Box->DrawBox         ;Draw the Box
      if new eq 2 then self.SkyWidth=newwidth else self.Rad=newrad           
      self->Phot
      self->Draw                ;Draw the Circles
   endif 
end

;=============================================================================
;	wDestroy: Destroy the widgets for photometry
;=============================================================================
pro tvPhot::wDestroy
   widget_control, self.wBase,/DESTROY
end

;=============================================================================
;	wShow: Show the widgets for photometry
;=============================================================================
pro tvPhot::wShow
   if self.recip.ACTIVE ne 1b then return ;show only if active
   widget_control, self.parent,UPDATE=0
   self.wBase=widget_base(self.parent,/COLUMN,SPACE=1)
   ;; A cosmetic line...
   self.oDraw->GetProperty, WINSIZE=ws
   ln=widget_base(self.wBase,FRAME=2,ysize=0,xsize=ws[0]-5)
   rbase=widget_base(self.wBase,/ROW,/ALIGN_CENTER)
   lab=widget_label(rbase,value='Photometry Radius:')
   self.wRad=widget_text(rbase, uvalue=self, EVENT_PRO='tvphot_event', $
                         /EDITABLE, value=string(FORMAT='(F5.2)',self.rad), $
                         XSIZE=5)
   lab=widget_label(rbase,value='Sky Radius Width:')
   self.wSW=widget_text(rbase,uvalue=self, $
                         EVENT_PRO='tvphot_event',/EDITABLE,XSIZE=5, $
                         value=string(FORMAT='(F5.2)',self.SkyWidth))
   b2=widget_base(self.wBase,/ROW,/ALIGN_CENTER)
   self.wSlab=widget_label(b2,_EXTRA=e,value=string(FORMAT='(T58,A)'," "))
   widget_control, self.parent,UPDATE=1
end

;=============================================================================
;	Cleanup.  Clean self up
;=============================================================================
pro tvPhot::Cleanup
   self->tvPlug::Cleanup
end

;=============================================================================
;   init.  Initialize the Phot object.  All tvRBox keywords are relevant
;   (see tvrbox).  If HIDE is set, the widget is hidden when turned off.
;=============================================================================
function tvPhot::Init,parent,oDraw,EXCLUSIVE=exc,HIDE=hide, $
               RADIUS=rad,SKY_WIDTH=sw,_EXTRA=e
   
   if (self->tvPlug::Init(oDraw,_EXTRA=e) ne 1) then return,0 ;chain up
   self.hide=keyword_set(hide)
   self.parent=parent
   if n_elements(rad) eq 0 then self.rad=12.0 else self.rad=rad
   if n_elements(sw) ne 0 then self.SkyWidth=sw else self.SkyWidth=2.5
   
   self.cntrd=[-1.,-1.]         ;no centroid yet!
   
   ;; Sign up with Draw object for exclusives only (we lead the tvrbox)
   self.recip.EXCLUSIVE=keyword_set(exc)
   self.recip.ACTIVE=1b-self.recip.EXCLUSIVE ;don't activate yet if exclusive
   self.recip.REDRAW=1b
   
   self->wShow 
   
   ;; Get a tvrbox object, signing *ourself* up for box messages.
   self.box=obj_new('tvrbox', oDraw, MsgList=[self],_EXTRA=e)
   
   self->Update               
   return,1
end

;=============================================================================
;	tvPhot__define.  Prototype the tvPhot class.
;=============================================================================
pro tvPhot__define
   struct={tvPhot, $ 
           INHERITS tvPlug, $   ;make it a plug-in
           box:obj_new(), $     ;a tvRBox to use.
           cntrd:[0.,0.], $     ;the centroid
           parent:0L, $         ;the parent base         
           photgood:0b, $       ;whether a good phot has been obtained
           hide:0b, $           ;whether to hide the phot line when off
           wBase:0L, $          ;a base to put the text widget in
           Rad:0.0, $           ;the radius of the phot aperture
           SkyWidth:0.0, $      ;the delta radius of the sky from phot aperture
           wRad:0L, $           ;the radius selector
           wSW:0L, $            ;the sky width
           phot:0.0, $          ;the measured phot
           sky:0.0, $           ;the sky flux
           wSlab:0L}            ;a text widget for the stats
end




