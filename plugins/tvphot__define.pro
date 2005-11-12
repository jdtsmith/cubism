
;;**************************OverRiding methods********************************
;=============================================================================
;	Message. Only snapshot, and box messages.
;=============================================================================
pro tvPhot::Message, msg
  self->tvPlug::Message,msg,TYPE=type
  ;; Whenever we get a POST, or a BOX, a redraw will let us change up
  case type of
     'BOX': self->Phot
     'TVDRAW_POSTDRAW': self->Phot              
     'TVDRAW_SNAPSHOT': self->Draw ;Make sure our circles are visible
  endcase 
end 

;=============================================================================
;	On:  Get all our messages.
;=============================================================================
pro tvPhot::On
  if self->On() then begin      ;if turned on *again* .. means turn *off*!
     self.Box->Reset
     self->Off
     return
  end
  self->tvPlug::On
  self.box->On
  self.oDraw->MsgSignup,self,/TVDRAW_POSTDRAW,/TVDRAW_SNAPSHOT
  if widget_info(self.wBase,/VALID_ID) eq 0 then self->wShow
  if self.Box->IsDrawn() then self->Phot
end

;=============================================================================
;       Off - Our box will stay around for us
;=============================================================================
pro tvPhot::Off
  if widget_info(self.wBase,/VALID_ID) then self->wDestroy
  self.box->Off
  self.oDraw->MsgSignup,self,/NONE
  if self->On() then self.oDraw->Redraw,/SNAPSHOT
  self->tvPlug::Off
end

function tvPhot::Icon
  return,[[  0b,  0b],[224b,  7b],[248b, 31b],[252b, 63b], $
          [ 60b, 60b],[ 30b,120b],[142b,112b],[142b,115b], $
          [206b,113b],[ 14b,113b],[ 30b,120b],[ 60b, 60b], $
          [252b, 63b],[248b, 31b],[224b,  7b],[  0b,  0b]]
end

function tvPhot::Description
  return,'Aperture Photometry'
end
;;************************End OverRiding methods*******************************

pro tvPhot::GetProperty, Phot=phot, Rad=rad, SkyWidth=sw, SKY=sky
  if arg_present(phot) then phot=self.Phot
  if arg_present(rad) then rad=self.Rad
  if arg_present(srad) then sw=self.SkyWidth
  if arg_present(sky) then sky=self.Sky
end

pro tvPhot::Draw
  if self.Box->IsDrawn() AND self.cntrd[0] ne -1. then begin 
     ;; draw a small circle on the centroid center.
     cendev=round(self.oDraw->Convert(self.cntrd,/FRAC,/DEVICE))
     self.oDraw->GetProperty,Zoom=zm
     self.Box->GetProperty,COLOR=cl
     x=findgen(250)/249.*2*!PI & y=sin(x) & x=cos(temporary(x))
     plots,2*x+cendev[0],2*y+cendev[1], COLOR=cl,THICK=1.5,/DEVICE
     if (self.photgood and 2b) ne 0b then begin ;a standard only centroid
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

function tvPhot::Centroid, im, ERROR=err,FWHM=fwhm, SILENT=silent,TRUST=tr
  if n_elements(fwhm) eq 0 then fwhm=5
  if n_elements(tr) eq 0 then tr=.2 else tr=0.>tr<1.
  err=0b
  
  if size(im,/N_DIMENSIONS) ne 2 then begin 
     err=1b
     return,[-1.,-1.]
  endif 
  
  s=size(im,/DIMENSIONS)
  
  ;; regular centroid
  tm=total(im,2,/NAN) & x=total(tm*(findgen(s[0])+.5),/NAN)/total(tm)
  tm=total(im,1,/NAN) & y=total(tm*(findgen(s[1])+.5),/NAN)/total(tm)
  
  ;; daophot-style spatial derivative centroid
  x2=round(x) & y2=round(y)     ;central pixel for centroid box (derivatives)
  
  nhalf =  fix(0.637*fwhm) > 2  ;
  nbox = 2*nhalf+1              ;Width of box to be used to compute centroid
  
  l=(x2-nhalf) & r=(x2+nhalf)
  b=(y2-nhalf) & t=(y2+nhalf)
  if l lt 1. or r gt s[0]-2. or b lt 1. or t gt s[1]-2. then begin
     if keyword_set(silent) eq 0b then  $
        message,/INFO,'The standard centroid falls too far from the center.'
     err=1b
     return,[-1.,-1.]
  endif 
  
  cbox=im[l:r,b:t]
  pos= findgen(nbox-1) + 0.5 - nhalf ;we truncate the last column/row
  
  ;; Weighting factor W unity in center, 0.5 at end, and linear in between 
  w=1.-0.5*(abs(pos)-0.5)/(nhalf-1.) 
  sumc=total(w)
  
  ;; compute x centroid
  d=shift(cbox,-1,0)-cbox       ;derivative in x direction
  d=d[0:nbox-2,*]               ;don't take the last edge
  d=total(d,2)
  sumd=total(w*d) & sumxd=total(w*d*pos) & sumxsq=total(w*pos^2)
  dx=sumxsq*sumd/(sumc*sumxd)
  if ( abs(dx) GT nhalf ) then begin ;Reject if centroid outside box
     err=2b
     if not keyword_set(silent) then $
        message,/INF,'Derivative X centroid out of range, using standard only.'
  endif else begin
     x2=x2+.5-dx & x=tr*x+(1.-tr)*x2
  endelse 
  
  ;; compute y centroid
  d=shift(cbox,0,-1)-cbox       ;derivative in x direction
  d=d[*,0:nbox-2]               ;don't take the last edge
  d=total(d,1)
  sumd=total(w*d) & sumxd=total(w*d*pos) & sumxsq=total(w*pos^2)
  
  dy=sumxsq*sumd/(sumc*sumxd)
  if ( abs(dy) GT nhalf ) then begin ;Reject if centroid outside box
     err=2b
     if not keyword_set(silent) then $
        message,/INF,'Derivative Y centroid out of range, using standard only.'
  endif else begin
     y2=y2+.5-dy & y=tr*y+(1.-tr)*y2
  endelse 
  
  if ~finite(x) || ~finite(y) then begin 
     err=1b
     return,[-1.,-1.]
  endif 
  
  if x lt 0. or x ge (r-1.) or y lt 0. or y ge (t-1.) then begin 
     if not keyword_set(silent) then $
        message,/INF,'Centroid lies outside of sub-array.'
     err=1b
     return,[-1.,-1.]
  endif 
  
  return,[x,y]
end

pro tvPhot::ApPhot, image, cntrd,SILENT=silent,ERROR=err
  srad=self.Rad+self.SkyWidth
  rad=self.Rad  
  err=0b
  
  ;; form an array which is the distance at each pixel from the centroid
  n=size(image,/DIMENSIONS)
  r=lindgen(n)
  r=sqrt((r mod n[0]+.5-cntrd[0])^2+(r/n[0]+.5-cntrd[1])^2)
  incircle=where(r le rad,cnt)  ;where we're in the circle
  insky=where(r le srad and r gt rad,scnt) ;where we're in the sky
  if cnt eq 0 or scnt eq 0 then begin
     err=1b
     return
  endif 
  
  ;; approximate the fraction of edge pixels.
  fractn= 0.0 > rad-r[incircle] < 1.0
  fractnsky= 0.0 > srad-r[insky] < 1.0

  ;; Compute the trimmed average sky background
  sky=image[insky]
  s=sort(sky)
  s=s[scnt/20:19*scnt/20]       ;remove top and bottom 5% from the sort
  sky=sky[s] 
  fractnsky=fractnsky[s]
  
  self.Sky=total(sky*fractnsky)/total(fractnsky)
  self.Phot=total(image[incircle]*fractn)-self.Sky*total(fractn)
end

pro tvPhot::Phot
  if NOT self.Box->IsDrawn() then return
  self.Box->Getlrtb,l,r,t,b
  self.oDraw->GetProperty,imorig=io,SIZE=sz ;get the image pointer
  if NOT ((l le r) and (b le t)) then return
  if r ge sz[0] OR t ge sz[1] then begin
     self->Off                  ;panic
     return
  endif
  take=(*io)[l:r,b:t]
  
  if self.do_cntrd then begin 
     ;;take as fwhm either half the geometric mean of box sides or the 
     ;;radius of the circle, whichever is smaller, or 4 if both smaller
     fw=self.rad<sqrt((r-l+1.)*(t-b+1.))/2.> 4.
     ;; the image centroid
     self.cntrd=self->Centroid(take,ERROR=cerr,FWHM=fw,TRUST=0.,/SILENT)
  endif else begin 
     cerr=0b
     self.cntrd=[float(r-l+1)/2.,float(t-b+1)/2.]
  endelse 
  
  if cerr eq 1b then begin      ;no good centroid found
     str=string(FORMAT='("CEN:  [",2A6,"] PHOT:",A12," SKY:",A12)', $
                '***','***', '***','***')
  endif else begin 
     self.cntrd=self.cntrd+[l,b] ;offset into the full array
     low=(floor(self.cntrd-self.Rad-self.SkyWidth)-1)>0
     high=(ceil(self.cntrd+self.Rad+self.SkyWidth)+1)< $
          (size(*io,/DIMENSIONS)-1)
     take=(*io)[low[0]:high[0],low[1]:high[1]]
     photcen=self.cntrd-low     ;relative to our new piece of the image
     self->ApPhot,take,photcen,ERROR=err,/SILENT
     self.photgood=(1b-err)+2b*(cerr eq 2b)
     if self.photgood then begin 
        str=string(FORMAT='("CEN:[",2F6.2,"]  PHOT:",G12.6," SKY:",G12.6)', $
                   self.cntrd,self.phot,self.sky )
     endif else $ 
        str=string(FORMAT='("CEN:[",2F6.2,"]  PHOT:",A12," SKY:",A12)', $
                   self.cntrd,'***','***') 
  endelse 
  widget_control, self.wSlab, set_value=str
  erase
  self.oDraw->ReDraw,/SNAPSHOT
end

pro tvphot_event, ev
  widget_control, ev.handler, get_uvalue=self
  self->Event,ev
end

pro tvPhot::Event,ev
  new=0
  case ev.id of
     self.wRad: begin 
        catch, converr
        if converr ne 0 then begin 
           catch,/CANCEL
           widget_control, ev.handler, set_value=string(self.rad, $
                                                        FORMAT='(F5.2)')
           return
        endif 
        widget_control, ev.id, get_value=tmp
        newrad=float(tmp[0])
        catch,/CANCEL
        if newrad le 1.5 then begin
           widget_control, ev.id, set_value=string(self.rad, $
                                                   FORMAT='(F5.2)')
           return
        endif else  $
           widget_control, ev.id, set_value=string(newrad,FORMAT='(F5.2)')
        if newrad ne self.rad then new=1   
     end
     
     self.wSW:  begin           ;the width
        catch, converr
        if converr ne 0 then begin 
           catch,/CANCEL
           widget_control, ev.handler, set_value=string(self.SkyWidth, $
                                                        FORMAT='(F5.2)')
           return
        endif 
        widget_control, ev.id, get_value=tmp
        newwidth=float(tmp[0])
        catch,/CANCEL
        if newwidth lt 1.0 then begin 
           widget_control, ev.id, set_value=string(self.SkyWidth, $
                                                        FORMAT='(F5.2)')
           return
        endif else $
           widget_control, ev.id, $
                           set_value=string(newwidth,FORMAT='(F5.2)')
        if newwidth ne self.SkyWidth then new=2
     end
     
     else: begin                ;centroid toggled
        new=3                   
        self.do_cntrd=ev.select
     endelse
  endcase 
  
  ;; do it again if changed
  if new ne 0 then begin 
     if new eq 2 then self.SkyWidth=newwidth else $
        if new eq 1 then self.Rad=newrad           
     self->Phot
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
  widget_control, self.parent,UPDATE=0
  self.wBase=widget_base(self.parent,/COLUMN,SPACE=1)
  ;; A cosmetic line...
  self.oDraw->GetProperty, WINSIZE=ws
  ln=widget_base(self.wBase,FRAME=2,ysize=0,xsize=ws[0]-5)
  rbase=widget_base(self.wBase,/ROW,/ALIGN_CENTER, $
                    EVENT_PRO='tvphot_event',uvalue=self)
  lab=widget_label(rbase,value='Radius:')
  self.wRad=widget_text(rbase,/EDITABLE, XSIZE=5, $
                        value=string(FORMAT='(F5.2)',self.rad))
  lab=widget_label(rbase,value='Sky Width:')
  self.wSW=widget_text(rbase,/EDITABLE,XSIZE=5, $
                       value=string(FORMAT='(F5.2)',self.SkyWidth))
  self.wCen=cw_bgroup(rbase,/NONEXCLUSIVE,'Centroid',SET_VALUE=[self.do_cntrd])
  b2=widget_base(self.wBase,/ROW,/ALIGN_CENTER)
  self.wSlab=widget_label(b2,_EXTRA=e,value=string(FORMAT='(T58,A)'," "))
  widget_control, self.parent,UPDATE=1
end


;=============================================================================
;   Init - Initialize the Phot object.  All tvRBox keywords are
;          relevant (see tvrbox).  If HIDE is set, the widget is
;          hidden when turned off.
;=============================================================================
function tvPhot::Init,parent,oDraw,HIDE=hide,RADIUS=rad,SKY_WIDTH=sw,_EXTRA=e
  if (self->tvPlug::Init(oDraw,_EXTRA=e) ne 1) then return,0 ;chain up
  self.hide=keyword_set(hide)
  self.parent=parent
  if n_elements(rad) eq 0 then self.rad=12.0 else self.rad=rad
  if n_elements(sw) ne 0 then self.SkyWidth=sw else self.SkyWidth=2.5
  self.cntrd=[-1.,-1.]          ;no centroid yet!
  self.do_cntrd=1               ;default to doing it
  ;; Get a tvrbox object, signing *ourself* up for box messages.
  self.Box=obj_new('tvrbox', oDraw,/CORNERS,/NO_REDRAW,_EXTRA=e)
  self.Box->MsgSignup,self,/BOX
  self->Off
  return,1
end

;=============================================================================
;	tvPhot__define -  Prototype the tvPhot class.
;=============================================================================
pro tvPhot__define
  struct={tvPhot, $ 
          INHERITS tvPlug, $    ;make it a plug-in
          box:obj_new(), $      ;a tvRBox to use.
          cntrd:[0.,0.], $      ;the centroid
          parent:0L, $          ;the parent base         
          photgood:0b, $        ;whether a good phot has been obtained
          hide:0b, $            ;whether to hide the phot line when off
          do_cntrd:0b, $        ;Do the centroid
          wBase:0L, $           ;a base to put the text widget in
          Rad:0.0, $            ;the radius of the phot aperture
          SkyWidth:0.0, $       ;the delta radius of the sky from phot aperture
          wRad:0L, $            ;the radius selector
          wSW:0L, $             ;the sky width
          wCen:0L, $            ;choice of whether to compute centroid or not
          phot:0.0, $           ;the measured phot
          sky:0.0, $            ;the sky flux
          wSlab:0L}             ;a text widget for the stats
end
