;=============================================================================
;  Message - Display the values.  We have signed up for motion and
;            tracking messages, and will hear from the CubeRec widget
;            on changes in mode, etc.
;=============================================================================
pro WSMask::Message,msg
  self->tvPlug_lite::Message,msg,TYPE=type ;pass it up
  case type of 
     'CUBEREC_UPDATE': begin    ;cuberec tells us about new bcd's/cubes
        if ~msg.bcd_mode then begin 
           self->Off,/DISABLE
           return
        endif else self->Enable
        ;; Change in cube: update mask
        if self.cube ne msg.cube then ptr_free,self.mask
        self.cube=msg.cube
     end
     
     'TVDRAW_PREDRAW': begin
        self.cube->GetProperty,APERTURE=ap,/POINTER
        if ~ptr_valid(self.aper) || $
           self.aper ne ap then begin 
           ptr_free,self.mask
           self.aper=ap
        endif 
        self->Blank,msg.im      ;set the image blank
     end 
  endcase 
end 

pro WSMask::Off,_EXTRA=e
  self.oDraw->MsgSignup,self,/NONE
  self.oDraw->Draw
  self->tvPlug::Off,_EXTRA=e
end

pro WSMask::On
  self.oDraw->MsgSignup,self,/TVDRAW_PREDRAW
  self.oDraw->Draw
  self->tvPlug::On
end

function WSMask::Icon
  return, [[199B, 049B],[199B, 049B],[199B, 049B],[199B, 113B],$
           [199B, 113B],[135B, 097B],[143B, 227B],[142B, 227B],$
           [142B, 227B],[014B, 199B],[030B, 199B],[028B, 199B],$
           [028B, 143B],[028B, 142B],[028B, 014B],[028B, 014B]]

end

function WSMask::Description
  return,'Mask Outside of WAVSAMP'
end

;=============================================================================
;  MaskGen - Generate the mask
;=============================================================================
pro WSMask::MaskGen,MODULE=md
  self.cube->GetProperty,BCD_SIZE=sz
  mask=make_array(VALUE=1b,sz)
  prs=self.cube->PRs(/ALL_ORDERS,ORDERS=ords)
  
  for ord=0,n_elements(ords)-1 do $
     for i=0,n_elements(*prs[ord])-1 do $
        mask[*(*prs[ord])[i].PIXELS]=0b
  ptr_free,self.mask
  self.mask=ptr_new(mask,/NO_COPY)
  ptr_free,prs
end

;=============================================================================
;  Blank
;=============================================================================
pro WSMask::Blank,im_ptr
  if ~ptr_valid(self.mask) then self->MaskGen
  mn=mean(*im_ptr,/NAN)
  (*im_ptr)[where(*self.mask)]=mn
end

;=============================================================================
;  Cleanup
;=============================================================================
pro WSMask::Cleanup
  ptr_free,self.mask,self.aper
  self->tvPlug::Cleanup
end

;=============================================================================
;  WSMask__define - Prototype the WSMask class.
;=============================================================================
pro WSMask__define
  struct={WSMask, $ 
          INHERITS tvPlug,$     ;make it a plug-in
          cube: obj_new(), $    ;the cube
          aper: ptr_new(), $    ;saved aperture info
          mask:ptr_new()}       ;the mask to apply

end
