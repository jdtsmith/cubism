;-----------------------------------------------------------------------
;        MESSAGE FUNCTION
;-----------------------------------------------------------------------
;; We're only getting key messages
pro tvPSelect::Message,msg
  up=msg.key eq self.switch_key[0] & down=msg.key eq self.switch_key[1]
  if NOT up and NOT down then return
  pl=widget_info(self.wList,/DROPLIST_SELECT) 
  np=widget_info(self.wList,/DROPLIST_NUMBER) 
  if down then if pl eq 0 then pl=np-1 else pl=pl-1 ;decrement beam
  if up then if pl eq np-1 then pl=0 else pl=pl+1 ;increment beam
  widget_control, self.wList,SET_DROPLIST_SELECT=pl
  ;; fake a droplist event
  self->Event,{index:pl}
end


;-----------------------------------------------------------------------
;        PROPERTY  FUNCTION
;-----------------------------------------------------------------------

pro tvPSelect::GetProperty, PLANES=planes, IMAGE=image
  if arg_present(planes) then planes=self.Planes
  if arg_present(image) then image =self.Image
end 


pro tvPSelect::SetProperty, PLANES=pl
  if keyword_set(pl) then begin
     ptr_free,self.Planes
     self.Planes=ptr_new(pl)
     widget_control, self.wList, SET_VALUE=strtrim(pl,2)
  endif 
end

;-----------------------------------------------------------------------
;        EVENT FUNCTION
;-----------------------------------------------------------------------

pro tvPSelect_Event,ev
  widget_control, ev.handler, get_uvalue=self
  self->Event,ev
end


pro tvPSelect::Event, ev
  ;; Set the next image, without changing zoom etc (same sized image)
  self.oDraw->SetProperty, IMORIG=(*self.Image)[*,*,ev.index],/NO_RESIZE
end


;-----------------------------------------------------------------------
;        CLEANUP FUNCTION
;-----------------------------------------------------------------------
pro tvPSelect::Cleanup
  ptr_free,self.Image,self.Planes
  self->tvPlug::Cleanup
end


;-----------------------------------------------------------------------
;        START FUNCTION
;-----------------------------------------------------------------------


pro tvPSelect::Start
  self->tvPlug::Start
  ;; Probe for a tvKey plug-in, and sign up with it.
  oKey=(self.oDraw->GetMsgObjs(CLASS='tvKEY'))[0]
  if obj_valid(oKey) then oKey->MsgSignup,self,/TVKEY_KEY
end

;-----------------------------------------------------------------------
;        INIT FUNCTION
;-----------------------------------------------------------------------


function tvPSelect::Init, parent, oDraw, im,COLUMN=col,NOLABEL=nl, $
                          START=strt,PLANES=pl,SWITCH_KEYs=sk,_EXTRA=e
  if (self->tvPlug::Init(oDraw,_EXTRA=e) ne 1) then return,0 ;chain up
  s=size(im,/DIMENSIONS)
  if n_elements(s) eq 2 then begin ; I'm not needed!
     obj_destroy,self
     return,1
  endif 
  
  if n_elements(sk) eq 0 then self.switch_key=[',','.'] else $
     self.switch_key=sk
  
  self.Image=ptr_new(im)
  if keyword_set(pl) then self.Planes=ptr_new(pl) else $
     self.Planes=ptr_new(lindgen(s[2]))
  if keyword_set(nl) eq 0b then begin 
     if keyword_set(column) then base=widget_base(parent,/COL) else $
        base=widget_base(parent,/ROW)
     label=widget_label(base,value='Plane:')
  endif else base=parent
  if n_elements(strt) eq 0 then strt=0
  self.wList=widget_droplist(base,VALUE=strtrim(*self.Planes,2), UVALUE=self,$
                             EVENT_PRO='tvpselect_event')
  if strt ne 0 then widget_control, self.wList, set_droplist_select=strt
  return,1
end
;-----------------------------------------------------------------------
;        STRUCTURE DEFINITION 
;-----------------------------------------------------------------------
pro tvPSelect__define
  struct={tvPSelect, $
          Inherits tvPlug, $
          Image:ptr_new(), $    ;the image *cube*
          switch_key:['',''], $ ;keys to use for cycling planes
          Planes: ptr_new(), $  ;list of possible planes
          Cur: 0L, $            ;which plane is the current plane
          wList:0L}             ;the widget droplist for selecting
end
