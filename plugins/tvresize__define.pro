pro tvResize_event,ev
  widget_control, ev.id,GET_UVALUE=self
  self->Event,ev
end

pro tvResize::Event,ev
  widget_control, ev.id,GET_VALUE=val
  val=fix(val)
  self.oDraw->SetProperty,WINSIZE=[val,val]
end

pro tvResize::BuildMenu
  if ~ptr_valid(self.sizes) then return
  if ~widget_info(self.wMenu,/VALID_ID) then return
  for i=0,n_elements(*self.sizes)-1 do $
     b=widget_button(self.wMenu,VALUE=strtrim((*self.sizes)[i],2),UVALUE=self)
end


function tvResize::Init,oDraw,sizes,SIZE_MENU=menu
  if (self->tvPlug_lite::Init(oDraw,_EXTRA=e) ne 1) then return,0 ;chain up
  
  if n_elements(sizes) eq 0 then sizes=[256,384,512,768]
  self.sizes=ptr_new(sizes)
     
  if n_elements(menu) ne 0 then begin 
     if widget_info(menu,/VALID_ID) then begin 
        self.wMenu=widget_button(menu,VALUE='Set Size',/SEPARATOR,/MENU, $
                                 EVENT_PRO='tvResize_event',UVALUE=self)
        self->BuildMenu
     endif 
  endif 
     
  return,1
end

;=============================================================================
; tvResize__define - Prototype the tvResize class.
;=============================================================================
pro tvResize__define
  struct={tvResize, $
          INHERITS tvPlug_lite,$ ;make it a plug-in
          wMenu: 0L, $          ;widget ID for the size button
          sizes: ptr_new()}     ;which sizes we'll use
end
