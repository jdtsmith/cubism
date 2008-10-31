pro tvPrint_Event, ev
  widget_control, ev.id,get_uvalue=self
  self->Print
end

pro tvPrint::Print
  if NOT obj_valid(self.psconf) then begin 
     self.psconf=Obj_New('FSC_PSConfig')
  endif 
  self.oDraw->GetProperty,DRAWWIDGET=dw,OFFSET=offset
  self.psconf->GUI,GROUP_LEADER=widget_info(dw,/PARENT),CANCEL=cncld
  if(cncld) then return
  thisDevice = !D.Name
  set_plot, "PS"
  keys=self.psconf->GetKeywords()
  device, _EXTRA=keys
  self.oDraw->GetProperty,IMSCL=im
  tv,*im[offset[0]:offset[0]+dispsize[0]-1,$
         offset[1]:offset[1]+dispsize[1]-1], $
     keys.xoffset,keys.yoffset, $
     XSIZE=keys.xsize,YSIZE=keys.ysize, $
     INCHES=keys.inches,CENTIMETERS=keys.inches eq 0
  device, /Close_File
  set_plot, thisDevice
end

pro tvPrint::Cleanup
  obj_destroy,self.psconf
  self->tvPlug::Cleanup
end

function tvPrint::Init,oDraw,menu,_EXTRA=e
  if self->tvPlug::Init(oDraw,_EXTRA=e) ne 1 then return,0 ;chain up
  but=widget_button(menu,value='Print...',uvalue=self, $
                    event_pro='tvPrint_event')
  return,1
end

pro tvPrint__define
  st={TVPRINT, $
      INHERITS tvPlug, $
      psconf:obj_new()}
end
