pro tvPrint_Event, ev
  widget_control, ev.id,get_uvalue=self
  self->Print
end

pro tvPrint::Print
  if NOT obj_valid(self.psconf) then begin 
     self.psconf=Obj_New('FSC_PSConfig')
  endif 
  self.oDraw->GetProperty,DRAWWIDGET=dw,TOP=top,OFFSET=offset
  self.psconf->GUI,GROUP_LEADER=widget_info(dw,/PARENT),CANCEL=cncld
  if(cncld) then return
  thisDevice = !D.Name
  set_plot, "PS"
  device, _EXTRA=self.psconf->GetKeywords()
  self.oDraw->GetProperty,IMDISP=im
  tvfits, TOP=top, DSIZE=[keys.xsize,keys.ysize], $
          im[offset[0]:offset[0]+dispsize[0]-1,$
             offset[1]:offset[1]+dispsize[1]-1]
  Device, /Close_File
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
