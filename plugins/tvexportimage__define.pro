pro tvExportImage_Event, ev
  widget_control, ev.id,get_uvalue=self
  self->Export
end

pro tvExportImage::Export,file
  self.oDraw->GetProperty,DRAWWIDGET=dw,OFFSET=offset,IMDISP=im
  if ~ptr_valid(im) then self->Error,'No valid image'
  if n_elements(file) eq 0 then begin 
     title=self.oDraw->Title()
     p=stregex(title,': *',LENGTH=len)
     if p[0] ne -1 then title=strmid(title,p+len)
     parts=strsplit(title,' ,',/EXTRACT)
     len=strlen(parts)
     wh=where(total(len,/CUMULATIVE) lt 20,cnt)
     if cnt gt 0 then $
        name=idl_validname(strjoin(parts[wh],'_'),/CONVERT_ALL)+'.png' $
     else $
        name=idl_validname(parts[0],/CONVERT_ALL)+'.png'
     xf,file,/SAVE,FILTERLIST=['*.png','*.*','*'],TITLE='Save Image As...', $
        /NO_SHOW_ALL,SELECT=0,PARENT_GROUP=self.oDraw->TLBID(), $
        START=name,/MODAL
  endif 
  if size(file,/TYPE) ne 7 then return
  tvlct,r,g,b,/GET
  write_png,file,*im,r,g,b
end

function tvExportImage::Init,oDraw,menu,_EXTRA=e
  if self->tvPlug::Init(oDraw,_EXTRA=e) ne 1 then return,0 ;chain up
  but=widget_button(menu,value='Save as PNG...',uvalue=self, $
                    event_pro='tvExportImage_event')
  return,1
end

pro tvExportImage__define
  st={tvExportImage, $
      INHERITS tvPlug}
end
