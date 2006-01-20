;+
; NAME:
;
;    tvExportImage
;
; DESCRIPTION:
;
;    A tvTools plugin for exporting the current image as PNG.
;
; CATEGORY:
;
;    tvTools, Image Export.
;
; METHODS:
;
;    Init:  
;
;       CALLING SEQUENCE:
;
;          obj=obj_new('tvExportImage',oDraw,menu,[_EXTRA=e])
;          
;       INPUT PARAMETERS:
;
;	   oDraw: The tvDraw object.
;
;          menu: The widget id of the menu in which to place the menu
;             entry.
;
;       INPUT KEYWORD PARAMETERS:
;
;          _EXTRA: Any other ObjMsg initialization keywords
;             (e.g. message list).
;                          
; INHERITANCE TREE:
;
;    ObjMsg-->tvPlug-->tvExportImage
;
; MODIFICATION HISTORY:
;
;    2004-09-27 (J.D. Smith): Written.
;       
;-
;    $Id$
;##############################################################################
;
; LICENSE
;
;  Copyright (C) 2004 J.D. Smith
;
;  This file is part of tvTools.
;
;  tvTools is free software; you can redistribute it and/or modify it
;  under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2, or (at your option)
;  any later version.
;
;  tvTools is distributed in the hope that it will be useful, but
;  WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;  General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with tvTools; see the file COPYING.  If not, write to the
;  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;  Boston, MA 02110-1301, USA.
;
;##############################################################################

;;**************************OverRiding methods********************************

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
