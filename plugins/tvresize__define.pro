;+
; NAME:
;
;    tvResize
;
; DESCRIPTION:
;
;    A tvTools plugin to resize the draw window to fixed sizes.
;
; CATEGORY:
;
;    tvTools, Window Size
;
; METHODS:
;
;    Init:  
;
;       CALLING SEQUENCE:
;
;          obj=obj_new('tvResize',oDraw,[sizes,SIZE_MENU=,_EXTRA=e])
;          
;       INPUT PARAMETERS:
;
;	   oDraw: The tvDraw object.
;
;          sizes: Optional list of pixel sizes to set to, for square
;             windows.  Defaults to [256,384,512,768].
;	   
;       INPUT KEYWORD PARAMETERS:
;
;          SIZE_MENU: Widget ID of menu to root the "Set Size" menu group.
;
;          _EXTRA: Any other ObjMsg initialization keywords
;             (e.g. message list).
;          
; INHERITANCE TREE:
;
;    ObjMsg-->tvPlug-->tvResize
;
; MODIFICATION HISTORY:
;
;    2004-07-22 (J.D. Smith): Written
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


pro tvResize::Cleanup
  ptr_free,self.sizes
  self->tvPlug_lite::Cleanup
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
