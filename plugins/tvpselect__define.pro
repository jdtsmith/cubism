;+
; NAME:
;
;    tvPSelect
;
; DESCRIPTION:
;
;    A tvTools plugin to select planes from an image cube.
;
; CATEGORY:
;
;    tvTools, Image Cube Plane Selection
;
; METHODS:
;
;    Init:  
;
;       CALLING SEQUENCE:
;
;          obj=obj_new('tvPSelect',parent,oDraw,image,[/COLUMN,/NOLABEL=,
;                      START=,PLANES=,SWITCH_KEYS=,_EXTRA=e])
;          
;       INPUT PARAMETERS:
;
;	   oDraw: The tvDraw object.
;
;          parent: The widget ID where the line reporting label will
;            be placed.
;
;          image: The 3D image cube from which to select planes.
;	   
;       INPUT KEYWORD PARAMETERS:
;
;          COLUMN: If passed, orient selection popup as column.
;
;          NOLABEL: If passed, don't create a "Planes:" widget label.
;
;          START: The starting plane to select.
;
;          PLANES: The list of planes to permit selection: defaults to
;             all.
;
;          SWITCH_KEYS: A string array with two elements: the keys to
;            use to switch planes (defaults to [ ',' , '.' ]).
;
;          _EXTRA: Any other ObjMsg initialization keywords
;             (e.g. message list).
;          
; INHERITANCE TREE:
;
;    ObjMsg-->tvPlug-->tvPSelect
;
; MODIFICATION HISTORY:
;
;    2001-10-10 (J.D. Smith): Imported from SCORE-era source.
;       
;-
;    $Id$
;##############################################################################
;
; LICENSE
;
;  Copyright (C) 2001,2004 J.D. Smith
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

;=============================================================================
;  Message
;=============================================================================
;; We're only getting key messages
pro tvPSelect::Message,msg
  if msg.release then return    ; Press only
  if msg.type eq 6 then return  ; non-ASCII keys not allowed
  if msg.modifiers ne 0b then return 
  up=strlowcase(msg.ch) eq self.switch_key[0] 
  down=strlowcase(msg.ch) eq self.switch_key[1]
  if NOT up and NOT down then return
  pl=widget_info(self.wList,/DROPLIST_SELECT) 
  np=widget_info(self.wList,/DROPLIST_NUMBER) 
  if down then if pl eq 0 then pl=np-1 else pl=pl-1 ;decrement beam
  if up then if pl eq np-1 then pl=0 else pl=pl+1 ;increment beam
  widget_control, self.wList,SET_DROPLIST_SELECT=pl
  ;; fake a droplist event
  self->Event,{index:pl}
end


;=============================================================================
;  GetProperty
;=============================================================================
pro tvPSelect::GetProperty, PLANES=planes, IMAGE=image
  if arg_present(planes) then planes=self.Planes
  if arg_present(image) then image =self.Image
end 


;=============================================================================
;  SetProperty
;=============================================================================
pro tvPSelect::SetProperty, PLANES=pl
  if keyword_set(pl) then begin
     ptr_free,self.Planes
     self.Planes=ptr_new(pl)
     widget_control, self.wList, SET_VALUE=strtrim(pl,2)
  endif 
end

;=============================================================================
; Event
;=============================================================================
pro tvPSelect_Event,ev
  widget_control, ev.handler, get_uvalue=self
  self->Event,ev
end
pro tvPSelect::Event, ev
  ;; Set the next image, without changing zoom etc (same sized image)
  self.oDraw->SetProperty, IMORIG=(*self.Image)[*,*,ev.index],/NO_RESIZE
end


;=============================================================================
; Cleanup
;=============================================================================
pro tvPSelect::Cleanup
  ptr_free,self.Image,self.Planes
  self->tvPlug::Cleanup
end

;=============================================================================
;  Init
;=============================================================================
function tvPSelect::Init, oDraw, parent, im,COLUMN=col,NOLABEL=nl, $
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
     if keyword_set(col) then base=widget_base(parent,/COL) else $
        base=widget_base(parent,/ROW)
     label=widget_label(base,value='Plane:')
  endif else base=parent
  if n_elements(strt) eq 0 then strt=0
  self.wList=widget_droplist(base,VALUE=strtrim(*self.Planes,2), UVALUE=self,$
                             EVENT_PRO='tvpselect_event')
  if strt ne 0 then widget_control, self.wList, set_droplist_select=strt
  self.oDraw->MsgSignup,self,/DRAW_KEY
  return,1
end

;=============================================================================
; tvPSelect__define
;=============================================================================
pro tvPSelect__define
  struct={tvPSelect, $
          Inherits tvPlug, $
          Image:ptr_new(), $    ;the image *cube*
          switch_key:['',''], $ ;keys to use for cycling planes
          Planes: ptr_new(), $  ;list of possible planes
          Cur: 0L, $            ;which plane is the current plane
          wList:0L}             ;the widget droplist for selecting
end
