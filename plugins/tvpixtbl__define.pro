;+
; NAME:  
;
;    tvPixTbl
;
; DESCRIPTION:
;    
;    A tvTools plugin that displays a 5x5 table of the values of the
;    pixels around the mouse when it is over a tvdraw object.
;    
; CATEGORY:
;
;    tvTools, Data display.
;
; METHODS:
;
;    INIT:  
;
;       CALLING SEQUENCE:
;
;          obj=obj_new('tvPixTbl',oDraw,parent,[_EXTRA=]
;
;       INPUT PARAMETERS:
;
;	   oDraw: The tvDraw object.
;
;          parent:  The widget id of the object widget's parent.
;
;       KEYWORD INPUT PARAMETERS:
;
;          _EXTRA: Any extra keywords to pass up the class hierarchy.
;
; INHERITANCE TREE:
;
;    ObjMsg-->tvPlug-->tvPixTbl
;
; MODIFICATION HISTORY:
; 
;    2002-10-29 (J.D. Smith): Rewrote table display code, to highlight
;       the "central" pixel, and use dashes to indicate off the array.
;       Added a kill-notify so it won't receive events after
;       deactivated, etc.
;    
;    2002-01-30 (Nidhi): The table is now "all that it can be" given
;       IDL's limited implementation of the table widget. There is
;       that annoying space on the bottom and left sides, but there is
;       no known fix to that problem. Its either that or have scroll
;       bars there for effect, just to fill in the space. Right now,
;       I've left out the scrollers.
;
;    2002-01-26 (Nidhi): This is the "All Purpose" box for use with
;       general plugins that just need a selection tool. 
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2001 Nidhi Kalra, 2002 JD Smith
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
;=============================================================================
;       Message - Display the table. We have signed up for motion messages 
;=============================================================================
pro tvPixTbl::Message, msg
  self->tvPlug::Message, msg, TYPE=type ;pass it up to tvPlug
  case type of
     'DRAW_MOTION' : begin
        self.oDraw->GetProperty,IMORIG=imorig
        if NOT ptr_valid(imorig) OR $
           NOT widget_info(self.wBase, /VALID_ID) then return
        pt=self.oDraw->Convert([msg.X,msg.Y],/SHOWING)
        if pt[0] eq -1 then begin 
           widget_control, self.wtable,$;USE_TABLE_SELECT=[0,0,self.tblsize], $
                           set_value=make_array(self.tblsize,/string, $
                                                value='  --')
           self.savpoint=[-1,-1] ;ensure rentry works
           return
        endif 
        if array_equal(pt,self.savpoint) then return
        self.savpoint=pt
        self->UpdateTable,imorig,pt
     end
     'TVDRAW_POSTDRAW': begin 
        widget_control, self.wBase,TLB_SET_TITLE=(self.ODraw->Title()+ $
                                                  ' - PIXEL VALUES')
        self.oDraw->GetProperty,IMORIG=imorig
        self->UpdateTable,imorig,self.savpoint
     end
  endcase
end

pro tvPixTbl::Off
  self->tvPlug::Off
  self->wDestroy
  self.oDraw->MsgSignup,self,/NONE
end

pro tvPixTbl::On
  self->tvPlug::On
  title=self.ODraw->Title()+' - PIXEL VALUES'
  self.wBase=widget_base(/COLUMN, SPACE=1,GROUP_LEADER=self.parent, $
                         TITLE=title,KILL_NOTIFY='tvpixtbl_kill',UVALUE=self)
  self.wTable = widget_table(self.wBase,xsize=5, ysize=5,column_widths=100, $
                            /SCROLL)
  widget_control, self.wBase, /REALIZE
  
  ;; specify draw_motion events. always on
  self.oDraw->MsgSignup,self,/DRAW_MOTION,/TVDRAW_POSTDRAW
end

function tvPixTbl::Icon
  return, [                               $
           [000B, 000B],                   $
           [254B, 127B],                   $
           [002B, 064B],                   $
           [002B, 064B],                   $
           [218B, 093B],                   $
           [002B, 064B],                   $
           [218B, 093B],                   $
           [002B, 064B],                   $
           [218B, 093B],                   $
           [002B, 064B],                   $
           [218B, 093B],                   $
           [002B, 064B],                   $
           [218B, 093B],                   $
           [002B, 064B],                   $
           [254B, 127B],                   $
           [000B, 000B]                    $
          ]
end

function tvPixTbl::Description
  return,'Pixel Value Table'
end
;;************************End OverRiding methods*******************************


;=============================================================================
;       UpdateTable
;=============================================================================
pro tvPixTbl::UpdateTable, im,point
  pad = (self.tblsize-1)/2
  
  self.oDraw->GetProperty, SIZE=size
  mn = (point-pad) > 0
  mx = (point+pad) < (size-1)
  show=make_array(self.tblsize,/STRING,VALUE='  --')
  off=pad+(mn-point)
  show[off[0]:off[0]+mx[0]-mn[0],off[1]:off[1]+mx[1]-mn[1]]= $
     string((*im)[mn[0]:mx[0], mn[1]:mx[1]],FORMAT='(G12.6)')
  show=reverse(show,2)          ;why is it upside down?
  ;; create row and column labels
  widget_control, self.wTable, SET_VALUE=show, $
                  SET_TABLE_SELECT=[pad,pad], $
                  USE_TABLE_SELECT=[0,0,self.tblsize-1], $
                  COLUMN_LABELS=strtrim(indgen(self.tblsize[0])-pad[0]+ $
                                        point[0],2), $
                  ROW_LABELS=strtrim(indgen(self.tblsize[1])-pad[1]+point[1],2)
end

pro tvPixTbl_kill,id
  widget_control, id,get_uvalue=self
  if obj_valid(self) then self->Off
end

;=============================================================================
;	wDestroy - Destroy the Widget
;=============================================================================
pro tvPixTbl::wDestroy
  if NOT widget_info(self.wBase, /VALID_ID) then return
  widget_control, self.wBase,/DESTROY
end

;=============================================================================
;       Init - Initialize the PixTbl object.
;=============================================================================
function tvPixTbl::Init,oDraw,parent,_EXTRA=e
  if (self->tvPlug::Init(oDraw, _EXTRA=e) ne 1) then return,0 ; chain up
  self.parent = parent
  self.tblsize = [5,5]
  return,1
end

;=============================================================================
;       tvPixTbl__define - Prototype the tvPixTbl class.
;=============================================================================
pro tvPixTbl__define
  struct={tvPixTbl, $
          INHERITS tvPlug, $    ;make it a plug-in
          parent:0L, $          ;the parent of the widget set
          wBase:0L, $           ;a base to put the table in
          wTable:0L, $          ;the pixel table widget
          savpoint: [0,0], $    ;point to save
          tblsize: [0,0]}       ;columns x rows in table
  
  return
end
