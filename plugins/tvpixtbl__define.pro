;+
; NAME:  
;
;    tvPixtbl
;
; CONTACT:
;
;    UPDATED VERSIONs of SMART and more information can be found at:
;       http://isc.astro.cornell.edu/smart/download
;
; DESCRIPTION:
;    
;    A pixel table plugin that displays a 5x5 matrix of the values of
;    the pixels around the mouse when it is over a tvdraw object.
;    
; CATEGORY:
;
;    Data display.
;    	
;
; METHODS:
;

;    INIT:  (alway start with the INIT method function)
;
;       CALLING SEQUENCE:
;
;          obj=obj_new('tvPixtbl',parent,odraw, [Extra=]
;             [opt_arg1,...,/BIN_KEYWORD,KEYWORD=])
;
;       INPUT PARAMETERS:
;
;          parent:  The widget id of the object widget's parent.
;          
;	   oDraw: The tvDraw object.
;
;       KEYWORD INPUT PARAMETERS:
;
;          Extra: Any extra stuff you want to pass in.
;       ...
;
;
; INHERITANCE TREE:
;
;    ObjMsg-->tvPlug-->tvPixtbl
;
; EXAMPLE:
;
;    pixobj = obj_new('tvPixtbl', parent, odraw)
;
; MODIFICATION HISTORY:
;
;    $Log$
;    Revision 1.3  2002/01/30 03:34:21  nidhi
;    The table is now "all that it can be" given IDL's limited implementation of the table widget. There is that annoying space on the bottom and left sides, but there is no known fix to that problem. Its either that or have scroll bars there for effect, just to fill in the space. Right now, I've left out the scrollers.
;
;    Revision 1.2  2002/01/26 21:57:14  nidhi
;    This is the "All Purpose" box for use with general plugins that just need a selection tool. Also, I dont know how to word wrap this. sorry.
;
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2001 Cornell University
;
;  This file is part of SMART.
;
;  SMART is free software; you can redistribute it and/or modify it
;  under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2, or (at your option)
;  any later version.
;  
;  SMART is distributed in the hope that it will be useful, but
;  WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;  General Public License for more details.
;  
;  You should have received a copy of the GNU General Public License
;  along with SMART; see the file COPYING.  If not, write to the Free
;  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;  02111-1307, USA.
;
;##############################################################################


;=============================================================================
;       RANDOM JUNK 
;=============================================================================


function tvPixtbl::Icon
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


pro tvPixtbl::Off

  self->tvPlug::Off
  self->wDestroy
  self.oDraw->MsgSignup,self,/NONE

end


pro tvPixtbl::On

  self->tvPlug::On

  tmptbl = intarr(self.tblsize(0), self.tblsize(1))
  self.tblptr = ptr_new(tmptbl) ;a new empty table
  self.wBase=widget_base(/COLUMN, SPACE=1, /FLOATING, $
                        GROUP_LEADER=self.parent);, $
                        ;uvalue=self, kill_notify='tvPixtbl_killed')

  self.wTable = widget_table(self.wBase, value=*self.tblptr, $
                            xsize=5, ysize=5)

  widget_control, self.wBase, /REALIZE
  
  ;specify draw_motion events. always on
  self.oDraw->MsgSignup,self,/DRAW_MOTION

end


;=============================================================================
;       Message - Display the table. We have signed up for mtion messages 
;=============================================================================
pro tvPixtbl::Message, msg
  self->tvPlug::Message, msg, TYPE=type ;pass it up to tvPlug
  case type of
     'DRAW_MOTION' : begin
        self.oDraw->GetProperty,IMORIG=imorig
        if NOT ptr_valid(imorig) OR $
           NOT widget_info(self.wBase, /VALID_ID) then return
        pt=self.oDraw->Convert([msg.X,msg.Y],/SHOWING)
        if pt[0] eq -1 then begin 
           widget_control, self.wtable,set_value=[0];
           self.savpoint=[-1,-1] ;ensure rentry works
           return
        endif 
        if total(pt eq self.savpoint) eq 2 then return
        self.savpoint=pt
        temp = self->makeTable(imorig,pt)
        widget_control, self.wTable, set_value=*temp[2], $
                        column_labels=*temp[1] , row_labels=*temp[0], $
                        /No_Copy
     end
  endcase
end

;=============================================================================
;       MakeTable - The table associated with point (x,y)
;=============================================================================
function tvPixtbl::MakeTable, im,point
  
  numcols = self.tblsize[0]
  numrows = self.tblsize[1]
  colpad = (numcols-1)/2
  rowpad = (numrows-1)/2
  
  self.oDraw->GetProperty, SIZE=size
  colmin = (point[0]-colpad) > 0
  colmax = (point[0]+colpad) < (size[0]-1)
  rowmin = (point[1]-rowpad) > 0
  rowmax = (point[1]+rowpad) < (size[0]-1)

  ;if there we are on the edge of the image
  if ((colmax-colmin) LT (numcols-1)) OR ((rowmax-rowmin) LT (numrows-1)) $
     then begin
     colarr = fltarr(colpad, size[0]) - 1
     rowarr = fltarr(size[1]+2*colpad, rowpad) -1
     temparr = [colarr, (*im), colarr]
     temparr = [[rowarr],[temparr],[rowarr]]
     newpoint = [point[0]+colpad, point[1]+rowpad]
     temp = temparr[(newpoint[0]-colpad):(newpoint[0]+colpad),$
                  (newpoint[1]-rowpad):(newpoint[1]+rowpad)]
  endif else begin
     temp = (*im)[colmin:colmax, rowmin:rowmax]
  endelse

  ;create row and column labels
  tblarr = ptrarr(3)
  collbl = indgen(1, numcols) + (point[0] - colpad)
  collbl = collbl < colmax
  collbl = collbl > 0
  tblarr[1] = ptr_new(strcompress(string(collbl)))
  
  rowlbl = indgen(1, numrows) + (point[1] - rowpad)
  rowlbl = rowlbl < rowmax
  rowlbl = rowlbl > 0
  tblarr[0] = ptr_new(reverse(strcompress(string(rowlbl)),2))
  tblarr[2] = ptr_new(reverse(temp,2))
  

  return, tblarr

end

;=============================================================================
;	wDestroy - Destroy the Widget
;=============================================================================
pro tvPixtbl::wDestroy
  if NOT widget_info(self.wBase, /VALID_ID) then return
  ;widget_control, self.wBtn, set_value="PT  On"
  widget_control, self.wBase,/DESTROY
  self.oDraw->MsgSignup,self,/NONE
end

;=============================================================================
;	Cleanup
;=============================================================================
pro tvPixtbl::Cleanup
  ptr_free,self.tblptr
  self->tvPlug::Cleanup
end



;=============================================================================
;       Init - Initialize the Pixtbl object.
;=============================================================================

function tvPixtbl::Init,parent,oDraw,_EXTRA=e
  if (self->tvPlug::Init(oDraw, _EXTRA=e) ne 1) then return,0 ; chain up
  self.parent = parent

  ;set up table size
  self.tblsize = [5,5]

  return,1
end

;=============================================================================
;       tvPixtbl__define - Prototype the tvPixtbl class.
;=============================================================================
pro tvPixtbl__define
  struct={tvPixtbl, $
          INHERITS tvPlug, $    ;make it a plug-in
          parent:0L, $          ;the parent of the widget set
          wBase:0L, $           ;a base to put the table in
          wTable:0L, $          ;the pixel table widget
          savpoint: [0,0], $    ;point to save
          tblptr: ptr_new(), $  ;pointer to the data table
          tblsize: [0,0]}       ;columns x rows in table
          
  return
end
