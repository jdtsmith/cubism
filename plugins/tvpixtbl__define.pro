;=============================================================================
;       RANDOM JUNK 
;=============================================================================


function tvPixtbl::Icon2
  return,[[  0b,  0b],[224b,  7b],[248b, 31b],[252b, 63b], $
          [ 60b, 60b],[ 30b,120b],[142b,112b],[142b,115b], $
          [206b,113b],[ 14b,113b],[ 30b,120b],[ 60b, 60b], $
          [252b, 63b],[248b, 31b],[224b,  7b],[  0b,  0b]]
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
                         GROUP_LEADER=self.parent)

  self.wTable = widget_table(self.wBase, value=*self.tblptr,/SCROLL, $
                            scr_xsize=425, scr_ysize=150)

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
;	Event - Turn the widget on and off
;=============================================================================
pro tvpixtbl_event, ev
  widget_control, ev.handler, get_uvalue=self

  if TAG_NAMES(ev, /STRUCTURE_NAME) EQ $
     'WIDGET_KILL_REQUEST' then begin
     self->KillEvent, ev
     print, "auuugh!"
     return
  endif


  self->Event,ev
end

pro tvPixtbl::KillEvent, ev
  
  widget_control, self.wBtn, set_value="PT  On"
  self->wDestroy

end 

pro tvPixtbl::Event,ev
  widget_control, ev.handler, get_value=tmp

  if tmp eq 'PT  On' then begin
     widget_control, self.wBtn, set_value="PT Off"

     tmptbl = intarr(self.tblsize(0), self.tblsize(1))
     self.tblptr = ptr_new(tmptbl) ;a new empty table
     self.wBase=widget_base(/COLUMN, SPACE=1, /FLOATING, $
                            GROUP_LEADER=self.parent ,$
                            event_pro="tvpixtbl_event");, $
                           ; /TLB_KILL_REQUEST_EVENTS)
     self.wTable = widget_table(self.wBase, value=*self.tblptr,/SCROLL, $
                               scr_xsize=425, scr_ysize=150)

     widget_control, self.wBase, /REALIZE
  
     ;specify draw_motion events. always on
     self.oDraw->MsgSignup,self,/DRAW_MOTION
     
  endif else if tmp eq 'PT Off' then begin
     widget_control, self.wBtn, set_value="PT  On"
     self->wDestroy
     self.oDraw->MsgSignup,self,/NONE

  ;endif else if TAG_NAMES(ev, /STRUCTURE_NAME) EQ $
  ;   'WIDGET_KILL_REQUEST' then begin
  ;   widget_control, self.wBtn, set_value="PT  On"
  ;   self->wDestroy
  endif else begin
     print, "ERROR: Unknown value returned from tvpixtbl_wBtn"
  endelse


  
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

function tvPixtbl::Init,parent,oDraw,nbase=nbase,_EXTRA=e
  if (self->tvPlug::Init(oDraw, _EXTRA=e) ne 1) then return,0 ; chain up
  self.parent = parent

  ;set up table size
  self.tblsize = [5,5]
  
  ;if no icon, embed in parent
  ;if n_elements(nbase) eq 0 then begin 

     ;set up the table widget
     ;tmptbl = intarr(self.tblsize(0), self.tblsize(1))
     ;self.tblptr = ptr_new(tmptbl) ;a new empty table
     ;self.wBase=widget_base(/COLUMN, SPACE=1, /FLOATING, $
     ;                       GROUP_LEADER=self.parent)
     ;self.wTable = widget_table(self.wBase, value=*self.tblptr, /SCROLL)

     ;widget_control, self.wBase, /REALIZE
  
     ;specify draw_motion events. always on
     ;self.oDraw->MsgSignup,self,/DRAW_MOTION

  ;endif else begin
     ;Create a toggle button
     ;self.nbase = nbase
     ;self.wBtn = widget_button(self.nbase, value="PT  On", $
     ;                     event_pro='tvpixtbl_event', uvalue=self)
  ;endelse

  return,1
end

;=============================================================================
;       tvPixtbl__define - Prototype the tvPixtbl class.
;=============================================================================
pro tvPixtbl__define
  struct={tvPixtbl, $
          INHERITS tvPlug, $    ;make it a plug-in
          parent:0L, $          ;the parent of the widget set
          nbase:0L, $           ;the non-exclusive toolbar
          wBase:0L, $           ;a base to put the table in
          wTable:0L, $          ;the pixel table widget
          wBtn:0L, $            ;the button widget
          savpoint: [0,0], $    ;point to save
          tblptr: ptr_new(), $  ;pointer to the data table
          tblsize: [0,0]}       ;columns x rows in table
          
  return
end
