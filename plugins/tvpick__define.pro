;=============================================================================
;       Message. Only REDRAW
;=============================================================================
pro tvPick::Message, msg
  ;; let tvPlug handle the exclusive messages, if any...
  self->tvPlug::Message,msg,TYPE=type
  if type eq 'TVDRAW_EXCLUSIVE' then return ;already handled
  case type of
     'TVDRAW_REDRAW': begin 
        self.size=msg.zoom/7
        self->MarkAll           ;put them back on
     end
     'DRAW_BUTTON': begin       ;a button event
        ;; we require a first or third button *press*
        if msg.press ne 1b and msg.press ne 4b then return
        ;; convert to showing pixels only
        coord=self.oDraw->Convert([msg.X,msg.Y],/SHOWING,/SINGLE_INDEX)
        if coord[0] eq -1 then return
        if msg.press eq 1b then begin ;add
           if ptr_valid(self.list) then begin 
              wh=where(*self.list ne coord,cnt)
              if cnt gt 0 then *self.list=[(*self.list)[wh],coord] else return
           endif else self.list=ptr_new([coord])
           self->MarkOne, coord
        endif else begin        ;remove
           if NOT ptr_valid(self.list) then return
           wh=where(*self.list ne coord,cnt)
           if cnt eq n_elements(*self.list) then return
           if cnt eq 0 then ptr_free,self.list else $
              *self.list=(*self.list)[wh]
           self->EraseOne,coord
        endelse 
        widget_control, self.wLine, set_value= $
                        string(FORMAT='("Pixels Selected: ",I)', $
                               ptr_valid(self.list)?n_elements(*self.list):0)
     end 
  endcase 
end

function tvPick::Icon
  return,[[192B, 001B],[032B, 003B],[032B, 003B],[032B, 003B],$
          [032B, 003B],[032B, 003B],[046B, 031B],[057B, 117B],$
          [050B, 213B],[036B, 208B],[036B, 192B],[008B, 192B],$
          [016B, 192B],[016B, 096B],[032B, 096B],[032B, 096B]]
end

pro tvPick::MarkAll
  if NOT ptr_valid(self.list) then return
  self.oDraw->GetProperty,DISPSIZE=ds,Zoom=zm
  for i=0,n_elements(*self.list)-1 do begin 
     coords=self.oDraw->Convert((*self.list)[i],/DEVICE,/SHOWING)
     ;; Only plot on showing pixels
     if coords[0] ne -1 then $
        plots,coords[0],coords[1],/DEVICE,COLOR=self.color,PSYM=7, $
        SYMSIZE=self.size,THICK=self.size gt 1.?2.:1.
  endfor 
  widget_control, self.wLine, $
                  set_value=string(FORMAT='("Pixels Selected: ",I)',n_elements(*self.list)) 
end

pro tvPick::EraseOne,ind
  self.oDraw->GetProperty,DRAWWIN=win,PIXWIN=pixwin,WINSIZE=ws
  wset,win
  coord=self.oDraw->Convert(ind,/DEVICE)
  unit=round(3*self.size)
  low=coord-unit>0
  dist=2*unit+1
  dist=[dist,dist]<(ws-low)>1
  device,copy=[low,dist,low,pixwin]
end

pro tvPick::MarkOne,ind
  coord=self.oDraw->Convert(ind,/DEVICE,/SHOWING)
  plots,coord[0],coord[1],/DEVICE,COLOR=self.color,PSYM=7, $
        SYMSIZE=self.size,THICK=self.size gt 1.?2.:1.
end


;=============================================================================
;       List.  Get the list
;=============================================================================
function tvPick::List
  if ptr_valid(self.list) then return,*self.list else return, -1
end

;=============================================================================
;       Cleanup.  Clean self up
;=============================================================================
pro tvPick::Cleanup
  ptr_free,self.list
  self->tvPlug::Cleanup
end

;=============================================================================
;       Start.  Automatically called when everything's together.
;=============================================================================
pro tvPick::Start
  self->MarkAll
end

;=============================================================================
;       init.  Initialize the line.
;=============================================================================
function tvPick::Init,parent,oDraw,LIST=list,COLOR=col,_EXTRA=e
  if (self->tvPlug::Init(oDraw,_EXTRA=e) ne 1) then return,0 ;chain up
  
  if n_elements(col) ne 0 then self.color=0>col<(!D.TABLE_SIZE-1) else  $
     self.color=!D.TABLE_SIZE/2
  
  if n_elements(list) ne 0 then begin 
     self.list=ptr_new(list)
     n=n_elements(list) 
  endif else n=0
  str=string(FORMAT='("Pixels Selected: ",I)',n)
  ;; set up the format for printing x,y, value
  
  self.wLine=widget_label(parent,value=str,/dynamic_resize,_EXTRA=e)
  
  ;; specify motion & tracking events ... not exclusive
  self.oDraw->MsgSignup,self,/TVDRAW_EXCLUSIVE,/BUTTON,/TVDRAW_REDRAW 
  return,1
end 


;=============================================================================
;       tvPick__define.  Prototype the tvPick class.
;=============================================================================
pro tvPick__define
  struct={tvPick, $ 
          INHERITS tvPlug,$     ;make it a plug-in
          list: ptr_new(), $    ;list of picked pixels
          size:1.0, $           ;size of the plotted point
          color:0, $            ;the color to mark pixels with
          wLine:0L}             ;widget id of text line
  return
end

