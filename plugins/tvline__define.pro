;=============================================================================
;       Message - Display the vals.  We have signed up for motion and
;                 tracking messages
;=============================================================================
pro tvLine::Message,msg
  self->tvPlug_lite::Message,msg,TYPE=type ;pass it up
  case type of 
     'DRAW_MOTION': begin 
        self.oDraw->GetProperty,IMORIG=imorig
        if NOT ptr_valid(imorig) then return
        pt=self.oDraw->Convert([msg.X,msg.Y],/SHOWING)
        if pt[0] eq -1 then begin 
           widget_control, self.wLine,set_value=' '
           self.savpoint=[-1,-1] ;ensure rentry works
           return
        endif 
        if array_equal(pt,self.savpoint) then return
        widget_control, self.wLine,set_value=self->String(imorig,pt)
        self.savpoint=pt
     end
     
     'WIDGET_TRACKING': begin 
        if msg.enter eq 0 then begin ;just left window -- clear status line
           widget_control,self.wLine,set_value=' '
           self.savpoint=[-1,-1]
        endif 
     end
     
     'TVDRAW_POSTDRAW': begin 
        if self.savpoint[0] eq -1 then return ;not on a point
        self.oDraw->GetProperty,IMORIG=imorig
        widget_control, self.wLine,set_value=self->String(imorig,self.savpoint)
     end
  endcase 
end 

;=============================================================================
;       String - The string value associate with point X,Y
;=============================================================================
function tvLine::String, im,point
  return,string(FORMAT=self.form,point,(*im)[point[0],point[1]])
end

;=============================================================================
;       Init - Initialize the line.
;=============================================================================
function tvLine::Init,parent,oDraw,FORMAT=form,_EXTRA=e
  if (self->tvPlug_lite::Init(oDraw,_EXTRA=e) ne 1) then return,0 ;chain up
  ;; set up the format for printing x,y, value
  if n_elements(form) eq 0 then self.form='("(",I3,",",I3,") ",G14.8)' else $
     self.form=form
  self.wLine=widget_label(parent,value=' ',/dynamic_resize)
  
  ;; specify motion, tracking and postdraw events... we're always on
  self.oDraw->MsgSignup,self,/DRAW_MOTION,/WIDGET_TRACKING,/TVDRAW_POSTDRAW
  return,1
end 

;=============================================================================
;       tvLine__define - Prototype the tvLine class.
;=============================================================================
pro tvLine__define
  struct={tvLine, $ 
          INHERITS tvPlug_lite,$ ;make it a plug-in
          savpoint: [0,0], $    ;point to save
          form:'', $            ;format of printed text
          wLine:0L}             ;widget id of text line
  return
end
