;=============================================================================
;	Message.  Override the tvPlug Message to display vals.  We have
;	signed up for motion and tracking messages from the tvDraw object.
;=============================================================================
pro tvLine::Message,msg
   self->tvPlug::Message,msg,TYPE=type ;pass it up to tvPlug
   
   case type of 
      'WIDGET_DRAW': begin 
         self.oDraw->GetProperty,offset=offset,zoom=zoom,pan=pan, $
                                 imorig=imorig,dispsize=dispsize
         if NOT ptr_valid(imorig) then return
         X=FLOOR(Float(msg.X-pan[0])/ zoom)
         Y=FLOOR(Float(msg.Y-pan[1])/ zoom)
         if X lt 0 or X ge dispsize[0] or Y lt 0 or Y ge  $
          dispsize[1] then begin 
            widget_control, self.wLine,set_value=' '
            self.savpoint=[-1,-1] ;ensure rentry works
            return
         endif 
         X=X+offset[0] & Y=Y+offset[1]
         if X eq self.savpoint[0] and Y eq self.savpoint[1] then return
         str=string(FORMAT=self.form,X,Y,(*imorig)[X,Y])
         widget_control, self.wLine,set_value=str
         self.savpoint=[X,Y]
      end
      
      'WIDGET_TRACKING': begin 
         if msg.enter eq 0 then begin ;just left window -- clear status line
            widget_control,self.wLine,set_value=' '
            self.savpoint=[-1,-1]
         endif 
      end
      
      'TVDRAW_REDRAW': begin 
         if self.savpoint[0] eq -1 then return
         self.oDraw->GetProperty,IMORIG=imorig
         str=string(FORMAT=self.form,self.savpoint[0],self.savpoint[1], $
                    (*imorig)[self.savpoint[0],self.savpoint[1]])
         widget_control, self.wLine,set_value=str
      end
   endcase 
end 


;=============================================================================
;	Cleanup.  Clean self up
;=============================================================================
pro tvLine::Cleanup
   self->tvPlug::Cleanup
   return
end
;=============================================================================
;	init.  Initialize the line.
;=============================================================================
function tvLine::Init,parent,oDraw,FORMAT=form,_EXTRA=e
   if (self->tvPlug::Init(oDraw,_EXTRA=e) ne 1) then return,0 ;chain up
   ;; set up the format for printing x,y, value
   if n_elements(form) eq 0 then self.form='("(",I3,",",I3,") ",G14.8)' else $
    self.form=form
   self.wLine=widget_label(parent,value=' ',/dynamic_resize)
   ;; specify motion & tracking events ... not exclusive
   self.recip.MOTION=1b &  self.recip.TRACKING=1b
   self.recip.ACTIVE=1b         ;non-exclusive -- turn self on by default
   self.recip.REDRAW=1b
   self->Update                 ;sign up with the draw widget
   return,1
end 

;=============================================================================
;	tvLine__define.  Prototype the tvLine class.
;=============================================================================
pro tvLine__define
   struct={tvLine, $ 
           INHERITS tvPlug,$    ;make it a plug-in
           savpoint: [0,0], $   ;point to save
           form:'', $           ;format of printed text
           wLine:0L}            ;widget id of text line
   return
end











