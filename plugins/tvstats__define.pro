
;;**************************OverRiding methods********************************
;=============================================================================
;	Message. Only exclusive and box messages.
;=============================================================================
pro tvStats::Message, msg
   ;; check for changes in active state -- Update override controls box
   self->tvPlug::Message,msg,TYPE=type
   ;; turn (off/on) our box if it was an exclusive change.
   if type eq 'BOX' then begin ; a message from our box
      self->Stats               ;do the stats
      return
   endif 
   if type eq 'TVDRAW_REDRAW' then begin 
      self.Box->Message,msg     ;first alert our box of the redraw.
      if (msg.type and 2b) ne 0 then begin ; if original image redrawn
         ;; if our box is drawn, redo stats
         if self.Box->IsDrawn() and self.recip.ACTIVE then self->Stats
      endif 
   endif 
end 

pro tvStats::Update, ac
   if n_elements(ac) eq 0 then begin ;if no active change, just do it
      self->tvPlug::Update
      return
   endif 
   ;; otherwise only toggle box and map if active status has actually changed.
   case ac of
      0b: begin                  ;just turned off
         self.box->Off
         if self.hide then self->wDestroy
      end
      1b: begin                  ;just turned on
         self.box->On
         if self.hide then self->wShow
         if self.box->IsDrawn() then self->Stats
      end 
      2b: begin                 
         self.box->Reset
         widget_control, self.wSlab, set_value=''
      end 
   endcase
   self->tvPlug::Update,ac      ;chain up, sending only for change.
end
   
function tvStats::Icon
   return,[[  0b,  0b],[254b,127b],[  6b, 64b],[242b,111b], $
           [ 54b, 72b],[ 98b, 96b],[198b, 64b],[130b, 97b], $
           [134b, 65b],[194b, 96b],[102b, 64b],[ 50b,104b], $
           [246b, 79b],[  2b, 96b],[254b,127b],[  0b,  0b]]
end
;;************************End OverRiding methods*******************************
;=============================================================================
;	Stats.  Compute the stats in the box.
;=============================================================================
pro tvStats::Stats
   self.Box->Getlrtb,l,r,t,b
   self.oDraw->GetProperty,imorig=io
   if NOT ((l le r) and (b le t)) then return
   take=(*io)[l:r,b:t]
   n=n_elements(take) 
   max=max(take,MIN=min)
   med=median(take)
   avg=total(take)/n
   std=sqrt(total((take-avg)^2)/(n-1))
   str=string(FORMAT=self.form,l,b,r-l+1,t-b+1,max,min,avg,med,std)
   widget_control, self.wSlab, set_value=str
end

;=============================================================================
;	wShow: Show the widgets.
;=============================================================================
pro tvStats::wShow
   if self.recip.ACTIVE ne 1b then return
   widget_control, self.parent,UPDATE=0
   self.wBase=widget_base(self.parent,/COLUMN,SPACE=1)
   ;; A cosmetic line...
   self.oDraw->GetProperty, WINSIZE=ws
   ln=widget_base(self.wBase,FRAME=2,ysize=0,xsize=ws[0]-5)
   title=widget_label(self.wBase,value='Box Statistics', $
                      FONT=get_fonts(/BOLD,SIZE=14))
   self.wSlab=widget_text(self.wBase,value=' ',xsize=54,ysize=2,_EXTRA=e)
   widget_control, self.parent,UPDATE=1
end

;=============================================================================
;	wDestroy: Destroy the Widget
;=============================================================================
pro tvStats::wDestroy
   widget_control, self.wBase,/DESTROY
end

;=============================================================================
;	Cleanup.  Clean self up
;=============================================================================
pro tvStats::Cleanup
   self->tvPlug::Cleanup
   return
end

;=============================================================================
;   init.  Initialize the Stats object.  All tvRBox keywords are relevant
;   (see tvrbox).  If HIDE is set, the widget is hidden when turned off.
;=============================================================================
function tvStats::Init,parent,oDraw,HIDE=hide,FORMAT=form,EXCLUSIVE=exc, $
                _EXTRA=e
   if (self->tvPlug::Init(oDraw,_EXTRA=e) ne 1) then return,0 ;chain up
   self.hide=keyword_set(hide)
   self.parent=parent
   if n_elements(form) eq 0 then $
    self.form='("[",I3,",",I3,"]","(",I3," x",I3,")"," MAX:",G11.5,' + $
    '" MIN:",G11.5,/,"AVG:",G11.5,"  MEDIAN:",G11.5, "  STDEV:",G10.5)'  $
   else self.form=form
   
   ;; Sign up with Draw object for exclusives only (we lead the tvrbox)
   self.recip.EXCLUSIVE=keyword_set(exc)
   self.recip.ACTIVE=1b-self.recip.EXCLUSIVE
   self.recip.REDRAW=1b
   
   self->wShow                  ;show them if necessary
   
   ;; Get a tvrbox object, signing *ourself* up for box messages.
   self.box=obj_new('tvrbox', oDraw, MsgList=[self],_EXTRA=e)
   
   self->Update               
   return,1
end

;=============================================================================
;	tvStats__define.  Prototype the tvStats class.
;=============================================================================
pro tvStats__define
   struct={tvStats, $ 
           INHERITS tvPlug, $   ;make it a plug-in
           box:obj_new(), $     ;a tvRBox to use.
           hide:0b, $           ;whether to hide the stats line when off
           parent:0L, $         ;the parent of the widget set
           wBase:0L, $          ;a base to put the test widget in
           form:'', $           ;the format to print in 
           wSlab:0L}            ;a text widget for the stats
   return
end
