;;**************************OverRiding methods********************************
;=============================================================================
;  Message - We'll hear from CubeRec
;=============================================================================
pro CubeBadPix::Message, msg
  self->tvPlug::Message,msg,TYPE=type
  case type of
     'DRAW_BUTTON': begin 
        if msg.type ne 0 then return ;presses only
        pt=self.oDraw->Convert([msg.X,msg.Y],/SHOWING,/SINGLE_INDEX)
        if pt[0] eq -1 then return
        ;; Non-first button only: cycle through group
        if msg.press eq 4b then begin $
           self.showing=1-self.showing 
           self.oDraw->Redraw,/SNAPSHOT
           return
        endif else if msg.press ne 1b then return
        ;; Add or remove bp from list
        if ptr_valid(self.bp_list) then begin 
           got=where(*self.bp_list eq pt,ngot, $
                     COMPLEMENT=others,NCOMPLEMENT=nothers)
           if ngot gt 0 then begin ;already on list
              self->DrawMark,pt,/ERASE
              if nothers gt 0 then *self.bp_list=(*self.bp_list)[others] $
              else ptr_free,self.bp_list
           endif  else begin 
              self->DrawMark,pt
              *self.bp_list=[*self.bp_list,pt]
           endelse 
        endif else begin 
           self->DrawMark,pt
           self.bp_list=ptr_new([pt])
        endelse 
        self.cube->SetProperty,BAD_PIXEL_LIST=ptr_valid(self.bp_list)? $
                               *self.bp_list:-1
     end
     
     'TVDRAW_SNAPSHOT': begin 
        self.oDraw->GetProperty,ZOOM=zm
        self.zoom=zm
        if self.showing eq 0 then self->MarkStatic,/ALL
        ;; Only put bpl in background when On
        if ~self->On() then self->MarkAll
     end
          
     'TVDRAW_REDRAW': self->MarkAll
     
     'CUBEREC_UPDATE': begin 
        if msg.BCD_MODE then begin ; We only work in BCD mode
           self.bmask=msg.BMASK
           self.cube=msg.CUBE
           self.cube->GetProperty,PMASK=pm,/POINTER
           self.pmask=pm
           self.cube->GetProperty,BAD_PIXEL_LIST=bpl
           ptr_free,self.bp_list
           if n_elements(bpl) gt 0 then self.bp_list=ptr_new(bpl,/NO_COPY)
           if self->On() then self.oDraw->ReDraw,/ERASE,/SNAPSHOT
           self->Enable
        endif else self->Reset,/DISABLE ;cube mode
     end
  endcase
end

;=============================================================================
;  Reset - Take away the drawn glyphs
;=============================================================================
pro CubeBadPix::Reset,_EXTRA=e
  self.oDraw->MsgSignup,self,/NONE
  self.oDraw->ReDraw,/ERASE,/SNAPSHOT
  self->tvPlug::Off,_EXTRA=e
end


;=============================================================================
;  Off - No more events needed
;=============================================================================
pro CubeBadPix::Off,_EXTRA=e
  ;; Nothing but snapshot
  if self->On() then begin 
     was_on=1
     self.oDraw->MsgSignup,self,/NONE,/TVDRAW_SNAPSHOT
  endif else was_on=0
  self->tvPlug::Off,_EXTRA=e
  if was_on then self.oDraw->Redraw,/SNAPSHOT ;to get everything into bg.
end

;=============================================================================
;  On - Signup for all our messages.
;=============================================================================
pro CubeBadPix::On
  self->EnsureCube
  if self->On() then begin 
     self->Reset
     return
  endif 
  self->tvPlug::On
  self.oDraw->MsgSignup,self,/DRAW_BUTTON,/TVDRAW_SNAPSHOT,/TVDRAW_REDRAW
  self.oDraw->Redraw,/SNAPSHOT
end

;=============================================================================
;  Icon
;=============================================================================
function CubeBadPix::Icon
  return,[[008B, 016B],[008B, 016B],[008B, 016B],[255B, 255B], $
          [040B, 020B],[120B, 030B],[104B, 022B],[136B, 017B],$
          [136B, 017B],[104B, 022B],[120B, 030B],[040B, 020B],$
          [255B, 255B],[008B, 016B],[008B, 016B],[008B, 016B]]
end

;=============================================================================
;  Description
;=============================================================================
function CubeBadPix::Description
  return,'Mark Bad BCD Pixels'
end


function CubeBadPix::MouseHelp
  return,['Mark/Remove','','Show/Hide Static']
end


;=============================================================================
;  ReportWidget - Where to position our error and other messages
;=============================================================================
function CubeBadPix::ReportWidget
  return,self.parent
end
;;*************************End OverRiding methods******************************

;=============================================================================
;  MarkStatic - Mark static (bmask or pmask) pixels
;=============================================================================
pro CubeBadPix::MarkStatic,ind,ALL=all
  ok_mask=35583U
  fatal_mask=29696U
  
  if keyword_set(all) then begin 
     if ptr_valid(self.bmask) then begin 
        ;; non-fatal and fatal bmasks
        bind=where((*self.bmask AND ok_mask) gt 0,bcnt)
        fbind=where((*self.bmask AND fatal_mask) gt 0,fbcnt)
     endif else begin 
        bcnt=0 & fbcnt=0
     endelse 
     if ptr_valid(self.pmask) then begin 
        pind=where(*self.pmask gt 0,pcnt)
     endif else pcnt=0
  endif else if n_elements(ind) gt 0 then begin 
     if ptr_valid(self.bmask) then begin 
        ;; don't show off-flat values
        bind=where((*self.bmask)[ind] AND ok_mask gt 0,bcnt)
        if bcnt gt 0 then bind=ind[bind]
        fbind=where((*self.bmask AND fatal_mask) gt 0,fbcnt)
        if fbcnt gt 0 then fbind=ind[fbind]        
     endif else begin 
        bcnt=0 & fbcnt=0
     endelse 
     if ptr_valid(self.pmask) then begin 
        pind=where((*self.pmask)[ind] gt 0,pcnt)
        if pcnt gt 0 then pind=ind[pind]
     endif else pcnt=0
  endif else self->Error,'Must pass index, or display All'

  for i=0,bcnt-1 do begin 
     pt=self.oDraw->Convert(bind[i],/DEVICE,/SHOWING)
     if pt[0] eq -1 then continue
     plots,pt[0],pt[1],/DEVICE,COLOR=self.color[1],PSYM=4, $
           SYMSIZE=self.zoom/7,THICK=self.zoom ge 4?2.:1.
  endfor 
  for i=0,fbcnt-1 do begin
     pt=self.oDraw->Convert(fbind[i],/DEVICE,/SHOWING)
     if pt[0] eq -1 then continue
     plots,pt[0],pt[1],/DEVICE,COLOR=self.color[1],PSYM=7, $
           SYMSIZE=self.zoom/7,THICK=self.zoom ge 4?2.:1.
  endfor
  for i=0,pcnt-1 do begin 
     pt=self.oDraw->Convert(pind[i],/DEVICE,/SHOWING)
     if pt[0] eq -1 then continue
     plots,pt[0],pt[1],/DEVICE,COLOR=self.color[2],PSYM=1, $
           SYMSIZE=self.zoom/7,THICK=self.zoom ge 4?2.:1.
  endfor 
end

;=============================================================================
;  MarkAll - Mark all bad pixels
;=============================================================================
pro CubeBadPix::MarkAll
  if ~ptr_valid(self.bp_list) then return
  for i=0,n_elements(*self.bp_list)-1 do self->DrawMark,(*self.bp_list)[i]
end

;=============================================================================
;  DrawMark - Mark or erase a single index
;=============================================================================
pro CubeBadPix::DrawMark,ind,ERASE=erase
 pt=self.oDraw->Convert(ind,/DEVICE,/SHOWING)
  if pt[0] eq -1 then return
  if keyword_set(erase) then begin 
     self.oDraw->Erase,pt-self.zoom/2,[self.zoom,self.zoom]+2
     ;; put the static mark back if necessary
     if self.showing eq 0 then self->MarkStatic,ind       
  endif else $
     plots,pt[0],pt[1],/DEVICE,COLOR=self.color[0],PSYM=7, $
           SYMSIZE=self.zoom/7,THICK=self.zoom ge 4?2.:1.
end

;=============================================================================
;  EnsureCube - Make sure the cube we have is still valid
;=============================================================================
pro CubeBadPix::EnsureCube
  if NOT obj_valid(self.cube) then begin 
     self->Off
     self->Error,'Cube no longer valid.'
  endif 
end

;=============================================================================
;  Start 
;=============================================================================
pro CubeBadPix::Start
  self->tvPlug::Start
  col=self.oDraw->GetMsgObjs(CLASS='tvColor')
  col=col[0]
  if ~obj_valid(col) then begin 
     self.color=!D.TABLE_SIZE-1
     return
  endif 
  
  self.color=col->GetColor(['cyan','red','blue'])
  
end


;=============================================================================
;  Init -  Initialize the CubeBadPix object
;=============================================================================
function CubeBadPix::Init,parent,oDraw,_EXTRA=e
  if (self->tvPlug::Init(oDraw,_EXTRA=e) ne 1) then return,0 
  self.parent=parent
  return,1
end

;=============================================================================
;  CubeBadPix__define - Prototype the CubeBadPix class
;=============================================================================
pro CubeBadPix__define
  st={CubeBadPix, $
      INHERITS tvPlug,$         ;it's a full tvDraw plugin
      cube:obj_new(), $         ;the cube project we're servicing
      color:intarr(3), $       ;the three colors
      zoom:1.0, $               ;the zoom factor
      bp_list:ptr_new(), $      ;the bad pixel list
      bmask:ptr_new(), $        ;the per-BCD mask plane
      pmask:ptr_new(), $        ;the static mask plane
      showing:0, $              ;showing 0) all 1) user BPs
      parent:0L}

end
