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
        ;; First button only
        if msg.press ne 1b then return
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
     
     'TVDRAW_REDRAW': begin 
        self.oDraw->GetProperty,ZOOM=zm
        self.zoom=zm
        self->MarkStatic,/ALL
        self->MarkAll           ;put them back on
     end
     
     'CUBEREC_UPDATE': begin 
        if msg.BCD_MODE then begin ; We only work in BCD mode
           self.bmask=msg.BMASK
           if self.cube ne msg.CUBE then begin ;; a new cube
              self.cube=msg.CUBE
              self.cube->GetProperty,PMASK=pm,/POINTER
              self.cube->GetProperty,BAD_PIXEL_LIST=bpl
              ptr_free,self.bp_list
              if n_elements(bpl) gt 0 then self.bp_list=ptr_new(bpl,/NO_COPY)
              self.pmask=pm
              self.oDraw->ReDraw,/ERASE,/SNAPSHOT
           endif 
           self->Enable
        endif else self->Off,/DISABLE
     end
  endcase
end

;=============================================================================
;  Off - No more events needed
;=============================================================================
pro CubeBadPix::Off,_EXTRA=e
  self.oDraw->MsgSignup,self,/NONE
  if self->On() then self.oDraw->ReDraw,/ERASE,/SNAPSHOT
  self->tvPlug::Off,_EXTRA=e
end

;=============================================================================
;  On - Signup for all our messages.
;=============================================================================
pro CubeBadPix::On
  if self->On() then begin 
     self->Off
     return
  endif 
  self->EnsureCube
  self->tvPlug::On
  self.oDraw->MsgSignup,self,/DRAW_BUTTON,/TVDRAW_REDRAW
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
  if keyword_set(all) then begin 
     if ptr_valid(self.bmask) then begin 
        ;; don't show off-flat values
        bind=where((*self.bmask AND 65279) gt 0,bcnt)
     endif else bcnt=0
     if ptr_valid(self.pmask) then begin 
        pind=where(*self.pmask gt 0,pcnt)
     endif else pcnt=0
  endif else begin 
     if ptr_valid(self.bmask) then begin 
        ;; don't show off-flat values
        bind=where((*self.bmask)[ind] AND 65279 gt 0,bcnt)
        if bcnt gt 0 then bind=ind[bind]
     endif else bcnt=0
     if ptr_valid(self.pmask) then begin 
        pind=where((*self.pmask)[ind] gt 0,pcnt)
        if pcnt gt 0 then pind=ind[pind]
     endif else pcnt=0
  endelse 

  for i=0,bcnt-1 do begin 
     pt=self.oDraw->Convert(bind[i],/DEVICE,/SHOWING)
     if pt[0] eq -1 then continue
     plots,pt[0],pt[1],/DEVICE,COLOR=self.color[2],PSYM=4, $
           SYMSIZE=self.zoom/7,THICK=self.zoom ge 4?2.:1.
  endfor 
  for i=0,pcnt-1 do begin 
     pt=self.oDraw->Convert(pind[i],/DEVICE,/SHOWING)
     if pt[0] eq -1 then continue
     plots,pt[0],pt[1],/DEVICE,COLOR=self.color[1],PSYM=1, $
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
     self->MarkStatic,ind       ;put the static mark back if necessary
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
      parent:0L}

end
