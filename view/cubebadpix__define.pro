;;**************************OverRiding methods********************************
;=============================================================================
;  Message - We'll hear from CubeRec
;=============================================================================
pro CubeBadPix::Message, msg
  self->tvPlug::Message,msg,TYPE=type
  case type of
     'DRAW_MOTION': begin 
        pt=self.oDraw->Convert([msg.X,msg.Y],/SHOWING,/SINGLE_INDEX)
        pt=pt[0]
        if pt eq -1 then return
        if pt eq self.last_pt then return
        self.last_pt=pt
        if self.press eq 2b then begin ;middle click: this bcd only
           if ptr_valid(self.rec_set) then begin 
              if n_elements(*self.rec_set) gt 1 then return
              this_rec=(*self.rec_set)[0]
           endif else return
        endif 
        do_set=self.setting
        self.cube->ToggleBadPixel,pt,SET=do_set,RECORD_SET=this_rec
        if do_set ne self.setting then return
        self->DrawMark,pt,ERASE=~do_set,SINGLE_REC=n_elements(this_rec) gt 0
     end 
        
     'DRAW_BUTTON': begin 
        if msg.release eq 4b then return

        ;; Last button: cycle through group
        if msg.press eq 4b then begin 
           self.showing=(self.showing+1) mod 4
           self.oDraw->Redraw,/SNAPSHOT
           return
        endif
        
        ;; Other buttons, set/clear breakpoints
        if msg.type eq 1 then begin ;release
           self.oDraw->MsgSignup,self,DRAW_MOTION=0
           self.press=-1b
           self.last_pt=-1
           self.setting=-1
           return
        endif 
        
        pt=self.oDraw->Convert([msg.X,msg.Y],/SHOWING,/SINGLE_INDEX)
        pt=pt[0]
        if pt eq -1 then return
  
        
        if msg.press eq 2b then begin ;middle click: this bcd only
           if ptr_valid(self.rec_set) then begin 
              if n_elements(*self.rec_set) gt 1 then return
              this_rec=(*self.rec_set)[0]
           endif else return
        endif 
        
        self.press=msg.press
        self.last_pt=pt
        self.oDraw->MsgSignup,self,/DRAW_MOTION
        self.cube->ToggleBadPixel,pt,SET=set,RECORD_SET=this_rec
        self.setting=set
        self->DrawMark,pt,ERASE=~set,SINGLE_REC=n_elements(this_rec) gt 0
     end
     
     'TVDRAW_SNAPSHOT': begin 
        self.oDraw->GetProperty,ZOOM=zm
        self.zoom=zm
        if self.showing lt 3 then self->MarkStatic,/ALL
        ;; Only put bpl in background when ~On: in foreground otherwise
        if ~self->On() then self->MarkAll
     end
          
     'TVDRAW_REDRAW': self->MarkAll
     
     'CUBEREC_UPDATE': begin 
        if msg.BCD_MODE then begin ; We only work in BCD mode
           self.bmask=msg.BMASK
           self.cube=msg.CUBE
           self.cube->GetProperty,PMASK=pm,/POINTER
           self.rec_set=msg.record_set
           self.pmask=pm
           self->Enable
        endif else self->Reset,/DISABLE,/NO_REDRAW ;cube mode
        ;; Cuberec will redraw for us.
     end
  endcase
end

;=============================================================================
;  Reset - Take away the drawn glyphs
;=============================================================================
pro CubeBadPix::Reset,NO_REDRAW=nrd,_EXTRA=e
  self.oDraw->MsgSignup,self,/NONE
  if ~keyword_set(nrd) then self.oDraw->ReDraw,/ERASE,/SNAPSHOT
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
  if was_on then self.oDraw->Redraw,/SNAPSHOT ;to get *everything* into bg.
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
  return,['Mark/Remove','','ShowAll/Fatal/User']
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
  ;; We leave bit 8 of BMASK off, since it's just all the areas
  ;; outside the slit
  fatal_mask=28672U             ; bits 12 13 14
  ok_mask=36607U                ; all others (except bit 8, off-flat)
  
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
        fbind=where((*self.bmask)[ind] AND fatal_mask gt 0,fbcnt)
        if fbcnt gt 0 then fbind=ind[fbind]        
     endif else begin 
        bcnt=0 & fbcnt=0
     endelse 
     if ptr_valid(self.pmask) then begin 
        pind=where((*self.pmask)[ind] gt 0,pcnt)
        if pcnt gt 0 then pind=ind[pind]
     endif else pcnt=0
  endif else self->Error,'Must pass index, or display All'
  
  ;; Draw non-fatal bmask marks: red diamonds
  if self.showing eq 0 then begin 
     for i=0,bcnt-1 do begin 
        pt=self.oDraw->Convert(bind[i],/DEVICE,/SHOWING)
        if pt[0] eq -1 then continue
        plots,pt[0],pt[1],/DEVICE,COLOR=self.color[0],PSYM=4, $
              SYMSIZE=self.zoom/7,THICK=self.zoom ge 4?2.:1.
     endfor 
  endif 
  ;; Draw pmask marks: blue +'s
  if self.showing le 1 then begin 
     for i=0,pcnt-1 do begin    ;draw permanent marks
        pt=self.oDraw->Convert(pind[i],/DEVICE,/SHOWING)
        if pt[0] eq -1 then continue
        plots,pt[0],pt[1],/DEVICE,COLOR=self.color[1],PSYM=1, $
              SYMSIZE=self.zoom/7,THICK=self.zoom ge 4?2.:1.
     endfor 
  endif 
  ;; Draw fatal bmask marks: red X's
  if self.showing le 2 then begin 
     for i=0,fbcnt-1 do begin   ;draw fatal marks
        pt=self.oDraw->Convert(fbind[i],/DEVICE,/SHOWING)
        if pt[0] eq -1 then continue
        plots,pt[0],pt[1],/DEVICE,COLOR=self.color[0],PSYM=7, $
              SYMSIZE=self.zoom/7,THICK=self.zoom ge 4?2.:1.
     endfor
  endif 
;   if ptr_valid(self.non_finite) then begin 
;      nf=*self.non_finite
;      for i=0,n_elements(nf)-1 do begin ;draw non-finite marks
;         pt=self.oDraw->Convert(nf[i],/DEVICE,/SHOWING)
;         if pt[0] eq -1 then continue
;         plots,pt[0],pt[1],/DEVICE,COLOR=self.color[3],PSYM=6, $
;               SYMSIZE=self.zoom/7,THICK=self.zoom ge 4?2.:1.
;      endfor 
; endif 
end

;=============================================================================
;  MarkAll - Mark all bad pixels
;=============================================================================
pro CubeBadPix::MarkAll
  self.cube->GetProperty,GLOBAL_BAD_PIXEL_LIST=bpl,/POINTER
  if ptr_valid(bpl) then $
     for i=0,n_elements(*bpl)-1 do self->DrawMark,(*bpl)[i]
  if ptr_valid(self.rec_set) then begin 
     if n_elements(*self.rec_set) gt 1 then return
     self.cube->GetProperty,BAD_PIXEL_LIST=rbpl,RECORD_SET=(*self.rec_set)[0],$
                            /POINTER
     if n_elements(rbpl) gt 0 && ptr_valid(rbpl) then $
        for i=0,n_elements(*rbpl)-1 do self->DrawMark,(*rbpl)[i],/SINGLE_REC
  endif 
end

;=============================================================================
;  DrawMark - Mark or erase a single index
;=============================================================================
pro CubeBadPix::DrawMark,ind,ERASE=erase,SINGLE_REC=sr
  pt=self.oDraw->Convert(ind,/DEVICE,/SHOWING)
  if pt[0] eq -1 then return
  if keyword_set(erase) then begin 
     self.oDraw->Erase,pt-self.zoom/2,[self.zoom,self.zoom]+2
     ;; put the static mark back if necessary
     if self.showing lt 3 then self->MarkStatic,ind       
  endif else $
     plots,pt[0],pt[1],/DEVICE,COLOR=self.color[keyword_set(sr)?3:2],PSYM=7, $
           SYMSIZE=self.zoom/7,THICK=self.zoom ge 4?2.:1.
end

;=============================================================================
;  EnsureCube - Make sure the cube we have is still valid
;=============================================================================
pro CubeBadPix::EnsureCube
  if ~obj_valid(self.cube) then begin 
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
  self.color=col->GetColor(['red','blue','cyan','green'])
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
      color:intarr(4), $        ;the three colors
      zoom:1.0, $               ;the zoom factor
      last_pt:0L, $             ;last point
      press:0b, $               ;which was pressed
      setting:0b, $             ;are we setting or clearing multiple pixels?
      bmask:ptr_new(), $        ;the per-BCD mask plane
      pmask:ptr_new(), $        ;the static mask plane
      rec_set:ptr_new(), $      ;which records we're viewing (from cuberec)
;      non_finite:ptr_new(), $   ;where it's non-finite
      showing:0, $              ;showing 0)all 1) all but non-fatal
                                ;        2)only fatal 3) only user
      parent:0L}
end
