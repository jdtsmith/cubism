;;**************************OverRiding methods********************************
;=============================================================================
;  Message - Let's hear about new Stacks, Switch to Full, Records, or cubes 
;=============================================================================
pro CubeRec::Message, msg
  self->tvPlug::Message,msg,TYPE=type
  case type of
     'BOX': begin 
        self->Extract
        return
     end 
     'TVDRAW_POSTDRAW': begin 
;        self->Extract
        return
     end
     'CUBEVIEWSPEC_FULL': begin 
        self.cur_wav=value_locate(*self.wavelength,msg.wavelength)>0
        self->SwitchMode,/FULL
     end
     'CUBEVIEWSPEC_STACK': begin
        self->EnsureCube
        widget_control, self.wStackInfo,SET_VALUE=msg.info
        ptr_free,self.stack
        if ptr_valid(msg.background) then begin
           type=size(*msg.background,/TYPE)
           if type eq 4 OR type eq 5 then begin ;floating pt. continuum vals
              self.stack=ptr_new(self.cube->Stack(*msg.foreground, $
                                                  BG_VALS=*msg.background))
           endif else $
              self.STACK=ptr_new(self.cube->Stack(*msg.foreground,$
                                                  BACKRANGES=*msg.background)) 
        endif else self.STACK=ptr_new(self.cube->Stack(*msg.foreground))
        self->SwitchMode,/STACK
     end
     'CUBEPROJ_RECORD': begin 
        widget_control, self.wInfoLine,set_value='Record: '+msg.INFO
        self.oDraw->SetTitle,'CubeView: '+msg.INFO
        self.cube=msg.CUBE
        self.bcd=msg.BCD
        self.bcd_err=msg.ERROR
        self.module=msg.MODULE
        self->SwitchMode,/BCD
     end
     'CUBEPROJ_CUBE': begin 
        widget_control, self.wInfoLine,set_value='Cube: '+msg.INFO
        self.oDraw->SetTitle,'CubeView: '+msg.INFO
        self.cube=msg.CUBE
        if ptr_valid(self.wavelength) then $
           cur_wav=(*self.wavelength)[self.cur_wav]
        self.wavelength=msg.WAVELENGTH
        if n_elements(cur_wav) gt 0 then begin 
           mn=min(abs(*self.wavelength-cur_wav),pos)
           self.cur_wav=pos
        endif
        widget_control, self.wLambda,SET_VALUE= $
                        string(*msg.wavelength,FORMAT='(F7.4)')
        ;; we don't want BCD mode, but either cube mode will do
        self->SwitchMode,BCD=0
     end
  endcase
  self->UpdateView
  self->MsgSend,{CUBEREC_UPDATE,self.mode eq 2b,self.cube,self.MODULE}
end

;=============================================================================
;  On - (Extraction Tool) Signup for all our messages.
;=============================================================================
pro CubeRec::On
  if self.mode gt 1 then return ;don't enable it for Rec mode
  if self->On() then begin      ;if turned on *again* .. reset
     self->Reset                ;reset to "no box drawn"
     return
  end
  self->tvPlug::On
  self.Box->On
  self.oDraw->MsgSignup,self,/TVDRAW_POSTDRAW
end

;=============================================================================
;  Off - (Extraction Tool) No more postdraw events needed. 
;=============================================================================
pro CubeRec::Off,_EXTRA=e
  self->tvPlug::Off,_EXTRA=e
  self.Box->Off
  ;; If we have no box ever drawn, shut down the message flow
  if self.box->IsDrawn() eq 0b then $
     self.oDraw->MsgSignup,self,TVDRAW_POSTDRAW=0
end

pro CubeRec::Reset,_EXTRA=e
  self.Box->Reset
  self->Off,_EXTRA=e
end

;=============================================================================
;  Icon
;=============================================================================
function CubeRec::Icon
  return,[[000B, 000B],[224B, 063B],[016B, 048B],[232B, 040B],$
          [068B, 036B],[254B, 035B],[066B, 034B],[066B, 050B],$
          [066B, 042B],[254B, 039B],[066B, 034B],[066B, 018B],$
          [226B, 010B],[002B, 006B],[254B, 003B],[000B, 000B]]
end

;=============================================================================
;  Description
;=============================================================================
function CubeRec::Description
  return,'Extract Spectra and Stack Cubes'
end

;=============================================================================
;  ReportWidget - Where to position our error and other messages
;=============================================================================
function CubeRec::ReportWidget
  return,self.wBase[0]
end
;;*************************End OverRiding methods******************************

;=============================================================================
;  SwitchMode - Switch among full, stacked and record mode.  To switch
;               out of BCD mode to one of the cube modes (unless
;               already set), specify BCD=0.
;=============================================================================
pro CubeRec::SwitchMode,FULL=full,STACK=stack,BCD=bcd
  if keyword_set(full) then mode=0
  if keyword_set(stack) then mode=1
  if keyword_set(bcd) then mode=2
  
  if n_elements(mode) ne 0 then begin
     if mode eq self.mode then begin ;could have been de-sensitived before
        widget_control, self.wBase[self.mode],/SENSITIVE
        return
     endif
     self.mode=mode
  endif else begin
     if n_elements(bcd) ne 0 then begin ; BCD=0 was passed
        if self.mode eq 2 then self.mode=0 ;go to full by default
     endif                      ; otherwise, just leave it the same
  endelse

  if self.mode eq 2 then begin  ;bcd mode
     self->Reset,/DISABLE       ;no need for our extraction tool.
     self.oAper->On
  endif else begin              ; A cube mode
     if NOT self->Enabled() then self->Enable
     self.oAper->Off,/RESET
  endelse 

  ;;Switch the base showing
  others=where(indgen(3) ne self.mode)
  for i=0,1 do widget_control, self.wBase[others[i]],MAP=0
  widget_control, self.wBase[self.mode],/MAP,/SENSITIVE
end

;=============================================================================
;  UpdateView - Show the appropriate cube plane/bcd/stack.
;=============================================================================
pro CubeRec::UpdateView
  case self.mode of
     0: begin                   ;full cube, show the correct plane
        widget_control, self.wLambda,SET_DROPLIST_SELECT=self.cur_wav
        self.oDraw->SetProperty,/NO_RESIZE, $
           IMORIG=self.cube->Cube(self.cur_wav)
     end
     1: begin                   ;show the stack
        if ptr_valid(self.stack) then $
           self.oDraw->SetProperty,/NO_RESIZE,IMORIG=*self.STACK
     end
     2: begin                   ;show the record
        if ptr_valid(self.BCD) then $
           self.oDraw->SetProperty,/NO_RESIZE,IMORIG=*self.BCD
     end
  endcase 
end

;=============================================================================
;  EnsureCube - Make sure the cube we have is still valid
;=============================================================================
pro CubeRec::EnsureCube
  if NOT obj_valid(self.cube) then begin 
     widget_control, self.wBase[self.mode],SENSITIVE=0
     self->Error,'Cube no longer valid.'
  endif 
end

pro CubeRec_event,ev
  widget_control, ev.handler, get_uvalue=uv
  call_method,uv.method,uv.self,ev
end

;=============================================================================
;  FullEvent - Handle event from the Full base
;=============================================================================
pro CubeRec::FullEvent,ev
  case ev.id of
     self.wLambda: begin
        sel=widget_info(self.wLambda,/DROPLIST_SELECT)
        self.cur_wav=sel
     end
     
     self.wBrowse: begin 
        if tag_names(ev,/STRUCTURE_NAME) eq 'WIDGET_TIMER' then begin 
           if self.playing then widget_control, ev.id,TIMER=.5
           value=1              ;just increment
        endif else value=ev.value
        case value of 
           0:self.cur_wav=(self.cur_wav-1) eq -1? $ ;Previous
                          n_elements(*self.wavelength)-1: $
                          self.cur_wav-1
           1:self.cur_wav=(self.cur_wav+1) mod $ ;Next
                          n_elements(*self.wavelength) 
           2: begin             ;Play or Stop
              self.playing=1-self.playing
              if self.playing then widget_control, ev.id,TIMER=1.
              widget_control, self.wPlayStop, $
                              SET_VALUE=(["Play","Stop"])[self.playing]
           end
        endcase
     end
  endcase 
  self->UpdateView
  self->MsgSend,{CUBEREC_FULL,(*self.wavelength)[self.cur_wav]}
end

;=============================================================================
;  StackEvent - Handle event from the Stack base
;=============================================================================
pro CubeRec::StackEvent,ev
  ;; Only the "Full" button: If he's listening, the CubeSpec will send
  ;; out a message which we will hear about, and toggle mode
  ;; accordingly.  If he's not listening, we're in Full mode already
  ;; anyway (and hence won't be here).
  self->MsgSend,{CUBEREC_FULL,(*self.wavelength)[self.cur_wav]}
end

;=============================================================================
;  Extract -  Extract a spectrum from box
;=============================================================================
pro CubeRec::Extract
  if NOT obj_valid(self.cube) then $
     self->Error,'Cube no longer valid (perhaps it was destroyed?).'
  self.Box->Getlrtb,l,r,t,b
  spec=self.cube->Extract([l,b],[r,t])
  info=string(FORMAT='(%"Extracted from %s, [%d,%d]->[%d,%d]")', $
              self.cube->ProjectName(),l,b,r,t)
  if XRegistered("CubeViewSpec") eq 0 then begin 
     o=obj_new('CubeViewSpec',PARENT_GROUP=self.wBase[0])
     self->MsgSignup,o,/CUBEREC_SPEC,/CUBEREC_FULL
     o->MsgSignup,self,/CUBEVIEWSPEC_STACK
     o->MsgSignup,self,/CUBEVIEWSPEC_FULL
     if self.mode eq 0 then $
        self->MsgSend,{CUBEREC_FULL,(*self.wavelength)[self.cur_wav]}
  endif 
  sp=ptr_new(spec,/NO_COPY)
  self->MsgSend,{CUBEREC_SPEC,info,self.wavelength,sp}
  ptr_free,sp
end

;=============================================================================
;  Cleanup
;=============================================================================
pro CubeRec::Cleanup
  ptr_free,self.stack           ;bcd and error are not ours to destroy
  self->tvPlug::Cleanup
end

;=============================================================================
;  Init -  Initialize the CubeRec object
;=============================================================================
function CubeRec::Init,parent,oDraw,CUBE=cube,APER_OBJECT=aper,_EXTRA=e
  if (self->tvPlug::Init(oDraw,_EXTRA=e) ne 1) then return,0 
  
  if n_elements(color) eq 0 then color=!D.TABLE_SIZE-1
  
  ;; Get a tvrbox object, signing ourselves up for box messages from it.
  self.Box=obj_new('tvRBox', oDraw,/CORNERS,/SNAP,COLOR=color,_EXTRA=e)
  self.Box->MsgSignup,self,/BOX
  
  if obj_valid(cube) then cube->MsgSignup,self ;listen for this cube's messages
  
  ;; set up the different bases
  b=widget_base(parent,/COLUMN,/FRAME,/BASE_ALIGN_LEFT,SPACE=1, $
                UNAME='CubeRec',UVALUE=self)
  
  ;; A common line showing information on which cube or record we've got
  self.wInfoLine=widget_label(b,/FRAME,/ALIGN_LEFT, $
                              value=STRING(FORMAT='(A,T62)', $
                                           'No Cube or Record'))
  
  ;; A bulletin board to hold the three separate bases
  mapbase=widget_base(b)
  
  ;; Populate the first base: the full cube
  self.wBase[0]=widget_base(mapbase,/ROW,MAP=0, $
                            UVALUE={self:self,method:'FullEvent'}, $
                            /BASE_ALIGN_CENTER,EVENT_PRO='cuberec_event')
  
  self.wLambda=widget_droplist(self.wBase[0],Title="Wavelength", $
                               VALUE=[string(0.0)])
  self.wBrowse=cw_bgroup(self.wBase[0],['Prev','Next','Play'],/ROW, $
                         /NO_RELEASE,IDS=ids)
  self.wPlayStop=ids[2]
  
  ;; Populate the second base: the stacked cube
  self.wBase[1]=widget_base(mapbase,/ROW,MAP=0, $
                            UVALUE={self:self,method:'StackEvent'}, $
                            /BASE_ALIGN_CENTER,EVENT_PRO='cuberec_event')
  self.wStackInfo=widget_label(self.wBase[1],VALUE=string(FORMAT='(A,T40)',''))
  ;self.wFull=widget_button(self.wBase[1],value='Full Cube')
  
  ;; Populate the third base: for the BCD's
  self.wBase[2]=widget_base(mapbase,/ROW,MAP=0,/BASE_ALIGN_CENTER)
  self.oAper=(aper=obj_new('CubeAper',self.wBase[2],oDraw,_EXTRA=e))
  self->MsgSetup,['CUBEREC_SPEC','CUBEREC_FULL','CUBEREC_UPDATE']
  self->MsgSignup,self.oAper,/CUBEREC_UPDATE ;give them our message
  return,1
end

;=============================================================================
;  CubeRec__define - Prototype the CubeRec class
;=============================================================================
pro CubeRec__define
  st={CubeRec, $
      INHERITS tvPlug,$         ;it's a tvDraw plugin
      cube:obj_new(), $         ;the cube project we're servicing
      mode:0, $                 ;full (=0),stacked (=1), bcd (=2)
      wavelength:ptr_new(), $   ;the wavelength planes of our cube
      cur_wav:0L,$              ;the index of wavelength plane being viewed
      playing:0, $              ;whether we're "playing" the full cube
      spec:obj_new(), $         ;our CubeSpec tool
      box:obj_new(), $          ;our extraction box object
      oAper: obj_new(), $       ;our aperture viewing/editing tool
      STACK:ptr_new(), $        ;the stacked image
      BCD:ptr_new(), $          ;the BCD data
      BCD_ERR:ptr_new(), $      ;the BCD error
      MODULE:'',$               ;the modules for cube or rec
      cal:obj_new(), $          ;the object
      ;; Widget ID's
      wBase:lonarr(3), $        ;the three bases (full/stack/rec)
      wInfoLine:0L, $           ;A line of info on the current stack/etc.
      wLambda:0L, $             ;the wavelength choose list
      wBrowse:0L, $             ;wavelength browser buttons
      wPlayStop:0L,$            ;the play/stop button
      wStackInfo:0L, $          ;The stack information
      wFull:0L}                 ;The "Switch to Full mode" button
  
  ;; The messages we send
  msg={CUBEREC_SPEC,Info:'',wavelength:ptr_new(),spec:ptr_new()}
  ;;  spec is 2xn or 3xn, info is a text message describing the extraction.
  
  msg={CUBEREC_FULL,wavelength:0.0} ;s
  msg={CUBEREC_UPDATE,BCD_MODE:0,CUBE:obj_new(),MODULE:''} ;XXX WCS!
  
  ;;What about backtrack in full cube mode: showing which
  ;;BCD's/pixels contributed to a given cube pixel.  How?  Maybe
  ;;pop up a list with BCD/pixel
end
