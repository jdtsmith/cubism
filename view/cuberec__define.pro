;; A class which handles extractions on the cube, and also serves as a
;; concentrator for all messages to the viewer-related objects.

;;**************************OverRiding methods********************************
;=============================================================================
;  Message - Let's hear about new Stacks, Switch to Full, Records, or cubes 
;=============================================================================
pro CubeRec::Message, msg
  if widget_info(self.wBase[0],/VALID_ID) eq 0 then return
  self->tvPlug::Message,msg,TYPE=type
  cal_update=0b
  free=0b
  case type of
     'BOX': begin 
        self->Extract
        return
     end 
     'CUBEVIEWSPEC_SAVE': begin 
        method=self.region_file?"ExtractFileRegion":"Extract"
        if msg.export then $
           call_method,method,self,/EXPORT,FILE=self.region_file $
        else call_method,method,self,/SAVE,FILE=self.region_file
        return
     end
     ;; Send updates for these
     'CUBEVIEWSPEC_FULL': begin 
        if ~ptr_valid(self.wavelength) then return
        mn=min(abs(*self.wavelength-msg.wavelength),mpos)
        self.cur_wav=mpos
        self->SwitchMode,/FULL
     end
     'CUBEVIEWSPEC_STACK': begin
        self->EnsureCube
        *self.stack_msg=msg
        widget_control, self.wStackInfo,SET_VALUE=msg.info
        self->SwitchMode,/STACK
     end
     'CUBEPROJ_RECORD': begin 
        widget_control, self.wInfoLine,set_value='Record: '+msg.INFO
        self.oDraw->SetTitle,'CubeView: '+msg.INFO
        if self.free_bcd then ptr_free,self.BCD,self.BCD_UNC,self.BCD_BMASK
        self.free_bcd=msg.free
        self.cube=msg.CUBE
        self.bcd=msg.BCD
        self.BCD_UNC=msg.UNC
        self.BCD_BMASK=msg.BMASK
        self.BCD_BACKGROUND=msg.BACKGROUND
        self.BCD_BACKGROUND_UNC=msg.BACKGROUND_UNC
        self.module=msg.MODULE
        ptr_free,self.rec_set
        if ptr_valid(msg.RECORD_SET) then self.rec_set=ptr_new(*msg.RECORD_SET)
        self->SwitchMode,/BCD
     end
     'CUBEPROJ_CUBE': begin 
        widget_control, self.wInfoLine,set_value='Cube: '+msg.INFO
        self.oDraw->SetTitle,'CubeView: '+msg.INFO
        self.cube=msg.CUBE
        ptr_free,self.rec_set
        if ptr_valid(self.wavelength) then $
           cur_wav=(*self.wavelength)[self.cur_wav< $
                                      (n_elements(*self.wavelength)-1)]
        self.wavelength=msg.WAVELENGTH
        if n_elements(cur_wav) gt 0 then begin 
           mn=min(abs(*self.wavelength-cur_wav),pos)
           self.cur_wav=pos
        endif
        self.cube->GetProperty,ASTROMETRY=astr
        widget_control, self.wLambda,SET_VALUE= $
                        string(*msg.wavelength,FORMAT='(F7.4)'),/CLEAR_EVENTS
        ;; we don't want BCD mode, but either cube mode (full or stack) will do
        self->SwitchMode,BCD=0
     end
     'CUBEPROJ_VISUALIZE': begin 
        if ptr_valid(msg.ASTROMETRY) then astr=*msg.ASTROMETRY
        self.oDraw->SetTitle,'CubeView: '+msg.INFO
        widget_control, self.wInfoLine,set_value='Visualizing '+msg.INFO
        self.cube=msg.CUBE
        self.module=msg.MODULE
        self.visualize_image=msg.IMAGE
        self->SwitchMode,/VISUALIZE
     end
     
     'CUBEPROJ_UPDATE': begin 
        if msg.new_cube && self.mode ge 2 then return
        self.cube=msg.cube
        self.cube->GetProperty,WAVELENGTH=wl,ASTROMETRY=astr,CUBE_SIZE=cs, $
                               /POINTER
        
        if n_elements(wl) ne 0 && ptr_valid(wl) then self.wavelength=wl
        if self.mode eq 1 && n_elements(*self.stack_msg) gt 0 then begin 
           fg=*(*self.stack_msg).foreground
           if ~array_equal(fg le (cs[2]-1),1b) then $
              self->SwitchMode,/FULL
        endif 
     end 
     'CUBEPROJ_CALIB_UPDATE': cal_update=1b
     'CUBEPROJ_RECORD_UPDATE': begin 
        ;; Bring down the whole house of cards
        if msg.deleted && ~ptr_valid(self.IMAGE) then begin 
           obj_destroy,self.oDraw
           return
        endif 
     end 
     else:
  endcase
  self->UpdateData
  if n_elements(astr) ne 0 then astr=ptr_new(astr,/NO_COPY) else $
     astr=ptr_new()
  self->MsgSend,{CUBEREC_UPDATE, $
                 self.mode eq 2b,self.mode eq 0b,self.mode eq 3b,$
                 self.cur_wav, self.cube,self.MODULE,self.BCD,self.BCD_BMASK, $
                 self.UNCERTAINTY, astr,self.rec_set,cal_update}
  ptr_free,astr
  self->UpdateView
end

;=============================================================================
;  On - (Extraction Tool)
;=============================================================================
pro CubeRec::On
  if self.mode gt 1 then return ;don't enable it for Rec mode
  if self->On() then begin      ;if turned on *again* .. reset
     self->Reset                ;reset to "no box drawn"
     return
  end
  self->tvPlug::On
  self.Box->On
end

;=============================================================================
;  Off - (Extraction Tool)
;=============================================================================
pro CubeRec::Off,_EXTRA=e
  self->tvPlug::Off,_EXTRA=e
  self.Box->Off
  self.Region->Off
end

;=============================================================================
;  Reset
;=============================================================================
pro CubeRec::Reset,_EXTRA=e
  self.Box->Reset
  self.Region->Reset
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


function CubeRec::MouseHelp
  return,['Extract','','']
end

;=============================================================================
;  ReportWidget - Where to position our error and other messages
;=============================================================================
function CubeRec::ReportWidget
  return,self.wBase[0]
end


;;*************************End OverRiding methods******************************

;=============================================================================
;  GetProperty
;=============================================================================
pro CubeRec::GetProperty,BCD_MODE=bcd_mode,CUBE=cube,VISUALIZE_MODE=vm
  if arg_present(bcd_mode) then bcd_mode=self.mode eq 2
  if arg_present(cube) then cube=self.cube
  if arg_present(vm) then vm=self.mode eq 3
end

;=============================================================================
;  BuildStack
;=============================================================================
pro CubeRec::BuildStack,UNCERTAINTY=stack_unc,_EXTRA=e
  if ~ptr_valid(self.stack_msg) then $
     self->Error,'No stack yet received.'
  msg=*self.stack_msg
  ptr_free,self.stack
  if msg.name then begin        ; A named map
     self.STACK=ptr_new(self.cube->Stack(MAP_NAME=msg.name, $
                                         WAVELENGTH_WEIGHTED=msg.weight_cont,$
                                         STACK_UNCERTAINTY=stack_unc, $
                                         _EXTRA=e))
  endif else begin 
     if ptr_valid(msg.background) then begin
        type=size(*msg.background,/TYPE)
        if type eq 4 OR type eq 5 then begin ;floating pt. continuum vals
           self.STACK=ptr_new(self.cube-> $
                              Stack(*msg.foreground, $
                                    WAVELENGTH_WEIGHTED=msg.weight_cont,$
                                    STACK_UNCERTAINTY=stack_unc, $
                                    BG_VALS=*msg.background,_EXTRA=e))
        endif else $            ;background index ranges
           self.STACK=ptr_new(self.cube-> $
                              Stack(*msg.foreground, $
                                    WAVELENGTH_WEIGHTED=msg.weight_cont,$
                                    STACK_UNCERTAINTY=stack_unc, $
                                    BACKRANGES=*msg.background,_EXTRA=e))
     endif else self.STACK= $
        ptr_new(self.cube->Stack(*msg.foreground, $
                                 STACK_UNCERTAINTY=stack_unc,_EXTRA=e))
  endelse 
end


;=============================================================================
;  SwitchMode - Switch among full, stacked, record, and visualization
;               modes. To switch out of BCD mode to one of the cube
;               modes (unless already set), specify BCD=0.
;=============================================================================
pro CubeRec::SwitchMode,FULL=full,STACK=stack,BCD=bcd,VISUALIZE=viz
  if keyword_set(full) then mode=0
  if keyword_set(stack) then mode=1
  if keyword_set(bcd) then mode=2
  if keyword_set(viz) then mode=3
  ;; BCD=0 was passed
  if n_elements(mode) eq 0 AND n_elements(bcd) ne 0 then begin 
     if self.mode ge 2 then self.mode=0 ;go to full by default
  endif                         ; otherwise, just leave it the same
  
  if n_elements(mode) ne 0 then begin
     if mode eq self.mode then begin ;could have been de-sensitized before
        widget_control, self.wBase[self.mode],/SENSITIVE
        return
     endif
     self.mode=mode
  endif
  
  if self.mode ge 2 then begin  ;bcd or vis
     self->Reset,/DISABLE       ;no need for our extraction tool.
     if obj_valid(self.oView) then begin
        self.oView->MsgSignup,self,/NONE ;not listening to the view tool
        self.oView->Quit
     endif 
     if self.mode eq 2 then begin ;bcd mode
        self.oAper->On 
        self.oVis->Off,/RESET,/NO_REDRAW,/DISABLE
     endif else begin           ;visualize mode
        self.oAper->Off,/RESET,/NO_REDRAW
        self.oVis->On
     endelse 
  endif else begin              ; A cube mode
     if ~self->Enabled() then self->Enable ;need the extraction tool
     if obj_valid(self.oView) then $
        self.oView->MsgSignup,self,/CUBEVIEWSPEC_STACK,/CUBEVIEWSPEC_FULL, $
                              /CUBEVIEWSPEC_SAVE
     self.oAper->Off,/RESET,/NO_REDRAW 
  endelse 
  
  ;; The menu buttons
  if widget_info(self.wMapSaveBut,/VALID_ID) then begin 
     widget_control, self.wMapSaveBut,SENSITIVE=self.mode eq 1
  endif 
  if widget_info(self.wExtractRegionBut,/VALID_ID) then begin 
     widget_control, self.wExtractRegionBut,SENSITIVE=self.mode lt 2
  endif 
  
  ;;Switch the base showing
  others=where(indgen(4) ne self.mode)
  for i=0,n_elements(others)-1 do $
     widget_control, self.wBase[others[i]],MAP=0
  widget_control, self.wBase[self.mode],/MAP,/SENSITIVE
end


;=============================================================================
;  UpdateData - Update the image and its uncertainty
;=============================================================================
pro CubeRec::UpdateData
  if self.free_im then ptr_free,self.IMAGE,self.UNCERTAINTY
  case self.mode of
     0: begin                   ;full cube, show the correct plane
        self.IMAGE=ptr_new(self.cube->Cube(self.cur_wav,UNCERTAINTY=im_unc))
        if n_elements(im_unc) ne 0 then $
           self.UNCERTAINTY=ptr_new(im_unc,/NO_COPY) $
        else self.UNCERTAINTY=ptr_new()
        self.free_im=1b
     end
     1: begin                   ;show the stack
        self->BuildStack,UNCERTAINTY=im_unc
        if n_elements(im_unc) ne 0 then $
           self.UNCERTAINTY=ptr_new(im_unc,/NO_COPY) $
        else self.UNCERTAINTY=ptr_new()
        self.IMAGE=self.STACK
        self.free_im=1b
     end
     2: begin                   ;show the record
        if widget_info(self.wBGSub,/BUTTON_SET) && $
           ptr_valid(self.BCD_BACKGROUND) then begin 
           self.IMAGE=ptr_new(*self.BCD - *self.BCD_BACKGROUND)
           if ptr_valid(self.BCD_UNC) && ptr_valid(self.BCD_BACKGROUND_UNC) $
           then $
              self.UNCERTAINTY=ptr_new(sqrt(*self.BCD_UNC^2+ $
                                            *self.BCD_BACKGROUND_UNC^2)) $
           else self.UNCERTAINTY=ptr_new()
           self.free_im=1b
        endif else begin 
           self.image=self.BCD
           self.UNCERTAINTY=self.BCD_UNC
           self.free_im=0b
        endelse 
     end
     3: begin                   ;visualize an image
        self.image=self.VISUALIZE_IMAGE
        self.UNCERTAINTY=ptr_new()
        self.free_im=0b
     end 
  endcase
end

;=============================================================================
;  UpdateView - Show the appropriate cube plane/bcd/stack.
;=============================================================================
pro CubeRec::UpdateView
  case self.mode of
     0: begin                   ;full cube
        widget_control, self.wLambda,SET_COMBOBOX_SELECT=self.cur_wav
     end
     2: begin                   ;record, update BG button
        widget_control, self.wBGSub,SENSITIVE=ptr_valid(self.BCD_BACKGROUND)
     end
     else:
  endcase 
  if ptr_valid(self.IMAGE) then $
     self.oDraw->SetProperty,/NO_RESIZE,IMORIG=*self.IMAGE $ 
  else self.oDraw->Erase,/FULL
end


;=============================================================================
;  Export - Export the image to the command line
;=============================================================================
pro CubeRec::Export
  case self.mode of
     0: begin                   ;full cube, show the correct plane
        widget_control, self.wLambda,SET_COMBOBOX_SELECT=self.cur_wav
        im=self.cube->Cube(self.cur_wav)
     end
     1: begin                   ;export the stack
        if ~ptr_valid(self.STACK) then return
        im=*self.STACK
     end
     2: begin                   ; the record
        if ~ptr_valid(self.BCD) then return
        im=*self.BCD
        if (widget_info(self.wBGSub,/BUTTON_SET)) && $
            ptr_valid(self.BCD_BACKGROUND) then im-=*self.BCD_BACKGROUND
     end
  endcase
  self.cube->ExportToMain,MAP=im,TYPE=self.mode eq 2?"_im":void
end

;=============================================================================
;  EnsureCube - Make sure the cube we have is still valid
;=============================================================================
pro CubeRec::EnsureCube
  if ~obj_valid(self.cube) then begin 
     widget_control, self.wBase[self.mode],SENSITIVE=0
     self->Error,'Cube no longer valid.'
  endif 
end

pro CubeRec_event,ev
  widget_control, ev.handler, get_uvalue=uv
  if uv.event then $
     call_method,uv.method,uv.self,ev $
  else $
     call_method,uv.method,uv.self     
end

;=============================================================================
;  FullEvent - Handle event from the Full base
;=============================================================================
pro CubeRec::FullEvent,ev
  case ev.id of
     self.wLambda: begin
        self.cur_wav=ev.index
     end

     self.wSlider: begin 
        self.delay=(10.-ev.value)/4.+.1
        return
     end
          
     self.wBrowse: begin 
        if tag_names(ev,/STRUCTURE_NAME) eq 'WIDGET_TIMER' then begin 
           if self.playing then widget_control, ev.id,TIMER=self.delay
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
              if self.playing then widget_control, ev.id,TIMER=self.delay
              widget_control, self.wPlayStop, $
                              SET_VALUE=(["Play","Stop"])[self.playing]
           end
        endcase
     end
  endcase 
  self->UpdateData &  self->UpdateView
  self->MsgSend,{CUBEREC_FULL,self.cur_wav,(*self.wavelength)[self.cur_wav]}
end

;=============================================================================
;  StackEvent - Handle event from the Stack base
;=============================================================================
pro CubeRec::StackEvent,ev
  ;; Only the "Full" button press.  If he's listening, the CubeSpec
  ;; will send out a message which we will hear about, and toggle mode
  ;; accordingly.  If he's not listening, we're in Full mode already
  ;; anyway (and hence won't be here).
  self->MsgSend,{CUBEREC_FULL,(*self.wavelength)[self.cur_wav]}
end

;=============================================================================
;  SaveMapEvent -  Save the showing image as a map
;=============================================================================
pro CubeRec::SaveMapEvent,ev
  self->CheckCube
  if ~ptr_valid(self.STACK) || ~ptr_valid(self.stack_msg) then $
     self->Error,'No valid map to save.'
  self->BuildStack,/UNCERTAINTY,/SAVE
end


;=============================================================================
;  SetupViewSpec -  Get a spectrum viewer if necessary
;=============================================================================
pro CubeRec::SetupViewSpec
  ;; XXX ensure valid wavelength
  if obj_valid(self.oView) eq 0 then begin 
     self.oView=obj_new('CubeViewSpec',PARENT_GROUP=self.wBase[0])
     ;; Set up messages between us
     self->MsgSignup,self.oView,/CUBEREC_SPEC,/CUBEREC_FULL
     self.oView->MsgSignup,self,/CUBEVIEWSPEC_STACK,/CUBEVIEWSPEC_FULL, $
                           /CUBEVIEWSPEC_SAVE
     self.cur_wav <= n_elements(*self.wavelength)-1
     if self.mode eq 0 then $
        self->MsgSend, $
           {CUBEREC_FULL,self.cur_wav,(*self.wavelength)[self.cur_wav]}
  endif 
end

;=============================================================================
;  ExtractFileRegion -  Extract a region from a file
;=============================================================================
pro CubeRec::ExtractFileRegion,FILE=rff,_EXTRA=e
  if ~self->On() then self->On
  self->CheckCube

  if n_elements(rff) eq 0 then rff=1 ;select and return the file
  spec=self.cube->Extract(FROM_FILE=rff,OUTPUT_POLY=op,UNCERTAINTY=spec_unc, $
                          _EXTRA=e)
  if spec[0] eq -1 then return
  self.region_file=rff     
  self->SetupViewSpec
  
  spec=ptr_new(spec,/NO_COPY)
  if n_elements(spec_unc) ne 0 then spec_unc=ptr_new(spec_unc,/NO_COPY) else $
     spec_unc=ptr_new()
  info=string(FORMAT='(%"Region from %s")',file_basename(self.region_file))
  self.Box->Reset & self.Box->Off
  self.region->SetProperty,REGION=op
  if ~self.region->On() then self.region->On
  self->MsgSend,{CUBEREC_SPEC,info,self.wavelength,spec,spec_unc}
  ptr_free,spec,spec_unc
end


;=============================================================================
;  Extract -  Extract a spectrum from box
;=============================================================================
pro CubeRec::Extract,_EXTRA=e
  self->CheckCube
  self.Box->Getlrtb,l,r,t,b
  self.region_file=''           ;no longer extracting by region
  spec=self.cube->Extract([l,b],[r,t],UNCERTAINTY=spec_unc,_EXTRA=e)
  info=string(FORMAT='(%"Extracted from %s, [%d,%d]->[%d,%d]")', $
              self.cube->ProjectName(),l,b,r,t)
  self->SetupViewSpec
  spec=ptr_new(spec,/NO_COPY)
  if n_elements(spec_unc) ne 0 then spec_unc=ptr_new(spec_unc,/NO_COPY) else $
     spec_unc=ptr_new()
  self->MsgSend,{CUBEREC_SPEC,info,self.wavelength,spec,spec_unc}
  ptr_free,spec,spec_unc
end

;=============================================================================
;  CheckCube - Make sure our cube is still alive
;=============================================================================
pro CubeRec::CheckCube
  if NOT obj_valid(self.cube) then $
     self->Error,'Cube no longer valid (perhaps it was destroyed?).'
end

;=============================================================================
;  Cleanup
;=============================================================================
pro CubeRec::Cleanup
  ;; bcd,wavelength, and error are not ours to destroy
  ptr_free,self.STACK,self.stack_msg,self.rec_set
  if self.free_im then ptr_free,self.IMAGE,self.UNCERTAINTY
  if self.free_bcd then ptr_free,self.BCD,self.BCD_BMASK,self.BCD_UNC
  self->tvPlug::Cleanup
end

;=============================================================================
;  Init -  Initialize the CubeRec object
;=============================================================================
function CubeRec::Init,parent,oDraw,CUBE=cube,APER_OBJECT=aper, $
                       VISUALIZE_OBJECT=vis,MENU=menu, $
                       VISUALIZE_COLOR=vis_color,_EXTRA=e
  if (self->tvPlug::Init(oDraw,_EXTRA=e) ne 1) then return,0 

  self->MsgSetup,['CUBEREC_SPEC','CUBEREC_FULL','CUBEREC_UPDATE']
  
  ;; Get an extraction tvrbox object, signing ourselves up for box messages
  self.Box=obj_new('tvRBox', oDraw,/CORNERS,/SNAP,_EXTRA=e)
  self.Box->MsgSignup,self,/BOX
  
  self.Region=obj_new('tvFixedRegion',oDraw,_EXTRA=e)
  
  ;; listen for this cube's messages
  if obj_valid(cube) then $
     cube->MsgSignup,self,/CUBEPROJ_RECORD,/CUBEPROJ_CUBE,/CUBEPROJ_VISUALIZE,$
                     /CUBEPROJ_UPDATE,/CUBEPROJ_CALIB_UPDATE, $
                     /CUBEPROJ_RECORD_UPDATE

  ;; set up the different bases
  b=widget_base(parent,/COLUMN,/FRAME,/BASE_ALIGN_LEFT,SPACE=1, $
                UNAME='CubeRec',UVALUE=self)

  ;; A common line showing information on which cube or record we've got
  self.wInfoLine=widget_label(b,/FRAME,/ALIGN_LEFT, $
                              value=STRING(FORMAT='(A,T62)', $
                                           'No Cube or Record'))

  ;; A bulletin board to hold the 4 separate bases
  mapbase=widget_base(b)

  ;; Populate the first base: the full cube
  self.wBase[0]=widget_base(mapbase,/COLUMN,MAP=0, $
                            UVALUE={self:self,method:'FullEvent',event:1}, $
                            /BASE_ALIGN_CENTER,EVENT_PRO='cuberec_event')

  base=widget_base(self.wBase[0],/ROW,SPACE=1,/BASE_ALIGN_LEFT) 
  b1=widget_base(base,/ROW,SPACE=1) 
  lab=widget_label(b1,value='Wave:')
  self.wLambda=widget_combobox(b1,/DYNAMIC_RESIZE,XSIZE=80)
  b2=widget_base(base,/COLUMN,SPACE=1,/BASE_ALIGN_CENTER,/FRAME)
  self.wBrowse=cw_bgroup(b2,['Prev','Next','Play'],/ROW, $
                         /NO_RELEASE,IDS=ids)
  self.wPlayStop=ids[2]
  self.wSlider=widget_slider(b2,MINIMUM=0,MAXIMUM=10, $
                             TITLE="Play Speed",/SUPPRESS_VALUE,VALUE=6)
  self.delay=1.1
  
  
  ;; Populate the second base: the stacked cube
  self.wBase[1]=widget_base(mapbase,/ROW,MAP=0, $
                            UVALUE={self:self,method:'StackEvent',event:1}, $
                            /BASE_ALIGN_CENTER,EVENT_PRO='cuberec_event')
  self.wStackInfo=widget_label(self.wBase[1],VALUE=' ',/DYNAMIC_RESIZE)
  
  
  ;; Populate the third base: for the BCD's
  self.wBase[2]=widget_base(mapbase,/ROW,MAP=0,/BASE_ALIGN_CENTER)
  self.oAper=(aper=obj_new('CubeAper',self.wBase[2],oDraw,_EXTRA=e))
  self->MsgSignup,self.oAper,/CUBEREC_UPDATE ;give them our message
  b3=widget_base(self.wBase[2],/COLUMN,/NONEXCLUSIVE,/FRAME,SPACE=1)
  self.wBGsub=widget_button(b3,VALUE='BGSub',/FRAME,EVENT_PRO='cuberec_event',$
                            UVALUE=[{self:self, $
                                     method:'Message',event:1}])
  
  
  ;; Populate the fourth base: visualization
  self.wBase[3]=widget_base(mapbase,/COLUMN,MAP=0,/BASE_ALIGN_CENTER)
  self.oVis=(vis=obj_new('IRSMapVisualize',self.wBase[3], $
                         oDraw,COLOR=vis_color))
  self->MsgSignup,self.oVis,/CUBEREC_UPDATE
  
  ;; Add a menu items if allowed
  if n_elements(menu) ne 0 then if widget_info(menu,/VALID_ID) then begin 
     self.wMapSaveBut=widget_button(menu,value='Save Map as FITS...', $
                                    SENSITIVE=0,EVENT_PRO='cuberec_event', $
                                    UVALUE={self:self,method:'SaveMapEvent', $
                                            event:1})
     but=widget_button(menu,value='Export to Command Line...', $
                       SENSITIVE=~LMGR(/VM,/RUNTIME), $
                       EVENT_PRO='cuberec_event', $
                       UVALUE={self:self,method:'Export',event:0})
     self.wExtractRegionBut=widget_button(menu, $
                                          value='Extract Region From File...',$
                                          EVENT_PRO='cuberec_event', $
                                          UVALUE={self:self, $
                                                  method:'ExtractFileRegion', $
                                                  event:0},/SEPARATOR)
  endif
  self.stack_msg=ptr_new(/ALLOCATE_HEAP)
  return,1
end

;=============================================================================
;  CubeRec__define - Prototype the CubeRec class
;=============================================================================
pro CubeRec__define
  st={CubeRec, $
      INHERITS tvPlug,$         ;it's a tvDraw plugin
      cube:obj_new(), $         ;the cube project we're servicing
      mode:0, $                 ;full (=0),stacked (=1), bcd (=2), viz (=3)
      wavelength:ptr_new(), $   ;the wavelength planes of our cube
      cur_wav:0L,$              ;the index of wavelength plane being viewed
      playing:0, $              ;whether we're "playing" the full cube
      delay:0.0, $              ;the play speed delay
      spec:obj_new(), $         ;our CubeSpec tool
      box:obj_new(), $          ;our extraction box object
      region:obj_new(), $       ;fixed region object
      region_file:'', $         ;file from which region recovered
      oAper: obj_new(), $       ;our aperture viewing/editing tool
      oView: obj_new(), $       ;our ViewSpec tool
      oVis: obj_new(), $        ;the visualization tool
      STACK:ptr_new(), $        ;the stacked image
      stack_msg: ptr_new(), $   ;cache the stack message
      BCD:ptr_new(), $          ;the BCD (or flatap,droopres, etc.) data
      BCD_UNC:ptr_new(), $      ;uncertainty in the BCD
      BCD_BMASK:ptr_new(), $    ;the BCD mask data
      BCD_BACKGROUND:ptr_new(),$ ;the BCD background to subtract (togglable)
      BCD_BACKGROUND_UNC:ptr_new(),$ ;the uncertainty in the background
      VISUALIZE_IMAGE: ptr_new(), $ ;the vis image
      MODULE:'',$               ;the modules for cube or rec
      IMAGE: ptr_new(), $       ;the displayed image
      UNCERTAINTY:ptr_new(), $  ;the uncertainty in BCD,Cube,etc.
      free_im:0b, $             ;whether to free the image and uncertainty data
      free_bcd:0b, $            ;whether to free the BCD data
      cal:obj_new(), $          ;the calibration object
      rec_set:ptr_new(), $      ;the record(s) being examined
      ;; Widget ID's
      wBase:lonarr(4), $        ;the four bases (full/stack/rec/vis)
      wInfoLine:0L, $           ;A line of info on the current stack/etc.
      wLambda:0L, $             ;the wavelength choose list
      wBrowse:0L, $             ;wavelength browser buttons
      wPlayStop:0L,$            ;the play/stop button
      wSlider:0L, $             ;play speed slider
      wBGSub:0L, $              ;background subtract button
      wStackInfo:0L, $          ;The stack information
      wFull:0L,$                ;The "Switch to Full mode" button
      wMapSaveBut:0L, $         ;The Save Map as FITS button
      wExtractRegionBut:0L}     ;Extract Region from File button
  
  ;; The messages we send
  
  ;; Extracted Spectra: spec and spec_unc are 1D vectors, info is a
  ;; text message describing the extraction.
  msg={CUBEREC_SPEC,Info:'',wavelength:ptr_new(),spec:ptr_new(), $
       spec_unc:ptr_new()}
  
  ;; Special purpose: full Cube being viewed.
  msg={CUBEREC_FULL,plane:0L,wavelength:0.0}
  
  ;; General update
  msg={CUBEREC_UPDATE,BCD_MODE:0b,FULL_MODE:0b,VISUALIZE_MODE:0b,PLANE:0L, $
       CUBE:obj_new(),MODULE:'',BCD:ptr_new(), BMASK:ptr_new(), $
       UNC: ptr_new(), ASTROMETRY: ptr_new(), RECORD_SET: ptr_new(), $
       CALIB_UPDATE:0b}
end
