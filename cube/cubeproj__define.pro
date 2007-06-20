;+
; NAME:  
;
;    CubeProj
;
; CONTACT:
;
;    UPDATED VERSIONS of CUBISM and more information can be found at:
;       http://sings.stsci.edu/cubism
;
; DESCRIPTION:
;    
;    An object wrapper for CUBISM spectral mapping projects,
;    containing the input BCD and calibration data, along with the
;    routines to convert them into spectral cubes and analyze those
;    cubes.
;    
; CATEGORY:
;
;    CUBISM Spectral Cube Reduction, Analysis and Processing.
;    	
; COMMON BLOCKS:
;
;    CUBISM_DIR and CUBISM_VERSION: See include files.
;
; SIDE EFFECTS:
;
;    A GUI interface to CubeProj is available.
;
; METHODS:
;
;    Init: 
;
;       CALLING SEQUENCE:
;
;          cube=obj_new('CubeProj',[name])
;
;       OPTIONAL INPUT PARAMETERS:
;
;          name:  A name for the project.  Will otherwise be left blank.
;
;    Show:
;  
;	DESCRIPTION:
;
;          Run the GUI interface.  See the routine CUBISM for an
;          easier way to open an existing or new cube project and show
;          it.
;	
;       CALLING SEQUENCE:
;
;          cube->Show
;
; PROCEDURES:
;
;    Requires the IRS_Calib calibration class.
;
; NOTES:
;  
;    A Cube Project is a single, self-contained data object which
;    holds all the IRS BCD/BMASK/UNC/etc. data, references to
;    calibration parameters, and positional account information
;    required to create a single, 3D (two spatial, one wavelength)
;    "spectral cube" from input IRS Spectral Mapping Mode data sets
;    (see http://ssc.spitzer.caltech.edu/irs/aotintro.html).  In
;    addition, it can perform various manipulations and extractions on
;    the resulting cubes.  It exists both as a GUI interface, and a
;    scriptable back-end to the cube construction and spectral
;    extraction algorithms.
;
; INHERITANCE TREE:
;
;    ObjReport
;             \
;     ObjMsg-->CubeProj
;
; EXAMPLE:
;
;    a=obj_new('CubeProj')
;    a->Show
;    
; MODIFICATION HISTORY:
;
;    2003-12-05 (J.D. Smith): Position based-offsets and realistic
;      header parsing.
;
;    2002-12-06 (J.D. Smith): Communication with CubeView, per-BCD
;      accounting for speed and flexibility, enable/disable BCD
;      records without re-clipping the accounts, list-based pixel
;      accounting and reverse indexing.
;
;    2002-09-04 (J.D. Smith): Initially written, based on SCOREProj.
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2002-2006 J.D. Smith
;
;  This file is part of CUBISM.
;
;  CUBISM is free software; you can redistribute it and/or modify it
;  under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2, or (at your option)
;  any later version.
;  
;  CUBISM is distributed in the hope that it will be useful, but
;  WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;  General Public License for more details.
;  
;  You should have received a copy of the GNU General Public License
;  along with CUBISM; see the file COPYING.  If not, write to the Free
;  Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;  Boston, MA 02110-1301, USA.
;
;##############################################################################

;;=================  Interface ===============================================

pro CubeProj_show_event, ev
  widget_control, ev.top, get_uvalue=self
  self->ShowEvent,ev
end

;=============================================================================
;  ShowEvent - Handle events from the show interface, or dispatch to
;              the appropriate method
;=============================================================================
pro CubeProj::ShowEvent, ev
  if ~self.debug then begin     ;; trap errors unless debugging
     catch, err
     if err ne 0 then begin
        if strpos(!ERROR_STATE.MSG,'OBJREPORT-ERROR') eq -1 then $ 
           self->Error,!ERROR_STATE.MSG ;; if just a trap, skip it
        catch,/CANCEL
        return
     endif 
  endif 
  event_type=tag_names(ev,/STRUCTURE_NAME)
  if event_type eq 'WIDGET_BASE' then begin ;size
     new=ev.Y+(*self.wInfo).list_size_diff
     widget_control, (*self.wInfo).SList,  SCR_YSIZE=new
     geom=widget_info(ev.top,/GEOM)
     if geom.xsize ne geom.scr_xsize then begin 
        widget_control, ev.top,XSIZE=0 ;takes two changes to 'take'
        widget_control, ev.top,SCR_XSIZE=(*self.wInfo).xsize
     endif 
     ;; Workaround v6.1 resize offset bug:
     widget_control, ev.top, TLB_GET_OFFSET = offset
     widget_control, ev.top,TLB_SET_XOFFSET = offset[0], $
                     TLB_SET_YOFFSET = offset[1]
     return
  endif else if event_type eq 'WIDGET_TIMER' then begin 
     self->GetProperty,REPORT_STATUS=status
     ;; See if the startup message is still there
     if byte(strmid(status,0,1)) eq 164 then $
        self->Status,/CLEAR
     return
  endif 
  sel=widget_info((*self.wInfo).SList,/LIST_SELECT)
  if sel[0] eq -1 then n_sel=0 else n_sel=n_elements(sel)
  nr=self->N_Records()
  if ev.id eq (*self.wInfo).SList then begin ;somebody clicked
     self->UpdateButtons
     if nr eq 0 then return     ;just the blank nothing line
     if ev.clicks eq 2 then begin 
        self->SetListSelect,sel[0],/NO_PRESERVE_TOP
        action='viewrecord' 
     endif else begin 
        if n_sel eq 1 then begin 
           self->GetProperty,REPORT_STATUS=status
           if status then self->Status,/CLEAR
           self->Send,/SELECT,SINGLE_SELECT=sel[0] 
        end else begin 
           self->Status,string(FORMAT='(I0," records selected")',n_sel)
           self->Send,/SELECT
        endelse 
        return 
     endelse 
  endif else if event_type eq 'WIDGET_KILL_REQUEST' then action='Exit' $
  else widget_control, ev.id, get_uvalue=action
  if action eq 'bargroup' then action=ev.value
  
  widget_control, /HOURGLASS
  case action of
     'save-as': self->Save,/AS
     
     'viewuncertainty': self->ViewRecord,/UNCERTAINTY
     'viewuncertainty-new': self->ViewRecord,/UNCERTAINTY,/NEW_VIEWER
     'viewrecord-new': self->ViewRecord,/NEW_VIEWER
     'visaors-new': self->VisualizeAORs,/NEW_VIEWER
     'loadvisimage': self->LoadVisualize,/SELECT
     
     'viewbackground-new': self->ViewBackground,/NEW_VIEWER
     'remove-background': begin 
        ptr_free,self.BACKGROUND,self.BACKGROUND_UNC
        if self.BACK_RECS eq self.AS_BUILT.BACK_RECS then $
           self.BACK_RECS=ptr_new() else heap_free,self.BACK_RECS 
        self.BACK_DATE=0.0D
        self->SetDirty,/SAVE,/BACKGROUND
        self->UpdateButtons & self->UpdateTitle
     end 
     'setbackgroundfromrecsa': self->SetBackgroundFromRecs,BLEND=0
     'setbackgroundfromrecsb': self->SetBackgroundFromRecs,BLEND=1
     'viewbackgrounda': self->ViewBackground,BLEND=0
     'viewbackgroundb': self->ViewBackground,BLEND=1
     
     'remove-bg-sp': begin 
        ptr_free,self.BG_SP
        self.BG_SP_FILE=''
        self->SetDirty
        self->UpdateButtons & self->UpdateTitle
     end 
     'loadcalib': self->LoadCalib,/SELECT
     'load-append-badpixels': self->LoadBadPixels,/APPEND
     'feedback': begin 
        self->SetProperty,FEEDBACK=1b-self.feedback
        widget_control, ev.id, SET_BUTTON=self.feedback
     end
     'fluxcon': begin 
        self->SetProperty,FLUXCON=1b-self.fluxcon
        widget_control, ev.id, SET_BUTTON=self.fluxcon
     end
     'slcf': begin 
        self->SetProperty,SLCF=1b-self.slcf
        widget_control, ev.id, SET_BUTTON=self.slcf
     end
     'use_bg': begin 
        self->SetProperty,USE_BACKGROUND=1b-self.use_bg
        widget_control, ev.id, SET_BUTTON=self.use_bg
     end 
     'use_unc': begin 
        self->SetProperty,USE_UNCERTAINTY=1b-self.use_unc
        widget_control, ev.id, SET_BUTTON=self.use_unc
     end
     'load_masks': begin 
        self->SetProperty,LOAD_MASKS=1b-self.load_masks
        widget_control, ev.id, SET_BUTTON=self.load_masks
     end
     'load_unc': begin 
        self->SetProperty,LOAD_UNCERTAINTY=1b-self.load_unc
        widget_control, ev.id, SET_BUTTON=self.load_unc
     end
     'wavecut': begin 
        self->SetProperty,WAVECUT=1b-self.wavecut
        widget_control, ev.id, SET_BUTTON=self.wavecut
     end
     'reconstructed': begin 
        self->SetProperty,RECONSTRUCTED_POSITIONS=1b-self.reconstructed_pos
        widget_control, ev.id, SET_BUTTON=self.reconstructed_pos
     end
     'save-with-data': begin 
        self->SetProperty,SAVE_DATA=1b-logical_true(self.SaveMethod AND 1b)
        widget_control, ev.id, SET_BUTTON=self.SaveMethod AND 1b
     end 
     'relative-file-names': begin 
        self->SetProperty, $
           RELATIVE_FILENAMES=1b-logical_true(self.SaveMethod AND 4b)
        widget_control, ev.id,SET_BUTTON=logical_true(self.SaveMethod AND 4b)
     end 
        
     'save-with-accounts': begin 
        self->SetProperty,SAVE_ACCOUNTS=1b-logical_true(self.SaveMethod AND 2b)
        widget_control, ev.id, SET_BUTTON=logical_true(self.SaveMethod AND 2b)
     end 
     
     'add-droop-aor':     self->AddGroup,/DROOPRES
     'add-flatap-aor':    self->AddGroup,/FLATAP
     'add-module':        self->AddGroup,/MODULE_ONLY
     'add-droop-module':  self->AddGroup,/DROOPRES,/MODULE_ONLY
     'add-flatap-module': self->AddGroup,/FLATAP,/MODULE_ONLY
     
     'setorder': begin 
        self->GetProperty,CALIB=cal
        ords=strtrim(cal->Orders(self.MODULE),2)
        if self.MODULE eq 'SH' or self.MODULE eq 'LH' then ords=['All',ords]
        ord=popup('Choose Cube Build Order',ords,TITLE='Cube Build Order', $
                  PARENT_GROUP=self->TopBase(),/MODAL, $
                  SELECT=self.ORDER eq 0?0:where(ords eq $
                                                 strtrim(self.ORDER,2)))
        if ord eq 'All' then ord=0 else ord=long(ord)
        self->SetProperty,ORDER=ord
     end
     
     'viewcube-new': self->ViewCube,/NEW
          
     'select-all': begin        ;select all records
        self->SetListSelect,/ALL
        self->UpdateButtons
     end
     
     'select-by-filename': begin 
        pat=getinp('File Matching (regex):',(*self.DR)[0].FILE, $
                   TITLE='Select by File', $
                   PARENT_GROUP=self->TopBase(),/MODAL,XSIZE=40)
        if pat then begin 
           wh=where(stregex((*self.DR).FILE,pat,/BOOLEAN),cnt)
           if cnt eq 0 then self->Error,'No files matched'
           self->SetListSelect,wh
           self->UpdateButtons
        endif 
     end 
     
     'select-by-keyword': begin 
        keys=strtrim(strmid(*(*self.DR)[0].HEADER,0,8),2)
        wh=where(keys,good)
        if good eq 0 then return
        keys=keys[wh]
        ret=popup('Keyword',keys, ENTRY='Match (regex):', $
                    TITLE='Select File By Keyword', $
                  PARENT_GROUP=self->TopBase(),MODAL=0,/COMBOBOX)
        if ~ret.text then return
        hlist=(*self.DR).HEADER
        outlist=strarr(n_elements(hlist))
        for j=0,n_elements(hlist)-1 do begin 
           keys=strtrim(strmid(*hlist[j],0,8),2)
           wh=where(keys eq ret.select,cnt)
           outlist[j]=strtrim(strmid((*hlist[j])[wh[0]],10,20),2)
        endfor 
        wh=where(stregex(outlist,ret.text,/BOOLEAN),cnt)
        if cnt eq 0 then self->Error,'No files matched'
        self->SetListSelect,wh
        self->UpdateButtons
     end 
     
     'invert-select': begin 
        self->SetListSelect,/INVERT
        self->UpdateButtons
     end 
     
     'deselect-disabled': begin 
        self->SetListSelect,/NO_DISABLED
        self->UpdateButtons
     end 
     
     'filenames': begin 
        if sel[0] eq -1 then return
        if n_elements(sel) gt 1 then $
           strlist=string(FORMAT='(A," : ",A)', $
                          [transpose((*self.DR)[sel].ID), $
                           transpose((*self.DR)[sel].file)]) $
        else strlist=string(FORMAT='(A," : ",A)', $
                            (*self.DR)[sel].ID,(*self.DR)[sel].file)
        self->Info,strlist, $
                   TITLE='Filename Listing: '+self->ProjectName(),/SCROLL
     end 
     
     'headers':   $
        begin 
        if sel[0] eq -1 then return
        if NOT array_equal(ptr_valid((*self.DR).HEADER),1) then self->Error, $
           ['No header available for records (try restore):', $
            (*self.DR)[sel].ID]
        xpopdiag,(*self.DR)[sel].ID, (*self.DR)[sel].HEADER, $
                 BUTTON_TEXT='  Done  ', $
                 LABEL='File:', PARENT_GROUP=self->TopBase(), $
                 TITLE=self->ProjectName()+': Header Info', $
                 TSIZE=[65,15],TOP_LAB=self->ProjectName()
     end 
     
     'header-keyword': $
        begin 
        self->RestoreData,sel
        if sel[0] eq -1 then return
        ;; The first header of the first selected is used
        keys=strmid(*(*self.DR)[sel[0]].HEADER,0,8)
        which=popup('Choose Keyword',keys,TITLE='HEADER KEYWORDS', $
                    PARENT_GROUP=self->TopBase(),/MODAL,/COMBOBOX)
        hlist=(*self.DR)[sel].HEADER
        for j=0,n_elements(hlist)-1 do begin
           key=sxpar(*hlist[j],which)
           switch size(key,/TNAME) of
              'BYTE': begin
                 form='A' & key=key?'TRUE':'FALSE'
                 break
              end
              'STRING': begin 
                 key=strtrim(key,2) & form='A'
                 break
              end
              'LONG': begin 
                 form='I0'
                 break
              end
              'DOUBLE':
              'FLOAT': form='G0.7'
           endswitch

           str=string(FORMAT='(A,": ",'+form+')',(*self.DR)[sel[j]].ID,key) 
           if n_elements(strlist) eq 0 then strlist=[str] else  $
              strlist=[strlist,str]
        endfor 
        which=strtrim(which,2)
        self->Info,[which+':',strlist],TITLE=which+' Keyword Listing',/SCROLL
     end 
     
     'switchlist': begin 
        widget_control, ev.id,get_value=val
        widget_control, ev.top, UPDATE=0
        val=val eq "<"?">":"<"
        which=val eq "<"?1:0
        widget_control, (*self.wInfo).wHead[1-which],MAP=0
        widget_control, (*self.wInfo).wHead[which],/MAP
        (*self.wInfo).which_list=which
        widget_control, ev.id,set_value=val
        self->UpdateColumnHeads
        self->UpdateList
        widget_control, ev.top, /UPDATE
     end
     
     'sort': begin 
        widget_control,ev.id,get_value=v
        flags=bytarr(n_elements(v))
        self.sort=ev.value
        flags[ev.value-(6*(ev.value ge 6))]=1b
        widget_control, (*self.wInfo).Base,UPDATE=0
        widget_control,ev.id,SET_VALUE=flags
        self->UpdateList
        widget_control, (*self.wInfo).Base,/UPDATE
     end 
     
     'clear-rec-badpix': self->ClearBadPixels,/CLEAR_RECORDS
     'clear-all-badpix': begin 
         self->ClearBadPixels,/NO_UPDATE
         self->ClearBadPixels,/CLEAR_RECORDS,/ALL
     end 
     
     'autobadpixels-rec': self->AutoBadPixels,/RECORD_BAD_PIX
     
     'phist': self->Info,self->Info(/NO_DATA), $
                         TITLE=self->ProjectName()+': Parameters'
     
     'pasbuilt': self->Info,self->Info(/NO_DATA,/AS_BUILT), $
                         TITLE=self->ProjectName()+': As-Built Parameters'
     
     'calset': begin 
        self->LoadCalib
        info=ptrarr(4) & modules=strarr(4)
        for md=0,3 do begin 
           modules[md]=irs_module(md,/TO_NAME)
           info[md]=ptr_new(self.cal->Info(md))
        endfor  
        xpopdiag,modules,info,PARENT_GROUP=self->TopBase(), $
                 BUTTON_TEXT='  Done  ',LABEL='Module:', $
                 TITLE=self->ProjectName()+': Calibration Set <'+ $
                 file_basename(self.cal_file)+'>',TSIZE=[94,15], $
                 DEFAULT=irs_module(self.MODULE),/FREE_POINTERS
     end
     
     'about': begin 
        @cubism_version
        if self.version ne '' AND cubism_version ne self.version then begin 
           thiscube='(Curr. Cube: '+self.version+')'
           spaces=35-strlen(thiscube)
           if spaces gt 1 then begin 
              s=spaces/2>1
              thiscube=strjoin(replicate(' ',s))+thiscube+ $
                       strjoin(replicate(' ',spaces-s))
           endif 
        end else thiscube=''
        title='CUBISM '+cubism_version
        tlen=strlen(title) & left=(35-tlen)/2
        title=strjoin(replicate(' ',left>0)) + title
        self->Info,TITLE='About Cubism', $
                   ['***********************************', $
                    title,                                    $
                    thiscube,                                 $
                    '                                   ', $
                    '    JD Smith and the SINGS Team    ', $
                    '         (c) 2002-2007             ', $
                    '   http://sings.stsci.edu/cubism   ', $
                    '***********************************']
     end
     
     'manual': begin 
        @cubism_dir
        manual=filepath(ROOT=cubism_dir,SUBDIR='manual','cubism.pdf')
        if ~file_test(manual,/READ) then $
           self->Error,'Cannot locate Cubism manual.'
        online_help,BOOK=manual,/FULL_PATH
     end 
     
     'debug': begin 
        self->SetProperty,DEBUG=1b-self.debug
        widget_control, ev.id, SET_BUTTON=self.debug
     end    
     
     else: call_method,action,self ;all others, just call the named method
  endcase     
end

;=============================================================================
;  Exit - Get out of here, possibly committing hara kiri on the way.
;=============================================================================
pro CubeProj::Exit
  if self->Dirty() then begin 
     status=self.Spawned?"(changes will be lost)": $
            "(project available on command line)"
     ans=dialog_message(["Save changes to project "+self->ProjectName()+"?", $
                         self.SaveFile?"to file "+self.SaveFile:"",status], $
                        /QUESTION,TITLE="Save "+self->ProjectName()+"?", $
                        DIALOG_PARENT=self->TopBase(),/CANCEL,/DEFAULT_CANCEL)
     case ans of
        'Cancel': canceled=1
        'Yes': self->Save,CANCELED=canceled
        else:
     endcase
     if keyword_set(canceled) then begin 
        if ~widget_info((*self.wInfo).SList,/VALID_ID) then begin
           self->KillShow
           self->Show
        endif 
        return
     endif else self->KillShow
  endif 
  if self.Spawned then obj_destroy,self else self->KillShow
end
     
;=============================================================================
;  Show - Run the project interface
;=============================================================================
pro CubeProj::Show,FORCE=force,SET_NEW_PROJECTNAME=spn,_EXTRA=e
  if ~keyword_set(force) then if self->Showing() then return

  ;; make the info structure if we need it.
  if ~ptr_valid(self.wInfo) then self.wInfo=ptr_new({CubeProj_wInfo})
  
  self.feedback=1               ;default to showing cube build feedback
  
  base=widget_base(/COLUMN,/BASE_ALIGN_RIGHT,APP_MBAR=mbar,SPACE=1, $
                   /TLB_SIZE_EVENTS,/TLB_KILL_REQUEST_EVENTS,_EXTRA=e)
  (*self.wInfo).Base=base
  self->UpdateTitle
  
  ;; Populate the menu-bar
  
  ;;*** File menu
  file=widget_button(mbar,VALUE='File',/MENU)
  b1=widget_button(file,VALUE='New...',UVALUE='newproject') 
  b1=widget_button(file,VALUE='Open...',UVALUE='open')
  sm=widget_button(file,VALUE='Save Setup',/MENU)
  b2=widget_button(sm,VALUE='Save Data with Project', $
                   UVALUE='save-with-data', /CHECKED_MENU)
  widget_control, b2,SET_BUTTON=self.SaveMethod AND 1b
  
  b2=widget_button(sm,VALUE='Relative File Names', $
                   UVALUE='relative-file-names', $
                   /CHECKED_MENU)
  widget_control, b2, SET_BUTTON=logical_true(self.SaveMethod AND 4b)
  (*self.wInfo).MUST_NO_DATA=b2
  
  b2=widget_button(sm,VALUE='Save Clip Accounts with Project', $
                   UVALUE='save-with-accounts', /CHECKED_MENU)
  widget_control, b2,SET_BUTTON=logical_true(self.SaveMethod AND 2b)
  wMustAcct=b2
  
  b1=widget_button(file,VALUE='Save',UVALUE='save')
  b1=widget_button(file,VALUE='Save As...',UVALUE='save-as')
  
  (*self.wInfo).MUST_SAVE_CHANGED= $
     widget_button(file,VALUE='Revert To Saved...',uvalue='revert')
  ;;-------------  
  wMustCube=widget_button(file,VALUE='Write FITS Cube...',$
                          UVALUE='savecube',/SEPARATOR)
  ;;-------------
  b1=widget_button(file,VALUE='Rename Project...',uvalue='setprojectname', $
                   /SEPARATOR)
  b1=widget_button(file,VALUE='Export to Command Line...', $
                   UVALUE='ExportToMain',SENSITIVE=~LMGR(/VM,/RUNTIME))  
;   (*self.wInfo).MUST_UNRESTORED=  $
;      widget_button(file,value='Restore All Data',uvalue='restoreall', $
;                   /SEPARATOR) 
  ;;-------------
  b1=widget_button(file,VALUE='Load New Calibration Set...', $
                   UVALUE='loadcalib', /SEPARATOR)
  b1=widget_button(file,VALUE='Close',UVALUE='exit',/SEPARATOR)
  
  ;;*** Edit menu
  edit=widget_button(mbar,VALUE='Edit',/MENU)
  (*self.wInfo).MUST_PROJ[0]=widget_button(edit,VALUE='Select All', $
                                           UVALUE='select-all')
  (*self.wInfo).MUST_PROJ[1]=widget_button(edit,VALUE='Select By Filename',$
                                           UVALUE='select-by-filename', $
                                           /SEPARATOR)
  (*self.wInfo).MUST_PROJ[2]=widget_button(edit,VALUE='Select By Keyword',$
                                           UVALUE='select-by-keyword')
  (*self.wInfo).MUST_PROJ[3]=widget_button(edit,VALUE='Invert Selection',$
                                           UVALUE='invert-select',/SEPARATOR)
  (*self.wInfo).MUST_PROJ[4]=widget_button(edit,VALUE='Deselect Disabled', $
                                           UVALUE='deselect-disabled')
  
  wMustSel=widget_button(edit,VALUE='Replace File Substring...', $
                         UVALUE='replacefilenamesubstring',/SEPARATOR)
  
  ;;*** Data Record menu
  rec=widget_button(mbar,VALUE='Record',/MENU)
  b1=widget_button(rec,VALUE='Add Data...',UVALUE='adddata')
  b2=widget_button(rec,VALUE='Import Data from Mapping AOR',/MENU)
  tmp=widget_button(b2,VALUE='BCD...',UVALUE='addgroup')
  tmp=widget_button(b2,VALUE='DroopRes...',UVALUE='add-droop-aor')
  tmp=widget_button(b2,VALUE='FlatAp...',UVALUE='add-flatap-aor')
  b3=widget_button(rec,VALUE='Import Data by Module',/MENU)
  tmp=widget_button(b3,VALUE='BCD...',UVALUE='add-module')
  tmp=widget_button(b3,VALUE='DroopRes...',UVALUE='add-droop-module')
  tmp=widget_button(b3,VALUE='FlatAp...',UVALUE='add-flatap-module')
  
  b1=widget_button(rec,VALUE='Load Record Masks',UVALUE='load_masks', $
                   /CHECKED_MENU)
  widget_control, b1,SET_BUTTON=self.load_masks
  b1=widget_button(rec,VALUE='Load Record Uncertainties',UVALUE='load_unc', $
                   /CHECKED_MENU)
  widget_control, b1,SET_BUTTON=self.load_unc
  
  wMustSel=[wMustSel, $
            widget_button(rec,VALUE='Switch Record Data Type...', $
                          UVALUE='switchrecorddatatype')]
  
  ;;-------------  
  (*self.wInfo).MUST_UNRESTORED=widget_button(rec, UVALUE='restoreall',$
                                              VALUE='Restore All Record Data',$
                                              /SEPARATOR)  
  wMustSel=[wMustSel, $
            ;;-------------
            ((*self.wInfo).view_ids[0:1]= $
             [widget_button(rec,VALUE='View Record...',UVALUE='viewrecord', $
                            /SEPARATOR),$
              widget_button(rec,VALUE='View Record (new viewer)...', $
                            UVALUE='viewrecord-new')])]
  
  ;;-------------
  (*self.wInfo).MUST_REC_UNC= $
     [widget_button(rec,VALUE='View Uncertainty...', $
                    UVALUE='viewuncertainty',/SEPARATOR),$
      widget_button(rec,VALUE='View Uncertainty (new viewer)...', $
                    UVALUE='viewuncertainty-new')]
  
  wMustSel=[wMustSel, $
            ;;-------------
            widget_button(rec,VALUE='Delete',UVALUE='removebcd',/SEPARATOR),$
            widget_button(rec,VALUE='Rename',UVALUE='renamerecord'),$
            widget_button(rec,VALUE='Disable',UVALUE='disablerecord'),$
            widget_button(rec,VALUE='Enable',UVALUE='enablerecord'),$
            ;;-------------
            widget_button(rec,VALUE='Show Filenames...',UVALUE='filenames', $
                         /SEPARATOR),$
            widget_button(rec,VALUE='Show Header...',UVALUE='headers'),$
            widget_button(rec,VALUE='Show Keyword Value(s)...', $
                          UVALUE='header-keyword')]
  
   ;;-------------
  b1=widget_button(rec,VALUE='Visualize AORs...',UVALUE='VisualizeAORs', $
                   /SEPARATOR)
  b1=widget_button(rec,VALUE='Visualize AORs (new viewer)...', $
                   UVALUE='visaors-new')
  (*self.wInfo).MUST_VIS= $
     widget_button(rec,VALUE='Load New Visualization Image...', $
                   UVALUE='loadvisimage')
  
  ;;*** Cube menu  
  cube=widget_button(mbar,VALUE='Cube',/MENU)
  (*self.wInfo).build_ids[0]= $
     ((*self.wInfo).MUST_PROJ[5]= $
      widget_button(cube,VALUE='Build Cube',UVALUE='buildcube'))

  wMustAcct= [wMustAcct, $
              widget_button(cube,VALUE='Reset Accounts',UVALUE='resetaccounts')]
  ;;-------------
  wMustCube=[wMustCube, $
             widget_button(cube,VALUE='View Cube...',UVALUE='viewcube', $
                           /SEPARATOR) , $
             widget_button(cube,VALUE='View Cube (new viewer)...', $
                           UVALUE='viewcube-new')]
  
  ;;-------------
  b1=widget_button(cube,VALUE='Show Cube Build Feedback',UVALUE='feedback', $
                   /CHECKED_MENU,/SEPARATOR) 
  widget_control, b1, SET_BUTTON=self.feedback
  ;;-------------
  b1=widget_button(cube,VALUE='Build Cube with FLUXCON',UVALUE='fluxcon', $
                   /CHECKED_MENU,/SEPARATOR)
  widget_control, b1,SET_BUTTON=self.fluxcon
  
  b1=widget_button(cube,VALUE='Build Cube with SLCF',UVALUE='slcf', $
                   /CHECKED_MENU)
  widget_control, b1,SET_BUTTON=self.slcf  
  
  (*self.wInfo).MUST_ANY_BACK=widget_button(cube,VALUE='Subtract Background', $
                                            uvalue='use_bg',/CHECKED_MENU)
  widget_control, (*self.wInfo).MUST_ANY_BACK,SET_BUTTON=self.use_bg
  
  ;;-------------
  b1=widget_button(cube,VALUE='Trim Wavelengths',UVALUE='wavecut', $
                   /CHECKED_MENU,/SEPARATOR)
  widget_control, b1,SET_BUTTON=self.wavecut
  b1=widget_button(cube,VALUE='Use Reconstructed Positions', $
                   UVALUE='reconstructed', /CHECKED_MENU)
  widget_control, b1,SET_BUTTON=self.reconstructed_pos
  (*self.wInfo).MUST_UNCERTAINTIES= $
     widget_button(cube,VALUE='Build Uncertainty Cube',UVALUE='use_unc', $
                   /CHECKED_MENU)
  widget_control, (*self.wInfo).MUST_UNCERTAINTIES,SET_BUTTON=self.use_unc
  
  ;;-------------
  (*self.wInfo).MUST_MODULE= $
     widget_button(cube,value='Set Cube Build Order...',UVALUE='setorder', $
                   /SEPARATOR)
  (*self.wInfo).MUST_CAL=widget_button(cube,VALUE='Aperture(s)...', $
                                       UVALUE='showaperture')
  
  ;;*** Background menu    
  background=widget_button(mbar,VALUE='Background',/MENU)
  (*self.wInfo).background_menu=background
  wMustSel=[wMustSel,$
            widget_button(background,VALUE='Set Background from Rec(s)...', $
                          UVALUE='setbackgroundfromrecs')]
  but=widget_button(background,VALUE='Load Background Rec(s)...', $
                    UVALUE='loadbackgroundlist')
  
  bcomb=widget_button(background,VALUE='Background Blend',/MENU)
  wMustSel=[wMustSel, $
            widget_button(bcomb,VALUE='Set and Scale Background A...', $
                          UVALUE='setbackgroundfromrecsa'), $
            widget_button(bcomb,VALUE='Set and Scale Background B...', $
                          UVALUE='setbackgroundfromrecsb')]
  (*self.wInfo).MUST_BACK_AB= $
     widget_button(bcomb,VALUE='Blend A and B Backgrounds...', $
                   UVALUE='blendbackgrounds')
  ;;-------------
  (*self.wInfo).MUST_BACK_A=widget_button(bcomb,VALUE='View Background A...', $
                                          UVALUE='viewbackgrounda',/SEPARATOR)
  (*self.wInfo).MUST_BACK_B=widget_button(bcomb,VALUE='View Background B...', $
                                          UVALUE='viewbackgroundb')
  
  ;;-------------
  (*self.wInfo).MUST_BACK= $
     [widget_button(background,VALUE='Save Background Rec(s)...', $
                    UVALUE='savebackgroundlist',/SEPARATOR), $
      widget_button(background,VALUE='View Background...', $
                    UVALUE='viewbackground'), $
      widget_button(background,VALUE='View Background (new viewer)..', $
                    UVALUE='viewbackground-new'), $
      widget_button(background,VALUE='Remove Background', $
                    UVALUE='remove-background',/SEPARATOR), $
      widget_button(background,Value='Rebuild Background', $
                    UVALUE='RebuildBackground')]
  ;;-------------
  but=widget_button(background,VALUE='Load Background Spectrum...', $
                    /SEPARATOR, UVALUE='readbackgroundfromfile')
  (*self.wInfo).MUST_BG_SP= $
     widget_button(background,VALUE='Remove Background Spectrum', $
                   UVALUE='remove-bg-sp')
  
  
  ;;*** BadPix menu  
  badpix=widget_button(mbar,VALUE='BadPix',/MENU) 
  
  but=widget_button(badpix,VALUE='Load Bad Pixels...',UVALUE='loadbadpixels')
  but=widget_button(badpix,VALUE='Load and Append Bad Pixels...', $
                    UVALUE='load-append-badpixels')
  wMustAnyBPL=widget_button(badpix,VALUE='Save Bad Pixels...', $
                            UVALUE='savebadpixels')
  
  (*self.wInfo).MUST_GLOBAL_BPL= $
     widget_button(badpix,VALUE='Clear Global Bad Pixels', $
                    UVALUE='clearbadpixels',/SEPARATOR)
  (*self.wInfo).MUST_REC_BPL= $
      widget_button(badpix,VALUE="Clear Record Bad Pixels", $
                    UVALUE='clear-rec-badpix')
  (*self.wInfo).MUST_ANY_BPL= $
    [wMustAnyBPL, $
     widget_button(badpix,VALUE="Clear All Bad Pixels", $
                   UVALUE='clear-all-badpix')]
    
  (*self.wInfo).MUST_ACCT= $
    [wMustAcct, $
     widget_button(badpix,VALUE='Auto-Gen Global Bad Pixels...', $
                   UVALUE='autobadpixels',/SEPARATOR), $
     widget_button(badpix,VALUE='Auto-Gen Record Bad Pixels...', $
                   UVALUE='autobadpixels-rec')]
  
  ;;*** Info menu
  info=widget_button(mbar,VALUE='Info',/MENU)
  b1=widget_button(info,VALUE='Project Parameters...',UVALUE='phist')
  b2=widget_button(info,VALUE='As-Built Parameters...',UVALUE='pasbuilt')
  (*self.wInfo).MUST_CAL_FILE= $
     widget_button(info,VALUE='Calibration Set Details...',UVALUE='calset')
  ;;-------------
  b1=widget_button(info,VALUE='Debug Cubism',UVALUE='debug',/SEPARATOR, $
                   /CHECKED_MENU)
  widget_control, b1,SET_BUTTON=self.debug
  if lmgr(/VM,/RUNTIME) then widget_control, b1,SENSITIVE=0
  
  
  ;;*** Help menu
  help=widget_button(mbar,VALUE='Help',/MENU,/HELP)
  b1=widget_button(help,VALUE='About Cubism...',UVALUE='about')
  b1=widget_button(help,VALUE='Cubism Manual...',UVALUE='manual')
  
  b=widget_base(base,/COLUMN,/BASE_ALIGN_LEFT,SPACE=1,YPAD=0,XPAD=0)
  headbase=widget_base(b,/ROW,XPAD=0,YPAD=0,/FRAME,SPACE=1)
  headmap=widget_base(headbase)
  (*self.wInfo).wHead[0]= $
     cw_bgroup(/NONEXCLUSIVE,headmap, $
               ['ID               ', $
                'Exp   ', $
                'Observed       ', $
                'Added      ', $
                'Type ',$
                'Step '],UVALUE='sort',/ROW)
  (*self.wInfo).wHead[1]= $  
     cw_bgroup(/NONEXCLUSIVE,headmap, $
               ['Target            ', $
                'RA     ', $
                'Dec   ', $
                'DATA',$
                'UNC', $
                'BMSK',$
                'ACCT', $
                'BPL'], $
               BUTTON_UVALUE=6+indgen(8),UVALUE='sort',/ROW,MAP=0)
  
  b1=widget_button(headbase,VALUE='>',UVALUE='switchlist')
  
  width=86
  
  (*self.wInfo).SList= $
     widget_list(b,/MULTIPLE,/ALIGN_LEFT, $
                 YSIZE=8>self->N_Records()<16,XSIZE=width,/FRAME)
  
  (*self.wInfo).Status=widget_label(b,/ALIGN_LEFT,/SUNKEN_FRAME)
  
  bar=cw_bgroup(base,IDS=ids,/ROW,UVALUE='bargroup',$
                ['Build Cube','Enable','Disable', $
                 'View Record','View Cube', 'Import AOR',' Save ',' Close '],$
                BUTTON_UVALUE= $
                ['buildcube','enablerecord','disablerecord', $
                 'viewrecord','viewcube', 'addgroup',  'save',  'exit'])
  ;; list of buttons that require a selection to be meaningful
  (*self.wInfo).MUST_SELECT=[wMustSel,ids[1:3]]
  (*self.wInfo).build_ids[1] = ((*self.wInfo).MUST_PROJ[6]=ids[0])
  (*self.wInfo).view_ids[2]=ids[3]
  
  ;; a cube must exist
  (*self.wInfo).MUST_CUBE=[wMustCube,ids[4]]
  
  widget_control, base,set_uvalue=self,/REALIZE
  
  geom=widget_info(base,/GEOMETRY) 
  widget_control, (*self.wInfo).Status, $
                  XSIZE=geom.XSIZE-2*geom.MARGIN-2*geom.XPAD-2
  geom=widget_info((*self.wInfo).SList,/GEOMETRY)
  bgeom=widget_info(base,/GEOMETRY)
  (*self.wInfo).list_row=(geom.SCR_YSIZE-7)/geom.YSIZE
  (*self.wInfo).list_size_diff=geom.SCR_YSIZE-bgeom.SCR_YSIZE
  (*self.wInfo).xsize=bgeom.SCR_XSIZE
  
  @cubism_version
  status=string(164b)+' CUBISM version '+cubism_version+ $
         ' by JD Smith and the SINGS team'
  self->Status,status
  
  name='CubeProj_Show:'+self.ProjectName+file_basename(self.savefile)
  name=strmid(name,0,127)
  XManager,name,(*self.wInfo).Base,/JUST_REG
  if keyword_set(spn) then $
     self->SetProjectName,TITLE='New Project Name'
  self->UpdateAll
  (*self.wInfo).showing=1
  widget_control, base,TIMER=45.
  XManager,name, base,/NO_BLOCK,EVENT_HANDLER='CubeProj_show_event',_EXTRA=e
end

;=============================================================================
;  KillShow - Kill the widget GUI if it's alive
;=============================================================================
pro CubeProj::KillShow
  if self->isWidget() && widget_info((*self.wInfo).SList,/VALID_ID) then $
     widget_control, (*self.wInfo).Base,KILL_NOTIFY='',/DESTROY
  if ptr_valid(self.wInfo) then begin
     device,WINDOW_STATE=win_valid
     if (*self.wInfo).feedback_window gt 0 && $
        win_valid[(*self.wInfo).feedback_window] then $
           wdelete,(*self.wInfo).feedback_window
     if (*self.wInfo).feedback_pixmap gt 0 && $
        win_valid[(*self.wInfo).feedback_pixmap] then $
           wdelete,(*self.wInfo).feedback_pixmap
     (*self.wInfo).showing=-1
  endif
end

;=============================================================================
;  NewProject - Create a new empty project and Show it
;=============================================================================
pro CubeProj::NewProject
  proj=obj_new(obj_class(self),name)
  proj->Show,/FORCE
  proj->SetProjectName,TITLE='New Project Name'
  proj->SetProperty,/SPAWNED
end

;=============================================================================
;  ProjectName - Return the project name
;=============================================================================
function CubeProj::ProjectName
  return,self.ProjectName?self.ProjectName:"(untitled)"
end

;=============================================================================
;  FileBaseName - Canonicalized basename
;=============================================================================
function CubeProj::FileBaseName
  return,strlowcase(idl_validname(self->ProjectName(),/CONVERT_ALL))
end

;=============================================================================
;  SetProjectName - Set the project name, with input.
;=============================================================================
pro CubeProj::SetProjectName,pn,TITLE=title
  if n_elements(pn) eq 0 then begin 
     if self->IsWidget() then begin 
        if n_elements(title) eq 0 then title='Rename Project'
        pn=getinp('Project Name:',self.ProjectName,TITLE=title, $
                  PARENT_GROUP=self->TopBase(),/MODAL,XSIZE=32)
        ;; Put the new name on the list, for XRegistered.
        if pn then AddManagedWidget,'CubeProj_Show:'+pn,self->TopBase()
     endif else self->Error,'Project name required'
  endif
  if pn then begin 
     self->SetProperty,PROJECTNAME=pn
     self->UpdateTitle
  endif 
end

;=============================================================================
;  SnapshotParameters - Save a snapshot of the as-built parameters
;=============================================================================
pro CubeProj::SnapshotParameters
  
  as_built={CUBE_PARAMETERS}
  struct_assign,self,as_built,/NOZERO
  
  ;; find locations where old and new share the same pointer
  for i=0,n_tags(as_built)-1 do begin 
     ;; XXX Assumes only pointer structure members contain heap data, but
     ;; deeper structures/objects/etc. could cause trouble here.
     if size(as_built.(i),/TYPE) ne 10 then continue
     ;; Free existing as_built if it doesn't share global pointer
     if ptr_valid(as_built.(i)) && as_built.(i) ne self.as_built.(i) then $
        heap_free,self.as_built.(i)
  endfor 
  
  self.as_built=as_built
end

;=============================================================================
;  Open - Open a Project and show it
;=============================================================================
pro CubeProj::Open,pname,PROJECT=proj,SUCCESS=success,_EXTRA=e
  if size(pname,/TYPE) ne 7 then begin 
     xf,pname,/RECENT,FILTERLIST=['*.cpj','*.fits','*.*','*'],$
        TITLE='Load Cube Project...',/NO_SHOW_ALL,SELECT=0, $
        /MODAL,PARENT_GROUP=self->TopBase(),_EXTRA=e
  endif 
  widget_control, /HOURGLASS
  if size(pname,/TYPE) ne 7 then return ;cancelled
  proj=self->Load(pname,type)
  success=obj_valid(proj)
  if ~success then return
  proj->SetProperty,/SPAWNED,CHANGED=0b
  if type eq 'fits' then proj->ViewCube,/NEW_VIEWER,/KILL_CUBE else $
     proj->Show,_EXTRA=e
end

;=============================================================================
;  Load - Load a Project from file
;=============================================================================
function CubeProj::Load,file,type,ERROR=err
  on_error,2
  catch, err
  if err ne 0 then begin 
     catch,/cancel
     if obj_valid(cube) then obj_destroy,cube
     self->Error,['Error loading project from '+file,!ERROR_STATE.MSG],$
                 /RETURN_ONLY
     return,-1
  endif
  if self->IsWidget() then widget_control,/HOURGLASS
  
  if stregex(file,'\.fits$',/BOOLEAN) then begin 
     type='fits'
     cube=obj_new('CubeProj')
     cube->LoadCubeFromFITS,file
  endif else begin 
     cube=cubeproj_load(file)
     type='cpj'
  endelse 
  return,cube
end



;=============================================================================
;  Initialize - We want to do this on Init or Load
;=============================================================================
pro CubeProj::Initialize
  self->SetProperty,REPORT_TITLE_BASE='CUBISM Project'
  self->MsgSetup,['CUBEPROJ_CUBE','CUBEPROJ_RECORD','CUBEPROJ_VISUALIZE', $
                  'CUBEPROJ_UPDATE', 'CUBEPROJ_SELECT', $ $
                  'CUBEPROJ_BADPIX_UPDATE', $
                  'CUBEPROJ_RECORD_UPDATE','CUBEPROJ_CALIB_UPDATE']
  
  ;; Check IDL version
  if !VERSION.RELEASE lt '6.1' then $
     self->Error,'CUBISM requires IDL version 6.1 or greater.'
  
  ;; Show will change this
  if ~self->IsWidget() then self->SetProperty,FEEDBACK=0  
  
  ;; Look for old-style saved Reverse Accounts: rebuild if necessary.
  if ptr_valid(self.DR) && ~self->Dirty(/ACCOUNTS) then begin 
     wh=where(ptr_valid((*self.DR).REV_ACCOUNT),cnt)
     if cnt gt 0 && array_equal((*self.DR)[wh].REV_WIDTH,0L) then begin 
        self->BuildRevAcct
        self->Warning,"Old reverse accounts found: rebuilt."
     endif 
  endif
  
  ;; Look for missing headers/DCEIDs, from older versions of the cubes.
  if ptr_valid(self.DR) then begin 
     wh=where(~ptr_valid((*self.DR).HEADER),nohdrcnt)
     if nohdrcnt gt 0 then self->RestoreData,wh
     wh=where((*self.DR).DCEID eq 0L,cnt)
     for i=0,cnt-1 do $
        (*self.DR)[wh[i]].DCEID=sxpar(*(*self.DR)[wh[i]].HEADER,'DCEID')
     if cnt gt 0 then self->Warning,"Missing DCEIDs: restored"
     wh=where(~(*self.DR).OBJECT,noobjectcnt)
     for i=0,noobjectcnt-1 do $
        (*self.DR)[wh[i]].OBJECT=sxpar(*(*self.DR)[wh[i]].HEADER,'OBJECT')
     if noobjectcnt gt 0 then $
        self->Warning,'Missing OBJECT tags: restored'
  endif 
  
  if lmgr(/VM,/RUNTIME) then self.debug=0b
  
  ;; Default recent cube build parameters for older versions of cubes
  ;; (e.g. if loaded from file)
  if self.version then begin 
     parts=stregex(self.version,'v([0-9.]+)',/SUBEXPR,/EXTRACT)
     version=float(parts[1])
     if version lt 0.95 then begin ;; Didn't know about IN_CUBE back then
        if ptr_valid(self.DR) then (*self.DR).IN_CUBE=~(*self.DR).DISABLED
     endif 
     if version lt 0.91 then self->SetProperty,OVERSAMPLE_FACTOR=1.0
     if version lt 0.89 then self->SetProperty,/LOAD_MASKS
     if version lt 0.87 then self->SetProperty,/USE_BACKGROUND
     if version lt 0.82 then self->SetProperty,/RECONSTRUCTED_POSITIONS
  endif 
end

;=============================================================================
;  Save - Save a Project to file
;=============================================================================
pro CubeProj::Save,file,AS=as,CANCELED=canceled,COMPRESS=comp,NO_DATA=nodata, $
                   NO_ACCOUNT=noacc, RELATIVE_FILENAMES=relfile
  if (size(file,/TYPE) ne 7 AND self.SaveFile eq '') OR keyword_set(as) $
     then begin 
     if self.SaveFile then start=self.SaveFile else begin 
        start=self->FileBaseName()+".cpj"
     endelse 
     xf,file,/RECENT,FILTERLIST=['*.cpj','*.*','*'],/SAVEFILE, $
        TITLE='Save Cube Project As...',/NO_SHOW_ALL,SELECT=0, $
        START=start,PARENT_GROUP=self->TopBase(),/MODAL
     if size(file,/TYPE) ne 7 then begin
        canceled=1
        return                
     endif 
  endif else if size(file,/TYPE) ne 7 then file=self.SaveFile
  if self->IsWidget() then widget_control, /HOURGLASS
  
  ;; Check default save method
  if n_elements(nodata) eq 0 then nodata=~(self.SaveMethod AND 1b)
  if n_elements(noacc) eq 0 then noacc=~(self.SaveMethod AND 2b)
  if n_elements(relfile) eq 0 then relfile=logical_true(self.SaveMethod AND 4b)
  
  status='Saving project to '+file_basename(file)+ $
         ' ('+(nodata?'no data,':'with data,') + $
         (noacc?' no accts':' with accts') + $
         (relfile?', rel names)':')') + '...'
  self->Status,status
  ;; Detach the stuff we don't want to save!
  detMsgList=self.MsgList & self.MsgList=ptr_new() ;or the message list
  detCal=self.cal & self.cal=obj_new() ;or the calibration object
  detabpl_pix=self.AUTO_BPL.BCD_PIX & self.AUTO_BPL.BCD_PIX=ptr_new()
  detabpl_vals=self.AUTO_BPL.BCD_VALS & self.AUTO_BPL.BCD_VALS=ptr_new()
  detabpl_id=self.AUTO_BPL.DCEID & self.AUTO_BPL.DCEID=ptr_new()
  detabpl_cnt=self.AUTO_BPL.CNT_VEC & self.AUTO_BPL.CNT_VEC=ptr_new()
  detabpl_unc=self.AUTO_BPL.BCD_UNC & self.AUTO_BPL.BCD_UNC=ptr_new()
  
  nr=self->N_Records()
  
  if relfile then root=file_dirname(file_expand_path(file))
  if ptr_valid(self.DR) then begin 
     ;; Canonicalize the filenames
     if self.savefile then cd,file_dirname(self.savefile)
     for i=0,nr-1 do begin 
        (*self.DR)[i].file=file_expand_path((*self.DR)[i].file)
        if relfile then $
           (*self.DR)[i].file=make_filename_relative((*self.DR)[i].file,root)
     endfor 
     
     stub=ptrarr(nr)
     detRevAccts=(*self.DR).REV_ACCOUNT ;never save reverse accounts
     detRevBcdAccts=(*self.DR).REV_BCD_ACCOUNT ;never save reverse accounts
     detRevDual=(*self.DR).REV_DUAL
     (*self.DR).REV_ACCOUNT=stub
     (*self.DR).REV_BCD_ACCOUNT=stub
     (*self.DR).REV_DUAL=stub
     if keyword_set(nodata) then begin 
        detBCD=(*self.DR).BCD
        (*self.DR).BCD=stub
        detUNC=(*self.DR).UNC
        (*self.DR).UNC=stub
        detBMASK=(*self.DR).BMASK
        (*self.DR).BMASK=stub
     endif else self->RestoreAll
     if keyword_set(noacc) then begin 
        av=self.ACCOUNTS_VALID
        self.ACCOUNTS_VALID=0b
        detACCOUNT=(*self.DR).ACCOUNT
        (*self.DR).ACCOUNT=stub
     endif
  endif
  
  if keyword_set(nodata) then begin 
     detVIZ=self.visualize_image & self.visualize_image=ptr_new()
     detVIZH=self.visualize_header & self.visualize_header=ptr_new()
  endif 
  
  if relfile && self.visualize_file then $
     self.visualize_file=make_filename_relative(self.visualize_file,root)
  
  oldchange=self.Changed        ;we want the file written to have changed=0!
  self.Changed=0b               ;but save the old preference incase we fail
  
  catch, serr
  if serr ne 0 then self.Changed=oldchange $ ;failed, reassign old changed
  else begin 
      ;; save this for last to allow error reporting in the meantime
      detInfo=self.wInfo & self.wInfo=ptr_new() ;don't save the widgets
      save,self,FILENAME=file,COMPRESS=comp
  endelse 
  catch,/CANCEL
  
  ;; Reattach 
  self.wInfo=detInfo           
  self.MsgList=detMsgList   
  self.cal=detCal
  self.AUTO_BPL.BCD_PIX=detabpl_pix
  self.AUTO_BPL.BCD_VALS=detabpl_vals
  self.AUTO_BPL.DCEID=detabpl_id
  self.AUTO_BPL.BCD_UNC=detabpl_unc
  self.AUTO_BPL.CNT_VEC=detabpl_cnt
  
  if ptr_valid(self.DR) then begin 
     (*self.DR).REV_ACCOUNT=detRevAccts
     (*self.DR).REV_BCD_ACCOUNT=detRevBcdAccts
     (*self.DR).REV_DUAL=detRevDual
     if keyword_set(nodata) then begin 
        (*self.DR).BCD=detBCD
        (*self.DR).UNC=detUNC
        (*self.DR).BMASK=detBMASK
     endif 
     if keyword_set(noacc) then begin 
        self.ACCOUNTS_VALID=av
        (*self.DR).ACCOUNT=detACCOUNT
     endif
  endif 
  
  if keyword_set(nodata) then begin 
     self.visualize_image=detVIZ
     self.visualize_header=detVIZH
  endif 
  
  if serr then self->Error,['Error Saving to File: ',file]
  
  if strlen(self.SaveFile) eq 0 or keyword_set(AS) then $
     self.SaveFile=file_expand_path(file)
  self->Status,status+'done ['+ $
               file_size_string((file_info(self.SaveFile)).size)+']'
  self->UpdateTitle
end


;=============================================================================
;  RestoreData - Restore selected data from file, without invalidating
;                records.  If BCD, UNCERTAINTY, or BMASK is passed,
;                only restore that component (default to all 3).
;                UNCERTAINTY will only be read if
;=============================================================================
pro CubeProj::RestoreData,sel,RESTORE_CNT=cnt,BCD=bcd,UNCERTAINTY=unc, $
                          BMASK=bm,_EXTRA=e
  self->RecOrSelect,sel,_EXTRA=e
  
  restore_bcd=n_elements(bm) gt 0?keyword_set(bm):1b
  restore_unc=keyword_set(unc)  || self.load_unc
  restore_bmask=keyword_set(bm) || self.load_masks
  
  flags=replicate(0b,n_elements(sel)) 
  if restore_bcd   then flags OR= ~ptr_valid((*self.DR)[sel].BCD)
  if restore_unc   then flags OR= ~ptr_valid((*self.DR)[sel].UNC)
  if restore_bmask then flags OR= ~ptr_valid((*self.DR)[sel].BMASK)
  
  wh=where(flags,cnt)
  
  if cnt gt 0 then begin 
     status=string(FORMAT='("Restoring ",I0," records...")',cnt)
     self->Status,status
  endif else return
  
  if self->IsWidget() then widget_control, /HOURGLASS
  
  ;; Go to the cube directory first, for relative file-names
  if self.savefile then cd,file_dirname(self.savefile)
  
  for i=0,cnt-1 do begin 
     which=sel[wh[i]]
     
     file=(*self.DR)[which].FILE
     if ~file_test(file,/READ) then $
        self->Error,["Couldn't restore data from file (check filename): ",file]
     
     if restore_bcd then begin  ;or whatever other data type (droop,etc.)
        ptr_free,(*self.DR)[which].BCD,(*self.DR)[which].HEADER
        (*self.DR)[which].BCD=ptr_new(readfits(file,/SILENT,hdr))
        if ~stregex(file,'bcd(_fp)?\.fits$',/BOOLEAN) then begin 
           bcdfile=irs_associated_file(file)
           if bcdfile && file_test(bcdfile,/READ) then hdr=headfits(bcdfile)
        endif 
        (*self.DR)[which].HEADER=ptr_new(hdr)
     endif 
     
     if restore_bmask then begin 
        ptr_free,(*self.DR)[which].BMASK
        bmask_file=irs_associated_file(file,/BMASK)
        if bmask_file && file_test(bmask_file,/READ) then $
           (*self.DR)[which].BMASK=ptr_new(readfits(bmask_file,/SILENT))
     endif 
     
     if restore_unc then begin 
        ptr_free,(*self.DR)[which].UNC
        unc_file=irs_associated_file(file,/UNCERTAINTY)
        if unc_file && file_test(unc_file,/READ) then $
           (*self.DR)[which].UNC=ptr_new(readfits(unc_file,/SILENT))
     endif
  endfor
  if n_elements(status) gt 0 then self->Status,status+'done'
  self->UpdateList
end

;=============================================================================
;  RestoreAll - Restore All data
;=============================================================================
pro CubeProj::RestoreAll
  self->RestoreData,/ALL,RESTORE_CNT=cnt
  if cnt gt 0 then self->UpdateAll
end

;=============================================================================
;  ReplaceFilenameSubstring - Replace common substring in selected
;                             filenames.  The search text can be a
;                             regexp, and replacement text can be a
;                             pattern wildcard, matched to real files.
;=============================================================================
pro CubeProj::ReplaceFilenameSubstring,from,to,recs,VERIFY_FILE=vf,_EXTRA=e
  self->RecOrSelect,recs,_EXTRA=e
  if recs[0] eq -1 then return
  do_it=1
  if n_elements(from) eq 0 then begin 
     f=(*self.DR)[recs].FILE
     cnt=0
     nf=n_elements(f)
     if nf gt 1 then begin 
        lab='Replacing text of '+strtrim(nf,2)+' files:'
        wh=where(total(byte(f) eq shift(byte(f),0,1),2) ne nf,cnt)
        def=strmid(f[0],0,wh[0])
        do_it=twoin(from,to,def,def,LABEL=lab, $
                    TEXT1='Replace Substring (regexp):', $
                    TEXT2='With:',PARENT_GROUP=self->TopBase(),/SCROLL, $
                    TITLE='Replace File Substring',TEXT_LAB=f,YSIZE=nf<6)
     endif else begin
        do_it=twoin(from,to,f[0],f[0],TEXT1='Replace Substring (regexp):', $
                    TEXT2='With:',PARENT_GROUP=self->TopBase(), $
                    TITLE='Replace File Substring')
     endelse 
  endif 
  if ~do_it then return
  
  for i=0,n_elements(recs)-1 do begin
     file=(*self.DR)[recs[i]].FILE
     pos=stregex(file,from,length=len)
     if pos eq -1 then self->Error,['No match for '+from+' in: ',file]
     file=strmid(file,0,pos)+to+strmid(file,pos+len)
     if keyword_set(vf) then begin ;; Find the real file 
        file=file_search(file,COUNT=cnt)
        if cnt eq 0 then self->Error,'No matching file found'
        file=file[0]
     endif 
     (*self.DR)[recs[i]].FILE=file
  endfor
  self->SetDirty
  self->UpdateTitle & self->UpdateButtons
end


;=============================================================================
;  ToggleBadPixel - Toggle the given bad pixel
;=============================================================================
pro CubeProj::ToggleBadPixel,pix,SET=set,RECORD_INDEX=rec,RECORD_SET=rset, $
                             UPDATE=ud,AUTO=auto
  if n_elements(rset) ne 0 then rec=self->DCEIDtoRec(rset)
  if n_elements(rec) ne 0 then $
     list=(*self.DR)[rec].BAD_PIXEL_LIST $
  else begin 
     ;; Fork Global bad pixel list if necessary
     if ptr_valid(self.GLOBAL_BAD_PIXEL_LIST) && $
        self.AS_BUILT.GLOBAL_BAD_PIXEL_LIST eq self.GLOBAL_BAD_PIXEL_list $
     then self.GLOBAL_BAD_PIXEL_LIST= $
        ptr_new(*self.AS_BUILT.GLOBAL_BAD_PIXEL_LIST)
     list=self.GLOBAL_BAD_PIXEL_LIST
  endelse 
     
  ;; Add or remove bp from appropriate list(s)
  if n_elements(set) ne 0 then begin 
     do_set=keyword_set(set)
     do_clear=set eq 0
  endif else begin 
     do_set=1 & do_clear=1
  endelse
  
  status_changed=0b
  set=1                         ;primary status is setting
  pix=reform(pix,/OVERWRITE)
  if keyword_set(auto) then bit=2b else bit=1b ;record auto-bad pix as such
  
  self->SetDirtyPix,pix,RECORD=rec ;; these pixels are now dirty
  
  for i=0,n_elements(list)-1 do begin 
     if ptr_valid(list[i]) then begin ;; already have a list
        keep=make_array(n_elements(*list[i]),VALUE=1b)
        
        if do_set then begin 
           wh=where_not_array(*list[i],[pix],cnt) ;new pix not on the list
           if cnt gt 0 then add_new=pix[wh]
        endif 
     
        if do_clear then begin 
           wh=where_array([pix],*list[i],cnt) ;pix already on the list
           if cnt gt 0 then begin 
              keep[wh]=0b       ;toggle these off
              set=0             ;signify that we're toggling at least some off
           endif 
        endif
        
        wh=where(keep,nkeep)
        if nkeep eq 0 then begin 
           if n_elements(add_new) eq 0 then begin 
              ptr_free,list[i]
              status_changed=1b
           endif else *list[i]=add_new
        endif else begin 
           if n_elements(add_new) gt 0 then $
              *list[i]=[(*list[i])[wh],add_new] $
           else *list[i]=(*list[i])[wh]
        endelse
        self.GLOBAL_BP_TYPE OR=bit & self.GLOBAL_BP_SAVEFILE_UPTODATE=0b
     endif else begin                      ; no list exists yet
        if do_clear && ~do_set then return ;can't clear any if no list!
        if n_elements(rec) ne 0 then $
           (*self.DR)[rec[i]].BAD_PIXEL_LIST=ptr_new([pix]) $
        else self.GLOBAL_BAD_PIXEL_LIST=ptr_new([pix])
        status_changed=1b
        self.GLOBAL_BP_TYPE OR=bit & self.GLOBAL_BP_SAVEFILE_UPTODATE=0b
     endelse 
  endfor 
  if ~self->Dirty() then begin 
     self->SetDirty
     self->UpdateTitle
  endif 
  if status_changed then self->UpdateButtons
  if keyword_set(ud) then self->Send,/BADPIX_UPDATE
end


;=============================================================================
;  UpdateListForBPChange - Update the list if BPs are being listed.
;=============================================================================
pro CubeProj::UpdateListForBPChange
  if ptr_valid(self.wInfo) && (*self.wInfo).which_list eq 1 then $
     self->UpdateList     
end

;=============================================================================
;  LoadBadPixels - Load bad pixels from file
;=============================================================================
pro CubeProj::LoadBadPixels,file,APPEND=append,ERROR=err
  catch, err
  if err ne 0 then begin 
     if n_elements(un) ne 0 then free_lun,un
     self->Error,['Error loading bad pixel list from '+file, $
                  !ERROR_STATE.MSG], /RETURN_ONLY
     file=-1
  endif
  if size(file,/TYPE) ne 7 then begin 
     xf,file,/RECENT,FILTERLIST=['*.{bpl,fits}','*.*','*'],$
        TITLE=(keyword_set(append)?'Append':'Load')+' Bad Pixels...', $
        /NO_SHOW_ALL,SELECT=0,/MODAL,PARENT_GROUP=self->TopBase(),_EXTRA=e
  endif 
  catch,/cancel
  if size(file,/TYPE) ne 7 then return ;cancelled
  if stregex(file,'\.fits$',/BOOLEAN) then begin 
     mask=readfits(file)
     bp=where(mask,cnt)         ;non-zero pixels are bad, for global bp list
     if cnt eq 0 then return
  endif else begin 
     openr,un,file,/GET_LUN
     line='' & recid=0L & npix=0L
     nlines=file_lines(file)
     while ~eof(un) do begin 
        catch,err
        if err ne 0 then begin 
           ;; Failed to read the record bad pixel line tag
           catch,/cancel
           if n_elements(mark) ne 0 then point_lun,un,mark
           bp=lonarr(nlines,/NOZERO)
           readf,un,bp
           break
        endif else begin 
           point_lun,-un,mark
           readf,un,line
           reads,line,recid,npix
           catch,/cancel
           nlines--
           if npix gt 0 then begin 
              recbp=lonarr(npix,/NOZERO)
              readf,un,recbp
              this_rec={DCEID:recid,PIX:ptr_new(recbp)}
              if n_elements(bprecs) eq 0 then bprecs=[this_rec] else $
                 bprecs=[bprecs,this_rec]
              nlines-=npix
           endif 
        endelse 
     endwhile 
     free_lun,un
  endelse 
  
  ;; Global bad pixels, and file list
  if n_elements(bp) gt 0 then begin 
     if keyword_set(append) && $
        ptr_valid(self.GLOBAL_BAD_PIXEL_LIST) then begin 
        bp=[bp,*self.GLOBAL_BAD_PIXEL_LIST]
        u=uniq(bp,sort(bp))
        bp=bp[u]
     endif 
  
     ;; Fork the two file lists, current and as-built
     if ptr_valid(self.GLOBAL_BP_FILES) && $
        self.GLOBAL_BP_FILES eq self.AS_BUILT.GLOBAL_BP_FILES $
     then self.GLOBAL_BP_FILES=ptr_new(*self.AS_BUILT.GLOBAL_BP_FILES)
     if self.GLOBAL_BAD_PIXEL_LIST ne self.AS_BUILT.GLOBAL_BAD_PIXEL_LIST $
     then ptr_free,self.GLOBAL_BAD_PIXEL_LIST
     self->SetDirtyPix,bp
     self.GLOBAL_BAD_PIXEL_LIST=ptr_new(bp,/NO_COPY)
  endif 
  
  ;; Record bad pixels
  nbprecs=n_elements(bprecs) 
  if nbprecs ne 0 then begin 
     recs=self->DCEIDtoRec(bprecs.dceid)
     if recs[0] eq -1 then begin 
        self->Warning, $
           'No matching records present, omitting all single record badpix.'
     endif else begin 
        nrecs=n_elements(recs) 
        if nrecs lt nbprecs then $
           self->Warning,'Only '+strtrim(nrecs,2)+' of '+strtrim(nbprecs,2)+ $
                      ' records with bad pixels present.'
        for i=0,nrecs-1 do begin
           wh=where(bprecs.DCEID eq (*self.DR)[recs[i]].DCEID,cnt)
           if cnt eq 0 then continue
           badpix=*bprecs[wh[0]].PIX
           self->SetDirtyPix,RECORD=recs[i],badpix
           if keyword_set(append) then begin
              list=(*self.DR)[recs[i]].BAD_PIXEL_LIST
              if ptr_valid(list) then begin 
                 *list=[badpix,*list]
                 u=uniq(*list,sort(*list))
                 *list=(*list)[u]
              endif else (*self.DR)[recs[i]].BAD_PIXEL_LIST=ptr_new([badpix])
           endif else begin 
              ptr_free,(*self.DR)[recs[i]].BAD_PIXEL_LIST
              (*self.DR)[recs[i]].BAD_PIXEL_LIST=ptr_new([badpix])
           endelse 
        endfor 
     endelse 
  endif 
        
  ;; Update BP file record, a history of how the BP's have arrived
  if keyword_set(append) then begin 
     ;; Append to list of files
     if ptr_valid(self.GLOBAL_BP_FILES) then $
        *self.GLOBAL_BP_FILES=[*self.GLOBAL_BP_FILES,file] $
     else self.GLOBAL_BP_FILES=ptr_new([file])
  endif else begin 
     ;; All by-hand, auto, and other files' BP's have been replaced
     ptr_free,self.GLOBAL_BP_FILES
     self.GLOBAL_BP_FILES=ptr_new([file])
     self.GLOBAL_BP_TYPE=0b
  endelse 
  
  self.GLOBAL_BP_SAVEFILE_UPTODATE=0b
  self->SetDirty
  self->UpdateButtons & self->UpdateTitle
  self->Send,/BADPIX_UPDATE
end

;=============================================================================
;  SaveBadPixels - Save bad pixels to file
;=============================================================================
pro CubeProj::SaveBadPixels,file
  if ~ptr_valid(self.GLOBAL_BAD_PIXEL_LIST) && $
     (~ptr_valid(self.DR) || $
      array_equal(ptr_valid((*self.DR).BAD_PIXEL_LIST),0b)) then return
  
  if size(file,/TYPE) ne 7 then begin 
     start=self->FileBaseName()+".bpl"
     xf,file,/RECENT,FILTERLIST=['*.bpl','*.*','*'],/SAVEFILE, $
        TITLE='Save Bad Pixels As...',/NO_SHOW_ALL,SELECT=0, $
        START=start,PARENT_GROUP=self->TopBase(),/MODAL
  endif 
  if size(file,/TYPE) ne 7 then return ;cancelled
  openw,un,file,/GET_LUN
  
  ;; Write the record-specific bad pixels
  wh_bcd_bp=where(ptr_valid((*self.DR).BAD_PIXEL_LIST),cnt)
  for i=0,cnt-1 do begin 
     rec=(*self.DR)[wh_bcd_bp[i]]
     printf,un,rec.DCEID,n_elements(*rec.BAD_PIXEL_LIST)
     printf,un,transpose(*rec.BAD_PIXEL_LIST)
  endfor 
  
  ;; Simple style: list of bad pixels
  if ptr_valid(self.GLOBAL_BAD_PIXEL_LIST) then $
     printf,un,transpose(*self.GLOBAL_BAD_PIXEL_LIST)
     
  ;; All of our BPs are now stored in this one file
  self.GLOBAL_BP_SAVEFILE=file
  self.GLOBAL_BP_SAVEFILE_UPTODATE=1b
  free_lun,un
end


;=============================================================================
;  ClearBadPixels - Clear all bad pixels out
;=============================================================================
pro CubeProj::ClearBadPixels, recs, CLEAR_RECORDS=clear_recs, $
                              NO_UPDATE=nu,_EXTRA=e
  if keyword_set(clear_recs) then self->RecOrSelect,recs,_EXTRA=e
  
  ;; Clear the record based bad pixels
  if n_elements(recs) gt 0 then begin
     if ~ptr_valid(self.DR) then return
     for i=0,n_elements(recs)-1 do $
        if ptr_valid((*self.DR)[recs[i]].BAD_PIXEL_LIST) then $
           self->SetDirtyPix,*(*self.DR)[recs[i]].BAD_PIXEL_LIST,RECORD=recs[i]
     ptr_free,(*self.DR)[recs].BAD_PIXEL_LIST
  endif else begin 
     if ptr_valid(self.GLOBAL_BAD_PIXEL_LIST) then $
        self->SetDirtyPix,*self.GLOBAL_BAD_PIXEL_LIST ;; all now dirty
     
     ;; Clear out the bad pixels
     if self.GLOBAL_BAD_PIXEL_LIST ne self.AS_BUILT.GLOBAL_BAD_PIXEL_LIST $
     then ptr_free,self.GLOBAL_BAD_PIXEL_LIST else $
        self.GLOBAL_BAD_PIXEL_LIST=ptr_new()

     ;; Clear the list of files which contributed
     if self.GLOBAL_BP_FILES ne self.AS_BUILT.GLOBAL_BP_FILES then $
        ptr_free,self.GLOBAL_BP_FILES else self.GLOBAL_BP_FILES=ptr_new()
     
     ;; None by hand either
     self.GLOBAL_BP_TYPE=0b
     self.GLOBAL_BP_SAVEFILE_UPTODATE=0b
  endelse 
  self->SetDirty
  self->UpdateAll
  if ~keyword_set(nu) then self->Send,/BADPIX_UPDATE
end

;=============================================================================
;  SetDirtyPix - Set some pixels "dirty" globally, or per record.
;                Used to make subsequent cube re-builds very fast.
;=============================================================================
pro CubeProj::SetDirtyPix,RECORD=rec,pix
  if n_elements(rec) ne 0 then begin 
     if ~ptr_valid(self.DR) then return
     list=(*self.DR)[rec].DIRTY_PIX
  endif else list=self.GLOBAL_DIRTY_PIX
  
  update=0b
  for i=0,n_elements(list)-1 do begin 
     if ~ptr_valid(list[i]) then begin 
        if n_elements(rec) ne 0 then $
           (*self.DR)[rec[i]].DIRTY_PIX=ptr_new(pix) $
        else self.GLOBAL_DIRTY_PIX=ptr_new(pix)
        update=1b
     endif else begin 
        wh=where_not_array(*list[i],[pix],cnt)
        if cnt eq 0 then return ; already dirty
        *list[i]=[*list[i],pix[wh]]
     endelse 
  endfor 
  
  if update then self->UpdateButtons           ;new list, need updating
end

;=============================================================================
;  ClearDirtyPix - Clear all dirty pixel lists
;=============================================================================
pro CubeProj::ClearDirtyPix
  got=ptr_valid(self.GLOBAL_DIRTY_PIX)
  if got then ptr_free,self.GLOBAL_DIRTY_PIX
  if ptr_valid(self.DR) then begin 
     got OR= ~array_equal(ptr_valid((*self.DR).DIRTY_PIX),0b)
     ptr_free,(*self.DR).DIRTY_PIX
  endif 
  if got then self->UpdateButtons
end

;=============================================================================
;  LoadBackGroundList - Load background exposures from file
;=============================================================================
pro CubeProj::LoadBackGroundList,file,ERROR=err,_EXTRA=e
  catch, err
  if err ne 0 then begin 
     if n_elements(un) ne 0 then free_lun,un
     self->Error,['Error loading background list from '+ $
                  file,!ERROR_STATE.MSG],$
                 /RETURN_ONLY
     file=-1
  endif
  if size(file,/TYPE) ne 7 then begin 
     xf,file,/RECENT,FILTERLIST=['*.bgl','*.*','*'],$
        TITLE='Load Background List',/NO_SHOW_ALL,SELECT=0, $
        /MODAL,PARENT_GROUP=self->TopBase(),_EXTRA=e
  endif 
  catch,/cancel
  if size(file,/TYPE) ne 7 then return ;cancelled
  openr,un,file,/GET_LUN
  catch,type
  if type ne 0 then begin 
     catch,/cancel 
     point_lun,un,0
     bglist=lonarr(file_lines(file),/NOZERO)
     readf,un,bglist
     wh=self->DCEIDtoRec(bglist,goodcnt)
     if goodcnt lt n_elements(bglist) then $
        self->Warning,[strtrim(n_elements(bglist)-goodcnt,2)+ $
                       ' specified background record(s)', $
                       'not present in project']
     self->SetBackgroundFromRecs,wh,_EXTRA=e
  end else begin 
     line=''
     readf,un,line
     afrac=0.0 & acnt=0L
     reads,line,afrac,acnt,FORMAT='(F0,I0)' ; will throw error for "normal" BGL
     alist=lonarr(acnt)
     readf,un,alist
     bfrac=0.0 & bcnt=0L
     readf,un,bfrac,bcnt,FORMAT='(F0,I0)'
     blist=lonarr(bcnt)
     readf,un,blist
     
     wha=self->DCEIDtoRec(alist,agoodcnt)
     whb=self->DCEIDtoRec(blist,bgoodcnt)
     if agoodcnt eq 0 then $
        self->Error,'Project missing all records for combine set A.'
     if bgoodcnt eq 0 then $
        self->Error,'Project missing all records for combine set B.'
          
     if agoodcnt lt acnt || bgoodcnt lt bcnt then $
        self->Warning,[strtrim(acnt+bcnt-agoodcnt-bgoodcnt,2)+ $
                       ' specified background record(s)', $
                       'not present in project']
     self->SetBackgroundFromRecs,wha,BLEND=0,/SET_ONLY
     self->SetBackgroundFromRecs,whb,BLEND=1,/SET_ONLY
     self->BlendBackgrounds,ASCALE=afrac,BSCALE=bfrac, $
                            /USE_EXISTING_SCALES
  endelse 
  free_lun,un
end

;=============================================================================
;  SaveBackGroundList - Save list of background records to file
;=============================================================================
pro CubeProj::SaveBackGroundList,file
  if ~ptr_valid(self.BACK_RECS) then return
  if size(file,/TYPE) ne 7 then begin 
     start=self->FileBaseName()+".bgl"
     xf,file,/RECENT,FILTERLIST=['*.bgl','*.*','*'],/SAVEFILE, $
        TITLE='Save Background List As...',/NO_SHOW_ALL,SELECT=0, $
        START=start,PARENT_GROUP=self->TopBase(),/MODAL
  endif 
  if size(file,/TYPE) ne 7 then return ;cancelled
  openw,un,file,/GET_LUN
  if size(*self.BACK_RECS,/TYPE) eq 8 then begin 
     for i=0,1 do begin 
        printf,un,(*self.BACK_RECS)[i].SCALE, $
               n_elements(*(*self.BACK_RECS)[i].DCEIDs)
        printf,un,transpose(*(*self.BACK_RECS)[i].DCEIDs)
     endfor 
  endif else printf,un,1#(*self.BACK_RECS)
  free_lun,un
end


;=============================================================================
;  CheckRecordUncertainties - See if records have uncertainties set
;=============================================================================
function CubeProj::CheckRecordUncertainties,ENABLED=enabled
  if ~ptr_valid(self.DR) then return,1b
  if keyword_set(enabled) then begin
     wh=where(~(*self.DR).DISABLED,cnt)
     if cnt eq 0 then return,1b
     return,array_equal(ptr_valid((*self.DR)[wh].UNC),1b)
  endif else $
     return,array_equal(ptr_valid((*self.DR).UNC),1b)     
end

;=============================================================================
;  ExportToMain - Send ourselves to the $MAIN$ level, using the
;                 unsupported RSI routine routine_names().  Caveat
;                 Exportor.  Care is taken to ensure it will at least
;                 compile, and trap errors in case routine_names
;                 vanishes.  Also supports exporting spectra and maps
;                 with the SPECTRUM & MAP keywords.  TYPE is a suffix
;                 for the map.
;=============================================================================
pro CubeProj::ExportToMain, SPECTRUM=sp, MAP=mp, TYPE=map_type
  name=self->ProjectName()
  spQ=n_elements(sp) ne 0
  mpQ=n_elements(mp) ne 0
  if n_elements(map_type) eq 0 then map_type="_map"
  selfQ=~spQ && ~mpQ
  name+=spQ?'_sp':(mpQ?map_type:'')
  export=spQ?sp:(mpQ?mp:self)
  
   ;; Simple fix -- replace dashes/spaces with underscores
  def=strlowcase(idl_validname(name,/CONVERT_ALL))
  type=spQ?'spectrum':(mpQ?'map':'Cube object')
  title=spQ?'Spectrum':(mpQ?'Map':'Cube Project')
  var_name=getinp('Name of exported '+type+' var:',def, $
                  TITLE='Export '+title+' to Command Line', $
                  /MODAL, PARENT_GROUP=self->TopBase())
   if var_name eq '' then return
   if strmid(var_name,0,1) eq '$' then $
      self->Error,'Variable name must not start with $'
   if NOT execute(var_name+'=1',1) then $
      self->Error,'Not a valid variable name: '+var_name
   var_free=0
   catch, err
   if err ne 0 then begin       
      ;;An UNDEFVAR indicates routine_info ran and the variable is free
      if !ERROR_STATE.NAME ne 'IDL_M_UNDEFVAR' then begin 
         catch,/cancel
         self->Error,"Can't complete export operation"
      endif
      var_free=1
   endif 
   
   ;; Don't overwrite an existing variable
   if var_free eq 0 then $
      existing_var=call_function('routine_names',var_name,FETCH=1)
   if n_elements(existing_var) ne 0 then begin 
      ;;see if we're already there
      if selfQ && size(existing_var,/TYPE) eq 11 && existing_var[0] eq self $
         then begin
         self->Info,['The project is already exported', $
                     'to variable "'+var_name+'"']
         return
      endif 
      catch,/cancel
      self->Error,'A variable named '+var_name+' already exists'
   endif 
   
   ;; Still here... we need to export to the main level
   void=call_function('routine_names',var_name,export,store=1)
   self.Spawned=0b              ;no longer spawned
   print,' *** CUBISM: ',title,' exported to variable: '
   print,'   ====> '+var_name+' <==='
   self->Status,'Exported project to variable: '+var_name
end
   
;=============================================================================
;  Revert - Revert to the old version on disk
;=============================================================================
pro CubeProj::Revert
  if strlen(self.SaveFile) eq 0 then return ;can only revert to a file
  openr,un,/get_lun,self.SaveFile
  time=systime(0,(fstat(un)).mtime)
  free_lun,un
  if self->IsWidget() then begin 
     ans=dialog_message(['Revert Project '+self->ProjectName()+ $
                         ' from file:', '  '+self.SaveFile,'last modified '+ $
                         time+"?"], $
                        /QUESTION,TITLE='Revert Project', $
                        DIALOG_PARENT=self->TopBase())
     if ans ne 'Yes' then return
  endif 
  status='Reverting project from '+time+'...'
  self->Status,status
  oldself=self                  ;our old self, soon to be history!
  wsav=self.wInfo               ;detach wInfo and MsgList from Self, to retach
  msav=self.MsgList             ;to the transmogrified self.
  nsav=self.ProjectName
  self.wInfo=ptr_new() & self.MsgList=ptr_new() ;the detachment
  
  oldchange=self.Changed
  feedback=self.feedback
  self.Changed=0b               ;to ensure we won't try to save ourselves 
  self.ProjectName=''           ;avoid name conflict with new self.

  ;; Restore the on-disk self to this space.. this sets Changed to 0
  ;; on the new object, and sets up the new object's SaveFile, only
  ;; if the object was actually read in correctly.
  self=self->Load(self.SaveFile,ERROR=rerr)
  
  self.wInfo=wsav 
  self.MsgList=msav 
  
  if rerr eq 0 then begin 
     ;; Self has been overwritten by restore.... destroy our old self.
     obj_destroy,oldself        ;kill our old self, except detached
     
     ;; let any widgets know about the change in self!
     if self->isWidget() then $
        widget_control,(*self.wInfo).Base,SET_UVALUE=self
  endif else begin              ;failed, restore the old self
     self.ProjectName=nsav
     self.Changed=oldchange
  endelse 
  self.feedback=feedback
  
  self->Status,status+'done'
  self->Send,/UPDATE,/NEW_CUBE,/BADPIX_UPDATE
  self->UpdateAll
end

;=============================================================================
;  Showing - Is the project showing?
;=============================================================================
function CubeProj::Showing
  if ~ptr_valid(self.wInfo) then return,0
  if (*self.wInfo).showing eq -1 then $
     (*self.wInfo).showing=XRegistered('CubeProj_Show:'+self.ProjectName+ $
                                       self.savefile,/NOSHOW) gt 0
  return, (*self.wInfo).showing
end

;=============================================================================
;  TopBase - The top base (non-widget safe)
;=============================================================================
function CubeProj::TopBase
  return,ptr_valid(self.wInfo)?(*self.wInfo).Base:0L
end

;=============================================================================
;  UpdateTitle - Update the Show GUI Title
;=============================================================================
pro CubeProj::UpdateTitle
  if NOT self->IsWidget() then return
  pn=self->ProjectName()
  if self->Dirty() then pn=string('253'OB)+pn+string('273'OB)
  widget_control, (*self.wInfo).Base,  $
                  TLB_SET_TITLE='CUBISM Project: '+ pn + $
                  "  <"+(self.SaveFile?file_basename(self.SaveFile): $
                         "(unsaved)")+">"
end

;=============================================================================
;  UpdateList - Update the BCD list
;=============================================================================
pro CubeProj::UpdateList,CLEAR_SELECTION=cs,_EXTRA=e
  self->Sort
  if NOT self->IsWidget() then return
  widget_control, (*self.wInfo).Base,UPDATE=0
  if keyword_set(cs) then begin 
     widget_control, (*self.wInfo).SList, set_value=self->List()
     self->SetListSelect,-1,_EXTRA=e
  endif else begin 
     ls=self->CurrentSelect()<(self->N_Records()-1)
     t=widget_info((*self.wInfo).SList,/LIST_TOP) 
     widget_control, (*self.wInfo).SList, set_value=self->List(), $
                     SET_LIST_SELECT=ls,SET_LIST_TOP=t
  endelse
  widget_control, (*self.wInfo).Base,/UPDATE
end


;=============================================================================
;  SetListSelect - Set the list selection
;=============================================================================
pro CubeProj::SetListSelect,recs,ALL=all,NO_PRESERVE_TOP=npt,INVERT=inv, $
                            NONE=none,NO_DISABLED=nd,NO_UPDATE=nu, $
                            NO_STATUS=no_stat,_EXTRA=e
  
  if ~ptr_valid(self.wInfo) then return
  
  if keyword_set(none) then begin 
     nrec=0
     recs=-1                    ;clear selection
  endif else begin 
     self->RecOrSelect,recs, ALL=all,_EXTRA=e
     nrec=n_elements(recs) 
  endelse 
  
  if keyword_set(inv) then begin 
     nrec_all=self->N_Records()
     if nrec gt 0 then begin 
        inds=bytarr(nrec_all)
        inds[recs]=1b
        recs=where(~inds,nrec)
     endif else begin 
        self->RecOrSelect,recs, /ALL
        nrec=nrec_all
     endelse 
  endif 
  
  if keyword_set(nd) && nrec gt 0 then begin ;; remove disabled recs from list
     wh=where(~(*self.DR)[recs].DISABLED,nrec)
     if nrec eq 0 then recs=-1 else recs=recs[wh]
  endif 

  if ~keyword_set(npt) then $ ;; preserve top location
     top=widget_info((*self.wInfo).SList,/LIST_TOP)
  
  widget_control, (*self.wInfo).SList, SET_LIST_SELECT=recs
  
  if ~keyword_set(npt) then $ ;; restore top location
     widget_control, (*self.wInfo).SList,SET_LIST_TOP=top
    
  if ~keyword_set(no_stat) then $ ;; update status
     if nrec gt 1 then self->Status,strtrim(nrec,2)+' records selected' else $
        self->Status,/CLEAR

  self->UpdateButtons
  if keyword_set(nu) then return
  if nrec eq 1 && recs[0] ne -1 then begin 
     self->Send,/SELECT,SINGLE_SELECT=recs[0]
  endif else self->Send,/SELECT
end

;=============================================================================
;  List - List the data records
;=============================================================================
function CubeProj::List
   n=self->N_Records()
   if n eq 0 then return,' '
   which_list=0
   if self->isWidget() then if (*self.wInfo).which_list then which_list=1 
   
   if which_list eq 0 then begin 
      date_obs=jul2date((*self.DR).DATE_OBS,/D_T)
      date=jul2date((*self.DR).DATE,/D_T)
      fovname=irs_fov((*self.DR).FOVID,/SHORT_NAME)
   endif else begin 
      bcd_valid=ptr_valid((*self.DR).BCD)
      unc_valid=ptr_valid((*self.DR).UNC)
      bmask_valid=ptr_valid((*self.DR).BMASK)
      bpl_valid=ptr_valid((*self.DR).BAD_PIXEL_LIST)
      gacc_valid=~self->Dirty(/ACCOUNTS)
      acct_valid=ptr_valid((*self.DR).ACCOUNT)
      rev_acct_valid=ptr_valid((*self.DR).REV_ACCOUNT)
   endelse 
   
   list=strarr(n)
   for i=0,n-1 do begin 
      if which_list eq 0 then begin ;the standard list
         type=(*self.DR)[i].type>0
         tchar=([" ","d","c","f"])[type]
         s=string(FORMAT='(" ",A-20,T23,F6.2,T30,A,T49,A,T68,A8,T77,' + $
                  'I3,"[",I0,",",I0,"]")', $
                  (*self.DR)[i].ID, $
                  (*self.DR)[i].TIME, $
                  date_obs[i], $
                  date[i], $
                  tchar+fovname[i], $
                  (*self.DR)[i].EXP,(*self.DR)[i].ROW,(*self.DR)[i].COLUMN)
      endif else begin          ;the additional list
         if acct_valid[i] then begin 
            if gacc_valid && acct_valid[i] then begin 
               if rev_acct_valid[i] then acct="Y(+R)" $
               else acct="Y"
            endif else acct="INV"
         endif else acct="N"
         nbpl=bpl_valid[i]?n_elements(*(*self.DR)[i].BAD_PIXEL_LIST):0
         
         pos=self.reconstructed_pos?(*self.DR)[i].REC_POS: $
             (*self.DR)[i].RQST_POS
         s=string(FORMAT='(" ",A-19,T22,A11,T34,A12,T50,3(A1,6X),A5,2X,I6)', $
                  (*self.DR)[i].OBJECT, $
                  radecstring(pos[0],/RA),radecstring(pos[1]), $
                  bcd_valid[i]?'Y':'N', $
                  unc_valid[i]?'Y':'N', $
                  bmask_valid[i]?'Y':'N', $
                  acct,nbpl)
      endelse       
      if (*self.DR)[i].DISABLED then begin 
         b=byte(s)
         b[where(b eq 32b)]=45b ;replace space with dash
         s=string(b)
      endif
      list[i]=s
   endfor 
   return,list
end

;=============================================================================
;  UpdateButtons - Change Button sensitivity
;=============================================================================
pro CubeProj::UpdateButtons
  if NOT self->IsWidget() then return
  got_dr=ptr_valid(self.DR)
  got_cube=ptr_valid(self.CUBE)
  got_accounts=~self->Dirty(/ACCOUNTS)
  
  sel=self->CurrentSelect()
  got_sel=got_dr && sel[0] ne -1
  for i=0,n_elements((*self.wInfo).MUST_SELECT)-1  do  $
     widget_control,((*self.wInfo).MUST_SELECT)[i],SENSITIVE=got_sel
  nsel=n_elements(sel) 
  if (nsel gt 1) ne ((*self.wInfo).nsel_sav gt 1) then begin 
     ;; Change in number of selections to more than 1
     if nsel gt 1 then begin 
        widget_control,(*self.wInfo).view_ids[0],SET_VALUE='View Stack... '
        widget_control,(*self.wInfo).view_ids[1], $
                       SET_VALUE='View Stack (new viewer)... '
        widget_control,(*self.wInfo).view_ids[2], $
                       SET_VALUE='View Stack '
     endif else begin 
        widget_control,(*self.wInfo).view_ids[0],SET_VALUE='View Record...'
        widget_control,(*self.wInfo).view_ids[1], $
                       SET_VALUE='View Record (new viewer)...'
        widget_control,(*self.wInfo).view_ids[2], $
                       SET_VALUE='View Record'
     endelse 
     (*self.wInfo).nsel_sav=nsel
  endif 
  
  widget_control, (*self.wInfo).MUST_NO_DATA, $
                  SENSITIVE=~(self.SaveMethod AND 1b)
  
  bg_valid=ptr_valid(self.BACKGROUND) 
  
  for i=0,n_elements((*self.wInfo).MUST_CAL)-1  do  $
     widget_control,((*self.wInfo).MUST_CAL)[i], SENSITIVE=obj_valid(self.cal)
  
  for i=0,n_elements((*self.wInfo).MUST_CAL_FILE)-1  do  $
     widget_control,((*self.wInfo).MUST_CAL_FILE)[i], $
                    SENSITIVE=logical_true(self.cal_file)
  
  for i=0,n_elements((*self.wInfo).MUST_PROJ)-1  do  $
     widget_control, (*self.wInfo).MUST_PROJ[i], SENSITIVE=got_dr
  
  quickbuild=got_cube && got_dr && got_accounts && $
     (~self.use_unc || ptr_valid(self.CUBE_UNC)) && $
     (ptr_valid(self.GLOBAL_DIRTY_PIX) || $
      ~array_equal(ptr_valid((*self.DR).DIRTY_PIX),0b))
  
  if quickbuild ne (*self.wInfo).quickbuild_sav then begin 
     if quickbuild then begin 
        widget_control, (*self.wInfo).build_ids[0], $
                        SET_VALUE='Build Cube (Quick)'
        widget_control, (*self.wInfo).build_ids[1], SET_VALUE='QuickBuild'
     endif else begin 
        widget_control, (*self.wInfo).build_ids[0], $
                        SET_VALUE='Build Cube'
        widget_control, (*self.wInfo).build_ids[1], SET_VALUE='Build Cube'
     endelse
     (*self.wInfo).quickbuild_sav=quickbuild
  endif 
  
  for i=0,n_elements((*self.wInfo).MUST_ACCT)-1  do  $
     widget_control, (*self.wInfo).MUST_ACCT[i], $
                     SENSITIVE=got_accounts
  
  widget_control, (*self.wInfo).MUST_MODULE,SENSITIVE=self.MODULE ne ''
  
  widget_control, (*self.wInfo).MUST_VIS, $
                  SENSITIVE=ptr_valid(self.VISUALIZE_IMAGE)
  
  for i=0,n_elements((*self.wInfo).MUST_CUBE)-1 do $
     widget_control, ((*self.wInfo).MUST_CUBE)[i], SENSITIVE=got_cube
  
  for i=0,n_elements((*self.wInfo).MUST_BACK)-1 do $ 
     widget_control, (*self.wInfo).MUST_BACK[i], $
                     SENSITIVE=bg_valid
  
  got_comb=ptr_valid(self.BACK_RECS) && size(*self.BACK_RECS,/TYPE) eq 8
  got_back=got_comb?ptr_valid((*self.BACK_RECS).BACKGROUND):[0b,0b]
  widget_control, (*self.wInfo).MUST_BACK_A, SENSITIVE=got_back[0]
  widget_control, (*self.wInfo).MUST_BACK_B, SENSITIVE=got_back[1]
  widget_control, (*self.wInfo).MUST_BACK_AB,SENSITIVE=array_equal(got_back,1b)
  
  widget_control, (*self.wInfo).MUST_BG_SP,SENSITIVE=ptr_valid(self.BG_SP)
  
  widget_control, (*self.wInfo).MUST_ANY_BACK, $
                  SENSITIVE=bg_valid || ptr_valid(self.BG_SP)
  
  gbpl=ptr_valid(self.GLOBAL_BAD_PIXEL_LIST)
  
  widget_control, (*self.wInfo).MUST_GLOBAL_BPL,SENSITIVE=gbpl
  
  rbpl=got_sel && ~array_equal(ptr_valid((*self.DR)[sel].BAD_PIXEL_LIST),0b)
  widget_control, (*self.wInfo).MUST_REC_BPL,SENSITIVE=rbpl
  
  for i=0,n_elements((*self.wInfo).MUST_ANY_BPL)-1 do $
    widget_control, (*self.wInfo).MUST_ANY_BPL[i],SENSITIVE= $
                    (got_dr && $
                     ~array_equal(ptr_valid((*self.DR).BAD_PIXEL_LIST),0b)) || $
                    gbpl
  
  for i=0,n_elements((*self.wInfo).MUST_REC_UNC)-1 do $
     widget_control, (*self.wInfo).MUST_REC_UNC[i], $
                     SENSITIVE=got_sel && $
                     ~array_equal(ptr_valid((*self.DR)[sel].UNC),0b)
  
  widget_control, (*self.wInfo).MUST_SAVE_CHANGED,SENSITIVE= $
                  strlen(self.SaveFile) ne 0 AND self->Dirty()
  
  if ~got_dr then $
     unrestored=1b $
  else begin 
     unrestored=~array_equal(ptr_valid((*self.DR).BCD),1b) || $
                ~array_equal(ptr_valid((*self.DR).BMASK),1b)
     if ~unrestored && self.load_unc then $
        unrestored=~array_equal(ptr_valid((*self.DR).UNC),1b)
  endelse 
     
  widget_control, (*self.wInfo).MUST_UNRESTORED,SENSITIVE=unrestored  
    
  widget_control, (*self.wInfo).MUST_UNCERTAINTIES, $
                  SENSITIVE=self->CheckRecordUncertainties(/ENABLED) && $
                  (~(self.use_bg  && bg_valid) || $
                   ptr_valid(self.BACKGROUND_UNC))
end

;=============================================================================
;  UpdateColumnHeads - Update the sort selected button (carry to page 2).
;=============================================================================
pro CubeProj::UpdateColumnHeads
  if NOT self->IsWidget() then return
  flags=bytarr(14)
  flags[self.sort]=1b
  if (*self.wInfo).which_list eq 0 then flags=flags[0:5] else $
     flags=[flags[6:*]]
  widget_control, (*self.wInfo).wHead[(*self.wInfo).which_list],SET_VALUE=flags
end

;=============================================================================
;  UpdateAll
;=============================================================================
pro CubeProj::UpdateAll,NO_LIST=nl
  if ~self->IsWidget() then return
  self->UpdateButtons
  if ~keyword_set(nl) then self->UpdateColumnHeads
  if ~keyword_set(nl) then self->UpdateList
  self->UpdateTitle
end

;=============================================================================
;  FindViewer - Find a CubeView to which to send messages.  We send
;               messages to one and only one viewer, unless NEW_VIEWER
;               is set, in which case we spawn a new viewer.  The
;               viewer found will be the most recent one on the
;               managed list which is displaying products from this
;               cube of the same type (bcd or cube, bcd by default) as
;               indicated by the CUBE keyword (default to BCD mode)
;=============================================================================
pro CubeProj::FindViewer,NEW_VIEWER=new_viewer,CUBE_MODE=cube_mode, $
                         VISUALIZE_MODE=vmode,_EXTRA=e
  FORWARD_FUNCTION LookupManagedWidget
  
  recs=self->GetMsgObjs(CLASS='CubeRec')
  got_viewers=obj_valid(recs[0])
  
  if got_viewers then $ ;; don't let them hear viewing messages
     self->MsgSignup,recs,CUBEPROJ_CUBE=0,CUBEPROJ_RECORD=0, $
                     CUBEPROJ_VISUALIZE=0

  ;; None set?  First look for an existing, unassociated viewer
  if ~keyword_set(new_viewer) && ~got_viewers then begin
     catch, err                 ;Make sure we have LookUpManagedWidget.
     if err ne 0 then begin 
        resolve_routine,'XManager',/COMPILE_FULL_FILE
        catch,/cancel
     endif
     ids=LookupManagedWidget('CubeView')
     catch,/cancel
     for i=0,n_elements(ids)-1 do begin 
        if ~widget_info(ids[i],/VALID_ID) then continue
        rec=widget_info(ids[i],FIND_BY_UNAME='CubeRec')
        if ~widget_info(rec,/VALID_ID) then continue
        widget_control, rec,GET_UVALUE=rec
        if ~obj_valid(rec) || ~obj_isa(rec,'CubeRec') then continue
        rec->GetProperty,CUBE=cube
        if ~obj_valid(cube) then begin 
           free_rec=rec
           break
        endif 
     endfor 
     if n_elements(free_rec) ne 0 then begin 
        self->MsgSignup,free_rec,/ALL,CUBEPROJ_SELECT=0
        return
     endif 
  endif 
     
  ;; Do we need a new viewer?  Spawn one if necessary (i.e. if requested).
  if keyword_set(new_viewer) then begin 
     ;; Have them sign up for all our messages, by default
     cubeview,CUBE=self,_EXTRA=e 
     return
  endif
  
  ;; Pick the first match among the viewers we're communicating with
  if got_viewers then begin 
     for r=0,n_elements(recs)-1 do begin 
        rec=recs[r]
        rec->GetProperty,BCD_MODE=bcd_mode,VISUALIZE_MODE=rvmode
        if (keyword_set(vmode) && rvmode) || $
           bcd_mode eq ~keyword_set(cube_mode) then begin ;the right mode
           ;; sign up the correct one
           self->MsgSignup,rec,/ALL,CUBEPROJ_SELECT=0
           return
        endif 
     endfor 
     ;; Didn't find one with the right mode... just take the first one
     self->MsgSignup,recs[0],/ALL,CUBEPROJ_SELECT=0
  end else cubeview,CUBE=self
end

;=============================================================================
;  ViewCube - View the cube in an existing or new viewer
;=============================================================================
pro CubeProj::ViewCube,NEW_VIEWER=new,_EXTRA=e
  if NOT ptr_valid(self.CUBE) then self->Error,'No cube to view'
  self->FindViewer,/CUBE_MODE,NEW_VIEWER=new,_EXTRA=e
  self->Send,/CUBE
end

;=============================================================================
;  ViewBackground - View the background in an existing or new viewer
;=============================================================================
pro CubeProj::ViewBackground,NEW_VIEWER=new,BLEND=comb
  if n_elements(comb) ne 0 then begin 
     pos=keyword_set(comb)
     if ~ptr_valid(self.BACK_RECS) || $
        ~ptr_valid((*self.BACK_RECS)[pos].BACKGROUND) then $
           self->Error,'No background to view'
     if ptr_valid((*self.BACK_RECS)[pos].DCEIDs) then $
        self->SetListSelect,self->DCEIDtoRec(*(*self.BACK_RECS)[pos].DCEIDs)
  endif else begin 
     if ~ptr_valid(self.BACKGROUND) then self->Error,'No background to view'
     if ptr_valid(self.BACK_RECS) then begin 
        if size(*self.BACK_RECS,/TYPE) eq 8 then begin 
           if ptr_valid((*self.BACK_RECS)[0].DCEIDs) then $
              bglist=*(*self.BACK_RECS)[0].DCEIDs
           if ptr_valid((*self.BACK_RECS)[0].DCEIDs) then begin 
              bglist2=*(*self.BACK_RECS)[1].DCEIDs
              if n_elements(bglist) eq 0 then $
                 bglist=bglist2 else bglist=[bglist,bglist2]
           endif 
        endif else bglist=*self.BACK_RECS
        self->SetListSelect,self->DCEIDtoRec(bglist)
     endif 
  endelse 
  self->FindViewer,NEW_VIEWER=new
  self->Send,/BACKGROUND,BLEND=comb
end

;=============================================================================
;  ViewRecord - View the record(s) in an existing or new viewer
;=============================================================================
pro CubeProj::ViewRecord,rec,NEW_VIEWER=new,_EXTRA=e
  self->RecOrSelect,rec,_EXTRA=e
  self->RestoreData,rec
  self->FindViewer,NEW_VIEWER=new
  self->Send,RECORD=rec,_EXTRA=e
end

;=============================================================================
;  VisualizeAORs - View the AORs on top of a vis image, with select
;=============================================================================
pro CubeProj::VisualizeAORs,NEW_VIEWER=new
  self->Normalize
  self->LoadVisualize
  if ~ptr_valid(self.visualize_image) then return
  self->FindViewer,/VISUALIZE_MODE,NEW_VIEWER=new
  self->Send,/VISUALIZE
end
  
;=============================================================================
;  LoadVisualize - Ensure a visualization image is loaded
;=============================================================================
pro CubeProj::LoadVisualize,SELECT=sel
  ;; Go to the cube directory first, for relative file-names
  if self.savefile then cd,file_dirname(self.savefile)
  
  if ~keyword_set(sel) then begin 
     if ptr_valid(self.visualize_image) && ptr_valid(self.visualize_header) $
     then return
     if ~file_test(self.visualize_file) then sel=1
  endif
  
  if keyword_set(sel) then begin 
     xf,vis_file,/RECENT,FILTERLIST=['*.fits{,.gz}'], $
        TITLE='Load Visualization Image',/NO_SHOW_ALL,SELECT=0,/MODAL, $
        PARENT_GROUP=self->TopBase()
     if size(vis_file,/TYPE) ne 7 then return
     if vis_file ne self.visualize_file then begin 
        self.visualize_file=vis_file
        self->SetDirty
     endif 
  endif 
  
  catch,err
  if ~file_test(self.visualize_file) || err then begin 
     if n_elements(fcb) ne 0 then fits_close,fcb,/NO_ABORT
     self->Error,["Couldn't load visualization image:",self.visualize_file]
  endif 
  fits_open,self.visualize_file, fcb,/NO_ABORT
  
  if fcb.nextend gt 1 then begin 
     good=where(fcb.naxis eq 2,cnt)
     if cnt eq 0 then self->Error,'No image data found: ',self.visualize_file
     
     exts=strtrim(fcb.extname[good],2)
     if good[0] eq 0 and ~exts[0] then exts[0]='Main (PDU)'
     wh=where(~exts,cnt)
     if cnt gt 0 then exts[wh]='EXT '+strtrim(good[wh],2)
     ext=popup('Choose an extension to load: ',exts,INDEX=ind, $
               PARENT_GROUP=self->TopBase(),/MODAL, $
               TITLE='Visualization Image Extension')
     ext=good[ind]
     fits_read,fcb,im,hdr,EXTEN_NO=ext
  endif else fits_read,fcb,im,hdr
  fits_close,fcb
  catch,/cancel
  
  if size(im,/N_DIMENSIONS) ne 2 then $
     self->Error,"Failed to read visualization image."
  
  ptr_free,self.visualize_image,self.visualize_header
  self.visualize_image=ptr_new(im,/NO_COPY)
  self.visualize_header=ptr_new(hdr,/NO_COPY)
  
  self->UpdateTitle & self->UpdateButtons
end  

;=============================================================================
;  VisualizeAstrom - Return the visualization astrometry from the vis
;                    header.
;=============================================================================
function CubeProj::VisualizeAstrom
  if ~ptr_valid(self.visualize_header) then return,-1
  
  hdr=*self.visualize_header
  ;; First precess if necessary
  if get_equinox(hdr) ne 2000.0 then $
     hprecess,hdr,2000.0
 
  extast,hdr,astr,code
  if code eq 4 then begin       ;convert from gss to normal header
     gsss_stdast,hdr
     extast,hdr,astr
  endif 
  
  ;; Take care of non-celestial images (e.g. Galactic)
  if strpos(astr.ctype[0],'RA') ne 0 || strpos(astr.ctype[1],'DEC') ne 0 then $
     heuler,astr,/CELESTIAL
  
  return,astr
end

;=============================================================================
;  EnableRecord - Remove the records disable flag
;=============================================================================
pro CubeProj::EnableRecord,recs
  self->DisableRecord,recs,DISABLE=0b
end

;=============================================================================
;  DisableRecord - Set the records disable flag, so that it doesn't
;                  get used when building the cube.
;=============================================================================
pro CubeProj::DisableRecord,recs,DISABLE=dis,_EXTRA=e
  if NOT ptr_valid(self.DR) then return
  if n_elements(dis) eq 0 then dis=1b
  self->RecOrSelect,recs,_EXTRA=e
  if recs[0] eq -1 then return
  (*self.DR)[recs].DISABLED=dis
  self->SetDirty
  self->Send,/REC_UPDATE,/DISABLED
  self->UpdateAll
end

;=============================================================================
;  RenameRecord - Rename a record
;=============================================================================
pro CubeProj::RenameRecord,rec,name
  self->RecOrSelect,rec
  if n_elements(name) eq 0 AND self->IsWidget() then begin 
     name=getinp('New Name:',(*self.DR)[rec[0]].ID,TITLE="Rename Record", $
                 PARENT_GROUP=self->TopBase(),/MODAL)
     if name eq '' then return
  endif
  wh=where((*self.DR).ID eq name,cnt)
  if cnt ne 0 AND wh[0] ne rec[0] then $
     self->Error,'ID '+name+' already exists'
  (*self.DR)[rec[0]].ID=name
  self->SetDirty
  self->UpdateAll
end

;=============================================================================
;  SwitchRecordDataType - Switch the data type of a set of records,
;                         without losing any account information, etc.
;=============================================================================
pro CubeProj::SwitchRecordDataType,r,FLATAP=f2ap,BCD=bcd,DROOPRES=dr,_EXTRA=e
  if ~ptr_valid(self.DR) then return
  self->RecOrSelect,r,_EXTRA=e
  
  recs=(*self.DR)[r]
  
  all_types=["BCD","DroopRes","Coad2d","FlatAp"]
  if ~array_equal(recs.type ne 2,1b) then self->Error,'Cannot convert Coad2d'

  if array_equal(recs.type,recs[0].type) then begin 
     all_type=recs[0].type
     types=all_types[where(indgen(4) ne all_type AND all_types ne "Coad2d")]
  endif 
  
  if keyword_set(f2ap) then new_type=3
  if keyword_set(bcd) then new_type=0
  if keyword_set(dr) then new_type=1
  
  if n_elements(new_type) eq 0 then begin 
     which=popup('Switch Records to Type:',types,TITLE='Switch Record Type', $
                 PARENT_GROUP=self->TopBase(),/MODAL,SELECT=0)
     if which eq "Coad2d" then self->Error,'Cannot convert Coad2d'
     new_type=where(all_types eq which)
  endif 
  
  if n_elements(all_type) gt 0 && new_type eq all_type then return
  
  new_files=irs_associated_file(recs.file,FLATAP=new_type eq 3, $
                                DROOPRES=new_type eq 1)
  if ~array_equal(new_files ne '',1b) then $
     self->Error,'Alternate data files not found.'
  
  (*self.DR)[r].FILE=new_files
  (*self.DR)[r].TYPE=new_type[0]
  
  ptr_free,(*self.DR)[r].BCD
  self->RestoreData,r
  if self.BACK_DATE then begin 
     self->Info,'Rebuilding background from '+jul2date(self.BACK_DATE)
     self->RebuildBackground,/NO_UPDATE
  endif 
  self->SetDirty
  self->UpdateAll
end

;=============================================================================
;  SetBackgroundFromRecs - Set the BCD background from selected recs.
;=============================================================================
pro CubeProj::SetBackgroundFromRecs,recs, BLEND=comb, SET_ONLY=so, $
                                    REJECT_MIN_MAX=rmm,_EXTRA=e
  self->RecOrSelect,recs,_EXTRA=e
  if recs[0] eq -1 then return
  self->RestoreData,recs
  n=n_elements(recs) 
  
  if n_elements(rmm) eq 0 && self->IsWidget() then begin 
     if n ge 3 then begin 
        list=["Average","Average + Min/Max Trim"]
        sel=1 
     endif else begin 
        list=["Average"]
        sel=0
     endelse 
     choice=multchoice('Create background from '+strtrim(n,2)+' recs using:', $
                       list,TITLE='Set BCD Background', $
                       PARENT_GROUP=self->TopBase(),/MODAL,SELECT=n gt 2)
     choice=choice[0]
     if choice eq -1 then return
  endif else begin 
     choice=keyword_set(rmm) 
  endelse
  
  ;; Cube of BCD data
  bcds=self->BCD(recs,UNCERTAINTY=unc)
  
  if n le 2 && choice eq 1 then $
     self->Warning,'Fewer than 3 BG records -- reverting to average.'
  back=imcombine(bcds,UNCERTAINTY=unc,COMBINED_UNCERTAINTY=back_unc,/AVERAGE, $
                 REJECT_MINMAX=n gt 2 && choice eq 1)

  if n_elements(comb) gt 0  then begin 
     ;; Blend backgrounds
     pos=keyword_set(comb)
     if ptr_valid(self.BACK_RECS) && $
        size(*self.BACK_RECS,/TYPE) ne 8 then begin 
        if self.BACK_RECS eq self.AS_BUILT.BACK_RECS then $
           self.BACK_RECS=ptr_new() else ptr_free,self.BACK_RECS
     endif 
     
     ;; When scale-combining, use an array of two structures
     if ~ptr_valid(self.BACK_RECS) then begin 
        self.BACK_RECS=ptr_new(replicate({BACKGROUND:ptr_new(), $
                                          BACK_UNC:ptr_new(), $
                                          FIDUCIAL: 0.0, $
                                          SCALE: 0.0, $
                                          DCEIDS:ptr_new()},2)) 
     endif else heap_free,(*self.BACK_RECS)[pos]
     
     (*self.BACK_RECS)[pos].BACKGROUND=ptr_new(back,/NO_COPY)
     if n_elements(back_unc) ne 0 then $
        (*self.BACK_RECS)[pos].BACK_UNC=ptr_new(back_unc,/NO_COPY)     
     (*self.BACK_RECS)[pos].DCEIDs=ptr_new((*self.DR)[recs].DCEID)
     
     if keyword_set(so) then return ;just set the relevant background
     ;;self->ViewBackground,BLEND=pos
     
     widget_control, (*self.wInfo).background_menu,SENSITIVE=0
     val=getinp('Fiducial Background Value:',0.0, $
                TITLE="Background Combine "+(['A','B'])[pos], $
                PARENT_GROUP=self->TopBase(),/MODAL)
     if ~val then begin         ;cancelled
        heap_free,(*self.BACK_RECS)[pos]
     endif else (*self.BACK_RECS)[pos].FIDUCIAL=float(val)
     widget_control, (*self.wInfo).background_menu,/SENSITIVE
  endif else begin 
     ;; Straight averages
     ptr_free,self.BACKGROUND
     self.BACKGROUND=ptr_new(back,/NO_COPY)
     if n_elements(back_unc) ne 0 then $
        self.BACKGROUND_UNC=ptr_new(back_unc,/NO_COPY)
     self.BACK_DATE=systime(/JULIAN)
     self->SetDirty,/SAVE,/BACKGROUND
     if self.BACK_RECS ne self.AS_BUILT.BACK_RECS then $
        heap_free,self.BACK_RECS
     self.BACK_RECS=ptr_new((*self.DR)[recs].DCEID)
  endelse 
  self->UpdateButtons
end

;=============================================================================
;  BlendBackgrounds - Set the background from the scaling
;=============================================================================
pro CubeProj::BlendBackgrounds,USE_EXISTING_SCALES=ue, ASCALE=ascale, $
                               BSCALE=bscale, TARGET_VALUE=tval
  if ~ptr_valid(self.BACK_RECS) then return
  if size(*self.BACK_RECS,/TYPE) ne 8 then return
  
  if ~array_equal(ptr_valid((*self.BACK_RECS).BACKGROUND),1b) then return
  
  if ~keyword_set(ue) then begin 
     catch,err
     if err ne 0 then begin 
        self->Warning,['Invalid Target Fiducial Value:',!ERROR_STATE.MSG]
     endif else begin 
        aval=(*self.BACK_RECS)[0].FIDUCIAL
        bval=(*self.BACK_RECS)[1].FIDUCIAL
        if n_elements(tval) eq 0 then $
           tval=getinp( $
                string(FORMAT='(%"Fiducial Target Value (%0.2f-%0.2f):")', $
                       aval<bval,aval>bval), 0.0, $
                PARENT_GROUP=self->TopBase(),/MODAL, $
                TITLE="Background Combination")
        if ~tval then message,'No Target Value Selected',/NONAME
        tval=float(tval)
        
        if ~( (tval le aval && tval ge bval) || $
              (tval ge aval && tval le bval) ) then $
                 message,'Value must be between two background fiducials', $
                         /NONAME
        
        ;; Use fiducials and target value to compute blend fractions
        bscale=(tval-aval)/(bval-aval)
        ascale=(bval-tval)/(bval-aval)
        (*self.BACK_RECS).SCALE=[ascale,bscale]
        
        na=n_elements(*(*self.BACK_RECS)[0].DCEIDs)
        nb=n_elements(*(*self.BACK_RECS)[1].DCEIDs)
        self->Info,TITLE='Background Combination', $
                   ['Created scaled background with: ', $
                    string(FORMAT='("A: ",I3," records scaled at ",F0.2)', $
                           na,ascale), $
                    string(FORMAT='("B: ",I3," records scaled at ",F0.2)', $
                           nb,bscale)]
        self->SetDirty,/SAVE,/BACKGROUND
     endelse 
     catch,/cancel
  endif else begin 
     if n_elements(ascale) eq 0 then ascale=(*self.BACK_RECS)[0].SCALE $
     else (*self.BACK_RECS)[0].SCALE=ascale
     if n_elements(bscale) eq 0 then bscale=(*self.BACK_RECS)[1].SCALE $
     else (*self.BACK_RECS)[1].SCALE=bscale
  endelse 
  
  if n_elements(ascale) eq 0 then return
  
  ;; Create scaled background
  back=ascale*(*(*self.BACK_RECS)[0].BACKGROUND)+ $
       bscale*(*(*self.BACK_RECS)[1].BACKGROUND)
  if array_equal(ptr_valid((*self.BACK_RECS).BACK_UNC),1b) then $
     back_unc=sqrt(ascale^2 * (*(*self.BACK_RECS)[0].BACK_UNC)^2 + $
                   bscale^2 * (*(*self.BACK_RECS)[1].BACK_UNC)^2)
  
  ptr_free,self.BACKGROUND,self.BACKGROUND_UNC
  self.BACKGROUND=ptr_new(back,/NO_COPY)
  if n_elements(back_unc) ne 0 then $
     self.BACKGROUND_UNC=ptr_new(back_unc,/NO_COPY)
  self.BACK_DATE=systime(/JULIAN)
  self->UpdateButtons
end

;=============================================================================
;  RebuildBackground - Rebuild Background from records
;=============================================================================
pro CubeProj::RebuildBackground,NO_UPDATE=noupdate
  if ~ptr_valid(self.BACK_RECS) then return
  if size(*self.BACK_RECS,/TYPE) eq 8 then begin 
     if ~array_equal(ptr_valid((*self.BACK_RECS).DCEIDs),1b) then $
        self->Error,'Blended Background only partially set'
     for pos=0,1 do begin ;; Set the two backgrounds, leaving scaling alone
        list=self->DCEIDtoRec(*(*self.BACK_RECS)[pos].DCEIDs)
        self->SetListSelect,list
        self->SetBackgroundFromRecs,list,BLEND=pos,/SET_ONLY
     endfor 
     self->BlendBackgrounds,/USE_EXISTING_SCALES
  endif else begin 
     list=self->DCEIDtoRec(*self.BACK_RECS)
     self->SetListSelect,list
     self->SetBackgroundFromRecs,list
  endelse 
  self->SetDirty
  if ~keyword_set(noupdate) then self->UpdateTitle
end

;=============================================================================
;  ReadBackgroundFromFile - Read in a background spectrum file
;=============================================================================
pro CubeProj::ReadBackgroundFromFile,file
  if ptr_valid(self.BACKGROUND) then $
     self->Warning,['A record-based background is already set;', $
                    'both backgrounds will be used on cube rebuild.']
  
  if size(file,/TYPE) ne 7 then begin 
     xf,file,/RECENT,FILTERLIST=['*.{tbl,fits}','*.*','*'],$
        TITLE='Load Background Spectrum',/NO_SHOW_ALL,SELECT=0, $
        /MODAL,PARENT_GROUP=self->TopBase()
     if size(file,/TYPE) ne 7 then return
  endif 
     
  oSP=obj_new('IRS_Spectrum')
  oSP->Read,file
  oSP->GetProperty,WAVE_UNITS=wu,FLUX_UNITS=fu,WAVELENGTH=wav,SPECTRUM_FLUX=sp
  obj_destroy,oSP
  
  self.BG_SP_TYPE=~stregex(fu,'e/s',/BOOLEAN) ; unfluxed (0b) or fluxed (1b)
  ptr_free,self.BG_SP
  self.BG_SP=ptr_new(transpose([[wav],[sp]]))
  self.BG_SP_FILE=file
  self->SetDirty,/SAVE,/BACKGROUND
  self->UpdateButtons
end

;=============================================================================
;  CurrentSelect - Return the currently selected records (if any).
;=============================================================================
function CubeProj::CurrentSelect,DCEID=dceid
  if ~ptr_valid(self.wInfo) then return,-1
  sel=widget_info((*self.wInfo).SList, /LIST_SELECT)
  if keyword_set(dceid) && sel[0] ne -1 then $
     sel=self->DCEIDs(sel)
  return,sel
end

;=============================================================================
;  RecOrSelect - Convenience routine: use specified records, or take
;                from the GUI selection.
;=============================================================================
pro CubeProj::RecOrSelect,recs,ALL=all,DCEID=dceid
  if keyword_set(all) then begin 
     recs=lindgen(self->N_Records())
  endif else if n_elements(recs) eq 0 then begin 
     recs=self->CurrentSelect()
     if recs[0] eq -1 then $
        self->Error,'No Records Selected or Passed'
  endif else if keyword_set(dceid) then begin 
     recs=self->DCEIDtoRec(recs)
  endif 
end

;=============================================================================
;  DCEIDs - Give the DCEID of the given recs
;=============================================================================
function CubeProj::DCEIDs,recs,_EXTRA=e
  self->RecOrSelect,recs,_EXTRA=e
  if ~ptr_valid(self.DR) then return,-1
  if n_elements(recs) eq 0 then return,-1
  return,(*self.DR)[recs].DCEID
end

;=============================================================================
;  DCEIDtoRec - Convert a list of DCEIDs back to a record
;                 selection list.
;=============================================================================
function CubeProj::DCEIDtoRec,dceid,cnt,RECORDS=recs
  if ~ptr_valid(self.DR) then return,-1
  if n_elements(recs) ne 0 then $
     return,where_array([dceid],[(*self.DR)[recs].DCEID],cnt,/PRESERVE_ORDER) $
  else return,where_array([dceid],(*self.DR).DCEID,cnt,/PRESERVE_ORDER)
end

;=============================================================================
;  Sort - Sort the Data records. 
;=============================================================================
pro CubeProj::Sort,sort
  n=self->N_Records()
  if n le 1 then return         ;no sort for one only
  case self.sort of
     0:  s=sort((*self.DR).ID)
     1:  s=sort((*self.DR).TIME)
     2:  s=sort((*self.DR).DATE_OBS)
     3:  s=sort((*self.DR).DATE)
     4:  s=sort(long((*self.DR).FOVID)+ishft(long((*self.DR).TYPE),16))
     5:  s=sort((*self.DR).EXP)
     6:  s=sort((*self.DR).OBJECT)
     7:  s=sort(self.reconstructed_pos?(*self.DR).REC_POS[0]: $
                (*self.DR).RQST_POS[0])
     8:  s=sort(self.reconstructed_pos?(*self.DR).REC_POS[1]: $
                (*self.DR).RQST_POS[1])
     9:  s=sort(ptr_valid((*self.DR).BCD))
     10: s=sort(ptr_valid((*self.DR).UNC))
     11: s=sort(ptr_valid((*self.DR).BMASK))
     12: s=sort(ptr_valid((*self.DR).ACCOUNT))
     13: begin 
        nbpl=lonarr(n)
        for i=0,n-1 do nbpl[i]=ptr_valid((*self.DR)[i].BAD_PIXEL_LIST)? $
           n_elements(*(*self.DR)[i].BAD_PIXEL_LIST):0L
        s=sort(nbpl)
     end 
  endcase
  *self.DR=(*self.DR)[s]        ;rearrange
  if self->IsWidget() then begin 
     ;; Preserve selections
     ls=self->CurrentSelect()<(n-1)
     if ls[0] eq -1 then return
     b=bytarr(n)
     b[ls]=1b
     b=b[s]
     self->SetListSelect,where(b),/NO_UPDATE,/NO_STATUS
  endif 
end

pro CubeProj::Print,entries,_EXTRA=e
  print,transpose(self->Info(entries,_EXTRA=e))
end

;=============================================================================
;  Info - Info on the cube's contents and history
;=============================================================================
function CubeProj::Info,entries, NO_DATA=nd,AS_BUILT=as_built
  ;; Return either the object data, or the as-built, cached data
  this=keyword_set(as_built)?self.AS_BUILT:self
  
  str=['IRS Spectral Cube: '+self->ProjectName()+ $
       ((~keyword_set(as_built) && self->Dirty(/ACCOUNTS,/BACKGROUND))? $
        " (needs rebuilding)":"")]
  
  str=[str,string(FORMAT='("Cube Size: ",I0.3,"x",I0.3,"x",I0.3,", ' + $
                  'Center: ",A," ",A)', $
                  this.cube_size, $
                  radecstring(self.POSITION[0],/RA), $
                  radecstring(self.POSITION[1]))]
  
  if keyword_set(as_built) then begin 
     str=[str,'Cube Created: '+ $
          (self.CUBE_DATE eq 0.0d?"(not yet)":jul2date(self.CUBE_DATE))]
     if self.CUBE_DATE eq 0.0d then return,str
  endif 
  str=[str,$
       (this.MODULE?this.MODULE:"(no module)") + " " + $
       (this.ORDER ne 0?'Order '+strtrim(this.ORDER,2):'all orders') + $
       ' -- '+strtrim(self->N_Records(/ENABLED),2)+' BCDs ']
  
  str=[str,'Using IRS Calib object: '+(this.CAL_FILE?this.cal_file:"(none)")+ $
       " ("+(obj_valid(self.cal)?"":"not ")+"loaded"+")"]
  
  if this.BACK_DATE ne 0.0d then begin 
     if ptr_valid(this.BACK_RECS) then begin 
        if size(*this.BACK_RECS,/TYPE) eq 8 then begin 
           desc=string(FORMAT='(%"Blended: %d recs @%0.2f, %d recs @%0.2f")', $
                       n_elements(*(*this.BACK_RECS)[0].DCEIDS), $
                       (*this.BACK_RECS)[0].SCALE, $
                       n_elements(*(*this.BACK_RECS)[1].DCEIDS), $
                       (*this.BACK_RECS)[1].SCALE)
        endif else desc=strtrim(n_elements(*this.BACK_RECS),2)+' records'
     endif else desc='0 records'
     
     str=[str,('Background: '+ $
               desc + ', '+jul2date(this.BACK_DATE))+ $
          (this.use_bg?"":" (disabled)")]
  endif 
  if this.BG_SP_FILE then begin 
     str=[str,'Background: 1D from file'+ $
          ' ('+(this.BG_SP_TYPE?"fluxed":"raw e/s")+ $
               (this.use_bg?")":",disabled)")+ $
          ' --',$
          '   '+this.BG_SP_FILE]
  endif 
  
  if this.BACK_DATE eq 0.0d && this.BG_SP_FILE eq '' then $
     str=[str,'Background: none']
  
  str=[str,'FLUXCON: '+(this.FLUXCON?"Yes":"No")]
  str=[str,'   SLCF: '+(this.SLCF?"Yes":"No")]
  str=[str,'WAVECUT: '+(this.wavecut?"Yes":"No")]
  str=[str,'UNCERTAINTY: '+(this.use_unc?"Yes":"No")]
  str=[str,'Positions: '+(this.RECONSTRUCTED_POS?"Reconstructed":"Requested")]
  
  nbadpix=ptr_valid(this.GLOBAL_BAD_PIXEL_LIST)? $
          n_elements(*this.GLOBAL_BAD_PIXEL_LIST):0
  
  nrecbadpix=0
  if ptr_valid(self.DR) then begin 
      wh=where(ptr_valid((*self.DR).BAD_PIXEL_LIST),recbp_cnt)
      for i=0,recbp_cnt-1 do $
        nrecbadpix+=n_elements(*(*self.DR)[wh[i]].BAD_PIXEL_LIST)
  endif 
  
  bp_string='Bad Pixels: '
  
  if nbadpix+nrecbadpix eq 0 then bp_string+='none' else begin 
     if nbadpix gt 0 then $
        bp_string+='Global: '+strtrim(nbadpix,2)+(nrecbadpix gt 0?' ; ':'')
     if nrecbadpix gt 0 then $
        bp_string+=string(FORMAT='(%"Record: %d from %d recs")', $
                          nrecbadpix,recbp_cnt)
  endelse 
                                               
  str=[str,bp_string]
  
  if nbadpix gt 0 && $
     (this.GLOBAL_BP_TYPE gt 0b || ptr_valid(this.GLOBAL_BP_FILES)) then begin 
     str=[str,'Bad Pixel Sources:']
     if this.GLOBAL_BP_TYPE AND 1b then str=[str,' <By Hand>']
     if (this.GLOBAL_BP_TYPE AND 2b) ne 0b then str=[str,' <Auto BadPix>']
     if ptr_valid(this.GLOBAL_BP_FILES) then $
        str=[str,'  '+*this.GLOBAL_BP_FILES]
  endif 
  
  if this.GLOBAL_BP_SAVEFILE then $
     str=[str,'Bad Pixels Saved To'+ $
          (this.GLOBAL_BP_SAVEFILE_UPTODATE?':':' (outdated):'), $
          '  '+this.GLOBAL_BP_SAVEFILE]
  
  aps='Apertures:'
  if NOT ptr_valid(this.APERTURE) OR this.MODULE eq '' then begin 
     aps=aps+' (default)' 
  endif else begin 
     nap=n_elements(*this.APERTURE)
     ords=obj_valid(self.cal)?self.cal->Orders(this.MODULE):intarr(nap)+1
     for i=0,nap-1 do begin 
        ap=(*this.APERTURE)[i]
        aps=[aps,string(FORMAT='(%" %s  %4.2f->%4.2f : %4.2f->%4.2f")',$
                        (nap eq 1?"All Orders":("Order "+strtrim(ords[i],2))),$
                        ap.low,ap.high)]
     endfor 
  endelse 
  str=[str,aps]

  str=[str, $
       ' '+string(FORMAT='(I0,"x",I0," steps = ",F7.3," x ",F7.3,' + $
                  '" arcsec"," (",F6.3," arcsec/pixel)")',this.NSTEP, $
                  this.STEP_SIZE*3600.0D*(this.NSTEP-1), $
                  this.PLATE_SCALE*3600.0D/this.OVERSAMPLE_FACTOR)]
  str=[str,string(FORMAT='("PR Sample Size: ",F6.3," x ",F6.3," pixels")',$
                      this.PR_SIZE)]
  
  if keyword_set(nd) then return,str
  if NOT ptr_valid(self.DR) then begin
     str=[str,'No data']
     return,str
  endif   
  str=[str,' ===== DATA =====']
  if n_elements(entries) eq 0 then entries=lindgen(n_elements(*self.DR))
  for i=0,n_elements(entries)-1 do begin 
     rec=(*self.DR)[entries[i]]
     str=[str,rec.id+":"+rec.file]
     pos=self.reconstructed_pos?rec.REC_POS:rec.RQST_POS
     sign=pos[1] ge 0.0?'+':'-'
     radec,pos[0],abs(pos[1]),rh,rm,rs,dd,dm,ds
     ra=string(FORMAT='(I0,"h",I2.2,"m",F5.2,"s")',rh,rm,rs)
     dec=sign+string(FORMAT='(I0,"d",I2.2,"m",F5.2,"s")',abs(dd),dm,ds)
     str=[str,string(FORMAT='(%"  EXP: %2.0d (%d/%d) (%2.0d,%2.0d) ' + $
                     'RA: %s, DEC: %s")', $
                     rec.EXP,rec.CYCLE+1,rec.NCYCLES,rec.ROW,rec.COLUMN, $
                     ra,dec)]
  endfor 
  return,str
end

;=============================================================================
;  Dirty - With keyword, check with accounts, size, or background (any
;          set) are invalid, otherwise check if the Changed for save
;          is set.
;=============================================================================
function CubeProj::Dirty,ACCOUNTS=acc,LAYOUT=lo,BACKGROUND=bkg
  if keyword_set(acc) || keyword_set(lo) || keyword_set(bkg) then begin 
     test=0b
     if keyword_set(acc) then test OR=1b
     if keyword_set(lo)  then test OR=2b
     if keyword_set(bkg) then test OR=4b
     return,(self.ACCOUNTS_VALID AND test) ne test
  endif else return,self.Changed
end

;=============================================================================
;  SetDirty - Set the accounts, size, background status, and/or save
;             changed status as dirty.  Without keywords, set the save
;             changed status.
;=============================================================================
pro CubeProj::SetDirty,ACCOUNTS=acc,LAYOUT=lo,BACKGROUND=bkg, SAVE=save
  if keyword_set(acc) || keyword_set(lo) || keyword_set(bkg) then begin 
     if keyword_set(acc) then self.ACCOUNTS_VALID AND= NOT 1b
     if keyword_set(lo)  then self.ACCOUNTS_VALID AND= NOT 2b
     if keyword_set(bkg) then self.ACCOUNTS_VALID AND= NOT 4b
     if keyword_set(acc) || keyword_set(bkg) then self->ClearDirtyPix
     if keyword_set(save) then self.Changed=1b
  endif else self.Changed=1b
end


;=============================================================================
;  SetClean - Set the accounts, size, background status, and/or save
;             changed status as clean.  Without keywords, remove the
;             save changed status.  Clears the dirty pixel list if
;             accounts of background are set clean.
;=============================================================================
pro CubeProj::SetClean,ACCOUNTS=acc,LAYOUT=lo,BACKGROUND=bkg, SAVE=save
  if keyword_set(acc) || keyword_set(lo) || keyword_set(bkg) then begin 
     if keyword_set(acc) then self.ACCOUNTS_VALID OR= 1b
     if keyword_set(lo)  then self.ACCOUNTS_VALID OR= 2b
     if keyword_set(bkg) then self.ACCOUNTS_VALID OR= 4b
     if keyword_set(acc) || keyword_set(bkg) then self->ClearDirtyPix
     if keyword_set(save) then self.Changed=0b     
  endif else self.Changed=0b
end

;=============================================================================
;  SetProperty - Set various cube properties.  Most of these should be
;                automatically discovered from the BCD's.  If any are
;                actually changed, indicate that the account is no
;                longer valid.
;=============================================================================
pro CubeProj::SetProperty,OVERSAMPLE_FACTOR=osf,NSTEP=nstep, $
                          STEP_SIZE=stepsz, $
                          MODULE=md,ORDER=ord, PR_WIDTH=prw, $
                          PR_SIZE=prz,CAL_FILE=cal_file,CAL_OBJECT=cal, $
                          APERTURE=aper,SAVE_FILE=sf, $
                          PROJECTNAME=pn,SPAWNED=spn,FEEDBACK=fb, $
                          GLOBAL_BAD_PIXEL_LIST=gbpl, WAVECUT=wavecut, $
                          RECONSTRUCTED_POSITIONS=rcp, USE_BACKGROUND=ubg,$
                          USE_UNCERTAINTY=uunc, LOAD_MASKS=lm, $
                          LOAD_UNCERTAINTY=lu,FLUXCON=fc,SLCF=slcf, $
                          PIXEL_OMEGA=po, SAVE_ACCOUNTS=sa,SAVE_DATA=sd, $
                          RELATIVE_FILENAMES=relfile, $
                          DEBUG=debug, _REF_EXTRA=e
  if n_elements(e) ne 0 then begin 
     self->ObjReport::SetProperty,_EXTRA=e
  endif 
     
  update_cal=0b
  if n_elements(osf) ne 0 then begin 
     if self.OVERSAMPLE_FACTOR ne osf then begin 
        self.OVERSAMPLE_FACTOR=osf
        self->ResetAccounts,/NO_UPDATE & self->SetDirty
     endif 
  endif 
  if n_elements(nstep) ne 0 then self.NSTEP=nstep
  if n_elements(stepsz) ne 0 then begin 
     if NOT array_equal(self.STEP_SIZE,stepsz) then begin 
        self.STEP_SIZE=stepsz
        self->ResetAccounts,/NO_UPDATE & self->SetDirty
     endif 
  endif 
  if n_elements(md) ne 0 then begin 
     if md ne self.MODULE then begin
        self.MODULE=md
        self->ResetAccounts,/NO_UPDATE & self->SetDirty
     endif 
  endif 
  if n_elements(ord) ne 0 then begin 
     if ord ne self.ORDER then begin 
        self.ORDER=ord
        self->ResetAccounts,/NO_UPDATE & self->SetDirty
        update_cal=1b
     endif 
  endif 
  if n_elements(prw) ne 0 then begin 
     prw=0.>prw 
     if prw ne self.PR_SIZE[1] then begin 
        self.PR_SIZE[1]=prw
        self->ResetAccounts,/NO_UPDATE & self->SetDirty
        update_cal=1b
     endif
  endif
  if n_elements(prz) ne 0 then begin 
     if NOT array_equal(self.PR_SIZE,prz) then begin 
        self.PR_SIZE=prz
        self->ResetAccounts,/NO_UPDATE & self->SetDirty
        update_cal=1b
     endif
  endif
  if n_elements(cal_file) ne 0 then begin 
     if self.cal_file ne cal_file then begin 
        self.cal_file=cal_file
        self->ResetAccounts,/NO_UPDATE & self->SetDirty
        update_cal=1b
     endif 
  endif
  if n_elements(cal) ne 0 then begin 
     if obj_isa(cal,'IRS_Calib') then begin 
        if self.cal ne cal then begin 
           self.cal=cal 
           self->ResetAccounts,/NO_UPDATE & self->SetDirty
           update_cal=1b
        endif 
     endif else self->Error,'Calibration object not of correct type.'
  endif
  if n_elements(aper) ne 0 then begin 
     if ~self->ApertureEqual(aper) then begin 
        if self.APERTURE ne self.AS_BUILT.APERTURE then ptr_free,self.APERTURE
        self.APERTURE=ptr_new(aper)
        self->ResetAccounts,/NO_UPDATE & self->SetDirty
        update_cal=1b
     endif 
  endif 
  if n_elements(sf) ne 0 then self.SaveFile=sf
  if n_elements(pn) ne 0 then begin 
     pn=strmid(pn,0,32)         ;limit it
     self.ProjectName=pn
     self->SetDirty
  endif
  if n_elements(spn) ne 0 then self.Spawned=spn
  if n_elements(fb) ne 0 then self.feedback=fb
  if n_elements(gbpl) ne 0 then begin
     ptr_free,self.GLOBAL_BAD_PIXEL_LIST
     if bpl[0] ne -1 then self.GLOBAL_BAD_PIXEL_LIST=ptr_new(bpl)
     self->SetDirty
  endif
  
  if n_elements(wavecut) ne 0 then begin 
     if self.wavecut ne wavecut then begin 
        self.wavecut=wavecut
        self->ResetAccounts,/NO_UPDATE & self->SetDirty
        update_cal=1b
     endif
  endif
  
  if n_elements(rcp) ne 0 then begin 
     if self.reconstructed_pos ne keyword_set(rcp) then begin 
        self.reconstructed_pos=keyword_set(rcp) 
        self->ResetAccounts,/NO_UPDATE & self->SetDirty
     endif 
  endif
  
  if n_elements(debug) ne 0 then self.debug=keyword_set(debug) 
  
  if n_elements(ubg) ne 0 then begin 
     self.use_bg=keyword_set(ubg) 
     self->SetDirty
  endif 
  if n_elements(uunc) ne 0 then begin 
     self.use_unc=keyword_set(uunc) 
     self->SetDirty
  endif
  if n_elements(lm) ne 0 then begin 
     self.load_masks=keyword_set(lm) 
     self->SetDirty
  endif 
  if n_elements(lu) ne 0 then begin 
     self.load_unc=keyword_set(lu) 
     self->SetDirty
  endif 
  if n_elements(fc) ne 0 then begin 
     self.fluxcon=keyword_set(fc) 
     self->SetDirty
  endif 
  if n_elements(slcf) ne 0 then begin 
     self.slcf=keyword_set(slcf) 
     self->SetDirty
  endif 
  if n_elements(po) ne 0 then begin 
     self.pix_omega=keyword_set(po) 
     self->SetDirty
  endif 
  if n_elements(sa) ne 0 then $
     self.SaveMethod=(self.SaveMethod AND NOT 2b) OR ishft(keyword_set(sa),1)
  if n_elements(sd) ne 0 then $
     self.SaveMethod=(self.SaveMethod AND NOT 1b) OR keyword_set(sd)
  
  if n_elements(relfile) ne 0 then $
     self.SaveMethod=(self.SaveMethod AND NOT 4b) OR $
                     ishft(keyword_set(relfile),2)
    if update_cal then self->Send,/CALIB_UPDATE
  self->UpdateAll,/NO_LIST
end

;=============================================================================
;  GetProperty - Get properties, as a pointer to the original data if
;                appropriate and keyword POINTER is set.
;=============================================================================
pro CubeProj::GetProperty, $ 
   ;; Pointer or data, depending on POINTER
   POINTER=ptr,WAVELENGTH=wave, CUBE=cube, $
   UNCERTAINTY_CUBE=unc, DR=dr, BACKGROUND=bg, PMASK=pmask, $  
   ;; Global or AS_BUILT options
   AS_BUILT=as_built, GLOBAL_BAD_PIXEL_LIST=gbpl,APERTURE=ap, MODULE=module, $
   ORDER=order, BACK_DATE=bdate, FLUXCON=fc, SLCF=slcf, PIXEL_OMEGA=po, $
   RECONSTRUCTED_POSITIONS=rp,WAVECUT=wavecut, USE_BG=use_bg, $
   USE_UNCERTAINTY=use_unc, CAL_FILE=cf, PR_SIZE=prz, PR_WIDTH=prw, $
   ;; Global non-record based options
   SLIT_LENGTH=sl, CALIB=calib, PROJECT_NAME=pn, CUBE_SIZE=cs,$
   TLB_OFFSET=tboff, TLB_SIZE=tbsize,BCD_SIZE=bcdsz, VERSION=version, $
   ASTROMETRY=astr,POSITION=pos, POSITION_ANGLE=pa, SAVE_DATA=sd, $
   SAVE_ACCOUNT=sa, CUBE_DATE=cdate, $
   ;; Record based property options
   ALL_RECORDS=all_recs,RECORDS=recs, RECORD_SET=rec_set, DATE_OBS=dobs, $
   ACCOUNTS_VALID=av,BAD_PIXEL_LIST=bpl, FILENAMES=fn, DCEID=dceid, $
   DISABLED=dis,  _REF_EXTRA=e
   
  if n_elements(e) ne 0 then begin ;; chain up the inheritance chain
     self->ObjMsg::GetProperty,_EXTRA=e
     self->ObjReport::GetProperty,_EXTRA=e
  endif 
  
  ;;--- Global, potentially pointer-based data
  ptr=keyword_set(ptr)
  if arg_present(wave) && ptr_valid(self.WAVELENGTH) then $
     wave=ptr?self.WAVELENGTH:*self.WAVELENGTH
  if arg_present(cube) && ptr_valid(self.CUBE) then $
     cube=ptr?self.CUBE:*self.CUBE
  if arg_present(unc) && ptr_valid(self.CUBE_UNC) then begin
     if ptr then unc=self.CUBE_UNC else unc=*self.CUBE_UNC
  endif 
  if arg_present(dr) then begin 
     if ~ptr then begin 
        if ptr_valid(self.DR) then dr=*self.DR 
     endif else dr=self.DR
  endif 
  if arg_present(bg) && ptr_valid(self.BACKGROUND) then $
     bg=ptr?self.BACKGROUND:*self.BACKGROUND
  if arg_present(pmask) then begin 
     self->LoadCalib
     self.cal->GetProperty,self.module,PMASK=pmask
     if ~ptr && ptr_valid(pmask) then pmask=*pmask
  endif
 
  
  ;;--- AS_BUILT or current global settings
  this=keyword_set(as_built)?self.AS_BUILT:self
  if arg_present(gbpl) && ptr_valid(this.GLOBAL_BAD_PIXEL_LIST) then $
     gbpl=ptr?this.GLOBAL_BAD_PIXEL_LIST:*this.GLOBAL_BAD_PIXEL_LIST 
  if arg_present(ap) then begin
     self->NormalizeApertures
     ap=ptr?this.APERTURE:*this.APERTURE
  endif  
  if arg_present(module) then module=this.MODULE
  if arg_present(order) then order=this.order  
  if arg_present(bdate) then bdate=this.BACK_DATE
  if arg_present(fc) then fc=this.fluxcon
  if arg_present(slcf) then slcf=this.slcf
  if arg_present(po) then po=this.pix_omega
  if arg_present(rp) then rp=this.reconstructed_pos
  if arg_present(wavecut) then wavecut=this.wavecut
  if arg_present(use_bg) then use_bg=this.use_bg
  if arg_present(use_unc) then use_unc=this.use_unc
  if arg_present(cf) then cf=this.cal_file
  if arg_present(prz) then prz=this.PR_SIZE
  if arg_present(prw) then prw=this.PR_SIZE[1]
  
  
  ;;---  Global, non-build related properties
  if arg_present(sl) then begin 
     self->LoadCalib
     self.cal->GetProperty,self.module,self.order,SLIT_LENGTH=sl
  endif 
  if arg_present(calib) then begin 
     self->LoadCalib            ;ensure it's loaded
     calib=self.cal
  endif 
  if arg_present(pn) then pn=self->ProjectName()
  if arg_present(tboff) OR arg_present(tbsize) then begin 
     if NOT ptr_valid(self.wInfo) then begin 
        tboff=(tbsize=-1)
     endif else begin 
        if widget_info((*self.wInfo).Base,/VALID_ID) then begin 
           geom=widget_info((*self.wInfo).Base,/GEOMETRY) 
           tboff=[geom.xoffset,geom.yoffset]
           tbsize=[geom.xsize,geom.ysize]
        endif else begin 
           tboff=(tbsize=-1)
        endelse 
     endelse 
  endif
  if arg_present(cs) then cs=self.CUBE_SIZE
  if arg_present(tbsize) then begin 
     if ~ptr_valid(self.wInfo) then tbsize=-1 else $
        if widget_info((*self.wInfo).Base,/VALID_ID) then $
           widget_control, (*self.wInfo).Base,TLB_GET_SIZE=tbsize $
        else tbsize=-1
  endif
  if arg_present(version) then version=self.version
  if arg_present(astr) then astr=self->CubeAstrometryRecord()
  if arg_present(pos) then pos=self.POSITION
  if arg_present(pa) then pa=self.PA
  if arg_present(sd) then sd=(self.SaveMethod AND 1b) ne 0b
  if arg_present(sa) then sa=(self.SaveMethod AND 2b) ne 0b
  if arg_present(bcdsz) then begin 
     ;; Assume all the same BCD sizes
     if ~ptr_valid(self.DR) then bcdsz=-1 $
     else begin 
        wh=where(ptr_valid((*self.DR).HEADER),cnt)
        if cnt eq 0 then bcdsz=-1 else $
           bcdsz=[sxpar(*(*self.DR)[wh[0]].HEADER,'NAXIS*')]
     endelse 
  endif 
  if arg_present(cdate) then cdate=self.CUBE_DATE
  
  
  ;;--- Record-based properties
  if ~ptr_valid(self.DR) then return
  if n_elements(recs) eq 0 then begin 
     if keyword_set(all_recs) then recs=lindgen(self->N_Records()) $
     else if keyword_set(rec_set) then begin 
        recs=self->DCEIDtoRec(rec_set)
        if recs[0] eq -1 then self->Error,'No matching record found'
     endif 
  endif 
  ;; Default to all of them
  if n_elements(recs) eq 0 then recs=lindgen(self->N_Records())
  if arg_present(dobs) then dobs=(*self.DR)[recs].DATE_OBS
  
  if arg_present(bpl) then begin 
     if ptr then begin 
        ;; Just hand the first pointer
        bpl=(*self.DR)[recs[0]].BAD_PIXEL_LIST 
     endif else begin 
        ;; Concatenate all of the bad pixels
        first=1
        for i=0,n_elements(recs)-1 do begin 
           this_bpl=(*self.DR)[recs[i]].BAD_PIXEL_LIST
           if ~ptr_valid(this_bpl) then continue
           if first then begin 
              bpl=*this_bpl 
              first=0
           endif else bpl=[bpl,*this_bpl]
        endfor 
     endelse 
  endif 
  if arg_present(av) then av=ptr_valid((*self.DR)[recs].ACCOUNT)
  if arg_present(fn) then fn=(*self.DR)[recs].file
  if arg_present(dis) then dis=(*self.DR)[recs].DISABLED
  if arg_present(dceid) then dceid=(*self.DR)[recs].DCEID
end

;=============================================================================
;  GetKeyword - Return the keyword values of a given keyword for the
;               specified recs.
;=============================================================================
function CubeProj::GetKeyword,keyword,recs,_EXTRA=e
  self->RecOrSelect,recs,_EXTRA=e
  self->RestoreData,recs
  val=sxpar(*(*self.DR)[recs[0]].HEADER,keyword,/SILENT,COUNT=cnt)
  if cnt eq 0 then self->Error,"No matching keyword found: "+keyword
  vals=make_array(n_elements(recs),TYPE=size(val,/TYPE))
  vals[0]=val
  for i=1,n_elements(recs)-1 do $
     vals[i]=sxpar(*(*self.DR)[recs[i]].HEADER,keyword,/SILENT)
  return,vals
end

;=============================================================================
;  PRs - Return the WAVSAMP Pseudo-Rectangle Samples currently
;        set. ORDERS is input/output.  If passed in, return PRs for
;        the specified orders.  Otherwise, use the orders setup for
;        building the cube, or the full set of orders with ALL_ORDERS,
;        and return through ORDERS.  The array of pointers returned
;        must be freed by the caller (but their contents must be left
;        alone).
;=============================================================================
function CubeProj::PRs,ORDERS=ords,ALL_ORDERS=all,FULL=full,WAVECUT=wc
  self->LoadCalib               ;ensure we have a loaded calibration object
  self->NormalizeApertures
  if n_elements(ords) eq 0 or keyword_set(all) then begin 
     if self.ORDER eq 0 OR keyword_set(all) then $
        ords=self.cal->Orders(self.MODULE) else ords=self.ORDER
  endif
  nap=n_elements(*self.APERTURE) 
  nords=n_elements(ords) 
  prs=ptrarr(nords)
  for i=0,nords-1 do begin
     ap=nap eq 1?(*self.APERTURE)[0]:(*self.APERTURE)[i]
     prs[i]=ptr_new(self.cal->GetWAVSAMP( $
            self.MODULE,ords[i],/PIXEL_BASED,FULL=full,APERTURE=ap, $
            WAVECUT=n_elements(wc) gt 0?keyword_set(wc):self.wavecut, $
            PR_WIDTH=self.PR_SIZE[1] eq 0.0?1.0:self.PR_SIZE[1]))
  endfor 
  return,prs
end

;=============================================================================
;  Cube
;=============================================================================
function CubeProj::Cube,pln,UNCERTAINTY=unc
  if ~ptr_valid(self.CUBE) then return,-1
  if n_elements(pln) ne 0 then begin 
     pln=0>pln<((size(*self.CUBE,/DIMENSIONS))[2]-1)
     if arg_present(unc) && ptr_valid(self.CUBE_UNC) then $
        unc=(*self.CUBE_UNC)[*,*,pln]
     return,(*self.CUBE)[*,*,pln]
  endif else begin
     if arg_present(unc) && ptr_valid(self.CUBE_UNC) then $
        unc=*self.CUBE_UNC
     return,*self.CUBE 
  endelse
end

;=============================================================================
;  BCD
;=============================================================================
function CubeProj::BCD, which,UNCERTAINTY=unc,BMASK=bmask,ALL=all
  if ~ptr_valid(self.DR) then return,-1
  if keyword_set(all) then which=indgen(self->N_Records())
  if ~array_equal(which lt 0 or which ge self->N_Records(),0b) then $
     self->Error,"Invalid record number: "+strjoin(strtrim(which,2),",")
  nw=n_elements(which) 
  if nw eq 1 then begin 
     if arg_present(unc) && ptr_valid((*self.DR)[which].UNC) then $
        unc=*(*self.DR)[which].UNC
     if arg_present(bmask) && ptr_valid((*self.DR)[which].BMASK) then $
        bmask=*(*self.DR)[which].BMASK
     return,*(*self.DR)[which].BCD
  endif else begin 
     bcds=(*self.DR)[which].BCD
     s=size(*bcds[0],/DIMENSIONS)
     s=[s,nw]
     ret=make_array(/FLOAT,/NOZERO,s)
     for i=0,nw-1 do ret[0,0,i]=*bcds[i]
     if arg_present(unc) && array_equal(ptr_valid((*self.DR)[which].UNC),1b) $
     then begin 
        unc=make_array(/FLOAT,s,/NOZERO)
        for i=0,nw-1 do unc[0,0,i]=*(*self.DR)[which[i]].UNC
     endif 
     if arg_present(bmask) && $
        array_equal(ptr_valid((*self.DR)[which].BMASK),1b) $
     then begin 
        bmask=make_array(/INTEGER,s)
        for i=0,nw-1 do bmask[0,0,i]=*(*self.DR)[which[i]].BMASK
     endif 
     return,ret
  endelse 
end

;=============================================================================
;  LoadCalib - Ensure the calibration object is loaded and available.
;=============================================================================
pro CubeProj::LoadCalib,SELECT=sel,FORCE=force,NO_RESET=nr,SILENT=silent
  @cubism_dir
  if keyword_set(sel) then begin 
     cd,filepath(ROOT_DIR=irs_calib_dir,"sets")
     if self->IsWidget() then begin 
        xf,calname,/RECENT,FILTERLIST=['*.cal','*.*','*'], $
           TITLE='Load Calibration Object',/NO_SHOW_ALL,SELECT=0,/MODAL, $
           PARENT_GROUP=self->TopBase()
        if size(calname,/TYPE) eq 7 then begin 
           calname=file_basename(calname)
           if calname ne self.cal_file then begin 
              self.cal_file=calname
              self->SetDirty
              self->ResetAccounts,/NO_UPDATE
           endif 
        endif else return
     endif 
  endif else $
     if ~keyword_set(force) && obj_isa(self.cal,'IRS_Calib') then return

  if ~file_test(filepath(ROOT=irs_calib_dir, $
                         SUBDIR="sets",self.cal_file),/READ) $
  then begin  
     if ~keyword_set(silent) then $
        self->Warning,["Can't locate calibration file: ",'  '+self.cal_file, $
                       "Loading most recent"],TITLE='IRS Calibration'
     self.cal_file=""
     specified=1
  endif
  if ~self.cal_file || keyword_set(force) then begin 
     self.cal_file=file_basename(irs_recent_calib()) ;use the most recent
     if ~keyword_set(nr) then self->ResetAccounts,/NO_UPDATE
     self->SetDirty
     if n_elements(specified) eq 0 && ~keyword_set(silent) then $
        self->Info,['Calibration set unspecified, loading most recent: ', $
                    '  '+self.cal_file],TITLE='IRS Calibration'
  endif
  self.cal=irs_restore_calib(self.cal_file)
  self->UpdateButtons & self->UpdateTitle
end

;=============================================================================
;  Flux - Flux a spectrum using FLUXCON, SLCF, and effective pixel
;         solid angle, if SOLID_ANGLE requested.  Input is in e/s/pix.
;=============================================================================
pro CubeProj::Flux,lam,sp,ORDER=ord,SLCF=do_slcf,PIXEL_OMEGA=solid
  if n_elements(ord) eq 0 then ord=self.ORDER
  if ord eq 0 then self->Error,'Cannot flux combined multi-order spectrum.'
  self.cal->GetProperty,self.MODULE,ord,FLUXCON=fluxcon,TUNE=tune, $
                        KEY_WAV=key_wav,SLCF=slcf, $
                        PIXEL_OMEGA=pix_effective_omega, $
                        DATE_OBS=self->MinObservedDate()
  flux_conv=fluxcon*poly(lam-key_wav,tune) ;(e/s)/Jy
  
  if keyword_set(do_slcf) then begin 
     if ptr_valid(slcf) then begin 
        slcf=*slcf
        slcf=interpol(slcf[1,*],slcf[0,*],lam,/LSQUADRATIC)
        sp*=slcf/flux_conv      ;Jy/pix
     endif else sp/=flux_conv
  endif else sp/=flux_conv
  
  if keyword_set(solid) then sp/=pix_effective_omega*1.e6 ; MJy/sr
end


;=============================================================================
;  FluxImage - Make an image to match the BCD which contains, for each
;              pixel, the factor to convert it from e/s/pix to
;              Jy/pixel (or MJy/sr) by
;              multiplication. (fluxed=instrumental*fluximage).  Also
;              modify by the SLCF, if requested.  
;=============================================================================
function CubeProj::FluxImage
  if ~ptr_valid(self.DR) then return,-1
  
  fimage=make_array(size(*(*self.DR)[0].BCD,/DIMENSIONS),VALUE=0.)  
  total_areas=make_array(size(fimage,/DIMENSIONS),VALUE=0.)
  
  prs=self->PRs(/ALL_ORDERS,ORDERS=ords)
  
  for i=0,n_elements(ords)-1 do begin 
     lam=(*prs[i]).lambda
     self.cal->GetProperty,self.MODULE,ords[i],FLUXCON=fluxcon,TUNE=tune, $
                           KEY_WAV=key_wav,SLCF=slcf,PIXEL_OMEGA=pix_omega, $
                           DATE_OBS=self->MinObservedDate()
     
     flux_conv=fluxcon*poly(lam-key_wav,tune) ;(e/s)/Jy
     if ptr_valid(slcf) then begin 
        slcf=*slcf
        slcf=interpol(slcf[1,*],slcf[0,*],lam,/LSQUADRATIC)
     endif else tmp=temporary(slcf)
     
     for j=0,n_elements(lam)-1 do begin 
        fac=1.
        if self.fluxcon then fac/=flux_conv[j] ; now in Jy/px
        if self.slcf && n_elements(slcf) gt 0 then fac*=slcf[j]
        if self.pix_omega gt 0 then fac/=pix_omega*1.e6 ;MJy/sr
        pix=*(*prs[i])[j].pixels
        areas=*(*prs[i])[j].areas
        total_areas[pix]+=areas
        fimage[pix]+=fac*areas
     endfor 
  endfor 
  wh=where(total_areas gt 0.,cnt,COMPLEMENT=wh_none,NCOMPLEMENT=cnt_none)
  if cnt gt 0 then fimage[wh]/=total_areas[wh]
  if cnt_none gt 0 then fimage[wh_none]=1.
  ptr_free,prs
  return,fimage
end

;=============================================================================
;  FluxUnits - Return the currently set flux units
;=============================================================================
function CubeProj::FluxUnits,AS_BUILT=as_built,INTEGRATE=int
  this=keyword_set(as_built)?self.AS_BUILT:self
  if this.fluxcon then begin
     if keyword_set(int) then $
        units=this.pix_omega?'W/m^2/sr':'W/m^2' $
     else $
        units=this.pix_omega?'MJy/sr':'Jy/pix'
  endif else units='e/s/pix'
  return,units
end

;=============================================================================
;  MinObservedDate - Find the oldest observed date among non-disabled
;                    records
;=============================================================================
function CubeProj::MinObservedDate
  if ~ptr_valid(self.DR) then return,-1
  wh=where(~(*self.DR).DISABLED,good_cnt)
  if good_cnt eq 0 then return,-1
  return,min((*self.DR)[wh].DATE_OBS)
end

;=============================================================================
;  AddMergeRec - Add a merge record for a given order, index and
;                wavelength set.
;=============================================================================
pro CubeProj::AddMergeRec,order,planes,wave,OFFSET=off
  if n_elements(planes) ne 0 then begin 
     if ptr_valid((*self.MERGE)[order].planes) then $
        *(*self.MERGE)[order].planes=[*(*self.MERGE)[order].planes,planes] $
     else (*self.MERGE)[order].planes=ptr_new(planes)
  endif
  if n_elements(wave) ne 0 then begin 
     if ptr_valid((*self.MERGE)[order].wave) then $
        *(*self.MERGE)[order].wave=[*(*self.MERGE)[order].wave,wave] $
     else (*self.MERGE)[order].wave=ptr_new(wave)
  endif
  if n_elements(off) ne 0 then (*self.MERGE)[order].offset=off
end 

;=============================================================================
;  MergeSetup - Setup the account merge by finding interpolating
;               wavelengths, and saving information on which parts of
;               the orders get interpolated, and where.
;=============================================================================
pro CubeProj::MergeSetup,ORDS=ords
  self->LoadCalib
  ords=self->BuildOrders()
  ptr_free,self.WAVELENGTH
  heap_free,self.MERGE
  wave1=(self.cal->GetWAVSAMP(self.MODULE,ords[0],/PIXEL_BASED, $
                              WAVECUT=self.wavecut, $
                              PR_WIDTH=self.PR_SIZE[1], $
                              APERTURE=(*self.APERTURE)[0])).lambda
  if self.ORDER gt 0 then begin ;single order, nothing to do
     self.WAVELENGTH=ptr_new(wave1)
     return
  endif 
  
  nord=n_elements(ords) 
  
  ;; The merge setup:
  ;;     OFFSET: Plane offset into final cube (for unsplit portions)
  ;;     PLANES: The planes in the order account to merge.
  ;;     WAVE: The wavelength of said planes.
  ;;     TO: The final cube-plane to merge them to
  ;;     FRAC: The fraction to merge into the first plane
  self.MERGE=ptr_new(replicate({offset:0L,planes:ptr_new(), $
                                wave:ptr_new(), to:ptr_new(), $
                                frac:ptr_new()},nord))
  
  last_over=0
  wh_clear_sav=-1               ;to keep track of still 
  nap=n_elements(*self.APERTURE)
  wave_zero=lonarr(nord)
  for ord=1L,nord-1L do begin
     ;; Work with previous, overlap with current order
     wave2=(self.cal->GetWAVSAMP(self.MODULE,ords[ord],/PIXEL_BASED, $
                                 WAVECUT=self.wavecut, $
                                 PR_WIDTH=self.PR_SIZE[1], $
                                 APERTURE=(nap eq 1?(*self.APERTURE)[0]: $
                                           (*self.APERTURE)[ord]))).lambda
     nw1=n_elements(wave1) & nw2=n_elements(wave2) 
     min_wav1=min(wave1,max=max_wav1)
     min_wav2=min(wave2,max=max_wav2)
     
     ;; No overlap case... just concatenate
     if min_wav2 gt max_wav1 then begin 
        print,FORMAT='(%"No overlap:  %2d->%2d")',ords[ord-1],ords[ord]
        if n_elements(wave) gt 0 then wave=[wave,wave1] else wave=wave1
        wave_zero[ord]=wave_zero[ord-1]+n_elements(wave1) 
        last_over=0 & wh_clear_sav=-1
        wave1=wave2
        continue
     endif
  
     ;; Locations of the overlap region in each wavelength vector.
     wh_over1=where(wave1 gt min_wav2 AND $
                    wave1 lt max_wav2,cnt1,COMPLEMENT=wh_clear1)
     wh_over2=where(wave2 gt min_wav1 AND $
                    wave2 lt max_wav1,cnt2,COMPLEMENT=wh_clear2)
     
     ;; Use as the primary wavelength set whichever had more samples in
     ;; the overlap region
     if cnt1 ge cnt2 then begin ; merge in the 2nd order's overlap.
        ;print,FORMAT='(%"Merge 2nd orders overlap: %2d->%2d")',ords[ord-1],$
        ;      ords[ord]
        use_wav=wave1[wh_over1]
        lose_wav=wave2[wh_over2]
        self->AddMergeRec,ord,wh_over2,lose_wav
     endif else begin           ;merge in the 1st order's overlap
        ;print,FORMAT='(%"Merge 1st orders overlap: %2d->%2d")',ords[ord-1],$
        ;      ords[ord]
        use_wav=wave2[wh_over2]
        lose_wav=wave1[wh_over1] ;offset into *current* wave1, may have been
                                ; (likely ) trimmed 
        ;; wh_clear_sav is needed to account for the fact that wave1 was
        ;; very likely trimmed on the last round, and thus the appropriate
        ;; planes to use have changed
        self->AddMergeRec,ord-1, wh_clear_sav[0] ne -1? $
                          wh_clear_sav[wh_over1]:wh_over1,lose_wav
     endelse
     
     ;; accumulate offset information
     wave_zero[ord]=wave_zero[ord-1]+last_over
     if wh_clear1[0] ne -1 then wave_zero[ord]+=n_elements(wh_clear1) 
     ;; must correct via overlap count to ensure alignment
     wave_zero[ord]+=(cnt1-cnt2)>0
     
;     print,FORMAT='(%"Offsetting %2d to %3d with len1: %3d (%3d/%3d)")', $
;           ords[ord],wave_zero[ord], n_elements(wh_clear1),cnt1,cnt2
     
     ;; Excise the overlapping chunks from both sides
     if wh_clear1[0] ne -1 then wave1=wave1[wh_clear1]
     if wh_clear2[0] ne -1 then wave2=wave2[wh_clear2]
     
     ;; Concat wave1 and overlap into full wavelength
     if n_elements(wave) gt 0 then wave=[wave,wave1,use_wav] $
     else wave=[wave1,use_wav]
     
     last_over=cnt2 & wh_clear_sav=wh_clear2
     wave1=wave2
  endfor
  ;; graft on the last remaining piece
  wave=[wave,wave2]
  (*self.MERGE).offset=wave_zero
  
  ;; Finish the merge vector, computing plane split fractions and
  ;; merge locations
  for ord=0,nord-1 do begin 
     ;; Interpolate those being merged onto the new wavelength grid.  
     if NOT ptr_valid((*self.MERGE)[ord].wave) then continue
     merge_wave=*(*self.MERGE)[ord].wave
     ;; Location in the newly computed cube wavelength grid these
     ;; wavelengths must be merged onto.
     map_loc=value_locate(wave,merge_wave)
     if NOT array_equal(map_loc ne -1,1b) then $
        self->Error,'Cannot merge non-bracketing wavelengths'
     ;;  the fraction in the first bin (independent of up- vs. down-going)
     (*self.MERGE)[ord].frac=ptr_new((wave[map_loc+1]-merge_wave)/ $
                                     (wave[map_loc+1]-wave[map_loc]))
     (*self.MERGE)[ord].to=ptr_new(map_loc,/NO_COPY)
  endfor 
;  print,'Total wavelength: ',n_elements(wave) 
  self.WAVELENGTH=ptr_new(wave,/NO_COPY)
end

;=============================================================================
;  MergeAccount - Merge a new account into the existing account, for
;                 the given record.  If they overlap in wavelength,
;                 combine by using the pre-computed interpolated
;                 wavelength sampling in the region of overlap, and
;                 splitting individual polygon area overlaps between
;                 planes using linear interpolation.  If they don't
;                 overlap, just concatenate the accounting cubes.
;                 ORDER is the logical order number index in sequence
;                 (as opposed to the optical grating order number).
;=============================================================================
pro CubeProj::MergeAccount,dr,order,account
  if NOT ptr_valid(self.MERGE) then begin ; only one order
     (*self.DR)[dr].ACCOUNT=ptr_new(account)
     return
  endif 
     
  mrec=(*self.MERGE)[order]
  dr_acct=(*self.DR)[dr].ACCOUNT
  if NOT ptr_valid(mrec.planes) then begin 
     ;;no merging needed, just append list to account, suitably offset
     account.cube_plane+=mrec.offset
     if NOT ptr_valid(dr_acct) then begin 
        (*self.DR)[dr].ACCOUNT=ptr_new(account,/NO_COPY)
     endif else *dr_acct=[*dr_acct,account]
     return
  endif
  
  ;; Split planes in the merge vector among two planes.
  split_planes=*mrec.planes     ;order's account planes to be manipulated
  to_planes=*mrec.to            ; ... and sent to these cube planes
  fracs=*mrec.frac              ; ... with these fractions
  
  ;; First group the account records by the cube plane(s) they affect
  minsp=min(split_planes,MAX=maxsp)
  h=histogram(account.cube_plane,MIN=minsp,MAX=maxsp,REVERSE_INDICES=ri_cube)
  
  ;; Offset all to the correct cube plane (some we'll change soon)
  account.cube_plane+=mrec.offset
  
  ;; which account records hold planes which must be split and
  ;; interpolated onto the new wavelength grid?
  wh=where(h gt 0 AND $
           histogram(split_planes,REVERSE_INDICES=ri_order) gt 0,cnt) 
  for i=0,cnt-1 do begin 
     if ri_cube[wh[i]+1] eq ri_cube[wh[i]] then continue
     to_plane=to_planes[ri_order[ri_order[wh[i]]]]
     frac=fracs[ri_order[ri_order[wh[i]]]]
     
     ;; Split the elements which are on affected (overlap) planes
     changers=ri_cube[ri_cube[wh[i]]:ri_cube[wh[i]+1]-1]
     split_acc=account[changers]
     
     ;; The first target plane gets frac worth of the split element
     account[changers].area*=frac
     account[changers].CUBE_PLANE=to_plane
     ;; The next target plane gets 1-frac worth of the split element
     split_acc.area*=(1.-frac)
     split_acc.CUBE_PLANE=to_plane+1
     
     if n_elements(new_acc) eq 0 then new_acc=[split_acc] else $
        new_acc=[new_acc,split_acc]
  endfor
  if n_elements(new_acc) ne 0 then account=[account,new_acc]
  
  ;; Append this newly modified account list to the DR's accounts
  if ptr_valid(dr_acct) then *dr_acct=[*dr_acct,account] else $
     (*self.DR)[dr].ACCOUNT=ptr_new(account)     
end 



;=============================================================================
;  MakeFeedBackWindow - Make a feedback window of a certain size
;=============================================================================
pro CubeProj::MakeFeedBackWindow,xsize,ysize
  self->GetProperty,TLB_OFFSET=tboff,TLB_SIZE=tbsize
  device,GET_SCREEN_SIZE=ss
  
  window,XSIZE=xsize,YSIZE=ysize, $
         XPOS=tboff[0]+tbsize[0], YPOS=ss[1]-tboff[1]-ysize, $
         TITLE=string(FORMAT='(%"%s %s  (%s)  [%dx%d map; ' + $
                      '%6.1f\"x%6.1f\"]")',$
                      'Building Cube: ', $
                      self->ProjectName(), $
                      (self.MODULE?self.MODULE:"(no module)")+ $
                      (self.ORDER ne 0?' Order '+strtrim(self.ORDER,2): $
                       ' all orders'), $
                      self.NSTEP, $
                      self.CUBE_SIZE[0:1]* $
                      self.PLATE_SCALE/self.OVERSAMPLE_FACTOR*3600.0D),/FREE
  (*self.wInfo).feedback_window=!D.WINDOW
end


;=============================================================================
;  ShowFeedBackWindow - Show the feedback window with drawn grid
;=============================================================================
pro CubeProj::ShowFeedBackWindow
  aspect=float(self.CUBE_SIZE[0])/self.CUBE_SIZE[1] ;x=y*aspect
  
  ;; Keep the position outline square, 800 is the largest dimension
  ;; pos=[15,15]+[x,y]-[15,4]
  if self.CUBE_SIZE[0] gt self.CUBE_SIZE[1] then begin 
     xsize=800
     ysize=xsize/aspect
  endif else begin 
     ysize=800
     xsize=ysize*aspect
  endelse
  xsize=xsize+30 & ysize=ysize+19
  tvlct,[255b,0b,0b,0b],[0b,255b,0b,255b],[0b,0b,255b,255b], $
        !D.TABLE_SIZE-20
  
  if (*self.wInfo).feedback_window gt 0 then begin 
     device,WINDOW_STATE=win_valid
     if win_valid[(*self.wInfo).feedback_window] then $ 
        wdelete, (*self.wInfo).feedback_window
     if win_valid[(*self.wInfo).feedback_pixmap] then $ 
        wdelete, (*self.wInfo).feedback_pixmap
  endif
  
  window,XSIZE=xsize,YSIZE=ysize,/FREE,/PIXMAP
  (*self.wInfo).feedback_pixmap=!D.WINDOW
    
  plot,[0],/NODATA,xrange=[0,self.cube_size[0]], $
       POSITION=[15,15,xsize-15,ysize-4],/DEVICE, $
       yrange=[0,self.cube_size[1]],xstyle=1,ystyle=1,xticks=1,yticks=1
  
  (*self.wInfo).feedback_save=[!X.S,!Y.S]
  
  ;; Draw the grid
  for i=0L,self.cube_size[0] do plots,i,!Y.CRANGE
  for i=0L,self.cube_size[1] do plots,!X.CRANGE,i
  
  self->MakeFeedBackWindow,xsize,ysize
  self->SnapshotFeedBackWindow,/RESTORE
  wait,0
end

;=============================================================================
;  SnapshotFeedBackWindow - Save or restore the drawn contents of the
;                           feedback window.
;=============================================================================
pro CubeProj::SnapshotFeedBackWindow,RESTORE=res
  if (*self.wInfo).feedback_window eq 0 then self->ShowFeedBackWindow $
  else begin ;; window has been set, but has been destroyed
     catch,err
     if err ne 0 then begin 
        catch,/cancel
        wset,(*self.wInfo).feedback_pixmap
        self->MakeFeedBackWindow,!D.X_SIZE,!D.Y_SIZE
     endif 
     wset,(*self.wInfo).feedback_window
     catch,/cancel
  endelse 
  if keyword_set(res) then begin 
     device,copy=[0,0,!D.X_SIZE,!D.Y_SIZE,0,0,(*self.wInfo).feedback_pixmap]
     self->FeedbackWindowSet
  endif else begin 
     wset,(*self.wInfo).feedback_pixmap
     device,copy=[0,0,!D.X_SIZE,!D.Y_SIZE,0,0,(*self.wInfo).feedback_window]
     wset,(*self.wInfo).feedback_window     
  endelse 
end


;=============================================================================
;  FeedbackWindowSet - Restore Feedback Window set.
;=============================================================================
pro CubeProj::FeedbackWindowSet
  wset,(*self.wInfo).feedback_window
  !X.S=(*self.wInfo).feedback_save[0:1]
  !Y.S=(*self.wInfo).feedback_save[2:3]
end

;=============================================================================
;  BuildAccount - Build the accounting lists, listing, for each
;                 record, and for all pixels in the cube, all
;                 overlapping data pixels from that record, including
;                 the BCD index #, the pixel in that BCD which
;                 overlapped, and fraction which overlapped the
;                 corresponding cube pixel.
;=============================================================================
pro CubeProj::BuildAccount,_EXTRA=e
  widget_control, /HOURGLASS
  self->MergeSetup
  self.CUBE_SIZE[2]=n_elements(*self.WAVELENGTH) 

  ords=self->BuildOrders()
  nap=n_elements(*self.APERTURE) 
  RADEG = 180.0d/!DPI           ; preserve double
  
  if self.feedback then self->ShowFeedBackWindow
  
  wh=where(~(*self.DR).DISABLED,good_cnt)
  if good_cnt eq 0 then return
  t0=systime(1) 
  status=string(FORMAT='("Projecting and clipping ",I0," records...")', $
                good_cnt)
  self->Status,status
  
  astr=self->CubeAstrometryRecord()
  exp_off=-1
  for iwh=0L,good_cnt-1 do begin 
     i=wh[iwh]                  ;enabled record index
     
     ;; if the accounts are fully valid and an account exists for this
     ;; DR, assume it's valid.
     if self->Dirty(/ACCOUNTS,/LAYOUT) then ptr_free,(*self.DR)[i].ACCOUNT
     
     ;; Compute the pixel offset of the canonical PR's center for this
     ;; BCD N.B. The slit is laid out differently in Spectral Maps and
     ;; BCD's (ughh) so ROW<-->COLUMN.  A non-full aperture will
     ;; introduce a correction to this center.
     
     ;; XXX The details of this depend on the exact layout of the "+y"
     ;; direction w.r.t the detector coordinates, i.e. do maps start
     ;; top left, bottom left, etc  Revisit.
;      basic_offset=[((*self.DR)[i].ROW-1)*stepsz[0], $
;                    self.cube_size[1]-((*self.DR)[i].COLUMN-1)*stepsz[1]-1] + $
;                   self.PR_SIZE/2.
     
     pos=self.reconstructed_pos?(*self.DR)[i].REC_POS:(*self.DR)[i].RQST_POS
     pa=self.reconstructed_pos?(*self.DR)[i].PA:(*self.DR)[i].PA_RQST
     ;; Building in another order?  Transform coordinates over.
     if (*self.DR)[i].TARGET_ORDER ne self.ORDER AND $
        self.ORDER ne 0 then begin
        self.cal->TransformCoords,self.MODULE,pos,pa, $
                                  ORDER1=(*self.DR)[i].TARGET_ORDER, $
                                  self.MODULE,ORDER2=self.ORDER,newpos
        pos=newpos
     endif
     
     ;; Calculate precise sky pixel center of the slit center coordinate
     ad2xy,pos[0],pos[1],astr,x,y
     offset=[x,y]+.5            ;NASALib WCS: pixel centered at [0.0,0.0]
     
     ;; Set list select and color
     if self.feedback then begin 
        ;; Color for Feedback PR
        if exp_off lt 0 then exp_off=(*self.DR)[i].EXP
        color=!D.TABLE_SIZE-20+((*self.DR)[i].EXP-exp_off) mod 4
        self->SetListSelect,i,/NO_PRESERVE_TOP,/NO_STATUS
        self->FeedbackWindowSet
     endif 
     
     ;; (Probably small) difference between PA of this BCD and the
     ;; mean map PA
     delta_PA=pa-self.PA
     for ord=0,n_elements(ords)-1 do begin
        aper=nap eq 1?(*self.APERTURE)[0]:(*self.APERTURE)[ord]
        prs=self.cal->GetWAVSAMP(self.MODULE,ords[ord],APERTURE=aper, $
                                 WAVECUT=self.wavecut, $
                                 /PIXEL_BASED, /SAVE_POLYGONS, $
                                 PR_WIDTH=self.PR_SIZE[1],_EXTRA=e)
        
        ;; Pre-allocate an account list 
        nacc=4*n_elements(prs) 
        account=replicate({CUBE_ACCOUNT_LIST},nacc)
        acc_ind=0L
        
        ;; iterate over all the adjacent PRs in the order 
        for j=0L,n_elements(prs)-1 do begin 
           ;; Setup the rotation matrix to rotate this PR back to the
           ;; +x direction (which may lie at any angle to the cube grid)
           angle=-prs[j].angle+delta_PA
           if angle ne 0.0D then begin 
              ct=cos(angle/RADEG) & st=sin(angle/RADEG)
              rot=transpose([[ ct,-st], $
                             [ st, ct]])
           endif
           
           bcdpixels=*prs[j].PIXELS ;corresponding BCD pixel for each polygon
           polys=*prs[j].POLYGONS ; x, y list indexed by reverse indices
           poly_inds=*prs[j].POLY_INDS
           polys=(polys-rebin(prs[j].cen,size(polys,/DIMENSIONS),/SAMPLE)) * $
                 self.OVERSAMPLE_FACTOR
           
           ;; Rotate this polygon to the cube sky grid, if necessary
           if angle ne 0.0 then polys=rot#polys ;XXX check!!!
           ;; Offset the polygon correctly into the sky grid
           polys+=rebin(offset,size(polys,/DIMENSIONS),/SAMPLE)
           
           if (j eq 0L) && self.feedback then begin
              for k=0,n_elements(bcdpixels)-1L do $
                 plots,[reform(polys[0,poly_inds[k]:poly_inds[k+1]-1]), $
                        polys[0,poly_inds[k]]], $
                       [reform(polys[1,poly_inds[k]:poly_inds[k+1]-1]), $
                        polys[1,poly_inds[k]]],COLOR=color
              plots,offset,PSYM=4,COLOR=color
              wait,0
           endif
           
           cube_spatial_pix=polyfillaa(reform(polys[0,*]),reform(polys[1,*]),$
                                       self.CUBE_SIZE[0],self.CUBE_SIZE[1], $
                                       AREAS=areas,POLY_INDICES=poly_inds)
           
           if cube_spatial_pix[0] eq -1 then continue ;none clipped
           
           poly_cnt=poly_inds[1:*]-poly_inds ; per-bcdpix number pixels clipped
           keep=where(poly_cnt gt 0,kcnt)    ; only those which actually clipped
           if kcnt eq 0 then continue
           
           ;; chunk index the bcdpixel array
           h=histogram(total(poly_cnt[keep],/CUMULATIVE,/PRESERVE_TYPE)-1L, $
                       /BINSIZE,MIN=0,REVERSE_INDICES=ri)
           bcdpixels=bcdpixels[keep[ri[0:n_elements(h)-1]-ri[0]]]
           
           ncp=n_elements(cube_spatial_pix) ; Total number of new clipped polys
           
           ;; Add space to account list as necessary, large chunks at a time
           if acc_ind+ncp ge nacc then begin 
              add=nacc>ncp
              account=[account,replicate({CUBE_ACCOUNT_LIST},add)]
              nacc+=add
           endif
           
           account[acc_ind:acc_ind+ncp-1].cube_pix=cube_spatial_pix
           account[acc_ind:acc_ind+ncp-1].cube_plane=j ;just this pr's
           account[acc_ind:acc_ind+ncp-1].bcd_pix=bcdpixels
           account[acc_ind:acc_ind+ncp-1].area=areas
           acc_ind+=ncp
        endfor
        account=account[0:acc_ind-1] ; trim this order's account to size
        
        ;; Merge this account into the full cube account
        self->MergeAccount,i,ord,account
     endfor
  endfor
  if self.feedback then self->SnapshotFeedBackWindow
  self->BuildRevAcct            ;we need these too...
  self->Status,status+string(FORMAT='("done in ",F0.2,"s")',systime(1)-t0)
  self->SetClean,/ACCOUNTS,/LAYOUT,/BACKGROUND
  self->UpdateButtons
end


;=============================================================================
;  BuildRevAcct - Build the reverse accounts from the regular
;                 accounts.  There are three main reverse index
;                 classes, all computed per-record: A reverse index
;                 over cube pixels (REV_ACCOUNT), in an offset
;                 bounding box, a dual histogram reverse index over
;                 histogram count in the cube pixel list (REV_DUAL),
;                 and a BCD pixel reverse index of accounts (also in
;                 an offset bounding box): REV_BCD_ACCOUNT.  These are
;                 used for rapid backtracking, quickbuild of dirty
;                 cube pixels, and faster full rebuilds (via the dual
;                 histogram method).
;=============================================================================
pro CubeProj::BuildRevAcct
  widget_control, /HOURGLASS
  accs_changed=0b
  if self->Dirty(/ACCOUNTS,/LAYOUT) then $
     ptr_free,(*self.DR).REV_ACCOUNT,(*self.DR).REV_DUAL, $
              (*self.DR).REV_BCD_ACCOUNT
  need=where(~ptr_valid((*self.DR).REV_ACCOUNT) AND ~(*self.DR).DISABLED, $
             need_cnt)
  if need_cnt eq 0 then return
  
  sform='("Building reverse lookup information for ",I0," records...")'
  status=string(need_cnt,FORMAT=sform)
  self->Status,status
  self->GetProperty,BCD_SIZE=bcd_size
  for ineed=0L,need_cnt-1 do begin
     i=need[ineed]
     ;; Find the x/y bounding box of this DRs footprint in the cube:
     pix=(*(*self.DR)[i].ACCOUNT).cube_pix
     y=pix/self.CUBE_SIZE[0] & x=pix-y*self.CUBE_SIZE[0]
     mn_x=min(x,MAX=mx_x) & mn_y=min(y,MAX=mx_y)
     width=mx_x-mn_x+1 & height=mx_y-mn_y+1
     (*self.DR)[i].REV_WIDTH=width
     (*self.DR)[i].REV_HEIGHT=height
     (*self.DR)[i].REV_OFFSET=[mn_x,mn_y]
     x-=mn_x & y-=mn_y          ;offset into BB
     
     ;; Compute the reverse indices of cube pixels, within the smaller
     ;; bounding box where this DR falls.  E.g. the account records
     ;; from this BCD pertaining to bounding box pixel `z' are:
     ;; ri[ri[z-rev_min]:ri[z+1-rev_min]-1].
     h=histogram(x+width*(y + height * (*(*self.DR)[i].ACCOUNT).cube_plane), $
                 OMIN=om,REVERSE_INDICES=ri)
     (*self.DR)[i].REV_ACCOUNT=ptr_new(ri,/NO_COPY)
     (*self.DR)[i].REV_CNT=n_elements(h) 
     (*self.DR)[i].REV_MIN=om
     
     ;; Now calculate and cache the "dual histogram" reverse indices
     ;; for the cube pixels, binning by histogram count
     h2=histogram(h,MIN=1,REVERSE_INDICES=ri)
     (*self.DR)[i].REV_DUAL=ptr_new(ri,/NO_COPY) ; ri specifies bb-cube pix
     (*self.DR)[i].REV_DUAL_CNT=n_elements(h2) 
     accs_changed=1b
     
     ;; Now generate the BCD_PIX reverse index
     pix=(*(*self.DR)[i].ACCOUNT).bcd_pix
     y=pix/bcd_size[0] & x=pix-y*bcd_size[0]
     mn_x=min(x,MAX=mx_x)
     width=mx_x-mn_x+1
     h=histogram((x-mn_x) + y*width,REVERSE_INDICES=ri,OMIN=om)
     (*self.DR)[i].REV_BCD_ACCOUNT=ptr_new(ri,/NO_COPY)
     (*self.DR)[i].REV_BCD_XOFF=mn_x
     (*self.DR)[i].REV_BCD_WIDTH=width
     (*self.DR)[i].REV_BCD_MIN=om
     (*self.DR)[i].REV_BCD_CNT=n_elements(h) 
  endfor
  
  if accs_changed then begin 
     ptr_free,self.AUTO_BPL.BCD_PIX,self.AUTO_BPL.BCD_VALS, $
              SELF.AUTO_BPL.DCEID,self.AUTO_BPL.CNT_VEC, $
              self.AUTO_BPL.BCD_UNC
     self->UpdateButtons
  endif
  self->Status,status+'done'
end

;=============================================================================
;  BuildCube - Assemble the Cube from the accounting information, the
;              BCD data, the BMASK, and the uncertainties (along with
;              fluxing information if appropriate)
;=============================================================================
pro CubeProj::BuildCube
  widget_control, /HOURGLASS
  if ~ptr_valid(self.DR) then return
  self->RestoreAll              ;get all of the data, if necessary
  
  enabled=where(~(*self.DR).DISABLED,enabled_cnt)
  if enabled_cnt eq 0 then $
     self->Error,'Must enable some records to build cube.'
  
  self->Normalize
  
  ;; Check for a layout only account change first, and prompt user
  if self->Dirty(/LAYOUT) && ~self->Dirty(/ACCOUNTS) then begin
     self->Warning,'Cube layout changed, rebuild accounts?',/CANCEL, $
                   RESULT=res
     if res eq 'Cancel' then return
  endif 
  
  use_unc=self.use_unc && array_equal(ptr_valid((*self.DR)[enabled].UNC),1b)
  
  ;; See what needs rebuilding, and check for the quickbuild option.
  if self->Dirty(/ACCOUNTS,/LAYOUT) || $
     ~array_equal(ptr_valid((*self.DR)[enabled].ACCOUNT),1b) $
  then begin 
     quickbuild=0b
     self->BuildAccount
  endif else begin 
     ;; See if we can do a "quick build": only quickly re-processing
     ;; certain marked "dirty" pixels, re-using the rest of the cube
     ;; (and unc)
     dirty=ptr_valid((*self.DR)[enabled].DIRTY_PIX)
     global_dirty=ptr_valid(self.GLOBAL_DIRTY_PIX)
     quickbuild=ptr_valid(self.CUBE) && $
        n_elements(*self.CUBE) gt 0 && $
        (~use_unc || ptr_valid(self.CUBE_UNC)) && $
        (global_dirty  || ~array_equal(dirty,0b))
     if self.feedback then self->SnapshotFeedBackWindow,/RESTORE
     self->BuildRevAcct 
  endelse 
    
  cube_width=self.CUBE_SIZE[0]
  cube_plane_pix=self.CUBE_SIZE[0]*self.CUBE_SIZE[1]
  
  if self.fluxcon || self.slcf then fluxim=self->FluxImage()
  use_bg=self.use_bg && ptr_valid(self.BACKGROUND)
  use_flux=n_elements(fluxim) gt 0
  
  if use_bg && use_unc && ~ptr_valid(self.BACKGROUND_UNC) then $
     self->Error,'Must rebuild background for cube uncertainty.'
  
  ;; Create the list of dirty cube pixels to revisit for quick-build
  if quickbuild then begin 
     self->GetProperty,BCD_SIZE=bcd_size
     ncp=50L                     ;starting guess
     cube_dirty_pix=lonarr(ncp,/NOZERO)
     cube_dirty_cnt=0L
     for k=0L,enabled_cnt-1 do begin 
        rec=(*self.DR)[enabled[k]]
        ri=*rec.REV_BCD_ACCOUNT
        for i=0,1 do begin      ; record and global dirty pixels
           if i then begin 
              if ~global_dirty then continue
              pix=*self.GLOBAL_DIRTY_PIX
           endif else begin 
              if ~dirty[k] then continue
              pix=*rec.DIRTY_PIX
           endelse 
           ind=(pix mod bcd_size[0]) - rec.REV_BCD_XOFF + $
               pix/bcd_size[0]*rec.REV_BCD_WIDTH - rec.REV_BCD_MIN
           for j=0L,n_elements(ind)-1 do begin 
              if ind[j] lt 0 or ind[j] ge rec.REV_BCD_CNT then continue
              cnt=ri[ind[j]+1]-ri[ind[j]]
              if cnt eq 0 then continue
              if (cube_dirty_cnt + cnt) ge ncp then begin 
                 cube_dirty_pix=[cube_dirty_pix,lonarr(ncp)]
                 ncp*=2
              endif 
              acc=(*rec.ACCOUNT)[ri[ri[ind[j]]:ri[ind[j]+1]-1]]
              cube_dirty_pix[cube_dirty_cnt]= acc.cube_pix + $
                                              acc.cube_plane*cube_plane_pix
              cube_dirty_cnt+=cnt
           endfor
        endfor
     endfor
     if cube_dirty_cnt eq 0L then begin 
        self->ClearDirtyPix
        return 
     endif 
     if cube_dirty_cnt lt n_elements(cube_dirty_pix) then $
        cube_dirty_pix=cube_dirty_pix[0L:cube_dirty_cnt-1L]
     
     ;; Eliminate duplicates.
     cube_dirty_pix=cube_dirty_pix[uniq(cube_dirty_pix,sort(cube_dirty_pix))]
     cube_dirty_cnt=n_elements(cube_dirty_pix) 
     dfrac=100.*cube_dirty_cnt/product(self.CUBE_SIZE)
     if dfrac gt 15. then begin ; Approximate break-even in quickbuild speedup
        quickbuild=0b
        dfmsg=': full build forced'
     endif else dfmsg=''
     status=string(FORMAT='("Quick-Building ",I0,"x",I0,"x",I0,' + $
                   '" cube (",I0," dirty pixels - ",F0.1,"%",A,")...")', $
                   self.CUBE_SIZE,cube_dirty_cnt,dfrac,dfmsg)
     self->Status,status

     ;; Decompose into dirty X,Y,Z positions in the cube
     cube_dirty_x=cube_dirty_pix mod cube_plane_pix
     cube_dirty_y=cube_dirty_x/cube_width
     cube_dirty_x mod=cube_width
     cube_dirty_z=cube_dirty_pix/cube_plane_pix
  
     ;; Set up the (existing) cube and associated arrays
     cube=temporary(*self.CUBE)
     cube[cube_dirty_pix]=!VALUES.F_NAN
     if use_unc then begin 
        cube_unc=temporary(*self.CUBE_UNC)
        cube_unc[cube_dirty_pix]=!VALUES.F_NAN
     endif 
  endif else begin 
     status=string(FORMAT='("Building ",I0,"x",I0,"x",I0," cube...")', $
                   self.CUBE_SIZE)
     self->status,status
  endelse 
  
  if ~quickbuild then begin     ; A fresh build, make a new cube
     cube=make_array(self.CUBE_SIZE,/FLOAT,VALUE=!VALUES.F_NAN)
     if use_unc then $
        cube_unc=make_array(self.CUBE_SIZE,/FLOAT,VALUE=!VALUES.F_NAN)
  endif 
  areas=make_array(self.CUBE_SIZE,/FLOAT,VALUE=0.0)
  
  ;; Create mask from bad pixels
  if ptr_valid(self.GLOBAL_BAD_PIXEL_LIST) then begin 
     use_bpmask=1
     bpmask=make_array(size(*(*self.DR)[0].BCD,/DIMENSIONS),VALUE=1b)
     bpmask[*self.GLOBAL_BAD_PIXEL_LIST]=0b
  endif else use_bpmask=0
    
  update=10<(enabled_cnt/50)>1  ;status update interval
  
  for k=0L,enabled_cnt-1 do begin
     if (k mod update) eq 0 then $
        self->Status,status+string(FORMAT='(I2,"%")',k*100/enabled_cnt)
     
     rec=(*self.DR)[enabled[k]]
     rev_min=rec.REV_MIN
     rev_width=rec.REV_WIDTH
     rev_height=rec.REV_HEIGHT
     rev_wh=rev_width*rev_height
     rev_off=rec.REV_OFFSET
     
     ;; Locate dirty cube pixels in this record's bounding box
     if quickbuild then begin 
        inbb=where((cube_dirty_x-rev_off[0]) lt rev_width AND $
                   (cube_dirty_x-rev_off[0]) ge 0 AND $
                   (cube_dirty_y-rev_off[1]) lt rev_height AND $
                   (cube_dirty_y-rev_off[1]) ge 0,inbb_cnt)
        if inbb_cnt eq 0 then continue
        x=cube_dirty_x[inbb]
        y=cube_dirty_y[inbb]
        z=cube_dirty_z[inbb]
        
        ;; Index into XY bounding box reverse index vector for cube
        ind=(x-rev_off[0]) + (y-rev_off[1])*rev_width + z*rev_wh - rev_min
        
        ;; Bin on count in the subset of dirty cube pixels
        count=(*rec.REV_ACCOUNT)[ind+1]-(*rec.REV_ACCOUNT)[ind]
        if array_equal(count,0) then continue
        h=histogram(count,REVERSE_INDICES=dual,MIN=1)
        dual_cnt=n_elements(h) 
     endif else begin ;; Not a quickbuild but a full build, use dual histogram
        ;; Use pre-cached dual reverse index (binned on count per cube pix)
        dual=*rec.REV_DUAL
        dual_cnt=rec.REV_DUAL_CNT
     endelse 
     
     bcd=*rec.BCD
     if use_bg then bcd-=*self.BACKGROUND
     if use_flux then bcd*=fluxim
     if use_unc then begin 
        bcd_unc=*rec.UNC
        if use_bg then bcd_unc=sqrt(bcd_unc^2+(*self.BACKGROUND_UNC)^2)
        if use_flux then bcd_unc*=fluxim
     endif
     
     ;; --- Masks
     ;; Start with all BCD NaN's
     mask=finite(bcd)
     ;; Also exclude BCD pix with any of BMASK bits 8,12,13,& 14 set from
     ;; entering the cube, and add any global bad pixels
     if ptr_valid(rec.BMASK) then $
        mask AND= (*rec.BMASK AND 28928U) eq 0L 
     if use_bpmask then mask AND= bpmask
     
     ;; Add any per-BCD bad pixels
     if ptr_valid(rec.BAD_PIXEL_LIST) then $
        mask[*rec.BAD_PIXEL_LIST]=0b
     
     ;; --- Build using dual reverse histogram, looping over cube
     ;;     pixels binned by bin count
     for i=0L,dual_cnt-1L do begin  
        if dual[i+1] eq dual[i] then continue
        inds=dual[dual[i]:dual[i+1]-1]
        
        ;; Cube pixels and associated "notional" indices for reverse index
        if quickbuild then begin 
           pix=cube_dirty_pix[inbb[inds]]
           inds=ind[inds]       ;convert to real bb-index
        endif else begin ;; loop in dual histogram bin count 
           ;; Translate bounding-box-cube pixels into the real cube
           pix=inds+rev_min
           z=pix/rev_wh    &  pix-=z*rev_wh
           y=pix/rev_width &  pix-=y*rev_width
           y+=rev_off[1]   &  x=rev_off[0]+pix
           pix=z*cube_plane_pix + y*cube_width + x ;the real cube pixels!
        endelse
        
        ;; Find account records in cube from this BCD with this bin count
        d=[i+1L,n_elements(inds)] ;target (depth in bin, ncubepix)
        accts=rebin(transpose((*rec.REV_ACCOUNT)[inds]),d,/SAMP) + $
              rebin(lindgen(d[0]),d,/SAMP)
        accts=(*rec.ACCOUNT)[(*rec.REV_ACCOUNT)[temporary(accts)]]

        bcd_pix=accts.BCD_PIX   ;the various conributing BCD pixels
           
        ;; Clear out the empties (they're about to get set)
        empty=where(~finite(cube[pix]),cnt)
        if cnt gt 0 then begin 
           cube[pix[empty]]=0.0 
           if use_unc then cube_unc[pix[empty]]=0.0
           if self.feedback then begin 
              ;; Highlight newly filled pixels
              if quickbuild then begin 
                 wh=empty    ; Show all quick-builds (maybe none at half plane)
              endif else begin  ; show only those at halfway-plane
                 wh=where(z[empty] eq self.cube_size[2]/2,cnt)
                 if cnt gt 0 then wh=empty[wh]
              endelse 
              for j=0,cnt-1 do begin 
                 xp=x[wh[j]] & yp=y[wh[j]]
                 plots,[xp,xp,xp+1,xp+1,xp],[yp,yp+1,yp+1,yp,yp],THICK=2
              endfor 
           endif 
        endif 
        
        cube[pix]+=total(bcd[bcd_pix] * accts.AREA * mask[bcd_pix], /NAN,1)
        areas[pix]+=total(accts.AREA * mask[bcd_pix],1)
        if use_unc then $
           cube_unc[pix]+=total(bcd_unc[bcd_pix]^2 * accts.AREA^2 * $
                                mask[bcd_pix],/NAN,1)
     endfor 
  endfor
  
  ;; Normalize by areas
  if quickbuild then begin 
     ;; Just do the dirty pixels
     areas[cube_dirty_pix]>=1.e-10 ;avoid divide by zero errors
     cube[cube_dirty_pix]/=areas[cube_dirty_pix]
     self.CUBE=ptr_new(cube,/NO_COPY)
     if use_unc then begin
        cube_unc[cube_dirty_pix]=sqrt(cube_unc[cube_dirty_pix])/ $
                                 areas[cube_dirty_pix]
        self.CUBE_UNC=ptr_new(cube_unc,/NO_COPY)
     endif 
  endif else begin 
     ;; all pixels
     areas>=1.e-10
     ptr_free,self.CUBE,self.CUBE_UNC
     self.CUBE=ptr_new(cube/areas)
     if use_unc then self.CUBE_UNC=ptr_new(sqrt(cube_unc)/areas)
  endelse

  ;; Subtract off 1D BG spectrum (converting to correct units if necessary)
  if self.use_bg && ptr_valid(self.BG_SP) then begin 
     ;; Assume they are in correct units 
     if array_equal((*self.BG_SP)[0,*],*self.wavelength) then begin 
        bg=(*self.BG_SP)[1,*]
     endif else begin ;; interpolate
        bg=interpol((*self.BG_SP)[1,*],(*self.BG_SP)[0,*], $
                    *self.wavelength,/LSQUADRATIC)
     endelse 
     ;; First flux the BG spectrum if necessary
     if self.fluxcon && self.BG_SP_TYPE eq 0b then $
        self->Flux,*self.wavelength,bg,SLCF=self.slcf,PIXEL_OMEGA=self.pix_omega
     
     if quickbuild then $
        (*self.CUBE)[cube_dirty_pix]-=bg[cube_dirty_z] $
     else begin 
        bg=rebin(reform(bg,1,1,n_elements(*self.wavelength),/OVERWRITE), $
                 size(*self.CUBE,/DIMENSIONS),/SAMPLE)
        *self.CUBE-=bg
     endelse 
     ;; XXX Treat 1D BG errors
  endif
  
  self.CUBE_DATE=systime(/JULIAN)
  self->SnapshotParameters
  self->ClearDirtyPix           ;cube is fully valid now, no dirty pixels left
  self.Changed=1
  @cubism_version 
  self.version=cubism_version
  self->Status,status+'done'
  
  ;;Mark the records which went into this cube
  (*self.DR).IN_CUBE=0b & (*self.DR)[enabled].IN_CUBE=1b
  self->UpdateButtons & self->UpdateList & self->UpdateTitle
  self->Send,/UPDATE,/NEW_CUBE
end


;=============================================================================
;  ResetAccounts - Remove all accounting info, so a cube build will
;                  proceed anew.
;=============================================================================
pro CubeProj::ResetAccounts,NO_UPDATE=no
  self->SetDirty,/ACCOUNTS,/LAYOUT,/BACKGROUND
  if ~keyword_set(no) then self->UpdateButtons
end

;=============================================================================
;  Normalize - Map header info in the BCD's to cube-specific data, and
;              returns the status of the normalization.
;=============================================================================
pro CubeProj::Normalize
  self->LoadCalib
  
  if ~ptr_valid(self.DR) then $
     self->Error,'No data records'
  
  ;; For low-res use the target order as the order, if they're all the same.
;   if array_equal((*self.DR).TARGET_ORDER,(*self.DR)[0].TARGET_ORDER) AND $
;      self.ORDER eq 0 AND $
;      (self.module eq 'SL' OR self.module eq 'SH') $
;      then self.ORDER=(*self.DR)[0].TARGET_ORDER
  
  
  ;; Plate scale for our module
  self.cal->GetProperty,self.module,self.order,PLATE_SCALE=ps
  self.PLATE_SCALE=ps
    
  ;; Normalize the number of steps and step size (they should all be the same)
  enabled=where((*self.DR).DISABLED eq 0,good_cnt)
  if good_cnt eq 0 then return
  
  if ptr_valid((*self.DR)[enabled[0]].HEADER) then begin 
     stepsper=sxpar(*(*self.DR)[enabled[0]].HEADER,'STEPSPER')>1
     stepspar=sxpar(*(*self.DR)[enabled[0]].HEADER,'STEPSPAR')>1
     stepszpar=sxpar(*(*self.DR)[enabled[0]].HEADER,'SIZEPAR')
     stepszper=sxpar(*(*self.DR)[enabled[0]].HEADER,'SIZEPER')
     self.NSTEP=[stepsper[0],stepspar[0]]
     self.STEP_SIZE=[stepszper[0],stepszpar[0]]/3600.D ; in degrees
  endif 
  
  ;; Check to ensure all steps are present and accounted for
  self->CheckSteps
  
  ;; Ensure the pseudo-rect size is set up.
  self->NormalizePRSize
  
  ;; Normalize the build aperture(s)
  self->NormalizeApertures
  
  ;; And the cube size and center
  self->LayoutBCDs
end


;=============================================================================
;  NormalizePRSize - Make sure the apertures are all something useful
;=============================================================================
pro CubeProj::NormalizePRSize
  ;; Normalize the slit length
  if self.ORDER gt 0 then begin
     self.cal->GetProperty,self.module,self.order,SLIT_LENGTH=sl
     self.PR_SIZE[0]=sl
  endif else begin ;; find the longest slit and use it
     ords=self.cal->Orders(self.module)
     slmax=0.
     for i=0,n_elements(ords)-1 do begin 
        self.cal->GetProperty,self.module,ords[i],SLIT_LENGTH=sl
        slmax=sl>slmax
     endfor
     self.PR_SIZE[0]=slmax
  endelse
  len=0.                        ;adjust for a non-full aperture
  if ptr_valid(self.APERTURE) then begin 
     for i=0,n_elements(*self.APERTURE)-1 do begin 
        m=max((*self.APERTURE)[i].high-(*self.APERTURE)[i].low)
        len=m>len
     endfor 
     self.PR_SIZE[0]=self.PR_SIZE[0]*len
  endif   
  
  ;; The PR Width
  if self.PR_SIZE[1] eq 0.0 then begin
     self->ResetAccounts,/NO_UPDATE
     self.PR_SIZE[1]=1.D        ;the default, 1xn XXX
  endif 
end


;=============================================================================
;  NormalizeApertures - Make sure the apertures are all something useful
;=============================================================================
pro CubeProj::NormalizeApertures  
  ;; Normalize the build aperture(s)
  if ptr_valid(self.APERTURE) then begin 
     for i=0,n_elements(*self.APERTURE)-1 do begin 
        ap=(*self.APERTURE)[i]
        ;; default to the full aperture
        if array_equal(ap.low,0.) AND array_equal(ap.high,0.) then $
           (*self.APERTURE)[i]=irs_aperture(0.,1.)
     endfor 
  endif else self.APERTURE=ptr_new(irs_aperture(0.,1.))
end


;=============================================================================
;  ApertureEqual - Test if a given aperture is equal to the internal
;                  one.
;=============================================================================
function CubeProj::ApertureEqual,aper
  ;; See if aperture is the same or different from internal aperture
  if ~ptr_valid(self.APERTURE) then return,0
  if n_elements(*self.APERTURE) ne n_elements(aper) then return,0
  for i=0,n_elements(aper)-1 do $
     if ~array_equal((*self.APERTURE)[i].low,aper[i].low) || $
        ~array_equal((*self.APERTURE)[i].high,aper[i].high) || $
        (*self.APERTURE)[i].Wavscl ne aper[i].Wavscl || $
        ~array_equal((*self.APERTURE)[i].scale,aper[i].scale) then return,0
  return,1
end


;=============================================================================
;  RotFlipMatrix - Matrix for transforming between celestial and sky
;                  coordinate systems.
;=============================================================================
function CubeProj::RotFlipMatrix
  ;; CW rotation + flip of coordinates (for left-handed sky map)
  ;; orthogonal,symmetric rotation: rot^-1=rot
  RADEG = 180.0d/!DPI           ; preserve double
  c=cos((90.0D + self.PA)/RADEG) & s=sin((90.0D + self.PA)/RADEG)
  return,[[-c, s], $
          [ s, c]]
end


;=============================================================================
;  CubeAstrometryRecord - Compute an astrometry structure for use with
;                         the NASA WCS routines.
;                         N.B.:
;
;                         Pixel indexing conventions:
;                          FITS HEADERS/FORTRAN :  1st centered on [1.0,1.0]
;                          NASALIB WCS PROGRAMS :  1st centered on [0.0,0.0]
;                          CUBISM, aka God-Given : 1st centered on [0.5,0.5]
;
;                         CROTA vs PA:
;                          FITS standard (CROTA,CD,PC) measure CCW
;                            from N to +Y
;                          SIRTF PA's measure CCW from N to SIRTF +Z
;                            (or slit +w)
;                          +w and +x correspond, EXCEPT FOR LH, where +w=-x
;=============================================================================
function CubeProj::CubeAstrometryRecord,ZERO_OFFSET=zo
  ;; If astrometry has been recovered from FITS, it's all we've got,
  ;; so return it
  if ptr_valid(self.RECOV_ASTROM) then return,*self.RECOV_ASTROM
  
  RADEG = 180.0d/!DPI           ; preserve double
  angle=(self.PA + 90.D)/RADEG
  if self.module eq 'LH' then angle+=180./RADEG
  c=cos(angle) & s=sin(angle)
  cd=[[c,s],[-s,c]]
  if keyword_set(zo) then crpix=[0.5,0.5] else $
     crpix=self.CUBE_SIZE[0:1]/2.+.5 ;[1,1] => pixel center FITS silliness
  cdelt=self.PLATE_SCALE/self.OVERSAMPLE_FACTOR
  make_astr,astr,CD=cd,DELTA=[-cdelt,cdelt],CRPIX=crpix,CRVAL=self.POSITION, $
            CTYPE=['RA---TAN','DEC--TAN']
  return,astr
end

;=============================================================================
;  ConvertCoords - Convert between celestial and array coordinates,
;                  accounting correctly for pixel center conventions.
;=============================================================================
pro CubeProj::ConvertCoords,ra,dec,x,y,TO_RA_DEC=trd,TO_X_Y=txy
  if keyword_set(trd) then $
     xy2ad,x-.5,y-.5,self->CubeAstrometryRecord(),ra,dec $
  else begin 
     ad2xy,ra,dec,self->CubeAstrometryRecord(),x,y
     x+=.5 & y+=.5              ;restore *correct* pixel indexing
  endelse 
end

;=============================================================================
;  BCDBounds - Return the bounds of all BCD record slits (in the
;              appropriate sub-slit) in celestial coordinates.
;=============================================================================
function CubeProj::BCDBounds,recs,_EXTRA=e
  self->RecOrSelect,recs,_EXTRA=e
  ;; Construct the bounding aperture for all orders being combined into cube
  ords=self->BuildOrders()
  nap=n_elements(*self.APERTURE)
  for i=0,n_elements(ords)-1 do begin 
     ap=nap eq 1?(*self.APERTURE)[0]:(*self.APERTURE)[i]
     left=min(ap.low)           ;The bounding aperture for this order
     right=max(ap.high)
     acen=.5*(left+right)-.5    ;positive acen shifts right
     ;; Celestial (degree) left/right offsets from slit center
     off=([0.,1.]-.5+acen)*self.PR_SIZE[0]
     if n_elements(final_off) eq 0 then final_off=off else begin 
        final_off[0]=final_off[0]<off[0] ;Bounding aperture assumes same 
        final_off[1]=final_off[1]>off[1] ;slit length for all build orders
     endelse 
  endfor 
  
  ;; Rotate and offset final bounding polygon for each DR.
  pr_half=self.PR_SIZE[1]/2     ;pr width
  pr_rect=[[final_off[0],-pr_half], $
           [final_off[0], pr_half], $
           [final_off[1], pr_half], $
           [final_off[1],-pr_half]]-.5 ;nasalib standard: 0,0: center of pix
  
  nr=n_elements(recs)
  bounds=fltarr(8,nr)
  
  pos=self.reconstructed_pos?(*self.DR)[recs].REC_POS: $
      (*self.DR)[recs].RQST_POS
  RADEG = 180.0d/!DPI           ; preserve double
  for i=0,nr-1 do begin 
     pa=self.reconstructed_pos ? (*self.DR)[recs[i]].PA : $
        (*self.DR)[recs[i]].PA_RQST
     c_pa=cos((270.0D - pa)/RADEG) ;WCS uses CROTA = N CCW from +y 
     s_pa=sin((270.0D - pa)/RADEG)
     cd_pa=self.PLATE_SCALE*[[-c_pa,-s_pa],[-s_pa,c_pa]]
     
     ;; Compute the RA/DEC of the 4 corners.
     make_astr,astr,CD=cd_pa,DELTA=[1.D,1.D],CRPIX=[0.5,0.5], $
               CRVAL=pos[*,i],CTYPE=['RA---TAN','DEC--TAN']

     xy2ad,pr_rect[0,*],pr_rect[1,*],astr,a_rect,d_rect
     ;;prs[*,i]=[a_rect,d_rect]
     
     ;; Offset to the correct order: account for building a cube using
     ;; data targeted at another order.
     if (*self.DR)[recs[i]].TARGET_ORDER ne self.ORDER && $
        self.ORDER ne 0 then begin
        self.cal->TransformCoords,self.MODULE,[1.#a_rect,1.#d_rect],pa, $
                                  ORDER1=(*self.DR)[recs[i]].TARGET_ORDER, $
                                  self.MODULE,ORDER2=self.ORDER, $
                                  newcoords,newpa
        a_rect=reform(newcoords[0,*]) & d_rect=reform(newcoords[1,*])
     endif 
     bounds[0,i]=reform(a_rect) & bounds[4,i]=reform(d_rect)
  endfor 
  return,bounds
end


;=============================================================================
;  LayoutBCDs - Define the cube astrometry and determine the BCD
;               layout on the sky grid based on positions and PA's,
;               ignoring any disabled records.
;=============================================================================
pro CubeProj::LayoutBCDs
  self->LoadCalib
  
  ;; Find the PA of the dominant AOR
  good=where(~(*self.DR).DISABLED,goodcnt)
  if goodcnt eq 0 then self->Error,'Must enable some records.'
  
  recs=(*self.DR)[good]
  aorids=recs.AORKEY
  uniqids=aorids[uniq(aorids,sort(aorids))]
  cnt=0
  
  for i=0,n_elements(uniqids)-1 do begin 
     wh=where(aorids eq uniqids[i],thiscnt)
     if thiscnt gt cnt then begin
        cnt=thiscnt
        use=wh
     endif
  endfor
  pa=self.reconstructed_pos?recs[use].PA:recs[use].PA_RQST
  self.PA=mean(pa)              ;match our PA to the most numerous
  ;; Approximate cube center (pos average), for now (not critical)
  pos=self.reconstructed_pos?recs.REC_POS:recs.RQST_POS
  old_pos=self.POSITION
  self.POSITION=size(pos,/N_DIMENSIONS) eq 2?total(pos,2)/goodcnt:pos
  cubeastr=self->CubeAstrometryRecord(/ZERO_OFFSET)
  self.POSITION=old_pos
  
  ;; Compute the region bounds
  bounds=self->BCDBounds(good)
  
  for i=0,goodcnt-1 do begin 
     ;; Compute corner celestial positions in the cube frame
     ad2xy,bounds[0:3,i],bounds[4:7,i],cubeastr,x,y 
                                ;x,y in 0,0 pixel-centered coords
     x+=0.5 & y+=0.5            ;back to normal pixel convention (0.5-centered)

     ;; Accumulate pixel bounding rectangle
     if n_elements(x_min) ne 0 then begin 
        x_min=x_min<min(x) & y_min=y_min<min(y)
     endif else begin 
        x_min=min(x) & y_min=min(y)
     endelse 
     if n_elements(x_max) ne 0 then begin 
        x_max=x_max>max(x) & y_max=y_max>max(y)
     endif else begin 
        x_max=max(x) & y_max=max(y)
     endelse 
  endfor
  
  ;; Establish the dimensions of the cube in the sky coordinate system
  exact_size=[x_max-x_min,y_max-y_min]
  new_size=ceil(exact_size-.001) ; no hangers-on
  self.CUBE_SIZE[0:1]=new_size
  
  ;; See if size changed
  if ~self->Dirty(/ACCOUNTS,/LAYOUT) && $ 
     ~array_equal(self.AS_BUILT.CUBE_SIZE,0L) && $
     ~array_equal(self.CUBE_SIZE[0:1],self.AS_BUILT.CUBE_SIZE[0:1]) then $
        self->SetDirty,/LAYOUT

  ;; Find the cube center in the sky coordinate system
  xy2ad,x_min+float(new_size[0])/2.-.5,y_min+float(new_size[1])/2.-.5, $
        cubeastr,a,d
  
  self.POSITION=[a,d]
  
  ;; See if center changed
  if ~self->Dirty(/ACCOUNTS,/LAYOUT) && $ 
     ~array_equal(self.AS_BUILT.POSITION,0.0D) && $
     ~array_equal(abs(self.POSITION-self.AS_BUILT.POSITION) lt 1D-6,1b) then $
        self->SetDirty,/LAYOUT
end


;=============================================================================
;  BackTrackPix - Find the BCDs, pixels, and overlap fractions
;                 influencing the specified full cube pixel.  If
;                 FOLLOW is set, highlight the indicated BCDs too.
;                 The returned structure has the
;                 format:
;
;                     {DR:0,ID:O,BCD_PIX:0,BCD_VAL:0.0, $
;                      BCD_UNC:0.0,BACK_VAL:0.0,BACK_UNC:0.0, $
;                      AREA:0.0,BAD:0b,FLAGS:' '}
;
;                 where DR is the record number, ID is the record
;                 DCEID, BCD_PIX is the 1D bcd pixel, BCD_VAL is the
;                 value of that pixel, BACK_VAL is the value of that
;                 pixel in the currently set background (if any), AREA
;                 is the area of overlap the BCD pixels has in the
;                 cube pixel being tracked, BAD is bit-combination of
;                 0: for no bad pix, 1: for global bad pix and 2: for
;                 BCD-specific bad pix, 4: for all other BMASK/PMASK
;                 bad pixel.  Flags contain the mask flags along with
;                 "BP(1)" for bad pixel.
;=============================================================================
function CubeProj::BackTrackPix, pix, plane,FOLLOW=follow,COUNT=cnt, ERROR=err
  nrec=self->N_Records()
  if nrec eq 0 then return,-1
  if self->Dirty(/ACCOUNTS) || $
     array_equal(ptr_valid((*self.DR).ACCOUNT),0b) then begin 
     self->Error,"Must rebuild cube to backtrack",RETURN_ONLY=arg_present(err)
     err=1
     return,-1
  endif 
  
  ;; At least one reverse account required
  if array_equal(ptr_valid((*self.DR).REV_ACCOUNT),0b) then self->BuildRevAcct
  
  if n_elements(plane) ne 0 then z=plane $
  else begin 
     sz=self.CUBE_SIZE[0]*self.CUBE_SIZE[1]
     z=pix/sz
     pix-=z*sz
  endelse 
  if n_elements(pix) eq 2 then begin 
     x=pix[0] & y=pix[1]
  endif else begin 
     y=pix/self.CUBE_SIZE[0]
     x=pix-y*self.CUBE_SIZE[0]
  endelse 
  
  show=keyword_set(follow) && self->Showing()
  if show then show_vec=bytarr(nrec)
  for i=0,nrec-1 do begin 
     rec=(*self.DR)[i]
     if ~ptr_valid(rec.REV_ACCOUNT) then continue
     if x lt rec.REV_OFFSET[0] || $
        x ge rec.REV_OFFSET[0]+rec.REV_WIDTH || $
        y lt rec.REV_OFFSET[1] || $
        y ge rec.REV_OFFSET[1]+rec.REV_HEIGHT $
     then continue
     
     ;; offset into the bounding-box coordinates
     w=rec.REV_WIDTH
     w_h=w*rec.REV_HEIGHT
     ind=(x-rec.REV_OFFSET[0]) + w*(y-rec.REV_OFFSET[1]) + w_h*z
     
     if ind lt rec.REV_MIN then continue
     ind-=rec.REV_MIN
     
     ri=*rec.REV_ACCOUNT
     if ri[ind] ge ri[ind+1] then continue ;nothing in there
     self->RestoreData,i,RESTORE_CNT=rcnt
     if rcnt gt 0 then rec=(*self.DR)[i] ;new record copy needed
     
     ;; We've got a match
     if show then show_vec[i]=1b
     accs=(*rec.ACCOUNT)[ri[ri[ind]:ri[ind+1]-1]]
     naccs=n_elements(accs) 
     ret=replicate({DR:i,ID:rec.ID,DCEID:rec.DCEID, $
                    BCD_PIX:0L,BCD_VAL:0.0,BCD_UNC:0.0, $
                    BACK_VAL:0.0,BACK_UNC:0.0, $
                    AREA:0.0,BAD:0b,FLAGS:' '},naccs)
     ret.BCD_PIX=accs.BCD_PIX 
     ret.BCD_VAL=(*rec.BCD)[accs.BCD_PIX]
     if ptr_valid(rec.UNC) then ret.BCD_UNC=(*rec.UNC)[accs.BCD_PIX]
     
     if ptr_valid(self.BACKGROUND) then $
        ret.BACK_VAL=(*self.BACKGROUND)[accs.BCD_PIX]
     
     if ptr_valid(self.BACKGROUND_UNC) then $
        ret.BACK_UNC=(*self.BACKGROUND_UNC)[accs.BCD_PIX]
     
     ;; Add badpix
     if ptr_valid(self.GLOBAL_BAD_PIXEL_LIST) then begin 
        if naccs gt 1 then $
           wh=where_array(*self.GLOBAL_BAD_PIXEL_LIST,ret.BCD_PIX,cnt) $
        else begin 
           wh=where(*self.GLOBAL_BAD_PIXEL_LIST eq ret[0].BCD_PIX,cnt)
           if cnt gt 0 then wh=0
        endelse 
        if cnt gt 0 then begin
           ret[wh].BAD=1b
           ret[wh].FLAGS='BP'
        endif
     endif
     if ptr_valid(rec.BAD_PIXEL_LIST) then begin 
        if naccs gt 1 then $
           wh=where_array(*rec.BAD_PIXEL_LIST,ret.BCD_PIX,cnt) $
        else begin 
           wh=where(*rec.BAD_PIXEL_LIST eq ret[0].BCD_PIX,cnt)
           if cnt gt 0 then wh=0
        endelse 
        if cnt gt 0 then begin 
           ret[wh].BAD OR=2b
           ret[wh].FLAGS='BP(1)'
        endif 
     endif

     ;; Add BMASK Flags
     if ptr_valid(rec.BMASK) then begin 
        wh=where((*rec.BMASK)[ret.BCD_PIX] ne 0,cnt)
        if cnt gt 0 then begin 
           ret[wh].BAD OR= $
              4b*((*rec.BMASK)[ret[wh].BCD_PIX] AND 28928U ne 0b)
           for j=0,cnt-1 do begin 
              irs_bmask,(*rec.BMASK)[ret[wh[j]].BCD_PIX],CODE_STRING=cs
              ret[wh[j]].FLAGS+=' '+cs
           endfor 
        endif 
     endif 
   
     
     ret.AREA=accs.AREA
     if n_elements(all) eq 0 then all=[ret] else all=[all,ret]
  endfor 
  
  if show then begin 
     self->SetListSelect,where(show_vec),/NO_PRESERVE_TOP,/NO_UPDATE, $
                         /NO_STATUS
     self->UpdateButtons
  endif 
  cnt=n_elements(all) 
  return,cnt ne 0?all:-1
end


;=============================================================================
;  BackTrackFullCube - Run backtracking over the full array of cube
;                      pixel values into a reverse-indices style
;                      array: highly-optimized (much faster than one
;                      at a time).
;=============================================================================
pro CubeProj::BackTrackFullCube,bcd_pix,vals,cnt_vec,dceid,BCD_UNC=unc
  nrec=self->N_Records()
  if nrec eq 0 then return
  if self->Dirty(/ACCOUNTS) || $
     array_equal(ptr_valid((*self.DR).ACCOUNT),0b) then $
        self->Error,"Accounts not valid: must rebuild cube"
  
  ;; Must have all reverse accounts
  self->BuildRevAcct
  
  status='Backtracking full cube...'
  self->Status,status
  
  wh=where((*self.DR).IN_CUBE,cnt)
  if cnt eq 0 then self->Error,"Must rebuild cube."
  
  use_unc=array_equal(ptr_valid((*self.DR)[wh].UNC),1b)
  
  sz=self.CUBE_SIZE[0]*self.CUBE_SIZE[1]
  
  cnt_vec=intarr(self.CUBE_SIZE)
  
  ;; Count the total number of all backtracked pixels in the cube
  good=where(ptr_valid((*self.DR).REV_ACCOUNT) AND (*self.DR).in_cube, $
             goodcnt)
  if goodcnt eq 0 then return
  
  bt_cnt=lonarr(goodcnt,/NOZERO)
  for i=0,goodcnt-1 do $
     bt_cnt[i]=n_elements(*(*self.DR)[good[i]].ACCOUNT)
  
  all_bt_cnt=total(bt_cnt,/INTEGER)
  track_pix=lonarr(all_bt_cnt,/NOZERO)
  bcd_pix=lonarr(all_bt_cnt,/NOZERO)
  dceid=lonarr(all_bt_cnt,/NOZERO)
  vals=fltarr(all_bt_cnt,/NOZERO)
  if use_unc then unc=fltarr(all_bt_cnt,/NOZERO)
  
  cur=0L                        ;insert location in long all_bt_cnt arrays
  for i=0,goodcnt-1 do begin 
     rec=(*self.DR)[good[i]]
     if ~ptr_valid(rec.REV_ACCOUNT) || rec.DISABLED then continue
     
     w=rec.REV_WIDTH
     w_h=w*rec.REV_HEIGHT
     id=rec.DCEID
     
     ;; Only consider cube pixels within the bounding box of the
     ;; record's slit layout; form the sub-indices of a rectangular
     ;; core.
     targ=[rec.REV_WIDTH,rec.REV_HEIGHT]
     wh=lindgen(targ) + $
        rec.REV_OFFSET[1]*self.CUBE_SIZE[0] + rec.REV_OFFSET[0] + $
        rebin(transpose(lindgen(rec.REV_HEIGHT))* $
              (self.CUBE_SIZE[0]-rec.REV_WIDTH),targ,/SAMPLE)
     targ=[targ,self.CUBE_SIZE[2]]
     wh=rebin(temporary(wh),targ,/SAMPLE) + $
        rebin(reform(lindgen(self.CUBE_SIZE[2])*sz, $
                     1L,1L,self.CUBE_SIZE[2]),targ,/SAMPLE)
     
     ;; Trim WH to match actual reverse indices range
     if rec.REV_MIN gt 0L || rec.REV_CNT lt n_elements(wh) then $
       wh=wh[rec.REV_MIN:rec.REV_CNT-1]
     
     ri=*rec.REV_ACCOUNT
     bcd=*rec.BCD
     if use_unc then bcd_unc=*rec.UNC
     acc=*rec.ACCOUNT
     bpp_ri=*rec.REV_DUAL
     
     for j=0L,rec.REV_DUAL_CNT-1L do begin 
        if bpp_ri[j+1] eq bpp_ri[j] then continue ; none with that many dupes
        in_cnt_bin=bpp_ri[bpp_ri[j]:bpp_ri[j+1]-1] ;indices in this count bin
        nbin=n_elements(in_cnt_bin) ; number of cube pixels which had these counts
        
        cnt_vec[wh[in_cnt_bin]]+=j+1 ;increment the global count record
        
        if j eq 0 then begin    ; cube inds with just 1 back-tracked pixel
           pix=acc[ri[ri[in_cnt_bin]]].BCD_PIX 
        endif else begin        ; cube inds with more than 1 back-tracked pix
           rind=ri[in_cnt_bin]  ; First of the index range into ri
           ;; In each case, take the j+1 indices starting from the first index
           rind=reform(rebin(temporary(rind),nbin,j+1,/SAMPLE)+ $
                       rebin(transpose(lindgen(j+1)),nbin,j+1,/SAMPLE), $
                       nbin*(j+1))
           pix=acc[ri[rind]].BCD_PIX
        endelse 
        bcd_pix[cur]=pix
        vals[cur]=bcd[pix]
        if use_unc then unc[cur]=bcd_unc[pix]
        if j eq 0 then begin 
           track_pix[cur]=wh[in_cnt_bin]
        endif else begin 
           track_pix[cur]=wh[reform(rebin(in_cnt_bin,nbin,j+1L,/SAMPLE), $
                                    nbin*(j+1L))]
       endelse 
        dceid[cur]=make_array(nbin*(j+1L),VALUE=id)
        cur+=nbin*(j+1L)
     endfor  
  endfor 
  
  s=sort(temporary(track_pix))
  bcd_pix=bcd_pix[s]
  vals=vals[s]
  dceid=dceid[s]
  if use_unc then unc=unc[s]
  self->Status,status+'done (cached backtrack)'
end


;=============================================================================
;  AutoBadPixels - Prompt for and set automatic bad pixels
;=============================================================================
pro CubeProj::AutoBadPixels,NO_PROMPT=np,RECORD_BAD_PIX=rbp,_EXTRA=e
  if ~ptr_valid(self.cube) then self->Error,'Cube must be present'
  self->RestoreAll
  ;; Override defaults with prompt
  if self->Showing() && ~keyword_set(no_prompt) then begin 
     if ~obj_valid(self.oautobadpix) then $
        self.oautobadpix=obj_new('CubeAutoBadPix')
     self.oautobadpix->Prompt,/MODAL,GROUP_LEADER=self->TopBase(), $
                              maxvar,minfrac,wb,wu, $
                              DISABLE_WITH_BACKGROUND= $
                              ~ptr_valid(self.BACKGROUND), $
                              DISABLE_WITH_UNC= $
                              (~ptr_valid(self.DR) || $
                               array_equal(ptr_valid((*self.DR).UNC),0b)), $
                              RECORD_ONLY=rbp
     widget_control, /HOURGLASS
     if maxvar eq -1 then return
  endif 
  
  bp=self->GuessBadPix(MAXVAR=maxvar,MINFRAC=minfrac,WITH_BACKGROUND=wb, $
                       WITH_UNC=wu,RECORD_BAD_PIX=rbp,RECORD_INDEX=recids, $
                       _EXTRA=e)
  if bp[0] eq -1 then return
  
  if keyword_set(rbp) then begin 
     ;; Add record bad-pix, one record at a time
     self->GetProperty,BCD_SIZE=sz
     sz=sz[0]*sz[1]
     h=histogram(bp/sz,REVERSE_INDICES=ri,OMIN=om) ;bin by record
     for i=0,n_elements(h)-1 do begin 
        if ri[i] ge ri[i+1] then continue
        self->ToggleBadPixel,bp[ri[ri[i]:ri[i+1]-1]] mod sz,/SET,/AUTO, $
                             RECORD_INDEX=recids[i+om]
     endfor 
     self->Send,/BADPIX_UPDATE
  endif else self->ToggleBadPixel,bp,/SET,/UPDATE,/AUTO
end

;=============================================================================
;  GuessBadPix - Using the cube, guess all bad pixels by looking at
;                the global backtracking, for pixels which deviate
;                from the median of all other pixels by more than 10
;                median-absolute-sigmas (or whatever MAXVAR is set
;                to), at least 50% (or whatever MINFRAC is set to) of
;                the time the BCD pixel appears in the cube.  The
;                background-subtracted values can be used with
;                WITH_BACKGROUND set, in which case the default
;                clipping limits are more inclusive.  The
;                uncertainties can be used as the fiducial variance
;                estimate, rather than a data-based estimate.  If
;                RECORD_BAD_PIX is set, generate record-level bad pix,
;                in which case the return is a 2xn list of (dceid,
;                badpixel) pairs.
;=============================================================================
function CubeProj::GuessBadPix,MAXVAR=maxvar,MINFRAC=minfrac, $
                               WITH_BACKGROUND=wb,WITH_UNC=wu, $
                               RECORD_BAD_PIX=rbp,RECORD_INDEX=recids
  if ~ptr_valid(self.cube) then return,-1
  rbp=keyword_set(rbp) 
  
  t0=systime(1)
  wb=keyword_set(wb) 
  if n_elements(maxvar) eq 0 then maxvar=wb?5.0:10.0
  if n_elements(minfrac) eq 0 then minfrac=wb?.1:.5 
  
  use_bg=self.as_built.use_bg && wb && ptr_valid(self.BACKGROUND)
  
  if ~ptr_valid(self.AUTO_BPL.BCD_PIX) then begin 
     ;; Nothing cached, rebuild full cube backtrack
     restore=0b
     self->BackTrackFullCube,bcd_pix,bcd_vals,cnt_vec,dceid,BCD_UNC=bcd_unc
  endif else begin 
     ;; Use cached info
     restore=1b
     bcd_pix=temporary(*self.AUTO_BPL.BCD_PIX)
     bcd_vals=temporary(*self.AUTO_BPL.BCD_VALS)
     dceid=temporary(*self.AUTO_BPL.DCEID)
     cnt_vec=temporary(*self.AUTO_BPL.CNT_VEC)
     if ptr_valid(self.AUTO_BPL.BCD_UNC) then $
        bcd_unc=temporary(*self.AUTO_BPL.BCD_UNC)
  endelse 
  
  status='Generating '+(rbp?'record':'global')+' auto bad pixels...'
  self->Status,status
  
  self->GetProperty,BCD_SIZE=bcd_sz
  bcd_sz_all=bcd_sz[0]*bcd_sz[1]
  
  if rbp then begin 
     ;; Make it large enough to hold them
     mn=min(dceid,MAX=mx)
     allids=lonarr(size(dceid,/DIMENSION),/NOZERO)
     if mx-mn lt 1000000L then begin 
        h=histogram(dceid,MIN=mn,MAX=mx,REVERSE_INDICES=ri)
        wh=where(h gt 0,cnt)
        recids=self->DCEIDtoRec(mn+wh)
        for i=0,cnt-1 do $
           allids[ri[ri[wh[i]]:ri[wh[i]+1]-1]]=i ;among recids
     endif else begin 
        u=uniq(dceid,sort(dceid))
        recids=self->DCEIDtoRec(dceid[u])
        for i=0,n_elements(u)-1 do begin 
           wh=where(dceid eq dceid[u[i]])
           allids[wh]=i
        endfor
     endelse 
     bad_cnt=lonarr([bcd_sz,n_elements(recids)])
     tot_cnt=lonarr([bcd_sz,n_elements(recids)])
  endif else begin 
     bad_cnt=lonarr(bcd_sz)
     tot_cnt=lonarr(bcd_sz)
  endelse 
  
  use_unc=keyword_set(wu) && n_elements(bcd_unc) gt 0 
  
  ;; cnt_vec has the dimensions of the cube, and bcd_pix/vals/unc have
  ;; the dimensions of the total number of backtrack items.

  ;; Need at least 3 backtracks for useful statistics
  h=histogram(cnt_vec,MIN=3,REVERSE_INDICES=ri)
  
  ;; map inds into long bcd_pix, etc.
  ind_map=shift(total(cnt_vec,/CUMULATIVE,/INTEGER),1) 
  ind_map[0]=0
  
  ;; Looping over backtrack count vector
  for i=0L,n_elements(h)-1L do begin 
     if ri[i] eq ri[i+1] then continue
     n=i+3L                     ;starting with 3, we can't do with less
     targ=[h[i],n]
     in=rebin(ind_map[ri[ri[i]:ri[i+1]-1]],targ,/SAMPLE) + $
        rebin(transpose(lindgen(n)),targ,/SAMPLE)
     
     pix=bcd_pix[in]
     vals=bcd_vals[in]
     if rbp then ids=allids[in]
     if use_unc then unc=bcd_unc[in]
     
     if use_bg then vals-=(*self.BACKGROUND)[pix]
     
     med=median(vals,DIMENSION=2,/EVEN)
     dev=abs(vals-rebin(med,targ,/SAMPLE))
     
     if n eq 3L then begin      ;for the case of 3, use a different method
        ;; At most one bad pixel, maximum deviation
        wh=where(dev eq 0.0,cnt) ;flag out the actual median.
        if cnt gt 0 then dev[wh]=!VALUES.F_NAN ;don't let actual median in
        mx=max(dev,DIMENSION=2,MIN=mn,mxpos,/NAN)
        if use_unc then typical_dev=median(unc,DIMENSION=2,/EVEN) else $
           typical_dev=mn
        bad=where(mx gt maxvar*typical_dev,cnt)
        if cnt gt 0 then bad=mxpos[bad]
     endif else begin 
        if use_unc then typical_dev=median(unc,DIMENSION=2,/EVEN) else begin 
           wh=where(dev eq 0.0,cnt)
           if cnt gt 0 then dev[wh]=!VALUES.F_NAN 
           typical_dev=median(dev,DIMENSION=2,/EVEN)
        endelse 
        bad=where(dev gt maxvar*rebin(typical_dev,targ,/SAMPLE),cnt)
     endelse 
     
     if rbp then begin 
        if cnt gt 0 then bad_cnt[pix[bad]+bcd_sz_all*ids[bad]]++
        tot_cnt[pix+bcd_sz_all*ids]++
     endif else begin 
        if cnt gt 0 then bad_cnt[pix[bad]]++
        tot_cnt[pix]++
     endelse 
  endfor 
  
  if restore then begin 
     *self.AUTO_BPL.BCD_PIX=temporary(bcd_pix)
     *self.AUTO_BPL.BCD_VALS=temporary(bcd_vals)
     *self.AUTO_BPL.DCEID=temporary(dceid)
     if n_elements(bcd_unc) gt 0 then $
        *self.AUTO_BPL.BCD_UNC=temporary(bcd_unc)
     *self.AUTO_BPL.CNT_VEC=temporary(cnt_vec)
  endif else begin 
     self.AUTO_BPL.BCD_PIX=ptr_new(bcd_pix,/NO_COPY)
     self.AUTO_BPL.BCD_VALS=ptr_new(bcd_vals,/NO_COPY)
     self.AUTO_BPL.DCEID=ptr_new(dceid,/NO_COPY)
     if n_elements(bcd_unc) gt 0 then $
        self.AUTO_BPL.BCD_UNC=ptr_new(bcd_unc,/NO_COPY)
     self.AUTO_BPL.CNT_VEC=ptr_new(cnt_vec,/NO_COPY)
  endelse 
  
  allbad=where(float(bad_cnt)/(tot_cnt>1L) gt minfrac,allbadcnt)
  
  form='"done in ",F0.2,"s with ",I0," badpix"'
  self->Status,status+ $
               string(FORMAT='('+form+')',systime(1)-t0,allbadcnt)

  return,allbad
end


;=============================================================================
;  Extract - Extract a Spectrum from the Cube, and possibly save it
;             EXPORT - If set, export extracted spectrum to command line
;             SAVE - File to save to.
;             FROM_FILE - Load extraction region from spectrum file
;=============================================================================
function CubeProj::Extract,low,high, SAVE=sf, EXPORT=exp, FROM_FILE=rff, $
                           OUTPUT_POLY=op,REGION=oReg,PACKAGE_STRUCT=pkg, $
                           OVERLAP_ERROR=err,UNCERTAINTY=sp_unc,_EXTRA=e
  if ~ptr_valid(self.CUBE) then self->Error,'No cube to extract'
  
  use_unc=ptr_valid(self.CUBE_UNC)
  
  if keyword_set(rff) then begin
     oSP=obj_new('IRS_Spectrum',FILE_BASE=self->FileBaseName()+'_'+ $
                 self.MODULE+(self.ORDER gt 0?strtrim(self.ORDER,2):''), $
                 PARENT_GROUP=self->TopBase())
     oSP->Read,rff
     oSP->GetProperty,REGION=oReg
     if ~obj_valid(oReg) then return,-1
  endif
  
  if obj_valid(oReg) then begin ;we've got a region object
     ;; Celestial coord region: convert ra,dec to x,y
     r=oReg->Region()           ;first region
     self->ConvertCoords,r[0,*],r[1,*],x,y,/TO_X_Y 
     if arg_present(op) then op=[x,y]
     overlap_pix=polyfillaa(x,y,self.CUBE_SIZE[0],self.CUBE_SIZE[1], $
                            AREAS=areas)
     err=0b
     if overlap_pix[0] eq -1 then begin 
        err=1b
        self->Error,'Selected aperture does not overlap cube' 
     endif else if min(x,MAX=mxx) lt 0 or mxx gt self.CUBE_SIZE[0] or $
        min(y,MAX=mxy) lt 0 or mxy gt self.CUBE_SIZE[1] then begin 
        err=2b
        self->Warning,'Aperture lies partially outside of cube.'
     endif 
     nover=n_elements(overlap_pix)
     
     pix=rebin(self.CUBE_SIZE[0]*self.CUBE_SIZE[1]* $
               transpose(findgen(self.CUBE_SIZE[2])), $
               nover, self.CUBE_SIZE[2],/SAMPLE) + $
         rebin(overlap_pix,nover,self.CUBE_SIZE[2],/SAMPLE)
     area=rebin(areas,nover,self.CUBE_SIZE[2],/SAMPLE)
     core=(*self.CUBE)[pix]
     area_tot=total(areas*finite(core),1)
     sp=total(core*area,1,/NAN)/area_tot
     if use_unc then $
        sp_unc=sqrt(total((*self.CUBE_UNC)[pix]^2*area^2,1,/NAN))/area_tot
  endif else begin 
     ;; Regular "four corners" integral pixel extraction
     max=(size(*self.CUBE,/DIMENSIONS))[0:1]-1
     low=0>low<max
     high=0>high<max
     sp=total(total((*self.CUBE)[low[0]:high[0],low[1]:high[1],*],1,/NAN), $
              1,/NAN)/(high[1]-low[1]+1.)/(high[0]-low[0]+1.)
     if use_unc then begin 
        sp_unc=sqrt(total(total((*self.CUBE_UNC)[low[0]:high[0], $
                                                 low[1]:high[1],*]^2,1,/NAN), $
                          1,/NAN))/(high[1]-low[1]+1.)/(high[0]-low[0]+1.)
     endif 
     
     h=high+1
     op=[[low[0],low[1]], $
         [low[0],h[1]], $
         [h[0],h[1]], $
         [h[0],low[1]]]
     
     ;; Output region, from polygon
     if arg_present(oReg) then begin 
        oReg=obj_new('IRS_Region')
        oReg->SetRegion,op,ASTROMETRY=self->CubeAstrometryRecord()
     endif 
  endelse 
  
  if keyword_set(sf) then $
     self->SaveSpectrum,sf,sp,oSP,POLYGON=op,UNCERTAINTY=sp_unc,_EXTRA=e

  if keyword_set(exp) then begin 
     if use_unc then      $
        self->ExportToMain, SPECTRUM=transpose([[*self.WAVELENGTH], $
                                                [sp],[sp_unc]]) $
     else self->ExportToMain, SPECTRUM=transpose([[*self.WAVELENGTH],[sp]])
  endif 
  if n_elements(oSP) ne 0 then $
     obj_destroy,oSP,NO_REGION_DESTROY=arg_present(oReg)
  if keyword_set(pkg) then begin 
     if n_elements(sp_unc) ne 0 then $
        rec={WAVELENGTH:0.0,FLUX:0.0,UNCERTAINTY:0.0} else $
           rec={WAVELENGTH:0.0,FLUX:0.0}
     ret=replicate(rec,n_elements(sp))
     ret.WAVELENGTH=*self.WAVELENGTH
     ret.FLUX=sp
     if n_elements(sp_unc) ne 0 then ret.UNCERTAINTY=sp_unc
     return,ret
  endif else return,sp
end


;=============================================================================
;  AddOutputInfo - Add the output info only we know.
;=============================================================================
pro CubeProj::AddOutputInfo,out_obj,_EXTRA=e
  calname=self.cal_file
  fullname=irs_fov(MODULE=self.MODULE,ORDER=self.ORDER,POSITION=0,/SLIT_NAME, $
                   /LOOKUP_MODULE)
  out_obj->SetProperty,SOFTWARE='CUBISM',VERSION=self.version, $
                       FLUX_UNITS=self->FluxUnits(/AS_BUILT,_EXTRA=e), $
                       CAL_SET=calname,APERNAME=fullname
  widget_control,/HOURGLASS  
end


;=============================================================================
;  ParseCubeHeaderInfo - Read local information from the cube header.
;=============================================================================
pro CubeProj::ParseCubeHeaderInfo,in_obj
  in_obj->GetProperty,HEADER=hdr
  
  self.CUBE_SIZE=sxpar(hdr,'NAXIS*')
  
  ;; Here we assume the first header gives a valid step setup
  self.NSTEP=[sxpar(hdr,'STEPSPER'),sxpar(hdr,'STEPSPAR')]
  self.STEP_SIZE=[sxpar(hdr,'SIZEPER'),sxpar(hdr,'SIZEPAR')]/3600.D
end

;=============================================================================
;  SaveCube - Save the cube using a cube object
;=============================================================================
pro CubeProj::SaveCube,file,COMMENTS=comm
  if ~ptr_valid(self.cube) || ~ptr_valid(self.WAVELENGTH) then return

  oC=obj_new('IRS_Cube',FILE_BASE=self->FileBaseName()+'_'+ $
             self.MODULE+(self.ORDER gt 0?strtrim(self.ORDER,2):''), $
             PARENT_GROUP=self->TopBase(),/FITS)
  
  oC->SaveInit,file
  if size(file,/TYPE) ne 7 then begin 
     obj_destroy,oC
     return
  endif 
  
  self->AddOutputInfo,oC
  oC->SetProperty,CUBE_FLUX=*self.cube,WAVELENGTH=*self.wavelength, $
                  WAVE_UNITS='um',CUBE_DATE=self.CUBE_DATE, $
                  ASTROMETRY=self->CubeAstrometryRecord()
  
  if ptr_valid(self.cube_unc) then $
     oC->SetProperty, CUBE_UNCERTAINTY=*self.cube_unc
  
  oC->InitHeader
  
  ;; Add the first header as inheritance
  self->AddFirstHeader,oC,/STRIP

  oC->AddHist,['This file contains a 3D spectral cube created from',$
                'an IRS spectral mapping dataset.'],/COMMENT
  if n_elements(comm) ne 0 then oC->AddHist,comm,/COMMENT

  oC->Save,file
  obj_destroy,oC
end

;=============================================================================
;  LoadCubeFromFITS - Load a cube from a FITS file (not all data complete)
;=============================================================================
pro CubeProj::LoadCubeFromFITS,file
  oC=obj_new('IRS_Cube')
  oC->Read,file
  self->ParseCubeHeaderInfo,oC
  
  oC->GetProperty,CUBE_FLUX=cube,CUBE_UNCERTAINTY=cunc,ASTROMETRY=astr, $
                  CUBE_DATE=date,WAVELENGTH=wl,CAL_SET=cal,APERNAME=aper, $
                  SOFTWARE=soft, VERSION=soft_ver,FLUX_UNITS=un
  
  if soft ne 'CUBISM' then begin 
     obj_destroy,oC
     self->Error,'Not a CUBISM FITS cube.'
  endif 
  
  void=irs_fov(strtrim(aper,2)+'_cen',MODULE=md,ORDER=ord,/SHORT_NAME)
  self.MODULE=md
  self.ORDER=ord
  self.VERSION=soft_ver
  
  self.cal_file=cal
  
  
  ptr_free,self.cube,self.cube_unc,self.WAVELENGTH,self.RECOV_ASTROM
  self.cube=ptr_new(cube,/NO_COPY)
  self.WAVELENGTH=ptr_new(wl)
  if n_elements(cunc) gt 0 then self.cube_unc=ptr_new(cunc,/NO_COPY)
  self.cube_date=date
  self.RECOV_ASTROM=ptr_new(astr)
  self.ProjectName=file_basename(file,'.fits')+' [FITS]'
  
  if un eq 'MJy/sr' || un eq 'Jy/pix' then self.fluxcon=1b
  if stregex(un,'/sr',/BOOLEAN) then self.pix_omega=1b
  obj_destroy,oC
end

;=============================================================================
;  SaveMap - Save a Stacked Map to FITS (possibly with uncertainty plane)
;=============================================================================
pro CubeProj::SaveMap,map,sf,COMMENTS=comm,UNCERTAINTY=unc,_EXTRA=e

  oM=obj_new('IRS_Map',FILE_BASE=self->FileBaseName()+'_'+ $
             self.MODULE+(self.ORDER gt 0?strtrim(self.ORDER,2):''), $
             PARENT_GROUP=self->TopBase(),/FITS,_EXTRA=e)
  
  oM->SaveInit,sf
  if size(sf,/TYPE) ne 7 then begin 
     obj_destroy,oM
     return
  endif 
  
  self->AddOutputInfo,oM,_EXTRA=e
  oM->SetProperty,MAP_FLUX=map, ASTROMETRY=self->CubeAstrometryRecord()
  
  if array_equal(size(unc,/DIMENSIONS),size(map,/DIMENSIONS)) then $
     oM->SetProperty,MAP_UNCERTAINTY=unc
  
  oM->InitHeader
  
  ;; Add the first header as inheritance
  self->AddFirstHeader,oM,/STRIP
  
  oM->AddHist,['This file contains a 2D spectral map created from',$
               'an IRS spectral mapping dataset.'],/COMMENT
  if n_elements(comm) ne 0 then oM->AddHist,comm,/COMMENT

  oM->Save,sf
  obj_destroy,oM
end

;=============================================================================
;  AddFirstHeader - Add details from the first in-cube header.
;=============================================================================
pro CubeProj::AddFirstHeader,obj,_EXTRA=e
  if ~ptr_valid(self.DR) then return
  wh=where((*self.DR).IN_CUBE,cnt)
  if cnt eq 0 then self->Error,'No available records in built cube.'
  m=min((*self.DR)[wh].DCEID,pos)
  h1=(*self.DR)[wh[pos]].HEADER
  if ~ptr_valid(h1) then self->RestoreData,wh[pos]
  h1=*h1
  obj->InheritHeader,h1,'KEYWORDS FROM FIRST BCD',_EXTRA=e
end

;=============================================================================
;  SaveSpectrum - Save a Spectrum using a spectrum object
;=============================================================================
pro CubeProj::SaveSpectrum,file,sp,oSP,UNCERTAINTY=unc,POLYGON=op,COMMENTS=comm
  if ~obj_valid(oSP) then begin 
     oSP=obj_new('IRS_Spectrum',FILE_BASE=self->FileBaseName()+'_'+ $
                 self.MODULE+(self.ORDER gt 0?strtrim(self.ORDER,2):''), $
                 PARENT_GROUP=self->TopBase())
     ;; If the region was created by hand, create a region object for it
     if n_elements(op) ne 0 then begin 
        oReg=obj_new('IRS_Region')
        oReg->SetRegion,op,ASTROMETRY=self->CubeAstrometryRecord()
        oSP->SetProperty,REGION=oReg
     endif 
     destroy=1
  endif else begin 
     ;; Re-use a given region: it must have been extracted from that.
     oSP->GetProperty,REGION=oReg
     oReg->UpdateAstrometry,self->CubeAstrometryRecord()
  endelse 
  
  oSP->SaveInit,file
  if size(file,/TYPE) ne 7 then begin 
     if n_elements(destroy) ne 0 then obj_destroy,oSP
     return
  endif 
  
  oSP->SetProperty,SPECTRUM_FLUX=sp,SPECTRUM_UNCERTAINTY=unc, $
                   WAVELENGTH=*self.wavelength, WAVE_UNITS='um'
  
  self->AddOutputInfo,oSP
  
  oSP->InitHeader
  
  ;; Add the first header as inheritance
  self->AddFirstHeader,oSP
  
  oSP->AddHist,['This file contains a spectral extraction created from',$
                'an IRS spectral mapping dataset.'],/COMMENT
  
  if n_elements(comm) ne 0 then oSP->AddHist,comm,/COMMENT

  oSP->Save,file
  
  if n_elements(destroy) ne 0 then obj_destroy,oSP
end


;=============================================================================
;  Stack - Build a stacked image map from the constructed cube between
;          any number of sets of wavelength intervals, or optionally
;          weighting the individual planes with WEIGHTS.
;
;             FORERANGES: A 2xn list of foreground index ranges over
;                         which to stack.  Serve as output when
;                         MAP_NAME is passed.
;
;             BACKRANGES: A 2xn list of background index ranges
;                         over which to stack.
;
;             BG_VALS: A vector of continuum values underlying the
;                      foreground ranges (Probably bogus to assume the
;                      same set over a full map).
;
;             WEIGHTS: A 2xn weight vector list: lambda vs. weight
;                      [0-1] for weighting the foreground.  Will be
;                      interpolated onto the actual wavelength
;                      samples.  Overrides foreranges
;
;             MAP_NAME: A named map set to pull from the archive.
;                       Ranges and weights from the named map will
;                       override any passed otherwise.
;
;             WAVELENGTH_WEIGHTED: Rather than a simple average of
;                      backgrounds, at each foreground wavelength, a
;                      weighted average of all BG planes will be
;                      performed, with more weight given to planes
;                      according to their proximity by in wavelength.
;
;             INTEGRATE: Integrate over the map, rather than
;                      averaging.  Changes units from flux density to
;                      flux.
;
;=============================================================================
function CubeProj::Stack,foreranges,BACKRANGES=backranges,WEIGHTS=weights, $
                         BG_VALS=bg_vals,MAP_NAME=mname, SAVE=save, $
                         CONTINUUM_SAVE=save_c,CONTINUUM_IMAGE=background, $
                         CONTINUUM_UNCERTAINTY=background_unc, $
                         WAVELENGTH_WEIGHTED=ww, ALL=all, TEMP_STACK=ts, $
                         COMMENTS=comm, INTEGRATE=int, UNITS=units,$
                         STACK_UNCERTAINTY=stack_unc,_EXTRA=e
  
  if ~ptr_valid(self.CUBE) then self->Error,'No cube to stack'
  ;; Get a map set by name
  wl=*self.WAVELENGTH
  
  if n_elements(mname) ne 0 then begin 
     oMap=IRSMapSet()
     oMap->GetMap,mname,FORERANGES=foreranges, $
                  BACKRANGES=backranges, WAVELENGTH_CONVERT=wl, $
                  WEIGHTS=weights, _EXTRA=e
  endif else if keyword_set(all) then $
     foreranges=[0,n_elements(wl)-1]
  
  sf=size(foreranges,/DIMENSIONS)
  if n_elements(foreranges) ne 0 then begin 
     if foreranges[0] eq -1 then nfr=0 else $
        if n_elements(sf) eq 2 then nfr=sf[1] else nfr=1
  endif else nfr=0
  
  nbr=0
  if n_elements(backranges) ne 0 then begin 
     if backranges[0] ne -1 then begin 
        sb=size(backranges,/DIMENSIONS)
        if n_elements(sb) eq 2 then nbr=sb[1] else nbr=1
     endif 
  endif
  
  
  ;; Total number of planes involved
  if nfr gt 0 then fore_cnt=long(total(foreranges[1,*]-foreranges[0,*]+1))
  if nbr gt 0 then back_cnt=long(total(backranges[1,*]-backranges[0,*]+1))
  
  ;; Fixed background values
  nbgv=n_elements(bg_vals)
  if nbgv gt 0 && nbgv ne fore_cnt then $
     self->Error,'Wrong number of background values passed'
  
  if keyword_set(ww) && nbr gt 0 && back_cnt le 1 then ww=0
  
  use_unc=(arg_present(stack_unc) || keyword_set(stack_unc)) $
          && ptr_valid(self.CUBE_UNC)
  
  use_weights=keyword_set(weights) && weights[0] ne -1 && $
     size(weights,/N_DIMENSIONS) eq 2
  
  lam_weight=keyword_set(ww)
  integrate=keyword_set(int) 
  if integrate && ~self.as_built.fluxcon then $
     self->Error,'Integrated maps available for fluxed cubes only'
    
  if use_weights then begin     ;Overrides any foreground range passed
     ;; Disable all backgrounds
     nbgv=0 & ww=0 & nbr=0
     nfr=1
     foreranges=[0,self.CUBE_SIZE[2]-1]
     fore_cnt=self.CUBE_SIZE[2]
  endif 
  
  ;;--- Setup some useful background arrays beforehand
  use_bg=0
  if nbgv eq 0 then begin
     if lam_weight then begin 
        ;; Wavelength weighted Backgrounds
        if nbr eq 0 then $
           self->Error,'Must select continuum regions for wavelength weighting.'
        
        use_bg=1
        back_wav=fltarr(back_cnt) & back_planes=lonarr(back_cnt)
        cnt=0
        for i=0,nbr-1 do begin 
           this_cnt=backranges[1,i]-backranges[0,i]+1
           back_wav[cnt]=wl[backranges[0,i]:backranges[1,i]]
           back_planes[cnt]=backranges[0,i]+lindgen(this_cnt)
           cnt+=this_cnt
        endfor
     
        ;; All the planes in the background in one cube
        back_cube=(*self.CUBE)[*,*,back_planes]
        if use_unc then back_cube_unc=(*self.CUBE_UNC)[*,*,back_planes]
        back_finite=finite(back_cube)
     endif else if nbr gt 0 then begin 
        ;; Normal "averaged" backgrounds
        use_bg=1
        bcnt=lonarr(self.CUBE_SIZE[0:1])
        background=fltarr(self.CUBE_SIZE[0:1])
        if use_unc then background_unc=background
        for i=0,nbr-1 do begin 
           if backranges[0,i] eq backranges[1,i] then begin 
              background+=(*self.CUBE)[*,*,backranges[0,i]]
              if use_unc then $
                 background_unc=sqrt(background_unc^2+ $
                                     (*self.CUBE_UNC)[*,*,backranges[0,i]]^2)
              bcnt+=long(finite((*self.CUBE)[*,*,backranges[0,i]]))
           endif else begin 
              background+=total((*self.CUBE)[*,*,backranges[0,i]: $
                                             backranges[1,i]], /NAN,3)
              if use_unc then $
                 background_unc=sqrt(background_unc^2+ $
                                     total((*self.CUBE_UNC) $
                                           [*,*,backranges[0,i]:$
                                            backranges[1,i]]^2,/NAN,3))
              bcnt+=long(total(finite((*self.CUBE)[*,*,backranges[0,i]: $
                                                   backranges[1,i]]),3))
           endelse 
        endfor 
        bcnt>=1L
        background/=bcnt        ;average background, for *all* planes
        if use_unc then background_unc/=bcnt
     endif 
  endif else begin 
     use_bg=1
     use_unc=0
  endelse 

  ;;---- Compute the foreground stack
  stack=fltarr(self.CUBE_SIZE[0:1])
  if use_unc then stack_var=stack
  stack_cnt=lonarr(self.CUBE_SIZE[0:1])
  
  if integrate then begin 
     ;; Avoid roundoff.  Exponent in conversion:
     ;;   Jy -> W/m^2 or MJy/sr -> W/m^2/sr
     post_fac=self.as_built.pix_omega?1.e-6:1.e-12
     delta_nu_sum=0.0
  endif 
  
  ;mostly the same at each position
  if use_bg then back_weights_accum=fltarr([self.cube_size[0:1],back_cnt])
  
  for i=0,nfr-1 do begin ;; Iterate over all foreground ranges
     ;; First compute the per-plane background (and uncertainty) cube,
     ;; either a normal average, or "wavelength-weighted" average.  This
     ;; subtraction method is used even when integrating, or performing a
     ;; weighted integral or average using a weight vector.
     
     this_fore_cnt=foreranges[1,i]-foreranges[0,i]+1
     fore_wav=wl[foreranges[0,i]:foreranges[1,i]]
     
     ftarg=[self.CUBE_SIZE[0:1],this_fore_cnt]
     
     ;; Setup concatenated cube for this particular foreground range
     range_cube=(*self.CUBE)[*,*,foreranges[0,i]:foreranges[1,i]]
     if use_unc then range_cube_unc= $
        (*self.CUBE_UNC)[*,*,foreranges[0,i]:foreranges[1,i]]
     range_cube_finite=total(finite(range_cube),3)

     ;; Setup weightings and normalizations
     bottom_norm=1. & top_weight=1.
     if integrate then begin ;; integrate f_nu d_nu (or weight-avg with d_nu)
        delta_nu=fore_wav[1:*]-fore_wav
        conv=2.9979246/fore_wav^2 ;flux density to flux, modulo post_fac
        delta_nu=conv*[delta_nu[0], $
                       (delta_nu[1:*]+delta_nu)/2., $
                       delta_nu[n_elements(fore_wav)-2]]
        top_weight=delta_nu
        if use_unc && use_bg then delta_nu_sum+=total(delta_nu)
        bottom_norm=(range_cube_finite>1)/this_fore_cnt ;finite fraction
     endif 
     
     if use_weights then begin  ;A weight vector overrides foreground regions
        ;; An integrated weight vector is "synthetic photometry
        ;; style", aka sum(f_nu w d_nu)/sum(w d_nu)
        weight_vec=interpol(weights[1,*],weights[0,*],fore_wav)
        top_weight*=weight_vec
        bottom_norm*=integrate?total(weight_vec*delta_nu):total(weight_vec)
     endif 
     
     ;; Setup backgrounds (normal, or wavelength-weighted): not
     ;; applicable for Weight Maps
     if nbgv gt 0 then begin 
        ;; "One-value-per-plane" BG-subtraction
        if n_elements(bgv_off) eq 0 then bgv_off=0L
        these_bgs=rebin(reform(bg_vals[bgv_off:bgv_off+this_fore_cnt-1], $
                             1,1,this_fore_cnt),ftarg,/SAMPLE)
        bgv_off+=this_fore_cnt
     endif else if lam_weight then begin 
        ;; Wavelength-weighted BG averaging
        these_bgs=fltarr(ftarg)   ;different for each plane!
        if use_unc then these_bgs_unc=these_bgs
        
        for j=0,this_fore_cnt-1 do begin 
           ;; a background value for each foreground plane
           back_weights=rebin(reform(1./(1.+abs(fore_wav[j]-back_wav)), $
                                     [1,1,back_cnt]), $
                              [self.CUBE_SIZE[0:1],back_cnt],/SAMPLE)
           ;; normalized weights for this wavelength (all background frames)
           wvec=total(back_weights*back_finite>1.e-8,3)
           back_weights/=rebin(wvec,size(back_weights,/DIMENSIONS),/SAMPLE)
           
           if integrate then $
              back_weights_accum+=back_weights*delta_nu[j] $
           else back_weights_accum+=back_weights ;normal FG averaging
           these_bgs[0,0,j]=total(back_cube*back_weights*back_finite,3,/NAN)
        endfor
     endif else if nbr gt 0 then begin 
        ;; Normal BG-averaging
        these_bgs=rebin(background,ftarg,/SAMPLE) ; pre-averaged BG
     endif 

     ;; Background-subtract it
     if use_bg then range_cube-=temporary(these_bgs)
     
     ;; To test a given set, make a 'test' map, and:
     ;;
     
     ;; Sum and normalize this range's stack
     if integrate || use_weights then begin 
        top_weight=rebin(reform(top_weight,1,1,this_fore_cnt,/OVERWRITE), $
                         ftarg,/SAMPLE)
        this_stack=total(range_cube*top_weight, 3,/NAN)
        if use_unc then $
           this_stack_var=total(range_cube_unc^2*top_weight^2,3,/NAN)
        this_stack/=bottom_norm
        if use_unc then this_stack_var/=bottom_norm^2
     endif else begin ;; simple average, no weights or normalizations
        this_stack=total(range_cube,3,/NAN)
        if use_unc then this_stack_var=total(range_cube_unc^2,3,/NAN)
        stack_cnt+=range_cube_finite
     endelse 
     
     stack+=this_stack
     if use_unc then stack_var+=this_stack_var
  endfor

  ;; Normalize
  stack_cnt>=1L
  if ~integrate && ~use_weights then begin 
     ;; No integration or weighting
     stack/=stack_cnt
     if use_unc then stack_var/=stack_cnt^2
  endif 
  
  ;; Add in uncertainty from background
  if use_unc && use_bg then begin 
     ;; With the accumulated weights, we can now compute the BG uncertainty
     if lam_weight then $
        background_unc=sqrt(total(back_finite* $
                                  back_weights_accum^2*back_cube_unc^2, $
                                  3,/NAN))/stack_cnt $
     else if integrate then background_unc*=delta_nu_sum 
     stack_var+=background_unc^2
  endif 
  
  if use_unc then stack_unc=sqrt(temporary(stack_var))
  
  ;; Put post-multiply factors back in (except for weight maps: they
  ;; are weighted by d_nu only)
  if integrate && ~use_weights then begin 
     stack*=post_fac
     if use_unc then stack_unc*=post_fac
  endif 
  
  ;; Form comments regarding the stack
  if nfr gt 0 then begin 
     if use_weights then extras="weight"
     if n_elements(mname) ne 0 then if n_elements(extras) eq 0 then $
        extras="map "+mname else extras=[extras,"map "+mname]
     if integrate then if n_elements(extras) eq 0 then $
        extras="int d_nu" else extras=[extras,"int d_nu"]
     
     if n_elements(extras) ne 0 then extra=' ('+strjoin(extras,", ")+')' else $
        extra=""
     
     reg_comm=["Foreground regions"+extra+":",$
               "  "+strjoin(string(format='(F6.3,"um -> ",F6.3,"um")', $
                                   wl[foreranges]),", ")]
  endif 
  
  ;; Comment on the background
  if nbr gt 0 then begin 
     if n_elements(mname) ne 0 then bextra="map "+mname
     if lam_weight then begin 
        if n_elements(bextra) eq 0 then bextra="wave-weighted" $
        else bextra=[bextra,"wave-weighted"]
     endif 
     
     if n_elements(bextra) ne 0 then extra=" ("+strjoin(bextra,", ")+")" $
     else extra=""

     back_comm=["Continuum regions" + extra +":", $
                "  "+strjoin(string(format='(F6.3,"um -> ",F6.3,"um")', $
                                    wl[backranges]),", ")]
     if n_elements(reg_comm) eq 0 then reg_comm=back_comm else $
        reg_comm=[reg_comm,back_comm]
  endif 
   
  if n_elements(reg_comm) ne 0 then begin 
     if n_elements(comm) eq 0 then comment=reg_comm $
     else comment=[comm,reg_comm]
  endif 
  
  ;; Set units
  units=self->FluxUnits(INTEGRATE=integrate && ~use_weights,/AS_BUILT)
  
  ;; Possibly save the stack
  if keyword_set(save) then self->SaveMap,stack,save,COMMENTS=comment, $
                                          INTEGRATE=int && ~use_weights, $
                                          UNCERTAINTY=stack_unc
  if keyword_set(save_c) && n_elements(background) gt 0 $
  then self->SaveMap,background,save_c,COMMENTS=back_comm, $
                     UNCERTAINTY=background_unc
  return,stack
end 


;=============================================================================
;  CheckSteps - Ensure that all of the BCD's for this map are present.
;=============================================================================
pro CubeProj::CheckSteps
  ;; XXX Account for multiple sub-slit maps or multiple map cycles?
  if NOT ptr_valid(self.DR) then self->Error,'No BCD Data loaded.'
  if ~array_equal(self.NSTEP gt 0,1b) then $
     self->Error,'No map coordinates set.'
  got=bytarr(self.NSTEP)
  got[(*self.DR).ROW-1,(*self.DR).COLUMN-1]=1b
  wh=where(got eq 0b, cnt)
  if cnt eq 0 then return
  self->Error,['Missing Steps: ',$
               string(FORMAT='('+strtrim(cnt,2)+'("[",I0,", ",I0,"]"))', $
                      [1#(wh mod self.NSTEP[0]+1),1#(wh/self.NSTEP[0]+1)])]
end

;=============================================================================
;  CheckDCEIDs - Ensure all DCEIDs are unique, and remove duplicates
;=============================================================================
pro CubeProj::CheckDCEIDs
  if ~ptr_valid(self.DR) then return
  dceids=(*self.DR).DCEID
  u=uniq(dceids,sort(dceids))
  if n_elements(u) lt n_elements(dceids) then begin 
     self->Warning,'Duplicate records detected, removing!'
     remove=bytarr(n_elements(dceids))
     for i=0,n_elements(u)-1 do begin 
        wh=where(dceids eq dceids[u[i]],cnt)
        if cnt eq 1 then continue
        void=max((*self.DR)[wh].DATE,pos)
        dups=where(indgen(cnt) ne pos)
        remove[wh[dups]]=1b
     endfor 
     wh=where(remove,cnt)
     if cnt gt 0 then self->RemoveBCD,wh
  endif 
end

;=============================================================================
;  CheckModules - Make sure only data from one module is present, and
;                 select default build order.
;=============================================================================
pro CubeProj::CheckModules,id,ERROR=err
  ;; Check the data's module type
  err=0
  nr=self->N_Records()

  if n_elements(id) ne 0 then begin ;just check this id against the new one
     if nr eq 0 then self.MODULE='' ;no module without some data records
     fovname=irs_fov(id,MODULE=module)
     if self.MODULE ne '' then begin 
        if module ne self.MODULE then begin 
           err=1
           self->Error,["Data from only one module is permitted: ", $
                        "   "+self.module+","+module],/RETURN_ONLY
        endif 
     endif else self.MODULE=module ;no module yet, just set this one
     return                     
  endif
  
  modules=strarr(nr)
  orders=intarr(nr)
  for i=0,n_elements(*self.DR)-1 do begin 
     fovname=irs_fov((*self.DR)[i].FOVID,MODULE=module,ORDER=order)
     modules[i]=module
     orders[i]=order
  endfor
  u=uniq(modules,sort(modules))
  if n_elements(u) gt 1 then $
     self->Error,["Data from only one module is permitted: ", $
                  "   "+strjoin(modules[u],",")],/RETURN_ONLY
  self.module=modules[u[0]]
  
  if (self.module eq 'LL' || self.module eq 'SL') && self.ORDER eq 0 $
  then begin 
     if array_equal(orders,0) then begin 
        ;; Just a default order
        self.ORDER=(self.cal->Orders(self.module))[0]
     endif else if array_equal(orders,orders[0]) then $
        ;; All orders the same
        self.ORDER=orders[0] $
     else begin 
        ;; Pick the most represented order
        h=histogram(BINSIZE=1,orders,OMIN=om)
        mx=max(h,mpos)
        self.ORDER=om+mpos
     endelse 
  endif 
end

;=============================================================================
;  ShowAperture
;=============================================================================
pro CubeProj::ShowAperture
  if NOT self->IsWidget() then return
  if NOT ptr_valid(self.APERTURE) then begin 
     self->Info,'No apertures defined'
     return
  endif 
  
  nap=n_elements(*self.APERTURE)
  ords=self.cal->Orders(self.module)
  ap_str=strarr(nap)
  for i=0,nap-1 do begin 
     if self.ORDER eq 0 then begin 
        if nap eq 1 then ap_pref="All: " else $
           ap_pref=string(FORMAT='("Ord ",I2,":")',ords[i])
     endif else ap_pref=string(FORMAT='("Ord ",I2,":")',self.ORDER)
     
     ap=(*self.APERTURE)[i]
     if ap.low[0] eq ap.low[1] AND ap.high[0] eq ap.high[1] then $
        ap_str[i]=string(FORMAT='(%"%4.2f->%4.2f")', ap.low[0],ap.high[0]) $
     else ap_str[i]=string(FORMAT='(%"%4.2f->%4.2f : %4.2f->%4.2f")', $
                           ap.low,ap.high)
     ap_str[i]=ap_pref+ap_str[i]
  endfor 
  
  self->Info,['Apertures: ','',ap_str],TITLE=self->ProjectName()+' Apertures'
end
     
;=============================================================================
;  AddGroup - Add a group of data files from a directory, with choice
;             of module(s) and observations, and grouping by spectral
;             map (=object+aor+fovid, default) or module only.
;=============================================================================
pro CubeProj::AddGroup,DIR=dir,MODULE_ONLY=md,COADD=cd,DROOPRES=dr,FLATAP=fl
  if n_elements(dir) eq 0 then begin 
     type=keyword_set(cd)?"Coadd":(keyword_set(dr)? $
          "Droopres":(keyword_set(fl)?"FlatAp":"BCD"))
     xf,dir,/DIRECTORY,/RECENT,TITLE='Select '+type+' AOR Directory', $
        PARENT_GROUP=self->TopBase(),/MODAL
  endif 
  if size(dir,/TYPE) ne 7 then return
  
  if ~file_test(dir,/DIRECTORY) then $
     self->Error,'Must select directory.'
  if keyword_set(cd) then begin 
     type='Coadd' & filt='*coa*2d.fits' 
  endif else if keyword_set(dr) then begin 
     type='DroopRes' & filt='*droop{,res}.fits'
  endif else if keyword_set(fl) then begin 
     type='FlatAp' & filt='*f2ap.fits'
  endif else begin 
     type='BCD' & filt='*bcd{,_fp}.fits'
  endelse
  
  widget_control, /HOURGLASS
  files=file_search(dir,filt,/TEST_REGULAR,COUNT=cnt)
  if cnt eq 0 then $
     self->Error,['No data found in:',dir],TITLE='Data Import Error'
  if cnt gt 500 then begin 
     ans=dialog_message(["Selected directory contains "+strtrim(cnt,2)+ $
                         " data files. Continue?",dir],$
                        /QUESTION,TITLE="Add AORs: "+self->ProjectName()+"?", $
                        DIALOG_PARENT=self->TopBase())
     if ans ne 'Yes' then return
  endif 
  
  md=keyword_set(md) 
  choices=irs_file_filter(files,COUNT=cnt,MODULE_ONLY=md)
  if cnt eq 0 then return
  
  list=strarr(cnt)
  for i=0,n_elements(choices)-1 do begin 
     if md then begin 
        list[i]=string(FORMAT='(%"%s %s (%d files)")', $
                       choices[i].OBJECT,choices[i].MODULE,choices[i].CNT)
     endif else begin 
        list[i]=string(FORMAT='(%"%s %s (%d files) %dx%d@%d (%ss)")', $
                       choices[i].OBJECT,choices[i].FOVNAME,choices[i].CNT, $
                       choices[i].STEPS,choices[i].NCYCLES, $
                       strtrim(string(FORMAT='(F20.2)',choices[i].TIME),2))
     endelse 
  endfor 
  
  ch=multchoice('Choose '+type+' Data:',list,TITLE='Load Group', $
                PARENT_GROUP=self->TopBase(),/MODAL,/NONEXCLUSIVE)
  
  widget_control, /HOURGLASS
  
  if ch[0] ne -1 then $
     for i=0,n_elements(ch)-1 do self->AddData,*choices[ch[i]].FILES
  ptr_free,choices.FILES
end


;=============================================================================
;  AddData - Add one or more BCD data files to the cube.
;=============================================================================
pro CubeProj::AddData, files,DIR=dir,PATTERN=pat,_EXTRA=e
  if n_elements(files) eq 0 AND n_elements(pat) ne 0 then begin 
     if n_elements(dir) ne 0 then $
        files=file_search(dir+pat) $
     else files=file_search(pat)
  endif 
  if n_elements(files) eq 0 then begin 
     if self->IsWidget() then begin 
        files=bcd_multfls(PARENT_GROUP=self->TopBase())
        if size(files,/TYPE) ne 7 then return
     endif else self->Error,'Files required'
  endif
  for i=0,n_elements(files)-1 do begin 
     data=readfits(files[i],header,/SILENT)
     
     type=irs_file_type(files[i])
     if ~type eq 0 then begin ;; not a bcd, load the relevant header
        bcdfile=irs_associated_file(files[i])
        if bcdfile && file_test(bcdfile,/READ) then header=headfits(bcdfile)
     endif
     
     if self.load_masks then begin 
        bfile=irs_associated_file(files[i],/BMASK)
        if bfile && file_test(bfile,/READ) then $
           bmask=readfits(bfile,/SILENT) else bmask=0
     endif 
     
     ;; Uncertainty: use BCD uncertainty for all types
     if self.load_unc then begin 
        uncfile=irs_associated_file(files[i],/UNCERTAINTY)
        if uncfile && file_test(uncfile,/READ) then $
           unc=readfits(uncfile,/SILENT)
     endif 
     
     self->AddBCD,data,header,FILE=files[i],BMASK=bmask,ERR=err, $
                  UNCERTAINTY=unc,TYPE=type,_EXTRA=e
     if keyword_set(err) then break
  endfor
  self->CheckModules            ;Set default build order, and double check.
  self->CheckDCEIDs
  self->Send,/REC_UPDATE
  self->UpdateList & self->UpdateButtons
end


;=============================================================================
;  AddBCD - Add a bcd image to the cube, optionally overriding the
;           header derived values.
;=============================================================================
pro CubeProj::AddBCD,bcd,header, FILE=file,ID=id,UNCERTAINTY=unc,BMASK=bmask, $
                     DCEID=dceid,TYPE=type,EXP=exp,COLUMN=col, ROW=row, $
                     RQST_POS=rqpos, REC_POS=rpos, PA=pa,ERR=err,DISABLED=dis
  self->LoadCalib
  ;; Don't add same file twice, even symlinks (1 level deep)
  if n_elements(file) ne 0 AND ptr_valid(self.DR) then begin 
     if (file_info(file)).SYMLINK then chfile=file_readlink(file) else $
        chfile=file
     if ~array_equal((*self.DR).file ne chfile,1b) then return
  endif 
  
  s=size(bcd,/DIMENSIONS)
  rec={CUBE_DR}
  if n_elements(s) eq 3 then begin 
     ;; uncertainty plane
     if s[2] eq 2 then begin 
        rec.BCD=ptr_new(bcd[*,*,0]) & rec.UNC=ptr_new(bcd[*,*,1])
     endif else self->Error,'Incorrect BCD dimensions: '+ $
                            strjoin(strtrim(size(bcd,/DIMENSIONS),2),",")
  endif else if n_elements(s) eq 2 then begin 
     rec.BCD=ptr_new(bcd)
  endif
  
  if n_elements(bmask) gt 1 then rec.BMASK=ptr_new(bmask)
  
  if size(unc,/N_DIMENSIONS) eq 2 then rec.UNC=ptr_new(unc)
  if size(header,/TYPE) ne 7 then self->Error,'Header must be a string array'
  rec.header=ptr_new(header)
  
  if n_elements(file) ne 0 then rec.file=file
  if n_elements(type) eq 0 then type=irs_file_type(file)
  rec.type=type
  
  if n_elements(id) ne 0 then rec.id=id else if rec.file then begin 
     id=file_basename(rec.file)
     if stregex(id,'IRSX.*\.fits',/BOOLEAN) then begin ;Sandbox style names
        parts=strsplit(id,'.',/EXTRACT)
        id=parts[3]+'.'+parts[5]+'.'+parts[6]
     endif else begin ; also treat long archive names specially
        suffix=strpos(id,".fits",/REVERSE_SEARCH)
        if suffix[0] ne -1 then id=strmid(strmid(id,0,suffix),8)
     endelse 
     rec.id=id
  endif 
  
  rec.OBJECT=sxpar(header,'OBJECT')
  rec.FOVID=sxpar(header,'FOVID',COUNT=cnt)
  self->CheckModules,rec.FOVID,ERROR=err
  if err then begin 
     heap_free,rec
     return
  endif 
  
  if cnt gt 0 then begin 
     fovname=irs_fov(rec.FOVID,ORDER=target_order,POSITION=target_pos)
     rec.TARGET_ORDER=target_order
     rec.TARGET_POS=target_pos
  endif 
  
  ;; AOR
  rec.AORKEY=sxpar(header,'AORKEY')
  
  ;; Logical map sequencing
  if n_elements(dceid) ne 0 then rec.DCEID=dceid else $
     rec.DCEID=sxpar(header,'DCEID')
  if n_elements(exp) ne 0 then rec.EXP=exp else $
     rec.EXP=sxpar(header,'EXPID')
  if n_elements(cycle) ne 0 then rec.cycle=cycle else $
     rec.CYCLE=sxpar(header,'DCENUM')
  if n_elements(col) ne 0 then rec.COLUMN=col else $
     rec.COLUMN=sxpar(header,'COLUMN')
  if n_elements(row) ne 0 then rec.ROW=row else $
     rec.ROW=sxpar(header,'ROW')
  rec.NCYCLES=sxpar(header,'NCYCLES')
  
  ;; Requested Positions/PA
  ;; PA_RQST is SIRTF-field-centric, not FOV-centric.
  if n_elements(rqpos) eq 2 then rec.RQST_POS=rqpos else $
     rec.RQST_POS=[sxpar(header,'RA_RQST'),sxpar(header,'DEC_RQST')]
  if n_elements(pa_rqst) ne 0 then rec.PA_RQST=pa_rqst else begin 
     rec.PA_RQST=sxpar(header,'PA_RQST')
     self.cal->TransformBoreSightCoords,self.module,[0,0], $
                                        rec.PA_RQST,void,new_pa
     rec.PA_RQST=new_pa          ; transform requested PA to the slit
  endelse
  
  ;; Achieved (reconstructed) positions/PA
  if n_elements(rpos) eq 3 then rec.REC_POS=rpos else $
     rec.REC_POS=[sxpar(header,'RA_SLT'),sxpar(header,'DEC_SLT')]
  if n_elements(pa) ne 0 then rec.PA=pa else $
     rec.PA=sxpar(header,'PA_SLT')
  
  ;; Date/Obstime
  ;; J2000 Ephemeris time at MJD5144.5
  rec.DATE_OBS=sxpar(header,'UTCS_OBS')/3600.D/24.0D + 51544.5D + 2400000.5D
  
  rec.TIME=sxpar(header,'RAMPTIME')
  rec.DATE=systime(/JULIAN)
  
  ;; Disabled status
  if keyword_set(dis) then rec.DISABLED=1b
  
  if ptr_valid(self.DR) then *self.DR=[*self.DR,rec] else $
     self.DR=ptr_new(rec,/NO_COPY)
  self->SetDirty
end

;=============================================================================
;  RemoveBCD - Remove one or more BCD's
;=============================================================================
pro CubeProj::RemoveBCD,recs,_EXTRA=e
  self->RecOrSelect,recs,_EXTRA=e
  
  keep=where(histogram([recs],MIN=0,MAX=self->N_Records()-1) eq 0,keepcnt)
  if ptr_valid(self.BACK_RECS) then begin 
     if size(*self.BACK_RECS,/TYPE) eq 8 then begin 
        if ptr_valid((*self.BACK_RECS)[0].DCEIDs) then $
           bglist=*(*self.BACK_RECS)[0].DCEIDs
        if ptr_valid((*self.BACK_RECS)[0].DCEIDs) then begin 
           bglist2=*(*self.BACK_RECS)[1].DCEIDs
           if n_elements(bglist) eq 0 then $
              bglist=bglist2 else bglist=[bglist,bglist2]
        endif 
     endif else bglist=*self.BACK_RECS
     
     if keepcnt ne 0 then begin 
        brecs=self->DCEIDtoRec(bglist,RECORDS=keep,newbgcnt)
     endif else newbgcnt=0
     if newbgcnt ne n_elements(bglist) then begin 
        if self->IsWidget() then begin 
           ans=dialog_message( $
               ['Removing these BCDs invalidates the background', $
                'saved '+jul2date(self.BACK_DATE)+ '.  Continue?'], $
               /QUESTION,TITLE='Background Warning', $
               DIALOG_PARENT=self->TopBase(),/DEFAULT_NO)
           if ans eq 'No' then return
        endif 
        if self.BACK_RECS ne self.AS_BUILT.BACK_RECS then $
           heap_free,self.BACK_RECS else self.BACK_RECS=ptr_new()
        ptr_free,self.BACKGROUND
        self.BACK_DATE=0.0D
     endif 
  endif 
  
  heap_free,(*self.DR)[recs]
  if keepcnt ne 0 then (*self.DR)=(*self.DR)[keep] else ptr_free,self.DR
  
  self->SetDirty                ;but accounts remain valid!
  self->UpdateList,/CLEAR_SELECTION,/NO_UPDATE
  self->Send,/REC_UPDATE,/DELETED
  self->UpdateButtons & self->UpdateTitle
end

;=============================================================================
;  Send - Send one of our messages
;=============================================================================
pro CubeProj::Send,RECORD=record,CUBE=cube,BACKGROUND=back,BLEND=comb, $
                   UPDATE=update, NEW_CUBE=nc, $
                   BADPIX_UPDATE=bpl_update,SELECT=sel,SINGLE_SELECT=ss, $
                   REC_UPDATE=rec_update,DELETED=del, DISABLED=dis, $
                   CALIB_UPDATE=cal_update, VISUALIZE=vis, $
                   UNCERTAINTY=show_unc
  case 1b of
     keyword_set(sel): begin 
        if n_elements(ss) ne 0 && ptr_valid(self.DR) then $
           ss=(*self.DR)[ss[0]].DCEID else ss=-1
        self->MsgSend,{CUBEPROJ_SELECT,self,ss}
     end 
     keyword_set(rec_update): self->MsgSend,{CUBEPROJ_RECORD_UPDATE,self, $
                                             keyword_set(del),keyword_set(dis)}
     keyword_set(vis): begin 
        astr=self->VisualizeAstrom()
        if size(astr,/TYPE) ne 8 then $
           self->Error,'Cannot decode Visualization Image header'
        astr=ptr_new(astr)
        self->MsgSend, {CUBEPROJ_VISUALIZE, self, $
                        self->ProjectName() + ' '+ $
                        irs_fov(MODULE=self.module,ORDER=self.order, $
                                POSITION=0,/SLIT_NAME,/LOOKUP_MODULE) + ': ' +$
                        file_basename(self.visualize_file), $
                        self.MODULE,self.VISUALIZE_IMAGE, $
                        astr}
        ptr_free,astr
     end
     
     keyword_set(cube): $
        self->MsgSend,{CUBEPROJ_CUBE, $
                       self,$
                       string(FORMAT='(%"%s [%dx%d], %s")', $
                              self->ProjectName(), $
                              self.NSTEP,jul2date(self.CUBE_DATE)), $
                       self.MODULE,self.WAVELENGTH}
     
     keyword_set(back): begin 
        name=self->ProjectName()+' <Background'
        if ptr_valid(self.BACK_RECS) then begin 
           if size(*self.BACK_RECS,/TYPE) eq 8 then begin 
              if n_elements(comb) ne 0 then begin 
                 ;; Show A or B background
                 pos=keyword_set(comb) 
                 n=n_elements(*(*self.BACK_RECS)[pos].DCEIDs)
                 name+=(['A','B'])[pos]
                 bg=(*self.BACK_RECS)[pos].BACKGROUND
                 unc=(*self.BACK_RECS)[pos].BACK_UNC
              endif else begin 
                 ;; Show combine AB background
                 n=n_elements(*(*self.BACK_RECS)[0].DCEIDs)+ $
                   n_elements(*(*self.BACK_RECS)[1].DCEIDs)
                 name+=' Blended'
                 bg=self.BACKGROUND
                 unc=self.BACKGROUND_UNC
              endelse 
           endif else begin 
              bg=self.BACKGROUND
              unc=self.BACKGROUND_UNC
              n=n_elements(*self.BACK_RECS)
           endelse 
        endif 
        
        if n_elements(n) ne 0 then name+=' from '+strtrim(n,2)+' recs'
        name+='>'
        ;; Pretend it is a real BCD:
        self->MsgSend,{CUBEPROJ_RECORD, $
                       self,name,self.MODULE,self.ORDER,self.CAL,ptr_new(), $
                       bg,unc,ptr_new(),ptr_new(),ptr_new(),0b}
     end 
     
     keyword_set(update): $
        self->MsgSend,{CUBEPROJ_UPDATE,self,keyword_set(nc)}
     
     keyword_set(bpl_update): $
        self->MsgSend,{CUBEPROJ_BADPIX_UPDATE,self,self.GLOBAL_BAD_PIXEL_LIST}
        
     keyword_set(cal_update): $
        self->MsgSend,{CUBEPROJ_CALIB_UPDATE,self}
     
     else: begin         ;; Send the record or stacked record set for viewing
        nrec=n_elements(record) 
        if nrec eq 0 then return
        showing_unc=keyword_set(show_unc) 
        stackQ=nrec gt 1
        rec=(*self.DR)[record]
        if stackQ then begin 
           ;; Stack of BCDs
           bcd=*rec[0].BCD
           use_bmask=array_equal(ptr_valid(rec.BMASK),1b)
           use_unc=array_equal(ptr_valid(rec.UNC),1b)
           if use_unc then unc=*rec[0].UNC^2 ;add in quadrature
           if use_bmask then mask=*rec[0].BMASK
           for i=1,nrec-1 do begin 
              if ~showing_unc then bcd+=*rec[i].BCD
              if use_unc then unc+=*rec[i].UNC^2
              if use_bmask then mask OR=*rec[i].BMASK
           endfor 
           if ~showing_unc then begin 
              bcd/=nrec 
              bcd_p=ptr_new(bcd,/NO_COPY)
           endif 
           if n_elements(unc) gt 0 then begin 
              unc=sqrt(unc)/nrec
              unc_p=ptr_new(unc,/NO_COPY)
           endif else unc_p=ptr_new()
           mask_p=n_elements(mask) gt 0?ptr_new(mask,/NO_COPY):ptr_new()
           free=1
           if showing_unc then begin 
              str=string(FORMAT='(%"%s <Quadrature sum of %d uncs>")', $
                         self->ProjectName(),nrec)
           endif else begin 
              str=string(FORMAT='(%"%s <Average of %d recs>")', $
                         self->ProjectName(),nrec)
           endelse 
        endif else begin 
           ;; Single BCD
           bcd_p=rec.BCD
           unc_p=rec.UNC
           mask_p=rec.BMASK
           free=0
           str=string(FORMAT='(%"%s <%s%s> %s")', $
                      self->ProjectName(),rec.ID,showing_unc?" unc":"", $
                      irs_fov(rec.FOVID,/SHORT_NAME))
        endelse
        rec_set=ptr_new(rec.DCEID)
        if showing_unc then begin 
           bcd_p=unc_p 
           unc_p=(bg=(bg_unc=ptr_new()))
        endif else begin 
           bg=self.BACKGROUND
           bg_unc=self.BACKGROUND_UNC
        endelse 
        self->MsgSend,{CUBEPROJ_RECORD, $
                       self,str,self.MODULE,self.ORDER,self.CAL, $
                       rec_set,bcd_p,unc_p,mask_p, $
                       bg,bg_unc,free}
        ptr_free,rec_set
     end 
  endcase 
end

;=============================================================================
;  BuildOrders
;=============================================================================
function CubeProj::BuildOrders
  ;; Are we treating one order, or all of them?
  return,self.ORDER eq 0?self.cal->Orders(self.MODULE):self.ORDER
end

;=============================================================================
;  N_Records
;=============================================================================
function CubeProj::N_Records,ENABLED=enabled
  if ptr_valid(self.DR) then begin 
     if keyword_set(enabled) then begin 
        wh=where(~(*self.DR).DISABLED,good)
        return,good
     endif else $
        return,n_elements(*self.DR)
  endif else return,0
end

;=============================================================================
;  Cleanup 
;=============================================================================
pro CubeProj::Cleanup
  self->KillShow
  heap_free,self.DR
  heap_free,self.MERGE
  heap_free,self.AS_BUILT
  ptr_free,self.APERTURE,self.CUBE,self.CUBE_UNC, $
           self.BACKGROUND,self.BACKGROUND_UNC, $
           self.wInfo,self.WAVELENGTH,self.RECOV_ASTROM, $
           self.VISUALIZE_HEADER,self.VISUALIZE_IMAGE, $
           self.AUTO_BPL.BCD_PIX,self.AUTO_BPL.BCD_VALS, $
           self.AUTO_BPL.DCEID,self.AUTO_BPL.BCD_UNC,self.AUTO_BPL.CNT_VEC
  heap_free,self.BACK_RECS
  ;if self.spawned then obj_destroy,self.cal ;noone else will see it.
  self->OMArray::Cleanup
  self->ObjMsg::Cleanup
end

;=============================================================================
;  Init 
;=============================================================================
function CubeProj::Init, name, _EXTRA=e
  if (self->ObjMsg::Init(_EXTRA=e) ne 1) then return,0 ;chain up
;  if self->IDLitComponent::Init() ne 1 then return,0
  if n_elements(name) ne 0 then self->SetProjectName,name
  ;; A few defaults
  self->SetProperty,/LOAD_MASKS,/USE_BACKGROUND,/RECONSTRUCTED_POSITIONS, $
                    OVERSAMPLE_FACTOR=1.0D,/FLUXCON,/PIXEL_OMEGA,/WAVECUT, $
                    /LOAD_UNCERTAINTY,/SLCF,/USE_UNCERTAINTY
  self.Changed=0b               ;coming into existence doesn't count
  if n_elements(e) ne 0 then self->SetProperty,_EXTRA=e
  self->Initialize
  return,1
end

;=============================================================================
;  CubeProj - IRS Spectral (+MIPS SED) Cubes
;=============================================================================
pro CubeProj__define
  ;; Basic parameters which go into building a cube: they can be
  ;; changed without rebuilding a cube.  Useful for saving "as-built"
  ;; snapshots.  Careful when freeing shared pointers!
  cp={CUBE_PARAMETERS, $
      MODULE:'', $              ;The name of the module, one of
                                ;   SL,LL,SH,LH (IRS),MSED (MIPS)
      ORDER:0, $                ;The sub-slit order for which to build
                                ; the cube (or 0 to build and splice all
                                ; orders in the module)
      POSITION:[0.0D,0.0D], $   ;optimized position of the cube center
      CUBE_SIZE: [0L,0L,0L],$   ;the size of the cube, (n,m,l)
      APERTURE:ptr_new(), $     ;The aperture of the clip, or one
                                ; for each order
      BACK_DATE: 0.0D, $        ;date background created
      BACK_RECS: ptr_new(),$    ;list of expids used for the background,
                                ;or pair of structures containing A/B info
      BG_SP_FILE:'', $          ;file used for 1D BG subtraction
      BG_SP_TYPE: 0b, $         ;0b: raw electrons, 1b: fluxed
      GLOBAL_BAD_PIXEL_LIST: ptr_new(),$ ;a user list of bad pixels to exclude
      GLOBAL_BP_FILES: ptr_new(), $ ;files which contribed to the BPs
      GLOBAL_BP_TYPE: 0b, $         ;by hand: 1b, auto bps: 2b
      GLOBAL_BP_SAVEFILE: '', $ ;file, if any, BPs saved to
      GLOBAL_BP_SAVEFILE_UPTODATE:0b, $ ;whether the BP file is up to date
      BACK_SP_FILE: '', $       ;Background 1D spectrum file
      fluxcon:0b, $             ;whether to build with FLUXCON fluxes
      slcf: 0b, $               ;whether to build with the SLCF
      pix_omega: 0b, $          ;whether to build to MJy/sr units using
                                ; effective solid angle of pixels
      reconstructed_pos:0b, $   ;whether to build with reconstructed positions
      wavecut: 0b, $            ;whether to trim wavelengths with WAVECUT
      use_bg: 0b, $             ;whether to use the background
      use_unc: 0b, $            ;build an uncertainty cube
      cal_file:'', $            ;the calibration file used (if not a full
                                ; directory, in the "calib/" subdir)
      NSTEP:[0L,0L], $          ;perpendicular (row), parallel (col) steps
      STEP_SIZE: [0.0D,0.0D], $ ;perpendicular, parallel slit step sizes (deg)
      PLATE_SCALE:0.0D, $       ;the instrument plate scale (degrees/pixel)
      OVERSAMPLE_FACTOR: 0.0D,$ ;the factor by which the sky is oversampled
      PR_SIZE:[0.0,0.0]}        ;the, parallel (long axis), perpendicular
                                ; (short axis) size of the PRs to use
                                ; (pixels)
  
  ;; Cached Auto-bad pixel parameters.
  cbpl={CUBE_AUTO_BPL, $
        BCD_PIX:ptr_new(), $    ;cached copy of full cube BPL pix return
        BCD_VALS:ptr_new(),$    ;cached copy of full cube BPL vals return
        DCEID:ptr_new(), $      ;cached copy of full cube DCEID return
        BCD_UNC:ptr_new(),$     ;cached copy of full cube BPL unc return
        CNT_VEC:ptr_new()}      ;cached copy of full count vec
  
  c={CubeProj, $
     INHERITS OMArray, $        ;array messenger
     INHERITS ObjMsg, $         ;make it an object messanger
     INHERITS ObjReport, $      ;for error, etc. reporting
     INHERITS CUBE_PARAMETERS, $ ;simple structure inheritance of params
     ProjectName:'', $          ;the name of the current project
     DR: ptr_new(), $           ;All the BCD's: pointer to list of
                                ; data record structures of type CUBE_DR
     ACCOUNTS_VALID: 0b,$       ; are the account records valid?
                                ;  1:accts, 2:layout, 4: background
     CUBE: ptr_new(),$          ;a pointer to the nxmxl data cube
     CUBE_UNC:  ptr_new(),$     ;a pointer to the nxmxl uncertainty cube
     CUBE_DATE: 0.0D, $         ;date the cube was assembled (JULIAN)
     WAVELENGTH: ptr_new(), $   ;the cube's wavelength list
     AS_BUILT: {CUBE_PARAMETERS}, $ ;the as-built cube parameters
     BACKGROUND: ptr_new(),$    ;the background image (if any) to subtract
     BACKGROUND_UNC:ptr_new(),$ ;the uncertainty in the background
     BG_SP: ptr_new(), $        ;2xn background spectrum for 1D subtractions
     RECOV_ASTROM: ptr_new(), $ ;astrometry recovered from file
     PA:0.0D, $                 ;optimized position angle of the cube
     VISUALIZE_FILE:'',$        ;file for visualization image
     VISUALIZE_IMAGE:ptr_new(),$ ;image for AOR visulization
     VISUALIZE_HEADER:ptr_new(), $ ;header for vis_image
     MERGE:ptr_new(),$          ;a set of records to aid merging the orders
                                ;  ptr to list of {offset,inds,wave,to,frac}
     AUTO_BPL: {CUBE_AUTO_BPL},$ ;auto bad pixel cached copies
     GLOBAL_DIRTY_PIX: ptr_new(),$ ;list of globally dirty pixels (all records)
     debug:0b, $                ;whether to debug
     cal:obj_new(), $           ;the irs_calib object.
     oAutoBadPix: obj_new(), $  ;automatic bad pixel parameters
     Changed:0b, $              ;if the project is changed since last saved.
     Spawned:0b, $              ;whether we were opened by another instance
     feedback:0b, $             ;whether to show feedback when building cube
     SaveFile:'', $             ;the file we are saved to
     SaveMethod:0b, $           ;bit 1: data, bit 2: accounts, bit 4: rel. files
     sort:0b, $                 ;our sorting order
     load_masks:0b, $           ;whether to load masks by default
     load_unc:0b, $             ;whether to load uncertainties by default
     version:'', $              ;the Cubism version of this cube
     wInfo:ptr_new()}           ;the widget info struct.... a disconnectable ptr
      
  
  ;; The account structure is *big* (~1Mb per BCD)
  acc={CUBE_ACCOUNT_LIST, $
       cube_plane:0, $          ;the cube plane it goes to.
       cube_pix:0L, $           ;the cube pixel in that plane
       bcd_pix:0, $             ;the bcd pixel it came from
       area:0.0}                ;the area of overlap on the cube pixel
  
  ;; The data structure for each input BCD
  rec={CUBE_DR, $
       ID:'',$                  ;A unique (hopefully) ID
       OBJECT:'', $             ;The name of the object, from the header
       file:'', $               ;the original file read for this BCD dataset
       DCEID: 0L, $             ;unique exposure id
       TIME:0.0, $              ;The integration time
       DISABLED: 0b, $          ;whether this DR is disabled
       IN_CUBE: 0b, $           ;is this record in the built cube?
       ACCOUNT: ptr_new(), $    ;list of CUBE_ACCOUNT_LIST's
       REV_BCD_CNT:0L, $        ;number of bins in the BCD reverse index
       REV_ACCOUNT: ptr_new(),$ ;reverse indices of the cube index histogram
       REV_MIN:0L, $            ;minimum offset cube index affected
       REV_CNT:0L, $            ;how many bins in the cube_index histogram?
       REV_WIDTH:0L, $          ;width of the DR bounding-box on the cube
       REV_HEIGHT:0L, $         ;height of the DR bounding-box on the cube
       REV_OFFSET:[0L,0L], $    ;offset of the DR bounding-box into the cube
       REV_DUAL: ptr_new(), $   ;dual histogram reverse indices for rec
       REV_DUAL_CNT: 0L, $      ;how many bins in the dual histogram?
       REV_BCD_ACCOUNT: ptr_new(), $ ;reverse indices into ACCOUNT by bcd_pix
       REV_BCD_XOFF: 0, $       ;x offset of the BCD bounding box
       REV_BCD_WIDTH: 0, $      ;width of the BCD pixel bounding box
       REV_BCD_MIN:0L, $        ;minimum offset BCD index affected
       RQST_POS: [0.0D,0.0D], $ ;Commanded RA,DEC position of slit center
       REC_POS: [0.0D,0.0D],$   ;Reconstructed RA,DEC pos of slit center.
       FOVID: 0, $              ;The IRS Field of View ID
       TYPE: 0, $               ;0- BCD, 1- Droopres, 2- Coad2d 3- Flatap
       TARGET_ORDER: 0, $       ;which order was targetted by pointing
                                ;  (or 0 for module center targetting)
       TARGET_POS: 0, $         ;For low-res: 0, 1, or 2 for centered on
                                ; module, slit position 1, or slit position 2
       PA: 0.0D, $              ;Position angle of slit (+w?) E of N
       PA_RQST: 0.0D, $         ;Requested PA of slit (+w?)
       AORKEY: 0L, $            ;the AOR ID KEY
       BCD: ptr_new(), $        ;the BCD data (or varietal: FLATAP,DROOPRES...)
       UNC:ptr_new(), $         ;the BCD's uncertainty image
       BMASK:ptr_new(), $       ;the BCD's BMASK image
       BAD_PIXEL_LIST: ptr_new(),$ ;this BCD's specific bad pixel list (if any)
       DIRTY_PIX: ptr_new(), $     ;list of "dirty" pixels for this record
       EXP: 0L, $               ;the exposure number in the mapping sequence
       CYCLE:0L, $              ;the cycle number at this position
       NCYCLES:0L,$             ;the number of cycles at this position
       COLUMN: 0L,$             ;the step number perpendicular to the slit
       ROW:0L, $                ;the step number parallel to the slit
       DATE:0.0D, $             ;the date this BCD was added
       DATE_OBS:0.0D, $         ;the (julian) date this BCD was observed
       HEADER: ptr_new()}       ;a pointer to a string array
  
  ;; The widget info
  winfo={cubeProj_wInfo,$
         showing:0, $           ;whether we're showing
         Base:0L, $             ;the Show Widget, DR list display base
         Status: 0L, $          ;the status line widget
         SList:0L, $            ;the Widget List for Show 
         wHead:lonarr(2), $     ;the widget heads
         list_row:0.0, $        ;the height of list rows in pixels
         which_list:0, $        ;which list we're using
         list_size_diff:0, $    ;the difference in ysize between list and base
         xsize: 0, $            ;the screen xsize of the show widget
         lines:0, $             ;number of list lines last set
         view_ids:lonarr(3), $  ;record view button ids
         build_ids:lonarr(2), $ ;cube build button ids
         nsel_sav: 0, $         ;save number of selected records
         quickbuild_sav: 0b, $  ;save status of whether we can quick-build
         feedback_window: 0L, $ ;window id of feedback
         feedback_save:dblarr(4), $ ;feedback !X.S and !Y.S
         feedback_pixmap: 0L, $     ;id of feedback pixmap for quick restores
         background_menu: 0L, $ ;widget ID of background menu bar
         MUST_NO_DATA:0L, $     ;no data saved
         MUST_MODULE:lonarr(1),$ ;must have a module set
         MUST_CAL:lonarr(1), $   ;Must have a calibration set loaded
         MUST_CAL_FILE:0L, $     ;Must have a calibration set specified.
         MUST_SELECT:lonarr(17),$ ;the SW buttons which require any selected
         MUST_SAVE_CHANGED:0L, $ ;require changed and a saved File
         MUST_PROJ:lonarr(7), $ ;SW button which requires a valid project
         MUST_ACCT:lonarr(4), $ ;Must have valid accounts
         MUST_CUBE:lonarr(4), $ ;SW button requires valid cube created.
         MUST_BACK:lonarr(5), $ ;background record must be set
         MUST_BACK_A: 0L, $     ;back A of combined backs must be set
         MUST_BACK_B: 0L, $     ;back B of combined backs must be set
         MUST_BACK_AB: 0L, $    ;back A&B of combined backs must be set
         MUST_VIS: 0L, $        ;must have a visualization object loaded
         MUST_ANY_BACK: 0L, $   ;either BCD or 1D BG required
         MUST_BG_SP:0L, $       ;required background spectrum
         MUST_ANY_BPL: lonarr(2), $ ;must have any bad pixels
         MUST_GLOBAL_BPL:0L, $  ;must have a global list of bad pixels
         MUST_REC_BPL: 0L, $    ;must have record bad pixels
         MUST_UNRESTORED:0L, $  ;must have unrestored data
         MUST_REC_UNC: [0L,0L],$ ;selected record must have uncertainty
         MUST_UNCERTAINTIES: 0L} ;must have enabled records with uncertainties
  
  ;; Messages: send a cube, record, etc.
  msg={CUBEPROJ_CUBE, CUBE:obj_new(),INFO:'',MODULE:'', $
       WAVELENGTH:ptr_new()}
  msg={CUBEPROJ_RECORD,CUBE:obj_new(),INFO:'',MODULE:'',ORDER:0, $
       CAL:obj_new(),RECORD_SET:ptr_new(), BCD:ptr_new(),UNC:ptr_new(), $
       BMASK:ptr_new(), BACKGROUND:ptr_new(),BACKGROUND_UNC:ptr_new(), $
       FREE:0b}
  msg={CUBEPROJ_VISUALIZE,CUBE:obj_new(),INFO:'',MODULE:'', $
       IMAGE:ptr_new(),ASTROMETRY:ptr_new()}
  msg={CUBEPROJ_UPDATE, CUBE:obj_new(),NEW_CUBE:0b}
  msg={CUBEPROJ_BADPIX_UPDATE,CUBE:obj_new(),BAD_PIXEL_LIST:ptr_new()}
  msg={CUBEPROJ_SELECT, CUBE:obj_new(),SINGLE_SELECT: 0L}
  msg={CUBEPROJ_RECORD_UPDATE, CUBE:obj_new(),deleted:0b,disabled:0b}
  msg={CUBEPROJ_CALIB_UPDATE, CUBE:obj_new()}
end
