;+
; NAME:  
;
;    CubeProj
;
; CONTACT:
;
;    UPDATED VERSIONS of CUBISM and more information can be found at:
;       http://sings.sirtf.edu/cubism
;
; DESCRIPTION:
;    
;    A wrapper for CUBISM spectral mapping projects, containing the
;    input BCD and calibration data, along with the routines to
;    convert them into spectral cubes.
;    
; CATEGORY:
;
;    CUBISM Spectral Cube Reduction, Analysis and Processing.
;    	
; SIDE EFFECTS:
;
;    A GUI interface to CubeProj is available.
;
; RESTRICTIONS:
;
;    Any restrictions?
;
; METHODS:
;
;    Init:  (always start with the INIT method function)
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
;    contains all the IRS BCD data, calibration parameters, and
;    positional information required to create a single, 3D (two
;    spatial, one wavelength) "spectral cube" from input IRS Spectral
;    Mapping Mode data sets (see
;    http://sirtf.caltech.edu/SSC/irs/aotintro.html).  In addition, it
;    can perform various manipulations on the resulting cubes.  It
;    exists both as a GUI interface, and a scriptable back-end to the
;    cube construction and spectral extraction algorithms.
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
;  Copyright (C) 2002-2004 J.D. Smith
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
;  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;  02111-1307, USA.
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
  if tag_names(ev,/STRUCTURE_NAME) eq 'WIDGET_BASE' then begin ;size
     widget_control, (*self.wInfo).SList, $
                     SCR_YSIZE=ev.Y+(*self.wInfo).list_size_diff
     return
  endif 
  sel=widget_info((*self.wInfo).SList,/LIST_SELECT)
  n_sel=n_elements(sel) 
  nr=self->N_Records()
  if ev.id eq (*self.wInfo).SList then begin ;somebody clicked
     self->UpdateButtons
     if nr eq 0 then return     ;just the blank nothing line
     if ev.clicks eq 2 then begin 
        widget_control, (*self.wInfo).SList,SET_LIST_SELECT=sel[0]
        action='viewrecord' 
     endif else return 
  endif else widget_control, ev.id, get_uvalue=action
  if action eq 'bargroup' then action=ev.value
  
  widget_control, /HOURGLASS
  case action of
     'save-as': self->Save,/AS
     
     'viewrecord-new': self->ViewRecord,/NEW
     'viewbackground-new': self->ViewBackground,/NEW
     'remove-background': begin 
        ptr_free,self.BACKGROUND,self.BACKGROUND_UNC,self.SCALED_BACK
        self.BACK_CNT=0 & self.BACK_DATE=0.0D
        self.ACCOUNTS_VALID AND=NOT 4b
        self->UpdateButtons
     end 
     'loadcalib': self->LoadCalib,/SELECT
     'feedback': begin 
        self.feedback=1b-self.feedback
        widget_control, ev.id, SET_BUTTON=self.feedback
     end
     'fluxcon': begin 
        self.fluxcon=1b-self.fluxcon
        widget_control, ev.id, SET_BUTTON=self.fluxcon
        self.changed=1b
     end
     'save-with-data': begin 
        self.SaveMethod XOR=1b
        widget_control, ev.id, SET_BUTTON=self.SaveMethod AND 1b
     end 
     'save-with-accounts': begin 
        self.SaveMethod XOR=2b
        widget_control, ev.id, SET_BUTTON=(self.SaveMethod AND 2b) ne 0b
     end 
     
     'add-droop-aor': self->AddAOR,/DROOPRES
     'add-flatap-aor': self->AddAOR,/FLATAP
     
     'setorder': begin 
        self->GetProperty,CALIB=cal
        ords=strtrim(cal->Orders(self.MODULE),2)
        if self.MODULE eq 'SH' or self.MODULE eq 'LH' then ords=['All',ords]
        ord=popup('Choose Cube Build Order',ords,TITLE='Cube Build Order', $
                  PARENT_GROUP=self->TopBase(),/MODAL, $
                  SELECT=self.ORDER eq 0?0:where(ords eq self.ORDER))
        if ord eq 'All' then ord=0 else ord=ord
        self->SetProperty,ORDER=ord
     end
     
     'viewcube-new': self->ViewCube,/NEW
          
     'select-all': begin        ;select all records
        top=widget_info((*self.wInfo).SList,/LIST_TOP)
        widget_control, (*self.wInfo).SList, $
                        SET_LIST_SELECT=lindgen(self->N_Records())
        widget_control, (*self.wInfo).SList,SET_LIST_TOP=top
        self->UpdateButtons
     end
     
     'replace-string': $
        begin 
        if sel[0] eq -1 then return
        f=(*self.DR)[sel].FILE
        cnt=0
        nf=n_elements(f) 
        if nf gt 1 then begin 
           lab='Replacing text of '+strtrim(nf,2)+' files:'
           wh=where(total(byte(f) eq shift(byte(f),0,1),2) ne nf,cnt)
           def=strmid(f[0],0,wh[0])
           t=twoin(from,to,def,def,LABEL=lab, $
                   TEXT1='Replace Substring (regexp):', $
                   TEXT2='With:',PARENT_GROUP=self->TopBase(),/SCROLL, $
                   TITLE='Replace File Substring',TEXT_LAB=f,YSIZE=nf<6)
        endif else begin
           t=twoin(from,to,f[0],f[0],TEXT1='Replace Substring (regexp):', $
                   TEXT2='With:',PARENT_GROUP=self->TopBase(), $
                   TITLE='Replace File Substring')
        endelse 
        if t then begin 
           for i=0,n_sel-1 do begin
              file=(*self.DR)[sel[i]].FILE
              pos=stregex(file,from,length=len)
              (*self.DR)[sel[i]].FILE= $
                 strmid(file,0,pos)+to+strmid(file,pos+len)
           endfor
        endif
     end 
     
     'filenames': begin 
        if sel[0] eq -1 then return
        if n_elements(sel) gt 1 then $
           strlist=string(FORMAT='(A," : ",A)', $
                          [transpose((*self.DR)[sel].ID), $
                           transpose((*self.DR)[sel].file)]) $
        else strlist=string(FORMAT='(A," : ",A)', $
                            (*self.DR)[sel].ID,(*self.DR)[sel].file)
        self->Info,strlist,TITLE='Filename Listing'
     end 
     
     'headers':   $
        begin 
        if sel[0] eq -1 then return
        if NOT array_equal(ptr_valid((*self.DR).HEADER),1) then self->Error, $
           ['No Header available for records',(*self.DR)[wh].ID]
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
                    PARENT_GROUP=self->TopBase(),/MODAL)
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
        self->Info,[which+':',strlist],TITLE=which+' Keyword Listing'
     end 
     
     'switchlist': begin 
        widget_control, ev.id,get_value=val
        val=val eq "<"?">":"<"
        which=val eq "<"?1:0
        widget_control, (*self.wInfo).wHead[1-which],MAP=0
        widget_control, (*self.wInfo).wHead[which],/MAP
        (*self.wInfo).which_list=which
        widget_control, ev.id,set_value=val
        self->UpdateColumnHeads
        self->UpdateList
     end
     
     'sort': begin 
        widget_control,ev.id,get_value=v
        flags=bytarr(n_elements(v))
        self.sort=ev.value
        flags[ev.value-(5*(ev.value ge 6))]=1b ;keep ID selected if set
        widget_control, (*self.wInfo).Base,UPDATE=0
        widget_control,ev.id,SET_VALUE=flags
        self->UpdateList
        widget_control, (*self.wInfo).Base,/UPDATE
     end 
     
     'delete': if sel[0] ne -1 then self->RemoveBCD,sel
     
     'phist': self->Info,self->Info(/NO_DATA), $
                         TITLE=self->ProjectName()+': History'
     
     'calset': begin 
        info=ptrarr(4) & modules=strarr(4)
        for md=0,3 do begin 
           modules[md]=irs_module(md,/TO_NAME)
           info[md]=ptr_new(self.cal->Info(md))
        endfor  
        xpopdiag,modules,info,PARENT_GROUP=self->TopBase(), $
                 BUTTON_TEXT='  Done  ',LABEL='Module:', $
                 TITLE=self->ProjectName()+': Calibration Set <'+ $
                 filestrip(self.cal_file)+'>',TSIZE=[94,15], $
                 DEFAULT=irs_module(self.MODULE),/FREE_POINTERS
     end
     
     'about': begin 
        @cubism_version
        if self.version ne '' AND cubism_version ne self.version then $
           thiscube=' (Curr. Cube: '+self.version+')' else thiscube=''
        title='CUBISM '+cubism_version
        tlen=strlen(title) & left=(33-tlen)/2
        title=strjoin(replicate(' ',left>0)) + title
        self->Info,TITLE='About Cube Project', $
                   ['*********************************', $
                    title,                               $
                    thiscube,                            $
                    '                                 ', $
                    '       JD Smith -- 2002-2004     ', $
                    '  http://sings.stsci.edu/cubism  ', $
                    '*********************************']
     end
     
     else: call_method,action,self ;all others, just call the named method
  endcase     
end

pro CubeProj_Show_kill,id
  widget_control, id, get_uvalue=self
  self->Exit
end

;=============================================================================
;  Exit - Get out of here, possibly committing hara kiri on the way.
;=============================================================================
pro CubeProj::Exit
  if self.Changed then begin 
     status=self.Spawned?"(changes will be lost)": $
            "(project available on command line)"
     ans=dialog_message(["Save changes to project "+self->ProjectName()+"?", $
                         self.SaveFile?"to file "+self.SaveFile:"",status], $
                        /QUESTION,TITLE="Save "+self->ProjectName()+"?", $
                        DIALOG_PARENT=self->TopBase(),/CANCEL,/DEFAULT_CANCEL)
     if ans eq 'Cancel' then canceled=1 $
     else if ans eq 'Yes' then self->Save,CANCELED=canceled
     if keyword_set(canceled) then begin 
        if NOT widget_info((*self.wInfo).SList,/VALID_ID) then self->Show
        return
     endif 
  endif 
  if self->isWidget() then if widget_info((*self.wInfo).SList,/VALID_ID) $
     then widget_control, (*self.wInfo).Base,KILL_NOTIFY='',/DESTROY
  (*self.wInfo).showing=-1
  if self.Spawned then obj_destroy,self
end
     
;=============================================================================
;  Show - Run the project interface
;=============================================================================
pro CubeProj::Show,FORCE=force,SET_NEW_PROJECTNAME=spn,_EXTRA=e
  if NOT keyword_set(force) then if self->Showing() then return

  ;; make the info structure if we need it.
  if NOT ptr_valid(self.wInfo) then begin
     self.wInfo=ptr_new({CubeProj_wInfo})
  endif 
  
  self.feedback=1               ;default to showing cube build feedback
  
  base=widget_base(/COLUMN,/BASE_ALIGN_RIGHT,APP_MBAR=mbar,SPACE=1, $
                   /TLB_SIZE_EVENTS,_EXTRA=e)
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
  b2=widget_button(sm,VALUE='Save Clip Accounts with Project', $
                   UVALUE='save-with-accounts', /CHECKED_MENU)
  widget_control, b2,SET_BUTTON=logical_true(self.SaveMethod AND 2b)
  b1=widget_button(file,VALUE='Save',UVALUE='save')
  b1=widget_button(file,VALUE='Save As...',UVALUE='save-as')
  
  (*self.wInfo).MUST_SAVE_CHANGED= $
     widget_button(file,VALUE='Revert To Saved...',uvalue='revert')
  ;;-------------  
  wMustCube=widget_button(file,VALUE='Write FITS Cube...',$
                          UVALUE='writefits',/SEPARATOR)
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
  b1=widget_button(edit,VALUE='Select All',uvalue='select-all')
  wMustSel=widget_button(edit,VALUE='Replace File Substring...', $
                         UVALUE='replace-string') 
  
  ;;*** Data Record menu
  rec=widget_button(mbar,VALUE='Record',/MENU)
  b1=widget_button(rec,VALUE='Add Data...',UVALUE='adddata')
  b2=widget_button(rec,VALUE='Import Data from AOR',/MENU)
  tmp=widget_button(b2,VALUE='BCD...',UVALUE='addaor')
  tmp=widget_button(b2,VALUE='DroopRes...',UVALUE='add-droop-aor')
  tmp=widget_button(b2,VALUE='FlatAp...',UVALUE='add-flatap-aor')
  
  (*self.wInfo).MUST_UNRESTORED=widget_button(rec, UVALUE='restoreall',$
                                              VALUE='Restore All Record Data')
  wMustSel=[wMustSel, $
            ;;-------------
            widget_button(rec,VALUE='Delete',UVALUE='delete',/SEPARATOR),$
            widget_button(rec,VALUE='Rename',UVALUE='renamerecord'),$
            widget_button(rec,VALUE='Disable',UVALUE='disablerecord'),$
            widget_button(rec,VALUE='Enable',UVALUE='enablerecord'),$
            ;;-------------
            ((*self.wInfo).view_ids[0:1]= $
             [widget_button(rec,VALUE='View...      ',UVALUE='viewrecord', $
                            /SEPARATOR),$
              widget_button(rec,VALUE='View (new viewer)...      ', $
                            UVALUE='viewrecord-new')]), $
            widget_button(rec,VALUE='Show Filenames...',UVALUE='filenames'),$
            widget_button(rec,VALUE='Show Header...',UVALUE='headers'),$
            widget_button(rec,VALUE='Show Keyword Value(s)...', $
                          UVALUE='header-keyword')]
  
  ;;*** Cube menu  
  cube=widget_button(mbar,VALUE='Cube',/MENU)
  (*self.wInfo).MUST_PROJ= $
     widget_button(cube,VALUE='Build Cube',UVALUE='buildcube') 
  (*self.wInfo).MUST_ACCT= $
     widget_button(cube,VALUE='Reset Accounts',UVALUE='resetaccounts')
  ;;-------------
  b1=widget_button(cube,VALUE='Use Cube Build Feedback',UVALUE='feedback', $
                   /CHECKED_MENU,/SEPARATOR) 
  widget_control, b1, /SET_BUTTON
  b1=widget_button(cube,VALUE='Build Cube with FLUXCON',UVALUE='fluxcon', $ $
                   /CHECKED_MENU)
  ;;-------------
  (*self.wInfo).MUST_MODULE= $
     widget_button(cube,value='Set Cube Build Order...',UVALUE='setorder', $
                   /SEPARATOR)
  wMustCal=widget_button(cube,VALUE='Aperture(s)...',UVALUE='showaperture')
  ;;-------------
  wMustSel=[wMustSel,$
            widget_button(cube,VALUE='Set Background from Rec(s)...', $
                          UVALUE='setbackgroundfromrecs',/SEPARATOR)]
  (*self.wInfo).MUST_BACK= $
     [widget_button(cube,VALUE='View Background...', $
                    UVALUE='viewbackground'), $
      widget_button(cube,VALUE='View Background (new viewer)..', $
                    UVALUE='viewbackground-new'), $
      widget_button(cube,VALUE='Remove Background', $
                    UVALUE='remove-background')]
  
  but=widget_button(cube,VALUE='Load Bad Pixels...',UVALUE='loadbadpixels', $
                    /SEPARATOR)
  (*self.wInfo).MUST_BPL= $
     [widget_button(cube,VALUE='Save Bad Pixels...',UVALUE='savebadpixels') , $
      widget_button(cube,VALUE='Clear Bad Pixel List...', $
                    UVALUE='clearbadpixels')]
  
  wMustCube=[wMustCube, $
             ;;-------------
             widget_button(cube,VALUE='View Cube...',UVALUE='viewcube', $
                           /SEPARATOR) , $
             widget_button(cube,VALUE='View Cube (new viewer)...', $
                           UVALUE='viewcube-new')]
  
  ;;*** Info menu
  info=widget_button(mbar,VALUE='Info',/MENU)
  b1=widget_button(info,VALUE='Project History...',UVALUE='phist')
  (*self.wInfo).MUST_CAL= $
     [wMustCal, $
      widget_button(info,VALUE='Calibration Set Details...',UVALUE='calset')]
  ;;-------------
  b1=widget_button(info,VALUE='About CubeProject',UVALUE='about',/SEPARATOR)
  
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
               ['ID                 ', $
                'RA     ', $
                'Dec   ', $
                'UNC', $
                'BM',$
                'DATA',$
                'ACC'], $
               BUTTON_UVALUE=[0,6,7,8,9,10,11],UVALUE='sort',/ROW,MAP=0)
  
  b1=widget_button(headbase,VALUE='>',UVALUE='switchlist')
  
  (*self.wInfo).SList= $
     widget_list(b,/MULTIPLE,/ALIGN_LEFT, $
                 YSIZE=6>self->N_Records()<15,XSIZE=86,/FRAME)
  
  bar=cw_bgroup(base,IDS=ids,/ROW,UVALUE='bargroup', $
                ['Enable','Disable','Delete','Header', $
                 'View Record','View Cube','Import AOR','Save','Close'],$
                BUTTON_UVALUE= $
                ['enablerecord','disablerecord','delete','headers', $
                 'viewrecord','viewcube','addaor','save','exit'])
  ;; list of buttons that require a selection to be meaningful
  (*self.wInfo).MUST_SELECT=[wMustSel,ids[0:4]]
  (*self.wInfo).view_ids[2]=ids[4]
  
  ;; a cube must exist
  (*self.wInfo).MUST_CUBE=[wMustCube,ids[5]]
  
  widget_control, base,set_uvalue=self,/REALIZE
  
  geom=widget_info((*self.wInfo).SList,/GEOMETRY)
  (*self.wInfo).list_row=(geom.SCR_YSIZE-7)/geom.YSIZE
  (*self.wInfo).list_size_diff=geom.SCR_YSIZE- $
     (widget_info(base,/GEOMETRY)).SCR_YSIZE
  
  name='CubeProj_Show:'+self.ProjectName+self.savefile
  XManager,name,(*self.wInfo).Base,/JUST_REG
  if keyword_set(spn) then $
     self->SetProjectName,TITLE='New Project Name'
  self->UpdateAll
  (*self.wInfo).showing=1
  XManager,name, base,/NO_BLOCK,EVENT_HANDLER='CubeProj_show_event', $
           CLEANUP='CubeProj_show_kill'
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
;  ProjectName - (function) Return the project name
;=============================================================================
function CubeProj::ProjectName
  return,self.ProjectName?self.ProjectName:"(untitled)"
end

;=============================================================================
;  SetProjectName - (procedure) Set the project name, with input.
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
;  Open - Open a Project and show it
;=============================================================================
pro CubeProj::Open,pname,PROJECT=proj,_EXTRA=e
  if size(pname,/TYPE) ne 7 then begin 
     xf,pname,/RECENT,FILTERLIST=['*.cpj','*.*','*'],$
        TITLE='Load Cube Project...',/NO_SHOW_ALL,SELECT=0, $
        /MODAL,PARENT_GROUP=self->TopBase(),_EXTRA=e
  endif 
  widget_control, /HOURGLASS
  if size(pname,/TYPE) ne 7 then return ;cancelled
  proj=self->Load(pname)
  if NOT obj_valid(proj) then return
  proj->SetProperty,/SPAWNED
  proj->Show
end

;=============================================================================
;  Load - Load a Project from file
;=============================================================================
function CubeProj::Load,file,ERROR=err
  on_error,2
  catch, err
  if err ne 0 then begin 
     catch,/cancel
     self->Error,['Error loading project from '+file,!ERROR_STATE.MSG],$
                 /RETURN_ONLY
     return,-1
  endif
  widget_control,/HOURGLASS
  obj=restore_object(file,obj_class(self),OTHER_CLASSES='irs_aperture')
  if obj_valid(obj) then begin 
     if NOT obj_isa(obj,obj_class(self)) then $
        self->Error,'Invalid Cube Project'
     obj->SetProperty,CHANGED=0b,SAVE_FILE=file
  endif      
  return,obj
end

;=============================================================================
;  Save - Save a Project to file
;=============================================================================
pro CubeProj::Save,file,AS=as,CANCELED=canceled,COMPRESS=comp,NO_DATA=nodata, $
                   NO_ACOUNT=nacc
  if (size(file,/TYPE) ne 7 AND self.SaveFile eq '') OR keyword_set(as) $
     then begin 
     if self.SaveFile then start=self.SaveFile else begin 
        start=strlowcase(self->ProjectName())+".cpj"
        start=strjoin(strsplit(start,/EXTRACT),'_')
     endelse 
     xf,file,/RECENT,FILTERLIST=['*.cpj','*.*','*'],/SAVEFILE, $
        TITLE='Save Cube Project As...',/NO_SHOW_ALL,SELECT=0, $
        START=start,PARENT_GROUP=self->TopBase(),/MODAL
     if size(file,/TYPE) ne 7 then begin
        canceled=1
        return                
     endif 
  endif else if size(file,/TYPE) ne 7 then file=self.SaveFile
  widget_control, /HOURGLASS
  ;; Detach the stuff we don't want to save!
  detInfo=self.wInfo & self.wInfo=ptr_new() ;don't save the info
  detMsgList=self.MsgList & self.MsgList=ptr_new() ;or the message list
  detCal=self.cal & self.cal=obj_new() ;or the calibration object
  detCubeRecs=self.cuberecs & self.cuberecs=ptr_new() ; or the cuberec list
  
  nr=self->N_Records()
  
  if ptr_valid(self.DR) then begin 
     ;; Check default save method
     if n_elements(nodata) eq 0 then nodata=~(self.SaveMethod AND 1b)
     if n_elements(noacc) eq 0 then noacc=~(self.SaveMethod AND 2b)
     stub=ptrarr(nr)
     detRevAccts=(*self.DR).REV_ACCOUNT 
     (*self.DR).REV_ACCOUNT=stub
     if keyword_set(nodata) then begin 
        detBCD=(*self.DR).BCD
        (*self.DR).BCD=stub
        detHEADER=(*self.DR).HEADER
        (*self.DR).HEADER=stub
        detUNC=(*self.DR).UNC
        (*self.DR).UNC=stub
        detBMASK=(*self.DR).BMASK
        (*self.DR).BMASK=stub
     endif else self->RestoreAll
     if keyword_set(noacc) then begin 
        detACCOUNT=(*self.DR).ACCOUNT
        (*self.DR).ACCOUNT=stub
     endif
  endif
  
  
  oldchange=self.Changed        ;we want the file written to have changed=0!
  self.Changed=0b               ;but save the old preference incase we fail
  
  catch, serr
  if serr ne 0 then self.Changed=oldchange $ ;failed, reassign old changed
  else save,self,FILENAME=file,COMPRESS=comp
  catch,/CANCEL
  
  ;; Reattach 
  self.wInfo=detInfo           
  self.MsgList=detMsgList   
  self.cal=detCal
  self.cuberecs=detCubeRecs
  if ptr_valid(self.DR) then begin 
     (*self.DR).REV_ACCOUNT=detRevAccts
     if keyword_set(nodata) then begin 
        (*self.DR).BCD=detBCD
        (*self.DR).HEADER=detHEADER        
        (*self.DR).UNC=detUNC
        (*self.DR).BMASK=detBMASK
     endif 
     if keyword_set(noacc) then begin 
        (*self.DR).ACCOUNT=detACCOUNT
     endif 
  endif 
  
  if serr then self->Error,['Error Saving to File: ',file]
  
  if strlen(self.SaveFile) eq 0 or keyword_set(AS) then self.SaveFile=file
  self->UpdateTitle
end

;=============================================================================
;  RestoreData - Restore selected data from file
;=============================================================================
pro CubeProj::RestoreData,sel,RESTORE_CNT=cnt
  if n_elements(sel) eq 0 then sel=self->CurrentSelect()
  wh=where(~ptr_valid((*self.DR)[sel].BCD),cnt)
  if cnt eq 0 then return
  widget_control, /HOURGLASS
  
  for i=0,cnt-1 do begin 
     which=sel[wh[i]]
     file=(*self.DR)[which].FILE
     if ~file_test(file,/READ) then $
        self->Error,["Couldn't restore data from file (check filename): ",file]
     (*self.DR)[which].BCD=ptr_new(readfits(file,/SILENT,hdr))
     (*self.DR)[which].HEADER=ptr_new(hdr)
     
     ;; Optional BMASK, UNCERTAINTY
     ptr_free,(*self.DR)[which].BMASK,(*self.DR)[which].UNC
     unc_file=self->FileBCD(file,/UNCERTAINTY)
     bmask_file=self->FileBCD(file,/BMASK)
     
     if size(bmask_file,/TYPE) eq 7 && file_test(bmask_file) then $
        (*self.DR)[which].BMASK=ptr_new(readfits(bmask_file,/SILENT))
     
     if size(unc_file,/TYPE) eq 7 && file_test(unc_file) then $
        (*self.DR)[which].UNC=ptr_new(readfits(unc_file,/SILENT))
  endfor
end

;=============================================================================
;  RestoreAll - Restore All data
;=============================================================================
pro CubeProj::RestoreAll
  self->RestoreData,indgen(self->N_Records()),RESTORE_CNT=cnt
  if cnt gt 0 then self->Info, $
     string(FORMAT='("Restored ",I0," record",A)',cnt,cnt gt 1?"s":"")
end

;=============================================================================
;  LoadBadPixels - Load bad pixels from file
;=============================================================================
pro CubeProj::LoadBadPixels,file,ERROR=err
  catch, err
  if err ne 0 then begin 
     catch,/cancel
     if n_elements(un) ne 0 then free_lun,un
     self->Error,['Error loading bad pixel list from '+file,!ERROR_STATE.MSG],$
                 /RETURN_ONLY
  endif
  if size(file,/TYPE) ne 7 then begin 
     xf,file,/RECENT,FILTERLIST=['*.bpl','*.*','*'],$
        TITLE='Load Bad Pixels...',/NO_SHOW_ALL,SELECT=0, $
        /MODAL,PARENT_GROUP=self->TopBase(),_EXTRA=e
  endif 
  if size(file,/TYPE) ne 7 then return ;cancelled
  openr,un,file,/GET_LUN
  bp=lonarr(file_lines(file),/NOZERO)
  readf,un,bp
  free_lun,un
  self.bad_pixel_list=ptr_new(bp,/NO_COPY)
  self->UpdateButtons
  self->Send,/UPDATE
end


;=============================================================================
;  SaveBadPixels - Save bad pixels to file
;=============================================================================
pro CubeProj::SaveBadPixels,file
  if ~ptr_valid(self.bad_pixel_list) then return
  if size(file,/TYPE) ne 7 then begin 
     start=idl_validname(self->ProjectName(),/CONVERT_ALL)+".bpl"
     start=strjoin(strsplit(start,/EXTRACT),'_')
     xf,file,/RECENT,FILTERLIST=['*.bpl','*.*','*'],/SAVEFILE, $
        TITLE='Save Bad Pixels As...',/NO_SHOW_ALL,SELECT=0, $
        START=start,PARENT_GROUP=self->TopBase(),/MODAL
  endif 
  if size(file,/TYPE) ne 7 then return ;cancelled
  openw,un,file,/GET_LUN
  printf,un,1#(*self.bad_pixel_list)
  free_lun,un
end


;=============================================================================
;  ClearBadPixels - Clear all bad pixels out
;=============================================================================
pro CubeProj::ClearBadPixels, file
  ptr_free,self.bad_pixel_list
  self->UpdateButtons
  self->Send,/UPDATE
end

;=============================================================================
;  WriteFits - Write the Cube, Uncertainty, etc. to FITS file
;=============================================================================
pro CubeProj::WriteFits,file
  if NOT ptr_valid(self.CUBE) then return 
  if NOT ptr_valid(self.WAVELENGTH) then return
  if size(file,/TYPE) ne 7 then begin 
     xf,file,/RECENT,FILTERLIST=['*.fits','*.*','*'],/SAVEFILE, $
        TITLE='Save Cube As FITS File...',/NO_SHOW_ALL,SELECT=0, $
        START=strlowcase(strjoin(strsplit(self->ProjectName(),/EXTRACT),'_'))+$
        ".fits", PARENT_GROUP=self->TopBase(),/MODAL
     if size(file,/TYPE) ne 7 then return
  endif
  fxhmake,hdr,*self.CUBE,/extend,/date
  
  ;; Celestial coordinates
  putast,hdr,self->CubeAstrometryRecord()
  ; RADEG = 180.0d/!DPI           ; preserve double
;   fxaddpar,hdr,'EQUINOX', 2000.0,   ' Equinox of reference coordinate'
;   fxaddpar,hdr,'CTYPE1','RA---TAN', ' RA in tangent plane projection'
;   fxaddpar,hdr,'CTYPE2','DEC--TAN', ' DEC in tangent plane projection'
;   fxaddpar,hdr,'CRPIX1',(self.CUBE_SIZE[0]+1.)/2, $
;            ' Pixel x coordinate at reference point'
;   fxaddpar,hdr,'CRPIX2',(self.CUBE_SIZE[1]+1.)/2, $
;            ' Pixel y coordinate at reference point'
;   fxaddpar,hdr,'CRVAL1',self.POSITION[0],' [deg] RA at reference point'
;   fxaddpar,hdr,'CRVAL2',self.POSITION[1],' [deg] DEC at reference point'
  
;   ;; Old style angle, for older FITS readers
;   fxaddpar,hdr,'CROTA2',self.PA,' [deg] Rotation angle'
  
;   ;; New style coordinate transform
;   fxaddpar,hdr,'CDELT1',self.PLATE_SCALE, $
;            ' [deg/pix] Plate scale, coordinate 1'
;   fxaddpar,hdr,'CDELT2',self.PLATE_SCALE, $
;            ' [deg/pix] Plate scale, coordinate 2'
;   fxaddpar,hdr,'PC1_1' ,-cos(self.PA/RADEG),' Transformation matrix element'
;   fxaddpar,hdr,'PC1_2' ,-sin(self.PA/RADEG),' Transformation matrix element'
;   fxaddpar,hdr,'PC2_1' ,-sin(self.PA/RADEG),' Transformation matrix element'
;  fxaddpar,hdr,'PC2_2' , cos(self.PA/RADEG),' Transformation matrix element'
  
  ;; Wavelength coordinates and wavelength LUT binary table extension.
  fxaddpar,hdr,'CTYPE3','WAVE-TAB','Wavelength'
  fxaddpar,hdr,'CUNIT3','um','Wavelength units'
  fxaddpar,hdr,'PS3_0','WCS-TAB','Coordinate table extension name'
  fxaddpar,hdr,'PS3_1','WAVELENGTH','Coordinate table column name'
  
  ;; Description
  sxaddhist, ['The SIRTF Nearby Galaxy Survey (SINGS) Legacy Project', $
              'This file contains a spectral cube assembled from an IRS', $
              'step & stare spectral mapping dataset.', $
              'For more information on SINGS see http://sings.stsci.edu'], $
             hdr,/COMMENT
  
  fxaddpar,hdr,'CUBE-DT',jul2date(self.CUBE_DATE),' Cube build date'
  
  fxaddpar,hdr,'FILENAME',filestrip(file),' Name of this file'

  ;;Module/Order
  fxaddpar,hdr,'APERNAME',self.MODULE,' The IRS module'
  fxaddpar,hdr,'ORDER',self.ORDER,' The order: 0 for all orders'
  
  ;; Code version & calibration set used to create cube
  fxaddpar,hdr,'CUBE_VER',self.version,' CUBISM version used'
  self->LoadCalib
  fxaddpar,hdr,'CAL_SET',self.cal->Name(),' IRS Calibration set used'
  

  
  ;; Write the primary header and data
  fxwrite,file,hdr,*self.CUBE
;  fxwrite,file,hdr,*self.CUBE_UNC
  
  ;; Make the wavelength LUT extension header
  fxbhmake,hdr,1,'WCS-TAB','Wavelength look-up table for cube dimension 3'
  fxbaddcol,wcol,hdr,*self.WAVELENGTH,'WAVELENGTH','Column label'
  fxbcreate,unit,file,hdr
  fxbwrite,unit,*self.WAVELENGTH,wcol,1
  fxbfinish,unit
end

;=============================================================================
;  ExportToMain - Send ourselves to the $MAIN$ level, using the
;                 unsupported RSI routine routine_names().  Caveat
;                 Exportor.  Care is taken to ensure it will at least
;                 compile, and trap errors in case routine_names
;                 vanishes.  Also supports exporting spectra and maps
;                 with the SPECTRUM & MAP keywords.
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
   spQ: print,' *** CUBISM: ',title,' exported to variable: '
   print,'   ====> '+var_name+' <==='
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
  
  oldself=self                  ;our old self, soon to be history!
  wsav=self.wInfo               ;detach wInfo and MsgList from Self, to retach
  msav=self.MsgList             ;to the transmogrified self.
  nsav=self.ProjectName
  self.wInfo=ptr_new() & self.MsgList=ptr_new() ;the detachment
  oldchange=self.Changed
  self.Changed=0b               ;to ensure we won't try to save ourselves 
  self.ProjectName=''           ;avoid name conflict with new self.

  ;; Restore the on-disk self to this space.. this sets Changed to 0
  ;; on the new object, and sets up the new object's SaveFile, only
  ;; if the object was actually read in correctly.
  self=self->Load(self.SaveFile,ERROR=rerr)
  if rerr eq 0 then begin 
     ;; Self has been overwritten by restore.... kill our old self.
     
     obj_destroy,oldself        ;kill our old self, except wInfo and MsgList
     self.wInfo=wsav            ;attach wInfo and MsgList to new self
     self.MsgList=msav
     self->UpdateList           ;just in case things changed
     
     ;; let any widgets know about the change in self!
     if self->isWidget() then $
        widget_control,(*self.wInfo).Base,SET_UVALUE=self
     
     ;; make sure the title is set correctly!
     self->UpdateTitle
  endif else begin              ;failed, restore the old self
     self.ProjectName=nsav
     self.wInfo=wsav & self.MsgList=msav 
     self.Changed=oldchange
  endelse 
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
  if self.Changed then pn='*'+pn+'*'
  widget_control, (*self.wInfo).Base,  $
                  TLB_SET_TITLE='CUBISM Project: '+ pn + $
                  "  <"+(self.SaveFile?filestrip(self.SaveFile): $
                         "(unsaved)")+">"
end

;=============================================================================
;  UpdateList - Update the BCD list
;=============================================================================
pro CubeProj::UpdateList,CLEAR_SELECTION=cs
  self->Sort
  if NOT self->IsWidget() then return
  widget_control, (*self.wInfo).Base,UPDATE=0
  if keyword_set(cs) then begin 
     widget_control, (*self.wInfo).SList, set_value=self->List(), $
                     SET_LIST_SELECT=-1,SET_LIST_TOP=t
  endif else begin 
     ls=self->CurrentSelect()<(self->N_Records()-1)
     t=widget_info((*self.wInfo).SList,/LIST_TOP) 
     widget_control, (*self.wInfo).SList, set_value=self->List(), $
                     SET_LIST_SELECT=ls,SET_LIST_TOP=t
  endelse
  widget_control, (*self.wInfo).Base,/UPDATE
end

;=============================================================================
;  List - List the data records
;=============================================================================
function CubeProj::List
   n=self->N_Records()
   if n eq 0 then return,' '
   which_list=0
   if self->isWidget() then if (*self.wInfo).which_list then which_list=1 
   
   for i=0,n-1 do begin 
      if which_list eq 0 then begin ;the standard list
         tchar=([" ","d","c","f"])[(*self.DR)[i].type]
         s=string(FORMAT='(" ",A20,T23,F6.2,T30,A,T49,A,T68,A8,T78,' + $
                  'I3,"[",I0,",",I0,"]")', $
                  (*self.DR)[i].ID, $
                  (*self.DR)[i].TIME, $
                  jul2date((*self.DR)[i].DATE_OBS,FORM='D*T'), $
                  jul2date((*self.DR)[i].DATE,FORM='D*T'), $
                  tchar+irs_fov((*self.DR)[i].FOVID,/SHORT_NAME), $
                  (*self.DR)[i].EXP,(*self.DR)[i].ROW,(*self.DR)[i].COLUMN)
      endif else begin          ;the additional list
         if ptr_valid((*self.DR)[i].ACCOUNT) then begin 
            if self.ACCOUNTS_VALID AND 1b then begin 
               if ptr_valid((*self.DR)[i].REV_ACCOUNT) then acct="Y(+R)" $
               else acct="Y"
            endif else acct="INV"
         endif else acct="N"
         if self.reconstructed_pos then pos=(*self.DR).REC_POS else $
            pos=(*self.DR)[i].RQST_POS
         s=string(FORMAT= $
                  '(" ",A20,T23,A11,T34,A12,T49,A1,T56,A1,T63,A1,T69,A5)', $
                  (*self.DR)[i].ID, $
                  radecstring(pos[0],/RA),radecstring(pos[1]), $
                  ptr_valid((*self.DR)[i].UNC)?'Y':'N', $
                  ptr_valid((*self.DR)[i].BMASK)?'Y':'N', $
                  ptr_valid((*self.DR)[i].BCD)?'Y':'N', $
                  acct)
      endelse       
      if (*self.DR)[i].DISABLED then begin 
         b=byte(s)
         b[where(b eq 32b)]=45b ;replace space with dash
         s=string(b)
      endif
      if i eq 0 then list=s else list=[list,s]
   endfor 
   return,list
end

;=============================================================================
;  UpdateButtons - Change Button sensitivity
;=============================================================================
pro CubeProj::UpdateButtons
  if NOT self->IsWidget() then return
  sel=self->CurrentSelect()
  for i=0,n_elements((*self.wInfo).MUST_SELECT)-1  do  $
     widget_control,((*self.wInfo).MUST_SELECT)[i],  $
                    SENSITIVE=ptr_valid(self.DR) && sel[0] ne -1
  nsel=n_elements(sel) 
  if (nsel gt 1) ne ((*self.wInfo).nsel_sav gt 1)then begin 
     if nsel gt 1 then begin 
        widget_control,(*self.wInfo).view_ids[0],SET_VALUE='View Stack...'
        widget_control,(*self.wInfo).view_ids[1], $
                       SET_VALUE='View Stack (new viewer)...'
        widget_control,(*self.wInfo).view_ids[2], $
                       SET_VALUE='View Stack '
     endif else begin 
        widget_control,(*self.wInfo).view_ids[0],SET_VALUE='View...'
        widget_control,(*self.wInfo).view_ids[1], $
                       SET_VALUE='View (new viewer)...'
        widget_control,(*self.wInfo).view_ids[2], $
                       SET_VALUE='View Record'
     endelse 
     (*self.wInfo).nsel_sav=nsel
  endif 
     
  for i=0,n_elements((*self.wInfo).MUST_CAL)-1  do  $
     widget_control,((*self.wInfo).MUST_CAL)[i], SENSITIVE=obj_valid(self.cal)
  
  for i=0,n_elements((*self.wInfo).MUST_PROJ)-1  do  $
     widget_control, (*self.wInfo).MUST_PROJ[i], SENSITIVE=ptr_valid(self.DR)
  
  widget_control, (*self.wInfo).MUST_ACCT, SENSITIVE=self.ACCOUNTS_VALID AND 1b
  
  widget_control, (*self.wInfo).MUST_MODULE,SENSITIVE=self.MODULE ne ''
  
  for i=0,n_elements((*self.wInfo).MUST_CUBE)-1 do $
     widget_control, ((*self.wInfo).MUST_CUBE)[i], $
                     SENSITIVE=ptr_valid(self.CUBE)
  
  for i=0,n_elements((*self.wInfo).MUST_BACK)-1 do $ 
     widget_control, ((*self.wInfo).MUST_BACK)[i], $
                     SENSITIVE=ptr_valid(self.BACKGROUND)
  
  for i=0,n_elements((*self.wInfo).MUST_BPL-1)-1 do $
     widget_control, ((*self.wInfo).MUST_BPL)[i], $
                     SENSITIVE=ptr_valid(self.BAD_PIXEL_LIST)
  
  widget_control, (*self.wInfo).MUST_SAVE_CHANGED,SENSITIVE= $
                  strlen(self.SaveFile) ne 0 AND keyword_set(self.Changed)
  
  widget_control, (*self.wInfo).MUST_UNRESTORED,SENSITIVE= $
                  ptr_valid(self.DR) && $
                  ~array_equal(ptr_valid((*self.DR).BCD),1b)
end

;=============================================================================
;  UpdateColumnHeads - Update the sort selected button (carry to page 2).
;=============================================================================
pro CubeProj::UpdateColumnHeads
  if NOT self->IsWidget() then return
  flags=bytarr(12)
  flags[self.sort]=1b
  if (*self.wInfo).which_list eq 0 then flags=flags[0:5] else $
     flags=[flags[0],flags[6:*]]
  widget_control, (*self.wInfo).wHead[(*self.wInfo).which_list],SET_VALUE=flags
end

;=============================================================================
;  UpdateAll
;=============================================================================
pro CubeProj::UpdateAll,NO_LIST=nl
  self->UpdateButtons
  if NOT keyword_set(nl) then self->UpdateColumnHeads
  if NOT keyword_set(nl) then self->UpdateList
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
pro CubeProj::FindViewer,NEW_VIEWER=new_viewer,CUBE_MODE=cube_mode
  pvcr=ptr_valid(self.cuberecs)
  ;; clean list
  if pvcr then begin 
     wh=where(obj_valid(*self.cuberecs),NCOMPLEMENT=nc,cnt)
     if cnt eq 0 then begin 
        ptr_free,self.cuberecs
        pvcr=0 
     endif else if nc gt 0 then *self.cuberecs=(*self.cuberecs)[wh]
  endif 
  
  ;; Need a new viewer?
  if keyword_set(new_viewer) || ~pvcr then begin 
     if pvcr then self->MsgListRemove,*self.cuberecs
     cubeview,CUBE=self,RECORD=rec ;have them sign up for our messages     
     if pvcr then *self.cuberecs=[*self.cuberecs,rec] else $
        self.cuberecs=ptr_new([rec])
     return
  endif
  
  ;; Pick the best existing viewer
  recs=self->GetMsgObjs(CLASS='CubeRec')
  talking=obj_valid(recs[0])
  
  for r=0,n_elements(*self.cuberecs)-1 do begin 
     rec=(*self.cuberecs)[r]
     rec->GetProperty,BCD_MODE=bcd_mode
     if bcd_mode eq ~keyword_set(cube_mode) then begin 
        ;; Not already talking to him?
        if ~talking || rec ne recs[0] then begin 
           if talking then self->MsgListRemove,recs
           self->MsgSignup,rec
        endif 
        return
     endif 
  endfor 
  
  ;; Didn't find one with the right mode... just take the first one
  if size(recs[0],/TYPE) ne 11 || recs[0] ne (*self.cuberecs)[0] then begin 
     self->MsgListRemove,recs
     self->MsgSignup,(*self.cuberecs)[0]
  endif 
end

;=============================================================================
;  ViewCube - View the cube in an existing or new viewer
;=============================================================================
pro CubeProj::ViewCube,NEW_VIEWER=new
  if NOT ptr_valid(self.CUBE) then self->Error,'No cube to view'
  self->FindViewer,/CUBE_MODE,NEW_VIEWER=new
  self->Send,/CUBE
end

;=============================================================================
;  ViewBackground - View the background in an existing or new viewer
;=============================================================================
pro CubeProj::ViewBackground,NEW_VIEWER=new
  if NOT ptr_valid(self.BACKGROUND) then self->Error,'No background to view'
  self->FindViewer,NEW_VIEWER=new
  self->Send,/BACKGROUND
end

;=============================================================================
;  ViewRecord - View the record(s) in an existing or new viewer
;=============================================================================
pro CubeProj::ViewRecord,rec,NEW_VIEWER=new
  self->RecOrSelect,rec
  self->RestoreData,rec
  self->FindViewer,NEW_VIEWER=new
  self->Send,RECORD=rec
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
pro CubeProj::DisableRecord,recs,DISABLE=dis
  if NOT ptr_valid(self.DR) then return
  if n_elements(dis) eq 0 then dis=1b
  self->RecOrSelect,recs
  (*self.DR)[recs].DISABLED=dis
  self.Changed=1b
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
  self.Changed=1b
  self->UpdateAll
end


;=============================================================================
;  SetBackgroundFromRecs - Set the BCD background from selected recs.
;=============================================================================
pro CubeProj::SetBackgroundFromRecs,recs
  self->RecOrSelect,recs
  if recs[0] eq -1 then return
  n=n_elements(recs) 
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
  bcds=self->BCD(recs)
  back=imcombine(bcds,/AVERAGE,REJECT_MINMAX=choice eq 1)
  self.BACKGROUND=ptr_new(back,/NO_COPY)
  self.BACK_CNT=n
  self.BACK_DATE=systime(/JULIAN)
  self.Changed=1b
  self.ACCOUNTS_VALID OR= 4b
  self->UpdateButtons
end

;=============================================================================
;  CurrentSelect - Return the currently selected records (if any).
;=============================================================================
function CubeProj::CurrentSelect
  if NOT self->IsWidget() then return,-1
  return,widget_info((*self.wInfo).SList, /LIST_SELECT)
end

;=============================================================================
;  RecOrSelect - Convenience routine: use specified records, or take
;                from the GUI selection.
;=============================================================================
pro CubeProj::RecOrSelect,recs
  if n_elements(recs) eq 0 then begin 
     recs=self->CurrentSelect()
     if recs[0] eq -1 then $
        self->Error,'No Records Selected or Passed'
  endif   
end

;=============================================================================
;  Sort - Sort the Data records. 
;=============================================================================
pro CubeProj::Sort,sort
  n=self->N_Records()
  if n le 1 then return         ;no sort for one only
  case self.sort of
     0: s=sort((*self.DR).ID)
     1: s=sort((*self.DR).TIME)
     2: s=sort((*self.DR).DATE_OBS)
     3: s=sort((*self.DR).DATE)
     4: s=sort(long((*self.DR).FOVID)+ishft(long((*self.DR).TYPE),16))
     5: s=sort((*self.DR).EXP)
     6: s=sort(self.reconstructed_pos?(*self.DR).REC_POS[0]: $
               (*self.DR).RQST_POS[0])
     7: s=sort(self.reconstructed_pos?(*self.DR).REC_POS[1]: $
               (*self.DR).RQST_POS[1])
     8: s=sort(ptr_valid((*self.DR).UNC))
     9: s=sort(ptr_valid((*self.DR).BMASK))
     10: s=sort(ptr_valid((*self.DR).BCD))
     11: s=sort(ptr_valid((*self.DR).ACCOUNT))
  endcase
  *self.DR=(*self.DR)[s]        ;rearrange
  if self->IsWidget() then begin 
     ;; Preserve selections
     ls=widget_info((*self.wInfo).SList,/LIST_SELECT)<(n-1)
     if ls[0] eq -1 then return
     b=bytarr(n)
     b[ls]=1b
     b=b[s]
     widget_control,(*self.wInfo).SList,SET_LIST_SELECT=where(b)
  endif 
end

pro CubeProj::Print,entries,_EXTRA=e
  print,transpose(self->Info(entries,_EXTRA=e))
end

;=============================================================================
;  Info - Info on the cube's contents and history
;=============================================================================
function CubeProj::Info,entries, NO_DATA=nd
  str=['IRS Spectral Cube: '+self->ProjectName()+ $
       (((self.ACCOUNTS_VALID AND (NOT 1b)) ne 0b)?" (needs rebuilding)":"")]
  str=[str,' Cube Created: '+ $
       (self.CUBE_DATE eq 0.0d?"(not yet)":jul2date(self.CUBE_DATE))]
  str=[str,' ' + (self.MODULE?self.MODULE:"(no module)")+ $
       (self.ORDER ne 0?' Order '+strtrim(self.ORDER,2): $
        ' all orders') + (self->N_Records() gt 0? $
       (' -- '+strtrim(self->N_Records(),2)+' BCDs '):"")]
  str=[str,' Using IRS Calib object '+(self.cal_file?self.cal_file:"(none)")+ $
       " ("+(obj_valid(self.cal)?"":"not ")+"loaded"+")"]
  str=[str,' Background: '+ $
       (ptr_valid(self.BACKGROUND)? $
        (strtrim(self.back_cnt,2)+' records, '+jul2date(self.BACK_DATE)): $
        "none")]
  str=[str,' FLUXCON: '+(self.fluxcon?"Yes":"No")]
  str=[str,' Bad Pixels: '+(ptr_valid(self.BAD_PIXEL_LIST)? $
                            strtrim(n_elements(*self.BAD_PIXEL_LIST),2): $
                            "none")]
  aps=' Apertures:'
  if NOT ptr_valid(self.APERTURE) OR self.MODULE eq '' then begin 
     aps=aps+' (default)' 
  endif else begin 
     nap=n_elements(*self.APERTURE)
     ords=obj_valid(self.cal)?self.cal->Orders(self.MODULE):intarr(nap)+1
     for i=0,nap-1 do begin 
        ap=(*self.APERTURE)[i]
        aps=[aps,string(FORMAT='(%"  %s  %4.2f->%4.2f : %4.2f->%4.2f")',$
                        (nap eq 1?"All Orders":("Order "+strtrim(ords[i],2))),$
                        ap.low,ap.high)]
     endfor 
  endelse 
  str=[str,aps]

  str=[str, $
       ' '+string(FORMAT='(I0,"x",I0," steps = ",F7.3," x ",F7.3,' + $
                  '" arcsec"," (",F6.3," arcsec/pixel)")',self.NSTEP, $
                  self.STEP_SIZE*3600.0D*self.NSTEP,self.PLATE_SCALE*3600.0D)]
  str=[str,' '+string(FORMAT='("PR Sample Size: ",F6.3," x ",F6.3," pixels")',$
                      self.PR_SIZE)]
  
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
     str=[str,string(FORMAT='(%"   EXP: %2.0d (%d/%d) (%2.0d,%2.0d) ' + $
                     'RA: %s, DEC: %s")', $
                     rec.EXP,rec.CYCLE,rec.NCYCLES,rec.ROW,rec.COLUMN, ra,dec)]
  endfor 
  return,str
end

;=============================================================================
;  FileBCD - Find the BCD version of any given fits file
;=============================================================================
function CubeProj::FileBCD,file,BMASK=bm,UNCERTAINTY=unc
  if keyword_set(bm) then reg='bmask\.fits$' else $
     if keyword_set(unc) then reg='func\.fits$' else $
        reg='bcd(_fp)?\.fits$'
  if stregex(file,reg,/BOOLEAN) then return,file
  parts=stregex(file,'^(.*[0-9][._])[^.]+\.fits$',/EXTRACT,/SUBEXPR)
  base=parts[1]
  if strlen(base) eq 0 then return,-1
  if keyword_set(bm) then filt='bmask' else $
     if keyword_set(unc) then filt='func' else filt='bcd*'
  f=file_search(base+filt+'.fits',COUNT=cnt)
  if cnt eq 0 then return,-1
  return,f[0]
end

;=============================================================================
;  SetProperty - Set various cube properties.  Most of these should be
;                automatically discovered from the BCD's.  If any are
;                actually changed, indicate that the account is no
;                longer valid.
;=============================================================================
pro CubeProj::SetProperty,PLATE_SCALE=ps,NSTEP=nstep,STEP_SIZE=stepsz, $
                          MODULE=md,ORDER=ord, PR_WIDTH=prw, $
                          PR_SIZE=prz,CAL_FILE=cal_file,CAL_OBJECT=cal, $
                          APERTURE=aper,SAVE_FILE=sf,CHANGED=chngd, $
                          PROJECTNAME=pn,SPAWNED=spn,FEEDBACK=fb, $
                          BAD_PIXEL_LIST=bpl,RECONSTRUCTED_POSITIONS=rcp
  if n_elements(ps) ne 0 then begin 
     if self.PLATE_SCALE ne ps then begin 
        self.PLATE_SCALE=ps
        self->ResetAccounts,/NO_UPDATE & self.Changed=1b
     endif 
  endif 
  if n_elements(nstep) ne 0 then self.NSTEP=nstep
  if n_elements(stepsz) ne 0 then begin 
     if NOT array_equal(self.STEP_SIZE,stepsz) then begin 
        self.STEP_SIZE=stepsz
        self->ResetAccounts,/NO_UPDATE & self.Changed=1b
     endif 
  endif 
  if n_elements(md) ne 0 then begin 
     if md ne self.MODULE then begin
        self.MODULE=md
        self->ResetAccounts,/NO_UPDATE & self.Changed=1b
     endif 
  endif 
  if n_elements(ord) ne 0 then begin 
     if ord ne self.ORDER then begin 
        self.ORDER=ord
        self->ResetAccounts,/NO_UPDATE & self.Changed=1b
     endif 
  endif 
  if n_elements(prw) ne 0 then begin 
     prw=0.>prw 
     if prw ne self.PR_SIZE[1] then begin 
        self.PR_SIZE[1]=prw
        self->ResetAccounts,/NO_UPDATE & self.Changed=1b
     endif
  endif
  if n_elements(prz) ne 0 then begin 
     if NOT array_equal(self.PR_SIZE,prz) then begin 
        self.PR_SIZE=prz
        self->ResetAccounts,/NO_UPDATE & self.Changed=1b
     endif
  endif
  if n_elements(cal_file) ne 0 then begin 
     if self.cal_file ne cal_file then begin 
        if obj_valid(self.cal) then obj_destroy,self.cal
        self.cal_file=cal_file
        self->ResetAccounts,/NO_UPDATE & self.Changed=1b
     endif 
  endif
  if n_elements(cal) ne 0 then begin 
     if obj_isa(cal,'IRS_Calib') then begin 
        if self.cal ne cal then begin 
           self.cal=cal 
           self->ResetAccounts,/NO_UPDATE & self.Changed=1b
        endif 
     endif else self->Error,'Calibration object not of correct type.'
  endif
  if n_elements(aper) ne 0 then begin 
     ptr_free,self.APERTURE
     self.APERTURE=ptr_new(aper)
     self->ResetAccounts,/NO_UPDATE & self.Changed=1b
  endif 
  if n_elements(sf) ne 0 then self.SaveFile=sf
  if n_elements(pn) ne 0 then begin 
     pn=strmid(pn,0,32)         ;limit it
     self.ProjectName=pn
     self.Changed=1b
  endif
  if n_elements(chngd) ne 0 then self.Changed=chngd
  if n_elements(spn) ne 0 then self.Spawned=spn
  if n_elements(fb) ne 0 then self.feedback=fb
  if n_elements(bpl) ne 0 then begin 
     ptr_free,self.bad_pixel_list
     if bpl[0] ne -1 then self.bad_pixel_list=ptr_new(bpl)
     self.Changed=1b
  endif 
  if n_elements(rcp) ne 0 then begin 
     self.reconstructed_pos=keyword_set(rcp) 
     self->ResetAccounts,/NO_UPDATE & self.Changed=1b
  endif
  self->UpdateAll,/NO_LIST
end


;=============================================================================
;  GetProperty - Get properties, as a pointer to the original data if
;                POINTER set.
;=============================================================================
pro CubeProj::GetProperty, ACCOUNT=account, WAVELENGTH=wave, CUBE=cube, $
                           UNCERTAINTY_CUBE=err, PR_SIZE=prz, PR_WIDTH=prw, $
                           CALIB=calib, MODULE=module, APERTURE=ap, $
                           PROJECT_NAME=pn,DR=dr, TLB_OFFSET=tboff, $
                           TLB_SIZE=tbsize,BCD_SIZE=bcdsz, VERSION=version, $
                           ASTROMETRY=astr,POSITION_ANGLE=pa,BACKGROUND=bg, $
                           BAD_PIXEL_LIST=bpl,PMASK=pmask,POINTER=ptr
  ptr=keyword_set(ptr) 
  if arg_present(account) && ptr_valid(self.ACCOUNT) then $
     account=ptr?self.account:*self.account
  if arg_present(wave) && ptr_valid(self.WAVELENGTH) then $
     wave=ptr?self.WAVELENGTH:*self.WAVELENGTH
  if arg_present(cube) && ptr_valid(self.CUBE) then $
     cube=ptr?self.CUBE:*self.CUBE
  if arg_present(unc) && ptr_valid(self.CUBE_UNC) then unc=*self.CUBE_UNC
  if arg_present(prz) then prz=self.PR_SIZE
  if arg_present(prw) then prw=self.PR_SIZE[1]
  if arg_present(calib) then begin 
     self->LoadCalib            ;ensure it's loaded
     calib=self.cal
  endif 
  if arg_present(module) then module=self.MODULE
  if arg_present(ap) then begin 
     self->NormalizeApertures
     ap=ptr?self.APERTURE:*self.APERTURE
  endif 
  if arg_present(pn) then pn=self->ProjectName()
  if arg_present(dr) then dr=self.DR
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
  if arg_present(tbsize) then begin 
     if NOT ptr_valid(self.wInfo) then tbsize=-1 else $
        if widget_info((*self.wInfo).Base,/VALID_ID) then $
        widget_control, (*self.wInfo).Base,TLB_GET_SIZE=tbsize else tbsize=-1
  endif
  if arg_present(bcdsz) then begin 
     if NOT ptr_valid(self.DR) then bcdsz=0 else if $
        NOT ptr_valid((*self.DR)[0].BCD) then bcdsz=0 else  $
        bcdsz=size(*(*self.DR)[0].BCD,/DIMENSIONS)
  endif 
  if arg_present(version) then version=self.version
  if arg_present(astr) then astr=self->CubeAstrometryRecord()
  if arg_present(pa) then pa=self.PA
  if arg_present(bg) && ptr_valid(self.BACKGROUND) then $
     bg=ptr?self.BACKGROUND:*self.BACKGROUND
  if arg_present(bpl) && ptr_valid(self.bad_pixel_list) then $
     bpl=ptr?self.bad_pixel_list:*self.bad_pixel_list 
  if arg_present(pmask) then begin 
     self->LoadCalib
     self.cal->GetProperty,self.module,PMASK=pmask
     if ~ptr && ptr_valid(pmask) then pmask=*pmask
  endif
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
function CubeProj::PRs,ORDERS=ords,ALL_ORDERS=all,FULL=full
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
     prs[i]=ptr_new(self.cal->GetWAVSAMP(self.MODULE,ords[i],/PIXEL_BASED,$
                                         FULL=full,APERTURE=ap, $
                                         PR_WIDTH=self.PR_SIZE[1] eq 0.0? $
                                         1.0:self.PR_SIZE[1]))
  endfor 
  return,prs
end

;=============================================================================
;  Cube
;=============================================================================
function CubeProj::Cube,pln
  if NOT ptr_valid(self.CUBE) then return,-1
  if n_elements(pln) ne 0 then begin 
     pln=0>pln<((size(*self.CUBE,/DIMENSIONS))[2]-1)
     return,(*self.CUBE)[*,*,pln]
  endif else return,*self.CUBE 
end

;=============================================================================
;  BCD
;=============================================================================
function CubeProj::BCD, which,UNCERTAINTY=unc,BMASK=bmask
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
        unc=make_array(/FLOAT,s)
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
pro CubeProj::LoadCalib,SELECT=sel
  @cubism_dir
  if keyword_set(sel) then begin 
     cd,filepath(ROOT_DIR=irs_calib_dir,"sets")
     if self->IsWidget() then begin 
        xf,calname,/RECENT,FILTERLIST=['*.cal','*.*','*'], $
           TITLE='Load Calibration Object',/NO_SHOW_ALL,SELECT=0,/MODAL, $
           PARENT_GROUP=self->TopBase()
        if size(calname,/TYPE) eq 7 then begin 
           calname=filestrip(calname)
           if calname ne self.cal_file then begin 
              self.cal_file=calname
              self.changed=1b
              self->ResetAccounts,/NO_UPDATE
           endif 
        endif else return
     endif 
  endif else if obj_isa(self.cal,'IRS_Calib') then return

  if self.cal_file eq '' then begin 
     self.cal_file=filestrip(irs_recent_calib()) ;use the most recent
     self->ResetAccounts,/NO_UPDATE
     self.changed=1b            ;otherwise, same file, no change.
     self->Info,['Calibration set unspecified, loading most recent: ', $
                 '  '+self.cal_file]
  endif
  obj_destroy,self.cal
  self.cal=irs_restore_calib(self.cal_file)
  self->UpdateButtons & self->UpdateTitle
end


;=============================================================================
;  FluxConImage - Make an image to match the BCD which contains, for
;                 each pixel, the factor to convert it from e/s to Jy
;                 by division. (fluxcon=instrumental/fluxconimage)
;=============================================================================
function CubeProj::FluxConImage
  if ~ptr_valid(self.DR) then return,-1
  
  prs=self->PRs(/ALL_ORDERS,ORDERS=ords)
  fcimage=make_array(size(*(*self.DR)[0].BCD,/DIMENSIONS),VALUE=0.)
  total_areas=make_array(size(fcimage,/DIMENSIONS),VALUE=0.)
  for i=0,n_elements(ords)-1 do begin 
     lam=(*prs[i]).lambda
     self.cal->GetProperty,self.MODULE,ords[i],FLUXCON=fluxcon,TUNE=tune, $
                           KEY_WAV=key_wav
     flux_conv=fluxcon*poly(lam-key_wav,tune)
     for j=0,n_elements(lam)-1 do begin 
        pix=*(*prs[i])[j].pixels
        areas=*(*prs[i])[j].areas
        total_areas[pix]+=areas
        fcimage[pix]+=flux_conv[j]*areas
     endfor 
  endfor 
  wh=where(total_areas gt 0.,cnt,COMPLEMENT=wh_none,NCOMPLEMENT=cnt_none)
  if cnt gt 0 then fcimage[wh]/=total_areas[wh]
  if cnt_none gt 0 then fcimage[wh_none]=1.
  return,fcimage
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
  ords=self->BuildOrders()
  ptr_free,self.WAVELENGTH
  heap_free,self.MERGE
  wave1=(self.cal->GetWAVSAMP(self.MODULE,ords[0],/PIXEL_BASED, $
                              PR_WIDTH=self.PR_SIZE[1], /SAVE_POLYGONS, $
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
  
  nap=n_elements(*self.APERTURE)
  wave_zero=lonarr(nord) & wh_clear_sav=-1
;  plot,[18,40],[0,11],/NODATA,/XSTYLE,/YSTYLE
;  oplot,wave1,replicate(1,n_elements(wave1))
  for ord=1L,nord-1L do begin
     ;; Work with previous, overlap with current order
     wave2=(self.cal->GetWAVSAMP(self.MODULE,ords[ord],/PIXEL_BASED, $
                                 PR_WIDTH=self.PR_SIZE[1], $
                                 APERTURE=(nap eq 1?(*self.APERTURE)[0]: $
                                           (*self.APERTURE)[ord]))).lambda
     nw1=n_elements(wave1) & nw2=n_elements(wave2) 
     min_wav1=min(wave1,max=max_wav1)
     min_wav2=min(wave2,max=max_wav2)
;     oplot,wave2,replicate(ord+1,nw2) 
     
     ;; Are we prepending or appending?
     prepend=(min_wav2 lt min_wav1 AND wave2[0] lt wave2[1]) OR $
             (min_wav1 lt min_wav2 AND wave1[0] gt wave1[1])
     
     ;; No overlap case... just concatenate
     if min_wav2 gt max_wav1 OR max_wav2 lt min_wav1 then begin 
        if prepend then begin   ; prepend new
           if n_elements(wave) gt 0 then wave=[wave1,wave] else wave=wave1
           wave_zero[0:ord-1]=wave_zero[0:ord-1]+n_elements(wave2) 
        endif else begin        ; append new
           if n_elements(wave) gt 0 then wave=[wave,wave1] else wave=wave1
           wave_zero[ord]=wave_zero[ord-1]+n_elements(wave1) 
        endelse
        wh_clear_sav=-1
;        plots,wave(wave_zero[ord]),!Y.CRANGE,/DATA        
        continue
     endif
  
     ;; Locations of the overlap region in each wavelength vector.
     wh_over1=where(wave1 gt min_wav2 AND $
                    wave1 lt max_wav2,cnt1,COMPLEMENT=wh_clear1)
     wh_over2=where(wave2 gt min_wav1 AND $
                    wave2 lt max_wav1,cnt2,COMPLEMENT=wh_clear2)
     
     ;; Use as the primary wavelength set whichever had more samples in
     ;; the original overlap region
     if cnt1 ge cnt2 then begin  ; merge in the 2nd's waves.
        use_wav=wave1[wh_over1]
        lose_wav=wave2[wh_over2]
        self->AddMergeRec,ord,wh_over2,lose_wav
;        oplot,use_wav,replicate(ord,n_elements(use_wav)),COLOR=3
;        oplot,lose_wav,replicate(ord+1,n_elements(lose_wav)),COLOR=2
        ;;if max_wav1 gt max_wav2 then lead_in=1
        ;;if min_wav1 lt min_wav2 then lead_out=1
     endif else begin           ;merge in the 1st's remaining waves.
        use_wav=wave2[wh_over2]
        lose_wav=wave1[wh_over1]
        ;; This order's working segment may have already been
        ;; truncated on the last round
        self->AddMergeRec,ord-1,wh_clear_sav[0] ne -1? $
                          wh_clear_sav[wh_over1]:wh_over1,lose_wav
;        oplot,use_wav,replicate(ord+1,n_elements(use_wav)),COLOR=3
;        oplot,lose_wav,replicate(ord,n_elements(lose_wav)),COLOR=2
        ;if max_wav2 gt max_wav1 then lead_in=1
        ;if min_wav2 lt min_wav1 then lead_out=1
     endelse
     
;     nu=n_elements(use_wav)
;     nl=n_elements(lose_wav)
;      if lead_in then begin      ;ease into the new sampling at the short end
;         nlead=2>(nu-1)<3
;         su=sort(use_wav)
;         sl=sort(lose_wav)
;         delta_use=mean(use_wav[su[1:nlead-1]]-use_wav[su[0:nlead-2]])
;         delta_lose
        
;      endif
;      if lead_out then begin     ;ease into the new sampling at the long end
;     endif 

     ;; Excise the overlapping chunks from both, leaving the clear chunks
     if wh_clear1[0] ne -1 then wave1=wave1[wh_clear1] 
     if wh_clear2[0] ne -1 then wave2=wave2[wh_clear2]
     
     ;; Concat wave1 and overlap onto wavelengths: either
     ;;  [wave1 use_wav wave2] or [wave2 use_wav wave1], depending on order
     if prepend then begin
        if n_elements(wave) gt 0 then wave=[use_wav,wave1,wave] $
        else wave=[use_wav,wave1]
        wave_zero[0:ord-1]=wave_zero[0:ord-1]+n_elements(wave2) ;shift down
     endif else begin           
        ;; the next will go at the use_wave boundary
        wave_zero[ord]=n_elements(wave)+n_elements(wave1) 
        ;; appending new set of wavelengths
        if n_elements(wave) gt 0 then wave=[wave,wave1,use_wav] $
        else wave=[wave1,use_wav]
;        oplot,wave,replicate(ord+.25,n_elements(wave))
     endelse
;     plots,wave[wave_zero[ord]],!Y.CRANGE,/DATA
     wave1=wave2
     wh_clear_sav=wh_clear2
  endfor
  ;; graft on the last remaining piece
  if prepend then wave=[wave2,wave] else wave=[wave,wave2]
;  oplot,wave,replicate(ord+.25,n_elements(wave))
  
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
     ;;  the fraction in the first bin (independent of up-going vs. down-going)
     (*self.MERGE)[ord].frac=ptr_new((wave[map_loc+1]-merge_wave)/ $
                                     (wave[map_loc+1]-wave[map_loc]))
     (*self.MERGE)[ord].to=ptr_new(map_loc,/NO_COPY)
  endfor 
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
     account.cube_plane=account.cube_plane+mrec.offset
     if NOT ptr_valid(dr_acct) then begin 
        (*self.DR)[dr].ACCOUNT=ptr_new(account,/NO_COPY)
     endif else *dr_acct=[*dr_acct,account]
     return
  endif
  
  ;; Split planes in the merge vector among two planes.
  planes=*mrec.planes           ;order's account planes to be manipulated
  to_planes=*mrec.to            ; ... and sent to these cube planes
  fracs=*mrec.frac              ; ... with these fractions
  
  ;; First group the account records by the cube plane(s) they affect
  minp=min(planes,MAX=maxp)
  h=histogram(account.cube_plane,MIN=minp,MAX=maxp,REVERSE_INDICES=ri_cube)
  
  ;; Offset all to the correct cube plane (some we'll change soon)
  account.cube_plane=account.cube_plane+mrec.offset
  
  ;; which plane bins hold planes which must be split and interpolated?
  wh=where(h gt 0 and histogram(planes,REVERSE_INDICES=ri_order) gt 0,cnt) 
  for i=0,cnt-1 do begin 
     if ri_cube[wh[i]+1] eq ri_cube[wh[i]] then continue
     to_plane=to_planes[ri_order[ri_order[wh[i]]]]
     frac=fracs[ri_order[ri_order[wh[i]]]]
     
     ;; Split the elements on affected planes
     changers=ri_cube[ri_cube[wh[i]]:ri_cube[wh[i]+1]-1]
     split_acc=account[changers]
     
     ;; The first plane (gets frac worth)
     account[changers].area=account[changers].area*frac
     account[changers].CUBE_PLANE=to_plane
     ;; The next plane (gets 1-frac worth)
     split_acc.area=split_acc.area*(1.-frac)
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
;  BuildAccount - Build the accounting lists, listing, for each
;                 record, and for all pixels in the cube, all
;                 overlapping data pixels from that record, including
;                 the BCD index #, the pixel in that BCD which
;                 overlapped, and fraction which overlapped the
;                 corresponding cube pixel.
;=============================================================================
pro CubeProj::BuildAccount,_EXTRA=e
  self->Normalize
  self->MergeSetup
  self.CUBE_SIZE[2]=n_elements(*self.WAVELENGTH) 
  
  ;; XXX Stepsize is currently very wrong!
  ;;stepsz=float(self.STEP_SIZE/self.PLATE_SCALE) ; slit step size, in pixels
  
  ords=self->BuildOrders()
  nap=n_elements(*self.APERTURE) 
  RADEG = 180.0d/!DPI           ; preserve double
  ;;Feedback plots
  if self.feedback then begin 
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
     
     tvlct,[255b,0b,0b,0b],[0b,255b,0b,255b],[0b,0b,255b,255b],!D.TABLE_SIZE-5
     save_win=!D.WINDOW
     self->GetProperty,TLB_OFFSET=tboff,TLB_SIZE=tbsize
     device,GET_SCREEN_SIZE=ss
     window,XSIZE=xsize,YSIZE=ysize, $
            XPOS=tboff[0]+tbsize[0], YPOS=ss[1]-tboff[1]-ysize, $
            TITLE=string(FORMAT='(%"%s %s  (%s)  [%dx%d ; %5.1f\"x%5.1f\"]")',$
                         'Building Cube: ', $
                         self->ProjectName(), $
                         (self.MODULE?self.MODULE:"(no module)")+ $
                         (self.ORDER ne 0?' Order '+strtrim(self.ORDER,2): $
                          ' all orders'), $
                         self.NSTEP,self.STEP_SIZE*3600.0D*self.NSTEP)
     (*self.wInfo).feedback_window=!D.WINDOW
     plot,[0],/NODATA,xrange=[0,self.cube_size[0]], $
          POSITION=[15,15,xsize-15,ysize-4],/DEVICE, $
          yrange=[0,self.cube_size[1]],xstyle=1,ystyle=1,xticks=1,yticks=1
     ;; The grid
     for i=0,self.cube_size[0] do plots,i,!Y.CRANGE
     for i=0,self.cube_size[1] do plots,!X.CRANGE,i
     wait,0
  endif
  ;; End debugging plots
  
  ;; XXX Possible Time saver: If the cube size was changed, we might
  ;; need to shift existing accounts by one or more pixels ... only
  ;; relevant with POSITION-based (not GRID-based) cube layout.
  ;; E.g. making a cube with overlapping maps taken at different
  ;; epochs.
  
  ;; if self.ACCOUNTS_VALID eq 2b then begin 
  ;;    new=where(NOT ptr_valid((*self.DR).ACCOUNT)) ;newly added BCD's
  
  astr=self->CubeAstrometryRecord()
  exp_off=-1
  for i=0L,n_elements(*self.DR)-1 do begin 
     ;; skip disabled records unconditionally
     if (*self.DR)[i].DISABLED then continue 
     feedback_only=0            ;might just be showing the feedback
     if exp_off lt 0 then exp_off=(*self.DR)[i].EXP
     color=!D.TABLE_SIZE-5+((*self.DR)[i].EXP-exp_off) mod 4
     
     ;; if the accounts are fully valid and an account exists for this
     ;; DR, assume it's valid.
     if self.ACCOUNTS_VALID AND 1b AND ptr_valid((*self.DR)[i].ACCOUNT) $
        then begin 
        if self.feedback then feedback_only=1 else continue 
     endif else ptr_free,(*self.DR)[i].ACCOUNT
     
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
     
     ;; Building in another order?
     if (*self.DR)[i].TARGET_ORDER ne self.ORDER AND $
        self.ORDER ne 0 then begin
        self.cal->TransformCoords,self.MODULE,pos,(*self.DR)[i].PA_RQST, $
                                  ORDER1=(*self.DR)[i].TARGET_ORDER, $
                                  self.MODULE,ORDER2=self.ORDER,newpos
        pos=newpos
     endif

     ad2xy,pos[0],pos[1],astr,x,y
     offset=[x,y]+.5
     
     ;; (Probably small) difference between PA of this BCD and the
     ;; mean map PA
     delta_PA=(*self.DR)[i].PA_RQST-self.PA
     
     for ord=0,n_elements(ords)-1 do begin
        aper=nap eq 1?(*self.APERTURE)[0]:(*self.APERTURE)[ord]
        prs=self.cal->GetWAVSAMP(self.MODULE,ords[ord],APERTURE=aper, $
                                 /PIXEL_BASED, /SAVE_POLYGONS, $
                                 PR_WIDTH=self.PR_SIZE[1],_EXTRA=e)
        
        ;; Pre-allocate an account list 
        if ~feedback_only then begin 
           nacc=4*n_elements(prs) 
           account=replicate({CUBE_ACCOUNT_LIST},nacc)
           prmin=0L & prmax=n_elements(prs)-1
        endif else begin 
           prmin=1L & prmax=1L
        endelse 
        acc_ind=0L
        ;; iterate over all the adjacent PRs (different cube planes)
        for j=prmin,prmax do begin 
           ;; Setup the rotation matrix to rotate back to the +x
           ;; direction
           ;;
           ;; XXX pa_delta should remain, yes?  I.e. we should take
           ;; out the "false" rotation of optical distortion (slit
           ;; rotation), but *put in* the correct delta(PA) between
           ;; BCD and PA_0 as chosen for the cube.  Ensure this works
           ;; correctly, i.e. in the correct sense.
           
           ;; Another option: de-rotate the collection of clipped
           ;; polygons to take out optical slit rotation, setup a
           ;; slit-centered gnomic coordinate system, convert each
           ;; vertex of each polygon into ra,dec, and then project
           ;; this ra,dec onto the gnomic cube coordinate system.
           ;; Test for perceivable difference first?
           
           angle=-prs[j].angle+delta_PA
           if angle ne 0.0D then begin 
              ct=cos(angle/RADEG) & st=sin(angle/RADEG)
              rot=transpose([[ ct,-st], $
                             [ st, ct]])
           endif
           
           if self.feedback && j eq 1L && self->Showing() then begin 
              widget_control, (*self.wInfo).SList, SET_LIST_SELECT=i
           endif
              
           ;; Iterate over partial pixels clipped by the PR
           for k=0L,n_elements(*prs[j].POLYGONS)-1 do begin 
              bcdpixel=(*prs[j].PIXELS)[k]
              ;; associated polygon (2xn list) this pixel got clipped to
              ;; by the PR on the detector grid
              poly=*(*prs[j].POLYGONS)[k]
              ;; Offset to canonical slit center 
              poly=poly-rebin(prs[j].cen,size(poly,/DIMENSIONS))
              ;; Rotate this polygon to the cube sky grid, if necessary
              if angle ne 0.0 then poly=rot#poly ;XXX check!!!
              ;; Offset the polygon correctly into the sky grid
              poly=poly+rebin(offset,size(poly,/DIMENSIONS))
              if self.feedback AND j eq 1L then begin
                 plots,[reform(poly[0,*]),poly[0,0]], $
                       [reform(poly[1,*]),poly[1,0]], $
                       COLOR=color
                 plots,offset,PSYM=4,COLOR=color
                 wait,0
              endif
              
              if feedback_only then continue
              ;; Clip the offset polygon against the sky (cube) grid
              cube_spatial_pix=polyfillaa(reform(poly[0,*]),reform(poly[1,*]),$
                                          self.CUBE_SIZE[0],self.CUBE_SIZE[1],$
                                          AREAS=areas)
              if cube_spatial_pix[0] eq -1 then begin 
                 ; print,FORMAT='("Not hitting cube for pixel: "' + $
;                        ',I0,",",I0," -- step [",I0,",",I0,"]")', $
;                        bcdpixel mod 128, bcdpixel/128, $
;                        (*self.DR)[i].COLUMN,(*self.DR)[i].ROW
;                  print, poly
;                  print,'  original:'
;                 print,*(*prs[j].POLYGONS)[k]
                 continue ;; why isn't our cube big enough?
             endif
              
              ncp=n_elements(cube_spatial_pix)
              ;; Add space to account list, large chunks at a time
              if acc_ind+ncp ge nacc then begin 
                 account=[account,replicate({CUBE_ACCOUNT_LIST},nacc)]
                 nacc*=2
              endif
              account[acc_ind:acc_ind+ncp-1].cube_pix=cube_spatial_pix
              account[acc_ind:acc_ind+ncp-1].cube_plane=j ;just this order's
              account[acc_ind:acc_ind+ncp-1].bcd_pix=bcdpixel
              account[acc_ind:acc_ind+ncp-1].area=areas
              acc_ind=acc_ind+ncp
           endfor
                 
        endfor
        if feedback_only then continue
        account=account[0:acc_ind-1] ; trim this order's account to size
        
        ;; Merge this account into the full cube account
        self->MergeAccount,i,ord,account
     endfor
  endfor
  self->BuildRevAcct            ;we need these too...
  if self.feedback then wset,save_win
  self.ACCOUNTS_VALID OR= 1b
end

;=============================================================================
;  BuildRevAcct - Build the reverse accounts from the regular accounts.
;=============================================================================
pro CubeProj::BuildRevAcct
  for i=0L,n_elements(*self.DR)-1 do begin
     if self.ACCOUNTS_VALID AND 1b AND ptr_valid((*self.DR)[i].REV_ACCOUNT) $
        then continue else ptr_free,(*self.DR)[i].REV_ACCOUNT
     if (*self.DR)[i].DISABLED then continue
     
     ;; Compute the total reverse index of the full cube index
     ;; histogram for this DR's accounting.  E.g. the account records
     ;; from this BCD pertaining to cube pixel z are:
     ;; ri[ri[z-rev_min]:ri[z+1-rev_min]-1].
     h=histogram((*(*self.DR)[i].ACCOUNT).cube_pix+ $
                 (*(*self.DR)[i].ACCOUNT).cube_plane* $
                 self.CUBE_SIZE[0]*self.CUBE_SIZE[1], $
                 OMIN=om,REVERSE_INDICES=ri)
     (*self.DR)[i].REV_ACCOUNT=ptr_new(ri,/NO_COPY)
     (*self.DR)[i].REV_CNT=n_elements(h) 
     (*self.DR)[i].REV_MIN=om
  endfor
  self->UpdateButtons
end

;=============================================================================
;  ResetAccounts - Remove all accounting info, so a cube build will
;                  proceed anew.
;=============================================================================
pro CubeProj::ResetAccounts,NO_UPDATE=no
  self.ACCOUNTS_VALID=0b
  if ~keyword_set(no) then self->UpdateButtons
end

;=============================================================================
;  Normalize - Map header info in the BCD's to cube-specific data, and
;              returns the status of the normalization.
;=============================================================================
pro CubeProj::Normalize
  self->LoadCalib
  
  if NOT ptr_valid(self.DR) then $
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
  
  stepsper=(stepspar=lonarr(good_cnt))
  stepszper=(stepszpar=dblarr(good_cnt))
  for i=0,good_cnt-1 do begin 
     stepsper[i]=sxpar(*(*self.DR)[enabled[i]].HEADER,'STEPSPER')
     stepspar[i]=sxpar(*(*self.DR)[enabled[i]].HEADER,'STEPSPAR')
     stepszpar[i]=sxpar(*(*self.DR)[enabled[i]].HEADER,'SIZEPAR')
     stepszper[i]=sxpar(*(*self.DR)[enabled[i]].HEADER,'SIZEPER')
  endfor 
  
  ;; XXX No longer necessary with pos-based layout: can have arbitrary steps
  if (NOT array_equal(stepsper,stepsper[0])) or $
     (NOT array_equal(stepspar,stepspar[0])) then $
     self->Error,"BCD's have unequal map size"
  self.NSTEP=[stepsper[0],stepspar[0]]
  if (NOT array_equal(stepszper,stepszper[0])) or $
     (NOT array_equal(stepszpar,stepszpar[0])) then $
     self->Error,"BCD's have unequal step size"
  self.STEP_SIZE=[stepszpar[0],stepszper[0]]/3600.D ; in degrees
  
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
  
  ;; Check to ensure all steps are present and accounted for
  self->CheckSteps
  
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
  
  ;; Normalize the build aperture(s)
  self->NormalizeApertures
  
  ;; And the cube size and center
  self->LayoutBCDs
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
;                            from +Y to N
;                          SIRTF PA's measure CCW from N to SIRTF +Z
;                            (or slit +w)
;                          +w and +x correspond, EXCEPT FOR LH, where +w=-x
;=============================================================================
function CubeProj::CubeAstrometryRecord,ZERO_OFFSET=zo
  RADEG = 180.0d/!DPI           ; preserve double
  angle=(270.0D - self.PA)/RADEG
  ;;if self.module eq 'LH' then angle+=180./RADEG
  c=cos(angle) & s=sin(angle)
  cd=self.PLATE_SCALE*[[-c,-s],[-s,c]]
  if keyword_set(zo) then crpix=[0.5,0.5] else $
     crpix=self.CUBE_SIZE[0:1]/2.+.5 ;[1,1] => pixel center FITS silliness
  
  make_astr,astr,CD=cd,DELTA=[1.D,1.D],CRPIX=crpix,CRVAL=self.POSITION, $
            CTYPE=['RA---TAN','DEC--TAN']
  return,astr
end

;=============================================================================
;  LayoutBCDs - Define the cube astrometry and determine the BCD
;               layout on the sky grid based on positions and PA's.
;=============================================================================
pro CubeProj::LayoutBCDs
  self->LoadCalib
  
  ;; Find the PA of the most populated AOR
  aorids=(*self.DR).AORKEY
  uniqids=aorids[uniq(aorids,sort(aorids))]
  cnt=0
  RADEG = 180.0d/!DPI           ; preserve double
  
  for i=0,n_elements(uniqids)-1 do begin 
     wh=where(aorids eq uniqids[i],thiscnt)
     if thiscnt gt cnt then begin
        cnt=thiscnt
        use=wh
     endif
  endfor
  self.PA=mean((*self.DR)[use].PA_RQST) ;match our PA to the most numerous
  ;; Approximate cube center (pos average), for now (not critical)
  self.POSITION=total((*self.DR).RQST_POS,2)/self->N_Records()
  cubeastr=self->CubeAstrometryRecord(/ZERO_OFFSET)
  
  ords=self->BuildOrders()
  nap=n_elements(*self.APERTURE)
  
  ;; Construct the bounding aperture for all orders being combined into cube
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
  
;  skyc=dblarr(2,n_elements(*self.DR))
;  prsAD=(prs=dblarr(8,n_elements(*self.DR)))
  
  ;; Rotate and offset final bounding polygon for each DR.
  pr_half=self.PR_SIZE[1]/2 ;pr width
  pr_rect=[[final_off[0],-pr_half], $
           [final_off[0], pr_half], $
           [final_off[1], pr_half], $
           [final_off[1],-pr_half]]-.5 ;nasalib standard: 0,0: center of pix

  for i=0,n_elements(*self.DR)-1 do begin 
     if (*self.DR)[i].DISABLED then continue
     pa_rqst=(*self.DR)[i].PA_RQST
     c_pa=cos((270.0D - pa_rqst)/RADEG) ;WCS uses CROTA = N CCW from +y 
     s_pa=sin((270.0D - pa_rqst)/RADEG)
     cd_pa=self.PLATE_SCALE*[[-c_pa,-s_pa],[-s_pa,c_pa]]
     
     ;; Compute the RA/DEC of the 4 corners.
     make_astr,astr,CD=cd_pa,DELTA=[1.D,1.D],CRPIX=[0.5,0.5], $
               CRVAL=(*self.DR)[i].RQST_POS,CTYPE=['RA---TAN','DEC--TAN']

     xy2ad,pr_rect[0,*],pr_rect[1,*],astr,a_rect,d_rect
;     prs[*,i]=[a_rect,d_rect]
     
     ;; Offset to the correct order: account for building a cube using
     ;; data targeted at another order.
     if (*self.DR)[i].TARGET_ORDER ne self.ORDER AND $
        self.ORDER ne 0 then begin
        self.cal->TransformCoords,self.MODULE,[1.#a_rect,1.#d_rect],pa_rqst, $
                                  ORDER1=(*self.DR)[i].TARGET_ORDER, $
                                  self.MODULE,ORDER2=self.ORDER,newcoords,newpa
        a_rect=newcoords[0,*] & d_rect=newcoords[1,*]
     endif 
        
     ;; Compute corner celestial positions in cube frame
     ad2xy,a_rect,d_rect,cubeastr,x,y ;x,y in 0,0 pixel-centered
     x+=0.5 & y+=0.5            ;back to normal pixel convention (.5-centered)

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

  new_size=ceil(exact_size-.001) ;no hangers-on
  if NOT array_equal(self.CUBE_SIZE[0:1],new_size) then begin 
     self.CUBE_SIZE[0:1]=new_size
     self.ACCOUNTS_VALID OR= 2b
  endif

  ;; Find the cube center in the sky coordinate system
  xy2ad,x_min+float(new_size[0])/2.-.5,y_min+float(new_size[1])/2.-.5, $
        cubeastr,a,d
  self.POSITION=[a,d]
    
;  skyc=3600.D*(skyc-rebin(total(skyc,2)/(n_elements(skyc)/2), $
;                          size(skyc,/DIMENSIONS)))
;  plot,skyc[0,*],skyc[1,*],PSYM=4
    

;  cen_xy=pos_min+float(new_size)/2.*self.PLATE_SCALE
;  self.POSITION=rot_pa ## cen_xy ;rot_pa^-1=rot_pa
  
  ;; Debug stuff!!!
;   pos_max=[x_max,y_max] & pos_min=[x_min,y_min]  
;   ;;pos_max=[max(prs[indgen(4)*2,*],MIN=minx),max(prs[indgen(4)*2+1,*],MIN=miny)]
;   ;;pos_min=[minx,miny]
;   del=(pos_max-pos_min)
;   plot,[0],/NODATA, /XSTYLE, /YSTYLE, $
;        XRANGE=[pos_min[0]-.1*del[0],pos_max[0]+.1*del[0]], $
;        YRANGE=[pos_min[1]-.1*del[1],pos_max[1]+.1*del[1]]
  
;   for i=0,new_size[0] do $
;      plots,pos_min[0]+i, $
;            [pos_min[1],pos_min[1]+new_size[1]], $
;            COLOR=!D.TABLE_SIZE/2
  
;   for i=0,new_size[1] do $
;      plots,[pos_min[0],pos_min[0]+new_size[0]], $
;            pos_min[1]+i, $
;            COLOR=!D.TABLE_SIZE/2
  
;   for i=0,n_elements(prs)/8-1 do begin 
;      rect=prs[*,i]
;      x=rect[indgen(4)*2] & y=rect[indgen(4)*2+1]
;      plots,[x,x[0]],[y,y[0]],PSYM=-4
;      plots,skyc[0,i],skyc[1,i],PSYM=7
;   endfor 
  
;  plots,cen_xy[0],cen_xy[1],THICK=2,PSYM=6,SYMSIZE=2
  ;; XXX Apply FOV offset (often zero when targetting slit center)
;  wait,5
end


;=============================================================================
;  BuildCube - Assemble the Cube from the accounting information, the
;              BCD data, the BMASK, and the uncertainties (along with
;              fluxing information if appropriate)
;=============================================================================
pro CubeProj::BuildCube
  if NOT ptr_valid(self.DR) then return
  self->RestoreAll              ;get all of the data, if necessary
  if ~(self.ACCOUNTS_VALID AND 1b) OR $
     ~array_equal(ptr_valid((*self.DR).ACCOUNT),1b) OR $
     self.feedback then self->BuildAccount else self->BuildRevAcct
  
  cube=make_array(self.CUBE_SIZE,/FLOAT,VALUE=!VALUES.F_NAN)
  areas=make_array(self.CUBE_SIZE,/FLOAT,VALUE=0.0)
  
  ;; Bad pixels
  if ptr_valid(self.BAD_PIXEL_LIST) then begin 
     bpmask=make_array(size(*(*self.DR)[0].BCD,/DIMENSIONS),VALUE=1b)
     bpmask[*self.BAD_PIXEL_LIST]=0b
     use_bpmask=1
  endif else use_bpmask=0
  
  if self.feedback then begin 
     if ptr_valid(self.wInfo) then wset,(*self.wInfo).feedback_window
     csz=self.cube_size[0]*self.cube_size[1]
     ctarg=self.cube_size[2]/2
  endif 
  
  if self.fluxcon then fluxcon=self->FluxConImage()
  
  for dr=0,n_elements(*self.DR)-1 do begin 
     if (*self.DR)[dr].DISABLED then continue
     acct=*(*self.DR)[dr].ACCOUNT
     rev_acc=*(*self.DR)[dr].REV_ACCOUNT
     rev_min=(*self.DR)[dr].REV_MIN
     bcd=*(*self.DR)[dr].BCD
     if ptr_valid(self.BACKGROUND) then bcd-=*self.BACKGROUND
     if self.fluxcon then bcd/=fluxcon
     use_unc=ptr_valid((*self.DR)[dr].UNC)
     if use_unc then begin 
        unc=*(*self.DR)[dr].UNC
        if self.fluxcon then unc/=fluxcon ;XXX should include background error 
     endif
     
     ;; Exclude BCD pix with any of BMASK bits 8,10,12,13,& 14 set from
     ;; entering the cube
     if ptr_valid((*self.DR)[dr].BMASK) then begin 
        use_bmask=1
        bmask=(*(*self.DR)[dr].BMASK AND 29952UL) eq 0L 
        if use_bpmask then bmask AND= bpmask
     endif else if use_bpmask then begin 
        use_bmask=1
        bmask=bpmask
     endif else use_bmask=0
        
     ;; Use the reverse account to populate the cube -- slow
     for i=0L,(*self.DR)[dr].REV_CNT-1 do begin 
        if rev_acc[i] eq rev_acc[i+1] then continue ;nothing for this pixel
        these_accts=acct[rev_acc[rev_acc[i]:rev_acc[i+1]-1]]
        ;; XXX Error weighting, BMASK, other alternatives
        ;;  need all in one place? ... e.g trimmed mean?
        ;if array_equal(finite(bcd[these_accts.BCD_PIX]),0b) then $
        ;   print,rev_acc[rev_acc[i]:rev_acc[i+1]-1],these_accts.BCD_PIX, $
                                ;         bcd[these_accts.BCD_PIX]
        
        ;; Default pixel value is non-finite (NaN)
        pix=rev_min+i
        if use_bmask then begin 
           cube[pix]=(finite(cube[pix])?cube[pix]:0.0) + $
                     total(bcd[these_accts.BCD_PIX] * $
                           these_accts.AREA * $
                           bmask[these_accts.BCD_PIX],/NAN)
           areas[pix]+=total(these_accts.AREA * $
                             bmask[these_accts.BCD_PIX] * $
                             finite(bcd[these_accts.BCD_PIX]))
        endif else begin
           cube[pix]=(finite(cube[pix])?cube[pix]:0.0) + $
                     total(bcd[these_accts.BCD_PIX] * $
                           these_accts.AREA,/NAN)
           areas[pix]+=total(these_accts.AREA * $
                             finite(bcd[these_accts.BCD_PIX]))
        endelse 
        if self.feedback then begin ;highlight completed pixels
           if pix/csz eq ctarg then begin 
              pix=pix mod csz
              x=pix mod self.cube_size[0]
              y=pix/self.cube_size[0]
              plots,[x,x,x+1,x+1,x],[y,y+1,y+1,y,y],THICK=2
           endif 
        endif 
     endfor
  endfor
  
  areas=areas>1.e-10            ;avoid divide by zero errors
  ptr_free,self.CUBE,self.CUBE_UNC
  self.CUBE=ptr_new(cube/areas)
  self.CUBE_DATE=systime(/JULIAN)
  self.Changed=1
  @cubism_version 
  self.version=cubism_version
  self->UpdateButtons & self->UpdateList & self->UpdateTitle
end


;=============================================================================
;  Stack - Build a stacked image map from the constructed cube between
;          any number of sets of wavelength intervals, or optionally
;          weighting the individual planes with WEIGHTS.
;
;             FORERANGES: A 2xn list of foreground ranges over which
;                         to stack.
;
;             BACKRANGES: A 2xn list of background wavelength ranges
;                         over which to stack.
;
;             BG_VALS: A vector of continuum values underlying the
;                      foreground ranges.
;
;             WEIGHTS: A 2xn weight vector list: lambda vs. weight
;                      [0-1] for weighting the foreground.  Will be
;                      interpolated onto the actual wavelength
;                      samples.  Overrides foreranges
;
;             MNAME: A named map set to pull from the archive.  Ranges
;                    and weights from the named map will override any
;                    passed otherwise.
;
;     XXXX: Switch from position to wavelength based ????        
;=============================================================================
function CubeProj::Stack,foreranges,BACKRANGES=backranges,WEIGHTS=weights, $
                         BG_VALS=bg_vals,MAP_NAME=mname, SAVE=save
  if NOT ptr_valid(self.CUBE) then self->Error,'No cube to stack'
  if n_elements(mname) ne 0 then begin 
     oMap=IRSMapSet()
     oMap->GetMap,mname,WEIGHTS=weights,FORERANGES=foreranges, $
                  BACKRANGES=backranges, WAVELENGTH_CONVERT=*self.WAVELENGTH
  endif
  
  sf=size(foreranges,/DIMENSIONS)
  if n_elements(sf) eq 2 then nfr=sf[1] else nfr=1
  
  nbr=0
  if n_elements(backranges) ne 0 then begin 
     if backranges[0] ne -1 then begin 
        sb=size(backranges,/DIMENSIONS)
        if n_elements(sb) eq 2 then nbr=sb[1] else nbr=1
     endif 
  endif
  
  nbgv=n_elements(bg_vals)
  
  nw=n_elements(weights)
  use_weights=0
  if nw gt 0 then if weights[0] ne -1 then use_weights=1
  if use_weights then begin     ;A weight vector overrides foreground regions
     stack=total(*self.CUBE * $
                 rebin(reform(weights,[1,1,nw]),[self.CUBE_SIZE[0:1],nw],$
                       /SAMPLE),3,/NAN)/total(weights)
  endif else begin              ;Foreground regions
     stack=fltarr(self.CUBE_SIZE[0:1])
     fcnt=lonarr(self.CUBE_SIZE[0:1])
     bcnt=0
     for i=0,nfr-1 do begin 
        if foreranges[0,i] eq foreranges[1,i] then begin 
           stack+=(*self.CUBE)[*,*,foreranges[0,i]]
           fcnt+=long(finite((*self.CUBE)[*,*,foreranges[0,i]]))
        endif else begin 
           stack+= $
              total((*self.CUBE)[*,*,foreranges[0,i]:foreranges[1,i]],/NAN,3)
           fcnt+=long(total(finite((*self.CUBE)[*,*,foreranges[0,i]: $
                                                foreranges[1,i]]),3))
        endelse 
        bcnt+=foreranges[1,i]-foreranges[0,i]+1
     endfor
     if nfr gt 0 then stack/=(fcnt>1L)
  endelse 
  
  if nbr eq 0 AND nbgv eq 0 then return,stack
  
  ;; Background Values provided
  if nbgv gt 0 then begin 
     if nbgv ne bcnt then $
        self->Error,'Wrong number of background values passed'
     stack-=total(bg_vals)/bcnt ; average(fi-bgvi)
     return,stack
  endif
  
  ;; Compute a background
  bcnt=lonarr(self.CUBE_SIZE[0:1])
  background=fltarr(self.CUBE_SIZE[0:1])
  for i=0,nbr-1 do begin 
     if backranges[0,i] eq backranges[1,i] then begin 
        background+=(*self.CUBE)[*,*,backranges[0,i]]
        bcnt+=long(finite((*self.CUBE)[*,*,backranges[0,i]]))
     endif else begin 
        background+=total((*self.CUBE)[*,*,backranges[0,i]:backranges[1,i]], $
                          /NAN,3)
        bcnt+=long(total(finite((*self.CUBE)[*,*,backranges[0,i]: $
                                             backranges[1,i]]),3))
     endelse 
  endfor 
  background/=(bcnt>1L)
  stack-=background
	  
  if keyword_set(save) then self->SaveMap,stack,save
  return,stack
end

;=============================================================================
;  BackTrackPix - Find the BCDs, pixels, and overlap fractions
;                 influencing the specified full cube pixel.  If
;                 FOLLOW is set, highlight the indicated BCDs too.
;=============================================================================
function CubeProj::BackTrackPix, pix, plane,FOLLOW=follow
  nrec=self->N_Records()
  if nrec eq 0 then return,-1
  if ~(self.ACCOUNTS_VALID AND 1b) OR $
     ~array_equal(ptr_valid((*self.DR).ACCOUNT),1) then $
        self->Error,"Must rebuild cube to backtrack"
  
  ;; At least one reverse account required
  if array_equal(ptr_valid((*self.DR).REV_ACCOUNT),0b) then self->BuildRevAcct
  if n_elements(pix) eq 2 then pix=pix[0]+pix[1]*self.CUBE_SIZE[0]
  if n_elements(plane) ne 0 then $
     z=pix+plane*self.CUBE_SIZE[0]*self.CUBE_SIZE[1] $
  else z=pix
  show=keyword_set(follow) && self->Showing()
  if show then show_vec=bytarr(nrec)
  for i=0,nrec-1 do begin 
     if ~ptr_valid((*self.DR)[i].REV_ACCOUNT) then continue
     if z lt (*self.DR)[i].REV_MIN then continue
     thisz=z-(*self.DR)[i].REV_MIN
     ri=*(*self.DR)[i].REV_ACCOUNT
     if ri[thisz] ge ri[thisz+1] then continue
     if show then show_vec[i]=1b
     accs=(*(*self.DR)[i].ACCOUNT)[ri[ri[thisz]:ri[thisz+1]-1]]
     ret=replicate({DR:i,ID:(*self.DR)[i].ID,BCD_PIX:0,BCD_VAL:0.0, $
                    BACK_VAL:0.0,AREA:0.0},n_elements(accs))
     ret.BCD_PIX=accs.BCD_PIX 
     ret.BCD_VAL=(*(*self.DR)[i].BCD)[accs.BCD_PIX]
     if ptr_valid(self.BACKGROUND) then $
        ret.BACK_VAL=(*self.BACKGROUND)[accs.BCD_PIX]
     ret.AREA=accs.AREA
     if n_elements(all) eq 0 then all=[ret] else all=[all,ret]
  endfor 
  if show then begin 
     widget_control, (*self.wInfo).SList, SET_LIST_SELECT=where(show_vec)
     self->UpdateButtons
  endif 
  return,n_elements(all) ne 0?all:-1
end

;=============================================================================
;  SaveMap - Save a Stacked Map to FITS 
;=============================================================================
pro CubeProj::SaveMap,map,sf
  if size(sf,/TYPE) ne 7 then begin 
     xf,sf,/SAVEFILE, /RECENT, $
        FILTERLIST=['*.fits', '*.*', '*'], $
        TITLE='Save Map As FITS File...', $
        /NO_SHOW_ALL, SELECT=0, PARENT_GROUP=self->TopBase(), $
        START=strlowcase(strjoin(strsplit(self->ProjectName(),/EXTRACT),'_'))+$
        "_map.fits",/MODAL
     if size(sf,/TYPE) ne 7 then return
  endif
  
  catch, err
  if err ne 0 then begin 
     self->Error,['Error saving map to file '+sf,!ERROR_STATE.MSG]
  endif 
  widget_control,/HOURGLASS  

  fxhmake,hdr,/date
  putast,hdr,self->CubeAstrometryRecord()
  
  ;; Description
  sxaddhist, ['The SIRTF Nearby Galaxy Survey (SINGS) Legacy Project', $
              'This file contains a 2D map created from an IRS', $
              'spectral cube, assembled from a step & stare spectral', $
              'mapping dataset.', $
              'For more information on SINGS see http://sings.stsci.edu'], $
             hdr,/COMMENT
  fxaddpar,hdr,'FILENAME',filestrip(sf),' Name of this file'
  fxaddpar,hdr,'APERNAME',self.MODULE,' The IRS module'
  fxaddpar,hdr,'CUBE_VER',self.version,' CUBISM version used'
  self->LoadCalib
  name=self.cal->Name()
  if name eq '' then name=self.cal_file
  fxaddpar,hdr,'CAL_SET',name,' IRS Calibration set used'
  writefits,sf,map,hdr
end


;=============================================================================
;  Extract - Extract a Spectrum from the Cube, and possibly save it
;             EXP - If set, export extracted spectrum to command line
;             SF - File to save to (passed to SaveSpectrum)
;             ASCII - Pass to SaveSpectrum
;             P - Polygon (2xn) list of pixel or celestial coordinates
;                 to extract.  Will be converted to pixel coords if
;                 celestial. Partial pixel extraction occurs.
;=============================================================================
function CubeProj::Extract,low,high, EXPORT=exp, SAVE=sf, ASCII=ascii, $
                           POLY=p,OUTPUT_POLY=op,CELESTIAL=celestial, $
                           APERTURE_FROM_FILE=aff
  if NOT ptr_valid(self.CUBE) then self->Error,'No cube to extract'
  
  if keyword_set(aff) then begin 
     if size(aff,/TYPE) ne 7 then begin 
        xf,aff,/RECENT, $
           FILTERLIST=['*.txt', '*.*', '*'], $
        TITLE='Save Spectrum As '+(fits?'FITS':'Text')+' File...', $
           /NO_SHOW_ALL, SELECT=0, PARENT_GROUP=self->TopBase(),/MODAL
     endif 
     if size(aff,/TYPE) ne 7 then return,-1
     p=self->ReadSpecAperture(aff)
     if n_elements(p) eq 1 then self->Error,['No aperture found for file:',sf]
     celestial=1
  endif 
  
  if keyword_set(celestial) then begin 
     ad2xy,p[0,*],p[1,*],self->CubeAstrometryRecord(),x,y
     x+=.5 & y+=.5              ;restore *correct* pixel indexing
     op=[tranpose(x),transpose(y)]
     overlap_pix=polyfillaa(x,y,self.CUBE_SIZE[0],self.CUBE_SIZE[1], $
                            AREAS=areas)
     if overlap_pix[0] eq -1 then $
        self->Error,'Selected aperture does not overlap cube' $
     else if min(x,MAX=mxx) lt 0 or mxx gt self.CUBESIZE[0]-1 or $
        min(y,MAX=mxy) lt 0 or mxy gt self.CUBESIZE[1]-1 then $
           self->Warn,'Aperture lies partially outside of cube.'
  endif 
  
  
     
  sp=total(total((*self.CUBE)[low[0]:high[0],low[1]:high[1],*],1,/NAN), $
           1,/NAN)/(high[1]-low[1]+1.)/(high[0]-low[0]+1.)
  
  
  
  
  if keyword_set(exp) then self->ExportToMain, $
     SPECTRUM=transpose([[*self.WAVELENGTH],[sp]]) else $
        if keyword_set(sf) then self->SaveSpectrum,sp,sf,ASCII=ascii, $
                                                   COORDS=[[low],[high]]
  return,sp
end

;=============================================================================
;  SaveSpectrum - Save a Spectrum to FITS (the default) or ASCII
;=============================================================================
pro CubeProj::SaveSpectrum,sp,sf,ASCII=ascii,COORDS=coords
  fits=NOT keyword_set(ascii)
  if size(sf,/TYPE) ne 7 then begin 
     xf,sf,/SAVEFILE, /RECENT, $
        FILTERLIST=[(fits?'*.fits':'*.txt'), '*.*', '*'], $
        TITLE='Save Spectrum As '+(fits?'FITS':'Text')+' File...', $
        /NO_SHOW_ALL, SELECT=0, PARENT_GROUP=self->TopBase(), $
        START=strlowcase(strjoin(strsplit(self->ProjectName(),/EXTRACT),'_'))+$
        "_sp"+(fits?".fits":".txt"),/MODAL
     if size(sf,/TYPE) ne 7 then return
  endif
  
  catch, err
  if err ne 0 then begin 
     self->Error,['Error saving spectrum to file '+sf,!ERROR_STATE.MSG]
  endif 
  widget_control,/HOURGLASS  
  
  ;; Aperture Extraction coordinates
  delta=coords[*,1]-coords[*,0]+1
  ll=coords[*,0]-.5
  lr=[coords[0,1],coords[1,0]]+[.5,-.5]
  ur=coords[*,1]+.5
  ul=[coords[0,0],coords[1,1]]+[-.5,.5]
  all=[ [ll], [lr], [ur], [ul] ]
  xy2ad,all[0,*],all[1,*],self->CubeAstrometryRecord(),ra,dec
  
  if fits then begin 
     fxhmake,hdr,/date,/EXTEND
     ;; Description
     sxaddhist, ['The SIRTF Nearby Galaxy Survey (SINGS) Legacy Project', $
                 'This file contains a 1D spectrum extracted from an IRS', $
                 'spectral cube, assembled from a step & stare spectral', $
                 'mapping dataset.', $
                 'For more information on SINGS see http://sings.stsci.edu'], $
                hdr,/COMMENT
     fxaddpar,hdr,'FILENAME',filestrip(sf),' Name of this file'
     fxaddpar,hdr,'APERNAME',self.MODULE,' The IRS module'
     fxaddpar,hdr,'CUBE_VER',self.version,' CUBISM version used'
     fxaddpar,hdr,'RA_LL',ra[0],' RA of LL of extraction box'
     fxaddpar,hdr,'DEC_LL',dec[0],' DEC of LL of extraction box'
     fxaddpar,hdr,'RA_LR',ra[1],' RA of LR of extraction box'
     fxaddpar,hdr,'DEC_LR',dec[1],' DEC of LR of extraction box'
     fxaddpar,hdr,'RA_UR',ra[2],' RA of UR of extraction box'
     fxaddpar,hdr,'DEC_UR',dec[2],' DEC of UR of extraction box'
     fxaddpar,hdr,'RA_UL',ra[3],' RA of UL of extraction box'
     fxaddpar,hdr,'DEC_UL',dec[3],' DEC of UL of extraction box'
     self->LoadCalib
     name=self.cal->Name()
     if name eq '' then name=self.cal_file
     fxaddpar,hdr,'CAL_SET',name,' IRS Calibration set used'
     fxwrite,sf,hdr
     
     ;; Make the binary table
     fxbhmake,hdr,1,'CUBESPEC','CUBISM cube-extracted spectrum'
     fxbaddcol,wcol,hdr,*self.WAVELENGTH,'WAVELENGTH','Column label field 1', $
               TUNIT='Microns'
     fxbaddcol,fcol,hdr,*self.WAVELENGTH,'FLUX','Column label field 2', $
               TUNIT=self.fluxcon?'Jy':'e/s'
     fxbcreate,unit,sf,hdr
     fxbwrite,unit,*self.WAVELENGTH,wcol,1
     fxbwrite,unit,sp,fcol,1
     fxbfinish,unit
  endif else begin 
     openw,un,/get_lun,sf
     printf,un,'# Written '+systime(0)
     printf,un,'# Cube: '+self->ProjectName()
     if n_elements(coords) gt 0 then begin 
        printf,un,FORMAT='(%"# Extracted %dx%d [%d,%d] -> [%d,%d]")',delta, $
               coords
        printf,un,FORMAT= '("# Box: ",2(A,",",A,:," ; "),"]")', $
               radecstring(ra[0],/RA,PRECISION=3), $
               radecstring(dec[0],PRECISION=3), $
               radecstring(ra[1],/RA,PRECISION=3), $
               radecstring(dec[1],PRECISION=3)
        printf,un,FORMAT='("#      ",2(A,",",A,:," ; "),"]")', $
               radecstring(ra[2],/RA,PRECISION=3), $
               radecstring(dec[2],PRECISION=3), $
               radecstring(ra[3],/RA,PRECISION=3), $
               radecstring(dec[3],PRECISION=3)
        printf,un,'#      LAM (um)          FLUX (e/s/pixel)'
     endif 
     printf,un,FORMAT='(2G18.10)',transpose([[*self.WAVELENGTH],[sp]])
     free_lun,un
  endelse 
end


;=============================================================================
;  ReadSpecAperture - Read an aperture from a spectrum (ASCII or FITS)
;                     on file
;=============================================================================
function CubeProj::ReadSpecAperture,file
  aper=-1
  if stregex(file,'\.fits$',/BOOLEAN) then begin ;FITS file
     h=headfits(file,ERRMSG=err)
     return,-1
     aper=[ [sxpar(h,RA_LL),sxpar(h,DEC_LL)], $
            [sxpar(h,RA_LR),sxpar(h,DEC_LR)], $
            [sxpar(h,RA_UR),sxpar(h,DEC_UR)], $
            [sxpar(h,RA_UL),sxpar(h,DEC_UL)] ]
  endif else if stregex(file,'\.txt$',/BOOLEAN) then begin ; text
     line=''
     match='([+-]?[0-9]{2}:[0-9]{2}:[0-9.]+)'
     openr,un,file,/get_lun
     while not eof(un) do begin
        readf, un, line
        if strmid(line,0,1) ne '#' then break
        if strmid(line,2,4) eq 'Box:' then begin 
           ext=stregex(line,match+','+match+' ; '+match+','+match,/EXTRACT, $
                       /SUBEXPR)
           if n_elements(ext) lt 5 then return,-1
           ra=[ext[1],ext[3]]
           dec=[ext[2],ext[4]]
           readf, un, line     ;get next line
           ext=stregex(line,match+','+match+' ; '+match+','+match,/EXTRACT, $
                       /SUBEXPR)
           if n_elements(ext) lt 5 then return,-1
           ra=[ra,ext[1],ext[3]]
           dec=[dec,ext[2],ext[4]]
           aper=make_array(2,n_elements(ra),/DOUBLE)
           for i=0,n_elements(ra)-1 do begin 
              aper[0,i]=ten(strsplit(ra[i],":",/EXTRACT))*15.0D
              aper[1,i]=ten(strsplit(dec[i],":",/EXTRACT))
           endfor 
           break
        endif 
     endwhile 
     free_lun,un
  endif  
  return,aper
end

;=============================================================================
;  CheckSteps - Ensure that all of the BCD's for this map are present.
;=============================================================================
pro CubeProj::CheckSteps
  ;; XXX Account for multiple sub-slit maps or multiple map cycles?
  if NOT ptr_valid(self.DR) then self->Error,'No BCD Data loaded.'
  got=bytarr(self.NSTEP)
  got[(*self.DR).ROW-1,(*self.DR).COLUMN-1]=1b
  wh=where(got eq 0b, cnt)
  if cnt eq 0 then return
  self->Error,['Missing Steps: ',$
               string(FORMAT='('+strtrim(cnt,2)+'("[",I0,", ",I0,"]"))', $
                      [1#(wh mod self.NSTEP[0]+1),1#(wh/self.NSTEP[0]+1)])]
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
  
  if array_equal(orders,0) && (self.module eq 'LL' || self.module eq 'SL') $
  then begin 
     self.ORDER=(self.cal->Orders(self.module))[0]
  endif else if array_equal(orders,orders[0]) then $
     self.ORDER=orders[0] $
  else begin 
     h=histogram(BINSIZE=1,orders,OMIN=om)
     mx=max(h,mpos)
     self.ORDER=om+mpos
  endelse 
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
;  AddAOR - Add an entire AOR from a full AOR directory, with choice
;           of module(s) and observations.
;=============================================================================
pro CubeProj::AddAOR,AOR=aor,DIR=dir,COADD=cd,DROOPRES=dr,FLATAP=fl
  if n_elements(dir) eq 0 then begin 
     type=keyword_set(cd)?"Coadd":(keyword_set(dr)? $
          "Droopres":(keyword_set(fl)?"FlatAp":"BCD"))
     xf,dir,/DIRECTORY,/RECENT,TITLE='Select '+type+' AOR Directory', $
        PARENT_GROUP=self->TopBase(),/MODAL
  endif 
  if size(dir,/TYPE) ne 7 then return
  
  if ~file_test(dir,/DIRECTORY) then $
     self->Error,'Must select directory containing AORs'
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
     self->Error,['No data found in:',dir],TITLE='AOR Import Error'
  if cnt gt 500 then begin 
     ans=dialog_message(["Selected directory contains "+strtrim(cnt,2)+ $
                         " data files. Continue?",dir],$
                        /QUESTION,TITLE="Add AORs: "+self->ProjectName()+"?", $
                        DIALOG_PARENT=self->TopBase())
     if ans ne 'Yes' then return
  endif 

  for i=0,n_elements(files)-1 do begin 
     file=self->FileBCD(files[i]) ;get the bcd header...
     hdr=headfits(file)
     rec={FILE:files[i],OBJECT:strtrim(sxpar(hdr,'OBJECT'),2), $
          FOV:long(sxpar(hdr,'FOVID')), $
          NCYCLES:long(sxpar(hdr,'NCYCLES')), $
          TIME:float(sxpar(hdr,'EXPTOT_T')), $
          STEPS:long([sxpar(hdr,'STEPSPAR'),sxpar(hdr,'STEPSPER')])}
     if n_elements(all) eq 0 then all=[rec] else all=[all,rec]
  endfor 
  
  uniq_objs=uniq(all.OBJECT,sort(all.OBJECT))
  for i=0,n_elements(uniq_objs)-1 do begin 
     object=all[uniq_objs[i]].OBJECT
     wh=where(all.OBJECT eq object,cnt)
     uniq_fovs=uniq(all[wh].FOV,sort(all[wh].FOV))
     nfovs=n_elements(uniq_fovs) 
     for j=0,nfovs-1 do begin 
        this=all[wh[uniq_fovs[j]]]
        these=all[wh[where(all[wh].FOV eq this.fov)]]
        fovname=irs_fov(this.fov,/SHORT_NAME,MODULE=md,ORDER=ord)
        ;; A file we don't recognize
        if size(fovname,/TYPE) ne 7 then continue
        pos=strpos(fovname,'_cen')
        if pos ne -1 then fovname=strmid(fovname,0,pos)
        ;; Assume same FOV and same OBJECT == one map
        rec={CNT:n_elements(these),FOVNAME:fovname, OBJECT:this.OBJECT,$
             FILES:ptr_new(these.file),STEPS:this.STEPS, $
             TIME:this.TIME, NCYCLES:this.NCYCLES,MERGE:0b}
        if n_elements(choices) eq 0 then choices=rec else begin 
           choices=[choices,rec]
           ;; The separate low-res slits are compatible with each other
;            if ord gt 0 and (md eq 'SL' or md eq 'LL') then begin
;               other=md+strtrim(ord eq 2?1:2,2)
;               whfov=where(choices.FOVNAME eq other,cnt)
;               if cnt gt 0 then begin 
;                  ;; both are present
;                  rec.FOVNAME=other+'+'+fovname
;                  rec.FILES=ptr_new([*rec.FILES,*choices[whfov[0]].FILES])
;                  rec.cnt+=choices[whfov[0]].CNT
;                  rec.MERGE=1b
;                  choices=[choices,rec]
;               endif 
;            endif 
        endelse 
     endfor 
  endfor 
  
  list=strarr(n_elements(choices))
  for i=0,n_elements(choices)-1 do begin 
     if choices[i].MERGE then begin 
        list[i]=string(FORMAT='(%"%s %s (%d files)")', $
                       choices[i].OBJECT,choices[i].FOVNAME,choices[i].CNT)
     endif else begin 
        list[i]=string(FORMAT='(%"%s %s (%d files) %dx%d@%d (%ss)")', $
                       choices[i].OBJECT,choices[i].FOVNAME,choices[i].CNT, $
                       choices[i].STEPS,choices[i].NCYCLES, $
                       strtrim(string(FORMAT='(F20.2)',choices[i].TIME),2))
     endelse 
  endfor 
  
  ch=multchoice('Choose '+type+' Data:',list,TITLE='Load AOR', $
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
     if ~stregex(files[i],'bcd(_fp)?\.fits$',/BOOLEAN) then begin 
        bcdfile=self->FileBCD(files[i])
        if size(bcdfile,/TYPE) eq 7 then header=headfits(bcdfile)
     endif 
     
     bfile=self->FileBCD(files[i],/BMASK)
     if size(bfile,/TYPE) eq 7 && file_test(bfile,/READ) then $
        bmask=readfits(bfile,/SILENT) else bmask=0
     
     self->AddBCD,data,header,FILE=files[i],BMASK=bmask,ERR=err,_EXTRA=e
     if keyword_set(err) then break
  endfor
  self->CheckModules            ;Set default build order, and double check.
  self->UpdateList & self->UpdateButtons
end

;=============================================================================
;  AddBCD - Add a bcd image to the cube, optionally overriding the
;           header derived values.
;=============================================================================
pro CubeProj::AddBCD,bcd,header, FILE=file,ID=id,UNCERTAINTY=unc,BMASK=bmask, $
                     EXP=exp,COLUMN=col, ROW=row, RQST_POS=rqpos, $
                     REC_POS=rpos, PA=pa,ERR=err
  self->LoadCalib
  ;; Don't add same file twice
  if n_elements(file) ne 0 AND ptr_valid(self.DR) then $
     if NOT array_equal((*self.DR).file ne file,1b) then return
  s=size(bcd,/DIMENSIONS)
  rec={CUBE_DR}
  if n_elements(s) eq 3 then begin 
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
  if stregex(file,'droop(res)?\.fits$',/BOOLEAN) then rec.type=1 else $
     if stregex(file,'coad[^.]*\.fits$',/BOOLEAN) then rec.type=2 else $
        if stregex(file,'f2ap\.fits$',/BOOLEAN) then rec.type=3 else $   
           rec.type=0
  
  if n_elements(id) ne 0 then rec.id=id else if rec.file then begin 
     id=filestrip(rec.file)
     if stregex(id,'IRSX.*\.fits',/BOOLEAN) then begin ;Sandbox style names
        parts=strsplit(id,'.',/EXTRACT)
        id=parts[3]+'.'+parts[5]+'.'+parts[6]
     endif else begin ; also treat long archive names specially
        suffix=strpos(id,".fits",/REVERSE_SEARCH)
        if suffix[0] ne -1 then id=strmid(strmid(id,0,suffix),8)
     endelse 
     rec.id=id
  endif 
  
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
  if n_elements(rqpos) eq 2 then rec.RQST_POS=rqpos else $
     rec.RQST_POS=[sxpar(header,'RA_RQST'),sxpar(header,'DEC_RQST')]
  if n_elements(pa_rqst) ne 0 then rec.PA_RQST=pa_rqst else begin 
     rec.PA_RQST=sxpar(header,'PA_RQST')
     self.cal->TransformBoreSightCoords,self.module,[0,0], $
                                        rec.PA_RQST,void,new_pa
     rec.PA_RQST=new_pa          ; transform requested PA to the slit
  endelse
  
  ;; PA_RQST is SIRTF-field-centric, not FOV-centric.

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
  
  if ptr_valid(self.DR) then *self.DR=[*self.DR,rec] else $
     self.DR=ptr_new(rec,/NO_COPY)
  self.Changed=1b
end

;=============================================================================
;  RemoveBCD - Remove one or more BCD's
;=============================================================================
pro CubeProj::RemoveBCD,recs
  heap_free,(*self.DR)[recs]
  keep=where(histogram([recs],MIN=0,MAX=self->N_Records()-1) eq 0,cnt)
  if cnt ne 0 then (*self.DR)=(*self.DR)[keep] $
  else ptr_free,self.DR
  self.Changed=1b               ;but accounts remain valid!
  self->UpdateList,/CLEAR_SELECTION & self->UpdateButtons & self->UpdateTitle
end

;=============================================================================
;  Send - Send One of our messages
;=============================================================================
pro CubeProj::Send,RECORD=record,CUBE=cube,BACKGROUND=back,UPDATE=update
  if keyword_set(cube) then begin 
     self->MsgSend,{CUBEPROJ_CUBE, $
                    self,$
                    string(FORMAT='(%"%s [%dx%d], %s")',self->ProjectName(), $
                           self.NSTEP,jul2date(self.CUBE_DATE)), $
                    self.MODULE,self.WAVELENGTH}
     return
  endif else if keyword_set(back) then begin 
     self->MsgSend,{CUBEPROJ_RECORD, $
                    self,self->ProjectName()+' Background',self.MODULE, $
                    self.ORDER,self.CAL,self.BACKGROUND,self.BACKGROUND_UNC, $
                    ptr_new(),ptr_new()}
     return
  endif else if keyword_set(update) then begin 
     self->MsgSend,{CUBEPROJ_UPDATE,self,self.BAD_PIXEL_LIST}
     return
  endif 
  
  nrec=n_elements(record) 
  if nrec eq 0 then return
  
  stackQ=nrec gt 1
  rec=(*self.DR)[record]
  if stackQ then begin 
     bcd=*rec[0].BCD
     if ptr_valid(rec[0].UNC) then unc=*rec[0].UNC^2 ;add in quadrature
     if ptr_valid(rec[0].BMASK) then mask=*rec[0].BMASK
     for i=1,nrec-1 do begin 
        bcd+=*rec[i].BCD
        if ptr_valid(rec[i].UNC) then $
           if n_elements(unc) gt 0 then unc+=*rec[i].UNC^2 else $
              unc=*rec[i].UNC^2
        if ptr_valid(rec[i].BMASK) then $
           if n_elements(mask) gt 0 then mask OR=*rec[i].BMASK else $
              mask=*rec[i].BMASK ;accumulate mask flags
     endfor 
     bcd_p=ptr_new(bcd,/NO_COPY)
     unc_p=n_elements(unc) gt 0?ptr_new(sqrt(unc)):ptr_new()
     mask_p=n_elements(mask) gt 0?ptr_new(mask,/NO_COPY):ptr_new()
     str=string(FORMAT='(%"%s <Stack of %d recs>")', $
                self->ProjectName(),nrec)
     if ptr_valid(self.BACKGROUND) then begin 
        ptr_free,self.SCALED_BACK
        self.SCALED_BACK=ptr_new(nrec * *self.BACKGROUND)
     endif
  endif else begin 
     bcd_p=rec.BCD
     unc_p=rec.UNC
     mask_p=rec.BMASK
     str=string(FORMAT='(%"%s <%s> %s")', $
                self->ProjectName(),rec.ID, $
                irs_fov(rec.FOVID,/SHORT_NAME))
  endelse 
  self->MsgSend,{CUBEPROJ_RECORD, $
                 self,str,self.MODULE,self.ORDER,self.CAL, $
                 bcd_p,unc_p,mask_p, $
                 stackQ?self.SCALED_BACK:self.BACKGROUND}
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
function CubeProj::N_Records
   if ptr_valid(self.DR) then return,n_elements(*self.DR) else return,0
end

;=============================================================================
;  Cleanup 
;=============================================================================
pro CubeProj::Cleanup
  heap_free,self.DR
  heap_free,self.MERGE
  ptr_free,self.APERTURE,self.CUBE,self.CUBE_UNC, $
           self.BACKGROUND,self.BACKGROUND_UNC,self.SCALED_BACK, $
           self.cuberecs,self.wInfo
  ;if self.spawned then obj_destroy,self.cal ;noone else will see it.
  self->ObjMsg::Cleanup
end

;=============================================================================
;  Init 
;=============================================================================
function CubeProj::Init, name, _EXTRA=e
  if (self->ObjMsg::Init(_EXTRA=e) ne 1) then return,0 ;chain up
;  if self->IDLitComponent::Init() ne 1 then return,0

  self.Changed=0b               ;coming into existence doesn't count
  if n_elements(e) ne 0 then self->SetProperty,_EXTRA=e
  self->ObjReport::SetProperty,TITLE_BASE='CUBISM Project'
  
  ;; Properties:
;   self->SetPropertyAttribute,['Name','Description'],/HIDE
;   self->RegisterProperty,'BUILD_ORDER',NAME='Cube Build Order(s)',ENUMLIST=''
;   self->RegisterProperty,'REQUESTED_POSITIONS',NAME='Build Positions', $
;                          ENUMLIST=['Requested','Reconstructed']
;   self->RegisterProperty,'FEEDBACK',NAME='Show Build Feedback',/BOOLEAN
;   self->RegisterProperty,'PR_WIDTH',NAME='Slit PR Width (pix)',/FLOAT, $
;                          VALID_RANGE=[1.,3.]
  
  return,1
end

;=============================================================================
;  CubeProj - IRS Spectral (+MIPS SED) Cubes
;=============================================================================
pro CubeProj__define
  c={CubeProj, $
;     INHERITS IDLitComponent, $ ;Use Property stuff
     INHERITS ObjMsg, $         ;make it an object messanger
     INHERITS ObjReport, $      ;for error, etc. reporting
     ProjectName:'', $          ;the name of the current project 
     MODULE:'', $               ;The name of the module, one of
                                ;   SL,LL,SH,LH (IRS),MSED (MIPS)
     ORDER:0, $                 ;The sub-slit order for which to build
                                ; the cube (or 0 to build and splice all
                                ; orders in the module)
     APERTURE:ptr_new(), $      ;The aperture of the clip, or one
                                ; for each order
     DR: ptr_new(), $           ;All the BCD's: pointer to list of
                                ; data record structures of type CUBE_DR
     ACCOUNTS_VALID: 0b,$       ; are the account records valid?
                                ;  0: no, 1:yes, 2:size, 4: bg
     CUBE: ptr_new(),$          ;a pointer to the nxmxl data cube
     CUBE_UNC:  ptr_new(),$     ;a pointer to the nxmxl uncertainty cube
     CUBE_SIZE: [0L,0L,0L],$    ;the size of the cube, (n,m,l)
     CUBE_DATE: 0.0D, $         ;date the cube was assembled (JULIAN)
     BACKGROUND: ptr_new(),$    ;the background image (if any) to subtract
     BACKGROUND_UNC:ptr_new(),$ ;the uncertainty in the background
     SCALED_BACK: ptr_new(), $  ;the scaled background for stacks
     BACK_DATE: 0.0D, $         ;date background created
     BACK_CNT:0, $              ;count of records used to create background
     BAD_PIXEL_LIST: ptr_new(),$ ;a user list of bad pixels to exclude
     NSTEP:[0L,0L], $           ;parallel (col), perpendicular (row) steps
     STEP_SIZE: [0.0D,0.0D], $  ;parallel, perpendicular slit step sizes (deg)
     PLATE_SCALE:0.0D, $        ;the plate scale (degrees/pixel)
     PR_SIZE:[0.0,0.0], $       ;the , parallel (long axis), perpendicular
                                ; (short axis) size of the PRs to use (pixels)
     WAVELENGTH: ptr_new(), $   ;the cube's wavelength list
     POSITION:[0.0D,0.0D], $    ;optimized position of the cube center
     PA:0.0D, $                 ;optimized position angle of the cube
     MERGE:ptr_new(),$          ;a set of records to aid merging the orders
                                ;  ptr to list of {offset,inds,wave,to,frac}
     cal:obj_new(), $           ;the irs_calib object.
     cal_file:'', $             ;the calibration file used (if not a full
                                ; directory, in the "calib/" subdir)
     Changed:0b, $              ;if the project is changed since last saved.
     Spawned:0b, $              ;whether we were opened by another instance
     feedback:0b, $             ;whether to show feedback when building cube
     fluxcon:0b, $              ;whether to build with FLUXCON fluxes
     reconstructed_pos:0b, $    ;whether to build with reconstructed positions
     SaveFile:'', $             ;the file it was saved to
     SaveMethod:0b, $           ;bit 1: data, bit 2: accounts
     sort:0b, $                 ;our sorting order
     version:'', $              ;the Cubism version of this cube
     cuberecs:ptr_new(), $      ;a list of cubeview records's we've spawned
     wInfo:ptr_new()}           ;the widget info struct.... a diconnectable ptr
  
  
  ;; The account structure is *big* (~1Mb per BCD)
  acc={CUBE_ACCOUNT_LIST, $
       cube_plane:0, $          ;the cube plane it goes to.
       cube_pix:0, $            ;the cube pixel in that plane
       bcd_pix:0, $             ;the bcd pixel it came from
       area:0.0}                ;the area of overlap on the cube pixel
  
  ;; The data structure for each input BCD
  rec={CUBE_DR, $
       ID:'',$                  ;A unique (hopefully) ID
       file:'', $               ;the original file read for this BCD dataset
       TIME:0.0, $              ;The integration time
       DISABLED: 0b, $          ;whether this DR is disabled
       ACCOUNT: ptr_new(), $    ;list of {CUBE_PLANE,CUBE_PIX,BCD_PIX,AREAS}
       REV_ACCOUNT: ptr_new(),$ ;reverse indices of the CUBE_INDEX histogram
       REV_MIN:0L, $            ;minimum cube index affected
       REV_CNT:0L, $            ;how many bins in the cube_index histogram?
       RQST_POS: [0.0D,0.0D], $ ;Commanded RA,DEC position of slit center
       REC_POS: [0.0D,0.0D],$   ;Reconstructed RA,DEC pos of slit center.
       FOVID: 0, $              ;The IRS Field of View ID
       TYPE: 0, $               ;0- BCD, 1- Droopres, 2- Coad2d
       TARGET_ORDER: 0, $       ;which order was targetted by pointing
                                ;  (or 0 for module center targetting)
       TARGET_POS: 0, $         ;For low-res: 0, 1, or 2 for centered on
                                ; module, slit position 1, or slit position 2
       PA: 0.0D, $              ;Position angle of slit (+w?) E of N
       PA_RQST: 0.0D, $         ;Requestion PA of slit (+w?)
       AORKEY: 0L, $            ;the AOR ID KEY
       BCD: ptr_new(), $        ;the BCD
       UNC:ptr_new(), $         ;the BCD's uncertainty image
       BMASK:ptr_new(), $       ;the BCD's BMASK image
       EXP: 0L, $               ;the exposure number in the mapping sequence
       CYCLE:0L, $              ;the cycle number at this position
       NCYCLES:0L,$             ;the number of cycles at this position
       COLUMN: 0L,$             ;the step number perpendicular to the slit
       ROW:0L, $                ;the step number parallel to the slit
       DATE:0.0D, $             ;the date this BCD was added
       DATE_OBS:0.0D, $         ;the date this BCD was observed
       HEADER: ptr_new()}       ;a pointer to a string array
  
  ;; The widget info
  winfo={cubeProj_wInfo,$
         showing:0, $           ;whether we're showing
         Base:0L, $             ;the Show Widget, DR list display base
         SList:0L, $            ;the Widget List for Show 
         wHead:lonarr(2), $     ;the widget heads
         list_row:0.0, $        ;the height of list rows in pixels
         which_list:0, $        ;which list we're using
         list_size_diff:0, $    ;the difference in ysize between list and base
         lines:0, $             ;number of list lines last set
         view_ids:lonarr(3), $  ;record view button ids
         nsel_sav: 0, $          ;save number of selected records
         feedback_window: 0, $  ;window id of feedback
         MUST_MODULE:lonarr(1),$ ;must have a module set
         MUST_CAL:lonarr(2), $  ;Must have a calibration set loaded
         MUST_SELECT:lonarr(16),$ ;the SW buttons which require any selected
         MUST_SAVE_CHANGED:0L, $ ;require changed and a saved File
         MUST_PROJ:0L, $        ;SW button which requires a valid project
         MUST_ACCT:0L, $        ;Must have valid accounts
         MUST_CUBE:lonarr(4), $ ;SW button requires valid cube created.
         MUST_BACK:lonarr(3), $ ;background record must be set
         MUST_BPL:lonarr(2), $  ;must have a list of bad pixels
         MUST_UNRESTORED:0L}    ;must have unrestored data
  
  ;; Messages: send a cube, record, etc.
  
  ;; XXX send updates on new cube build (for those who are viewing them, etc.)
  msg={CUBEPROJ_CUBE,  CUBE:obj_new(),INFO:'',MODULE:'',WAVELENGTH:ptr_new()}
  msg={CUBEPROJ_RECORD,CUBE:obj_new(),INFO:'',MODULE:'',ORDER:0, $
       CAL:obj_new(),BCD:ptr_new(),UNC:ptr_new(),BMASK:ptr_new(), $
       BACKGROUND:ptr_new()}    ;XXX UNC
  msg={CUBEPROJ_UPDATE, CUBE:obj_new(), BAD_PIXEL_LIST:ptr_new()}
end


;; Switch to properties-based config using IDLitComponent
;; Properties:
;;   Cube Build:
;;     Build Order(s)
;;     Bonus Order (if appropriate)
;;     Requested or Reconstructed Positions
;;     Show feedback
;;     Reconstructed Positions
