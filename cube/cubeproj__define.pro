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
;    input data, and the routines to convert them into spectral cubes.
;    
; CATEGORY:
;
;    CUBISM Spectral Cube Reduction, Analysis and Processing.
;    	
; SIDE EFFECTS:
;
;    Especially for procedures with no return values.  May be unnecessary.
;
; RESTRICTIONS:
;
;    Any restrictions?
;
; METHODS:
;
;    N.B. The `METHODS' section is used in class definition files only
;    (like class__define.pro) which contain all the methods of that
;    class.  In it, only the PUBLIC methods should be documented in
;    this header.  Methods which are internal to the class, and not
;    for general consumption, should be informally documented in the
;    body of the code.  Within each individual METHOD section, the
;    subsections DESCRIPTION, and INPUT PARAMETERS through
;    RESTRICTIONS above (which should be omitted) can be included for
;    each method, as appropriate, and at an increased indentation.
;    See those sections above for style guidelines.
;
;    Init:  (always start with the INIT method function)
;
;       CALLING SEQUENCE:
;
;          obj=obj_new('CubeProj',[name])
;
;       OPTIONAL INPUT PARAMETERS:
;
;          name:  A name for the project.  Will otherwise be left blank.
;
;       INPUT KEYWORD PARAMETERS:
;
;          KEYWORD:
;       ...
;
;    Method1:
;  
;	DESCRIPTION:
;
;	   This method makes possible short duration levitation.
;	
;       CALLING SEQUENCE:
;
;          obj->Method1, req_arg1, req_arg2, ..., 
;             [opt_arg1,...,/BIN_KEYWORD,KEYWORD=]
;
;       INPUT PARAMETERS:
;
;          req_arg1:
;       ...
;    ...
;
; PROCEDURES:
;
;    Requires IRS_Calib calibration objects.
;
; NOTES:
;  
;    Additional description and other information which doesn't fit elsewhere.
;
; INHERITANCE TREE:
;
;     ObjMsg-->CubeProj
; EXAMPLE:
;
;    a=some_init()
;    CUBISM_ROUTINE,a,b,KEYWORD=foo
;    print,foo
;
; MODIFICATION HISTORY:
;    
;    Add here only important modifications which the end user might be
;    interested in seeing.  Fine level history should be reserved for
;    the CVS logs.  Use this format:
;
;    2001-08-10 (J.D. Smith): Initially written.
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2002 J.D. Smith
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

;=============================================================================
;  WriteFits - Write the Cube, Error, etc. to FITS file
;=============================================================================
pro cubeProj::WriteFits,file
  if NOT ptr_valid(self.CUBE) then return 
  if NOT ptr_valid(self.WAVELENGTH) then return
  fxhmake,hdr,*self.CUBE,/extend,/date
  ;; Description
  sxaddhist, ['The SIRTF Nearby Galaxy Survey (SINGS) Legacy Project', $
              'This file contains a spectral cube assembled from an IRS', $
              'step & stare spectral mapping dataset.', $
              'For more information on SINGS see http://sings.sirtf.edu'], $
             hdr,/COMMENT
  
  ;;Module/Order
  fxaddpar,hdr,'APERNAME',self.MODULE,' The IRS module'
  fxaddpar,hdr,'ORDER',self.ORDER,' The order: 0 for all orders'
  
  ;; Celestial coordinates
  RADEG = 180.0d/!DPI           ; preserve double
  fxaddpar,hdr,'EQUINOX', 2000.0,   ' Equinox of reference coordinate'
  fxaddpar,hdr,'CTYPE1','RA---TAN', ' RA in tangent plane projection'
  fxaddpar,hdr,'CTYPE2','DEC--TAN', ' DEC in tangent plane projection'
  fxaddpar,hdr,'CRPIX1',(self.CUBE_SIZE[0]+1.)/2, $
           ' Pixel x coordinate at reference point'
  fxaddpar,hdr,'CRPIX2',(self.CUBE_SIZE[1]+1.)/2, $
           ' Pixel y coordinate at reference point'
  fxaddpar,hdr,'CRVAL1',self.POSITION[0],' [deg] RA at reference point'
  fxaddpar,hdr,'CRVAL2',self.POSITION[1],' [deg] DEC at reference point'
  
  ;; Old style angle, for older FITS readers
  fxaddpar,hdr,'CROTA2',self.PA,' [deg] Rotation angle'
  
  ;; New style coordinate transform
  fxaddpar,hdr,'CDELT1',self.PLATE_SCALE, $
           ' [deg/pix] Plate scale, coordinate 1'
  fxaddpar,hdr,'CDELT2',self.PLATE_SCALE, $
           ' [deg/pix] Plate scale, coordinate 2'
  fxaddpar,hdr,'PC1_1' ,-cos(self.PA/RADEG),' Transformation matrix element'
  fxaddpar,hdr,'PC1_2' ,-sin(self.PA/RADEG),' Transformation matrix element'
  fxaddpar,hdr,'PC2_1' ,-sin(self.PA/RADEG),' Transformation matrix element'
  fxaddpar,hdr,'PC2_2' , cos(self.PA/RADEG),' Transformation matrix element'
  
  ;; Wavelength coordinates and wavelength LUT binary table extension.
  fxaddpar,hdr,'CTYPE3','WAVE-TAB','Wavelength'
  fxaddpar,hdr,'CUNIT3','um','Wavelength units'
  fxaddpar,hdr,'PS3_0','WCS-TAB','Coordinate table extension name'
  fxaddpar,hdr,'PS3_1','WAVELENGTH','Coordinate table column name'
  
  ;; Write the primary header and data
  fxwrite,file,hdr,*self.CUBE
;  fxwrite,file,hdr,*self.ERR
  
  ;; Make the wavelength LUT extension header
  fxbhmake,hdr,1,'WCS-TAB','Wavelength look-up table for cube dimension 3'
  fxbaddcol,wcol,hdr,*self.WAVELENGTH,'WAVELENGTH','Column label'
  fxbcreate,unit,file,hdr
  fxbwrite,unit,*self.WAVELENGTH,wcol,1
  fxbfinish,unit
end

;=============================================================================
;  Save - Save the full cube project 
;=============================================================================
pro cubeProj::Save, file,AS=as
  if size(file,/TYPE) ne 7 or keyword_set(as) or $
     strlen(self.SaveFile) eq 0 then begin 
     ;; select a file
  endif 
  
  ;; Detach the stuff we don't want to save!
  detInfo=self.wInfo & self.wInfo=ptr_new() ;don't save the info
  detMsgList=self.MsgList & self.MsgList=ptr_new() ;or the message list
  detCal=self.cal & self.cal=obj_new() ;or the calibration object
  
  oldchange=self.Changed        ;we want the file written to have changed=0!
  self.Changed=0b               ;but save the old preference in case we fail
  
  catch, serr
  if serr ne 0 then begin       ;it failed!
     catch,/CANCEL
     self.wInfo=detInfo         ;reattach them
     self.MsgList=detMsgList
     self.cal=detCal
     self.Changed=oldchange     ;reassign our old changed status
     self->Error,'Error Saving to File: '+file
     return
  endif 
  save,self,FILENAME=file
  catch,/CANCEL
  ;; Reattach 
  self.wInfo=detInfo           
  self.MsgList=detMsgList   
  self.cal=detCal
end

;=============================================================================
;  Print - Print the cube's contents
;=============================================================================
pro cubeProj::Print,entries, NO_DATA=nd
  print,'IRS Spectral Cube: '+self.ProjectName,' Created: '+ $
        (self.CUBE_DATE eq 0.0d?"(not yet)":jul2date(self.CUBE_DATE))
  print,self.MODULE,self.ORDER ne 0? $
        ' Order '+strtrim(self.ORDER,2): $
        ' all orders'
  print,'Using IRS Calib object ',self.cal_file, $
        " (",(obj_valid(self.cal)?"":"not")+"loaded",")"
  print,FORMAT='(I0,"x",I0," steps = ",F6.3," x ",F6.3," arcsec",' + $
        '" (",F6.3," arcsec/pixel)")',self.NSTEP, $
        self.STEP_SIZE*3600*self.NSTEP,self.PLATE_SCALE*3600
  print,FORMAT='("PR Sample Size: ",F6.3," x ",F6.3," pixels")', $
        self.PR_SIZE
  if keyword_set(nd) then return
  if NOT ptr_valid(self.DR) then begin
     print,'No data'
     return
  endif   
  print,' ===== DATA ====='
  if n_elements(entries) eq 0 then entries=lindgen(n_elements(*self.DR))
  for i=0,n_elements(entries)-1 do begin 
     rec=(*self.DR)[entries[i]]
     print,rec.id,":",rec.file
     sign=rec.CMD_POS[1] ge 0.0?'+':'-'
     radec,rec.CMD_POS[0],abs(rec.CMD_POS[1]),rh,rm,rs,dd,dm,ds
     ra=string(format='(I0,"h",I2.2,"m",F5.2,"s")',rh,rm,rs)
     dec=sign+string(format='(I0,"d",I2.2,"m",F5.2,"s")',abs(dd),dm,ds)
     print,FORMAT='(%"  EXP: %2.0d (%d/%d) (%2.0d,%2.0d) RA: %s, DEC: %s")', $
           rec.EXP,rec.CYCLE,rec.NCYCLES,rec.ROW,rec.COLUMN, ra,dec
  endfor 
end

;=============================================================================
;  SetProperty - Set various cube properties.  Most of these should be
;                automatically discovered from the BCD's.  If any are
;                actually changed, indicate that the account is no
;                longer valid.
;=============================================================================
pro cubeProj::SetProperty,PLATE_SCALE=ps,NSTEP=nstep,STEP_SIZE=stepsz, $
                          MODULE=md,ORDER=ord,SLIT_SIZE=slitsz, $
                          SLIT_LENGTH=slitl, SLIT_WIDTH=slitw, PR_WIDTH=prw, $
                          PR_SIZE=prz,CAL_FILE=cal_file,CAL_OBJECT=cal, $
                          APERTURE=aper
  if n_elements(ps) ne 0 then begin 
     if self.PLATE_SCALE ne ps then begin 
        self.PLATE_SCALE=ps
        self.ACCOUNTS_VALID=0b
     endif 
  endif 
  if n_elements(nstep) ne 0 then self.NSTEP=nstep
  if n_elements(stepsz) ne 0 then begin 
     if NOT array_equal(self.STEP_SIZE,stepsz) then begin 
        self.STEP_SIZE=stepsz
        self.ACCOUNTS_VALID=0b
     endif 
  endif 
  if n_elements(md) ne 0 then begin 
     if md ne self.MODULE then begin
        self.MODULE=md
        self.ACCOUNTS_VALID=0b
     endif 
  endif 
  if n_elements(ord) ne 0 then begin 
     if ord ne self.ORDER then begin 
        self.ORDER=ord
        self.ACCOUNTS_VALID=0b
     endif 
  endif 
  if n_elements(slitsz) eq 2 then begin 
     if self.SLIT_SIZE ne slitsz then begin 
        self.SLIT_SIZE=slitsz
        self.ACCOUNTS_VALID=0b
     endif 
  endif else begin 
     if n_elements(slitl) ne 0 then begin 
        if self.SLIT_SIZE[0] ne slitl then begin 
           self.SLIT_SIZE[0]=slitl
           self.ACCOUNTS_VALID=0b
        endif
     endif
     if n_elements(slitw) ne 0 then begin 
        if self.SLIT_SIZE[1] ne slitw then begin 
           self.SLIT_SIZE[1]=slitw
           self.ACCOUNTS_VALID=0b
        endif
     endif
  endelse
  if n_elements(prw) ne 0 then begin 
     prw=0.>prw 
     if prw ne self.PR_SIZE[1] then begin 
        self.PR_SIZE[1]=prw
        self.ACCOUNTS_VALID=0b
     endif
  endif
  if n_elements(prz) ne 0 then begin 
     if NOT array_equal(self.PR_SIZE,prz) then begin 
        self.PR_SIZE=prz
        self.ACCOUNTS_VALID=0b
     endif
  endif
  if n_elements(cal_file) ne 0 then begin 
     if self.cal_file ne cal_file then begin 
        if obj_valid(self.cal) then obj_destroy,self.cal
        self.cal_file=cal_file
     endif 
  endif
  if n_elements(cal) ne 0 then begin 
     if obj_isa(cal,'IRS_Calib') then begin 
        if self.cal ne cal then begin 
           self.cal=cal 
           self.ACCOUNTS_VALID=0b
        endif 
     endif else message,'Calibration object not of correct type.'
  endif
  if n_elements(aper) ne 0 then begin 
     ptr_free,self.APERTURE
     self.APERTURE=ptr_new(aper)
     self.ACCOUNTS_VALID=0b
  endif 
end

;=============================================================================
;  GetProperty
;=============================================================================
pro cubeProj::GetProperty, ACCOUNT=account, WAVELENGTH=wave, CUBE=cube, $
                           ERROR=err, PR_SIZE=prz, CALIB=calib
  if arg_present(account) then $
     if ptr_valid(self.ACCOUNT) then account=*self.account
  if arg_present(wave) then $
     if ptr_valid(self.WAVELENGTH) then wave=*self.WAVELENGTH
  if arg_present(cube) then $
     if ptr_valid(self.CUBE) then cube=*self.CUBE
  if arg_present(err) then $
     if ptr_valid(self.ERR) then err=*self.ERR
  if arg_present(prz) then prz=self.PR_SIZE
  if arg_present(calib) then begin 
     self->LoadCalib            ;ensure it's loaded
     calib=self.cal
  endif 
end

;=============================================================================
;  Cube
;=============================================================================
function cubeProj::Cube
  if ptr_valid(self.CUBE) then return,*self.CUBE 
  return,-1
end

;=============================================================================
;  BCD
;=============================================================================
function cubeProj::BCD, which
  return,(*self.DR)[which].BCD
end

;=============================================================================
;  LoadCalib - Ensure the calibration object is loaded and available.
;=============================================================================
pro cubeProj::LoadCalib
  if obj_isa(self.cal,'IRS_Calib') then return
  if self.cal_file eq '' then $
     message,'Error: no calibration object and no cal file specified.'
  self.cal=irs_restore_calib(self.cal_file)
  self.ACCOUNTS_VALID=0b
end

;=============================================================================
;  CheckSteps - Ensure that all of the BCD's for this map are present.
;=============================================================================
pro cubeProj::CheckSteps
  if NOT ptr_valid(self.DR) then message, 'No BCD Data loaded.'
  got=bytarr(self.NSTEP)
  got[(*self.DR).ROW-1,(*self.DR).COLUMN-1]=1b
  wh=where(got eq 0b, cnt)
  if cnt eq 0 then return
  message,'Missing Steps: '+ string(10b)+ $
          string(FORMAT='('+strtrim(cnt,2)+'("[",I0,", ",I0,"]"))', $
                 [1#(wh mod self.NSTEP[0]+1),1#(wh/self.NSTEP[0]+1)])
end

;=============================================================================
;  Normalize - Map header info in the BCD's to cube-specific data, and
;              returns the status of the normalization.
;=============================================================================
pro cubeProj::Normalize
  self->LoadCalib

  if NOT ptr_valid(self.DR) then $
     message,'No data records'
  
  ;; Normalize the module type
  modules=strarr(n_elements(*self.DR))
  for i=0,n_elements(*self.DR)-1 do begin 
     fovid=sxpar(*(*self.DR)[i].HEADER,'FOVID',COUNT=cnt) 
     if cnt eq 0 then message,"No FOVID keyword present"
     fovname=irs_fov(fovid,MODULE=module)
     modules[i]=module
  endfor 
  u=uniq(modules,sort(modules))
  if n_elements(u) gt 1 then $
     message,string(FORMAT='("Too many module types: ",A0, : ",")',modules[u])
  self.module=modules[u[0]]
  
  ;; For low-res use the target order as the order, if they're all the same.
  if array_equal((*self.DR).TARGET_ORDER,(*self.DR)[0].TARGET_ORDER) AND $
     self.ORDER eq 0 AND $
     (self.module eq 'SL' OR self.module eq 'SH') $
     then self.ORDER=(*self.DR)[0].TARGET_ORDER
  
  ;; Normalize the plate scale... they should all be the same!
  ps=dblarr(2*n_elements(*self.DR))
  for i=0,n_elements(*self.DR)-1 do $
     ps[2*i]=sxpar(*(*self.DR)[i].HEADER,'CDELT*') 
  psm=mean(ps)
  if NOT array_equal(abs(ps-psm) lt 1.e-4,1b) then $
     message,"Plate scale mismatch among input BCD's"
  self.PLATE_SCALE=psm
  
  ;; Normalize the number of steps and step size (they should all be the same)
  stepsper=(stepspar=lonarr(n_elements(*self.DR)))
  stepszper=(stepszpar=dblarr(n_elements(*self.DR)))
  for i=0,n_elements(*self.DR)-1 do begin 
     stepsper[i]=sxpar(*(*self.DR)[i].HEADER,'STEPSPER')
     stepspar[i]=sxpar(*(*self.DR)[i].HEADER,'STEPSPAR')
     stepszpar[i]=sxpar(*(*self.DR)[i].HEADER,'SIZEPAR')
     stepszper[i]=sxpar(*(*self.DR)[i].HEADER,'SIZEPER')
  endfor 
  if (NOT array_equal(stepsper,stepsper[0])) or $
     (NOT array_equal(stepspar,stepspar[0])) then $
     message,"BCD's have unequal map size."
  self.NSTEP=[stepspar[0],stepsper[0]]
  if (NOT array_equal(stepszper,stepszper[0])) or $
     (NOT array_equal(stepszpar,stepszpar[0])) then $
     message,"BCD's have unequal step size."
  self.STEP_SIZE=[stepszpar[0],stepszper[0]]/3600.D
  
  ;; Normalize the slit length
  if self.ORDER gt 0 then begin 
     self.cal->GetProperty,self.module,self.order,SLIT_LENGTH=sl
     if self.SLIT_SIZE[0] eq 0.0 then self.SLIT_SIZE[0]=sl*self.PLATE_SCALE
     self.PR_SIZE[0]=sl
  endif else begin ;; find the longest slit and use it
     ords=self.cal->Orders(self.module)
     slmax=0.
     for i=0,n_elements(ords)-1 do begin 
        self.cal->GetProperty,self.module,ords[i],SLIT_LENGTH=sl
        slmax=sl>slmax
     endfor 
     if self.SLIT_SIZE[0] eq 0.0 then self.SLIT_SIZE[0]=slmax*self.PLATE_SCALE
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
     self.ACCOUNTS_VALID=0b
     self.PR_SIZE[1]=1.D        ;the default, 1xn XXX
  endif 
  
  ;; Normalize the build aperture(s)
  if ptr_valid(self.APERTURE) then begin 
     for i=0,n_elements(*self.APERTURE)-1 do begin 
        ap=(*self.APERTURE)[i]
        ;; default to the full aperture
        if array_equal(ap.low,0.) AND array_equal(ap.high,0.) then $
           (*self.APERTURE)[i]={IRS_APERTURE,[0.,0.],[1.,1.]}
     endfor 
  endif else self.APERTURE=ptr_new({IRS_APERTURE,[0.,0.],[1.,1.]})
  
  ;; Find the average position angle over the course of the map
  pa=mean((*self.DR).PA)
  
  ;; Establish the dimensions of the cube in the slit coordinate system
  new_size=ceil((self.NSTEP-1L)*self.STEP_SIZE/self.PLATE_SCALE+ $
                self.PR_SIZE)
  if NOT array_equal(self.CUBE_SIZE[0:1],new_size) then begin 
     self.CUBE_SIZE[0:1]=new_size
     self.ACCOUNTS_VALID=0b
  endif
end


;=============================================================================
;  AddMergeRec - Add a merge record for a given order, index and
;                wavelength set.
;=============================================================================
pro cubeProj::AddMergeRec,order,planes,wave,OFFSET=off
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
;  MergeSetup - Setup the merge by finding interpolating wavelengths,
;               and recording which parts of the orders get
;               interpolated, and where.
;=============================================================================
pro cubeProj::MergeSetup,ORDS=ords
  if self.ORDER eq 0 then ords=self.cal->Orders(self.MODULE) else $
     ords=self.ORDER
  ptr_free,self.WAVELENGTH
  heap_free,self.MERGE
  wave1=(self.cal->GetWAVSAMP(self.MODULE,ords[0],/PIXEL_BASED, $
                              APERTURE=(*self.APERTURE)[0])).lambda
  if self.ORDER gt 0 then begin ;nothing to do
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
  for ord=1L,nord-1L do begin
     ;; Work with previous, overlap with current order
     wave2=(self.cal->GetWAVSAMP(self.MODULE,ords[ord],/PIXEL_BASED, $
                                 APERTURE=nap eq 1?(*self.APERTURE)[0]: $
                                 (*self.APERTURE)[ord])).lambda
     nw1=n_elements(wave1) & nw2=n_elements(wave2) 
     min_wav1=min(wave1,max=max_wav1)
     min_wav2=min(wave2,max=max_wav2)
     
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
        continue
     endif
  
     ;; Locations of the overlap region in each wavelength vector.
     wh_over1=where(wave1 gt min_wav2 AND $
                    wave1 lt max_wav2,cnt1,COMPLEMENT=wh_clear1)
     wh_over2=where(wave2 gt min_wav1 AND $
                    wave2 lt max_wav1,cnt2,COMPLEMENT=wh_clear2)
     
     ;; Use as the primary wavelength set whichever had more samples in
     ;; the original overlap region
     if cnt1 ge cnt2 then begin  
        use_wav=wave1[wh_over1]
        lose_wav=wave2[wh_over2]
        self->AddMergeRec,ord,wh_over2,lose_wav
        ;;if max_wav1 gt max_wav2 then lead_in=1
        ;;if min_wav1 lt min_wav2 then lead_out=1
     endif else begin
        use_wav=wave2[wh_over2]
        lose_wav=wave1[wh_over1]
        ;; The working segment may have been truncated on the last round
        self->AddMergeRec,ord-1,wh_clear_sav[0] ne -1? $
                          wh_clear_sav[wh_over1]:wh_over1,lose_wav
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
     
     ;; Concat wave1 and overlap onto wavelengths:
     ;;  [wave1 use_wav wave2] or [wave2 use_wav wave1]
     if prepend then begin
        if n_elements(wave) gt 0 then wave=[use_wav,wave1,wave] $
        else wave=[use_wav,wave1]
        wave_zero[0:ord-1]=wave_zero[0:ord-1]+n_elements(wave2) 
     endif else begin           
        ;; appending new ones
        if n_elements(wave) gt 0 then wave=[wave,wave1,use_wav] $
        else wave=[wave1,use_wav]
        wave_zero[ord]=wave_zero[ord-1]+n_elements(wave1)
     endelse
     wave1=wave2
     wh_clear_sav=wh_clear2
  endfor
  ;; graft on the last remaining piece
  if prepend then wave=[wave2,wave] else wave=[wave,wave2]
  
  (*self.MERGE).offset=wave_zero
  
  ;; Finish the merge vector, computing split fractions and merge locations
  for ord=0,nord-1 do begin 
     ;; Interpolate those being merged onto the new wavelength grid.  
     if NOT ptr_valid((*self.MERGE)[ord].wave) then continue
     merge_wave=*(*self.MERGE)[ord].wave
     map_loc=value_locate(wave,merge_wave)
     if NOT array_equal(map_loc ne -1,1b) then $
        message,'Cannot merge non-bracketed wavelengths.'
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
;                 planes with with linear interpolation.  If they
;                 don't overlap, just concatenate the accounting
;                 cubes.  ORDER is the logical order number index in
;                 sequence (as opposed to the optical order).
;=============================================================================
pro cubeProj::MergeAccount,dr,order,account
  
  if NOT ptr_valid(self.MERGE) then begin ; only one order
     (*self.DR)[dr].ACCOUNT=ptr_new(account)
     return
  endif 
     
  mrec=(*self.MERGE)[order]
  dr_acct=(*self.DR)[dr].ACCOUNT
  if NOT ptr_valid(mrec.planes) then begin 
     ;;no merging needed, just append account list, suitably offset
     account.cube_plane=account.cube_plane+mrec.offset
     if NOT ptr_valid(dr_acct) then begin 
        (*self.DR)[dr].ACCOUNT=ptr_new(account)
     endif else *dr_acct=[*dr_acct,account]
     return
  endif

  planes=*mrec.planes           ;order account planes to be manipulated
  to_planes=*mrec.to            ; ... and sent to these cube planes
  fracs=*mrec.frac              ; ... with these fractions
  
  ;; Group the account records by the cube plane affected
  minp=min(planes,MAX=maxp)
  h=histogram(account.CUBE_PLANE,MIN=minp,MAX=maxp,REVERSE_INDICES=ri_cube)
  
  ;; Offset all to the correct cube plane (some we'll change soon)
  account.cube_plane=account.cube_plane+mrec.offset
  ;; which bins hold planes which must be interpolated?
  wh=where(h gt 0 and histogram(planes,REVERSE_INDICES=ri_order) gt 0,cnt) 
  for i=0,cnt-1 do begin 
     if ri_cube[wh[i]+1] eq ri_cube[wh[i]] then continue
     to_plane=to_planes[ri_order[ri_order[wh[i]]]]
     frac=fracs[ri_order[ri_order[wh[i]]]]
     
     ;; Accounting elements on affected planes
     changers=ri_cube[ri_cube[wh[i]]:ri_cube[wh[i]+1]-1]
     split_acc=account[changers]
     
     ;; The first plane (gets frac worth)
     account[changers].areas=account[changers].areas*frac
     account[changers].CUBE_PLANE=to_plane
     ;; The next plane (gets 1-frac worth)
     split_acc.areas=split_acc.areas*(1.-frac)
     split_acc.CUBE_PLANE=to_plane+1
     
     if n_elements(new_acc) eq 0 then new_acc=[split_acc] else $
        new_acc=[new_acc,split_acc]
  endfor
  
  ;; Append this newly modified account
  if ptr_valid(dr_acct) then *dr_acct=[*dr_acct,account] else $
     (*self.DR)[dr].ACCOUNT=ptr_new(account)     
end 


;=============================================================================
;  BuildAccount - Build the accounting lists, listing, for each
;                 record, and for all pixels in the cube, all
;                 overlapping data pixels, including the BCD index #,
;                 the pixel in that BCD which overlapped, and fraction
;                 which overlapped the corresponding cube pixel.
;=============================================================================
pro cubeProj::BuildAccount,_EXTRA=e
  self->Normalize
  self->MergeSetup
  self.CUBE_SIZE[2]=n_elements(*self.WAVELENGTH) 
  
  stepsz=self.STEP_SIZE/self.PLATE_SCALE ; slit step size, in pixels
  
  ;; Are we treating one order, or all of them?
  if self.ORDER eq 0 then ords=self.cal->Orders(self.MODULE) else $
     ords=self.ORDER
  nap=n_elements(*self.APERTURE) 
  
    ;;Debugging plots
  tvlct,[255b,0b,0b,0b],[0b,255b,0b,255b],[0b,0b,255b,255b],1
  plot,[0],/NODATA,xrange=[0,self.cube_size[0]], $
       yrange=[0,self.cube_size[1]],xstyle=1,ystyle=1,xticks=1,yticks=1
  for i=0,self.cube_size[0] do plots,i,!Y.CRANGE
  for i=0,self.cube_size[1] do plots,!X.CRANGE,i
  wait,0
  ;; End debugging plots

  for i=0L,n_elements(*self.DR)-1 do begin 
     ;; skip disabled records unconditionally
     if (*self.DR)[i].DISABLED then continue 
     
     ;; if the accounts are still valid and an account exists for this
     ;; DR, assume it's valid.
     if self.ACCOUNTS_VALID AND ptr_valid((*self.DR)[i].ACCOUNT) then $
        continue else ptr_free,(*self.DR)[i].ACCOUNT
     
     ;; Compute the pixel offset of the slit center for this BCD
     ;; The slit is laid out differently in Spectral Maps and BCD's (ughh)
     ;; so ROW<-->COLUMN.  
     basic_offset=([(*self.DR)[i].ROW,(*self.DR)[i].COLUMN]-1)*stepsz+ $
                  self.PR_SIZE/2.
     
     ;; (Small) difference between PA of this BCD and the mean map PA
     pa_delta=((*self.DR)[i].PA-self.PA)
     for ord=0,n_elements(ords)-1 do begin
        aper=nap eq 1?(*self.APERTURE)[0]:(*self.APERTURE)[ord]
        prs=self.cal->GetWAVSAMP(self.MODULE,ords[ord],APERTURE=aper, $
                                 /PIXEL_BASED, /SAVE_POLYGONS, $
                                 PR_WIDTH=self.PR_SIZE[1],_EXTRA=e)
        offset=basic_offset
        
        ;;X-Offset for non-centered aperture
        mlow=min(aper.low) & mhigh=max(aper.high)
        if mlow+mhigh ne 1. then $
           offset[0]=offset[0]-(.5*(mlow+mhigh)-.5)* $
                     self.PR_SIZE[0]/(mhigh-mlow)
        
        ;; Pre-allocate an account list 
        nacc=4*n_elements(prs) 
        account=replicate({CUBE_ACCOUNT_LIST, $
                           cube_plane:0L,cube_pix:0L, bcd_pix:0L, areas:0.0}, $
                          nacc)
        
        acc_ind=0L
        for j=0L,n_elements(prs)-1 do begin ; iterate over all the PRs
           ;; Setup the rotation matrix to rotate back to the +x direction
           angle=prs[j].angle+pa_delta
           if angle ne 0.0D then begin 
              ct=cos(angle/!radeg) & st=sin(angle/!radeg)
              rot=transpose([[ ct, st], $
                             [-st, ct]])
           endif
           ;; Iterate over partial pixels clipped by the PR
           for k=0L,n_elements(*prs[j].POLYGONS)-1 do begin 
              bcdpixel=(*prs[j].PIXELS)[k] 
              
              ;; associated polygon (2xn list) this pixel got clipped to
              ;; by the PR on the detector grid
              poly=*(*prs[j].POLYGONS)[k]
           
              ;; Offset to poly's center
              poly=poly-rebin(prs[j].cen,size(poly,/DIMENSIONS))
           
              ;; Rotate this polygon to the cube sky grid, if necessary
              if angle ne 0.0 then poly=rot#poly

              ;; Offset the polygon correctly into the sky grid
              poly=poly+rebin(offset,size(poly,/DIMENSIONS))
              if j eq 1 then begin 
                 plots,[reform(poly[0,*]),poly[0,0]], $
                       [reform(poly[1,*]),poly[1,0]],COLOR=1+i mod 4
                 plots,offset,PSYM=4,COLOR=1+i mod 4
                 wait,0
              endif 
              
              ;; Clip it against the sky grid
              cube_spatial_pix=polyfillaa(reform(poly[0,*]),reform(poly[1,*]),$
                                          self.CUBE_SIZE[0],self.CUBE_SIZE[1],$
                                          AREAS=areas)
           
              if cube_spatial_pix[0] eq -1 then continue
;                  print,FORMAT='("Not hitting cube for pixel: "' + $
;                        ',I0,",",I0," -- step [",I0,",",I0,"]")', $
;                        bcdpixel mod 128, bcdpixel/128, $
;                        (*self.DR)[i].COLUMN,(*self.DR)[i].ROW
;                  print, poly
;                  print,'  original:'
;                  print,*(*prs[j].POLYGONS)[k]
;                 continue ;; why isn't our cube big enough?
;              endif
              
              ncp=n_elements(cube_spatial_pix)
              ;; Add space to account list, large chunks at a time
              if acc_ind+ncp ge nacc then begin 
                 account=[account,replicate({CUBE_ACCOUNT_LIST},nacc)]
                 nacc=2*nacc
              endif
              account[acc_ind:acc_ind+ncp-1].cube_pix=cube_spatial_pix
              account[acc_ind:acc_ind+ncp-1].cube_plane=j ;just this order's
              account[acc_ind:acc_ind+ncp-1].bcd_pix=bcdpixel
              account[acc_ind:acc_ind+ncp-1].areas=areas
              acc_ind=acc_ind+ncp
           endfor
        endfor
        account=account[0:acc_ind-1] ; trim this order's account to size
        
        ;; Merge this account into the full cube account
        self->MergeAccount,i,ord,account
     endfor
     ;; Compute the total reverse index of the full cube indices for
     ;; this DR's accounting.  E.g. the account records from this BCD
     ;; pertaining to cube pixel z are: ri[ri[z]:ri[z+1]-1].
     h=histogram((*(*self.DR)[i].ACCOUNT).cube_pix+ $
                 (*(*self.DR)[i].ACCOUNT).cube_plane* $
                 self.CUBE_SIZE[0]*self.CUBE_SIZE[1], $
                 OMIN=om,REVERSE_INDICES=ri)
     (*self.DR)[i].REV_ACCOUNT=ptr_new(ri,/NO_COPY)
     (*self.DR)[i].REV_CNT=n_elements(h) 
     (*self.DR)[i].REV_MIN=om
  endfor

  self.ACCOUNTS_VALID=1b
end

;=============================================================================
;  BuildCube - Assemble the Cube from the accounting information, the
;              BCD data and the uncertainties.
;=============================================================================
pro cubeProj::BuildCube
  if NOT ptr_valid(self.DR) then return
  if NOT self.ACCOUNTS_VALID OR $
     NOT array_equal(ptr_valid((*self.DR).ACCOUNT),1b) then self->BuildAccount
  
  cube=make_array(self.CUBE_SIZE,/FLOAT,VALUE=!VALUES.F_NAN)
  big_bcd=(*self.DR).BCD        ;pre-cache all the BCDs.. big (128KB*n_bcd)!
  areas=make_array(self.CUBE_SIZE,/FLOAT,VALUE=0.0)

  bcd_size=128L*128L*2L         ;each BCD is two planes XXX not for MIPSSED
  error_offset=128L*128L        ;the error plane is second
  
  for dr=0,n_elements(*self.DR)-1 do begin 
     if (*self.DR)[dr].DISABLED then continue
     acct=*(*self.DR)[dr].ACCOUNT
     rev_acc=*(*self.DR)[dr].REV_ACCOUNT
     rev_min=(*self.DR)[dr].REV_MIN
     ;; Use the reverse account to populate the cube
     for i=0L,(*self.DR)[dr].REV_CNT-1 do begin 
        if rev_acc[i] eq rev_acc[i+1] then continue ;nothing for this pixel
        pix_acc=acct[rev_acc[rev_acc[i]:rev_acc[i+1]-1]]
        ;; XXX Error weighting, other alternatives
        ;;  need all in one place? ... e.g trimmed mean?
        cube[rev_min+i]=(finite(cube[rev_min+i])?cube[rev_min+i]:0.0) + $
           total(big_bcd[dr*bcd_size+pix_acc.BCD_PIX] * pix_acc.AREAS)
        areas[rev_min+i]=areas[rev_min+i]+total(pix_acc.AREAS)
     endfor
  endfor 
  
  areas=areas>1.e-7
  ptr_free,self.CUBE,self.ERR
  self.CUBE=ptr_new(cube/areas,/NO_COPY)
end

;=============================================================================
;  Stack - Extract a stack from the built cube between two wavelength
;          intervals, optionally weighting the individual planes with
;          WEIGHTS
;=============================================================================
function cubeProj::Stack,lam1,lam2,WEIGHTS=weights
  if NOT ptr_valid(self.CUBE) then message,'No cube to stack'
  p1=value_locate(*self.WAVELENGTH,lam1)+1 ;not less than lam1
  p2=value_locate(*self.WAVELENGTH,lam2) ;not greater than lam2
  nw=n_elements(weights) 
  if nw ne 0 then begin 
     if nw ne p2-p1+1 then message,'Incorrect number of weights passed'
     return,total(*self.CUBE[*,*,p1:p2] * $
                  rebin(reform(weights,[1,1,nw]),[self.CUBE_SIZE[0:1],nw], $
                        /SAMPLE), 3)
  endif 
  return,total(*self.CUBE[*,*,p1:p2],3)
end


;=============================================================================
;  N_Records
;=============================================================================
function cubeProj::N_Records
   if ptr_valid(self.DR) then return,n_elements(*self.DR) else return,0
end

;=============================================================================
;  AddData - Add one or more BCD data files to the cube.
;=============================================================================
pro cubeProj::AddData, files,DIR=dir,PATTERN=pat,_EXTRA=e
  if n_elements(files) eq 0 AND n_elements(pat) ne 0 then begin 
     if n_elements(dir) ne 0 then $
        files=findfile(dir+pat) $
     else files=findfile(pat)   ;assume pat contains the pattern
  endif 
  if n_elements(files) eq 0 then message,'No files or data passed'
  for i=0,n_elements(files)-1 do begin 
     bcd=readfits(files[i],header,/SILENT)
     self->AddBCD,bcd,header,FILE=files[i],_EXTRA=e
  endfor
end

;=============================================================================
;  AddBCD - Add a bcd image to the cube, optionally overriding the
;           header derived values.
;=============================================================================
pro cubeProj::AddBCD,bcd,header, FILE=file,ID=id,ERROR=err,EXP=exp, $
                     COLUMN=col, ROW=row, CMD_POS=cpos, REC_POS=rpos, PA=pa
  s=size(bcd,/DIMENSIONS)
  rec={CUBE_DR}
  if n_elements(s) eq 3 then begin 
     if array_equal(s,[128,128,2]) then begin 
        rec.BCD=bcd
     endif else message,'Incorrect BCD dimensions'
  endif else if n_elements(s) eq 2 then begin 
     if array_equal(s,[128,128]) then begin 
        if array_equal(size(error,/DIMENSIONS),[128,128]) then $
           rec.BCD=[ [[bcd]], [[error]] ] else $
           rec.BCD=[ [[bcd]], [[make_array(/FLOAT,128,128,VALUE=1.)]] ]
     endif else message,"BCD's must be 128x128[x2]"
  endif else message,"BCD's must be 128x128[x2]"
  if size(header,/TYPE) ne 7 then message,'Header must be a string array'
  rec.header=ptr_new(header)
  if n_elements(file) ne 0 then rec.file=file
  
  if n_elements(id) ne 0 then rec.id=id else if rec.file then begin 
     id=strmid(rec.file,strpos(rec.file,path_sep(),/REVERSE_SEARCH)+1)
     suffix=strpos(id,".fits",/REVERSE_SEARCH)
     if suffix[0] ne -1 then id=strmid(id,0,suffix)
     rec.id=id
  endif 
  
  if n_elements(exp) ne 0 then rec.EXP=exp else $
     rec.EXP=sxpar(header,'EXPNUM')
  if n_elements(cycle) ne 0 then rec.cycle=cycle else $
     rec.CYCLE=sxpar(header,'CYCLENUM')
  if n_elements(col) ne 0 then rec.COLUMN=col else $
     rec.COLUMN=sxpar(header,'COLUMN')
  if n_elements(row) ne 0 then rec.ROW=row else $
     rec.ROW=sxpar(header,'ROW')
  if n_elements(cpos) eq 2 then rec.CMD_POS=cpos else $
     rec.CMD_POS=[sxpar(header,'RA_CMD'),sxpar(header,'DEC_CMD')]
  if n_elements(rpos) eq 3 then rec.REC_POS=rpos else $
     rec.REC_POS=sxpar(header,'CRVAL*')
  if n_elements(pa) ne 0 then rec.PA=pa else $
     rec.PA=sxpar(header,'PA')
  if array_equal(self.slit_size,0.0) then $
     self.slit_size=[sxpar(header,'SLITSZ2'),sxpar(header,'SLITSZ1')]
  rec.NCYCLES=sxpar(header,'NCYCLES')
  fovid=sxpar(header,'FOVID',COUNT=cnt)
  if cnt gt 0 then begin 
     fovname=irs_fov(fovid,ORDER=target_order,POSITION=target_pos)
     rec.TARGET_ORDER=target_order
     rec.TARGET_POS=target_pos
  endif 
  
  if ptr_valid(self.DR) then *self.DR=[*self.DR,rec] else $
     self.DR=ptr_new(rec,/NO_COPY)
end

;=============================================================================
;  RemoveBCD - Remove one or more BCD's
;=============================================================================
pro cubeProj::RemoveBCD,recs
  heap_free,(*self.DR)[recs]
  keep=where(histogram(recs,MIN=0,MAX=self->N_Records()) eq 0,cnt)
  if cnt ne 0 then begin
     (*self.DR)=(*self.DR)[keep] 
     wh=where(keep eq self.cur,cnt) ;see if current is kept... shift it
     if cnt ne 0 then self.cur=wh[0] else self.cur=-1
  endif else ptr_free,self.DR
  self->UpdateList
  self.Changed=1b               ;but accounts remain valid!
end

;=============================================================================
;  Sort - Sort the Data records. 
;=============================================================================
pro scoreProj::Sort,sort
  n=self->N_Records()
  if n le 1 then return         ;no sort for one only
  if n_elements(sort) ne 0 then sort=0>sort<5
  
  case sort of
     0: s=sort((*self.DR).ID)
     1: s=sort((*self.DR).Object)
     2: s=sort((*self.DR).ITIME)
     3: s=sort((*self.DR).DATE)
     4: s=sort((*self.DR).Date)
  endcase
  *self.DR=(*self.DR)[s]        ;rearrange
end

;=============================================================================
;  Cleanup 
;=============================================================================
pro cubeProj::Cleanup
  ;; !!! Write a real cleanup.
  heap_free,self.DR
  heap_free,self.MERGE
  ptr_free,self.APERTURE,self.CUBE,self.ERR,self.wInfo
end

;=============================================================================
;  Init 
;=============================================================================
function cubeProj::Init, name, _EXTRA=e
  if (self->ObjMsg::Init(_EXTRA=e) ne 1) then return,0 ;chain up (add msglist)
  self.cur=-1                   ;none yet set current
  self.Changed=0b               ;coming into existence doesn't count
  self.SpaceSaver=1b            ;use spacesaver by default
  if n_elements(e) ne 0 then self->SetProperty,_EXTRA=e
  return,1
end

;=============================================================================
;  CubeProj - IRS Spectral (+MIPS SED) Cubes
;=============================================================================
pro cubeProj__define
  c={cubeProj, $
     INHERITS ObjMsg, $         ;make it an object messanger
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
     CUBE: ptr_new(),$          ;a pointer to the nxmxl data cube
     ERR:  ptr_new(),$          ;a pointer to the nxmxl error cube
;     ITIME: ptr_new(),$         ;nxmxl integration time per pixel
     CUBE_SIZE: [0L,0L,0L],$    ;the size of the cube, (n,m,l)
     CUBE_DATE: 0.0D, $         ;date the cube was assembled (JULIAN)
     NSTEP:[0L,0L], $           ;parallel (col), perpendicular (row) steps
     STEP_SIZE: [0.0D,0.0D], $  ;parallel, perpendicular slit step sizes (deg)
     SLIT_SIZE:[0.0D,0.0D], $   ;the parallel, perpendicular slit size (deg)
     PLATE_SCALE:0.0D, $        ;the plate scale (degrees/pixel)
     PR_SIZE:[0.0,0.0], $       ;the , parallel (long axis), perpendicular
                                ; (short axis) size of the PRs to use (pixels)
     WAVELENGTH: ptr_new(), $   ;the cube's wavelength list
     POSITION:[0.0D,0.0D], $    ;optimized position of the cube center
     PA:0.0D, $                 ;optimized position angle of the cube
     MERGE:ptr_new(),$          ;a set of records to aid merging the orders
                                ;  {offset,inds,wave,to,frac}
     cur:0, $                   ;the current record.
     cal:obj_new(), $           ;the irs_calib object.
     cal_file:'', $             ;the calibration file used (if not a full
                                ; directory, in the "calib/" subdir)
     Changed:0b, $              ;if the project is changed since last saved.
     SpaceSaver:0b,  $          ;whether to use space saver -- omits saving raw
     SaveFile:'', $             ;the file it was saved to
     wInfo:ptr_new()}           ;the widget info struct.... a diconnectable ptr
  
  acc={CUBE_ACCOUNT, $
       DR: 0L, $                ;index into the DR list
       pixel: 0L, $             ;which pixel in the BCD overlapped
       frac: 0.0}               ;the fraction of overlap
  
  ;; The data structure for each input BCD
  rec={CUBE_DR, $
       ID:'',$                  ;A unique (hopefully) ID
       file:'', $               ;the original file read for this data
       DISABLED: 0b, $          ;whether this DR is disabled
       ACCOUNT: ptr_new(), $    ;list of {CUBE_PLANE,CUBE_PIX,BCD_PIX,AREAS}
       REV_ACCOUNT: ptr_new(),$ ;reverse indices of the CUBE_INDEX histogram
       REV_MIN:0L, $            ;minimum cube index affected
       REV_CNT:0L, $            ;how many bins in the cube_index histogram?
       CMD_POS: [0.0D,0.0D], $  ;Commanded RA,DEC position of slit center
       REC_POS: [0.0D,0.0D],$   ;Reconstructed RA,DEC pos of slit center.
       TARGET_ORDER: 0, $       ;which order was targetted by pointing
                                ;  (or 0 for module center targetting)
       TARGET_POS: 0, $         ;For low-res: 0, 1, or 2 for centered on
                                ; module, slit position 1, or slit position 2
       PA: 0.0D, $              ;Position angle of slit E of N
       BCD: fltarr(128,128,2), $ ;the 2 plane BCD (error plane second).
       EXP: 0L, $               ;the exposure number in the mapping sequence
       CYCLE:0L, $              ;the cycle number at this position
       NCYCLES:0L,$             ;the number of cycles at this position
       COLUMN: 0L,$             ;the step number perpendicular to the slit
       ROW:0L, $                ;the step number parallel to the slit
       Date:0.0D, $             ;the date this BCD was added
       HEADER: ptr_new()}       ;a pointer to a string array.
end

; NOTES:
;
; speedup possibilities:
;
;   Drop the ragged 4d array format for the accounting cubes.  Cube
;   pixels can have 0 or more contributing pixels.  Code the index as
;   a single integer master BCD index into the large 128x128x(2*n*bcd)
;   array, e.g.:
;
;      DR: 4, PIXEL:5206 ====> INDEX:4*128*128*2+5206=136278L
;   
;   cache the 128x128x(2*n_bcd) *(self.DR).BCD array prior to building
;   the cube.  Index it in parallel using the full index as developed
;   above.  This index will change when you add or remove cubes (so
;   will the cube number, so it's not such a big deal).  Do you always
;   have to rebuild the ACCOUNT when you make a new cube?  What's the
;   use of separating it then?  I guess you could *disable* cubes
;   without rebuilding the account, but adding or removing any messes
;   things up.  It seems account isn't as flexible as I had thought.
;   If there are different options for weighting the various
;   contributing data, at least *that* doesn't mess up the account.
;
;   Make a flat 4D hypercube of accounting info with up to 4 entries
;   per pixel.  Additional entries (beyond 4) are stored as a big list
;   of form:
;
;     [ {cube_pixel, master_bcd_index, fraction}, ..., ]
;
;   Too much of a pain.  What about using this type of list for the
;   full accounting structure?  Building the cube then would be as
;   simple then as:
;
;     cube[cube_pixel]=cube[cube_pixel]+big_bcd[master_bcd_index]*fraction
;   
;   except this doesn't work, thanks to repeated indices being ignored
;   in assignment (thanks IDL).  Also, when you want the accounting
;   information for a pixel, you have to search through the *long*
;   list.  Oh for the want of hashes.... AHA... use the sparse array
;   technique, with the slight difference that you compute:
;
;     val=big_bcd[master_bcd_index]*frac
;
;   it would go something like:
;
;     At cube build time:
;     
;     super_ind=acct.DR*128L*128L*2L+acct.PIXEL
;     mx=max(super_ind)
;     h1=histogram(super_ind,MIN=0,REVERSE_INDICES=ri1) 
;     col=ri1[n_elements(h1)+1:*]
;     h2=histogram(total(h1,/cumulative)-1,MIN=0,reverse_indices=ri2)
;     row=ri2[0:n_elements(h2)-1]-ri2[0]+om ; chunk indices = row number
;     sparse_array=sprsin(col,row,replicate(1.,n_ind),(mx+1)>n_ind)
;     if mx ge n_ind then $
;      vec4=sprsax(sparse_array,[val,replicate(0.,mx+1-n_ind)]) $
;     else vec4=(sprsax(sparse_array,val))[0:mx]
;
;   Instead of using 1. as the value to multiply by, you could use:
;
;     area_i*time_i/sigma_i^2, and then apply a sparse array of 1.'s
;     to this quantity as the X vector to generate the normalizing sum
;     over weights.
;
;   Hmmm... well, just pre-caching the big_bcd was fast enough.  Now
;   we need to concentrate on avoiding the need to rebuild the
;   Accounting cube.  With no histogram, when you delete a BCD, you
;   need to rebuild.  You could have an "ACTIVE" flag for each DR to
;   multiply in to both the numerator and denominator of the total.
;   This would allow you to temporarily disable a certain BCD or group
;   of BCDs without rebuilding the account.  To *add* BCD's, you could
;   do a partial accounting build, where you just add to the existing
;   accounting information for those smaller number of pixels in the
;   cube which are affected.  This will be especially useful for very
;   large cubes.  It only works if the new BCD fits in the same cube
;   as the before, and doesn't bump the map's size.  Hmm...
;
;   Also, with no histogram, we won't easily be able to de-overlap 2xn
;   PRs by summing over fractions requested for a single pixel.  This
;   is probably required.  
;     
;   When deleting BCD's, you can use the reverse indices to find and
;   re-calculate only those account list entries which are affected!
;
;   The 4D + extras is inconvenient too: you index and search a
;   (presumably smaller) list for any extras to append.
;
;
;   Could use a fast histogram method to search the accounting list.
