;+
; NAME:  
;
;    IRS_Calib
;
; DESCRIPTION:
;    
;    A class defining IRS calibration objects, each of which hold a
;    specific set of WAVSAMP, order, wavelength, and tilt solution
;    calibration data for all orders of all modules.
;    
; CATEGORY:
;
;    IRS Spectral Reduction, Analysis and Processing.
;    Calibration
;    	
; COMMON BLOCKS:
;
;    CUBISM_DIR: Included through file cubism_dir.pro.  Defines the path
;       location of the IRS calibration directory.  This path,
;       irs_calib_dir, is auto-defined if it doesn't yet exist.
;
; METHODS:
;
;    Init:  
;
;       CALLING SEQUENCE:
;
;          obj=obj_new('IRS_Calib',name)
;
;       OPTIONAL INPUT PARAMETERS:
;
;          name: The name of the calibration object.
;
;    (Get|Set)Property:
;  
;	DESCRIPTION:
;
;          Get or Set properties of the calibration object.  See also
;          (Get|Set)Record
;	
;       CALLING SEQUENCE:
;
;          obj->(Get|Set)Property, NAME=
;
;	KEYWORD PARAMETERS:
;
;	   NAME: Input or output of the calibration object's name.
;
;    GetRecord:
;    
;	DESCRIPTION:
;
;          Get a given module's record for a single order, or a new
;          record if none yet exists.
;	
;       CALLING SEQUENCE:
;
;          record=obj->GetRecord(module,order,[/MUST_EXIST])
;
;	INPUT PARAMETERS:
;
;          module: Module for which to return the calibration record.
;            Can either be a string (SL,SH,LL,LH) or integer (0-3,
;            respectively).
;
;          order: Integer order of the module for which a calibration
;            record is requested.  Order "3" represents the bonus in
;            both SL and LL.
;            
;       INPUT KEYWORD PARAMETERS:
;
;          MUST_EXIST: If set, a new record will not be created if it
;             doesn't exist, but instead, -1 will be returned.
;
;	OUTPUTS:
;
;          record: The IRS_CalibRec record structure for the given
;             module and order.
;
;    Orders:
;    
;	DESCRIPTION:
;
;          Return the list of order numbers for a given module.
;          	
;       CALLING SEQUENCE:
;
;          orders=obj->Orders(module)
;          
;	INPUT PARAMETERS:
;
;          module: The number (or name) of the module for which to
;             read calibration data.
;             
;	OUTPUTS:
;
;          orders: The integer list of order numbers present.
;             
;    GetWAVSAMP:
;    
;	DESCRIPTION:
;
;          Return a list of IRS_WAVSAMP_PSEUDORECT structures for
;          the given module, order and aperture.  If, for a given
;          module and order, no clipped IRS_WAVSAMP exists, create
;          one, append it to the list, and return.  WAVSAMP clipping
;          is very expensive.  This serves to cache commonly used
;          apertures for quicker access to extraction boundaries.
;          	
;       CALLING SEQUENCE:
;
;          wavsamp=obj->GetWAVSAMP(module,order, [APERTURE=, /FULL,
;                                  /NO_CACHE,/PIXEL_BASED,PR_WIDTH=])
;          
;	INPUT PARAMETERS:
;
;          module: The number (or name) of the module for which to
;             read calibration data.
;
;          order: Integer order of the module for which a calibration
;            record is requested.  Order "3" represents the bonus in
;            both SL and LL.z
;
;       INPUT KEYWORD PARAMETERS:
;
;          APERTURE: A structure of type IRS_APERTURE.  See
;             IRS_Aperture__define for more information, and
;             irs_aperture for a convenient method to make such
;             aperture.  If omitted, the full, constant length slit
;             aperture is used.
;
;          FULL: If set, the full slit aperture is used no matter what
;             (if anything) is passed in APERTURE.
;
;          NO_CACHE: If set, any WAVSAMP created will not be cached.
;             This helps save memory.
;             
;          PIXEL_BASED: If set, a pixel-based WAVSAMP is returned.  If
;             no full, pixel-based WAVSAMP yet exists from which to
;             generate this WAVSAMP, it is created.
;
;          PR_WIDTH: If PIXEL_BASE is set, the width of the PR in
;             pixels.  Defaults to 1.
;
;          SAVE_POLYGONS: If set, the polygons created by clipping to
;             the detector grid are saved.  These polygons are
;             required for building spectral cubes from step and stare
;             maps.
;
;	OUTPUTS:
;
;          wavsamp: The IRS_WAVSAMP structure for the given module,
;             order and aperture.  See definition at end of file.
;            
;    ReadCalib:
;    
;	DESCRIPTION:
;
;          Read and parse calibration data from files and populate the
;          IRS_Calib object.
;	
;       CALLING SEQUENCE:
;
;          obj->ReadCalib, [module, /ONLY,WAVSAMP_VERSION=,ORDER_VERSION=, $
;                           TILT_VERSION=]
;
;	OPTIONAL INPUT PARAMETERS:
;
;          module: The number (or name) of the module for which to
;             read calibration data.  Defaults to all four modules, if
;             omitted.
;
;       INPUT KEYWORD PARAMETERS:
;
;          ONLY: If set, only versions explicitly passed with the
;             *_VERSION keywords described below will be read, as
;             opposed to the normal case of the *latest* version being
;             used if none is specified.
;             
;          WAVSAMP_VERSION: The version number of the WAVSAMP file to
;             use.  Defaults to the latest version, unless ONLY is
;             set, in which case no WAVSAMP data is read or set for
;             the module(s) unless specified here.  A version of 0
;             (zero) means use the latest version.
;
;          ORDER_VERSION: The version number of the order
;             position/wavelength solution file to use.  Defaults to
;             the latest version, unless ONLY is set, in which case no
;             ORDER data is read or set for the module(s) unless
;             specified here.  A version of 0 (zero) means use the
;             latest version.
;
;          TILT_VERSION: The version number of the tilt solution file
;             to use.  Defaults to the latest version, unless ONLY is
;             set, in which case no TILT data is read or set for the
;             module(s) unless specified here.  A version of 0 (zero)
;             means use the latest version.
;            
;    SetRecord:
;    
;	DESCRIPTION:
;
;          Set a given module's record for a single order, overwriting
;          an existing record for the same module/order, if any.
;	
;       CALLING SEQUENCE:
;
;          obj->SetRecord, record
;
;	INPUT PARAMETERS:
;
;          record: The IRS_CalibRec record structure to be recorded
;            in the object.
;            
;          
; PROCEDURE:
;
;    read_ipac_table: For reading files in the form of IPAC tables.
;    
; NOTES:
;    
;    IRS calibration data is generated at the SSC, and used by IRS
;    for spectral extractions.  The three important types of
;    calibration files, all of which come in the form of an IPAC
;    table, are:
;
;     - WAVSAMP: A list of psuedo-rectangles each corresponding to a
;       single spectral resolution element, encompassing the entire
;       slit.
;
;     - ORDER position and WAVELENGTH solution: A single file which is
;       generated by the ORDFIND calibration tool at SSC, and further
;       refined by the WAVELENGTH CALIBRATION thread, it
;       simultaneously gives polynomial fits to the order positions on
;       the array, and the wavelength solution along the order.
;
;     - TILT solution: A file which lists the order by order tilt
;       solutions, i.e. the angle the slit image makes with respect to
;       the detector rows.
;     
;    The traditional WAVSAMP is driven by wavelength: a starting
;    wavelength is picked, and each subsequent wavelength is computed
;    using a resolution model.  It does not concern itself with the
;    pixel boundaries, instead computing these as an artifact.
;    Another WAVSAMP formulation can be computed by demanding fixed
;    width samples, aligned with the pixel grid as nearly as possible.
;    This form of WAVSAMP is called pixel-based, and is available here
;    (look for the PIXEL_BASED keyword).  The pixel-based WAVSAMP
;    offers some advantages for optimal extraction, and is required
;    for spectral-mapping data sets.  Both types can be accomodated by
;    a single IRS_Calib object.
;
;    The IRS_Calib class should encapsulate all calibration data
;    necessary for full reduction of IRS spectra, and should be
;    expanded as appropriate.  Each individual IRS_Calib object
;    represents a particular snapshot of calibration data, which may
;    evolve over time, due to updates in the calibration thread, or
;    changing instrumental performance.  
;
;    Pre-made IRS_Calib object sets can be found in the subdirectory
;    calib/sets in this distribution.
;
; INHERITANCE TREE:
;
;     IRS_Calib
;
; EXAMPLE:
; 
;     Populate a new cal object with all the latest calibration data:
;     
;    cal=obj_new('IRS_Calib','2002-04-01: Pre-Launch')
;    cal->ReadCalib
;
;     Update the WAVSAMP to the latest version for long-low only:
;     
;    cal=irs_restore_calib('irs_2006_01_01.cal')
;    a->ReadCalib,'LL',WAVSAMP_VERSION=0,/ONLY
;
; MODIFICATION HISTORY:
;
;       2004-03-12 (J.D. Smith): Fit ORDFIND,LINETILT polys from
;          WAVSAMP.  Apertures can now be wavelength scaled.
;	2002-08-23 (J.D. Smith): Added variable-width pixel-based WAVSAMPs.
;	2002-04-30 (J.D. Smith): Added pixel-based WAVSAMPs.	
;       2001-12-08 (J.D. Smith): Written.
; 
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2001-2005 J.D. Smith
;
;  This file is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published
;  by the Free Software Foundation; either version 2, or (at your
;  option) any later version.
;  
;  This file is distributed in the hope that it will be useful, but
;  WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;  General Public License for more details.
;  
;  You should have received a copy of the GNU General Public License
;  along with this file; see the file COPYING.  If not, write to the
;  Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;  Boston, MA 02111-1307, USA.
;
;##############################################################################

;=============================================================================
;  GetProperty - Get object properties
;=============================================================================
pro IRS_Calib::GetProperty, module, order, NAME=name,SLIT_LENGTH=sl, $
                            WAVE_CENTER=wc,WAV_MIN=wmn,WAV_MAX=wmx, $
                            PLATE_SCALE=ps,PIXEL_OMEGA=po,PMASK=pmask, $
                            FLUXCON=fluxcon,KEY_WAV=fluxcon_kw,TUNE=tune, $
                            SLCF=slcf
  if arg_present(pmask) then begin 
     m=irs_module(module)
     pmask=self.PMASK[m]
  endif
  if arg_present(slcf) then begin 
     m=irs_module(module)
     slcf=self.SLCF[m]
  endif
  if arg_present(name) then name=self.name
  if n_elements(module) ne 0 then $
     rec=self->GetRecord(module,order,/MUST_EXIST)
  if size(rec,/TYPE) ne 8 then return
  if arg_present(sl) then   sl=rec.SLIT_LENGTH
  if arg_present(wc) then   wc=rec.WAV_CENTER
  if arg_present(wmn) then  wmn=rec.WAV_MIN
  if arg_present(wmx) then  wmx=rec.WAV_MAX
  if arg_present(ps) then   ps=rec.PLATE_SCALE
  if arg_present(po) then po=rec.PIXEL_OMEGA
  if arg_present(fluxcon) then fluxcon=rec.FLUXCON
  if arg_present(fluxcon_kw) then fluxcon_kw=rec.FLUXCON_KEY_WAV
  if arg_present(tune) then tune=rec.TUNE
  ;;if arg_present(sz) then sz=self.detsize[*,0>irs_module(module)<5]
end

;=============================================================================
;  SetProperty - Set object properties
;=============================================================================
pro IRS_Calib::SetProperty,NAME=name
  if n_elements(name) eq 0 then self.name=name
end

;=============================================================================
;  Save - Save this cal set
;=============================================================================
pro IRS_Calib::Save,file,_EXTRA=e
  if self.Name eq '' then self.Name=filestrip(file)
  save,self,FILENAME=file,_EXTRA=e
end

;=============================================================================
;  Print - Print a listing of what this cal object contains.
;=============================================================================
pro IRS_Calib::Print,modules,orders,_EXTRA=e
  print,transpose(self->Info(modules,orders,_EXTRA=e))
end

function IRS_Calib::Info, modules, orders, SHORT=short
  if n_elements(modules) eq 0 then modules=indgen(4)
  str=' == IRS Calibration Object: '+self.Name+' =='
  str=[str,'', $
       '  PLATESCALE: '+self.PLATESCALE_FILE, $
       ' FRAME_TABLE: '+self.FRAMETABLE_FILE, $
       ' PIXEL_OMEGA: '+self.PIXEL_OMEGA_FILE]
  for i=0,n_elements(modules)-1 do begin 
     md=irs_module(modules[i])
     module=irs_module(md,/TO_NAME)
     no=ptr_valid(self.cal[md])?n_elements(*self.cal[md]):0
     real_orders=no gt 0?n_elements(self->Orders(md)):no
     str=[str,string(FORMAT='(%"\n ==> Module %s: %s")',module, $
                     (no gt 0?strtrim(real_orders,2)+ $
                      " orders.":" not loaded."))]
     if no eq 0 then continue
     if n_elements(orders) eq 0 then ords=indgen(no) else $
        ords=where_array((*self.cal[md]).ORDER,[orders],no,/SWITCHAB)
     if no eq 0 then continue
     str=[str,'']
     wsf=self.WAVSAMP_FILE[md]
     if wsf ne '' then str=[str,string(FORMAT='(A12,": ",A)', 'WAVSAMP',wsf)]
     orf=self.ORDER_FILE[md]
     if orf ne '' then str=[str,string(FORMAT='(A12,": ",A)', 'ORDER',orf)]
     tif=self.TILT_FILE[md]
     if tif ne '' then str=[str,string(FORMAT='(A12,": ",A)', 'TILT',tif)]
     pmf=self.PMASK_FILE[md]
     if pmf ne '' then str=[str,string(FORMAT='(A12,": ",A)', 'PMASK',pmf)]
     fcf=self.FLUXCON_FILE[md]
     if fcf ne '' then str=[str,string(FORMAT='(A12,": ",A)', 'FLUXCON',fcf)]
     slcff=self.SLCF_FILE[md]
     if slcff ne '' then str=[str,string(FORMAT='(A12,": ",A)', 'SLCF',slcff)]
     wcf=self.WAVECUT_FILE[md]
     if wcf then str=[str,string(FORMAT='(A12,": ",A)','WAVECUT',wcf)]
     str=[str,string(FORMAT='(A39,": ",G12.5)',"Plate Scale (deg)", $
                     (*self.cal[md])[0].PLATE_SCALE)]
     str=[str,string(FORMAT='(A39,": ",G12.5)', $
                     "Pixel Effective Solid Angle (sr)", $
                     (*self.cal[md])[0].PIXEL_OMEGA)]
     
     str=[str,'']
     for j=0,no-1 do begin 
        rec=(*self.cal[md])[ords[j]]
        if rec.order eq 0 then str=[str,"    ==> Target "+module+" center"] $
        else str=[str,string(FORMAT='(%"    ==> Order %2s%d  (%s): ")', $
                             module,rec.order, $
                             rec.Date eq 0.0D?"--":jul2date(rec.Date))]
        nw=ptr_valid(rec.WAVSAMPS)?n_elements(*rec.WAVSAMPS):0
        str=[str, $
             string(FORMAT='("         Position: Z: ",F8.3," Y:",' + $
                    'F8.3," Angle:",F8.3)',rec.TPF_Z,rec.TPF_Y,REC.TPF_ANGLE)]
        if rec.order eq 0 then continue ;just a stub order
        if rec.RecoveredPolys then $
           str=[str,"          Recovered from WAVSAMP:"]
        str=[str, $
             "          A:"+strjoin(string(FORMAT='(G11.3)',rec.A)), $
             "          B:"+strjoin(string(FORMAT='(G11.3)',rec.B)), $
             "          C:"+strjoin(string(FORMAT='(G11.3)',rec.C)), $
             "          SLIT_LENGTH:"+string(FORMAT='(G10.3)', $
                                             rec.SLIT_LENGTH),$
             "          WAVELENGTH(min,center,max):" + $
             string(FORMAT='(3F10.4)',rec.WAV_MIN,rec.WAV_CENTER,rec.WAV_MAX),$
             "          WAVECUT:" + $
             (array_equal(rec.WAVECUT,0.0)?" none": $
              string(FORMAT='(" Low: ",F10.4," High:",F10.4)',rec.WAVECUT)), $
             "          FLUXCON:"+string(FORMAT='(G12.3)',rec.FLUXCON)+ $
             " KEY WAVLENGTH: "+ string(FORMAT='(G8.3)',rec.FLUXCON_KEY_WAV),$
             "          TUNE: "+strjoin(string(FORMAT='(G9.3)',rec.TUNE))]
        if nw gt 0 then $
           str=[str,"          Apertures:"] else $
           str=[str,"       No Apertures"]
        for k=0,nw-1 do begin 
           flags=""
           ws=(*rec.WAVSAMPS)[k]
           nsamp=n_elements(*(*rec.WAVSAMPS)[k].PR)
           
           if ws.FULL then begin 
              aps=string(FORMAT='(%"            FULL         %5.2f->%5.2f'+ $
                         ' -- %03d samples")', $
                         ws.Aperture.low[0],ws.Aperture.high[0],nsamp)
           endif else begin 
              if ws.Aperture.wavscl eq 0 then begin 
                 aps=string(FORMAT='(%"            %5.2f->%5.2f:' + $
                            '%5.2f->%5.2f -- %03d samples")', $
                            ws.Aperture.low,ws.Aperture.high,nsamp)
              endif else begin 
                 aps=string(FORMAT='(%"       WV %5.2f@%5.2fum ' + $
                            'cen:%4.2f -- %03d samples")', $
                            ws.Aperture.scale,nsamp)
              endelse 
           endelse 
           if ws.PIXEL_BASED ne 0.0 then $
              flags=[string(FORMAT='("pixel-based, width=",F5.3)', $
                            ws.PR_WIDTH)]
           if ptr_valid((*ws.PR)[0].POLYGONS) then $
              if flags[0] eq "" then flags=["with polygons"] else $
              flags=[flags,"with polygons"]
           nf=n_elements(flags) 
           if flags[0] ne "" and nf gt 0 then $
              aps=aps+" ("+string(FORMAT='('+ $
                                  strtrim(n_elements(flags),2)+ $
                                  '(A,:,", "))',flags)+")"
           str=[str,aps]
        endfor 
     endfor
  endfor
  return,str
end

;=============================================================================
;  Orders - Return the list of orders for a given module
;=============================================================================
function IRS_Calib::Orders,module
  m=irs_module(module)
  if NOT ptr_valid(self.cal[m]) then return,-1
  ords=(*self.cal[m]).ORDER
  wh=where(ords ne 0,cnt)
  if cnt gt 0 then return,ords[wh] else return,-1
end

;=============================================================================
;  FindWAVSAMP - Find and return the indices of WAVSAMPs matching the
;                criteria passed. The module and order arguments are
;                required, but if APERTURE, WAVELENGTH_SCALED, and
;                FULL are omitted, matching WAVSAMPs of any aperture
;                in that record will be returned.  The default is
;                non-pixelbased, or, when PIXEL_BASED is set, a
;                PR_WIDTH of 1.  If ALL is set, no defaults are put in
;                place, only passed options specify find keys.
;=============================================================================
function IRS_Calib::FindWAVSAMP, module, order, APERTURE=aperture, $
                                 COUNT=nsamp, PIXEL_BASED=pb, PR_WIDTH=width, $
                                 FULL=full, RECORD=rec, $
                                 WAVELENGTH_SCALED=wvscld,ALL=all
  rec=self->GetRecord(module,order,/MUST_EXIST)
  nsamp=0
  if ~ptr_valid(rec.WAVSAMPS) then return,-1
  
  if ~keyword_set(all) then begin 
     if n_elements(pb) eq 0 then pb=0b ;default to non-pb
     if n_elements(width) eq 0 then width=1.0 ;Default to 1xn PRs
  endif 
  
  ;; Find specified aperture, or any aperture
  if n_elements(aperture) ne 0 then aper=aperture
  
  wh_pix=indgen(n_elements(*rec.WAVSAMPS))
  
  if n_elements(pb) gt 0 then begin 
     ;; Find WAVSAMPs of the right type (pixel-based or traditional)
     wh_pb=where((*rec.WAVSAMPS)[wh_pix].PIXEL_BASED eq keyword_set(pb),nsamp)
     if nsamp gt 0 then wh_pix=wh_pix[wh_pb] else return,-1
  endif
  
  eps=1.e-5                 ;match tolerance
  
  ;; Find WAVSAMP's with the right slit width (if pixel-based)
  if n_elements(width) gt 0 && keyword_set(pb) then begin 
     wh_width=where(abs((*rec.WAVSAMPS)[wh_pix].PR_WIDTH-width) $
                    le eps, nsamp)
     if nsamp gt 0 then wh_pix=wh_pix[wh_width] else return,-1
  endif 
  
  ;; If looking for full, return it...
  if keyword_set(full) then begin 
     wh_full=where((*rec.WAVSAMPS)[wh_pix].FULL,nsamp)
     if nsamp gt 0 then return,wh_pix[wh_full] else return,-1
  endif 

  ;; Find WAVSAMP's with matching wavelength scaling and reference, or
  ;; search for one of this type already cached with this aperture
  if keyword_set(wvscld) then begin 
     wh_scale=where(total(abs((*rec.WAVSAMPS)[wh_pix].WAVE_SCALED- $
                              rebin(wvsld,2,nsamp)) le eps,1) eq 2,nsamp)
     if nsamp gt 0 then wh_pix=wh_pix[wh_scale] else return,-1
  endif else if n_elements(aper) ne 0 then begin 
     aps=(*rec.WAVSAMPS)[wh_pix].Aperture
     wh_ap=where(total(abs(aps.low- rebin(aper.low,2,nsamp))  le eps AND $
                       abs(aps.high-rebin(aper.high,2,nsamp)) le eps,1) eq 2 $
                 AND $
                 total(abs(aps.scale- $
                           rebin(aper.scale,3,nsamp)) le eps,1) eq 3,nsamp)

     if nsamp gt 0 then wh_pix=wh_pix[wh_ap] else return,-1
  endif 

  return,wh_pix
end


;=============================================================================
;  GetWAVSAMP - Generate a list of IRS_WAVSAMP_PSEUDORECT structures
;               for a given module, order, and APERTURE (or the FULL
;               aperture), with the option for PIXEL_BASED WAVSAMPs.
;               If a PIXEL_BASED clip is requested, and the FULL
;               PIXEL_BASED WAVSAMP does not exist, it is created and
;               cached.
;
;               To return a wavsamp which scales in size with
;               wavelength, use, e.g., WAVELENGTH_SCALED=[16.,4] for a
;               four pixel width at 16um, scaling linearly with
;               wavelength within the order.
;  
;               The newly created WAVSAMP is cached for fast recovery,
;               unless NO_CACHE is set.  If WAVECUT is passed, the
;               pseudo_rects are trimmed to the WAVECUT wavelengths.
;=============================================================================
function IRS_Calib::GetWAVSAMP, module, order, APERTURE=aperture, FULL=full, $
                                PIXEL_BASED=pb, PR_WIDTH=width, $
                                NO_CACHE=nc, SAVE_POLYGONS=sp, $
                                WAVELENGTH_SCALED=wvscld,WAVECUT=wavecut
  rec=self->GetRecord(module,order,/MUST_EXIST)
  ws=self->FindWAVSAMP(module,order,APERTURE=aperture,COUNT=nsamp,FULL=full, $
                       PIXEL_BASED=pb, PR_WIDTH=width,WAVELENGTH_SCALED=wvscld)
  
  if ~keyword_set(sp) then begin ;no polygons requested
     ;; return the first match
     if nsamp gt 0 then prs=*(*rec.WAVSAMPS)[ws[0]].PR 
  endif else begin 
     for i=0,nsamp-1 do begin 
        ;; return the first match with saved polygons
        pr=(*rec.WAVSAMPS)[ws[i]].PR
        if ptr_valid((*pr)[0].POLYGONS) then begin 
           prs=*pr
           break
        endif 
     endfor 
     ;; free all these matches, to create them again below *with* POLYs
     if nsamp gt 0 && n_elements(prs) eq 0 then $
        self->FreeWAVSAMP,RECORD=rec,INDEX=ws
  endelse
  
  
  if n_elements(prs) eq 0 then begin 
     ;; No clip is cached for this aperture, so make one
     ws=self->Clip(module,order,APERTURE=aperture,PIXEL_BASED=pb, $
                   PR_WIDTH=width,SAVE_POLYGONS=sp,FULL=full,RECORD=rec)
     if size(ws,/TYPE) ne 8 then message,'Error clipping WAVSAMP.'
     
     prs=*ws.PR
     ;; And add it to the cache for rapid recovery, unless asked not to
     if ~keyword_set(nc) then begin 
        if ptr_valid(rec.WAVSAMPS) then begin 
           ;; Only add it if it's a new, distinct WAVSAMP record
           wh=where((*rec.WAVSAMPS).PR eq ws.PR,cnt)
           if cnt eq 0 then *rec.WAVSAMPS=[*rec.WAVSAMPS,ws] 
        endif else rec.WAVSAMPS=ptr_new(ws)
     endif 
     self->SetRecord,rec
  endif 
  
  ;; Trim wavelengths if requested
  if keyword_set(wavecut) then begin 
     if array_equal(rec.WAVECUT,0.0) then $
        message,'No WAVECUT wavelength trim data loaded in calib set.'
     low=min(rec.WAVECUT,MAX=high)
     wh=where(prs.lambda gt low AND prs.lambda lt high,cnt)
     if cnt eq 0 then message,'WAVECUT Error: No PRs remain'
     prs=prs[wh]
  endif 
  
  ;; Return the list of pseudo-rects
  return,prs
end

;=============================================================================
;  FreeWAVSAMP - Remove one or more WAVSAMPS from a record, matching
;                from FindWAVSAMP, or those specified with indices
;                provided by optional keyword INDEX.
;=============================================================================
pro IRS_Calib::FreeWAVSAMP, module, order,RECORD=rec,INDEX=inds, _EXTRA=e
  if n_elements(rec) eq 0 then rec=self->GetRecord(module,order,/MUST_EXIST)
  if size(rec,/TYPE) ne 8 then return ;nothing to delete
  if NOT ptr_valid(rec.WAVSAMPS) then return
  
  if n_elements(inds) eq 0 then begin 
     if n_elements(module) eq 0 then begin 
        module=rec.module
        order=rec.order
     endif 
     inds=self->FindWAVSAMP(module,order,COUNT=nsamp,_EXTRA=e)
     if nsamp eq 0 then return
  endif 
  
  ;; Normalize the indices
  mask=bytarr(n_elements(*rec.WAVSAMPS))
  mask[inds]=1b
  inds=where(mask,COMPLEMENT=good,NCOMPLEMENT=ngood)
  self->CleanWAVSAMP,(*rec.WAVSAMPS)[inds]
  
  if ngood eq 0 then ptr_free,rec.WAVSAMPS else $
     *rec.WAVSAMPS=(*rec.WAVSAMPS)[good]
  
  self->SetRecord,rec
end 

;=============================================================================
;  PixelWAVSAMP - Generate a list of pixel-based
;                 IRS_WAVSAMP_PSEUDORECT structures for a given module
;                 and order, for the FULL aperture, and the PR_WIDTH
;                 indicated (defaults to 1).  In contrast to the
;                 SSC-supplied WAVSAMP, these have fixed sample
;                 widths, and align as closely as possible to the
;                 pixel boundaries.  Set this new WAVSAMP into the
;                 record, and clip the full version.
;
;                 This will add a full pixel-based WAVSAMP to all records:
;
;                 for md=0,3 do begin & ords=c->Orders(md) &
;                 for ord=0,n_elements(ords)-1 do
;                 c->PixelWAVSAMP,md,ords[ord] &
;                 endfor
;
;                 save,c,FILENAME='irs_2004_01_01-pb.cal',/COMPRESS
;=============================================================================
pro IRS_Calib::PixelWAVSAMP, module, order,PR_WIDTH=width, _EXTRA=e
  rec=self->GetRecord(module,order,/MUST_EXIST)
  B=rec.B
  last=5
  for last=5,0,-1 do if B[last] ne 0. then break
  B=B[0:last]
  
  if n_elements(width) eq 0 then width=1.
  
  ;; Pre-specified PSEUDORECT y centers, one for each row
  y_cen=findgen(128)+0.5        
  for i=0,n_elements(y_cen)-1 do begin 
     ;; Solve for the normalized wavelength roots, in normalized wavelength
     ;;   y=Sum(i=0,5){ B[i]*lam_norm^i }
     lam_norm=fz_roots([B[0]-y_cen[i],B[1:*]])
     ;; only real, non-negative wavelengths, please
     good=where(imaginary(lam_norm) eq 0.,cnt)
     if cnt eq 0 then continue
     lam_norm=lam_norm[good]
     
     ;; Convert to real wavlength: lam_norm=(lam-lam_c)/lam_c
     lam=float(lam_norm)*rec.WAV_CENTER+rec.WAV_CENTER
     good=where(lam ge 0.,cnt)
     if cnt eq 0 then continue  ;;this sample is out of range.
     lam=lam[good[0]] & lam_norm=float(lam_norm[good[0]])
     if lam gt rec.wav_max or lam lt rec.wav_min then continue
     x_cen=poly(lam_norm,rec.A)
     
     ;; We now have the exact center of the slit... rotate edges about edge
     ;;   centers. 
     ;; Use the previous bottom edge pair as the top edge, if available
;      if n_elements(top_edge) ne 0 then begin 
;         y=y_cen[i]+0.5
;         bottom_edge=top_edge 
;      endif else y=y_cen[i]-0.5
;      while 1 do begin 
;         theta=poly(y,rec.C)/!radeg ;angle in radians CCW from +x direction
;         delX=cos(theta)*rec.SLIT_LENGTH/2.
;         delY=sin(theta)*rec.SLIT_LENGTH/2.
;         edge=[{X:x_cen-delX,Y:y-delY}, $ ; 1 ------> 0 or 2 ------> 3
;               {X:x_cen+delX,Y:y+delY}]
;         if n_elements(bottom_edge) eq 0 then begin ; first run through
;            bottom_edge=edge 
;            y=y_cen[i]+0.5       ;now do bottom edge
;         endif else begin 
;            top_edge=edge
;            break
;         endelse 
;     endwhile 
;     pr.x=[reverse(top_edge.X),bottom_edge.X]
;     pr.y=[reverse(top_edge.Y),bottom_edge.Y]
;     pr.angle=poly(y_cen[i],rec.C) ;average angle, in degrees
     
     ;; Form relative coordinates and rotate PR about the slit center.
     y_rel=[1.,1.,-1.,-1.]*width/2.
     x_rel=[rec.SLIT_LENGTH ,-rec.SLIT_LENGTH, $
            -rec.SLIT_LENGTH,rec.SLIT_LENGTH]/2
     theta=poly(y_cen[i],rec.C)
     if theta ne 0. then begin 
        ct=cos(theta/!radeg) & st=sin(theta/!radeg)
        ;; Rotate about the PR center
        xr=ct*x_rel-st*y_rel
        yr=st*x_rel+ct*y_rel
        x_rel=xr & y_rel=yr
     endif 
     
     pr={IRS_WAVSAMP_PSEUDORECT}
     pr.x=x_rel+x_cen
     pr.y=y_rel+y_cen[i]
     pr.lambda=lam
     pr.angle=theta
     pr.cen=[x_cen,y_cen[i]]
     if n_elements(prs) eq 0 then begin 
        prs=[pr]
     endif else begin 
        if width le 1. then begin 
           ;; XXX- should we really do this?  It avoids gaps for 1xn
           ;;      PRs, but with 2xn we should leave it alone.
           ;; Connect bottom edge to previous top edge so as to smoothly tile.
           npr=n_elements(prs) 
           pr.x[2:3]=prs[npr-1].x[[1,0]]
           pr.y[2:3]=prs[npr-1].y[[1,0]]
           ;;pr.cen=[total(pr.x),total(pr.y)]/4.
        endif 
        prs=[prs,pr]
     endelse 
  endfor 
  
  ;; ensure ascending wavelength
  prs=prs[sort(prs.lambda)]
  
  ;; Construct the WAVSAMP and set it into the record
  ws={IRS_WAVSAMP}
  ws.PIXEL_BASED=1b
  ws.PR_WIDTH=width
  ws.Aperture=irs_aperture(0.,1.)
  ws.PR=ptr_new(prs,/NO_COPY)
  
  ws.FULL=1b
  
  if ptr_valid(rec.WAVSAMPS) then *rec.WAVSAMPS=[*rec.WAVSAMPS,ws] else $
     rec.WAVSAMPS=ptr_new(ws,/NO_COPY)
  self->SetRecord,rec           ; we may have made a new WAVSAMPS pointer
  
  ;; Clip this new, full, pixel-based WAVSAMP we just added, which also
  ;; takes care of overlapping areas in adjacent PRs (XXX not yet)
  ws=self->Clip(module,order,/FULL,/PIXEL_BASED,_EXTRA=e)
end

function IRS_Calib::IsFullAperture,aps
  if n_elements(aps) eq 0 then return,0
  return,total(aps.low eq 0. AND aps.high eq 1.,1) eq 2.
end

;=============================================================================
;  Clip - Clip a WAVSAMP with specified aperture against the pixels,
;         and add it to the records WAVSAMP list, returning the new
;         IRS_WAVSAMP struture.
;
;         If SAVE_POLYGONS is set, populate the POLYGONS field of each
;         WAVSAMP with the resultant clipped polygons.
;
;         APERTUREs are specified in normalized coordinates, with 0.0
;         at the "bottom" of the slit (at left), and 1.0 at right in
;         the "top" of the slit:
;
;                     0.0                       1.0 
;                      |=========================|
;             
;                        <--low                         ^
;                      1-------------    high-->         \   angle theta
;                      |             \-----------0        \   increasing
;                      2-------------            |         |
;                                    \-----------3         |
;                                    
;=============================================================================
function IRS_Calib::Clip, module, order, APERTURE=aper, FULL=clip_full, $
                          PIXEL_BASED=pb, PR_WIDTH=width, SAVE_POLYGONS=sp, $
                          RECORD=rec
  
  ;; Find the always-present full slit aperture wavsamp clip, i.e.
  ;;    low: [0.,0.] ; high: [1.,1.]
  wh_full=self->FindWAVSAMP(module,order,/FULL,PIXEL_BASED=pb, $
                            PR_WIDTH=width, COUNT=cnt,RECORD=rec)
  if cnt eq 0 then begin 
     if keyword_set(pb) then begin ; Just make a full wavsamp for them
        self->PixelWAVSAMP,module,order,PR_WIDTH=width
        wh_full=self->FindWAVSAMP(module,order,/FULL,PIXEL_BASED=pb, $
                                  PR_WIDTH=width, COUNT=cnt,RECORD=rec)
        if cnt eq 0 then message,'Failed to created PIXEL-BASED full WAVSAMP.'
        if self->IsFullAperture(aper) then clip_full=1
     endif else message,'Full slit aperture WAVSAMP not available ' + $
        '(check for WAVSAMP files).'
  endif
  
  full=(*rec.WAVSAMPS)[wh_full[0]]
  new=full                      ;The new clip
  if n_elements(aper) eq 0 then aper=irs_aperture(0.,1.)
  npr=n_elements(*new.PR)
  if ~keyword_set(clip_full) then begin
     new.PR=ptr_new(*full.PR)   ; Copy the psuedo-rects
     new.FULL=0b
     (*new.PR).POLYGONS=ptrarr(npr) ;In case these aren't asked for
     newap=new.Aperture
     struct_assign,aper,newap
     new.Aperture=newap
  endif else begin 
     self->CleanWAVSAMP,new,/PA_ONLY ;just clip "in place"
     new.FULL=1b                ;pure pedantry
  endelse 
  
  if ~keyword_set(clip_full) then begin
     ;; Scale the aperture from low to high.
     x=(*new.PR).x & y=(*new.PR).y
     self->Trim,x,y,APERTURE=aper,LAMBDA=(*new.PR).lambda, $
                SLIT_LENGTH=rec.SLIT_LENGTH
     (*new.PR).x=x & (*new.PR).y=y
  endif 
  
  for i=0,npr-1 do begin
     ;; Clip the new PRs
     if keyword_set(sp) then $
        pixels=polyfillaa((*new.PR)[i].x,(*new.PR)[i].y,128,128,AREAS=ar, $
                          POLYGONS=polys) $
     else pixels=polyfillaa((*new.PR)[i].x,(*new.PR)[i].y,128,128,AREAS=ar)
     
     if pixels[0] ne -1 then begin 
        ;; Overwrite the pixel and area pointers with new ones The old
        ;; ones were either freed (if FULL), or remain valid somewhere
        ;; in the original WAVSAMP set.
        (*new.PR)[i].PIXELS=ptr_new(pixels,/NO_COPY)
        (*new.PR)[i].AREAS =ptr_new(ar,/NO_COPY)
        if keyword_set(sp) then $
           (*new.PR)[i].POLYGONS=ptr_new(polys,/NO_COPY)
     endif else begin 
        (*new.PR)[i].PIXELS=ptr_new()
        (*new.PR)[i].AREAS =ptr_new()
        if keyword_set(sp) then $
           (*new.PR)[i].POLYGONS=ptr_new()
     endelse 
  endfor
  ;; Remove the PR's which didn't fall on the array
  good=where(ptr_valid((*new.PR).PIXELS),goodcnt)
  if goodcnt eq 0 then message,'No WAVSAMP pseudo-rects exist on array'
  if goodcnt lt npr then (*new.PR)=(*new.PR)[good]
  
;  if keyword_set(pb) AND NOT array_equal(prs.angle,0.) then begin 
;     print,'deoverlapping'
;     self->DeOverlap,prs
;  endif 
  return,new
end

;=============================================================================
;  Trim - Trim a set of PRs to the aperture.  x,y are 4xn
;=============================================================================
pro IRS_Calib::Trim,x,y,APERTURE=aper,LAMBDA=lambda,SLIT_LENGTH=sl
  ;; Scale each aperture linearly from low to high or with wavelength,
  ;; preserving angle
  
  if aper.wavscl then begin 
     ;; size/wave/pos scaling
     ;; width, normalized to slit len
     width=aper.scale[0]*(lambda/aper.scale[1])
     
     pos=aper.scale[2]          ;center position, normalized to slit length
     delx=(x[0,*]-x[1,*]) & dely=(y[0,*]-y[1,*])
     
     x[0,*]=x[1,*]+(pos+width/2)*delx
     y[0,*]=y[1,*]+(pos+width/2)*dely
     x[1,*]=x[1,*]+(pos-width/2)*delx
     y[1,*]=y[1,*]+(pos-width/2)*dely
     
     delx=(x[3,*]-x[2,*]) & dely=(y[3,*]-y[2,*])
     x[3,*] = x[2,*]+(pos+width/2)*delx
     y[3,*] = y[2,*]+(pos+width/2)*dely
     x[2,*] = x[2,*]+(pos-width/2)*delx
     y[2,*] = y[2,*]+(pos-width/2)*dely
  endif else begin 
     n=(size(x,/DIMENSIONS))[1]
     low=(aper.low[0]+(aper.low[1]-aper.low[0])*findgen(n)/(n-1))
     high=(aper.high[0]+(aper.high[1]-aper.high[0])*findgen(n)/(n-1))
     
     delx=(x[0,*]-x[1,*]) & dely=(y[0,*]-y[1,*])
     x[0,*] = x[1,*]+high*delx
     y[0,*] = y[1,*]+high*dely
     x[1,*] = x[1,*]+low *delx
     y[1,*] = y[1,*]+low *dely
     
     delx=(x[3,*]-x[2,*]) & dely=(y[3,*]-y[2,*])
     x[3,*] = x[2,*]+high*delx
     y[3,*] = y[2,*]+high*dely
     x[2,*] = x[2,*]+low *delx
     y[2,*] = y[2,*]+low *dely
  endelse 
end 

;=============================================================================
;  TransformBoreSightCoords - Transform from a SIRTF-FOV-centric
;                             coordinate set (including PA) to one
;                             centered on a given module/order.
;=============================================================================
pro IRS_Calib::TransformBoreSightCoords,module,coords,pa, $
                                        ORDER=ord,outcoords,outpa
  ;; XXX CHECK THIS
  rec=self->GetRecord(module,ord,/MUST_EXIST)
  RADEG = 180.0d/!DPI           ; preserve double
  ang=pa/RADEG
  c=cos(ang) & s=sin(ang)
  delta=rebin(reform([[c,s],[-s,c]] ## [rec.TPF_Y,rec.TPF_Z]), $
              size(coords,/DIMENSIONS),/SAMPLE)
  delta[0,*]/=cos(coords[1]/RADEG)
  outcoords=coords + delta
  outpa=pa+(360.0D - rec.TPF_ANGLE)
end

;=============================================================================
;  TransformCoords - Transform coords/pa from one FOV to another
;=============================================================================
pro IRS_Calib::TransformCoords,module1,coords1,pa1, $
                               ORDER1=ord1,module2,ORDER2=ord2, coords2,pa2
  rec1=self->GetRecord(module1,ord1,/MUST_EXIST)
  rec2=self->GetRecord(module2,ord2,/MUST_EXIST)
  pa2=(pa1+rec1.TPF_ANGLE-rec2.TPF_ANGLE) mod 360.0d
  pa_bs=(pa1+rec1.TPF_ANGLE)    ;infer the spacecraft PA from the slit PA
  RADEG = 180.0d/!DPI           ; preserve double
  ang=pa_bs/RADEG
  c=cos(ang) & s=sin(ang)
  delta=rebin(reform([[c,s],[-s,c]] ## $
                     ([rec2.TPF_Y,rec2.TPF_Z]-[rec1.TPF_Y,rec1.TPF_Z])), $
              size(coords1,/DIMENSIONS),/SAMPLE)
  delta[0,*]/=cos(coords1[1]/RADEG) ;small-angle law-of-cosines approx.
  coords2=coords1 + delta
end

;=============================================================================
;  DeOverlap - Renormalize fractional areas for any areas of overlap
;              between two PRs, given a list of PRs.  This assumes at
;              most 2 PRs can overlap in any given location.  For unit
;              width, unit spaced PRs, this is valid up to tilt angles
;              of 60 degrees (i.e. always). XXX what about 2xn?
;=============================================================================
; pro IRS_Calib::DeOverlap, prs
;   thisx=prs[0].X & thisy=prs[0].Y
;   for i=0,n_elements(prs)-2 do begin
;      nextx=prs[i+1].X
;      nexty=prs[i+1].Y
;      ;; Clip the adjacent PR's to find the overlap
;      twopolyclip,thisx,thisy,nextx,nexty
;      if thisx[0] ne -1 then begin
;         print,'Got an overlap: ',thisx,thisy
;         ;; Clip the overlapping area to the grid
;         overlap_pix=polyfillaa(thisx,thisy,128,128,AREAS=overlap_areas)
;         if overlap_pix[0] ne -1 then begin
;            wh=where_array(overlap_pix,*prs[i].PIXELS,cnt)
;            if cnt gt 0 then begin 
;               print,'deoverlapping pixels: ',(*prs[i].PIXELS)[wh]
;               print,'with overlap areas: ',overlap_areas
;               (*prs[i].AREAS)[wh]=(*prs[i].AREAS)[wh]-.5*overlap_areas
;            endif 
;            wh=where_array(overlap_pix,*prs[i+1].PIXELS,cnt)
;            if cnt gt 0 then $
;               (*prs[i+1].AREAS)[wh]=(*prs[i+1].AREAS)[wh]-.5*overlap_areas
;         endif 
;      endif 
;      thisx=nextx & thisy=nexty
;   endfor  
;end


;=============================================================================
;  GetRecord - Get the calibration record for a given module and
;              order, or make one, if none yet exists.  If order is
;              omitted, the first order for the matching module is
;              returned.
;=============================================================================
function IRS_Calib::GetRecord, module, order, MUST_EXIST=me, EXISTS=exi
  m=irs_module(module)
  exi=1
  if ptr_valid(self.cal[m]) then begin 
     if n_elements(order) eq 0 then begin 
        cnt=1 & wh=0            ; default to using the first order on the list
     endif else wh=where((*self.cal[m]).ORDER eq order,cnt)
     if cnt gt 0 then return,(*self.cal[m])[wh[0]]
  endif 
  exi=0
  if keyword_set(me) then begin 
     message,'No such record exists: '+ $
             irs_module(module,/TO_NAME)+' Order '+strtrim(order,2)
     return,-1
  endif 
  st={IRS_CalibRec}           ; None found, just create a new one
  st.MODULE=m
  st.ORDER=order
  return,st
end

;=============================================================================
;  SetRecord - Set a calibration record for a given module and order.
;              If such a record already exists, overwrite, otherwise
;              append to the end of the list.
;=============================================================================
pro IRS_Calib::SetRecord, record
  if tag_names(record,/STRUCTURE_NAME) ne 'IRS_CALIBREC' then $
     message,'Wrong record type: '+tag_names(record,/STRUCTURE_NAME)
  record.Date=systime(/JULIAN) ; The date set
  if ptr_valid(self.cal[record.module]) then begin 
     wh=where((*self.cal[record.module]).ORDER eq record.ORDER,cnt)
     if cnt ne 0 then begin     ; overwrite that list item
        cal=(*self.cal[record.module])[wh[0]]
        ;; If it's a new WAVSAMPS pointer, free the old one.
        if ptr_valid(cal.WAVSAMPS) and $
           record.WAVSAMPS ne cal.WAVSAMPS then begin 
           self->CleanWAVSAMP,*cal.WAVSAMPS
           ptr_free,cal.WAVSAMPS
        endif
        (*self.cal[record.module])[wh[0]]=record 
     endif else $         ;; Just append to the module's list
        *self.cal[record.module]=[*self.cal[record.module],record]
  endif else self.cal[record.module]=ptr_new([record])
end

;=============================================================================
;  ReadCalib - Read (up to) all four calibration files for a given
;              module or modules, and record in the object.  Also read
;              module-inspecific calibration files and add data as
;              appropriate.
;=============================================================================
pro IRS_Calib::ReadCalib,module, WAVSAMP_VERSION=wv,ORDER_VERSION=orv, $
                         LINETILT_VERSION=tv,FRAMETABLE_VERSION=fv, $
                         PLATESCALE_VERSION=psv,PMASK_VERSION=pmv, $
                         FLUXCON_VERSION=fcv,SLCF_VERSION=slcfv, $
                         WAVECUT_VERSION=wvcv, PIXEL_OMEGA_VERSION=pov, $
                         ONLY=only, RECOVER_FROM_WAVSAMP=rcvfws
  
  cals=[{name:'WAVSAMP'   , group:'single', type:'tbl' }, $
        {name:'ORDFIND'   , group:'single', type:'tbl' }, $
        {name:'PMASK'     , group:'single', type:'fits'}, $
        {name:'LINETILT'  , group:'single', type:'tbl' }, $
        {name:'FLUXCON'   , group:'single', type:'tbl' }, $
        {name:'SLCF'      , group:'single', type:'tbl' }, $
        {name:'FRAMETABLE', group:'all'   , type:'tbl' }, $
        {name:'PLATESCALE', group:'all'   , type:'tbl' }, $
        {name:'PIXEL_OMEGA',group:'all'   , type:'tbl' }, $
        {name:'WAVECUT'   , group:'single', type:'tbl' }]
  
  version=[n_elements(wv)  gt 0?fix(wv)>0:0,  $
           n_elements(orv) gt 0?fix(orv)>0:0, $
           n_elements(pmv) gt 0?fix(pmv)>0:0, $
           n_elements(tv)  gt 0?fix(tv)>0:0,  $
           n_elements(fcv) gt 0?fix(fcv)>0:0, $
           n_elements(slcfv) gt 0?fix(slcfv)>0:0, $ 
           n_elements(fv)  gt 0?fix(fv)>0:0,  $
           n_elements(psv) gt 0?fix(psv)>0:0, $
           n_elements(pov) gt 0?fix(pov)>0:0, $
           n_elements(wvcv) gt 0?fix(wvcv)>0:0]
  
  if keyword_set(only) then begin 
     which=where([n_elements(wv), $
                  n_elements(orv), $
                  n_elements(pmv), $
                  n_elements(tv), $
                  n_elements(fcv), $
                  n_elements(slcfv), $
                  n_elements(fv), $
                  n_elements(psv),$
                  n_elements(pov), $
                  n_elements(wvcv)] ne 0 ,cnt)
     if cnt eq 0 then return
     cals=cals[which]           ;only these cals are to be used
     version=version[which]
  endif 

  if n_elements(module) ne 0 then modules=[irs_module(module)] $
  else modules=indgen(4)        ;do them all, by default
  
  ;; Singles load a single set of data per module
  singles=where(cals.group eq 'single',COMPLEMENT=alls,NCOMPLEMENT=acnt,scnt)
  for i=0,n_elements(modules)-1 do begin 
     md=modules[i]
     for j=0,scnt-1 do begin 
        ;; Skip ORDFIND and LINETILT if we are recovering them from WS
        if keyword_set(rcvfws) then $
           if cals[singles[j]].name eq 'ORDFIND' || $
              cals[singles[j]].name eq 'LINETILT' then continue
        base="irs_b"+strtrim(md,2)+"_"+ cals[singles[j]].name+"v"
        cfile=self->CalibrationFileVersion(base,version[singles[j]], $
                                           cals[singles[j]].name, $
                                           cals[singles[j]].type,md)
        if size(cfile,/TYPE) ne 7 then continue
        call_method,'Parse'+cals[singles[j]].name,self,cfile,md
     endfor
  endfor
  
  ;; Alls are not module specific, but are relevant for all modules
  for j=0,acnt-1 do begin 
     base="irs_"+cals[alls[j]].name+"v"
     cfile=self->CalibrationFileVersion(base,version[alls[j]], $
                                        cals[alls[j]].name,cals[alls[j]].type)
     call_method,'Parse'+cals[alls[j]].name,self,cfile 
  endfor
  
  if keyword_set(rcvfws) then $
     for i=0,n_elements(modules)-1 do self->RecoverPolynomials,modules[i]
  
  self->SortOrders
end


;=============================================================================
;  SortOrders - Sort Orders according to minimum wavelength
;=============================================================================
pro IRS_Calib::SortOrders,module
  if n_elements(module) ne 0 then modules=[irs_module(module)] $
  else modules=indgen(4)        ;do them all, by default
  for i=0,n_elements(modules)-1 do begin 
     s=sort((*self.cal[modules[i]]).WAV_MIN)
     *self.cal[modules[i]]=(*self.cal[modules[i]])[s]
  endfor 
end

;=============================================================================
;  Return - Latest calibration file, or requested version
;=============================================================================
function IRS_Calib::CalibrationFileVersion,base,version,name,type,md
  @cubism_dir                    ;get irs_calib_dir
  ;; A specific version was requested, check it
  if version gt 0 then begin 
     cfile=filepath(ROOT_DIR=irs_calib_dir,SUBDIRECTORY='ssc', $
                    base+strtrim(version,2)+'.'+type)
     if ~file_test(cfile,/READ,/REGULAR) then $
        message,'No such calibration file: '+cfile
  endif 
        
  ;; Find all versions for this module
  if version eq 0 then begin 
     cal_files=findfile(COUNT=fcnt,filepath(ROOT=irs_calib_dir, $
                                            SUBDIR="ssc",base+"*."+type))
     if fcnt eq 0 then begin 
        message,/CONTINUE,"Didn't find any calibration files: " +$
                name+(n_elements(md) ne 0?(' for '+irs_module(md,/TO_NAME)):"")
        return,-1
     endif
     vers=max(fix((stregex(cal_files,base+"([0-9]+)"+"."+type+"$",$
                           /EXTRACT,/SUBEXPR))[1,*]),mpos)
     cfile=cal_files[mpos]      ;use the latest version
  endif 
  return,cfile
end


;=============================================================================
;  RecoverPolynomials - Work around the limited (and braindead) lack
;                       of access to the ORDFIND and LINETILT A,B,C
;                       polynomial coefficient files by recovering
;                       them by polynomial fitting the full WAVSAMP.
;=============================================================================
pro IRS_Calib::RecoverPolynomials,module,orders
  if n_elements(module) ne 0 then begin 
     need_orders=n_elements(orders) eq 0
     modules=[irs_module(module)] 
  endif else begin 
     modules=indgen(4)          ;do them all, by default
     need_orders=1
  endelse 
  
  for md=0,n_elements(modules)-1 do begin  
     if need_orders then orders=self->Orders(modules[md])
     for i=0,n_elements(orders)-1 do begin 
        rec=self->GetRecord(modules[md],orders[i],/MUST_EXIST)
        ;; Get the full, original WAVSAMP
        prs=self->GetWAVSAMP(modules[md],orders[i],/FULL)
        rec.SLIT_LENGTH=mean([sqrt((prs.x[0,*]-prs.x[1,*])^2+ $
                                   (prs.y[0,*]-prs.y[1,*])^2), $
                              sqrt((prs.x[3,*]-prs.x[2,*])^2+ $
                                   (prs.y[3,*]-prs.y[2,*])^2)])
        rec.WAV_MIN=min(prs.lambda,MAX=mx)
        rec.WAV_MAX=mx
        x_cen=reform(prs.cen[0,*]) & y_cen=reform(prs.cen[1,*])
        ymax=max(y_cen,MIN=ymin)
        ymid=ymin+.5*(ymax-ymin)
        rec.WAV_CENTER=interpol(prs.lambda,y_cen,ymid,/QUADRATIC)
        
        ;;print,rec.slit_length,rec.WAV_CENTER
        
        
        ;; Fit a's => x(lam_norm) and b's=> y(lam_norm) to polynomials
        lam_norm=(prs.lambda-rec.WAV_CENTER)/rec.WAV_CENTER
        a_fit=svdfit(lam_norm,x_cen,6,YFIT=x_cen_fit,STATUS=stat)
        b_fit=svdfit(lam_norm,y_cen,6,YFIT=y_cen_fit,STATUS=stat)
        
        ;; Check for constant angle first
        if array_equal(prs.angle,prs[0].angle) then begin 
           c_fit=[prs[0].angle,replicate(0.0,3)]
        endif else begin 
           ;; Start small and work your way up to avoid runaway fit
           for polyord=2,4 do begin 
              c_fit_try=svdfit(y_cen,prs.angle,polyord,YFIT=angle_fit_try, $
                               CHISQ=chi_sq,STATUS=stat)
              ;; If it got worse, abort and use the last one
              if n_elements(old_chisq) gt 0 && chi_sq gt old_chisq then break
              angle_fit=angle_fit_try 
              c_fit=[c_fit_try,replicate(0.0,4-polyord)] & old_chisq=chi_sq
              ;;print,'CHI_SQ:',chi_sq
           endfor 
        endelse 
        rec.A=a_fit & rec.B=b_fit & rec.C=c_fit
        rec.RecoveredPolys=1
        self->FreeWAVSAMP,modules[md],orders[i],RECORD=rec,/ALL,/PIXEL_BASED
        self->SetRecord,rec
        ;; Plot Debug
;         !P.MULTI=[0,3,3,0,1]
;         plot,lam_norm,x_cen,PSYM=4,/YNOZERO,CHARSIZE=2
;         x_cen_orig=poly(lam_norm,rec.A)
;         print,'Original A: ',rec.A
;         oplot,lam_norm,x_cen_orig,COLOR=2
;         oplot,lam_norm,x_cen_fit,COLOR=1
;         plot,lam_norm,x_cen-x_cen_fit,CHARSIZE=2
;         plot,lam_norm,x_cen_fit-x_cen_orig,CHARSIZE=2
;         print,"New A: ",a_fit
        
        
;         plot,lam_norm,y_cen,PSYM=4,/YNOZERO,CHARSIZE=2
;         y_cen_orig=poly(lam_norm,rec.B)
;         print,'Original B: ',rec.B
;         oplot,lam_norm,y_cen_orig,COLOR=2 ;old fit in blue
;         oplot,lam_norm,y_cen_fit,COLOR=1 ;fit in red
;         plot,lam_norm,y_cen-y_cen_fit,CHARSIZE=2
;         plot,lam_norm,y_cen_fit-y_cen_orig,CHARSIZE=2
;         print,"New B: ",b_fit
        
        
;         plot,y_cen,prs.angle,PSYM=4,/YNOZERO,CHARSIZE=2
;         angle_orig=poly(y_cen,rec.C)
;         print,'Original C: ',rec.C
;         oplot,y_cen,angle_orig,COLOR=2
;         if n_elements(angle_fit) gt 0 then begin 
;            oplot,y_cen,angle_fit,COLOR=1
;            plot,y_cen,prs.angle-angle_fit,CHARSIZE=2
;            plot,y_cen,angle_fit-angle_orig,CHARSIZE=2
;         endif 
;        print,"New C: ",c_fit
        
     endfor 
  endfor 
end


;=============================================================================
;  ParseWAVSAMP - Read and parse the specified WAVSAMP file, clipping
;                 and caching the PSUEDO-RECT full-slit polygons for
;                 fast access.
;=============================================================================
pro IRS_Calib::ParseWAVSAMP,file,module
  m=irs_module(module)
  data=read_ipac_table(file)
  orders=data[uniq(data.ORDER)].ORDER
  for ord=0,n_elements(orders)-1 do begin 
     rec=self->GetRecord(m,orders[ord])
     wh=where(data.ORDER eq orders[ord],cnt)
     if cnt eq 0 then continue  ;no data for this order
     ;; Collect the data and create the WAVSAMP structure list
     pr=replicate({IRS_WAVSAMP_PSEUDORECT},cnt)
     pr.lambda=data[wh].wavelength
     pr.angle=data[wh].angle
     pr.x=[transpose(data[wh].x0), $
           transpose(data[wh].x1), $
           transpose(data[wh].x2), $
           transpose(data[wh].x3)]
     pr.y=[transpose(data[wh].y0), $
           transpose(data[wh].y1), $
           transpose(data[wh].y2), $
           transpose(data[wh].y3)]
     pr.cen=[transpose(data[wh].x_center),transpose(data[wh].y_center)]
     pr=pr[sort(pr.lambda)]
     ws={IRS_WAVSAMP, $
         0b,1b,0.0,irs_aperture(0.,1.),ptr_new(pr,/NO_COPY)}
     
     ;; Clean out any old WAVSAMP's
     self->FreeWAVSAMP,RECORD=rec,/ALL
     
     rec.WAVSAMPS=ptr_new(ws,/NO_COPY)
     ;; Put the record back
     self->SetRecord,rec
     
     ;; Clip the full slit as a starting point.
     c=self->Clip(m,orders[ord],/FULL)
  endfor
  self.WAVSAMP_FILE[m]=file
end

;=============================================================================
;  ParseOrdFind - Read and parse the specified ORDFIND file, saving
;                 the polynomial fit data in the object.
;=============================================================================
pro IRS_Calib::ParseOrdFind,file,module
  m=irs_module(module)
  data=read_ipac_table(file)
  for i=0,n_elements(data.ORDER)-1 do begin 
     rec=self->GetRecord(m,data[i].ORDER)
     rec.a=[data[i].a0,data[i].a1,data[i].a2,data[i].a3,data[i].a4,data[i].a5]
     rec.b=[data[i].b0,data[i].b1,data[i].b2,data[i].b3,data[i].b4,data[i].b5]
     rec.WAV_CENTER=data[i].wavelength_center
     rec.WAV_MIN=data[i].wavelength_min
     rec.WAV_MAX=data[i].wavelength_max
     rec.SLIT_LENGTH=data[i].width_found
     self->SetRecord,rec
  endfor
  self.ORDER_FILE[m]=file 
end

;=============================================================================
;  ParseLineTilt - Read and parse the specified LINETILT file, saving
;                  the polynomial fit data in the object.
;=============================================================================
pro IRS_Calib::ParseLineTilt,file,module
  m=irs_module(module)
  data=read_ipac_table(file)
  
  ;; set all of them at once 
  if data[0].ORDER eq 9999 then begin 
     if NOT ptr_valid(self.cal[m]) then return
     ;; Set all by hand
     for i=0,n_elements(*self.cal[m])-1 do begin 
        (*self.cal[m])[i].c=[data[0].c0,data[0].c1,data[0].c2,data[0].c3]
     endfor 
  endif else begin
     for i=0,n_elements(data.ORDER)-1 do begin
        rec=self->GetRecord(module,(data.ORDER)[i])
        rec.c=[data[i].c0,data[i].c1,data[i].c2,data[i].c3]
        self->SetRecord,rec
     endfor
  endelse 
  self.TILT_FILE[m]=file
end


;=============================================================================
;  ParsePMASK
;=============================================================================
pro IRS_Calib::ParsePMASK,file,module
  m=irs_module(module)
  self.PMASK[m]=ptr_new(readfits(file,/SILENT))
  self.PMASK_FILE[m]=file
end

;=============================================================================
;  ParseSLCF
;=============================================================================
pro IRS_Calib::ParseSLCF,file,module
  m=irs_module(module)
  data=read_ipac_table(file)
  self.SLCF[m]=ptr_new(transpose([[data.wavelength],[data.slcf]]))
  self.SLCF_FILE[m]=file
end


;=============================================================================
;  ParseWAVECUT
;=============================================================================
pro IRS_Calib::ParseWAVECUT,file,module
  m=irs_module(module)
  data=read_ipac_table(file)
  for i=0,n_elements(data.ORDER)-1 do begin 
     rec=self->GetRecord(m,data[i].ORDER)
     rec.WAVECUT=[data[i].wavecut_low,data[i].wavecut_high]
     self->SetRecord,rec
  endfor 
  self.WAVECUT_FILE[m]=file
end

;=============================================================================
;  ParseFluxcon - Read and parse the specified FLUXCON file, saving
;                 the data in the object.
;=============================================================================
pro IRS_Calib::ParseFluxcon,file,module
  m=irs_module(module)
  data=read_ipac_table(file)
  for i=0,n_elements(data.ORDER)-1 do begin 
     rec=self->GetRecord(m,data[i].ORDER)
     rec.TUNE=[data[i].a0,data[i].a1,data[i].a2,data[i].a3, $
               data[i].a4,data[i].a5]
     rec.FLUXCON=data[i].fluxcon
     rec.FLUXCON_KEY_WAV=data[i].key_wavelength
     self->SetRecord,rec
  endfor
  self.FLUXCON_FILE[m]=file 
end


;=============================================================================
;  ParseFrameTable - Parse frame table and update individual orders
;                    offsets/angles.
;=============================================================================
pro IRS_Calib::ParseFrameTable,file
  data=read_ipac_table(file)
  names=strupcase(strtrim(data.fov_name,2))
  for md=0,4 do begin 
     if ptr_valid(self.cal[md]) eq 0 then continue
     for j=0,n_elements(*self.cal[md])-1 do begin 
        mname=irs_fov(/LOOKUP_MODULE, /RETURN_NAME, $
                      MODULE=(*self.cal[md])[j].MODULE, $
                      ORDER=(*self.cal[md])[j].ORDER, $
                      POSITION=0)
        wh=where(names eq strupcase(mname),cnt)
        if cnt eq 0 then message,'No frame table entry found for: '+mname
        (*self.cal[md])[j].TPF_Z= data[wh[0]].brown_theta_Y/60.D
        (*self.cal[md])[j].TPF_Y=-data[wh[0]].brown_theta_Z/60.D
        (*self.cal[md])[j].TPF_ANGLE=data[wh[0]].angle
     endfor 
     ;; Add order-0 (slit center) reference if necessary, purely for
     ;; frametable offsetting
     mname=irs_fov(/LOOKUP_MODULE,/RETURN_NAME,MODULE=md,ORDER=0,POSITION=0)
     wh=where(names eq strupcase(mname),cnt)
     if cnt gt 0 then begin 
        rec=self->GetRecord(md,0,EXISTS=exists) ;create a new order "0" record
        if ~exists then begin 
           rec.TPF_Z= data[wh[0]].brown_theta_Y/60.D
           rec.TPF_Y=-data[wh[0]].brown_theta_Z/60.D
           rec.TPF_ANGLE=data[wh[0]].angle
           self->SetRecord,rec
        endif 
     endif 
  endfor 
  self.FRAMETABLE_FILE=file
end


;=============================================================================
;  ParsePlateScale
;=============================================================================
pro IRS_Calib::ParsePlateScale,file
  data=read_ipac_table(file)
  for i=0,4 do begin 
     if ptr_valid(self.cal[i]) eq 0 then continue
     wh=where(data.mod_num eq i,cnt)
     if cnt eq 0 then continue
     (*self.cal[i]).PLATE_SCALE=data[wh[0]].plate_scale/3600.0D
  endfor 
  self.PLATESCALE_FILE=file
end


;=============================================================================
;  ParsePixel_Omega
;=============================================================================
pro IRS_Calib::ParsePixel_Omega,file
  data=read_ipac_table(file)
  for i=0,4 do begin 
     if ptr_valid(self.cal[i]) eq 0 then continue
     wh=where(data.mod_num eq i,cnt)
     if cnt eq 0 then continue
     (*self.cal[i]).PIXEL_OMEGA=data[wh[0]].pix_solid_angle
  endfor 
  self.PIXEL_OMEGA_FILE=file
end


;=============================================================================
;  CleanWAVSAMP - Delete the contents of one or more WAVSAMP records
;                 by freeing the internal pointers, or just clear the
;                 PIXELS and AREAS (and optionally POLYGONS) if
;                 PA_ONLY set.
;=============================================================================
pro IRS_Calib::CleanWAVSAMP, ws,PA_ONLY=pao
  for i=0,n_elements(ws)-1 do begin
     pr=ws[i].PR
     if ptr_valid(pr) && n_elements(*pr) gt 0 then begin 
        ptr_free,(*pr).PIXELS,(*pr).AREAS
        for j=0,n_elements(*pr)-1 do begin 
           if ptr_valid((*pr)[j].POLYGONS) then $
              ptr_free,*(*pr)[j].POLYGONS, (*pr)[j].POLYGONS
        endfor 
        if keyword_set(pao) eq 0 then ptr_free,pr
     endif 
  endfor 
end

;=============================================================================
;  Cleanup - Free all resources.
;=============================================================================
pro IRS_Calib::Cleanup
  for i=0,3 do begin 
     if ptr_valid(self.cal[i]) then begin
        ws_list=(*self.cal[i]).WAVSAMPS
        for j=0,n_elements(ws_list)-1 do $
           if ptr_valid(ws_list[j]) then self->CleanWAVSAMP,*ws_list[j]
        ptr_free,ws_list,self.cal[i]
     endif 
  endfor 
  ptr_free,self.PMASK
end

;=============================================================================
;  Name
;=============================================================================
function IRS_Calib::Name
  return,self.Name
end

;=============================================================================
;  Init - Create a new IRS_Calib object
;=============================================================================
function IRS_Calib::Init,name
  if n_elements(name) ne 0 then self.Name=name
  return,1
end

;=============================================================================
;  IRS_Calib__define - Define the IRS_Calib class
;=============================================================================
pro IRS_Calib__define
  class={IRS_Calib, $         
         Name: '', $            ;A name for this IRS Calibration object
         PLATESCALE_FILE:'', $  ;The name of the plate scale file
         PIXEL_OMEGA_FILE: '', $ ;Name of the pixel solid angle file
         FRAMETABLE_FILE:'', $  ;name for frametable file
         WAVSAMP_FILE:strarr(5), $ ;the names of the wavsamp files (WAVSAMP)
         TILT_FILE:strarr(5),$  ;the name of the LINETILT file (c)
         ORDER_FILE:strarr(5),$ ;the name of the ORDFIND file (a's & b's)
         FLUXCON_FILE:strarr(5),$ ;the name of the FLUXCON coefficient file
         PMASK_FILE:strarr(5),$ ;the name of the pmask files   
         SLCF_FILE:strarr(5), $ ;name of the SLCF files
         WAVECUT_FILE:strarr(5), $ ;the name of the WAVECUT files (if any)
         PMASK:ptrarr(5), $     ;the pmask, one for each module
         SLCF: ptrarr(5), $     ;SLCF for each module (at most)
         cal: ptrarr(5)}        ;Lists of IRS_CalibRec structs, one list
                                ;for each module: 0:LH, 1:LL, 2:SH, 3:SL 4:MSED
  
  ;; The complete calibration set for a single order in one module.
  ;; Note that the bonus 1st order segment in the low-res modules is
  ;; known as order "3".
  rec={IRS_CalibRec, $
       MODULE: 0, $             ;which module 0:LH, 1:LL, 2:SH, 3:SL, 4:MSED
       ORDER: 0, $              ;the order number this data corresponds to.
       Date:0.0D, $             ;Date First constructed
       TPF_Z:0.0D, $            ;[deg] Offset of slit center in +Z
       TPF_Y:0.0D, $            ;[deg] Offset of slit center in +Y
       TPF_ANGLE:0.0D, $        ;[deg] angle of FOV +w C.W. from TPF +Z
       WAV_CENTER: 0.0, $       ;[um] the central order wavelength
       WAV_MIN: 0.0, $          ;[um] the minimum order wavelength
       WAV_MAX: 0.0, $          ;[um] the maximum order wavelength
       WAVECUT: [0.0,0.0], $    ;[um] suggested low and high trim wavelengths
       SLIT_LENGTH: 0.0, $      ;[pix] the length of the slit
       PLATE_SCALE: 0.0, $      ;[deg/pix] the plate scale along the slit
       PIXEL_OMEGA: 0.0, $      ;[sr] effective solid angle per pixel (2x1)
       A:fltarr(6), $           ;x(lambda)=sum_i a_i lambda^i
       B:fltarr(6), $           ;y(lambda)=sum_i b_i lambda^i
       C:fltarr(4), $           ;tilt_ang(s)=sum_i c_i s^i
       RecoveredPolys: 0,$      ;recovered ordfind/linetilt from WS
       FLUXCON:0.0, $           ;the fluxcon value for this order e/s/Jy
       FLUXCON_KEY_WAV:0.0,$    ;fluxcon reference wavelength
       TUNE:fltarr(6),$         ;fluxcon tuning coefficients "a"
       WAVSAMPS: ptr_new()}     ;A list of IRS_WAVSAMP structs
                                ; (cached for various apertures, etc.)
  
  ;; A wavsamp set for a single order and a given aperture, either
  ;; "traditional" (from the WAVSAMP file), or "pixel-based"
  ;; (generated directly from the A,B and C coefficients).
  st={IRS_WAVSAMP, $
      PIXEL_BASED: 0b, $        ;whether traditional or pixel-based WAVSAMP
      FULL: 0b, $               ;is this the full (untrimmed) WAVSAMP?
      PR_WIDTH: 0.0, $          ;if PIXEL_BASED, the width of the PR
      Aperture:{IRS_APERTURE}, $ ;The IRS_APERTURE aperture used for this WS
      PR: ptr_new()}            ;Sorted list of IRS_WAVSAMP_PSEUDORECT structs
  
  ;; A single WAVSAMP pseudo-rectangle (PR), with pre-computed
  ;; full-slit overlap areas.  Vertices are listed counter-clockwise
  ;; from the top-right:
  ;;         1         0
  ;;         2         3
  st={IRS_WAVSAMP_PSEUDORECT , $
      lambda: 0.0, $            ;wavelength of PR center
      cen:[0.0,0.0], $          ;x,y, center of full-slit PR
      x:fltarr(4), $            ;X positions of the PR vertices
      y:fltarr(4), $            ;Y positions of the PR vertices
      angle: 0.0, $             ;Angle, anti-clockwise about x axis
      PIXELS: ptr_new(), $      ;The pixels at least partially inside the PR
      AREAS: ptr_new(), $       ;The area inside the PR for each of PIXELS.
      POLYGONS: ptr_new()}      ;(optional) the resulting clipped polygons
                                ; as a list of pointers to 2xn coord pairs
end
