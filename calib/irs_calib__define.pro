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
;    IRS_DIR: Included through file irs_dir.pro.  Defines the path
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
;          APERTURE: A structure of the form:
;          
;               {IRS_APERTURE, $
;                  Low: [low_top,low_bottom], $
;                  High:[high_top,high_bottom]}
;             
;             specifying the normalized coordinates along the slit for
;             low and high aperture boundaries.  See
;             IRS_Aperture__define for more information.  If
;             omitted, the full slit aperture is used.
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
;cal=             pixels.  Defaults to 1.
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
;    See the document "SSC--ISC: Interface Products and Conventions"
;    for further description of these calibration products.
;
;    IRS_Calib object sets can be found in the subdirectory
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
;    cal=sm_restore_calib('sm_2001_12_14.cal') 
;    a->ReadCalib,'LL',WAVSAMP_VERSION=0,/ONLY
;
; MODIFICATION HISTORY:
;
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
;  Copyright (C) 2001,2002 J.D. Smith
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
                            WAVE_CENTER=wc,WAV_MIN=wmn,WAV_MAX=wmx
  rec=self->GetRecord(module,order,/MUST_EXIST)
  if size(rec,/TYPE) ne 8 then return
  if arg_present(name) then name=self.name
  if arg_present(sl) then sl=rec.SLIT_LENGTH
  if arg_present(wc) then  wc=rec.WAV_CENTER
  if arg_present(wmn) then  wmn=rec.WAV_MIN
  if arg_present(wmx) then  wmx=rec.WAV_MAX
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
pro IRS_Calib::Save,file
  if self.Name eq '' then self.Name=filestrip(file)
  save,self,FILENAME=file
end

;=============================================================================
;  Print - Print a listing of what this cal object contains.
;=============================================================================
pro IRS_Calib::Print,modules,orders
  print,transpose(self->Info(modules,orders))
end

function IRS_Calib::Info, modules, orders
  if n_elements(modules) eq 0 then modules=indgen(4)
  str=' == IRS Calibration Object: '+self.Name+' =='
  for i=0,n_elements(modules)-1 do begin 
     md=irs_module(modules[i])
     module=irs_module(md,/TO_NAME)
     no=ptr_valid(self.cal[md])?n_elements(*self.cal[md]):0
     str=[str,string(FORMAT='(%"\n ==> Module %s: %s")',module, $
                     (no gt 0?strtrim(n_elements(*self.cal[md]),2)+ $
                      " orders.":" not loaded."))]
     if no eq 0 then continue
     if n_elements(orders) eq 0 then ords=indgen(no) else $
        ords=where_array((*self.cal[md]).order,[orders],no,/SWITCHAB)
     if no eq 0 then continue
     str=[str,'']
     wsf=self.WAVSAMP_FILE[md]
     if wsf ne '' then str=[str,string(FORMAT='(A12,": ",A)', 'WAVSAMP',wsf)]
     orf=self.ORDER_FILE[md]
     if orf ne '' then str=[str,string(FORMAT='(A12,": ",A)', 'ORDER',orf)]
     tif=self.TILT_FILE[md]
     if tif ne '' then str=[str,string(FORMAT='(A12,": ",A)', 'TILT',tif)]
     str=[str,'']
     for j=0,no-1 do begin 
        rec=(*self.cal[md])[ords[j]]
        str=[str,string(FORMAT='(%"    ==> Order %2s%d  (%s): ")', $
                        module,rec.order, $
                        rec.Date eq 0.0D?"--":jul2date(rec.Date))]
        nw=ptr_valid(rec.WAVSAMPS)?n_elements(*rec.WAVSAMPS):0
        str=[str, $
             "          A:"+strjoin(string(FORMAT='(G10.3)',rec.A)), $
             "          B:"+strjoin(string(FORMAT='(G10.3)',rec.B)), $
             "          C:"+strjoin(string(FORMAT='(G10.3)',rec.C)), $
             "          SLIT_LENGTH:"+string(FORMAT='(G10.3)', $
                                             rec.SLIT_LENGTH),$
             "          WAVELENGTH(min,center,max):" + $
             string(FORMAT='(3F10.4)',rec.WAV_MIN,rec.WAV_CENTER,rec.WAV_MAX)]
        if nw gt 0 then $
           str=[str,"          Apertures:"] else $
           str=[str,"       No Apertures"]
        for k=0,nw-1 do begin 
           flags=""
           ws=(*rec.WAVSAMPS)[k]
           apstr=string(FORMAT='(%"            %4.2f->%4.2f : %4.2f->%4.2f' + $
                        ' -- %03d samples")', $
                        ws.Aperture.low,ws.Aperture.high, $
                        n_elements(*(*rec.WAVSAMPS)[k].PR))
           if ws.PIXEL_BASED ne 0.0 then $
              flags=[string(FORMAT='("pixel-based, width=",F5.3)', $
                            ws.PR_WIDTH)]
           if ptr_valid((*ws.PR)[0].POLYGONS) then $
              if flags[0] eq "" then flags=["with polygons"] else $
              flags=[flags,"with polygons"]
           nf=n_elements(flags) 
           if flags[0] ne "" and nf gt 0 then $
              apstr=apstr+" ("+string(FORMAT='('+ $
                                      strtrim(n_elements(flags),2)+ $
                                      '(A,:,", "))',flags)+")"
           str=[str,apstr]
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
  return,(*self.cal[m]).ORDER
end

;=============================================================================
;  FindWAVSAMP - Find and return the indices of WAVSAMPs matching the
;                criteria passed. The module and order arguments are
;                required, but if APERTURE is omitted, and FULL isn't
;                set, matching WAVSAMPs of any aperture in that record
;                will be returned.  The default is non-pixelbased, or,
;                when PIXEL_BASED is set, a PR_WIDTH of 1.
;=============================================================================
function IRS_Calib::FindWAVSAMP, module, order, APERTURE=aperture, $
                                 COUNT=nsamp, PIXEL_BASED=pb, PR_WIDTH=width, $
                                 FULL=full, RECORD=rec
  rec=self->GetRecord(module,order,/MUST_EXIST)
  if NOT ptr_valid(rec.WAVSAMPS) then begin 
     nsamp=0
     return,-1
  endif 
  
  ;; Find full aperture, specified aperture, or any aperture
  if keyword_set(full) then begin 
     aper={IRS_APERTURE,[0.,0.],[1.,1.]}
  endif else if n_elements(aperture) ne 0 then aper=aperture
  
  if n_elements(pb) eq 0 then pb=0b ;default to non-pb
  if n_elements(width) eq 0 then width=1.0 ;Default to 1xn PRs
  
  eps=1.e-5                 ;match tolerance
  
  ;; Find WAVSAMPs of the right type (pixel-based or no)
  wh_pix=where((*rec.WAVSAMPS).PIXEL_BASED eq keyword_set(pb),nsamp)
  if nsamp eq 0 then return,-1
  
  ;; Find WAVSAMP's with the right slit pixel width (if pixel-based)
  if keyword_set(pb) then begin 
     wh_width=where(abs((*rec.WAVSAMPS)[wh_pix].PR_WIDTH-width) $
                    le eps, nsamp)
     if nsamp gt 0 then wh_pix=wh_pix[wh_width] else return,-1
  endif 
  
  ;; Search for one of this type already cached with this aperture
  if n_elements(aper) ne 0 then begin 
     aps=(*rec.WAVSAMPS)[wh_pix].Aperture
     wh_ap=where(total(abs(aps.low- rebin(aper.low,2,nsamp))  le eps AND $
                       abs(aps.high-rebin(aper.high,2,nsamp)) le eps,1) $
                 eq 2,nsamp)
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
;               cached.  The newly created WAVSAMP is cached for fast
;               recovery, unless NO_CACHE is set.
;=============================================================================
function IRS_Calib::GetWAVSAMP, module, order, APERTURE=aperture, FULL=full, $
                                PIXEL_BASED=pb, PR_WIDTH=width, $
                                NO_CACHE=nc, SAVE_POLYGONS=sp
  rec=self->GetRecord(module,order,/MUST_EXIST)
  
  ws=self->FindWAVSAMP(module,order,APERTURE=aperture,COUNT=nsamp,FULL=full, $
                       PIXEL_BASED=pb, PR_WIDTH=width)
  
  if keyword_set(sp) eq 0 then begin 
     ;; return the first match
     if nsamp gt 0 then return,*(*rec.WAVSAMPS)[ws[0]].PR 
  endif else begin 
     for i=0,nsamp-1 do begin 
        ;; return the first match with saved polygons
        pr=(*rec.WAVSAMPS)[ws[i]].PR
        if ptr_valid((*pr)[0].POLYGONS) then return,*(*rec.WAVSAMPS)[ws[i]].PR
     endfor 
     ;; free matches, to create one again later *with* POLYs
     if nsamp gt 0 then self->FreeWAVSAMP,RECORD=rec,INDEX=ws
  endelse
  
  ;; No clip is cached for this aperture, so make a new one
  ws=self->Clip(module,order,APERTURE=aperture,PIXEL_BASED=pb,PR_WIDTH=width, $
                SAVE_POLYGONS=sp,FULL=full,RECORD=rec)
  
  if size(ws,/TYPE) ne 8 then message,'Error clipping WAVSAMP.'
  
  ;; And add it to the cache for rapid recovery, unless asked not to
  if keyword_set(nc) eq 0 then begin 
     if ptr_valid(rec.WAVSAMPS) then begin 
        ;; Only add it if it's a distinct wavesamp record
        wh=where((*rec.WAVSAMPS).PR eq ws.PR,cnt)
        if cnt eq 0 then *rec.WAVSAMPS=[*rec.WAVSAMPS,ws] 
     endif else rec.WAVSAMPS=ptr_new(ws)
  endif 
  
  self->SetRecord,rec
  
  ;; Return the list of pseudo-rects
  return,*ws.PR
end

;=============================================================================
;  FreeWAVSAMP - Remove one or more WAVSAMPS from a record, optionally
;                removing ALL of them, or those specified with indices
;                provided by optional keyword INDEX.
;=============================================================================
pro IRS_Calib::FreeWAVSAMP, module, order, APERTURE=aperture,RECORD=rec, $
                            INDEX=inds, ALL=all,_EXTRA=e
  if n_elements(rec) eq 0 then rec=self->GetRecord(module,order,/MUST_EXIST)
  if size(rec,/TYPE) ne 8 then return ;nothing to delete
  if NOT ptr_valid(rec.WAVSAMPS) then return
  
  ;; Clean all of the records for this module and order
  if keyword_set(all) then begin 
     self->CleanWAVSAMP,*rec.WAVSAMPS
     ptr_free,rec.WAVSAMPS
     return
  endif
  
  if n_elements(inds) eq 0 then begin 
     inds=self->FindWAVSAMP(module,order,APERTURE=aperture,COUNT=nsamp, $
                            _EXTRA=e)
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
  
  ;; Construct the WAVSAMP and set it into the record
  ws={IRS_WAVSAMP}
  ws.PIXEL_BASED=1b
  ws.PR_WIDTH=width
  ws.Aperture={IRS_APERTURE,[0.,0.],[1.,1.]}
  ws.PR=ptr_new(prs,/NO_COPY)
  
  if ptr_valid(rec.WAVSAMPS) then *rec.WAVSAMPS=[*rec.WAVSAMPS,ws] else $
     rec.WAVSAMPS=ptr_new(ws,/NO_COPY)
  self->SetRecord,rec           ; we may have made a new WAVSAMPS pointer
  
  ;; Clip this new, full, pixel-based WAVSAMP we just added, which also
  ;; takes care of overlapping areas in adjacent PRs (XXX not yet)
  ws=self->Clip(module,order,/FULL,/PIXEL_BASED,_EXTRA=e)
end

function IRS_Calib::IsFullAperture,aps
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
;                        <--low                   
;                      1-------------    high-->  
;                      |             \-----------0 
;                      2-------------            |
;                                    \-----------3
;                                    
;=============================================================================
function IRS_Calib::Clip, module, order, APERTURE=aper, FULL=clip_full, $
                          PIXEL_BASED=pb, PR_WIDTH=width, SAVE_POLYGONS=sp, $
                          RECORD=rec
  
  if n_elements(aper) eq 0 then aper={IRS_APERTURE,[0.,0.],[1.,1.]}
  
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
  
  new=full
  if keyword_set(clip_full) eq 0 then begin
     new.PR=ptr_new(*full.PR)   ; Copy the psuedo-rects
     newap=new.Aperture
     struct_assign,aper,newap
     new.Aperture=newap
  endif else self->CleanWAVSAMP,new,/PA_ONLY ;just clip "in place"
  
  npr=n_elements(*new.PR)
  for i=0,npr-1 do begin
     ;; Unless clipping the full aperture in place, scale the PR's
     if keyword_set(clip_full) eq 0 then begin
        ;; Start with the full slit pseudo-rect positions
        x=(*new.PR)[i].x
        y=(*new.PR)[i].y
        
        ;; Scale each aperture linearly from low to high, preserving angle
        low=(aper.low[0]+(aper.low[1]-aper.low[0])*float(i)/(npr-1))
        high=(aper.high[0]+(aper.high[1]-aper.high[0])*float(i)/(npr-1))
        
        delx=(x[0]-x[1]) & dely=(y[0]-y[1])
        x[0] = x[1]+high*delx
        y[0] = y[1]+high*dely
        x[1] = x[1]+low *delx
        y[1] = y[1]+low *dely
        
        delx=(x[3]-x[2]) & dely=(y[3]-y[2])
        x[3] = x[2]+high*delx
        y[3] = y[2]+high*dely
        x[2] = x[2]+low *delx
        y[2] = y[2]+low *dely
        
        ;; Set in the updated x,y
        (*new.PR)[i].x=x
        (*new.PR)[i].y=y
     endif
     
     ;; Actually clip the new PR
     if keyword_set(sp) then $
        pixels=polyfillaa((*new.PR)[i].x,(*new.PR)[i].y,128,128,AREAS=ar, $
                          POLYGONS=polys) $
     else pixels=polyfillaa((*new.PR)[i].x,(*new.PR)[i].y,128,128,AREAS=ar)
     
     if pixels[0] ne -1 then begin 
        ;; Overwrite the pixel and area pointers with new ones
        ;;  The old ones were either freed (if FULL), or still valid
        ;;  somewhere in the original full WAVSAMP (for a copy).
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
;  DeOverlap - Renormalize fractional areas for any areas of overlap
;              between two PRs, given a list of PRs.  This assumes at
;              most 2 PRs can overlap in any given location.  For unit
;              width, unit spaced PRs, this is valid up to tilt angles
;              of 60 degrees.
;=============================================================================
pro IRS_Calib::DeOverlap, prs
  thisx=prs[0].X & thisy=prs[0].Y
  for i=0,n_elements(prs)-2 do begin
     nextx=prs[i+1].X
     nexty=prs[i+1].Y
     ;; Clip the adjacent PR's to find the overlap
     twopolyclip,thisx,thisy,nextx,nexty
     if thisx[0] ne -1 then begin
        print,'Got an overlap: ',thisx,thisy
        ;; Clip the overlapping area to the grid
        overlap_pix=polyfillaa(thisx,thisy,128,128,AREAS=overlap_areas)
        if overlap_pix[0] ne -1 then begin
           wh=where_array(overlap_pix,*prs[i].PIXELS,cnt)
           if cnt gt 0 then begin 
              print,'deoverlapping pixels: ',(*prs[i].PIXELS)[wh]
              print,'with overlap areas: ',overlap_areas
              (*prs[i].AREAS)[wh]=(*prs[i].AREAS)[wh]-.5*overlap_areas
           endif 
           wh=where_array(overlap_pix,*prs[i+1].PIXELS,cnt)
           if cnt gt 0 then $
              (*prs[i+1].AREAS)[wh]=(*prs[i+1].AREAS)[wh]-.5*overlap_areas
        endif 
     endif 
     thisx=nextx & thisy=nexty
  endfor  
end


;=============================================================================
;  GetRecord - Get the calibration record for a given module and
;              order, or make one, if none yet exists.
;=============================================================================
function IRS_Calib::GetRecord, module, order, MUST_EXIST=me
  m=irs_module(module)
  if ptr_valid(self.cal[m]) then begin 
     wh=where((*self.cal[m]).Order eq order,cnt)
     if cnt gt 0 then return,(*self.cal[m])[wh[0]]
  endif 
  if keyword_set(me) then begin 
     message,'No such record exists: '+ $
             irs_module(module,/TO_NAME)+' Order: '+strtrim(order,2)
     return,-1
  endif 
  st={IRS_CalibRec}           ; None found, just create a new one
  st.Module=m
  st.Order=order
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
     wh=where((*self.cal[record.module]).Order eq record.order,cnt)
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
;  ReadCalib - Read (up to) all three calibration files for a given
;              module or modules, and record in the object.
;=============================================================================
pro IRS_Calib::ReadCalib,module, WAVSAMP_VERSION=wv,ORDER_VERSION=orv, $
                           TILT_VERSION=tv,ONLY=only
  @irs_dir                    ;get irs_calib_dir
  
  cals=['WAVSAMP','ORDFIND','LINETILT']
  version=[n_elements(wv)  gt 0?fix(wv)>0:0, $
           n_elements(orv) gt 0?fix(orv)>0:0, $
           n_elements(tv)  gt 0?fix(tv)>0:0]
  
  if keyword_set(only) then begin 
     which=where([n_elements(wv) ne 0, $
                  n_elements(orv) ne 0, $
                  n_elements(tv) ne 0],cnt)
     if cnt eq 0 then return
     cals=cals[which]
     version=version[which]
  endif 

  if n_elements(module) ne 0 then modules=[irs_module(module)] $
  else modules=indgen(4)        ;do them all, by default
  
  for i=0,n_elements(modules)-1 do begin 
     md=modules[i]
     for j=0,n_elements(cals)-1 do begin 
        ;; BEWARE!!!! This relies on file naming conventions which the
        ;; SSC has not yet agreed upon.
        base="irs_b"+strtrim(md,2)+"_"+ cals[j]+"v"
        
        ;; A specific version was requested, check it
        if version[j] gt 0 then begin 
           cfile=base+strtrim(version[j],2)+'.tbl'
           if file_test(filepath(ROOT=irs_calib_dir,SUBDIR="ssc", $
                                 cfile),/READ,/REGULAR) eq 0 then begin 
              message,'No such calibration file: '+cfile
              version[j]=0
           endif 
        endif 
        
        ;; Find all versions for this module
        if version[j] eq 0 then begin 
           cal_files=findfile(COUNT=fcnt,filepath(ROOT=irs_calib_dir, $
                                       SUBDIR="ssc",base+"*.tbl"))
           if fcnt eq 0 then begin 
              message, /CONTINUE,"Didn't find any calibration files: " +$
                       cals[j]+' for '+irs_module(md,/TO_NAME)
              continue
           endif 
           vers=max(fix((stregex(cal_files,base+"([0-9]+)"+".tbl$",$
                                 /EXTRACT,/SUBEXPR))[1,*]),mpos)
           cfile=cal_files[mpos] ;use the latest version
        endif 
        
        ;; Now that we have the relevant calibration file, parse it.
        call_method,'Parse'+cals[j],self,cfile,md
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
  orders=(data.order)[uniq(data.order)]
  for ord=0,n_elements(orders)-1 do begin 
     rec=self->GetRecord(m,orders[ord])
     wh=where(data.order eq orders[ord],cnt)
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
     ws={IRS_WAVSAMP, $
         0b,0.0,{IRS_APERTURE,[0.,0.],[1.,1.]},ptr_new(pr,/NO_COPY)}
     
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
  for i=0,n_elements(data.order)-1 do begin 
     rec=self->GetRecord(m,data[i].order)
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
  
  ;; set all of them at once (usually to 0)
  if (data.order)[0] eq 9999 then begin 
     if NOT ptr_valid(self.cal[m]) then return
     for i=0,n_elements(*self.cal[m])-1 do begin 
        (*self.cal[m])[i].c=[data[0].c0,data[0].c1,data[0].c2,data[0].c3]
     endfor 
  endif else begin
     for i=0,n_elements(data.order)-1 do begin
        rec=self->GetRecord(module,(data.order)[i])
        rec.c=[data[i].c0,data[i].c1,data[i].c2,data[i].c3]
        self->SetRecord,rec
     endfor
  endelse 
  self.TILT_FILE[m]=file
end

;=============================================================================
;  CleanupWAVSAMP - Delete the contents of one or more WAVSAMP records
;                   by freeing the internal pointers, or just clear
;                   the PIXELS and AREAS (and optionally POLYGONS) if
;                   PA_ONLY set.
;=============================================================================
pro IRS_Calib::CleanWAVSAMP, ws,PA_ONLY=pao
  for i=0,n_elements(ws)-1 do begin
     pr=ws[i].PR
     if ptr_valid(pr) then begin 
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
         WAVSAMP_FILE:strarr(5), $ ;the names of the wavsamp files (WAVSAMP)
         TILT_FILE:strarr(5),$  ;the name of the tilt file (c)
         ORDER_FILE:strarr(5), $ ;the name of the ordfind output file (a & b)
         cal: ptrarr(5)}        ;Lists of IRS_CalibRec structs, one list
                                ;for each module: 0:LH, 1:LL, 2:SH, 3:SL 4:MSED
  
  ;; The complete calibration set for a single order in one module.
  ;; Note that the bonus 1st order segment in the low-res modules is
  ;; known as order "3".
  st={IRS_CalibRec, $
      MODULE: 0, $              ;which module 0:LH, 1:LL, 2:SH, 3:SL
      ORDER: 0, $               ;the order number this data corresponds to.
      Date:0.0D, $              ;Date First constructed
      WAV_CENTER: 0.0, $        ;the central order wavelength
      WAV_MIN: 0.0, $           ;the minimum order wavelength
      WAV_MAX: 0.0, $           ;the maximum order wavelength
      SLIT_LENGTH: 0.0, $       ;the length of the slit in pixels
      A:fltarr(6), $            ;x(lambda)=sum_i a_i lambda^i
      B:fltarr(6), $            ;y(lambda)=sum_i b_i lambda^i
      C:fltarr(4), $            ;tilt_ang(s)=sum_i c_i s^i
      WAVSAMPS: ptr_new()}      ;A list of IRS_WAVSAMP structs
  
  ;; A wavsamp set for a single order and a given aperture, either
  ;; "traditional" (from the WAVSAMP file), or "pixel-based"
  ;; (generated directly from the A,B and C coefficients).
  st={IRS_WAVSAMP, $
      PIXEL_BASED: 0b, $        ;whether traditional or pixel-based WAVSAMP
      PR_WIDTH: 0.0, $          ;if PIXEL_BASED, the width of the PR
      Aperture:{IRS_APERTURE}, $ ;The IRS_APERTURE aperture
      PR: ptr_new()}            ;A list of IRS_WAVSAMP_PSEUDORECT structs
  
  ;; A single WAVSAMP pseudo-rectangle (PR), with pre-computed
  ;; full-slit overlap areas.  Vertices are listed counter-clockwise
  ;; from the top-right:
  ;;         1         0
  ;;         2         3
  st={IRS_WAVSAMP_PSEUDORECT , $
      lambda: 0.0, $            ;wavelength of PR center
      cen:[0.0,0.0], $          ;x,y, center of PR
      x:fltarr(4), $            ;X positions of the PR vertices
      y:fltarr(4), $            ;Y positions of the PR vertices
      angle: 0.0, $             ;Angle, anti-clockwise about x axis
      PIXELS: ptr_new(), $      ;The pixels at least partially inside the PR
      AREAS: ptr_new(), $       ;The area inside the PR for each of PIXELS.
      POLYGONS: ptr_new()}      ;(optional) the resulting clipped polygons
                                ; as a list of pointers to 2xn coord pairs
end
