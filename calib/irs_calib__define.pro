;+
; NAME:  
;
;    SMART_Calib
;
; CONTACT:
;
;    UPDATED VERSIONs of SMART and more information can be found at:
;       http://isc.astro.cornell.edu/smart/download
;
; DESCRIPTION:
;    
;    A class defining SMART calibration objects, each of which hold a
;    specific set of WAVSAMP, order, wavelength, and tilt solution
;    calibration data for all orders of all modules.
;    
; CATEGORY:
;
;    SMART IRS Spectral Reduction, Analysis and Processing.
;    Extraction and Calibration
;    	
; COMMON BLOCKS:
;
;    SMART_DIR: Included through file smart_dir.pro.  Defines the path
;       location of the SMART calibration directory.  This path,
;       smart_calib_dir, is auto-defined if it doesn't yet exist, and
;       is taken from the !PATH.
;
; METHODS:
;
;    Init:  
;
;       CALLING SEQUENCE:
;
;          obj=obj_new('SMART_Calib',name)
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
;            Can either be a string (LH,LL,SH,SL) or integer (0-3,
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
;          record: The SMART_CalibRec record structure for the given
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
;          Return a list of SMART_WAVSAMP_PSEUDORECT structures for
;          the given module, order and aperture.  If, for a given
;          module and order, no clipped SMART_WAVSAMP exists, create
;          one, append it to the list, and return.  WAVSAMP clipping
;          is very expensive.  This serves to cache commonly used
;          apertures for quicker access to extraction boundaries.
;          	
;       CALLING SEQUENCE:
;
;          wavsamp=obj->GetWAVSAMP(module,order, [aperture, /FULL])
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
;	OPTIONAL INPUT PARAMETERS:
;
;          aperture: A structure of the form:
;          
;               {SMART_APERTURE, $
;                  Low: [low_top,low_bottom], $
;                  High:[high_top,high_bottom]}
;             
;             specifying the normalized coordinates along the slit for
;             low and high aperture boundaries.  See
;             SMART_Aperture__define for more information.  If
;             omitted, the full slit aperture is used.
;             
;       INPUT KEYWORD PARAMETERS:
;
;          FULL: If set, the full slit aperture is used, and any
;             passed aperture argument is ignored.
;             
;	OUTPUTS:
;
;          wavsamp: The SMART_WAVSAMP structure for the given module,
;             order and aperture.  See definition at end of file.
;            
;    ReadCalib:
;    
;	DESCRIPTION:
;
;          Read and parse calibration data from files and populate the
;          SMART_Calib object.
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
;             _VERSION keywords described below will be read, as
;             opposed to the normal case of the *latest* version being
;             used if none is specified.
;             
;          WAVSAMP_VERSION: The version number of the WAVSAMP file to
;             use.  Defaults to the latest version, unless ONLY is
;             set, in which case no WAVSAMP data is read or set for
;             the module(s).  A version of 0 (zero) means use the
;             latest version.
;
;          ORDER_VERSION: The version number of the order
;             position/wavelength solution file to use.  Defaults to
;             the latest version, unless ONLY is set, in which case no
;             ORDER data is read or set for the module(s).  A version
;             of 0 (zero) means use the latest version.
;
;          TILT_VERSION: The version number of the tilt solution file
;             to use.  Defaults to the latest version, unless ONLY is
;             set, in which case no TILT data is read or set for the
;             module(s).  A version of 0 (zero) means use the latest
;             version.
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
;          record: The SMART_CalibRec record structure to be recorded
;            in the object.
;            
;          
; PROCEDURE:
;
;    read_ipac_table: For reading files in the form of IPAC tables.
;    
; NOTES:
;    
;    IRS calibration data is generated at the SSC, and used by SMART
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
;    The SMART_Calib class should encapsulate all calibration data
;    necessary for full reduction of IRS spectra, and should be
;    expanded as appropriate.  Each individual SMART_Calib object
;    represents a particular snapshot of calibration data, which may
;    evolve over time, due to updates in the calibration thread, or
;    changing instrumental preformance.  This object can be loaded and
;    saved into a SMART_Proj (either fully, or just by name), in order
;    to associate a given extraction with the calibration parameters
;    associated with it.
;
;    See the document "SSC--ISC: Interface Products and Conventions"
;    for further description of these calibration products.
;
;    SMART_Calib object sets can be found in the subdirectory
;    calib/sets, under the smart installation path.
;
; INHERITANCE TREE:
;
;     SMART_Calib
;
; EXAMPLE:
; 
;     Populate a new cal object with all the latest calibration data:
;     
;    cal=obj_new('SMART_Calib','2002-04-01: Pre-Launch')
;    cal->ReadCalib
;
;     Update the WAVSAMP to the latest version for long-low only:
;     
;    cal=sm_restore_calib('sm_2001_12_14.cal') 
;    a->ReadCalib,'LL',WAVSAMP_VERSION=0,/ONLY
;
; MODIFICATION HISTORY:
;
;       2001-12-08 (J.D. Smith): Written.
; 
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2001 Cornell University
;
;  This file is part of SMART.
;
;  SMART is free software; you can redistribute it and/or modify it
;  under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2, or (at your option)
;  any later version.
;  
;  SMART is distributed in the hope that it will be useful, but
;  WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;  General Public License for more details.
;  
;  You should have received a copy of the GNU General Public License
;  along with SMART; see the file COPYING.  If not, write to the Free
;  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;  02111-1307, USA.
;
;##############################################################################

;=============================================================================
;       GetProperty - Get object properties
;=============================================================================
pro SMART_Calib::GetProperty, NAME=name
  if arg_present(name) then name=self.name
end

;=============================================================================
;       SetProperty - Set object properties
;=============================================================================
pro SMART_Calib::SetProperty,NAME=name
  if n_elements(name) eq 0 then self.name=name
end

;=============================================================================
;       Print - Print a listing of what this cal object contains.
;=============================================================================
pro SMART_Calib::Print, modules
  if n_elements(modules) eq 0 then modules=indgen(4)
  print,' == SMART Calibration Object: '+self.Name+' =='
  for i=0,n_elements(modules)-1 do begin 
     md=smart_module(modules[i])
     module=smart_module(md,/TO_NAME)
     no=ptr_valid(self.cal[md])?n_elements(*self.cal[md]):0
     print,FORMAT='(%"\n ==> Module %s: %s")',module, $
           (no gt 0?strtrim(n_elements(*self.cal[md]),2)+ $
            " orders.":" not loaded.")
     if no eq 0 then continue
     print,''
     wsf=self.WAVSAMP_FILE[md]
     if wsf ne '' then print,FORMAT='(A12,": ",A)', 'WAVSAMP',wsf
     orf=self.ORDER_FILE[md]
     if orf ne '' then print,FORMAT='(A12,": ",A)', 'ORDER',orf
     tif=self.TILT_FILE[md]
     if tif ne '' then print,FORMAT='(A12,": ",A)', 'TILT',tif
     print,''
     for j=0,no-1 do begin 
        rec=(*self.cal[md])[j]
        npr=n_elements(*(*rec.WAVSAMPS)[0].PR)

        print,FORMAT='(%"    ==> Order %2s%d  (%s) -- %03d samples: ")', $
              module,rec.order,rec.Date eq 0.0D?"--":jul2date(rec.Date),npr
        nw=n_elements(*rec.WAVSAMPS) 
        
        print,"         A:"+strjoin(string(FORMAT='(G10.3)',rec.A))
        print,"         B:"+strjoin(string(FORMAT='(G10.3)',rec.B))
        print,"         C:"+strjoin(string(FORMAT='(G10.3)',rec.C))
        print,"         Apertures:"
        for k=0,nw-1 do begin 
           ws=(*rec.WAVSAMPS)[k]
           print, FORMAT='(%"           %4.2f->%4.2f : %4.2f->%4.2f")', $
                  ws.Aperture.low,ws.Aperture.high
        endfor 
        print,""
     endfor 
  endfor 
end

;=============================================================================
;       Orders - Return the list of orders for a given module
;=============================================================================
function SMART_Calib::Orders,module
  m=smart_module(module)
  if NOT ptr_valid(self.cal[m]) then return,-1
  return,(*self.cal[m]).ORDER
end

;=============================================================================
;       GetWAVSAMP - Get the list of SMART_WAVSAMP_PSEUDORECT
;                    structures for a given module, order, and
;                    aperture (or the FULL aperture).
;=============================================================================
function SMART_Calib::GetWAVSAMP, module, order, aperture, FULL=full
  rec=self->GetRecord(module,order,/MUST_EXIST)
  if size(rec,/TYPE) ne 8 then message,'No such record exists: '+ $
     smart_module(module,/TO_NAME)+' Order: '+strtrim(order,2)
  if n_elements(aperture) eq 0 or keyword_set(full) then $
     aperture={low:[0.,0.],high:[1.,1.]}
  if ptr_valid(rec.WAVSAMPS) then begin 
     aps=(*rec.WAVSAMPS).Aperture
     na=n_elements(aps)
     match=where(total(aps.low eq rebin(aperture.low,2,na) AND $
                       aps.high eq rebin(aperture.high,2,na),1) eq 2,cnt)
     if cnt gt 0 then return,*(*rec.WAVSAMPS)[match[0]].PR
  endif
  
  ;; None exists for this aperture, so make a new one
  ws=self->Clip(module,order,aperture) ;this creates new pointers
  
  ;; And add it to the cache for rapid recovery.
  if ptr_valid(rec.WAVSAMPS) then *rec.WAVSAMPS=[*rec.WAVSAMPS,ws] else $
     rec.WAVSAMPS=ptr_new(ws)
  
  ;; Just give the list of pseudo-rects
  return,*ws.PR
end

;=============================================================================
;       Clip - Clip a WAVSAMP with specified aperture against the
;              pixels, and return a pointer to a SMART_WAVSAMP
;              structure.  Apertures are specified in normalized
;              coordinates, with 0.0 at the "bottom" of the slit (at
;              left), and 1.0 at right in the "top" of the slit:
;
;                     0.0                       1.0 
;                      |=========================|
;             
;                        <--low                   
;                      1-------------    high-->  
;                      |             \-----------0 
;                      2-------------            |
;                                    \-----------3
;=============================================================================
function SMART_Calib::Clip, module, order, aper,FULL=clip_full
  rec=self->GetRecord(module,order,/MUST_EXIST)
  if size(rec,/TYPE) ne 8 then return,-1
  if NOT ptr_valid(rec.WAVSAMPS) then return, -1
  aps=(*rec.WAVSAMPS).Aperture
  
  ;; Find the always-present full slit aperture wavsamp clip, i.e.
  ;;    low: [0.,0.] ; high: [1.,1.]
  wh=where(total(aps.low eq 0. AND aps.high eq 1.,1) eq 2., cnt)
  if cnt eq 0 then message,'Must have full slit aperture WAVSAMP.'
  full=(*rec.WAVSAMPS)[wh[0]]
  
  new=full
  if keyword_set(clip_full) eq 0 then begin
     new.PR=ptr_new(*full.PR)   ; Copy the psuedo-rects
     newap=new.Aperture
     struct_assign,aper,newap
     new.Aperture=newap
  endif
  npr=n_elements(*new.PR)
  for i=0,npr-1 do begin
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
        x[1] = x[1]+low* delx
        y[1] = y[1]+low* dely
        
        delx=(x[3]-x[2]) & dely=(y[3]-y[2])
        x[3] = x[2]+high*delx
        y[3] = y[2]+high*dely
        x[2] = x[2]+low* delx
        y[2] = y[2]+low* dely
        
        ;; Set in the updated x,y
        (*new.PR)[i].x=x
        (*new.PR)[i].y=y
     endif
     
     pixels=polyfillaa((*new.PR)[i].x,(*new.PR)[i].y,128,128,AREAS=ar)
     
     if pixels[0] ne -1 then begin 
        ;; Overwrite the pixel and area pointers with new ones
        (*new.PR)[i].PIXELS=ptr_new(pixels,/NO_COPY)
        (*new.PR)[i].AREAS =ptr_new(ar,/NO_COPY)
     endif 
  endfor 
  good=where(ptr_valid((*new.PR).PIXELS),goodcnt)
  if goodcnt eq 0 then message,'No WAVSAMP pseudo-rects on array'
  if goodcnt lt npr then *new.PR=(*new.PR)[good]
  return,new
end

;=============================================================================
;       GetRecord - Get the calibration record for a given module and
;                   order, or make one, if none yet exists.
;=============================================================================
function SMART_Calib::GetRecord, module, order, MUST_EXIST=me
  m=smart_module(module)
  if ptr_valid(self.cal[m]) then begin 
     wh=where((*self.cal[m]).Order eq order,cnt)
     if cnt gt 0 then return,(*self.cal[m])[wh[0]]
  endif 
  if keyword_set(me) then return,-1
  st={SMART_CalibRec}           ; Just create a new one
  st.Module=m
  st.Order=order
  return,st
end

;=============================================================================
;       SetRecord - Set a calibration record for a given module and
;                   order.  If such a record already exists,
;                   overwrite, otherwise append to the end of the
;                   list.  
;=============================================================================
pro SMART_Calib::SetRecord, record
  if tag_names(record,/STRUCTURE_NAME) ne 'SMART_CALIBREC' then $
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
;       ReadCalib - Read (up to) all three calibration files for a
;                   given module or modules, and record in the object.
;=============================================================================
pro SMART_Calib::ReadCalib,module, WAVSAMP_VERSION=wv,ORDER_VERSION=orv, $
                           TILT_VERSION=tv,ONLY=only
  @smart_dir                    ;get smart_calib_dir
  
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

  if n_elements(module) ne 0 then modules=[smart_module(module)] $
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
           if file_test(filepath(ROOT=smart_calib_dir,SUBDIR=["data","ssc"], $
                                 cfile),/READ,/REGULAR) eq 0 then begin 
              message,'No such calibration file: '+cfile
              version[j]=0
           endif 
        endif 
        
        ;; Find all versions for this module
        if version[j] eq 0 then begin 
           cal_files=findfile(COUNT=fcnt,filepath(ROOT=smart_calib_dir, $
                                       SUBDIR=["data","ssc"],base+"*.tbl"))
           if fcnt eq 0 then begin 
              message, /CONTINUE,"Didn't find any calibration files: " +$
                       cals[j]+' for '+smart_module(md,/TO_NAME)
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
;       ParseWAVSAMP - Read and parse the specified WAVSAMP file,
;                      clipping the PSUEDO-RECT full-slit polygons for
;                      fast access.
;=============================================================================
pro SMART_Calib::ParseWAVSAMP,file,module
  m=smart_module(module)
  data=read_ipac_table(file)
  orders=(data.order)[uniq(data.order)]
  for ord=0,n_elements(orders)-1 do begin 
     rec=self->GetRecord(m,orders[ord])
     wh=where(data.order eq orders[ord],cnt)
     if cnt eq 0 then continue  ;no data for this order
     ;; Collect the data and create the WAVSAMP structure list
     pr=replicate({SMART_WAVSAMP_PSEUDORECT},cnt)
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
     ws={SMART_WAVSAMP,{SMART_APERTURE,[0.,0.],[1.,1.]},ptr_new(pr,/NO_COPY)}
     
     ;; Clean out any old WAVSAMP's
     if ptr_valid(rec.WAVSAMPS) then begin 
        self->CleanWAVSAMP,*rec.WAVSAMPS
        ptr_free,rec.WAVSAMPS
     endif 
     
     rec.WAVSAMPS=ptr_new(ws,/NO_COPY)
     ;; Put the record back
     self->SetRecord,rec
     
     ;; Clip the full slit as a starting point.
     c=self->Clip(m,orders[ord],/FULL)
  endfor
  self.WAVSAMP_FILE[m]=file
end

;=============================================================================
;       ParseOrdFind - Read and parse the specified ORDFIND file,
;                      saving the polynomial fit data in the object.
;=============================================================================
pro SMART_Calib::ParseOrdFind,file,module
  m=smart_module(module)
  data=read_ipac_table(file)
  for i=0,n_elements(data.order)-1 do begin 
     rec=self->GetRecord(m,data[i].order)
     rec.a=[data[i].a0,data[i].a1,data[i].a2,data[i].a3,data[i].a4,data[i].a5]
     rec.b=[data[i].b0,data[i].b1,data[i].b2,data[i].b3,data[i].b4,data[i].b5]
     self->SetRecord,rec
  endfor
  self.ORDER_FILE[m]=file
end

;=============================================================================
;       ParseLineTilt - Read and parse the specified LINETILT file,
;                       saving the polynomial fit data in the object.
;=============================================================================
pro SMART_Calib::ParseLineTilt,file,module
  m=smart_module(module)
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
;       CleanupWAVSAMP - Delete one or more WAVSAMP records by freeing
;                        the internal pointers.
;=============================================================================
pro SMART_Calib::CleanWAVSAMP, ws
  for i=0,n_elements(ws)-1 do begin
     pr=ws[i].PR
     if ptr_valid(pr) then ptr_free,(*pr).PIXELS,(*pr).AREAS,pr
  endfor 
end

;=============================================================================
;       Cleanup - Free all resources.
;=============================================================================
pro SMART_Calib::Cleanup
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
;       Init - Create a new SMART_Calib object
;=============================================================================
function SMART_Calib::Init,name
  if n_elements(name) ne 0 then self.Name=name
  return,1
end

;=============================================================================
;       SMART_Calib__define - Define the SMART_Calib class
;=============================================================================
pro SMART_Calib__define
  class={SMART_Calib, $         
         Name: '', $            ;A name for this SMART Calibration object
         WAVSAMP_FILE:strarr(4), $ ;the names of the wavsamp files (WAVSAMP)
         TILT_FILE:strarr(4),$  ;the name of the tilt file (c)
         ORDER_FILE:strarr(4), $ ;the name of the ordfind output file (a & b)
         cal: ptrarr(4)}        ;Four lists of SMART_CalibRec structs, one
                                ;for each module: 0:LH, 1:LL, 2:SH, 3:SL
  
  ;; The complete calibration set for a single order in one module.
  ;; Note that the bonus 1st order segment in the low-res modules is
  ;; known as order "3".
  st={SMART_CalibRec, $
      Date:0.0D, $              ;Date First constructed
      MODULE: 0, $              ;which module 0:LH, 1:LL, 2:SH, 3:SL
      ORDER: 0, $               ;the order number this data corresponds to.
      A:fltarr(6), $            ;x(lambda)=sum_i a_i lambda^i
      B:fltarr(6), $            ;y(lambda)=sum_i b_i lambda^i
      C:fltarr(4), $            ;tilt_ang(s)=sum_i c_i s^i
      WAVSAMPS: ptr_new()}      ;A list of SMART_WAVSAMP structs
  
  
  ;; A wavsamp set for a single order and a given aperture.
  st={SMART_WAVSAMP, $
      Aperture:{SMART_APERTURE}, $ ;The SMART_APERTURE aperture
      PR: ptr_new()}            ;A list of SMART_WAVSAMP_PSEUDORECT structs
  
  ;; A single WAVSAMP pseudo-rectangle (PR), with pre-computed
  ;; full-slit overlap areas.  Vertices are listed counter-clockwise
  ;; from the top-right:
  ;;         1         0
  ;;         2         3
  st={SMART_WAVSAMP_PSEUDORECT , $
      lambda: 0.0, $            ;wavelength of PR center
      cen:[0.0,0.0], $          ;x,y, center of PR
      x:fltarr(4), $            ;X positions of the PR vertices
      y:fltarr(4), $            ;Y positions of the PR vertices
      angle: 0.0, $             ;Angle, anti-clockwise about x axis
      PIXELS: ptr_new(), $      ;The pixels at least partially inside the PR
      AREAS: ptr_new()}         ;The area inside the PR for each of PIXELS.
end
