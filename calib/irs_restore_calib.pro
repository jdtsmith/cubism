;+
; NAME:  
;
;    IRS_RESTORE_CALIB
;
; CONTACT:
;
;    UPDATED VERSIONS of CUBISM and more information can be found at:
;       http://ssc.spitzer.caltech.edu/cubism
;
; DESCRIPTION:
;    
;    Restores saved IRS calibration objects, and caches results.
;    
; CATEGORY:
;
;    IRS Calibration
;    	
; CALLING SEQUENCE:
;
;    cal=irs_restore_calib(file,[/NON_CACHED])
;
; INPUT PARAMETERS:
;
;    file: The calibration file containing the IRS_Calib object to
;       restore.
;			
; INPUT KEYWORD PARAMETERS:
;
;    NON_CACHED: If set, don't recover from the cache, read from disk.
;       Can create multiple copies of the same basic object in the
;       session.
;
; OUTPUTS:
;
;    cal: The IRS_Calib object (see irs_calib__define.pro).
;
; COMMON BLOCKS:
;
;    CUBISM_DIR: For locating the IRS calibration file directories.
;
; MODIFICATION HISTORY:
;
;    2004-02-24 (J.D. Smith): Cache restored objects for effiency.
;    2002-08-17 (J.D. Smith): Restored calibration file path query.
;    2001-12-14 (J.D. Smith): Written
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2001,2002,2004 J.D. Smith
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
;  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;
;##############################################################################
function irs_restore_calib, cfile,NON_CACHED=nc,RECENT=recent
  common irs_calib_store, irs_calib_store_files, irs_calib_store_objects
  @cubism_dir
  if keyword_set(recent) || n_elements(cfile) eq 0 then $
     cfile=irs_recent_calib()
  if file_test(cfile,/READ,/REGULAR) then file=cfile else begin 
     file=filepath(ROOT=irs_calib_dir,SUBDIR="sets",cfile)
     if file_test(file,/READ,/REGULAR) eq 0 then $
        message,'No such calibration object file: '+cfile
  endelse 
  
  ;; Recover from the stored object cache
  if ~keyword_set(nc) && n_elements(irs_calib_store_files) ne 0 then begin 
     keep=where(obj_valid(irs_calib_store_objects),keep_cnt,NCOMPLEMENT=reject)
     if keep_cnt ne 0 then begin 
        if reject gt 0 then begin 
           irs_calib_store_files=irs_calib_store_files[keep]
           irs_calib_store_objects=irs_calib_store_objects[keep]
        endif 
        match=where(irs_calib_store_files eq file,match_cnt)
        if match_cnt gt 0 then return, irs_calib_store_objects[match[0]]
     endif 
  endif else keep_cnt=0
  
  cal=restore_object(file,'IRS_Calib',OTHER_CLASSES='irs_aperture')
  cal->Validate
  
  ;; Store to the cache
  if ~keyword_set(nc) then begin 
     if keep_cnt eq 0 then begin
        irs_calib_store_files=[file] & irs_calib_store_objects=[cal] 
     endif else begin 
        irs_calib_store_files=[irs_calib_store_files,file] 
        irs_calib_store_objects=[irs_calib_store_objects,cal]
     endelse 
  endif 
                                                            
  return,cal
end
