;+
; NAME:  
;
;    irs_restore_calib
;
; CONTACT:
;
;    UPDATED VERSIONS of CUBISM and more information can be found at:
;       http://sings.sirtf.edu/cubism
;
; DESCRIPTION:
;    
;    Restores saved IRS calibration objects.
;    
; CATEGORY:
;
;    IRS Calibration
;    	
; CALLING SEQUENCE:
;
;    cal=irs_restore_calib(file)
;
; INPUT PARAMETERS:
;
;    file: The calibration file containing the IRS_Calib object to
;       restore.
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
;    2002-08-17 (J.D. Smith): Restored calibration file path query.
;    2001-12-14 (J.D. Smith): Written
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
function irs_restore_calib, cfile
  @cubism_dir
  if file_test(cfile,/READ,/REGULAR) then file=cfile else begin 
     file=filepath(ROOT=irs_calib_dir,SUBDIR="sets",cfile)
     if file_test(file,/READ,/REGULAR) eq 0 then $
        message,'No such calibration object file: '+cfile
  endelse 
  return,restore_object(file,'IRS_Calib')
end
