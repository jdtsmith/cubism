;+
; NAME:  
;
;    IRS_DIR
;
; DESCRIPTION:
;    
;    Sets up a common block (IRS_DIR) describing standard locations
;    in the IRS file structure, when included in procedures.
;    
; CATEGORY:
;
;    IRS IRS Spectral Reduction, Analysis and Processing.
;    Setup
;    	
; CALLING SEQUENCE:
;
;    @irs_dir (at the beginning of a file to use the irs_calib_dir variable)
;
; SIDE EFFECTS:
;
;    If the calibration data directory are not yet setup, they will be.
;
; RESTRICTIONS:
;
;    IRS_DIR relies on the IDL path containing the relevant
;    directories for accessing calibration files.  Normally, this
;    should work seamlessly.  If you have a highly customized IDL
;    !PATH, you must ensure that at least the calib/data directory is
;    represented there.
;
; EXAMPLE:
;
;    pro my_routine_which_needs_irs_directory_info
;       @irs_dir
;       f=func(irs_calib_dir)
;       ...
;
; MODIFICATION HISTORY:
;
;    2002-08-27 (J.D. Smith): Updated to use a marker procedure
;       irs_calib_dir_marker.pro.
;    2001-11-15 (J.D. Smith): Written
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2001,2002 J.D. Smith
;
;  This file is free software; you can redistribute it and/or modify it
;  under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2, or (at your option)
;  any later version.
;  
;  This file is distributed in the hope that it will be useful, but
;  WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;  General Public License for more details.
;  
;  You should have received a copy of the GNU General Public License
;  along with This file; see the file COPYING.  If not, write to the Free
;  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;  02111-1307, USA.
;
;##############################################################################
common irs_dir, irs_calib_dir
if n_elements(irs_calib_dir) eq 0 then begin
   irs_dir_marker
   source=(routine_info('irs_dir_marker',/SOURCE)).PATH
   dir=strmid(source,0,strpos(source,path_sep(),/REVERSE_SEARCH))
   if NOT file_test(dir,/DIRECTORY) then $
      message,'Cannot locate IRS calibration data directory.'
   irs_calib_dir=dir
endif
