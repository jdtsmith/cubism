;+
; NAME:  
;
;    SMART_DIR
;
; CONTACT:
;
;    UPDATED VERSIONs of SMART and more information can be found at:
;       http://isc.astro.cornell.edu/smart/download
;
; DESCRIPTION:
;    
;    Sets up a common block (SMART_DIR) describing standard locations
;    in the SMART file structure, when included in procedures.
;    
; CATEGORY:
;
;    SMART IRS Spectral Reduction, Analysis and Processing.
;    Setup
;    	
; CALLING SEQUENCE:
;
;    SMART_DIR
;
; SIDE EFFECTS:
;
;    If the directories are not yet setup, they will be.
;
; RESTRICTIONS:
;
;    SMART_DIR relies on the IDL !PATH to contain the relevant
;    directories for accessing, e.g., calibration files.  Normally,
;    this should work seamlessly.  If you have a highly customized IDL
;    !PATH, you must ensure that at least the smart/calib directory is
;    represented there.
;
; EXAMPLE:
;
;    pro my_routine_which_needs_smart_directory_info
;       @smart_dir
;       ...
;
; MODIFICATION HISTORY:
;
;    2001-11-15 (J.D. Smith): Written
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

common smart_dir, smart_calib_dir

if n_elements(smart_calib_dir) eq 0 then begin
   ;;figure out the calibration directory
   if !VERSION.RELEASE le '5.4.1' then sep=":" else $
      sep=call_function('path_sep',/SEARCH_PATH)
   paths=strsplit(!PATH,sep,/EXTRACT)
   wh=where(stregex(paths,filepath(ROOT="smart","calib")+'$') ne -1,cnt)
   if cnt eq 0 then message,'Cannot locate SMART calibration directory.'
   smart_calib_dir=paths[wh[0]]
endif
