;+
; NAME:  
;
;    CUBISM_DIR
;
; DESCRIPTION:
;    
;    Sets up a common block (CUBISM_DIR) giving the location of the
;    CUBISM main directory, and calibration data directory.  
;    
; CATEGORY:
;
;    IRS Spectral Reduction, Analysis and Processing.
;    Setup
;    	
; CALLING SEQUENCE:
;
;    @cubism_dir (at the beginning of a routine to use the
;    irs_calib_dir or cubism_dir variables)
;
; SIDE EFFECTS:
;
;    If the calibration data directory are not yet setup, they will be.
;
; RESTRICTIONS:
;
;    CUBISM_DIR relies on the IDL path containing the relevant
;    directories for accessing calibration files.  Normally, this
;    should work seamlessly.  If you have a highly customized IDL
;    !PATH, you must ensure that at least the calib/data directory is
;    represented there.  Do not move any procedure, or the cubism.sav
;    file (if running through the VM).
;
; EXAMPLE:
;
;    pro my_routine_which_needs_irs_directory_info
;       @cubism_dir
;       f=func(irs_calib_dir)
;       ...
;
; MODIFICATION HISTORY:
;
;    2003-12-10 (J.D. Smith): Renamed from irs_dir to cubism_dir and
;       relocated to support calling from the IDLVM.  No longer using
;       irs_calib_dir_marker.pro.
;    2002-08-27 (J.D. Smith): Updated to use a marker procedure
;       irs_calib_dir_marker.pro.
;    2001-11-15 (J.D. Smith): Written
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2001-2003 J.D. Smith
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
;  along with This file; see the file COPYING.  If not, write to the
;  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;  Boston, MA 02110-1301, USA.
;
;##############################################################################
common cubism_dir, irs_calib_dir,cubism_dir
if n_elements(irs_calib_dir) eq 0 then begin
   resolve_routine,'cubism',/NO_RECOMPILE
   cubism_dir=(routine_info('cubism',/SOURCE)).PATH
   ps=path_sep()
   if strmid(cubism_dir, 0,1) ne ps then $ ;relative filename
      cubism_dir=file_expand_path(cubism_dir)
   for i=0,1 do $
      cubism_dir=strmid(cubism_dir,0,strpos(cubism_dir,ps,/REVERSE_SEARCH))
   dir=filepath(ROOT=cubism_dir,SUBDIR='calib','data')
   if NOT file_test(dir,/DIRECTORY) then $
      message,'Cannot locate IRS calibration data directory: '+dir
   irs_calib_dir=dir
endif
