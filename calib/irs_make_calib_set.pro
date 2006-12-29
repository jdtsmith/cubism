;+
; NAME:
;
;    MAKE_CALIB_SET
;
; DESCRIPTION:
;
;    Create a CUBISM calibration set from input calibration files, and
;    save it.
;
; CATEGORY:
;
;    IRS Spectral Reduction, Analysis and Processing.
;
; CALLING SEQUENCE:
;
;      make_calib_set,filename, [ WAVSAMP_VERSION=,ORDER_VERSION=, $
;                     LINETILT_VERSION=,FRAMETABLE_VERSION=, $
;                     PLATESCALE_VERSION=,PMASK_VERSION=, $
;                     FLUXCON_VERSION=,SLCF_VERSION=, $
;                     WAVECUT_VERSION=, PIXEL_OMEGA_VERSION=,
;                     RECOVER_FROM_WAVSAMP=, CALIB_OBJECT= ,
;                     /OVERWRITE]
;
; INPUT PARAMETERS:
;
;    filename: The file name ('file.cal') to save the set as, into
;       calib/data/sets/.  To make the file loadable automatically as
;       by date, name the file like 'irs_YYYY_MM_DD....cal'.
;       Otherwise, it can be loaded manually.
;
; KEYWORD PARAMETERS:
;
;    X_VERSION: Specify the numeric version of calibration file type X
;      (e.g. WAVSAMP), stored in calib/data/ssc/.  Must be an integer,
;      By default, the highest version available is automatically
;      employed.  For multiple versions of a given file, use the same
;      integer followed by a letter, e.g. v7 and v7a.  This is only
;      necessary for calibrations which change depending on the date
;      the data were obtained (e.g. LH Fluxcon bias change).
;      
;    RECOVER_FROM_WAVSAMP: Rather than using the LINETILT and ORDER
;      calibration files (not shipped by default from the SSC with
;      archive data), recover the values by fitting polynomials to the
;      WAVSAMP.  Only needed if corresponding LINETILT and ORDER files
;      are not available.
;
;    CALIB_OBJECT: On output, a variable to hold the calibration object.
;
;    OVERWRITE: If set, overwrite any existing calibration set.
;
; NOTES:
;
;    Typically, a given set of calibration inputs are only applicable
;    to a single pipeline processing version (e.g. S15).  Though no
;    check is made, using outdated calibration files with current data
;    can create problems.
;    
; MODIFICATION HISTORY:
;
;    2006-12-29 (J.D. Smith): Written
;-
;    $Id$
;##############################################################################
;
; LICENSE
;
;  Copyright (C) 2006 J.D. Smith
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
;  Boston, MA 02110-1301, USA.
;
;##############################################################################

pro make_calib_set,calibname,CALIB_OBJECT=c,OVERWRITE=ow,_EXTRA=e
  @cubism_dir
  on_error,2
  
  if ~stregex('\.cal$',calibname,/BOOLEAN) then $
     calibname+='.cal'
  file=filepath(ROOT=irs_calib_dir,SUBDIRECTORY='sets',calibname)
  if ~keyword_set(ow) && file_test(file) then $
     message,'Calibration file exists: ' + file
  
  c=obj_new('IRS_Calib')
  c->ReadCalib,_EXTRA=e
  for md=0,3 do begin
     ords=c->Orders(md)
     for ord=0,n_elements(ords)-1 do c->PixelWAVSAMP,md,ords[ord]
  endfor

  save,c,FILENAME=file,/COMPRESS
end
