;+
; NAME:
;
;    IRS_FILE_TYPE
;
; DESCRIPTION:
;
;    Return the file type of the IRS file passed.
;
; CATEGORY:
;
;    IRS Spectral Reduction, Analysis and Processing.
;    
; CALLING SEQUENCE:
;
;    type=irs_file_type(files,[IS_BCD=,IS_FLATAP=,IS_DROOP=)
;
; INPUT PARAMETERS:
;
;    files: The filename or names of an IRS data product.
;
; KEYWORD PARAMETERS:
;
;    IS_BCD: If set, return boolean if the file is a BCD file.
;
;    IS_FLATAP: If set, return boolean if the file is a FLATAP file.
;    
;    IS_DROOP: If set, return boolean if the file is a DROOPRES file.
;
; OUTPUTS:
;
;    type: The type code, or boolean value if a keyword is set.  The
;      code is:
;      
;        0: BCD.
;        1: DROOPRES
;        2: COADD2D
;        3: FLATAP
;        4: FUNC
;        5: F2UNC
;
; MODIFICATION HISTORY:
;
;    2005-07-26 (J.D. Smith): Written
;-
;    $Id$
;##############################################################################
;
; LICENSE
;
;  Copyright (C) 2005 J.D. Smith
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

function irs_file_type,file,IS_BCD=is_bcd,IS_FLATAP=is_flatap,IS_DROOP=is_droop
  case 1 of
     stregex(file,'bcd\.fits$',/BOOLEAN): type=0
     stregex(file,'f2ap\.fits$',/BOOLEAN): type=3
     stregex(file,'droop(res)?\.fits$',/BOOLEAN): type=1
     stregex(file,'coad[^.]*\.fits$',/BOOLEAN): type=2
     stregex(file,'func\.fits$',/BOOLEAN): type=4
     stregex(file,'f2unc\.fits$',/BOOLEAN): type=5
     else: type=-1
  endcase
  if keyword_set(is_bcd) then return,type eq 0
  if keyword_set(is_droop) then return,type eq 1
  if keyword_set(is_flatap) then return,type eq 3
  return,type
end
