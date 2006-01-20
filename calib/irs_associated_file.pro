;+
; NAME:
;
;    IRS_ASSOCIATED_FILE
;
; DESCRIPTION:
;
;    Turn a filename for an IRS file into a related filename, for
;    another data product.
;
; CATEGORY:
;
;    IRS Spectral Reduction, Analysis and Processing.
;    
; CALLING SEQUENCE:
;
;    afile=irs_associated_file(files,[BMASK=,UNCERTAINTY=,FLATAP=,DROOPRES=])
;
; INPUT PARAMETERS:
;
;    files: The filename or names of an IRS data product.
;
; KEYWORD PARAMETERS:
;
;    BMASK: If set, return the bmask equivalent of the passed file(s).
;
;    UNCERTAINTY: If set, return the func equivalent of the passed file(s).
;       
;    FLATAP: If set, return in the f2ap equivalent of the passed file(s).
;       
;    DROOPRES: If set, return in the droop equivalent of the passed file(s).
; 
; OUTPUTS:
;
;    afile:  The associated filenames.
;    
; NOTES:
;
;    Assumes all files are in the directory structure delivered by the
;    SSC Archive.
;
; MODIFICATION HISTORY:
;
;    2004-09-21 (J.D. Smith): Written
;-
;    $Id$
;##############################################################################
;
; LICENSE
;
;  Copyright (C) 2004, 2005 J.D. Smith
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

function irs_associated_file,file,BMASK=bm,UNCERTAINTY=unc,FLATAP=f2ap, $
                             DROOPRES=dr
  parts=stregex(file,'^(.*[0-9][._])[^.]+\.fits$',/EXTRACT,/SUBEXPR)
  base=parts[1,*]
  
  case 1 of  
     keyword_set(bm): begin 
        reg='bmask\.fits$' & filt='bmask'
     end 
     keyword_set(unc): begin 
        if keyword_set(f2ap) then begin
           filt='f2unc' & reg='f2unc\.fits$'           
        endif else begin 
           filt='func' & reg='func\.fits$'
        endelse 
     end 
     keyword_set(f2ap): begin 
        filt='f2ap' & reg='f2ap\.fits$'
     end 
     keyword_set(dr): begin 
        filt='droop' & reg='droop\.fits$'
     end 
     else: begin 
        filt='bcd*' & reg='bcd(_fp)?\.fits$'
     end 
  end 
  
  nb=n_elements(base) 
  ret=strarr(nb)
  for i=0,nb-1 do begin 
     if stregex(file[i],reg,/BOOLEAN) then begin 
        ret[i]=file[i]
        continue
     endif 
     if ~base[i] then continue
     f=file_search(base[i]+filt+'.fits',COUNT=cnt)
     if cnt gt 0 then ret[i]=f[0]
  endfor 
  return,nb eq 1?ret[0]:ret
end
