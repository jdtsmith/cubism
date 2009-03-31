;+
; NAME:  
;
;    IMCOMBINE
;
; DESCRIPTION:
;    
;    Combine 3D stack of images.
;    
; CATEGORY:
;
;    Array utility
;    	
; CALLING SEQUENCE:
;
;    im=imcombine(array,[AVERAGE=,MEDIAN=,REJECT_MINMAX=, $
;                        UNCERTAINTY=,COMBINED_UNCERTAINTY=])
; INPUT PARAMETERS:
;
;    array: The 3D array to combine.
;
; INPUT KEYWORD PARAMETERS:
;
;    AVERAGE: Combine by averate
;    
;    MEDIAN: Combine by median
;
;    REJECT_MINMAX: Combine after rejecting the min and max along the
;       3rd dimension.  Must have at least 3 planes.
;
;    UNCERTAINTY: Input uncertainty image stack, same dimensions as
;       array.
;
; OUTPUT KEYWORD PARAMETERS:
;
;    COMBINED_UNCERTAINTY: The combined image's uncertainty.
;
; OUTPUTS:
;
;    im: The combined image.
;
; EXAMPLE:
;
;    im=imcombine(randomu(sd,10,10,5),/AVERAGE,/REJECT_MINMAX)
;    
; MODIFICATION HISTORY:
;    
;    2004-02-06 (J.D. Smith): Written
;    2008-11-05 (J.D. Smith): Fixed NaN Treatment for untrimmed averaged.
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2004,2005,2008 J.D. Smith
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

function imcombine,array,AVERAGE=avg,MEDIAN=med,REJECT_MINMAX=rmm, $
                   UNCERTAINTY=unc,COMBINED_UNCERTAINTY=comb_unc
  dims=size(array,/DIMENSIONS)
  narray=n_elements(array) 
  
  if n_elements(dims) eq 2 || dims[2] eq 1 then begin 
     ;; Nothing to combine, just return
     if n_elements(unc) ne 0 then comb_unc=unc
     return,array
  endif 

  if keyword_set(avg) then begin 
     ;; Average
     if keyword_set(rmm) then begin 
        ;; MIN/MAX rejection
        if n_elements(dims) lt 3 || dims[2] lt 3 then $
           message,'Array with at least 3 planes required'
        ;; Average excluding min/max
        m=max(array,DIMENSION=3,max_list,MIN=m,SUBSCRIPT_MIN=min_list,/NAN)
        array[max_list]=0.0 & array[min_list]=0.0
        if n_elements(unc) ne 0 then begin 
           unc[max_list]=0.0 & unc[min_list]=0.0
           cnt_unc=(total(finite(unc),3)-2)>1.
        endif 
        cnt=(total(finite(array),3)-2)>1.
     endif else begin 
        ;; No rejection, use everything
        cnt=total(finite(array),3)>1.
        if n_elements(unc) ne 0 then cnt_unc=total(finite(unc),3)>1.
     endelse 
     
     if n_elements(unc) ne 0 then comb_unc=sqrt(total(unc^2,3,/NAN))/cnt_unc
     return,total(array,3,/NAN)/cnt
  endif else if keyword_set(med) then begin 
     ;; Median sans min/max is the same thing!
     return,median(array,DIMENSION=3) 
  endif 
  return,-1
end
