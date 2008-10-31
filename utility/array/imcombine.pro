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
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2004,2005 J.D. Smith
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
  ;; MIN/MAX rejection
  if keyword_set(rmm) then begin 
     if n_elements(dims) lt 3 || dims[2] lt 3 then $
        message,'3-D with at least 3 planes required'
     if keyword_set(avg) then begin 
        ;; Average excluding min/max
        m=max(array,DIMENSION=3,max_list,MIN=m,SUBSCRIPT_MIN=min_list,/NAN)
        array[max_list]=0.0 & array[min_list]=0.0
        if n_elements(unc) ne 0 then begin 
           unc[max_list]=0.0 & unc[min_list]=0.0
           comb_unc=sqrt(total(unc^2,3,/NAN))/((total(finite(unc),3)-2)>1.)
        endif 
        return,total(array,3,/NAN)/((total(finite(array),3)-2)>1.)
     endif else if keyword_set(med) then begin 
        return,median(array,DIMENSION=3) ;median sans min/max is the same
     endif 
  end else begin 
     if n_elements(dims) eq 2 || dims[2] eq 1 then begin 
        if n_elements(unc) ne 0 then comb_unc=unc
        return,array
     endif 
     if n_elements(unc) ne 0 then comb_unc=sqrt(total(unc^2,3))/dims[2]
     if keyword_set(avg) then return,total(array,3)/dims[2] else $
        return,median(array,DIMENSION=3)
  endelse
  return,-1
end

;; Median, excluding min/max
; array[max_list]=!VALUES.F_INFINITY
; array[min_list]=!VALUES.F_
; keep_list=where(histogram([max_list,min_list], $
;                           MIN=0,MAX=narray-1) eq 0)
; dims[2]-=2                      ;two fewer
; new=make_array(TYPE=size(array,/TYPE),dims)
; new[0]=array[keep_list]
; if dims[2] le 1 then return,new else return,median(new,DIMENSION=3)
; endif
