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
     if n_elements(dims) eq 2 || dims[2] eq 1 then return,array
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
