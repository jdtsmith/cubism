;; Convert an IRS data filename to an existing bmask/uncertainty/bcd file
function irs_associated_file,file,BMASK=bm,UNCERTAINTY=unc,FLATAP=f2ap, $
                             DROOPRES=dr
  parts=stregex(file,'^(.*[0-9][._])[^.]+\.fits$',/EXTRACT,/SUBEXPR)
  base=parts[1,*]
  
  case 1 of  
     keyword_set(bm): begin 
        reg='bmask\.fits$' & filt='bmask'
     end 
     keyword_set(unc): begin 
        filt='func' & reg='func\.fits$'
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
