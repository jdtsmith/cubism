;; Translate BMASK codes into conditions
pro irs_bmask,mask,PMASK=pm,SATURATED=sat,NO_FLAT=nf,NO_PLANES=np, $
              ONE_PLANE=op,CODE_STRING=cs
  flags=bytarr(15)
  for i=0,14 do flags[i]=(mask AND 2^i) ne 0
  ;;pm =(mask AND 2^14) ne 0b     ;pmask flagged 
  ;;sat=(mask AND 2^10) ne 0b    ;saturated
  ;;nf =(mask AND 2^8)  ne 0b    ;No flat
  ;;np =(mask AND 2^13) ne 0b    ;No planes
  ;;op =(mask AND 2^12) ne 0b    ;One plane
  if arg_present(cs) then begin 
     cs=(flags[14]?'*':'.')+(flags[10]?'S':'.')+(flags[8]?'F':'.')+ $
        (flags[12]?'1':(flags[13]?'0':'.'))
     if (mask AND 35583UL) gt 0 then begin 
        flags[[14,10,8,13,12]]=0b
        cs+="["+strjoin(strtrim(where(flags),2),",")+"]"
     endif 
  endif 
     
end
