;; Translate BMASK codes into conditions
pro irs_bmask,mask,PMASK=pm,SATURATED=sat,NO_FLAT=nf,NO_PLANES=np, $
              ONE_PLANE=op,CODE_STRING=cs
  flags=bytarr(15)
  for i=0,14 do flags[i]=(mask AND 2U^i) ne 0
  pm =flags[14] ne 0b           ;pmask flagged 
  sat=flags[10] ne 0b           ;saturated
  nf=flags[8] ne 0b             ;No flat
  op=flags[12] ne 0b            ;One plane
  np=flags[13] ne 0b            ;No planes

  if arg_present(cs) then begin 
     cs=(pm?'*':'.')+(sat?'S':'.')+(nf?'F':'.')+ $
        (op?'1':(np?'0':'.'))
     if (mask AND 35583U) gt 0 then begin 
        flags[[14,10,8,13,12]]=0b
        cs+="["+strjoin(strtrim(where(flags),2),",")+"]"
     endif 
  endif
     
end
