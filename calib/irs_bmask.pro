;; Translate BMASK codes into conditions
pro irs_bmask,mask,PMASK=pm,SATURATED=sat,NO_FLAT=nf,NO_PLANES=np, $
              ONE_PLANE=op,CODE_STRING=cs
  pm =(mask AND 2L^14) ne 0b
  sat=(mask AND 2L^10) ne 0b
  nf =(mask AND 2L^8)  ne 0b
  np =(mask AND 2L^13) ne 0b 
  op =(mask AND 2L^12) ne 0b 
  if arg_present(cs) then $
     cs=(pm?'*':'.')+(sat?'S':'.')+(nf?'F':'.')+(op?'1':(np?'0':'.'))
end
