function spherical_poly_area,ra,dec
  RADEG=180.D/!DPI
  HalfPi=!DPI/2.
  lam1=ra/RADEG
  lam2=shift(lam1,1)
  beta1=dec/RADEG
  beta2=shift(beta1,1)
  cbeta1=cos(beta1)
  cbeta2=shift(cbeta1,1)
  
  HavA=sin((beta2-beta1)/2.D)^2 + cbeta1*cbeta2*sin((lam2-lam1)/2.D)^2
  
  A= 2.D*asin(sqrt(HavA))         
  B= HalfPi-beta2              
  C= HalfPi-beta1              
  S= 0.5D*(A+B+C)                
  T= tan(S/2.D) * tan((S-A)/2.D) * tan((S-B)/2.D) * tan((S-C)/2.D)
  
  lam=(lam2-lam1) + 2*!DPI*(lam1 ge lam2)
  
  Excess= abs(4.D*atan(sqrt(abs(T)))) * (1.D - 2.D*(lam gt !DPI))
  return,abs(total(Excess*(lam2 ne lam1),/DOUBLE))
end
