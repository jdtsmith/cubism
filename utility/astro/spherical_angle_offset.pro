function spherical_angle_offset,ra1,dec1,ra2,dec2
  RADEG=180.D/!DPI
  d1=dec1/RADEG & d2=dec2/RADEG & r1=ra1/RADEG & r2=ra2/RADEG
  return,acos(cos(d1)*cos(d2)*cos(r1-r2) + sin(d1)*sin(d2))*RADEG
end
