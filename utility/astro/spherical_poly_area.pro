;+
; NAME:  
;
;    SPHERICAL_POLY_AREA
;
; DESCRIPTION:
;    
;    Calculate the solid angle subtended by a spherical polygon in
;    celestial coordinates.
;    
; CATEGORY:
;
;    Astro Utility
;
; CALLING SEQUENCE:
;
;    area=spherical_angle_offset(ra,dec)
;
; INPUT PARAMETERS:
;
;    (ra,dec): The RA/DEC for all vertices of the spherical polygon,
;    in decimal degrees.
;
; OUTPUT:
;
;    area: The solid angle subtended in steradians.
;    
; MODIFICATION HISTORY:
;    
;    2005-11-12 (J.D. Smith): Written
;-
;    $Id$
;##############################################################################
;
; LICENSE
;
;  Copyright (C) 2005 J.D. Smith
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
