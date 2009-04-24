;+
; NAME:  
;
;    SPHERICAL_COORD_ROTATE
;
; DESCRIPTION:
;    
;    Rotate a given celestial coordinate by an amount and about an
;    axis implied by two other points.  E.g. given a reference point
;    (ra_ref, dec_ref), and a new point (ra_new, dec_new) into which
;    the reference point will be rotated, update the original point
;    (ra, dec) by applying this same spherical rotation.
;    
; CATEGORY:
;
;    Astro Utility, Coordinate Offseting
;
; CALLING SEQUENCE:
;
;    spherical_coord_rotate, ra_ref,dec_ref,ra_new,dec_new,ra,dec
;
; INPUT PARAMETERS:
;
;    (ra,dec)_ref: The RA/DEC for the reference position in decimal
;       degrees.
;       
;    (ra,dec)_new: The new coordinate into which (ra_ref,dec_ref) is
;      to be rotated, in decimal degrees.
;
;    (ra, dec): The coordinate to update with the rotation implied by
;      the above two points, in decimal degrees.
;
; OUTPUT:
;
;    (ra, dec): Will be udpated by rotating along the angle between
;      (ra_ref, dec_ref) and (ra_new, dec_new).
;
; NOTES: Based loosely on the vector method described in the
;    2000-09-09 sci.math posting of James Van Buskirk:
;
;      http://groups.google.com/group/sci.math/msg/0c95aa791f44b3cd?hl=en&
;    
; MODIFICATION HISTORY:
;    
;    2007-01-17 (J.D. Smith): Written
;-
;    $Id$
;##############################################################################
;
; LICENSE
;
;  Copyright (C) 2007 J.D. Smith
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

function spherical_coord_crossp,a,b
  return,[a[1,*]*b[2,*]-b[1,*]*a[2,*], $
          a[2,*]*b[0,*]-b[2,*]*a[0,*], $
          a[0,*]*b[1,*]-b[0,*]*a[1,*]]
end

function spherical_coord_rotate_cartesian,ra,dec
  ;; Convert to cartesian coordinates
  cd=cos(dec)
  if n_elements(ra) gt 1 then $
     return, [transpose(cd*cos(ra)),transpose(cd*sin(ra)),transpose(sin(dec))]
  return,[cd*cos(ra),cd*sin(ra),sin(dec)]
end

pro spherical_coord_rotate,ra_ref,dec_ref,ra_new,dec_new,ra,dec
  RADEG=180.D/!DPI
  
  ;; Convert to cartesian coords
  v_new=spherical_coord_rotate_cartesian(ra_new/RADEG,dec_new/RADEG)
  v_ref=spherical_coord_rotate_cartesian(ra_ref/RADEG,dec_ref/RADEG)
  v=spherical_coord_rotate_cartesian(ra/RADEG,dec/RADEG)
  
  ;; Construct coordinate frame with x -> ref point & z -> rotation axis
  x=v_ref
  z=crossp(v_new,v_ref)         ; rotate about this axis
  z/=sqrt(total(z^2))                         ; normalize
  y=crossp(z,x)
  y/=sqrt(total(y^2))
  
  ;; Construct new rotated coordinate frame (x along new direction)
  x2=v_new
  y2=crossp(z,x2) ; z axis is the same in new frame
  y2/=sqrt(total(y2^2))
  
  ;; Project onto the initial frame, then re-express in the rotated one
  nra=n_elements(ra) 
  if nra eq 1 then begin 
     v=total(v*x,1)*x2 + total(v*y,1)*y2 + total(v*z,1)*z
     
     dec=asin(v[2])*RADEG
     ra=atan(v[1],v[0])*RADEG
     if ra lt 0.D then ra+=360.D
  endif else begin 
     targ=[3,nra]
     v=rebin(transpose(total(v*rebin(x,targ,/SAMPLE),1)),targ,/SAMPLE) * $
       rebin(x2,targ,/SAMPLE) + $
       rebin(transpose(total(v*rebin(y,targ,/SAMPLE),1)),targ,/SAMPLE) * $
       rebin(y2,targ,/SAMPLE) + $
       rebin(transpose(total(v*rebin(z,targ,/SAMPLE),1)),targ,/SAMPLE) * $
       rebin(z,targ,/SAMPLE)
     dec=asin(v[2,*])*RADEG
     ra=atan(v[1,*],v[0,*])*RADEG
     ra+= (ra lt 0.D) * 360.D
  endelse 
end
