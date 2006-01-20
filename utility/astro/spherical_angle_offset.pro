;+
; NAME:  
;
;    SPHERICAL_ANGLE_OFFSET
;
; DESCRIPTION:
;    
;    Calculate angle on sphere between two coordinates.
;    
; CATEGORY:
;
;    Astro Utility
;
; CALLING SEQUENCE:
;
;    off=spherical_angle_offset(ra1,dec1,ra2,dec2)
;
; INPUT PARAMETERS:
;
;    (ra,dec)(1,2): The RA/DEC for positions 1 and 2, in decimal
;       degrees.
;
; OUTPUT:
;
;    off: The spherical offset angle between the two positions, in
;       decimal degrees.
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

function spherical_angle_offset,ra1,dec1,ra2,dec2
  RADEG=180.D/!DPI
  d1=dec1/RADEG & d2=dec2/RADEG & r1=ra1/RADEG & r2=ra2/RADEG
  return,acos(cos(d1)*cos(d2)*cos(r1-r2) + sin(d1)*sin(d2))*RADEG
end
