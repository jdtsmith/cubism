;+
; NAME:  
;
;    IRS_APERTURE
;
; DESCRIPTION:
;    
;    Defines the IRS_APERTURE structure for specifying extraction apertures.
;    
; CATEGORY:
;
;    IRS IRS Spectral Reduction, Analysis and Processing.
;    Extraction apertures.
;    	
; CALLING SEQUENCE:
;
;    Not called directly, use:
;    
;      st={IRS_APERTURE}
;
;    and see class IRS_Calib
;
; NOTES:
;
;    The extraction apertures are defined for a single order.  There
;    are two types of aperture: box, and wavelength-scaled.  If scale
;    is non-zero, it takes precedence.
;
;    WAVELENGTH SCALED
;
;    Wavelength scaled apertures are specified with a normalized
;    coordinate for the aperture center, a fiducial reference
;    wavelength, and a normalized width at the fiducial wavelength.
;    The aperture will be scaled to this pixel size at this wavlength,
;    with the size scaling linearly with wavelength for all other
;    aperture positions.
;    
;    BOX SCALED
;
;    Box scaled apertures consist of two pairs of normalized (on
;    [0.0,1.0]) floating point numbers: low, and high.  Each pair
;    specifies the starting (top, or shorter wavelength) aperture, and
;    the ending (bottom, or longer wavelength) aperture in terms of
;    fractional slit lengths measured from the "low" side of the slit.
;    This means a value of ".2" corresponds to 20% of the slit length
;    from the leftmost edge, and traces any line tilt.
;
;    A single, slightly tilted slit image demonstrating the normalized
;    coordinates, with spatial direction nearly left-right, and
;    cross-dispersion (spectral) direction nearly up-down is shown:
;
;
;      0.0                                               1.0
;       |=================================================|
;
;       +--------
;       |        \----------------
;       |                         \---------------
;       |                                         \-------+
;       |                                                 |
;       |                                                 |
;       +--------                                         |
;                \----------------                        |
;                                 \---------------        |
;                                                 \-------+
;
;       <------ Low -------------------------  High ------>
;
;    The aperture is linearly interpolated between the two endpoints
;    across the full length of the order, in the cross dispersion
;    direction.  E.g. if "Low" were [.2,.3], then the leftmost border
;    of the aperture would span linearly from .2 at the long
;    wavelength end of the order, to .3 at the short wavelength end.
;    The "low" and "high" apertures should not cross.
;
; MODIFICATION HISTORY:
;
;    2004-03-07 (J.D. Smith): Added wavelength-scaled apertures
;    2002-08-27 (J.D. Smith): Initial migration from SMART codebase.
;    2001-12-13 (J.D. Smith): Written
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2001,2002,2004 J.D. Smith
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
;  Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;  Boston, MA 02111-1307, USA.
;
;##############################################################################

pro IRS_APERTURE__define
  st={IRS_APERTURE,$
      Low:  [0.0,0.0], $        ; The "low in the slit" (left) aperture pair
      High: [0.0,0.0], $        ; & "high in the slit" (right). Normalized
      Wavscl: 0b, $             ; Whether we are wavelength scaling   
      Scale:[0.0,0.0,0.0]}      ; width/wavelength/cenpos for wav-scaling

end
