;+
; NAME:  
;
;    IRS_APERTURE
;
; DESCRIPTION:
;    
;    Return an IRS_APERTURE structure.
;    
; CATEGORY:
;
;    IRS IRS Spectral Reduction, Analysis and Processing.
;    Extraction apertures.
;    	
; CALLING SEQUENCE:
;
;      st=irs_aperture(low,high,pix_sz)
;
; INPUT PARAMETERS:
;
;    low, high: Either the low and high normalized coordinates to
;       scale to (one or two values each), or, if width is passed, the
;       wavelength fiducial and normalized position.
;
; OPTIONAL INPUT PARAMETERS:
;
;    width: The normalized width of the aperture at the wavelength
;       fiducial.  If this argument is passed, a scaled wavelength
;       aperture is returned, otherwise a normal aperture is returned.
;
; OUTPUTS:
;
;    st: The IRS_APERTURE structure with the requested characteristics.
;
; NOTES:
;  
;    See irs_aperture__define.pro for more on the aperture structure.
;
; EXAMPLE:
;
;    st=irs_aperture(.1,[.8,.7])  ; normal ap: left edge@.1, right .8->.7
;    st2=irs_aperture(18.,.3,.7) ; wave-scaled ap: 70% @ 18um, centered @0.3
;
; MODIFICATION HISTORY:
;
;    2004-03-07 (J.D. Smith): Written
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2004 J.D. Smith
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

function irs_aperture, low, high, width
  ap={IRS_APERTURE}
  if keyword_set(width) then begin 
     ap.wavscl=1b
     ap.scale=[width,low,high]
  endif else begin 
     if n_elements(low) eq 1 then low=[low,low]
     if n_elements(high) eq 1 then high=[high,high]
     ap.low=low & ap.high=high
  endelse 
  return,ap
end 

