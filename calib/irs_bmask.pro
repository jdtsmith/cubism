;+
; NAME:
;
;    IRS_BMASK
;
; DESCRIPTION:
;
;    Translate BMASK bit values into status string codes, and
;    optionally return bit set values.
;
; CATEGORY:
;
;    IRS Spectral Reduction, Analysis and Processing.
;    Calibration
;    
; CALLING SEQUENCE:
;
;    irs_bmask,mask_val,[PMASK=,SATURATED=,NO_FLAT=,NO_PLANES=
;              ONE_PLANE=, CODE_STRING=]
;
; INPUT PARAMETERS:
;
;    mask_val: A BMASK 16-bit value
;
; KEYWORD PARAMETERS:
;
;    PMASK: If set, return in the passed variable the boolean value of
;       pmask flagged.
;
;    SATURATED: If set, return in the passed variable the boolean value of
;       saturated.
;       
;    NO_FLAT: If set, return in the passed variable the boolean value of
;       no flat field.
;       
;    NO_PLANES: If set, return in the passed variable the boolean value of
;       no usable ramp planes.
;
;    ONE_PLANE: If set, return in the passed variable the boolean value of
;       only one usable ramp plane
;
;    PMASK: If set, return in the passed variable the boolean value of
;       pmask flagged.
;
;    PMASK: If set, return in the passed variable the boolean value of
;       pmask flagged.
;
;    CODE_STRING: If set, return in the passed variable the string
;       code corresponding to the mask value passed.
;       
; MODIFICATION HISTORY:
;
;    2004-01-05 (J.D. Smith): Written
;-
;    $Id$
;##############################################################################
;
; LICENSE
;
;  Copyright (C) 2004, 2005 J.D. Smith
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

pro irs_bmask,mask,PMASK=pm,SATURATED=sat,NO_FLAT=nf,NO_PLANES=np, $
              ONE_PLANE=op,CODE_STRING=cs
  flags=bytarr(15)
  for i=0,14 do flags[i]=(mask AND 2U^i) ne 0
  pm =flags[14] ne 0b           ;pmask flagged 
  sat=flags[10] ne 0b           ;saturated
  digsat=flags[2] ne 0b         ;correctable saturation
  nf=flags[8] ne 0b             ;No flat
  op=flags[12] ne 0b            ;One plane
  np=flags[13] ne 0b            ;No planes
  rh=flags[3] ne 0b             ;Radiation Hit
  
  
  if arg_present(cs) then begin 
     cs=(pm?'*':'')+(sat?'S':'')+(digsat?'s':'')+(nf?'F':'')+(rh?'R':'')+ $
        (op?'1':(np?'0':''))
     if (mask AND 35571U) gt 0 then begin 
        flags[[14,10,8,13,12,3,2]]=0b
        cs+="["+strjoin(strtrim(where(flags),2),",")+"]"
     endif 
  endif
     
end
