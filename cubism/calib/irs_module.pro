;+
; NAME:  
;
;    IRS_MODULE
;
; DESCRIPTION:
;
;    Regularize the module, mapping names to integers and visa versa:
;
;      0: SL
;      1: SH
;      2: LL
;      3: LH
;    
; CATEGORY:
;
;    IRS Spectral Reduction, Analysis and Processing.
;    	
; CALLING SEQUENCE:
;
;    mod_num_or_name=irs_module(module_name_or_number, [/TO_NAME])
;
; INPUT PARAMETERS:
;
;    module_name_or_number: A string such as "LL" or "long-lo",
;       etc. to be converted, or an integer to clip to the range
;       [0,3].  String or integer vectors are also accepted.  Or, If
;       TO_NAME is set, a module number to be converted to a string
;       name.
;			
; INPUT KEYWORD PARAMETERS:
;
;    TO_NAME: Instead of returning a module number, return a string
;       name of form LL,SL,LH,SH, from the given integer module
;       number.
;
; OUTPUTS:
;
;    mod_num_or_name: The module number, an integer from 0-5, or an
;       array of module number of the same dimensions as the module
;       argument, OR, if TO_NAME is set, the module name.
;
; EXAMPLE:
;
;    mod=irs_module('Long Low')
;    mod_name=irs_module(3,/TO_NAME)
;    
; MODIFICATION HISTORY:
;
;    2002-08-27 (J.D. Smith) Updated integer<->module mapping to track SSC
;    2002-05-22 SJUH need additional modules as SL and LL have two apertures
;    2001-12-08 (J.D. Smith): Written
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2001,2002 J.D. Smith
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
;; Boston, MA 02110-1301, USA.
;
;##############################################################################
function irs_module,module, TO_NAME=tn
  if size(module[0],/TYPE) eq 7 then begin 
     for i=0,n_elements(module)-1 do begin 
        match=0
        long='^L(o|on|ong)?'
        short='^S(h|ho|hor|hort)?'
        high='H(i|ig|igh)?$'
        low='L(o|ow)?$'
        sep='[ -/=+~]*'
        mod_match=[short+sep+low,short+sep+high,long+sep+low,long+sep+high]
        for j=0,3 do begin 
           if stregex(module[i],mod_match[j],/FOLD_CASE,/BOOLEAN) then begin 
              match=1
              if n_elements(ret) eq 0 then ret=j else ret=[ret,j]
              break
           endif
        endfor
        if match eq 0 then $
           message,'Module name not recognized: '+strtrim(module[i],2)
     endfor  
  endif else ret=0>fix(module)<3

  if keyword_set(tn) then return,(['SL','SH','LL','LH'])[ret]
  return,ret
end
