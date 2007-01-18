;+
; NAME:  
;
;    IRS_UPDATE_MOVING_TARGET_POSITION
;
; DESCRIPTION:
;    
;    Given a directory, scan for moving target AOR BCD's beneath it,
;      and update the (RA,DEC)_(SLT,RQST) header keywords by fixing a
;      reference position on the first BCD in a given directory.  A
;      separate position is utilized for each directory, corresponding
;      to the reference for the first step of the map sequence.  The
;      resulting coordinate system is not a true celestial system, but
;      enables CUBISM to build cubes as if it were.
;    
; CATEGORY:
;
;    Astro Utility
;
; CALLING SEQUENCE:
;
;    irs_update_moving_target_position,directory
;
; INPUT PARAMETERS:
;
;    directory: The directory beneath which to scan for and correct
;      moving target position information.
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

pro irs_update_moving_target_position, dir
  f=file_search(dir,'*bcd{,_fp}.fits')
  d=file_dirname(f)
  dirs=d[uniq(d,sort(d))]
  ;; Process by directory
  for i=0,n_elements(dirs)-1 do begin 
     w=where(d eq dirs[i],cnt)
     ref_set=0
     for j=0,cnt-1 do begin 
        h=headfits(f[w[j]])
        if strmid(sxpar(h,'OBJTYPE'),0,12) ne 'TargetMoving' then continue
        if ~stregex(sxpar(h,'FOVNAME'),'IRS_(Short|Long)',/BOOLEAN) then $
           continue
        ra_ref=sxpar(h,'RA_REF')
        dec_ref=sxpar(h,'DEC_REF')
        
        if ~ref_set then begin  ;Setup the original reference position
           ra_ref0=ra_ref
           dec_ref0=dec_ref
           ref_set=1
           continue
        endif 
        
        ra_slt=sxpar(h,'RA_SLT') & dec_slt=sxpar(h,'DEC_SLT')
        spherical_coord_rotate,ra_ref,dec_ref,ra_ref0,dec_ref0, $
                               ra_slt,dec_slt
        
        sxaddpar,h,'RA_SLT',ra_slt,/SAVECOMMENT,FORMAT='(G22.15)'
        sxaddpar,h,'DEC_SLT',dec_slt,/SAVECOMMENT,FORMAT='(G22.15)'
        
        ra_rqst=sxpar(h,'RA_RQST') & dec_rqst=sxpar(h,'DEC_RQST')
        spherical_coord_rotate,ra_ref,dec_ref,ra_ref0,dec_ref0, $
                               ra_rqst,dec_rqst
        
        sxaddpar,h,'RA_RQST',ra_rqst,/SAVECOMMENT,FORMAT='(G22.15)'
        sxaddpar,h,'DEC_RQST',dec_rqst,/SAVECOMMENT,FORMAT='(G22.15)'
        
        sxaddhist, $
           ['irs_update_moving_target_position: modified coordinates ', $
            '   RA_SLT, DEC_SLT, RA_RQST, DEC_RQST', $
            string(FORMAT='("   ",A,G0.9,", ",G0.9)', $
                   'co-moving frame reference position: ', $
                   ra_ref0,dec_ref0)],h
        file_copy,f[w[j]],f[w[j]]+'.bak',/OVERWRITE
        modfits,f[w[j]], 0, h
     endfor 
  endfor 
end
