;+
; NAME:  
;
;    irs_fov
;
;
; DESCRIPTION:
;    
;    Maps SSC/BCD Header FOVID and FOVNAME, and optionally returns
;    module, order, and position for IRS keywords (except Peakup).
;    
; CATEGORY:
;
;    IRS Spectrograph: Field of View Apertures
;    	
; CALLING SEQUENCE:
;
;    (fovid|fovname)=irs_fov((fovname|fovid),[MODULE=,ORDER=,POSITION=])
;    
; INPUT PARAMETERS:
;
;    fovname|fovid: The string name or integer id of the IRS fov
;       aperture.
;			
; OUTPUT KEYWORD PARAMETERS:
;
;    MODULE: The string module name this fov corresponds to.
;
;    ORDER: The integer order number which was targetted, or 0 if the
;       module center was targetted.
;
;    POSITION: The integer position which was targetted within the
;       order (if relevant).  0 means the center position (unless
;       ORDER is 0 in which case the *module* center was targetted).
;			
; INPUT KEYWORD PARAMETERS:
;
;    LOOKUP_MODULE: Lookup the module,order and position, returning
;       the fovid (default), or fovname if /RETURN_NAME is set.
;
;    RETURN_NAME: Return the fovname.  Usually, the return type is
;       defined by the type of the input.
;
; OUTPUTS:
;
;    fovid|fovname: The integer id or string name of the IRS fov
;       aperture.
;
; NOTES:
;  
;    
;
; EXAMPLE:
;
;    fovid=irs_fov('IRS_Short-Lo_1st_Order_1st_Position')
;
; MODIFICATION HISTORY:
;    
;    2002-09-30 (J.D. Smith): Written.
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2002,2003 J.D. Smith
;
;  This file is part of CUBISM.
;
;  CUBISM is free software; you can redistribute it and/or modify it
;  under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2, or (at your option)
;  any later version.
;  
;  CUBISM is distributed in the hope that it will be useful, but
;  WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;  General Public License for more details.
;  
;  You should have received a copy of the GNU General Public License
;  along with CUBISM; see the file COPYING.  If not, write to the Free
;  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;  02111-1307, USA.
;
;##############################################################################

function irs_fov, fov, SHORT_NAME=sn,MODULE=md, ORDER=ord, POSITION=pos, $
                  LOOKUP_MODULE=lm, RETURN_NAME=nm
  
  void={IRS_FOV,ID:0,NAME:'',SHORT_NAME:'',MODULE:'',ORDER:0,POSITION:0}
  f=[{IRS_FOV,26,'IRS_Short-Lo_1st_Order_1st_Position',   'SL1_a',  'SL',1,1},$
     {IRS_FOV,27,'IRS_Short-Lo_1st_Order_2nd_Position',   'SL1_b',  'SL',1,2},$
     {IRS_FOV,28,'IRS_Short-Lo_1st_Order_Center_Position','SL1_cen','SL',1,0},$
     {IRS_FOV,29,'IRS_Short-Lo_Module_Center',            'SL_cen', 'SL',0,0},$
     {IRS_FOV,32,'IRS_Short-Lo_2nd_Order_1st_Position',   'SL2_a',  'SL',2,1},$
     {IRS_FOV,33,'IRS_Short-Lo_2nd_Order_2nd_Position',   'SL2_b',  'SL',2,2},$
     {IRS_FOV,34,'IRS_Short-Lo_2nd_Order_Center_Position','SL2_cen','SL',2,0},$
     {IRS_FOV,38,'IRS_Long-Lo_1st_Order_1st_Position',    'LL1_a',  'LL',1,1},$
     {IRS_FOV,39,'IRS_Long-Lo_1st_Order_2nd_Position',    'LL1_b',  'LL',1,2},$
     {IRS_FOV,40,'IRS_Long-Lo_1st_Order_Center_Position', 'LL1_cen','LL',1,0},$
     {IRS_FOV,41,'IRS_Long-Lo_Module_Center',             'LL_cen', 'LL',0,0},$
     {IRS_FOV,44,'IRS_Long-Lo_2nd_Order_1st_Position',    'LL2_a',  'LL',2,1},$
     {IRS_FOV,45,'IRS_Long-Lo_2nd_Order_2nd_Position',    'LL2_b',  'LL',2,2},$
     {IRS_FOV,46,'IRS_Long-Lo_2nd_Order_Center_Position', 'LL2_cen','LL',2,0},$
     {IRS_FOV,50,'IRS_Short-Hi_1st_Position',             'SH_a',   'SH',0,1},$
     {IRS_FOV,51,'IRS_Short-Hi_2nd_Position',             'SH_b',   'SH',0,2},$
     {IRS_FOV,52,'IRS_Short-Hi_Center_Position',          'SH_cen', 'SH',0,0},$
     {IRS_FOV,56,'IRS_Long-Hi_1st_Position',              'LH_a',   'LH',0,1},$
     {IRS_FOV,57,'IRS_Long-Hi_2nd_Position',              'LH_b',   'LH',0,2},$
     {IRS_FOV,58,'IRS_Long-Hi_Center_Position',           'LH_cen', 'LH',0,0}]
  
  if keyword_set(lm) then begin 
     if n_elements(md) eq 0 OR n_elements(ord) eq 0 OR $
        n_elements(pos) eq 0 then $
        message,'Must specify Module,Order and Position'
     
     if ord eq 3 and strmid(strupcase(md),1) eq 'L' then ord=2
     wh=where(f.module eq strupcase(md) AND $
              (f.order eq long(ord) OR $
               (strmid(f.module,1) eq 'H' AND f.order eq 0)) AND $
              f.position eq long(pos),cnt)
     if cnt eq 0 then return,-1
     return,keyword_set(nm)? $
            (keyword_set(sn)?f[wh[0]].SHORT_NAME:f[wh[0]].NAME): $
            f[wh[0]].ID
  endif 
  
  if size(fov,/type) eq 7 AND keyword_set(nm) eq 0 then begin
     wh=where(strupcase(f.NAME) eq strupcase(fov),cnt)
     if cnt eq 0 then return,-1
     ret=f[wh[0]].ID
  endif else begin 
     wh=where(f.ID eq long(fov),cnt)
     if cnt eq 0 then return,-1
     if keyword_set(sn) then ret=f[wh[0]].SHORT_NAME else ret=f[wh[0]].NAME
  endelse 
  
  if arg_present(md)  then md= f[wh[0]].MODULE
  if arg_present(ord) then ord=f[wh[0]].ORDER
  if arg_present(pos) then pos=f[wh[0]].POSITION
  return,ret
end
