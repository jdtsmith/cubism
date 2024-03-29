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
;       aperture.  Can be vectors, in which case the return has the
;       same dimensions.
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
;    SHORT_NAME: Return or query the abbreviated name.
;
;    SLIT_NAME: Return the abbreviated slit name, without the position
;       information.
;  
;    ON_ARRAY: If set, SLIT_NAME refers to the order *on the array*,
;       e.g. SL3 is valid as an array, though it is not a valid
;       aperture for pointing.
;
; OUTPUTS:
;
;    fovid|fovname: The integer id (default) or string name (long,
;       short or slit-only) of the IRS fov aperture.
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
;    2004-09-19 (J.D. Smith): Restructure, and accomodate slit only names.
;    2002-09-30 (J.D. Smith): Written.
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2002,2003,2004, 2009 J.D. Smith
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
;  Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;
;##############################################################################

function irs_fov, fov, SHORT_NAME=sn,MODULE=md_in, ORDER=ord, POSITION=pos, $
                  LOOKUP_MODULE=lm, RETURN_NAME=nm, SLIT_NAME=sno,ON_ARRAY=array
  
  void={IRS_FOV,ID:0,NAME:'',SHORT_NAME:'',MODULE:'',ORDER:0,POSITION:0}
  f=[{IRS_FOV,18,'IRS_Red_Peak-Up_FOV_Center',          'PU_R_cen','PUR',0,0},$
     {IRS_FOV,19,'IRS_Red_Peak-Up_FOV_Sweet_Spot',    'PU_R_sweet','PUR',0,0},$
     {IRS_FOV,22,'IRS_Blue_Peak-Up_FOV_Center',         'PU_B_cen','PUB',0,0},$
     {IRS_FOV,23,'IRS_Blue_Peak-Up_FOV_Sweet_Spot',   'PU_B_sweet','PUB',0,0},$
     {IRS_FOV,26,'IRS_Short-Lo_1st_Order_1st_Position',   'SL1_a',  'SL',1,1},$
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
  
  bonus=0b
  if keyword_set(lm) then begin 
     if n_elements(md_in) ne 0 then md=irs_module(md_in,/TO_NAME)
     if n_elements(md) eq 0 OR n_elements(ord) eq 0 OR $
        n_elements(pos) eq 0 then $
        message,'Must specify Module,Order and Position'
     
     if ord eq 3 && (md eq 'SL' || md eq 'LL') then begin 
        ord=2
        bonus=1b
     endif 
     wh=where(f.module eq md AND $
              (f.order eq long(ord) OR $
               (strmid(f.module,1) eq 'H' AND f.order eq 0)) AND $
              f.position eq long(pos),cnt)
  endif else begin 
     if size(fov,/type) eq 7 then begin 
        if keyword_set(sn) then $
           wh=where_array(strupcase([fov]),strupcase(f.SHORT_NAME),cnt, $
                         /PRESERVE_ORDER) $
        else wh=where_array(strupcase([fov]),strupcase(f.NAME),cnt, $
                            /PRESERVE_ORDER)
     endif else wh=where_array(long([fov]),f.ID,cnt,/PRESERVE_ORDER)
  endelse 
  if cnt eq 0 then return,-1
  
  ;; Return 
  if ~keyword_set(lm) then begin 
     if arg_present(md_in) then md_in= f[wh].MODULE
     if arg_present(ord)   then ord=f[wh].ORDER
     if arg_present(pos)   then pos=f[wh].POSITION
  endif 
  
  case 1 of
     keyword_set(sno): begin    
        short=f[wh].SHORT_NAME
        pos=strpos(short,'_',/REVERSE_SEARCH)
        if pos ne -1 then short=strmid(short,0,pos)
        if bonus && keyword_set(array) then begin     
           pos=strpos(short,strtrim(f[wh].ORDER,2))
           if pos ne -1 then short=strmid(short,0,pos)+'3'
        endif 
        return,short
     end 
     keyword_set(sn): return,f[wh].SHORT_NAME
     keyword_set(nm): return,f[wh].NAME
     else: return,f[wh].ID
  endcase
end
