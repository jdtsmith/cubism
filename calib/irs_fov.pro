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
;    (fovid|fovid)=irs_fov((fovname|fovid),[MODULE=,ORDER=,POSITION=])
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
; OUTPUTS:
;
;    fovid|fovname: The integer id or string name of the IRS fov
;       aperture.
;
; NOTES:
;  
;    Additional description and other information which doesn't fit elsewhere.
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
;  Copyright (C) 2002 J.D. Smith
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

function irs_fov, fov, MODULE=md, ORDER=ord, POSITION=pos
  
  void={IRS_FOV,ID:0,NAME:'',MODULE:'',ORDER:0,POSITION:0}
  fvs=[{IRS_FOV, 26, 'IRS_Short-Lo_1st_Order_1st_Position','SL',1,1}, $
       {IRS_FOV, 27, 'IRS_Short-Lo_1st_Order_2nd_Position','SL',1,2}, $
       {IRS_FOV, 28, 'IRS_Short-Lo_1st_Order_Center_Position','SL',1,0}, $
       {IRS_FOV, 29, 'IRS_Short-Lo_Module_Center','SL',0,0}, $
       {IRS_FOV, 32, 'IRS_Short-Lo_2nd_Order_1st_Position','SL',2,1}, $
       {IRS_FOV, 33, 'IRS_Short-Lo_2nd_Order_2nd_Position','SL',2,2}, $
       {IRS_FOV, 34, 'IRS_Short-Lo_2nd_Order_Center_Position','SL',2,0}, $
       {IRS_FOV, 38, 'IRS_Long-Lo_1st_Order_1st_Position','LL',1,1}, $
       {IRS_FOV, 39, 'IRS_Long-Lo_1st_Order_2nd_Position','LL',1,2}, $
       {IRS_FOV, 40, 'IRS_Long-Lo_1st_Order_Center_Position','LL',1,0}, $
       {IRS_FOV, 41, 'IRS_Long-Lo_Module_Center','LL',0,0}, $
       {IRS_FOV, 44, 'IRS_Long-Lo_2nd_Order_1st_Position','LL',2,1}, $
       {IRS_FOV, 45, 'IRS_Long-Lo_2nd_Order_2nd_Position','LL',2,2}, $
       {IRS_FOV, 46, 'IRS_Long-Lo_2nd_Order_Center_Position','LL',2,0}, $
       {IRS_FOV, 50, 'IRS_Short-Hi_1st_Position','SH',0,1}, $
       {IRS_FOV, 51, 'IRS_Short-Hi_2nd_Position','SH',0,2}, $
       {IRS_FOV, 52, 'IRS_Short-Hi_Center_Position','SH',0,0}, $
       {IRS_FOV, 56, 'IRS_Long-Hi_1st_Position','LH',0,1}, $
       {IRS_FOV, 57, 'IRS_Long-Hi_2nd_Position','LH',0,2}, $
       {IRS_FOV, 58, 'IRS_Long-Hi_Center_Position','LH',0,0} ]
  
  if size(fov,/type) eq 7 then begin 
     wh=where(strupcase(fvs.name) eq strupcase(fov),cnt)
     if cnt eq 0 then return,-1
     ret=fvs[wh[0]].id
  endif else begin 
     wh=where(fvs.id eq fov,cnt)
     if cnt eq 0 then return,-1
     ret=fvs[wh[0]].name
  endelse 
  
  if arg_present(md)  then md= fvs[wh[0]].MODULE
  if arg_present(ord) then ord=fvs[wh[0]].ORDER
  if arg_present(pos) then pos=fvs[wh[0]].POSITION
  return,ret
end
