;+
; NAME:  
;
;    PLOT_WAVSAMP
;
; DESCRIPTION:
;    
;    Over-plots WAVSAMP pseudo-rectangles.
;    
; CATEGORY:
;
;    IRS Spectral Reduction, Analysis and Processing.
;    	
; CALLING SEQUENCE:
;
;    plot_wavsamp,ws_list,[_EXTRA=]
;
; INPUT PARAMETERS:
;
;    ws_list: A list of WAVSAMP psuedo-rectangles, as returned by
;      IRS_Calib::GetWAVSAMP.
;			
; INPUT KEYWORD PARAMETERS:
;
;    _EXTRA: Additional plot keywords to use.
;
;    BOOLEAN_KEYWORD: Aside from the leading `/' in the calling
;       sequence, boolean keywords are described in the same way.
;			
; EXAMPLE:
;
;    plot_wavsamp, $
;       cal->GetWAVSAMP('LL',1,{IRS_APERTURE,[.2,.5],[.7,.4]}), $
;       COLOR=!D.TABLE_SIZE-4,/NOERASE
;
; MODIFICATION HISTORY:
;    
;    2002-08-27 (J.D. Smith): Migrated from SMART codebase.
;    2001-12-14 (J.D. Smith): Written
;    
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2001 J.D. Smith
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

pro plot_wavsamp, ws_list, _EXTRA=e
  plot,[0],[0],XRANGE=[0,128],YRANGE=[0,128],XSTYLE=5,YSTYLE=5,/NODATA, $
       POSITION=[0,0,1,1],_EXTRA=e
  for i=0,n_elements(ws_list)-1 do begin 
     x=ws_list[i].X
     y=ws_list[i].Y
     oplot,[x,x[0]],[y,y[0]],_EXTRA=e
  endfor 
end
