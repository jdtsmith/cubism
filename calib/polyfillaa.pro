;+
; NAME:
;
;    POLYFILLAA
;
; DESCRIPTION:
;
;    Finds the fractional area of all pixels at least partially inside
;    a specified polygon.
;
; CATEGORY:
;
;    GRAPHICS, REGION OF INTEREST
;
; CALLING SEQUENCE:
;
;    inds=polyfillaa(x,y,sx,sy,[AREAS=])
;
; INPUT PARAMETERS:
;
;    x,y: The vectors containing the x and y subscripts of the
;       polygon.  May be in fractional units.
;
;    sx,sy: The size of the pixel grid on which the polygon is
;       superposed.  
;
; OUTPUT KEYWORD PARAMETERS:
;
;    AREAS: For each pixel index returned, the fractional area of that
;       pixel contained inside the polygon, between 0 and 1.
;
;    POLYGONS: A list of pointers to 2xn arrays containing the polygon
;       vertex information (as columns x,y).
;
; OUTPUTS:
;
;    inds: The indices of all pixels at least partially inside the
;       polygon.
;
; PROCEDURES:
;
;    polyclip
;
; EXAMPLE:
;
;    inds=polyfillaa([1.2,3,5.3,3.2],[1.3,6.4,4.3,2.2],10,10,AREAS=areas)
;
; MODIFICATION HISTORY:
;
;       2001-09-26 (J.D. Smith): Written.  Initial documentation.
;-
;   $Id$
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
;  Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;  Boston, MA 02111-1307, USA.
;
;##############################################################################

function polyfillaa, x,y,sx,sy, AREAS=areas, POLYGONS=polys
  ;; Clip to the nearest enclosing region
  left=floor(min(x,max=maxx))>0
  right=ceil(maxx)<(sx-1)
  bottom=floor(min(y,max=maxy))>0
  top=ceil(maxy)<(sy-1)
  plist=[transpose(x),transpose(y)] ;the vertex list
  for j=bottom,top do begin 
     for i=left,right do begin
        px=x & py=y
        polyclip,i,j,px,py
        if px[0] ne -1 then begin
           a=abs(total(px*shift(py,-1) - py*shift(px,-1))/2.)
           if n_elements(ret) eq 0 then begin 
              if arg_present(polys) then $
                 polys=[ptr_new([transpose(px),transpose(py)])]
              ret=[i+j*sx] 
              areas=[a]
           endif else begin
              if arg_present(polys) then $
                 polys=[polys,ptr_new([transpose(px),transpose(py)])]
              ret=[ret,i+j*sx]
              areas=[areas,a]
           endelse
        endif
     endfor
  endfor
  if n_elements(ret) eq 0 then return,-1 else return,ret
end
