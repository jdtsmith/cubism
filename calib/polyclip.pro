;+
; NAME:
;
;    POLYCLIP
;
; DESCRIPTION:
;
;    Clips a polygon to a square unit pixel, using the
;    Sutherland-Hodgman polygon clipping algorithm.
;
; CATEGORY:
;
;    GRAPHICS, REGION OF INTEREST
;
; CALLING SEQUENCE:
;
;    plist=polyclip(i,j,x,y,[AREAS=])
;
; INPUTS:
;
;    i,j: The pixel coordinates to which to clip.
;
;    x,y: The vectors containing the x and y subscripts of the
;       polygon.  May be in fractional units; pixel centers for pixel
;       (i,j) is at (x,y)=(i+.5,j+.5).
;
; OUTPUTS:
;
;    plist: A new 2xn polygon list, with adjacent columns giving the X
;       and Y coordinates of the clipped polygon vertices.  If a pixel
;       is fully outside the specified polygon, -1 is returned.
;
; EXAMPLE:
;
;    p=polyclip(5,4,[1.2,3,5.3,3.2],[1.3,6.4,4.3,2.2])
;
; MODIFICATION HISTORY:
;
;       2002-08-27 (J.D. Smith): Migrated from SMART codebase
;       2001-09-26 (J.D. Smith): Written and documented
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

;; Compute whether a given point is "inside" the specified edge.
function clip_inside,i,j,p,t
  case t of
     0b: return, p[0] gt i    ;left
     1b: return, p[0] lt i+1  ;right
     2b: return, p[1] lt j+1  ;top
     3b: return, p[1] gt j    ;bottom
  endcase 
end

;; Compute the intersection of a given polygon side and the specified
;; unit pixel edge
function clip_intersection,i,j,s,p,t
  case t of
     0b: return,[i,s[1]+(p[1]-s[1])/(p[0]-s[0])*(i-s[0])] ;left
     1b: return,[i+1,s[1]+(p[1]-s[1])/(p[0]-s[0])*(i+1-s[0])] ; right
     2b: return,[s[0]+(p[0]-s[0])/(p[1]-s[1])*(j+1-s[1]),j+1] ; top
     3b: return,[s[0]+(p[0]-s[0])/(p[1]-s[1])*(j-s[1]),j] ; bottom
  endcase
end

function polyclip,i,j,px,py
  ;; Sutherland-Hodgman's polygon-clipping algorithm for a square unit pixel
  ;;       A polygon side looks like:
  ;;                 s-------->p
  plist=[transpose(px),transpose(py)] ;the vertex list
  for ctype=0b,3b do begin      ;clip left, right, top, bottom
     np=(size(plist,/DIMENSIONS))[1]
     ;; start with the final->first vertex segment
     s=plist[*,np-1] & in_s=clip_inside(i,j,s,ctype) 
     for k=0,np-1 do begin
        p=plist[*,k] & in_p=clip_inside(i,j,p,ctype)
        if in_s XOR in_p then begin ; in->out or out->in, add intersection
           ci=clip_intersection(i,j,s,p,ctype)
           if size(pnew,/N_DIMENSIONS) eq 0 then $
              pnew=[ci] else pnew=[[pnew],[ci]]
        endif
        if in_p then $          ; out->in or in->in, add 2nd point
           if size(pnew,/N_DIMENSIONS) eq 0 then $
           pnew=[p] else pnew=[[pnew],[p]]
        s=p & in_s=in_p         ; rotate
     endfor
     if size(pnew,/N_DIMENSIONS) eq 0 then return,-1 ; entirely outside
     plist=pnew & pnew=0
  endfor
  return,plist
end
