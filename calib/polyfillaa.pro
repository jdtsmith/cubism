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
; INPUTS:
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
; OUTPUTS:
;
;    inds: The indices of all pixels at least partially inside the
;       polygon.
;
; PROCEDURE:
;
;    polyclip
;
; EXAMPLE:
;
;    inds=polyfillaa([1.2,3,5.3,3.2],[1.3,6.4,4.3,2.2],10,10,AREAS=areas)
;
; MODIFICATION HISTORY:
;
;       Wed Sep 26 12:27:55 2001, JD Smith <jdsmith@astro.cornell.edu>
;
;		Initial documentation.
;-
 
function polyfillaa, x,y,sx,sy, AREAS=areas
  ;; Clip to the nearest enclosing region
  left=floor(min(x,max=maxx))>0
  right=ceil(maxx)<(sx-1)
  bottom=floor(min(y,max=maxy))>0
  top=ceil(maxy)<(sy-1)
  bb=[left,bottom,right,top]
  plist=[transpose(x),transpose(y)] ;the vertex list
  for j=bottom,top do begin 
     for i=left,right do begin
        pc=polyclip(i,j,x,y,BOUNDING_BOX=bb)
        if pc[0] ne -1 then begin
           px=reform(pc[0,*]) & py=reform(pc[1,*])
           a=abs(total(px*shift(py,-1) - py*shift(px,-1))/2.)
           if n_elements(ret) eq 0 then begin 
              ret=[i+j*sx] 
              areas=[a]
           endif else begin
              ret=[ret,i+j*sx]
              areas=[areas,a]
           endelse
        endif
     endfor
  endfor
  if n_elements(ret) eq 0 then return,-1 else return,ret
end
