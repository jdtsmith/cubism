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
;    inds=polyfillaa(px,py,sx,sy,[AREAS=,POLYGONS=,/NO_COMPILED,/RECOMPILE])
;
; INPUT PARAMETERS:
;
;    px,py: The vectors containing the x and y subscripts of the
;       polygon.  May be in fractional units.
;
;    sx,sy: The size of the pixel grid on which the polygon is
;       superposed.  
;
; INPUT KEYWORD PARAMETERS:
;
;    NO_COMPILED: If set, the IDL-native method will be used to
;       compute the clipped polygons, regardless of whether
;       compilation of the external C version succeeded.  Otherwise,
;       POLYFILLAA will attempt to compile a C version of the
;       Sutherland Hodgemand algorithm found in the file "polyclip.c".
;
;    RECOMPILE: If set, recompile the C version, even if it has
;      already been compiled.  Note: if compilation has already
;      succeeded once, to actually link to the recompiled version you
;      must unload the old version.
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
; NOTES:
;
;    POLYFILLAA attempts to auto-compile a C-language version of the
;    clipping algorithm, found in polyclip.c.  In order for this
;    compilation to succeed, a compiler which IDL recognizes must be
;    installed.  See MAKE_DLL and the !MAKE_DLL system variable for
;    more information.
;
; EXAMPLE:
;
;    inds=polyfillaa([1.2,3,5.3,3.2],[1.3,6.4,4.3,2.2],10,10,AREAS=areas)
;
; MODIFICATION HISTORY:
;
;       2003-01-03 (J.D. Smith): Substantial rewrite to use external C
;          code (if possible) for a significant performance
;          improvement.
;
;       2001-09-26 (J.D. Smith): Written.  Initial documentation.
;-
;   $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2001,2002,2003 J.D. Smith
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

function polyfillaa, px,py,sx,sy, AREAS=areas, POLYGONS=polys,NO_COMPILED=nc, $
                     RECOMPILE=rc
  common polyfillaa_external,polyclip_compiled,polyclip_path
  if n_elements(polyclip_compiled) eq 0 OR keyword_set(rc) then begin 
     catch, err
     if err ne 0 then begin   ; any failure in compiling, just use the IDL vers
        message, /CONTINUE, $
           'Failed compiling DLM clipper -- reverting to internal version.'
        polyclip_compiled=0
     endif else begin 
        resolve_routine,'polyclip'
        path=(routine_info('polyclip',/SOURCE)).PATH
        path=strmid(path,0,strpos(path,path_sep(),/REVERSE_SEARCH))
        make_dll,'polyclip',['polyclip_test','polyclip'],INPUT_DIRECTORY=path,$
                 DLL_PATH=polyclip_path,/REUSE_EXISTING
        ;; Test for a correctly compiled library
        tmp=call_external(polyclip_path,'polyclip_test',/B_VALUE)
        if tmp[0] ne 42b then $
           message,'Testing clipper DLM: Incorrect value returned.'
        polyclip_compiled=1
     endelse 
     catch,/cancel
  endif
  
  ;; Clip grid to the nearest enclosing region
  left=floor(min(px,max=maxx))>0
  right=floor(maxx)<(sx-1)
  bottom=floor(min(py,max=maxy))>0
  top=floor(maxy)<(sy-1)
  nx=right-left+1 & ny=top-bottom+1
  npol=long(n_elements(px)) & npix=long(nx*ny)
  if npix eq 0L then return,-1
  ret=lonarr(npix,/NOZERO)
  apa=arg_present(areas)
  areas=fltarr(npix,/NOZERO)
  app=arg_present(polys)
  if app then polys=ptrarr(npix,/NOZERO)
  ind=0L
  
  if keyword_set(nc) OR polyclip_compiled eq 0 then begin ; IDL version
     for j=bottom,top do begin 
        for i=left,right do begin
           px_out=px & py_out=py
           polyclip,i,j,px_out,py_out
           if px_out[0] eq -1 then continue
           ret[ind]=i+j*sx
           if apa then $
              areas[ind]=abs(total(double(px_out)*shift(double(py_out),-1) - $
                                   double(py_out)*shift(double(px_out),-1))/2.)
           if app then $
              polys[ind]=ptr_new([transpose(px_out),transpose(py_out)])
           ind=ind+1L
        endfor
     endfor
  endif else begin        ; Compiled code, use call_external and the shared lib
     inds=reform(rebin(lindgen(nx),nx,ny)+left+ $
                 (rebin(transpose(lindgen(ny)),nx,ny)+bottom)*sx, npix)
     vi=inds mod sx & vj=inds/sx
     px_out=fltarr((npol+4)*npix,/NOZERO)
     py_out=fltarr((npol+4)*npix,/NOZERO)
     ri_out=lonarr(npix+1)
     if size(px,/TYPE) eq 5 then begin ; No double please
        px=float(px) & py=float(py)
     endif
     tmp=call_external(polyclip_path,'polyclip',$
                       VALUE= $
                       [0b,0b,1b,   0b,0b,1b,    0b,    0b,    0b,   0b], $
                       vi,vj,npix,  px,py,npol,  px_out,py_out,areas,ri_out)

     for i=0,npix-1 do begin 
        if ri_out[i] ge ri_out[i+1] then continue ;no overlap for this one
        px_new=px_out[ri_out[i]:ri_out[i+1]-1] 
        py_new=py_out[ri_out[i]:ri_out[i+1]-1]
;         ;;oplot,[px_new,px_new[0]],[py_new,py_new[0]],COLOR=!D.TABLE_SIZE/2
        ret[ind]=vi[i]+vj[i]*sx
        areas[ind]=areas[i]
        if app then polys[ind]=ptr_new([transpose(px_new),transpose(py_new)])
        ind=ind+1L
     endfor 
  endelse 
  if ind eq 0L then return,-1
  if apa then areas=areas[0L:ind-1L]
  if app then polys=polys[0L:ind-1L]
  return,ret[0L:ind-1L]
end
