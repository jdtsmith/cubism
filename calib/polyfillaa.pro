
;; Polyfillaa, using either built-in or auto-compiled code.

function polyfillaa, px,py,sx,sy, AREAS=areas, POLYGONS=polys,NO_COMPILED=nc, $
                     RECOMPILE=rc
  common polyfillaa_external,polyclip_compiled,polyclip_path
  if n_elements(polyclip_compiled) eq 0 OR keyword_set(rc) then begin 
     catch, err
     if err ne 0 then begin   ; any failure in compiling, just use the IDL vers
        polyclip_compiled=0
     endif else begin 
        resolve_routine,'polyclip'
        path=(routine_info('polyclip',/SOURCE)).PATH
        path=strmid(path,0,strpos(path,path_sep(),/REVERSE_SEARCH))
        make_dll,'polyclip_new','polyclip',INPUT_DIRECTORY=path, $
                 DLL_PATH=polyclip_path ;,/REUSE_EXISTING
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
