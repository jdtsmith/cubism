

;; Coordinates assumed J2000 decimal degrees (FK5).

;=============================================================================
;  UpdateAstrometry -- Convert region to a new astrometry
;=============================================================================
pro IRS_Region::UpdateAstrometry,astr
  if ~ptr_valid(self.region) then return
  if ptr_valid(self.astr) then begin 
     x=(*self.region)[0,*]
     y=(*self.region)[1,*]
     xy2ad,x-.5,y-.5,*self.astr,ra,dec
  endif else begin 
     ra=(*self.region)[0,*]
     dec=(*self.region)[1,*]
     self.astr=ptr_new(/ALLOCATE_HEAP)
  endelse 
  
  ad2xy,ra,dec,astr,newx,newy
  *self.region=[newx+0.5,newy+0.5]
  *self.astr=astr
end


;=============================================================================
;  Region -- Return specified region as 2xn list of celestial coords
;=============================================================================
function IRS_Region::Region
  if ~ptr_valid(self.region) then return,-1
  
  reg=*self.region
  if ptr_valid(self.astr) then begin 
     x=reg[0,*] & y=reg[1,*]
     xy2ad,x-0.5,y-0.5,*self.astr,ra,dec
     reg=transpose([[ra],[dec]])
  endif
  return,reg
  
end

;=============================================================================
;  SetRegion -- Region must be 2xn list,  ASTROMETRY can be passed 
;=============================================================================
pro IRS_Region::SetRegion,reg,RESET=reset,ASTROMETRY=astr
  if n_elements(reset) eq 0 then heap_free,self.region
  if (size(reg,/DIMENSIONS))[0] ne 2 then message,'Region must be 2xn.'
  
  if ptr_valid(self.region) then *self.region=reg else $
     self.region=ptr_new(reg)
  
  ptr_free,self.astr
  if n_elements(astr) ne 0 then self.astr=ptr_new(astr)
end

;=============================================================================
;  WriteRegion -- Write Region info to header (IPAC table) or append
;                 to file as REGION extension (FITS).
;=============================================================================
pro IRS_Region::WriteRegion,bhdr,file,IPAC_TBL=ipac_tbl
  if ~ptr_valid(self.region) then return
  if ~keyword_set(ipac_tbl) then begin 
     ;; XXX write Chandra region file extension(s) to FITS file
  endif else begin 
     ra=(*self.region)[0,*]
     dec=(*self.region)[1,*]
     if ptr_valid(self.astr) then begin 
        ;; assume they are in Cubism native pixel units, aka 0.5,0.5 centering
        xy2ad,ra-0.5,dec-0.5,*self.astr,newra,newdec
        ra=newra & dec=newdec
     endif 
     
     ;; XXX more general ASCII region format here
        
     s=string(FORMAT= '("# Box: ",2(A,",",A,:," ; "),"]")', $
              radecstring(ra[0],/RA,PRECISION=3), $
              radecstring(dec[0],PRECISION=3), $
              radecstring(ra[1],/RA,PRECISION=3), $
              radecstring(dec[1],PRECISION=3))
     s=[s,string(FORMAT='("#      ",2(A,",",A,:," ; "),"]")', $
                 radecstring(ra[2],/RA,PRECISION=3), $
                 radecstring(dec[2],PRECISION=3), $
                 radecstring(ra[3],/RA,PRECISION=3), $
                 radecstring(dec[3],PRECISION=3))]
     ipac_table_addhist,s,bhdr,/BLANK
  endelse 
end

;=============================================================================
;  ParseRegion -- Read region from file extension or header
;=============================================================================
pro IRS_Region::ParseRegion,hdr,file,IPAC_TABLE=ipac_table
  if ~keyword_set(ipac_table) then begin 
     ;; XXX read Chandra region file extension(s) from FITS file
     ;; N.B. Use Cubism-Native indexing convention [0.5,0.5] internally
  endif else begin 
     ;; XXX Better ASCII region encoding...
     match='([+-]?[0-9]{2}:[0-9]{2}:[0-9.]+)'
     got_box=0
     for i=0,n_elements(hdr)-1 do begin 
        line=strmid(hdr[i],1)
        if strmid(line,0,1) ne '#' then continue
        if ~got_box then begin 
           if strmid(line,2,4) eq 'Box:' then begin 
              ext=stregex(line,match+','+match+' ; '+match+','+match, $
                          /EXTRACT,/SUBEXPR)
              if n_elements(ext) lt 5 then return
              ra=[ext[1],ext[3]]
              dec=[ext[2],ext[4]]
              got_box=1
           endif 
        endif else begin 
           ext=stregex(line,match+','+match+' ; '+match+','+match,/EXTRACT, $
                       /SUBEXPR)
           if n_elements(ext) lt 5 then return
           ra=[ra,ext[1],ext[3]]
           dec=[dec,ext[2],ext[4]]
           aper=make_array(2,n_elements(ra),/DOUBLE)
           for i=0,n_elements(ra)-1 do begin 
              get_coords,c,INSTRING=ra[i]+' '+dec[i]
              c[0]*=15.0D
              aper[0,i]=c[0]
              aper[1,i]=c[1]
           endfor 
           break
        endelse 
     endfor 
     self.region=ptr_new(aper,/NO_COPY)
     ptr_free,self.astr         ;they're celestial coords already
  endelse 
end

;=============================================================================
;  Cleanup
;=============================================================================
pro IRS_Region::Cleanup
  heap_free,self.region
  heap_free,self.astr
end

;=============================================================================
;  IRS_Region__define
;=============================================================================
pro IRS_Region__define
  st={IRS_Region, $
      source:'', $              ;source file, if any for region
      region: ptr_new(), $      ; region as 2xn lists of points, 
                                ; in pixel or celestial coordinates
      astr:ptr_new()}           ;astrometry structure, if celestial
                                ; coordinates
  
;XXX other pertinent information from the Chandra REGION standard?  
end
