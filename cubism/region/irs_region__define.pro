;+
; NAME:  
;
;    IRS_Region
;
; CONTACT:
;
;    UPDATED VERSIONS of CUBISM and more information can be found at:
;       http://sings.stsci.edu/cubism
;
; DESCRIPTION:
;    
;    Class for holding and querying polygonal regions.
;
; CATEGORY:
;
;    IRS Spectral Reduction, Analysis and Processing.
;    Regions
;
; MODIFICATION HISTORY:
;
;    2012-03-21 (J.D. Smith): Fixed 1/2 pixel offset from double-conversion.
;    2005-03-24 (J.D. Smith): Initially written.
;   
;-
;    $Id: irs_region__define.pro,v 1.19 2008/09/04 22:03:02 jdsmith Exp $
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2005-2010 J.D. Smith
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
;  Boston, MA 02110-1301, USA.
;
;##############################################################################


;; Coordinates assumed J2000 decimal degrees (FK5).

;=============================================================================
;  ClipRegion -- Clip this region against an image with the given
;                astrometry record, and return the clipped pixels.
;=============================================================================
function IRS_Region::ClipRegion,image,clip_astr,_REF_EXTRA=ref
  if ~ptr_valid(self.region) then return,-1
  self->GetProperty,X=x,Y=y,ASTROMETRY=clip_astr
  s=size(image,/DIMENSIONS)
  return,polyfillaa(x,y,s[0],s[1],_EXTRA=ref)
end

;=============================================================================
;  RegionPhotometry -- Compute area averaged photometry of an image
;                      over our region
;=============================================================================
function IRS_Region::RegionPhotometry,image,clip_astr,AREAS=areas, $
                                      IMAGE_UNCERTAINTY=unc_image, $
                                      UNCERTAINTY=unc
  pix=self->ClipRegion(image,clip_astr,AREAS=areas)
  if pix[0] eq -1 then return,!VALUES.F_NAN
  if keyword_set(unc_image) && arg_present(unc) then $
     unc=sqrt(total(unc_image[pix]^2*areas^2,/NAN))/total(areas,/NAN)
  return,total(image[pix]*areas,/NAN)/total(areas,/NAN)
end


;=============================================================================
;  GetProperty
;=============================================================================
pro IRS_Region::GetProperty,ASTROMETRY=astr,RA=ra,DEC=dec,X=x,Y=y, $
                            SOLID_ANGLE=sa,CENTROID=cen,OFFSET_ANGLES=oa
  if arg_present(reg) && ptr_valid(self.region) then reg=*self.region
  
  if arg_present(ra) || arg_present(dec) || arg_present(cen) || $
     arg_present(x) || arg_present(y) then begin 
     if ptr_valid(self.region) then begin 
        if n_elements(*self.region) eq 3 then begin 
           r=self->CircleRegion(ASTROMETRY=astr)
           ra=reform(r[0,*]) & dec=reform(r[1,*])
        endif else begin 
           ra=reform((*self.region)[0,*])
           dec=reform((*self.region)[1,*])
        endelse 
        if arg_present(x) && n_elements(astr) ne 0 then begin 
           ad2xy,ra,dec,astr,x,y ;; "IDL convention: first pixel center 0.0"
           x+=0.5 & y+=0.5
        endif 
        cen=[mean(ra),mean(dec)] ;XXX not true generally!!!
     endif 
  endif 
  if arg_present(sa) then sa=self->RegionSolidAngle()
  if arg_present(oa) then oa=self->OffSetAngles()
end

;=============================================================================
;  RegionSolidAngle -- Return the solid angle of the polygonal region
;                      in steradians.
;=============================================================================
function IRS_Region::RegionSolidAngle
  if ~ptr_valid(self.region) then return,-1
  if n_elements(*self.region) eq 3 then $
     return,2.D*!DPI*(1.D - cos((*self.region)[2]/3600.D/(180.D/!DPI)))
  self->GetProperty,RA=ra,DEC=dec
  return,spherical_poly_area(ra,dec)
end

;=============================================================================
;  OffsetAngles -- Calculate the angles of offset between consecutive
;                  points in the region, or the radius of a circle,
;                  return in decimal degrees.
;=============================================================================
function IRS_Region::OffsetAngles
  if ~ptr_valid(self.region) then return,-1
  if n_elements(*self.region) eq 3 then return,(*self.region)[3]/3600.D
  reg=*self.region
  reg2=shift(reg,0,1)
  return,spherical_angle_offset(reg[0,*],reg[1,*],reg2[0,*],reg2[1,*])
end

;=============================================================================
;  Centroid -- Compute the approximate centroid of the spherical
;              polygonal region.  Project to tangent plane, form 2D
;              centroid, and de-project.
;=============================================================================
;;function IRS_Region::Centroid
;;  reg=self->Region()            ;in ra/dec degrees
;;  if reg[0] eq -1 then return,-1
;;  RADEG=180.D/!DPI
;;  ra=reg[0,*]/RADEG & dec=reg[1,*]/RADEG
;;  ra_mean=mean(ra) & dec_mean=mean(dec)
;;  cosc=sin(dec_mean)*sin(dec) + cos(dec_mean)*cos(dec)*cos(ra-ra_mean)
;;  x=cos(dec)*sin(ra-ra_mean)/cosc
;;  y=(cos(dec_mean)*sin(dec)-sin(dec_mean)*cos(dec)*cos(ra-ra_mean))/cosc
;;  return
;;end


;=============================================================================
;  CircleRegion -- Convert a given center (decimal degrees or pixels)
;                  and radius (arcsec, or pixels) into a polygon of
;                  appropriate length.
;=============================================================================
function IRS_Region::CircleRegion,c,r,ASTROMETRY=castr
  ;; Convert circle region directly to polygon
  if keyword_set(castr) then begin 
     getrot,castr,rot,cd
     cd=mean(abs(cd*3600.D))    ;arcsec/pixel
  endif else cd=1.D             ;just assume 1 arcsec/pixel
  
  if n_elements(c) eq 0 then begin 
     if ptr_valid(self.region) && n_elements(*self.region) eq 3 then begin 
        c=(*self.region)[0:1]
        r=(*self.region)[2]
     endif else message,"No circular region specified."
  endif 
  
  n_pol=12>long(4*!DPI*r/cd)<200 ;aka circumference in 1/2 pixel units
  ang=findgen(1,n_pol)/(n_pol-1.)*2*!PI
  pol=[c[0]+cos(ang)*r/3600.D/cos(c[1]/!RADEG), $
       c[1]+sin(ang)*r/3600.D]
  return,pol
end

;=============================================================================
;  SetRegionFromBox -- Set region from simple pixel box coordinates
;=============================================================================
pro IRS_Region::SetRegionFromBox,ll,ur0,OUTPUT_POLY=op,ASTROMETRY=astr
  ur=ur0+1 ;; bounding region goes up to the next higher pixel
  op=[[ll[0],ll[1]], $
      [ll[0],ur[1]], $
      [ur[0],ur[1]], $
      [ur[0],ll[1]]]
  self->SetRegion,op,ASTROMETRY=astr
end

;=============================================================================
;  SetRegion -- Region must be 2xn or 3 element list. ASTROMETRY can
;               be passed, and should if REG is in pixel coordinates
;               (it will be converted to RA,DEC).
;=============================================================================
pro IRS_Region::SetRegion,reg,RESET=reset,ASTROMETRY=astr
  
  ;; Pixel indexing conventions:
  ;; FITS HEADERS/FORTRAN :  1st centered on [1.0,1.0]
  ;; NASALIB WCS PROGRAMS :  1st centered on [0.0,0.0]
  ;; CUBISM, aka God-Given : 1st centered on [0.5,0.5]

  if n_elements(reset) eq 0 then heap_free,self.region
  if (size(reg,/DIMENSIONS))[0] ne 2 && n_elements(reg) ne 3 then $
     message,'Region must be 2xn or 3 elements.'
  
  if n_elements(astr) ne 0 then begin 
     if n_elements(reg) eq 3 then begin 
        ;; circular region in pixel coordinates
        xy2ad,reg[0]-.5,reg[1]-.5,astr,ra,dec
        getrot,astr,r,cd
        r=reg[2]*mean(abs(cd*3600.D)) ;convert radius in pixels to arcsec
        region=[ra,dec,r]
     endif else begin 
        xy2ad,reg[0,*]-.5,reg[1,*]-.5,astr,ra,dec ;convert pixels to coords
        region=[1#ra,1#dec]
     endelse 
  endif else region=reg
  if ptr_valid(self.region) then *self.region=region else $
     self.region=ptr_new(region,/NO_COPY)
end

;=============================================================================
;  ReadRegion -- Read spectrum-based or DS9 region file (fk5, n-sided
;                polygon or circle only!)
;=============================================================================
pro IRS_Region::ReadRegion,file,_EXTRA=e
  if size(file,/TYPE) ne 7 then begin 
     xf,file,/RECENT,FILTERLIST=['*.{tbl,reg}','*.*','*'], $
        TITLE='Read Region File...',/MODAL,SELECT=0,/NO_SHOW_ALL, $
        _EXTRA=e
     if size(file,/TYPE) ne 7 then return                
  endif 
  err_msg='Only FK5 region polygons (any length) accepted.'

  if strpos(file,'.reg') eq strlen(file)-4 then begin ;; DS9 region!
     openr,un,/get_lun,file
     line=''
     got_fk=0
     while ~eof(un) do begin 
        readf,un,line
        if strmid(line,0,1) eq '#' then continue
        
        if ~got_fk && $
           (fk=(stregex(line,'^ *(fk[0-9])',/EXTRACT,/SUBEXPR))[1]) then begin
           if fk then begin 
              got_fk=1
              if fk ne 'fk5' then begin 
                 free_lun,un
                 message,err_msg
                 return
              endif 
           endif 
        endif 
        if strpos(line,'polygon(') ne -1 then begin ; got a polygon
           if ~got_fk || fk ne 'fk5' then begin 
              free_lun,un
              message,err_msg
              return
           endif 
           parts=stregex(line,'polygon\(([^)]+)\)',/SUBEXPR,/EXTRACT)
           coords=parts[1]
           coords=strsplit(coords,',',/EXTRACT)
           if strpos(coords[0],':') ne -1 then begin ;segixesimal
              fcoords=dblarr(2,n_elements(coords)/2)
              for i=0,n_elements(coords)/2-1 do begin 
                 get_coords,c,INSTRING=coords[2*i]+' '+coords[2*i+1]
                 c[0]*=15.D     ;to degrees
                 fcoords[0,i]=c
              endfor 
              coords=fcoords
           endif else coords=double(coords)
           coords=reform(coords,2,n_elements(coords)/2)
           break
        endif else if strpos(line,'circle(') ne -1 then begin ;circle
           if ~got_fk || fk ne 'fk5' then begin 
              free_lun,un
              message,err_msg
              return
           endif 
           parts=stregex(line,'circle\(([^)]+)\)',/SUBEXPR,/EXTRACT)
           crad=parts[1]
           crad=strsplit(crad,',',/EXTRACT)
           rad=double(crad[2])  ;radius in arcsecs!
           crad=crad[0:1]
           if strpos(crad[0],':') ne -1 then begin ;segixesimal
              fcoords=dblarr(2)
              get_coords,crad,INSTRING=crad[0]+' '+crad[1]
              crad[0]*=15.D        ;to degrees
           endif else crad=double(crad)
           coords=[crad,rad]
           break
        endif 
     endwhile
     if n_elements(coords) eq 0 then begin 
        free_lun,un
        message,err_msg
        return
     endif 
     self->SetRegion,coords
     free_lun,un
  endif else begin              ;IPAC table spectrum file
     st=read_ipac_table(file,hdr)
     self->ParseRegion,hdr,file,/IPAC_TABLE
  endelse 
end

;=============================================================================
;  WriteDS9Region -- Write region as a DS9 region file
;=============================================================================
pro IRS_Region::WriteDS9Region,file,_EXTRA=e
  if ~ptr_valid(self.region) then return
  if size(file,/TYPE) ne 7 then begin 
     xf,file,/RECENT,FILTERLIST=['*.reg','*.*','*'],/SAVEFILE, $
        TITLE='Save as DS9 Region File...',/MODAL,SELECT=0,/NO_SHOW_ALL, $
        _EXTRA=e
     if size(file,/TYPE) ne 7 then return                
  endif 
  openw,un,/get_lun,file
  printf,un,'# Region file format: DS9 version 3.0'
  printf,un,'# CUBISM-derived spectral aperture region file'
  if self.source then printf,un,'# Source: '+self.source
  printf,un,'global color=green select=1 edit=0 move=0 delete=1 include=1 ' + $
         'fixed=0 source'
  
  if n_elements(*self.region) eq 3 then begin 
     printf,un,' fk5; circle('+ $
            strjoin(string(FORMAT='(F0.7)',*self.region,","),",") +'")'
  endif else $
     printf,un,' fk5; polygon('+strjoin(string(FORMAT='(F0.7)',*self.region),",")+')'
  free_lun,un
end

;=============================================================================
;  WriteRegion -- Write Region info to header (IPAC table) or append
;                 to file as REGION extension (FITS, TBD).
;=============================================================================
pro IRS_Region::WriteRegion,bhdr,file,IPAC_TBL=ipac_tbl
  if ~ptr_valid(self.region) then return
  if ~keyword_set(ipac_tbl) then begin 
     ;; XXX write Chandra region file extension(s) to FITS file
  endif else begin 
     s='Extraction Area:'
     if n_elements(*self.region) eq 3 then begin ;circle
        s=[s,string(FORMAT= '("# Circle: ",A,",",A,",",F-0.4)', $
                    radecstring((*self.region)[0],/RA,PRECISION=3), $
                    radecstring((*self.region)[1],PRECISION=3),(*self.region)[2])]
     endif else begin 
        self->GetProperty,RA=ra,DEC=dec
        nra=n_elements(ra) 
        s=[s,string(FORMAT= '("# Poly: ",2(A,",",A,:," ; "),"]")', $
                    radecstring(ra[0],/RA,PRECISION=3), $
                    radecstring(dec[0],PRECISION=3), $
                    radecstring(ra[1],/RA,PRECISION=3), $
                    radecstring(dec[1],PRECISION=3))]
        for i=1,nra/2-1 do $
           s=[s,string(FORMAT='("#       ",2(A,",",A,:," ; "),"]")', $
                       radecstring(ra[2*i],/RA,PRECISION=3), $
                       radecstring(dec[2*i],PRECISION=3), $
                       radecstring(ra[2*i+1],/RA,PRECISION=3), $
                       radecstring(dec[2*i+1],PRECISION=3))]
        if nra mod 2 ne 0 then $
           s=[s,string(FORMAT='("#       ",A,",",A)', $
                      radecstring(ra[nra-1],/RA,PRECISION=3), $
                      radecstring(dec[nra-1],PRECISION=3))]
     endelse 
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
        if strmid(line,1,1) ne '#' then continue
        if ~got_box then begin 
           if strmid(line,3,4) eq 'Box:' || strmid(line,3,5) eq 'Poly:' $
           then begin 
              ext=stregex(line,match+','+match+' ; '+match+','+match, $
                          /EXTRACT,/SUBEXPR)
              if ~ext[0] then return
              ra=[ext[1],ext[3]]
              dec=[ext[2],ext[4]]
              got_box=1
           endif else if strmid(line,3,7) eq 'Circle:' then begin 
              ext=stregex(line,match+','+match+','+'([0-9]+\.[0-9]*)',$
                          /EXTRACT,/SUBEXPR)
              if ~ext[0] then return
              ra=ext[1]
              dec=ext[2]
              radius=ext[3]
              break
           endif 
        endif else begin 
           ;; Continuing lines
           ext=stregex(line,match+','+match+' ; '+match+','+match,/EXTRACT, $
                       /SUBEXPR)
           if ~ext[0] then begin ;; check for a final line on it's own
              ext=stregex(line,match+','+match,/EXTRACT,/SUBEXPR)
              if ~ext[0] then break ;first non match
              ra=[ra,ext[1]] & dec=[dec,ext[2]]
              break
           endif else begin 
              ra=[ra,ext[1],ext[3]]
              dec=[dec,ext[2],ext[4]]
           endelse 
        endelse 
     endfor 
     if n_elements(radius) ne 0 then begin 
        get_coords,c,INSTRING=ra+' '+dec
        c[0]*=15.0D
        aper=[c,radius]
     endif else begin 
        aper=make_array(2,n_elements(ra),/DOUBLE)
        for i=0,n_elements(ra)-1 do begin 
           get_coords,c,INSTRING=ra[i]+' '+dec[i]
           c[0]*=15.0D
           aper[0,i]=c[0]
           aper[1,i]=c[1]
        endfor 
     endelse 
     self->SetRegion,aper
  endelse 
end

;=============================================================================
;  Cleanup
;=============================================================================
pro IRS_Region::Cleanup
  heap_free,self.region
end


;=============================================================================
;  Init
;=============================================================================
function IRS_Region::Init,source
  if n_elements(source) ne 0 then self.source=source
  return,1
end

;=============================================================================
;  IRS_Region__define
;=============================================================================
pro IRS_Region__define
  st={IRS_Region, $
      source:'', $              ; source file, if any for region
      region: ptr_new()}        ; region as 2xn lists of points, 
                                ; in ra/dec degree coordinates, or 3
                                ; element vector,
                                ; [ra_cen,dec_cen,rad], in degrees,
                                ; degrees, arcsec.  All J2000
end
