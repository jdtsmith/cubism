;+
; NAME:  
;
;    IRS_Region
;
; CONTACT:
;
;    UPDATED VERSIONS of CUBISM and more information can be found at:
;       http://ssc.spitzer.caltech.edu/cubism
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
;    2005-03-24 (J.D. Smith): Initially written.
;   
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2005 J.D. Smith
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
  self->GetProperty,RA=ra,DEC=dec
  ad2xy,ra,dec,clip_astr,x,y
  s=size(image,/DIMENSIONS)
  return,polyfillaa(x-0.5,y-0.5,s[0],s[1],_EXTRA=ref)
end


;=============================================================================
;  RegionPhotometry -- Compute area averaged photometry of an image
;                      over our region
;=============================================================================
function IRS_Region::RegionPhotometry,image,clip_astr
  pix=self->ClipRegion(image,clip_astr,AREAS=areas)
  if pix[0] eq -1 then return,!VALUES.F_NAN
  return,total(image[pix]*areas,/NAN)/total(areas,/NAN)
end

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
;  GetProperty
;=============================================================================
pro IRS_Region::GetProperty,REGION=reg,ASTROMETRY=astr,RA=ra,DEC=dec, $
                            SOLID_ANGLE=sa,CENTROID=cen,OFFSET_ANGLES=oa
  if arg_present(reg) && ptr_valid(self.region) then reg=*self.region
  if arg_present(astr) && ptr_valid(self.astr) then astr=*self.astr
  if arg_present(ra) || arg_present(dec) || arg_present(cen) then begin 
     if ptr_valid(self.region) then begin 
        ra=(*self.region)[0,*]
        dec=(*self.region)[1,*]
        if ptr_valid(self.astr) then begin 
           ;; assume they are in Cubism native pixel units, aka 0.5,0.5 centers
           xy2ad,ra-0.5,dec-0.5,*self.astr,newra,newdec
           ra=newra & dec=newdec
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
  reg=self->Region()            ;in ra/dec degrees
  if reg[0] eq -1 then return,-1
  return,spherical_poly_area(reg[0,*],reg[1,*])
end



;=============================================================================
;  OffsetAngles -- Calculate the angles of offset between consecutive
;                  points in the region
;=============================================================================
function IRS_Region::OffsetAngles
  reg=self->Region()
  if reg[0] eq -1 then return,-1
  RADEG=180.D/!DPI
  reg/=RADEG
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
;  Region -- Return specified region as 2xn list of celestial coords
;=============================================================================
function IRS_Region::Region
  if ~ptr_valid(self.region) then return,-1
  
  reg=*self.region
  if ptr_valid(self.astr) then begin 
     x=reg[0,*] & y=reg[1,*]
     xy2ad,x-0.5,y-0.5,*self.astr,ra,dec
     if size(ra,/N_DIMENSIONS) eq 1 then reg=transpose([[ra],[dec]]) $
     else reg=[ra,dec]
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
;  WriteDS9Region -- Write region as a DS9 region file
;=============================================================================
pro IRS_Region::WriteDS9Region,file
  openw,un,/get_lun,file
  printf,un,'# Region file format: DS9 version 3.0'
  printf,un,'# CUBISM-derived spectral aperture region file'
  if self.source then printf,un,'# Source: '+self.source
  printf,un,'global color=green select=1 edit=0 move=0 delete=1 include=1 ' + $
         'fixed=0 source fk5;'
  reg=self->Region()
  printf,un,'polygon('+strjoin(string(FORMAT='(F0.7)',reg),",")+')'
  free_lun,un
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
     self->GetProperty,RA=ra,DEC=dec
     
     ;; XXX more general ASCII region format here
     s='Extraction Rectangle:'
     s=[s,string(FORMAT= '("# Box: ",2(A,",",A,:," ; "),"]")', $
              radecstring(ra[0],/RA,PRECISION=3), $
              radecstring(dec[0],PRECISION=3), $
              radecstring(ra[1],/RA,PRECISION=3), $
              radecstring(dec[1],PRECISION=3))]
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
        if strmid(line,1,1) ne '#' then continue
        if ~got_box then begin 
           if strmid(line,3,4) eq 'Box:' then begin 
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
      source:'', $              ;source file, if any for region
      region: ptr_new(), $      ; region as 2xn lists of points, 
                                ; in pixel or ra/dec degree coordinates
      astr:ptr_new()}           ;astrometry structure, if celestial
                                ; coordinates
  
;XXX other pertinent information from the Chandra REGION standard?  
end
