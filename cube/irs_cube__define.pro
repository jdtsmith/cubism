;+
; NAME:  
;
;    IRS_Cube
;
; CONTACT:
;
;    UPDATED VERSIONS of CUBISM and more information can be found at:
;       http://sings.stsci.edu/cubism
;
; DESCRIPTION:
;    
;    File I/O handler for IRS spectral cubes.
;    
; CATEGORY:
;
;    CUBISM Spectral Reduction, File I/O
;    	
; INHERITANCE TREE:
;
;   IRS_File_IO-->IRS_Cube
;
; MODIFICATION HISTORY:
;    
;    2005-03-24 (J.D. Smith): Written
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

;=============================================================================
;  SetProperty
;=============================================================================
pro IRS_Cube::SetProperty,WAVELENGTH=wl,CUBE_FLUX=cube, $
                          CUBE_UNCERTAINTY=cube_unc,WAVE_UNITS=wu, $
                          ASTROMETRY=astr,CUBE_DATE=cube_date, $
                          _EXTRA=e
  if n_elements(wl) ne 0 then begin 
     ptr_free,self.wavelength
     self.wavelength=ptr_new(wl)
  endif 
  if n_elements(cube) ne 0 then begin 
     ptr_free,self.cube
     self.cube=ptr_new(cube)
  endif 
  
  if n_elements(cube_unc) ne 0 then begin 
     ptr_free,self.cube_unc
     self.cube_unc=ptr_new(cube_unc)
  endif 
  
  if n_elements(astr) ne 0 then begin 
     ptr_free,self.astrometry
     self.astrometry=ptr_new(astr)
  endif 
  
  if n_elements(wu) ne 0 then self.wave_units=wu
  if n_elements(cube_date) ne 0 then self.cube_date=cube_date
  self->IRS_File_IO::SetProperty,_EXTRA=e
end


;=============================================================================
;  GetProperty
;=============================================================================
pro IRS_Cube::GetProperty,WAVE_UNITS=wu, $
                          WAVELENGTH=wl, CUBE_FLUX=cube, $
                          CUBE_UNCERTAINTY=cube_unc,ASTROMETRY=astr, $
                          CUBE_DATE=cube_date,_REF_EXTRA=e
  if arg_present(wu) then wu=self.wave_units
  if arg_present(wl) && ptr_valid(self.wavelength) then wl=*self.wavelength
  if arg_present(cube) && ptr_valid(self.cube) then cube=*self.cube
  if arg_present(cube_unc) && ptr_valid(self.cube_unc) then $
     cube_unc=*self.cube_unc
  if arg_present(astr) && ptr_valid(self.astrometry) then astr=*self.astrometry
  if arg_present(cube_date) then cube_date=self.cube_date
  self->IRS_File_IO::GetProperty,_EXTRA=e
end

;=============================================================================
;  InitHeader
;=============================================================================
pro IRS_Cube::InitHeader
  if ~ptr_valid(self.cube) then return
  self->IRS_File_IO::InitHeader,*self.cube,/EXTEND
  
  ;; Insert celestial coordinates... always in FITS
  if ptr_valid(self.astrometry) then $
     putast,*self.hdr,*self.astrometry,CD_TYPE=0
  
  ;; Wavelength coordinates and wavelength LUT binary table extension.
  self->AddPar,'PC3_3', 1,' Index Separation in wavelength table'
  self->AddPar,'CRPIX3',1,' Reference pixel'
  self->AddPar,'CRVAL3',1,' Table index for reference pixel'
  self->AddPar,'CTYPE3','WAVE-TAB',' Wavelength'
  self->AddPar,'CUNIT3',self.wave_units,' Wavelength units'
  self->AddPar,'PS3_0','WCS-TAB',' Coordinate table extension name'
  self->AddPar,'PS3_1','WAVELENGTH',' Coordinate table column name'
  
  ;; Date
  self->AddPar,'CUBE-DT',jul2date(self.CUBE_DATE),' Cube build date'
end


;=============================================================================
;  Save - Save the Cube to FITS with LUT extension
;=============================================================================
pro IRS_Cube::Save,sf
  self->SaveInit,sf
  if size(sf,/TYPE) ne 7 then return
  
  catch, err
  if err ne 0 then begin
     catch,/cancel
     self->Error,['Error saving cube to file '+sf,!ERROR_STATE.MSG]
  endif 
  
  self->AddPar,'FILENAME',file_basename(sf),' Name of this file'
  
  fxwrite,sf,*self.hdr,*self.CUBE
  
  ;; Make the wavelength LUT extension header
  fxbhmake,hdr,1,'WCS-TAB','Wavelength look-up table for cube dimension 3'
  wave=transpose(*self.WAVELENGTH)
  fxbaddcol,wcol,hdr,wave,'WAVELENGTH','Column label',TUNIT='um'
  fxbcreate,unit,sf,hdr
  fxbwrite,unit,wave,wcol,1
  fxbfinish,unit
  
  ;; Recursively save uncertainty cube
  if ptr_valid(self.cube_unc) then begin 
     sf=file_basename(sf,'.fits')+'_unc.fits'
     cube=self.cube
     self.cube=self.cube_unc & self.cube_unc=ptr_new()
     self->Save,sf
     self.cube_unc=self.cube & self.cube=cube
  endif 
end

;=============================================================================
;  Read - Read a Cube file.
;=============================================================================
pro IRS_Cube::Read,file
  file=self->ReadFile(file)
  
  cube=readfits(file,hdr,/SILENT)
  self->ReadHeader,hdr

  self.cube=ptr_new(cube,/NO_COPY)
  
  unc_f=file_basename(file,'.fits')+'_unc.fits'
  if file_test(unc_f,/READ) then $
     self.cube_unc=ptr_new(readfits(unc_f,/SILENT))
  
  fxbopen,un,file,'WCS-TAB'
  fxbread,un,wl,'WAVELENGTH'
  fxbclose,un
  
  self.WAVELENGTH=ptr_new(reform(wl))
end


;=============================================================================
;  ReadHeader - Read info from the FITS header
;=============================================================================
pro IRS_Cube::ReadHeader,hdr
  if sxpar(hdr,'NAXIS') ne 3 then message,'Incorrect cube dimensions.'
  self.cube_date=date2jul(sxpar(hdr,'CUBE-DT'))
    
  extast,hdr,astr,np
  if np ne 3 then message,'Failed to identify cube astrometry.'
  self.astrometry=ptr_new(astr,/NO_COPY)
  
  self->IRS_File_IO::ReadHeader,hdr
end

;=============================================================================
;  Cleanup
;=============================================================================
pro IRS_Cube::Cleanup
  ptr_free,self.wavelength,self.cube,self.cube_unc,self.astrometry
  self->IRS_File_IO::Cleanup
end


;=============================================================================
;  Init
;=============================================================================
function IRS_Cube::Init,_EXTRA=e
  if (self->IRS_File_IO::Init(_EXTRA=e) ne 1) then return,0 ;chain up
  ;; Append to default file_base
  if self.file_base then self.file_base+='_cube'
  self.file_title='Cube'
  return,1
end


pro IRS_Cube__define
  sp={IRS_Cube, $
      INHERITS IRS_File_IO, $   
      wavelength: ptr_new(), $  ;wavelength
      wave_units: '', $         ;units for wavelength
      cube: ptr_new(), $        ;the cube
      cube_unc: ptr_new(), $    ;the cube uncertainty
      astrometry: ptr_new(), $  ;the astrometry
      cube_date:0.0D}           ;julian date of cube build
  
end
