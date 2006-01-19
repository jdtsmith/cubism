;+
; NAME:  
;
;    IRS_Map
;
; CONTACT:
;
;    UPDATED VERSIONS of CUBISM and more information can be found at:
;       http://spitzer.caltech.edu/cubism
;
; DESCRIPTION:
;    
;    File I/O handler for IRS spectral maps.
;    
; CATEGORY:
;
;    CUBISM Spectral Reduction, File I/O
;    	
; INHERITANCE TREE:
;
;   IRS_File_IO-->IRS_Map
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
pro IRS_Map::SetProperty,WAVELENGTH=wl,MAP_FLUX=map, $
                         MAP_UNCERTAINTY=map_unc,$
                         ASTROMETRY=astr, _EXTRA=e
  if n_elements(map) ne 0 then begin 
     ptr_free,self.map
     self.map=ptr_new(map)
  endif 
  
  if n_elements(map_unc) ne 0 then begin 
     ptr_free,self.map_unc
     self.map_unc=ptr_new(map_unc)
  endif 
  
  if n_elements(astr) ne 0 then begin 
     ptr_free,self.astrometry
     self.astrometry=ptr_new(astr)
  endif
  self->IRS_File_IO::SetProperty,_EXTRA=e
end


;=============================================================================
;  GetProperty
;=============================================================================
pro IRS_Map::GetProperty,MAP_FLUX=map, $
                         MAP_UNCERTAINTY=map_unc,ASTROMETRY=astr, $
                         _REF_EXTRA=e
  if arg_present(map) && ptr_valid(self.map) then map=*self.map
  if arg_present(map_unc) && ptr_valid(self.map_unc) then $
     map_unc=*self.map_unc
  if arg_present(astr) && ptr_valid(self.astrometry) then astr=*self.astrometry

  self->IRS_File_IO::GetProperty,_EXTRA=e
end

;=============================================================================
;  InitHeader
;=============================================================================
pro IRS_Map::InitHeader
  if ~ptr_valid(self.map) then return
  self->IRS_File_IO::InitHeader,*self.map,/EXTEND
  
  ;; Insert celestial coordinates
  if ptr_valid(self.astrometry) then $
     putast,*self.hdr,*self.astrometry,CD_TYPE=0
end


;=============================================================================
;  Save - Save the Map to FITS
;=============================================================================
pro IRS_Map::Save,sf
  self->SaveInit,sf
  if size(sf,/TYPE) ne 7 then return
  
  catch, err
  if err ne 0 then begin
     catch,/cancel
     self->Error,['Error saving map to file '+sf,!ERROR_STATE.MSG]
  endif 
  
  self->AddPar,'FILENAME',file_basename(sf),' Name of this file'
  
  fxwrite,sf,*self.hdr,*self.MAP
    
  ;; Recursively save uncertainty map
  if ptr_valid(self.map_unc) then begin 
     sf2=file_basename(sf,'.fits')+'_unc.fits'
     map=self.map
     self.map=self.map_unc & self.map_unc=ptr_new()
     self->Save,sf2
     self.map_unc=self.map & self.map=map
  endif 
end


;=============================================================================
;  Read - Read a Map file.
;=============================================================================
pro IRS_Map::Read,file
  ;; XXXXXX Not yet implemented
end


;=============================================================================
;  Cleanup
;=============================================================================
pro IRS_Map::Cleanup
  ptr_free,self.map,self.map_unc,self.astrometry
  self->IRS_File_IO::Cleanup
end


;=============================================================================
;  Init
;=============================================================================
function IRS_Map::Init,_EXTRA=e
  if (self->IRS_File_IO::Init(_EXTRA=e) ne 1) then return,0 ;chain up
  ;; Append to default file_base
  if self.file_base then self.file_base+='_map'
  self.file_title='Map'
  return,1
end


pro IRS_Map__define
  sp={IRS_Map, $
      INHERITS IRS_File_IO, $   
      map: ptr_new(), $        ;the cube
      map_unc: ptr_new(), $     ;the cube uncertainty
      astrometry: ptr_new()}    ;the astrometry
  
end
