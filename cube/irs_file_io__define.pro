;+
; NAME:  
;
;    IRS_File_IO
;
; CONTACT:
;
;    UPDATED VERSIONS of CUBISM and more information can be found at:
;       http://sings.stsci.edu/cubism
;
; DESCRIPTION:
;    
;    Generic class for implementing IRS map, cube, and spectral IO to
;    IPAC tables and FITS files.
;    
; CATEGORY:
;
;    CUBISM Spectral Reduction, File I/O
;    	
; INHERITANCE TREE:
;
;   IRS_File_IO
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

function IRS_File_IO::ReportWidget
  if widget_info(self.parent_group,/VALID_ID) then return,self.parent_group $
  else return,-1
end

;=============================================================================
;  SetProperty
;=============================================================================
pro IRS_File_IO::SetProperty,FITS=fits,IPAC_TABLE=it,VERSION=cv, $
                             CAL_SET=cs, SOFTWARE=soft, FLUX_UNITS=fu, $
                             APERNAME=an
  if n_elements(fits) ne 0 then new_fits=keyword_set(fits) else $
     if n_elements(it) ne 0 then new_fits=~keyword_set(it)
  if n_elements(new_fits) ne 0 && new_fits ne self.fits then $
     self.fits=new_fits

  if n_elements(cv) ne 0 then self.version=cv
  if n_elements(cs) ne 0 then self.cal=cs
  if n_elements(soft) ne 0 then self.software=soft
  if n_elements(fu) ne 0 then self.flux_units=fu
  if n_elements(an) ne 0 then self.apername=an
end

;=============================================================================
;  GetProperty
;=============================================================================
pro IRS_File_IO::GetProperty,HEADER=hdr,SOFTWARE=sw,VERSION=vers, $
                             APERNAME=aper,CAL_SET=cs,FLUX_UNITS=fu, $
                             _REF_EXTRA=e
  if arg_present(hdr) && ptr_valid(self.hdr) then hdr=*self.hdr
  if arg_present(sw) then sw=self.software
  if arg_present(vers) then vers=self.version
  if arg_present(cs) then cs=self.cal
  if arg_present(aper) then aper=self.apername
  if arg_present(fu) then fu=self.flux_units
  self->ObjMsg::GetProperty,_EXTRA=e
end

;=============================================================================
;  ReadHeader
;=============================================================================
pro IRS_File_IO::ReadHeader,hdr
  ptr_free,self.hdr
  self.hdr=ptr_new(hdr,/NO_COPY)
  self.apername=self->GetPar('APERNAME')
  self.version=strtrim(self->GetPar('SOFT_VER'),2)
  self.cal=strtrim(self->GetPar('CAL_SET'),2)
  self.software=strtrim(self->GetPar('SOFTWARE'),2)
  self.flux_units=strtrim(self->GetPar('BUNIT'),2)
end

;=============================================================================
;  InitHeader - Initialize the header, with date, etc.
;=============================================================================
pro IRS_File_IO::InitHeader,data,_EXTRA=e
  if ~ptr_valid(self.hdr) then self.hdr=ptr_new(/ALLOCATE_HEAP)
  if self.fits then begin 
     fxhmake,*self.hdr,data,/DATE,/INITIALIZE,_EXTRA=e
  endif else begin 
     ipac_table_hmake,*self.hdr,/DATE,/INITIALIZE
  endelse

  if self.apername then $
     self->AddPar,'APERNAME',self.apername,' IRS module and order'
  if self.flux_units then $
     self->AddPar,'BUNIT',self.flux_units,' Units of flux data'

  if self.cal then self->AddPar,'CAL_SET',self.cal,' Calibration set'
  if self.software then self->AddPar,'SOFTWARE',self.software, $
                                     ' Product assembly software'
  if self.version then $
     self->AddPar,'SOFT_VER',self.version,' Software version used'
end

;=============================================================================
;  GetPar
;=============================================================================
function IRS_File_IO::GetPar,name
  if self.fits then return,sxpar(*self.hdr,name) else $
     return,ipac_table_xpar(*self.hdr,name)
end

;=============================================================================
;  AddPar
;=============================================================================
pro IRS_File_IO::AddPar,name,value,comment,_EXTRA=e
  if ~ptr_valid(self.hdr) then self.hdr=ptr_new(/ALLOCATE_HEAP)
  if self.fits then fxaddpar,*self.hdr,name,value,comment,_EXTRA=e else $
     ipac_table_addpar,*self.hdr,name,value,_EXTRA=e
end


;=============================================================================
;  AddHist
;=============================================================================
pro IRS_File_IO::AddHist,value,_EXTRA=e
  if ~ptr_valid(self.hdr) then self.hdr=ptr_new(/ALLOCATE_HEAP)
  if self.fits then sxaddhist, value,*self.hdr,_EXTRA=e else $
     ipac_table_addhist,value,*self.hdr,_EXTRA=e
end

;=============================================================================
;  InheritHeader - Inherit a fixed header, with optional leading comment
;=============================================================================
pro IRS_File_IO::InheritHeader,hdr,comment,STRIP=strip
  if ptr_valid(self.hdr) then begin 
     keys=strmid(*self.hdr,0,8)
     if self.fits then endpos=where(keys eq 'END     ',endcnt) $
     else endcnt=0
     wh=where(strtrim(keys,2),cnt)
     if cnt gt 0 then keys=keys[wh] 
     if endcnt gt 0 then begin 
        preh=(*self.hdr)[0:endpos-1]
        posth=(*self.hdr)[endpos:*]
     endif 
  endif 
  
  
  ;; add a comment ahead of the header material, if necessary
  if n_elements(comment) ne 0 then begin 
     blank=self.fits?' ':'\'
     add_comm=[blank, $
               (self.fits?'':'\')+'          '+ (self.fits?'/':' ') + $
               strmid(comment,0,80-13),$
               blank]
  endif 
  
  
  ;; strip the new header of any pre-existing keywords in our header
  if keyword_set(strip) && ptr_valid(self.hdr) then begin 
     new_keys=strmid(hdr,0,8)
     wh=where_not_array(keys,new_keys,cnt)
     if cnt eq 0 then return
     hdr=hdr[wh]
     ;; Clear out any astrometry keywords, for safety
     wh=where(~stregex(hdr,'^((CD|PC)[0-9]_[0-9]|CDELT[0-9]|CRVAL[0-9]|' + $
                       'CRPIX[0-9])',/BOOLEAN),cnt2)
     if cnt2 eq 0 then return
     if cnt2 lt cnt then hdr=hdr[wh]
  endif 
     
  if ~self.fits then h='\'+hdr else h=hdr
  if n_elements(add_comm) gt 0 then h=[add_comm,h]
  
  if ~ptr_valid(self.hdr) then $
     self.hdr=ptr_new(h) $
  else begin 
     if n_elements(preh) gt 0 then $
        *self.hdr=[preh,h,posth] $
     else *self.hdr=[*self.hdr,h]
  endelse 
end


;=============================================================================
;  SaveFile - Return a save file, and set FITS status based on extension
;=============================================================================
function IRS_File_IO::SaveFile,sf
  if size(sf,/TYPE) ne 7 then begin 
     xf,sf,/SAVEFILE, /RECENT, $
        FILTERLIST=['*.{fits,tbl}','*.*', '*'], $
        TITLE='Save '+self.file_title+' as '+ 'File...',/NO_SHOW_ALL, $
        SELECT=0, PARENT_GROUP=self.parent_group, $
        START=self.file_base?(self.file_base+(self.fits?".fits":".tbl")):'', $
        /MODAL
  endif
  if size(sf,/TYPE) eq 7 then self.fits=stregex(sf,'\.fits?$',/BOOLEAN)
  return,sf
end 

;=============================================================================
;  SaveInit - Normalize save file, and pull FITS status from extension.
;=============================================================================
pro IRS_File_IO::SaveInit,sf
  sf=self->SaveFile(sf)
end

;=============================================================================
;  ReadFile - Return a save file, and set FITS status based on extension
;=============================================================================
function IRS_File_IO::ReadFile,sf
  if size(sf,/TYPE) ne 7 then begin 
     xf,file,/RECENT,FILTERLIST=['*.tbl','*.fits','*.*','*'], $
        TITLE='Read '+self.file_title+' from File',/NO_SHOW_ALL,SELECT=0, $
        PARENT_GROUP=self.parent_group,/MODAL
     if size(file,/TYPE) ne 7 then return,-1
     sf=file
  endif
  if size(sf,/TYPE) eq 7 then self.fits=stregex(sf,'\.fits$',/BOOLEAN)
  return,sf
end 


pro IRS_File_IO::Cleanup
  ptr_free,self.hdr
end

function IRS_File_IO::Init,PARENT_GROUP=pg,FILE_TITLE=st,FILE_BASE=fb, $
                           _EXTRA=e
  if (self->ObjMsg::Init(_EXTRA=e) ne 1) then return,0 ;chain up
  if n_elements(pg) ne 0 then self.parent_group=pg
  if n_elements(st) ne 0 then self.file_title=st else $
     self.file_title='File'
  if n_elements(fb) ne 0 then self.file_base=fb
  if n_elements(e) ne 0 then self->SetProperty,_EXTRA=e
  return,1
end


pro IRS_File_IO__define
  sp={IRS_File_IO, $
      INHERITS ObjMsg, $        ;make it an object messanger
      INHERITS ObjReport, $     ;for error, etc. reporting
      fits: 0b, $               ;whether we're writing FITS files
      parent_group: 0L, $       ;parent group
      file_title:'', $          ;title for saving
      file_base:'', $           ;default basename of files to save
      version:'', $             ;software version used
      cal:'', $                 ;calibration set used
      software:'', $            ;the software version
      flux_units:'', $          ;units of the flux
      apername:'', $            ;aperture name (module/order)
      hdr:ptr_new() }           ;the header itself
end
