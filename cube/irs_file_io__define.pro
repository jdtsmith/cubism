
function IRS_File_IO::ReportWidget
  if widget_info(self.parent_group,/VALID_ID) then return,self.parent_group $
  else return,-1
end

;=============================================================================
;  SetProperty
;=============================================================================
pro IRS_File_IO::SetProperty,FITS=fits,IPAC_TABLE=it
  if n_elements(fits) ne 0 then new_fits=keyword_set(fits) else $
     if n_elements(it) ne 0 then new_fits=~keyword_set(it)
  if n_elements(new_fits) ne 0 && new_fits ne self.fits then begin 
     self.fits=new_fits
     self->InitHeader
  endif 
end

;=============================================================================
;  InitHeader - Initialize the header, with date
;=============================================================================
pro IRS_File_IO::InitHeader,data
  if ~ptr_valid(self.hdr) then self.hdr=ptr_new(/ALLOCATE_HEAP)
  if self.fits then begin 
     fxhmake,*self.hdr,data,/DATE,/INITIALIZE
  endif else begin 
     ipac_table_hmake,*self.hdr,/DATE,/INITIALIZE
  endelse
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
pro IRS_File_IO::InheritHeader,hdr,comment
  ;; add a comment ahead of the header material, if necessary
  if n_elements(comment) ne 0 then begin 
     blank=self.fits?' ':'\'
     add_comm=[blank, $
               (self.fits?'':'\')+'          '+ (self.fits?'/':' ') + $
               strmid(comment,0,80-13),$
               blank]
     if ptr_valid(self.hdr) then *self.hdr=[*self.hdr,add_comm] else $
        self.hdr=ptr_new([add_comm])
  endif 
  
  if ~ptr_valid(self.hdr) then $
     self.hdr=ptr_new(self.fits?hdr:'\'+hdr) $
  else $
     *self.hdr=[*self.hdr,self.fits?hdr:'\'+hdr]
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
  if size(sf,/TYPE) eq 7 then self.fits=stregex(sf,'\.fits\$',/BOOLEAN)
  return,sf
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
  if size(sf,/TYPE) eq 7 then self.fits=stregex(sf,'\.fits\$',/BOOLEAN)
  return,sf
end 


pro IRS_File_IO::Cleanup
  ptr_free,self.hdr
  heap_free,self.hdr_extra
end

function IRS_File_IO::Init,PARENT_GROUP=pg,FILE_TITLE=st,FILE_BASE=fb
  if (self->ObjMsg::Init(_EXTRA=e) ne 1) then return,0 ;chain up
  if n_elements(pg) ne 0 then self.parent_group=pg
  if n_elements(st) ne 0 then self.file_title=st else $
     self.file_title='File'
  if n_elements(fb) ne 0 then self.file_base=fb
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
      hdr:ptr_new() }           ;the header itself
end
