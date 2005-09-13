
;=============================================================================
;  AddDataColumn - Add an extra column of data
;=============================================================================
pro IRS_Spectrum::AddDataColumn,name,units,data
  add={IRS_Spectrum_Extra_Data,name,units,ptr_new(data)}
  if ptr_valid(self.extra_data) then *self.extra_data=[*self.extra_data,add] $
  else self.extra_data=ptr_new(add,/NO_COPY)
end


;=============================================================================
;  SetProperty
;=============================================================================
pro IRS_Spectrum::SetProperty,REGION=region,WAVELENGTH=wl,SPECTRUM_FLUX=flux, $
                              SPECTRUM_UNCERTAINTY=unc,WAVE_UNITS=wu,_EXTRA=e
  if n_elements(region) ne 0 then self.region=region
  if n_elements(wl) ne 0 then begin 
     ptr_free,self.wavelength
     self.wavelength=ptr_new(wl)
  endif 
  if n_elements(flux) ne 0 then begin 
     ptr_free,self.flux
     self.flux=ptr_new(flux)
  endif 
  
  if n_elements(unc) ne 0 then begin 
     ptr_free,self.unc
     self.unc=ptr_new(unc)
  endif 
  
  if n_elements(wu) ne 0 then self.wave_units=wu
  self->IRS_File_IO::SetProperty,_EXTRA=e  
end


;=============================================================================
;  GetProperty
;=============================================================================
pro IRS_Spectrum::GetProperty,REGION=region,WAVE_UNITS=wu, $
                              WAVELENGTH=wl, SPECTRUM_FLUX=flux, $
                              SPECTRUM_UNCERTAINTY=unc,_REF_EXTRA=e
  if arg_present(wu) then wu=self.wave_units
  if arg_present(region) then region=self.region
  if arg_present(wl) && ptr_valid(self.wavelength) then wl=*self.wavelength
  if arg_present(flux) && ptr_valid(self.flux) then flux=*self.flux
  if arg_present(unc) && ptr_valid(self.unc) then unc=*self.unc
  self->IRS_File_IO::GetProperty,_EXTRA=e
end


;=============================================================================
;  SaveInit - Normalize save file, and pull FITS status from extension.
;=============================================================================
pro IRS_Spectrum::SaveInit,sf
  sf=self->SaveFile(sf)
end

;=============================================================================
;  SaveSpectrum - Save the Spectrum to FITS or IPAC_TABLE
;=============================================================================
pro IRS_Spectrum::Save,sf
  self->SaveInit,sf
  if size(sf,/TYPE) ne 7 then return
  
  catch, err
  if err ne 0 then begin
     catch,/cancel
     self->Error,['Error saving spectrum to file '+sf,!ERROR_STATE.MSG]
  endif 
  
  
  self->AddPar,'FILENAME',filestrip(sf),' Name of this file'
  
  if obj_valid(self.region) then $
     self.region->WriteRegion,*self.hdr,sf,IPAC_TBL=~self.fits
  
;   if n_elements(ra) eq 0 && n_elements(box) gt 0 then begin 
;      ;; Aperture Extraction coordinates
;      delta=box[*,1]-box[*,0]+1
;      ll=box[*,0]
;      lr=[box[0,1],box[1,0]]+[1,0]
;      ur=box[*,1]+1
;      ul=[box[0,0],box[1,1]]+[0,1]
;      all=[ [ll], [lr], [ur], [ul] ]
;      self->ConvertCoords,ra,dec,all[0,*],all[1,*],/TO_RA_DEC
;  endif 
  
  if self.fits then begin 
;      fxhmake,hdr,/date,/EXTEND
     
;      fxaddpar,hdr,'FILENAME',filestrip(sf),' Name of this file'
     
;      if n_elements(ra) ne 0 then begin 
;         fxaddpar,hdr,'RA_LL',ra[0],' RA of LL of extraction box'
;         fxaddpar,hdr,'DEC_LL',dec[0],' DEC of LL of extraction box'
;         fxaddpar,hdr,'RA_LR',ra[1],' RA of LR of extraction box'
;         fxaddpar,hdr,'DEC_LR',dec[1],' DEC of LR of extraction box'
;         fxaddpar,hdr,'RA_UR',ra[2],' RA of UR of extraction box'
;         fxaddpar,hdr,'DEC_UR',dec[2],' DEC of UR of extraction box'
;         fxaddpar,hdr,'RA_UL',ra[3],' RA of UL of extraction box'
;         fxaddpar,hdr,'DEC_UL',dec[3],' DEC of UL of extraction box'
;      endif 
     
;      fxwrite,sf,hdr
     
;      ;; Make the binary table
;      fxbhmake,hdr,1,'SCI','Extracted spectrum'
;      fxbaddcol,wcol,hdr,*self.WAVELENGTH,'WAVELENGTH','Column label field 1', $
;                TUNIT='Microns'
;      fxbaddcol,fcol,hdr,*self.WAVELENGTH,'FLUX','Column label field 2', $
;                TUNIT=self.fluxcon?'Jy/pixel':'e/s/pixel'
;      fxbcreate,unit,sf,hdr
;      fxbwrite,unit,*self.WAVELENGTH,wcol,1
;      fxbwrite,unit,sp,fcol,1
;     fxbfinish,unit
  endif else begin 
     base=create_struct('Wavelength',0.0,'Flux',0.0)
     units=[self.wave_units,self.flux_units]
     if ptr_valid(self.unc) then begin 
        base=create_struct(base,'Flux_Uncertainty',0.0)
        units=[units,self.flux_units]
     endif 
     
     if ptr_valid(self.extra_data) then begin 
        for i=0,n_elements(*self.extra_data)-1 do begin 
           base=create_struct(base,(*self.extra_data)[i].colname, $
                              (*(*self.extra_data)[i].data)[0])
           units=[units,(*self.extra_data)[i].units]
        endfor
     endif
     
     data=replicate(base,n_elements(*self.wavelength))
     data.wavelength=*self.wavelength
     data.flux=*self.flux
     off=2
     if ptr_valid(self.unc) then begin 
        data.flux_uncertainty=*self.unc
        off++
     endif 
     
     if ptr_valid(self.extra_data) then $
        for i=0,n_elements(*self.extra_data)-1 do $
           data.(off+i)=*(*self.extra_data)[i].data
     
     write_ipac_table,sf,DATA=data,HEADER=*self.HDR,UNITS=units
  endelse 
end


;=============================================================================
;  Read - Read a spectrum file, including region data, etc.
;=============================================================================
pro IRS_Spectrum::Read,file
  file=self->ReadFile(file)
  if size(file,/TYPE) ne 7 then return

  catch, err
  if err ne 0 then begin
     catch,/cancel
     self->Error,['Error reading spectrum from file '+file,!ERROR_STATE.MSG]
  endif 
  widget_control,/HOURGLASS  

  if self.fits then begin 
     ;; XXX
  endif else begin 
     st=read_ipac_table(file,hdr,UNITS=units)
     
     if ~obj_valid(self.region) then self.region=obj_new('IRS_Region')
     self.region->ParseRegion,hdr,file,/IPAC_TABLE
     
     ptr_free,self.hdr
     self.hdr=ptr_new(hdr,/NO_COPY)
     
     ptr_free,self.wavelength,self.flux
     self.wavelength=ptr_new(st.wavelength)
     self.flux=ptr_new(st.flux)
     self.wave_units=units[0]
     self.flux_units=units[1]
     
     names=tag_names(st)
     off=2
     if n_elements(names) gt 2 && strupcase(names[2]) eq 'FLUX_UNCERTAINTY' $
     then begin 
        self.unc=ptr_new(st.flux_uncertainty)
        off++
     endif 
     
     heap_free,self.extra_data
     for i=off,n_elements(names)-1 do $
        self->AddDataColumn,names[i],units[i],st.(i)
  endelse 
end


;=============================================================================
;  Cleanup
;=============================================================================
pro IRS_Spectrum::Cleanup
  ptr_free,self.wavelength,self.flux,self.unc
  heap_free,self.extra_data
  if obj_valid(self.region) then obj_destroy,self.region
  self->IRS_File_IO::Cleanup
end


;=============================================================================
;  Init
;=============================================================================
function IRS_Spectrum::Init,wavelength,flux,error, WAVELENGTH_UNITS=wu,$
                            _EXTRA=e
  if (self->IRS_File_IO::Init(_EXTRA=e) ne 1) then return,0 ;chain up
  ;; Append to default file_base
  if self.file_base then self.file_base+='_sp'
  self.file_title='Spectrum'
  if n_elements(wu) eq 0 then self.wave_units='um' else self.wave_units=wu
  
  if n_elements(wavelength) ne 0 then self.wavelength=ptr_new(wavelength)
  if n_elements(flux) ne 0 then self.flux=ptr_new(flux)
  if n_elements(error) ne 0 then self.error=ptr_new(error)
  
  return,1
end


pro IRS_Spectrum__define
  sp={IRS_Spectrum, $
      INHERITS IRS_File_IO, $   
      wavelength: ptr_new(), $  ;wavelength
      wave_units: '', $         ;units for wavelength
      flux: ptr_new(), $        ;the spectrum
      unc: ptr_new(), $         ;the spectrum uncertainty
      extra_data: ptr_new(), $  ;list of IRS_Spectrum_EXTRA_Data structures
      region: obj_new() }       ;region object, if any regions attached
  
  st={IRS_Spectrum_EXTRA_Data, $
      colname:'', $
      units:'', $
      data:ptr_new()}
end
