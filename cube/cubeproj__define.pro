;=============================================================================
;  WriteFits - Write the Cube, Error, etc. to FITS file
;=============================================================================
pro cubeProj::WriteFits,file
  if NOT ptr_valid(self.CUBE) then return 
  if NOT ptr_valid(self.WAVELENGTH) then return
  fxhmake,hdr,*self.CUBE,/extend,/date
  
  ;; Celestial coordinates
  RADEG = 180.0d/!DPI           ; preserve double
  fxaddpar,hdr,'EQUINOX', 2000.0,   ' Equinox of reference coordinate'
  fxaddpar,hdr,'CTYPE1','RA---TAN', ' RA in tangent plane projection'
  fxaddpar,hdr,'CTYPE2','DEC--TAN', ' DEC in tangent plane projection'
  fxaddpar,hdr,'CRPIX1',(self.CUBE_SIZE[0]+1.)/2, $
           ' Pixel x coordinate at reference point'
  fxaddpar,hdr,'CRPIX2',(self.CUBE_SIZE[1]+1.)/2, $
           ' Pixel y coordinate at reference point'
  fxaddpar,hdr,'CRVAL1',self.POSITION[0],' [deg] RA at reference point'
  fxaddpar,hdr,'CRVAL2',self.POSITION[1],' [deg] DEC at reference point'
  
  ;; Old style coordinate transform, for compatibility
  fxaddpar,hdr,'CROTA2',self.PA,' [deg] Rotation angle'
  fxaddpar,hdr,'CDELT1',self.PLATE_SCALE, $
           ' [deg/pix] Plate scale, coordinate 1'
  fxaddpar,hdr,'CDELT2',self.PLATE_SCALE, $
           ' [deg/pix] Plate scale, coordinate 2'
  
  ;; New style coordinate transform
  fxaddpar,hdr,'CD1_1' ,-cos(self.PA/RADEG)*self.PLATE_SCALE, $
           ' Transformation matrix element'
  fxaddpar,hdr,'CD1_2' ,-sin(self.PA/RADEG)*self.PLATE_SCALE, $
           ' Transformation matrix element'
  fxaddpar,hdr,'CD2_1' ,-sin(self.PA/RADEG)*self.PLATE_SCALE, $
           ' Transformation matrix element'
  fxaddpar,hdr,'CD2_2' , cos(self.PA/RADEG)*self.PLATE_SCALE, $
           ' Transformation matrix element'
  
  ;; Wavelength coordinates and wavelength LUT binary table extension.
  fxaddpar,hdr,'CTYPE3','WAVE-TAB','Wavelength'
  fxaddpar,hdr,'CUNIT3','um','Wavelength units'
  fxaddpar,hdr,'PS3_0','WCS-TAB','Coordinate table extension name'
  fxaddpar,hdr,'PS3_1','WAVELENGTH','Coordinate table column name'
  
  ;; Write the primary header and data
  fxwrite,file,hdr,*self.CUBE
;  fxwrite,file,hdr,*self.ERR
  
  ;; Make the wavelength LUT extension header
  fxbhmake,hdr,1,'WCS-TAB','Wavelength look-up table for cube dimension 3'
  fxbaddcol,wcol,hdr,*self.WAVELENGTH,'WAVELENGTH','Column label'
  fxbcreate,unit,file,hdr
  fxbwrite,unit,*self.WAVELENGTH,wcol,1
end

;=============================================================================
;  Print - Display the cube's contents
;=============================================================================
pro cubeProj::Print
  print,FORMAT='(I0,"x",I0," = ",F6.3," x ",F6.3," arcsec")',self.NSTEP, $
        self.STEP_SIZE*3600*self.NSTEP
  if NOT ptr_valid(self.DR) then begin
     print,'No data'
     return
  endif 
  print,'DATA:'
  for i=0,n_elements(*self.DR)-1 do begin 
     dr=(*self.DR)[i]
     print,dr.id,":",dr.file
     sign=dr.CMD_POS[1] ge 0.0?'+':'-'
     radec,dr.CMD_POS[0],abs(dr.CMD_POS[1]),rh,rm,rs,dd,dm,ds
     ra=string(format='(I0,"h",I2.2,"m",F5.2,"s")',rh,rm,rs)
     dec=sign+string(format='(I0,"d",I2.2,"m",F5.2,"s")',abs(dd),dm,ds)
     print,FORMAT='("  STEP: ",I2.2," (",I0,",",I0,") RA: ",A0," DEC: ",A0)', $
           dr.STEP,dr.ROW,dr.COLUMN, ra,dec
  endfor 
end

;=============================================================================
;  SetProperty - Set various cube properties.  Most of these should be
;                automatically discovered from the BCD's.
;=============================================================================
pro cubeProj::SetProperty,PLATE_SCALE=ps,NSTEP=nstep,STEP_SIZE=stepsz, $
                          MODULE=md,ORDER=ord,SLIT_SIZE=slitsz, $
                          SLIT_LENGTH=slitl, SLIT_WIDTH=slitw, $
                          CAL_FILE=cal_file,CAL_OBJECT=cal
  if n_elements(ps) ne 0 then self.PLATE_SCALE=ps
  if n_elements(nstep) ne 0 then self.NSTEP=nstep
  if n_elements(stepsz) ne 0 then self.STEP_SIZE=stepsz
  if n_elements(md) ne 0 then self.MODULE=md
  if n_elements(ord) ne 0 then self.ORDER=ord
  if n_elements(slitsz) eq 2 then self.SLIT_SIZE=slitsz else begin 
     if n_elements(slitl) ne 0 then self.SLIT_SIZE[0]=slitl
     if n_elements(slitw) ne 0 then self.SLIT_SIZE[1]=slitw
  endelse
  if n_elements(cal_file) ne 0 then self.cal_file=cal_file
  if n_elements(cal) ne 0 then begin 
     if obj_isa(cal,'IRS_Calib') then self.cal=cal else $
        message,'Calibration object not of correct type.'
  endif 
  
end

;=============================================================================
;  GetProperty 
;=============================================================================
pro cubeProj::GetProperty, ACCOUNT=account
  if arg_present(account) then account=self.account
end

;=============================================================================
;  Cube 
;=============================================================================
function cubeProj::Cube,ERROR=err
  if arg_present(err) then err=*self.ERR
  return,*self.CUBE
end

;=============================================================================
;  LoadCalib - Ensure the calibration object is loaded and available.
;=============================================================================
pro cubeProj::LoadCalib
  if obj_isa(self.cal,'IRS_Calib') then return
  if n_elements(self.cal) ne 0 then $
     message, "Missing or destroyed calibration object."
  if self.cal_file eq '' then message,'Error: no cal file specified.'
  self.cal=sm_restore_calib(self.cal_file)
end

;=============================================================================
;  CheckSteps - Ensure that all of the BCD's for this map are present.
;=============================================================================
function cubeProj::CheckSteps
  got=bytarr(self.NSTEP)
  got[(*self.DR).COLUMN-1,(*self.DR).ROW-1]=1b
  wh=where(got eq 0b, cnt)
  if cnt eq 0 then return,1
  message,'Missing Steps: '+ $
          string(FORMAT='('+strtrim(cnt,2)+'("[",I0,", ",I0,"]"))', $
                 [wh/self.NSTEP[0]+1,wh mod self.NSTEP[0]+1])
  return,0
end

;=============================================================================
;  Normalize - Map header info in the BCD's to cube-specific data, and
;              returns the status of the normalization.
;=============================================================================
function cubeProj::Normalize
  self->LoadCalib
  if NOT ptr_valid(self.DR) then return,0
  
  ;; Normalize the module type
;  modules=strarr(n_elements(*self.DR))
;  for i=0,n_elements(*self.DR)-1 do begin 
;     ;; FIXME
;     modules[i]=sxpar(*(*self.DR)[i].HEADER,'APERNAME',COUNT=cnt) 
;     if cnt eq 0 then message,"No APERNAME keyword present"
;  endfor 
;  u=uniq(modules,sort(modules))
;  if n_elements(u) ne 1 then $
;     message,string(FORMAT='("Too many module types: ",A0, : ",")',modules[u])
;  self.module=modules[u[0]]
  
  ;; Normalize slit width
  if self.SLIT_SIZE[0] eq 0.0 then begin 
     if self.ORDER gt 0 then begin 
        self.cal->GetProperty,self.module,self.order,SLIT_LENGTH=sl
        self.SLIT_SIZE[0]=sl*self.PLATE_SCALE
     endif else begin ;; find the longest slit and use it
        ords=self.cal->Orders(self.module)
        slmax=0.
        for i=0,n_elements(ords)-1 do begin 
           self.cal->GetProperty,self.module,ords[i],SLIT_LENGTH=sl
           slmax=sl>slmax
        endfor 
        self.SLIT_SIZE[0]=slmax*self.PLATE_SCALE
     endelse 
  endif 
  
  ;; Normalize the position angle
  pa=0.0D
  for i=0L,n_elements(*self.DR)-1 do pa=pa+(*self.DR)[i].PA
  self.PA=pa/n_elements(*self.DR)
  
  ;; Normalize sub-slit (if any)
  return,1
end


;=============================================================================
;  MergeAccounts - Merge overlapping-in-wavelength order accounts
;                  together by developing an interpolated wavelength
;                  sampling in the region of overlap, and splitting
;                  with a linear interpolation the area of overlap for
;                  pixels in account planes which overlap according to
;                  the wavelength.
;=============================================================================
pro cubeProj::MergeAccounts,account,wavelength
  
  ;;; FIXME: Should a cube be mergeable with any other cube, e.g. SL, SH?
  if self.ORDER ne 0 then begin ;; only one order, please
     ptr_free,self.ACCOUNT
     ptr_free,self.WAVELENGTH
  endif 
  if NOT ptr_valid(self.ACCOUNT) then begin 
     self.ACCOUNT=ptr_new(account,/NO_COPY)
     self.WAVELENGTH=ptr_new(wavelength)
     return
  endif 
  
  ;; Merge in an existing account.
  min_wav=min(*self.WAVELENGTH,max=max_wav)
  min_new_wav=min(wavlength,max=max_new_wav)
  
  ;; No overlap case... just concatenate
  if min_new_wav gt max_wav OR max_new_wav lt min_wav then begin 
     if min_new_wav lt min_wav then begin ; prepend new
        *self.WAVELENGTH=[wavelength,*self.WAVELENGTH]
        *self.ACCOUNT=[ [[account]], [[*self.ACCOUNT]] ]
     endif else begin           ; append new
        *self.WAVELENGTH=[*self.WAVELENGTH,wavelength]
        *self.ACCOUNT=[ [[*self.ACCOUNT]], [[account]] ]
     endelse 
     return
  endif 
  
  ;; Find an interpolating set of wavelengths.  
  
end

;=============================================================================
;  BuildAccount - Build the accounting cube, listing, for all pixels
;                 in the cube, all overlapping data pixels, including
;                 the BCD index #, the pixel in that BCD which
;                 overlapped, and fraction which overlapped the
;                 corresponding cube pixel.
;=============================================================================
pro cubeProj::BuildAccount
  if self->Normalize() eq 0 then return
  if self->CheckSteps() eq 0 then return
  
  ;; Establish the dimensions of the cube in the slit coordinate system
  self.CUBE_SIZE[0:1]=ceil(((self.NSTEP-1)*self.STEP_SIZE+self.SLIT_SIZE)/ $
                           self.PLATE_SCALE)+1
  
  steps=self.STEP_SIZE/self.PLATE_SCALE ; slit step size, in pixels
  
  ;; Find the Wavelength dimension of the cube
  ;; !!!! HANDLE MULTIPLE SLITS, e.g. SH!
  
  self->LoadCalib               ;ensure the calibration object is available
  
  if self.ORDER eq 0 then ords=self.cal->Orders(self.MODULE) else $
     ords=self.ORDER
  for ord=0,n_elements(ords)-1 do begin 
     prs=self.cal->GetWAVSAMP(self.MODULE,ords[ord],/PIXEL_BASED, $
                              /SAVE_POLYGONS)

     self.CUBE_SIZE[2]=n_elements(prs) 
     account=ptrarr(self.CUBE_SIZE)
     
     for i=0L,n_elements(*self.DR)-1 do begin 
        ;; Compute the pixel offset of the slit center for this BCD
        offset=([(*self.DR)[i].COLUMN,(*self.DR)[i].ROW]-1)*steps+ $
               self.SLIT_SIZE/self.PLATE_SCALE/2.
        ;; Difference (small) between PA of this observation and the mean PA
        pa_delta=((*self.DR)[i].PR-self.PR)
        
        for j=0L,self.CUBE_SIZE[2]-1 do begin ; iterate over all the PRs
           ;; Setup the rotation matrix to rotate back to the +x direction
           angle=prs[j].angle+pa_delta
           if angle ne 0.0D then begin 
              ct=cos(angle/!radeg) & st=sin(angle/!radeg)
              rot=transpose([[ct,-st], $
                             [st, ct]])
           endif
           for k=0L,n_elements(*prs[j].POLYGONS)-1 do begin 
              bcdpixel=(*prs[j].PIXELS)[k] 
           
              ;; associated polygon (2xn list) this pixel got clipped to
              ;; on detector
              poly=*(*prs[j].POLYGONS)[k]
           
              ;; Offset to poly center
              poly=poly-rebin(prs[j].cen,size(poly,/DIMENSIONS))
           
              ;; Rotate this polygon to the cube sky grid, if necessary
              if angle ne 0.0 then poly=rot#poly
           
              ;; Offset the polygon correctly onto the sky grid
              poly=poly+rebin(offset,size(poly,/DIMENSIONS))

              ;; Clip it against the sky grid
              cube_pixels=polyfillaa(reform(poly[0,*]),reform(poly[1,*]), $
                                     self.CUBE_SIZE[0],self.CUBE_SIZE[1], $
                                     AREAS=areas)
           
              if cube_pixels[0] eq -1 then begin
                 print,FORMAT='("not hitting cube for pixel: "' + $
                       ',I0," [",I0,",",I0,"]")', bcdpixel, $
                       (*self.DR)[i].COLUMN,(*self.DR)[i].ROW
                 print, poly
                 continue ;; why isn't our cube big enough?
              endif 
              cube_ind=j*self.CUBE_SIZE[0]*self.CUBE_SIZE[1]+cube_pixels
           
              for l=0,n_elements(cube_ind)-1 do begin 
                 acc={CUBE_ACCOUNT,DR:i,PIXEL:bcdpixel,FRAC:areas[l]}
                 if ptr_valid(account[cube_ind[l]]) then $
                    *account[cube_ind[l]]=[*account[cube_ind[l]], $
                                           temporary(acc)] $
                 else account[cube_ind[l]]=ptr_new(acc,/NO_COPY)
              endfor 
           endfor 
        endfor 
     endfor
     ;; Merge this account with any existing one
     self->MergeAccounts,account,prs.lambda
  endfor 
  
  
  self.CUBE_SIZE[2]=n_elements(prs)   
;  ptr_free,self.ACCOUNT
;  self.ACCOUNT=ptr_new(account,/NO_COPY)
end

;=============================================================================
;  BuildCube - Assemble the Cube from the accounting information, and
;              the BCD data itself.
;=============================================================================
pro cubeProj::BuildCube
  if NOT ptr_valid(self.DR) then return
  if NOT ptr_valid(self.ACCOUNT) OR self.Changed then self->BuildAccount
  
  cube=make_array(self.CUBE_SIZE,/FLOAT,VALUE=!VALUES.F_NAN)
  areas=make_array(self.CUBE_SIZE,/FLOAT,VALUE=1.)
  ncube=n_elements(*self.ACCOUNT)
  for i=0L,ncube-1L do begin 
     if i mod 1000 eq 0 then print, FORMAT='(F8.3,"%")',float(i)/ncube*100
     if NOT ptr_valid((*self.ACCOUNT)[i]) then continue
     als=*(*self.ACCOUNT)[i]    ;list of {CUBE_ACCOUNT} structures
     
     ;;
     if n_elements(als) eq 1 then begin 
        cube[i]=((*self.DR)[als.dr].BCD)[als.pixel]*als.frac 
     endif else begin 
        cube[i]=0.0
        h=histogram(als.dr,BINSIZE=1,REVERSE_INDICES=ri,OMIN=min)
        for j=0,n_elements(h)-1 do begin 
           if ri[j] eq ri[j+1] then continue ;empty bin
           inds=ri[ri[j]:ri[j+1]-1]
           cube[i]=cube[i]+ $
                   total(((*self.DR)[min+j].BCD)[als[inds].pixel] * $
                         als[inds].frac)
        endfor
     endelse 
     ;; ===========> below takes around 63 seconds!!!
;     endif else if array_equal(als.dr,als[0].dr) then begin  
;        ;; all the same BCD.
;        cube[i]=total(((*self.DR)[als[0].dr].BCD)[als.pixel] * als.frac) 
;     endif else begin 
;        stop
        ;;different pixels from different BCDs
;        cube[i]=total(((*self.DR)[als.dr].BCD)[als.pixel+ $
;                                               lindgen(n_elements(als))* $
;                                               128L*128L*2L] * als.frac)
;     endelse 
     ;; ===========> 
     areas[i]=total(als.frac)
     ;; error cube computation
  endfor 
  ptr_free,self.CUBE,self.ERR,self.ITIME ;just in case  
  self.CUBE=ptr_new(cube/areas,/NO_COPY)
end

;=============================================================================
;  AddData - Add BCD data to the cube.
;=============================================================================
pro cubeProj::AddData, bcd, header,ERROR=error, STEP=step,COLUMN=col, $
                       ROW=row, CMD_POS=cpos, REC_POS=rpos, PA=pa,FILE=file, $
                       ID=id
  
  if n_elements(bcd) eq 0 AND n_elements(file) ne 0 then $
     bcd=readfits(file,header)
  
  s=size(bcd,/DIMENSIONS)
  rec={CUBE_DR}
  if n_elements(s) eq 3 then begin 
     if array_equal(s,[128,128,2]) then begin 
        rec.BCD=bcd
     endif else message,'Incorrect BCD dimensions'
  endif else if n_elements(s) eq 2 then begin 
     if array_equal(s,[128,128]) then begin 
        if array_equal(size(error,/DIMENSIONS),[128,128]) then $
           rec.BCD=[ [[bcd]], [[error]] ] else $
           rec.BCD=[ [[bcd]], [[make_array(/FLOAT,128,128,VALUE=1.)]] ]
     endif else message,"BCD's must be 128x128[x2]"
  endif else message,"BCD's must be 128x128[x2]"
  
  if size(header,/TYPE) ne 7 then message,'Header must be a string array'
  rec.header=ptr_new(header)
  
  if n_elements(file) ne 0 then rec.file=file
  if n_elements(id) ne 0 then rec.id=id else if rec.file then begin 
     id=strmid(rec.file,strpos(rec.file,path_sep(),/REVERSE_SEARCH)+1)
     id=strmid(id,0,strpos(id,".fits",/REVERSE_SEARCH))
     rec.id=id
  endif 
  
  ;; !!!!!! FIXME: These keywords are by no means finalized
  if n_elements(step) ne 0 then rec.STEP=step else $
     rec.STEP=sxpar(header,'EXPNUM')
  if n_elements(col) ne 0 then rec.COLUMN=col else $
     rec.COLUMN=sxpar(header,'COLUMN')
  if n_elements(row) ne 0 then rec.ROW=row else $
     rec.ROW=sxpar(header,'ROW')
  if n_elements(cpos) eq 2 then rec.CMD_POS=cpos else $
     rec.CMD_POS=[sxpar(header,'RA_CMD'),sxpar(header,'DEC_CMD')]
  if n_elements(rpos) eq 3 then rec.REC_POS=rpos else $
     rec.REC_POS=sxpar(header,'CRVAL*')
  if n_elements(pa) ne 0 then rec.PA=pa else $
     rec.PA=sxpar(header,'ORIENTAT')
  if array_equal(self.slit_size,0.0) then $
     self.slit_size=sxpar(header,'SLIT_SZ*')     
  
  ;; !!!!!! FIXME: These keywords are by no means finalized
  if ptr_valid(self.DR) then *self.DR=[*self.DR,rec] else $
     self.DR=ptr_new(rec,/NO_COPY)
end

;=============================================================================
; Cleanup 
;=============================================================================
pro cubeProj::Cleanup
  ;; !!! Write a real cleanup.
  heap_free,self.DR
  heap_free,self.ACCOUNT
end

;=============================================================================
; Init 
;=============================================================================
function cubeProj::Init, name, _EXTRA=e
  if (self->ObjMsg::Init(_EXTRA=e) ne 1) then return,0 ;chain up (add msglist)
  self.cur=-1                   ;none yet set current
  self.Changed=0b               ;coming into existence doesn't count
  self.SpaceSaver=1b            ;use spacesaver by default
  if n_elements(e) ne 0 then self->SetProperty,_EXTRA=e
  return,1
end

;=============================================================================
; CubeProj - IRS Spectral (+MIPS SED) Cubes
;=============================================================================
pro cubeProj__define
  c={cubeProj, $
     INHERITS ObjMsg, $         ;make it an object messanger
     ProjectName:'', $          ;the name of the current project 
     MODULE:'', $               ;The name of the module, one of
                                ;   SL,LL,SH,LH (IRS),MSED (MIPS)
     ORDER:0, $                 ;The sub-slit order for which to build
                                ; the cube (or 0 to build and splice all
                                ; orders in the module)
     DR: ptr_new(), $           ;All the BCD's: pointer to list of
                                ; data record structures of type CUBE_DR
     ACCOUNT: ptr_new(),$       ;a cube of CUBE_ACCOUNT accounting structures
     CUBE: ptr_new(),$          ;a pointer to the nxmxl data cube
     ERR:  ptr_new(),$          ;a pointer to the nxmxl error cube
     ITIME: ptr_new(),$         ;nxmxl integration time per pixel
     CUBE_SIZE: [0,0,0], $      ;the size of the cube, (n,m,l)
     CUBE_DATE: 0.0D, $         ;date the cube was assembled (JULIAN)
     NSTEP:[0,0], $             ;parallel (col), perpen. (row) number of steps
     STEP_SIZE: [0.0D,0.0D], $  ;parallel, perpendicular slit step sizes (deg)
     SLIT_SIZE:[0.0D,0.0D], $   ;the parallel, perpendicular slit size (deg)
     PLATE_SCALE:0.0D, $        ;the plate scale (degrees/pixel)
     WAVELENGTH: ptr_new(), $   ;the cube's wavelength list
     POSITION:[0.0D,0.0D], $    ;optimized position of the cube center
     PA:0.0D, $                 ;optimized position angle of the cube
     cur:0, $                   ;the current record.
     sort:0, $                  ;which project field to sort on
     cal:obj_new(), $           ;the irs_calib object.
     cal_file:'', $             ;the calibration file used (if not a full
                                ; directory, in the "calib/" subdir)
     Changed:0b, $              ;if the project is changed since last saved.
     SpaceSaver:0b,  $          ;whether to use space saver -- omits saving raw
     SaveFile:'', $             ;the file it was saved to
     wInfo:ptr_new()}           ;the widget info struct.... a diconnectable ptr
  
  st={CUBE_ACCOUNT, $
      DR: 0L, $                 ;index into the DR list
      pixel: 0L, $              ;which pixel in the BCD overlapped
      frac: 0.0}                ;the fraction of overlap
  
  ;; The data structure for each input BCD
  st={CUBE_DR, $
      ID:'',$                   ;A unique (hopefully) ID
      file:'', $                ;the original file read for this data
      CMD_POS: [0.0D,0.0D], $   ;Commanded RA,DEC position of slit center
      REC_POS: [0.0D,0.0D],$    ;Reconstructed RA,DEC pos of slit center.
      PA: 0.0D, $               ;Position angle of slit E of N
      BCD: fltarr(128,128,2), $ ;the 2 plane BCD (error plane second).
      STEP: 0, $                ;the step number in the mapping sequence
      COLUMN: 0,$               ;the step number perpendicular to the slit
      ROW:0, $                  ;the step number parallel to the slit
      Date:0.0D, $              ;the date this BCD was added
      HEADER: ptr_new()}        ;a pointer to a string array.
end



;;; NOTES:
;;; We can create an objerror (or objinfo) superclass that handles
;;; displaying errors (and/or info), either in the console without a
;;; GUI, or in a popup with a GUI.  Sometimes we want to error and
;;; continue, sometimes error and cancel.  Sometimes the error should
;;; affect a calling routine, sometimes not... hmm....
;;;    
