
;;**************************OverRiding methods********************************

function IRSMapSet::ReportWidget
  if obj_valid(self.reporter) then return,self.reporter->ReportWidget() else $
     return,-1L
end

pro IRSMapSet::SetProperty,REPORTER=reporter,_EXTRA=e
  if obj_valid(reporter) then if obj_isa(reporter,'ObjReport') then $
     self.reporter=reporter
  self->ObjReport::SetProperty,_EXTRA=e
end

;;*************************End OverRiding methods******************************

function IRSMapSet::Names
  if NOT ptr_valid(self.map_sets) then return,-1
  return,(*self.map_sets).name
end


pro IRSMapSet::AddMap,name, WEIGHTS=weights, FORERANGES=fr, BACKRANGES=br, $
                      FORCE=force,CANCELED=cncld
  replace_cnt=0
  if ptr_valid(self.map_sets) then begin 
     replace_map=where((*self.map_sets).name eq name,replace_cnt)
     if NOT keyword_set(force) then begin 
        if replace_cnt gt 0 then begin 
           ans=dialog_message("Replace existing set "+name+"?", $
                              /QUESTION,TITLE="Replace "+name+"?", $
                              DIALOG_PARENT=self->ReportWidget())
           if ans eq 'No' then begin 
              cncld=1
              return
           endif 
        endif
     endif
  endif 
  
    ;; Add this map to the list, or replace an existing map
  if replace_cnt gt 0 then begin 
     ptr_free,(*self.map_sets)[replace_map[0]].foreranges, $
              (*self.map_sets)[replace_map[0]].backranges, $
              (*self.map_sets)[replace_map[0]].weights
     if (size(fr,/DIMENSIONS))[0] gt 0 then $
        (*self.map_sets)[replace_map[0]].foreranges=ptr_new(fr)
     if (size(br,/DIMENSIONS))[0] gt 0 then $
        (*self.map_sets)[replace_map[0]].backranges=ptr_new(br)
     if (size(weights,/DIMENSIONS))[0] gt 0 then $
        (*self.map_sets)[replace_map[0]].weights=ptr_new(weights)
     return
  endif else begin 
     ;; Just append it
     rec={IRS_MAPSET}
     rec.name=name
     if (size(fr,/DIMENSIONS))[0] gt 0 then rec.foreranges=ptr_new(fr)
     if (size(br,/DIMENSIONS))[0] gt 0 then rec.backranges=ptr_new(br)
     if (size(weights,/DIMENSIONS))[0] gt 0 then rec.weights=ptr_new(weights)
     if ptr_valid(self.map_sets) then $
        *self.map_sets=[*self.map_sets,rec] else $
        self.map_sets=ptr_new(rec,/NO_COPY)
  endelse
end

;=============================================================================
;  GetMap - Get map data by name.  Ouput are WEIGHTS, FORERANGES, and
;           BACKRANGES, if any (-1 otherwise).  If WAVELENGTH_CONVERT
;           is set, convert ranges and weights from lambda to
;           index-based using the passed wavelength, checking to make
;           sure the regions fall in the given wavelength domain.
;=============================================================================
pro IRSMapSet::GetMap,name,WEIGHTS=weights,FORERANGES=fr,BACKRANGES=br, $
                      WAVELENGTH_CONVERT=wc,NO_WEIGHT_CONVERT=nwc
  weights=(fr=(br=-1))
  if NOT ptr_valid(self.map_sets) then return
  wh=where(strlowcase((*self.map_sets).name) eq strlowcase(name),cnt)
  if cnt eq 0 then return
  map=(*self.map_sets)[wh[0]]
  if ptr_valid(map.weights) then begin 
     weights=*map.weights
     if n_elements(wc) ne 0 and keyword_set(nwc) eq 0 then begin 
        minlam=min(weights[0,*],MAX=maxlam)
        weights=interpol(weights[1,*],weights[0,*],wc)>0.
        wh=where(wc gt maxlam OR wc lt minlam,NCOMPLEMENT=good,cnt)
        if good eq 0 then $
           self->Error,map.name+': No overlap with current wavelengths.'
        if cnt gt 0 then weights[wh]=0.0 ;no extrapolation, please
     endif
  endif 
  
  if ptr_valid(map.foreranges) then begin 
     fr=*map.foreranges
     if n_elements(wc) ne 0 then begin 
        foreranges=make_array(/LONG,size(fr,/DIMENSIONS))
        for i=0,n_elements(fr)/2-1 do begin 
           void=min(abs(wc-fr[0,i]),low)
           void=min(abs(wc-fr[1,i]),high)
           foreranges[0,i]=[low,high]
        endfor
        fr=foreranges
        if array_equal(fr[1,*] gt fr[0,*],0b) then $
           self->Error,map.name+': No overlap with current wavelengths.'
     endif
  endif
     
  if ptr_valid(map.backranges) then begin
     br=*map.backranges
     if n_elements(wc) ne 0 then begin 
        backranges=make_array(/LONG,size(br,/DIMENSIONS))
        for i=0,n_elements(br)/2-1 do begin 
           void=min(abs(wc-br[0,i]),low)
           void=min(abs(wc-br[1,i]),high)
           backranges[0,i]=[low,high]
        endfor
        br=backranges
     endif
  endif

end

;=============================================================================
;  SaveMap - Save a named map set to file, and add it to the list.
;=============================================================================
pro IRSMapSet::SaveMap,name,WEIGHTS=weights,FORERANGES=fr,BACKRANGES=br, $
                       FILE=file,WAVELENGTH_CONVERT=wc,FORCE=force, $
                       CANCELED=cncld
  cncld=0
  if n_elements(file) eq 0 then begin 
     if n_elements(name) eq 0 then name='untitled'
     file=strlowcase(name)
     file=strjoin(strsplit(file,/EXTRACT),'_')+'.map' ;no spaces
     xf,file,/RECENT,FILTERLIST=['*.map','*.*','*'],/SAVEFILE, $
        TITLE='Save Map Set As...',/NO_SHOW_ALL,SELECT=0, $
        START=file,PARENT_GROUP=self->ReportWidget()
     if size(file,/TYPE) ne 7 then begin 
        cncld=1
        return
     endif 
     name=strmid(file,strpos(file,path_sep(),/REVERSE_SEARCH)+1)
     name=strmid(name,0,strpos(name,'.',/REVERSE_SEARCH))
  endif else if n_elements(name) eq 0 then begin 
     name=strmid(file,strpos(file,path_sep(),/REVERSE_SEARCH)+1)
     name=strmid(name,0,strpos(file,'.',/REVERSE_SEARCH))
  endif 
  
  openw,lun,file,/GET_LUN,ERROR=err
  if err then self->Error,["Cannot write file: "+file+':',!ERROR_STATE.MSG]

  printf,lun,name
  
  ;; Dereference pointers, if passed
  if size(fr,/TYPE) eq 10 then if ptr_valid(fr) then fr=*fr 
  if size(br,/TYPE) eq 10 then if ptr_valid(br) then br=*br 
  if size(weights,/TYPE) eq 10 then if ptr_valid(weights) then $
     weights=*weights 
  
  nf=n_elements(fr)/2
  nb=n_elements(br)/2
  nw=n_elements(weights)/2
  printf,lun,FORMAT='(3(I0,:," "))',nf,nb,nw
  if nf gt 0 then begin 
     if n_elements(wc) ne 0 then fr=wc[fr]
     printf,lun,fr
  endif 
  if nb gt 0 then begin 
     if n_elements(wc) ne 0 then br=wc[br]
     printf,lun,br
  endif
  if nw gt 0 then printf,lun,weights
  close,lun
  
  self->AddMap,name,FORERANGES=fr,BACKRANGES=br,WEIGHTS=weights,FORCE=force, $
               CANCELED=cncld

end

;=============================================================================
;  LoadSets - Load Map Sets from file
;=============================================================================
pro IRSMapSet::LoadSets,files, NAMES=names
  if n_elements(files) eq 0 then begin 
     xf,files,/RECENT,FILTERLIST=['*.map','*.*','*'],/MULTIPLE, $
        TITLE='Load Map Sets',/NO_SHOW_ALL,SELECT=0, $
        START=file,PARENT_GROUP=self->ReportWidget()
  endif 
  if size(files,/TYPE) ne 7 then return ;cancelled  
  cnt=n_elements(files) 
  
  names=strarr(cnt)
  for i=0,cnt-1 do begin
     fr=(br=(weights=0))
     self->ReadMapSetFile,files[i],FORERANGES=fr,BACKRANGES=br, $
                          WEIGHTS=weights, NAME=name
     names[i]=name
     self->AddMap,name,FORERANGES=fr,BACKRANGES=br,WEIGHTS=weights,/FORCE
  endfor
end

;=============================================================================
;  LoadDefaultSets - Load all map sets in the "map_sets" subdir.  An
;                    array of {IRS_MAPSET} structures is stored.
;=============================================================================
pro IRSMapSet::LoadDefaultSets
  @cubism_dir
  if ptr_valid(self.map_sets) then heap_free,self.map_sets
  files=file_search(filepath(ROOT=cubism_dir,SUBDIRECTORY='map_sets','*.map'),$
                    /TEST_READ,COUNT=cnt)
  if cnt eq 0 then return
  self->LoadSets,files
end

;=============================================================================
;  ReadMapSetFile - Read in a map file from cubism/map_sets, or
;                   elsewhere, if a full path is specified.  Map set
;                   files have the format:
;                      name
;                      NFore NBack NWeight
;                      fore_lam_1_low fore_lam_1_high
;                      ...
;                      fore_lam_NFore_low fore_lam_NFore_high
;                      back_lam_1_low back_lam_1_high
;                      ...
;                      back_lam_NBack_low back_lam_NBack_high;
;                      weight_lam_1 weight_1
;                      ...
;                      weight_lam_NWeight weight_NWeight
;=============================================================================
pro IRSMapSet::ReadMapSetFile,file,NAME=name,FORERANGES=fr,BACKRANGES=br, $
                             WEIGHTS=weights
  if NOT file_test(file,/READ) then begin 
     cdir=(routine_info('CubeProj__define',/SOURCE)).PATH
     cdir=strmid(cdir,0,strpos(cdir,path_sep(),/REVERSE_SEARCH))
     file=filepath(ROOT_DIR=cdir,SUBDIR='map_sets',file)
  endif 
  if NOT file_test(file,/READ) then self->Error,"No such file found: ",file
  nf=(nb=(nw=0))
  openr,unit,file,/GET_LUN
  name=''
  readf,unit,name
  readf,unit,nf,nb,nw
  if nf gt 0 then begin 
     fr=fltarr(2,nf,/NOZERO)
     readf,unit,fr
  endif 
  if nb gt 0 then begin 
     br=fltarr(2,nb,/NOZERO)
     readf,unit,br
  endif 
  if nw gt 0 then begin 
     weights=fltarr(2,nw,/NOZERO)
     readf,unit,weights
  endif 
  free_lun,unit
end

pro IRSMapSets::Cleanup
  heap_free,self.map_sets
end

function IRSMapSet::Init
  self->LoadDefaultSets
  self->SetProperty,TITLE_BASE='IRS Map Set'
  return,1
end

pro IRSMapSet__define
  st={IRSMapSet,$
      INHERITS ObjReport, $     ;make it a reporting object
      map_sets:ptr_new(), $
      reporter:obj_new()}       ;ObjReport object where we'll report errors
  ;; Map Set Structure
  mapset={IRS_MAPSET, $
          name:'', $            ;the set's name
          foreranges:ptr_new(), $ ;2xn set of foreground wavelength ranges
          backranges:ptr_new(), $ ;2xn set of background wavelength ranges
          weights:ptr_new()}    ; 2xn set of weights -- (lambda,weight) pairs
;         fit:0}                ; fit order to use (if any).
end

pro IRSMapSetDestroy
  common irs_map_set_object, msObj
  if obj_valid(msObj) then obj_destroy,msObj
end

;; The IRSMapSet object is a a singleton, only accesible through this
;; function.  Pass ObjReporter object REPORTER to control error reporting.
function IRSMapSet,reporter
  common irs_map_set_object, msObj
  need_obj=0
  if n_elements(msObj) eq 0 then need_obj=1 else $
     if obj_valid(msObj) eq 0 then need_obj=1
  if need_obj then msObj=obj_new('IRSMapSet')
  ;; Set or clear the reporter
  if obj_valid(reporter) then msObj->SetProperty,REPORTER=reporter
  return,msObj
end
