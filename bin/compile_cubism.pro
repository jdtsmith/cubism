;+
; NAME:  
;
;    COMPILE_CUBISM
;
; CONTACT:
;
;    UPDATED VERSIONS of CUBISM and more information can be found at:
;       http://ssc.spitzer.caltech.edu/cubism
;
; DESCRIPTION:
;    
;    Compile cubism into an IDL SAV binary.
;
; CALLING SEQUENCE:
;
;    compile_cubism
;    
; MODIFICATION HISTORY:
;
;    2003-12-18 (J.D. Smith): Initially written.
;   
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2003-2005 J.D. Smith
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

pro compile_cubism
  FORWARD_FUNCTION TRNLOG
  @cubism_dir
  ps=path_sep()
  bindir=filepath(ROOT=cubism_dir,'bin')
  if file_test(filepath(ROOT=bindir,'cubism_vm.sav')) then $
     file_delete,filepath(ROOT=bindir,'cubism_vm.sav')
  
  ;; Go one level up and compile everything
  sourcepath=cubism_dir
  sourcepath=strmid(sourcepath,0,strpos(sourcepath,ps,/REVERSE_SEARCH))
  
  ;; Normalize path
  path=strsplit(!PATH,':',/EXTRACT)
  wh=where((sp=strpos(path,ps+'nasa',/REVERSE_SEARCH)) ne -1)
  if wh[0] eq -1 then message,'No NASALib found, aborting.'
  nasa=strmid(path[wh[0]],0,sp[wh[0]]+5)
  !PATH=expand_path('<IDL_DEFAULT>'+":+"+sourcepath+':+'+nasa)
  
  files=file_search(sourcepath,'*.pro')
  skip_files=['cubism_dir','cubism_version','compile_cubism', $
              'do_compile_cubism',ps+'scraps'+ps,'CVS'+ps]
  resolve_routine,'XManager',/COMPILE_FULL_FILE
  skip=0
  for i=0,n_elements(files)-1 do begin 
     for j=0,n_elements(skip_files)-1 do begin 
        if strpos(files[i],skip_files[j]) ne -1 then begin 
           print,'Skipping '+files[i]
           skip=1
           break
        endif
     endfor 
     if skip then begin 
        skip=0
        continue
     endif
     print,'Compiling '+files[i]
     routine=strmid(files[i],0,strpos(files[i],".pro",/REVERSE_SEARCH))
     routine=strmid(routine,strpos(files[i],ps,/REVERSE_SEARCH)+1)
     resolve_routine,routine,/EITHER,/NO_RECOMPILE,/COMPILE_FULL_FILE
  endfor 
  resolve_all,/CONTINUE_ON_ERROR
  save,/ROUTINES,FILENAME=filepath(ROOT=bindir,'cubism_vm.sav')
end
