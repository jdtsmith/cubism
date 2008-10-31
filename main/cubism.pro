;+
; NAME:  
;
;    CUBISM
;
; CONTACT:
;
;    UPDATED VERSIONS of CUBISM and more information can be found at:
;       http://sings.stsci.edu/cubism
;
; DESCRIPTION:
;    
;    An simple entry routine for starting CUBISM with a selected or
;    specified project file.
;
; CALLING SEQUENCE:
;
;    cubism, [pname]
;
; OPTIONAL INPUT PARAMETERS:
;
;    pname: The path to a saved Cubism project to open.
;
; EXAMPLE:
;
;    cubism,'/path/to/myproj.cpj'
;    
; MODIFICATION HISTORY:
;
;    2006-01-04 (J.D. Smith): Create new projects for non-existent
;       project file.
;
;    2003-01-22 (J.D. Smith): Initially written.
;   
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2002,2003,2005 J.D. Smith
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
pro cubism,pname,PROJECT=project,_EXTRA=e
  project=obj_new('CubeProj')
  
  pname_passed=n_elements(pname) ne 0
  ;; Open a new project, unless a valid name was passed
  if ~pname_passed || file_test(pname,/READ) then $
     project->Open,pname,SUCCESS=success,PROJECT=opened_project, $
                   CANCEL_TEXT='Create New Cube Project', $
                   _EXTRA=e
  
  if ~keyword_set(success) then begin ;; didn't open any project
     ;; Use the pre-existing project, possibly with a passed name
     got_name=pname_passed && size(pname,/TYPE) eq 7
     if got_name then begin 
        ;; Non-existent file passed: create new project
        pname=file_expand_path(pname)
        if file_test(file_dirname(pname),/WRITE) then begin 
           pn=file_basename(pname,'.cpj')
           project->SetProperty,SAVE_FILE=pname,PROJECTNAME=pn
        endif 
     endif 
     project->SetProperty,/SPAWNED 
     project->Show,/FORCE,SET_NEW_PROJECTNAME=~got_name,_EXTRA=e
  endif else begin 
     obj_destroy,project
     project=opened_project
  endelse 
end
