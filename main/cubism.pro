;+
; NAME:  
;
;    CUBISM
;
; CONTACT:
;
;    UPDATED VERSIONS of CUBISM and more information can be found at:
;       http://sings.sirtf.edu/cubism
;
; DESCRIPTION:
;    
;    An simple entry routine for starting CUBISM.
;
; EXAMPLE:
;
;    cubism
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2002,2003 J.D. Smith
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
;  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;  02111-1307, USA.
;
;##############################################################################
pro cubism,pname,_EXTRA=e
  project=obj_new('CubeProj')
  project->Open,pname,PROJECT=opened_project, $
                CANCEL_TEXT='Create New Cube Project', $
                _EXTRA=e
  if NOT obj_valid(opened_project) then begin
     project->SetProperty,/SPAWNED 
     project->Show,/FORCE,/SET_NEW_PROJECTNAME
  endif else obj_destroy,project
end
