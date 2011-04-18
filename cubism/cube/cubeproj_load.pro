;+
; NAME:  
;
;    cubeproj_load
;
; CONTACT:
;
;    UPDATED VERSIONS of CUBISM and more information can be found at:
;       http://sings.stsci.edu/cubism
;
; DESCRIPTION:
;    
;    Loads CUBISM Project files ('.cpj' files) from disk.
;    
; CATEGORY:
;
;    CUBISM Spectral Cube Reduction, Analysis and Processing.
;    	
; CALLING SEQUENCE:
;
;    cube_object=cubeproj_load(file)
;
; INPUT PARAMETERS:
;
;    file: The name of a '.cpj' Cubism Project file, or a FITS cube
;    (.fits) file written by CUBISM.
;
; OUTPUTS:
;
;   The recovered CubeProj object.
;
; EXAMPLE:
;
;    cube=cubism_load('/path/to/file.cpj')
;    
; MODIFICATION HISTORY:
;
;   2011-04-15 (J.D. Smith): Load FITS cubes as well.
;   2006-11-20 (J.D. Smith): Written.
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2006, 2011 J.D. Smith
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

function cubeproj_load,file
  if stregex(file,'.fits$',/BOOLEAN) then begin 
     cube=obj_new('CubeProj')
     cube->LoadCubeFromFITS,file
  endif else begin 
     cube=restore_object(file,'CubeProj', $
                         OTHER_CLASSES=['irs_aperture','irs_file_io', $
                                        'irs_spectrum','irs_map','irs_cube', $
                                        'cubeautobadpix'])
  endelse 
  
  if obj_valid(cube) then begin 
     if ~obj_isa(cube,'CubeProj') then $
        message,'Invalid Cube Project'
     cube->SetProperty,CHANGED=0b, $
                       SAVE_FILE=file_expand_path((file_info(file)).name)
     cube->Initialize
  endif      
  
  return,cube
end
