;+
; NAME:  
;
;    RESTORE_OBJECT
;
; DESCRIPTION:
;    
;    Restores an object from file, with care taken to ensure that the
;    object's class methods are available, and that older versions of
;    the class don't shadow the current one.
;    
; CATEGORY:
;
;    Object Restoration.
;    	
; CALLING SEQUENCE:
;
;    obj=restore_object(file,class,[OTHER_CLASSES=,VAR_NAME=])
;
; INPUT PARAMETERS:
;
;    file: The file name for the IDL SAVE file which contains a single
;       object to be restored.
;       
;    class: The class name of the object to be restored.
;			
; INPUT KEYWORD PARAMETERS:
;
;    OTHER_CLASSES: List other classes to pre-resolve.  Only necessary
;       if the object to be restored contains somewhere in its data
;       hierarchy other objects of different classes whose class
;       definitions will be saved and restored in the same way.
;
;    VAR_NAME: The string name of a variable saved in the object.  If
;       passed, the object saved with this variable name (e.g. "self")
;       will be returned, in preference to any others.
;       
; OUTPUTS:
;
;    obj: The restored object.
;
; NOTES:
;  
;    An interesting chicken-and-egg dilemma presents itself when
;    objects are restored from IDL SAVE file: these objects contain
;    implicit definitions of their classes, so that, if restored
;    before the class is defined in the IDL session, the older
;    definition will supercede the more recent.  What's more, since
;    IDL already knows about the class, it will see no need to seek
;    out and compile "class__define.pro", which is where all class
;    methods are typically located.  This will lead to many "Unknown
;    Method" errors.
;
;    The solution is to pre-resolve the class and all superclasses on
;    its inheritance tree before restoration.  This is implemented
;    here.  See http://www.dfanning.com/tips/saved_objects.html for a
;    more in-depth disucussion of these issues.
;
;    Note that any structures or objects contained in a saved object
;    will also be subject to this restore definition shadowing
;    problem.  If all auxiliary structures used by a class are defined
;    in the class__define procedure alongside the class, this problem
;    will also be circumvented using RESTORE_OBJECT.
;
;    Object which contain other object will be written and read from
;    the save file in the order they were created, not in hierarchical
;    order.  To ensure the correct object is returned, we check the
;    class.  However, if an object contains another object of the
;    *same* class (e.g. an IDL_Container containing another
;    IDL_Container somewhere), you need to resolve the ambiguity by
;    providing the variable name under which the topmost object was
;    saved (e.g. "self").  This is always safe, if more cumbersome.
;
; EXAMPLE:
;
;    obj=restore_object('/path/to/object.sav','MyClass')
;    ;; Get the object saved as "self", and pre-compile some helper classes.
;    obj2=restore_object('/path/to/object2.sav','ThisClass',VAR_NAME='self',$
;                        OTHER_CLASSES=['HelperClass','OtherHelperClass'])
;
; MODIFICATION HISTORY:
;
;    2002-09-04 (J.D. Smith): Added VAR_NAME, and test for correct class.
;    2002-08-18 (J.D. Smith): Wrapped class_define call in a catch.
;    2001-12-12 (J.D. Smith): Written.  Based directly on my routine
;       `resolve_obj' from SCOREX project code.  
;
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2001,2002 J.D. Smith
;
;  This file is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published
;  by the Free Software Foundation; either version 2, or (at your
;  option) any later version.
;  
;  This file is distributed in the hope that it will be useful, but
;  WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;  General Public License for more details.
;  
;  You should have received a copy of the GNU General Public License
;  along with this file; see the file COPYING.  If not, write to the
;  Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;  Boston, MA 02111-1307, USA.
;
;##############################################################################

;=============================================================================
;       restore_object_resolve - Resolve the entire class inheritance
;                                tree, starting with the class passed.
;=============================================================================
pro restore_object_resolve,class, routine_info
  if n_elements(routine_info) eq 0 then routine_info=routine_info()
  for i=0,n_elements(class)-1 do begin 
     defpro=class[i]+'__DEFINE'
     if (where(routine_info eq defpro))[0] eq -1 then begin
        ;; Compile and define the class in one swell foop.
        catch, err
        if err eq 0 then call_procedure,defpro else catch,/cancel
     endif 
     supers=obj_class(class[i],/SUPERCLASS,COUNT=cnt)
     if cnt gt 0 then restore_object_resolve,supers,routine_info
  endfor 
end

;=============================================================================
;       restore_object - Restore the object from file, resolving it's
;                        class and methods beforehand.
;=============================================================================
function restore_object,file,class, OTHER_CLASSES=oc, VAR_NAME=var
  restore_object_resolve,class, ri
  if n_elements(oc) ne 0 then restore_object_resolve,oc,ri
  restore,file,/RELAXED_STRUCTURE_ASSIGNMENT,RESTORED_OBJECTS=objs
  if n_elements(var) ne 0 then begin 
     tmp=execute('thisvar='+var)
     wh=where(objs eq thisvar,cnt)
     if cnt gt 0 then return,objs[wh[0]]
  endif 
  ;; Just find the first object of the correct class
  c=strupcase(class)
  for i=0,n_elements(objs)-1 do $
     if obj_class(objs[i]) eq c then return, objs[i]
  return,-1
end
