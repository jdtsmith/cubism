pro cubism,NEW=new
  project=obj_new('CubeProj')
  project->Open,PROJECT=opened_project,CANCEL_TEXT='Create New Cube Project'
  if NOT obj_valid(opened_project) then begin
     project->SetProperty,/SPAWNED 
     project->Show,/FORCE
     project->SetProjectName,TITLE='New Project Name'
  endif else obj_destroy,project
end
