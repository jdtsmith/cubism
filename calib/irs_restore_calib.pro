function sm_restore_calib, cfile
;  @smart_dir
  smart_calib_dir=!smart_dir + '/calib'
  file=filepath(ROOT=smart_calib_dir,SUBDIR=["data","sets"],cfile)
  if file_test(file,/READ,/REGULAR) eq 0 then $
     message,'No such calibration object file: '+cfile
  return,restore_object(file,'SMART_Calib')
end
