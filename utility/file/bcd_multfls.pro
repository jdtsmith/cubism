
;; Multfls with default string and LISTFUNC, and TITLE.
;; To be updated when real header keywords are available.  
function bcd_mult_showfits,file
  fshow=file
  pos=strpos(file,path_sep(),/REVERSE_SEARCH)
  if pos ne -1 then fshow=strmid(file,pos+1)
  catch, err
  if err ne 0 then $
     return,string(FORMAT='(A18,5X,A14)',fshow,'------------')
  hdr=headfits(file)
  ra=sxpar(hdr,'RA_HMS')
  dec=sxpar(hdr,'DEC_DMS')
  row=sxpar(hdr,'ROW')
  col=sxpar(hdr,'COLUMN')
  exp=sxpar(hdr,'EXPID')
  date=sxpar(hdr,'DATE_OBS')
  fov=irs_fov(sxpar(hdr,'FOVID'),/SHORT_NAME)
   
  return,string(FORMAT='(A20,1X,"[",A11,",",A10,"]",1X,A10,1X,I2,' + $
                '"[",I0,",",I0,"]",1X,A)',$
                fshow,ra,dec,date,exp,row,col,fov)
end

function bcd_multfls,_EXTRA=e
  filt=['*','*.fits']
  s=string(FORM='(T7,A,T24,A,T36,A,T48,A,T59,A,T67,A,T76)', $
           'File','RA','Dec','Date','Step','FOV')
  return, multfls(TITLE='Select BCDs',_EXTRA=e, $
                  LISTFUNC='bcd_mult_showfits', TOPLABEL=s,/NO_SHOW_ALL, $
                  FILTERLIST=filt,SELECT=1,/MULTIPLE,LIST_SIZE=19)
end
