function irs_file_type,file,IS_BCD=is_bcd,IS_FLATAP=is_flatap,IS_DROOP=is_droop
  case 1 of
     stregex(file,'bcd\.fits$',/BOOLEAN): type=0
     stregex(file,'f2ap\.fits$',/BOOLEAN): type=3
     stregex(file,'droop(res)?\.fits$',/BOOLEAN): type=1
     stregex(file,'coad[^.]*\.fits$',/BOOLEAN): type=2
     stregex(file,'func\.fits$',/BOOLEAN): type=4
     stregex(file,'f2unc\.fits$',/BOOLEAN): type=5
     else: type=-1
  endcase
  if keyword_set(is_bcd) then return,type eq 0
  if keyword_set(is_droop) then return,type eq 1
  if keyword_set(is_flatap) then return,type eq 3
  return,type
end
