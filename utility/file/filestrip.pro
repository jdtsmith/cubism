function filestrip,filepath
  return,strmid(filepath,strpos(filepath,path_sep(),/REVERSE_SEARCH)+1)
end
