pro cu_ctdef,_EXTRA=e
  defsysv,'!ctabl',Exists=exi
  if exi eq 1 then return
  defsysv,'!ctabl',{!ctabl,cmap:0,low:0.,high:1.,gamma:1.0}
end
