
function polyclip_version
  common polyfillaa_external,polyclip_compiled,polyclip_path,polyclip_version
  if n_elements(polyclip_compiled) eq 0 then return,'Unknown' else $
     return,polyclip_compiled?'Compiled C, v'+strtrim(fix(polyclip_version),2): $
            'Internal IDL'
end
