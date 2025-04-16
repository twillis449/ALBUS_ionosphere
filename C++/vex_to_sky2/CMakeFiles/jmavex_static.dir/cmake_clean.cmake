file(REMOVE_RECURSE
  "libjmavex.a"
  "libjmavex.pdb"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX Fortran)
  include(CMakeFiles/jmavex_static.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
