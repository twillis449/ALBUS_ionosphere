file(REMOVE_RECURSE
  ".1"
  "libjmavex.pdb"
  "libjmavex.so"
  "libjmavex.so.1"
  "libjmavex.so.1.0.0"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX Fortran)
  include(CMakeFiles/jmavex_shared.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
