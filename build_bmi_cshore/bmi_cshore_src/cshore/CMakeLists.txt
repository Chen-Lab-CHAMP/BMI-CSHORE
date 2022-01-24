# cshore

# Create shared library, except on Windows.
if(WIN32)
  add_library(cshore_module cshore_new.f)
else()
  add_library(cshore_module SHARED string_processing.f90 cshore_helper.f90 cshore_new.f)
endif()

add_executable(run_cshore main.f90)
target_link_libraries(run_cshore cshore_module)

install(
  TARGETS run_cshore
  RUNTIME DESTINATION bin
)
install(
  TARGETS cshore_module
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
  RUNTIME DESTINATION bin
)
install(
  FILES ${CMAKE_Fortran_MODULE_DIRECTORY}/cshore_helper_module.mod
  FILES ${CMAKE_Fortran_MODULE_DIRECTORY}/string_processing.mod
  FILES ${CMAKE_Fortran_MODULE_DIRECTORY}/cshore_module.mod
  DESTINATION include
)
