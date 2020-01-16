cmake_minimum_required(VERSION 3.16)
project(nemsiogfs VERSION 2.2.1)
set(${PROJECT_NAME}_VERSION ${PROJECT_VERSION} CACHE INTERNAL "${PROJECT_NAME} version number")

enable_language (Fortran)
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

if(CMAKE_CXX_COMPILER_ID STREQUAL "Intel")
  set(IntelComp true )
elseif(CMAKE_CXX_COMPILER_ID MATCHES "GNU*" OR CMAKE_CXX_COMPILER_ID MATCHES "Clang*")
  set(GNUComp true )
elseif(CMAKE_CXX_COMPILER_ID MATCHES "pgc*")
  set(PGIComp true )
endif()
if(CMAKE_BUILD_TYP)
  STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "RelWithDebInfo" BUILD_RELEASE)
  if(NOT BUILD_RELEASE )
    STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "RELEASE" BUILD_RELEASE)
  endif()
  STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "PRODUCTION" BUILD_PRODUCTION)
  STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "DEBUG" BUILD_DEBUG)
endif()


if(IntelComp)
  set(fortran_flags "-O3" "-FR" "-axCore-AVX2")
elseif(GNUComp)
  set(fortran_flags "-O3" "-ffree-form")
else()
  message("unknown compiler!")
  exit()
endif()

file(GLOB fortran_src
  ${CMAKE_CURRENT_SOURCE_DIR}/src/*.f
  ${CMAKE_CURRENT_SOURCE_DIR}/src/*.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/src/*.F)

if(NOT TARGET nemsio)
  find_package(nemsio REQUIRED CONFIG)
endif()

if(NOT TARGET bacio_4)
  find_package(bacio REQUIRED CONFIG)
endif()

if(NOT TARGET w3nco_d)
  find_package(w3nco REQUIRED CONFIG)
endif()

if(NOT TARGET ip_d)
  find_package(ip REQUIRED CONFIG)
endif()

if(NOT TARGET sp_d)
  find_package(sp REQUIRED CONFIG)
endif()

find_package(OpenMP REQUIRED)


set(lib_name ${PROJECT_NAME})
set(versioned_lib_name ${PROJECT_NAME}_v${PROJECT_VERSION})

add_library(${lib_name} STATIC ${fortran_src})
set_target_properties(${lib_name} PROPERTIES OUTPUT_NAME "${versioned_lib_name}")

set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include")
set_target_properties(${lib_name} PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")

target_include_directories(${lib_name} PUBLIC
  $<BUILD_INTERFACE:${module_dir}>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_PREFIX}/include>)

target_link_libraries(${lib_name} PRIVATE nemsio bacio_4 w3nco_d ip_d sp_d ${OpenMP_Fortran_FLAGS})

install(TARGETS ${lib_name}
  EXPORT ${PROJECT_NAME}-config
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib) 
install(DIRECTORY ${module_dir} DESTINATION ${CMAKE_INSTALL_PREFIX})
install(EXPORT ${PROJECT_NAME}-config DESTINATION ${CMAKE_INSTALL_PREFIX})



