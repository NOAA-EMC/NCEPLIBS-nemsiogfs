cmake_minimum_required(VERSION 3.15)

file(STRINGS "VERSION" pVersion)

project(nemsiogfs VERSION ${pVersion} LANGUAGES Fortran)

option(OPENMP "use OpenMP threading" OFF)

if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel)$")
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE
      "Release"
      CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
                                               "MinSizeRel" "RelWithDebInfo")
endif()

if(NOT CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel|GNU|Clang|AppleClang)$")
  message(
    WARNING "Compiler not officially supported: ${CMAKE_Fortran_COMPILER_ID}")
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "-g")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU|Clang|AppleClang)$")
  set(CMAKE_Fortran_FLAGS "-g")
endif()

set(CMAKE_FIND_PACKAGE_PREFER_CONFIG true)
if(NOT TARGET nemsio)
  find_package(nemsio REQUIRED)
endif()

if(NOT TARGET bacio_4)
  find_package(bacio REQUIRED)
endif()

if(NOT TARGET w3nco_d)
  find_package(w3nco REQUIRED)
endif()

if(NOT TARGET ip_d)
  find_package(ip REQUIRED)
endif()

if(NOT TARGET sp_d)
  find_package(sp REQUIRED)
endif()

if(OPENMP)
  find_package(OpenMP REQUIRED COMPONENTS Fortran)
endif()

set(fortran_src src/nemsio_gfs.f90)

set(lib_name ${PROJECT_NAME})
set(versioned_lib_name ${PROJECT_NAME}_v${PROJECT_VERSION})
set(module_dir ${CMAKE_CURRENT_BINARY_DIR}/include)

add_library(${lib_name} STATIC ${fortran_src})
set_target_properties(${lib_name} PROPERTIES OUTPUT_NAME ${versioned_lib_name})
set_target_properties(${lib_name} PROPERTIES Fortran_MODULE_DIRECTORY
                                             ${module_dir})
if(OpenMP_Fortran_FOUND)
  set_target_properties(${lib_name} PROPERTIES COMPILE_FLAGS
                                               "${OpenMP_Fortran_FLAGS}")
endif()
set_target_properties(${lib_name} PROPERTIES INTERFACE_LINK_LIBRARIES
                                             ${lib_name})
target_include_directories(
  ${lib_name} PUBLIC $<BUILD_INTERFACE:${module_dir}>
                     $<INSTALL_INTERFACE:${CMAKE_INSTALL_PREFIX}/include>)
target_link_libraries(${lib_name} PRIVATE nemsio bacio_4 w3nco_d ip_d sp_d)

install(
  TARGETS ${lib_name}
  EXPORT ${PROJECT_NAME}-config
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib)
install(DIRECTORY ${module_dir} DESTINATION ${CMAKE_INSTALL_PREFIX})

install(EXPORT ${PROJECT_NAME}-config
        DESTINATION ${CMAKE_INSTALL_PREFIX}/lib/cmake/${PROJECT_NAME})
