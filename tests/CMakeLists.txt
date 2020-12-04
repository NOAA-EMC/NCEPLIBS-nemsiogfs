
link_libraries(nemsiogfs::nemsiogfs)
add_executable(test_nemsiogfs test_nemsiogfs.f90)


add_test(test_nemsiogfs test_nemsiogfs)
