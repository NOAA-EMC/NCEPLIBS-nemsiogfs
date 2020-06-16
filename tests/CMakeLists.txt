list(APPEND CMAKE_CTEST_ARGUMENTS "--output-on-failure")

add_pfunit_ctest (nemsiogfs_test
  TEST_SOURCES test_mod.pf
  LINK_LIBRARIES nemsiogfs::nemsiogfs
)
