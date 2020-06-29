list(APPEND CMAKE_CTEST_ARGUMENTS "--output-on-failure")

# need to manually link to w3nco and bacio as dependencies of nemsio because they can't be transitive
add_pfunit_ctest (nemsiogfs_test
  TEST_SOURCES test_mod.pf
  LINK_LIBRARIES nemsiogfs::nemsiogfs bacio::bacio_4 w3nco::w3nco_d
)
