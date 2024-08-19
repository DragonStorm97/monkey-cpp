# TODO: add docstring

function(add_catch2_tests app_name is_lib)
  if(monkeypp_ENABLE_CATCH2_TESTS)
    set(app_test_name ${app_name}_tests)
    if(monkeypp_ENABLE_GLOBS)
      file(
        GLOB_RECURSE
        sources
        CONFIGURE_DEPENDS
        "*.cpp")
    else()
      set(sources "${app_test_name}.cpp")
    endif()

    # build a second version of the test with STATIC_REQUIRE as REQUIRE to debug constexpr test failures
    set(test_names relaxed_constexpr_${app_test_name} ${app_test_name})
    foreach(test_name IN LISTS test_names)
      add_executable(${test_name} ${sources})

      set_target_properties(${test_name} PROPERTIES CXX_EXTENSIONS OFF)
      target_compile_features(${test_name} PUBLIC cxx_std_23)
      target_compile_options(${test_name} PRIVATE ${DEFAULT_COMPILER_OPTIONS_AND_WARNINGS})

      # Add library as dependency
      target_include_directories(${test_name} PUBLIC ${PROJECT_SOURCE_DIR}/include/${CMAKE_PROJECT_NAME})
      target_include_directories(${test_name} PUBLIC ${PROJECT_SOURCE_DIR}/${CMAKE_PROJECT_NAME}/library/include)

      # add the app as a dependency if it's not the library
      if(NOT IS_LIB)
        target_include_directories(${test_name} PUBLIC ${PROJECT_SOURCE_DIR}/${CMAKE_PROJECT_NAME}/apps/${app_name}/include)
      endif()

      target_link_libraries(${test_name} PRIVATE libmonkeypp Catch2::Catch2WithMain)

      catch_discover_tests(${test_name} LABEL ${app_name})

      if(${test_name} MATCHES "^relaxed.*")
        target_compile_definitions(${test_name} PRIVATE -DCATCH_CONFIG_RUNTIME_STATIC_REQUIRE)
      endif()
    endforeach()
  endif()
endfunction()
