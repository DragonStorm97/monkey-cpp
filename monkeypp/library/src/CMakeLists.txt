set(LIB_NAME libmonkeypp)

# Whenever this glob's value changes, cmake will rerun and update the build with
# the new/removed files.
if(monkeypp_ENABLE_GLOBS)
  file(
    GLOB_RECURSE
    SOURCES
    CONFIGURE_DEPENDS
    "*.cpp")
else()
  set(SOURCES "${LIB_NAME}.cpp")
endif()

# Note: for header-only libraries change all PUBLIC flags to INTERFACE and create an interface
# target: add_library(${PROJECT_NAME} INTERFACE)

add_library(${LIB_NAME} STATIC ${SOURCES})

if(monkeypp_ENABLE_RAYLIB)
  #set(raylib_VERBOSE 1)
  target_link_libraries(${LIB_NAME} raylib)

  # Web Configurations
  if(${PLATFORM} STREQUAL "Web")
    # Tell Emscripten to build an example.html file.
    set_target_properties(${LIB_NAME} PROPERTIES SUFFIX ".html")
    target_link_options(${LIB_NAME} PUBLIC "-sUSE_GLFW=3")
  endif()

  # Checks if OSX and links appropriate frameworks (Only required on MacOS)
  if(APPLE)
    target_link_libraries(${LIB_NAME} "-framework IOKit")
    target_link_libraries(${LIB_NAME} "-framework Cocoa")
    target_link_libraries(${LIB_NAME} "-framework OpenGL")
  endif()
endif()

set_target_properties(${LIB_NAME} PROPERTIES CXX_EXTENSIONS OFF)
target_compile_features(${LIB_NAME} PUBLIC cxx_std_23)
target_compile_options(${LIB_NAME} PRIVATE ${SANITIZER_FLAGS} ${DEFAULT_COMPILER_OPTIONS_AND_WARNINGS})
target_link_libraries(${LIB_NAME} PRIVATE ${SANITIZER_FLAGS})
target_include_directories(${LIB_NAME} PUBLIC ${PROJECT_SOURCE_DIR}/include/${CMAKE_PROJECT_NAME})
target_include_directories(${LIB_NAME} PUBLIC ${PROJECT_SOURCE_DIR}/${CMAKE_PROJECT_NAME}/library/include)

