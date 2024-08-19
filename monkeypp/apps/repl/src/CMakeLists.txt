set(APP_NAME repl)

# Whenever this glob's value changes, cmake will rerun and update the build with
# the new/removed files.
if(monkeypp_ENABLE_GLOBS)
  file(
    GLOB_RECURSE
    SOURCES
    CONFIGURE_DEPENDS
    "*.cpp")
else()
  set(SOURCES "${APP_NAME}.cpp")
endif()
set(SOURCES ${SOURCES})

add_executable(${APP_NAME} ${SOURCES})

# include the library:
target_link_libraries(${APP_NAME} PRIVATE libmonkeypp)

if(monkeypp_ENABLE_RAYLIB)
  #set(raylib_VERBOSE 1)
  target_link_libraries(${APP_NAME} PRIVATE raylib)

  # Web Configurations
  if(${PLATFORM} STREQUAL "Web")
    # Tell Emscripten to build an example.html file.
    set_target_properties(${APP_NAME} PROPERTIES SUFFIX ".html")
    target_link_options(${APP_NAME} PUBLIC "-sUSE_GLFW=3")
  endif()

  # Checks if OSX and links appropriate frameworks (Only required on MacOS)
  if(APPLE)
    target_link_libraries(${APP_NAME} "-framework IOKit")
    target_link_libraries(${APP_NAME} "-framework Cocoa")
    target_link_libraries(${APP_NAME} "-framework OpenGL")
  endif()
endif()

if(monkeypp_ENABLE_FLECS)
  target_link_libraries(${APP_NAME} PRIVATE flecs::flecs_static)
endif()

target_link_libraries(${APP_NAME} PRIVATE ${SANITIZER_FLAGS})
target_include_directories(${APP_NAME} PUBLIC ${PROJECT_SOURCE_DIR}/include/${CMAKE_PROJECT_NAME})
target_include_directories(${APP_NAME} PUBLIC ${PROJECT_SOURCE_DIR}/${CMAKE_PROJECT_NAME}/apps/${APP_NAME}/include)

set_target_properties(${APP_NAME} PROPERTIES CXX_EXTENSIONS OFF)
target_compile_features(${APP_NAME} PUBLIC cxx_std_23)
target_compile_options(${APP_NAME} PRIVATE ${SANITIZER_FLAGS} ${DEFAULT_COMPILER_OPTIONS_AND_WARNINGS})
