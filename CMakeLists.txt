cmake_minimum_required(VERSION 3.26)
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

project("monkeypp" LANGUAGES CXX)

if(PROJECT_SOURCE_DIR STREQUAL PROJECT_BINARY_DIR)
  message(
    FATAL_ERROR
      "In-source builds not allowed. Please make a new directory (called a build directory) and run CMake from there."
  )
endif()

# Check if the project is the top-level project
set(monkeypp_IS_TOP_LEVEL_PROJECT FALSE)
if(CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
  set(monkeypp_IS_TOP_LEVEL_PROJECT TRUE)
endif()

if(monkeypp_IS_TOP_LEVEL_PROJECT)
  # Generate compile_commands.json
  set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
  option(monkeypp_ENABLE_CATCH2_TESTS "Enable Tests for monkeypp" ON)
  option(monkeypp_ENABLE_GLOBS "Enable GLOB sourcing for monkeypp" ON)
  option(monkeypp_ENABLE_RAYLIB "Enable Raylib for monkeypp" OFF)
  option(monkeypp_ENABLE_FLECS "Enable flecs for monkeypp" OFF)
  option(monkeypp_BUILD_APPS "Enable App building for monkeypp" ON)
  # create a symbolic link to the compile_commands file:
  file(
    CREATE_LINK
    ${CMAKE_BINARY_DIR}/compile_commands.json
    compile_commands.json
    COPY_ON_ERROR
    SYMBOLIC)
else()
  # disable tests if we're a sub-project
  set(monkeypp_ENABLE_CATCH2_TESTS FALSE)
  # enable globs if we're a sub-project?
  set(monkeypp_ENABLE_GLOBS TRUE)
endif()

add_subdirectory(${CMAKE_PROJECT_NAME}/library/src)

# adjust or override some CMake defaults
include(cmake/OverrideCMakeDefaults.cmake)

# default compiler options and warnings
include(cmake/DefaultCompilerOptionsAndWarnings.cmake)

# static analyzers
include(cmake/StaticAnalyzers.cmake)

# sanitizers
include(cmake/Sanitizers.cmake)

# Catch2 v3
if(monkeypp_ENABLE_CATCH2_TESTS)
  # enable testing with CTest
  include(CTest)
  find_package(Catch2 QUIET)
  if(NOT Catch2_FOUND)
    include(FetchContent)
    FetchContent_Declare(
      Catch2
      GIT_REPOSITORY https://github.com/catchorg/Catch2.git
      GIT_TAG devel)
    FetchContent_MakeAvailable(Catch2)
    list(APPEND CMAKE_MODULE_PATH ${Catch2_SOURCE_DIR}/extras)
  endif()
  include(Catch)

  include(cmake/AddCatch2Tests.cmake)
  add_subdirectory(${CMAKE_PROJECT_NAME}/library/tests)
endif()

# Web Configurations
if(${PLATFORM} STREQUAL "Web")
  message("#######${PLATFORM}#####")
  # add emscripten include directory to a .clangd file
  # as we want the LSP to be happy, but not effect the build! (not we include the systroot emscripten, but the system includes to help with linter errors)
  file(WRITE ".clangd" "CompileFlags:\n  Add:\n    - \"-I${EMSDK}/upstream/emscripten/cache/sysroot/include/emscripten\"\n    - \"-I${EMSDK}/upstream/emscripten/system/include/\"")
  # file(WRITE ".clangd" "CompileFlags:\n  Add:\n    - \"-I${EMSDK}/upstream/emscripten/cache/sysroot/include/\"")
  # file(WRITE ".clangd" "CompileFlags:\n  Add:\n    - \"-I${EMSDK}/upstream/emscripten/system/include/\"")

  # include_directories(${EMSDK}/upstream/emscripten/cache/sysroot/include/)
  # add_compile_options(-I${EMSDK}/upstream/emscripten/cache/sysroot/include/emscripten)
  # include_directories(SYSTEM ${EMSDK}/upstream/emscripten/system/include/)
endif()

# raylib:
if(monkeypp_ENABLE_RAYLIB)
  set(RAYLIB_VERSION 5.0)
  find_package(raylib ${RAYLIB_VERSION} QUIET) # QUIET or REQUIRED
  if(NOT raylib_FOUND) # If there's none, fetch and build raylib
    include(FetchContent)
    FetchContent_Declare(
      raylib
      DOWNLOAD_EXTRACT_TIMESTAMP OFF
      URL https://github.com/raysan5/raylib/archive/refs/tags/${RAYLIB_VERSION}.tar.gz
    )
    FetchContent_GetProperties(raylib)
    if(NOT raylib_POPULATED) # Have we downloaded raylib yet?
      set(FETCHCONTENT_QUIET NO)
      FetchContent_Populate(raylib)
      set(BUILD_EXAMPLES OFF CACHE BOOL "" FORCE) # don't build the supplied examples
      add_subdirectory(${raylib_SOURCE_DIR} ${raylib_BINARY_DIR})
    endif()
  endif()

  # add_custom_command(
  #   TARGET ${CMAKE_PROJECT_NAME} POST_BUILD
  #   COMMENT "Copy Resources"
  #   COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/resources $<TARGET_FILE_DIR:${CMAKE_PROJECT_NAME}>/resources
  #   DEPENDS ${CMAKE_PROJECT_NAME})
endif()

# flecs:
if(monkeypp_ENABLE_FLECS)
  set(FLECS_VERSION 4.0.0)
  find_package(flecs ${FLECS_VERSION} QUIET) # QUIET or REQUIRED
  if(NOT flecs_FOUND) # If there's none, fetch and build flecs
    include(FetchContent)
    FetchContent_Declare(
      flecs
      DOWNLOAD_EXTRACT_TIMESTAMP OFF
      URL https://github.com/SanderMertens/flecs/archive/refs/tags/v${FLECS_VERSION}.tar.gz
    )
    FetchContent_GetProperties(flecs)
    if(NOT flecs_POPULATED) # Have we downloaded flecs yet?
      set(FETCHCONTENT_QUIET NO)
      FetchContent_Populate(flecs)
      add_subdirectory(${flecs_SOURCE_DIR} ${flecs_BINARY_DIR})
    endif()
  endif()
endif()

if(monkeypp_BUILD_APPS)
  # loop through each directory and add their sources, etc. to their own executable
  file(GLOB found_folders LIST_DIRECTORIES true ${CMAKE_PROJECT_NAME}/apps/*)
  foreach(folder ${found_folders})
    if(IS_DIRECTORY ${folder})
      message(STATUS "adding folder as app: ${folder}")
      add_subdirectory(${folder}/src/)
      if(monkeypp_ENABLE_CATCH2_TESTS)
        add_subdirectory(${folder}/tests/)
      endif()
    endif()
  endforeach()
endif()
