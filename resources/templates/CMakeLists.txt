# Project info
cmake_minimum_required(VERSION "3.2.2" FATAL_ERROR)
project("[[ PROJECT_NAME ]]")
set(CMAKE_PATH "$ENV{CMAKE_PATH}")

# Utilities
include("${CMAKE_PATH}/macros/configure_project.cmake")
include("${CMAKE_PATH}/macros/module_dependency.cmake")
include("${CMAKE_PATH}/macros/add_lib.cmake")
include("${CMAKE_PATH}/macros/test_module_dependency.cmake")
include("${CMAKE_PATH}/macros/add_lib_tests.cmake")

# Configuration
configure_project()

# Library
module_dependency_versioned("SFML" "2.3.2" "system" "window" "graphics" "network" "audio")
module_dependency("JCore")
add_lib(SHARED)

# Tests
test_module_dependency("GoogleTest")
add_lib_tests()
