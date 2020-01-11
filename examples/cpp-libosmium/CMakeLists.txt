cmake_minimum_required(VERSION 3.6)

set (CMAKE_CXX_STANDARD 17)

# ---[ Project name
project(hello-world)

# ---[ Output directory
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# ---[ Using cmake scripts and modules
list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/Modules)

# ---[ Includes
include_directories(${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/src/include)
include_directories(${EXTERNAL_INCLUDE_DIRECTORIES})

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

# ---[ Compiler flags
set(CMAKE_CXX_FLAGS_DEBUG "-g -pthread")
set(CMAKE_CXX_FLAGS_RELEASE "-O3 -pthread")
add_compile_options(-Wall -Wextra -pedantic -Werror)

# ---[ Handle dependencies
include(cmake/Dependencies.cmake)

# ---[ Subdirectories
add_subdirectory(src)

