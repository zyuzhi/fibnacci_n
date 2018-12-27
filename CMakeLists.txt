cmake_minimum_required(VERSION 3.2)
project(fibonacci_n)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_FLAGS "-std=c++14 -march=native -pipe -O3 -Wall -Wextra")

add_executable(fibonacci_n main.cpp)