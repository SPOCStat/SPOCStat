set(SRC_DIRS common statistical-distributions spocs)
foreach(SUBDIR ${SRC_DIRS})
  add_subdirectory(${SUBDIR})
endforeach()
