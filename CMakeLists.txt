cmake_minimum_required(VERSION 3.10)
project(swipl-libedit)

include("../cmake/PrologPackage.cmake")

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/libedit/CMakeLists.txt)
  option(SYSTEM_LIBEDIT
	 "Use OS version of libedit"
	 OFF)
else()
  option(SYSTEM_LIBEDIT
	 "Use OS version of libedit"
	 ON)
endif()

if(SYSTEM_LIBEDIT)
  find_package(Libedit)
  if(LIBEDIT_FOUND)
    set(CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} ${LIBEDIT_LIBRARIES})
    AC_CHECK_FUNCS(el_cursor el_wset el_init_handles)
  endif()
else()
  add_subdirectory(libedit EXCLUDE_FROM_ALL)
  set(LIBEDIT_LIBRARIES edit)
  set(LIBEDIT_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/libedit/src)
  set(LIBEDIT_FOUND ON)
  set(HAVE_EL_CURSOR ON)
  set(HAVE_EL_WSET ON)
  if(WIN32)
    set(HAVE_EL_INIT_HANDLES ON)
  endif()
endif()

if(LIBEDIT_FOUND)
AC_CHECK_HEADERS(fcntl.h poll.h sys/ioctl.h sys/select.h unistd.h)
AC_CHECK_FUNCS(poll open_memstream funopen)

if(WIN32)
  set(O_SIGNALS)
endif()

configure_file(config.h.cmake config.h)

swipl_plugin(
    libedit
    MODULE libedit4pl
    C_SOURCES libedit4pl.c
    C_LIBS ${LIBEDIT_LIBRARIES}
    C_INCLUDE_DIR ${LIBEDIT_INCLUDE_DIR}
    PL_LIBS editline.pl)

pkg_doc(libedit
	SECTION
            editline.pl)

endif(LIBEDIT_FOUND)
