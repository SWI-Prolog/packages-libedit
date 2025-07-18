cmake_minimum_required(VERSION 3.10)
project(swipl-libedit)

include("../cmake/PrologPackage.cmake")

find_package(Libedit)
if(LIBEDIT_FOUND)
set(CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} ${LIBEDIT_LIBRARIES})

AC_CHECK_HEADERS(fcntl.h poll.h sys/ioctl.h sys/select.h unistd.h)
AC_CHECK_FUNCS(el_cursor el_wset poll el_init_handles)

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
