cmake_minimum_required(VERSION 2.8.12)
project(swipl-libedit)

include("../cmake/PrologPackage.cmake")

include(FindLibedit)
set(CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} ${LIBEDIT_LIBRARIES})

set(PKG_LIBEDIT_PL editline.pl)

check_include_file(fcntl.h HAVE_FCNTL_H)
check_include_file(poll.h HAVE_POLL_H)
check_include_file(sys/ioctl.h HAVE_SYS_IOCTL_H)
check_include_file(sys/select.h HAVE_SYS_SELECT_H)
check_include_file(unistd.h HAVE_UNISTD_H)

check_function_exists(el_cursor HAVE_EL_CURSOR)
check_function_exists(el_wset HAVE_EL_WSET)
check_function_exists(poll HAVE_POLL)

configure_file(config.h.cmake config.h)

add_library(
    libedit4pl MODULE
    libedit4pl.c)
set_target_properties(libedit4pl PROPERTIES PREFIX "")

target_link_libraries(libedit4pl ${LIBEDIT_LIBRARIES} ${SWIPL_LIBRARIES})

install(TARGETS libedit4pl
	LIBRARY DESTINATION ${SWIPL_INSTALL_MODULES})
install(FILES ${PKG_LIBEDIT_PL}
	DESTINATION ${SWIPL_INSTALL_LIBRARY})
