#*****************************************************************************
#              Makefile Build System for Fawkes: ROS Fawkes
#                            -------------------
#   Created on Wed May 04 18:48:20 2011
#   Copyright (C) 2011 by Tim Niemueller, AllemaniACs RoboCup Team
#
#*****************************************************************************
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#*****************************************************************************

BASEDIR = .

SUBDIRS = fawkes src

include $(BASEDIR)/etc/buildsys/config.mk
include $(BUILDSYSDIR)/rules.mk
include $(BUILDSYSDIR)/root/root.mk

src: fawkes

ros:
	$(SILENT)$(MAKE) -f Makefile.cmake

ros%:
	$(SILENT)$(MAKE) -f Makefile.cmake $*

