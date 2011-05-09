#*****************************************************************************
#          Makefile Build System for Fawkes : ROS Main Application
#                            -------------------
#   Created on Thu May 05 15:20:43 2011
#   copyright (C) 2011 by Tim Niemueller, AllemaniACs RoboCup Team
#
#*****************************************************************************
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#*****************************************************************************

BASEDIR = ../..

include $(BASEDIR)/etc/buildsys/config.mk

CFLAGS  += $(shell rospack export --lang=cpp --attrib=cflags roscpp)
LDFLAGS += $(shell rospack export --lang=cpp --attrib=lflags roscpp)

LIBS_rosfawkes = stdc++ fawkescore fawkesbaseapp
OBJS_rosfawkes = $(patsubst %.cpp,%.o,$(patsubst qa/%,,$(subst $(SRCDIR)/,,$(realpath $(wildcard $(SRCDIR)/*.cpp)))))

OBJS_all     = $(OBJS_rosfawkes)
BINS_all     = $(BINDIR)/rosfawkes
#MANPAGES_all = $(MANDIR)/man8/rosfawkes.8

include $(BUILDSYSDIR)/base.mk

