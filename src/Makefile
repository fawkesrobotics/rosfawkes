#*****************************************************************************
#                      Makefile Build System for Fawkes
#                            -------------------
#   Created on Wed May 04 19:58:26 2011
#   Copyright (C) 2006-2011 by Tim Niemueller, AllemaniACs RoboCup Team
#
#*****************************************************************************
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#*****************************************************************************

BASEDIR = ..

SUBDIRS = rosmainapp

include $(BASEDIR)/etc/buildsys/config.mk
include $(BUILDSYSDIR)/rules.mk

# Explicit dependencies, this is needed to have make bail out if there is any
# error. This is also necessary for working parallel build (i.e. for dual core)
#interfaces firevision plugins tools: libs

