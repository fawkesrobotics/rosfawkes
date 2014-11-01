#*****************************************************************************
#             Makefile Build System for Fawkes: ROS Agent Info Plugin
#                            -------------------
#   Created on Fri Oct 31 14:43:25 2014 (Hybris 2014 Preparations)
#   Copyright (C) 2014 by Tim Niemueller
#
#*****************************************************************************
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#*****************************************************************************

BASEDIR = ../../..
include $(BASEDIR)/etc/buildsys/config.mk
include $(BUILDSYSDIR)/ros.mk
include $(BUILDSYSDIR)/boost.mk

LIBS_ros_agent_info = fawkescore fawkesutils fawkesaspects fawkesblackboard \
		      fawkesinterface fawkesrosaspect AgentInfoInterface
OBJS_ros_agent_info = ros_agent_info_plugin.o ros_agent_info_thread.o


OBJS_all = $(OBJS_ros_agent_info)

ifeq ($(HAVE_ROS),1)
  CFLAGS  += $(CFLAGS_ROS) -Wno-unknown-pragmas -Wno-deprecated-declarations
  LDFLAGS += $(LDFLAGS_ROS)

  CFLAGS_ros_agent_info_thread += $(call ros-pkg-cflags,std_msgs) $(CFLAGS)
  LDFLAGS_ros_agent_info += $(LDFLAGS) $(call ros-pkg-lflags,std_msgs)

  PLUGINS_all = $(PLUGINDIR)/ros-agent-info.so
else
  WARN_TARGETS += warning_ros
endif

ifeq ($(OBJSSUBMAKE),1)
all: $(WARN_TARGETS)

.PHONY: warning_ros
warning_ros:
	$(SILENT)echo -e "$(INDENT_PRINT)--> $(TRED)Omitting ros-agent-info plugin$(TNORMAL) (ROS not found)"
endif

include $(BUILDSYSDIR)/base.mk

