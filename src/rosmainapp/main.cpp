
/***************************************************************************
 *  main.cpp - ROS-Fawkes main application
 *
 *  Created: Mon May 9 17:59:20 2011
 *  Copyright  2006-2014  Tim Niemueller [www.niemueller.de]
 ****************************************************************************/

/*  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version. A runtime exception applies to
 *  this software (see LICENSE.GPL_WRE file mentioned below for details).
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  Read the full text in the LICENSE.GPL_WRE file in the doc directory.
 */

#include <baseapp/run.h>
#include <baseapp/main_thread.h>
#include <core/exception.h>
#include <core/version.h>
#include <aspect/manager.h>
#include <plugins/ros/aspect/ros.h>
#include <plugins/ros/aspect/ros_inifin.h>

#include <ros/ros.h>

#include <cstdio>


/** Fawkes application.
 * @param argc argument count
 * @param argv array of arguments
 */
int
main(int argc, char **argv)
{
  ros::init(argc, argv, "rosfawkes");

  fawkes::runtime::InitOptions init_options(argc, argv);
  std::string plugins;
  if (ros::param::get("~plugins", plugins) && plugins != "") {
    if (init_options.has_load_plugin_list()) {
      plugins += std::string(",") + init_options.load_plugin_list();
    }
    init_options.load_plugins(plugins.c_str());
  }

  try {
    int rv = 0;
    if (! fawkes::runtime::init(init_options, rv)) {
      return rv;
    }
  } catch (fawkes::Exception &e) {
    printf("Fawkes initialization failed, exception follows.\n");
    e.print_trace();
    return 1;
  }

  fawkes::LockPtr<ros::NodeHandle> node_handle(new ros::NodeHandle());
  fawkes::ROSAspectIniFin ros_aspect_inifin;
  ros_aspect_inifin.set_rosnode(node_handle);
  fawkes::runtime::aspect_manager->register_inifin(&ros_aspect_inifin);

  if (init_options.has_load_plugin_list()) {
    fawkes::runtime::logger->log_info("ROS-Fawkes", "Loading plugins: %s",
				      init_options.load_plugin_list());
  }
  fawkes::runtime::main_thread->full_start();
  fawkes::runtime::logger->log_info("ROS-Fawkes", "ROS-Fawkes %s startup complete",
				    FAWKES_VERSION_STRING);

  ros::spin();

  fawkes::runtime::main_thread->cancel();
  fawkes::runtime::main_thread->join();
  fawkes::runtime::cleanup();

  return 0;
}
