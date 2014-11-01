
/***************************************************************************
 *  ros_agent_info_plugin.cpp - Publis agent info to ROS
 *
 *  Created: Fri Oct 31 14:46:51 2014
 *  Copyright  2014  Tim Niemueller [www.niemueller.de]
 ****************************************************************************/

/*  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  Read the full text in the LICENSE.GPL file in the doc directory.
 */

#include <core/plugin.h>
#include "ros_agent_info_thread.h"

using namespace fawkes;

/** Send agent info to ROS.
 * @autho Tim Niemueller
 */
class RosAgentInfoPlugin : public fawkes::Plugin
{
 public:
  /** Constructor.
   * @param config Fawkes configuration
   */
  RosAgentInfoPlugin(Configuration *config)
    : Plugin(config)
  {
    thread_list.push_back(new RosAgentInfoThread());
  }
};

PLUGIN_DESCRIPTION("Send agent info to ROS")
EXPORT_PLUGIN(RosAgentInfoPlugin)
