
/***************************************************************************
 *  ros_agent_info_thread.h - Send agent info to ROS
 *
 *  Created: Fri Oct 31 14:47:57 2014
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
 
#ifndef __ROS_AGENT_INFO_THREAD_H_
#define __ROS_AGENT_INFO_THREAD_H_

#include <core/threading/thread.h>
#include <aspect/blocked_timing.h>
#include <aspect/logging.h>
#include <aspect/blackboard.h>
#include <aspect/configurable.h>
#include <plugins/ros/aspect/ros.h>

#include <interfaces/AgentInfoInterface.h>

#include <cmath>
#include <ros/ros.h>

namespace fawkes {
  class AgentInfoInterface;
}

class RosAgentInfoThread
: public fawkes::Thread,
  public fawkes::BlockedTimingAspect,
  public fawkes::LoggingAspect,
  public fawkes::BlackBoardAspect,
  public fawkes::ConfigurableAspect,
  public fawkes::ROSAspect
{
 public:
  RosAgentInfoThread();

  virtual void init();
  virtual void finalize();
  virtual void loop();

 private:
  fawkes::AgentInfoInterface *agent_info_if_;
  ros::Publisher pub_agent_info_;

};

#endif /* __ROS_AGENT_INFO_THREAD_H_ */
