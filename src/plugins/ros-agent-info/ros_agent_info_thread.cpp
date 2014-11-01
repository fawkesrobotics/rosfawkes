
/***************************************************************************
 *  ros_agent_info_thread.cpp - Publish agent info to ROS
 *
 *  Created: Fri Oct 31 14:49:51 2014
 *  Copyright  2014  Tim Niemueller
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

#include "ros_agent_info_thread.h"

#include <core/threading/mutex_locker.h>

#include <std_msgs/String.h>

using namespace fawkes;

/** @class RosAgentInfoThread "move_base_thread.h"
 * Accept locomotion commands from ROS (emulate move_base).
 * @author Sebastian Reuter
 */

/** Contructor. */
RosAgentInfoThread::RosAgentInfoThread()
  : Thread("RosAgentInfoThread", Thread::OPMODE_WAITFORWAKEUP),
    BlockedTimingAspect(BlockedTimingAspect::WAKEUP_HOOK_ACT)
{
}

void
RosAgentInfoThread::init()
{
  agent_info_if_ = blackboard->open_for_reading<AgentInfoInterface>("Agent");

  pub_agent_info_ = rosnode->advertise<std_msgs::String>("agent_info", false);
}

void
RosAgentInfoThread::finalize()
{
  try {
    blackboard->close(agent_info_if_);
  } catch (Exception& e) {
    logger->log_error(name(), "Closing interface failed!");
    logger->log_error(name(), e);
  }
  pub_agent_info_.shutdown();
}


void
RosAgentInfoThread::loop()
{
  agent_info_if_->read();
  if (agent_info_if_->changed()) {
    std_msgs::String msg;
    msg.data = agent_info_if_->message();
    pub_agent_info_.publish(msg);
  }
}
