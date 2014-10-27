
----------------------------------------------------------------------------
--  init.lua - Skiller AtHome/PR2 skill space
--
--  Created: Sat Oct 11 18:31:41 2014
--  Copyright  2008-2009  Tim Niemueller [www.niemueller.de]
----------------------------------------------------------------------------

--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU Library General Public License for more details.
--
--  Read the full text in the LICENSE.GPL file in the doc directory.

local _G = _G

require("fawkes.modinit")
module(..., fawkes.modinit.register_all);

if not _G.HAVE_ROS then
   error("ROS support is required for PR2 skill space!")
end

local action_skill = require("skiller.ros.action_skill")
local service_skill = require("skiller.ros.service_skill")

skillenv.use_skill("skills.generic.relgoto")
skillenv.use_skill("skills.generic.goto")
skillenv.use_skill("skills.generic.ppgoto")

action_skill.use("pr2.planexec", "set_planning_goal", "continual_planning_server/PlanExec")
action_skill.use("pr2.update_objects", "do_object_tracking", "hybris_c1_msgs/DoObjectTracking")
--action_skill.use("pr2.get_new_sensor_data", "foo", "continual_planning_server/PlanExec")
service_skill.use("pr2.reset_perception", "reset_perception_service", "hybris_c1_msgs/ResetPerceptionService")

skillenv.use_skill("skills.pr2.perceive_objects")
