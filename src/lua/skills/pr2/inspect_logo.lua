
----------------------------------------------------------------------------
--  inspect_logo.lua - Call perception to identify specific logo
--
--  Created: Mon Oct 27 11:09:18 2014
--  Copyright  2014  Tim Niemueller [www.niemueller.de]
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

-- Initialize module
module(..., skillenv.module_init)

-- Crucial skill information
name               = "inspect_logo"
fsm                = SkillHSM:new{name=name, start="RUN_PERCEPTION", debug=true}
depends_skills     = {}
depends_interfaces = {
   {v = "obj_type_logo", type = "MultiTypedObjectInterface"}
}
depends_actions  = {
   {v = "inspect_logo", name="do_logo_detection", type="hybris_c1_msgs/DoLogoDetection"}
}
depends_topics   = nil


documentation      = [==[Call perception ROS action and post result to blackboard.]==]

-- Initialize as skill module
skillenv.skill_module(...)

-- Constants

-- Jump conditions


-- States
fsm:define_states{
   export_to=_M,

   {"RUN_PERCEPTION", ActionJumpState, action_client=inspect_logo, exit_to="PUBLISH", fail_to="FAILED"},
   {"PUBLISH", JumpState}
}

-- Transitions
fsm:add_transitions{
   {"RUN_PERCEPTION", "FAILED", timeout=20},
   {"PUBLISH", "FINAL", cond=true}
}

function RUN_PERCEPTION:exit()
   local goal_handle = self:goal_handle()
   if goal_handle and goal_handle:succeeded() then
      self.fsm.vars.logo_type = self:goal_handle().result.values.logo_type.values.data
   end
end

function PUBLISH:init()
   print("Received object type: %s", self.fsm.vars.logo_type)
   obj_type_logo:set_obj_id(0)
   obj_type_logo:set_type_1(self.fsm.vars.logo_type)
end
