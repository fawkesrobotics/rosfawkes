
----------------------------------------------------------------------------
--  perceive_objects.lua - Call perception and post findings to blackboard
--
--  Created: Sat Oct 11 18:38:11 2014
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
name               = "perceive_objects"
fsm                = SkillHSM:new{name=name, start="WAIT_A_LITTLE", debug=true}
depends_skills     = {}

depends_interfaces = {}

local MAX_NUM_OBJECTS = 20
for i = 1, MAX_NUM_OBJECTS do
   table.insert(depends_interfaces, {v = "obj_type_"..i, type = "MultiTypedObjectInterface"})
   table.insert(depends_interfaces, {v = "obj_pose_"..i, type = "Position3DInterface"})
end

depends_actions  = {
   {v = "update_objects", name="do_object_tracking", type="hybris_c1_msgs/DoObjectTracking"}
}
depends_topics   = {
   --{v="objects", name="perceived_tabletop_objects", type="hybris_c1_msgs/PerceivedObjects", latching=true}
}


documentation      = [==[Call perception ROS action and post result to blackboard.]==]

-- Initialize as skill module
skillenv.skill_module(...)

-- Constants
local type_ifs = {}
local pose_ifs = {}
for i = 1, MAX_NUM_OBJECTS do
    type_ifs[i] = _M["obj_type_"..i]
    pose_ifs[i] = _M["obj_pose_"..i]
end

-- Jump conditions


-- States
fsm:define_states{
   export_to=_M,
   closure={objects=objects},

   {"WAIT_A_LITTLE", JumpState},
   {"UPDATE_OBJECTS", ActionJumpState, action_client=update_objects, exit_to="PUBLISH", fail_to="FAILED"},
   {"PUBLISH", JumpState}
}

-- Transitions
fsm:add_transitions{
   {"WAIT_A_LITTLE", "UPDATE_OBJECTS", timeout=3.0},
   {"PUBLISH", "FINAL", cond=true}
}

function UPDATE_OBJECTS:exit()
   if self:goal_handle():succeeded() then
      self.fsm.vars.message = self:goal_handle().result.values.detections
   end
end

function PUBLISH:init()
   local m = self.fsm.vars.message

   if #m.values.objects > #type_ifs then
      print_warn("More than %d objects received", #type_ifs)
   end

   local num_obj = math.min(#m.values.objects, #type_ifs)
   local set_objs = {}

   if self.fsm.debug then
      print("Received message:")
      m:print("  ")
   end

   for i = 1, num_obj do
      local id    = m.values.objects[i].values.id
      local types = m.values.objects[i].values.types
      local frame = m.values.header.values.frame_id
      -- With geometry_msgs/Point
      local pos   = m.values.objects[i].values.centroid.values
      local rot   = {x=0., y=0., z=0., w=1.}
      -- If we were using geometry_msgs/Pose
      --local pos   = m.values.objects[i].values.centroid.values.position.values
      --local rot   = m.values.objects[i].values.centroid.values.orientation.values

      if #types > 4 then
	 print_warn("Object %d has more than 4 types: %s", i, tostring(types))
      end

      if id < 0 or id >= #pose_ifs then
	 print_error("Object iD out of range, ignoring")
      else
	 -- object IDs are zero-based, arrays start with idx 1
	 local idx = id + 1

	 -- remember which objects we set
	 set_objs[idx] = true

	 type_ifs[idx]:set_obj_id(id)

	 if #types > 0 then type_ifs[idx]:set_type_1(types[1]) end
	 if #types > 1 then type_ifs[idx]:set_type_2(types[2]) end
	 if #types > 2 then type_ifs[idx]:set_type_3(types[3]) end
	 if #types > 3 then type_ifs[idx]:set_type_4(types[4]) end

	 pose_ifs[idx]:set_frame(frame)

	 pose_ifs[idx]:set_translation(0, pos.x)
	 pose_ifs[idx]:set_translation(1, pos.y)
	 pose_ifs[idx]:set_translation(2, pos.z)

	 pose_ifs[idx]:set_rotation(0, rot.x)
	 pose_ifs[idx]:set_rotation(1, rot.y)
	 pose_ifs[idx]:set_rotation(2, rot.z)
	 pose_ifs[idx]:set_rotation(3, rot.w)

	 if m.values.objects[i].values.active then
	    -- Just set to one, would need proper ID matching and since we
	    -- expect this particular perception to be one-shot it wouldn't
	    -- make much sense anyway
	    printf("Object %d is active with types [%s]", id, table.concat(types, ", "))
	    pose_ifs[idx]:set_visibility_history(1)
	 else
	    printf("Object %d is INactive with types [%s]", id, table.concat(types, ", "))
	    pose_ifs[idx]:set_visibility_history(-1)
	 end
      end
   end

   -- Mark remaining interfaces as object not visible
   for i = 1, #pose_ifs do
      if not set_objs[i] then
	 printf("Object %d not (yet) seen", i)
	 pose_ifs[i]:set_visibility_history(-1)
      end
   end
end
