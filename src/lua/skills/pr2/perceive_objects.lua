
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
fsm                = SkillHSM:new{name=name, start="UPDATE_OBJECTS", debug=true}
depends_skills     = {"update_objects"}
depends_interfaces = {
   {v = "obj_type_1", type = "MultiTypedObjectInterface"},
   {v = "obj_type_2", type = "MultiTypedObjectInterface"},
   {v = "obj_type_3", type = "MultiTypedObjectInterface"},
   {v = "obj_type_4", type = "MultiTypedObjectInterface"},
   {v = "obj_type_5", type = "MultiTypedObjectInterface"},
   {v = "obj_type_6", type = "MultiTypedObjectInterface"},
   {v = "obj_type_7", type = "MultiTypedObjectInterface"},
   {v = "obj_type_8", type = "MultiTypedObjectInterface"},
   {v = "obj_pose_1", type = "Position3DInterface"},
   {v = "obj_pose_2", type = "Position3DInterface"},
   {v = "obj_pose_3", type = "Position3DInterface"},
   {v = "obj_pose_4", type = "Position3DInterface"},
   {v = "obj_pose_5", type = "Position3DInterface"},
   {v = "obj_pose_6", type = "Position3DInterface"},
   {v = "obj_pose_7", type = "Position3DInterface"},
   {v = "obj_pose_8", type = "Position3DInterface"}
}
depends_actions  = nil
depends_topics   = {
   {v="objects", name="perceived_tabletop_objects", type="hybris_c1_msgs/PerceivedObjects", latching=true}
}


documentation      = [==[Call perception ROS action and post result to blackboard.]==]

-- Initialize as skill module
skillenv.skill_module(...)

-- Constants
local type_ifs = { obj_type_1, obj_type_2, obj_type_3, obj_type_4,
		   obj_type_5, obj_type_6, obj_type_7, obj_type_8 }
local pose_ifs = { obj_pose_1, obj_pose_2, obj_pose_3, obj_pose_4,
		   obj_pose_5, obj_pose_6, obj_pose_7, obj_pose_8 }

-- Jumpconditions


-- States
fsm:define_states{
   export_to=_M,
   closure={objects=objects},

   {"UPDATE_OBJECTS", SkillJumpState, skills={{update_objects}}, final_to="WAIT_MESSAGE", fail_to="FAILED"},
   {"WAIT_MESSAGE", JumpState},
   {"PUBLISH", JumpState}
}

-- Transitions
fsm:add_transitions{
   {"WAIT_MESSAGE", "PUBLISH", cond="#objects.messages > 0"},
   {"PUBLISH", "FINAL", cond=true}
}

function UPDATE_OBJECTS:init()
   -- reset latching subscriber so we later retrieve the latest message
   objects:reset_messages()
end


function PUBLISH:init()
   -- Should not happen, but just in case...
   if #objects.messages == 0 then
      -- Mark interfaces as object not visible
      for i = num_obj, #pose_ifs do
	 pose_ifs[i]:set_visibility_history(-1)
      end
   end

   local m = objects.messages[1]
   if #m.values.objects > #type_ifs then
      print_warn("More than five objects received, will only add five")
   end

   local num_obj = math.min(#m.values.objects, #type_ifs)
   local set_objs = {}

   for i = 1, num_obj do
      if self.fsm.debug then
	 print("Received message:")
	 m:print("  ")
      end
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
	 print_warn("Object %d has more than 4 types: %s", #types, tostring(types))
      end

      type_ifs[i]:set_obj_id(id)

      if #types > 0 then type_ifs[i]:set_type_1(types[1]) end
      if #types > 1 then type_ifs[i]:set_type_2(types[2]) end
      if #types > 2 then type_ifs[i]:set_type_3(types[3]) end
      if #types > 3 then type_ifs[i]:set_type_4(types[4]) end

      pose_ifs[i]:set_frame(frame)

      pose_ifs[i]:set_translation(0, pos.x)
      pose_ifs[i]:set_translation(1, pos.y)
      pose_ifs[i]:set_translation(2, pos.z)

      pose_ifs[i]:set_rotation(0, rot.x)
      pose_ifs[i]:set_rotation(1, rot.y)
      pose_ifs[i]:set_rotation(2, rot.z)
      pose_ifs[i]:set_rotation(3, rot.w)

      -- Just set to one, would need proper ID matching and since we
      -- expect this particular perception to be one-shot it wouldn't
      -- make much sense anyway
      pose_ifs[i]:set_visibility_history(1)
   end

   -- Mark remaining interfaces as object not visible
   for i = num_obj + 1, #pose_ifs do
      pose_ifs[i]:set_visibility_history(-1)
   end
end
