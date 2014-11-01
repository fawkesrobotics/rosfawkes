/***************************************************************************
 *  hybris_2014.ecl - Agent for DFG Hybris Site Visit Demo 2014
 *
 *  Created: Fri Oct 10 15:27:45 2014
 *  Copyright  2014  Gesche Gierse
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


/* This agent will grab a cup from a table and put it in the dishwasher, over and over again.
 * While the cups on the table are detected, cups in the dishwasher have to be removed, since
 * no visual detection is used there.
 */

:- module(hybris_2014).

:- use_module(filepath).
:- use_module(library(util)).

:- setval(use_esl, false).

:- if(getval(use_esl, true)).
  % Declare operators for ESL formulas:
  % (Hopefully there is no conflict with some code that expects different
  % operator precedence.)
  :- op(820, fx, ~).    /* Negation */
  :- op(840, xfy, ^).   /* Conjunction */
  :- op(850, xfy, v).   /* Disjunction */
  :- op(870, xfy, ->).  /* Implication */
  :- op(880, xfy, <->). /* Equivalence */
  :- op(890, xfy, =>).  /* Belief conditional */
  % Load the BAT shared library:
  :- load('/u/hybrisc1/esbl/bats/libBAT-kitchen.so').
  % Load the ESBL interface:
  :- load('/u/hybrisc1/esbl/eclipse-clp/libEclipseESBL.so').
  :- external(kcontext/1, p_kcontext).
  :- external(bcontext/2, p_bcontext).
  :- external(context_store/2, p_context_store).
  :- external(context_retrieve/2, p_context_retrieve).
  :- external(context_guarantee_consistency/2, p_context_guarantee_consistency).
  :- external(context_exec/3, p_context_exec).
  :- external(context_assert/2, p_context_assert).
  :- external(context_entails/3, p_context_entails).
  % Simple wrapper predicates:
  esl_reset :- kcontext(Ctx), context_store(eslctx, Ctx).
  esl_assert(Phi) :- context_retrieve(eslctx, Ctx), context_assert(Ctx, Phi).
  esl_holds(K, Phi) :- context_retrieve(eslctx, Ctx), context_entails(Ctx, K, Phi).
  % Conversions Prolog to ESL:
  object_to_stdname(N, StdName) :- concat_atom(['o', N], StdName).
  boxtype_to_stdname(T, StdName) :- atom_string(StdName, T).
  % Initialize a context to start with:
  :- esl_reset.
:- endif.

:- include("indigolog-esl").

% This doesn't work and we do not have a proper solution at this time
%?- locate_file("indigolog-esl.ecl", F), include(F).
?- locate_module("blackboard", F), use_module(F).
?- locate_module("rcsoft_map_graph", F), use_module(F).
?- locate_module("quaternions", F), use_module(F).
?- locate_module("logging", F), use_module(F).
?- locate_module("tktools", F), use_module(F).
?- locate_module("check_indigolog", F), use_module(F).

%% :- use_module(library(util)).
%% %:- load("/home/tim/robotics/projects/esbl/bats/libBAT-kitchen.so").
%% ?- locate_file("libBAT-kitchen.so", F), load(F).
%% :- ["/home/tim/robotics/projects/esbl/bats/proper-plus.pl"].

%% % Load the ESBL interface:
%% :- load('/home/tim/robotics/projects/esbl/eclipse-clp/libEclipseESBL.so').
%% :- external(kcontext/1, p_kcontext).
%% :- external(bcontext/2, p_bcontext).
%% :- external(context_store/2, p_context_store).
%% :- external(context_retrieve/2, p_context_retrieve).
%% :- external(context_exec/4, p_context_exec).
%% :- external(context_entails/3, p_context_entails).

%% % Create a context and store it in an extra-logical variable:
%% :- kcontext(Ctx), store_context(kctx, Ctx).

% Now test the properties (some are taken from the KR-2014 paper, some are
% additional tests; they also match the kr2014.c example):
%:- retrieve_context(kctx, Ctx),
%   print("Got context\n"),
%   context_entails(Ctx, 1, p v ~p),
%   context_entails(Ctx, 1, p v ~p),
%   context_entails(Ctx, 1, p v ~p).


%% It is only possible to either run the agent or check for Golog
%% specific warnings.
%% If you want to see warnings include the predicate "debug_true" here.

debug_true :- fail.


:- dynamic update/1.
:- dynamic terminate/1.
:- dynamic verbose_mode/1.

:- dynamic place/1.
:- dynamic situation/1.
:- dynamic object_visible/2.
:- dynamic object_visible_processed/2.
:- dynamic object_types/2.
:- dynamic object_types_processed/2.
:- dynamic object_inspected/2.
:- dynamic object_inspected_processed/2.

% the following dynamic declarations are only needed
% for the use of the indigolog ckecker (check_indigolog.ecl).
:- if(debug_true).
:- dynamic execute/2.
:- dynamic prim_action/1.
:- dynamic poss/2.
:- dynamic prim_fluent/1.
:- dynamic initially/2.
:- dynamic senses/2.
:- dynamic causes_val/4.
:- dynamic proc/2.
:- endif.


:- local initialization(init).
:- local finalization(fin).

:- log_info("Loading hybris_2014 IndiGolog agent").

max_num_objects(20).
max_num_places(4).

%% event handlers
handle_update(update) :-
    %log_debug("Event: UPDATE"),
    asserta(update(update)).

handle_terminate(terminate) :-
    log_info("Event: TERMINATE"),
    (bb_send_message("SkillerInterface::Skiller", "ReleaseControlMessage", []) ; true),
    log_info("Released skiller control"),
    asserta(terminate(1)).
    %yield("terminate", A).
    %halt.

%% setup event handlers
:- set_event_handler(update, handle_update/1).
:- set_event_handler(terminate, handle_terminate/1).


open_object_interfaces :-
  max_num_objects(M),
  \+ (between(1,M,N),
      \+ (concat_string(["/percobj/", N], Id),
	  log_info("Opening interfaces Position3DInterface|MultiTypedObjectInterface::%s", [Id]),
	  bb_open_interface_reading("Position3DInterface", Id),
	  bb_open_interface_reading("MultiTypedObjectInterface", Id))),
  bb_open_interface_reading("MultiTypedObjectInterface", "/percobj/logo").


close_object_interfaces :-
  max_num_objects(M),
  \+ (between(1,M,N),
      \+ (concat_string(["Position3DInterface::/percobj/", N], PosId),
	  concat_string(["MultiTypedObjectInterface::/percobj/", N], MultiId),
	  log_info("Closing interface %s", [PosId]),
	  bb_close_interface(PosId),
	  log_info("Closing interface %s", [MultiId]),
	  bb_close_interface(MultiId))),
  bb_close_interface("MultiTypedObjectInterface::/percobj/logo").
  

%% initialisation and finalisation - only open all interfaces when on host caesar
%% otherwise use less interfaces to enable simulation on desktops
:- if(get_flag(hostname, "c1")).
% On c1:

%% initialisation: open all interfaces you want to use here,
%% gain skiller control
init :-
    bb_ensure_connected,!,
    bb_open_interface_reading("SkillerInterface","Skiller"),
    bb_open_interface_reading("Position3DInterface", "Pose"),
    open_object_interfaces,
    bb_read_interfaces,
    % acquire control to skiller
    (bb_send_message("SkillerInterface::Skiller", "AcquireControlMessage", [[steal_control,true]])
      -> log_info("Successfully acquired control")
      ;  log_warn("Failed to acquire exclusive control")
    ),
    %bb_send_message("Navigator", "ResetParametersMessage", []),
    asserta(update("initial")),
    sleep(0.1).

%% finalisation: close all interfaces & release skiller control
fin :-
    bb_send_message("SkillerInterface::Skiller", "ReleaseControlMessage", []) ; true,
    bb_close_interface("SkillerInterface::Skiller"),
    bb_close_interface("Position3DInterface::Pose"),
    close_object_interfaces,
    log_info("finalized hybris2014").

:- else.
% on desktops:

%% initialisation: open all interfaces you want to use here,
%% be aware that on dekstops not everything is possible,
%% gain skiller control
init :-
    bb_ensure_connected,!,
    bb_open_interface(r,"SkillerInterface","Skiller"),
    open_object_interfaces,
    bb_read_interfaces,
    % acquire control to skiller
    (bb_send_message("SkillerInterface::Skiller", "AcquireControlMessage", [[steal_control,true]])
      -> log_info("Successfully acquired control")
      ;  log_warn("Failed to acquire exclusive control")
    ),
    asserta(update("initial")),
    sleep(0.1).

%% finalisation: close all interfaces & release skiller control
fin :-
    bb_send_message("SkillerInterface::Skiller", "ReleaseControlMessage", []) ; true,
    close_object_interfaces,
    bb_close_interface("SkillerInterface::Skiller"),
    log_info("finalized hybris_2014").

:- endif.

% load navgraph
load_graph(Path) :- log_info("loading graph %s", [Path]), map_graph_load(Path).
%    log_debug("demo2014.ecl: navgraph map loaded."),
%    % define nodes as possible locations
%    map_graph_get_nodes(X),
%    define_locations(X).
%    %map_graph_search_nodes("TableNode", Res),
%    %define_tables(Res).


% define nodes of navgraph map as locations
define_locations([]).
define_locations([[Node|_]|Tail]) :- assert(place(Node)), define_locations(Tail).

define_places([]).
define_places([[Node|_]|Tail]) :- assert(place(Node)), log_info("Defining %w to be a place", [Node]), define_places(Tail).

:- if(getval(use_esl, true)).

update_objects_data :-
  log_info("Processing blackboard data, ESL version"),
  % iterate over all objects, use pattern \+ (generator(N), \+ process(N))
  \+ (object(N),
      log_info("Processing object interface %d", [N]),
      \+ (interface_changed(N)  % the interface has changed, therefore process it
	  -> (interface_object_visible(N)
	      -> log_info("Object %d is VISIBLE", [N]),
                 %retract(object_visible(N, _)), assert(object_visible(N, true)),
                 object_to_stdname(N, N_StdName),
                 esl_assert(object_visible(N_StdName)),
		 get_multitypes(N, T), object_types(N, OldT),
		 intersection(T, OldT, CommonT), length(CommonT, LCommonT), length(T, LT), length(OldT, LOldT),
		 join_string(T, " ", ST), join_string(OldT, " ", SOldT),
		 log_info("Object %d old types: %s  new types: %s", [N, SOldT, ST]),
		 ( (LCommonT =\= LT ; LCommonT =\= LOldT)
		   -> %retract(object_types(N, _)), assert(object_types(N, T)),
                      ( member("box", T) ->
                          %esl_assert(exists(t, box_type(t) ^ object_type(N_StdName, t)))
                          esl_assert(object_type(N_StdName, biscuit) v object_type(N_StdName, chocolate) v object_type(N_StdName, dummy))
                      ;
                          %esl_assert(forall(t, box_type(t) -> ~object_type(N_StdName, t)))
                          esl_assert(~object_type(N_StdName, biscuit) ^ ~object_type(N_StdName, chocolate) ^ ~object_type(N_StdName, dummy))
                      ),
		      retract(object_types_processed(N, _)), assert(object_types_processed(N, false))
		   ; true
		 )
	      ;
              %retract(object_visible(N, _)), assert(object_visible(N, false))
              true
	     ),
	     retract(object_visible_processed(N, _)), assert(object_visible_processed(N, false))
	  ; true
	 )
     ).

:- else.

% Possible types for object 1,2,... are stored in Types1,Types2,...
% If the visibility history is less or equal to zero, an empty list is stored in the according variable
update_objects_data :-
  log_info("Processing blackboard data"),
  % iterate over all objects, use pattern \+ (generator(N), \+ process(N))
  \+ (object(N),
      log_info("Processing object interface %d", [N]),
      \+ (interface_changed(N)  % the interface has changed, therefore process it
	  -> (interface_object_visible(N)
	      -> log_info("Object %d is VISIBLE", [N]),
		 retract(object_visible(N, _)), assert(object_visible(N, true)),
		 get_multitypes(N, T), object_types(N, OldT),
		 intersection(T, OldT, CommonT), length(CommonT, LCommonT), length(T, LT), length(OldT, LOldT),
		 join_string(T, " ", ST), join_string(OldT, " ", SOldT),
		 log_info("Object %d old types: %s  new types: %s", [N, SOldT, ST]),
		 ( (LCommonT =\= LT ; LCommonT =\= LOldT)
		   -> retract(object_types(N, _)), assert(object_types(N, T)),
		      retract(object_types_processed(N, _)), assert(object_types_processed(N, false))
		   ; true
		 )
	      ;
	      retract(object_visible(N, _)), assert(object_visible(N, false))
	     ),
	     retract(object_visible_processed(N, _)), assert(object_visible_processed(N, false))
	  ; true
	 )
     ).

:- endif.

interface_object_visible(N) :-
  concat_string(["Position3DInterface::/percobj/", N], Interface),
  bb_get(Interface, "visibility_history", Visible),
  Visible > 0.

interface_changed(N) :-
  concat_string(["Position3DInterface::/percobj/", N], Interface),
  bb_read_interface(Interface),
  bb_interface_changed(Interface),
  log_warn("Interface %s has changed", [Interface]),
  concat_string(["MultiTypedObjectInterface::/percobj/", N], TypeInterface),
  bb_read_interface(TypeInterface).


get_multitypes(N, []) :- \+ interface_object_visible(N).

get_multitypes(N, List ) :- 
  interface_object_visible(N),
  concat_string(["MultiTypedObjectInterface::/percobj/", N], Interface),
  %bb_read_interface(Interface),
  append_types(1, Interface, [], Res1),
  append_types(2, Interface, Res1, Res2),
  append_types(3, Interface, Res2, Res3),
  append_types(4, Interface, Res3, List),
  log_info("Object %w could be of following types: %w", [N, List]).


append_types(TypeId, Interface, List, Res) :-
  concat_string(["type_", TypeId], Field),
  bb_get(Interface, Field, T1),
  string_length(T1, Len),
  ( Len > 0 
    -> Res = [T1|List]
    ; Res = List
  ).
  


% this predicate is called at the start of the think-hook
% if an agent is writer to some interface, it should check for new messages here
check_for_msg.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%       definitions needed for IndiGolog       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

want_object("chocolate").

%place("start").
place(X) :- max_num_places(M), between(1,M,I), concat_string(["table1_loc", I, "_room1"], X).

object(N) :- max_num_objects(M), !, between(1, M, N).
?- \+ (object(N), \+ assert(object_visible(N, false))).
?- \+ (object(N), \+ assert(object_visible_processed(N, false))).
?- \+ (object(N), \+ assert(object_types(N, []))).
?- \+ (object(N), \+ assert(object_types_processed(N, false))).
?- \+ (object(N), \+ assert(object_inspected(N, false))).
?- \+ (object(N), \+ assert(object_inspected_processed(N, false))).

object_to_planner(N, M) :- M is N - 1.

perception_to_object("chocolateFront.jpg", "chocolate").
perception_to_object("chocolateBack.jpg", "chocolate").
perception_to_object("biscuitsFront.jpg", "biscuits").
perception_to_object("biscuitsBack.jpg", "biscuits").

%% auxiliary predicates to handle skiller status
is_running("S_RUNNING").
not_running("S_FINAL").
not_running("S_FAILED").
is_final("S_FINAL").
is_failed("S_FAILED").

wait_for_skiller :-
  repeat, sleep(0.1),
  bb_read_interface("SkillerInterface::Skiller"),
  bb_get("SkillerInterface::Skiller", "status", Status),
  not_running(Status).

wait_for_skiller(MsgId) :-
  repeat, sleep(0.1),
  bb_read_interface("SkillerInterface::Skiller"),
  bb_get("SkillerInterface::Skiller", "msgid", SkillerMsgId),
  SkillerMsgId = MsgId,
  bb_get("SkillerInterface::Skiller", "status", Status),
  not_running(Status).

decide_on_sensing(true) :- success.
decide_on_sensing(false) :- failed.

inverse_decide(false) :- success.
inverse_decide(true) :- failed.

% Set to not actually call skills but simulate them
%:- assert(fake_skills).

:- if(fake_skills).
?- log_warn("FAKING SKILLS").
success :- true.
failed  :- fail.

exec_skill_wait(Skill, Arguments) :-
  log_info("Faking execution of --- %s{%s} ---", [Skill, Arguments]),
  sleep(1.0).

:- else.
:- print("NOT FAKING SKILLS\n").
success :- bb_get("SkillerInterface::Skiller", "status", Status), is_final(Status).
failed  :- bb_get("SkillerInterface::Skiller", "status", Status), is_failed(Status).

%% auxiliary predicates to execute skills
exec_skill_wait(Skill, Arguments) :-
  log_info("Executing --- %s{%s} ---", [Skill, Arguments]),
  exec_skill(Skill, Arguments, MsgId),
  wait_for_skiller(MsgId).
:- endif.

exec_skill(Skill, Arguments) :-
  exec_skill(Skill, Arguments, _).

exec_skill(Skill, Arguments, MsgId) :-
  append_strings(Skill, "{", Str1),
  append_strings(Str1, Arguments, Str2),
  append_strings(Str2, "}", Str3),
  bb_send_message("SkillerInterface::Skiller", "ExecSkillMessage", [["skill_string", Str3]], MsgId).

exec_skill2(Skillmsg) :-
  bb_send_message("SkillerInterface::Skiller", "ExecSkillMessage", [["skill_string", Skillmsg]]).

%% constants

rotation_tolerance(0.15).

% use one of the following: mute, demo, debug
verbose_mode(demo).

%% Definitions what to do when an action should be executed
%% execute(action, sensing_result) :- prolog predicates describing what to do.

object_type_is_box([]) :- fail.
object_type_is_box([H|T]) :- H = "box" ; object_type_is_box(T).

object_type_is_wanted([], _) :- fail.
object_type_is_wanted([H|T], O) :- H = O ; object_type_is_wanted(T, O).


execute(update, Sr) :-
    log_debug("Executing: update"),
    check_for_msg,
    bb_read_interface("SkillerInterface::Skiller"),
    bb_get("SkillerInterface::Skiller", "status", Sr), log_debug("skiller status is: %w", [Sr]).

execute(wait_for_skiller, false) :-
    log_debug("wait..."),
    sleep(0.1),
    wait_for_skiller.

execute(read_skiller_status, Sr) :-
    log_info("Executing: read_skiller_status"),
    bb_get("SkillerInterface::Skiller", "status", Sr), log_debug("skiller status is: %w", [Sr]).

execute(drive_to(Node), Sr) :- 
    log_info("Executing: drive_to(%w)", [Node]),
    %  use_env_server=true"
    concat_string(["goal=\"(at-base ", Node, ")\""], Arg),
    log_warn("Drive to %s", [Node]),
    exec_skill_wait("planexec", Arg),
    log_error("DRIVE completed"),
    sleep(0.1),
    ( success, !, Sr=Node
      ;
      failed, !, Sr=""
    ).

:- if(getval(use_esl, true)).

execute(perceive_objects, false) :- 
    log_info("Executing: perceive_objects"),
    send_agent_info("Perceiving objects"),
    exec_skill_wait("perceive_objects", ""),
    % fake a wanted object
    %% (object_visible(1, false)
    %%  -> log_warn("Setting object to visible"),
    %% 	%retract(object_visible(1, _)), assert(object_visible(1, true)),
    %% 	esl_assert(object_visible(o1)),
    %% 	%%retract(object_inspected(1, _)), assert(object_inspected(1, true)),
    %% 	%retract(object_types(1, _)), assert(object_types(1, ["box"]))
    %%     %esl_assert(exists(t, box_type(t) ^ object_type(o1, t)))
    %%     esl_assert(object_type(o1, biscuit) v object_type(o1, chocolate) v object_type(o1, dummy))
    %%  ;
    %%  log_info("Object already visible")
    %% ),
    ( success, !, Sr=true, log_info("Perceiving objects succeeded"), update_objects_data
      ;
      failed, !, Sr=false, log_info("Perceiving objects failed")
    ).

:- else.

execute(perceive_objects, false) :- 
    log_info("Executing: perceive_objects"),
    exec_skill_wait("perceive_objects", ""),
    log_info("perceive_objects finished executing"),
    % fake a wanted object
    %% (object_visible(1, false)
    %%  -> log_warn("Setting object 1 to visible"),
    %% 	retract(object_visible(1, _)), assert(object_visible(1, true)),
    %% 	%retract(object_inspected(1, _)), assert(object_inspected(1, true)),
    %% 	retract(object_types(1, _)), assert(object_types(1, ["box"]))
    %%  ;
    %%  log_info("Object 1 already visible")
    %% ),
    %% (object_visible(2, false)
    %%  -> log_warn("Setting object 2 to visible"),
    %% 	retract(object_visible(2, _)), assert(object_visible(2, true)),
    %% 	%retract(object_inspected(2, _)), assert(object_inspected(2, true)),
    %% 	retract(object_types(2, _)), assert(object_types(2, ["box"]))
    %%  ;
    %%  log_info("Object 2 already visible")
    %% ),
    ( success, !, Sr=true, log_info("Perceiving objects succeeded"), update_objects_data
      ;
      failed, !, Sr=false, log_warn("Perceiving objects failed")
    ).

:- endif.

execute(deliver_object(N, Where), false) :-
  object_to_planner(N, NP),
  log_info("Delivering object %d (planner: %d) to %s", [N, NP, Where]),
  concat_string(["goal=\"(and (on obj_", NP, " ", Where, ") (arms-drive-pose))\""], Arg),
  exec_skill_wait("planexec", Arg),
  retract(object_visible(N, _)), assert(object_visible(N, false)),
  retract(object_visible_processed(N, _)), assert(object_visible_processed(N, false)),
  retract(object_inspected(N, _)), assert(object_inspected(N, false)),
  retract(object_inspected_processed(N, _)), assert(object_inspected_processed(N, false)),
  retract(object_types(N, _)), assert(object_types(N, [])),
  retract(object_types_processed(N, _)), assert(object_types_processed(N, false)),
  %wait_for_skiller,
  ( success, !, Sr=N
    ;
    failed, !, Sr=""
  ).

execute(pickup_object(N), Sr) :-
  object_to_planner(N, NP),
  log_info("Picking up object %d (planner: %d)", [N, NP]),
  concat_string(["goal=\"(and (object-inspectable obj_", NP, ") (grasped obj_", NP, " right_arm))\""], Arg),
  exec_skill_wait("planexec", Arg),
  ( success, !, Sr=N, log_info("Picking up %d succeeded", [N])
    ;
    failed, !, Sr=none, log_warn("Picking up %d failed", [N])
  ).

:- if(getval(use_esl, true)).

execute(inspect_object(N), Sr) :-
  log_info("Inspecting object %d", [N]),
  exec_skill_wait("inspect_logo", ""),
  object_to_stdname(N, N_StdName),
  %retract(object_inspected(N, _)), assert(object_inspected(N, true)),
  retract(object_inspected_processed(N, _)), assert(object_inspected_processed(N, false)),
  %retract(object_types(N, _)), assert(object_types(N, ["box", "chocolate"])),
  %retract(object_types_processed(N, _)), assert(object_types_processed(N, true)),
  %object_types(N, T), join_string(T, " ", S), log_info("Types now: %s", [S]),
  ( success, !,
    bb_read_interface("MultiTypedObjectInterface::/percobj/logo"),
    bb_get("MultiTypedObjectInterface::/percobj/logo", "type_1", LogoType),
    log_info("Read logo type '%s'", [LogoType]),
    %retract(object_types(N, _)), assert(object_types(N, [LogoType|T])),
    boxtype_to_stdname(LogoType, LogoType_StdName),
    esl_assert(object_type(N_StdName, LogoType_StdName)),
    Sr=LogoType
    ;
    failed, !, Sr="",
    esl_assert(~object_type(N_StdName, biscuit) ^ ~object_type(N_StdName, chocolate))
  ).

:- else.

execute(inspect_object(N), Sr) :-
  log_info("Inspecting object %d", [N]),
  exec_skill_wait("inspect_logo", ""),
  retract(object_inspected(N, _)), assert(object_inspected(N, true)),
  retract(object_inspected_processed(N, _)), assert(object_inspected_processed(N, false)),
  %retract(object_types(N, _)), assert(object_types(N, ["box", "chocolate"])),
  %retract(object_types_processed(N, _)), assert(object_types_processed(N, true)),
  object_types(N, T), join_string(T, " ", S), log_info("Types unmodified: %s", [S]),
  ( success, !,
    bb_read_interface("MultiTypedObjectInterface::/percobj/logo"),
    bb_get("MultiTypedObjectInterface::/percobj/logo", "type_1", LogoType),
    %LogoType="chocolate",
    perception_to_object(LogoType, LT),
    log_info("Read logo type %s", [LT]),
    join_string([LT|T], " ", SM), log_info("Types now: %s", [SM]),
    retract(object_types(N, _)), assert(object_types(N, [LT|T])),
    Sr=LT
    ;
    failed, !, Sr=""
  ).

:- endif.

execute(putdown_object(N, Where), false) :-
  object_to_planner(N, NP),
  log_info("Placing object %d (planner %d) at %s", [N, NP, Where]),
  concat_string(["goal=\"(and (on obj_", NP, " ", Where, ") (searched ", Where, "))\", use_env_server=true"], Arg),
  exec_skill_wait("planexec", Arg),
  ( success, !, log_info("Putting down %d at %s succeeded", [N, Where])
    ;
    failed, !, log_warn("Putting down %d at %s failed", [N, Where])
  ).

execute(sleep, false) :- log_debug("Executing: sleep"), sleep(0.5).

execute(sleep(N), false) :- log_info("Executing: sleep(%w)", [N]), sleep(N).

execute(print(S), false) :- log_info(S).

execute(print_var(S,V), false) :- log_info(S, [V]).

execute(restart, false) :- log_info("Executing: restart").

% this is more or less a "hack"-action, since it will change any fluent
% to any value without any pre-conditions, use as less as possible
% and with great care
execute(change_fluent(Fluent, Value), false) :- log_info("Executing: change_fluent(%w, %w)", [Fluent, Value]).


:- if(get_flag(hostname, "c1")).
execute(send_switch_msg(Iface, Msg), false) :-
    log_info("Executing: send_switch_msg(%w, %w)", [Iface, Msg]),
    bb_send_message(Iface, Msg, []),
    sleep(0.1).
:- else.
execute(send_switch_msg(Iface, Msg), false) :-
    log_info("Executing: send_switch_msg(%w, %w) - DEBUG! DOES NOTHING", [Iface, Msg]).
:- endif.


:- if(getval(use_esl, true)).

execute(reset_scene, false) :-
  % reset all current knowledge about objects
  \+ (object(N),
      \+ (log_info("Resetting object %d", N),
	  retract(object_visible(N, _)), assert(object_visible(N, false)),
	  retract(object_visible_processed(N, _)), assert(object_visible_processed(N, false)),
	  retract(object_inspected(N, _)), assert(object_inspected(N, false)),
	  retract(object_inspected_processed(N, _)), assert(object_inspected_processed(N, false)),
	  retract(object_types(N, _)), assert(object_types(N, [])),
	  retract(object_types_processed(N, _)), assert(object_types_processed(N, false)),
          esl_reset
     )),
  log_info("Calling reset_perception skill"),
  exec_skill_wait("reset_perception", ""),
  log_info("Will reset fluents on reset completion"),
  log_info("*** RESET COMPLETED ***").

:- else.

execute(reset_scene, false) :-
  % reset all current knowledge about objects
  \+ (object(N),
      \+ (log_info("Resetting object %d", N),
	  retract(object_visible(N, _)), assert(object_visible(N, false)),
	  retract(object_visible_processed(N, _)), assert(object_visible_processed(N, false)),
	  retract(object_inspected(N, _)), assert(object_inspected(N, false)),
	  retract(object_inspected_processed(N, _)), assert(object_inspected_processed(N, false)),
	  retract(object_types(N, _)), assert(object_types(N, [])),
	  retract(object_types_processed(N, _)), assert(object_types_processed(N, false))
     )),
  log_info("Calling reset_perception skill"),
  exec_skill_wait("reset_perception", ""),
  log_info("Will reset fluents on reset completion"),
  log_info("*** RESET COMPLETED ***").

:- endif.


execute(set_verbose(Mode), false) :-
    log_info("Setting verbose_mode to %w", [Mode]),
    retractall(verbose_mode(_)),
    assert(verbose_mode(Mode)).

% ask for sensing result on command line
% this is only a bug catch - if this appears,
% something with the action is most likely wrong.
%execute(A,Sr) :- ask_execute(A,Sr).

% Just to get rid of warnings
execute(start_interrupts, false) :- true.
execute(stop_interrupts, false) :- true.

:- if(\+ debug_true).
execute(A,_) :- (\+ senses(A, _), term_string(A, S),
		 log_error("Action %s does not have an execute function or failed", [S])) ; true.
:- endif.

%% exogenous actions
exog_occurs(req_update) :- update(Date), retract(update(Date)).

:- if(not(getval(use_esl, true))).

exog_occurs(object_seen(N)) :-
  object(N), object_visible_processed(N, false), object_visible(N, true),
  retract(object_visible_processed(N, false)), assert(object_visible_processed(N, true)),
  log_error("EXOG: object_seen(%d)", [N]).

exog_occurs(object_is_box(N)) :-
  object(N), object_types_processed(N, false), object_types(N, T), object_type_is_box(T),
  retract(object_types_processed(N, false)), assert(object_types_processed(N, true)),
  log_error("EXOG: object_is_box (%d)", [N]).

exog_occurs(object_is_wanted(N)) :-
  object(N), object_inspected_processed(N, false), object_inspected(N, true), object_types(N, T),
  want_object(O), object_type_is_wanted(T, O),
  retract(object_inspected_processed(N, false)), assert(object_inspected_processed(N, true)),
  log_error("EXOG: object_is_wanted(%d)", [N]).

:- endif.

exog_occurs(_) :- fail.

exog_action(req_update).


:- if(not(getval(use_esl, true))).

exog_action(object_seen(N)).
exog_action(object_is_box(N)).
exog_action(object_is_wanted(N)).

:- endif.


%% primitive actions
prim_action(drive_to(N)) :- place(N).
prim_action(perceive_objects).
prim_action(read_skiller_status).
prim_action(sleep).
prim_action(sleep(N)).
prim_action(print(S)).
prim_action(print_var(S,V)).
prim_action(restart).
prim_action(update).
prim_action(change_fluent(Fluent, Value)).
prim_action(wait_for_skiller).
prim_action(send_switch_msg(Iface, Msg)).
prim_action(set_verbose(Mode)).
%prim_action(grab_box).
prim_action(reset_scene).
prim_action(deliver_object(N, Where)) :- object(N).
prim_action(inspect_object(N)) :- object(N).
prim_action(pickup_object(N)) :- object(N).
prim_action(putdown_object(N, Where)) :- object(N).


%% fluents
prim_fluent(at).
prim_fluent(skiller_status).
prim_fluent(holding).
prim_fluent(ignore_status).
prim_fluent(up_to_date).
prim_fluent(intro).
prim_fluent(num_runs).
prim_fluent(error(E)).
prim_fluent(exec_once).
prim_fluent(place_visited(P)).
prim_fluent(place_explored(P)).
prim_fluent(reset_places).
prim_fluent(obj_inspected(N, Where)).
prim_fluent(obj_pickedup(N)).
:- if(not(getval(use_esl, true))).
prim_fluent(obj_exists(N)).
prim_fluent(obj_is_box(N)).
prim_fluent(obj_is_wanted(N)).
:- endif.
prim_fluent(obj_delivered(N)).
%prim_fluent(object_visible(N)).
%prim_fluent(object_types(N,T)).


%% sensing
senses(read_skiller_status, skiller_status).
senses(update, skiller_status).
senses(drive_to(N), at).
senses(pickup_object(N), holding).
%senses(grab_box, holding_box).

%% causal laws
causes_val(change_fluent(Fluent, Value), Fluent, Value, true).

%causes_val(drive_to(_), detected_box, false, true).
%causes_val(drive_to(_), close_to_table, false, true).
causes_val(drive_to(N), place_visited(N), true, true).

causes_val(req_update, up_to_date, false, true).
causes_val(update, up_to_date, true, true).

causes_val(reset_scene, num_runs, N, N is num_runs+1  ).
causes_val(reset_scene, reset_places, true, true).
causes_val(reset_scene, obj_exists(N), false, true) :- object(N).
causes_val(reset_scene, obj_is_box(N), false, true) :- object(N).
causes_val(reset_scene, obj_inspected(N, Where), false, true) :- object(N), place(Where).
causes_val(reset_scene, obj_pickedup(N), false, true) :- object(N).
causes_val(reset_scene, obj_is_wanted(N), false, true) :- object(N).
causes_val(reset_scene, obj_delivered(N), false, true) :- object(N).
causes_val(reset_scene, place_visited(N), false, true) :- place(N).
causes_val(reset_scene, place_explored(N), false, true) :- place(N).

% skiller status should be ignored for the first action after a restart
causes_val(restart, ignore_status, true, true).
causes_val(drive_to(N), ignore_status, false, true).
%causes_val(grab_box, ignore_status, false, true).
%

:- if(not(getval(use_esl, true))).
causes_val(deliver_object(N, Where), obj_is_wanted(N), false, true).
causes_val(deliver_object(N, Where), obj_is_box(N), false, true).
:- endif.
causes_val(deliver_object(N, Where), obj_inspected(N, AnyWhere), false, true) :- place(AnyWhere).
causes_val(deliver_object(N, Where), obj_delivered(N), true, true).
%causes_val(inspect_object(N, Where), obj_inspected(N, Where), true, true).

:- if(not(getval(use_esl, true))).
causes_val(object_seen(N),   obj_exists(N), true, true).
causes_val(object_is_box(N), obj_is_box(N), true, true).
causes_val(object_is_wanted(N), obj_is_wanted(N), true, true).
:- endif.

causes_val(putdown_object(N, Where), holding, none, true).


%% preconditions
% currently most actions are just possible, preconditions are
% modelled to a great extent in the agents procedure
poss(change_fluent(Fluent, Value),true).
poss(drive_to(N), and(place(N), executable) ).
poss(read_skiller_status, up_to_date=true).
poss(sleep, executable).
poss(sleep(N), true).
poss(print(S), true).
poss(print_var(S,V), true).
poss(wait_for_skiller, true).
poss(send_switch_msg(I,M), true).
%poss(grab_box, and(executable, holding_box=false)).
poss(set_verbose(Mode), true).
poss(perceive_objects, true).
poss(deliver_object(N, Where), and(object(N), obj_is_wanted(N))).
poss(inspect_object(N), and(holding=N, and(object(N), obj_is_box(N)))).
poss(pickup_object(N), and(holding=none, and(object(N), obj_is_box(N)))).
poss(putdown_object(N, Where), and(holding=N, and(object(N), obj_is_box(N)))).

poss(reset_scene, true).

poss(update, true).
poss(restart, true).

%% initial state
initially(at, "start").
initially(holding, none).
initially(ignore_status, false).
initially(intro, false).
initially(num_runs, 1).
initially(error(no_boxs_detected), false).
initially(error(grab_failed), false).
initially(exec_once, true).
initially(up_to_date, false).
initially(skiller_status, "S_INACTIVE").
initially(place_visited(N), false) :- place(N).
initially(place_explored(N), false) :- place(N).
initially(reset_places, false).
:- if(not(getval(use_esl, true))).
initially(obj_exists(N), false) :- object(N).
initially(obj_is_box(N), false) :- object(N).
initially(obj_is_wanted(N), false) :- object(N).
:- endif.
initially(obj_inspected(N, Where), false) :- object(N), place(Where).
initially(obj_is_delivered(N), false) :- object(N).
initially(obj_pickedup(N), false) :- object(N).

:- if(getval(use_esl, true)).

obj_exists(N) :-
   object_to_stdname(N, N_StdName),
   esl_holds(1, object_visible(N_StdName)).
obj_is_box(N) :-
   object_to_stdname(N, N_StdName),
   esl_holds(1, object_type(N_StdName, biscuit) v object_type(N_StdName, chocolate) v object_type(N_StdName, dummy)).
obj_is_wanted(N) :-
   object_to_stdname(N, N_StdName),
   want_object(O),
   boxtype_to_stdname(O, O_StdName),
   esl_holds(1, object_type(N_StdName, O_StdName)).

:- endif.

%% definition of complex conditions
proc(inactive, skiller_status = "S_INACTIVE").
proc(running, skiller_status = "S_RUNNING").
proc(final, skiller_status = "S_FINAL").
proc(failed, skiller_status = "S_FAILED").
proc(executable, or(inactive, or(final, ignore_status=true)) ).

proc(at_place(N), and(at=N, place(N))).
proc(not_at_place(N), and(neg(at=N), place(N))).

proc(next_action_goto_counter, and(at_place("start"), and(executable, holding=none))).

proc(next_action_explore(N), and(holding=none, and(at_place(N), neg(place_explored(N))))).

proc(next_action_move_on(N), and(holding=none, and(place(N), neg(place_visited(N))))).

proc(next_action_reset_place(N), and(reset_places=true, and(place(N), and(place_visited(N)=true, place_explored(N)=true)))).

proc(next_action_deliver(N), and(object(N), and(obj_is_wanted(N), holding=N))).
proc(next_action_putback(N), and(object(N), and(holding=N, and(neg(obj_is_wanted(N)), obj_pickedup(N))))).

proc(next_action_inspect(N),
     and(holding=none, and(object(N), and(obj_is_box(N), and(at_place(Where),
     and(neg(obj_inspected(N, Where)), neg(obj_pickedup(N)))))))).

proc(should_exit, terminate(1)).

%proc(drive_to_place, [search(pi(n, [?(place(n)), drive_to(n)]))]).

/* Main procedure running the agent.
 *
 * Interrupts are designed that way, that they will be executed from top to bottom
 * and then again from top. An exception are the interrupts that trigger updates,
 * sleeping and restart.
 *
*/
proc(control, prioritized_interrupts(
    [interrupt(up_to_date=false, update),
     interrupt(running, sleep),
     interrupt(should_exit, stop_interrupts),

     % put things to exec once initially here
     interrupt(exec_once=true,
	       [print("***** Agent startup, resetting scene *****"),
		reset_scene, change_fluent(exec_once,false)]),

     interrupt(and(neg(verbose_mode(mute)), num_runs=2), set_verbose(mute)),

     interrupt(n, obj_delivered(n),
	       [print("Object delivered, I'm done."), sleep(5.0)]),

     %% Resetting of places after a run
     %interrupt(n, next_action_reset_place(n),
	%       [print_var("Resetting place %s", n),
	%	change_fluent(place_visited(n), false), change_fluent(place_explored(n), false)]),

     % for testing, stop after single cycle
     %interrupt(reset_places=true, [sleep]),

     %interrupt(reset_places=true,
%	       [print("Resetting places done"),
%		change_fluent(reset_places, false), change_fluent(at, "start")]),

     % if an action failed, ignore the status of the skiller and retry
     interrupt(and(failed, ignore_status=false), restart),

     /* Action flow
      * goto counter
      * perceive
      * check object availability
      *  -> available: fetch and carry
      *  -> otherwise: can acquire more information?
      *     -> Yes: acquire, goto perceive
      *     -> No: fail, possibly move to next table
      */

     
     interrupt(n, next_action_deliver(n),
	       [print_var("FOUND wanted object %d", n),
		deliver_object(n, "table2")]),

     interrupt(n, next_action_putback(n),
	       [print_var("Placing back unwanted object %d", n),
		putdown_object(n, "table1")]),

     % The if is required here, even (or because) holding is in poss(inspect_object(N)).
     % Otherwise, if pickup failed, and consequently holding=none poss would fail, which
     % somehow leaves the interrupt stale and would never revisit.
     interrupt(n, next_action_inspect(n),
	       [print_var("Picking up and inspecting %d", n),
		pi(w, [?(at_place(w)), change_fluent(obj_inspected(n, w), true)]),
		pickup_object(n),
		if(holding\=none, [inspect_object(n), change_fluent(obj_pickedup(n), true)], [])]),

     %interrupt(next_action_goto_counter,
	%       [print("Moving to start position"),
	%	drive_to("table1_loc1_room1"), change_fluent(place_visited("table1_loc1_room1"), true)]),

     % run basic perception if we do not know any object worth of
     % closer inspection, yet.
     interrupt(n, next_action_explore(n),
	       [print_var("Perceiving objects at %s", n),
		change_fluent(place_explored(n), true),
		perceive_objects]),

     % run basic perception if we do not know any object worth of
     % closer inspection, yet.
     interrupt(n2, next_action_move_on(n2),
	       [print_var("Moving on to %s", n2),
		drive_to(n2)]),

     % fail, cannot find object
%     interrupt(n, and(at_place("counter"), neg(positions_left(n))),
%	       [restart]),

     % at the counter, having an object which is of type box, but it
     % is yet unknown which specific one it is -> inspect
%     interrupt(and(at_place("counter"),and(holding_box=false, esl)),
%	       [generate_goal_inspect(ObjID, Goal), plan_exec(Goal)]),

     % at the counter, picked up an object, and determined it to be
     % the wrong one -> put down and go on
%     interrupt(and(at_place("counter"),and(holding_box=true, esl)),
%	       [generate_goal_put_down(ObjID, Goal), plan_exec(Goal)]),

     % at the counter, picked up an object, and determined it to be
     % the correct one -> bring to table
%     interrupt(and(at_place("counter"),and(holding_box=true, esl)),
%	       [generate_goal_deliver(ObjID, Goal), plan_exec(Goal)]),

     interrupt(true,
	       [print("***** Run completed, resetting scene *****"),
		reset_scene]),

     interrupt(true, sleep)
    ])).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%        End of IndiGolog definitions          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- if(debug_true).
% check for Golog syntax (experimental, only definitions
% of actions and fluents are currenty checked)
run :- basic_check, !, repeat, fail.
:- else.
% the acutal program being performed (called from eclipse_thread.cpp)
run :-
    (
        %basic_check, halt,
        %bb_read_interfaces,
        %check_for_msg,
        run_agent_once,
        % loop infinitly, needed for proper unloading
        repeat,
        fail
    ).
:- endif.



run_agent_once :-
    log_info("IndiGolog agent start"),
    % start the desire IndiGolog procedure
    indigolog(control),
    log_info("IndiGolog agent finished").


:- log_info("Loading hybris_2014 IndiGolog agent done").

