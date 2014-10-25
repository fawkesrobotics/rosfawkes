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

max_num_objects(5).
max_num_places(4).

%% event handlers
handle_update(update) :-
    %log_debug("Event: UPDATE"),
    asserta(update(update)).

handle_terminate(terminate) :-
    log_info("Event: TERMINATE"),
    bb_send_message("SkillerInterface::Skiller", "ReleaseControlMessage", []) ; true,
    log_info("Released skiller control"),
    asserta(terminate(1)),
    yield("terminate", A).
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
	  bb_open_interface_reading("MultiTypedObjectInterface", Id))).

close_object_interfaces :-
  max_num_objects(M),
  \+ (between(1,M,N),
      \+ (concat_string(["Position3DInterface::/percobj/", N], PosId),
	  concat_string(["MultiTypedObjectInterface::/percobj/", N], MultiId),
	  log_info("Closing interface %s", [PosId]),
	  bb_close_interface(PosId),
	  log_info("Closing interface %s", [MultiId]),
	  bb_close_interface(MultiId))).
  

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

% Possible types for object 1,2,... are stored in Types1,Types2,...
% If the visibility history is less or equal to zero, an empty list is stored in the according variable
update_objects_data :-
    bb_read_interface("Position3DInterface::/percobj/1"),
    bb_read_interface("Position3DInterface::/percobj/2"),
    bb_read_interface("Position3DInterface::/percobj/3"),
    bb_read_interface("Position3DInterface::/percobj/4"),
    bb_read_interface("Position3DInterface::/percobj/5"),
    bb_read_interface("MultiTypedObjectInterface::/percobj/1"),
    bb_read_interface("MultiTypedObjectInterface::/percobj/2"),
    bb_read_interface("MultiTypedObjectInterface::/percobj/3"),
    bb_read_interface("MultiTypedObjectInterface::/percobj/4"),
    bb_read_interface("MultiTypedObjectInterface::/percobj/5"),
    bb_get("Position3DInterface::/percobj/1", "visibility_history", V1),
    log_info("Object 1 visiblity history %d", [V1]),
    get_multitypes(1, V1, Types1),
    bb_get("Position3DInterface::/percobj/2", "visibility_history", V2),
    log_info("Object 2 visiblity history %d", [V2]),
    get_multitypes(2, V2, Types2),
    bb_get("Position3DInterface::/percobj/3", "visibility_history", V3),
    log_info("Object 3 visiblity history %d", [V3]),
    get_multitypes(3, V3, Types3),
    bb_get("Position3DInterface::/percobj/4", "visibility_history", V4),
    log_info("Object 4 visiblity history %d", [V4]),
    get_multitypes(4, V4, Types4),
    bb_get("Position3DInterface::/percobj/5", "visibility_history", V5),
    log_info("Object 5 visiblity history %d", [V5]),
    get_multitypes(5, V5, Types5),
    log_warn("READ ALL INTERFACES\n").

get_multitypes(ObjNr, Visible, []) :-
  Visible =< 0.

get_multitypes(ObjNr, Visible, List ) :- 
  Visible > 0,
  concat_string(["MultiTypedObjectInterface::/percobj/", ObjNr], Interface),
  %bb_read_interface(Interface),
  append_types(1, Interface, [], Res1),
  append_types(2, Interface, Res1, Res2),
  append_types(3, Interface, Res2, Res3),
  append_types(4, Interface, Res3, List),
  log_info("Object %w could be of following types: %w", [ObjNr, List]).


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


%% auxiliary predicates to handle skiller status
is_running("S_RUNNING").
not_running("S_FINAL").
not_running("S_FAILED").
is_final("S_FINAL").
is_failed("S_FAILED").

wait_for_skiller :-
    bb_read_interfaces,
    bb_get("SkillerInterface::Skiller", "status", Status),
    is_running(Status), sleep(0.1),
    wait_for_skiller.
wait_for_skiller :-
    bb_get("SkillerInterface::Skiller", "status", Status),
    not_running(Status).

success :- bb_get("SkillerInterface::Skiller", "status", Status), is_final(Status).
failed :- bb_get("SkillerInterface::Skiller", "status", Status), is_failed(Status).

decide_on_sensing(true) :- success.
decide_on_sensing(false) :- failed.

inverse_decide(false) :- success.
inverse_decide(true) :- failed.

%% auxiliary predicates to execute skills
exec_skill(Skill, Arguments) :-
    append_strings(Skill, "{", Str1),
    append_strings(Str1, Arguments, Str2),
    append_strings(Str2, "}", Str3),
    bb_send_message("SkillerInterface::Skiller", "ExecSkillMessage", [["skill_string", Str3]]).

exec_skill2(Skillmsg) :- bb_send_message("SkillerInterface::Skiller", "ExecSkillMessage", [["skill_string", Skillmsg]]).

%% constants

rotation_tolerance(0.15).

% use one of the following: mute, demo, debug
verbose_mode(demo).

%% Definitions what to do when an action should be executed
%% execute(action, sensing_result) :- prolog predicates describing what to do.

execute(update, Sr) :-
    log_debug("Executing: update"),
    bb_read_interfaces,
    check_for_msg,
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
    concat_string(["goal=\"(at-base ", Node, ")\", use_env_server=true"], Arg),
    exec_skill("planexec", Arg),
    log_info("Drive to %s", [Node]),
    sleep(0.1),
    wait_for_skiller,
    ( success, !, Sr=Node
      ;
      failed, !, Sr=""
    ).

execute(perceive_objects, false) :- 
    log_info("Executing: perceive_objects"),
    exec_skill("perceive_objects", ""),
    sleep(0.1),
    wait_for_skiller,
    log_info("Perceiving objects"),
    ( success, !, Sr=true, print("Perceiving objects succeeded"), update_objects_data
      ;
      failed, !, Sr=false, print("Perceiving objects failed")
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


execute(end_of_run, false) :- log_info("One run completed!").

execute(set_verbose(Mode), false) :-
    log_info("Setting verbose_mode to %w", [Mode]),
    retractall(verbose_mode(_)),
    assert(verbose_mode(Mode)).

% ask for sensing result on command line
% this is only a bug catch - if this appears,
% something with the action is most likely wrong.
execute(A,Sr) :- ask_execute(A,Sr).

%% exogenous actions
exog_occurs(req_update) :- update(Date), retract(update(Date)).
exog_occurs(_) :- fail.

exog_action(req_update).


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
prim_action(end_of_run).
prim_action(set_verbose(Mode)).
prim_action(grab_box).


%% fluents
prim_fluent(at).
prim_fluent(skiller_status).
prim_fluent(holding_box).
prim_fluent(ignore_status).
prim_fluent(up_to_date).
prim_fluent(intro).
prim_fluent(num_runs).
prim_fluent(error(E)).
prim_fluent(exec_once).
prim_fluent(place_visited(P)).
prim_fluent(place_explored(P)).
prim_fluent(reset_places).


%% sensing
senses(read_skiller_status, skiller_status).
senses(update, skiller_status).
senses(drive_to(N), at).
senses(grab_box, holding_box).

%% causal laws
causes_val(change_fluent(Fluent, Value), Fluent, Value, true).

%causes_val(drive_to(_), detected_box, false, true).
%causes_val(drive_to(_), close_to_table, false, true).
%causes_val(drive_to(_), rotatation_reached, false, true).

causes_val(req_update, up_to_date, false, true).
causes_val(update, up_to_date, true, true).

causes_val(end_of_run, num_runs, N, N is num_runs+1  ).

% skiller status should be ignored for the first action after a restart
causes_val(restart, ignore_status, true, true).
causes_val(drive_to(N), ignore_status, false, true).
causes_val(grab_box, ignore_status, false, true).

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
poss(grab_box, and(executable, holding_box=false)).
poss(set_verbose(Mode), true).
poss(end_of_run, true).
poss(perceive_objects, true).

poss(update, true).
poss(restart, true).


%% initial state
initially(at, "start").
initially(holding_box, false).
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

%% definition of complex conditions
proc(inactive, skiller_status = "S_INACTIVE").
proc(running, skiller_status = "S_RUNNING").
proc(final, skiller_status = "S_FINAL").
proc(failed, skiller_status = "S_FAILED").
proc(executable, or(inactive, or(final, ignore_status=true)) ).

proc(at_place(N), and(at=N, place(N))).
proc(not_at_place(N), and(neg(at=N), place(N))).

proc(next_action_goto_counter, and(at_place("start"), and(executable, holding_box=false))).

proc(next_action_explore(N), and(at_place(N), neg(place_explored(N)))).

proc(next_action_move_on(N), and(place(N), neg(place_visited(N)))).

proc(next_action_reset_place(N), and(reset_places=true, and(place(N), and(place_visited(N)=true, place_explored(N)=true)))).

%proc(next_action_restart, neg(and(place(N), and(place_visited(N)=true, place_explored(N)=true)))).

proc(next_action_goto_place(N), and(not_at_place(N), and(executable, and(holding_box=true, at_place("counter"))))).

proc(positions_left(N),neg(place_visited(N))).

proc(drive_to_place, [search(pi(n, [?(place(n)), drive_to(n)]))]).

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
     
     % put things to exec once initially here
     %interrupt(exec_once=true,
	%[print("Executing perceive_objects"),
	% perceive_objects,
	% change_fluent(exec_once,false)]),

     interrupt(and(neg(verbose_mode(mute)), num_runs=2), set_verbose(mute)),

     %% Resetting of places after a run
     interrupt(n, next_action_reset_place(n),
	       [print_var("Resetting place %s", n),
		change_fluent(place_visited(n), false), change_fluent(place_explored(n), false)]),

     interrupt(reset_places=true,
	       [print("Resetting places done"),
		change_fluent(reset_places, false), change_fluent(at, "start")]),

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

     interrupt(next_action_goto_counter,
	       [print("Moving to start position"),
		drive_to("table1_loc1_room1"), change_fluent(place_visited("table1_loc1_room1"), true)]),

     % run basic perception if we do not know any object worth of
     % closer inspection, yet.
     interrupt(n, next_action_explore(n),
	       [print_var("Perceiving objects at %s", n),
		perceive_objects, change_fluent(place_explored(n), true)]),

     % run basic perception if we do not know any object worth of
     % closer inspection, yet.
     interrupt(n2, next_action_move_on(n2),
	       [print_var("Moving on to %s", n2),
		drive_to(n2), change_fluent(place_visited(n2), true)]),

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
	       [print("***** Run completed, resetting places *****"),
		change_fluent(reset_places, true)]),

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
        bb_read_interfaces,
        check_for_msg,
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

