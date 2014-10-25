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

:- dynamic location/1.
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
:- endif.


:- local initialization(init).
:- local finalization(fin).

:- log_info("Loading hybris_2014 IndiGolog agent").


%% event handlers
handle_update(update) :-
    %log_debug("Event: UPDATE"),
    asserta(update(update)).

handle_terminate(terminate) :-
    log_info("Event: TERMINATE"),
    bb_send_message("SkillerInterface::Skiller", "ReleaseControlMessage", []),
    log_info("Released skiller control"),
    asserta(terminate(1)),
    yield("terminate", A).
    %halt.

%% setup event handlers
:- set_event_handler(update, handle_update/1).
:- set_event_handler(terminate, handle_terminate/1).


%% initialisation and finalisation - only open all interfaces when on host caesar
%% otherwise use less interfaces to enable simulation on desktops
:- if(get_flag(hostname, "c1")).
% On c1:

%% initialisation: open all interfaces you want to use here,
%% gain skiller control
init :-
    bb_ensure_connected,!,
    bb_open_interface_reading("SkillerInterface","Skiller"),
    bb_open_interface_reading("Position3DInterface","/percobj/1"),
    bb_open_interface_reading("Position3DInterface","/percobj/2"),
    bb_open_interface_reading("Position3DInterface","/percobj/3"),
    bb_open_interface_reading("Position3DInterface","/percobj/4"),
    bb_open_interface_reading("Position3DInterface","/percobj/5"),
    bb_open_interface_reading("MultiTypedObjectInterface","/percobj/1"),
    bb_open_interface_reading("MultiTypedObjectInterface","/percobj/2"),
    bb_open_interface_reading("MultiTypedObjectInterface","/percobj/3"),
    bb_open_interface_reading("MultiTypedObjectInterface","/percobj/4"),
    bb_open_interface_reading("MultiTypedObjectInterface","/percobj/5"),
    bb_open_interface_reading("Position3DInterface", "Pose"),
    bb_read_interfaces,
    % acquire control to skiller
    bb_send_message("SkillerInterface::Skiller", "AcquireControlMessage", [[steal_control,true]]),
    log_debug("hybris_2014.ecl: acquired control."),
    %bb_send_message("Navigator", "ResetParametersMessage", []),
    asserta(update("initial")),
    sleep(0.1).

%% finalisation: close all interfaces & release skiller control
fin :-
    bb_send_message("SkillerInterface::Skiller", "ReleaseControlMessage", []),
    bb_close_interface("SkillerInterface::Skiller"),
    bb_close_interface("Position3DInterface::Pose"),
    bb_close_interface("Position3DInterface::/percobj/1"),
    bb_close_interface("Position3DInterface::/percobj/2"),
    bb_close_interface("Position3DInterface::/percobj/3"),
    bb_close_interface("Position3DInterface::/percobj/4"),
    bb_close_interface("Position3DInterface::/percobj/5"),
    bb_close_interface("MultiTypedObjectInterface::/percobj/1"),
    bb_close_interface("MultiTypedObjectInterface::/percobj/2"),
    bb_close_interface("MultiTypedObjectInterface::/percobj/3"),
    bb_close_interface("MultiTypedObjectInterface::/percobj/4"),
    bb_close_interface("MultiTypedObjectInterface::/percobj/5"),
    log_info("finalized hybris2014").

:- else.
% on desktops:

%% initialisation: open all interfaces you want to use here,
%% be aware that on dekstops not everything is possible,
%% gain skiller control
init :-
    bb_ensure_connected,!,
    bb_open_interface(r,"SkillerInterface","Skiller"),
    bb_read_interfaces,
    % acquire control to skiller
    bb_send_message("SkillerInterface::Skiller", "AcquireControlMessage", [[steal_control,true]]),
    log_debug("demo2014.ecl: acquired control."),
    bb_open_interface(r,"Position3DInterface","/percobj/1"),
    bb_open_interface(r,"Position3DInterface","/percobj/2"),
    bb_open_interface(r,"Position3DInterface","/percobj/3"),
    bb_open_interface(r,"Position3DInterface","/percobj/4"),
    bb_open_interface(r,"Position3DInterface","/percobj/5"),
    bb_open_interface(r,"MultiTypedObjectInterface","/percobj/1"),
    bb_open_interface(r,"MultiTypedObjectInterface","/percobj/2"),
    bb_open_interface(r,"MultiTypedObjectInterface","/percobj/3"),
    bb_open_interface(r,"MultiTypedObjectInterface","/percobj/4"),
    bb_open_interface(r,"MultiTypedObjectInterface","/percobj/5"),
    asserta(update("initial")),
    sleep(0.1).

%% finalisation: close all interfaces & release skiller control
fin :-
    bb_send_message("SkillerInterface::Skiller", "ReleaseControlMessage", []),
    bb_close_interface("SkillerInterface::Skiller"),
    log_info("finalized hybris_2014").

:- endif.

% load navgraph
load_graph(Path) :- log_info("loading graph %s", [Path]), map_graph_load(Path),
    log_debug("demo2014.ecl: navgraph map loaded."),
    % define nodes as possible locations
    map_graph_get_nodes(X),
    define_locations(X).
    %map_graph_search_nodes("TableNode", Res),
    %define_tables(Res).


% define nodes of navgraph map as locations
define_locations([]).
define_locations([[Node|_]|Tail]) :- assert(location(Node)), define_locations(Tail).

define_tables([]).
define_tables([[Node|_]|Tail]) :- assert(table(Node)), log_info("Defining %w to be a table", [Node]), define_tables(Tail).


% this predicate is called at the start of the think-hook
% if an agent is writer to some interface, it should check for new messages here
check_for_msg.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%       definitions needed for IndiGolog       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

want_object("chocolate").
table("counter").
table("dinnertable").

location(L) :- table_pos("counter", L).

table_pos("counter", "table1_loc1_room1").
table_pos("counter", "table1_loc2_room1").
table_pos("counter", "table1_loc3_room1").
table_pos("counter", "table1_loc4_room1").

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
spot("C").
spot("A").
spot("B").

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
    sleep(0.1),
    wait_for_skiller,
    ( success, !, Sr=Node
        ;
       failed, !, Sr=""
    ).

execute(sleep, false) :- log_debug("Executing: sleep"), sleep(0.5).

execute(sleep(N), false) :- log_info("Executing: sleep(%w)", [N]), sleep(N).

execute(print(S), false) :- log_info(S).

execute(print_var(S,V), false) :- log_info(S, [V]).

execute(restart, false) :- log_info("Executing: restart").

execute(say(S, true, mutable), false) :-
    verbose_mode(mute)
    ;
    (
        log_info("Executing: say(%w) with wait", [S]),
        concat_string(["text=", "\"", S, "\", wait=true"], Arg),
        %exec_skill("say", Arg),
        sleep(0.1)
    ).
execute(say(S, _, mutable), false) :-
    verbose_mode(mute)
    ;
    (
        log_info("Executing: say(%w) without wait", [S]),
        concat_string(["text=", "\"", S, "\""], Arg),
        %exec_skill("say", Arg),
        sleep(0.1)
    ).
execute(say(S, true, unmutable), false) :-
    log_info("Executing: say(%w) with wait", [S]),
    concat_string(["text=", "\"", S, "\", wait=true"], Arg),
    %exec_skill("say", Arg),
    sleep(0.1).
execute(say(S, _, unmutable), false) :-
    log_info("Executing: say(%w) without wait", [S]),
    concat_string(["text=", "\"", S, "\""], Arg),
    %exec_skill("say", Arg),
    sleep(0.1).


execute(plan_exec(Goal), false) :-
    log_info("Executing: plan_exec(goal=\"%w\")", [Goal]),
    concat_string(["goal=", "\"", Goal, "\", use_env_server=true"], Arg),
    exec_skill("plan_exec", Arg),
    sleep(0.1).

execute(detect_box, Sr):- 
    log_info("Executing: detect_box"),
    exec_skill("cup_detect", ""),
    sleep(0.1),
    wait_for_skiller,
    decide_on_sensing(Sr).

execute(grab_box, Sr) :- 
    log_info("Executing: grab_box"),
    exec_skill("katana_pick_and_grab", "id=1"),
    sleep(0.1),
    wait_for_skiller,
    decide_on_sensing(Sr).

execute(put_box_in_dishwasher(Spot, side), Sr) :-
    log_info("Executing: put_box_in_dishwasher(%w, side)", [Spot]),
    concat_string(["spot=\"", Spot, "\", pos=\"side\""], Arg),
    exec_skill("katana_drop_in_dishwasher", Arg),
    sleep(0.1),
    wait_for_skiller,
    decide_on_sensing(Sr).
execute(put_box_in_dishwasher(Spot, front), Sr) :-
    log_info("Executing: put_box_in_dishwasher(%w, front)", [Spot]),
    concat_string(["spot=\"", Spot, "\""], Arg),
    exec_skill("katana_drop_in_dishwasher", Arg),
    sleep(0.1),
    wait_for_skiller,
    decide_on_sensing(Sr).

execute(align_to_table, Sr) :- 
    log_info("Executing: align_to_table"),
    exec_skill("align_at_table", ""), % align_at_table{dist=DIST}, optional default 0.2
    sleep(0.1),
    wait_for_skiller,
    decide_on_sensing(Sr).

execute(backoff_from_table, false) :- 
    log_info("Executing: backoff_from_table"),
    exec_skill("back_off", "dist=0.4"), % back_off{dist=DIST}, optional default 0.2
    sleep(0.1),
    wait_for_skiller,
    inverse_decide(Sr).

:- if(get_flag(hostname, "c1")).
execute(sense_tilt, Sr) :-
    log_info("Sensing tilt value"),
    bb_read_interfaces,
    bb_read_interface("PanTilt RX28", "tilt", Sr).
:- else.
execute(sense_tilt, Sr) :-
    log_info("Sensing tilt value (DEBUG - DOES NOTHING!)").
:- endif.


execute(pantilt(Pan, Tilt), false) :-
    log_info("Executing: pantilt(pan=%w, tilt=%w)", [Pan, Tilt]),
    concat_string(["tilt=", Tilt, ", pan=", Pan, ", max_speed=0.5"], Arg),
    exec_skill("pantilt", Arg).

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

:- if(get_flag(hostname, "c1")).
execute(sense_rotation, Sr) :-
  log_info("Executing: sense_rotation"),
  bb_read_interfaces,
  bb_read_interface("Pose", "rotation", Quat),
  quat_get_yaw(Quat, Sr),
  log_info("Orientation is: %w", [Sr]).
:- else.
execute(sense_rotation, 0) :-
  log_info("Executing: sense_rotation (DEBUG! - Always senses 0!)").
:- endif.

execute(sense_holding_box, Sr) :-
    log_info("Sensing holding_box"),
    bb_read_interface("Katana", "sensor_value", Res),
    array_list(Array, Res),
    X is Array[12],
    log_info("central sensor is %w", [X]),
    (( Array[12] > 120,
       Sr = true,
       log_info("Holding a cup!"))
    ;
    ( Sr = false,
      log_info("NOT holding a cup!"))).



% This action lacks normalization of the orientation and therefore
% does not work properly. Normalization would need to be ported
% to ECLiPSe to use it.
execute(has_approx_ori(Current_ori, Wanted_ori), true) :-
  (rotation_tolerance(Tol),
  ( Current_ori > Wanted_ori ->
    (Temp is Current_ori - Wanted_ori,
    Temp =< Tol)
    ;
    (Temp is Wanted_ori - Current_ori,
    Temp =< Tol)
  ),
  log_debug("has_approx_ori(%w, %w) is true with Temp=%w", [Current_ori, Wanted_ori, Temp])).
execute(has_approx_ori(Current_ori, Wanted_ori), false) :- 
    log_debug("has_approx_ori(%w, %w): false", [Current_ori, Wanted_ori]).

execute(align_at_dishwasher(front), Sr) :-
    log_info("Executing: align_at_dishwasher(front)"),
    exec_skill("align_at_dishwasher", ""),
    sleep(0.1),
    wait_for_skiller,
    decide_on_sensing(Sr).
execute(align_at_dishwasher(side), Sr) :-
    log_info("Executing: align_at_dishwasher(side)"),
    exec_skill("align_at_dishwasher", "pos=\"side\""),
    sleep(0.1),
    wait_for_skiller,
    decide_on_sensing(Sr).

execute(backoff_from_dishwasher(front), Sr) :-
    log_info("Executing: backoff_from_dishwasher(front)"),
    exec_skill("relgoto", "x=-0.5, y=0.5, ori=3.2, backwards=true"),
    sleep(0.1),
    wait_for_skiller,
    inverse_decide(Sr).
execute(backoff_from_dishwasher(side), Sr) :-
    log_info("Executing: backoff_from_dishwasher(side)"),
    exec_skill("relgoto", "x=-0.5, y=-0.5, backwards=true"),
    sleep(0.1),
    wait_for_skiller,
    inverse_decide(Sr).

% since relgoto normalizes the differance calculated
% this action works correctly
execute(turn(Current_ori, Wanted_ori), false) :-
  Temp is Wanted_ori - Current_ori,
  log_debug("turn(%w, %w) is true with Temp=%w", [Current_ori, Wanted_ori, Temp]),
  concat_string(["x=0, y=0, ori=", Temp], Arg),
  exec_skill("relgoto", Arg).

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
prim_action(drive_to(N)) :- location(N).
prim_action(read_skiller_status).
prim_action(sleep).
prim_action(sleep(N)).
prim_action(print(S)).
prim_action(print_var(S,V)).
prim_action(detect_box).
prim_action(put_box_in_dishwasher(Spot, Pos)).
prim_action(restart).
prim_action(say(String, Wait, Mutable)).
prim_action(update).
prim_action(align_to_table).
prim_action(backoff_from_table).
prim_action(change_fluent(Fluent, Value)).
prim_action(pantilt(Pan, Tilt)).
prim_action(sense_tilt).
prim_action(wait_for_skiller).
prim_action(send_switch_msg(Iface, Msg)).
prim_action(sense_rotation).
prim_action(has_approx_ori(Ori, Rot)).
prim_action(align_at_dishwasher(Pos)).
prim_action(backoff_from_dishwasher(S)).
prim_action(turn(W,C)).
prim_action(grab_box).
prim_action(end_of_run).
prim_action(set_verbose(Mode)).
prim_action(sense_holding_box).
prim_action(plan_exec(Goal)).


%% fluents
prim_fluent(at).
prim_fluent(skiller_status).
prim_fluent(holding_box).
prim_fluent(detected_box).
prim_fluent(ignore_status).
prim_fluent(up_to_date).
prim_fluent(intro).
prim_fluent(num_runs).
prim_fluent(error(E)).
prim_fluent(exec_once).
prim_fluent(table_pos_visited(P)).


%% sensing
senses(read_skiller_status, skiller_status).
senses(update, skiller_status).
senses(drive_to(N), at).
senses(detect_box, detected_box).
senses(align_to_table, close_to_table).
senses(backoff_from_table, close_to_table).
senses(sense_tilt, tilt_value).
senses(sense_rotation, rotation).
senses(has_approx_ori(Ori, Rot), rotation_reached).
senses(align_at_dishwasher(Pos), close_to_dishwasher).
senses(backoff_from_dishwasher(front), close_to_dishwasher).
senses(backoff_from_dishwasher(side), close_to_dishwasher).
senses(grab_box, holding_box).
senses(sense_holding_box, retry_drop).

%% causal laws
causes_val(change_fluent(Fluent, Value), Fluent, Value, true).

causes_val(put_box_in_dishwasher(Spot, Pos), holding_box, false, true).

causes_val(drive_to(_), detected_box, false, true).
causes_val(drive_to(_), close_to_table, false, true).
%causes_val(drive_to(_), rotatation_reached, false, true).

causes_val(req_update, up_to_date, false, true).
causes_val(update, up_to_date, true, true).

causes_val(put_box_in_dishwasher(Spot, Pos), free(Spot), false, true).

causes_val(end_of_run, num_runs, N, N is num_runs+1  ).

% skiller status should be ignored for the first action after a restart
causes_val(restart, ignore_status, true, true).
causes_val(drive_to(N), ignore_status, false, true).
causes_val(detect_box, ignore_status, false, true).
causes_val(grab_box, ignore_status, false, true).
causes_val(put_box_in_dishwasher(Spot, Pos), ignore_status, false, true).

%% preconditions
% currently most actions are just possible, preconditions are
% modelled to a great extent in the agents procedure
poss(change_fluent(Fluent, Value),true).
poss(drive_to(N), and(location(N), executable) ).
poss(read_skiller_status, up_to_date=true).
poss(sleep, executable).
poss(sleep(N), true).
poss(print(S), true).
poss(print_var(S,V), true).
poss(detect_box, at="Livingroom Table").
poss(align_to_table, at="Livingroom Table").
poss(backoff_from_table, close_to_table=true).
poss(say(S, W, M), true).
poss(sense_tilt, true).
poss(pantilt(Pan, Tilt), true).
poss(wait_for_skiller, true).
poss(send_switch_msg(I,M), true).
poss(sense_rotation, true).
poss(has_approx_ori(Ori, Rot), true).
poss(align_at_dishwasher(Pos), at="Before Dishwasher").
poss(backoff_from_dishwasher(front), close_to_dishwasher=true).
poss(backoff_from_dishwasher(side), close_to_dishwasher=true).
poss(turn(W,C), true).
poss(grab_box, and(executable, holding_box=false)).
poss(set_verbose(Mode), true).
poss(end_of_run, true).
poss(sense_holding_box, true).

poss(update, true).

poss(restart, true).


%% initial state
initially(at, "Charger Home").
initially(holding_box, false).
initially(detected_box, false).
initially(close_to_table, false).
initially(ignore_status, false).
initially(rotation_reached, false).
initially(close_to_dishwasher, false).
initially(free(A), true).
initially(free(B), true).
initially(free(C), true).
initially(intro, false).
initially(num_runs, 1).
initially(said_unfortunate, false).
initially(error(no_boxs_detected), false).
initially(error(grab_failed), false).
initially(at_dishwasher_side, false).
initially(exec_once, true).
initially(up_to_date, false).
initially(retry_drop, false).
initially(table_pos_visited("table1_loc1_room1"), false).
initially(table_pos_visited("table1_loc2_room1"), false).
initially(table_pos_visited("table1_loc3_room1"), false).
initially(table_pos_visited("table1_loc4_room1"), false).

%% definition of complex conditions
proc(inactive, skiller_status = "S_INACTIVE").
proc(running, skiller_status = "S_RUNNING").
proc(final, skiller_status = "S_FINAL").
proc(failed, skiller_status = "S_FAILED").
proc(executable, or(inactive, or(final, ignore_status=true)) ).

proc(at_table(N), and(at=N, table(N))).
proc(not_at_table(N), and(at=N, neg(table(N)))).


proc(next_action_goto_counter(N), and(not_at_table(N), and(executable, and(holding_box=false)))).

proc(next_action_goto_table(N), and(not_at_table(N), and(executable, and(holding_box=true, at_table("counter"))))).

proc(positions_left(N),neg(table_pos_visited(N))).



%% procedures
proc(has_ori(Ori), [sense_rotation, ?(rotation=Rot), has_approx_ori(Rot, Ori)]).

proc(turn_to_wanted_ori(Ori), [sleep(0.1), sense_rotation, ?(rotation=Rot), turn(Rot, Ori)]).

/*
proc(turn_tabletop_off, [send_switch_msg("object-tracking", "DisableSwitchMessage"),
             send_switch_msg("object-fitting", "DisableSwitchMessage"),
             send_switch_msg("object-detection", "DisableSwitchMessage"),
             send_switch_msg("tabletop-detection", "DisableSwitchMessage")]).

proc(turn_tabletop_on, [send_switch_msg("tabletop-detection", "EnableSwitchMessage"),
            send_switch_msg("object-detection", "EnableSwitchMessage"),
            send_switch_msg("object-fitting", "EnableSwitchMessage"),
            send_switch_msg("object-tracking", "EnableSwitchMessage") ]).
*/

proc(turn_new_tabletop_off, send_switch_msg("tabletop-objects", "DisableSwitchMessage")).
proc(turn_new_tabletop_on, send_switch_msg("tabletop-objects", "EnableSwitchMessage")).

proc(drive_to_table, [search(pi(n, [?(table(n)), drive_to(n)]))]).

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
     interrupt(exec_once=true,
	[print("Executing planexec"),
	 drive_to("table1_loc1_room1"),
	 change_fluent(exec_once,false)]),

     interrupt(and(neg(verbose_mode(mute)), num_runs=2), set_verbose(mute)),

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

%     interrupt(n, next_action_goto_counter(n),
%	       [plan_exec("at(\"table1_loc1_room1\")")]),

     % run basic perception if we do not know any object worth of
     % closer inspection, yet.
%     interrupt(n, and(at_table("counter"), (and(esl, positions_left(n)))),
%	       [generate_goal_goto(N, Goal), plan_exec(Goal)]),

     % fail, cannot find object
%     interrupt(n, and(at_table("counter"), neg(positions_left(n))),
%	       [restart]),

     % at the counter, having an object which is of type box, but it
     % is yet unknown which specific one it is -> inspect
%     interrupt(and(at_table("counter"),and(holding_box=false, esl)),
%	       [generate_goal_inspect(ObjID, Goal), plan_exec(Goal)]),

     % at the counter, picked up an object, and determined it to be
     % the wrong one -> put down and go on
%     interrupt(and(at_table("counter"),and(holding_box=true, esl)),
%	       [generate_goal_put_down(ObjID, Goal), plan_exec(Goal)]),

     % at the counter, picked up an object, and determined it to be
     % the correct one -> bring to table
%     interrupt(and(at_table("counter"),and(holding_box=true, esl)),
%	       [generate_goal_deliver(ObjID, Goal), plan_exec(Goal)]),

     interrupt(true, sleep)
    ])).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%        End of IndiGolog definitions          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- if(debug_true).
% check for Golog syntax (experimental, only definitions
% of actions and fluents are currenty checked)
run :- basic_check, repeat, fail.
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

