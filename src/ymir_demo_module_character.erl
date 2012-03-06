-module(ymir_demo_module_character).

-behaviour(ymir_demo_module).

-include("event_state.hrl").

-define(TOGGLE_DANCE, {keyReleased, ?KC_E}).

-export([title/0, description/0, actions/0, start/0, stop/0]).

title() -> "Character Controller".
description() -> "Kinematic Character Controller Demo".
actions() -> [{?TOGGLE_DANCE, fun(S) -> toggle_dance(true, S) end},
              {frameStarted, fun(S) -> move(S) end}].
start() -> 

    Light = { "Light", "light", [{"position", {-10.0, 40.0, 20.0}},
                                 {"source", "point"} ]},

    Ground = {"Ground", "static_entity", [{"shape", "cube"},
                                          {"mass", 0.0},
                                          {"orientation", {0.0, 0.0, 0.0, 1.0}}, 
                                          {"scale", {1.25, 0.10, 1.25}},
                                          {"material", "Examples/Rockwall"}]},
    
    Camera = { "Camera", "camera", [{"position", {0.0, 10.0, 15.0}},
                                    {"lookAt", {0.0, 0.0, 0.0}},
                                    {"nearClip", 5.0},
                                    {"fixYaw", true}] },

    Sinbad = { "Sinbad", "animate_entity", [{"mesh", "Sinbad.mesh"},
                                            {"animationFadeSpeed", 7.8},
                                            {"position", {0.00, 10.00, 0.00}},
                                            {"skeletalEntities", [{"Sheath.L", "Sword1", "Sword.mesh"},
                                                                  {"Sheath.R", "Sword2", "Sword.mesh"}
                                                                 ]},
                                            {"animations", ["IdleTop", "IdleBase"]}]},
                                                

    Scene = { title(), "scene", [{"ambient", {0.3, 0.3, 0.3, 1.0}},
                                 {"backgroundColour", {1.0, 1.0, 0.8, 1.0}},
                                 %%{"fog", {"linear", {1.0, 1.0, 0.8, 0.0}, 0.0, 15.0, 100.00}},
                                 {"debug", true},
                                 {"gravity", {0.0, -9.8, 0.0}},
                                 {"viewport", "Camera"},
                                 {"objects", [Light, Ground, Camera, Sinbad]}] },

    ymir_demo:core_call({create, [Scene]}).

stop() ->
    ymir_demo:core_call({destroy, [{"Sinbad", "animate_entity", []},
                                   {title(), "scene", []}]}).

%%%%%% Action Definitions
mouse_rotate(?MB_Right, true, {Dx, Dy, Dz}) -> {Dx * 0.001, Dy * 0.0, Dz * 0.0};
mouse_rotate(_Key, _Val, Offset) -> Offset.

position_offset(?KC_W, true, {DX,DY,DZ}) -> {DX, DY, DZ - 1.0}; 
position_offset(?KC_S, true, {DX,DY,DZ}) -> {DX, DY, DZ + 1.0};
position_offset(?KC_A, true, {DX,DY,DZ}) -> {DX - 1.0, DY, DZ};
position_offset(?KC_D, true, {DX,DY,DZ}) -> {DX + 1.0, DY, DZ};
position_offset(_Key, _Val, Offset) -> Offset.

position(State) when is_record(State, eventState) ->
    Default = {0.0, 0.0, 0.0},   
    F = fun( Key, Val, In ) -> position_offset(Key, Val, In) end,
    Offset = dict:fold(F, Default, State#eventState.keyboard),
    if
        Offset /= Default ->
            [{"move", Offset}];  

        true ->
            []
    end.

rotation(State) when is_record(State, eventState) ->

    case dict:fetch( moved, State#eventState.mouse ) of
        true -> 
            {{_Ax, Rx}, {_Ay, Ry}, {_Az, _Rz}} = dict:fetch(current, State#eventState.mouse),
            {Yaw, Pitch, _Roll} = {Rx * -0.001, Ry * -0.001, 0},
            [ {Name, Val} || {Name, Val} <- [{"yaw", Yaw}, {"pitch", Pitch}], Val /= 0.0 ];

        false -> 
            []
     end.
        
move(State) when is_record(State, eventState) ->
    
        case position(State) ++ rotation(State) of

            Updates when Updates /= []->

                %io:format("Updates: ~p~n", [Updates]),
                %%{atomic, Camera}  = update_camera( "Camera", Updates ),

                %%Tell OgreManager about the updated object
                gen_server:call(ymir_demo, {core, { update,
                                                    [{"Camera", 
                                                      "camera",
                                                      Updates
                                                     }]}} );
            _Else ->
                ok

        end,

        State.

toggle_dance(true, State) ->
   gen_server:call(ymir_demo, {core, {update, [{"Sinbad", "animate_entity", 
        [{"animations", ["Dance"]}]}]}}),

   State#eventState{actions = 
       dict:store(?TOGGLE_DANCE, fun(S) -> toggle_dance(false, S) end, State#eventState.actions)};

toggle_dance(false, State) -> 

   gen_server:call(ymir_demo, {core, {update, [{"Sinbad", "animate_entity", 
        [{"animations", ["IdleBase", "IdleTop"]}]}]}}),

   State#eventState{actions = 
       dict:store(?TOGGLE_DANCE, fun(S) -> toggle_dance(true, S) end, State#eventState.actions)}.

   
