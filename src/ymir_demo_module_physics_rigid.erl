-module(ymir_demo_module_physics_rigid).

-behaviour(ymir_demo_module).

-include("event_state.hrl").

-export([title/0, description/0, actions/0, start/0, stop/0]).

title() -> "Rigid Physics Demo".
description() -> "Physics demo with simple rigid objects.".
actions() -> [{frameStarted, fun(S) -> move(S) end}].
start() ->
     %%Generate some objects
    Light = { "Light", "light", [{"position", {20.0, 80.0, 0.0}},
                                 {"source", "point"} ]},
    Ground = {"Ground", "static_entity", [{"shape", "cube"},
                                   {"mass", 0.0},
                                   {"orientation", {0.0, 0.0, 0.0, 1.0}}, 
                                   {"scale", {1.0, 0.05, 1.0}},
                                   {"material", "Examples/Rockwall"}]},

    Ball = {"Ball", "static_entity", [{"shape", "cube"},
                               {"scale", {0.1, 0.1, 0.1}},
                               {"mass", 100.0},
                               {"position", {10.0, 60.0, 10.0}},
                               {"material", "Examples/BumpyMetal"}]},

    Ball2 = {"Ball2", "static_entity", [{"shape", "cube"},
                               {"scale", {0.1, 0.1, 0.1}},
                               {"mass", 400.0},
                               {"position", {5.0, 80.0, 5.0}},
                               {"material", "Examples/BumpyMetal"}]},

    Camera = { "Camera", "camera", [{"type", "free"},
                                    {"position", {0.0, 30.0, 65.0}},
                                    {"moveSpeed", 20.0},
                                    {"rotateSpeed", 50.0},
                                    {"lookAt", {0.0, 0.0, 0.0}},
                                    %%{"pitch", -0.25},
                                    %%{"nearClip", 5.0},
                                    {"nearClip", 5.0}] },
                                    %%{"fixYaw", true}] },

    Scene = { title(), "scene", [{"ambient", {0.5, 0.5, 0.5, 1.0}},
                                 {"debug", true},
                                 {"gravity", {0.0, -9.8, 0.0}},
                                 {"viewport", "Camera"},
                                 {"objects", [Light, Ground, Ball, Ball2, Camera]}] },

    ymir_demo:core_call({create, [Scene]}).

stop() ->
    ymir_demo:core_call({destroy, [{title(), "scene", []}]}).

position_offset(?KC_W, true, {DX,DY,DZ}) -> {DX, DY, DZ + 1.0}; 
position_offset(?KC_S, true, {DX,DY,DZ}) -> {DX, DY, DZ - 1.0};
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
            [{"rotate", {Rx * 1.0, Ry * 1.0, 0.0}}];
            %%{Yaw, Pitch, _Roll} = {Rx * -0.001, Ry * -0.001, 0},
            %%[ {Name, Val} || {Name, Val} <- [{"yaw", Yaw}, {"pitch", Pitch}], Val /= 0.0 ];

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
