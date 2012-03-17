-module(ymir_demo_module_terrain).

-behavior(ymir_demo_module).

-include("event_state.hrl").

-export([title/0, description/0, actions/0, start/0, stop/0]).

title() -> "Terrain Demo".
description() -> "Simple terrain with paging".
actions() -> [{frameStarted, fun(S) -> move(S) end}].
start() -> 
    Terrain = [ {"align", "align_x_z"},
                {"terrainSize", 513},
                {"worldSize", 12000.0},
                {"prefix", "testTerrain"},
                {"postfix", "dat"},
                {"origin", {0.0, 0.0, 0.0}},
                {"loadRadius", 2000},
                {"holdRadius", 3000},
                {"camera", "Camera"},
                {"minY", 0}, {"maxY", 0},
                {"minX", 0}, {"maxX", 0} ],

    Camera = {"Camera", "camera", [{"type", "free"},
                                   {"position", {1683.0, 50.0, 2116.0}},
                                   {"moveSpeed", 20.0},
                                   {"rotateSpeed", 50.0},
                                   %%{"lookAt", {1963.0, 50.0, 1660.0}},
                                   {"nearClip", 0.01},
                                   {"farClip", 500.0}]},

    Scene = { title(), "scene", [ {"viewport", "Camera"},
                                  {"terrain", Terrain},
                                  {"objects", [Camera]} ] },
    
    ymir_demo:core_call({create, [Scene]}).

stop() -> 
    ymir_demo:core_call({destroy, [{title(), "scene", []}]}).

%%%%%% Action Definitions
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

        false -> 
            []
     end.
        
move(State) when is_record(State, eventState) ->
    
        case position(State) ++ rotation(State) of

            Updates when Updates /= []->

                %io:format("Updates: ~p~n", [Updates]),
                %%{atomic, Camera}  = update_camera( "Camera", Updates ),

                gen_server:call(ymir_demo, {core, { update,
                                                    [{"Camera", 
                                                      "camera",
                                                      Updates
                                                    }]}} );
            _Else ->
                ok

        end,

        State.
