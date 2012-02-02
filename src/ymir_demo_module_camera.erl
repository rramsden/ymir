-module(ymir_demo_module_camera).

-behaviour(ymir_demo_module).

-include("event_state.hrl").

-export([title/0, description/0, actions/0, start/0, stop/0]).

call_worker( Func, Args ) ->
    gen_server:call(ogre_manager, {worker, ymir_core, Func, Args}).

title() -> "Camera Demo".
description() -> "Free motion camera demo.".
actions() -> [{frameStarted, fun(S) -> move(S) end}].
start() ->
     %%Generate some objects
    Light = { "Light", "light", [{"position", {20.0, 80.0, 0.0}},
                                 {"source", "point"} ]},
    Head = {"Head", "entity", [{"mesh", "ogrehead.mesh"}]},
    Camera = { "Camera", "camera", [{"position", {0.0, 0.0, 80.0}},
                                    {"lookAt", {0.0, 0.0, -300.0}},
                                    {"nearClip", 5.0},
                                    {"fixYaw", true}] },

    io:format("Loading objects!!~n"),
    ok = call_worker(create, [Light, Head]),

    ok = call_worker(update, [Camera]). 

stop() ->
    ok = call_worker(destroy, [{"Light", "light", []}, 
                               {"Head", "entity", []}]).


%%%%%% Action Definitions

mouse_rotate(?MB_Right, true, {Dx, Dy, Dz}) -> {Dx * 0.001, Dy * 0.0, Dz * 0.0};
mouse_rotate(_Key, _Val, Offset) -> Offset.

position_offset(?KC_W, true, {DX,DY,DZ}) -> {DX, DY, DZ - 10.0}; 
position_offset(?KC_S, true, {DX,DY,DZ}) -> {DX, DY, DZ + 10.0};
position_offset(?KC_A, true, {DX,DY,DZ}) -> {DX - 10.0, DY, DZ};
position_offset(?KC_D, true, {DX,DY,DZ}) -> {DX + 10.0, DY, DZ};
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
                ok = gen_server:call(ogre_manager, { worker, 
                                                     'ymir_core', 
                                                     update,
                                                     [{"Camera", 
                                                       "camera",
                                                       Updates
                                                    }]} );
            _Else ->
                ok

        end,

        State.
