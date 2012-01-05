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
                                    {"nearClip", 5.0}] },

    io:format("Loading objects!!~n"),
    ok = call_worker(create, [Light, Head]),

    ok = call_worker(update, [Camera]). 

stop() ->
    ok = call_worker(destroy, ["Light", "Head"]).


%%%%%% Action Definitions

mouse_rotate(1, true, {Dx, Dy, Dz}) -> {Dx * -0.02, Dy * -0.02, Dz};
mouse_rotate(_Key, _Val, Offset) -> Offset.

position_offset(17, true, {DX,DY,DZ}) -> {DX, DY, DZ - 10.0}; 
position_offset(31, true, {DX,DY,DZ}) -> {DX, DY, DZ + 10.0};
position_offset(30, true, {DX,DY,DZ}) -> {DX - 10.0, DY, DZ};
position_offset(32, true, {DX,DY,DZ}) -> {DX + 10.0, DY, DZ};
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
    F = fun( Key, Val, In ) -> mouse_rotate(Key, Val, In) end,

    {Cx, Cy, Cz} = dict:fetch(current, State#eventState.mouse),
    {Px, Py, Pz} = dict:fetch(previous, State#eventState.mouse),
    Diff = {Cx-Px, Cy-Py, 0},

    Offset = dict:fold(F, Diff, State#eventState.mouse),
    if
        Offset /= Diff ->
            {Yaw, Pitch, Roll} = Offset,
            Actions = [{"yaw", Yaw}, {"pitch", Pitch}, {"roll", Roll}],
            [ {Name, Val} || {Name, Val} <- Actions, Val /= 0.0 ];

        true ->
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
