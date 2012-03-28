-module(ymir_demo_module_character).

-behaviour(ymir_demo_module).

-include("event_state.hrl").

-define(RUN_SPEED, 17.0).
-define(TURN_SPEED, 500.0).
-define(FADE_SPEED, 7.8).
-define(TOGGLE_DANCE, {keyReleased, ?KC_E}).


-export([title/0, description/0, actions/0, start/0, stop/0]).

title() -> "Character Controller".
description() -> "Kinematic Character Controller Demo".
actions() -> [{?TOGGLE_DANCE, fun(S) -> toggle_dance(true, S) end},
              %%{{mouseReleased, ?MB_LEFT}, fun(S) -> position(S) end}, 
              {frameStarted, fun(S) -> move(S) end}].
start() -> 

    Light = { "Light", "light", [{"position", {-10.0, 40.0, 20.0}},
                                 {"source", "point"} ]},

    Ground = {"Ground", "static_entity", [{"shape", "cube"},
                                          {"mass", 0.0},
                                          {"orientation", {0.0, 0.0, 0.0, 1.0}}, 
                                          {"scale", {1.25, 0.10, 1.25}},
                                          {"material", "Examples/Rockwall"}]},
    
    Camera = { "Camera", "camera", [{"type", "orbit"},
                                    {"target", "Sinbad"},
                                    {"position", {0.0, 5.0, 15.0}},
                                    {"lookAt", {0.0, 0.0, 0.0}},
                                    {"moveSpeed", 20.0},
                                    {"rotateSpeed", 100.0},
                                    {"zoomSpeed", 20.0},
                                    
                                    {"nearClip", 0.1},
                                    {"farClip", 150.0},
                                    {"fixYaw", true}] },

    Sinbad = { "Sinbad", "animate_entity", [{"mesh", "Sinbad.mesh"},

                                            {"velocity", 0.0},
                                            {"velocityMax", ?RUN_SPEED},
                                            {"acceleration", 10 * ?RUN_SPEED},
                                            {"accelerationFactor", 0.0},
                                            {"animationFadeSpeed", ?FADE_SPEED},
                                            {"position", {0.00, 10.00, 0.00}},
                                            %%{"camera", "Camera"},
                                            {"skeletalEntities", [{"Sheath.L", "Sword1", "Sword.mesh"},
                                                                  {"Sheath.R", "Sword2", "Sword.mesh"}
                                                                 ]},
                                            {"animations", ["IdleTop", "IdleBase"]}]},
                                                

    Scene = { title(), "scene", [{"type", "exterior_small"},
                                 {"ambient", {0.3, 0.3, 0.3, 1.0}},
                                 {"backgroundColour", {1.0, 1.0, 0.8, 1.0}},
                                 %%{"fog", {"linear", {1.0, 1.0, 0.8, 0.0}, 0.0, 15.0, 100.00}},
                                 {"debug", true},
                                 {"gravity", {0.0, -9.8, 0.0}},
                                 {"viewport", "Camera"},
                                 {"objects", [Light, Ground, Sinbad, Camera]}] },

    ymir_demo:core_call({create, [Scene]}).

stop() ->
    ymir_demo:core_call({destroy, [{"Sinbad", "animate_entity", []},
                                   {title(), "scene", []}]}).

%%%%%% Action Definitions
mouse_rotate(State) ->
    case dict:fetch( moved, State#eventState.mouse ) of
        true -> 
            {{_Ax, Rx}, {_Ay, Ry}, {_Az, _Rz}} = dict:fetch(current, State#eventState.mouse),
            Rot = {Rx * 10.0, Ry * 10.0, 0.0},
            [{"rotate", Rot}];

        false -> 
            []
     end.

entity_offset(?KC_W, true, {AF, Rot}) -> {AF + 1.0, Rot};
entity_offset(?KC_S, true, {AF, Rot}) -> {AF - 1.0, Rot};
entity_offset(?KC_A, true, {AF, {DX,DY,DZ}}) -> {AF, {DX, DY - 1.0, DZ}};
entity_offset(?KC_D, true, {AF, {DX,DY,DZ}}) -> {AF, {DX, DY + 1.0, DZ}};
entity_offset(_Key, _Val, Diff) -> Diff.


rotation_offset(_Key, _Val, Offset) -> Offset.

position(State) when is_record(State, eventState) ->
    Default = {0.0, {0.0, 0.0, 0.0}},   
    F = fun( Key, Val, In ) -> entity_offset(Key, Val, In) end,
    {AF, Rot} = dict:fold(F, Default, State#eventState.keyboard),
    case AF of
        0.0 -> 
            [{"accelerationFactor", AF}, {"rotate", Rot}, {"animations", ["IdleBase", "IdleTop"]}];
        _ ->
            [{"accelerationFactor", AF}, {"rotate", Rot}, {"animations", ["RunBase", "RunTop"]}]
    end.

%move_if_ground([{"Ground", _Type, _Dist, Pos} | _T]) ->
%    io:format("Moving to: ~p~n", [Pos]),
%    
%    gen_server:call(ymir_demo, {core, { update,
%                                            [{"Sinbad", 
%                                              "animate_entity",
%                                               [{"moveTo", Pos}] }]}} );
%
%move_if_ground(_Else)-> [].
%
%move_to(State) when is_record(State, eventState) ->
%
%    {{Ax, _Rx}, {Ay, _Ry}, _Dz} = dict:fetch(current, State#eventState.mouse),
%    case ymir_demo:core_call({rayCast, [(Ax * 1.0), (Ay * 1.0)]}) of
%        Objects when is_list(Objects) -> io:format("Got: ~p~n", [Objects]), 
%        Ents = [X || X <- Objects, element(2, X) == "Entity"],
%        move_if_ground(Ents);
%
%        Else -> io:format("Not right:  ~p~n", [Else]), []
%    end.
%
%position(State) when is_record(State, eventState) ->
%   
%    case dict:find( ?MB_LEFT, State#eventState.mouse ) of
%        {ok, true} ->
%            move_to(State);
%        _Else ->
%            []
%    end,
%
%    State.

rotation(State) when is_record(State, eventState) ->

    case dict:find( ?MB_MIDDLE, State#eventState.mouse ) of
        {ok, true} ->
            mouse_rotate(State);
        _else ->
            []
    end.

zoom(State) when is_record(State, eventState) ->
    case dict:fetch( moved, State#eventState.mouse ) of
        true -> 
            {{_Ax, _Rx}, {_Ay, _Ry}, {Az, Rz}} = dict:fetch(current, State#eventState.mouse),
            case Rz of
                0 -> [];
                _Else ->
                    Zoom = (-1 * (Rz/abs(Rz))),
                    [{"zoom", Zoom}]
            end;

        false -> 
            []
     end.
   
move_camera(State) ->
    case rotation(State) ++ zoom(State) of

        Updates when Updates /= []->

            %io:format("Updates: ~p~n", [Updates]),
            %%{atomic, Camera}  = update_camera( "Camera", Updates ),

            %%Tell OgreManager about the updated object
            [{"Camera", "camera", Updates}];

        _Else ->
            []
    end.

move_entity(State) ->
            [{"Sinbad", "animate_entity", position(State)}].


move(State) when is_record(State, eventState) ->
    
        case move_entity(State) ++ move_camera(State) of

            Updates when Updates /= []->

                %io:format("Updates: ~p~n", [Updates]),
                %%{atomic, Camera}  = update_camera( "Camera", Updates ),

                %%Tell OgreManager about the updated object
                gen_server:call(ymir_demo, {core, { update, Updates}});

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
