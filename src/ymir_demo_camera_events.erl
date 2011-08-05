-module(ymir_demo_camera_events).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_info/2, handle_call/2, terminate/2]).

-record( inputState, { keyboard, mouse, map } ).

init(_Args)->
    Keyboard = dict:new(),
    Mouse = dict:from_list( [{current, {0,0,0}},
                             {previous, {0,0,0}}] ),
    Map = dict:from_list( [ {quit, {keyboard, 1}}, 
                            {move_forth, {keyboard, 17}},
                            {move_back, {keyboard, 31}},
                            {move_right, {keyboard, 30}},
                            {move_left, {keyboard, 32}} ]),

    {ok, #inputState{ keyboard = Keyboard, mouse = Mouse, map = Map }}.

%update_camera( UUID, {DX, DY, DZ}, ) ->
%    F = fun() ->
%        [Camera] = mnesia:read(object, UUID, write),
%        {X, Y, Z} = dict:fetch("position", Camera#object.props),
%
%        New = Camera#object{props = dict:store("position",
%                                               { X + DX,
%                                                 Y + DY,
%                                                 Z + DZ },
%                                                Camera#object.props)},
%        ok = mnesia:write(New),
%        New
%    end,
%    mnesia:transaction(F).
%
%merge_camera( "position", {X1, Y1, Z1}, {X2, Y2, Z2} )->
%    {X1 + X2, Y1 + Y2, Z1 + Z2};
%
%merge_camera( "yaw", Val1, Val2 ) -> Val1 + Val2;
%merge_camera( "pitch", Val1, Val2 ) -> Val1 + Val2;
%merge_camera( "roll", Val1, Val2 ) -> Val1 + Val2;
%
%merge_camera( _Key, _Val1, Val2 ) -> Val2.
%
%update_camera( UUID, Updates ) when is_list(UUID), 
%                                    is_list(Updates) ->
%    Merge = fun( Key, Val1, Val2 ) -> merge_camera(Key, Val1, Val2) end,
%    F = fun() ->
%        [Camera] = mnesia:read(object, UUID, write),
%        
%        New = Camera#object{props = dict:merge( Merge,
%                                                Camera#object.props,
%                                                dict:from_list(Updates) )},
%        ok = mnesia:write(New),
%        New
%    end,
%
%    mnesia:transaction(F).
                                            
mouse_rotate(2, true, {Dx, Dy, Dz}) -> {Dx * -0.02, Dy * -0.02, Dz};
mouse_rotate(_Key, _Val, Offset) -> Offset.

position_offset(17, true, {DX,DY,DZ}) -> {DX, DY, DZ - 10.0}; 
position_offset(31, true, {DX,DY,DZ}) -> {DX, DY, DZ + 10.0};
position_offset(30, true, {DX,DY,DZ}) -> {DX - 10.0, DY, DZ};
position_offset(32, true, {DX,DY,DZ}) -> {DX + 10.0, DY, DZ};
position_offset(_Key, _Val, Offset) -> Offset.

position(State) when is_record(State, inputState) ->
    Default = {0.0, 0.0, 0.0},   
    F = fun( Key, Val, In ) -> position_offset(Key, Val, In) end,
    Offset = dict:fold(F, Default, State#inputState.keyboard),
    if
        Offset /= Default ->
            [{"move", Offset}];  

        true ->
            []
    end.

rotation(State) when is_record(State, inputState) ->
    F = fun( Key, Val, In ) -> mouse_rotate(Key, Val, In) end,

    {Cx, Cy, Cz} = dict:fetch(current, State#inputState.mouse),
    {Px, Py, Pz} = dict:fetch(previous, State#inputState.mouse),
    Diff = {Cx-Px, Cy-Py, 0},

    Offset = dict:fold(F, Diff, State#inputState.mouse),
    if
        Offset /= Diff ->
            {Yaw, Pitch, Roll} = Offset,
            Actions = [{"yaw", Yaw}, {"pitch", Pitch}, {"roll", Roll}],
            [ {Name, Val} || {Name, Val} <- Actions, Val /= 0.0 ];

        true ->
            []
    end.

move(State) when is_record(State, inputState) ->
    
        case position(State) ++ rotation(State) of
            [] ->
                ok;

            Updates ->

                %io:format("Updates: ~p~n", [Updates]),
                %%{atomic, Camera}  = update_camera( "Camera", Updates ),

                %%Tell OgreManager about the updated object
                ok = gen_server:call(ogre_manager, { worker, 
                                                     'OgreManager', 
                                                     updateObject,
                                                     [{"Camera", 
                                                       "camera",
                                                       Updates
                                                    }]} )
        end.

%%Frame has started, simply prompts ymir_demo to examine input state.
handle_event(frameStarted, State) when is_record(State, inputState) ->
    Keyboard = State#inputState.keyboard,    

    move(State),

    Current = dict:fetch( current, State#inputState.mouse ),
    NewState = State#inputState{mouse = dict:store( previous, 
                                                    Current, 
                                                    State#inputState.mouse ) },
    {ok, NewState};

%%Key/Mouse pressed and release event handlers, simply update stored state
handle_event({keyPressed, Key}, State) when is_record(State, inputState) ->
    Keyboard = State#inputState.keyboard,
    Mouse = State#inputState.mouse,

    {ok, #inputState{ keyboard = dict:store(Key, true, Keyboard), 
                      mouse = Mouse }};

handle_event({keyReleased, Key}, State) when is_record(State, inputState) ->
    Keyboard = State#inputState.keyboard,
    Mouse = State#inputState.mouse,

    {ok, #inputState{ keyboard = dict:store(Key, false, Keyboard), 
                      mouse = Mouse }};

handle_event({mousePressed,{Key, _Pos}}, State) when is_record(State, inputState) ->
    Keyboard = State#inputState.keyboard,
    Mouse = State#inputState.mouse,

    {ok, #inputState{ keyboard = Keyboard, 
                      mouse = dict:store(Key, true, Mouse) }};

handle_event({mouseReleased, {Key, _Pos}}, State) when is_record(State, inputState) ->
    Keyboard = State#inputState.keyboard,
    Mouse = State#inputState.mouse,

    {ok, #inputState{ keyboard = Keyboard, 
                      mouse = dict:store(Key, false, Mouse) }};

handle_event({mouseMoved, Pos}, State) when is_record(State, inputState) ->
    Keyboard = State#inputState.keyboard,
    Mouse = State#inputState.mouse,
    {_ID, [{Ax,_Rx},{Ay, _Ry}, {Az, _Rz}]} = Pos,

    %%Update current mouse position
    {ok, #inputState{ keyboard = Keyboard,
                      mouse = dict:store(current, {Ax, Ay, Az}, Mouse) }};

handle_event(Event, State) ->
    io:format("**** EVENT ***** ~p~n", [Event]),
    {ok, State}.

handle_call( _Call, State ) ->
    {ok, ok, State}.

handle_info( _Info, State )->
    {ok, State}.

terminate( _Args, _State ) ->
    stop.
