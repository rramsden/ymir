-module(ymir_demo).

-behaviour(gen_event).

-export([start_link/0, init/1, handle_event/2, handle_info/2, handle_call/2, terminate/2]).

-include_lib("stdlib/include/qlc.hrl").

-record( object, { uuid, type, props } ).
-record( inputState, { keyboard, mouse, map } ).

call( Msg )->
    gen_server:call(ogre_manager, Msg).


insert_object( Object ) when is_record(Object, object) ->
    Fun = fun() ->
        mnesia:write( object )
    end,
    mnesia:transaction(Fun);

insert_object( { UUID, Type, Props } ) ->
    Fun = fun() ->
        mnesia:write(#object{ uuid = UUID, type = Type, props = Props })
        end,

    mnesia:transaction(Fun).

insert_objects( [] ) -> ok;
insert_objects( [ Object | T ] ) ->
    {atomic, ok} = insert_object(Object),
    insert_objects(T).

find_object( UUID ) when is_list(UUID) ->
    Fun = fun () ->
            Q = qlc:q([Object || Object <- mnesia:table(object), Object#object.uuid == UUID]),
            qlc:e(Q)
          end,
    mnesia:transaction(Fun).

encode_object( {UUID, Type, Props} ) when is_list(UUID),
                                          is_list(Type) ->
    {UUID, Type, dict:to_list(Props)}.

encode_objects([]) -> [];
encode_objects( [H|T] ) -> 
    [encode_object(H)] ++ encode_objects(T).

load() ->
    ok = call({load,'OgreManager'}),

    ok = call({'OgreManager', start, [ "Ymir Demo",
                                       "./build/plugins.cfg",
                                       "./ogre.cfg",
                                       "./Ymir.log" ]}),
   
    %%Load test module
    ok = call({'OgreManager', addResourceLocation, [ 
            {"./TestModule/Models", "FileSystem", "Ymir", false},
            {"./TestModule/Materials", "FileSystem", "Ymir", false},
            {"./TestModule/Scripts", "FileSystem", "Ymir", false},
            {"./TestModule/Programs", "FileSystem", "Ymir", false} ]}),

    %%Generate some objects
    Light = { "Light", "light", dict:from_list( [{"position", {20.0, 80.0, 0.0}},
                                                 {"source", "point"} ])},
    Head = {"Head", "entity", dict:from_list( [{"mesh", "ogrehead.mesh"}])},
    Camera = { "Camera", "camera", dict:from_list( [{"position", {0.0, 0.0, 80.0}},
                                                    {"lookAt", {0.0, 0.0, -300.0}},
                                                    {"nearClip", 5.0}]) },

    io:format("Adding objects to local state db...~n"),
    ok = insert_objects([Light, Camera, Head]),

    ok = call({'OgreManager', initialiseAllResourceGroups, []}),

    io:format("Loading objects!!~n"),
    ok = call({'OgreManager', addObject, encode_objects([Camera, Light, Head])}),

    io:format("Assigning viewport!~n"),
    ok = call({'OgreManager', setViewport, ["Camera"]}),
    
    io:format("Registering for input events!~n"),
    ok = call({'OgreManager', addEventHandler, []}).

render() ->
    ok = gen_event:add_handler(event_manager, ?MODULE, []),

    %%Start Mnesia database to track object states
    mnesia:create_schema([node()]),
    ok = mnesia:start(),

    %%Create table to store object state
    mnesia:create_table(object, [{attributes, record_info(fields, object)}]),

    %%Clear any old data lingering about
    mnesia:clear_table(object),

    %Load demo
    load(),

    ok = gen_server:call( ogre_manager, 
                          {'OgreManager', renderStart, []}, 
                          infinity).

start_link()->
    {ok, spawn( fun() -> render() end )}.

 
%%%%%%%%%%%%%%%%%%%%%%%%%%
%  GEN_EVENT EXPORTS
%%%%%%%%%%%%%%%%%%%%%%%%%%
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

merge_camera( "position", {X1, Y1, Z1}, {X2, Y2, Z2} )->
    {X1 + X2, Y1 + Y2, Z1 + Z2};

merge_camera( "yaw", Val1, Val2 ) -> Val1 + Val2;
merge_camera( "pitch", Val1, Val2 ) -> Val1 + Val2;
merge_camera( "roll", Val1, Val2 ) -> Val1 + Val2;

merge_camera( _Key, _Val1, Val2 ) -> Val2.

update_camera( UUID, Updates ) when is_list(UUID), 
                                    is_list(Updates) ->
    Merge = fun( Key, Val1, Val2 ) -> merge_camera(Key, Val1, Val2) end,
    F = fun() ->
        [Camera] = mnesia:read(object, UUID, write),
        
        New = Camera#object{props = dict:merge( Merge,
                                                Camera#object.props,
                                                dict:from_list(Updates) )},
        ok = mnesia:write(New),
        New
    end,

    mnesia:transaction(F).
                                            
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

shutdown() ->
    ok = gen_server:call(ogre_manager, ({worker, 'OgreManager', renderStop, []})),
    ok = call({'OgreManager', stop, []}).

handle_event(windowClosed, State) when is_record(State, inputState) ->
   shutdown(),
   {ok, State}; 

%%Frame has started, simply prompts ymir_demo to examine input state.
handle_event(frameStarted, State) when is_record(State, inputState) ->
    Keyboard = State#inputState.keyboard,    

    move(State),

    Current = dict:fetch( current, State#inputState.mouse ),
    NewState = State#inputState{mouse = dict:store( previous, 
                                                    Current, 
                                                    State#inputState.mouse ) },

    case dict:find(1, Keyboard) of
        {ok, true} -> 
            shutdown(),
            {ok, NewState};
        _Default ->
            {ok, NewState} 
    end;

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
