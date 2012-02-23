-module(ymir_demo).
-behaviour(gen_server).

-export( [  start_link/0,
            init/1,
            handle_call/3,
            handle_cast/2,
            handle_info/2,
            code_change/3,
            terminate/2   
         ] ).

-export([stop/1, demo_start/2, core_call/1]).

-include("event_state.hrl").

-define(TIMER_DWELL_DEFAULT, 16).
-define(TIMER_DWELL_MIN, 8).
-define(TIMER_DWELL_MAX, 120).

-define(FRAME_SENSITIVITY, 8). 
-define(FRAME_LOW_TIMEOUT, 100000).
-define(FRAME_HIGH_TIMEOUT, 100000).

%-record( demo, { title, module } ).
-record( demoState, { demos = {[],[],[]},
                      timerDwell = ?TIMER_DWELL_DEFAULT,
                      renderTimer,
                      frames = 0,
                      framesSampled = 0,
                      frameTime = 0, 
                      call_count = 0,
                      call_finish = 0,
                      low_start = {0,0,0},
                      high_start = {0,0,0},
                      calls = dict:new()
                    } ).

core_call( {Func, Args} ) ->
    {Res, _Time} = gen_server:call(ogre_manager, {parent, 'ymir_core', Func, Args}),
    Res.

core_call_process( {Func, Args} ) ->
    Res = gen_server:call(ogre_manager, {parent, 'ymir_core', Func, Args}, infinity),
    exit(Res).

core_cast( {Func, Args} ) ->
    gen_server:cast(ogre_manager, {parent, 'ymir_core', Func, Args}).

render_timer( Dwell ) ->
    timer:apply_interval( Dwell, 
                          gen_event, 
                          notify, 
                          [event_manager, ticktock] ).

draw_main_menu(Demos) when is_list(Demos) ->

    io:format("drawing menu...~n"), 
    Buttons = lists:map(fun({_M, B, _A}) -> B end, Demos),
    Actions = lists:map(fun({_M, _B, A}) -> A end, Demos),

    io:format("Creating scene with Buttons: ~p!~n", [Buttons]),
    Camera = {"Camera", "camera", []},
    ok = core_call({create, [{"Main Menu", "scene", [{"viewport", "Camera"},
                                                      {"objects", [Camera] ++ Buttons}]}]}),

    Actions ++ [{{keyDown, ?KC_ESCAPE}, fun(S) -> stop(S) end}].



%% Throttle down if C-side is getting swamped. 
%% This should only occur if either: 
%% rendering is slow or physics sim is slow.
throttle( Desired, Actual, State ) when Desired > Actual + ?FRAME_SENSITIVITY,
                                        State#demoState.timerDwell > ?TIMER_DWELL_MIN ->

    Diff = case State#demoState.low_start of
            {0,0,0} -> 0;
            Time -> timer:now_diff(erlang:now(), Time)
           end,

    case Diff of

        Any when Any > ?FRAME_LOW_TIMEOUT -> 
            TimerDwell = State#demoState.timerDwell - 1,
            {ok, cancel} = timer:cancel(State#demoState.renderTimer),
            io:format("Speeding up rendering:  Desired FPS: ~p, Actual FPS: ~p, NewDwell: ~p~n", 
            [Desired, Actual, TimerDwell]),
            {ok, TRef} = render_timer(TimerDwell),

            State#demoState{ timerDwell = TimerDwell, 
                             renderTimer = TRef, 
                             low_start = {0,0,0} };

        0 ->
            State#demoState{low_start = erlang:now()}; 

        _Below ->
            State

    end; 

throttle( Desired, Actual, State ) when Desired < Actual - ?FRAME_SENSITIVITY,
                                   State#demoState.timerDwell < ?TIMER_DWELL_MAX ->

    Diff = case State#demoState.high_start of
            {0,0,0} -> 0;
            Time -> timer:now_diff(erlang:now(), Time)
           end,

    case Diff of

        Any when Any > ?FRAME_HIGH_TIMEOUT -> 
            TimerDwell = State#demoState.timerDwell + 8,
            {ok, cancel} = timer:cancel(State#demoState.renderTimer),
            io:format("Slowing rendering: Desired FPS: ~p, Actual FPS: ~p,  New Dwell: ~p~n", 
            [Desired, Actual, TimerDwell]),

            {ok, TRef} = render_timer(TimerDwell),
            State#demoState{timerDwell = TimerDwell, 
                            renderTimer = TRef, 
                            high_start = {0,0,0}};

        0 ->
            State#demoState{high_start = erlang:now()}; 

        _Below ->
            State

    end;

throttle( _Desired, _Actual, State ) ->
    State#demoState{high_start = {0,0,0},
                    low_start = {0,0,0}}.

remove_outstanding() ->
    receive
        _Msg ->
            remove_outstanding()
       
    after 50 ->
        ok
    end.

pause_rendering(TRef) ->
    %%Disable render calls - This prevents the C side from
    %% getting flooded while loading big scenes.
    {ok, cancel} = timer:cancel(TRef),
   
    %%Drain any outstanding messages
    remove_outstanding().

unpause_rendering(Dwell) ->
    ok = core_call({resetTimer, []}),
    {ok, TRef} = render_timer(Dwell),
    TRef.

%%%% gen_server bits

%%Every 8 frames determine the render time on the C Side
handle_call({core, {ticktock, Args}}, _From, State ) when State#demoState.frames rem 8 == 0->
    Pid = spawn_link( fun () -> core_call_process({ticktock, Args}) end ),
    {reply, ok, State#demoState{
            frames = State#demoState.frames + 1,
            calls = dict:store(Pid, {ticktock, Args}, State#demoState.calls)}}; 

%% If not a sample frame, simply cast the message to C Side
handle_call({core, {ticktock, Args}}, _From, State ) ->
    core_cast({ticktock, Args}),
    {reply, ok, State#demoState{frames = State#demoState.frames + 1}};

handle_call( {core, Action}, _From, State ) ->
   
    Pid = spawn_link(fun () -> core_call_process(Action) end ),

    {reply, ok, State#demoState{
                    call_count = State#demoState.call_count + 1,
                    calls = dict:store(Pid, Action, State#demoState.calls)}};

handle_call( {unload_demo, Module}, _From, State ) ->
    Module:stop(),

    %%Temporarily disable render
    pause_rendering(State#demoState.renderTimer),

    %F = fun(D, {Buttons, Actions}) -> 
  
    %    { Buttons ++ [{D#demo.title, "button", []}],
    %      Actions ++ [{{guiMouseButtonClick, D#demo.title},
    %          fun(S) -> ymir_demo:demo_start(D#demo.module, S) end}] }       
    %end,

    %{atomic, {Buttons, Actions}} = mnesia:transaction( fun() -> mnesia:foldl(F, {[],[]}, demo) end ),

    Actions = draw_main_menu(State#demoState.demos),
   
    %% Start rendering again
    TRef = unpause_rendering(?TIMER_DWELL_DEFAULT),

    {reply, Actions, State#demoState{ timerDwell = ?TIMER_DWELL_DEFAULT,
                                      frames = 0, 
                                      framesSampled = 0, 
                                      frameTime = 0, 
                                      renderTimer = TRef}};

handle_call( {load_demo, Module}, _From, State ) ->

    pause_rendering(State#demoState.renderTimer),

    %%Destroy the main menu scene and allow module to create their own
    ok = core_call({destroy, [{"Main Menu", "scene", []}]}),

    %% Turn over control to Module
    ok = Module:start(),

    %%Actions for event_manager handlers to register
    Actions = 
        [{{keyDown, ?KC_Q}, fun(S) -> show_main_menu(Module, S) end }] ++ 
        Module:actions(),

    %ok = gen_event:call(event_manager, 
    %                    ymir_demo_event_manager, 
    %                    {set_actions, Actions}),

    %% Start rendering again
    TRef = unpause_rendering(?TIMER_DWELL_DEFAULT),

    {reply, Actions, State#demoState{ timerDwell = ?TIMER_DWELL_DEFAULT,
                                      frames = 0, 
                                      framesSampled = 0, 
                                      frameTime = 0, 
                                      renderTimer = TRef}};

handle_call( _Msg, _From, State ) ->
    { reply, ok, State }.

handle_cast( {core, Action}, State ) -> 
    core_cast(Action),
    {noreply, State};

handle_cast( _Msg, State ) ->
    {noreply, State}.

handle_process_exit( {ticktock, _Args}, {ok, Time}, State ) when is_integer(Time) ->

    FramesSampled = State#demoState.framesSampled + 1,
    FrameTime = State#demoState.frameTime + (Time/1000),

    DesiredRate = State#demoState.timerDwell,
    ActualRate = erlang:round(FrameTime / FramesSampled),
    
    throttle( DesiredRate, 
              ActualRate, 
              State#demoState{ framesSampled = FramesSampled,
                               frameTime = FrameTime } );

handle_process_exit( _Other, {ok, _Time}, State ) ->
    State#demoState{ call_finish = State#demoState.call_finish + 1 };

handle_process_exit( Other, Res, State ) -> 

    error_logger:error_msg(
        "Call to core failed! Action: ~p, Result: ~p", [Other, Res]),

    State#demoState{ call_finish = State#demoState.call_finish + 1 }.

handle_info( {'EXIT', Pid, Reason}, State ) ->

    Action =  dict:fetch(Pid, State#demoState.calls),
    
    NewState = handle_process_exit(Action, Reason, State),
    {noreply, NewState#demoState{calls = dict:erase(Pid, State#demoState.calls)}}; 
       
handle_info( _Info, State ) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra ) -> {ok, State}.

%%
start_link()->
    gen_server:start_link({local, ?MODULE}, ?MODULE, #demoState{}, []).

init(State) ->

    process_flag( trap_exit, true ),

    io:format("Loding ymir_core library..~n"),

    {ok, _Time} = gen_server:call(ogre_manager, {load, ymir_core}),
 
    io:format("Loaded ymir_core library..~n"),

    ok = core_call({start, [ "Ymir Demo",
                             "./plugins.cfg",
                             "./ogre.cfg",
                             "./Ymir.log" ]}),

    %%Load test module media
    ok = core_call({addResourceLocation, [ 
            {"./media/fonts", "FileSystem", "General", false},
            {"./media/themes", "FileSystem", "General", false},
            {"./media/materials/programs", "FileSystem", "General", false},
            {"./media/materials/scripts", "FileSystem", "General", false},
            {"./media/materials/textures", "FileSystem", "General", false},
            {"./media/models", "FileSystem", "General", false} ]}),

    ok = core_call({initialiseAllResourceGroups, []}),

    {Demos, _Final} = load_modules(),

    ok = gen_event:add_handler( event_manager, ymir_demo_event_manager, [] ),

    Actions = draw_main_menu(Demos),
   
    ok = gen_event:call(event_manager, 
                   ymir_demo_event_manager, 
                   {set_actions, Actions}),

    ok = core_call({addEventHandler, []}),

    %ok = core_call({create, [{ "Camera",
    %                           "camera",
    %                           []}]}),

    %ok = core_call({setViewport, ["Camera"]}),

    %ok = core_call({initialiseMyGUI, ["core.xml"]}),

    ok = timer:start(),
    {ok, TRef} = render_timer(?TIMER_DWELL_DEFAULT),

    {ok, State#demoState{ demos = Demos, timerDwell = ?TIMER_DWELL_DEFAULT, renderTimer = TRef}}.

load_modules() -> 


    io:format("Node: ~p~n", [node()]),

    %%Init mnesia to store set of active demos
    %mnesia:create_schema([node()]), 
    %mnesia:start(),
    %mnesia:create_table(demo, [{attributes, record_info(fields, demo)}]),

    Files = filelib:wildcard("./ebin/ymir_demo_module_*.beam"),

    lists:foldl( fun(M, A) -> load_demo(M, A) end, 
                 {[], {10, 10, 300, 26}},
                 Files).

 
load_demo(ModulePath, {Modules, {Left, Top, Width, Height}}) ->   
    ModuleRoot = filename:rootname(ModulePath),
    
    io:format("Trying to load ~s~n", [ModuleRoot]),

    %Load the demo module
    { module, Module } = code:load_abs(ModuleRoot),

    Button = {Module:title(),
                               "button",
                                [
                                       {"position", {Left, Top, Width, Height}},
                                       {"align", "Default"},
                                       {"layer", "Main"},
                                       {"skin", "Button"},
                                       {"caption", Module:title()} 
                                 ]},

    B = fun(S) -> demo_start(Module, S) end, 
    Action = {{guiMouseButtonClick, Module:title()}, B},
    %Record the demo in the database
    %F = fun() ->
    %        mnesia:write( #demo{ title = Module:title(), module = Module} ) 
    %    end,

    %{atomic, ok} = mnesia:transaction(F),

    { Modules ++ [{Module, Button, Action}], 
        {Left, Top + Height + 10, Width, Height} }.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%  GEN_EVENT EXPORTS
%%%%%%%%%%%%%%%%%%%%%%%%%%
stop(State) -> 
    %%ok = call_worker({ymir_core, renderStop, []}),

    ymir_demo:core_call({stop, []}),

    application:stop(ymir_demo),
     
    State.


show_main_menu(Module, State) ->

    Actions = gen_server:call(ymir_demo, {unload_demo, Module}), 

    State#eventState{actions = dict:from_list(Actions)}.

%hide_main_menu() ->
%    F = fun(D, A) ->
%        ymir_demo:core_call({update, [ {D#demo.title, "button", [{"visible", false}]} ]}),
%        A        
%    end,
%
%    %%Hide every loaded demo's GUI button
%    {atomic, ok} = mnesia:transaction( fun() -> mnesia:foldl(F, ok, demo) end ).

demo_start(Module, State) when is_atom(Module) ->
  
    Actions = gen_server:call(ymir_demo, {load_demo, Module}),
    
    State#eventState{actions = dict:from_list(Actions)}.
