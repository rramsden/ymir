-module(ymir_demo).

-export([start_link/0, stop/1, demo_start/2]).

-include("event_state.hrl").

-record( demo, { title, module } ).

-record( demoState, { modules } ).

call( Msg )->
    gen_server:call(ogre_manager, Msg).

call_worker( {Lib, Func, Args} ) -> 
    gen_server:call(ogre_manager, {worker, Lib, Func, Args}).

load_demo(ModulePath, {Actions, {Left, Top, Width, Height}}) ->   
    ModuleRoot = filename:rootname(ModulePath),
    
    io:format("Trying to load ~s~n", [ModuleRoot]),

    %Load the demo module
    { module, Module } = code:load_abs(ModuleRoot),

    %%Create the demo's button 
    ok = call({ymir_core, create, [ {Module:title(),
                                           "button",
                                           [
                                            {"position", {Left, Top, Width, Height}},
                                            {"align", "Default"},
                                            {"layer", "Main"},
                                            {"skin", "Button"},
                                            {"caption", Module:title()} ]}
                                         ]}),

    %Record the demo in the database
    F = fun() ->
            mnesia:write( #demo{ title = Module:title(), module = Module} ) 
        end,

    {atomic, ok} = mnesia:transaction(F),


    B = fun(S) -> demo_start(Module, S) end, 
    {Actions ++ [{{guiMouseButtonClick, Module:title()}, B}], {Left, Top + Height + 10, Width, Height}}.

init() -> 

  
    ok = call({load, ymir_core}),

    ok = call({ymir_core, start, [ "Ymir Demo",
                                   "./plugins.cfg",
                                   "./ogre.cfg",
                                   "./Ymir.log" ]}),

    %%Load test module media
    ok = call({ymir_core, addResourceLocation, [ 
            {"./media/fonts", "FileSystem", "General", false},
            {"./media/themes", "FileSystem", "General", false},
            {"./media/materials/programs", "FileSystem", "General", false},
            {"./media/materials/scripts", "FileSystem", "General", false},
            {"./media/materials/textures", "FileSystem", "General", false},
            {"./media/models", "FileSystem", "General", false} ]}),

    ok = call({ymir_core, initialiseAllResourceGroups, []}),

    ok = call({ymir_core, create, [{ "Camera",
                                      "camera",
                                      []}]}),

    ok = call({ymir_core, setViewport, ["Camera"]}),

    ok = call({ymir_core, initialiseMyGUI, ["core.xml"]}),

    ok = call({ymir_core, addEventHandler, []}),


    io:format("Node: ~p~n", [node()]),

    %%Init mnesia to store set of active demos
    mnesia:create_schema([node()]),    
    mnesia:start(),
    mnesia:create_table(demo, [{attributes, record_info(fields, demo)}]),

    Files = filelib:wildcard("./ebin/ymir_demo_module_*.beam"),
    {Actions, _Final} = lists:foldl( fun(M, A) -> load_demo(M, A) end, 
                                     {[], {10, 10, 300, 26}},
                                     Files), 

    ok = gen_event:add_handler( event_manager, 
                                ymir_demo_event_manager, 
                                Actions++ [{{keyDown, ?KC_ESCAPE}, fun(S) -> stop(S) end}]),

    #demoState{ }.

render() ->

    %Load demo
    init(),

    ok = gen_server:call( ogre_manager, 
                          {ymir_core, renderStart, []}, 
                          infinity).

start_link()->
    {ok, spawn( fun() -> render() end )}.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%  GEN_EVENT EXPORTS
%%%%%%%%%%%%%%%%%%%%%%%%%%
stop(State) -> 
    ok = call_worker({ymir_core, renderStop, []}),

    ok = call_worker({ymir_core, stop, []}),

    application:stop(ymir_demo),
     
    State.


show_main_menu(Module, State) ->

    Module:stop(),

    F = fun(D, A) -> 
        ok = call_worker({ymir_core, update, [ {D#demo.title, "button", [{"visible", true}]} ]}),
        A        
    end,

    %%Hide every loaded demo's GUI button
    {atomic, ok} = mnesia:transaction( fun() -> mnesia:foldl(F, ok, demo) end ),

    State.

hide_main_menu() ->
    F = fun(D, A) -> 
        ok = call_worker({ymir_core, update, [ {D#demo.title, "button", [{"visible", false}]} ]}),
        io:format("Hiding button: ~p~n", [D#demo.title]), 
        A        
    end,

    %%Hide every loaded demo's GUI button
    {atomic, ok} = mnesia:transaction( fun() -> mnesia:foldl(F, ok, demo) end ).

demo_start(Module, State) when is_atom(Module) ->
   
    hide_main_menu(),
 
    %% Turn over control to Module
    ok = Module:start(),

    %% Q key returns to menu menu
    ymir_demo_event_manager:add_actions( [{{keyDown, ?KC_Q}, fun(S) -> show_main_menu(Module, S) end }] ++
                                         Module:actions(), State ).
