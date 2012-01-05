-module(ymir_demo).

-export([start_link/0, stop/1, module_start/2]).

-include("event_state.hrl").

-record( demoState, { modules } ).

call( Msg )->
    gen_server:call(ogre_manager, Msg).

call_worker( {Lib, Func, Args} ) -> 
    gen_server:call(ogre_manager, {worker, Lib, Func, Args}).

demo_load_module(ModulePath, {Modules, {Left, Top, Width, Height}}) ->   
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
   
    F = fun(State) -> module_start(Module, State) end,
    ok = gen_event:call( event_manager, 
                         ymir_demo_event_manager,
                         {add_actions, [{{guiMouseButtonClick, Module:title()}, F}]} ),

    { dict:store(Module:title(), Module, Modules), {Left, Top + Height + 10, Width, Height}}.

demo_load_modules(ModuleFiles) when is_list(ModuleFiles) ->
    F = fun(M, A) -> demo_load_module(M, A) end,
    
    %%Add
    {Modules, _Final} = lists:foldl(F, {dict:new(),{10, 10, 300, 26}}, ModuleFiles),

    Modules.

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

    F = fun(S) -> stop(S) end,
    ok = gen_event:add_handler( event_manager, 
                                ymir_demo_event_manager, 
                                [{{keyDown, 1}, F}]),

    Files = filelib:wildcard("./ebin/ymir_demo_module_*.beam"),
    Modules = demo_load_modules(Files),

    %%<<HERE>> Load window components
    #demoState{ modules = Modules }.

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


main_menu(Module, State) -> 
    ok =  Module:stop(),

    %% Hide the button <<HERE>> Hide all buttons or hide layer!
    ok = call_worker({ymir_core, update, [ {Module:title(), "button", [{"visible", true}]} ]}),

    State.

   

module_start(Module, State) when is_atom(Module) ->
    
    %% Hide the button <<HERE>> Hide all buttons or hide layer!
    ok = call_worker({ymir_core, update, [ {Module:title(), "button", [{"visible", false}]} ]}),

    %% Turn over control to Module
    ok = Module:start(),

    %% Escape key returns to menu menu
    %%F = fun(S) -> main_menu(S) end,
    %%Actions = [{{keyDown, 1}, F}] ++ Module:actions(),
    ymir_demo_event_manager:add_actions( [{{keyDown, 16}, fun(S) -> main_menu(Module, S) end }] ++
                                         Module:actions(), State ).
