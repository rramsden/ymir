-module(ymir_demo).

-export([start_link/0, stop/2]).

-record( demoState, { modules } ).

call( Msg )->
    gen_server:call(ogre_manager, Msg).

demo_load_module(ModulePath, {Modules, {Left, Top, Width, Height}}) ->   
    ModuleRoot = filename:rootname(ModulePath),
    
    io:format("Trying to load ~s~n", [ModuleRoot]),

    %Load the demo module
    { module, Module } = code:load_abs(ModuleRoot),

    %%Create the demo's button 
    ok = call({'OgreManager', addObject, [ {Module:title(),
                                           "button",
                                           [
                                            {"position", {Left, Top, Width, Height}},
                                            {"align", "Default"},
                                            {"layer", "Main"},
                                            {"skin", "Button"},
                                            {"caption", Module:title()} ]}
                                         ]}),
    
    { dict:store(Module:title(), Module, Modules), {Left, Top + Height + 10, Width, Height}}.

demo_load_modules(ModuleFiles) when is_list(ModuleFiles) ->
    F = fun(M, A) -> demo_load_module(M, A) end,
    
    %%Add
    {Modules, _Final} = lists:foldl(F, {dict:new(),{10, 10, 300, 26}}, ModuleFiles),

    Modules.

init() -> 

    ok = call({load,'OgreManager'}),

    ok = call({'OgreManager', start, [ "Ymir Demo",
                                       "./plugins.cfg",
                                       "./ogre.cfg",
                                       "./Ymir.log" ]}),

    %%Load test module media
    ok = call({'OgreManager', addResourceLocation, [ 
            {"./media/fonts", "FileSystem", "General", false},
            {"./media/themes", "FileSystem", "General", false},
            {"./media/materials/programs", "FileSystem", "General", false},
            {"./media/materials/scripts", "FileSystem", "General", false},
            {"./media/materials/textures", "FileSystem", "General", false},
            {"./media/models", "FileSystem", "General", false} ]}),

    ok = call({'OgreManager', initialiseAllResourceGroups, []}),

    ok = call({'OgreManager', addObject, [{ "Camera",
                                            "camera",
                                            []}]}),

    ok = call({'OgreManager', setViewport, ["Camera"]}),

    ok = call({'OgreManager', initialiseMyGUI, ["MyGUI_Core.xml"]}),

    ok = call({'OgreManager', addEventHandler, []}),

    Files = filelib:wildcard("./ebin/ymir_demo_module_*.beam"),
    Modules = demo_load_modules(Files),

    %%<<HERE>> Load window components
    #demoState{ modules = Modules }.

render() ->

    %Load demo
    State = init(),

    F = fun(A, B) -> stop(A, B) end,
    ok = gen_event:add_handler( event_manager, 
                                ymir_demo_event_manager, 
                                [{{keyDown, 1}, F}]),

    ok = gen_server:call( ogre_manager, 
                          {'OgreManager', renderStart, []}, 
                          infinity).

start_link()->
    {ok, spawn( fun() -> render() end )}.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%  GEN_EVENT EXPORTS
%%%%%%%%%%%%%%%%%%%%%%%%%%
stop(_Keyboard, _Mouse) -> 
    ok = gen_server:call(ogre_manager, 
            {worker, 'OgreManager', renderStop, []}),

   ok = gen_server:call(ogre_manager, 
            {worker, 'OgreManager', stop, []}),

    application:stop(ymir_demo).
