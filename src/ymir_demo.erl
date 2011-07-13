-module(ymir_demo).

-behaviour(gen_event).

-export([start_link/0, init/1, handle_event/2, handle_info/2, handle_call/2, terminate/2]).

call( Msg )->
    gen_server:call(ogre_manager, Msg).

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
    Light = { "Light", "light", [ {"position", [{"x", 20.0}, {"y", 80.0}, {"z", 0.0}]},
                                  {"source", "point"} ]},
    Head = {"Head", "entity", [{"mesh", "ogrehead.mesh"}]},
    Camera = { "Camera", "camera", [{"position", [{"x", 0.0}, {"y", 0.0}, {"z", 80.0}]},
                                    {"lookAt", [{"x", 0.0}, {"y", 0.0}, {"z", -300.0}]},
                                    {"nearClip", 5.0}] },

    ok = call({'OgreManager', initialiseAllResourceGroups, []}),

    io:format("Loading objects!!~n"),
    ok = call({'OgreManager', createObject, [Camera, Light, Head]}),

    io:format("Assigning viewport!~n"),
    ok = call({'OgreManager', setViewport, ["Camera"]}),
    
    io:format("Registering for input events!~n"),
    ok = call({'OgreManager', addEventHandler, []}).

render() ->
   ok = gen_event:add_handler(event_manager, ?MODULE, []),

   %Load demo
   load(),

   ok = gen_server:call(ogre_manager, {'OgreManager', renderStart, []}, infinity). 


start_link()->
    {ok, spawn( fun() -> render() end )}.


%%%%%%%%%%%%%%%%%%%%%%%%%%
%  GEN_EVENT EXPORTS
%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_Args)->
    io:format("INIT GEN_EVENT SIDE!~n"),
    {ok, []}.

handle_event(Event, State) ->
    io:format("**** EVENT ***** ~p~n", [Event]),
    {ok, State}.

handle_call( _Call, State ) ->
    {ok, ok, State}.

handle_info( _Info, State )->
    {ok, State}.

terminate( _Args, _State ) ->
    stop.
