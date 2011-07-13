-module(ymir_demo_sup).

-behaviour(supervisor).

-export( [ start_link/1, init/1 ] ).

start_link(_StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    RestartStrategy    = one_for_one,
    MaxRestarts        = 10,
    MaxTimeBetRestarts = 30,

    Flags = {RestartStrategy, MaxRestarts, MaxTimeBetRestarts},

    Specs = [
                %% Gen_cnode interface to Ogre3D
                {   ogre_manager,
                    {gen_cnode, start_link, [[{name, ogre_manager}, {port, 30000}, {workers, 2}]]},
                    permanent, 
                    1000,
                    worker,
                    [gen_cnode]
                },

                %% Dispatches events originating form ogre_manager
                {   event_manager,
                    {gen_event, start_link, [{local, event_manager}]},
                    permanent, 
                    1000,
                    worker,
                    [gen_event]
                },

                %% Renders the demo and handles events originating from demo
                {   ymir_demo, 
                    {ymir_demo, start_link, []}, 
                    permanent, 
                    1000, 
                    worker, 
                    [ymir_demo]
                }
            ],

    {ok, {Flags, Specs}}.
