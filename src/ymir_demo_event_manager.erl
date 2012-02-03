-module(ymir_demo_event_manager).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_info/2, 
         handle_call/2, terminate/2, set_actions/2, add_actions/2]).

-include("event_state.hrl").

init(Actions) when is_list(Actions) ->
    {ok, #eventState{actions = dict:from_list(Actions)} }.

set_actions( Actions, State ) when is_list(Actions),
                                   is_record(State, eventState) ->
    
    State#eventState{ actions = dict:from_list(Actions) }.

add_actions( Actions, State ) when is_list(Actions),
                              is_record(State, eventState) ->
    State#eventState{ actions = dict:merge( fun(_K, _V1, V2) -> V2 end,
                                            State#eventState.actions,
                                            dict:from_list(Actions))}.
%Events
%{keyPressed, X}
%{keyReleased, X}
%{mousePressed, X}
%{mouseReleased, X}
%{mouseMoved, Pos}

%%Actions

%{Event, Action}
%{Prereq, Action}%

%Pre = fun( Keyboard, Mouse ) -> true / false. 
%Action = fun( Keyboard, Mouse ) -> ok.  

gui_action(ID, Val, Action, State) ->
    Gui = State#eventState.gui,
    NotVal = not Val,

    case dict:find(ID, Gui) of
        {ok, Val} ->
            Action(State); 
   
        {ok, NotVal} ->
           State; 

        _Undef when Val == false ->
            Action(State);
        
        _Undef ->
           State 
    end.

keyboard_action(ID, Val, Action, State) ->
    Keyboard = State#eventState.keyboard,
    Mouse = State#eventState.mouse,
    NotVal = not Val,

    case dict:find(ID, Keyboard) of
        {ok, Val} ->
            Action(State); 
        {ok, NotVal} ->
            State; 

        _Undef when Val == false ->
            Action(State);
        _Undef ->
           State 
    end.

mouse_action(ID, Val, Action, State) ->
    Keyboard = State#eventState.keyboard,
    Mouse = State#eventState.mouse,
    NotVal = not Val,

    case dict:find(ID, Mouse) of
        {ok, Val} ->
            Action(State); 
        {ok, NotVal} ->
           State; 

        _Undef when Val == false ->
            Action(State);
        _Undef ->
            State
    end.

process_action(frameStarted, Action, State) when is_function(Action, 1) ->
    Action(State);

process_action({keyUp, ID}, Action, State) when is_integer(ID),
                                                 is_function(Action, 1) ->

    keyboard_action(ID, false, Action, State);                                                

process_action({keyDown, ID}, Action, State) when is_integer(ID),
                                                    is_function(Action, 1) ->
    keyboard_action(ID, true, Action, State);                                                       

process_action({mouseUp, ID}, Action, State) when is_integer(ID),
                                                    is_function(Action, 1) ->
    mouse_action(ID, false, Action, State);

process_action(mouseMoved, Action, State) when is_function(Action, 1) ->

   case dict:fetch(moved, State#eventState.mouse) of

        true -> 
            Action(State);

        false ->
            State 
            
   end;

process_action({mouseDown, ID}, Action, State) when is_integer(ID), 
                                                      is_function(Action, 1) ->
    mouse_action(ID, true, Action, State);

process_action({guiMouseDown, {Name, Button}}, Action, State) when is_list(Name),
                                                                     is_integer(Button) ->
    gui_action({Name, Button}, true, Action, State);

process_action({guiMouseUp, {Name, Button}}, Action, State) when is_list(Name),
                                                                   is_integer(Button) ->
    gui_action({Name, Button}, false, Action, State);

process_action({guiMouseButtonClick, Name}, Action, State) ->
    State2 = gui_action(Name, true, Action, State),



    %%Process each click only once, clear the true flag
    State2#eventState{ gui = dict:store( Name,
                                         false,
                                         State2#eventState.gui) };

%process_action({Prereq, Action}, State) when is_function(Prereq, 2),
%                                             is_function(Action, 1) ->
%     Keyboard = State#eventState.keyboard,
%     Mouse = State#eventState.mouse,
%
%     case Prereq(Keyboard, Mouse) of
%        true -> Action(Keyboard, Mouse);
%        false -> State
%     end;
     
process_action(Something, _Val, State) ->
    io:format("What?  ~p~n", [Something]),
    State.
        
handle_event(frameStarted, State) ->
    F = fun(K, V, A) -> process_action(K, V, A) end,

    Temp = dict:fold(F, State, State#eventState.actions),

    %%Process the list of actions, calling every
    %%action whose prerequisites have been met.
    {ok, Temp#eventState{mouse = dict:store(moved, false, Temp#eventState.mouse)} };

handle_event({keyPressed, Key}, State) when is_record(State, eventState) ->
    Keyboard = State#eventState.keyboard,
    Mouse = State#eventState.mouse,

    {ok, State#eventState{ keyboard = dict:store(Key, true, Keyboard), 
                      mouse = Mouse }};

handle_event({keyReleased, Key}, State) when is_record(State, eventState) ->
    Keyboard = State#eventState.keyboard,
    Mouse = State#eventState.mouse,

    {ok, State#eventState{ keyboard = dict:store(Key, false, Keyboard), 
                           mouse = Mouse }};

handle_event({mousePressed,{Key, _Pos}}, State) when is_record(State, eventState) ->
    Keyboard = State#eventState.keyboard,
    Mouse = State#eventState.mouse,

    {ok, State#eventState{ keyboard = Keyboard, 
                           mouse = dict:store(Key, true, Mouse) }};

handle_event({mouseReleased, {Key, _Pos}}, State) when is_record(State, eventState) ->
    Keyboard = State#eventState.keyboard,
    Mouse = State#eventState.mouse,

    {ok, State#eventState{ keyboard = Keyboard, 
                           mouse = dict:store(Key, false, Mouse) }};

handle_event({mouseMoved, Pos}, State) when is_record(State, eventState) ->
    Keyboard = State#eventState.keyboard,
    Mouse = State#eventState.mouse,
    {_ID, [Dx, Dy, Dz]} = Pos,

    Temp = State#eventState{mouse = dict:store(moved, true, State#eventState.mouse) },

    %%Update current mouse position
    {ok, State#eventState{ keyboard = Keyboard,
                           mouse = dict:store(current, {Dx, Dy, Dz}, Temp#eventState.mouse) }};


handle_event({guiMousePressed, {ID, Button}}, State) when is_list(ID), 
                                                          is_integer(Button),
                                                          is_record(State, eventState) ->
    
    {ok, State#eventState{ gui = dict:store({ID, Button},
                                            true,
                                            State#eventState.gui) }};

handle_event({guiMouseReleased, {ID, Button}}, State) when is_list(ID),
                                                          is_integer(Button),
                                                          is_record(State, eventState) ->
    
    {ok, State#eventState{ gui = dict:store({ID, Button},
                                            false,
                                            State#eventState.gui) }};

handle_event({guiMouseButtonClick, ID}, State ) ->

    {ok, State#eventState{ gui = dict:store(ID,
                                            true,
                                            State#eventState.gui) }};

handle_event( Event, State )->
    io:format("Got Event! ~p~n", [Event]),
    {ok, State}.

%%Add/remove actions
handle_call({set_actions, Actions}, State) when is_list(Actions) ->
    {ok, ok, set_actions(Actions, State)};

handle_call({add_actions, Actions}, State) when is_list(Actions) ->
    {ok, ok, add_actions(Actions, State)};

handle_call(clear_actions, State) ->
    {ok, ok, State#eventState{ actions = [] }};

handle_call( _Call, State ) ->
    {ok, ok, State}.

handle_info( _Info, State )->
    {ok, State}.

terminate( _Args, _State ) ->
    stop.
