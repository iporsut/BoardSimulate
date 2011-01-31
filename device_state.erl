-module(device_state).
-behaviour(gen_server).
-compile(export_all).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Public API

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [{input,"DRY1: OPEN\r\nDRY2: OPEN\r\nDRY3: OPEN\r\nDRY4: OPEN\r\nDRY5: OPEN\r\nDRY6: OPEN\r\nDRY7: OPEN\r\nDRY8: OPEN\r\nDRY9: OPEN\r\nDRY10: OPEN\r\nDRY11: OPEN\r\nDRY12: OPEN\r\nDRY13: OPEN\r\nDRY14: OPEN\r\nDRY15: OPEN\r\nDRY16: OPEN\r\n"},{output,"RELAY1: OFF\r\nRELAY2: OFF\r\nRELAY3: OFF\r\nRELAY4: OFF\r\nRELAY5: OFF\r\nRELAY6: OFF\r\nRELAY7: OFF\r\nRELAY8: OFF\r\nRELAY9: OFF\r\nRELAY10: OFF\r\nRELAY11: OFF\r\nRELAY12: OFF\r\nRELAY13: OFF\r\nRELAY14: OFF\r\nRELAY15: OFF\r\nRELAY16: OFF\r\n"}],[]).


stop(Module) ->
  gen_server:call(Module, stop).

stop() ->
  stop(?MODULE).

state(Module) ->
  gen_server:call(Module, state).

set_input(Port,Value) ->
  gen_server:call(?MODULE,{input,Port,Value}).

set_output(Port,Value) ->
  gen_server:call(?MODULE,{output,Port,Value}).
  
state() ->
  state(?MODULE).


init(State) ->
    {ok, State}.

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(state, _From, State) ->
  {reply, State, State};

handle_call({input,Port,Value}, _From, State) ->
    Input = proplists:get_value(input,State),
            InputChanged = re:replace(Input,"DRY"++Port++": (OPEN|CLOSE)","DRY"++Port++": "++Value,[{return,list}]),
            NewState = [{input,InputChanged} | proplists:delete(input,State)],
  {reply, NewState,NewState};

handle_call({output,Port,Value},_From,State) ->
    Output = proplists:get_value(output,State),
            OutputChanged = re:replace(Output,"RELAY"++Port++": (OFF|ON)","RELAY"++Port++": "++Value,[{return,list}]),
            NewState = [{output,OutputChanged} | proplists:delete(output,State)],
    {reply, NewState,NewState};
    
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
