%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : test application calc
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(app_deploy).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%-include("interfacesrc/brd_local.hrl").

%% --------------------------------------------------------------------
 
%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state,{
	      }).

%% --------------------------------------------------------------------
-define(SYNC_INTERVAL,2*1000*60).
%% ====================================================================
%% External functions
%% ====================================================================


-export([load_start_app/2,
	 stop_unload_app/2
	]).

-export([start/0,
	 stop/0
	 ]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================

%% Asynchrounus Signals

%% Gen server function

start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).



%%-----------Call ------------------------------------------------------------
load_start_app(Node,App)->
    gen_server:call(?MODULE, {load_start_app,Node,App},10*10000).

stop_unload_app(Node,App)->
    gen_server:call(?MODULE, {stop_unload_app,Node,App},10*1000).

%%----------Cast-------------------------------------------------------------

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%
%% --------------------------------------------------------------------
init([]) ->
    StartEvent=[{node,node()},{event_level,info},{event_info,['Started server',?MODULE]}],
    rpc:cast(node(),app_deploy_lib,cast,[log,{log,add_event,[StartEvent]}]),
    {ok, #state{}}.   
    
%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (aterminate/2 is called)
%% --------------------------------------------------------------------

handle_call({load_start_app,Node,App}, _From, State) ->
%    Reply={?MODULE,?LINE},
    Reply=rpc:call(node(),app_deploy_lib,load_start_app,[Node,App],5000),
   % Event=[{node,node()},{event_level,info},{event_info,['load_start_app ',Node,App,Reply]}],
   % rpc:cast(node(),controller_lib,cast,[log,{log,add_event,[Event]}]),
    {reply, Reply, State};

handle_call({stop_unload_app,Node,App}, _From, State) ->
%    Reply={?MODULE,?LINE},
    Reply=rpc:call(node(),app_deploy_lib,stop_unload_app,[Node,App],5000),
   % Event=[{node,node()},{event_level,info},{event_info,['stop_unload_app ',Node,App,Reply]}],
  %  rpc:cast(node(),controller_lib,cast,[log,{log,add_event,[Event]}]),
    {reply, Reply, State};


handle_call({stop}, _From, State) ->
  %  io:format("stop ~p~n",[{?MODULE,?LINE}]),
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{?MODULE,?LINE,Msg}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)

handle_info(Info, State) ->
    io:format("unmatched match info ~p~n",[{?MODULE,?LINE,Info}]),
    {noreply, State}.


%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Exyernal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------
    

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

