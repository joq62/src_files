%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : test application calc
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(controller).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%-include("interfacesrc/brd_local.hrl").

%% --------------------------------------------------------------------
-define(NUM_TRIES,5).
-define(DEPLOY_INTERVAL,5000).
%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state,{available_nodes,node_apps_list,latest_start_result
	      }).

%% --------------------------------------------------------------------
-define(SYNC_INTERVAL,10*1000).
%% ====================================================================
%% External functions
%% ====================================================================


-export([start_app/2,stop_app/2,
	 print_events/1,
	 ctrl_info/0,
	 update_config/2,
	 sync/1
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
ctrl_info()-> gen_server:call(?MODULE, {ctrl_info},infinity).

start_app(Node,Application)-> gen_server:call(?MODULE, {start_app,Node,Application},infinity).
stop_app(Node,Application)-> gen_server:call(?MODULE, {stop_app,Node,Application},infinity).

%%----------Cast-------------------------------------------------------------
print_events(Num)->
    gen_server:cast(?MODULE, {print_events,Num}). 

sync(Interval)->
    gen_server:cast(?MODULE, {sync,Interval}).    

update_config(Filename,Binary)->
    gen_server:cast(?MODULE, {update_config,Filename,Binary}).
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
  %   
    application:start(app_discovery),    
    application:start(log), 
    application:start(app_deploy),
    _R=rpc:call(node(),controller_lib,deploy_app_discovery,[?NUM_TRIES,?DEPLOY_INTERVAL,false]),
  %  io:format("R= ~p~n",[{date(),time(),?MODULE,?LINE,R}]),
    spawn(controller,sync,[?SYNC_INTERVAL]),
    StartEvent=[{node,node()},{event_level,info},{event_info,['Started server',?MODULE]}],
    rpc:cast(node(),controller_lib,cast,[log,{log,add_event,[StartEvent]}]),
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
handle_call({start_app,Node,Application},_From, State) ->
    case rpc:call(node(),app_deploy,load_start_app,[Node,Application]) of
	ok->
	    Event=[{node,node()},{event_level,info},{event_info,['Application started',Node,Application]}],
	    rpc:cast(node(),controller_lib,cast,[log,{log,add_event,[Event]}]),
	    Reply=ok;
	{error,Err}->
	    Event=[{node,node()},{event_level,error},{event_info,[Err]}],
	    rpc:cast(node(),controller_lib,cast,[log,{log,add_event,[Event]}]),
	    Reply={error,Err};
	{badrpc,Err}->
	    Event=[{node,node()},{event_level,error},{event_info,[badrpc,Err]}],
	    rpc:cast(node(),controller_lib,cast,[log,{log,add_event,[Event]}]),
	    Reply={error,Err}
    end,
    {reply, Reply, State};

handle_call({stop_app,Node,Application},_From, State) ->
    case rpc:call(node(),app_deploy,stop_unload_app,[Node,Application]) of
	ok->
            Event=[{node,node()},{event_level,info},{event_info,['Application stopped',Node,Application]}],
	    rpc:cast(node(),controller_lib,cast,[log,{log,add_event,[Event]}]),
	    Reply=ok;
	{error,Err}->
	    Event=[{node,node()},{event_level,error},{event_info,[Err]}],
	    rpc:cast(node(),controller_lib,cast,[log,{log,add_event,[Event]}]),
	    Reply={error,Err};
	{badrpc,Err}->
	    Event=[{node,node()},{event_level,error},{event_info,[badrpc,Err]}],
	    rpc:cast(node(),controller_lib,cast,[log,{log,add_event,[Event]}]),
	    Reply={error,Err}
    end,
    {reply, Reply, State};




handle_call({ctrl_info},_From, State) ->
    Reply = {State#state.available_nodes,
	     State#state.node_apps_list,State#state.latest_start_result},
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
handle_cast({print_events,Num},State) ->
    Events=log:read_events(Num),
    [rpc:call(node(),controller_lib,print_event,[Event])||Event<-Events],
    {noreply, State};


handle_cast({sync,Interval},State) ->
%    io:format("*************** Start Campaign ***********' ~p~n",[{date(),time()}]), 
    case rpc:call(node(),controller_lib,campaign,[],30000) of
	{badrpc,Err}->
	    NewState=State,
	    io:format(" = ~p~n",[{?MODULE,?LINE,error,Err}]);
	{StartResult,AvailableNodes,NodeAppsList}->
	    NewState=State#state{available_nodes=AvailableNodes,
				 node_apps_list=NodeAppsList,
				 latest_start_result=StartResult},
%	    io:format("NodeAppsList = ~p~n",[{?MODULE,?LINE,AvailableNodes,NodeAppsList}]),
	   % Event=[{node,node()},{event_level,info},{event_info,['NodeAppsList',NodeAppsList]}],
	   % rpc:cast(node(),controller_lib,cast,[log,{log,add_event,[Event]}]),
	    ok
    end,
 %   case log:new_event() of
%	false->
%	    ok;
%	NewEvent->
%	    rpc:call(node(),controller_lib,print_event,[NewEvent])
 %   end,
 %   io:format("------------- End Campaign ----------' ~p~n",[{date(),time()}]),
    spawn(controller_lib,tick,[Interval]),
    {noreply, NewState};

handle_cast({update_config,Filename,Binary},State) ->
    ok=file:write_file(Filename,Binary),
    rpc:call(node(),controller_lib,campaign,[],5000),
    {noreply, State};

handle_cast(Msg, State) ->
    Event=[{node,node()},{event_level,error},{event_info,['unmathced signal ',?MODULE,?LINE,Msg]}],
    rpc:cast(node(),controller_lib,cast,[log,{log,add_event,[Event]}]),
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

