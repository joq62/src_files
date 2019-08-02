%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : test application calc
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(app_discovery).

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
-record(state,{node_apps_list,
	       app_nodes_list
	      }).

%% --------------------------------------------------------------------
-define(SYNC_INTERVAL,10*1000).
%% ====================================================================
%% External functions
%% ====================================================================


-export([ query/1,
	  update_app_lists/0,
	  all_apps/0,
	  node_apps_list/0,
	  app_nodes_list/0,
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
app_nodes_list()-> gen_server:call(?MODULE, {app_nodes_list},infinity).
node_apps_list()-> gen_server:call(?MODULE, {node_apps_list},infinity).

query(App)->
    gen_server:call(?MODULE, {query,App},15000).

all_apps()->
    gen_server:call(?MODULE, {all_apps},15000).
%%----------Cast-------------------------------------------------------------
update_app_lists()->
    gen_server:cast(?MODULE, {update_app_lists}).    

sync(Interval)->
    gen_server:cast(?MODULE, {sync,Interval}).    

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
    {NodeAppList,AppNodesList}=rpc:call(node(),app_discovery_lib,sync,[?SYNC_INTERVAL],5000),
    io:format("Started server ~p~n",[{date(),time(),?MODULE}]),
    {ok, #state{node_apps_list=NodeAppList,app_nodes_list=AppNodesList}}.   
    
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

handle_call({node_apps_list}, _From, State) ->
    Reply=State#state.node_apps_list,
    {reply, Reply, State};

handle_call({app_nodes_list}, _From, State) ->
    Reply=State#state.app_nodes_list,
    {reply, Reply, State};

handle_call({query,WantedApp}, _From, State) ->
%  {NodeAppList,AppNodesList}=app_discovery_lib:check_apps(),
    AppNodesList=State#state.app_nodes_list,
   % glurk=AppNodesList,
    
    Reply=[Node||{App,Node}<-AppNodesList,true==(WantedApp==App)],
    {reply, Reply, State};

handle_call({all_apps}, _From, State) ->
%    Reply={?MODULE,?LINE},
    Reply=State#state.node_apps_list,
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
handle_cast({update_app_lists},State) ->
    {NodeAppList,AppNodesList}=rpc:call(node(),app_discovery_lib,check_apps,[],5000),
    NewState=State#state{node_apps_list=NodeAppList,app_nodes_list=AppNodesList},
    spawn(app_discovery_lib,tick,[?SYNC_INTERVAL]),
    {noreply, NewState};

handle_cast({sync,Interval},State) ->
    {NodeAppList,AppNodesList}=rpc:call(node(),app_discovery_lib,sync,[Interval],5000),
    NewState=State#state{node_apps_list=NodeAppList,app_nodes_list=AppNodesList},
    {noreply, NewState};

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

