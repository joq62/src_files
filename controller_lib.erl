%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(controller_lib).
 



%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-define(NODE_APP_CONFIG,"node_app.config").
-define(NODES_CONFIG,"nodes.config").
-define(PATH_EBIN,"ebin_files").
-define(PATH_APP_FILES,"app_files").

-define(GIT_APP_FILES,"https://github.com/joq62/app_files.git").
-define(GIT_EBIN_FILES,"https://github.com/joq62/ebin_files.git").

%% External exports,
%-compile(export_all).

-export([campaign/0,deploy_app_discovery/3,tick/1,
	 cast/2
	]).


%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
cast(App,{M,F,A})->
 %   io:format("App,{M,F,A} = ~p~n",[{?MODULE,?LINE,App,{M,F,A}}]),
    case rpc:call(node(),app_discovery,query,[App]) of
	{badrpc,Err}->
	    Reply={error,[Err]};
	[AppNode|_] ->
	    Reply=rpc:cast(AppNode,M,F,A);
	[] ->
	    Reply={error,[{?MODULE,?LINE,'no avaible application',App}]}
    end,
    Reply.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
deploy_app_discovery(0,_,R)-> 
    R;
deploy_app_discovery(_,_,{ok,R}) -> 
    {ok,R};
deploy_app_discovery(N,Interval,_R)->  
    case lists:member(app_deploy,registered()) of
	false->
	    timer:sleep(Interval),
	    NewN=N-1,
	    NewR=error;
	true->
	    {ok,ConfigNodes}=file:consult(?NODES_CONFIG),
	    [net_adm:ping(Node)||{Node}<-ConfigNodes],
	    Nodes=nodes(),
	    _R=[{Node,app_discovery,rpc:call(node(),app_deploy,load_start_app,[Node,app_discovery])}||Node<-Nodes],
	    NewN=N-1,
	    NewR=ok
    end,
    deploy_app_discovery(NewN,Interval,NewR).

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
campaign()->

    {ok,ConfigNodes}=file:consult(?NODES_CONFIG),
    % Ping all nodes to secure tha resent started nodes are connected
  %  _A=[{Node,net_adm:ping(Node)}||{Node}<-ConfigNodes],
    [net_adm:ping(Node)||{Node}<-ConfigNodes],
    AvailableNodes=nodes(), % Exclude controller node

    {ok,Info}=file:consult(?NODE_APP_CONFIG),
    NodeAppsList=app_discovery:node_apps_list(),

    % Check applications to start and start them
    StartResult=apps_to_start(Info,NodeAppsList,AvailableNodes,[]),
    
    {StartResult,AvailableNodes,NodeAppsList}.

apps_to_start([],_,_,StartedApps)->
 %   io:format("StartedApps= ~p~n",[{?MODULE,?LINE,StartedApps}]),    
    StartedApps;
apps_to_start([{App,Nodes}|T],NodeAppsList,AvailableNodes,Acc) ->
  %  io:format("Info, T= ~p~n",[{?MODULE,?LINE,App,Nodes,T}]),
    % Check if non of the nodes has the the required applications
    %
    case Nodes of
	all->
	    A=[app_deploy:load_start_app(Node,App)||Node<-AvailableNodes];
	any ->
%	    io:format("~p~n",[{?MODULE,?LINE,any}]),
	    case app_discovery:query(App) of
		[]->
		    [Node|_]=AvailableNodes,
		   % io:format("Node ~p~n",[{?MODULE,?LINE,Node}]),
		    A=[app_deploy:load_start_app(Node,App)];
		   % io:format("A, Node,~p~n",[{?MODULE,?LINE,A,Node}]);
		_->
		    A=[{error,[?MODULE,?LINE,eexists]}]
	    end; 
	  Nodes->
	   %   io:format("~p~n",[{?MODULE,?LINE,Nodes}]),
	      A=[app_deploy:load_start_app(Node,App)||Node<-Nodes]
      end,
    NewAcc=lists:append(A,Acc),
   % io:format("NewAcc ~p~n",[{?MODULE,?LINE,NewAcc}]),
    apps_to_start(T,NodeAppsList,AvailableNodes,NewAcc).

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
tick(Interval)->
    timer:sleep(Interval),
    controller:sync(Interval).

