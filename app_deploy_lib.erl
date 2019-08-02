%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(app_deploy_lib).
 


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-define(NODE_APP_CONFIG,"node_app.config").
-define(PATH_EBIN,"ebin_files").
-define(PATH_APP_FILES,"app_files").
-define(PATH_SRC_FILES,"src_files").
%% External exports,
%-compile(export_all).

-export([load_start_app/2,stop_unload_app/2,
	 cast/2
	]).


%% ====================================================================
%% External functions
%% ====================================================================

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
load_start_app(Node,Application)->
    case net_adm:ping(Node) of
	pang->
	    Reply={error,[?MODULE,?LINE,date(),time(),nodedown,Node]};
	pong->
	    Apps=rpc:call(Node,application,which_applications,[],5000),
%	    Apps=rpc:call(Node,application,loaded_applications,[],5000),
%	    io:format("Node, Apps = ~p~n",[{?MODULE,?LINE,Node,Apps}]),
	    case lists:keymember(Application,1,Apps) of
		false->	  
		   % Read app file 
		    AppFilename=atom_to_list(Application)++".app",
		    AppFullFilename=filename:join(?PATH_APP_FILES,AppFilename),
		    {ok,Terms}=file:consult(AppFullFilename),
		    [{application,Application,Info}]=Terms,
		    {modules,Modules}=lists:keyfind(modules,1,Info),
		 %   Modules_Filenames=[{Module,filename:join(?PATH_EBIN,atom_to_list(Module)++".beam")}||Module<-Modules],
		    Modules_Filenames=[{Module,filename:join(?PATH_SRC_FILES,atom_to_list(Module)++".erl")}||Module<-Modules],
		    Result = load_modules(Node,Modules_Filenames,[]),
%		    io:format("Result = load_modules ~p~n",[{?MODULE,?LINE,Result}]),
		    {ok,Binary}=file:read_file(AppFullFilename),
		    ok=rpc:call(Node,file,write_file,[AppFilename,Binary],5000),
		    case rpc:call(Node,application,start,[Application]) of
			ok->
			    Reply=ok,
			    rpc:eval_everywhere(app_discovery,update_app_lists,[]);
			Err->
			    Reply={error,[?MODULE,?LINE,date(),time(),Err,Node,Application]}
		    end;    
		true->
		    Reply={error,[?MODULE,?LINE,date(),time(),'already_loaded_started',Node,Application]}
	    end
    end,
    Reply.

load_modules(_Node,[],Result)->
    Result;
load_modules(Node,[{Module, Filename}|T],Acc)->
    {ok,Binary}=file:read_file(Filename),
    BaseName=filename:basename(Filename),
  %  R=rpc:call(Node,code,load_binary,[Module, Filename, Binary],5000),  
    ok=rpc:call(Node,file,write_file,[BaseName, Binary],5000),
    R=rpc:call(Node,c,c,[Module],5000),
    
  %  R=rpc:call(Node,code,load_binary,[Module, BaseName, Binary],5000), 
    NewAcc=[{Node,Module,R}|Acc],
  %  NewAcc=[{Node,Module}|Acc],
    load_modules(Node,T,NewAcc).


stop_unload_app(Node,Application)->
    Apps=rpc:call(Node,application,loaded_applications,[],5000),
    case lists:keymember(Application,1,Apps) of
	true->
              % Read app file and extract modules
	    AppFilename=atom_to_list(Application)++".app",
	    {ok,Terms}=rpc:call(Node,file,consult,[AppFilename]),
	    [{application,Application,Info}]=Terms,
	    {modules,Modules}=lists:keyfind(modules,1,Info),
	    [rpc:call(Node,file,delete,[atom_to_list(Module)++".erl"])||Module<-Modules],
	    [rpc:call(Node,file,delete,[atom_to_list(Module)++".beam"])||Module<-Modules],
	    rpc:call(Node,application,stop,[Application]),
	    rpc:call(Node,application,unload,[Application]),
	    rpc:call(Node,file,delete,[AppFilename]),
	    unload_modules(Node,Modules),
	    Reply=ok,
	    rpc:eval_everywhere(app_discovery,update_app_lists,[]);
	false ->
	    Reply={error,['eexist',Application]}
    end,
    Reply.

unload_modules(_Node,[])->
    ok;
unload_modules(Node,[Module|T])->
    _RP=rpc:call(Node,code,purge,[Module],5000),
    _RD=rpc:call(Node,code,delete,[Module],5000),
    unload_modules(Node,T).

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
