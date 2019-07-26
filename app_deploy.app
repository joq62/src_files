%% This is the application resource file (.app file) for the 'base'
%% application.
{application, app_deploy,
[{description, "app_deploy" },
{vsn, "1.0.0" },
{modules, 
	  [app_deploy_app,app_deploy_sup,
	   app_deploy,app_deploy_lib]},
{registered,[app_deploy]},
{applications, [kernel,stdlib]},
{mod, {app_deploy_app,[]}},
{start_phases, []}
]}.
