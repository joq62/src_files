%% This is the application resource file (.app file) for the 'base'
%% application.
{application, app_discovery,
[{description, "app_discovery" },
{vsn, "1.0.0" },
{modules, 
	  [app_discovery_app,app_discovery_sup,
	   app_discovery,app_discovery_lib]},
{registered,[app_discovery]},
{applications, [kernel,stdlib]},
{mod, {app_discovery_app,[]}},
{start_phases, []}
]}.
