%% This is the application resource file (.app file) for the 'base'
%% application.
{application, system_controller,
[{description, "system_controller" },
{vsn, "1.0.0" },
{modules, 
	  [system_controller_app,system_controller_sup,
	   controller,controller_lib},
{registered,[controller,system_controller]},
{applications, [kernel,stdlib]},
{mod, {system_controller_app,[]}},
{start_phases, []}
]}.
