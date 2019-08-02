%% This is the application resource file (.app file) for the 'base'
%% application.
{application, oam,
[{description, "oam" },
{vsn, "1.0.0" },
{modules, 
	  [oam_app,oam_sup,
	   mon,mon_lib]},
{registered,[oam]},
{applications, [kernel,stdlib]},
{mod, {oam_app,[]}},
{start_phases, []}
]}.
