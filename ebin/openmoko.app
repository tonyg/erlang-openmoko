{application, openmoko,
 [{description, "OpenMoko"},
  {id, "OpenMoko"},
  {vsn, "%%VERSION%%"},
  {modules, [openmoko]},
  {registered, [modem_server,
                openmoko_sup]},
  {applications, [kernel,
		  stdlib,
		  sasl
		  %, mnesia
		 ]},
  {mod, {openmoko, []}},
  {env, [

	 {modem_module, fake_serial},
	 {modem_power_control_file, "/dev/null"},

%% 	 {modem_module, serial},
%% 	 {modem_power_control_file, "/sys/devices/platform/gta01-pm-gsm.0/power_on"},

	 {modem_device, "/dev/ttySAC0"}]}
 ]}.
