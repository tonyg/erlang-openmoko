{application, openmoko, %% -*- erlang -*-
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

%% 	 {modem_module, fake_serial},
%% 	 {modem_power_control_file, "/dev/null"},
%% 	 {dets_path, "/tmp"},

	 {modem_module, serial},
	 {modem_power_control_file, "/sys/devices/platform/neo1973-pm-gsm.0/power_on"},
	 {dets_path, "/media/card"},

	 {modem_device, "/dev/ttySAC0"}]}
 ]}.
