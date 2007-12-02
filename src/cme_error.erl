-module(cme_error).
-export([lookup/1]).

lookup(0) ->    phone_failure;
lookup(1) ->    no_connection_to_phone;
lookup(2) ->    phone_adapter_link_reserved;
lookup(3) ->    operation_not_allowed;
lookup(4) ->    operation_not_supported;
lookup(5) ->    ph_sim_pin_required;
lookup(6) ->    ph_fsim_pin_required;
lookup(7) ->    ph_fsim_puk_required;
lookup(10) ->   sim_not_inserted;
lookup(11) ->   sim_pin_required;
lookup(12) ->   sim_puk_required;
lookup(13) ->   sim_failure;
lookup(14) ->   sim_busy;
lookup(15) ->   sim_wrong;
lookup(16) ->   incorrect_password;
lookup(17) ->   sim_pin2_required;
lookup(18) ->   sim_puk2_required;
lookup(20) ->   memory_full;
lookup(21) ->   invalid_index;
lookup(22) ->   not_found;
lookup(23) ->   memory_failure;
lookup(24) ->   text_string_too_long;
lookup(25) ->   invalid_characters_in_text_string;
lookup(26) ->   dial_string_too_long;
lookup(27) ->   invalid_characters_in_dial_string;
lookup(30) ->   no_network_service;
lookup(31) ->   network_timeout;
lookup(32) ->   network_not_allowed_emergency_calls_only;
lookup(40) ->   network_personalization_pin_required;
lookup(41) ->   network_personalization_puk_required;
lookup(42) ->   network_subset_personalization_pin_required;
lookup(43) ->   network_subset_personalization_puk_required;
lookup(44) ->   service_provider_personalization_pin_required;
lookup(45) ->   service_provider_personalization_puk_required;
lookup(46) ->   corporate_personalization_pin_required;
lookup(47) ->   corporate_personalization_puk_required;
lookup(48) ->   ph_sim_puk_required;
lookup(100) ->  unknown_error;
lookup(103) ->  illegal_ms;
lookup(106) ->  illegal_me;
lookup(107) ->  gprs_services_not_allowed;
lookup(111) ->  plmn_not_allowed;
lookup(112) ->  location_area_not_allowed;
lookup(113) ->  roaming_not_allowed_in_this_location_area;
lookup(126) ->  operation_temporary_not_allowed;
lookup(132) ->  service_operation_not_supported;
lookup(133) ->  requested_service_option_not_subscribed;
lookup(134) ->  service_option_temporary_out_of_order;
lookup(148) ->  unspecified_gprs_error;
lookup(149) ->  pdp_authentication_failure;
lookup(150) ->  invalid_mobile_class;
lookup(256) ->  operation_temporarily_not_allowed;
lookup(257) ->  call_barred;
lookup(258) ->  phone_is_busy;
lookup(259) ->  user_abort;
lookup(260) ->  invalid_dial_string;
lookup(261) ->  ss_not_executed;
lookup(262) ->  sim_blocked;
lookup(263) ->  invalid_block;
lookup(772) ->  sim_powered_down;
lookup(_) -> unlisted_code.
