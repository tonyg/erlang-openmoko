-module(openmoko_audio).

-export([select_profile/1]).

select_profile(Profile) when is_atom(Profile) ->
    case os:cmd("alsactl -f /etc/" ++ atom_to_list(Profile) ++ ".state restore") of
	[] ->
	    ok;
	ErrorMessage ->
	    ErrorAtom = case string:re_match(ErrorMessage, "No such file or directory") of
			    {match,_,_} -> not_found;
			    _ -> unknown
			end,
	    {error, ErrorAtom, ErrorMessage}
    end.
