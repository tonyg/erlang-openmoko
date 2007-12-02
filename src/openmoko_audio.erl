-module(openmoko_audio).

-export([list_profiles/0, select_profile/1]).

list_profiles() ->
    {ok, Files} = file:list_dir("/etc"),
    ProfileFiles = lists:filter(fun (X) ->
					regexp:match(X, "\\.state$" % " - emacs balancing
						     ) =/= nomatch
				end, Files),
    lists:map(fun (X) ->
		      list_to_atom(string:substr(X, 1, length(X) - 6))
	      end, ProfileFiles).

select_profile(Profile) when is_atom(Profile) ->
    case os:cmd("alsactl -f /etc/" ++ atom_to_list(Profile) ++ ".state restore") of
	[] ->
	    ok;
	ErrorMessage ->
	    ErrorAtom = case regexp:match(ErrorMessage, "No such file or directory") of
			    {match, _, _} -> not_found;
			    _ -> unknown
			end,
	    {error, ErrorAtom, ErrorMessage}
    end.
