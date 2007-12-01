-module(gui).
-export([priv_dir/0]).
-export([start_glade/2, stop/1]).
-export([cmd/3, cmd/4]).

-export([new_tree_view_column/3]).
-export([new_list_store/2, list_store_append/2, list_store_set/4]).

%-----------------------------------------------------------------

priv_dir() ->
    case code:priv_dir(?MODULE) of
	{error, bad_name} ->
	    "./priv";
	D ->
	    D
    end.

start_glade(WindowAtom, GladeFile) ->
    gtknode:start(WindowAtom),
    void = cmd(WindowAtom, 'GN_glade_init', [filename:join(priv_dir(), GladeFile)]),
    ok.

stop(WindowAtom) ->
    gtknode:stop(WindowAtom).

cmd(Node, Widget, Cmd, Args) ->
    cmd(Node, Cmd, [Widget | Args]).

cmd(Node, Cmd, Args) ->
    gtknode:cmd(Node, [{Cmd, Args}]).

new_tree_view_column(Node, DataCol, Title) ->
    Col = cmd(Node, 'Gtk_tree_view_column_new', []),
    Renderer = cmd(Node, 'Gtk_cell_renderer_text_new', []),
    cmd(Node, 'Gtk_tree_view_column_pack_start', [Col, Renderer, false]),
    cmd(Node, 'Gtk_tree_view_column_set_title', [Col, Title]),
    cmd(Node, 'Gtk_tree_view_column_set_resizable', [Col, true]),
    cmd(Node, 'Gtk_tree_view_column_add_attribute', [Col, Renderer, "text", DataCol]),
    Col.

new_list_store(Node, Columns) ->
    cmd(Node, 'Gtk_list_store_newv', [length(Columns), Columns]).

list_store_append(Node, Store) ->
    cmd(Node, 'Gtk_list_store_append', [Store, iter]).

list_store_set(Node, Store, Col, Val) ->
    cmd(Node, 'GN_value_set', [val, Val]),
    cmd(Node, 'Gtk_list_store_set_value', [Store, iter, Col, val]).
