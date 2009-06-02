-module(rberl_server).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
-export([start_link/0, start_link/1, start_link/2,
         start/0, start/1, start/2]).

%% Default callback functions
-export([custom_dir/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).



-define(SERVER, ?MODULE).
-define(TABLE_NAME, rberl_server_db).

-record(state, {
	cbmod = ?MODULE,   % callback module
	cache = [],        % list_of( #cache{} )
	directory,       % Dir where all the data are stored
	table_name = ?TABLE_NAME % If several gettext_servers start then
			   }).                       % tables must have different names

%%%
%%% Hold info about the languages stored.
%%%
-record(cache, {
	language  = ?DEFAULT_LANG,
	charset   = ?DEFAULT_CHARSET
			   }).

%%%
%%% Hold key-value pairs like this:
%%%   {value, key = {basename ++ "_" ++ language ++ "_" ++ country ++ "_" ++ variant, key}, value=value}
%%%
-record(value, {
	key = {"", ?DEFAULT_LANG, "", "", ""},
	value = ""
			   }).


%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
	start_link(?MODULE).

start_link(CallBackMod) ->
	start_link(CallBackMod, ?SERVER).

start_link(CallBackMod, Name) ->
	gen_server:start_link({local, Name}, ?MODULE, [CallBackMod, Name],[]).

%%--------------------------------------------------------------------

start() ->
	start(?MODULE).

start(CallBackMod) ->
	start(CallBackMod, ?SERVER).

start(CallBackMod, Name) ->
	gen_server:start({local, Name}, ?MODULE, [CallBackMod, Name], []).



%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([CallBackMod0, Name]) ->
	CallBackMod = get_callback_mod(CallBackMod0),
	RberlDir = get_rberl_dir(CallBackMod),
	TableNameStr = atom_to_list(Name) ++ "_db",
	TableName = list_to_atom(TableNameStr),
	Cache = create_db(TableName, Rberl),
	{ok, #state{cache       = Cache,
				cbmod       = CallBackMod,
				rberl_dir = Rberl,
				table_name  = TableName}}.

%%%
%%% The RBERL_CBMOD environment variable takes precedence!
%%%
get_callback_mod(CallBackMod0) ->
	case os:getenv("RBERL_CBMOD") of
		false -> CallBackMod0;
		CbMod -> list_to_atom(CbMod)
	end.

%%%
%%% The RBERL_DIR environment variable takes precedence!
%%% Next we will try to get hold of the value from the callback.
%%%
get_rberl_dir(CallBackMod) ->
	case os:getenv("RBERL_DIR") of
		false ->
			case catch CallBackMod:rberl_dir() of
				Dir when is_list(Dir) -> Dir;
				_ -> code:priv_dir(gettext) % fallback
			end;
		Dir   -> Dir
    end.

%% Default callback function
custom_dir() ->
    code:priv_dir(gettext).


%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({load}, _From, State) ->
    TableName = State#state.table_name,
    Dir = State#state.directory,

	init_table(TableName),

	Bundles = filelib:fold_files(Dir, ".*\\.properties",	false,
	                             fun(F, Acc) -> [F]++Acc end, []),

	lists:foreach(
		fun(El) ->
				load_bundle(El, TableName)
		end,
			Bundles
				 ),

    {reply, Reply, State};

handle_call({get, BaseName, Locale, Key}, _From, State) ->
    TableName = State#state.table_name,
	{Language, Country, Variant}  = normalize_locale(Locale),

    {reply, Reply, State};





%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

load_bundle(File, TableName) ->
	Locale = filename:basename(File, ".properties"),
	KeyValueList = rberl:parse_file(File),
	lists:foreach(
		fun({Key, Value}) ->
			ets:insert(TableName,#value{key={Locale, Key}, value=Value})
		end,
		KeyValueList
	),
	ok.

init_table(TableName) ->
	case ets:info(TableName) of
		undefined ->
			ets:new(TableName, [set, named_table]),
			ok;
		_ ->
			ok
	end.

normalize_locale({Language}) ->
	{Language, "", ""};
normalize_locale({Lang, Country}) ->
	{Language, Country, ""};
normalize_locale({_Language, _Country, _Variant} = Locale) ->
	Locale;
normalize_locale(_) ->
	{"", "", ""}.