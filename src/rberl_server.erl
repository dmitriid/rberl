%% Copyright (c) 2009 Dmitrii Dimandt <dmitrii@dmitriid.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%%
%% Change Log:
%% * v2009-06-19 dmitriid
%%   - First version of the server
%%
%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%% @copyright 2009 Dmitrii Dimandt
%% @version 0.1
%% @reference see <a href="http://java.sun.com/j2se/1.5.0/docs/api/java/util/Properties.html">java.util.Properties</a>
%%	and <a href="http://java.sun.com/docs/books/tutorial/i18n/resbundle/index.html">tutorial on Java's resource bundles</a>
%%
%% @see rberl
%%
%% @doc Persistent server for parsed Java Resource Bundles
%%
%% This module provides a gen_server that persists parsed Java Resource Bundles
%%
%% For this version the server creates an ets table named rberl_server and stores all values there


-module(rberl_server).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0]).

%% API
-export([load/2, reload/0, get/2]).

% These are all wrappers for calls to the server
start() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%
%% @doc Load all BaseName*.properties files in a directory
%% <pre><code lang="erlang">
%%   &gt;rberl_server:load("./../examples/", "lang").
%%    some_value
%% </code></pre>
%% Directory name must end in a trailing slash.
%%
%% @spec load(Dir::string(), FileName::string()) -> ok
%%

load(Dir, BaseName) ->
    gen_server:call(?MODULE, {load, Dir, BaseName}).

%%
%% @doc Reload all previously loaded files/strings
%% @spec reload() -> ok
%%
reload() ->
    gen_server:call(?MODULE, {reload}).

%%
%% @doc Get Value for specified Key and specified Locale
%% <pre><code lang="erlang">
%%   &gt;rberl_server:get("a key", "ru_RU").
%%    some_value
%% </code></pre>
%%
%% The values are searched in top-down manner from the "outmost" locale. That is,
%% for <code lang="Erlang">get("a key", "ru_RU_UNIX")</code> the server will look
%% for keys in the following locales, in order:
%% <ul>
%%   <li>ru_RU_UNIX</li>
%%   <li>ru_RU</li>
%%   <li>ru</li>
%%   <li>(keys for no locale)</li>
%% </ul>
%%
%% Locales are stored according the *.properties files. That is, if you call
%% <pre><code lang="erlang">
%%   rberl_server:load("./../examples/", "lang").
%% </code></pre>
%% and the example directory contains the following files:
%% <pre><code>
%%   lang.properties
%%   lang_ru.properties
%%   lang_ru_RU.properties
%%   lang_ru_RU_UNIX.properties
%%   lang_tr.properties
%%   lang_tr_TR.properties
%% </code></pre>
%% then the following locales will be available:
%% <pre><code>
%%   (no locale)
%%   ru
%%   ru_RU
%%   ru_RU_UNIX
%%   tr
%%   tr_TR
%% </code></pre>
%%
%% When no value can be found, the key is returned
%%
%% @spec get(Key::string(), Locale::string()) -> string()
%%
get(Key, Locale) ->
    gen_server:call(?MODULE, {get, Key, Locale}).


% This is called when a connection is made to the server
init([]) ->
    Tab = ets:new(?MODULE, [set]),
    FileTab = ets:new(list_to_atom(atom_to_list(?MODULE) ++ "file"), []),
    {ok, {Tab, FileTab}}.

% handle_call is invoked in response to gen_server:call
handle_call({load, Dir, BaseName}, _From, {Tab, FileTab}) ->
    Files = filelib:wildcard(BaseName ++ "*", Dir),
    lists:foreach(
		fun(F) ->
				File = filename:basename(F, ".properties"),
				Locale = case File -- BaseName of
							 [$_|T] ->T;
							 L -> L
						 end,
				Strings = rberl:process_file(Dir ++ F),
				lists:foreach(
					fun({Key, Value}) ->
							ets:insert(Tab, {{Locale, Key}, Value})
					end,
						Strings
							 ),
				ets:insert(FileTab, {{Dir, File, Locale}, F})
		end, Files
				 ),
    {reply, ok, {Tab, FileTab}};

handle_call({reload}, _From, {Tab, FileTab}) ->
    ets:delete_all_objects(Tab),
    ets:foldl(
		fun({{Dir, _F, Locale}, File}, _AccIn) ->
				Strings = rberl:process_file(Dir ++ File),
				lists:foreach(
					fun({Key, Value}) ->
							ets:insert(Tab, {{Locale, Key}, Value})
					end,
						Strings
							 ),
				[]
		end,
			[], FileTab
			 ),
    {reply, ok, {Tab, FileTab}};

handle_call({get, Key, Locale}, _From, {Tab, FileTab}) ->
    Value = lookup(Tab, {Locale, Key}),
    {reply, Value, {Tab, FileTab}}.


handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% @doc Recursively lookup value in all locales in top-down manner.
%% @see get/2
%%

lookup(Tab, {"" = _Locale, K} = Key) ->
    case ets:lookup(Tab, Key) of
		[] ->
			Key;
		[{_, Value}] ->
			Value
    end;
lookup(Tab, {Locale, K} = Key) ->
    case ets:lookup(Tab, Key) of
		[] ->
			NewLocale = shorten_locale(Locale),
			lookup(Tab, {NewLocale, K});
		[{_, Value}] ->
			Value
    end.

%%
%% @doc Reduce locale from ru_RU_UNIX to ru_RU then to ru then to ""
%%
shorten_locale(Locale) ->
    case string:str(Locale, "_") of
		0 -> %% we have a simple two-letter symbol for locale, return empty locale
			"";
		_ -> %% we have a complex locale, sth like ru_RU_UNIX, strip UNIX
			[_|T] = lists:reverse(string:tokens(Locale, "_")),
			case T of
				[[_|_]|_] -> %% we have a complex locale, sth like ["RU", "ru"]
					string:join(lists:reverse(T), "_");
				_ -> %% we have a simple locale, sth like "ru"
					T
			end
			
    end.