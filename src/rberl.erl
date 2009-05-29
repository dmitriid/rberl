-module(rberl).

-export([start/0, start/1]).

start() ->
	start("./../examples/lang.properties").

start(FileName) ->
	{ok, Binary} = file:read_file(FileName),
    Lines = string:tokens(erlang:binary_to_list(Binary), "\n"),
	process_lines(Lines).

process_lines(Lines) ->
	process_lines(Lines, [], "").

process_lines([], Acc, _) ->
	Acc;
process_lines([H|T], Acc, CurrentKey) ->
	%% strip comments
	{ok, Re} = re:compile("(?<!\\\\)#"),
	CommentMatch = re:run(H, Re),
	{Acc1, Key} = case CommentMatch of
		{match, [{Position, _}|_]} -> %% comment line
			String = string:sub_string(H, 1, Position),
			process_line(String, Acc, CurrentKey);
		nomatch ->
			process_line(H, Acc, CurrentKey)
	end,
	process_lines(T, Acc1, Key).

process_line(String, Acc, CurrentKey) ->
	%% find
	%%     a=b
	%% look out for
	%%     a=a\=b
	%% and
	%%     a=abcdef\
	%%       a\=b
	case string:strip(String) of
		"" ->
			{Acc, CurrentKey};
		_ ->
			{ok, Re} = re:compile("(?<!\\\\)="),
			Match = re:run(String, Re),
			case Match of
				nomatch ->  %% we are in the middle of a value
					Acc1 = append_value(Acc, CurrentKey, String),
					{Acc1, CurrentKey};
				{match, [{Position, _}|_T]} -> %% we've come across a new value
					Key = string:left(String, Position),
					Value = string:substr(String, Position+2),
					Acc1 = append_value(Acc, Key, Value),
					{Acc1, Key}
			end
	end.


append_value(Acc, K, V) ->
	Value = process_string(V),
	Key = process_string(K),
	Val = proplists:get_value(Key, Acc),
	case Val of
		undefined ->
			[{Key, Value}] ++ Acc;
		_ ->
			[{Key, Val ++ Value}] ++ proplists:delete(Key, Acc)
	end.

process_string(Value) ->
	S = list_to_binary(string:strip(string:strip(Value, both, $\t))),
	S1 = convert(S, []),
	L = lists:last(S1),
	%% remove
	case L of
		$\\ ->
			string:left(S1, string:len(S1)-1);
		_ ->
			S1
	end.

convert(<<>>, Acc) ->
	binary_to_list(unicode:characters_to_binary(lists:reverse(Acc), unicode));
convert(<<$\t, Rest/binary>>, Acc) ->
	convert(Rest, "\t" ++ Acc);
convert(<<$\n, Rest/binary>>, Acc) ->
	convert(Rest, "\n" ++ Acc);
convert(<<$\\, $#, Rest/binary>>, Acc) ->
	convert(Rest, "#" ++ Acc);
convert(<<$\\, $u, Unicode:4/binary, Rest/binary>>, Acc) ->
	{ok, Character, _} = io_lib:fread("~#", "16#" ++ binary_to_list(Unicode)),
	convert(Rest, Character ++ Acc);
convert(<<Char:1/binary, Rest/binary>>, Acc) ->
	Character = binary_to_list(Char),
	convert(Rest, Character ++ Acc).
