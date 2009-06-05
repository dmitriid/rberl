-module(rberl).

-export([start/0, start/1]).

start() ->
	start("./../examples/lang.properties").

start(FileName) ->
	process_file(FileName).

process_file(Name) ->
	{ok, Device} = file:open(Name, [read_ahead]),
	for_each_line(Device, "", "", "", false, false).

for_each_line(Device, Key, Value, Accum, HasSep, PrecedingBackslash) ->
	case io:get_line(Device, "") of
		eof  ->
			file:close(Device), Accum;
		Line ->
			{CurrentKey, CurrentValue, NewAccum, NewHasSep, NewPrecedingBackslash} =
				parse_line(list_to_binary(strip(Line)), Key, Value, Accum, HasSep, PrecedingBackslash),
			for_each_line(Device, CurrentKey, CurrentValue, NewAccum, NewHasSep, NewPrecedingBackslash)
	end.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    Parse each line                                                        %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% http://java.sun.com/j2se/1.5.0/docs/api/java/util/Properties.html
%% A comment line has an ASCII '#' or '!' as its first non-white space character;
%% comment lines are also ignored and do not encode key-element information
parse_line(<<Comment, _/binary>>,
           Key, Value, Accum, HasSep, false = _PrecedingBackslash) when Comment   =:= $#
                                                                   orelse Comment =:= $! ->
	{Key, Value, Accum, HasSep, false};
parse_line(Line, Key, Value, Accum, HasSep, PrecedingBackslash) ->
	parse_line1(Line, Key, Value, Accum, HasSep, PrecedingBackslash).




%% end of line
parse_line1(<<>>, Key, Value, Accum, HasSep, PrecedingBackslash) ->
	{Key, Value, Accum, HasSep, PrecedingBackslash};

%% possibly we have space as separator
parse_line1(<<Sep, Rest/binary>>, "" = _Key,
           Value, Accum, false = _HasSep, false = _PrecedingBackslash) when   (Sep =:= $\t
                                                                       orelse Sep =:= 32
                                                                       orelse Sep =:= $\f)
                                                                       andalso Value =/= "" ->
	parse_line1(Rest, "", Value, Accum, true, false);

%% we've hit a key
parse_line1(<<Sep, Rest/binary>>, "" = _Key,
           Value, Accum, _HasSep, false = _PrecedingBackslash) when   Sep =:= $:
                                                               orelse Sep =:= $= ->
	parse_line1(Rest, Value, "", Accum, false, false);

%% we've hit a character other than : or =
%% and we have a preceding whitespace
%% this means we've hit space as separator
parse_line1(<<Char, _Rest/binary>> = Bin,
           _Key, Value, Accum, true, false = _PrecedingBackslash) when    Char  =/= $\t
                                                                  andalso Char  =/= 32
                                                                  andalso Char  =/= $\f ->
	parse_line1(Bin, Value, "", Accum, false, false);

%% we've hit backslash
parse_line1(<<$\\, Rest/binary>>, Key, Value, Accum, HasSep, false = _PrecedingBackslash) ->
	parse_line1(Rest, Key, Value, Accum, HasSep, true);


%% end of line, yet the value continues on the next line
parse_line1(<<$\n, Rest/binary>>, Key, Value, Accum, HasSep, true = _PrecedingBackslash) ->
	NewAccum = append_value(Accum, Key,Value),
	parse_line1(Rest, Key, "", NewAccum, HasSep, false);
parse_line1(<<$\r, Rest/binary>>, Key, Value, Accum, HasSep, true = _PrecedingBackslash) ->
	NewAccum = append_value(Accum, Key,Value),
	parse_line1(Rest, Key, "", NewAccum, HasSep, false);

%% end of line, yet the value doesn't continue on the next line
parse_line1(<<$\n, _Rest/binary>>, Key, Value, Accum, HasSep, false = _PrecedingBackslash) ->
	NewAccum = append_value(Accum, Key,Value),
	{"", "", NewAccum, HasSep, false};
parse_line1(<<$\r, _Rest/binary>>, Key, Value, Accum, HasSep, false = _PrecedingBackslash) ->
	NewAccum = append_value(Accum, Key,Value),
	{"", "", NewAccum, HasSep, false};


%% we've hit a character preceded by backslash
%% simply append the character, drop the backslash
%% don't forget the newlines, of course
parse_line1(<<$n, Rest/binary>>, Key, Value, Accum, HasSep, true) ->
	parse_line1(Rest, Key, Value ++ [$\n], Accum, HasSep, false);
%% special case — end of file
parse_line1(<<Char>>, Key, Value, Accum, HasSep, true) ->
	NewAccum = append_value(Accum, Key, Value ++ [Char]),
	{Key, Value, NewAccum, HasSep, false};
parse_line1(<<Char, Rest/binary>>, Key, Value, Accum, HasSep, true) ->
	parse_line1(Rest, Key, Value ++ [Char], Accum, HasSep, false);

%% all other characters just go in
%% special case — end of file
parse_line1(<<Char>>, Key, Value, Accum, HasSep, PrecedingBackSlash) ->
	NewAccum = append_value(Accum, Key, Value ++ [Char]),
	{Key, Value, NewAccum, HasSep, PrecedingBackSlash};
parse_line1(<<Char, Rest/binary>>, Key, Value, Accum, HasSep, PrecedingBackSlash) ->
	parse_line1(Rest, Key, Value ++ [Char], Accum, HasSep, PrecedingBackSlash).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    Append and process values and keys                                     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% skip empty keys
append_value(Acc, "", _) ->
	Acc;
%% if a key exists, append value
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

process_string([]) ->
	[];
process_string(Value) ->
	NewValue = strip(Value),
	S = list_to_binary(NewValue),
	convert(S, []).

convert(<<>>, Acc) ->
	binary_to_list(unicode:characters_to_binary(lists:reverse(Acc), unicode));
convert(<<$\t, Rest/binary>>, Acc) ->
	convert(Rest, "\t" ++ Acc);
convert(<<$\n, Rest/binary>>, Acc) ->
	convert(Rest, "\n" ++ Acc);
convert(<<$\\, $u, Unicode:4/binary, Rest/binary>>, Acc) ->
	{ok, Character, _} = io_lib:fread("~#", "16#" ++ binary_to_list(Unicode)),
	convert(Rest, Character ++ Acc);
convert(<<Char:1/binary, Rest/binary>>, Acc) ->
	Character = binary_to_list(Char),
	convert(Rest, Character ++ Acc).


%% "deep-strip" a string
%% convert "  \t\t    value \t    \t" into "value"
strip(Value) ->
	lists:reverse(strip1(lists:reverse(strip1(Value)))).

strip1([]) ->
	[];
strip1([32 | _T] = Value) ->
	strip1(string:strip(Value));
strip1([$\t | _T] = Value) ->
	strip1(string:strip(Value, both, $\t));
strip1([$\f | _T] = Value) ->
	strip1(string:strip(Value, both, $\f));
strip1(Value) ->
	Value.