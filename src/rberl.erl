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
%% * v2009-06-05 dmitriid
%%   - Rewrote parsing functions to deal with .properties files
%%     according to http://java.sun.com/j2se/1.5.0/docs/api/java/util/Properties.html#load(jav
%%     a.io.InputStream)
%%
%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%% @copyright 2009 Dmitrii Dimandt
%% @version 0.1
%% @reference see <a href="http://java.sun.com/j2se/1.5.0/docs/api/java/util/Properties.html">java.util.Properties</a>
%%	and <a href="http://java.sun.com/docs/books/tutorial/i18n/resbundle/index.html">tutorial on Java's resource bundles</a>
%%
%% @see rberl_server
%%
%% @doc Parse Java Resource Bundles in Erlang
%%
%% This module deals with parsing Java properties files,
%% see <a href="http://java.sun.com/j2se/1.5.0/docs/api/java/util/Properties.html">http://java.sun.com/j2se/1.5.0/docs/api/java/util/Properties.html</a>,
%% especially the load function
%%
%% This module makes part of rberl, a library that provides functionality
%% similar to Java's resource bundles (see tutorial for java resource bundles at
%% <a href="http://java.sun.com/docs/books/tutorial/i18n/resbundle/index.html">http://java.sun.com/docs/books/tutorial/i18n/resbundle/index.html</a>)
%%
%% This code is available as Open Source Software under the MIT license.
%%
%% Updates at http://github.com/dmitriid/rberl/
%%
%%
%% <h2>HERE BE LIONS.</h2> Documentation for properties files. Quoted almost word for word from the javadocs
%%
%% The spec for .properties files is weird, to say the least.
%%
%% <h3>Incoming data</h3>
%%
%% The stream is assumed to be using the ISO 8859-1 character encoding; that is each byte is one Latin1 character.
%% Characters not in Latin1, and certain special characters, can be represented in keys and elements using escape sequences
%% similar to those used for character and string literals (see 
%% <a href="http://java.sun.com/docs/books/jls/second_edition/html/lexical.doc.html#100850">3.3</a> and
%% <a href="http://java.sun.com/docs/books/jls/second_edition/html/lexical.doc.html#101089">3.10.6</a> of the Java Language Specification).
%% The differences from the character escape sequences used for characters and strings are:
%% <ul>
%%   <li>Octal escapes are not recognized.</li>
%%   <li>The character sequence \b does not represent a backspace character (see next for treatment of backslashes).</li>
%%   <li>The method does not treat a backslash character, \, before a non-valid escape character as an error;
%%       the backslash is silently dropped. For example, in a Java string the sequence "\z" would cause a compile time error.
%%       In contrast, this method silently drops the backslash. Therefore, this method treats the two character sequence "\b"
%%       as equivalent to the single character 'b'.</li>
%%   <li>Escapes are not necessary for single and double quotes; however, by the rule above, single and double quote characters
%%       preceded by a backslash still yield single and double quote characters, respectively.</li>
%% </ul>
%%
%% <strong>Unlike it's Java counterpart, the module doesn't yet throw any exceptions if a malformed Unicode escape appears in the
%%         input, other than possible exceptions by the unicode module. It doesn't attempt to recover from any such errors either</strong>
%%
%% <h3>Lines</h3>
%%
%% This module processes input in terms of lines. A natural line of input is terminated 
%% either by a set of line terminator characters (\n or \r or \r\n) or by the end of the file.
%% A natural line may be either a blank line, a comment line, or hold some part of a key-element pair.
%%
%% The logical line holding all the data for a key-element pair may be spread out across several
%% adjacent natural lines by escaping the line terminator sequence with a backslash character, \.
%%
%% Note that a comment line cannot be extended in this manner; every natural line that is a comment
%% must have its own comment indicator, as described below. If a logical line is continued
%% over several natural lines, the continuation lines receive further processing, also described below.
%% Lines are read from the input stream until end of file is reached.
%%
%% <h3>Blank lines and comments</h3>
%%
%% A natural line that contains only white space characters is considered blank and is ignored.
%% A comment line has an ASCII '#' or '!' as its first non-white space character; comment lines
%% are also ignored and do not encode key-element information. In addition to line terminators,
%% this module considers the characters space (' ', '\u0020'), tab ('\t', '\u0009'), and form feed ('\f', '\u000C') to be white space.
%%
%% <h3>Values spanning several lines</h3>
%%
%% If a logical line is spread across several natural lines, the backslash escaping the line terminator sequence,
%% the line terminator sequence, and any white space at the start the following line have no affect on the key or element values.
%% The remainder of the discussion of key and element parsing will assume all the characters constituting the key and element
%% appear on a single natural line after line continuation characters have been removed.
%% Note that it is not sufficient to only examine the character preceding a line terminator sequence to see
%% if the line terminator is escaped; there must be an odd number of contiguous backslashes for the line terminator
%% to be escaped. Since the input is processed from left to right, a non-zero even number of 2n contiguous backslashes
%% before a line terminator (or elsewhere) encodes n backslashes after escape processing.
%%
%% <h3>Key-value pairs</h3>
%%
%% The key contains all of the characters in the line starting with the first non-white space character
%% and up to, but not including, the first unescaped '=', ':', or white space character other than
%% a line terminator. All of these key termination characters may be included in the key by
%% escaping them with a preceding backslash character; for example,
%% <pre><code>
%%   \:\=
%% </code></pre>
%%
%% would be the two-character key ":=". Line terminator characters can be included using \r and \n
%% escape sequences. Any white space after the key is skipped; if the first non-white space character
%% after the key is '=' or ':', then it is ignored and any white space characters after it are also skipped.
%% All remaining characters on the line become part of the associated element string;
%% if there are no remaining characters, the element is the empty string "".
%% Once the raw character sequences constituting the key and element are identified, escape processing
%% is performed as described above.
%%
%% As an example, each of the following three lines specifies the key "Truth" and the associated element value "Beauty":
%% <pre><code>
%%   Truth = Beauty
%%          Truth:Beauty
%%   Truth                  :Beauty
%% </code></pre>
%%
%% As another example, the following three lines specify a single property:
%% <pre><code>
%%   fruits                           apple, banana, pear, \
%%                                    cantaloupe, watermelon, \
%%                                    kiwi, mango
%% </code></pre>
%% The key is "fruits" and the associated element is:
%% <pre><code>
%%   "apple, banana, pear, cantaloupe, watermelon, kiwi, mango"
%% </code></pre>
%% Note that a space appears before each \ so that a space will appear after each comma in the final result;
%% the \, line terminator, and leading white space on the continuation line are merely discarded and are not
%% replaced by one or more other characters.
%%
%%As a third example, the line:
%% <pre><code>
%%   cheeses
%% </code></pre>
%%
%% specifies that the key is "cheeses" and the associated element is the empty string "".

%%
%% @type key_value_pair() = {Key::string(), Value::string()}.
%% @type key_value_list() = [key_value_pair()].
%%
%%


-module(rberl).

-export([process_file/1, start/0]).

start() ->
	rberl_server:start(),
	rberl_server:load("./../examples/", "lang"),
	rberl_server:get("multiline", "en").

%%
%% @doc Process the specified file. Only one file at a time
%% @spec process_file(FileName::string()) -> key_value_list()
%%

process_file(FileName) ->
	case file:open(FileName, [read_ahead]) of
		{ok, Device} ->
			for_each_line(Device, "", "", "", false, false);
		_ ->
			[]
	end.

%%
%% @private
%% @doc Process each line in the file. 
%%
%%  Device is the pid of the current open file.
%%
%%  Key is the current key (in case value spans several lines).
%%
%%  Value holds the current found characters.
%%
%%  Accum holds a proplist containing all key-vaue pairs found so far.
%%
%%  HasSep indicates whether we've found any spaces before we found a key-value separator.
%%
%%  PrecedingBackslash indicates whether we've hit backslash and need any extra processing
%%
%% @spec for_each_line(Device::pid(), Key::string(), Value::string(), Accum::key_value_list(), HasSep::bool(), PrecedingBackslash::bool()) -> key_value_list()
%%
for_each_line(Device, Key, Value, Accum, HasSep, PrecedingBackslash) ->
	case io:get_line(Device, "") of
		eof  ->
			file:close(Device), Accum;
		Line ->
			{CurrentKey, CurrentValue, NewAccum, NewHasSep, NewPrecedingBackslash} =
				parse_line(list_to_binary(lstrip(Line)), Key, Value, Accum, HasSep, PrecedingBackslash),
			for_each_line(Device, CurrentKey, CurrentValue, NewAccum, NewHasSep, NewPrecedingBackslash)
	end.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    Parse each line                                                        %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% @private
%% @doc Parses each line according to http://java.sun.com/j2se/1.5.0/docs/api/java/util/Properties.html
%%
%% Line - is the line that is currently beeing processed, one character at the time, recursively
%% 
%% For the meaning of the rest of parameters see for_each_line
%%
%% @see for_each_line/6
%% @spec parse_line(Line::binary(), Key::string(), Value::string(), Accum::key_value_list(), HasSep::bool(), PrecedingBackslash::bool()) ->
%%	{CurrentKey::string(), CurrentValue::string(), NewAccum::key_value_list(), NewHasSep::bool(), NewPrecedingBackslash::bool()}

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

%% @doc Process and append values and keys
%%
%% Appends values to existing values, creates new key-value pairs if key doesn't exist
%%
%% Trims (strips in erlang-talk) keys, since keys cannot contain spaces
%%
%% Converts unicode-encoded values into lists
%%
%% @spec append_value(Acc::key_value_list(), K::string(), V::string()) -> key_value_list()

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

%% @doc Strips and converts strings
%% @spec process_string(Value::string()) -> string()
process_string([]) ->
	[];
process_string(Value) ->
	S = list_to_binary(strip(Value)),
	convert(S, []).

%% @doc Converts unicode-encoded strings into lists and does some other processing
%% @spec convert(String::binary(), list()) -> string()
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


%% @doc
%% "Deep-strips" a string. Removes any mix of spaces tabs and line-feeds
%%
%% E.g. converts <code>"  \t\t    value \t    \t"</code> into <code>"value"</code>
%%
%% @spec strip(Value::string()) -> string()
strip(Value) ->
	lists:reverse(strip1(lists:reverse(strip1(Value)))).

%% @doc
%% "Deep-strips" a string from the left. Removes any mix of spaces tabs and line-feeds
%%
%% E.g. converts <code>"  \t\t    value \t    \t"</code> into <code>"value \t    \t"</code>
%%
%% @spec lstrip(Value::string()) -> string()
lstrip(Value) ->
	strip1(Value).

%% @spec strip1(Value::string()) -> string()
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