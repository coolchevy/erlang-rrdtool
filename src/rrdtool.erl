%%% Copyright 2009 Andrew Thompson <andrew@hijacked.us>. All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%
%%%   1. Redistributions of source code must retain the above copyright notice,
%%%      this list of conditions and the following disclaimer.
%%%   2. Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimer in the
%%%      documentation and/or other materials provided with the distribution.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE FREEBSD PROJECT ``AS IS'' AND ANY EXPRESS OR
%%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
%%% EVENT SHALL THE FREEBSD PROJECT OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
%%% INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

% @doc An erlang module to interface with rrdtool's remote control mode as an
% erlang port.
-module(rrdtool).

-behaviour(gen_server).

% public API
-export([
		start/0,
		start/1,
		start_link/0,
		start_link/1,
		stop/0,
        create/3,
		create/4,
		update/2,
		update/3,
        graph/5
]).

% gen_server callbacks
-export([init/1,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
		terminate/2,
		code_change/3
]).

-define(STORE_TYPES,
	['GAUGE', 'COUNTER', 'DERIVE', 'ABSOLUTE', 'COMPUTE']).

-define(COLORS, ["#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF", "#00FFFF", "#000000",
    "#800000", "#008000", "#000080", "#808000", "#800080", "#008080", "#808080",
    "#C00000", "#00C000", "#0000C0", "#C0C000", "#C000C0", "#00C0C0", "#C0C0C0",
    "#400000", "#004000", "#000040", "#404000", "#400040", "#004040", "#404040",
    "#200000", "#002000", "#000020", "#202000", "#200020", "#002020", "#202020",
    "#600000", "#006000", "#000060", "#606000", "#600060", "#006060", "#606060",
    "#A00000", "#00A000", "#0000A0", "#A0A000", "#A000A0", "#00A0A0", "#A0A0A0",
    "#E00000", "#00E000", "#0000E0", "#E0E000", "#E000E0", "#00E0E0", "#E0E0E0"]).

-define(SERVER, ?MODULE).

% public API

start() ->
    application:start(rrdtool).

start(RRDTool) when is_list(RRDTool) ->
	gen_server:start(?MODULE, [RRDTool], []).

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [os:find_executable("rrdtool")], []).

start_link(RRDTool) when is_list(RRDTool) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [RRDTool], []).

stop() ->
    application:stop(rrdtool).

create(Filename, Datastores, RRAs) ->
    create(Filename, Datastores, RRAs, []).

create(Filename, Datastores, RRAs, Options) ->
	gen_server:call(?SERVER, {create, Filename, format_datastores(Datastores), format_archives(RRAs), format_create_options(Options)}, infinity).

update(Filename, DatastoreValues) ->
	gen_server:call(?SERVER, {update, Filename, format_datastore_values(DatastoreValues), n}, infinity).

update(Filename, DatastoreValues, Time) ->
	gen_server:call(?SERVER, {update, Filename, format_datastore_values(DatastoreValues), Time}, infinity).

graph(Filename, Imagename, Datastores, RRAs, Options) ->
    gen_server:call(?SERVER, {graph, Filename, Imagename, Datastores, RRAs, Options}, infinity).


% gen_server callbacks

%% @hidden
init([RRDTool]) ->
	Port = open_port({spawn_executable, RRDTool}, [{line, 1024}, {args, ["-"]}]),
	{ok, Port}.

%% @hidden
handle_call({create, Filename, Datastores, RRAs, Options}, _From, Port) ->
	Command = ["create ", Options, Filename, " ", string:join(Datastores, " "), " ", string:join(RRAs, " "), "\n"],
	%io:format("Command: ~p~n", [lists:flatten(Command)]),
	port_command(Port, Command),
	receive
		{Port, {data, {eol, "OK"++_}}} ->
			{reply, ok, Port};
		{Port, {data, {eol, "ERROR:"++Message}}} ->
			{reply, {error, Message}, Port}
	end;
handle_call({update, Filename, {Datastores, Values}, Time}, _From, Port) ->
	Timestamp = case Time of
		n ->
			"N";
		{Megaseconds, Seconds, _Microseconds} ->
			integer_to_list(Megaseconds) ++ integer_to_list(Seconds);
		Other when is_list(Other) ->
			Other
	end,
	Command = ["update ", Filename, " -t ", string:join(Datastores, ":"), " ", Timestamp, ":", string:join(Values, ":"), "\n"],
	%io:format("Command: ~p~n", [lists:flatten(Command)]),
	port_command(Port, Command),
	receive
		{Port, {data, {eol, "OK"++_}}} ->
			{reply, ok, Port};
		{Port, {data, {eol, "ERROR:"++Message}}} ->
			{reply, {error, Message}, Port}
	end;
handle_call({graph, Filename, Imagename, Datastores, RRAs, Options}, _From, Port) ->
    {Megaseconds, Seconds, _Microseconds} = now(),
    Timestamp = integer_to_list(Megaseconds * 1000000 + Seconds),
    GraphOptions = format_options(Options),
    GraphData = format_graph_datastores(Filename, Datastores, RRAs),
    Command = ["graph ", Imagename, " ", GraphOptions, " ", GraphData, "--end ", Timestamp, "\n"],
    io:format("Command: ~p~n", [lists:flatten(Command)]),
    %{reply, ok, Port};
    port_command(Port, Command),
    receive
        {Port, {data, {eol, "OK"++_}}} ->
            {reply, ok, Port};
        {Port, {data, {eol, "ERROR:"++Message}}} ->
            {reply, {error, Message}, Port}
    end;
handle_call(stop, _From, State) ->
	{stop, normal, ok, State};
handle_call(Request, _From, State) ->
	{reply, {unknown_call, Request}, State}.

%% @hidden
handle_cast(_Msg, State) ->
	{noreply, State}.

%% @hidden
handle_info(Info, State) ->
	io:format("info: ~p~n", [Info]),
	{noreply, State}.

%% @hidden
terminate(_Reason, Port) ->
	port_command(Port, "quit\n"),
	ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% internal functions

format_datastores(Datastores) ->
	format_datastores(Datastores, []).

format_datastores([], Acc) ->
	lists:reverse(Acc);
format_datastores([H | T], Acc) ->
	case H of
		{Name, DST, Arguments} when is_list(Name), is_atom(DST), is_list(Arguments) ->
			case re:run(Name, "^[a-zA-Z0-9_]{1,19}$", [{capture, none}]) of
				nomatch ->
					throw({error, bad_datastore_name, Name});
				match ->
					case lists:member(DST, ?STORE_TYPES) of
						false ->
							throw({error, bad_datastore_type, DST});
						true ->
							format_datastores(T, [["DS:", Name, ":", atom_to_list(DST), ":", format_arguments(DST, Arguments)] | Acc])
					end
			end;
		_ ->
			throw({error, bad_datastore, H})
	end.

format_arguments(DST, Arguments) ->
	case DST of
		'COMPUTE' ->
			% TODO rpn expression validation
			Arguments;
		_ ->
			case Arguments of
				[Heartbeat, Min, Max] when is_integer(Heartbeat), is_integer(Min), is_integer(Max) ->
					io_lib:format("~B:~B:~B", [Heartbeat, Min, Max]);
				[Heartbeat, undefined, undefined] when is_integer(Heartbeat) ->
					io_lib:format("~B:U:U", [Heartbeat]);
				_ ->
					throw({error, bad_datastore_arguments, Arguments})
			end
	end.

format_archives(RRAs) ->
	format_archives(RRAs, []).

format_archives([], Acc) ->
	lists:reverse(Acc);
format_archives([H | T], Acc) ->
	case H of
		{CF, Xff, Steps, Rows} when CF =:= 'MAX'; CF =:= 'MIN'; CF =:= 'AVERAGE'; CF =:= 'LAST' ->
			format_archives(T, [io_lib:format("RRA:~s:~.2f:~B:~B", [CF, Xff, Steps, Rows]) | Acc]);
		_ ->
			throw({error, bad_archive, H})
	end.

format_datastore_values(DSV) ->
	format_datastore_values(DSV, [], []).

format_datastore_values([], TAcc, Acc) ->
	{lists:reverse(TAcc), lists:reverse(Acc)};
format_datastore_values([H | T], TAcc, Acc) ->
	case H of
		{Name, Value} ->
			case re:run(Name, "^[a-zA-Z0-9_]{1,19}$", [{capture, none}]) of
				nomatch ->
					throw({error, bad_datastore_name, Name});
				match ->
					format_datastore_values(T, [Name | TAcc], [value_to_list(Value) | Acc])
			end;
		_ ->
			throw({error, bad_datastore_value, H})
	end.

value_to_list(Value) when is_list(Value) ->
	Value;
value_to_list(Value) when is_integer(Value) ->
	integer_to_list(Value);
value_to_list(Value) when is_float(Value) ->
	float_to_list(Value);
value_to_list(Value) when is_binary(Value) ->
	binary_to_list(Value);
value_to_list(_) ->
    "U".

format_create_options(Options) ->
	StepOpt = case proplists:get_value(step, Options) of
		undefined ->
			[];
		Step when is_integer(Step) ->
			["-s ", integer_to_list(Step), " "]
	end,

	StartOpt = case proplists:get_value(start, Options) of
		undefined ->
			[];
		Start ->
			["-b ", value_to_list(Start), " "]
	end,

	lists:flatten([StepOpt, StartOpt]).

format_options(Options) ->
    lists:flatten(
        lists:reverse(
            lists:foldl(
            fun({Oname, Ovalue}, Acc) when is_atom(Oname) ->
                    [["--", atom_to_list(Oname), " ", Ovalue, " "]|Acc];
                (Oname, Acc) when is_atom(Oname) ->
                    [["--", atom_to_list(Oname), " "]|Acc]
            end, [], Options))).

format_graph_datastores(Filename, Datastores, RRAs) ->
    Values = [[["DEF:", format_graph_value_name(X, R),"=", Filename, ":", X, ":", atom_to_list(R), " "] || R <- RRAs] || X <- Datastores],
    CurValues = [["CDEF",":",format_graph_value_name(X, 'LAST'), "=", format_graph_value_name(X, hd(RRAs)), " "] || X <- Datastores],
    Comments = ["COMMENT:.   ", [[" COMMENT:", atom_to_list(X)] || X <- RRAs], "\\j "],
    Lines = lists:reverse(
        lists:foldl(fun(X, Acc) ->
                case Acc of
                    [] -> TYPE = " AREA:";
                    _ -> TYPE = " STACK:"
                end,
                [[TYPE,format_graph_value_name(X, 'LAST'),lists:nth(length(Acc) +1, ?COLORS), ":", X, [" GPRINT:", format_graph_value_name(X, 'LAST'), ":", "LAST:%6.2lf%s"], [[" GPRINT:", format_graph_value_name(X, R), ":", atom_to_list(R), ":", "%6.2lf%s"] || R <- RRAs],
                    "\\j "] | Acc]
        end, [], Datastores)),
    lists:flatten([Values,CurValues, Comments, Lines]).

format_graph_value_name(DS, RRA) ->
    lists:flatten([DS,"_", atom_to_list(RRA)]).
