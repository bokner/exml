%% @author bokner
%% @doc Handler of SAX events.

-module(exml_msg).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([load/0]).
-export([
		 new_parser/0,
         reset_parser/1,
         free_parser/1,
         parse/2]).

%% NIFs
-export([
		 new_parser_nif/0,
         reset_parser_nif/1,
         free_parser_nif/1,
         parse_nif/3]).

-export_type([msg_parser/0]).

-opaque msg_parser() :: pid().

-on_load(load/0).

-define(APPNAME, exml).
-define(LIBNAME, exml_msg).

%% ====================================================================
%% API functions
%% ====================================================================
-spec load() -> any().
load() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

-spec new_parser() -> {ok, msg_parser()}.
new_parser() ->
    try
        {ok, EventParser} = new_parser_nif(),
		{ok, _Pid} = gen_server:start_link(?MODULE, [EventParser], [])
    catch
        E:R ->
            {error, {E, R}}
    end.

%% NIFs
-spec new_parser_nif() -> {ok, exml_event:c_parser()}.
new_parser_nif() ->
    erlang:nif_error(not_loaded).

-spec reset_parser_nif(exml_event:c_parser()) -> ok.
reset_parser_nif(_Parser) ->
    erlang:nif_error(not_loaded).	

-spec free_parser_nif(exml_event:c_parser()) -> ok.
free_parser_nif(_Parser) ->
    erlang:nif_error(not_loaded).	

-spec parse_nif(exml_event:c_parser(), binary(), integer()) -> {ok, list()} | {error, string()}.
parse_nif(Parser, Data, Final) ->
    erlang:nif_error(not_loaded, [Parser, Data, Final]).

%% End of NIFs

-spec reset_parser(msg_parser()) -> ok.
reset_parser(Parser) ->
	gen_server:call(Parser, reset_parser).

-spec free_parser(msg_parser()) -> ok.
free_parser(Parser) ->
    gen_server:call(Parser, free_parser).

-spec parse(msg_parser(), binary()) -> ok.
parse(Parser, Data) ->
	gen_server:cast(Parser, {parse, Data}).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {event_parser :: exml_event:c_parser(), level :: integer()}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([EventParser]) ->
    {ok, #state{event_parser = EventParser, level = 0}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call(reset_parser, _From, State) ->
	reset_parser_nif(State#state.event_parser),
    {reply, ok, State#state{level = 0}};

handle_call(free_parser, _From, State) ->
	free_parser_nif(State#state.event_parser),
    {reply, ok, State};

handle_call(Request, _From, State) ->
    Reply = {unknown_call_request, Request},
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast({parse, Data}, State) ->
	ok = parse_nif(State#state.event_parser, Data, 0),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info({xml_element_start, Name, XmlNS, Attrs}, #state{level = Level} = State) ->
	io:format("<~s ~s>~n", [Name, attrs_to_iolist(Attrs, [])]),
    {noreply, State#state{level = Level + 1}};
handle_info({xml_element_end, Name}, #state{level = Level} = State) ->
	io:format("</~s>~n", [Name]),
	case Level of
		1 ->
			self() ! parse_end;
		_ ->
			void
	end,
    {noreply, State#state{level = Level - 1}};
handle_info({xml_cdata, CData}, State) ->
	io:format("~s", [CData]),
    {noreply, State};
handle_info(parse_end, State) ->
	io:format("Done."),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(Reason, State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(OldVsn, State, Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
-spec attrs_to_iolist([{binary(), binary()}], iolist()) -> iolist().
attrs_to_iolist([], Acc) ->
    Acc;
attrs_to_iolist([{Name, Value} | Rest], Acc) ->
    attrs_to_iolist(Rest, [" ", Name, "='", Value, "'" | Acc]).

