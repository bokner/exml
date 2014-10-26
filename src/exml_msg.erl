%% @author bokner
%% @doc Handler of SAX events.

-module(exml_msg).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([load/0]).
-export([
  new_parser/0,
  new_parser/1,
  reset_parser/1,
  free_parser/1,
  parse/2]).

%% NIFs
-export([
  new_parser_nif/0,
  %%reset_parser_nif/1,
  free_parser_nif/1,
  parse_nif/3]).

-export_type([msg_parser/0]).

-opaque msg_parser() :: pid().

-on_load(load/0).

-define(APPNAME, exml).
-define(LIBNAME, exml_msg).

-define(DEFAULT_HANDLER, exml_default_handler).

-define(NO_STREAM, <<>>).

-define(EXML_PATH_KEY, exml_path).

-record(state, {event_parser :: exml_event:c_parser(),
  level = 0 :: integer(),
  stream_tag = <<>> :: binary(),
  path = <<>> :: binary,
  handler :: module(),
  handler_state :: any()
}).

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
  new_parser([]).

-spec new_parser([{any(), any()}]) -> {ok, msg_parser()}.
new_parser(Opts) ->
  try
    {ok, EventParser} = new_parser_nif(),
    {ok, _Pid} = gen_server:start_link(?MODULE, [EventParser, Opts], [])
  catch
    E:R ->
      {error, {E, R}}
  end.

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
%% NIF interface
%% ====================================================================
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

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

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
init([EventParser, Opts]) ->
  HandlerModule = proplists:get_value(handler, Opts, ?DEFAULT_HANDLER),
  HandlerData = proplists:get_value(handler_data, Opts),
  {NewHandler, HandlerState} = init_handler(HandlerModule, HandlerData),
  {ok, #state{event_parser = EventParser,
  stream_tag = proplists:get_value(stream_tag, Opts, ?NO_STREAM),
  path = <<>>,
  handler_state = HandlerState}}.


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
  {ok, Reset}  = reset_parser_nif(State#state.event_parser),
  {reply, ok, State#state{event_parser = Reset}};

%% TODO : is this call necessary (maybe stop the process on the parser being freed) ?
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
handle_info({xml_element_start, Name, XmlNS, Attrs},
    #state{level = Level, handler = Handler, handler_state = HandlerState, stream_tag = StreamTag} = State)
  when Level == 0 andalso Name == StreamTag -> %% Stream starts
  {noreply, State#state{level = Level + 1,
  handler_state = Handler:stream_start(Name, XmlNS, Attrs, HandlerState)}};

handle_info({xml_element_start, Name, XmlNS, Attrs},
    #state{level = Level, handler = Handler, handler_state = HandlerState} = State)
  when (Level == 0 andalso stream_tag == ?NO_STREAM) orelse Level == 1 -> %% Document starts
  {noreply, State#state{level = Level + 1,
  path = Name,
  handler_state = Handler:document_start(Name, XmlNS, Attrs, HandlerState)}};

handle_info({xml_element_start, Name, XmlNS, Attrs},
    #state{level = Level, handler = Handler, handler_state = HandlerState, path = Path} = State) -> %% Subelement starts
  NewPath = path_down(Path, Name),
  {noreply, State#state{level = Level + 1,
  path = NewPath,
  handler_state = Handler:element_start(NewPath, XmlNS, Attrs, HandlerState)}};

handle_info({xml_element_end, Name},
    #state{level = Level, handler = Handler,
      handler_state = HandlerState, stream_tag = StreamTag} = State)
  when Level == 1 andalso Name == StreamTag -> %% Stream ends
  reset_parser_nif(State#state.event_parser),
  {noreply, State#state{level = Level - 1, handler_state = Handler:stream_end(StreamTag, HandlerState)}};

handle_info({xml_element_end, Name},
    #state{level = Level, handler = Handler,
      handler_state = HandlerState,
      stream_tag = StreamTag} = State)
  when Level == 1 orelse (Level == 2 andalso StreamTag /= ?NO_STREAM) -> %% Top element ends
  reset_parser_nif(State#state.event_parser),
  {noreply, State#state{level = Level - 1, path = <<>>, handler_state = Handler:document_end(Name, HandlerState)}};

handle_info({xml_element_end, Name},
    #state{level = Level, handler = Handler,
      path = Path,
      handler_state = HandlerState} = State) when Level > 1 -> %% Subelement ends
  NewPath = path_up(Path, Name),
  {noreply, State#state{level = Level - 1,
  path = NewPath,
  handler_state = Handler:element_end(Path, HandlerState)}};

handle_info({xml_cdata, CData},
    #state{handler = Handler,
      handler_state = HandlerState} = State) ->
  {noreply, State#state{handler_state = Handler:cdata(CData, HandlerState)}};

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
%% Context path
%% TODO: Consider another representation
%%
init_handler(HandlerModule, HandlerData) ->
  try HandlerModule:init(HandlerData) of
    {ok, HState} ->
      {HandlerModule, HState};
    {ok, HState, HModule} ->
      {HModule, HState}
  catch
    _Other ->
      throw({error, {broken_handler, _Other}})
  end.

path_down(Path, Name) ->
  <<Path/binary, "/", Name/binary>>.

path_up(Path, Name) ->
  binary:part(Path, 0, byte_size(Path) - byte_size(Name) - 1).
