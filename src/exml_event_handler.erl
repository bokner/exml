%% @author bokner
%% @doc Behaviour for SAX handler.


-module(exml_event_handler).

-callback init(any()) -> any().

-callback stream_start(Name :: binary(), XmlNS :: binary(), Attrs :: list(), HandlerState :: any()) -> any().

-callback element_start(Name :: binary(), XmlNS :: binary(), Attrs :: list(), HandlerState :: any()) -> any().

-callback stream_end(Name :: binary(), HandlerState :: any()) -> any().

-callback element_end(Name :: binary(), HandlerState :: any()) -> any().

-callback cdata(CData :: binary(), HandlerState :: any()) -> any().

-callback document_start(Name :: binary(), XmlNS :: binary(), Attrs :: list(), HandlerState :: any()) -> any().

-callback document_end(Name :: binary(), HandlerState :: any()) -> any().


%% ====================================================================
%% Internal functions
%% ====================================================================


