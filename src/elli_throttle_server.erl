-module(elli_throttle_server).
-behaviour(gen_server).

%% API
-export([start_link/1, request/1, stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(DEFAULT_HOURLY_LIMIT, 3600).
-define(DEFAULT_DAILY_LIMIT,  86400).
-define(SECS_PER_HOUR,        3600).
-define(SECS_PER_DAY,         86400).

-record(state,    {startup_time,
                   hourly_limit,
                   daily_limit}).

-record(counters, {hour,
                   hourly_requests,
                   day,
                   daily_requests}).


%%%===================================================================
%%% API
%%%===================================================================


start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).


request(Id) ->
    gen_server:cast(?MODULE, {request, Id}).


stats() ->
    gen_server:call(?MODULE, stats).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Config]) ->
    {ok, #state{startup_time = current_timestamp(),
                hourly_limit = hourly_limit(Config),
                daily_limit = daily_limit(Config)}}.


handle_call(stats, _From, State) ->
    Requests = lists:filter(fun ({{elli_throttle, _}, _}) ->
                                    true;
                                (_) ->
                                    false
                            end, get()),
    {reply, {ok, Requests}, State};

handle_call(_, _From, State) ->
    {reply, ok, State}.


handle_cast({request, Id}, #state{startup_time = StartupTime} = State) ->
    Key = {elli_throttle, Id},
    put(Key, update_counters(get(Key), StartupTime)),
    {noreply, State};

handle_cast(_, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

hourly_limit(Config) ->
    proplists:get_value(hourly_limit, Config, ?DEFAULT_HOURLY_LIMIT).

daily_limit(Config) ->
    proplists:get_value(daily_limit, Config, ?DEFAULT_DAILY_LIMIT).


update_counters(undefined, StartupTime) ->
    #counters{hour = current_hour(StartupTime),
              hourly_requests = 1,
              day = current_day(StartupTime),
              daily_requests = 1};

update_counters(#counters{hour = TrackedHour,
                          hourly_requests = HourlyRequests,
                          day = TrackedDay,
                          daily_requests = DailyRequests}, StartupTime) ->

    {NewHour, NewHourlyRequests} =
        updated_hourly(current_hour(StartupTime), TrackedHour, HourlyRequests),
    {NewDay, NewDailyRequests} =
        updated_daily(current_day(StartupTime), TrackedDay, DailyRequests),
    #counters{hour = NewHour,
              hourly_requests = NewHourlyRequests,
              day = NewDay,
              daily_requests = NewDailyRequests}.


updated_hourly(CurrentHour, TrackedHour, Count)
  when CurrentHour =:= TrackedHour ->
    {TrackedHour, Count + 1};
updated_hourly(CurrentHour, _TrackedHour, _Count) ->
    {CurrentHour, 1}.

updated_daily(CurrentDay, TrackedDay, Count)
  when CurrentDay =:= TrackedDay ->
    {TrackedDay, Count + 1};
updated_daily(CurrentDay, _TrackedDay, _Count) ->
    {CurrentDay, 1}.


current_hour(StartupTime) ->
    (current_timestamp() - StartupTime) div ?SECS_PER_HOUR.

current_day(StartupTime) ->
    (current_timestamp() - StartupTime) div ?SECS_PER_DAY.

current_timestamp() ->
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
    MegaSecs * 1000000 + Secs.
