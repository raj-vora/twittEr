-module(simulator).
-compile(export_all).

-define(ASCII_CHARACTER_LOWER_LIMIT, 97).
-define(ASCII_CHARACTER_UPPER_LIMIT, 122).
-define(RANDOM_CHARACTER_SEQUENCE_LENGTH, 8).

run_simulation(No_of_Users, No_of_tweets) ->
    ets:new(times, [set, named_table, public]),
    Start = erlang:timestamp(),
    {Users, Registration_Time} = register_users(No_of_Users, []),
    ets:insert(times, {register, timer:now_diff(Registration_Time, Start)}),
    timer:sleep(10000),
    Subscription_Time = zipf_subscribers(No_of_Users, Users, No_of_Users, 1),
    ets:insert(times, {subscribe, timer:now_diff(Subscription_Time, Registration_Time)}),
    timer:sleep(10000),
    Hashtags = generate_random_character_sequence(rand:uniform(16), 5, []),
    Tweet_time = send_tweets(Users, No_of_tweets, Hashtags),
    ets:insert(times, {tweet, timer:now_diff(Tweet_time, Subscription_Time)}),
    timer:sleep(10000),
    io:format("~n~nChecking mentions~n~n"),
    Mentions_time = check_mentions(Users),
    ets:insert(times, {mentions, timer:now_diff(Mentions_time, Tweet_time)}),
    timer:sleep(10000),
    Hashtag_time = check_hashtags(Users, Hashtags),
    ets:insert(times, {hashtag, timer:now_diff(Hashtag_time, Mentions_time)}),
    Timeline_time = get_timeline(Users),
    ets:insert(times, {timeline, timer:now_diff(Timeline_time, Hashtag_time)}).

get_times() ->
    Registration_Time = ets:lookup(times, register),
    if Registration_Time > 0 ->
    io:format("register: ~p~n", [Registration_Time]);
    true -> ok
    end,
    Subscription_Time = ets:lookup(times, subscribe),
    if Subscription_Time > 0 ->
        io:format("subscribe: ~p~n", [Subscription_Time]);
    true -> ok
    end,
    Tweet_time = ets:lookup(times, tweet),
    if Tweet_time > 0 ->
        io:format("tweet: ~p~n", [Tweet_time]);
    true -> ok
    end,
    Mentions_time = ets:lookup(times, mentions),
    if Mentions_time > 0 ->
        io:format("mentions: ~p~n", [Mentions_time]);
    true -> ok
    end,
    Hashtag_time = ets:lookup(times, hashtag),
    if Hashtag_time > 0 ->
        io:format("hashtag: ~p~n", [Hashtag_time]);
    true -> ok
    end,
    Timeline_time = ets:lookup(times, timeline),
    if Timeline_time > 0 ->
        io:format("timeline: ~p~n", [Timeline_time]);
    true -> ok
    end.

register_users(0, Usernames) ->
    {Usernames, erlang:timestamp()};
register_users(No_of_Users, Usernames) ->
    [RandomString | _Rest] = generate_random_character_sequence(?RANDOM_CHARACTER_SEQUENCE_LENGTH, 1, []),
    client:register(list_to_atom(RandomString)),
    New_Usernames = [list_to_atom(RandomString) | Usernames],
    register_users(No_of_Users-1, New_Usernames).

generate_random_character_sequence(SequenceLength) when
    SequenceLength > 0
->
    [RandomString] = generate_random_character_sequence(SequenceLength, 1, []),
    RandomString.
generate_random_character_sequence(_, 0, Accumulator) ->
    Accumulator;
generate_random_character_sequence(SequenceLength, NoOfTimes, Accumulator) when
    SequenceLength > 0, NoOfTimes > 0
->
    RandomString = [
        generate_random_integer_between(?ASCII_CHARACTER_LOWER_LIMIT, ?ASCII_CHARACTER_UPPER_LIMIT)
        || _ <- lists:seq(1, SequenceLength)
    ],
    generate_random_character_sequence(
        SequenceLength,
        NoOfTimes - 1,
        Accumulator ++ [RandomString]
    ).

generate_random_integer_between(LowerLimit, UpperLimit) ->
    N = UpperLimit - LowerLimit + 1,
    LowerLimit + rand:uniform(N) - 1.

zipf_subscribers(0, _Usernames, _Total, _Rank) ->
    erlang:timestamp();
zipf_subscribers(No_of_Users, Usernames, Total, Rank) ->
    [User | Rest] = Usernames,
    No_of_subscribers = (0.1 / Rank) * Total,
    subscribe(User, Rest, math:floor(No_of_subscribers)),
    zipf_subscribers(No_of_Users-1, Rest, Total, Rank+1).

subscribe(_User, _Subscribers, 0.0) ->
    ok;
subscribe(User, Subscribers, No_of_subscribers) ->
    [Subscriber | Rest] = Subscribers,
    client:subscribe(Subscriber, User),
    subscribe(User, Rest, No_of_subscribers-1).

send_tweets([], _Number, _Hashtags) ->
    erlang:timestamp();
send_tweets(_Users, 0, _Hashtags) ->
  erlang:timestamp();
send_tweets(Users, No_of_tweets, Hashtags) ->
    [User | Rest] = Users,
    Mention = lists:nth(rand:uniform(length(Users)), Users),
    tweet(User, No_of_tweets, Mention, Hashtags),
    send_tweets(Rest, No_of_tweets-1, Hashtags).

tweet(_User, 0, _Mention, _Hashtags) ->
    ok;
tweet(User, Number, Mention, Hashtags) ->
    [Tweet | _Random] = generate_random_character_sequence(rand:uniform(10), 1, []),
    if Number rem 2 =:= 0 ->
        New_tweet = lists:append(Tweet, lists:append(" @", atom_to_list(Mention))),
        if 
            Number rem 4 =:= 0 ->
                if 
                    Number rem 8 =:= 0 ->
                        client:retweet(User, Mention, lists:append(New_tweet, lists:append(" #", lists:nth(rand:uniform(length(Hashtags)), Hashtags))));
                    true -> 
                        client:tweet(User, lists:append(New_tweet, lists:append(" #", lists:nth(rand:uniform(length(Hashtags)), Hashtags))))
                end;
            true -> client:tweet(User, New_tweet)
        end;
        true -> client:tweet(User, Tweet)
    end,
    tweet(User, Number-1, Mention, Hashtags).

check_mentions([]) ->
    erlang:timestamp();
check_mentions(Users) ->
    [First | Rest] = Users,
    client:mentions(First),
    check_mentions(Rest).

check_hashtags(_Users, []) ->
    erlang:timestamp();
check_hashtags(Users, Hashtags) ->
    [First | Rest] = Hashtags,
    User = lists:nth(rand:uniform(length(Users)), Users),
    client:hashtag(User, list_to_atom(First)),
    check_hashtags(Users, Rest).

get_timeline([]) ->
  erlang:timestamp();
get_timeline(Users) ->
  [First | Rest] = Users,
  client:timeline(First),
  get_timeline(Rest).

% cd("C:/Users/rajvo/Downloads/Twitter-Engine").