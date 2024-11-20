-module(server).
-compile(export_all).

start() ->
    ets:new(users, [set, named_table, public]),
    ets:new(tweets, [bag, named_table, public]),
    ets:new(retweets, [bag, named_table, public]),
    ets:new(mentions, [bag, named_table, public]),
    ets:new(hashtags, [bag, named_table, public]),
    ets:new(subscribers, [bag, named_table, public]),
    ets:new(subscribed_to, [bag, named_table, public]),
    register(server, spawn(?MODULE, server, [])),
    io:format("Server Started~n").

server() ->
    receive 
        {register, Username} ->
            Res = registration(Username),
            Username ! {Res},
            server();
        {login, Username} ->
            login(Username),
            server();
        {logout, Username} ->
            logout(Username),
            server();
        {tweet, Username, Tweet} ->
            Status = get_status(Username),
            if 
                Status =:= online ->
                    tweet(Username, Tweet);
                true ->
                    do_nothing
            end,
            server();
        {subscribe, Username, Subscribe_To} ->
            Status = get_status(Username),
            if 
                Status =:= online ->
                    subscribe(Username, Subscribe_To);
                true ->
                    do_nothing
            end,
            server();
        {retweet, Username, Tweeter, Tweet} ->
            Status = get_status(Username),
            if 
                Status =:= online ->
                    retweet(Username, Tweeter, Tweet);
                true ->
                    do_nothing
            end,
            server();
        {timeline, Username} ->
            Status = get_status(Username),
            if 
                Status =:= online ->
                    get_timeline(Username);
                true ->
                    do_nothing
                end,
            server();
        {mentions, Username} ->
            Status = get_status(Username),
            if 
                Status =:= online ->
                    mentions(Username);
                true ->
                    do_nothing
                end,
            server();
        {hashtag, HashTag, Username} ->
            Status = get_status(Username),
            if 
                Status =:= online ->
                    hashtag(Username, HashTag);
                true ->
                    do_nothing
            end,
            server()
        end.

registration(Username) ->
    Lookup = ets:lookup(users, Username),
    if length(Lookup) > 0 ->
        user_exists;
    true ->
        ets:insert(users, {Username, online}),
        registered
    end.

login(Username) ->
    Lookup = ets:lookup(users, Username),
    if 
        length(Lookup) > 0 ->
            ets:insert(users, {Username, online}),
            Status = online;
        true ->
            Status = invalid_user
        end,
    Username ! {Status}.

logout(Username) ->
    Lookup = ets:lookup(users, Username),
    if 
        length(Lookup) > 0 ->
            ets:insert(users, {Username, offline}),
            Status = offline;
        true ->
            Status = invalid_user
        end,
    Username ! {Status}.

get_status(Username) ->
    Lookup = ets:lookup(users, Username),
    if
        length(Lookup) > 0 ->
            [{_, Status} | _Rest] = Lookup,
            if Status =:= offline ->
                Username ! {Status};
                true -> online
            end
        end.

get_timeline(Username) ->
    Lookup = ets:lookup(users, Username),
    if 
        length(Lookup) > 0 ->
            Subscribed = ets:lookup(subscribed_to, Username),         
            if 
                length(Subscribed) > 0 ->
                    Timeline = get_tl(Subscribed, []);
                true ->
                    Timeline = []
            end
        end,
    Username ! {timeline, Timeline}.

get_tl([], TL) ->
    TL;
get_tl(Subscribed, TL) ->
    [First | Rest] = Subscribed,
    {_User, Subscribed_to} = First,
    Tweets = ets:lookup(tweets, Subscribed_to),
    if 
        length(Tweets) > 0 ->
            New_TL = lists:append(TL, Tweets);
        true ->
            New_TL = TL
    end,
    Retweets = ets:lookup(retweets, Subscribed_to),
    if 
        length(Retweets) > 0 ->
            New_RTL = lists:append(New_TL, Retweets);
        true ->
            New_RTL = New_TL
    end,
    get_tl(Rest, New_RTL).

tweet(Username, Tweet) ->
    ets:insert(tweets, {Username, Tweet}),
    Mentions = get_mentions(Tweet),
    lists:foreach(
        fun(Mention) ->
            ets:insert(mentions, {Mention, Tweet, Username}),
            mentions(list_to_atom(Mention))
        end, Mentions),
    Hashtags = get_hashtags(Tweet),
    lists:foreach(
        fun(HashTag) ->
            ets:insert(hashtags, {HashTag, Tweet, Username})
        end, Hashtags),
    Lookup = ets:lookup(subscribers, Username),
    if 
        length(Lookup) > 0 ->
            list_subscribers(Lookup, Username, Tweet, no);
        true ->
            ok
    end.

get_mentions(Tweet) ->
    F = re:run(Tweet, "(?<=@)\\w+", [global, {capture,all,list}]),
    if F /= nomatch ->
        {match, Mentions} = F,
        Mention_List = construct_list(length(Mentions), Mentions, []),
        Mention_List;
        true -> Mention_List = [],
            Mention_List
    end.

get_hashtags(Tweet) ->
    F = re:run(Tweet, "(?<=#)\\w+", [global, {capture,all, list}]),
    if 
        F /= nomatch ->
            {match, HashTags} = F,
            HashTag_List = construct_list(length(HashTags), HashTags, []),
            HashTag_List;
        true -> HashTag_List = [],
            HashTag_List
    end.

construct_list(0, _Mentions, Array) ->
    Array;
construct_list(Len, Mentions, Array) ->
    Ele = lists:nth(Len, Mentions),
    Inner_Ele = lists:nth(1, Ele),
    construct_list(Len - 1, Mentions, lists:append(Array, [Inner_Ele])).

list_subscribers([], _Username, _Tweet, _Retweet) ->
    ok;
list_subscribers(List, Username, Tweet, Retweet) ->
    [First | Rest] = List,
    {_User, Subscriber} = First,
    if 
        Retweet =:= yes ->
            Subscriber ! {timeline, [{Username, Tweet, retweet}]};
        true ->
            Subscriber ! {timeline, [{Username, Tweet}]}
        end,
    list_subscribers(Rest, Username, Tweet, Retweet).

subscribe(Username, Subscribe_To) ->
    ets:insert(subscribers, {Subscribe_To, Username}),
    ets:insert(subscribed_to, {Username, Subscribe_To}),
    Username ! {subscribed, Subscribe_To}.

retweet(Username, Tweeter, Tweet) ->
    tweet(Tweeter, Tweet),
    ets:insert(retweets, {Username, Tweet, Tweeter}),
    Mentions = get_mentions(Tweet),
    lists:foreach(
        fun(Mention) ->
            ets:insert(mentions, {Mention, Tweet, Username, retweet})
        end, Mentions),
    Hashtags = get_hashtags(Tweet),
    lists:foreach(
        fun(HashTag) ->
            ets:insert(hashtags, {HashTag, Tweet, Username, retweet})
        end, Hashtags),
    Subscribers = ets:lookup(subscribers, Username),
    if 
        length(Subscribers) > 0 ->
            list_subscribers(Subscribers, Username, Tweet, yes);
        true ->
            ok
    end.      

mentions(Username) ->
    Lookup = ets:lookup(mentions, atom_to_list(Username)),
    if
        length(Lookup) > 0 ->
            Username ! {mentions, Lookup};
        true ->
            no_mentions
        end.

hashtag(Username, HashTag) ->
    Lookup = ets:lookup(hashtags, atom_to_list(HashTag)),
    if
        length(Lookup) > 0 ->
            Username ! {hashtag, Lookup};
        true ->
            no_mentions
        end.

table_lookup(Table, Value) ->
    Lookup = ets:lookup(Table, Value),
    io:format("~p values ~p~n", [Table, Lookup]).