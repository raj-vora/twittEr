-module(client).
-compile(export_all).

register(Username) ->
    get_client_id(Username),
    server ! {register, Username}.

get_client_id(Username) ->
    Client = whereis(Username),
    if 
        Client =:= undefined ->
            Client_ID = spawn(?MODULE, client, [Username]),
            register(Username, Client_ID);
        true ->
            whereis(Username)
        end.

login(Username) ->
    get_client_id(Username),
    server ! {login, Username}.

logout(Username) ->
    server ! {logout, Username}.

tweet(Username, Tweet) ->
    server ! {tweet, Username, Tweet}.

subscribe(Username, Subscribe_To) ->
    server ! {subscribe, Username, Subscribe_To}.

retweet(Username, Tweeter, Tweet) ->
    server ! {retweet, Username, Tweeter, Tweet}.

mentions(Username) ->
    server ! {mentions, Username}.

hashtag(Username, Hashtag) ->
    server ! {hashtag, Hashtag, Username}.

timeline(Username) ->
    server ! {timeline, Username}.

client(Username) ->
    receive
        {subscribed, User} ->
            io:format("~p subscribed to ~p~n", [Username, User]),
        client(Username);
        {timeline, Timeline} ->
            display_timeline(Username, Timeline),
            client(Username);
        {Status} ->
            if 
                Status =:= offline ->
                    io:format("~p is ~p, please login ~n", [Username, Status]);
                true ->
                    io:format("~p: ~p ~n", [Username, Status])
            end,
            client(Username);
        {mentions, Tweets} ->
            display_mentions(Tweets),
            client(Username);
        {hashtag, Tweets} ->
            display_hashtag(Tweets),
            client(Username)
        end.
display_timeline(_Username, []) ->
    ok;
display_timeline(User, Timeline) ->
    [First | Rest] = Timeline,
    if 
        tuple_size(First) =:= 2 ->
            {Username, Tweet} = First,
            io:format("TL for ~p: ~p tweeted: ~p~n",[User, Username, Tweet]);
        tuple_size(First) =:= 3 ->
            {Username, Tweet, _} = First,
            io:format("TL for ~p: ~p retweeted: ~p~n",[User, Username, Tweet])
        end,
        display_timeline(User, Rest).

display_mentions([]) ->
    mentions_over;
display_mentions(Tweets) ->
    [First | Rest] = Tweets,
    if
        tuple_size(First) =:= 3 ->
        {Mention, Tweet, Username} = First,
        if is_list(Username) ->
            User = list_to_atom(Username);
        true ->
            User = Username
        end,
        io:format("~p mentioned @~p in ~p~n",[User, Mention, Tweet]);
        tuple_size(First) =:= 4 ->
            {Mention, Tweet, Username, _} = First,
        io:format("~p mentioned @~p in retweet \"~p\"~n",[Username, Mention, Tweet])
        end,
    display_mentions(Rest).

display_hashtag([]) ->
    hashtag_over;
display_hashtag(Tweets) ->
    [First | Rest] = Tweets,
    if
        tuple_size(First) =:= 3 ->
        {Hashtag, Tweet, Username} = First,
        io:format("~p used #~p in ~p~n",[Username, Hashtag, Tweet]);
        tuple_size(First) =:= 4 ->
            {Hashtag, Tweet, Username, _} = First,
        io:format("~p  used #~p in retweet ~p~n",[Username, Hashtag, Tweet])
        end,
        display_hashtag(Rest).