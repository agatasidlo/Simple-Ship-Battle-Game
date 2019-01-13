-module(computer).
-compile(export_all).

start()->
	spawn(?MODULE, init, []).

start_link() -> 
	spawn_link(?MODULE, init, []).

init() -> 
	computerTurn().

computerTurn() ->
	receive
	{MPid, pickPlaces, LayoutComputer} ->	
		LayoutWithShips = generatingComputersShips(LayoutComputer),
		MPid!LayoutWithShips;
			
	{ok} -> io:fwrite("Computer turn\n");

	{MPid, LayoutPlayer, CounterPlayer} -> 
			{C,R}=pickPlace(),
			case maps:get({C,R}, LayoutPlayer, none) of
					ship -> NewLayoutPlayer = maps:put({C,R}, sunken, LayoutPlayer),			%shooting to ship -> sunken
								NewCounterPlayer = CounterPlayer+1;
					sunken -> NewLayoutPlayer = maps:put({C,R}, sunken, LayoutPlayer),		%shooting to sunken ship -> no change and pick place again
								NewCounterPlayer = CounterPlayer;
					missed -> NewLayoutPlayer = maps:put({C,R}, missed, LayoutPlayer),		%shooting to missed -> no change and pick place again
								NewCounterPlayer = CounterPlayer;
					_ -> NewLayoutPlayer = maps:put({C,R}, missed, LayoutPlayer),				%shooting to empty square -> missed sign
								NewCounterPlayer = CounterPlayer
			end,
			MPid!{NewLayoutPlayer, NewCounterPlayer};

		{endGame} -> io:fwrite("End of game\n"),
							 exit(normal)
			
	after 5000 ->
	io:format("Computer waits for player\n")
	end,
computerTurn().


randomPlaceOnBoard() ->		%return random postion, 0<C<7, 0<R<7 
		C=rand:uniform(8)-1,
		R=rand:uniform(8)-1,
		{C,R}.

generatingComputersShips(LayoutComputer) ->		%randomly generate 5 ships on computer's board
		generatingComputersShips(LayoutComputer,5).
generatingComputersShips(LayoutComputer,ShipsCounter) ->
		if
				ShipsCounter<1 ->
						LayoutComputer;
				true ->
						{C,R}=randomPlaceOnBoard(),
						case maps:get({C,R}, LayoutComputer, none) of
										ship -> generatingComputersShips(LayoutComputer, ShipsCounter);			%square is not empty
										none -> NewLayoutComputer = maps:put({C,R}, ship, LayoutComputer),	%put ship on empty square
														erlang:display("column, row:"),
														erlang:display(C),
														erlang:display(R),
														generatingComputersShips(NewLayoutComputer, ShipsCounter-1)
						end
		end.



pickPlace() ->		%return square picked by computer
		C=rand:uniform(8)-1,
		R=rand:uniform(8)-1,
		{C,R}.
