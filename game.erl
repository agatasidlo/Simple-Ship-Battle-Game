-module(game).
-behaviour(wx_object).		%module with specifict set of functions
-include_lib("wx/include/wx.hrl").	%library to use wxErlang
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 handle_event/2, handle_sync_event/3,
	 terminate/2, code_change/3]).

%erlang:display("Message") - wyswietlanie w konsoli - pomocne przy szukaniu bledu


start_link() ->						%start game function
    wx_object:start_link(?MODULE, [], []).

init([]) ->								%init function
    wx:new(),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Ship battle"),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    Panel = wxPanel:new(Frame,[{size, {1250,600}},
			       {style, ?wxFULL_REPAINT_ON_RESIZE}]),
    wxSizer:add(Sizer, Panel, [{proportion, 1},
			       {flag, ?wxEXPAND bor ?wxALL}, 
			       {border, 5}]),
    wxFrame:setSizer(Frame, Sizer),
    wxSizer:setSizeHints(Sizer, Frame),
    wxPanel:connect(Panel, paint, [callback]),
    White = {0,100,200},
    Black = {0,50,200},
    LayoutWithShips = generatingComputersShips(maps:from_list([])),
    State = #{frame => Frame,
	      panel => Panel,
	      layoutPlayer => maps:from_list([]),		% player's board
		  	layoutComputer => LayoutWithShips,		% computer's board
	      image_map => load_images(),
	      white_brush => wxBrush:new(White),
	      black_brush => wxBrush:new(Black),
				selected_brush => wxBrush:new({238,232,170}),
				state => choosingShipsPlaces,						% state = [choosingShipsPlaces | battle | endOfGame]
				counter => 0,														% counter of sunken player's ships
				counterComputer => 0,								% counter of sunken computer's ships
				counterPlayer => 0								% counter of sunken player's ships			
				},

    wxFrame:show(Frame),
		M = wxMessageDialog:new(wx:null(), "Pick 5 places for your ships on left board"),
    wxMessageDialog:showModal(M),
		wxMessageDialog:destroy(M),
    wxPanel:connect(Panel, left_down),
    wxFrame:refresh(Frame),
    {Panel, State}.

%events
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

	%HELP, moze jakby te wiadomosci przekazywac sobie jakos wspołbieżnie między aktualnymi stanami?
%handle_info(info,State=
%						 #{panel := Panel,
%							 layoutPlayer := LayoutPlayer,
%							 layoutComputer := LayoutComputer,
%							 %state := battle,
%							 counter := Counter,
%							 counterComputer := CounterComputer}) ->
%			 M = wxMessageDialog:new(wx:null(), "Shoot your oponent's ships!"),
%   	 wxMessageDialog:showModal(M),
											
%    {noreply, State}.



handle_event(#wx{event=#wxMouse{leftDown=true, x=X, y=Y}}, State=				%mouse event when choosing places for ships
						 #{panel := Panel,
						 layoutPlayer := LayoutPlayer,
						 state := choosingShipsPlaces,
						 counter := Counter
						 }) ->
	
		{C,R} = where(X,Y,Panel),
			if C>=8 -> 		{noreply, State};
				true ->
				case maps:get({C,R}, LayoutPlayer, none) of
					none ->								% left mouses button clicked on empty place
						NewLayoutPlayer = maps:put({C,R}, ship, LayoutPlayer),
						wxPanel:refresh(Panel),
						case Counter+1 of		% Counter - counter of ships on board
								5 -> {noreply, State#{layoutPlayer => NewLayoutPlayer, state => battle, counter => 0}};
								_ -> {noreply, State#{layoutPlayer => NewLayoutPlayer, counter => Counter+1}}
						end;
					_ ->									% left mouses button clicked on place with ship
						NewLayoutPlayer = maps:remove({C,R}, LayoutPlayer),
						wxPanel:refresh(Panel),
						{noreply, State#{layoutPlayer => NewLayoutPlayer, counter => Counter-1}}
				end
		end;


handle_event(#wx{event=#wxMouse{leftDown=true, x=X, y=Y}}, State=					% mouse event - shooting
						 #{panel := Panel,
							 layoutPlayer := LayoutPlayer,
							 layoutComputer := LayoutComputer,
							 state := battle,
							 counter := Counter,
							 counterComputer := CounterComputer,
							 counterPlayer := CounterPlayer}) ->
		{C,R} = where(X,Y,Panel),
		%erlang:display(Counter),
		if Counter == 0 ->  M = wxMessageDialog:new(wx:null(), "Shoot your oponent's ships!"),			%niestety pojawia się dopiero jak klikne, trzeba chyba tym handle_info?
   									 	wxMessageDialog:showModal(M),
											wxMessageDialog:destroy(M),
											{noreply, State#{counter => 1}};
				true ->																					
				{NewLayoutComputer, NewCounterComputer} = playerShoots({C-9,R}, #{panel => Panel, layoutPlayer => LayoutPlayer,
																																												layoutComputer => LayoutComputer, counterComputer => CounterComputer}),
			if NewCounterComputer == 5 -> 										%player wins
					M = wxMessageDialog:new(wx:null(), "You win!"),
					wxMessageDialog:showModal(M),
					{noreply, State#{layoutComputer => NewLayoutComputer, counterComputer => NewCounterComputer}};
					true ->
					{NewLayoutPlayer, NewCounterPlayer} = computerShoots(#{panel => Panel, layoutPlayer => LayoutPlayer,
																																													layoutComputer => LayoutComputer, counterPlayer => CounterPlayer}),
					if NewCounterPlayer ==5 ->										%computer wins
							M = wxMessageDialog:new(wx:null(), "You lose!"),
							wxMessageDialog:showModal(M),
							{noreply, State#{layoutPlayer => NewLayoutPlayer, counterPlayer => NewCounterPlayer}};
						true ->																			%nobody wins
						{noreply, State#{layoutPlayer => NewLayoutPlayer, layoutComputer => NewLayoutComputer, counterComputer => NewCounterComputer, counterPlayer => NewCounterPlayer}}
					end
			end
		end.



handle_sync_event(#wx{event=#wxPaint{}}, _, State) ->			%painting board event depending on State
    paint_board(State).

playerShoots({C,R}, #{panel := Panel,															%player shooting
										layoutComputer := LayoutComputer,
										counterComputer := CounterComputer}) ->
		if
			C<(-1) ->																			%player shoots to he's own board
				M = wxMessageDialog:new(wx:null(), "Don't shoot yourself!!!"),
				wxMessageDialog:showModal(M),
				{LayoutComputer, CounterComputer};
			C==-1 -> {LayoutComputer, CounterComputer};		%player shoots not to board	
			true ->
		wxPanel:refresh(Panel),
		case maps:get({C,R}, LayoutComputer, none) of		%player shoots to computer's board
				ship -> NewLayoutComputer = maps:put({C,R}, sunken, LayoutComputer),	%shooting to ship -> sunken
								NewCounterComputer = CounterComputer+1;
				sunken -> NewLayoutComputer = maps:put({C,R}, sunken, LayoutComputer),	%shooting to sunken ship -> no change and shoot again
								NewCounterComputer = CounterComputer,
								playerShoots({C,R}, #{panel => Panel, layoutComputer => NewLayoutComputer, counterComputer => NewCounterComputer}),
								wxPanel:refresh(Panel);
				missed -> NewLayoutComputer = maps:put({C,R}, missed, LayoutComputer),	%shooting to missed -> no change and shoot again
								NewCounterComputer = CounterComputer,
								playerShoots({C,R}, #{panel => Panel, layoutComputer => NewLayoutComputer, counterComputer => NewCounterComputer}),
								wxPanel:refresh(Panel);
				_ -> NewLayoutComputer = maps:put({C,R}, missed, LayoutComputer),				%shooting to empty square -> missed sign
							NewCounterComputer = CounterComputer
		end,
		{NewLayoutComputer, NewCounterComputer}
		end.


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

computerShoots(#{panel := Panel,															%computer shooting
										layoutPlayer := LayoutPlayer,
										counterPlayer := CounterPlayer}) ->
		{C,R}=pickPlace(),
		wxPanel:refresh(Panel),
		case maps:get({C,R}, LayoutPlayer, none) of
				ship -> NewLayoutPlayer = maps:put({C,R}, sunken, LayoutPlayer),			%shooting to ship -> sunken
							NewCounterPlayer = CounterPlayer+1;
				sunken -> NewLayoutPlayer = maps:put({C,R}, sunken, LayoutPlayer),		%shooting to sunken ship -> no change and pick place again
							NewCounterPlayer = CounterPlayer, 
							computerShoots(#{panel => Panel, layoutPlayer => NewLayoutPlayer, counterPlayer => NewCounterPlayer}),
							wxPanel:refresh(Panel);
				missed -> NewLayoutPlayer = maps:put({C,R}, missed, LayoutPlayer),		%shooting to missed -> no change and pick place again
							NewCounterPlayer = CounterPlayer, 
							computerShoots(#{panel => Panel, layoutPlayer => NewLayoutPlayer, counterPlayer => NewCounterPlayer}),
							wxPanel:refresh(Panel);
				_ -> NewLayoutPlayer = maps:put({C,R}, missed, LayoutPlayer),				%shooting to empty square -> missed sign
							NewCounterPlayer = CounterPlayer
		end,
		{NewLayoutPlayer, NewCounterPlayer}.



where(X,Y,Panel) -> 								%return position of square
		{W,H} = wxPanel:getSize(Panel), 
		SquareSize = square_size(W,H),
		{X div SquareSize, Y div SquareSize}.

terminate(_Reason, #{black_brush := BlackBrush,				%destroy functions while terminating program
		     white_brush := WhiteBrush,
		     image_map := ImageMap}) ->
    wxBrush:destroy(BlackBrush),
    wxBrush:destroy(WhiteBrush),
    [wxImage:destroy(I) || I <- maps:values(ImageMap)],
    wx:destroy().

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

square_size(W,H) ->		%square size depending on size of Panel
    ((min(W,H) div 8) div 2) * 2.

%% to_internal([File,Rank]) -> 
%%     {File - $A, $8 - Rank}.

%% from_internal({Column, Row}) -> 
%%     [Column + $A, $8 - Row].

rectangle(Column,Row,SquareSize) -> 		%position and dimension of square
    {Column * SquareSize, Row * SquareSize, SquareSize, SquareSize}.

%init_board() ->
    %Columns = lists:seq(0,7),
    %Row6 = [{{C,6}, missed} || C <- Columns],	
    %Row1 = [{{C,1}, ship} || C <- Columns],	
%    maps:from_list([]).

square_colour(Col, Row) ->		%checkered board
    case ((Col + Row) rem 2) of
	0 -> white;
	1 -> black
    end.
	     
load_images() ->				%loading images from 'images' folder
    ImageFileNames = #{
      missed => "redX.png",
			ship => "ship.png",
			sunken => "sunken.png"},
    maps:map(fun(_K,V) -> wxImage:new(
			    filename:join("./images", V),
			    [{type, ?wxBITMAP_TYPE_PNG}]) end,
	     ImageFileNames).

paint_board(#{panel := Panel,							%paint board with appropriate parameters
	      layoutPlayer := LayoutPlayer,
	      layoutComputer := LayoutComputer,
	      image_map := ImageMap,
	      white_brush := WhiteBrush,
	      black_brush := BlackBrush}) ->
    {W,H} = wxPanel:getSize(Panel),
    SquareSize = square_size(W,H),

    PaintSquare =												%paint one square, param: DC - Panel, C - cols, R - rows, P - position (player's/computer's board), Layout - LayoutPlayer/LayoutComputer, W - player/computer 
	fun(DC,C,R,P,Layout,W) ->
		Brush = case square_colour(C,R) of
			    black -> BlackBrush;
			    white -> WhiteBrush
		end,
		Rectangle = rectangle(C+P,R,SquareSize),
		wxDC:setBrush(DC,Brush),
		wxDC:drawRectangle(DC, Rectangle),
				case maps:get({C,R}, Layout, none) of
						none -> ok;
						ship ->
								if W==p ->						%drawing ships
										{X,Y,SW,SH} = Rectangle,
										Image = wxImage:scale(maps:get(ship, ImageMap),SW,SH),
										PieceBitmap = wxBitmap:new(Image),
										wxDC:drawBitmap(DC, PieceBitmap, {X,Y}),
										wxImage:destroy(Image);
								true -> ok
								end;
						Piece ->									%drawing any image?
								{X,Y,SW,SH} = Rectangle,
								Image = wxImage:scale(maps:get(Piece, ImageMap),SW,SH),
								PieceBitmap = wxBitmap:new(Image),
								wxDC:drawBitmap(DC, PieceBitmap, {X,Y}),
								wxImage:destroy(Image),
								wxBitmap:destroy(PieceBitmap)
				end
	end,
    
    DC = wxPaintDC:new(Panel),
    wxDC:setPen(DC, ?wxTRANSPARENT_PEN),
    Seq0to7 = lists:seq(0,7),
    PosList=lists:duplicate(8,0),
    [PaintSquare(DC,C,R,P,LayoutPlayer,p) || R <- Seq0to7, C <- Seq0to7, P <- PosList], %P - position, p - player, c - computer
    PosList2=lists:duplicate(8,9),
    [PaintSquare(DC,C,R,P,LayoutComputer,c) || R <- Seq0to7, C <- Seq0to7, P <- PosList2],
wxPaintDC:destroy(DC).