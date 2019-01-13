-module(game).
-behaviour(wx_object).		%module with specifict set of functions
-include_lib("wx/include/wx.hrl").	%library to use wxErlang
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 handle_event/2, handle_sync_event/3,
	 terminate/2, code_change/3]).
-import(computer, []).

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
		CPid = createComputer(),
		MPid = self(),
		CPid!{MPid, pickPlaces, maps:from_list([])},
		receive
			LayoutWithShips ->
				%LayoutWithShips = generatingComputersShips(maps:from_list([])),
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
						counterPlayer => 0,							% counter of sunken player's ships
						cPid => CPid										%computer's pid	
						},

				wxFrame:show(Frame),
				M = wxMessageDialog:new(wx:null(), "Pick 5 places for your ships on left board"),
				wxMessageDialog:showModal(M),
				wxMessageDialog:destroy(M),
				wxPanel:connect(Panel, left_down),
				wxFrame:refresh(Frame),
				{Panel, State}
		end.

%events
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.


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
								5 ->
											M = wxMessageDialog:new(wx:null(), "Shoot your oponent's ships!"),
											wxMessageDialog:showModal(M),
											wxMessageDialog:destroy(M),
											
											wxPanel:refresh(Panel),
											{noreply, State#{layoutPlayer => NewLayoutPlayer, state => battle, counter => 0}};
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
							 cPid := ComPid,
							 counterComputer := CounterComputer,
							 counterPlayer := CounterPlayer}) ->
				{C,R} = where(X,Y,Panel),
				{NewLayoutComputer, NewCounterComputer} = playerShoots({C-9,R}, #{panel => Panel,layoutComputer => LayoutComputer, counterComputer => CounterComputer}),
			 	%check if move was correct
				if NewLayoutComputer == LayoutComputer -> {noreply, State#{layoutComputer => NewLayoutComputer, counterComputer => NewCounterComputer}};
					true ->
						if NewCounterComputer == 5 -> 										%player wins
								M = wxMessageDialog:new(wx:null(), "You won!"),
								wxMessageDialog:showModal(M),
								wxPanel:refresh(Panel),
								%ComPid!{endGame},
								{noreply, State#{layoutComputer => NewLayoutComputer, counterComputer => NewCounterComputer, state => endOfGame}};
								true ->
									{NewLayoutPlayer, NewCounterPlayer} = computerTurn(#{panel => Panel,							
																	layoutPlayer => LayoutPlayer, counterPlayer => CounterPlayer, cPid => ComPid}),
									{noreply, State#{layoutComputer => NewLayoutComputer, counterComputer => NewCounterComputer, layoutPlayer => NewLayoutPlayer, counterPlayer => NewCounterPlayer}}
					end
			end;

handle_event(#wx{event=#wxMouse{leftDown=true}}, State=					% end of game - nothing is happening
						 #{state := endOfGame}) -> {noreply, State#{}}.

handle_sync_event(#wx{event=#wxPaint{}}, _, State) ->			%painting board event depending on State
    paint_board(State).

createComputer()->
	CPid = computer:start_link(),
	CPid.

computerTurn(#{panel := Panel,	layoutPlayer := LayoutPlayer, counterPlayer := CounterPlayer, cPid := ComPid}) ->
				ComPid!{ok},
				wxPanel:refresh(Panel),
				MPid = self(),
				ComPid!{MPid, LayoutPlayer, CounterPlayer},
				receive 
					{NewLayoutPlayer, NewCounterPlayer} -> io:fwrite("Got new LayoutPlayer\n"),
					%check if move is correct
					if NewLayoutPlayer == LayoutPlayer -> io:fwrite("TU\n"), computerTurn(#{panel => Panel, layoutPlayer => LayoutPlayer, counterPlayer => CounterPlayer, cpid => ComPid});
						true ->
							if NewCounterPlayer == 5 ->										%computer wins
								M = wxMessageDialog:new(wx:null(), "You lost!"),
								wxMessageDialog:showModal(M),
								wxPanel:refresh(Panel),
								%ComPid!{endGame},
								{NewLayoutPlayer, NewCounterPlayer};
							true -> wxPanel:refresh(Panel), {NewLayoutPlayer, NewCounterPlayer}
							end
					end
				end.
								
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
								NewCounterComputer = CounterComputer;
				missed -> NewLayoutComputer = maps:put({C,R}, missed, LayoutComputer),	%shooting to missed -> no change and shoot again
								NewCounterComputer = CounterComputer;
				_ -> NewLayoutComputer = maps:put({C,R}, missed, LayoutComputer),				%shooting to empty square -> missed sign
							NewCounterComputer = CounterComputer
		end,
		{NewLayoutComputer, NewCounterComputer}
		end.


where(X,Y,Panel) -> 								%return position of square
		{W,H} = wxPanel:getSize(Panel), 
		SquareSize = square_size(W,H),
		{X div SquareSize, Y div SquareSize}.

terminate(_Reason, #{black_brush := BlackBrush,				%destroy functions while terminating program
		     white_brush := WhiteBrush,
		     image_map := ImageMap, cPid := ComPid}) ->
    wxBrush:destroy(BlackBrush),
    wxBrush:destroy(WhiteBrush),
    [wxImage:destroy(I) || I <- maps:values(ImageMap)],
		erlang:display(ComPid),
		ComPid!{endGame},
    wx:destroy().

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

square_size(W,H) ->		%square size depending on size of Panel
    ((min(W,H) div 8) div 2) * 2.

rectangle(Column,Row,SquareSize) -> 		%position and dimension of square
    {Column * SquareSize, Row * SquareSize, SquareSize, SquareSize}.

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
								if W==p ->						%drawing player's ships
										{X,Y,SW,SH} = Rectangle,
										Image = wxImage:scale(maps:get(ship, ImageMap),SW,SH),
										PieceBitmap = wxBitmap:new(Image),
										wxDC:drawBitmap(DC, PieceBitmap, {X,Y}),
										wxImage:destroy(Image),
										wxBitmap:destroy(PieceBitmap); %?
								true -> ok
								end;
						Piece ->									%drawing images: missed, sunken
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
