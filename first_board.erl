-module(first_board).
-behaviour(wx_object).
-include_lib("wx/include/wx.hrl").
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 handle_event/2, handle_sync_event/3,
	 terminate/2, code_change/3]).


% przy zmienianiu rozmiaru okna druga plansza moze wyjsc za krawedz
%jak zrobić zeby napis był nad druga plansza? - moze po prostu nie dawac zadnych napisow staticText,
%					tylko wyswietlic te messageDialog? przynajmniej wiem, jak to zrobic XD

%erlang:display("Message") - wyswietlanie w konsoli - pomocne przy szukaniu bledu


start_link() ->
    wx_object:start_link(?MODULE, [], []).

init([]) ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Gra w statki"),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
		Text = wxStaticText:new(Frame, ?wxID_ANY, "Pick 5 places for your ships																	Shoot your oponent's ships!", [{pos, {50, 50}}]), %tymczasowe rozwiazanie
		wxSizer:add(Sizer, Text),
		%Text2 = wxStaticText:new(Frame, ?wxID_ANY, "Shoot your oponent's ships!"), 
		%wxSizer:add(Sizer, Text2),
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
    State = #{frame => Frame,
	      panel => Panel,
	      layoutPlayer => maps:from_list([]),			% player's board
				layoutComputer => maps:from_list([]),		% computer's board
	      image_map => load_images(),
	      white_brush => wxBrush:new(White),
	      black_brush => wxBrush:new(Black),
				selected_brush => wxBrush:new({238,232,170}),
				state => choosingShipsPlaces,						% state = [choosingShipsPlaces | battle | endOfGame]
				counter => 0,														% counter of sunken player's ships
				counterComputer => 0										% counter of sunken computer's ships
				},

    wxFrame:show(Frame),
		wxPanel:connect(Panel, left_down),
    wxFrame:refresh(Frame),

    {Panel, State}.

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
							 %counter := Counter,
							 counterComputer := CounterComputer}) ->
		{C,R} = where(X,Y,Panel),
																																										% C-9 - bo pole (0,0) na layoutComputer to (9,0) w calosci
		{NewLayoutComputer, NewCounterComputer} = playerShoots({C-9,R}, #{panel => Panel, layoutPlayer => LayoutPlayer,
																																											layoutComputer => LayoutComputer, counterComputer => CounterComputer}),
		M = wxMessageDialog:new(wx:null(), "Hello"),
		wxMessageDialog:showModal(M),
		{noreply, State#{layoutPlayer => LayoutPlayer, layoutComputer => NewLayoutComputer, counterComputer => NewCounterComputer}}.



handle_sync_event(#wx{event=#wxPaint{}}, _, State) ->
    paint_board(State).

playerShoots({C,R}, #{
										layoutComputer := LayoutComputer,
										counterComputer := CounterComputer}) ->
		if
			C<(-1) ->
				M = wxMessageDialog:new(wx:null(), "Don't shoot yourself!!!"),
				wxMessageDialog:showModal(M),
				{LayoutComputer, CounterComputer};
			C==-1 -> {LayoutComputer, CounterComputer};
			true ->
		case maps:get({C,R}, LayoutComputer, none) of
				ship -> NewLayoutComputer = maps:put({C,R}, sunken, LayoutComputer),
								NewCounterComputer = CounterComputer+1;
				sunken -> NewLayoutComputer = maps:put({C,R}, sunken, LayoutComputer),
								NewCounterComputer = CounterComputer;
				_ -> NewLayoutComputer = maps:put({C,R}, missed, LayoutComputer),
							NewCounterComputer = CounterComputer
		end,
		{NewLayoutComputer, NewCounterComputer}
		end.
		

generatingComputersShips(#{layoutComputer := LayoutComputer}) -> true.
computerShoots(#{layoutPlayer := LayoutPlayer, counterComputer := CounterComputer}) -> true.

where(X,Y,Panel) -> 
		{W,H} = wxPanel:getSize(Panel), 
		SquareSize = square_size(W,H),
		{X div SquareSize, Y div SquareSize}.

terminate(_Reason, #{black_brush := BlackBrush,
		     white_brush := WhiteBrush,
		     image_map := ImageMap}) ->
    wxBrush:destroy(BlackBrush),
    wxBrush:destroy(WhiteBrush),
    [wxImage:destroy(I) || I <- maps:values(ImageMap)],
    wx:destroy().

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

square_size(W,H) ->
    ((min(W,H) div 8) div 2) * 2.

%% to_internal([File,Rank]) -> 
%%     {File - $A, $8 - Rank}.

%% from_internal({Column, Row}) -> 
%%     [Column + $A, $8 - Row].

rectangle(Column,Row,SquareSize) -> 
    {Column * SquareSize, Row * SquareSize, SquareSize, SquareSize}.

%init_board() ->
    %Columns = lists:seq(0,7),
    %Row6 = [{{C,6}, missed} || C <- Columns],	
    %Row1 = [{{C,1}, ship} || C <- Columns],	
%    maps:from_list([]).

square_colour(Col, Row) ->
    case ((Col + Row) rem 2) of
	0 -> white;
	1 -> black
    end.
	     
load_images() ->
    ImageFileNames = #{
      missed => "redX.png",
			ship => "ship.png",
			sunken => "sunken.png"},
    maps:map(fun(_K,V) -> wxImage:new(
			    filename:join("./images", V), 
			    [{type, ?wxBITMAP_TYPE_PNG}]) end,
	     ImageFileNames).

paint_board(#{panel := Panel,
	      layoutPlayer := LayoutPlayer,
	      layoutComputer := LayoutComputer,
	      image_map := ImageMap,
	      white_brush := WhiteBrush,
	      black_brush := BlackBrush}) ->
    {W,H} = wxPanel:getSize(Panel),
    SquareSize = square_size(W,H),

    PaintSquare =
	fun(DC,C,R,P,Layout) ->
		Brush = case square_colour(C,R) of
			    black -> BlackBrush;
			    white -> WhiteBrush
		end,
		Rectangle = rectangle(C+P,R,SquareSize),
		wxDC:setBrush(DC,Brush),
		wxDC:drawRectangle(DC, Rectangle),
		case maps:get({C,R}, Layout, none) of
    		none -> ok;
    		Piece ->
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
    [PaintSquare(DC,C,R,P,LayoutPlayer) || R <- Seq0to7, C <- Seq0to7, P <- PosList], %P - position
    PosList2=lists:duplicate(8,9),
    [PaintSquare(DC,C,R,P,LayoutComputer) || R <- Seq0to7, C <- Seq0to7, P <- PosList2],
wxPaintDC:destroy(DC).
