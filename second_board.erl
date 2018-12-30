-module(second_board).
-behaviour(wx_object).
-include_lib("wx/include/wx.hrl").
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 handle_event/2, handle_sync_event/3,
	 terminate/2, code_change/3]).

start_link() ->
    wx_object:start_link(?MODULE, [], []).

init([]) ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Gra w statki"),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    DownSizer = wxBoxSizer:new(?wxHORIZONTAL),
    Panel = wxPanel:new(Frame,[{size, {600,700}},
			       {style, ?wxFULL_REPAINT_ON_RESIZE}]),
    %ST2001 = wxStaticText:new(Panel, 2001,"Strzel w statki przeciwnika!", []),
    wxSizer:add(Sizer, Panel, [{proportion, 1}, 
			       {flag, ?wxEXPAND bor ?wxALL}, 
			       {border, 5}]), 
   % wxSizer:addSpacer(DownSizer, 5),
   % wxSizer:add(DownSizer, ST2001, []),
    wxFrame:setSizer(Frame, Sizer),
    wxSizer:setSizeHints(Sizer, Frame),
    wxPanel:connect(Panel, paint, [callback]),

    White = {0,100,200},
    Black = {0,50,200},
    State = #{frame => Frame,
	      panel => Panel,
	      layout => init_board(),
	      image_map => load_images(),
	      white_brush => wxBrush:new(White),
	      black_brush => wxBrush:new(Black)},

    wxFrame:show(Frame),
    %% wxFrame:refresh(Frame),

    {Panel, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_event(#wx{}, State) ->
    {noreply, State}.

handle_sync_event(#wx{event=#wxPaint{}}, _, State) ->
    paint_board(State).

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

init_board() ->
    Columns = lists:seq(0,7),
    BlackPieces = [{black,rook}, {black,knight}, {black,bishop}, {black,queen}, 
		   {black,king}, {black,bishop}, {black,knight}, {black,rook}],
    WhitePieces = [{white,rook}, {white,knight}, {white,bishop}, {white,queen}, 
		   {white,king}, {white,bishop}, {white,knight}, {white,rook}],
    Row6 = [{{C,6}, {white,pawn}} || C <- Columns],	
    Row1 = [{{C,1}, {black,pawn}} || C <- Columns],	
    Row7 = [{{C,7}, lists:nth(C+1, WhitePieces)} || C <- Columns],
    Row0 = [{{C,0}, lists:nth(C+1, BlackPieces)} || C <- Columns],

    maps:from_list(Row0 ++ Row1 ++ Row6 ++ Row7).

square_colour(Col, Row) ->
    case ((Col + Row) rem 2) of
	0 -> white;
	1 -> black
    end.
	     
load_images() ->
    ImageFileNames = #{
      {black, rook} 	=> "redX.png",
      {black, knight} 	=> "redX.png",
      {black, bishop} 	=> "redX.png",
      {black, queen} 	=> "redX.png",
      {black, king} 	=> "redX.png",
      {black, pawn} 	=> "redX.png",
      {white, rook} 	=> "redX.png",
      {white, knight}	=> "redX.png",
      {white, bishop} 	=> "redX.png",
      {white, queen}	=> "redX.png",
      {white, king}	=> "redX.png",
      {white, pawn}	=> "redX.png"},
    maps:map(fun(_K,V) -> wxImage:new(
			    filename:join("../images", V), 
			    [{type, ?wxBITMAP_TYPE_PNG}]) end,
	     ImageFileNames).

paint_board(#{panel := Panel,
	      layout := Layout,
	      image_map := ImageMap,
	      white_brush := WhiteBrush,
	      black_brush := BlackBrush}) ->
    {W,H} = wxPanel:getSize(Panel),
    SquareSize = square_size(W,H),

    PaintSquare = 
	fun(DC,C,R) ->
		Brush = case square_colour(C,R) of
			    black -> BlackBrush;
			    white -> WhiteBrush
		end,
		Rectangle = rectangle(C,R,SquareSize),		
		wxDC:setBrush(DC,Brush),
		wxDC:drawRectangle(DC, Rectangle)	    
	end,
    
    DC = wxPaintDC:new(Panel),
    wxDC:setPen(DC, ?wxTRANSPARENT_PEN),
    Seq0to7 = lists:seq(0,7),
    [PaintSquare(DC,C,R) || R <- Seq0to7, C <- Seq0to7], 
wxPaintDC:destroy(DC).