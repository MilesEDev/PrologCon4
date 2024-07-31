
:- dynamic rlstate/3.
:- dynamic yelAI/1.
:- dynamic action/2.
:- dynamic redAI/1.


yelAI([]).%stores action pairs

redAI([]).




replace(I, L, Term, G) :-%replaces a element in list as a postion
  nth0(I,L,_,R),
  nth0(I,G,Term,R).



setcols(Colums):- %Sets up the initial columbs
  length(Colums,7).

validateinput(X):-
  integer(X),X < 7. %Make sure input is less than 7


mylength(Col,Len):-
  (atom(Col)
  ->  Len is 1
  ;   length(Col,Len)). %measures length of list makes sure unit is list

blanktozero(L1,Output):-% stops unified values being unified unintendidly
  member(X,L1),
  (   not(atom(X)),not(is_list(X))
  ->  Output is 0

  ;   Output = X).



gamestart():-% starts player vs player game
  setcols(G),taketurn(G,yellow).

gamestartAI(Epcount,Eptotal,Epsilon,Staticepcount):-%initiate player vs AI uses epsilon greedy
  setcols(G),
  taketurnAI(G,yellow,[],5),
  nl,
  write("NEW game!"),
  Newstaticepcount is Staticepcount + 1,
    (   Epcount >= Eptotal/10
   -> Neweps is Epsilon + 1,
      Newepcount is 1
    ; Neweps is Epsilon,
      Newepcount is Epcount  +1),


  (   Newstaticepcount = Eptotal
  ->  !
  ;   gamestartAI(Newepcount,Eptotal,Neweps,Newstaticepcount)).


gamestartAIvsAI(Epcount,Eptotal,Epsilon,Staticepcount):-%does AI vs AI to populate states with values
  setcols(G),
  taketurnAIvsAI(G,yellow,[],[],5),
  nl,
  write("NEW game!"),
  Newstaticepcount is Staticepcount + 1,
    (   Epcount >= Eptotal/10
   -> Neweps is Epsilon + 1,
      Newepcount is 1
    ; Neweps is Epsilon,
      Newepcount is Epcount  +1),


  (   Newstaticepcount = Eptotal
  ->  !
  ;   gamestartAIvsAI(Newepcount,Eptotal,Neweps,Newstaticepcount)).



updatebanned(I,X,Updatedx,Banned):- %updates the banned values
  member(X,Banned),
  (   X < I
  ->  Updatedx = X
  ;   Updatedx is X -1).

actgreedy(Vals,Max,Banned):- %chooses the greatest value move to take

  max_list(Vals,M),
  nth0(I,Vals,M),
  (   member(I,Banned)
  ->  nth0(I,Vals,_,Newvals),
      delete(Banned,I,Newbanned),
      findall(Updatedx,updatebanned(I,X,Updatedx,Newbanned),Updatedbanned),
      actgreedy(Newvals,NM,Updatedbanned),
      Max = NM

  ;   Max = M).


isin(L,X):- %sees if element is in list
  member(Y,L),
  X ==Y.

deletelistlist(L1,L2,X):- %combines 2 lists and deletes duplicates
  exclude(isin(L2),L1,X).



sublist(S, L) :- %gets sublist of a list
	append(L1, L2, L),
	append(S, L3, L2).


readrow(Len,Colums,Item):- %this reads a horizontol row in board
  member(X,Colums),
  (   nth0(Len,X,Item)
->
      (  not(atom(Item))
      -> nl,
         Item is 1
      ;  nth0(Len,X,Item))
;     Item is 1).


readdiag(Colums,NL,X,Pos):- % this reads a diagnol row in board
  nth0(Pos,Colums,Poscol),
  mylength(Poscol,Len),
  member(Y,NL),
  Posdiff is Pos - Y,
  Hindex is Len - Posdiff -1,
 (    nth0(Y,Colums,Col)
 ->  nth0(Hindex,Col,X),
     (   not(atom(X))
     ->
         X is 1
     ;   nth0(Hindex,Col,X))
 ;   X is 1).


readdiagreverse(Colums,NL,X,Pos):- %this reads a reverse diagnol row in board
   nth0(Pos,Colums,Poscol),
   mylength(Poscol,Poslen),
   reverse(NL,Rnl),
   member(Y,Rnl),
   Index is 0+Y,
   Posdiff is Pos-Y,
   Hindex is Poslen + Posdiff -1,
   nth0(Index,Colums,Col),
   nth0(Hindex,Col,X),
   (   not(atom(X))
   ->  X is 1
   ;   nth0(Hindex,Col,X)).

posdiagwin(Pos,Colums,Counter):- %this sets up values to be read for pos diag win making sure that there is sufficiant height and space to sides of counter to win
  nth0(Pos,Colums,Col),
  mylength(Col,Len),
  Firstband is Pos - min(Pos,Len-1),
  Maxheight is 6-Len,
  Maxtoright is 7-Pos,
  Secondband is Pos  + min(Maxheight,Maxtoright  -1),
  numlist(Firstband,Secondband,NL),

  findall(X,readdiag(Colums,NL,X,Pos),Bag),
  sublist([Counter,Counter,Counter,Counter],Bag).

negdiag(Pos,Colums,Counter):- %this sets up values to be read for neg diag win making sure that there is sufficiant height and space to sides of counter to win

  nth0(Pos,Colums,Col),
  mylength(Col,Len),

  Firstband is Pos - min(Pos,7-Len),
  Maxtoright is 7-Pos,
  Maxheight is Len-1,
  Secondband is Pos + min(Maxheight,Maxtoright -1),
  numlist(Firstband,Secondband,NL),
  findall(X,readdiagreverse(Colums,NL,X,Pos),Bag),
  sublist([Counter,Counter,Counter,Counter],Bag).

vertwin(Pos,Colums,Counter):-%this checks for a vertical win on board
  nth0(Pos,Colums,Col),
  sublist([Counter,Counter,Counter,Counter],Col).

horiwin(Pos,Colums,Counter):- %this sets up values to be read for horizontol win making sure that there is sufficiant height and space to sides of counter to win

  nth0(Pos,Colums,Col),
  mylength(Col,Len),
  Newlen is Len -1,
  findall(Item,readrow(Newlen,Colums,Item),Bag),
  sublist([Counter,Counter,Counter,Counter],Bag).

win(Pos,Colums,Counter):- %checks all win condititions
  vertwin(Pos,Colums,Counter);
  horiwin(Pos,Colums,Counter);
  posdiagwin(Pos,Colums,Counter);
  negdiag(Pos,Colums,Counter).

aimove(Columb,Epsilon,Pos,Episode,Newepisode,Counter):-%this allows the AI to take a move and updates the AI episodes and actions
  [H|T] = Columb,
  Copy = [H|T],
    (   findstate(X,Copy,Z,Found,Counter,Null),Found = true
  ->  random(Epsilon,10,R),
      (   R = Epsilon
      ->
          actgreedy(Z,Max,Null),
          nth0(Pos,Z,Max),
          findall(Output,blanktozero(Copy,Output),Bag),
          assert(action(Bag,Pos)),
          Newepisode = [action(Bag,Pos)|Episode]
      ;
          findall(Output,blanktozero(Copy,Output),Bag),
          deletelistlist([0,1,2,3,4,5,6],Null,Randlist),
          random_member(Pos,Randlist),
          assert(action(Bag,Pos)),
          Newepisode = [action(Bag,Pos)|Episode])
   ; (Counter == red
    ->
     findall(Output,blanktozero(Copy,Output),Bag),
     assert(rlstate(Bag,[0,0,0,0,0,0,0],[])),
     redAI(X),
     NewX = [rlstate(Bag,[0,0,0,0,0,0,0],[])|X],
     retract(redAI(X)),
     assert(redAI(NewX))

  ;
     findall(Output,blanktozero(Copy,Output),Bag),
     assert(rlstate(Bag,[0,0,0,0,0,0,0],[])),
     yelAI(X),
     NewX = [rlstate(Bag,[0,0,0,0,0,0,0,[]])|X],
     retract(yelAI(X)),
     assert(yelAI(NewX))),

     random(0,7,Pos),
     assert(action(Bag,Pos)),
     Newepisode = [action(Bag,Pos)|Episode]).


montecarlo(Reward,Z,Y,Repisode,A,Counter,Null):- %this updates the values of states
   (   Counter == red
   ->  redAI(X)

  ;    yelAI(X)),

  length(Repisode,Len),
  member(action(Z,Y),Repisode),
  rlstate(Z,A,Null),
  nth0(Dismult,Repisode,action(Z,Y)),
  Discount is 2**Dismult,
  nth0(Y,A,Oldval),
 %calc rewrd
  Cost is Reward - Oldval,
  NR is 0.001 * Cost/Discount,
  Newv is Oldval + NR,
  replace(Y,A,Newv,Repac),%update reward
  retract(rlstate(Z,A,Null)),
  delete(X,rlstate(Z,A,Null),Retx),
  assert(rlstate(Z,Repac,Null)),
  NewX = [rlstate(Z,Repac,Null)|Retx],
  (   Counter == red
  ->  retractall(redAI(X)),
      assert(redAI(NewX))

  ;   retractall(yelAI(X)),
      assert(yelAI(NewX))),

   (   I is Len -1,
       nth0(I,Repisode,action(Z,Y))
   ->  !
   ;    montecarlo(Reward,Z,Y,Repisode,A,Counter,Null)).










isboardfull(Columbs,X):- %checks for draw by seeing if board is full
    member(X,Columbs),
    (X is 0
    ->  !
    ;
    mylength(X,Len),
    Len < 6).



findstate(X,State,Z,Found,Counter,Null):- %searches for a specific state
  (   Counter == red
  ->  redAI(X)
  ;   yelAI(X)),
   findall(Output,blanktozero(State,Output),Statezero),

  (   member(rlstate(Statezero,Z,Null),X)
  ->  Found = true,
      !

  ;   Found = false).

propagatevalues(Counter,Episode,X,Z,Y,Reward,Null):- %gets actions and inserts into monte carlo for positive value update
   reverse(Episode,Repisode),

                montecarlo(Reward,Z,Y,Episode,A,Counter,Null).


propagatevaluesnegative(Counter,Episode,B,C,D,Reward,Null):-  %gets actions and inserts into monte carlo for negative value update
 reverse(Episode,Repisode),


montecarlo(Reward,C,D,Episode,A,Counter,Null).



maxexceed(Columbs,Counter,Pos):- %sees if the max height of a columb has been exceeded and if it has add a pos to the banned list
  (   Counter == yellow
  ->  yelAI(X),
      rlstate(Columbs,Actions,Nullmoves),
     (   not(member(Pos,Nullmoves))
      ->  Newnullmoves = [Pos|Nullmoves],
      retract(rlstate(Columbs,Actions,Nullmoves)),
      delete(X,rlstate(Columbs,Actions,Nullmoves),Retx),
      assert(rlstate(Columbs,Actions,Newnullmoves)),
      NewX = [rlstate(Columbs,Actions,Newnullmoves)|Retx],
      retract(yelAI(X)),
      assert(yelAI(NewX)))

  ;   redAI(X),
      rlstate(Columbs,Actions,Nullmoves),
     (   not(member(Pos,Nullmoves))
      -> Newnullmoves = [Pos|Nullmoves],
      retract(rlstate(Columbs,Actions,Nullmoves)),
      delete(X,rlstate(Columbs,Actions,Nullmoves),Retx),
      assert(rlstate(Columbs,Actions,Newnullmoves)),
      NewX = [rlstate(Columbs,Actions,Newnullmoves)|Retx],
      retract(redAI(X)),
      assert(redAI(NewX)))).



taketurnAIvsAI(G,Counter,Redepisode,Yellowepisode,Epsilon):- %this does an interation for a turn with AI vs Ai
      (   Counter == yellow
      ->  aimove(G,Epsilon,Pos,Yellowepisode,Newepisode,Counter)

     ;   aimove(G,Epsilon,Pos,Redepisode,Newepisode,Counter)),

      drop(Pos,G,Counter,A,Error),
      (win(Pos,A,Counter)

      ->   (   Counter  == yellow
         ->    propagatevalues(Counter,Newepisode,X,Z,Y,1,N),
               propagatevaluesnegative(red,Redepisode,B,C,D,-1,Null)

         ;     propagatevalues(Counter,Newepisode,X,Z,Y,1,N),
               propagatevaluesnegative(yellow,Yellowepisode,B,C,D,-1,Null)),

         format("the winner is.. ~w",[Counter]),
         !

      ; (not(isboardfull(A,X))
           ->  nl,
               write("this game is a draw")

             ; (Error = true
                 ->  maxexceed(G,Counter,Pos),
                     taketurnAIvsAI(G,Counter,Redepisode,Yellowepisode,Epsilon)
                 ;   (Counter == yellow
                     ->  taketurnAIvsAI(A,red,Redepisode,Newepisode,Epsilon)
                     ;   taketurnAIvsAI(A,yellow,Newepisode,Yellowepisode,Epsilon))))).



taketurnAI(G,Counter,Episode,Epsilon):- %this does an interation for a turn with human  vs AI
  (   Counter = red
  ->     aimove(G,Epsilon,Pos,Episode,Newepisode,Counter)

  ;   write("choose a pos"),
      read(Pos)),
  (validateinput(Pos)

  ->  drop(Pos,G,Counter,A,Error),
      (win(Pos,A,Counter)
      ->
      format("the winner is.. ~w",[Counter]),
      (Counter == red
          ->  propagatevalues(red,Episode,X,Z,Y,-1,Null)),
      !



      ;    (not(isboardfull(A,X))
           ->  nl,
               write("this game is a draw")
          ;  (Error = true
                 ->  maxexceed(A,Counter,Pos),
                     taketurnAI(A,Counter,Episode,Epsilon)
                 ;   (Counter == yellow
                     ->  taketurnAI(A,red,Newepisode,Epsilon)
                     ;   taketurnAI(A,yellow,Newepisode,Epsilon))))
  ; write("This game is over"))).


taketurn(G,Counter):- %this does an iteration of turn for human vs human
  write("choose a pos"),
  read(Pos),
  (validateinput(Pos)
  -> drop(Pos,G,Counter,A,Error),
     (   win(Pos,A,Counter)
     ->  format("the winner is.. ~w",[Counter]),
         !

     ;  (not(isboardfull(A,X))
           ->  nl,
               write("this game is a draw")
; (   Error = true
   ->  taketurn(G,Counter)
   ;   (Counter == yellow
       ->  taketurn(A,red)
       ;   taketurn(A,yellow)))))


  ;  write("over")).



drop(Pos,Colums,Counter,F,Error):- %this drop a peice in connect 4 board
    nth0(Pos,Colums,Elem),
    write(Elem),
    nl,
   (not(atom(Elem)),not(is_list(Elem)),not(compound(Elem))
    ->
         Toreplace = Counter,
         replace(Pos,Colums,Toreplace,F),

         Error = false,
         nl,
         write(F)
   ;
        (atom(Elem)
        ->  Toreplace = [Elem,Counter]

        ;   [H|T] = Elem,
            append(T,[Counter],T1),
            Toreplace = [H|T1]),
         mylength(Elem,Len),
         (Len < 6
         ->  replace(Pos,Colums,Toreplace,F),
             Error = false,
             nl,
             write(F)

         ;    write("columb reached max height please choose diff pos"),
              nl,
              write(Pos),
              F = Colums,
              Error =true)
   ).



