% prolog ascii art mandelbrot maker, makes cheesy mandelbrot ascii art in prolog
% ck, januar 2010

%:- dynamic state/7.
:- dynamic storedvalue/3.
drawanddebug:-
    guitracer, trace,
   set(5,5,-1.4,1.4,-2,2,20).

% start state
default:-
  abolish(storedvalue/3),
   (dynamic storedvalue/3),
   set(170,40,-1.4,1.4,-2,2,20).

% paint a nice spiral arm somewhere in the fold between the two primary bulbs
default2:-
    set(240,120,0.131004,0.131402,-0.743567,-0.743965,750).

default3:-
    set(170,90,0.131004,0.131402,-0.743567,-0.743965,120).

% set parameters, redraw
set(Xres,Yres,Imv,Imb,Rev,Reb,Iterations) :-
   retractall(state),
    abolish(state/7), %ugly? mh.
    (dynamic state/7),
   asserta(state(Xres,Yres,Imv,Imb,Rev,Reb,Iterations)),!,
   main(Xres,Yres,(Imv:Imb),(Rev:Reb),Iterations),!.


% navigation shortcuts
left :- left(4).
right :- right(4).
up :- up(4).
down :- down(4).
zoomin :- zoom(1.25).
zoomout :- zoom(0.85).
helper :- writeln(' to get the start state: default. \n to get a nice picture: default2. \n move: left. right. up. down. all move by one quarter of the screen, can move arbitrarily with e.g. left(8), moving one eighth. \n zoom: zoomin. zoomout. or relative zoom(0<x<whathaveyou). zoom is somewhat broken, but, too lazy. \n set number iterations with iter(X). increase,decrease number of iterations by 5 with finer. simpler. \n change resolution with setres(X,Y) \n
set everything manually with set(X_resolution_in_charactes,Y_resolution,Complex_neg_boundary,Complex_pos_boundary,Real_neg_boundary,Real_pos_boundary,Number_of_iterations).').
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% iterate over the given number of pixels, get a value, and draw it.

main(Xres,Yres,(Imv:Imb),(Rev:Reb),Iterations) :-
    writeln(Xres:Yres:'--':Iterations:'--':Imv:Imb:',':Rev:Reb),
    between(1,Yres,Currentrow),
        between(1,Xres,Currentcolumn),
            zoomandc((Imv:Imb),(Rev:Reb),Xres,Yres,Im:Re,Currentcolumn:Currentrow),
	    getvalue(0,0,Im,Re,Iterations,Iterations,Value),
            %write(Value),

            draw(Value,Iterations),
            %draw(5,5),
            %draw(Currentpixel,Currentrow,Value).
            ((Currentcolumn < Xres) -> fail ; write('\n')),

    ((Currentrow < Yres) -> fail ; write('this: set('),write(Xres),write(','),write(Yres),write(','),write(Imv),write(','),write(Imb),write(','),write(Rev),write(','),write(Reb),write(','),write(Iterations),write(').') ).


% the mandelbrot set definition hides somwhere in here.
% infinity is 2, maxiterations is infinitely many iterations,
% the other values are there for recursion and stuff.
% Iterations is _how many iterations are left_ in the recursion

getvalue(_,_,Im,Re,Iterations,Iterations,Value):-
    storedvalue(Im,Re,Value),!.
getvalue(_,_,Im,Re,Iterations,Iterations,Iterations):- Im > -0.5, Im < 0.5, Re > -0.5, Re < 0.2,!.
getvalue(ZnR,ZnI,Im,Re,Maxiterations,Iterations,Value):-
     complexsquare(ZnR,ZnI,ZnsquareR,ZnsquareI),
     complexadd(ZnsquareR,ZnsquareI,Re,Im,Znp1R,Znp1I),
     ((abs(Znp1I) < 2, abs(Znp1R) < 2, Iterations > 0)  ->
           Iminus1 is Iterations -1,
           getvalue(Znp1R,Znp1I,Im,Re,Maxiterations,Iminus1,Value)
       ; (Value is Maxiterations - Iterations,(dynamic state/7),assert(storedvalue(Im,Re,Value)),!)

          ),!.

% give section of the complex plane, total resolution in pixels
% get the respective complex coordinates to certain pixels
% zoomandc(+(Imv:Imb),+(Rev:Reb),+Xres,+Yres,-Im:Re,+X:Y) :-
zoomandc((Imv:Imb),(Rev:Reb),Xres,Yres,Im:Re,X:Y) :-
    Re is Rev + (abs(Rev - Reb) * X/Xres),
    Im is Imv + (abs(Imv - Imb) * Y/Yres).


complexadd(Re1,Im1,Re2,Im2,Re,Im):-
    Re is Re1 + Re2,
    Im is Im1 + Im2.

complexsquare(Rein,Imin,Reout,Imout):-
   Reout is ((Rein * Rein) - (Imin * Imin)),
   Imout is ((Rein * Imin) * 2).
/*
complexmult(Re1,Im1,Re2,Im2,Re,Im):-
   Re is ((Re1 * Re2) - (Im1 * Im2))
   Im is ((Re1 * Im2) + (Im1 * Re2)).
*/


% map values between 1 and unknown to dots and letters, and write them
draw(1,_) :- write('.'),!.
draw(2,_) :- write('.'),!.
draw(3,_) :- write(';'),!.
draw(X,Iterations) :-  X =< Iterations/10,  write(';'),!.

draw(X,Iterations) :-  X =< (2* Iterations/10) + 2,  write('+'),!.
draw(X,Iterations) :-  X =< (3* Iterations/10) + 1,  write('π'),!.
draw(X,Iterations) :-  X =< (4* Iterations/10) + 1,  write('Z'),!.
draw(X,Iterations) :-  X =< (5* Iterations/10) ,  write('X'),!.
draw(X,Iterations) :-  X =< (6* Iterations/10) ,  write('#'),!.
draw(X,Iterations) :-  X =< (7* Iterations/10) ,  write('$'),!.
draw(X,Iterations) :-  X < (8* Iterations/10) ,  write('M'),!.
draw(_,_) :- write('-'),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% move about.

left(Factor) :-
   state(Xres,Yres,Imv,Imb,Rev,Reb,Iterations),!,

   writeln(state(Xres,Yres,Imv,Imb,Rev,Reb,Iterations)),
   retractall(state),
    abolish(state/7),
    (dynamic state/7),
   RevN is Rev - (abs(Rev - Reb) / Factor),
   RebN is Reb - (abs(Rev - Reb) / Factor) ,
   %ImvN is Imv + (abs(Imv - Imb) / Factor)  ,
   %ImbN is Imb + (abs(Imv - Imb) / Factor)   ,
   asserta(state(Xres,Yres,Imv,Imb,RevN,RebN,Iterations)),!,
   main(Xres,Yres,(Imv:Imb),(RevN:RebN),Iterations),!.


right(Factor) :-
   state(Xres,Yres,Imv,Imb,Rev,Reb,Iterations),!,

   writeln(state(Xres,Yres,Imv,Imb,Rev,Reb,Iterations)),
   retractall(state),
    abolish(state/7),
    (dynamic state/7),
   RevN is Rev + (abs(Rev - Reb) / Factor),
   RebN is Reb + (abs(Rev - Reb) / Factor) ,
   %ImvN is Imv + (abs(Imv - Imb) / Factor)  ,
   %ImbN is Imb + (abs(Imv - Imb) / Factor)   ,
   asserta(state(Xres,Yres,Imv,Imb,RevN,RebN,Iterations)),!,
   main(Xres,Yres,(Imv:Imb),(RevN:RebN),Iterations),!.


up(Factor) :-
   state(Xres,Yres,Imv,Imb,Rev,Reb,Iterations),!,

   writeln(state(Xres,Yres,Imv,Imb,Rev,Reb,Iterations)),
   retractall(state),
    abolish(state/7),
    (dynamic state/7),
  % RevN is Rev - (abs(Rev - Reb) / Factor),
  % RebN is Reb - (abs(Rev - Reb) / Factor) ,
   ImvN is Imv - (abs(Imv - Imb) / Factor)  ,
   ImbN is Imb - (abs(Imv - Imb) / Factor)   ,
   asserta(state(Xres,Yres,ImvN,ImbN,Rev,Reb,Iterations)),!,
   main(Xres,Yres,(ImvN:ImbN),(Rev:Reb),Iterations),!.

down(Factor) :-
   state(Xres,Yres,Imv,Imb,Rev,Reb,Iterations),!,

   writeln(state(Xres,Yres,Imv,Imb,Rev,Reb,Iterations)),
   retractall(state),
    abolish(state/7),
    (dynamic state/7),
  % RevN is Rev - (abs(Rev - Reb) / Factor),
  % RebN is Reb - (abs(Rev - Reb) / Factor) ,
   ImvN is Imv + (abs(Imv - Imb) / Factor)  ,
   ImbN is Imb + (abs(Imv - Imb) / Factor)   ,
   asserta(state(Xres,Yres,ImvN,ImbN,Rev,Reb,Iterations)),!,
   main(Xres,Yres,(ImvN:ImbN),(Rev:Reb),Iterations),!.

zoom(Factor) :-
   state(Xres,Yres,Imv,Imb,Rev,Reb,Iterations),!,
   retractall(state),
    abolish(state/7),
    (dynamic state/7),
   abolish(storedvalue/3),
   (dynamic storedvalue/3),
   Imspan is abs(Imv - Imb),
   Respan is abs(Rev - Reb),

   ImvN is Imb - Imspan / Factor,
   ImbN is Imv + Imspan / Factor,
   RevN is Reb - Respan / Factor,
   RebN is Rev + Respan / Factor,
   asserta(state(Xres,Yres,ImvN,ImbN,RevN,RebN,Iterations)),!,
   main(Xres,Yres,(ImvN:ImbN),(RevN:RebN),Iterations),!.


iter(IterationsN) :-
    state(Xres,Yres,Imv,Imb,Rev,Reb,_),!,
    retractall(state),
    abolish(state/7),
    (dynamic state/7),
   abolish(storedvalue/3),
   (dynamic storedvalue/3),
   asserta(state(Xres,Yres,Imv,Imb,Rev,Reb,IterationsN)),!,
   main(Xres,Yres,(Imv:Imb),(Rev:Reb),IterationsN),!.


setres(Xres,Yres) :-
    state(_,_,Imv,Imb,Rev,Reb,Iterations),!,
    retractall(state),
    abolish(state/7),
    retractall(state),
    (dynamic state/7),
    abolish(storedvalue/3),
   (dynamic storedvalue/3),
   asserta(state(Xres,Yres,Imv,Imb,Rev,Reb,Iterations)),!,
   main(Xres,Yres,(Imv:Imb),(Rev:Reb),Iterations),!.


finer :-
   state(Xres,Yres,Imv,Imb,Rev,Reb,Iterations),!,
   retractall(state),
    abolish(state/7),
    (dynamic state/7),
    abolish(storedvalue/3),
   (dynamic storedvalue/3),
   IterationsN is Iterations + 5,
   asserta(state(Xres,Yres,Imv,Imb,Rev,Reb,IterationsN)),!,
   main(Xres,Yres,(Imv:Imb),(Rev:Reb),IterationsN),!.

simpler :-
   state(Xres,Yres,Imv,Imb,Rev,Reb,Iterations),!,
   ((Iterations < 6) -> Iterations = IterationsN ; (
   retractall(state),
    abolish(state/7),
    (dynamic state/7),
   abolish(storedvalue/3),
   (dynamic storedvalue/3),
   IterationsN is Iterations - 5,
   asserta(state(Xres,Yres,Imv,Imb,Rev,Reb,IterationsN)),!)),
   main(Xres,Yres,(Imv:Imb),(Rev:Reb),IterationsN),!.
                                                            /*
draw(X,Iterations) :-  X < Iterations -6, X > Iterations/2,  write(;),!.
draw(X,Iterations) :-  X is Iterations-6, write('x'),!.
draw(X,Iterations) :-  X is Iterations-5, write('X'),!.
draw(X,Iterations) :-  X is Iterations-4, write('Z'),!.
draw(X,Iterations) :-  X is Iterations-3, write('#'),!.
draw(X,Iterations) :-  X is Iterations-2, write('M'),!.
draw(X,Iterations) :-  X is Iterations-1, write('§'),!.
draw(_,_) :- write('-'),!.
                                                              */
%draw(X,Y,Value) :-

/*

    e.g. x at Im -0.75, Re 0.9

    0                              80
                    | 2
                    |
                    |
              x     |
                    |
    ----------------|-----------------
      -2            |               2
                    |
                    |
                    |
                    | -2
                                   80




    0,0 => -1,3
    40,40 =>


    0                              80
              | 3
              |
              |
              |
              |
        x     |
              |
    ----------|-----------------------
      -1      |                   3
              |
              | -1
                                   80



    x 0 => 1
    x 1 => 1 + (1/80) * 1.5 -1

    pixel       Re
    0           Rev
    1           Rev + 1/80 * Rev -Reb
    X           Rev + (abs(Rev - Reb) * X/Xpixels)


    0                              80
                   2





     1                              1.5



                  1.5
                                   80
                                  */
