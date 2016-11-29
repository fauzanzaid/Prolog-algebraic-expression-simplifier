solve( add(Addend,Augend), Result ):-
	solve(Addend,SubResult1),
	solve(Augend,SubResult2),
	polyadd(SubResult1,SubResult2,Result).

solve( subtract(Minuend,Subtrahend), Result ):-
	solve(Minuend,SubResult1),
	solve(Subtrahend,SubResult2),
	polysubtract(SubResult2,SubResult1,Result).

solve( multiply(Multiplicand, Multiplier), Result ):-
	solve(Multiplicand, SubResult1),
	solve(Multiplier, SubResult2),
	polymultiply(SubResult1, SubResult2, Result).

solve( divide(Dividend, Divisor), Result ):-
	solve(Dividend, SubResult1),
	solve(Divisor, SubResult2),
	polydivide(SubResult2, SubResult1, Result).

solve( [ mono(CoefHead,VarListHead) | Tail ], [ mono(CoefHead,VarListHead) | Tail ] ).
solve( [], [] ).


polyadd( [], Poly2, Poly2 ).

polyadd( [ HeadPoly1 | TailPoly1 ], Poly2, Result ):-
	polyadd( TailPoly1, Poly2, SubResult ),
	monoadd( HeadPoly1, SubResult, [], Result ).




polysubtract( [], Poly2, Poly2 ).

polysubtract( [ HeadPoly1 | TailPoly1 ], Poly2, Result ):-
	polysubtract( TailPoly1, Poly2, SubResult ),
	arg(1, HeadPoly1, CoefHeadPoly1),
	arg(2, HeadPoly1, VarListHeadPoly1),
	%% NewCoefHeadPoly1 = (-1)*(CoefHeadPoly1),
	fractmultiply( fract(-1,1), CoefHeadPoly1, NewCoefHeadPoly1 ),
	monoadd( mono(NewCoefHeadPoly1,VarListHeadPoly1), SubResult, [], Result ).




monoadd( Mono, [], Poly2, Result ):-
	append([Mono], Poly2, Result).

monoadd( Mono, [ HeadPoly1 | TailPoly1 ], Poly2, Result ):-
	
	arg(2, Mono, VarListMono),
	arg(2, HeadPoly1, VarListHeadPoly1),
	(
		(
			VarListMono == VarListHeadPoly1,
			arg(1, Mono, CoefMono),
			arg(1, HeadPoly1, CoefHeadPoly1),
			
			%% NewCoefMono is CoefMono+CoefHeadPoly1,
			fractadd( CoefMono, CoefHeadPoly1, NewCoefMono ),

			(
				(
					%% NewCoefMono =\= 0,
					not(arg(1, NewCoefMono, 0)),
					append( [mono(NewCoefMono,VarListMono)], TailPoly1, NewPoly1 ),
					append( NewPoly1, Poly2, Result )
				);
				(
					%% NewCoefMono == 0,
					arg(1, NewCoefMono, 0),
					append( TailPoly1, Poly2, Result )
				)
			)
		);

		(
			VarListMono \== VarListHeadPoly1,
			append([HeadPoly1], Poly2, NewPoly2),		
			monoadd( Mono, TailPoly1, NewPoly2, Result )
		)
	).




polymultiply( [], _, [] ).

polymultiply( [ HeadPoly1 | TailPoly1 ], Poly2, Result ):-
	monomultiply( HeadPoly1, Poly2, SubResult1 ),	
	polymultiply( TailPoly1, Poly2, SubResult2 ),
	polyadd(SubResult1, SubResult2, Result).


monomultiply( _, [], []).

monomultiply( Mono, [ HeadPoly | TailPoly ], Result ):-
	arg(1, Mono, CoefMono),
	arg(2, Mono, VarListMono),
	arg(1, HeadPoly, CoefHeadPoly),
	arg(2, HeadPoly, VarListHeadPoly),

	%% NewCoefHeadPoly is CoefMono*CoefHeadPoly,
	fractmultiply( CoefMono, CoefHeadPoly, NewCoefHeadPoly ),
	addList( VarListMono, VarListHeadPoly, NewVarListHeadPoly ),

	append( [mono(NewCoefHeadPoly,NewVarListHeadPoly)], SubResult, Result ),
	monomultiply( Mono, TailPoly, SubResult ).




polydivide( [ Mono | _ ], Poly, Result ):-
	arg(1, Mono, CoefMono),
	arg(2, Mono, VarListMono),
	%% NewCoefMono = 1/CoefMono,
	fractdivide( fract(1,1), CoefMono, NewCoefMono ),
	negList( VarListMono, NewVarListMono ),
	monomultiply( mono(NewCoefMono, NewVarListMono), Poly, Result ).



	
addList( [], [], []).
addList( [H1|T1], [H2|T2], Result):-
	Temp is H1 + H2,
	append([Temp], SubResult, Result),
	addList(T1, T2, SubResult).

negList( [], [] ).
negList( [H|T], Result ):-
	Temp is (-1)*H,
	append( [Temp], SubResult, Result ),
	negList( T, SubResult ).




fractadd( fract(N1,D1), fract(N2, D2), fract(NR, DR) ):-
	NT is (N1*D2)+(N2*D1),
	DT is D1*D2,
	gcd(NT, DT, Temp),
	NR is NT/Temp,
	DR is DT/Temp.

fractsubtract( fract(N1,D1), fract(N2, D2), fract(NR, DR) ):-
	NT is (N1*D2)-(N2*D1),
	DT is D1*D2,
	gcd(NT, DT, Temp),
	NR is NT/Temp,
	DR is DT/Temp.

fractmultiply( fract(N1,D1), fract(N2, D2), fract(NR, DR) ):-
	NT is N1*N2,
	DT is D1*D2,
	gcd(NT, DT, Temp),
	NR is NT/Temp,
	DR is DT/Temp.

fractdivide( fract(N1,D1), fract(N2, D2), fract(NR, DR) ):-
	NT is N1*D2,
	DT is D1*N2,
	gcd(NT, DT, Temp),
	NR is NT/Temp,
	DR is DT/Temp.


%% gcd(0, A, A):- A > 0, !.
%% gcd(A, B, R):- A >= B, NewA is A-B, gcd(NewA,B,R).
%% gcd(A, B, R):- A < B, NewA is B-A, gcd(NewA,A,R).



gcd(A, B, R) :-
    A < 0, !,
    gcd(-A, B, R).
gcd(A, B, R) :-
    B < 0, !,
    gcd(A, -B, R).
gcd(A, 0, A) :- A > 0.
gcd(0, B, B) :- B > 0.
gcd(A, B, R) :-
    A > B, B > 0,
    NewA is A - B,
    gcd(B, NewA, R).
gcd(A, B, R) :-
    A =< B, A > 0,
    NewB is B - A,
    gcd(A, NewB, R).


equal(X,Y,Result):-
	solve(X, SubResult1),
	solve(Y, SubResult2),
	(
		(
			SubResult1 = SubResult2,
			Result is 1
		);
		(
			SubResult1 \= SubResult2,
			Result is -1
		)
	).


%% solve( multiply( divide( multiply( [mono(1,[1,0,0])] , [mono(1,[0,1,0])] ) , [mono(1,[0,0,0])] ) , add( [mono(1,[0,0,1])] , subtract( [mono(1,[0,0,0])] , [mono(1,[0,0,1])] ) ) ) , R ).
