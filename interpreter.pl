factorial(
	compound([
		assign(n, 7),
		assign(fact, 1),
		while(gt(n,1),
			compound([
				assign(fact, mult(fact,n)),
				assign(n, minus(n,1))
			])
		),
		print(fact)
	])
).

gcd(
	compound([
		assign(a, 10),
		assign(b, 4),
		while(noteq(b, 0),
			compound([
				assign(r, mod(a, b)),
				assign(a, b),
				assign(b, r)
			])
		),
		print(a)
	])
).

primes(
	for(assign(p,2), lt(p,20), inc(p),
		compound([
			assign(isprime,1),
			for(assign(d,2), lteq(mult(d,d), p), inc(d),
				if(eq(mod(p,d),0), assign(isprime, 0))
			),
			if(eq(isprime,1),
				print(p)
			)
		])
	)
).

fibonacci(
	compound([
		assign(n, 7),
		assign(p, 1),
		assign(q, 1),
		while(noteq(n, 1),
			compound([
				assign(r, p),
				assign(p, q),
				assign(q, plus(r, q)),
				assign(n, minus(n, 1))
			])
		),
		print(q)
	])
).


power(
	compound([
		assign(x, 5),
		assign(y, 7), 
		assign(p, x),
		while(noteq(y, 1),
			compound([
				assign(p, mult(p, x)),
				assign(y, minus(y, 1))
			])
		),
		print(p)
	])
).

printTabs(0) :- !.
printTabs(N) :- write('  '), N1 is N -1, printTabs(N1).

align(_, compound([])).

align(N, compound([H|T])) :- 
	align(N, H),
	align(N, compound(T)),!.

align(N, while(CondExpression, Statement)) :- 
	printTabs(N), write('while('), print(CondExpression), write(') {\n'),
	N1 is N + 1, align(N1, Statement),
	printTabs(N), write('}\n'),!.

align(N, if(CondExpression, Statement)) :- 
	printTabs(N), write('if('), print(CondExpression), write(') {\n'),
	N1 is N + 1, align(N1, Statement),
	printTabs(N), write('}\n'),!.

align(N, if(CondExpression, Statement, ElseStatement)) :- 
	printTabs(N), write('if('), print(CondExpression), write(') {\n'),
	N1 is N + 1, align(N1, Statement),
	printTabs(N), write('} else {\n'),
	N1 is N + 1, align(N1, ElseStatement),
	printTabs(N), write('}\n'),!.

align(N, for(StartStatement, Condition, EndStatement, Statement)) :- 
	printTabs(N), write('for('), print(StartStatement), write('; '), print(Condition),
	write('; '), print(EndStatement),  write(') {\n'),
	N1 is N + 1, align(N1, Statement),
	printTabs(N), write('}\n'),!.

align(N, Statement) :- 
	printTabs(N), print(Statement), write(';\n').

print(print(Expression)) :- write('print '), print(Expression),!.
print(assign(Variable, Expression)) :- write(Variable), write(' := '), print(Expression),!.

print(inc(Variable)) :- write(Variable), write('++'),!.
print(dec(Variable)) :- write(Variable), write('--'),!.
print(lt(E1,E2)) :- print(E1), write(' < '), print(E2),!.
print(gt(E1,E2)) :- print(E1), write(' > '), print(E2),!.
print(lteq(E1,E2)) :- print(E1), write(' <= '), print(E2),!.
print(gteq(E1,E2)) :- print(E1), write(' >= '), print(E2),!.
print(plus(E1,E2)) :- print(E1), write(' + '), print(E2),!.
print(mult(E1,E2)) :- print(E1), write(' * '), print(E2),!.
print(minus(E1,E2)) :- print(E1), write(' - '), print(E2),!.
print(divide(E1,E2)) :- print(E1), write(' / '), print(E2),!.
print(mod(E1,E2)) :- print(E1), write(' % '), print(E2),!.
print(eq(E1,E2)) :- print(E1), write(' == '), print(E2),!.
print(noteq(E1,E2)) :- print(E1), write(' != '), print(E2),!.
print(Expression) :- write(Expression).

indent(P) :- align(0, P).


run(P) :- eval(P,[],_).


check(lt(E1,E2), S) :- evalexpr(E1, S, V1), evalexpr(E2, S, V2), V1 < V2.
check(gt(E1,E2), S) :- evalexpr(E1, S, V1), evalexpr(E2, S, V2), V1 > V2.
check(lteq(E1,E2), S) :- evalexpr(E1, S, V1), evalexpr(E2, S, V2), V1 =< V2.
check(gteq(E1,E2), S) :- evalexpr(E1, S, V1), evalexpr(E2, S, V2), V1 >= V2.
check(eq(E1,E2), S) :- evalexpr(E1, S, V1), evalexpr(E2, S, V2), V1 == V2.
check(noteq(E1,E2), S) :- evalexpr(E1, S, V1), evalexpr(E2, S, V2), V1 \= V2.
check(invert(E1), S) :- not(check(E1,S)).

evalexpr(A, _, A) :- number(A).
evalexpr(A, S, V) :- member((A, V), S).

evalexpr(plus(A,B), S, V) :- evalexpr(A,S,V1), evalexpr(B, S, V2), V is V1 + V2.
evalexpr(mult(A,B), S, V) :- evalexpr(A,S,V1), evalexpr(B, S, V2), V is V1 * V2.
evalexpr(minus(A,B), S, V) :- evalexpr(A,S,V1), evalexpr(B, S, V2), V is V1 - V2.
evalexpr(divide(A,B), S, V) :- evalexpr(A,S,V1), evalexpr(B, S, V2), V is V1 // V2.
evalexpr(mod(A,B), S, V) :- evalexpr(A,S,V1), evalexpr(B, S, V2), V is V1 mod V2.

eval(compound([]), S, S).
eval(compound([H|T]),A,C) :- eval(H, A, B),
%	write(H), write('  :  '), write(A), write('  ->  '), write(B), write('\n'),
	eval(compound(T), B, C).
eval(while(Cond, _), S, S) :- not(check(Cond,S)).
eval(while(Cond, Statement), A, C) :- check(Cond,A), eval(Statement, A, B), eval(while(Cond, Statement), B, C).

eval(if(Cond, _), A, A) :- not(check(Cond, A)).
eval(if(Cond, Statement), A, B) :- check(Cond, A), eval(Statement, A, B).

eval(if(Cond, _, ElseStatement), A, B) :- not(check(Cond, A)), eval(ElseStatement, A, B),!.
eval(if(Cond, Statement, _), A, B) :- check(Cond, A), eval(Statement, A, B).

% for(i=0; i<10; i++) { Statement }
eval(for(StartStatement, Condition, EndStatement, Statement), A, B) :-
	eval(compound([StartStatement, while(Condition, compound([Statement, EndStatement]))]), A, B).


eval(assign(Variable, Expression), A, B) :- evalexpr(Expression, A, Value), modifyRAM((Variable, Value), A, B).

eval(inc(Variable), A, B) :- eval(assign(Variable, plus(Variable,1)), A, B).
eval(dec(Variable), A, B) :- eval(assign(Variable, minus(Variable,1)), A, B).

eval(print(Expression),S,S) :- evalexpr(Expression, S, V), write(V), write('\n').

modifyRAM((Variable, Value), [(Variable,_)|T], [(Variable,Value)|T]) :- !.
modifyRAM((Variable, Value), [H|T1], [H|T2]) :- modifyRAM((Variable, Value), T1, T2).
modifyRAM((Variable, Value), [], [(Variable, Value)]).



:-  write('Factorial Program:\n'), factorial(F), indent(F),
	write('\nRunning Factorial Program:\n'), run(F),
	write('\n\ngcd Program:\n'), gcd(G), indent(G),
	write('\nRunning gcd Program:\n'), run(G),
	write('\n\nPrimes Program:\n'), primes(P), indent(P),
	write('\nRunning Primes Program:\n'), run(P),
	write('\n\nFibonacci Program:\n'), fibonacci(R), indent(R),
	write('\nFibonacci Program:\n'), run(R),
	write('\n\nPower Program:\n'), power(Power), indent(Power),
	write('\nPower Program:\n'), run(Power).
