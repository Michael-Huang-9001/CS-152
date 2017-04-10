eval(num(X), X).
eval(sum(X, Y), Z) :- eval(X, N1), eval(Y, N2), Z is N1+N2.
eval(prod(X, Y), Z) :- eval(X, N1), eval(Y, N2), Z is N1*N2.