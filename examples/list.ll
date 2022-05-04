-- consts: nil, a, b, c
-- fns: cons/2

-- Relational characterizations:

-- member/2

forall X Y. member(X, cons(X, Y));

forall X Y Z. member(X, Z) ==> member(X, cons(Y, Z));

-- append/3

forall Z. append(nil, Z, Z);

forall X Y Z W. append(Y, Z, W) ==> append(cons(X, Y), Z, cons(X, W));