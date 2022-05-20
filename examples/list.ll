-- consts: nil, a, b, c
-- fns: cons/2

-- Relational characterizations:

-- is_list/2

is_list(nil);
~is_list(a);
~is_list(b);
~is_list(c);

forall X. is_list(cons(X, nil));

forall X Y. is_list(Y) ==> is_list(cons(X, Y));

-- member/2

forall X. ~member(X, nil);
forall X. ~member(X, a);
forall X. ~member(X, b);
forall X. ~member(X, c);

forall X Y. member(X, cons(X, Y));

forall X Y Z. member(X, Z) ==> member(X, cons(Y, Z));
forall X Y Z. ~member(X, Z) ==> 


-- append/3

forall Z. append(nil, Z, Z);

forall X Y Z W. append(Y, Z, W) ==> append(cons(X, Y), Z, cons(X, W));