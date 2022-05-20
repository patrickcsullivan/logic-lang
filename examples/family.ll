--       Amy     
--        |      
--      --------
--      |      |
--     Bud     Bob
--      |      | |
--      ? --?--- | 
--      | |      |   
--      Coe     Cal 

p(amy, bob);
p(amy, bud);

p(bob, cal);
p(bud, coe) or p(bob, coe); 

forall X Y Z. p(X, Y) and p(Y, Z) ==> g(X, Z);