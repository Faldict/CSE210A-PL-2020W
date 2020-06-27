load ../harness

@test "970ce7a0c549" {
  check 'while z *x     =   -3  ∧     3     *   x   =    z+R    do z   :=y  *     z    ;
 
y  :=     1     -   0 ' '{y → 1}'
}

@test "839c49277c30" {
  check 'if (true   ∨    true)   then  x  :=   z  + y else     x   :=   y +   1;
 

skip' '{x → 0}'
}

@test "28ec8cf41e69" {
  check 'if (true ∧   true)   then 
 

if (-2    = 0     +     x    ∨i   -   1   <     x-   1)   then  
 skip     else  


skip  else z  :=  x    *  y   ;   if (true ∧  true) then  y     :=    nX   --4     else  

 x    :=     z   *     -2     ' '{y → 4}'
}

@test "2591747b49ee" {
  check 'if (2  *    y  =z    + -3   ∨ x -     Zw    =z)     then 
EA     :=     N   *   1      else  skip  ' '{EA → 0}'
}

@test "1ef34d82e56f" {
  check 'if (false     ∧  -2    -  0    =     z -     0)  then 
while (¬(y -z     <    x *    -1))    do  
 
skip else  

while (¬true) do 
skip    ;
  
while 2- -3  =  P5    *  x     do  

  skip   ' '{}'
}

@test "e881fa6e1469" {
  check 'if (3 -   -3   =    z-    0 ∨ y     -     2= y    *     -2) then 
while -3     -x    * 4   =     y     -     z    ∧  z *   y= -2    *   4   do    
skip else 
  
if (¬(z     +     -2    <  YT     +  -1))    then   skip else skip;  
if (1 -1  < y)    then skip    else 

if (2   -  x   <    z  -   -4  ∧   true)   then  
   x  :=    G    +     1     else   
 
skip     ' '{x → 1}'
}

@test "1476420098d6" {
  check 'if (false  ∧ -2     +     y   <     -1  +1)    then 

  z:=   -3-  -4     else  if (false∨   -3     +x     =  -3    * -1)   then     y     :=z   -   z else  z    :=   y*-2; 

  if (¬false)   then 
   skip     else 
skip ' '{z → 0}'
}

@test "ae78f671a8bb" {
  check 'if ((-2  +  -4) *y     =2 -  4 ∨     -4   -     y <  y     *    UP)  then 

  skip else  

y     :=   -2   -    XN;if (false ∨    true) then 

 
 if true    then 
skip   else   

 x :=-3 -     -3      else   
  z    :=    4    +   x' '{}'
}

@test "d255c79a28d7" {
  check 'if (¬(-4  +z<1 +     y))   then 
 
z :=    -4  +     1 else   skip     ' '{}'
}

@test "d06218f98a39" {
  check 'if (-1   +     x  =x+ Az   ∨    -2   *2 = -2   -y)   then  
if (true ∨false)    then 

 
skip  else   
z:=     1     +  -3     else  


 skip   ; 
if (co   +    z    =-1    -     0     ∨ x     +     -4 <  y +   1) then     
if (-2+  2   =    -3 +  z)  then 

skip     else x := 3 - -1  else  
  skip   ' '{x → 4}'
}

@test "f697c4d25f2e" {
  check 'if (2 *     y     <   y *   -2 ∧2     *     2=-2     -  -1)   then  

  skip    else 
 

skip  ;   
if (false    ∧c--4     <    -1     -     z)  then y:= y  +   x    else  
if (true    ∨   3     -   y <  -2   *    4)      then  S2 :=   y + z      else   
 x    :=  -4    -   -1     ' '{S2 → 0}'
}

@test "6beec90accec" {
  check 'if (¬(4   +   y  =  -4  * z))    then 

K :=     jr   -    0      else 

TZ  :=  1;   
if (¬(y   -  4     <   -1   *2))     then y:=    3  -   y  else  

 skip' '{K → 0}'
}

@test "832192f3f248" {
  check 'while -4 *     x   <    z    ∨1   <    y    - z do  


x :=  -2 *2     ' '{}'
}

@test "74558b2aa8f8" {
  check 'if (¬(y   +    1   <1*   -1))  then 



while 3   *   x=     y+    1∧   z    +   d     < -4     do 
  
skip      else 


 while (¬(3+    Q=     v   -   y))    do 
   x  :=   j1;
 if (true   ∧ -3  -     x    <4 *x)   then  

while (¬(z  * u   <  -2  -    -3))    do 

  
skip      else   
y     :=x+ -3   ;if (y   +     -2<  -4    *Aw) then  
 skip  else   skip ' '{}'
}

@test "ce00f73b05b0" {
  check 'if (true ∨   true)  then skip   else   while (¬false)  do 


x   := x ;if (¬(3 =    x -     P7)) then      skip    else  
skip' '{}'
}

@test "4bb776ca0b55" {
  check 'if (¬true)  then 
    z :=z     *x  else Pz  :=     -4     *-3  ;

 
 y  :=     3    +  x    ' '{Pz → 12, y → 3}'
}

@test "baa4b282ee95" {
  check 'while (¬true)   do 
 
skip ;while false   ∧    true   do   
  x   := x  +y' '{}'
}

@test "dab9ae666760" {
  check 'if (SB     *   z=    1+x    ∨  hK * -2  =1     -  y)     then 
z   :=    -1  - z     else    z  :=   4    -    -4     ;
  while false    do     skip     ' '{z → 8}'
}

@test "459b763f131d" {
  check 'if (¬(-2     *    x   < y  * 4))  then  
skip     else 

skip    ' '{}'
}

@test "cd4c8103271d" {
  check 'while -2 *x   <1    *     w   ∧   true      do x  := -3    -    x     ; 
 
if (z -   3<     -4  + z  ∨     false)  then 
 
skip   else 




z  :=z    ' '{z → 0}'
}

@test "d53e5efab807" {
  check 'if (-2 =     z  -    x ∧  0  -   -2=     z) then 
 skip      else 

 
 while 1  +-2=   x  ∨   false do 
 y  :=   y     *-4 ; 

y  :=     -4  *   3   ' '{y → -12}'
}

@test "3a14211141dd" {
  check 'if (z  -1   <1+ -4∨     false)  then  
skip     else    skip  ;
 

while z*   x     <    -3  -    1 do Y2   :=    z  +   x  -  3  ' '{}'
}

@test "f6fe76a88314" {
  check 'skip     ; 

 if (false  ∧   x + -1   =     z) then 
 skip     else  

skip     ' '{}'
}

@test "f722ffc22b9c" {
  check 'if (¬(y*x  < y    -     Tm))    then  

 
skip     else y :=3    +    3    ;    if (true  ∧   y  *     x  <  x - -2)      then  if (y +    z  =     0   -     -3   ∨   z    -    y   =3    +4)   then  skip    else   
 skip   else 
 
 
skip  ' '{}'
}

@test "26f79b729183" {
  check 'y    :=    1     *   -1    ;

if (x + 2   <1    - he   ∨    y +  0    =   -3  -    2) then 
 

while z    +    z  =    4   + x     ∧ true  do 
  
skip  else 
 skip   ' '{y → -1}'
}

@test "54df573f6e62" {
  check 'while x     -    y  <    2+    4   ∧    Q-    -2  <-3    + x     do 
  

if (true     ∧true) then 
  
skip  else 
 
k     :=  z  -    y   ;if (4  +    z    <   z     -    1  ∨false)  then 
while 1  +     y     <     -4     -   -1    ∨   false      do 
skip   else  if (x     +   3   =  x    +    y     ∨y*-4     =    z    +   M) then 
  
z :=     x   -y  else  skip ' '{z → 0}'
}

@test "33bd7ce3839f" {
  check 'while false   ∧  kU-  z    <  -1   +    y   do skip   ;     if (false ∧ s  +     -4    <    -1 +z)      then while 2*     -2    =    g   *  1     ∨ false  do  
 y :=    z+  x      else 
skip ' '{}'
}

@test "28a72f083b4d" {
  check 'if false    then while false     ∨    4  +l8     =   y      do 
 
 skip    else  

 skip    ;

while -4   *    -2     = z∨-2  +   m=   z *x    do 

skip     ' '{}'
}

@test "2c7cd0ab06cb" {
  check 'while z    + z<    z-    u   ∧true    do  x:=    -1 -    4   ; 

if (¬false)      then 
 
if (¬(2  +    z  <     z  -   x))     then 
  skip     else 


while 0     *     S8  <  (x -    4)     -   -4     ∨ K+     kl<    K    *  y  do 

 skip     else    skip     ' '{}'
}

@test "d5999be317b8" {
  check 'if (¬(1   +   x   =   z   -  3))  then  x   :=   -2    *    -4     else 

 y:= 2-    z   ; if (3  *-4<    y -  z)     then  
if (¬(0 - Zn = nO   *     4)) then 


  skip    else  

z    :=    y    +   x   else  if (false    ∨  y     -     (-4 + 3)  <1  -x)     then   z   :=   x +-1 else 
skip ' '{x → 8, z → 8}'
}

@test "089b01d73fd2" {
  check 'if (¬true)     then  

  skip      else 



skip    ;
 

if (3  * x    <  2     +   x ∨    l - 2     <    -2)   then  
if (2*  y   <    4  -   z  ∨ 1    -wb  =-1     +  y)   then 
 y4:=   x   + 4     else 


 skip     else 



skip   ' '{y4 → 4}'
}

@test "acab0838d3c5" {
  check 'skip ;

skip  ' '{}'
}

@test "7b93c425c0b6" {
  check 'if (¬(o     -  yV    <y    - y))    then skip     else    

skip;while true    ∧false      do if (y     +  z  =   2    - 1   ∧ x    *    -4   < T    -2)   then 
 

skip      else 


 skip   ' '{}'
}

@test "590cc651e830" {
  check 'if (true     ∧   0  *  0    <-3     +    z)     then  

  z :=    -4  *    -3 else skip     ; 
while false∨     1 +  x=   y *  0  do 
 

 Z:=  z   --1 ' '{}'
}

@test "c22ac60a93f2" {
  check 'if (-2  *     4   <     -2  *    4  ∧   3   --4   <   x    *    3)    then z     := -3*4    else   if (¬(x    *   2    = 1   -    -1)) then 

skip     else skip ' '{}'
}

@test "139baa7d6933" {
  check 'if (false   ∧   2 -   y   =   Q0   *1)     then 
 y     :=-3     +   z      else 


  skip ;

if (-2    - y< z   -     -1)  then  
if (z  -  2   <y     *     -3   ∨ -1     <     z-    -3)      then   u   :=     z     else  z   :=   0     +    y     else  

v     :=2   -     -1' '{u → 0}'
}

@test "06c317ef2c2c" {
  check 'if (z* y     =     y  +    z     ∧  3    -   4  =    y-     1)    then  y  :=x     *x   else  
 skip;  skip ' '{y → 0}'
}

@test "fc7990c92606" {
  check 'if (true     ∧    true)     then while (¬(-2 *    x   = -3 *z)) do skip     else skip ; 


z    := -4     + 1' '{z → -3}'
}

@test "185402248910" {
  check 'while y<x+     y∧1  *   4   < z    +   -3    do skip;
if (¬(2 -  -2 = -1  * -3))   then  


y     :=    -3  +   -2 else   
while true    ∨ z     *  m    < 1   -  -3      do skip   ' '{y → -5}'
}

@test "bae33625b381" {
  check 'if (false∨   U   +  -3    <Im)   then 
 

x   :=  -3   - z   else 
 x    :=  2 -    y ;
  if (¬(m-  -2     <     2    -  p6))    then 

   if (¬(y     + 0     =    -4  *-4))      then skip      else 
z     :=    4  else 

 
 while true∧  E*    -4     <    x  +   -3      do  y    :=   0     -   4  ' '{x → -3}'
}

@test "73785697f25f" {
  check 'skip    ;if (x  -  1   =   2+  -4     ∨-3  +  x  < -1     *   y) then 
  
skip else 


z   :=   (z -    x)*   0    ' '{}'
}

@test "c334aff8c541" {
  check 'if (¬(1  +  E9     = z-    -4)) then 
 if (¬true)      then  
skip   else  

Hr :=    1     -    R    else 

   jN    :=-1-     4 ;if (z     *     2=2     -    -1 ∧ x +    y=    y  *  J)      then skip    else 

if (-3   +     x    =     y  -     x   ∧    z     + 2<2*     y) then  
skip     else  
P     :=    CQ    *  -4' '{Hr → 1, P → 0}'
}

@test "f82333f1efb3" {
  check 'while 1 +    w  <   -4- h   ∨0  +  y  =     -4   -  2 do skip    ' '{}'
}

@test "425998e20c7c" {
  check 'while false    ∧  true    do  
  
if (true ∧0  +n    =    y   +   4)   then skip     else C  :=-2 --4   ;
 if (2* 1   < x *  y  ∧ KM    -     y = y    *     y)      then 
 if true     then  


 x  :=  -4    +  y   else 

qb   := z*  -3 else 

  skip    ' '{}'
}

@test "72d659b771aa" {
  check 'if (¬true)  then  skip     else 
skip;
   if (x -    -4  =3 -   x     ∨    true)  then if (¬true) then  skip   else y:= -1     -     y   else  
 

x :=    1*     3     ' '{y → -1}'
}

@test "7ace4e23ed9e" {
  check 'skip   ;skip  ' '{}'
}

@test "c2435a1d3c28" {
  check 'while (¬(-1-    -1     <   -1    *    -3))     do 
z    :=     x    -mM     ;  


if (false∨     false)    then 
skip   else   

z:=   3   +  p' '{z → 3}'
}

@test "32ffcb378697" {
  check 'if (false     ∨ true)     then  

 x := 2     +     -1    else 
  skip   ;

if (¬(1-   4    <     4  + y))   then 


 
uy   :=   -1  *     (vC -    0)   else  
 
if (¬true)   then  if (¬false)  then skip  else 

skip      else  


skip' '{x → 1}'
}

@test "63867625a1e1" {
  check 'if (0 * z <y-  z ∨    true)    then skip   else  skip    ;
while (¬true)   do 

   skip   ' '{}'
}

@test "b997d52f1ce6" {
  check 'if (0 =   3     -   1)  then y     :=1     - x else   
 
while (¬(-4    +    -4   < z))  do skip  ;if (3 *  4  <    3   *  4∨  true) then v :=     -3    +-4      else 
skip    ' '{v → -7}'
}

@test "7bccab60edb7" {
  check 'if (false    ∨  false)    then 

 skip   else 
   if (¬false)    then  
 
 skip   else 




y    := z     +y    ;   
if (¬false)      then 
  skip      else 

 while (¬true)  do y     :=   3     -2   ' '{}'
}

@test "9a2db825fa13" {
  check 'skip;


  while (¬(x  *  z     <     z -  1))     do 
z :=  y     -    -4  ' '{z → 4}'
}

@test "4a5972c9757e" {
  check 'while (¬(z*z   <     4   - Z))   do skip    ;if (¬(Ri    +  -2 <   a  *-4))  then 


 skip    else   

while 2 +     x =    x +-3    ∧  x  -     3   < q -   z      do 
skip   ' '{}'
}

@test "3032dd037a2c" {
  check 'while y     +   z     =1     ∧ 3 -     0  <  4    *    -3      do   


while true∧ x - 0=   z    +    3      do  

 
skip ;




if (true    ∧     x-    z=  1) then  
E9  := 1+   x     else   
y   :=x  +     x ' '{y → 0}'
}

@test "c30d875447d7" {
  check 'skip     ' '{}'
}

@test "6acb4e2ae33c" {
  check 'while -4     - z     =   -3 *  0  ∧     false   do   x :=    x*y    ;
 while false ∧    ZZ   -   z  <   -1   *     4 do 


 skip   ' '{}'
}

@test "86e9d81bce6b" {
  check 'if (¬(z   +  z<x     -    z))    then    
skip else y := y     +   0 ;
while (¬(I*  wG  <  -1+    4))    do skip   ' '{}'
}

@test "05494c51ce0c" {
  check 'if (false     ∧ -4     -  y  <     x   + -1)   then 
   
skip else   z   :=   2 -  z     ;
if (0+x <   4   - -2∧-1     -   -3 = x    *     0)      then 
if (false∧     true) then 
 H   :=     x    -    y      else   skip    else 
skip  ' '{z → 2}'
}

@test "f4c3d8f4e0fc" {
  check 'if (¬(1 +   2     =    -4  --2))    then   
if (true    ∧ 4 <     -3    *  -3)      then  z  :=  0   +-1    else 
 
skip  else 
 
 
y   :=  -1 +     3  ;
if (pk   < 2     - y    ∨  true)  then   z  := y  +wS     else   
x     := BQ   + y   ' '{z → 0}'
}

@test "616ece267fbd" {
  check 'while z +l     =    -1-1  ∨    z    *  z  <  -4     -  x      do skip     ;



while -1  *   z    <z     +   y +    -1     ∧     0    -  2  =a  *   0   do 

 
skip  ' '{}'
}

@test "691ee9811c35" {
  check 'while true    ∧ false    do z  :=  z +-2 ;
if (true     ∧    0=     z   *-2)     then if (X+   -4     < y     * -1     ∧   z     * 4  = Fc   *    z)    then 
 
 y    :=   1    -   -2  else  

skip  else 
 

skip ' '{y → 3}'
}

@test "54c874b6f1ec" {
  check 'skip     ;
 if (y   -z   =    r -   -4     ∧false)      then  
skip    else  
ra  :=  0   +a    ' '{ra → 0}'
}

@test "4b3d2911cc38" {
  check 'while (¬(HL   +  z     <  x  -    -3))  do  
  d     :=  g   +  3  ;
if (x =  2   * 1)    then  y     :=  G1   - x      else 


 y  :=z     +     0    ' '{y → 0}'
}

@test "5b0d9f6076a4" {
  check 'if (¬(0  *    2   =  2   +  -4))     then      G:= 4  +-2   +  y     else  z  :=     z     ;skip     ' '{G → 2}'
}

@test "702e4b4fa54b" {
  check 'while false  ∧   true      do  
  if (¬(z     = nv     *     2)) then    skip else 
  x    :=  x    -     0     ' '{}'
}

@test "7583eb4e650f" {
  check 'if (-3 *  tA  = 3 - 4  ∨   E    <   -1) then 

 skip      else 
skip;



 z:=-1   ' '{z → -1}'
}

@test "0d2ba37524b8" {
  check 'if (¬(z  --1    <  1   + -2))     then     x     :=x   else 

  
skip ;  

 if (x     + 3   = M    +-1∧   w  -   z  < 1    +   z)  then  
i:=  z  +   M   else  
y  :=    4     +     u ' '{x → 0, y → 4}'
}

@test "771c3833e46c" {
  check 'if (y   * x     = 1     ∨   0     * -2  <     z  *    -4) then  
 x   :=     1     -    D1 else 
while true   ∧   o -     z     <    -3 *z    do 
skip ; if (q     +  y   < m    - D∨   hI  +  y <    -1 +    1) then if (x     -   3 < 3   +    -1  ∨  x   -     x   =    x  -  o)    then 

  d8:=     x    *  y else     
skip      else 
  

skip    ' '{}'
}

@test "c0cefa118bfc" {
  check 'if (x-  Bj     < z - 1    ∧ x<  -3     +   Lt)     then  

z    :=    b *  Wb   else 
   
x   :=     0    * -3   ;
  
while 3     --1=  0    +     x     ∨false    do 

skip ' '{x → 0}'
}

@test "dff922db2bce" {
  check 'if (-1     +  0  <   x    * y)  then  
 
if (4   -    -4    < -3)    then  x     :=   1     -  NX  else 
z:=    y    -    x     else 
  skip;
 
while false   ∧  -2*     4    =   -3 -   -1      do  
  
while (¬true)    do    x:= -2  +3   ' '{z → 0}'
}

@test "3773c13dfd43" {
  check 'if (0< z  *0  ∨     y  +2    <    z     -   4)    then 



w    :=  0   +4     else  
 y:= 0    *     3    ;while false     ∧0    <  (x+z4)     *     4      do   if (¬(0     -   -2    =   1  -    y))    then 

z  :=  -3  -    -1    else 
  skip ' '{y → 0}'
}

@test "4de239365cf6" {
  check 'if (-2  + y     =4 +3∧ -3   *   z   = x     *     Mz) then 
skip     else  

 if ((3     +     z) *  y   <     -1+     z   ∨     z    *     -2=   -4  -    y) then  


x:=     W0-   -2  else 
T2:=     x  ' '{T2 → 0}'
}

@test "863051142462" {
  check 'if (-4 -   4   = 3+ c  ∨    -2    *    1   =  4)      then   
skip      else  if (y   -z    =4  +TL ∧    4   -  z   =  y +  x)     then   x    :=     K  *  x   else  
skip;   

while y  +  -2    = x   * x   do  


skip   ' '{}'
}

@test "ca1caebfa9e8" {
  check 'while z     *   x     =     x-  4  ∨false  do 
 


m4  :=   t3     +     -2 ;
   if (z= 3   *    -1)  then 

skip  else while (¬true)     do 
  skip ' '{}'
}

@test "ec1967361a96" {
  check 'if (3    + z     =    0 +ny  ∨ y*  Ve    <1)    then 


 
x:= KQ  +y   else 

 y   :=  F-0    ;   while 1-   jq<M     *z   do if (false   ∧     true)  then 
 
m :=  x  *    z  else   y :=-4  ' '{x → 0}'
}

@test "d7800ecd747f" {
  check 'if (true∧u    + 2  <  y)   then 

 

while 2     +-3   <     -1     +z ∧   -1   *  0 =    4   +   y   do 
 x  :=y  -   p    else skip     ; while false    ∧   true   do  
 
 z  :=     3   -   Bm  ' '{}'
}

@test "ff42cc723e0a" {
  check 'while false    ∧     false      do 
skip;
 


while 3    =  3*    -2 ∨  false      do  

z    :=    (x    -   1)   - -1' '{}'
}

@test "cc42f69dc030" {
  check 'if (J - 2 =  2    ∧-4     *x<  0    *  -2)     then 
 
 while (¬(-3     *  -3=    z     +    K9))      do 

x    :=     -3 * -3      else 
   skip     ;




while x     -   y <     0    - 2 ∧x    - 3     =(z     -    y) *     x      do 
 

y     :=     -1   -     2 ' '{}'
}

@test "ab7369e831b5" {
  check 'while o    =3   +    2 ∨1   + 4=    (z  +     2)     -4      do 

  
y  :=     1  * y    ;if (true ∧     false)    then   
x    :=   z    *   x      else 
 z     :=   3+z    ' '{z → 3}'
}

@test "c7d4f2f4fbe2" {
  check 'while (¬(Z  -    z   =2     *x))   do  
y:=    4*     y     ' '{}'
}

@test "267af985daee" {
  check 'if (z   =    y     *  1  ∨   z    - 1    <     y     *    bw) then 
 

HS :=   3   + Z8    else 


 skip ;if (¬true)      then 


skip   else z  := -4  + -3 ;

 
if (y     +     2     <y +  z)     then 

while (¬(3   +  z    =  -1   +    n))     do 


x :=    z    *  -3     else 
while 2  +   -1   =    A *    1 do z     :=   x   *   S    ' '{HS → 3, z → -7}'
}

@test "098b7e021a1c" {
  check 'if (false∧    y     + -2     <     4     -  4) then skip else 

x  :=    1     ;    while 0*   f2     <x     -  x    ∧    true    do 

while false    ∨  true   do  skip    ' '{x → 1}'
}

@test "e0d83edc8e31" {
  check 'if (¬(4     *  4< y   +    y))  then 

   z :=-2     +   A  else  if (-3  +    y    <    y  +-4)      then 
  while (¬(d-3    <x  *x)) do skip    else  

 
y    :=z    -  4     ;   
 skip   ' '{z → -2}'
}

@test "25b9d247915a" {
  check 'if (true   ∧   z    *  -1 =    -2     -  -3)      then 
 skip else   J := z ;if (¬(-3    -  0<y+    -3))      then 

 x :=  -1  + y  else if (2     +   z    <     x     -   Ns   ∧ false)     then 
 
while -2  +   -1     =z+    z∧   -3 -    x     = -1  +0  do   

y:=     z   * x      else 

Z3    :=     x    +     1' '{J → 0, x → -1}'
}

@test "fb07a80aaa5e" {
  check 'while 3 -     4= 0    *  1 ∨  z  *   x  <  R2     * lf      do   

if false then  z := -2     +     -2   else  

  skip;Xl  :=    z     -  ef     ' '{Xl → 0}'
}

@test "b255c333a97c" {
  check 'if (x *     y=  z+    x    ∨   3  -  1   =0 -    y)   then 

 skip     else  
Bf :=    z  * z    ; 



while true     ∧   z    =     0  --3   do     skip     ' '{}'
}

@test "2e56f3a82ed7" {
  check 'if (z-     1    =x *     -4     ∨   -2   +  3 <    z    -  -3)     then  

 z :=   1-    y    else 


skip   ;if (¬false)   then 
 if (¬(y+  -4    <  z  -    f))     then skip   else skip   else   
 y :=   x+    x  ' '{z → 1}'
}

@test "b9f6b1e06af6" {
  check 'if (¬(3   *   -3<  x  -     (1     + 4)))      then 
   while y   +  -3  =    x    ∨  x*     z< 3    -   x  do 

 
skip else 
  

skip    ; 
 if (¬true)      then 
  x  := x  * y  else 
  

x     :=  -3   +3     ' '{x → 0}'
}

@test "c1bd74bd3d5b" {
  check 'if (true     ∨Id *    1     <  y   *     z)      then 
while false    ∨x+x <    2   *     -4      do 




J   :=     x     -   x  else  

 skip     ' '{}'
}

@test "0a82d9a89fc0" {
  check 'if (true∨   false)  then 

 skip   else skip  ;
if (false ∧   j-    1<  -2     *    z)     then 
   x  :=    -1 -y  else  while x   + y<E   *  wq  do skip  ' '{}'
}

@test "fbc6770e0337" {
  check 'if true      then  if (-1+  1    <  y+s    ∨     y  -2    =     gh+    -3)   then 


  if (x    *x     <    L+ 1   ∨   z <  z   -  -3)     then 

 x   :=    -2 + x    else 
 skip  else  

 
z   :=H+g1      else      while y   *   z    =z     -x ∨ 2  +  z     <   -2 +  -3      do 

skip   ;
 


while (¬(x     *   3    <x- -2))  do 

 skip    ' '{z → 0}'
}

@test "1f6ac3f91dec" {
  check 'if (¬(2  *    -4 =y *   -4))      then 
 y  :=-3     else 
 y   :=    yH     -   z   ' '{y → -3}'
}

@test "e9d2295ef588" {
  check 'if (¬(-2 +    2   = 1     +     3))    then 
 
  y    := 0 -    jv     else 
 if (¬false)   then  

skip     else  skip ;
while (¬true) do 
y    :=  -1   *2     ' '{y → 0}'
}

@test "aa523576162e" {
  check 'if (1 *  Hv     <   3 *     -1)      then 
skip      else  
skip     ; if (1  +  3  <   z-  -2    ∨   3    +4    =   x     +  z)   then   
 
if (y   +    1  <   1 *   -3∨    true)    then  
v    :=    -1  else 

 
oe :=0  *kl     else     y     :=H     +    2  ' '{y → 2}'
}

@test "9301881e0920" {
  check 'while (¬(-1    *  y  =y   - y))  do 
x  := 1     - -2     ;  
if (y     - z   =     -2     *   x ∧true)    then 
 z:=     -1    *   1  else 
if (¬true)   then    
z    :=     4   -     x     else skip   ' '{z → -1}'
}

@test "f7665e096673" {
  check 'if (¬(1    +     4  < -4 +  -4))  then y :=    y   +x else skip     ; 

z  :=     r    -  0     ' '{y → 0, z → 0}'
}

@test "cd27616047ec" {
  check 'if (true    ∧   false) then  z  :=   x   *   4      else skip ' '{}'
}

@test "9668783103da" {
  check 'while (¬true)   do 

x     :=  4  - z     ; 
 
if (false∨  y*  1  <     4    *    x)    then   z   :=  y*     z   else skip ' '{}'
}

@test "b1af2e3ef84c" {
  check 'if (x * -1     =     x     *  z∨  k +  -4  < 2)     then 

 
skip     else 

 

if (1   -     4   <4     +  4)   then 

  
while -3  <  z   - z   ∨  1  -  -2   =  0     -     Ka   do 
 p   :=    y  *    -1 else  skip  ;if (1     + 1     =   z+  -1∨    b    *     -2=    2)  then 
if (true     ∧true)   then 



skip  else  

  skip  else  if (y     +    y < 2  +   2    ∨ -1     -1=-1-  q)   then if (false ∧  false) then 
   skip      else U  :=    -1   -RT     *    y      else 
while (¬(z-    -4   =-1     -  -1))     do 
y    := -2  -x    ' '{U → -1}'
}

@test "32d7b8c08f40" {
  check 'if (¬(2-  2     =     -3   +     -4))    then y:=eN -2   else      skip  ;
 
if (4  -   tE  < -1-     -4   ∧    z   *   y   <   3  -   y)    then 
  skip else  

if (¬(y     *    1   =   o))  then x     :=     W2   -   vn     else 
 

 WT :=3   * y' '{x → 0, y → -2}'
}
