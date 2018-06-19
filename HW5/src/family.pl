/* sisterOf(X, Y) 는 Y 가 여성이고, X와 Y의 부모가 같고, X != Y 이면
Y는 X의 자매임을 정의한다. */
sisterOf(X,Y) :-
    female(Y),
    parents(Y,M,W),
    parents(X,M,W),
    X \= Y.

/* 가족 1: aM 와 bF 는 ab1F, ab2F, ab3M 의 부모
   가족 2: cM 와 dF 는 cd1F와 cd2M의 부모
   가족 3: ab3M 와 cd1F 는 abcd1M 와 abcd2F의 부모
   가족 4: ab1F 와 cd2M 는 cdab1M의 부모 */
/* 논리식 male(PERSON) 은 PERSON은 남성이라는 뜻
   논리식 female(PERSON) 은 PERSON은 여성이라는 뜻 */
male(aM).
male(ab3M).
male(cM).
male(cd2M).
male(abcd1M).
male(cdab1M).
female(bF).
female(dF).
female(ab1F).
female(ab2F).
female(cd1F).
female(abcd2F).
/* 논리식 parents(CHILD, FATHER, MOTHER)는 CHILD의 아버지는 FATHER이고, 어머니는 MOTHER 라는 뜻 */
parents(ab1F, aM, bF).
parents(ab2F, aM, bF).
parents(ab3M, aM, bF).
parents(cd1F, cM, dF).
parents(cd2M, cM, dF).
parents(abcd1M, ab3M, cd1F).
parents(abcd2F, ab3M, cd1F).
parents(cdab1M, cd2M, ab1F).

/* uncleOf(P, U) : U는 P의 삼촌이다. */
uncleOf(P,U) :-
    male(U),
    sisterOf(U,Who),
    parents(P,_,Who).

/* auntOf(P, A) : A는 P의 이모(또는 고모)이다. */
auntOf(P,A) :-
    female(A),
    sisterOf(Who,A),
    parents(P,Who,_).

/* cousinOf(P, Q) : Q는 P의 사촌이다. */
cousinOf(P,Q) :-    
    parents(Q,Z,R),
    uncleOf(P,Z),    
    auntOf(P,R).    