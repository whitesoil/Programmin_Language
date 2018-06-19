1. lambda 과제
- let, ifnil, cons, hd, tl 구현 완료.
- lambda의 경우는 파싱까지만 구현이 되어 있습니다.
- Edit Configuration에 세팅 필요
- Edit Configuration에 Input값을 넣어주어야 한다.
- 모든 Input은 "" 로 감싸져야 한다.

Ex1) "\"abc\" :: nil"
Ex2) "hd (\"abc\" :: nil)"
Ex3) "ifnil nil then hd (\"abc\" :: nil) else \"def\""
Ex4) "let val x = \"abc\" in x :: nil end"
Ex5) "let val x = \"abc\" :: nil in x :: x end"
Ex6) "let val x = \"abc\" :: nil in let val y = nil in x :: y end end"
Ex7) "(lambda (list) let val rest = tl (list) in hd (rest) end)"
Ex8) "let val second = (lambda(list)let val rest = tl (list) in hd (rest) end) in second(tl(\"a\"::\"b\"::\"c\"::nil)) end"
 
