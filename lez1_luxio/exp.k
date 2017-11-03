module EXP-SYNTAX
	//NT =variabili

	//sintassi
	syntax  Exp ::= Int | Bool //sintassi dinamica
				 | Exp "*" Exp [seqstrict] //valuta tutto left to right
				 | Exp "/" Exp [seqstrict]
				 > Exp "+" Exp [seqstrict]
				 | Exp "-" Exp [seqstrict]
				 > Exp "<" Exp [strict]
				 | Exp ">" Exp [strict]
				 | Exp "<=" Exp [strict]
				 | Exp ">=" Exp [strict]
				 | Exp "&&" Exp 
				 | Exp "||" Exp
				 | "if" "(" Exp ")" Exp "else" Exp [strict(1)]//valuto fino al risultato finale e poi applico una regola 
				 | Exp "?" Exp ":" Exp
				 | "(" Exp ")" [bracket] //tutti i simboli possono 
				 						//essere parenteizzati
				 						// lo indico con [bracket]
				
//si definisce precedenza uguale con | mentre con rpecendenza maggiore >

endmodule


module EXP

	imports EXP-SYNTAX

	//regole semantiche....
	syntax KResult ::= Int | Bool
	//produzione riservata di k che specifica i risultati finali
	rule I1:Int + I2:Int => I1 +Int I2//rule introduce  una regola semantica
	//	 |= metavariabile         |gia definito dentro k framework ch fa la somma tra due interi
	rule I1:Int * I2:Int => I1 *Int I2
	rule I1:Int - I2:Int => I1 -Int I2

	rule I1:Int / I2:Int => I1 -Int I2 when I2 =/=Int 0

	rule if (true) E1:Exp else E2:Exp => E1
	rule if (false) _ else E2:Exp => E2 //l'_ significa che non viene considerato cosa succede
					//se nell'if è vero l'else il true non vinee valutato
	//rule if (true) E:Exp else _ => E
	//rule if (false) _ else E:exp => E
	rule I1:Int > I2:Int => I1>Int I2
	rule I1:Int < I2:Int => I1<Int I2
	rule I1:Int >= I2:Int => I1>=Int I2
	rule I1:Int <= I2:Int => I1<=Int I2
	//uguale a
	 rule E1:Exp ? E2:Exp : E3:Exp => if (E1) E2 else E3 [structural] 
	 rule E1:Int < E2:Int => E1 < E2

	 //rule true < true => false
	 //rule false < true => true
	 //rule false > true => false
	 //rule false > false => false 


	 //AND e OR
	rule true && E:Exp => E
	rule false && _ => false

	rule true || _ =>true
	rule false || E:Exp => E

endmodule 


/*

(5+2+3)*(2+4+7) seqstrict valuta da sx a dx
(7+3....

(5+2+3)*(2+4+7)
(5+5)*(2+4+7)
.
.
. strict è randomico
*/