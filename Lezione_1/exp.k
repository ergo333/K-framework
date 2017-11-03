//Sintassi di Exp
module EXP-SYNTAX

	//La definiamo come una grammatica BNF
	syntax Exp ::= Int | Bool   			//Aggiungiamo i booleani
				|  Exp "*" Exp  [seqstrict] //Automaticamente valuta tutti i simboli non terminali fino ad un valore terminale
											//Così facendo scriviamo una sola regola semantica -> valuta tutto left to right
				|  Exp "/" Exp  [seqstrict]
				>  Exp "+" Exp  [seqstrict]
				|  Exp "-" Exp  [seqstrict]

				>  Exp "<" Exp 	[strict]
				|  Exp ">" Exp  [strict]
				|  Exp "<=" Exp [strict]
				|  Exp ">=" Exp [strict]
				|  Exp "&&" Exp 
				|  Exp "||" Exp 
				//strict(1) : valuto la guardia e poi guardo il resto -> valuto fino al valore finale e poi applico una regola
				|  "if" "(" Exp ")" Exp "else" Exp [strict(1)]
				|  "(" Exp ")"  [bracket] 	//Per parentesizzare le espressioni
										  	//Diamo al compilatore un'informazione che aiuta il compilatore 
										 	//a riconoscere che la nostra grammatica è parentesizzata

			 	|  Exp "?" Exp ":" Exp      //Operatore ternario

/* Il > al posto di | indica che il primo operatore ha maggior precedenza rispetto al secondo 
   Le parentesi servono solo per togliere ambiguità */

endmodule

/* Il modulo main che viene invocato è quello che ha lo stesso nome del file -> Exp*/

//Regole semantiche di Exp
module EXP
	imports EXP-SYNTAX   	//importo la sintassi di Exp

	syntax KResult ::= Int 	//produzione riservata di K dove si specifica quali sono i valori finali

	/* Regole semantiche */

	//metavariabile:tipoMetavariabile
	rule I1:Int + I2:Int => I1 +Int I2    
	//+Int è una funzione già definita in K che fa la somma di 2 int

	rule I1:Int * I2:Int => I1 *Int I2

	rule I1:Int - I2:Int => I1 -Int I2

	//Divisione solo se il secondo è diverso da 0
	rule I1:Int / I2:Int => I1 /Int I2 when I2 =/=Int 0

	//_ indica: indipendentemente da quello che c'è lì dentro (metavariabile)
	rule if (true) 	E:Exp 	else 	_     => E  //abbiamo messo strict(1) ed E non è stata valutata
	rule if (false) _    	else 	E:Exp => E
 	

 	/* regole logiche */

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