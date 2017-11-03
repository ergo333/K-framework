module EXP-TYPED-SYNTAX
	//NT =variabili

	//sintassi
	syntax  Exp ::= Int | Bool
				 | Exp "*" Exp [seqstrict] //valuta tutto left to right
				 | Exp "/" Exp [seqstrict]
				 > Exp "+" Exp [seqstrict]
				 | Exp "-" Exp [seqstrict]
				 > Exp "<" Exp [strict]
				 | Exp ">" Exp [strict]
				 | Exp "<=" Exp [strict]
				 | Exp ">=" Exp [strict]
				 | Exp "&&" Exp [strict]
				 | Exp "||" Exp [strict]
				 | "if" "(" Exp ")" Exp "else" Exp [strict]//valuto A CASO TUTTO
				 | Exp "?" Exp ":" Exp
				 | "(" Exp ")" [bracket] 
				 | Exp "<" Exp

endmodule


module EXP-TYPED

	imports EXP-TYPED-SYNTAX //sintassi statica

	//regole semantiche....
	syntax Type ::= "int" | "bool"
	syntax Exp ::= Type
	syntax KResult ::= Type

	rule I:Int => int
	rule B:Bool => bool

	rule int + int => int
	rule int * int => int
	rule int - Iint => int

	rule int / int => int

	rule if (bool) int else int => int
	rule if (bool) bool else bool => bool

	rule E1:Exp ? E2:Exp : E3:Exp => if (E1) E2 else E3 [structural] //rimangono invariate 

	rule E1:Exp < E2:Exp => bool
	rule E1:Exp <= E2:Exp => bool
	rule E1:Exp > E2:Exp => bool
	rule E1:Exp >= E2:Exp => bool


	//AND e OR 
	rule bool && bool => bool
	rule bool || bool => bool

endmodule 


