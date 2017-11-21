module IMP-SYNTAX

	syntax AExp ::= Int
				|	Id
				|	"read" "(" ")"   	//ipotizziamo di leggere solo interi
				| 	AExp "*" AExp 		[seqstrict]
				|   AExp "/" AExp 		[seqstrict]
				>	AExp "+" AExp 		[seqstrict]
				|	AExp "-" AExp 		[seqstrict]
				|	AExp "(" AExp ")" 	[bracket]

	syntax BExp ::= Bool
				|   AExp ">" AExp 	[seqstrict]
				| 	AExp "<" AExp 	[seqstrict]
				| 	BExp "&&" BExp 	[strict(1)]
				| 	BExp "||" BExp 	[strict(1)]
				| 	"!" BExp 		[strict] 

	//Sintassi del blocco -> non crea nessun ambiente nuovo 
	syntax Block ::= "{" "}"
				  |  "{" Stmt "}"

	syntax Stmt ::= "int" Id "=" AExp ";" [strict(2)]   //Sposta in cima alla configurazione il secondo parametro -> provare con il debug
				| 	Id "=" AExp ";"   	  [strict(2)]	//Assegnamento	
				|	"print" "(" AExp ")" ";" [strict]
				|	Block
				|  	"if" "(" BExp ")" Block "else" Block [strict(1)]
				|   "while" "(" BExp ")" Block  
				|	"for" "(" Stmt BExp ";" Stmt ")" Block  //for (s e; e) -> se mettiamo come e finale solamente x++ (ad esempio)
				| 	Stmt Stmt							//Concatenazione
endmodule

module IMP
	imports IMP-SYNTAX

	configuration 
	<T>
		<k> $PGM:Stmt </k>
		<env> .Map </env>		//Var -> Loc
		<store> .Map </store> 	//Loc -> Int
			//I / O 
		<in stream="stdin"> .List </in>  //inizializzato a lista vuota
		<out stream="stdout"> .List </out>
		<nextLoc> 0 </nextLoc>  //La prima locazione utile è 0, poi ad ogni successiva incrementa
	</T>

	syntax KResult ::= Int | Bool

	//Tutte le espressioni scritte qui sotto vengono automaticamente messe nella configurazione k
	rule I1:Int + I2:Int => I1 +Int I2	
	rule I1:Int * I2:Int => I1 *Int I2
	rule I1:Int - I2:Int => I1 -Int I2
	rule I1:Int / I2:Int => I1 /Int I2 when I2 =/=Int 0

	rule I1:Int > I2:Int => I1 >Int I2	
	rule I1:Int < I2:Int => I1 <Int I2

	rule true && B:BExp => B
	rule false && _ => false

	rule true || _ => true
	rule false || B:BExp => B

	rule ! true => false
	rule ! false => true

	//Regole nuove
	//Ogni volta che eseguo uno statement, lo prendo e lo butto via
	//La mappa è la memoria in cui inseriamo il valore dell'espressione
	rule <k> ( int X:Id = I:Int ; => .K ) ... </k> //.K sostituisce con il comando vuoto -> computazione vuota
											  // ... lasciano inalterato quello che c'è dopo
		<nextLoc> L:Int </nextLoc>
		when notBool(X in keys(M))
		//Modifico la mappa M con la stessa mappa ma aggiung X sovrascritto da I
		//Questo lo faccio solo quando X non è già stata dichiarata! Non posso fare due ' int x = '
		//keys ritorna tutte le chiavi di M

	//Assegnamento
	
	rule <k> ( X:Id = I:Int ; => .K) ... </k>  //Buttiamo via e modifichiamo lo stato
		 <state> M:Map => M[X <- I] </state>
		 when X in keys(M) 	//solo quando M è già stata dichiarata altrove	

	//Concatenazione di due stringhe
	rule S1:Stmt S2:Stmt => S1 ~> S2 [structural]
	//Dobbiamo prima valutare S1 e S2 -> non c'è un calcolo da fare
	//C'è solo una riscrittura -> REGOLA STRUTTURALE
	/*
		S1 : x = 1;
		S2 : y = 3; z = 5;

		In cima alla computazione x = 1 ~> S2 
		Poi S2 diventa: y = 3 ~> z = 5

		Non cambia lo stato. Non c'è una computazione, è solo una riscrittura
	*/		

	rule <k> ( X:Id => I) ... </k>
		 <state> ... X |-> I:Int ... </state>
		 //... = qualcosa prima / dopo
		 //Se trovo qualcosa lo rimpiazzo con I 
		 /*
		 	Immaginiamo la mappa:
				a |-> 3  ...
		 		x |-> 7  ...
		 		y |-> 9 <- ci troviamo qui (9 è di tipo Int)
		 		z |-> 2  ...
		 */

	rule { } => .K //non faccio nulla
	rule { S:Stmt } => S [structural]
	rule if (true) B:Block else _ => B
	rule if (false) _ else B:Block => B

	//Regole per la read e print
	rule <k> read() => I ... </k>
		 <in> ListItem(I:Int) => .List ... </in> 	//togliamo l'elemento e lo rimpiazziamo con lista vuota

	rule <k> print(I:Int) ; => .K ... </k>
		 <out> ... .List => ListItem(I) </out>

	//while
	//Se la guardia è vera eseguo il blocco e rifaccio il while, altrimenti non faccio nulla
	rule while (B:Bexp) Bl:Block => if (B) { Bl while (B) } else { }

	rule for (Init:Stmt B:Bexp ; Step:Stmt) Bl:Block 
		=> Init while (B) { Bl Step }	[structural]
endmodule

