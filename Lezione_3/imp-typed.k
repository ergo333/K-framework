//Implementare Id++ e ++Id
module IMP-TYPED-SYNTAX

	syntax AExp ::= Int
				|	Id
				|	Id "++"	//Inserisco l'operatore ++
				| 	"++" Id
				|	"read" "(" ")"   	//ipotizziamo di leggere solo interi
				| 	AExp "*" AExp 		[seqstrict]
				|   AExp "/" AExp 		[seqstrict]
				>	AExp "+" AExp 		[seqstrict]
				|	AExp "-" AExp 		[seqstrict]
				|	AExp "(" AExp ")" 	[bracket]

	syntax BExp ::= Bool
				|   AExp ">" AExp 	[seqstrict]
				| 	AExp "<" AExp 	[seqstrict]
				| 	BExp "&&" BExp 	[seqstrict]
				| 	BExp "||" BExp 	[seqstrict]
				| 	"!" BExp 		[strict] 
				|	AExp "==" AExp [seqstrict]
				| "(" BExp ")" [bracket]

	//Sintassi del blocco -> non crea nessun ambiente nuovo 
	syntax Block ::= "{" "}"
				  |  "{" Stmt "}"

	syntax Stmt ::= "int" Id "=" AExp ";" [strict(2)]   //Sposta in cima alla configurazione il secondo parametro -> provare con il debug
				| 	Id "=" AExp ";"   	  [strict(2)]	//Assegnamento	
				|	"print" "(" AExp ")" ";" [strict(1)]
				|	Block
				|  	"if" "(" BExp ")" Block "else" Block [seqstrict] //valutiamo i blocchi e la guardia
				|   "while" "(" BExp ")" Block  		 [seqstrict]
				| 	Stmt Stmt							//Concatenazione

				|	"for" "(" Stmt BExp ";" Stmt ")" Block  //for (s e; e) -> se mettiamo come e finale solamente x++ (ad esempio)
endmodule

module IMP-TYPED
	imports IMP-TYPED-SYNTAX

	//aggiungiamo la semantica statica
	syntax IType ::= "int"
	syntax BType ::= "bool"
	syntax SType ::= "unit"	//tipo degli statement

	syntax Type ::= IType | BType | SType

	syntax AExp ::= IType
	syntax BExp ::= BType
	syntax Block ::= SType
	syntax Stmt ::= SType

	syntax KResult ::= Type

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

	//Dobbiamo aggiungere due regole, perchè interi e booleani non sono finali

	rule _:Int => int 	 //_ = indipendentemente da come si chiami
	rule _:Bool => bool

	//Tutte le espressioni scritte qui sotto vengono automaticamente messe nella configurazione k
	rule int + int => int	
	rule int * int => int
	rule int - int => int
	rule int / int => int

	rule int > int => bool	
	rule int < int => bool
	rule int == int => bool

	rule bool || bool => bool
	rule bool && bool => bool

	rule true && B:BExp => B
	rule false && _ => false

	rule true || _ => true
	rule false || B:BExp => B

	rule ! true => false
	rule ! false => true

	//Regole nuove
	//Ogni volta che eseguo uno statement, lo prendo e lo butto via
	//La mappa è la memoria in cui inseriamo il valore dell'espressione
	rule <k> ( int X:Id = int ; => unit ) ... </k> //unit -> lo statement va bene e viene tipato
											  // ... lasciano inalterato quello che c'è dopo
		<state> M:Map => M[X <- I] </state> 
		when notBool(X in keys(M))
		//Modifico la mappa M con la stessa mappa ma aggiung X sovrascritto da I
		//Questo lo faccio solo quando X non è già stata dichiarata! Non posso fare due ' int x = '
		//keys ritorna tutte le chiavi di M

	//Assegnamento
	
	rule <k> ( X:Id = int ; => unit) ... </k>  //Buttiamo via e modifichiamo lo stato
		<state> ... Z |-> int ... </state>	 //Non mi serve modificare lo state
		

	//Concatenazione di due stringhe
	rule S1:Stmt S2:Stmt => S1 ~> S2 [structural]

	//aggiungiamo la regola per lo statement
	rule unit ~> S:Stmt => S	

	rule <k> ( X:Id => int) ... </k>
		 <state> ... X |-> int ... </state>
		 //... = qualcosa prima / dopo
		 //Se trovo qualcosa lo rimpiazzo con I 
		 /*
		 	Immaginiamo la mappa:
				a |-> 3  ...
		 		x |-> 7  ...
		 		y |-> 9 <- ci troviamo qui (9 è di tipo Int)
		 		z |-> 2  ...
		 */

	rule { } => unit //non faccio nulla
	rule { S:Stmt } => S 
	rule if (bool) unit else unit => unit

	//Regole per la read e print
	rule <k> ( read() => int ) ... </k>
		 <in> ListItem(I:IType) => .List ... </in> 	//togliamo l'elemento e lo rimpiazziamo con lista vuota
		 //se al posto di IType mettiamo int non riesce a fare il match

	rule <k> print(int) ; => unit ... </k>
		 <out> ... .List => ListItem(int) </out>

	//while
	//Se la guardia è vera eseguo il blocco e rifaccio il while, altrimenti non faccio nulla
	rule while (bool) unit => unit

	//Aggiungo la regola dell'operatore ++ -> crea side effect: x++ + ++x -> dipende da come abbiamo implementato la somma
	rule <k> X:Id ++ => int ... </k>
		<state> ... X |-> int  ... </state>

	rule <k> ++ X:Id => int ... </k>
		<state>  ... X |-> int ... </state>

	//Aggiungo la regola del for
	rule for (Init:Stmt B:Bexp ; Step:Stmt) Bl:Block 
		=> Init while (B) { Bl Step }	[structural]

endmodule