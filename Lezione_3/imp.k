//Implementare Id++ e ++Id
module IMP-SYNTAX

	syntax AExp ::= Int
				|	Id
				|	Id "++"	//Inserisco l'operatore ++
				| 	"++" Id
				|	"read" "(" ")"   	//ipotizziamo di leggere solo interi
				| 	AExp "*" AExp 		[seqstrict]
				|   AExp "/" AExp 		[seqstrict]
				>	AExp "+" AExp 		[seqstrict]
				|	AExp "-" AExp 		[seqstrict]
				>	Id "=" AExp 		[strict(2)]  //Aggiungiamo la parte del side effect  
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

	syntax IDS ::= List{Id, ","}  //Lista di Id dove il separatore è ','

	syntax Stmt ::= //int" Id "=" AExp ";" [strict(2)]   //Sposta in cima alla configurazione il secondo parametro -> provare con il debug
				//| 	Id "=" AExp ";"   	  [strict(2)]	//Assegnamento	

				| 	"int" Ids "=" AExp ";" [strict(2)]
				|	"print" "(" AExp ")" ";" [strict]
				|	Block
				|  	"if" "(" BExp ")" Block "else" Block [strict(1)]
				|   "while" "(" BExp ")" Block  
				| 	Stmt Stmt							//Concatenazione
				| 	AExp ";"		[strict]	//Valuto AExp e poi verranno applicate le altre regole
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

	rule _:Int ; => .K 		//Se trovo un int seguito da ; lo butto via -> es 5 ;  ==> .K

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
	//Fai qualcosa con il primo id e ricorsivamente passa agli altri
	rule <k> int ( X:Id , Xs:Ids => Xs ) = I:Int ; ... </k> //.K sostituisce con il comando vuoto -> computazione vuota
											  // ... lasciano inalterato quello che c'è dopo
		<env> Env:Map => Env[X <- L] </env>
		<store> Store:Map => Store[L <- I] </store>
		<nextLoc> L:int => L +Int 1</nextLoc>
		//Modifico la mappa M con la stessa mappa ma aggiung X sovrascritto da I
		//Questo lo faccio solo quando X non è già stata dichiarata! Non posso fare due ' int x = '
		//keys ritorna tutte le chiavi di M

	rule <k> int .Ids = I:Int ; => .K ... </k>  //Caso base dell'assegnamento .Ids indica la lista vuota di identificatori e butto via tutto 

	//Assegnamento
	/*
	rule <k> ( X:Id = I:Int ; => .K) ... </k>  //Buttiamo via e modifichiamo lo stato
		<env> ... X |-> L ... </store>
		<store> ... L |-> ( _ => I) ... </store>
	*/

	//Assegnamento dopo la modifica con il side effect y = (x=5) + (x=7)
	rule <k> ( X:Id = I:Int ; => I) ... </k>  //Buttiamo via e modifichiamo lo stato
		<env> ... X |-> L ... </store>
		<store> ... L |-> ( _ => I) ... </store>

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
		<env> ... X |-> L ... </env>
		<store> ... L |-> I:Int ... </store>
		 //... = qualcosa prima / dopo
		 //Se trovo qualcosa lo rimpiazzo con I 
		 /*
		 	Immaginiamo la mappa:
				a |-> 3  ...
		 		x |-> 7  ...
		 		y |-> 9 <- ci troviamo qui (9 è di tipo Int)
		 		z |-> 2  ...
		 */

	syntax K ::= Map //Posso usare map in k ma non fa parte della sintassi del linguaggio
	//Il programmatore non può scrivere X |-> ... 

	rule { } => .K //non faccio nulla
	rule <k>{ S:Stmt } => S ~> Env ... </k> //Esegui S, dopodichè Env. Ma Env non è parte della sintassi -> vedi 2 righe sopra
		 <env> Env:Map </env>

	//Se trovo l'Env in Map 
	rule <k> Env:Map => .K ... </k>   //devo ripristinare il vecchio env indipendentemente da quello che c'è
		 <env> _ => Env </env>
		 /*                           x->1  (Env) ; 1 -> 0 (Store)
		 int x = 0;								
		 {
			int x = 7;				  x->2; 1->0, 2->7
			x = x+1;				              2->8
		 }
		 print (x);					  ripristino il vecchio ambiente che mi ero salvato e butto via l'altro  x->1; 1->0  e tutto torna
		 */

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


	//Aggiungo la regola dell'operatore ++ -> crea side effect: x++ + ++x -> dipende da come abbiamo implementato la somma
	rule <k> X:Id ++ => I ... </k>
		 <env> ... X |-> L ... </env>
		 <store> ... L |-> ( I:Int => I +Int 1 ) ... </store>

	rule <k> ++ X:Id => I +Int 1 ... </k>
		 <env> ... X |-> L ... </env>
		 <store> ... L |-> ( I:Int => I +Int 1 ) ... </store>

endmodule

