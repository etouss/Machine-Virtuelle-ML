Projet de MV6 -- annee 2015
===========================

Adequin - Renaud - 21001718
Toussaint - Etienne - 21001624


CONTENU
=======

A priori l'ensemble des fonctionnalité ont été remplies.

  #Problemes Rencontrés:

* Le probleme majeur que nous avons rencontré concerne l'expression LEQ ne voulant
pas ajouter d'instruction à la machine virtuelle pour cela, nous avons tenté de 
le calculer grace a une suite d'instruction cependant elle presente pluisieur faille:
-> la division par 0
-> les chiffre négatif

Edit : Probleme Corrigé avec l'algo : 
if(a=b) then Const(1)
  else if(b=0) then (a+1)/a
  else if(a/b) then (a+1)/a
  else (b+1)/b

On exploite ici le fait que (comme en C) notre machine interprete 0 comme false et tout autre int comme true
Et on implémente une suite d'instruction implémentant l'algo precédant.

* Le 2nd probleme concerne le Lexer/Parser nous regrettons le fait de ne pas avoir était capable
de mieux comprendre les fichier proposé pour ajouter des syntaxe pour les tableau.

* Enfin et finalement nous n'avons pas pris le temps de réaliser une analyse de la liste d'instruction 
génerer pour réaliser une initialisation dynamique de la machine 
---> taille de la pile variable
---> taille du tas variable 
---> optimisation de l'appelle au rammase miette
Ainsi la machine pourrait être plus rapide d'une part 
et certain programme ne pourront pas être executer si la taille de la pile n'est pas suffisante ou la taille du tas.


  #Choix d'implémentation:

* Tout d'abord nous avons fait le choix d'implémenter les closure en aval du code de la fonction
différament au fichier codex fournis.
* Le tag est codé sur un char (en raison de l'assamble/dessasemble en i8 / out8)
* Pour des raison de simplicité d'implémentation plus que d'optimisation les block et closure dont de type
===>BlockTableau(tag*mot array) plutot que BlockTableau(tag*mot list)

  #Extenstion et ajout:

* Pour implémenter le débugueur nous avons simplement ajouter un read_line sur l'entrée standart
===> Pour l'utiliser il faut simplement changer eval en eval_d dans les fonction

* Pour impémenter les fonction récursive nous avons fait le choix de préparer un environement "faux"
ou la liste d'association et érroné au moment de l'instruction closure. 
De facon a pouvoir avoir une liste vrai et juste juste aprés grace au push: 
Ainsi la pile aprés une creation de fonction est : f;x;e1;e2;e3...  ou f la fonction x l'argument et e1 ... les variable d'environement.
Donc f est récuperable au sein meme de la fonctio donc recursif.

* Pour implémenter les tableau: Nous avons simplement mis a profit l'instruction MakeBlock 
Cependant la difficulter a résider dans l'incrémentation de l'environement pendant la compilation des différent
element du tableau suivit d'un push.
-> En plus des tableau nous avons implémenter divers instruction lié:
::::::Proj qui permet de récuper l'element situé en ieme position ou i et de type INT donc pas variable
      Utilisation simple de GetBlock i
::::::ProjVar qui permet de recuperer de facon "dynamique" situé en postion x 
      Ici on a du creer un nouvelle instruction GetBlockStack qui va chercher l'element n situé dans le BlockTableau de l'accumulateur
      Ou n est le MotInt contenu dans le premiere element de la pile.
::::::Cat a était modifié de facon a pouvoir concatener tout type de mot avec a un tableau déja existant
::::::Del qui permet de supprimer le dernier element ajouter a un Tableau
NB : Les tableau implémenter ne sont pas typer et peuvent contenir a la foi des MotInt et des Pointeurs.

* Pour implémenter l'optimisation des appels terminaux nous avons d'une part du creer un nouelle instruction
AppTerm n :: Qui permet a la pile de rester intact entre chaque appelle recursif elle prend un parametre n
car elle doit se comporter a la maniere d'un return et pop n variable (bien sur le PC ne subit pas les meme modif qu'avec un return)
(En réalité la pile n'est pas tout a fait intact puisque le parametre d'appelle de f x a changé en y et divers effet de bort interne a la fonction.
Cependant la postion dans la pile de chacun des élément celon leur role n'a pas changé)
----> Pour déterminer si une fonction est terminal nous avons tenter d'analyser statiquement l'arbre syntaxique de la fonction
et appeller le cas échéant App ou AppTerm a la compilation.
(Les appelle a Binop(App) doivent être les Binop les plus haut dans l'arbre syntaxique)

* Pour implémenter le tas :
Nous nous sommes inspiré du l'implementation du tas de la machine virtuelle OCaml décrit en cours et dans le projet
Ainsi les Mot de la machine sont soit des MotInt représentant les int soit des Pointeur contenant un indice vers un élement du tas
Le tas étant un tableau de Block les Blocks étant de 3 type différents
        ->BlockString   contenant une string
        ->BlockTableau  contenant les tableau closure et paire
        ->Null simplement pour initialiser le avec une valeur "absurde"
Pour implémenter le rammasse miette nous avons tenter d'utiliser un algorytme inspiré de 
l'union-find : en effet aprés avoir constater que les seul référence accésible par le logiciel était
celle de la pile et de l'accumulateur nous en déduisons la composante connexe des references "encors atteignable"
et nous supprimons les autres.
Malheuresement nous n'avons pas pris le temps d'implémenter un union-find intéligent basé sur les modification effectuer
a la pile et à l'accumulateur ainsi nous effecteur un calcul complet de la composante connexe a chaque appel
du rammasse - miette
Les appel du rammasse miette sont effectuer quand le tas est plein.
Il n'y as pas nous plus de réindexation du tas car cela impliquer trop de travail sur les pointeur déja creer.
On a fait le choix de creer une liste contenant les élément libre.

* Pour implémenter la machine virtuelle myrthe:
Nous avons creer une machine virtuelle myrthe coder  directement sous forme d'arbre syntaxique
car nous n'avons pas été a meme de comprendre le lexer parser suffisament pour ajouter la syntax des tableau
Ainsi ---> nous commencer par convertir la suite d'instruction myrthe en couple de Int (a,b)
          ou a = l'instruction et b = l'argument si il existe 0 sinon
      ---> ensuite la fonction machine est une fonction recusive terminal de parametre
          [pc,acc,sh,stack] et en variable d'environement instr et nb_instr
      En effet n'ayant pas implémenter d'instruction pour calcluer la longeur d'un tableau nous avons du ajouter
      en parametre le nombre d'instruction
      stack et instr sont implémenter sous forme de tableau.
      ---> le Code de la machine n'est pas optimiser pour permettre de garder en visibilité en effet
      nous commencer par poser des variable instr acc pc sh et stack ce qui genere de nombreuse instruction inutile qui ralentise
      l'execution du code.

Nous n'avons pas implementer de compilateur myrthe ce dernier faisant l'objet du TP1 et TP2 
cependant nous avons recopier un désambleur myrthe pour facilter les test.
Ce désambleur a supprimer l'instruction Andi car il n'y a que 8 place disponible dans l'octet de description d'instruction
et nous n'avons pas pris le temps de le modifier.


Ici, dites ce que vous avez fait: les fonctions remplies, les
problemes rencontres, les choix d'implementation.


