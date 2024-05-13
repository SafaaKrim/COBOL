      *********************************************************
      *    un programme simple permettant de : Additionner     *
      *     ,Soustraire, Diviser et Multiplication                * 
      *                                                        *
      **********************************************************
        
        IDENTIFICATION DIVISION. 
       PROGRAM-ID.calcul-test.
  
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Définition de les variableS

      * ZONE DES VARIABLES POUR CALCUL

        01 WS-Num1          PIC S9(5)V9(2).
        01 WS-Num2          PIC S9(5)V9(2).
        01 WS-Result        PIC S9(5)V9(2).

      *  
      * ZONE EDES VARIABLES POUR EDITION : -ZZZ :
      * - : si valeur positive alors signe remplace par espace
      *     si valeur négative alors signe négatif présent  
        01 WS-Num1ED        PIC -ZZZZ9.99.
        01 WS-Num2ED        PIC -ZZZZ9.99.
        01 WS-ResultED      PIC -ZZZZ9.99.

      *
        01 WS-OPERATION     PIC X(1).
           88 OPERATION-VRAI  VALUE 'A' 'S' 'D' 'M'.
           
        01 WS-Error-Message PIC X(50).
        01 WS-DECISION      PIC X(50).
       
       PROCEDURE DIVISION.
      *
           PERFORM INTRO-BIENVENUE
           THRU    INTRO-BIENVENUE-FIN.
      *     
           PERFORM CHOIX-OPER
           THRU    CHOIX-OPER-FIN.
      *
           PERFORM CHOIX-VALEUR1
           THRU    CHOIX-VALEUR1-FIN.

           PERFORM CHOIX-VALEUR2
           THRU    CHOIX-VALEUR2-FIN.

           PERFORM CALCUL-OPERATION
           THRU    CALCUL-OPERATION-FIN.
           
           PERFORM AFFICHAGE-RESULTAT
           THRU    AFFICHAGE-RESULTAT-FIN.

           PERFORM UNTIL FUNCTION UPPER-CASE (WS-DECISION) = 'OUI'
              PERFORM SUITE-CALCUL
              THRU    SUITE-CALCUL-FIN
           END-PERFORM.
           
           PERFORM ARRET-CALCULATRICE
           THRU    ARRET-CALCULATRICE.
           
           STOP RUN.
      *
      *****************************
       INTRO-BIENVENUE.
      *****************************
           DISPLAY 'BIENVENUE SUR LA CALCULATRICE !'.

       INTRO-BIENVENUE-FIN. EXIT.

      *****************************
       ARRET-CALCULATRICE.
      *****************************
           DISPLAY 'MERCI ET AU REVOIR !'.
       ARRET-CALCULATRICE-FIN. EXIT.
     
      *****************************
       CHOIX-OPER.    
      *****************************
      *       
           DISPLAY 'LISTE DES OPERATIONS POSSIBLES : '.
           DISPLAY "A : Addition".
           DISPLAY "S :Soustraction".
           DISPLAY "D : Division".
           DISPLAY "M : Multiplication".
           ACCEPT WS-OPERATION.
      
      *  SI OPERATEUR VALIDE, ON CONTINUE
      *  SI OPERATEUR NON VALIDE ON REVIENT AU DÉBUT DE LA SECTION

      * FONCTION UPPER-CASE : FORCE EN MAJUSCULE LA VALEUR SAISIE
           IF NOT OPERATION-VRAI
              DISPLAY 'OPERATEUR NON VALIDE : VEUILLEZ LE RESAISIR :'
              GO TO CHOIX-OPER
           END-IF
           .
       CHOIX-OPER-FIN. EXIT.
      *
      ****************************
       CHOIX-VALEUR1.
      ****************************
           DISPLAY "Entrez le premier nombre : "
           ACCEPT WS-Num1

           IF WS-Num1 IS NOT NUMERIC
              DISPLAY 'ERREUR : Num1 est non numérique ! '
              GO TO CHOIX-VALEUR1
           END-IF
      * On alimente la zone d'edition pour l'affichage   
          
           MOVE WS-Num1 TO WS-Num1ED.
       CHOIX-VALEUR1-FIN. EXIT.
      *
      *****************************
       CHOIX-VALEUR2.
      *****************************
           DISPLAY "Entrez le deuxième nombre : "
           ACCEPT WS-Num2
           IF WS-Num2 IS NOT NUMERIC
              DISPLAY 'ERREUR : Num2 est non numérique ! '
              GO TO CHOIX-VALEUR2
           END-IF.
      * On alimente la zone d'edition pour l'affichage 
           MOVE WS-Num2 TO WS-Num2ED.
       CHOIX-VALEUR2-FIN. EXIT.
      *
      *****************************
       CALCUL-OPERATION.
      *****************************
           EVALUATE FUNCTION UPPER-CASE (WS-OPERATION)
            WHEN 'A' 
               DISPLAY 'ADDITION DEMANDE : ' WS-Num1ED ' + ' WS-Num2ED
               COMPUTE WS-Result = WS-Num1 + WS-Num2
            WHEN 'S' 
            DISPLAY 'SOUSTRACTION DEMANDE : ' WS-Num1ED ' - ' WS-Num2ED
            COMPUTE WS-Result = WS-Num1 - WS-Num2
            WHEN 'D' 
      *Vérifier si le deuxième nombre est zéro pour la division
      * ON BOUCLE TANT QUE WS-Num2 est égal à 0
              PERFORM UNTIL WS-Num2 NOT = 0
               MOVE "Erreur: Division par zéro!" TO WS-Error-Message
               DISPLAY WS-Error-Message
               DISPLAY 'Entrer une autre valeur pour Num2 :'
               ACCEPT WS-Num2
               END-PERFORM   
               DISPLAY 'DIVISION DEMANDE :  'WS-Num1ED '/' WS-Num2ED
               COMPUTE WS-Result = WS-Num1 / WS-Num2
            WHEN 'M'
            DISPLAY 'MULTIPLICATION DEMANDE : ' WS-Num1ED '*' WS-Num2ED
            COMPUTE WS-Result = WS-Num1 * WS-Num2
           END-EVALUATE
           .
       CALCUL-OPERATION-FIN. EXIT.

      *
       AFFICHAGE-RESULTAT SECTION.

           MOVE WS-Result TO WS-ResultED
           DISPLAY "Résultat : " WS-ResultED.
           
       AFFICHAGE-RESULTAT-FIN. EXIT.

       SUITE-CALCUL.

           DISPLAY 'ARRET DE LA CALCULATRICE ? TAPEZ OUI ou NON :'
           ACCEPT WS-DECISION
           IF FUNCTION UPPER-CASE (WS-DECISION) NOT = 'OUI'
              MOVE WS-RESULT TO WS-Num1
                                WS-Num1ED
              PERFORM CHOIX-OPER
              THRU    CHOIX-OPER-FIN
              PERFORM CHOIX-VALEUR2
              THRU    CHOIX-VALEUR2-FIN
              PERFORM CALCUL-OPERATION
              THRU    CALCUL-OPERATION-FIN
              PERFORM AFFICHAGE-RESULTAT
              THRU    AFFICHAGE-RESULTAT-FIN
           END-IF
           .
       SUITE-CALCUL-FIN. EXIT.
           