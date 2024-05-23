      *********************************************************
      *    un programme simple permettant de : Additionner     *
      *  ,Soustraire,Diviser,Multiplication            
      *                                                        *
      **********************************************************
        
        IDENTIFICATION DIVISION. 
       PROGRAM-ID. Calculator.
  
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Définition de les variableS 
        01 Num1          PIC 9(5)V99.
        01 Num2          PIC 9(5)V99.
        01 Operation     PIC X(1).
        01 Result        PIC 9(3)V99.
        01 Error-Message PIC X(50).
      *Afficher une invite pour choisir une opération
       PROCEDURE DIVISION.
           DISPLAY "Choisissez une opération : "
           DISPLAY "+. Addition"
           DISPLAY "-. Soustraction"
           DISPLAY "/. Division"
           DISPLAY "*. Multiplication"
      *Accepter l'entrée de l'utilisateur pour le choix de l'opération
        
           ACCEPT Operation

           DISPLAY "Entrez le premier nombre : "
         ACCEPT Num1

          DISPLAY "Entrez le deuxième nombre : "
         ACCEPT Num2

         EVALUATE Operation
        WHEN '+' 
            COMPUTE Result = Num1 + Num2
        WHEN '-' 
            COMPUTE Result = Num1 - Num2
        WHEN '/' 
      *Vérifier si le deuxième nombre est zéro pour la division 
            IF Num2 = 0
                MOVE "Erreur: Division par zéro!" TO Error-Message
                DISPLAY Error-Message
            ELSE
                COMPUTE Result = Num1 / Num2
              END-IF
            WHEN '*' 
                COMPUTE Result = Num1 * Num2
            WHEN OTHER
                DISPLAY "Opération invalide"
            END-EVALUATE

               IF Operation NOT EQUAL '/'
               DISPLAY "Résultat : " Result
           END-IF

        STOP RUN.
