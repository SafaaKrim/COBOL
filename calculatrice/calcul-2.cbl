       IDENTIFICATION DIVISION.
       PROGRAM-ID. Calculatrice.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Nombre1   PIC 9(5)V99.
       01 Nombre2   PIC 9(5)V99.
       01 Resultat  PIC 9(5)V99.
       01 Operation PIC X.
       01 Continuer PIC X VALUE 'O'.  
       01 Choix     PIC X.

         PROCEDURE DIVISION.

           DISPLAY "Calculatrice".
      *Afficher une invite pour choisir une opération

           DISPLAY "Choisissez une opération : "
           DISPLAY "+. Addition"
           DISPLAY "-. Soustraction"
           DISPLAY "/. Division"
           DISPLAY "*. Multiplication"
    
           DISPLAY "Entrez le premier nombre: "
           ACCEPT Nombre1
        
           DISPLAY "Choisissez l'opération à effectuer (+, -, *, /, ^): "
           ACCEPT Operation
        
           IF Operation = '^'
            DISPLAY "Entrez le deuxième nombre: "
            ACCEPT Nombre2
           ELSE
            DISPLAY "Entrez un entier numérique de 3 chiffres: "
            ACCEPT Nombre2
           END-IF
        
         PERFORM CALCULER-OPERATION
        
           DISPLAY "Voulez-vous continuer (O/N) ? "
           ACCEPT Choix
             IF Choix = 'N'
            MOVE 'N' TO Continuer
            END-IF
    
       STOP RUN.

         CALCULER-OPERATION.


        IF Operation = '+'
             COMPUTE Resultat = Nombre1 + Nombre2
        ELSE IF Operation = '-'
             COMPUTE Resultat = Nombre1 - Nombre2
        ELSE IF Operation = '*'
              COMPUTE Resultat = Nombre1 * Nombre2
        ELSE IF Operation = '/'
          IF Nombre2 NOT = 0
            COMPUTE Resultat = Nombre1 / Nombre2
          ELSE
            DISPLAY "Erreur: Division par zéro!"
        END-IF
       ELSE IF Operation = '^'
           COMPUTE Resultat = FUNCTION POWER(Nombre1, Nombre2)
       END-IF
            DISPLAY "Résultat: ", Resultat.

