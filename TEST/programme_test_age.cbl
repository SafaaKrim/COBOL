       IDENTIFICATION DIVISION.
        PROGRAM-ID. Exercice.
        AUTHOR. SAFAA.

       DATA DIVISION. 
       WORKING-STORAGE SECTION.
       01  WS-MSG            PIC X(60).
       01  WS-COMPTEUR       PIC 9(02).
       01  WS-PRIX           PIC 9(06)V99.
       01  WS-INFO.
           05 WS-PRENOM      PIC X(15).
           05 WS-NOM         PIC X(15).
           05 WS-AGE         PIC 9(02).

       PROCEDURE DIVISION.
      **DONNER UNE VALEUR  A L'AGE 
       
           PERFORM VALEUR-AGE
           THRU FIN-VALEUR-AGE.


      ** TESTER C'EST L'AGE EST NUMERIQUE OU PAS 
           PERFORM TEST-NUM
           THRU FIN-TEST-NUM.
      ** TESTER C'EST LA PERSONE EST MINEUR OU MAJEUR 

           PERFORM TEST-AGE
           THRU FIN-TEST-AGE.

        STOP RUN.
          
           VALEUR-AGE SECTION.
           MOVE 10 TO WS-AGE.
           FIN-VALEUR-AGE. EXIT.


            TEST-NUM  SECTION.

            IF  WS-AGE IS NUMERIC
              DISPLAY 'WS-AGE EST BIEN NUMERIQUE'
           ELSE
             DISPLAY 'WS-AGE EST PAS NUMERIQUE'
            END-IF.

            FIN-TEST-NUM. EXIT.


           TEST-AGE SECTION.
            PERFORM VARYING WS-AGE FROM 10 BY 1 UNTIL WS-AGE >18
                IF WS-AGE > 18
                  DISPLAY 'LA PERSONNE EST MAJEUR : ' WS-AGE ' ans'
                ELSE
                  DISPLAY 'LA PERSONNE EST MINEURE : ' WS-AGE ' ans'
               END-IF
            END-PERFORM.
    
            FIN-TEST-AGE. EXIT.

