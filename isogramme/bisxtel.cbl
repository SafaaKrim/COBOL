        IDENTIFICATION DIVISION.
       PROGRAM-ID. bissext.          

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Variable pour stocker l'année saisie par l'utilisateur
       01  WS-YEAR   PIC 9(4).  
      *  -- Variable pour stocker le message de résultat
       01  WS-RESULT PIC X(49).        
      *-- Variable pour contrôler la boucle principale
       01  WS-CONTINUE PIC X(03) VALUE "Y". 
           88 WS-YES VALUE "Y".        
           88 WS-NO  VALUE "N".         

       PROCEDURE DIVISION.
      *Boucle principale jusqu'à ce que 
      *l'utilisateur choisisse de quitter 
           PERFORM UNTIL WS-NO      
      
               DISPLAY "Saisir une annee : " SPACE WITH NO ADVANCING
      *-- Saisir l'année depuis l'utilisateur         
               ACCEPT WS-YEAR
      *- Vérifier si l'année est divisible par 4           
                 IF FUNCTION MOD(WS-YEAR, 4) EQUAL ZERO
               IF FUNCTION MOD(WS-YEAR, 100) EQUAL ZERO
                   IF FUNCTION MOD(WS-YEAR, 400) EQUAL ZERO
                       MOVE "Oui, c'est une annee bissextile" 
                       TO WS-RESULT
                   ELSE
                       MOVE "Non, ce n'est pas une annee bissextile" 
                       TO WS-RESULT
                   END-IF
               ELSE
                   MOVE "Oui, c'est une annee bissextile" 
                   TO WS-RESULT
               END-IF
           ELSE
               MOVE "Non, ce n'est pas une annee bissextile" 
               TO WS-RESULT
           END-IF
       
           DISPLAY WS-RESULT  

               DISPLAY "Continuer (Y/N) ?" SPACE WITH NO ADVANCING
      *  -- Demander à l'utilisateur de continuer ou de quitter         
               ACCEPT WS-CONTINUE     
               DISPLAY SPACE    
           END-PERFORM.
           STOP RUN.
