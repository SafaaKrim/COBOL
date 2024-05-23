      ****************************************************************

*     *Ce code COBOL est un programme simple qui affiche "Hello World" suivi 
      *d'un numéro de 1 à 10, puis affiche "FIN DE BOUCLE 1", puis répète le 
      *même processus dans une seconde boucle
      * ***********************************************************************
       
      
       
        IDENTIFICATION DIVISION.
        PROGRAM-ID. HelloWorld.

        ENVIRONMENT DIVISION.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-MESSAGE PIC X(20) VALUE "Hello World".
        01 WS-I       PIC 9(02) VALUE 1.


      *deux faction pour faire une boucle
      *1 er boucle
        PROCEDURE DIVISION.
           PERFORM UNTIL WS-I > 10
              DISPLAY    WS-MESSAGE
              DISPLAY    WS-I
              add 1 TO   ws-I
           END-PERFORM.
l
           DISPLAY 'FIN DE BOUCLE 1'.
      *2eme boucle 
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
              DISPLAY  WS-MESSAGE
              DISPLAY      WS-I
           END-PERFORM.

        STOP RUN.  
