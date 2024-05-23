      ******************************************************************
      *    Calculatrice basique ne repondant pas au brief
      *================================================================= 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. calc.
       AUTHOR. Yves.
      ******************************************************************
       DATA DIVISION.
      *****************************************************************
       WORKING-STORAGE SECTION.
       01  WS-NUM1        PIC 9(3).
       01  WS-NUM2        PIC 9(3).
       01  WS-OPE         PIC X(3).
       01  WS-RESULT      PIC 9(5).
       01  WS-TEMP-RESULT PIC 9(5).
       01  WS-SHOW        PIC Z(10)9.

       PROCEDURE DIVISION.
           DISPLAY " " NO ADVANCING
              ACCEPT WS-NUM1.
           DISPLAY WS-NUM1 
              SPACE NO ADVANCING
              ACCEPT WS-OPE.
           DISPLAY WS-NUM1 
              SPACE WS-OPE 
              SPACE NO ADVANCING 
              ACCEPT WS-NUM2.       
           EVALUATE WS-OPE
              WHEN "+" 
                 ADD WS-NUM1 WS-NUM2 TO WS-TEMP-RESULT
              WHEN "-"
                 MOVE WS-NUM1 TO WS-TEMP-RESULT 
                 SUBTRACT WS-NUM2 FROM WS-TEMP-RESULT
              WHEN "*"
                 MULTIPLY WS-NUM1 BY WS-NUM2 GIVING WS-TEMP-RESULT
              WHEN "/"
                 DIVIDE WS-NUM1 BY WS-NUM2 GIVING WS-TEMP-RESULT
              WHEN OTHER
                 DISPLAY "Opérateur inconnu"
           END-EVALUATE.
     

           DISPLAY WS-NUM1 
              SPACE WS-OPE
              SPACE WS-NUM2
              SPACE "=" WS-TEMP-RESULT.
       
       STOP RUN.
