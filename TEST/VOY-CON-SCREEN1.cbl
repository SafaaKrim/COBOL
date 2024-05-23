       IDENTIFICATION DIVISION.
        PROGRAM-ID. VOY-CON-SCREEN1.
        ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.


       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Prendre en compte le masque négatif
       77 SOLDE PIC S9(5)V99.
       77 SOLDEZ PIC -Z(5).99.

        PROCEDURE DIVISION.
        PAR.
            DISPLAY "ENCODEZ LE SOLDE - MAX 5 CHIFFRES".
            DISPLAY "---------------".
      * ACCEPT SOLDE.
            COMPUTE SOLDE = 5 + 6.
      * MOVE -123 TO SOLDE.
            MOVE SOLDE TO SOLDEZ.
            IF SOLDE > 0
               THEN DISPLAY SOLDEZ AT LINE 10 WITH FOREGROUND-COLOR IS 1
                       BACKGROUND-COLOR IS 7
            ELSE
               DISPLAY SOLDEZ AT LINE 10 WITH FOREGROUND-COLOR IS 7
                       BACKGROUND-COLOR IS 4

            END-IF.
            
           STOP RUN.