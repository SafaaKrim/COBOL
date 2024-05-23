       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       DATA DIVISION.

       WORKING-STORAGE SECTION.
         01 WS-PRENOM   PIC X(5).
         01 WS-NOM      PIC X(4).
         01 WS-DATE-DE-NAISSANCE.
           05 WS-JOUR   PIC 9(2).
           05 WS-MOIS   PIC 9(2).
           05 WS-ANNEE  PIC 9(4).

       PROCEDURE DIVISION.
           MOVE "SAFAA" TO WS-PRENOM.
           MOVE "KRIM"  TO WS-NOM.
           MOVE 20      TO WS-JOUR.
           MOVE 05      TO WS-MOIS.
           MOVE 1994    TO WS-ANNEE.

       STOP RUN.


      