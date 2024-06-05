       IDENTIFICATION DIVISION.
       PROGRAM-ID. main-screen.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
     
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 USER-CHOICE PIC X(01).
       01 SC-MENU-RETURN PIC X(01).

       SCREEN SECTION.

       COPY '33.cpy'.
       
       01 MAIN-MENU-SCREEN.
           05 BLANK SCREEN.
           05 LINE 14  COL 89 VALUE "1. Gestion des adherents"
           FOREGROUND-COLOR IS 2.
           05 LINE 16  COL 89 VALUE "2. Gestion des cotisations"
           FOREGROUND-COLOR IS 2.
           05 LINE 18  COL 89 VALUE "3. Gestion des remboursements"
           FOREGROUND-COLOR IS 2.
           05 LINE 20  COL 89 VALUE "4. Gestion des prestations"
           FOREGROUND-COLOR IS 2.
           05 LINE 22  COL 89 VALUE "5. Generation de rapports"
           FOREGROUND-COLOR IS 2.
           05 LINE 24  COL 89 VALUE "Q. Quitter l'application"
           FOREGROUND-COLOR IS 2.
           05 LINE 26  COL 89 VALUE "Veuillez choisir une option:"
           FOREGROUND-COLOR IS 2.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY MAIN-MENU-SCREEN.
           DISPLAY SCREEN-FRAME.
           ACCEPT USER-CHOICE.
            EVALUATE TRUE
                 WHEN USER-CHOICE = "1" PERFORM GESTION-ADHERENTS
                 WHEN USER-CHOICE = "2" PERFORM GESTION-COTISATIONS
                 WHEN USER-CHOICE = "3" PERFORM GESTION-REMBOURSEMENTS
                 WHEN USER-CHOICE = "4" PERFORM GESTION-PRESTATIONS
                 WHEN USER-CHOICE = "5" PERFORM GENERATE-REPORTS
                 WHEN USER-CHOICE = "Q" 
            
              STOP RUN
                 WHEN OTHER    DISPLAY "CHOIX INVALIDE, ESSAYEZ ENCORE."
            END-EVALUATE.
       
       GESTION-ADHERENTS.
      * Sous-routine pour la gestion des adhérents
           CONTINUE.
       
       GESTION-COTISATIONS.
      * Sous-routine pour la gestion des cotisations
           CONTINUE.
       
       GESTION-REMBOURSEMENTS.
      * Sous-routine pour la gestion des remboursements
           CONTINUE.
       
       GESTION-PRESTATIONS.
      * Sous-routine pour la gestion des prestations
           CONTINUE.
       
       GENERATE-REPORTS.
      * Sous-routine pour la génération des rapports
           CONTINUE.

