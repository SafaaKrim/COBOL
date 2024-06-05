       IDENTIFICATION DIVISION.
       PROGRAM-ID. main-screen.
       AUTHOR. safaa.

      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Variable pour stocker le choix de l'utilisateur. 
       01  USER-CHOICE        PIC X(01).      
      *Variable pour gérer le retour après la sélection du menu. 
       01  SC-MENU-RETURN     PIC X(01).       
      ******************************************************************
       SCREEN SECTION.
      *Inclusion des définitions de l'écran depuis un fichier externe.
       COPY '33.cpy'.                          

       01 MAIN-MENU-SCREEN.
      *Nettoyage de l'écran avant l'affichage du menu. 
           05 BLANK SCREEN.                 
           05 LINE 14 COL 89 VALUE "1. Gestion des adherents"
               FOREGROUND-COLOR IS 2.         
           05 LINE 16 COL 89 VALUE "2. Gestion des cotisations"
               FOREGROUND-COLOR IS 2.         
           05 LINE 18 COL 89 VALUE "3. Gestion des remboursements"
               FOREGROUND-COLOR IS 2.        
           05 LINE 20 COL 89 VALUE "4. Gestion des prestations"
               FOREGROUND-COLOR IS 2.         
           05 LINE 22 COL 89 VALUE "5. Generation de rapports"
               FOREGROUND-COLOR IS 2.         
           05 LINE 24 COL 89 VALUE "Q. Quitter l'application"
               FOREGROUND-COLOR IS 2.        
           05 LINE 26 COL 89 VALUE "Veuillez choisir une option:"
               FOREGROUND-COLOR IS 2.        
      ******************************************************************
       PROCEDURE DIVISION.
       0000-MAIN-LOGIC.
      *---------------------------------------------------------------
      * Gestion principale du logic du menu. Affiche le menu principal,
      * accepte le choix de l'utilisateur et dirige vers la 
      *routine appropriée.
      *---------------------------------------------------------------
           DISPLAY MAIN-MENU-SCREEN.
           DISPLAY SCREEN-FRAME.
           ACCEPT USER-CHOICE.
            EVALUATE TRUE
                 WHEN USER-CHOICE = "1" 
                     PERFORM 1000-GESTION-ADHERENTS
                 WHEN USER-CHOICE = "2" 
                     PERFORM 2000-GESTION-COTISATIONS
                 WHEN USER-CHOICE = "3" 
                     PERFORM 3000-GESTION-REMBOURSEMENTS
                 WHEN USER-CHOICE = "4" 
                     PERFORM 4000-GESTION-PRESTATIONS
                 WHEN USER-CHOICE = "5" 
                     PERFORM 5000-GENERATE-REPORTS
                 WHEN USER-CHOICE = "Q" 
                     STOP RUN
                 WHEN OTHER    
                     DISPLAY "CHOIX INVALIDE, ESSAYEZ ENCORE."
            END-EVALUATE.
           EXIT.

      ******************************************************************
       1000-GESTION-ADHERENTS.
      *---------------------------------------------------------------
      * Gère les opérations liées aux adhérents.
      *---------------------------------------------------------------
           CONTINUE.
           EXIT.

      ******************************************************************
       2000-GESTION-COTISATIONS.
      *---------------------------------------------------------------
      * Gère les opérations liées aux cotisations.
      *---------------------------------------------------------------
           CONTINUE.
           EXIT.

      ******************************************************************
       3000-GESTION-REMBOURSEMENTS.
      *---------------------------------------------------------------
      * Gère les opérations liées aux remboursements.
      *---------------------------------------------------------------
           CONTINUE.
           EXIT.

      ******************************************************************
       4000-GESTION-PRESTATIONS.
      *---------------------------------------------------------------
      * Gère les opérations liées aux prestations.
      *---------------------------------------------------------------
           CONTINUE.
           EXIT.

      ******************************************************************
       5000-GENERATE-REPORTS.
      *---------------------------------------------------------------
      * Gère la génération de rapports.
      *---------------------------------------------------------------
           CONTINUE.
           EXIT.
