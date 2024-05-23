      ********************************************************************
      *     Programme COBOL pour la gestion des fichiers d'assurance     *
      *                     et la génération d'un rapport.               *
      *                                                                  *    
      ********************************************************************
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. assur.
       AUTHOR. K.SAFAA.
         
      * Cette section définit les enregistrements de données 
      * pour les fichiers d'entrée et de sortie.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * Déclaration des fichiers d'entrée et de sortie. 
           SELECT ASSUR-PART1-FILE ASSIGN TO 'assurances-part1.dat'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS  IS WS-STATUS-FICHIER.

           SELECT ASSUR-PART2-FILE ASSIGN TO 'assurances-part2.dat'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS  IS WS-STATUS-FICHIER2.

           SELECT RAPPORT-ASSURANCES-FILE 
           ASSIGN TO 'rapport-assurances.dat'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS  IS WS-STATUS-FIC-SORTIE.


       DATA DIVISION.
      * Cette section définit les enregistrements de données 
      *pour les fichiers d'entrée et de sortie.
       FILE SECTION.
       FD ASSUR-PART1-FILE
           RECORD CONTAINS 121 CHARACTERS
           DATA RECORD     IS WS-ASSUR-PART1-RECORD.
       01 WS-ASSUR-PART1-RECORD.
               05 WS-ID        PIC X(8).
               05 FILLER       PIC X(1).
               05 WS-NOM       PIC X(14).
               05 FILLER       PIC X(1).
               05 WS-IRP       PIC X(14).
               05 FILLER       PIC X(1).
               05 WS-NOM-ASS   PIC X(41).
               05 FILLER       PIC X(1).
               05 WS-STATUT    PIC X(8).
               05 FILLER       PIC X(1).
               05 WS-Date1     PIC X(8).
               05 FILLER       PIC X(1).
               05 WS-date2     PIC X(8).
               05 FILLER       PIC X(1).
               05 WS-PRIX      PIC X(9).
               05 FILLER       PIC X(1).
               05 WS-DEVISE    PIC X(3).

       FD ASSUR-PART2-FILE
           RECORD CONTAINS 121  CHARACTERS
           DATA RECORD     IS WS-ASSUR-PART2-RECORD.
       01 WS-ASSUR-PART2-RECORD.
               05 WS-ID           PIC X(8).
               05 FILLER       PIC X(1).
               05 WS-NOM       PIC X(14).
               05 FILLER       PIC X(1).
               05 WS-IRP       PIC X(14).
               05 FILLER       PIC X(1).
               05 WS-NOM-ASS   PIC X(41).
               05 FILLER       PIC X(1).
               05 WS-STATUT    PIC X(8).
               05 FILLER       PIC X(1).
               05 WS-Date1     PIC X(8).
               05 FILLER       PIC X(1).
               05 WS-date2     PIC X(8).
               05 FILLER       PIC X(1).
               05 WS-PRIX      PIC X(9).
               05 FILLER       PIC X(1).
               05 WS-DEVISE    PIC X(3).   
            
       FD RAPPORT-ASSURANCES-FILE
      * Définition des champs pour le fichier ASSUR-PART2. 
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD     IS RAPPORT-ASSURANCES-RECORD.
       01  RAPPORT-ASSURANCES-RECORD.
      * Définition des champs pour le rapport d'assurance. 
         05 WS-ENTETE.
            10 WS-LIG1-ETOILE  PIC X(57) VALUE SPACES.
            10 WS-TITRE-ENTETE PIC X(17) VALUE 'LISTING ASSURANCE'.
            10 WS-LIG2-ETOILE  PIC X(58) VALUE SPACES.
        
         05 WS-TITRE-SECTION REDEFINES WS-ENTETE.
            10 WS-LIG1-ETOILE PIC X(52) VALUE SPACES.
            10 WS-TITRE-SECTION.
               15 WS-SECTION    PIC X(19) VALUE 'SECTION POUR STATUT'.
               15 FILLER        PIC X(1) VALUE SPACE.
               15 WS-NOM-STATUT PIC X(8).
            10 WS-LIG2-ETOILE PIC X(52) VALUE SPACES.

         05 WS-CORPS REDEFINES WS-ENTETE.
            10 WS-CORPS-DETAILS.
               15 WS-ID        PIC X(8).
               15 FILLER       PIC X(1).
               15 WS-NOM       PIC X(14).
               15 FILLER       PIC X(1).
               15 WS-IRP       PIC X(14).
               15 FILLER       PIC X(1).
               15 WS-NOM-ASS   PIC X(41).
               15 FILLER       PIC X(1).
               15 WS-STATUT    PIC X(8).
               15 FILLER       PIC X(1).
               15 WS-Date1     PIC X(8).
               15 FILLER       PIC X(1).
               15 WS-date2     PIC X(8).
               15 FILLER       PIC X(1).
               15 WS-PRIX      PIC X(9).
               15 FILLER       PIC X(1).
               15 WS-DEVISE    PIC X(3).
               15 FILLER       PIC X(11).

         05 WS-ENPIED REDEFINES WS-ENTETE.
           10 WS-LIG1-ETOILE    PIC X(24) VALUE SPACES.
           10 WS-NBRE-STATUT-ACTIF.
             15 WS-LIBELLE-ACTIF PIC X(20) VALUE 'NBRE STATUT ACTIF : '.
             15 WS-NBRE-ACTIF    PIC 9(3).
           10 WS-LIG3-ETOILE PIC X(5) VALUE SPACES.
           10 WS-NBRE-STATUT-SUSPENDU.
            15 WS-LIBELLE-SUS PIC X(23) VALUE 'NBRE STATUT SUSPENDU : '.
            15 WS-NBRE-SUS    PIC 9(3).
           10 WS-LIG3-ETOILE PIC X(5) VALUE SPACES.
           10 WS-NBRE-STATUT-ACTIF.
             15 WS-LIBELLE-RES PIC X(22) VALUE 'NBRE STATUT RESILIE : '.
             15 WS-NBRE-RESILIE  PIC 9(3).
           10 WS-LIG2-ETOILE     PIC X(24) VALUE SPACES.

        WORKING-STORAGE SECTION.
      * Cette section contient des variables de contrôle 
      * et des indicateurs de statut.

       01 WS-STATUS-FICHIER         PIC 9(2) VALUE ZERO.
           88 STATUT-FICHIER-OK     VALUE 00.
           88 STATUT-FICHIER-FIN    VALUE 10.
       01 WS-STATUS-FICHIER2        PIC 9(2) VALUE ZERO.
           88 STATUT-FICHIER2-OK    VALUE 00.
           88 STATUT-FICHIER2-FIN   VALUE 10.    

       01 WS-REC-COUNT         PIC 9(4) VALUE 0.
       01 WS-REC2-COUNT        PIC 9(4) VALUE 0.
       01 WS-ECRITURE          PIC 9(4) VALUE 0.
       01 WS-ECRITURE2         PIC 9(4) VALUE 0.
       
       01 WS-I PIC 9(3) VALUE 0.
       01 WS-J PIC 9(3) VALUE 0.

       01 WS-STATUS-FIC-SORTIE PIC 9(2) VALUE ZERO.
           88 STATUT-SORTIE-OK           VALUE 00.
           88 STATUT-SORTIE-FIN          VALUE 10.

       01 WS-TABLEAU-FICHIER OCCURS 72.
          05 WS-ID        PIC X(8).
          05 FILLER       PIC X(1).
          05 WS-NOM       PIC X(14).
          05 FILLER       PIC X(1).
          05 WS-IRP       PIC X(14).
          05 FILLER       PIC X(1).
          05 WS-NOM-ASS   PIC X(41).
          05 FILLER       PIC X(1).
          05 WS-STATUT-TABLE    PIC X(8).
          05 FILLER       PIC X(1).
          05 WS-Date1     PIC X(8).
          05 FILLER       PIC X(1).
          05 WS-date2     PIC X(8).
          05 FILLER       PIC X(1).
          05 WS-PRIX      PIC X(9).
          05 FILLER       PIC X(1).
          05 WS-DEVISE    PIC X(3).   

       01 WS-NBRE-ACTIF-STK PIC 9(3).
       01 WS-NBRE-RESILIE-STK PIC 9(3).
       01 WS-NBRE-SUS-STK PIC 9(3).
       01 WS-NUM-SECTION-ENC PIC 9(3).

      * Programme principal pour le traitement des fichiers. 
       PROCEDURE DIVISION.

      * ouverture du ficher       
           PERFORM OUVERTURE-FICHIER
           THRU    OUVERTURE-FICHIER-FIN.

           PERFORM LECTURE-FICHIER
           THRU    LECTURE-FICHIER-FIN

           PERFORM UNTIL STATUT-FICHIER-FIN
              ADD 1 TO WS-I
              MOVE WS-ASSUR-PART1-RECORD TO WS-TABLEAU-FICHIER (WS-I)
              PERFORM LECTURE-FICHIER
              THRU    LECTURE-FICHIER-FIN
           END-PERFORM
      * Programme principal pour le traitement des fichiers.
           PERFORM LECTURE-FICHIER2
           THRU    LECTURE-FICHIER2-FIN

           PERFORM UNTIL STATUT-FICHIER2-FIN
               ADD 1 TO WS-I
               PERFORM LECTURE-FICHIER2
               THRU    LECTURE-FICHIER2-FIN
               MOVE WS-ASSUR-PART2-RECORD TO WS-TABLEAU-FICHIER (WS-I)
           END-PERFORM
           
           SORT WS-TABLEAU-FICHIER 
           ASCENDING KEY WS-STATUT-TABLE OF WS-TABLEAU-FICHIER.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 72
           DISPLAY WS-STATUT-TABLE (WS-I)
           DISPLAY WS-TABLEAU-FICHIER (WS-I)
           END-PERFORM
      * Procédure pour traiter le tableau des enregistrements. 
           PERFORM TRAITER-TABLEAU 
           THRU    TRAITER-TABLEAU-FIN.

      *la fermeture du fichier
           PERFORM FERMETURE-FICHIER
           THRU    FERMETURE-FICHIER-FIN.

      *fin du fichier 
           PERFORM COMPTE-RENDU
           THRU    COMPTE-RENDU-FIN.


           STOP RUN.
      **********************************************
      * Procédure pour ouvrir les fichiers
       
      **********************************************
       OUVERTURE-FICHIER.
           OPEN INPUT ASSUR-PART1-FILE.
           IF NOT STATUT-FICHIER-OK
               DISPLAY 'ERREUR OUVERTURE FICHIER ENTRANT'
               DISPLAY 'CODE ERREUR : ' WS-STATUS-FICHIER
               STOP RUN
           END-IF.

           OPEN INPUT ASSUR-PART2-FILE.
           IF NOT STATUT-FICHIER2-OK
               DISPLAY 'ERREUR OUVERTURE FICHIER ENTRANT'
               DISPLAY 'CODE ERREUR : ' WS-STATUS-FICHIER2
               STOP RUN
           END-IF.

           OPEN OUTPUT RAPPORT-ASSURANCES-FILE.
           IF NOT STATUT-SORTIE-OK
               DISPLAY 'ERREUR OUVERTURE FICHIER SORTIE'
               DISPLAY 'CODE ERREUR : ' WS-STATUS-FIC-SORTIE
               STOP RUN
           END-IF.

       OUVERTURE-FICHIER-FIN. EXIT.

       LECTURE-FICHIER. 
           READ ASSUR-PART1-FILE.

           IF WS-REC-COUNT = 0 AND STATUT-FICHIER-FIN
              DISPLAY 'FICHIER VIDE !' WS-STATUS-FICHIER
              STOP RUN
           END-IF.
           
           IF NOT STATUT-FICHIER-OK AND NOT STATUT-FICHIER-FIN
              DISPLAY 'ERREUR LECTURE FICHIER'
              DISPLAY 'CODE ERREUR : ' WS-STATUS-FICHIER
              STOP RUN
           END-IF.

              IF STATUT-FICHIER-FIN
                 DISPLAY 'FIN DE FICHIER ATTEINTE'   
              ELSE
                 ADD 1 TO WS-REC-COUNT
              END-IF.
       LECTURE-FICHIER-FIN. EXIT.


       LECTURE-FICHIER2. 
           READ ASSUR-PART2-FILE.

           IF WS-REC2-COUNT = 0 AND STATUT-FICHIER2-FIN
              DISPLAY 'FICHIER 2 VIDE !' WS-STATUS-FICHIER2
              STOP RUN
           END-IF.
           
           IF NOT STATUT-FICHIER2-OK AND NOT STATUT-FICHIER2-FIN
              DISPLAY 'ERREUR LECTURE FICHIER 2'
              DISPLAY 'CODE ERREUR : ' WS-STATUS-FICHIER2
              STOP RUN
           END-IF.

              IF STATUT-FICHIER2-FIN
                 DISPLAY 'FIN DE FICHIER ATTEINTE'   
              ELSE
                 ADD 1 TO WS-REC2-COUNT
              END-IF.

       LECTURE-FICHIER2-FIN. EXIT.
      **************************************************
      *Procédure pour lire les enregistrements du fichier

      *******************************************
       
       TRAITER-TABLEAU.
      * ECRITURE EN-TETE GLOBAL
           INITIALIZE RAPPORT-ASSURANCES-RECORD .
           MOVE ALL '*' TO WS-LIG1-ETOILE OF WS-ENTETE.
           MOVE 'LISTING ASSURANCE' TO WS-TITRE-ENTETE.
           MOVE ALL '*' TO WS-LIG2-ETOILE OF WS-ENTETE.
           PERFORM ECRITURE-SORTIE
           THRU    ECRITURE-SORTIE-FIN
      * INITALISATION DU CURSEUR DU TABLEAU
           MOVE 1 TO WS-J.
           MOVE 1 TO WS-NUM-SECTION-ENC.
      * ECRITURE DU 1ER TITRE DE SECTION
           PERFORM GESTION-TITRE-SECTION
           THRU    GESTION-TITRE-SECTION-FIN.

           INITIALIZE RAPPORT-ASSURANCES-RECORD.

      * BOUCLE CORPS ET TITRE DE SECTION

           PERFORM UNTIL WS-J > WS-I
            MOVE WS-TABLEAU-FICHIER (WS-J) TO WS-CORPS
            PERFORM ECRITURE-SORTIE
            THRU    ECRITURE-SORTIE-FIN
            IF (WS-STATUT-TABLE (WS-J + 1) NOT = WS-STATUT-TABLE (WS-J)
                AND (WS-J + 1 < WS-I))
                IF WS-NUM-SECTION-ENC = 1
                  MOVE WS-J TO WS-NBRE-ACTIF-STK
               ELSE
                  SUBTRACT WS-NBRE-ACTIF-STK FROM WS-J 
                                             GIVING WS-NBRE-RESILIE-STK
               END-IF
               ADD 1 TO WS-NUM-SECTION-ENC
               ADD 1 TO WS-J
               PERFORM GESTION-TITRE-SECTION
               INITIALIZE RAPPORT-ASSURANCES-RECORD
            ELSE
               ADD 1 TO WS-J
            END-IF

           END-PERFORM.

           SUBTRACT WS-NBRE-ACTIF-STK FROM WS-J 
                                           GIVING WS-NBRE-SUS-STK.
           SUBTRACT WS-NBRE-RESILIE-STK FROM WS-NBRE-SUS-STK 
                                           GIVING WS-NBRE-SUS-STK.   
      * ECRITURE EN-PIED

           INITIALIZE RAPPORT-ASSURANCES-RECORD.
           MOVE ALL '*' TO WS-LIG1-ETOILE OF WS-ENPIED.
           MOVE 'NBRE STATUT ACTIF : '    TO WS-LIBELLE-ACTIF.
           MOVE 'NBRE STATUT SUSPENDU : ' TO WS-LIBELLE-SUS.
           MOVE 'NBRE STATUT RESILIE : '  TO WS-LIBELLE-RES.
           MOVE ALL '*' TO WS-LIG2-ETOILE OF WS-ENPIED.
           MOVE WS-NBRE-ACTIF-STK TO WS-NBRE-ACTIF.
           MOVE WS-NBRE-RESILIE-STK TO WS-NBRE-RESILIE.
           MOVE WS-NBRE-SUS-STK TO WS-NBRE-SUS.
           PERFORM ECRITURE-SORTIE
           THRU    ECRITURE-SORTIE-FIN.

       TRAITER-TABLEAU-FIN. EXIT.

       GESTION-TITRE-SECTION.

           INITIALIZE RAPPORT-ASSURANCES-RECORD.
           MOVE ALL '*' TO WS-LIG1-ETOILE OF WS-TITRE-SECTION.
           MOVE ALL '*' TO WS-LIG2-ETOILE OF WS-TITRE-SECTION.
           MOVE 'SECTION POUR STATUT' TO WS-SECTION.
           MOVE WS-STATUT-TABLE (WS-J) TO WS-NOM-STATUT.
           PERFORM ECRITURE-SORTIE
           THRU    ECRITURE-SORTIE-FIN.

       GESTION-TITRE-SECTION-FIN. EXIT.

       ECRITURE-SORTIE.

           WRITE RAPPORT-ASSURANCES-RECORD.

       ECRITURE-SORTIE-FIN. EXIT.

      ******************************************
      * Procédure pour fermer les fichiers
      *******************************************    
       FERMETURE-FICHIER.

           CLOSE ASSUR-PART1-FILE.
           CLOSE ASSUR-PART2-FILE.
           CLOSE RAPPORT-ASSURANCES-FILE.

       FERMETURE-FICHIER-FIN. EXIT.

       COMPTE-RENDU.

           DISPLAY "FIN DE TRAITEMENT DES ENREGISTREMENTS.".
           DISPLAY "NOMBRE D'ENREGISTREMENT LUS FICHIER 1 : "
                   WS-REC-COUNT.
           DISPLAY "NOMBRE D'ENREGISTREMENT LUS FICHIER 2 : "
                   WS-REC2-COUNT.
       COMPTE-RENDU-FIN. EXIT.
