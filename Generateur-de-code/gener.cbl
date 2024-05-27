       IDENTIFICATION DIVISION.
       PROGRAM-ID. Gener.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FICHIER-GEN ASSIGN TO 'generated_program.cbl'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  FICHIER-GEN.
       01  ENREG-GEN PIC X(80).
       
       WORKING-STORAGE SECTION.
       01  PRINT-LINE.
           05 SPACE-06   PIC X(06) VALUE ALL SPACES.
           05 SPACE-07   PIC X(07) VALUE ALL SPACES.
           05 SPACE-11   PIC X(11) VALUE ALL SPACES.
           05 SPACE-14   PIC X(14) VALUE ALL SPACES.
           05 SPACE-17   PIC X(17) VALUE ALL SPACES.
           05 SPACE-20   PIC X(20) VALUE ALL SPACES.
           05 STARS-66   PIC X(66) VALUE ALL "*".
           05 BLANK-72   PIC X(72).
       
       01  CHOIX-PS.
           05 CHOIX-PS1      PIC 9 VALUE 0.
           05 CHOIX-PS2      PIC 9 VALUE 0.
           05 CHOIX-PS3      PIC 9 VALUE 0.
           05 CHOIX-PS4      PIC 9 VALUE 0.
       01  CHOIX-SGBD.
           05 CHOIX-SGBD1      PIC 9 VALUE 0.
           05 CHOIX-SGBD2      PIC 9 VALUE 0.
           05 CHOIX-SGBD3      PIC 9 VALUE 0.
           05 CHOIX-SGBD4      PIC 9 VALUE 0.
       01  CHOIX-SOUS.
           05 CHOIX-SOUS1      PIC 9 VALUE 0.
           05 CHOIX-SOUS2      PIC 9 VALUE 0.
       01  CHOIX-WS.
           05 CHOIX-WS1      PIC 9 VALUE 0.
           05 CHOIX-WS2      PIC 9 VALUE 0.
       01  CHOIX-PROC.
           05 CHOIX-PROC1      PIC 9 VALUE 0.
           05 CHOIX-PROC2      PIC 9 VALUE 0.
       
       SCREEN SECTION.
       01 ECRAN-PRINCIPAL.
           05 LINE 1 COL 1 
           VALUE "Generateur d'application COBOL" FOREGROUND-COLOR 1.
           05 LINE 3 COL 1 
           VALUE "Partie PS :" FOREGROUND-COLOR 3.
           05 LINE 4 COL 3 
           VALUE "1 - Lire un fichier sequentiel" FOREGROUND-COLOR 5.
           05 LINE 5 COL 3 
           VALUE "2 - Ecrire un fichier sequentiel" FOREGROUND-COLOR 5.
           05 LINE 6 COL 3 
           VALUE "3 - Trier un fichier" FOREGROUND-COLOR 5.
           05 LINE 7 COL 3 
           VALUE "4 - Fusionner plusieurs fichiers" FOREGROUND-COLOR 5.
           05 LINE 8 COL 1 VALUE "Choix :" FOREGROUND-COLOR 5.
           05 LINE 8 COL 8 PIC  9 TO CHOIX-PS1 FOREGROUND-COLOR 5.
           05 LINE 8 COL 9 VALUE "|" FOREGROUND-COLOR 2.
           05 LINE 8 COL 10 PIC  9 TO CHOIX-PS2 FOREGROUND-COLOR 5.
           05 LINE 8 COL 11 VALUE "|" FOREGROUND-COLOR 2.
           05 LINE 8 COL 12 PIC 9 TO CHOIX-PS3 FOREGROUND-COLOR 5.
           05 LINE 8 COL 13 VALUE "|" FOREGROUND-COLOR 2.
           05 LINE 8 COL 14 PIC 9 TO CHOIX-PS4 FOREGROUND-COLOR 5.
       
           05 LINE 10 COL 1 VALUE "Partie SGBD :"FOREGROUND-COLOR 3.
           05 LINE 11 COL 3
           VALUE "1 - Acceder a une base de donnee designee"
           FOREGROUND-COLOR 5.
           05 LINE 12 COL 3 
           VALUE "2 - Generer une requete SELECT COUNT(*)"
           FOREGROUND-COLOR 5.
           05 LINE 13 COL 3 VALUE "3 - Generer un curseur"
           FOREGROUND-COLOR 5.
           05 LINE 14 COL 3 VALUE "4 - Generer une requete UPDATE"
           FOREGROUND-COLOR 5.
           05 LINE 15 COL 1 VALUE "Choix :" FOREGROUND-COLOR 5.
           05 LINE 15 COL 8 PIC 9 TO CHOIX-SGBD1 FOREGROUND-COLOR 5.
           05 LINE 15 COL 9 VALUE "|" FOREGROUND-COLOR 2.
           05 LINE 15 COL 10 PIC 9 TO CHOIX-SGBD2 FOREGROUND-COLOR 5.
           05 LINE 15 COL 11 VALUE "|" FOREGROUND-COLOR 2.
           05 LINE 15 COL 12 PIC 9 TO CHOIX-SGBD3 FOREGROUND-COLOR 5.
           05 LINE 15 COL 13 VALUE "|" FOREGROUND-COLOR 2.
           05 LINE 15 COL 14 PIC 9 TO CHOIX-SGBD4 FOREGROUND-COLOR 5.
       
           05 LINE 17 COL 1 VALUE "Partie Sous-routines :"
           FOREGROUND-COLOR 3.
           05 LINE 18 COL 3 VALUE "1 - Creer une sous-routine"
           FOREGROUND-COLOR 5.
           05 LINE 19 COL 3 
           VALUE "2 - Integrer un appel type de sous-routine"
           FOREGROUND-COLOR 5.
           05 LINE 20 COL 1 VALUE "Choix :"FOREGROUND-COLOR 5.
           05 LINE 20 COL 8 PIC 9 TO CHOIX-SOUS1 FOREGROUND-COLOR 5.
           05 LINE 20 COL 9 VALUE "|" FOREGROUND-COLOR 2.
           05 LINE 20 COL 10 PIC 9 TO CHOIX-SOUS2 FOREGROUND-COLOR 5.
       
           05 LINE 22 COL 1 VALUE "Partie WS :" FOREGROUND-COLOR 3.
           05 LINE 23 COL 3 
           VALUE "1 - Preparer un template de Copybook"
           FOREGROUND-COLOR 5.
           05 LINE 24 COL 3 VALUE "2 - Integrer un Copybook"
           FOREGROUND-COLOR 5.
           05 LINE 25 COL 1 VALUE "Choix :" FOREGROUND-COLOR 5.
           05 LINE 25 COL 8 PIC 9 TO CHOIX-WS1 FOREGROUND-COLOR 5.
           05 LINE 25 COL 9 VALUE "|" FOREGROUND-COLOR 2.
           05 LINE 25 COL 10 PIC 9 TO CHOIX-WS2 FOREGROUND-COLOR 5.
       
           05 LINE 27 COL 1 VALUE "Partie Procedure :"
           FOREGROUND-COLOR 5.
           05 LINE 28 COL 3 VALUE "1 - Ajouter un HelloCobol"
           FOREGROUND-COLOR 5.
           05 LINE 29 COL 3 
           VALUE "2 - Integrer des paragraphes type"
           FOREGROUND-COLOR 5.
           05 LINE 30 COL 1 VALUE "Choix :"FOREGROUND-COLOR 5.
           05 LINE 30 COL 8 PIC 9 TO CHOIX-PROC1 FOREGROUND-COLOR 5.
           05 LINE 30 COL 9 VALUE "|" FOREGROUND-COLOR 2.
           05 LINE 30 COL 10 PIC 9 TO CHOIX-PROC2 FOREGROUND-COLOR 5.
       
       PROCEDURE DIVISION.
           DISPLAY ECRAN-PRINCIPAL.
           ACCEPT ECRAN-PRINCIPAL.
       
           PERFORM GENERER-APPLICATION.
           STOP RUN.
       
       GENERER-APPLICATION.
           IF CHOIX-PS1 = 1
               PERFORM GENERER-LIRE-FICHIER
           END-IF.
           IF CHOIX-PS2 = 2
               PERFORM GENERER-ECRIRE-FICHIER
           END-IF.
           IF CHOIX-PS3 = 3
               PERFORM GENERER-TRIER-FICHIER
           END-IF .       
           IF CHOIX-PS4 = 4
               PERFORM GENERER-FUSIONNER-FICHIERS
           END-IF.
           IF CHOIX-SGBD1 = 1
               PERFORM GENERER-ACCEDER-DB
           END-IF.       
           IF CHOIX-SGBD2 = 2
               PERFORM GENERER-SELECT-COUNT
           END-IF.    
           IF CHOIX-SGBD3 = 3
               PERFORM GENERER-CURSEUR
           END-IF.   
           IF CHOIX-SGBD4 = 4
               PERFORM GENERER-UPDATE-DB
           END-IF.
           IF CHOIX-SOUS1 = 1
               PERFORM GENERER-SOUSROUTINE
           END-IF. 
           IF CHOIX-SOUS2 = 2
               PERFORM GENERER-APPEL-SOUSROUTINE
           END-IF.
           IF CHOIX-WS1 = 1
               PERFORM GENERER-COPYBOOK
           END-IF.   
           IF CHOIX-WS2 = 2
               PERFORM GENERER-INTEGRER-COPYBOOK
           END-IF.
           IF CHOIX-PROC1 = 1
               PERFORM GENERER-HELLOCobol
           END-IF.    
           IF CHOIX-PROC2 = 2
               PERFORM GENERER-PARAGRAPHES-TYPE
           END-IF.
       
       GENERER-LIRE-FICHIER.
           OPEN OUTPUT FICHIER-GEN
           MOVE "       IDENTIFICATION DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       PROGRAM-ID. LireFichierSeq." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       ENVIRONMENT DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       INPUT-OUTPUT SECTION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       FILE-CONTROL." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           SELECT IN-FILE ASSIGN TO 'inputfile.txt'." 
           TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           ORGANIZATION IS LINE SEQUENTIAL." 
           TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       DATA DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       FILE SECTION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       FD  IN-FILE." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       01  IN-RECORD PIC X(80)." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       WORKING-STORAGE SECTION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       01  WS-RECORD PIC X(80)." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       PROCEDURE DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           OPEN INPUT IN-FILE." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           PERFORM UNTIL EOF" TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "               READ IN-FILE INTO WS-RECORD" 
           TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "               AT END MOVE 'YES' TO EOF" TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "               NOT AT END DISPLAY WS-RECORD"
            TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           END-READ" TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           END-PERFORM." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           CLOSE IN-FILE." TO ENREG-GEN
           WRITE ENREG-GEN
           CLOSE FICHIER-GEN.
       
       GENERER-ECRIRE-FICHIER.
           OPEN OUTPUT FICHIER-GEN
           MOVE "       IDENTIFICATION DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       PROGRAM-ID. EcrireFichierSeq." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       ENVIRONMENT DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       INPUT-OUTPUT SECTION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       FILE-CONTROL." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           SELECT OUT-FILE ASSIGN TO 'outputfile.txt'." 
           TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           ORGANIZATION IS LINE SEQUENTIAL." 
           TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       DATA DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       FILE SECTION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       FD  OUT-FILE." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       01  OUT-RECORD PIC X(80)." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       WORKING-STORAGE SECTION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       01  WS-RECORD PIC X(80) VALUE 'EXAMPLE DATA'." 
           TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       PROCEDURE DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           OPEN OUTPUT OUT-FILE." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           MOVE WS-RECORD TO OUT-RECORD." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           WRITE OUT-RECORD." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           CLOSE OUT-FILE." TO ENREG-GEN
           WRITE ENREG-GEN
           CLOSE FICHIER-GEN.
       
       GENERER-TRIER-FICHIER.
           OPEN OUTPUT FICHIER-GEN
           MOVE "       IDENTIFICATION DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       PROGRAM-ID. TrierFichier." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       ENVIRONMENT DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       INPUT-OUTPUT SECTION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       FILE-CONTROL." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "          SELECT IN-FILE ASSIGN TO 'unsortedfile.txt'." 
           TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           SELECT OUT-FILE ASSIGN TO 'sortedfile.txt'."
            TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           ORGANIZATION IS LINE SEQUENTIAL." 
           TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       DATA DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       FILE SECTION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       FD  IN-FILE." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       01  IN-RECORD PIC X(80)." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       FD  OUT-FILE." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       01  OUT-RECORD PIC X(80)." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       WORKING-STORAGE SECTION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       01  WS-RECORD PIC X(80)." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       PROCEDURE DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           SORT IN-FILE ON ASCENDING KEY IN-RECORD" 
           TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           GIVING OUT-FILE." TO ENREG-GEN
           WRITE ENREG-GEN
           CLOSE FICHIER-GEN.
       
       GENERER-FUSIONNER-FICHIERS.
           OPEN OUTPUT FICHIER-GEN
           MOVE "       IDENTIFICATION DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       PROGRAM-ID. FusionnerFichiers." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       ENVIRONMENT DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       INPUT-OUTPUT SECTION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       FILE-CONTROL." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           SELECT IN-FILE1 ASSIGN TO 'file1.txt'." 
           TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           SELECT IN-FILE2 ASSIGN TO 'file2.txt'." 
           TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           SELECT OUT-FILE ASSIGN TO 'mergedfile.txt'." 
           TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           ORGANIZATION IS LINE SEQUENTIAL." 
           TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       DATA DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       FILE SECTION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       FD  IN-FILE1." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       01  IN-RECORD1 PIC X(80)." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       FD  IN-FILE2." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       01  IN-RECORD2 PIC X(80)." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       FD  OUT-FILE." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       01  OUT-RECORD PIC X(80)." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       WORKING-STORAGE SECTION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       01  WS-RECORD PIC X(80)." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       PROCEDURE DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           MERGE IN-FILE1 IN-FILE2" TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           ON ASCENDING KEY IN-RECORD1, IN-RECORD2" 
           TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           GIVING OUT-FILE." TO ENREG-GEN
           WRITE ENREG-GEN
           CLOSE FICHIER-GEN.
       
       GENERER-ACCEDER-DB.
           OPEN OUTPUT FICHIER-GEN
           MOVE "       IDENTIFICATION DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       PROGRAM-ID. AccederDB." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       ENVIRONMENT DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       INPUT-OUTPUT SECTION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       FILE-CONTROL." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           SELECT DB-FILE ASSIGN TO DATABASE." 
           TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       DATA DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       FILE SECTION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       FD  DB-FILE." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       01  DB-RECORD." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           05 DB-NAME PIC X(30)." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           05 DB-VALUE PIC X(50)." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       PROCEDURE DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           OPEN INPUT DB-FILE." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           READ DB-FILE INTO DB-RECORD" TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           DISPLAY DB-NAME DB-VALUE." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           CLOSE DB-FILE." TO ENREG-GEN
           WRITE ENREG-GEN
           CLOSE FICHIER-GEN.
       
       GENERER-SELECT-COUNT.
           OPEN OUTPUT FICHIER-GEN
           MOVE "       IDENTIFICATION DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       PROGRAM-ID. SelectCount." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       ENVIRONMENT DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       INPUT-OUTPUT SECTION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       FILE-CONTROL." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           SELECT DB-FILE ASSIGN TO DATABASE." 
           TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       DATA DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       FILE SECTION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       FD  DB-FILE." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       01  DB-RECORD." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           05 DB-COUNT PIC 9(9)." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       PROCEDURE DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           OPEN INPUT DB-FILE." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           EXEC SQL" TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "               SELECT COUNT(*) INTO :DB-COUNT" 
           TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "               FROM MY_TABLE" TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           END-EXEC." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           DISPLAY 'Number of records: ' DB-COUNT." 
           TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           CLOSE DB-FILE." TO ENREG-GEN
           WRITE ENREG-GEN
           CLOSE FICHIER-GEN.
       
       GENERER-CURSEUR.
           OPEN OUTPUT FICHIER-GEN
           MOVE "       IDENTIFICATION DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       PROGRAM-ID. GenerateCursor." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       ENVIRONMENT DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       INPUT-OUTPUT SECTION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       FILE-CONTROL." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           SELECT DB-FILE ASSIGN TO DATABASE." 
           TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       DATA DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       FILE SECTION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       FD  DB-FILE." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       01  DB-RECORD." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           05 DB-NAME PIC X(30)." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           05 DB-VALUE PIC X(50)." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       PROCEDURE DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           OPEN INPUT DB-FILE." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           EXEC SQL" TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "               DECLARE MY_CURSOR CURSOR FOR"
            TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "               SELECT NAME, VALUE" TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "               FROM MY_TABLE" TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           END-EXEC." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           OPEN MY_CURSOR." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           PERFORM UNTIL SQLCODE = 100" TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "              FETCH MY_CURSOR INTO :DB-NAME, :DB-VALUE" 
           TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "         DISPLAY 'Name: ' DB-NAME ' Value: ' DB-VALUE."
            TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           END-PERFORM." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           CLOSE MY_CURSOR." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           CLOSE DB-FILE." TO ENREG-GEN
           WRITE ENREG-GEN
           CLOSE FICHIER-GEN.
       
       GENERER-UPDATE-DB.
           OPEN OUTPUT FICHIER-GEN
           MOVE "       IDENTIFICATION DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       PROGRAM-ID. UpdateDB." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       ENVIRONMENT DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       INPUT-OUTPUT SECTION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       FILE-CONTROL." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           SELECT DB-FILE ASSIGN TO DATABASE." 
           TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       DATA DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       FILE SECTION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       FD  DB-FILE." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       01  DB-RECORD." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           05 DB-NAME PIC X(30)." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           05 DB-VALUE PIC X(50)." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       PROCEDURE DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           OPEN INPUT DB-FILE." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           EXEC SQL" TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "               UPDATE MY_TABLE" TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "               SET VALUE = :DB-VALUE" TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "               WHERE NAME = :DB-NAME" TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           END-EXEC." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           DISPLAY 'Updated ' DB-NAME ' to ' DB-VALUE."
            TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           CLOSE DB-FILE." TO ENREG-GEN
           WRITE ENREG-GEN
           CLOSE FICHIER-GEN.
       
       GENERER-SOUSROUTINE.
           OPEN OUTPUT FICHIER-GEN
           MOVE "       IDENTIFICATION DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       PROGRAM-ID. MySubroutine." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       ENVIRONMENT DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       DATA DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       WORKING-STORAGE SECTION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       01  WS-PARAMETERS." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           05 WS-INPUT  PIC X(50)." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           05 WS-OUTPUT PIC X(50)." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       PROCEDURE DIVISION USING WS-INPUT WS-OUTPUT." 
           TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           MOVE WS-INPUT TO WS-OUTPUT." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           EXIT PROGRAM." TO ENREG-GEN
           WRITE ENREG-GEN
           CLOSE FICHIER-GEN.
       
       GENERER-APPEL-SOUSROUTINE.
           OPEN OUTPUT FICHIER-GEN
           MOVE "       IDENTIFICATION DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       PROGRAM-ID. CallSubroutine." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       ENVIRONMENT DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       DATA DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       WORKING-STORAGE SECTION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       01  WS-PARAMETERS." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           05 WS-INPUT  PIC X(50) VALUE 'HELLO'." 
           TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           05 WS-OUTPUT PIC X(50)." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       PROCEDURE DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "         CALL 'MySubroutine' USING WS-INPUT WS-OUTPUT."
            TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "         DISPLAY 'Output from subroutine: ' WS-OUTPUT." 
           TO ENREG-GEN
           WRITE ENREG-GEN
           CLOSE FICHIER-GEN.
       
       GENERER-COPYBOOK.
           OPEN OUTPUT FICHIER-GEN
           MOVE "       01 MY-COPYBOOK." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           05 COPYBOOK-FIELD-1 PIC X(10)." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           05 COPYBOOK-FIELD-2 PIC 9(5)." TO ENREG-GEN
           WRITE ENREG-GEN
           CLOSE FICHIER-GEN.
       
       GENERER-INTEGRER-COPYBOOK.
           OPEN OUTPUT FICHIER-GEN
           MOVE "       IDENTIFICATION DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       PROGRAM-ID. IntegrerCopybook." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       ENVIRONMENT DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       DATA DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       WORKING-STORAGE SECTION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       COPY MY-COPYBOOK." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       PROCEDURE DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           DISPLAY 'Field 1: ' COPYBOOK-FIELD-1."
            TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           DISPLAY 'Field 2: ' COPYBOOK-FIELD-2."
            TO ENREG-GEN
           WRITE ENREG-GEN
           CLOSE FICHIER-GEN.
       
       GENERER-HELLOCobol.
           OPEN OUTPUT FICHIER-GEN
           MOVE "       IDENTIFICATION DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       PROGRAM-ID. HelloCobol." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       ENVIRONMENT DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       DATA DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       PROCEDURE DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           DISPLAY 'HELLO, COBOL!'." TO ENREG-GEN
           WRITE ENREG-GEN
           CLOSE FICHIER-GEN.
       
       GENERER-PARAGRAPHES-TYPE.
            OPEN OUTPUT FICHIER-GEN
           MOVE "       IDENTIFICATION DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       PROGRAM-ID. ParagraphesType." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       ENVIRONMENT DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       DATA DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       WORKING-STORAGE SECTION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       01 WS-VAR1 PIC X(10) VALUE 'HELLO'." 
           TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       01 WS-VAR2 PIC X(10)." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       PROCEDURE DIVISION." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       PARA-DEBUT." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           DISPLAY 'START'." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           PERFORM PARA-TRAITEMENT." TO ENREG-GEN
           MOVE "           PERFORM PARA-FIN." TO ENREG-GEN
           MOVE "           STOP RUN." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       PARA-TRAITEMENT." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           MOVE WS-VAR1 TO WS-VAR2." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           DISPLAY 'Processing: ' WS-VAR2." 
           TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "       PARA-FIN." TO ENREG-GEN
           WRITE ENREG-GEN
           MOVE "           DISPLAY 'END'." TO ENREG-GEN
           WRITE ENREG-GEN
           CLOSE FICHIER-GEN.
       