       IDENTIFICATION DIVISION.
       PROGRAM-ID.people.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
          DECIMAL-POINT IS COMMA.

       DATA DIVISION.   
       WORKING-STORAGE SECTION.
       01 F-INPUT-STATUS      PIC X(02) VALUE SPACE.
           88 F-INPUT-STATUS-OK    VALUE '00'.        
           88 F-INPUT-STATUS-EOF   VALUE '10'.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01 DBNAME                PIC  X(30) VALUE 'citoyens'.
       01 USERNAME              PIC  X(30) VALUE 'cobol'.
       01 PASSWD                PIC  X(10) VALUE SPACE.

       01 PHRASE-DATA.
           05 PH-COUNTRY-CODE   PIC X(50).
           05 PH-PHRASE         PIC X(50).

       01 DATABANK-DATA.
           05 DK-FIRST-NAME     PIC X(50).
           05 DK-LAST-NAME      PIC X(50).
           05 DK-EMAIL          PIC X(50).
           05 DK-GENDER         PIC X(50).
           05 DK-AGE            PIC 9(10).   
           05 DK-SPOKEN         PIC X(50).
           05 DK-COUNTRY        PIC X(50).
           05 DK-COUNTRY-CODE   PIC X(50).
           05 DK-INFO-PHONE     PIC X(50).
        
       EXEC SQL INCLUDE SQLCA END-EXEC.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  SQL-MAX-AGE             PIC 9(03).
       01  SQL-MIN-AGE             PIC 9(03).
       01  SQL-AGE-COUNT           PIC 9(03).
       01  SQL-AGE-VALUE           PIC 9(03).
      
     
       EXEC SQL INCLUDE SQLCA END-EXEC.

       PROCEDURE DIVISION.
       1000-MAIN-START.

           EXEC SQL
               CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME 
           END-EXEC.

           IF  SQLCODE NOT = 0 
               PERFORM 1001-ERROR-RTN-START 
               THRU 1001-ERROR-RTN-END
           END-IF.

           PERFORM 4000-CALCULATE-AGE-STATS.
           PERFORM 6000-DISPLAY-MAX-MIN-AGE.
           PERFORM 7000-DISPLAY-AGE-COUNT.
           PERFORM 8000-DISPLAY-BELGIUM-CITIZENS.
           PERFORM 9000-UPDATE-COUNTRY-CODES.
           PERFORM 10000-CORRECT-INCONSISTENCIES.
           PERFORM 11000-UPPERCASE-COLUMNS.
       1000-MAIN-END.
           EXEC SQL COMMIT WORK END-EXEC.
           EXEC SQL DISCONNECT ALL END-EXEC.  
           STOP RUN.

      ******************************************************************
      * SECTION POUR GERER LES ERREURS SQL
      ******************************************************************
       1001-ERROR-RTN-START.
           DISPLAY "*** SQL ERROR ***".
           DISPLAY "SQLCODE: " SQLCODE SPACE.
           EVALUATE SQLCODE
              WHEN  +100
                 DISPLAY "Record not found"
              WHEN  -01
                 DISPLAY "Connection failed"
              WHEN  -20
                 DISPLAY "Internal error"
              WHEN  -30
                 DISPLAY "PostgreSQL error"
                 DISPLAY "ERRCODE:" SPACE SQLSTATE
                 DISPLAY SQLERRMC
                 EXEC SQL
                     ROLLBACK
                 END-EXEC
              WHEN  OTHER
                 DISPLAY "Undefined error"
                 DISPLAY "ERRCODE:" SPACE SQLSTATE
                 DISPLAY SQLERRMC
           END-EVALUATE.
       1001-ERROR-RTN-END.
           STOP RUN.              

      ******************************************************************
      * SECTION POUR CALCULER LES STATISTIQUES D'ÂGE
      ******************************************************************
       4000-CALCULATE-AGE-STATS.
             MOVE 0 TO SQL-MIN-AGE SQL-MAX-AGE.
           EXEC SQL
            SELECT MAX(age) INTO  :SQL-MAX-AGE FROM databank
           END-EXEC.
           EXEC SQL 
            SELECT MIN(age) INTO  :SQL-MIN-AGE FROM databank
           END-EXEC.
       4000-CALCULATE-AGE-STATS-END.

      ******************************************************************
      * SECTION POUR AFFICHER LES STATISTIQUES D'ÂGE
      ******************************************************************
       6000-DISPLAY-MAX-MIN-AGE.
           IF SQLCODE NOT = 100
              DISPLAY "Âge Maximum: " SQL-MAX-AGE
              DISPLAY "Âge Minimum: " SQL-MIN-AGE
           END-IF.
       6000-DISPLAY-MAX-MIN-AGE-END.

      ******************************************************************
      * SECTION POUR AFFICHER LE DÉCOMPTE DES TRANCHE D'ÂGE
      ******************************************************************
       7000-DISPLAY-AGE-COUNT.
           DISPLAY 'Age Counts: '.
           EXEC SQL DECLARE AGE_CUR CURSOR FOR
               SELECT age, COUNT(*)
               FROM databank
               GROUP BY age
               ORDER BY age ASC
           END-EXEC.
           EXEC SQL OPEN AGE_CUR END-EXEC.
   
           PERFORM UNTIL SQLCODE = +100
              EXEC SQL
                   FETCH AGE_CUR
                   INTO :SQL-AGE-VALUE, :SQL-AGE-COUNT
               END-EXEC

               IF SQLCODE = 0 THEN
                   DISPLAY 'Age ' SQL-AGE-VALUE ': ' SQL-AGE-COUNT
               END-IF
           END-PERFORM.
          

           EXEC SQL CLOSE AGE_CUR END-EXEC.

       7000-DISPLAY-AGE-COUNT-END.

      ******************************************************************
      * SECTION POUR AFFICHER LES CITOYENS BELGES
      ******************************************************************
       8000-DISPLAY-BELGIUM-CITIZENS.
           EXEC SQL
               DECLARE CUR_CITOYENS CURSOR FOR
                   SELECT first_name, last_name, email, phrase
                   FROM databank A
                   INNER JOIN phrase B ON A.id = B.id
                   WHERE "DK-COUNTRY" = 'Belgium'
           END-EXEC.
           
           EXEC SQL
               OPEN CUR_CITOYENS;
           END-EXEC.
           EXEC SQL
               FETCH CUR_CITOYENS INTO :DK-FIRST-NAME, 
               :DK-LAST-NAME, :DK-EMAIL, :PH-PHRASE;
           END-EXEC.
          
           PERFORM UNTIL SQLCODE <> 0
               DISPLAY "Nom: " DK-FIRST-NAME 
               DISPLAY" Prénom: " DK-LAST-NAME 
               DISPLAY "Email: " DK-EMAIL 
               DISPLAY" Citation: " PH-PHRASE
               EXEC SQL
                   FETCH CUR_CITOYENS INTO :DK-FIRST-NAME, 
                   :DK-LAST-NAME, :DK-EMAIL, :PH-PHRASE;
               END-EXEC
               
           END-PERFORM.
           EXEC SQL
               CLOSE cur;
           END-EXEC.
       8000-DISPLAY-BELGIUM-CITIZENS-END.

      ******************************************************************
      * SECTION POUR METTRE À JOUR LES CODES PAYS POUR CERTAINES 'ÂGE
      ******************************************************************
       9000-UPDATE-COUNTRY-CODES.
           DISPLAY " Mettre à jour les codes de pays."
           EXEC SQL
             UPDATE DATABANK
             SET "country_code" = 'BE'
             WHERE age > 35 AND age < 40
           END-EXEC.
           DISPLAY SQLCODE.

         
       9000-UPDATE-COUNTRY-CODES-END.

      ******************************************************************
      * SECTION POUR CORRIGER LES INCONSISTANCES DANS LES CODES PAYS
      ******************************************************************
       10000-CORRECT-INCONSISTENCIES.
           DISPLAY "  corriger les pays / code pays."
           EXEC SQL
               UPDATE DATABANK
               SET "country_code" = 'BE'
               WHERE country = 'Belgium'
           END-EXEC.
             EXEC SQL
               UPDATE DATABANK
               SET "country_code" = 'FR'
               WHERE country = 'France'
           END-EXEC.

           DISPLAY SQLCODE.

         
       10000-CORRECT-INCONSISTENCIES-END.
        
      ******************************************************************
      * SECTION POUR METTRE EN MAJUSCULE CERTAINES COLONNES
      ******************************************************************
       11000-UPPERCASE-COLUMNS.
           DISPLAY" mettre en majuscule les pays et langue parlée"
             EXEC SQL
               UPDATE DATABANK
               SET "country"= UPPER(country),
                   "spoken" = UPPER(spoken)
           END-EXEC.

           DISPLAY SQLCODE.

       11000-UPPERCASE-COLUMNS-END.
