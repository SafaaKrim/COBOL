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

OCESQL*EXEC SQL BEGIN DECLARE SECTION END-EXEC.

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

OCESQL*EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  SQL-MAX-AGE             PIC 9(03).
       01  SQL-MIN-AGE             PIC 9(03).
       01  SQL-AGE-COUNT           PIC 9(03).
       01  SQL-AGE-VALUE           PIC 9(03).
       01  I                       PIC 9(5) VALUE 1.
     
OCESQL*EXEC SQL INCLUDE SQLCA END-EXEC.
OCESQL     copy "sqlca.cbl".

OCESQL*
OCESQL 01  SQ0001.
OCESQL     02  FILLER PIC X(014) VALUE "DISCONNECT ALL".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0002.
OCESQL     02  FILLER PIC X(029) VALUE "SELECT MAX(age) FROM databank".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0003.
OCESQL     02  FILLER PIC X(029) VALUE "SELECT MIN(age) FROM databank".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0004.
OCESQL     02  FILLER PIC X(066) VALUE "SELECT age, COUNT( * ) FROM da"
OCESQL  &  "tabank GROUP BY age ORDER BY age ASC".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0005.
OCESQL     02  FILLER PIC X(123) VALUE "SELECT first_name, last_name, "
OCESQL  &  "email, phrase FROM databank A INNER JOIN phrase B ON A.id "
OCESQL  &  "= B.id WHERE DK-COUNTRY = 'Belgium'".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0006.
OCESQL     02  FILLER PIC X(067) VALUE "UPDATE DATABANK SET country_co"
OCESQL  &  "de = 'BE' WHERE age > 35 AND age < 40".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0007.
OCESQL     02  FILLER PIC X(065) VALUE "UPDATE DATABANK SET country_co"
OCESQL  &  "de = 'BE' WHERE country = 'Belgium'".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0008.
OCESQL     02  FILLER PIC X(064) VALUE "UPDATE DATABANK SET country_co"
OCESQL  &  "de = 'FR' WHERE country = 'France'".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0009.
OCESQL     02  FILLER PIC X(068) VALUE "UPDATE DATABANK SET country = "
OCESQL  &  "UPPER(country), spoken = UPPER(spoken)".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
       PROCEDURE DIVISION.
       1000-MAIN-START.

OCESQL*    EXEC SQL
OCESQL*        CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME 
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLConnect" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE USERNAME
OCESQL          BY VALUE 30
OCESQL          BY REFERENCE PASSWD
OCESQL          BY VALUE 10
OCESQL          BY REFERENCE DBNAME
OCESQL          BY VALUE 30
OCESQL     END-CALL.

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
OCESQL*    EXEC SQL COMMIT WORK END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "COMMIT" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.
OCESQL*    EXEC SQL DISCONNECT ALL END-EXEC.  
OCESQL     CALL "OCESQLDisconnect" USING
OCESQL          BY REFERENCE SQLCA
OCESQL     END-CALL.
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
OCESQL*          EXEC SQL
OCESQL*              ROLLBACK
OCESQL*          END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "ROLLBACK" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL
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
OCESQL*    EXEC SQL
OCESQL*     SELECT MAX(age) INTO  :SQL-MAX-AGE FROM databank
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 3
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-MAX-AGE
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecSelectIntoOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0002
OCESQL          BY VALUE 0
OCESQL          BY VALUE 1
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.
OCESQL*    EXEC SQL 
OCESQL*     SELECT MIN(age) INTO  :SQL-MIN-AGE FROM databank
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 3
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-MIN-AGE
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecSelectIntoOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0003
OCESQL          BY VALUE 0
OCESQL          BY VALUE 1
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.
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
OCESQL*    EXEC SQL DECLARE AGE_CUR CURSOR FOR
OCESQL*        SELECT age, COUNT(*)
OCESQL*        FROM databank
OCESQL*        GROUP BY age
OCESQL*        ORDER BY age ASC
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorDeclare" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "people_AGE_CUR" & x"00"
OCESQL          BY REFERENCE SQ0004
OCESQL     END-CALL.
OCESQL*    EXEC SQL OPEN AGE_CUR END-EXEC.
OCESQL     CALL "OCESQLCursorOpen" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "people_AGE_CUR" & x"00"
OCESQL     END-CALL.
   
           PERFORM UNTIL SQLCODE = +100
OCESQL*       EXEC SQL
OCESQL*            FETCH AGE_CUR
OCESQL*            INTO :SQL-AGE-VALUE, :SQL-AGE-COUNT
OCESQL*        END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 3
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-AGE-VALUE
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 3
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-AGE-COUNT
OCESQL     END-CALL
OCESQL     CALL "OCESQLCursorFetchOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "people_AGE_CUR" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL

               IF SQLCODE = 0 THEN
                   DISPLAY 'Age ' SQL-AGE-VALUE ': ' SQL-AGE-COUNT
               END-IF
           END-PERFORM.
          

OCESQL*    EXEC SQL CLOSE AGE_CUR END-EXEC.
OCESQL     CALL "OCESQLCursorClose"  USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "people_AGE_CUR" & x"00"
OCESQL     END-CALL
OCESQL    .

       7000-DISPLAY-AGE-COUNT-END.

      ******************************************************************
      * SECTION POUR AFFICHER LES CITOYENS BELGES
      ******************************************************************
       8000-DISPLAY-BELGIUM-CITIZENS.
OCESQL*    EXEC SQL
OCESQL*        DECLARE CUR CURSOR FOR
OCESQL*            SELECT first_name, last_name, email, phrase
OCESQL*            FROM databank A
OCESQL*            INNER JOIN phrase B ON A.id = B.id
OCESQL*            WHERE "DK-COUNTRY" = 'Belgium'
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorDeclare" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "people_CUR" & x"00"
OCESQL          BY REFERENCE SQ0005
OCESQL     END-CALL.
OCESQL*    EXEC SQL
OCESQL*        OPEN cur;
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorOpen" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "people_cur" & x"00"
OCESQL     END-CALL.
OCESQL*    EXEC SQL
OCESQL*        FETCH cur INTO :DK-FIRST-NAME, 
OCESQL*        :DK-LAST-NAME, :DK-EMAIL, :PH-PHRASE;
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE DK-FIRST-NAME
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE DK-LAST-NAME
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE DK-EMAIL
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE PH-PHRASE
OCESQL     END-CALL
OCESQL     CALL "OCESQLCursorFetchOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "people_cur" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.
          
           PERFORM UNTIL SQLCODE <> 0
               DISPLAY "Nom: " DK-FIRST-NAME 
               DISPLAY" Prénom: " DK-LAST-NAME 
               DISPLAY "Email: " DK-EMAIL 
               DISPLAY" Citation: " PH-PHRASE
OCESQL*        EXEC SQL
OCESQL*            FETCH cur INTO :DK-FIRST-NAME, 
OCESQL*            :DK-LAST-NAME, :DK-EMAIL, :PH-PHRASE;
OCESQL*        END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE DK-FIRST-NAME
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE DK-LAST-NAME
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE DK-EMAIL
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE PH-PHRASE
OCESQL     END-CALL
OCESQL     CALL "OCESQLCursorFetchOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "people_cur" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL
               
           END-PERFORM.
OCESQL*    EXEC SQL
OCESQL*        CLOSE cur;
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorClose"  USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "people_cur" & x"00"
OCESQL     END-CALL
OCESQL    .
       8000-DISPLAY-BELGIUM-CITIZENS-END.

      ******************************************************************
      * SECTION POUR METTRE À JOUR LES CODES PAYS POUR CERTAINES 'ÂGE
      ******************************************************************
       9000-UPDATE-COUNTRY-CODES.
           DISPLAY " Mettre à jour les codes de pays."
OCESQL*    EXEC SQL
OCESQL*      UPDATE DATABANK
OCESQL*      SET "country_code" = 'BE'
OCESQL*      WHERE age > 35 AND age < 40
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0006
OCESQL     END-CALL.
           DISPLAY SQLCODE.

         
       9000-UPDATE-COUNTRY-CODES-END.

      ******************************************************************
      * SECTION POUR CORRIGER LES INCONSISTANCES DANS LES CODES PAYS
      ******************************************************************
       10000-CORRECT-INCONSISTENCIES.
           DISPLAY "  corriger les pays / code pays."
OCESQL*    EXEC SQL
OCESQL*        UPDATE DATABANK
OCESQL*        SET "country_code" = 'BE'
OCESQL*        WHERE country = 'Belgium'
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0007
OCESQL     END-CALL.
OCESQL*      EXEC SQL
OCESQL*        UPDATE DATABANK
OCESQL*        SET "country_code" = 'FR'
OCESQL*        WHERE country = 'France'
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0008
OCESQL     END-CALL.

           DISPLAY SQLCODE.

         
       10000-CORRECT-INCONSISTENCIES-END.
        
      ******************************************************************
      * SECTION POUR METTRE EN MAJUSCULE CERTAINES COLONNES
      ******************************************************************
       11000-UPPERCASE-COLUMNS.
           DISPLAY" mettre en majuscule les pays et langue parlée"
OCESQL*      EXEC SQL
OCESQL*        UPDATE DATABANK
OCESQL*        SET "country"= UPPER(country),
OCESQL*            "spoken" = UPPER(spoken)
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0009
OCESQL     END-CALL.

           DISPLAY SQLCODE.

       11000-UPPERCASE-COLUMNS-END.
       11000-UPPERCASE-COLUMNS-END.
       11000-UPPERCASE-COLUMNS-END.
