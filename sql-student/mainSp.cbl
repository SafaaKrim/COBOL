      * ****************************************************************
      * Auteur       : K.SAFAA                                         *
      * Date         : 02/05/2024                                      *      
      * Description  : Ce programme COBOL gère les données             *
      * des étudiants brièvement.                                      *
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. mainsp.
       AUTHOR. K-SAFAA.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-INPUT
               ASSIGN TO 'input.dat'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS F-INPUT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  F-INPUT
           RECORD CONTAINS 2 TO 1000 CHARACTERS 
           RECORDING MODE IS V.

       01  REC-F-INPUT-2         PIC X(02).

       01  REC-STUDENT.
           03 R-S-KEY            PIC 9(02).       
           03 R-S-LASTNAME       PIC X(07).       
           03 R-S-FIRSTNAME      PIC X(06).       
           03 R-S-AGE            PIC 9(02).       

       01  REC-COURSE.
           03 R-C-KEY            PIC 9(02).       
           03 R-C-LABEL          PIC X(21).       
           03 R-C-COEF           PIC X(03).       
           03 R-C-GRADE          PIC X(05).


       WORKING-STORAGE SECTION.
       01  F-INPUT-STATUS      PIC X(02) VALUE SPACE.
           88 F-INPUT-STATUS-OK    VALUE '00'.        
           88 F-INPUT-STATUS-EOF   VALUE '10'.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  DBNAME                  PIC  X(30) VALUE 'school'.
       01  USERNAME                PIC  X(30) VALUE 'cobol'.
       01  PASSWD                  PIC  X(10) VALUE SPACE.
       
       01  WS-SQL-STUDENT.
           05  SQL-S-ID                 PIC 9(05).
           05  SQL-S-LASTNAME           PIC X(07).
           05  SQL-S-FIRSTNAME          PIC X(06).
           05  SQL-S-AGE                PIC 9(02).
           05  SQL-S-MOY-GEN            PIC 99V99.


       01  WS-SQL-GRADE.
           05 SQL-G-ID                 PIC 9(05).
           05 SQL-G-LASTNAME           PIC X(07).
           05 SQL-G-FIRSTNAME          PIC X(06).
           05 SQL-G-LABEL              PIC X(25).
           05 SQL-G-GRADE              PIC 99V99.


       01  WS-SQL-COURSE.
           05 SQL-C-ID                 PIC 9(05).
           05 SQL-C-LABEL              PIC X(25).
           05 SQL-C-COEFF              PIC 9V9.
           05 SQL-MOY-MAT              PIC 99V99.

       EXEC SQL END DECLARE SECTION END-EXEC.
       
       01  WS-IND-CHG-ELEVE PIC X(03) VALUE SPACE.
           88 WS-CHG                  VALUE 'OUI'.
           88 WS-NON-CHG              VALUE 'NON'.

       EXEC SQL INCLUDE SQLCA END-EXEC.

       PROCEDURE DIVISION.

           EXEC SQL
               CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME 
           END-EXEC.

           IF  SQLCODE NOT = ZERO 
               PERFORM 1001-ERROR-RTN-START
                   THRU 1001-ERROR-RTN-END
           END-IF.
               
           PERFORM 3001-SQL-TBL-STUDENT-START
               THRU 3001-SQL-TBL-STUDENT-END.

           PERFORM 3002-SQL-TBL-GRADE-START
              THRU 3002-SQL-TBL-GRADE-END.

           PERFORM 3003-SQL-TBL-COURSE-START
              THRU 3003-SQL-TBL-COURSE-END.

           PERFORM TRAITEMENT-PRINCIPAL
           THRU    TRAITEMENT-PRINCIPAL

           EXEC SQL COMMIT WORK END-EXEC.
           EXEC SQL DISCONNECT ALL END-EXEC. 

           STOP RUN. 
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
       
       3001-SQL-TBL-STUDENT-START.
           CREATE TABLE STUDENT (
               SQL-S-ID INT PRIMARY KEY,
               SQL-S-LASTNAME VARCHAR(7) NOT NULL,
               SQL-S-FIRSTNAME VARCHAR(6) NOT NULL,
               SQL-S-AGE INT,
               SQL-S-MOY-GEN DECIMAL(5,2)
           );
       3001-SQL-TBL-STUDENT-END.
      ******************************************************************
       3002-SQL-TBL-GRADE-START.
           CREATE TABLE GRADE (
               SQL-G-ID INT PRIMARY KEY,
               SQL-G-LASTNAME VARCHAR(7) NOT NULL,
               SQL-G-FIRSTNAME VARCHAR(6) NOT NULL,
               SQL-G-LABEL VARCHAR(25) NOT NULL,
               SQL-G-GRADE DECIMAL(5,2)
           );
       3002-SQL-TBL-GRADE-END. 
      ******************************************************************
        3003-SQL-TBL-COURSE-START.
           CREATE TABLE COURSE (
               SQL-C-ID INT PRIMARY KEY,
               SQL-C-LABEL VARCHAR(25) NOT NULL,
               SQL-C-COEFF DECIMAL(3,1) NOT NULL,
               SQL-MOY-MAT DECIMAL(5,2)
           );
           
       3003-SQL-TBL-COURSE-END.
      ******************************************************************
       
       ALIM-TABLE-ETUDIANT.
           MOVE R-FIRSTNAME TO SQL-S-FIRSTNAME.
           MOVE R-LASTNAME TO SQL-S-LASTNAME.
           MOVE R-AGE TO SQL-S-AGE.
           ADD 1 TO SQL-S-ID.

             EXEC SQL
               INSERT INTO STUDENT25
               VALUES (:SQL-S-ID,
                  :SQL-S-FIRSTNAME ,
                  :SQL-S-LASTNAME ,
                  :SQL-S-AGE,
                  2)
           END-EXEC.
       ALIM-TABLE-ETUDIANT-FIN.

       ALIM-TABLE-COURSE.
           MOVE SQL-R-COURSE TO SQL-C-LABEL.
           MOVE SQL-R-COEFF TO SQL-C-COEFF.
           ADD 1 TO SQL-C-ID.

           EXEC SQL
           INSERT INTO COURSE
            VALUES (:SQL-C-ID,
                  :SQL-C-LABEL,
                  :SQL-C-COEFF)
           END-EXEC.
       
       ALIM-TABLE-NOTE.
           MOVE R-COURSE TO SQL-G-COURSE.
           MOVE R-GRADE TO SQL-G-GRADE.
           ADD 1  TO SQL-G-ID
       
           EXEC SQL
           INSERT INTO GRADE
           VALUES(:SQL-G-ID,
               :SQL-G-LASTNAME,
               :SQL-G-FIRSTNAME,
               :SQL-G-LABEL,
               :SQL-G-GRADE)
               
           END-EXEC.

       TRAITEMENT-PRINCIPAL.

      * OBJECTIF : LIRE LE FICHIER ENTRANT DE LA FACON SUIVANTE :
      * -> quand j'ai une ligne 01 j'utilise la zone 01 REC-STUDENT
      * -> quand j'ai une ligne 02 j'utilise la zone 01 REC-COURSE

           READ INPUT
           MOVE ZERO TO SQL-S-ID
                        SQL-C-ID
                        SQL-G-ID
633
           IF REC-F-INPUT-2 = 01
             PERFORM ALIM-TABLE-ETUDIANT
           ELSE
             PERFORM ALIM-TABLE-COURSE
             PERFORM ALIM-TABLE-NOTE
           END-IF

           SET WS-NON-CHG TO TRUE.

           PERFORM UNTIL EOF
              READ INPUT
              IF REC-F-INPUT-2 = 01
                 SET WS-CHG TO TRUE
                 PERFORM ALIM-TABLE-ETUDIANT
      * GERER LE CALCUL DE LA MOYENNE GENERAL
     
              ELSE
                 PERFORM ALIM-TABLE-NOTE
                 IF WS-NON-CHG
                    PERFORM ALIM-TABLE-COURSE
      * GERER LE CALCUL DE LA MOYENNE MATIERE
     
                 END-IF
              END-IF
           END-PERFORM

           PERFORM UNTIL WS-EOF = 'Y'
           READ INPUT
               AT END
                   SET WS-EOF TO 'Y'
                   EXIT PERFORM
               NOT AT END
                   MOVE REC-F-INPUT-2 TO WS-RECORD-TYPE
                   IF WS-RECORD-TYPE = '01'
                       SET WS-CHG TO TRUE
                       PERFORM ALIM-TABLE-ETUDIANT
                       * CALCULER LA MOYENNE GÉNÉRALE
                   ELSE
                       PERFORM ALIM-TABLE-NOTE
                       IF WS-NON-CHG
                           PERFORM ALIM-TABLE-COURSE
                           
                           * CALCULER LA MOYENNE DE MATIÈRE
                       END-IF
                   END-IF
           END-READ

       TRAITEMENT-PRINCIPAL-FIN.
       
