       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.

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

       01  REC-F-INPUT          PIC X(02).

       01  REC-STUDENT.
           03 R-S-KEY            PIC X(02).       
           03 R-S-LASTNAME       PIC X(07).       
           03 R-S-FIRSTNAME      PIC X(06).       
           03 R-S-AGE            PIC X(03).

       01  REC-COURSE.
           03 R-C-KEY            PIC X(02).       
           03 R-C-LABEL          PIC X(21).       
           03 R-C-COEF           PIC X(03).       
           03 R-C-GRADE          PIC X(05).

       WORKING-STORAGE SECTION.
       01  F-INPUT-STATUS      PIC X(02) VALUE SPACE.
           88 F-INPUT-STATUS-OK    VALUE '00'.        
           88 F-INPUT-STATUS-EOF   VALUE '10'.

       01  CURRENT-STUDENT-ID  PIC 9(02).

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME                  PIC  X(30) VALUE 'school'.
       01  USERNAME                PIC  X(30) VALUE 'cobol'.
       01  PASSWD                  PIC  X(10) VALUE SPACES.

       01  SQL-STUDENT.
           05  SQL-S-LASTNAME           PIC X(07).
           05  SQL-S-FIRSTNAME          PIC X(06).
           05  SQL-S-AGE                PIC 9(02).
       
       01  SQL-COURSE.
           05  SQL-C-LABEL              PIC X(21).
           05  SQL-C-COEF               PIC 9V9.
          

       01  SQL-GRADE.
           05 SQL-G-STUDENT-ID PIC 9.
           05 SQL-G-COURSE-ID  PIC 9.
           05 SQL-G-GRADE      PIC 99V99.    

       EXEC SQL END DECLARE SECTION END-EXEC.
       EXEC SQL INCLUDE SQLCA END-EXEC.

       PROCEDURE DIVISION.
       1000-MAIN-START.
           EXEC SQL
               CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME 
           END-EXEC.

           IF  SQLCODE NOT = ZERO 
               PERFORM 1001-ERROR-RTN-START
                   THRU 1001-ERROR-RTN-END
           END-IF.
          
           PERFORM 7001-FILE-READ-START
               THRU 7001-FILE-READ-END.

       1000-MAIN-END.
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
      ******************************************************************
       7001-FILE-READ-START.
           OPEN INPUT F-INPUT.
           IF NOT F-INPUT-STATUS-OK
               DISPLAY 'ABORT POPULATING TABLE'
               GO TO 7001-FILE-READ-END
           END-IF.
           
           PERFORM UNTIL F-INPUT-STATUS-EOF
               READ F-INPUT
               AT END
                   SET F-INPUT-STATUS-EOF TO TRUE
               NOT AT END
               IF NOT F-INPUT-STATUS-OK AND NOT F-INPUT-STATUS-EOF
                DISPLAY ' PROBLEME LECTURE FICHIER : CR ' F-INPUT-STATUS  
               END-IF
               DISPLAY 'PREFIXE ENREG : ' REC-F-INPUT
               DISPLAY 'LASTNAME : ' R-S-LASTNAME
               DISPLAY 'MATIERE : ' R-C-LABEL
                   EVALUATE REC-F-INPUT
                       WHEN '01'
                           PERFORM 7101-FILE-HANDLE-STUDENT-START
                               THRU 7101-FILE-HANDLE-STUDENT-END
                       WHEN '02'
                           IF CURRENT-STUDENT-ID < 2
                              PERFORM 7201-FILE-HANDLE-COURSE-START
                                 THRU 7201-FILE-HANDLE-COURSE-END  
                           END-IF 
                            PERFORM 7301-START-HANDLE-GRADE 
                          THRU 7301-HANDLE-GRADE-END   
                       WHEN OTHER
                           CONTINUE
                   END-EVALUATE
           END-PERFORM.
       7001-FILE-READ-END.
           CLOSE F-INPUT.

      ******************************************************************
       7101-FILE-HANDLE-STUDENT-START.
           MOVE R-S-LASTNAME TO SQL-S-LASTNAME.
           MOVE R-S-FIRSTNAME TO SQL-S-FIRSTNAME.
           MOVE R-S-AGE TO SQL-S-AGE.

           DISPLAY ' FLAG 1'
           EXEC SQL
               INSERT INTO student (LASTNAME, FIRSTNAME, AGE) 
               VALUES (
                   :SQL-S-LASTNAME, 
                   :SQL-S-FIRSTNAME,
                   :SQL-S-AGE
               )
           END-EXEC.
           
           ADD 1 TO CURRENT-STUDENT-ID.
           DISPLAY 'FLAG 1-2'.
      *     EXEC SQL
      *         SELECT MAX(ID) INTO :CURRENT-STUDENT-ID FROM STUDENT
      *     END-EXEC.
       7101-FILE-HANDLE-STUDENT-END.
           EXIT.
      ******************************************************************
       7201-FILE-HANDLE-COURSE-START.
           MOVE R-C-LABEL TO SQL-C-LABEL.
           MOVE R-C-COEF  TO SQL-C-COEF.
           
           DISPLAY 'FLAG 1-3'
           DISPLAY 'R-C-LABEL : ' R-C-LABEL.
           DISPLAY 'SQL-C-LABEL : ' SQL-C-LABEL.

           EXEC SQL
               INSERT INTO COURSE (LABEL, COEF)
               VALUES (:SQL-C-LABEL, :SQL-C-COEF)
               
           END-EXEC.

           DISPLAY 'FLAG 1-4'.
       7201-FILE-HANDLE-COURSE-END.
       7301-START-HANDLE-GRADE.
           
           DISPLAY 'FLAG 1-5'
           DISPLAY 'SQL-S-LASTNAME : ' SQL-S-LASTNAME
           DISPLAY 'SQL-S-FIRSTNAME : ' SQL-S-FIRSTNAME

           EXEC SQL
                  SELECT STUDENT.ID INTO :SQL-G-STUDENT-ID FROM STUDENT
                  WHERE LASTNAME = :SQL-S-LASTNAME 
                  AND FIRSTNAME = :SQL-S-FIRSTNAME
           END-EXEC.
           DISPLAY 'FLAG 1-6'
           
           MOVE R-C-LABEL TO SQL-C-LABEL.
           EXEC SQL
                  SELECT COURSE.ID INTO :SQL-G-COURSE-ID FROM COURSE
                  WHERE LABEL = :SQL-C-LABEL
           END-EXEC.
           DISPLAY 'FLAG 1-7'

           MOVE R-C-GRADE TO SQL-G-GRADE.
           EXEC SQL
                  INSERT INTO GRADE (STUDENT_ID,COURSE_ID,GRADE) 
                  VALUES (
                      :SQL-G-STUDENT-ID, 
                      :SQL-G-COURSE-ID,
                      :SQL-G-GRADE
                      )
           END-EXEC.
           DISPLAY 'FLAG 1-8'.

       7301-HANDLE-GRADE-END.
               EXIT.
