       IDENTIFICATION DIVISION.
       PROGRAM-ID.prog.
      
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REPORT-FILE ASSIGN TO "output.dat"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD REPORT-FILE.
       01 REPORT-RECORD PIC X(200).

       WORKING-STORAGE SECTION.

       01 DB-CONNECTION.
           05 DB-USER     PIC X(30) VALUE "cobol".
           05 DB-PASS     PIC X(10) VALUE SPACES.
           05 DB-DBNAME   PIC X(30) VALUE "school".

       01 STUDENT-INFO.
           05 STUDENT-ID PIC 9(4).
           05 LAST-NAME  PIC X(35).
           05 FIRST-NAME PIC X(35).
           05 AVERAGE-GRADE PIC 99V99.

       01 COURSE-INFO.
           05 COURSE-ID     PIC 9(4).
           05 COURSE-NAME   PIC X(35).
           05 COURSE-COEF   PIC 9(3)V9.
           05 COURSE-AVG-GRADE PIC 99V99.

       01 GRADE-INFO.
           05 GRADE-STUDENT-ID PIC 9(4).
           05 GRADE-COURSE-ID  PIC 9(4).
           05 GRADE-VALUE      PIC 99V99.

       01 HEADER-LINE PIC X(200) VALUE ALL "*".
       01 TITLE-LINE  PIC X(200) VALUE SPACES.
       01 COLUMN-HEADER PIC X(200) 
       VALUE "NOM        PRENOM     MOYENNE     C1        C2        C3"
           "        C4        C5        C6".
       01 STUDENT-REPORT-LINE PIC X(200).
       01 COURSES-LINE PIC X(48).

       01 WS-COURSE-NAMES OCCURS 6 TIMES PIC X(8).
       01 WS-GRADE PIC 99V99 VALUE 0.
       01 WS-STUDENT-COUNT PIC 9(3) VALUE 0.
       01 WS-TOTAL-AVG PIC 99V99 VALUE 0.
       01 WS-CLASS-AVG PIC 99V99 VALUE 0.

       01 COURSE-LIST.
           05 COURSE-DETAIL OCCURS 6 TIMES.
               10 COURSE-ID PIC 9(4).
               10 COURSE-LABEL PIC X(35).
               10 COURSE-COEF PIC 9(3)V9.
               10 COURSE-AVG PIC 99V99.

       EXEC SQL INCLUDE SQLCA END-EXEC.

       PROCEDURE DIVISION.
       MAIN-SECTION.
           OPEN OUTPUT REPORT-FILE

           PERFORM CONNECT-DB
           PERFORM GENERATE-REPORT-HEADER
           PERFORM FETCH-COURSES
           PERFORM FETCH-STUDENTS
           PERFORM GENERATE-CLASS-AVG
           PERFORM GENERATE-COURSE-DETAILS
           PERFORM CLOSE-DB

           CLOSE REPORT-FILE
           STOP RUN.

       CONNECT-DB.
           EXEC SQL
               CONNECT TO :DB-DBNAME USER :DB-USER USING :DB-PASS
           END-EXEC.

       FETCH-STUDENTS.
           EXEC SQL
               DECLARE STUDENT_CURSOR CURSOR FOR
               SELECT ID, LASTNAME, FIRSTNAME, TOTAL_GRADE FROM STUDENT
           END-EXEC.
           EXEC SQL
               OPEN STUDENT_CURSOR
           END-EXEC.

           PERFORM UNTIL SQLCODE = 100
               EXEC SQL FETCH STUDENT_CURSOR INTO :STUDENT-ID,
                                                 :LAST-NAME,
                                                 :FIRST-NAME,
                                                 :AVERAGE-GRADE
               END-EXEC
               IF SQLCODE = 0
                   ADD 1 TO WS-STUDENT-COUNT
                   ADD AVERAGE-GRADE TO WS-TOTAL-AVG
                   PERFORM FETCH-GRADES
                   PERFORM GENERATE-STUDENT-REPORT
               END-IF
           END-PERFORM.

           EXEC SQL
               CLOSE STUDENT_CURSOR
           END-EXEC.

       FETCH-COURSES.
           EXEC SQL
               DECLARE COURSE_CURSOR CURSOR FOR
               SELECT ID, NAME, COEF, AVG_GRADE FROM COURSE
           END-EXEC.
           EXEC SQL
               OPEN COURSE_CURSOR
           END-EXEC.

           PERFORM VARYING COURSE-ID FROM 1 BY 1 UNTIL COURSE-ID > 6
               EXEC SQL FETCH COURSE_CURSOR INTO
                   :COURSE-LIST(COURSE-ID)-COURSE-ID,
                   :COURSE-LIST(COURSE-ID)-COURSE-NAME,
                   :COURSE-LIST(COURSE-ID)-COURSE-COEF,
                   :COURSE-LIST(COURSE-ID)-COURSE-AVG
               END-EXEC
           END-PERFORM.

           EXEC SQL
               CLOSE COURSE_CURSOR
           END-EXEC.

       FETCH-GRADES.
           EXEC SQL
               DECLARE GRADE_CURSOR CURSOR FOR
               SELECT COURSE_ID, GRADE FROM GRADE 
               WHERE STUDENT_ID = :STUDENT-ID
           END-EXEC.
           EXEC SQL
               OPEN GRADE_CURSOR
           END-EXEC.

           MOVE SPACES TO COURSES-LINE
           PERFORM VARYING COURSE-ID FROM 1 BY 1 UNTIL COURSE-ID > 6
               MOVE 0 TO WS-GRADE
               EXEC SQL FETCH GRADE_CURSOR INTO :GRADE-COURSE-ID, 
                                                :GRADE-VALUE
               END-EXEC
               IF SQLCODE = 0
                   STRING GRADE-VALUE DELIMITED BY SIZE 
                       INTO COURSES-LINE (COURSE-ID:8)
               END-IF
           END-PERFORM.

           EXEC SQL
               CLOSE GRADE_CURSOR
           END-EXEC.

       GENERATE-REPORT-HEADER.
           MOVE HEADER-LINE TO REPORT-RECORD.
           WRITE REPORT-RECORD FROM REPORT-RECORD.

           STRING "BULLETIN DE NOTES" INTO TITLE-LINE(65:17)
           WRITE REPORT-RECORD FROM TITLE-LINE.

           MOVE HEADER-LINE TO REPORT-RECORD.
           WRITE REPORT-RECORD FROM REPORT-RECORD.

           MOVE COLUMN-HEADER TO REPORT-RECORD.
           WRITE REPORT-RECORD FROM REPORT-RECORD.

           MOVE HEADER-LINE TO REPORT-RECORD.
           WRITE REPORT-RECORD FROM REPORT-RECORD.

       GENERATE-STUDENT-REPORT.
           MOVE SPACES TO STUDENT-REPORT-LINE
           STRING LAST-NAME DELIMITED BY SIZE
                  "       "
                  FIRST-NAME DELIMITED BY SIZE
                  "       "
                  AVERAGE-GRADE DELIMITED BY SIZE
                  "       "
                  COURSES-LINE DELIMITED BY SIZE
                  INTO STUDENT-REPORT-LINE
           WRITE REPORT-RECORD FROM STUDENT-REPORT-LINE.

       GENERATE-CLASS-AVG.
           DIVIDE WS-TOTAL-AVG BY WS-STUDENT-COUNT GIVING WS-CLASS-AVG

           MOVE SPACES TO STUDENT-REPORT-LINE
           STRING "CLASSE" DELIMITED BY SIZE
                  "       "
                  WS-CLASS-AVG DELIMITED BY SIZE
                  "       "
                  COURSE-LIST(1)-COURSE-AVG DELIMITED BY SIZE
                  "       "
                  COURSE-LIST(2)-COURSE-AVG DELIMITED BY SIZE
                  "       "
                  COURSE-LIST(3)-COURSE-AVG DELIMITED BY SIZE
                  "       "
                  COURSE-LIST(4)-COURSE-AVG DELIMITED BY SIZE
                  "       "
                  COURSE-LIST(5)-COURSE-AVG DELIMITED BY SIZE
                  "       "
                  COURSE-LIST(6)-COURSE-AVG DELIMITED BY SIZE
                  INTO STUDENT-REPORT-LINE
           WRITE REPORT-RECORD FROM STUDENT-REPORT-LINE.

       GENERATE-COURSE-DETAILS.
           PERFORM VARYING COURSE-ID FROM 1 BY 1 UNTIL COURSE-ID > 6
               MOVE SPACES TO STUDENT-REPORT-LINE
               STRING "C" COURSE-ID " => COEF: " 
                   COURSE-LIST(COURSE-ID)-COURSE-COEF
                      " LABEL: " COURSE-LIST(COURSE-ID)-COURSE-NAME
                      INTO STUDENT-REPORT-LINE
               WRITE REPORT-RECORD FROM STUDENT-REPORT-LINE
           END-PERFORM

           MOVE SPACES TO STUDENT-REPORT-LINE
           STRING "NOMBRE D'ELEVES => " WS-STUDENT-COUNT
                  " NOMBRE DE COURS => 6"
                  " NOMBRE DE NOTES => " WS-STUDENT-COUNT * 6
                  INTO STUDENT-REPORT-LINE
           WRITE REPORT-RECORD FROM STUDENT-REPORT-LINE.

       CLOSE-DB.
           EXEC SQL
               COMMIT WORK
           END-EXEC.
           EXEC SQL
               DISCONNECT CURRENT
           END-EXEC.
