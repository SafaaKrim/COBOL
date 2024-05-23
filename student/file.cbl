      ****************************************************************** 
      * Auteur       : K.SAFAA                                         *
      * Date         : 24/04/2024                                      *      
      * Description  : Ce programme COBOL gère les données             *
      * des étudiants brièvement.                                       *
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STUDENT.
       AUTHOR. K-SAFAA.

      ****************************************************************** 
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

           SELECT F-OUTPUT
               ASSIGN TO 'output.dat'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS F-OUTPUT-STATUS.

      ****************************************************************** 
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

       FD  F-OUTPUT
           RECORD CONTAINS 1700 CHARACTERS
           RECORDING MODE IS F.
       01  REC-F-OUTPUT        PIC X(1700).

      ******************************************************************
       WORKING-STORAGE SECTION.
       01  F-INPUT-STATUS      PIC X(02) VALUE SPACE.
           88 F-INPUT-STATUS-OK    VALUE '00'.        
           88 F-INPUT-STATUS-EOF   VALUE '10'.

       01  F-OUTPUT-STATUS     PIC X(02) VALUE SPACE.
           88 F-OUTPUT-STATUS-OK    VALUE '00'.        
           88 F-OUTPUT-STATUS-EOF   VALUE '10'.

       01  DATA-STUDENT.
           03 STUDENT-LGTH     PIC 9(03) VALUE 1.
           03 STUDENT  
               OCCURS 1 TO 999 TIMES
               DEPENDING ON STUDENT-LGTH
               INDEXED BY IDX-STUDENT.
                   05 S-LASTNAME   PIC X(06).
                   05 S-FIRSTNAME  PIC X(07).
                   05 S-AGE        PIC 9(02).
                  
       01  DATA-COURSE.
           03 COURSE-LGTH     PIC 9(03) VALUE 1.
           03 COURSE
               OCCURS 1 TO 999 TIMES
               DEPENDING ON COURSE-LGTH
               INDEXED BY IDX-COURSE. 
                    05 C-COEFF      PIC 9V9.
                    05 C-LABEL      PIC X(25).

                   

       01  DATA-GRADE.
           03 GRADE-LGTH      PIC 9(03) VALUE 1.
           03 GRADE
               OCCURS 1 TO 999 TIMES
               DEPENDING ON GRADE-LGTH
               INDEXED BY IDX-GRADE. 
                   05 G-S-FULLNAME     PIC X(13).
                   05 G-C-LABEL        PIC X(25).
                   05 G-GRADE          PIC 99V99.
                   
       01  WS-BUFFER   PIC X(03) VALUE SPACE.
           88  WS-VALUE-NOT-PRESENT VALUE 'Y'.

       01  WS-PNT.
           03 WS-PNT-NBR      PIC Z9.
           03 WS-PNT-GRADE    PIC Z9,99.
           03 WS-PNT-COEF     PIC 9,9.
           03 WS-PNT-AST      PIC X(50).
           03 WS-PNT-STRING   PIC X(50).
       
       01  WS-MOY-ENCOURS     PIC 999V99.
       01  WS-ED-MOY-ENCOURS  PIC Z9,99.
       01  WS-NOTE-POND   PIC 99V99.           

       01   WS-POS-DEPART     PIC 9(03).
      
       PROCEDURE DIVISION.
       1000-MAIN-START.
           PERFORM 7000-READ-START 
           THRU 7000-READ-END. 

           DISPLAY G-S-FULLNAME(1).
           DISPLAY G-S-FULLNAME(10).


           PERFORM 7100-WRITE-START 
           THRU 7100-WRITE-END.
       1000-MAIN-END.
           STOP RUN.
      ****************************************************************** 
       7000-READ-START.
           OPEN INPUT F-INPUT.          

           IF NOT F-INPUT-STATUS-OK
               DISPLAY 'ERROR INPUT FILE'
               GO TO 7000-READ-END
           END-IF.

           PERFORM UNTIL F-INPUT-STATUS-EOF
               READ F-INPUT
               IF F-INPUT-STATUS-EOF
                   GO TO 7000-READ-END
               END-IF
               EVALUATE REC-F-INPUT-2
                   WHEN '01'
                       PERFORM 8010-HANDLE-STUDENT-START 
                           THRU 8010-HANDLE-STUDENT-END
                   WHEN '02'
                       PERFORM  8020-HANDLE-COURSE-START 
                           THRU 8020-HANDLE-COURSE-END
                       PERFORM 8030-HANDLE-GRADE-START
                           THRU 8030-HANDLE-GRADE-END
           END-PERFORM.

       7000-READ-END.
           SUBTRACT GRADE-LGTH   FROM 1 GIVING GRADE-LGTH.
           SUBTRACT COURSE-LGTH  FROM 1 GIVING COURSE-LGTH.
           SUBTRACT STUDENT-LGTH FROM 1 GIVING STUDENT-LGTH.
           CLOSE F-INPUT.  
      ******************************************************************
       7100-WRITE-START.
           OPEN OUTPUT F-OUTPUT.
           PERFORM 9010-HEADER-START   THRU 9010-HEADER-END.

           PERFORM 9030-BODY-START     THRU 9030-BODY-END.

           PERFORM 9020-FOOTER-START   THRU 9020-FOOTER-END.
       7100-WRITE-END.
           CLOSE F-OUTPUT.
      ******************************************************************  
       8010-HANDLE-STUDENT-START.
           MOVE R-S-FIRSTNAME  TO S-FIRSTNAME(STUDENT-LGTH).
           MOVE R-S-LASTNAME   TO S-LASTNAME(STUDENT-LGTH).
           MOVE R-S-AGE        TO S-AGE(STUDENT-LGTH).
            
            DISPLAY 'R-S-FIRSTNAME : ' R-S-FIRSTNAME.
            DISPLAY 'R-S-LASTNAME : ' R-S-LASTNAME.
           ADD 1 TO STUDENT-LGTH.           
       8010-HANDLE-STUDENT-END.
      ****************************************************************** 
       8020-HANDLE-COURSE-START.
           INITIALIZE WS-BUFFER.
           SET IDX-COURSE TO 1.

           SEARCH COURSE VARYING IDX-COURSE
               WHEN C-LABEL(IDX-COURSE) = R-C-LABEL
                   GO TO 8020-HANDLE-COURSE-END 
           END-SEARCH.

               MOVE R-C-COEF   TO C-COEFF(COURSE-LGTH).
               MOVE R-C-LABEL  TO C-LABEL(COURSE-LGTH).
               ADD 1 TO COURSE-LGTH.
           
       8020-HANDLE-COURSE-END.
      ****************************************************************** 
       8030-HANDLE-GRADE-START.
           STRING 
               S-FIRSTNAME(STUDENT-LGTH - 1) 
               S-LASTNAME(STUDENT-LGTH - 1) 
               DELIMITED BY SIZE 
           INTO G-S-FULLNAME(GRADE-LGTH).

      *      DISPLAY 'G-S-FULLNAME : ' G-S-FULLNAME.

           MOVE R-C-LABEL TO G-C-LABEL(GRADE-LGTH).
           MOVE R-C-GRADE TO G-GRADE(GRADE-LGTH).

           ADD 1 TO GRADE-LGTH.
           
       8030-HANDLE-GRADE-END.
      ****************************************************************** 
       9010-HEADER-START.
           INITIALIZE REC-F-OUTPUT.
           MOVE ALL '*' TO REC-F-OUTPUT(1:170).
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT.
           MOVE '*' TO REC-F-OUTPUT(1:1).
           MOVE '*' TO REC-F-OUTPUT(170:1).
           MOVE 'BULLETIN DE NOTES' TO REC-F-OUTPUT(85:20).
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT.
           MOVE ALL '*' TO REC-F-OUTPUT(1:170).
           WRITE REC-F-OUTPUT.           
       9010-HEADER-END.
      ****************************************************************** 
       9020-FOOTER-START.
           INITIALIZE REC-F-OUTPUT.
           MOVE ALL '*' TO REC-F-OUTPUT(1:170).
           WRITE REC-F-OUTPUT.
           INITIALIZE REC-F-OUTPUT.
           MOVE '*' TO REC-F-OUTPUT(1:1).
           MOVE '*' TO REC-F-OUTPUT(170:1).

             WRITE REC-F-OUTPUT FROM WS-PNT-AST.

           PERFORM VARYING IDX-COURSE FROM 1 BY 1 UNTIL 
           IDX-COURSE >  COURSE-LGTH
               INITIALIZE WS-PNT-STRING
               INITIALIZE WS-PNT-NBR
               INITIALIZE WS-PNT-COEF

               MOVE C-COEFF(IDX-COURSE) TO WS-PNT-COEF 
              STRING FUNCTION TRIM (G-S-FULLNAME (IDX-GRADE))
              
               SPACE SPACE SPACE 
               "=>coefficients:" SPACE FUNCTION TRIM(WS-PNT-COEF) 
               SPACE "matière: " SPACE C-LABEL(IDX-COURSE)
               DELIMITED BY SIZE
               INTO WS-PNT-STRING

               WRITE REC-F-OUTPUT FROM WS-PNT-STRING
           END-PERFORM.

           WRITE REC-F-OUTPUT FROM WS-PNT-AST.
           INITIALIZE REC-F-OUTPUT.
           MOVE ALL '*' TO REC-F-OUTPUT(1:170).
           WRITE REC-F-OUTPUT.
           INITIALIZE REC-F-OUTPUT.
           MOVE '*' TO REC-F-OUTPUT(1:1).
           MOVE '*' TO REC-F-OUTPUT(170:1).
           MOVE 'NOMBRE DE :' TO REC-F-OUTPUT(75:9).
           INITIALIZE REC-F-OUTPUT(85:9).
           MOVE 'ELEVES : '   TO REC-F-OUTPUT(85:9).
           MOVE STUDENT-LGTH TO WS-PNT-NBR.
           MOVE FUNCTION TRIM(WS-PNT-NBR) TO REC-F-OUTPUT(93:2).
           WRITE REC-F-OUTPUT.
           INITIALIZE REC-F-OUTPUT(85:9).
           MOVE 'NOTES : '     TO REC-F-OUTPUT(85:9).
           MOVE GRADE-LGTH TO WS-PNT-NBR.
           MOVE FUNCTION TRIM(WS-PNT-NBR) TO REC-F-OUTPUT(93:2).
           WRITE REC-F-OUTPUT.
           INITIALIZE REC-F-OUTPUT(85:9).
           MOVE 'COURS : '     TO REC-F-OUTPUT(85:9).
           MOVE COURSE-LGTH TO WS-PNT-NBR.
           MOVE FUNCTION TRIM(WS-PNT-NBR) TO REC-F-OUTPUT(93:2).
           WRITE REC-F-OUTPUT.
           INITIALIZE REC-F-OUTPUT.
           MOVE ALL '*' TO REC-F-OUTPUT(1:170).
           WRITE REC-F-OUTPUT.
           INITIALIZE REC-F-OUTPUT.
           MOVE '*' TO REC-F-OUTPUT(1:1).
           MOVE '*' TO REC-F-OUTPUT(170:1).
           MOVE 'FIN DU RAPPORT' TO REC-F-OUTPUT(85:20).
           WRITE REC-F-OUTPUT.   
           INITIALIZE REC-F-OUTPUT.
           MOVE ALL '*' TO REC-F-OUTPUT(1:170).
           WRITE REC-F-OUTPUT.   
       9020-FOOTER-END.
      ****************************************************************** 
       9030-BODY-START. 
           INITIALIZE REC-F-OUTPUT.
           MOVE ALL '_' TO REC-F-OUTPUT(1:170).
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT.
           MOVE '|' TO REC-F-OUTPUT(1:1).
           MOVE '|' TO REC-F-OUTPUT(170:1).
           MOVE 'ELEVE' TO REC-F-OUTPUT(3:14).
           MOVE '|'  TO REC-F-OUTPUT(18:1).
           MOVE 'MOYENNE GENERALE' TO REC-F-OUTPUT(19:16).
           MOVE '|' TO REC-F-OUTPUT(35:1).
           MOVE 37 TO WS-POS-DEPART.

           PERFORM VARYING IDX-COURSE FROM 1 BY 1
                UNTIL IDX-COURSE > COURSE-LGTH
                MOVE C-LABEL(IDX-COURSE) 
                TO REC-F-OUTPUT(WS-POS-DEPART:25)
                ADD 20 TO WS-POS-DEPART
                MOVE '|' TO REC-F-OUTPUT (WS-POS-DEPART:1)
                ADD 2 TO WS-POS-DEPART
            END-PERFORM
            WRITE REC-F-OUTPUT.
               
           INITIALIZE REC-F-OUTPUT.
           MOVE ALL '_' TO REC-F-OUTPUT(1:170).
           WRITE REC-F-OUTPUT.
        
           INITIALIZE REC-F-OUTPUT.
           MOVE 37 TO WS-POS-DEPART.

           MOVE 1 TO IDX-GRADE
           MOVE G-S-FULLNAME(IDX-GRADE) TO REC-F-OUTPUT(3:13)

            PERFORM  VARYING IDX-GRADE FROM 1 BY 1
            UNTIL IDX-GRADE > GRADE-LGTH

           IF IDX-GRADE NOT = 1
            IF G-S-FULLNAME(IDX-GRADE) NOT = G-S-FULLNAME(IDX-GRADE - 1)
               PERFORM SUITE-CALCUL-MOY
               THRU    SUITE-CALCUL-MOY-END
             ELSE IF IDX-GRADE = GRADE-LGTH
               PERFORM CALCUL-MOYENNE
               THRU CALCUL-MOYENNE-END
               PERFORM AFFICHAGE-NOTE
               THRU AFFICHAGE-NOTE-END
               PERFORM SUITE-CALCUL-MOY
               THRU    SUITE-CALCUL-MOY-END
             END-IF
            END-IF
               PERFORM CALCUL-MOYENNE
               THRU CALCUL-MOYENNE-END
               PERFORM AFFICHAGE-NOTE
               THRU AFFICHAGE-NOTE-END
            END-IF

           END-PERFORM.
          
        9030-BODY-END.
      ******************************************************************

       CALCUL-MOYENNE.
      *           RECHERCHE DU COEFFICIENT POUR LA MATIERE ACTUELLE
              PERFORM VARYING IDX-COURSE FROM 1 BY 1
              UNTIL IDX-COURSE >= COURSE-LGTH
                 IF G-C-LABEL(IDX-GRADE) = C-LABEL(IDX-COURSE)
      *     COMPUTE WS-MOY-ENCOURS = 
      *           C-COEFF(IDX-COURSE) * G-GRADE(IDX-GRADE)
                  MULTIPLY C-COEFF(IDX-COURSE) 
                           BY G-GRADE(IDX-GRADE) GIVING WS-NOTE-POND
                DISPLAY 'NOM : ' G-S-FULLNAME(IDX-GRADE)
                DISPLAY 'G-GRADE : ' G-GRADE(IDX-GRADE)
                DISPLAY 'WS-NOTE-POND ETAPE 1 : ' WS-NOTE-POND

                  ADD WS-NOTE-POND 
                           TO WS-MOY-ENCOURS GIVING WS-MOY-ENCOURS
                DISPLAY 'WS-MOY-ENCOURS ETAPE 1 : ' WS-MOY-ENCOURS
                END-IF
              END-PERFORM.
    
       CALCUL-MOYENNE-END.
      ****************************************************************** 
       AFFICHAGE-NOTE.
             MOVE G-GRADE(IDX-GRADE) TO WS-PNT-GRADE
             MOVE WS-PNT-GRADE TO REC-F-OUTPUT(WS-POS-DEPART: 25)
             ADD 20 TO WS-POS-DEPART
             MOVE '|' TO REC-F-OUTPUT (WS-POS-DEPART:1)
             ADD 2 TO WS-POS-DEPART.

       AFFICHAGE-NOTE-END.
      ****************************************************************** 
       SUITE-CALCUL-MOY.

           DIVIDE WS-MOY-ENCOURS BY COURSE-LGTH GIVING 
                      WS-MOY-ENCOURS
                DISPLAY 'WS-MOY-ENCOURS ETAPE 2 : ' WS-MOY-ENCOURS
      *       ON MET LA MOY FINALE DANS LA LIGNE REC-F-OUTPUT
               MOVE WS-MOY-ENCOURS TO WS-ED-MOY-ENCOURS
                DISPLAY 'WS-ED-MOY-ENCOURS ETAPE 3 : ' WS-ED-MOY-ENCOURS
               MOVE WS-ED-MOY-ENCOURS TO REC-F-OUTPUT(19:5)
               WRITE REC-F-OUTPUT
               INITIALIZE REC-F-OUTPUT
               MOVE 37 TO WS-POS-DEPART
               MOVE ZERO TO WS-MOY-ENCOURS
               MOVE G-S-FULLNAME(IDX-GRADE) TO REC-F-OUTPUT(3:13).
       SUITE-CALCUL-MOY-END.
      ******************************************************************
      