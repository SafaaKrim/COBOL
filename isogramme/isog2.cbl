       IDENTIFICATION DIVISION.
       PROGRAM-ID. IsogramCheck.
       AUTHOR. K-SAFAA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TABLE-MOTS OCCURS 5 TIMES.
         03 WS-MOT           PIC X(25).
         03 WS-MOT-UPPER     PIC X(25).
         03  WS-TABLE-COUNTER OCCURS 26 TIMES.
             05 WS-COUNT-CHAR PIC 9(01).
         03  WS-ISO           PIC X(01) VALUE "N".
           88 WS-ISO-YES                VALUE "Y".
           88 WS-ISO-NO                 VALUE "N".


       01  WS-ALPHABET PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".

       01  WS-MOT-I                PIC 9(02) VALUE 1.
       01  WS-COUNT-CHAR-I         PIC 9(02).
       01  WS-TABLE-MOTS-LENGTH    PIC 9(02).
       01  WS-STOP                 PIC X(01) VALUE "Y".

       PROCEDURE DIVISION.
           PERFORM START-TEXT     
           THRU END-TEXT.

           PERFORM START-CHECK-ISO 
           THRU END-CHECK-ISO.

           PERFORM START-INSPECT-WORD 
           THRU END-INSPECT-WORD

           STOP RUN.



       START-TEXT.
           PERFORM UNTIL WS-STOP EQUAL "N" OR WS-MOT-I > 5
               DISPLAY SPACE
               DISPLAY "Saisi un mot :" SPACE WITH NO ADVANCING
               ACCEPT WS-MOT(WS-MOT-I)
      
               MOVE FUNCTION UPPER-CASE(WS-MOT(WS-MOT-I)) 
               TO WS-MOT-UPPER(WS-MOT-I)

               ADD 1 TO WS-MOT-I

               IF WS-MOT-I <= 5
                   DISPLAY "Continuer (Y/N):" SPACE WITH NO ADVANCING
                   ACCEPT WS-STOP
               ELSE    
                   DISPLAY "erreur"
               END-IF

           END-PERFORM.

           DISPLAY SPACE.

       END-TEXT.

       START-INSPECT-WORD.
           PERFORM VARYING WS-COUNT-CHAR-I FROM 1 BY 1 
                   UNTIL WS-COUNT-CHAR-I > 26

               INSPECT WS-MOT-UPPER(WS-MOT-I)
               TALLYING WS-COUNT-CHAR(WS-MOT-I, WS-COUNT-CHAR-I) 
               FOR ALL WS-ALPHABET(WS-COUNT-CHAR-I:1)
               
               IF WS-COUNT-CHAR(WS-MOT-I, WS-COUNT-CHAR-I) > 1
                   SET WS-ISO-YES(WS-MOT-I) TO TRUE
               END-IF
           END-PERFORM.
       END-INSPECT-WORD.

       START-CHECK-ISO.
           MOVE WS-MOT-I TO WS-TABLE-MOTS-LENGTH.
           PERFORM VARYING WS-MOT-I FROM 1 BY 1 
                   UNTIL WS-MOT-I >= WS-TABLE-MOTS-LENGTH

               IF WS-ISO-YES(WS-MOT-I)
                  DISPLAY FUNCTION TRIM(WS-MOT(WS-MOT-I))
                  SPACE "est un isogramme."
               ELSE
                  DISPLAY FUNCTION TRIM(WS-MOT(WS-MOT-I))
                  SPACE "n'est pas un isogramme."
               END-IF
           END-PERFORM.
       END-CHECK-ISO.
       