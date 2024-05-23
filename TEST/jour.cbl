        IDENTIFICATION DIVISION.
        PROGRAM-ID. BONJOUR.
        DATA DIVISION.
        WORKING-STORAGE SECTION.

           01 WS-DATEJOUR PIC 9(8).
      
           01 WS-CURRENT-DATE-FIELDS.
              05  WS-CURRENT-DATE.
                 10  WS-CURRENT-YEAR    PIC  9(4).
                 10  FILLER             PIC  X(1) VALUE "/".
                 10  WS-CURRENT-MONTH   PIC  9(2).
                 10  FILLER             PIC  X(1) VALUE "/".
                 10  WS-CURRENT-DAY     PIC  9(2).
             05  WS-CURRENT-TIME.
                 10  WS-CURRENT-HOUR    PIC  9(2).
                 10  FILLER             PIC  X(1) VALUE ":".
                 10  WS-CURRENT-MINUTE  PIC  9(2).
                 10  FILLER             PIC  X(1) VALUE ":".
                 10  WS-CURRENT-SECOND  PIC  9(2).
                 
             
        PROCEDURE DIVISION.
           
           MOVE FUNCTION CURRENT-DATE(1:4)  TO WS-CURRENT-YEAR.
           MOVE FUNCTION CURRENT-DATE(5:2)  TO WS-CURRENT-MONTH.
           MOVE FUNCTION CURRENT-DATE(7:2)  TO WS-CURRENT-DAY.
           MOVE FUNCTION CURRENT-DATE(9:2)  TO WS-CURRENT-HOUR.
           MOVE FUNCTION CURRENT-DATE(10:2) TO WS-CURRENT-MINUTE.
           MOVE FUNCTION CURRENT-DATE(12:2) TO WS-CURRENT-SECOND.

           DISPLAY '********************************************'. 
           DISPLAY '                 BIENVENUE                  '.
           DISPLAY '********************************************'. 

           DISPLAY  'La date du jour est : ' WS-CURRENT-DATE. 
           DISPLAY  'Heure exacte: ' WS-CURRENT-TIME.

           DISPLAY '********************************************'. 
           DISPLAY '               Bonne journée                '.
           DISPLAY '********************************************'. 
  
        STOP RUN.
