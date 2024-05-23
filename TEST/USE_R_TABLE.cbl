        IDENTIFICATION DIVISION.            
        PROGRAM-ID. TABLE-EXAMPLE.
        DATA DIVISION.
           WORKING-STORAGE SECTION.
      *Creé DEUX tablau      
       01 WS-TABLE-1.
           05 WS-ID             PIC 9(5).
           05 WS-CITY           PIC X(8).
           05 WS-STBEET         PIC X(20). 
           05 WS-GSM-NUBER      PIC X(14).
           05 WS-LEST-NAME      PIC 9(10).
           05 WS-FIRST-NAME     PIC X(10).

       01 WS-TABLE-2.
           05 WS-ID.    
             10 WS-key          PIC X(3).
             10 WS-NUM          PIC 9(5).
           05 WS-GODER          PIC 9(5).
           05 WS-NAME           PIC X(3).
           05 WS-LOOF           PIC X(3).
           05 WS-AGE            PIC 9(3).
           05 WS-MASTER.
              10 WS-LS-NAME     PIC X(10).
              10 WS-LEST-NAME   PIC X(10).
              10 WS-ID          PIC 9(5).
.           
       
        PROCEDURE DIVISION.

            DISPLAY WS-TABLE-1.
            DISPLAY WS-TABLE-2.

       STOP RUN.  

