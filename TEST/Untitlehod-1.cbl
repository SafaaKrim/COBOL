        IDENTIFICATION DIVISION.
        PROGRAM-ID. HelloWorld.

        ENVIRONMENT DIVISION.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-MESSAGE PIC X(20) VALUE "Hello World".

           PROCEDURE DIVISION.
          DISPLAY WS-MESSAGE.
         STOP RUN.


        