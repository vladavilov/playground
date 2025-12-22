       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQLDEMO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-ID PIC 9(4) VALUE 1.
       01 WS-NAME PIC X(10).
       PROCEDURE DIVISION.
           EXEC SQL
              SELECT NAME
                INTO :WS-NAME
                FROM CUSTOMER
               WHERE ID = :WS-ID
           END-EXEC.
           STOP RUN.
