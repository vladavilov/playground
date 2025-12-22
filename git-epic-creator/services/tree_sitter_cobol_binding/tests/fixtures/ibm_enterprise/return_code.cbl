       IDENTIFICATION DIVISION.
       PROGRAM-ID. RETCODE.
       PROCEDURE DIVISION.
           MOVE 0 TO RETURN-CODE.
           IF RETURN-CODE = 0
               DISPLAY 'OK'
           END-IF
           GOBACK.
