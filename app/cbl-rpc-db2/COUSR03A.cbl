******************************************************************
      * Program     : COUSR03A.CBL
      * Application : CardDemo
      * Type        : COBOL RPC Program
      * Function    : Lookup/Delete user in USERSEC DB2 table
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COUSR03A.
       AUTHOR.     AWS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
      *----------------------------------------------------------------*
      *                     WORKING STORAGE SECTION
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
         05 WS-PGMNAME             PIC X(08) VALUE 'COUSR03A'.
         05 WS-ERR-FLG             PIC X(01) VALUE 'N'.
           88 ERR-FLG-ON                   VALUE 'Y'.
           88 ERR-FLG-OFF                  VALUE 'N'.

      *----------------------------------------------------------------*
      *                     DB2 SQLCA AND VARIABLES
      *----------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA END-EXEC.

       01 WS-SQL-STATUS                PIC S9(09) COMP VALUE ZEROS.

      * User Security Record Structure
       01 SEC-USER-DATA.
         05 SEC-USR-ID             PIC X(08).
         05 SEC-USR-FNAME          PIC X(20).
         05 SEC-USR-LNAME          PIC X(20).
         05 SEC-USR-PWD            PIC X(08).
         05 SEC-USR-TYPE           PIC X(01).

      *----------------------------------------------------------------*
      *                        LINKAGE SECTION
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01 DFHCOMMAREA.
           05 LK-OPERATION               PIC X(01).
               88 LK-OP-LOOKUP           VALUE 'L'.
               88 LK-OP-DELETE           VALUE 'D'.
           05 LK-INPUT-PARMS.
              10 LK-IN-USER-ID           PIC X(08).
           05 LK-OUTPUT-STATUS.
              10 LK-OUT-RETURN-CODE      PIC 9(02).
                 88 RC-SUCCESS           VALUE 00.
                 88 RC-NOT-FOUND         VALUE 01.
                 88 RC-VALIDATION-ERROR  VALUE 10.
                 88 RC-DATABASE-ERROR    VALUE 99.
              10 LK-OUT-MESSAGE          PIC X(80).
           05 LK-OUTPUT-USER.
              10 LK-OUT-USER-ID          PIC X(08).
              10 LK-OUT-USER-FNAME       PIC X(20).
              10 LK-OUT-USER-LNAME       PIC X(20).
              10 LK-OUT-USER-TYPE        PIC X(01).

      *----------------------------------------------------------------*
      *                       PROCEDURE DIVISION
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

       MAIN-PARA.

           INITIALIZE LK-OUTPUT-STATUS
                      LK-OUTPUT-USER

           SET ERR-FLG-OFF TO TRUE
           SET RC-SUCCESS TO TRUE

           MOVE SPACES TO LK-OUT-MESSAGE

      * Process based on operation flag from calling program
           EVALUATE TRUE
               WHEN LK-OP-LOOKUP
                   PERFORM PROCESS-LOOKUP
               WHEN LK-OP-DELETE
                   PERFORM PROCESS-DELETE
               WHEN OTHER
                   SET RC-VALIDATION-ERROR TO TRUE
                   MOVE 'Invalid operation code' TO LK-OUT-MESSAGE
           END-EVALUATE

           GOBACK.

      *----------------------------------------------------------------*
      *                      PROCESS-LOOKUP
      *----------------------------------------------------------------*
       PROCESS-LOOKUP.

           IF LK-IN-USER-ID = SPACES OR LOW-VALUES
               SET RC-VALIDATION-ERROR TO TRUE
               MOVE 'User ID cannot be empty for lookup'
                    TO LK-OUT-MESSAGE
           ELSE
               MOVE LK-IN-USER-ID TO SEC-USR-ID
               PERFORM READ-USER-SEC-TABLE-LOOKUP

               IF NOT ERR-FLG-ON
                   MOVE SEC-USR-ID    TO LK-OUT-USER-ID
                   MOVE SEC-USR-FNAME TO LK-OUT-USER-FNAME
                   MOVE SEC-USR-LNAME TO LK-OUT-USER-LNAME
                   MOVE SEC-USR-TYPE  TO LK-OUT-USER-TYPE
                   MOVE 'User data retrieved successfully'
                        TO LK-OUT-MESSAGE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      *                      PROCESS-DELETE
      *----------------------------------------------------------------*
       PROCESS-DELETE.

           IF LK-IN-USER-ID = SPACES OR LOW-VALUES
               SET RC-VALIDATION-ERROR TO TRUE
               MOVE 'User ID cannot be empty for delete'
                    TO LK-OUT-MESSAGE
           ELSE
               MOVE LK-IN-USER-ID TO SEC-USR-ID
               PERFORM READ-USER-SEC-TABLE-DELETE

               IF NOT ERR-FLG-ON
      * Copy user details to output before deletion
                   MOVE SEC-USR-ID TO LK-OUT-USER-ID
                   MOVE SEC-USR-FNAME TO LK-OUT-USER-FNAME
                   MOVE SEC-USR-LNAME TO LK-OUT-USER-LNAME
                   MOVE SEC-USR-TYPE TO LK-OUT-USER-TYPE

      * Delete the user record
                   PERFORM DELETE-USER-SEC-TABLE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      *                      READ-USER-SEC-TABLE-LOOKUP
      *----------------------------------------------------------------*
       READ-USER-SEC-TABLE-LOOKUP.

           EXEC SQL
               SELECT USR_ID, USR_FNAME, USR_LNAME, USR_PWD, USR_TYPE
               INTO :SEC-USR-ID, :SEC-USR-FNAME, :SEC-USR-LNAME,
                    :SEC-USR-PWD, :SEC-USR-TYPE
               FROM USERSEC
               WHERE USR_ID = :SEC-USR-ID
           END-EXEC.

           MOVE SQLCODE TO WS-SQL-STATUS

           EVALUATE WS-SQL-STATUS
               WHEN 0
                   CONTINUE
               WHEN +100
                   SET ERR-FLG-ON TO TRUE
                   SET RC-NOT-FOUND TO TRUE
                   MOVE 'User ID not found' TO LK-OUT-MESSAGE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   SET RC-DATABASE-ERROR TO TRUE
                   MOVE 'Unable to lookup User' TO LK-OUT-MESSAGE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      READ-USER-SEC-TABLE-DELETE
      *----------------------------------------------------------------*
       READ-USER-SEC-TABLE-DELETE.

           EXEC SQL
               SELECT USR_ID, USR_FNAME, USR_LNAME, USR_PWD, USR_TYPE
               INTO :SEC-USR-ID, :SEC-USR-FNAME, :SEC-USR-LNAME,
                    :SEC-USR-PWD, :SEC-USR-TYPE
               FROM USERSEC
               WHERE USR_ID = :SEC-USR-ID
           END-EXEC.

           MOVE SQLCODE TO WS-SQL-STATUS

           EVALUATE WS-SQL-STATUS
               WHEN 0
                   CONTINUE
               WHEN +100
                   SET ERR-FLG-ON TO TRUE
                   SET RC-NOT-FOUND TO TRUE
                   MOVE 'User ID not found' TO LK-OUT-MESSAGE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   SET RC-DATABASE-ERROR TO TRUE
                   MOVE 'Unable to lookup User' TO LK-OUT-MESSAGE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      DELETE-USER-SEC-TABLE
      *----------------------------------------------------------------*
       DELETE-USER-SEC-TABLE.

           EXEC SQL
               DELETE FROM USERSEC
               WHERE USR_ID = :SEC-USR-ID
           END-EXEC.

           MOVE SQLCODE TO WS-SQL-STATUS

           EVALUATE WS-SQL-STATUS
               WHEN 0
                   EXEC SQL COMMIT END-EXEC
                   MOVE SPACES TO LK-OUT-MESSAGE
                   STRING 'User ' DELIMITED BY SIZE
                          SEC-USR-ID DELIMITED BY SPACE
                          ' has been deleted successfully'
                          DELIMITED BY SIZE
                          INTO LK-OUT-MESSAGE
               WHEN +100
                   EXEC SQL ROLLBACK END-EXEC
                   SET ERR-FLG-ON TO TRUE
                   SET RC-NOT-FOUND TO TRUE
                   MOVE 'User ID not found' TO LK-OUT-MESSAGE
               WHEN OTHER
                   EXEC SQL ROLLBACK END-EXEC
                   SET ERR-FLG-ON TO TRUE
                   SET RC-DATABASE-ERROR TO TRUE
                   MOVE 'Unable to delete User' TO LK-OUT-MESSAGE
           END-EVALUATE.