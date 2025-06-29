      *<modified_cobol_program>
      *****************************************************************
      * Program     : COUSR01L.CBL
      * Application : CardDemo
      * Type        : CICS COBOL RPC Program
      * Function    : Add a new Regular/Admin user to USRSEC file
      ******************************************************************
      * Copyright Amazon.com, Inc. or its affiliates.
      * All Rights Reserved.
      *
      * Licensed under the Apache License, Version 2.0 (the "License").
      * You may not use this file except in compliance with the License.
      * You may obtain a copy of the License at
      *
      *    http://www.apache.org/licenses/LICENSE-2.0
      *
      * Unless required by applicable law or agreed to in writing,
      * software distributed under the License is distributed on an
      * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
      * either express or implied. See the License for the specific
      * language governing permissions and limitations under the License
      ******************************************************************
      * Modification History
      * 2025-05-09 - Converted from screen-based program COUSR01C
      *              to RPC program for API integration
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COUSR01L.
       AUTHOR.     AWS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
      *----------------------------------------------------------------*
      *                     WORKING STORAGE SECTION
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
         05 WS-PGMNAME                 PIC X(08) VALUE 'COUSR01L'.
         05 WS-USRSEC-FILE             PIC X(08) VALUE 'USRSEC  '.
         05 WS-RESP-CD                 PIC S9(09) COMP VALUE ZEROS.
         05 WS-REAS-CD                 PIC S9(09) COMP VALUE ZEROS.
         05 WS-RESP-MSG                PIC X(80) VALUE SPACES.

         05 WS-RESP-CD-DISP            PIC 9(09) VALUE ZEROS.
         05 WS-REAS-CD-DISP            PIC 9(09) VALUE ZEROS.
         05 WS-RPC-RESP-CD             PIC S9(04) COMP VALUE ZEROS.
           88 RPC-RESP-OK                        VALUE 0.
           88 RPC-RESP-EMPTY-FNAME               VALUE 1.
           88 RPC-RESP-EMPTY-LNAME               VALUE 2.
           88 RPC-RESP-EMPTY-USERID              VALUE 3.
           88 RPC-RESP-EMPTY-PASSWORD            VALUE 4.
           88 RPC-RESP-EMPTY-USERTYPE            VALUE 5.
           88 RPC-RESP-DUPLICATE-USER            VALUE 6.
           88 RPC-RESP-WRITE-ERROR               VALUE 7.


       COPY CSUSR01Y.

      *----------------------------------------------------------------*
      *                        LINKAGE SECTION
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01 DFHCOMMAREA.
          05 LK-INPUT-PARMS.
             10 LK-USER-FNAME             PIC X(20).
             10 LK-USER-LNAME             PIC X(20).
             10 LK-USER-ID                PIC X(08).
             10 LK-USER-PASSWORD          PIC X(08).
             10 LK-USER-TYPE              PIC X(01).
                88 LK-USRTYP-ADMIN        VALUE 'A'.
                88 LK-USRTYP-USER         VALUE 'U'.

          05 LK-OUTPUT-PARMS.
             10 LK-RESP-CODE              PIC S9(04) COMP.
             10 LK-RESP-MSG               PIC X(80).

      *----------------------------------------------------------------*
      *                      PROCEDURE DIVISION
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

       MAIN-PARA.


           MOVE ZEROS TO WS-RPC-RESP-CD
           MOVE SPACES TO WS-RESP-MSG
           MOVE ZEROS TO WS-RESP-CD
           MOVE ZEROS TO WS-REAS-CD

           PERFORM PROCESS-INPUT-PARMS
           MOVE WS-RPC-RESP-CD TO LK-RESP-CODE
           MOVE WS-RESP-MSG TO LK-RESP-MSG

           GOBACK.

      *----------------------------------------------------------------*
      *                      PROCESS-INPUT-PARMS -> Enter
      *----------------------------------------------------------------*
       PROCESS-INPUT-PARMS.


           EVALUATE TRUE
               WHEN LK-USER-FNAME = SPACES OR LOW-VALUES
                   SET RPC-RESP-EMPTY-FNAME TO TRUE
                   MOVE 'First Name cannot be empty' TO WS-RESP-MSG
               WHEN LK-USER-LNAME = SPACES OR LOW-VALUES
                   SET RPC-RESP-EMPTY-LNAME TO TRUE
                   MOVE 'Last Name cannot be empty' TO WS-RESP-MSG
               WHEN LK-USER-ID = SPACES OR LOW-VALUES
                   SET RPC-RESP-EMPTY-USERID TO TRUE
                   MOVE 'User ID cannot be empty' TO WS-RESP-MSG
               WHEN LK-USER-PASSWORD = SPACES OR LOW-VALUES
                   SET RPC-RESP-EMPTY-PASSWORD TO TRUE
                   MOVE 'Password cannot be empty' TO WS-RESP-MSG
               WHEN LK-USER-TYPE = SPACES OR LOW-VALUES
                   SET RPC-RESP-EMPTY-USERTYPE TO TRUE
                   MOVE 'User Type cannot be empty' TO WS-RESP-MSG
               WHEN LK-USER-TYPE NOT = 'A' AND LK-USER-TYPE NOT = 'U'
                   SET RPC-RESP-EMPTY-USERTYPE TO TRUE
                   MOVE 'User Type must be A or U' TO WS-RESP-MSG
           END-EVALUATE.

           IF RPC-RESP-OK
              MOVE LK-USER-ID TO SEC-USR-ID
              MOVE LK-USER-FNAME TO SEC-USR-FNAME
              MOVE LK-USER-LNAME TO SEC-USR-LNAME
              MOVE LK-USER-PASSWORD TO SEC-USR-PWD
              MOVE LK-USER-TYPE TO SEC-USR-TYPE
              MOVE SPACES TO SEC-USR-FILLER
              PERFORM WRITE-USER-SEC-FILE
           END-IF.

      *----------------------------------------------------------------*
      *                      WRITE-USER-SEC-FILE
      *----------------------------------------------------------------*
       WRITE-USER-SEC-FILE.

           EXEC CICS WRITE
                DATASET   (WS-USRSEC-FILE)
                FROM      (SEC-USER-DATA)
                LENGTH    (LENGTH OF SEC-USER-DATA)
                RIDFLD    (SEC-USR-ID)
                KEYLENGTH (LENGTH OF SEC-USR-ID)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC.

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   SET RPC-RESP-OK TO TRUE
                   STRING 'User '     DELIMITED BY SIZE
                          SEC-USR-ID  DELIMITED BY SPACE
                          ' has been added successfully'
                           DELIMITED BY SIZE
                     INTO WS-RESP-MSG
               WHEN DFHRESP(DUPKEY)
               WHEN DFHRESP(DUPREC)
                   SET RPC-RESP-DUPLICATE-USER TO TRUE
                   STRING 'User ID '  DELIMITED BY SIZE
                          SEC-USR-ID  DELIMITED BY SPACE
                          ' already exists...' DELIMITED BY SIZE
                     INTO WS-RESP-MSG
               WHEN OTHER
                   SET RPC-RESP-WRITE-ERROR TO TRUE
                   MOVE WS-RESP-CD TO WS-RESP-CD-DISP
                   MOVE WS-REAS-CD TO WS-REAS-CD-DISP
                   STRING 'Error adding user. RESP: ' DELIMITED BY SIZE
                          WS-RESP-CD-DISP            DELIMITED BY SIZE
                          ' RESP2: '                  DELIMITED BY SIZE
                          WS-REAS-CD-DISP            DELIMITED BY SIZE
                     INTO WS-RESP-MSG
           END-EVALUATE.
      *</modified_cobol_program>