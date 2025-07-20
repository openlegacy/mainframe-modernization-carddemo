******************************************************************
      * Program     : COCRDSLA.CBL
      * Application : CardDemo
      * Type        : COBOL RPC Program
      * Function    : Card lookup/read operations for DB2 integration
      * Description : Receives card search criteria and returns card data
      *               Handles both card number and account ID lookups
      *               DB2 version replacing COCRDSLL (VSAM)
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
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     COCRDSLA.
       AUTHOR.     AWS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
      *----------------------------------------------------------------*
      *                     WORKING STORAGE SECTION
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
         05 WS-PGMNAME                 PIC X(08) VALUE 'COCRDSLA'.
         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.
           88 ERR-FLG-ON                         VALUE 'Y'.
           88 ERR-FLG-OFF                        VALUE 'N'.
         05 WS-OPERATION               PIC X(01) VALUE SPACES.
           88 OPERATION-LOOKUP-CARD             VALUE 'L'.
           88 OPERATION-LOOKUP-ACCT             VALUE 'A'.

      * DB2 related variables
       01 WS-DB2-VARS.
         05 WS-SQLCODE                 PIC S9(09) COMP VALUE ZEROS.
         05 WS-TABLE-NAME              PIC X(08) VALUE 'CARDDAT'.
         05 WS-DB2-ERROR-MESSAGE.
           10 FILLER                   PIC X(12)
                                       VALUE 'DB2 Error: '.
           10 ERROR-OPNAME             PIC X(8)
                                       VALUE SPACES.
           10 FILLER                   PIC X(4)
                                       VALUE ' on '.
           10 ERROR-TABLE              PIC X(9)
                                       VALUE SPACES.
           10 FILLER                   PIC X(9)
                                       VALUE
                                       ' SQLCODE '.
           10 ERROR-SQLCODE            PIC X(10)
                                       VALUE SPACES.
           10 FILLER                   PIC X(5)
                                       VALUE SPACES.

      ******************************************************************
      * DB2 SQLCA
      ******************************************************************
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      ******************************************************************
      * DB2 HOST VARIABLE DECLARATIONS
      ******************************************************************
           EXEC SQL BEGIN DECLARE SECTION END-EXEC.

      * Host variables for CARDDAT table
       01  HV-CARD-VARIABLES.
           05  HV-CARD-NUM                    PIC X(16).
           05  HV-CARD-ACCT-ID                PIC S9(11) COMP-3.
           05  HV-CARD-CVV-CD                 PIC S9(3) COMP-3.
           05  HV-CARD-EMBOSSED-NAME          PIC X(50).
           05  HV-CARD-EXPIRAION-DATE         PIC X(10).
           05  HV-CARD-ACTIVE-STATUS          PIC X(1).

      * Host variables for search criteria
       01  HV-SEARCH-VARIABLES.
           05  HV-SEARCH-CARD-NUM             PIC X(16).
           05  HV-SEARCH-ACCT-ID              PIC S9(11) COMP-3.

           EXEC SQL END DECLARE SECTION END-EXEC.

      *----------------------------------------------------------------*
      *                        LINKAGE SECTION
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01  DFHCOMMAREA.
           05  LK-OPERATION               PIC X(01).
               88  LK-OP-LOOKUP-CARD      VALUE 'L'.
               88  LK-OP-LOOKUP-ACCT      VALUE 'A'.
           05  LK-INPUT-CRITERIA.
               10  LK-IN-ACCT-ID             PIC 9(11).
               10  LK-IN-CARD-NUM            PIC 9(16).
           05  LK-OUTPUT-STATUS.
               10  LK-OUT-RETURN-CODE        PIC 9(02).
                   88  RC-SUCCESS             VALUE 00.
                   88  RC-NOT-FOUND           VALUE 01.
                   88  RC-VALIDATION-ERROR    VALUE 10.
                   88  RC-DATABASE-ERROR      VALUE 99.
               10  LK-OUT-MESSAGE            PIC X(80).
           05  LK-OUTPUT-CARD-DATA.
               10  LK-OUT-CARD-NUM           PIC X(16).
               10  LK-OUT-CARD-ACCT-ID       PIC 9(11).
               10  LK-OUT-CARD-CVV-CD        PIC 9(03).
               10  LK-OUT-CARD-EMBOSSED-NAME PIC X(50).
               10  LK-OUT-CARD-EXPIRY-DATE   PIC X(10).
               10  LK-OUT-CARD-STATUS        PIC X(01).

      *Dataset layouts
      *CARD RECORD LAYOUT
       COPY CVACT02Y.

      *----------------------------------------------------------------*
      *                       PROCEDURE DIVISION
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

       MAIN-PARA.
           INITIALIZE LK-OUTPUT-STATUS
                      LK-OUTPUT-CARD-DATA
                      HV-CARD-VARIABLES
                      HV-SEARCH-VARIABLES

           SET ERR-FLG-OFF TO TRUE
           SET RC-SUCCESS TO TRUE

           MOVE SPACES TO LK-OUT-MESSAGE

      * Process based on operation flag from calling program
           EVALUATE TRUE
               WHEN LK-OP-LOOKUP-CARD
                   PERFORM PROCESS-CARD-LOOKUP
               WHEN LK-OP-LOOKUP-ACCT
                   PERFORM PROCESS-ACCT-LOOKUP
               WHEN OTHER
                   SET RC-VALIDATION-ERROR TO TRUE
                   MOVE 'Invalid operation code' TO LK-OUT-MESSAGE
           END-EVALUATE

           GOBACK.

      *----------------------------------------------------------------*
      *                      PROCESS-CARD-LOOKUP
      *----------------------------------------------------------------*
       PROCESS-CARD-LOOKUP.

           IF LK-IN-CARD-NUM = ZEROS
               SET RC-VALIDATION-ERROR TO TRUE
               MOVE 'Card number cannot be zero for lookup'
                    TO LK-OUT-MESSAGE
           ELSE
               MOVE LK-IN-CARD-NUM TO HV-SEARCH-CARD-NUM
               PERFORM READ-CARD-BY-CARDNUM
               IF NOT ERR-FLG-ON
                   PERFORM POPULATE-CARD-OUTPUT-DATA
                   MOVE 'Card data retrieved successfully'
                        TO LK-OUT-MESSAGE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      *                      PROCESS-ACCT-LOOKUP
      *----------------------------------------------------------------*
       PROCESS-ACCT-LOOKUP.

           IF LK-IN-ACCT-ID = ZEROS
               SET RC-VALIDATION-ERROR TO TRUE
               MOVE 'Account ID cannot be zero for lookup'
                    TO LK-OUT-MESSAGE
           ELSE
               MOVE LK-IN-ACCT-ID TO HV-SEARCH-ACCT-ID
               PERFORM READ-CARD-BY-ACCT

               IF NOT ERR-FLG-ON
                   PERFORM POPULATE-CARD-OUTPUT-DATA
                   MOVE 'Card data retrieved successfully'
                        TO LK-OUT-MESSAGE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      *                      READ-CARD-BY-CARDNUM
      *----------------------------------------------------------------*
       READ-CARD-BY-CARDNUM.

           EXEC SQL
                SELECT CARD_NUM,
                       CARD_ACCT_ID,
                       CARD_CVV_CD,
                       CARD_EMBOSSED_NAME,
                       CARD_EXPIRY_DATE,
                       CARD_ACTIVE_STATUS
                INTO   :HV-CARD-NUM,
                       :HV-CARD-ACCT-ID,
                       :HV-CARD-CVV-CD,
                       :HV-CARD-EMBOSSED-NAME,
                       :HV-CARD-EXPIRAION-DATE,
                       :HV-CARD-ACTIVE-STATUS
                FROM   ALAINL.CARDDAT
                WHERE  CARD_NUM = :HV-SEARCH-CARD-NUM
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE

           EVALUATE WS-SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   SET ERR-FLG-ON TO TRUE
                   SET RC-NOT-FOUND TO TRUE
                   MOVE 'Card not found' TO LK-OUT-MESSAGE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   SET RC-DATABASE-ERROR TO TRUE
                   MOVE 'SELECT'                    TO ERROR-OPNAME
                   MOVE WS-TABLE-NAME               TO ERROR-TABLE
                   MOVE WS-SQLCODE                  TO ERROR-SQLCODE
                   MOVE WS-DB2-ERROR-MESSAGE        TO LK-OUT-MESSAGE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      READ-CARD-BY-ACCT
      *----------------------------------------------------------------*
       READ-CARD-BY-ACCT.

           EXEC SQL
                SELECT CARD_NUM,
                       CARD_ACCT_ID,
                       CARD_CVV_CD,
                       CARD_EMBOSSED_NAME,
                       CARD_EXPIRY_DATE,
                       CARD_ACTIVE_STATUS
                INTO   :HV-CARD-NUM,
                       :HV-CARD-ACCT-ID,
                       :HV-CARD-CVV-CD,
                       :HV-CARD-EMBOSSED-NAME,
                       :HV-CARD-EXPIRAION-DATE,
                       :HV-CARD-ACTIVE-STATUS
                FROM   ALAINL.CARDDAT
                WHERE  CARD_ACCT_ID = :HV-SEARCH-ACCT-ID
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE

           EVALUATE WS-SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   SET ERR-FLG-ON TO TRUE
                   SET RC-NOT-FOUND TO TRUE
                   MOVE 'Card not found for account' TO LK-OUT-MESSAGE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   SET RC-DATABASE-ERROR TO TRUE
                   MOVE 'SELECT'                    TO ERROR-OPNAME
                   MOVE WS-TABLE-NAME               TO ERROR-TABLE
                   MOVE WS-SQLCODE                  TO ERROR-SQLCODE
                   MOVE WS-DB2-ERROR-MESSAGE        TO LK-OUT-MESSAGE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      POPULATE-CARD-OUTPUT-DATA
      *----------------------------------------------------------------*
       POPULATE-CARD-OUTPUT-DATA.

           MOVE HV-CARD-NUM              TO LK-OUT-CARD-NUM
           MOVE HV-CARD-ACCT-ID          TO LK-OUT-CARD-ACCT-ID
           MOVE HV-CARD-CVV-CD           TO LK-OUT-CARD-CVV-CD
           MOVE HV-CARD-EMBOSSED-NAME    TO LK-OUT-CARD-EMBOSSED-NAME
           MOVE HV-CARD-EXPIRAION-DATE   TO LK-OUT-CARD-EXPIRY-DATE
           MOVE HV-CARD-ACTIVE-STATUS    TO LK-OUT-CARD-STATUS.

      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:12:33 CDT
      *