***********************************************************
      * Program:     COACCSTU.CBL                                     *
      * Layer:       Screen/UI logic only                             *
      * Function:    Simple customer search - call COCUSTMA          *
      *              (Simplified but keeping CardDemo structure)      *
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
       PROGRAM-ID.
           COACCSTU.
       DATE-WRITTEN.
           April 2022.
       DATE-COMPILED.
           Today.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01  WS-MISC-STORAGE.
      ******************************************************************
      * General CICS related
      ******************************************************************
         05 WS-CICS-PROCESSNG-VARS.
            07 WS-RESP-CD                          PIC S9(09) COMP
                                                   VALUE ZEROS.
            07 WS-REAS-CD                          PIC S9(09) COMP
                                                   VALUE ZEROS.
            07 WS-TRANID                           PIC X(4)
                                                   VALUE SPACES.
            07 WS-UCTRANS                          PIC X(4)
                                                   VALUE SPACES.
      ******************************************************************
      *      Input edits
      ******************************************************************

         05  WS-INPUT-FLAG                         PIC X(1).
           88  INPUT-OK                            VALUE '0'.
           88  INPUT-ERROR                         VALUE '1'.
           88  INPUT-PENDING                       VALUE LOW-VALUES.
         05  WS-EDIT-CUST-FLAG                     PIC X(1).
           88  FLG-CUSTFILTER-NOT-OK               VALUE '0'.
           88  FLG-CUSTFILTER-ISVALID              VALUE '1'.
           88  FLG-CUSTFILTER-BLANK                VALUE ' '.
         05  WS-RETURN-FLAG                        PIC X(1).
           88  WS-RETURN-FLAG-OFF                  VALUE LOW-VALUES.
           88  WS-RETURN-FLAG-ON                   VALUE '1'.
         05  WS-PFK-FLAG                           PIC X(1).
           88  PFK-VALID                           VALUE '0'.
           88  PFK-INVALID                         VALUE '1'.

      ******************************************************************
      * Output edits
      ******************************************************************
         05 CICS-OUTPUT-EDIT-VARS.
           10  CUST-ID-X                           PIC X(09).
           10  CUST-ID-N REDEFINES CUST-ID-X       PIC 9(09).

      ******************************************************************
      *      Output Message Construction
      ******************************************************************
         05  WS-INFO-MSG                           PIC X(40).
           88  WS-NO-INFO-MESSAGE                 VALUES
                                                  SPACES LOW-VALUES.
           88  PROMPT-FOR-SEARCH-KEYS              VALUE
               'Enter Customer ID'.

         05  WS-RETURN-MSG                         PIC X(75).
           88  WS-RETURN-MSG-OFF                   VALUE SPACES.
           88  WS-EXIT-MESSAGE                     VALUE
               'PF03 pressed.Exiting              '.
           88  WS-PROMPT-FOR-CUST                  VALUE
               'Customer ID not provided'.
           88  NO-SEARCH-CRITERIA-RECEIVED         VALUE
               'No input received'.
           88  SEARCHED-CUST-ZEROES                VALUE
               'Customer ID must be a non zero 9 digit number'.
           88  SEARCHED-CUST-NOT-NUMERIC           VALUE
               'Customer ID must be a non zero 9 digit number'.
           88  DID-NOT-FIND-CUSTOMER               VALUE
               'Customer not found in database'.
           88  CUST-READ-ERROR                     VALUE
               'Error reading Customer Data File'.

      ******************************************************************
      *      Literals and Constants
      ******************************************************************
       01 WS-LITERALS.
          05 LIT-THISPGM                           PIC X(8)
                                                   VALUE 'COACCSTU'.
          05 LIT-THISTRANID                        PIC X(4)
                                                   VALUE 'AASA'.
          05 LIT-THISMAPSET                        PIC X(8)
                                                   VALUE 'COACCST '.
          05 LIT-THISMAP                           PIC X(7)
                                                   VALUE 'COACCST'.
          05 LIT-MENUPGM                           PIC X(8)
                                                   VALUE 'COMEN01U'.
          05 LIT-MENUTRANID                        PIC X(4)
                                                   VALUE 'AAUM'.
          05 LIT-RPC-PROGRAM                       PIC X(8)
                                                   VALUE 'COCUSTMA'.

      ******************************************************************
      *Other common working storage Variables
      ******************************************************************
       COPY CVCRD01Y.

      ******************************************************************
      *Application Commmarea Copybook
       COPY COCOM01Y.

       01 WS-THIS-PROGCOMMAREA.
          05 SEARCH-SCREEN-DATA.
             10 CACC-SEARCH-ACTION                 PIC X(1)
                                                   VALUE LOW-VALUES.
                88 CACC-DETAILS-NOT-FETCHED        VALUES
                                                   LOW-VALUES,
                                                   SPACES.
                88 CACC-SHOW-DETAILS               VALUE 'S'.
             10 CACC-SEARCH-CUST-ID                PIC S9(09) COMP-3.

       01  WS-COMMAREA                             PIC X(2000).

      * RPC Communication Area - matches cleaned up COCUSTMA
       01 WS-RPC-COMMAREA.
           05 LK-INPUT-CUST-ID         PIC S9(09) COMP-3.
           05 LK-OUTPUT-STATUS.
               10 LK-OUT-RETURN-CODE   PIC 9(02).
                   88 RC-SUCCESS       VALUE 00.
                   88 RC-NOT-FOUND     VALUE 01.
                   88 RC-VALIDATION-ERROR VALUE 10.
                   88 RC-DATABASE-ERROR VALUE 99.
               10 LK-OUT-MESSAGE       PIC X(80).
           05 LK-OUTPUT-CUSTOMER.
               10 LK-OUT-CUST-ID       PIC S9(09) COMP-3.
               10 LK-OUT-FIRST-NAME    PIC X(25).
               10 LK-OUT-MIDDLE-NAME   PIC X(25).
               10 LK-OUT-LAST-NAME     PIC X(25).
               10 LK-OUT-ADDR-LINE-1   PIC X(50).
               10 LK-OUT-ADDR-LINE-2   PIC X(50).
               10 LK-OUT-ADDR-LINE-3   PIC X(50).
               10 LK-OUT-ADDR-STATE-CD PIC X(02).
               10 LK-OUT-ADDR-COUNTRY-CD PIC X(03).
               10 LK-OUT-ADDR-ZIP      PIC X(10).
               10 LK-OUT-PHONE-NUM-1   PIC X(15).
               10 LK-OUT-PHONE-NUM-2   PIC X(15).
               10 LK-OUT-SSN           PIC S9(09) COMP-3.
               10 LK-OUT-GOVT-ISSUED-ID PIC X(20).
               10 LK-OUT-DOB-YYYY-MM-DD PIC X(10).
               10 LK-OUT-EFT-ACCOUNT-ID PIC X(15).
               10 LK-OUT-PRI-CARD-HOLDER-IND PIC X(01).
               10 LK-OUT-FICO-CREDIT-SCORE PIC S9(03) COMP-3.

      *IBM SUPPLIED COPYBOOKS
       COPY DFHBMSCA.
       COPY DFHAID.

      *COMMON COPYBOOKS
      *Screen Titles
       COPY COTTL01Y.
      *Account/Customer Search Screen Layout
       COPY COACCST.

      *Current Date
       COPY CSDAT01Y.

      *Common Messages
       COPY CSMSG01Y.

      *Abend Variables
       COPY CSMSG02Y.

      *Signed on user data
       COPY CSUSR01Y.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
         05  FILLER                                PIC X(1)
             OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.

       PROCEDURE DIVISION.
       0000-MAIN.

           EXEC CICS HANDLE ABEND
                     LABEL(ABEND-ROUTINE)
           END-EXEC

           INITIALIZE CC-WORK-AREA
                      WS-MISC-STORAGE
                      WS-COMMAREA
      *****************************************************************
      * Store our context
      *****************************************************************
           MOVE LIT-THISTRANID       TO WS-TRANID
      *****************************************************************
      * Ensure error message is cleared                               *
      *****************************************************************
           SET WS-RETURN-MSG-OFF  TO TRUE
      *****************************************************************
      * Store passed data if  any                *
      *****************************************************************
           IF EIBCALEN IS EQUAL TO 0
               OR (CDEMO-FROM-PROGRAM = LIT-MENUPGM
               AND NOT CDEMO-PGM-REENTER)
              INITIALIZE CARDDEMO-COMMAREA
                         WS-THIS-PROGCOMMAREA
              SET CDEMO-PGM-ENTER TO TRUE
              SET CACC-DETAILS-NOT-FETCHED TO TRUE
           ELSE
              MOVE DFHCOMMAREA (1:LENGTH OF CARDDEMO-COMMAREA)  TO
                                CARDDEMO-COMMAREA
              MOVE DFHCOMMAREA(LENGTH OF CARDDEMO-COMMAREA + 1:
                               LENGTH OF WS-THIS-PROGCOMMAREA ) TO
                                WS-THIS-PROGCOMMAREA
           END-IF
      *****************************************************************
      * Remap PFkeys as needed.
      * Store the Mapped PF Key
      *****************************************************************
           PERFORM YYYY-STORE-PFKEY
              THRU YYYY-STORE-PFKEY-EXIT
      *****************************************************************
      * Check the AID to see if its valid at this point               *
      * F3 - Exit
      * F4 - Clear
      * Enter - Process
      *****************************************************************
           SET PFK-INVALID TO TRUE
           IF CCARD-AID-ENTER OR
              CCARD-AID-PFK03 OR
              CCARD-AID-PFK04
              SET PFK-VALID TO TRUE
           END-IF

           IF PFK-INVALID
              SET CCARD-AID-ENTER TO TRUE
           END-IF

      *****************************************************************
      * Decide what to do based on inputs received
      *****************************************************************
           EVALUATE TRUE
      ******************************************************************
      *       USER PRESSES PF03 TO EXIT
      *            XCTL TO CALLING PROGRAM OR MAIN MENU
      ******************************************************************
              WHEN CCARD-AID-PFK03
                   SET CCARD-AID-PFK03     TO TRUE

                   IF CDEMO-FROM-TRANID    EQUAL LOW-VALUES
                   OR CDEMO-FROM-TRANID    EQUAL SPACES
                      MOVE LIT-MENUTRANID  TO CDEMO-TO-TRANID
                   ELSE
                      MOVE CDEMO-FROM-TRANID  TO CDEMO-TO-TRANID
                   END-IF

                   IF CDEMO-FROM-PROGRAM   EQUAL LOW-VALUES
                   OR CDEMO-FROM-PROGRAM   EQUAL SPACES
                      MOVE LIT-MENUPGM     TO CDEMO-TO-PROGRAM
                   ELSE
                      MOVE CDEMO-FROM-PROGRAM TO CDEMO-TO-PROGRAM
                   END-IF

                   MOVE LIT-THISTRANID     TO CDEMO-FROM-TRANID
                   MOVE LIT-THISPGM        TO CDEMO-FROM-PROGRAM

                   SET  CDEMO-USRTYP-USER  TO TRUE
                   SET  CDEMO-PGM-ENTER    TO TRUE
                   MOVE LIT-THISMAPSET     TO CDEMO-LAST-MAPSET
                   MOVE LIT-THISMAP        TO CDEMO-LAST-MAP

                   EXEC CICS
                        SYNCPOINT
                   END-EXEC

                   EXEC CICS XCTL
                        PROGRAM (CDEMO-TO-PROGRAM)
                        COMMAREA(CARDDEMO-COMMAREA)
                   END-EXEC
      ******************************************************************
      *       USER PRESSES PF04 TO CLEAR
      *            RESET THE SCREEN
      ******************************************************************
              WHEN CCARD-AID-PFK04
                   INITIALIZE WS-THIS-PROGCOMMAREA
                   SET CDEMO-PGM-ENTER TO TRUE
                   SET CACC-DETAILS-NOT-FETCHED TO TRUE
                   PERFORM 3000-SEND-MAP THRU
                           3000-SEND-MAP-EXIT
                   SET CDEMO-PGM-REENTER TO TRUE
                   GO TO COMMON-RETURN
      ******************************************************************
      *       FRESH ENTRY INTO PROGRAM
      *            ASK THE USER FOR SEARCH CRITERIA
      ******************************************************************
              WHEN CACC-DETAILS-NOT-FETCHED
               AND CDEMO-PGM-ENTER
              WHEN CDEMO-FROM-PROGRAM   EQUAL LIT-MENUPGM
               AND NOT CDEMO-PGM-REENTER
                   INITIALIZE WS-THIS-PROGCOMMAREA
                   PERFORM 3000-SEND-MAP THRU
                           3000-SEND-MAP-EXIT
                   SET CDEMO-PGM-REENTER        TO TRUE
                   SET CACC-DETAILS-NOT-FETCHED TO TRUE
                   GO TO COMMON-RETURN
      ******************************************************************
      *      PROCESS SEARCH REQUEST
      ******************************************************************
              WHEN OTHER
                   PERFORM 1000-PROCESS-INPUTS
                      THRU 1000-PROCESS-INPUTS-EXIT
                   PERFORM 2000-DECIDE-ACTION
                      THRU 2000-DECIDE-ACTION-EXIT
                   PERFORM 3000-SEND-MAP
                      THRU 3000-SEND-MAP-EXIT
                   GO TO COMMON-RETURN
           END-EVALUATE
           .

       COMMON-RETURN.
           MOVE WS-RETURN-MSG     TO CCARD-ERROR-MSG

           MOVE  CARDDEMO-COMMAREA    TO WS-COMMAREA
           MOVE  WS-THIS-PROGCOMMAREA TO
                  WS-COMMAREA(LENGTH OF CARDDEMO-COMMAREA + 1:
                               LENGTH OF WS-THIS-PROGCOMMAREA )

           EXEC CICS RETURN
                TRANSID (LIT-THISTRANID)
                COMMAREA (WS-COMMAREA)
                LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC
           .
       0000-MAIN-EXIT.
           EXIT
           .

       1000-PROCESS-INPUTS.
           PERFORM 1100-RECEIVE-MAP
              THRU 1100-RECEIVE-MAP-EXIT
           PERFORM 1200-EDIT-MAP-INPUTS
              THRU 1200-EDIT-MAP-INPUTS-EXIT
           MOVE WS-RETURN-MSG  TO CCARD-ERROR-MSG
           MOVE LIT-THISPGM    TO CCARD-NEXT-PROG
           MOVE LIT-THISMAPSET TO CCARD-NEXT-MAPSET
           MOVE LIT-THISMAP    TO CCARD-NEXT-MAP
           .

       1000-PROCESS-INPUTS-EXIT.
           EXIT
           .

       1100-RECEIVE-MAP.
           EXEC CICS RECEIVE MAP(LIT-THISMAP)
                     MAPSET(LIT-THISMAPSET)
                     INTO(COACCSTI)
                     RESP(WS-RESP-CD)
                     RESP2(WS-REAS-CD)
           END-EXEC

      *    Get customer ID from simplified screen
           IF  CUSTIDINI OF COACCSTI = '*'
           OR  CUSTIDINI OF COACCSTI = SPACES
               MOVE LOW-VALUES           TO  CUST-ID-X
           ELSE
               MOVE CUSTIDINI OF COACCSTI TO  CUST-ID-X
           END-IF
           .

       1100-RECEIVE-MAP-EXIT.
           EXIT
           .

       1200-EDIT-MAP-INPUTS.
           SET INPUT-OK TO TRUE

           IF CACC-DETAILS-NOT-FETCHED
      *        VALIDATE CUSTOMER ID
               PERFORM 1220-EDIT-CUSTOMER
                  THRU 1220-EDIT-CUSTOMER-EXIT

      *       IF CUSTOMER ID IS BLANK
               IF FLG-CUSTFILTER-BLANK
                   SET NO-SEARCH-CRITERIA-RECEIVED TO TRUE
               END-IF
           ELSE
      *        DATA ALREADY FETCHED - JUST REDISPLAY
               CONTINUE
           END-IF
           .

       1200-EDIT-MAP-INPUTS-EXIT.
           EXIT
           .

       1220-EDIT-CUSTOMER.
           SET FLG-CUSTFILTER-NOT-OK TO TRUE

      *    Not supplied
           IF CUST-ID-X   EQUAL LOW-VALUES
           OR CUST-ID-X   EQUAL SPACES
           OR CUST-ID-N   EQUAL ZEROS
              SET FLG-CUSTFILTER-BLANK  TO TRUE
              MOVE ZEROES        TO CACC-SEARCH-CUST-ID
              GO TO  1220-EDIT-CUSTOMER-EXIT
           END-IF

      *    Not numeric
           IF CUST-ID-X  IS NOT NUMERIC
              SET INPUT-ERROR TO TRUE
              SET FLG-CUSTFILTER-NOT-OK TO TRUE
              IF WS-RETURN-MSG-OFF
                 MOVE
              'CUSTOMER ID MUST BE A 9 DIGIT NUMBER'
                                 TO WS-RETURN-MSG
              END-IF
              MOVE ZERO          TO CACC-SEARCH-CUST-ID
              GO TO 1220-EDIT-CUSTOMER-EXIT
           ELSE
              MOVE CUST-ID-N     TO CACC-SEARCH-CUST-ID
              SET FLG-CUSTFILTER-ISVALID TO TRUE
           END-IF
           .

       1220-EDIT-CUSTOMER-EXIT.
           EXIT
           .

       2000-DECIDE-ACTION.
           EVALUATE TRUE
      ******************************************************************
      *       NO DETAILS SHOWN - NEED TO FETCH DATA
      ******************************************************************
              WHEN CACC-DETAILS-NOT-FETCHED
                 IF FLG-CUSTFILTER-ISVALID
                     DISPLAY 'COACCSTU: Calling COCUSTMA...'
                     PERFORM 9000-READ-CUSTOMER-DATA
                        THRU 9000-READ-CUSTOMER-DATA-EXIT
                 END-IF
      ******************************************************************
      *       DETAILS ALREADY SHOWN - JUST REDISPLAY
      ******************************************************************
              WHEN CACC-SHOW-DETAILS
                 DISPLAY 'COACCSTU: Data already fetched, redisplaying'
                 SET FLG-CUSTFILTER-ISVALID TO TRUE
                 MOVE 'Customer data shown (F3=Exit F4=Clear)'
                      TO WS-INFO-MSG

              WHEN OTHER
                   MOVE LIT-THISPGM    TO ABEND-CULPRIT
                   MOVE '0001'         TO ABEND-CODE
                   MOVE SPACES         TO ABEND-REASON
                   MOVE 'UNEXPECTED DATA SCENARIO'
                                       TO ABEND-MSG
                   PERFORM ABEND-ROUTINE
                      THRU ABEND-ROUTINE-EXIT
           END-EVALUATE
           .
       2000-DECIDE-ACTION-EXIT.
           EXIT
           .

       3000-SEND-MAP.
           PERFORM 3100-SCREEN-INIT
              THRU 3100-SCREEN-INIT-EXIT
           PERFORM 3200-SETUP-SCREEN-VARS
              THRU 3200-SETUP-SCREEN-VARS-EXIT
           PERFORM 3250-SETUP-INFOMSG
              THRU 3250-SETUP-INFOMSG-EXIT
           PERFORM 3300-SETUP-SCREEN-ATTRS
              THRU 3300-SETUP-SCREEN-ATTRS-EXIT
           PERFORM 3400-SEND-SCREEN
              THRU 3400-SEND-SCREEN-EXIT
           .

       3000-SEND-MAP-EXIT.
           EXIT
           .

       3100-SCREEN-INIT.
           MOVE LOW-VALUES TO COACCSTO
           .

       3100-SCREEN-INIT-EXIT.
           EXIT
           .

       3200-SETUP-SCREEN-VARS.
      *    Show customer ID on screen - only field that exists
           IF CDEMO-PGM-ENTER
              CONTINUE
           ELSE
              IF CACC-SEARCH-CUST-ID = 0
                MOVE LOW-VALUES           TO CUSTIDINO OF COACCSTO
              ELSE
                MOVE CACC-SEARCH-CUST-ID  TO CUSTIDINO OF COACCSTO
              END-IF
           END-IF
           .

       3200-SETUP-SCREEN-VARS-EXIT.
           EXIT
           .

       3250-SETUP-INFOMSG.
      *    SETUP INFORMATION MESSAGE
           EVALUATE TRUE
               WHEN CDEMO-PGM-ENTER
                    SET PROMPT-FOR-SEARCH-KEYS TO TRUE
               WHEN WS-NO-INFO-MESSAGE
                   SET PROMPT-FOR-SEARCH-KEYS TO TRUE
           END-EVALUATE

      *    If there's an error message, show it; otherwise show info msg
           IF WS-RETURN-MSG NOT = SPACES
               MOVE WS-RETURN-MSG TO ERRMSGO OF COACCSTO
           ELSE
               MOVE WS-INFO-MSG   TO ERRMSGO OF COACCSTO
           END-IF
           .
       3250-SETUP-INFOMSG-EXIT.
           EXIT
           .

       3300-SETUP-SCREEN-ATTRS.
      *    PROTECT OR UNPROTECT BASED ON CONTEXT
           EVALUATE TRUE
              WHEN CACC-DETAILS-NOT-FETCHED
                   MOVE DFHBMFSE      TO CUSTIDINA OF COACCSTI
              WHEN OTHER
                   MOVE DFHBMFSE      TO CUSTIDINA OF COACCSTI
           END-EVALUATE

      *    POSITION CURSOR
           EVALUATE TRUE
              WHEN FLG-CUSTFILTER-NOT-OK
              WHEN FLG-CUSTFILTER-BLANK
                   MOVE -1             TO CUSTIDINL OF COACCSTI
              WHEN OTHER
                  MOVE -1              TO CUSTIDINL OF COACCSTI
           END-EVALUATE

      *    SETUP COLOR
           IF FLG-CUSTFILTER-NOT-OK
              MOVE DFHRED              TO CUSTIDINC OF COACCSTO
           END-IF

           IF  FLG-CUSTFILTER-BLANK
           AND CDEMO-PGM-REENTER
               MOVE '*'                TO CUSTIDINO OF COACCSTO
               MOVE DFHRED             TO CUSTIDINC OF COACCSTO
           END-IF
           .
       3300-SETUP-SCREEN-ATTRS-EXIT.
           EXIT
           .

       3400-SEND-SCREEN.
           MOVE LIT-THISMAPSET         TO CCARD-NEXT-MAPSET
           MOVE LIT-THISMAP            TO CCARD-NEXT-MAP

           EXEC CICS SEND MAP(CCARD-NEXT-MAP)
                          MAPSET(CCARD-NEXT-MAPSET)
                          FROM(COACCSTO)
                          CURSOR
                          ERASE
                          FREEKB
                          RESP(WS-RESP-CD)
           END-EXEC
           .
       3400-SEND-SCREEN-EXIT.
           EXIT
           .

       9000-READ-CUSTOMER-DATA.
           INITIALIZE WS-RPC-COMMAREA

      *    Just pass customer ID to COCUSTMA
           MOVE CACC-SEARCH-CUST-ID TO LK-INPUT-CUST-ID

           EXEC CICS LINK
                PROGRAM(LIT-RPC-PROGRAM)
                COMMAREA(WS-RPC-COMMAREA)
                LENGTH(LENGTH OF WS-RPC-COMMAREA)
                RESP(WS-RESP-CD)
                RESP2(WS-REAS-CD)
           END-EXEC

           DISPLAY 'COACCSTU: RPC Response Code: ' WS-RESP-CD
           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   EVALUATE TRUE
                       WHEN RC-SUCCESS
                           SET FLG-CUSTFILTER-ISVALID TO TRUE
                           SET CACC-SHOW-DETAILS TO TRUE
                           MOVE LK-OUT-MESSAGE TO WS-RETURN-MSG
                       WHEN RC-NOT-FOUND
                           SET INPUT-ERROR TO TRUE
                           SET FLG-CUSTFILTER-NOT-OK TO TRUE
                           IF WS-RETURN-MSG-OFF
                               SET DID-NOT-FIND-CUSTOMER TO TRUE
                           END-IF
                       WHEN OTHER
                           SET INPUT-ERROR TO TRUE
                           IF WS-RETURN-MSG-OFF
                               SET CUST-READ-ERROR TO TRUE
                           END-IF
                           MOVE LK-OUT-MESSAGE TO WS-RETURN-MSG
                   END-EVALUATE
               WHEN DFHRESP(PGMIDERR)
                   SET INPUT-ERROR TO TRUE
                   IF WS-RETURN-MSG-OFF
                       SET CUST-READ-ERROR TO TRUE
                   END-IF
                   MOVE 'RPC Program COCUSTMA not found'
                    TO WS-RETURN-MSG
               WHEN OTHER
                   SET INPUT-ERROR TO TRUE
                   IF WS-RETURN-MSG-OFF
                       SET CUST-READ-ERROR TO TRUE
                   END-IF
                   MOVE 'Error calling RPC program' TO WS-RETURN-MSG
           END-EVALUATE
           .

       9000-READ-CUSTOMER-DATA-EXIT.
           EXIT
           .

      ******************************************************************
      *Common code to store PFKey
      ******************************************************************
       COPY 'CSSTRPFY'
           .

       ABEND-ROUTINE.

           IF ABEND-MSG EQUAL LOW-VALUES
              MOVE 'UNEXPECTED ABEND OCCURRED.' TO ABEND-MSG
           END-IF

           MOVE LIT-THISPGM       TO ABEND-CULPRIT

           EXEC CICS SEND
                            FROM (ABEND-DATA)
                            LENGTH(LENGTH OF ABEND-DATA)
                            NOHANDLE
                            ERASE
           END-EXEC

           EXEC CICS HANDLE ABEND
                CANCEL
           END-EXEC

           EXEC CICS ABEND
                ABCODE('9999')
           END-EXEC
           .
       ABEND-ROUTINE-EXIT.
           EXIT
           .

      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:12:33 CDT
      *