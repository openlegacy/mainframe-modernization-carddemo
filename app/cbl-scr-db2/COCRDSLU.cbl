****************************************************************
      * Program:     COCRDSLU.CBL                                     *
      * Layer:       Screen logic (UI only)                          *
      * Function:    Accept and process credit card detail request    *
      *              Screen interactions only*
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
           COCRDSLU.
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
            07 WS-RESP-DISP                          PIC S9(09)
                                                   VALUE ZEROS.
            07 WS-REAS-DISP                          PIC S9(09)
                                                   VALUE ZEROS.
            07 WS-TRANID                           PIC X(4)
                                                   VALUE SPACES.
      ******************************************************************
      *      Input edits
      ******************************************************************

         05  WS-INPUT-FLAG                         PIC X(1).
           88  INPUT-OK                            VALUE '0'.
           88  INPUT-ERROR                         VALUE '1'.
           88  INPUT-PENDING                       VALUE LOW-VALUES.
         05  WS-EDIT-ACCT-FLAG                     PIC X(1).
           88  FLG-ACCTFILTER-NOT-OK               VALUE '0'.
           88  FLG-ACCTFILTER-ISVALID              VALUE '1'.
           88  FLG-ACCTFILTER-BLANK                VALUE ' '.
         05  WS-EDIT-CARD-FLAG                     PIC X(1).
           88  FLG-CARDFILTER-NOT-OK               VALUE '0'.
           88  FLG-CARDFILTER-ISVALID             VALUE '1'.
           88  FLG-CARDFILTER-BLANK                VALUE ' '.
         05  WS-RETURN-FLAG                        PIC X(1).
           88  WS-RETURN-FLAG-OFF                  VALUE LOW-VALUES.
           88  WS-RETURN-FLAG-ON                   VALUE '1'.
         05  WS-PFK-FLAG                           PIC X(1).
           88  PFK-VALID                           VALUE '0'.
           88  PFK-INVALID                         VALUE '1'.
         05  WS-ERR-FLG                            PIC X(01) VALUE 'N'.
           88 ERR-FLG-ON                                    VALUE 'Y'.
           88 ERR-FLG-OFF                                   VALUE 'N'.

      ******************************************************************
      * Output edits
      ******************************************************************
         05 CICS-OUTPUT-EDIT-VARS.
           10  CARD-ACCT-ID-X                      PIC X(11).
           10  CARD-ACCT-ID-N REDEFINES CARD-ACCT-ID-X
                                                   PIC 9(11).
           10  CARD-CVV-CD-X                       PIC X(03).
           10  CARD-CVV-CD-N REDEFINES  CARD-CVV-CD-X
                                                   PIC 9(03).
           10  CARD-CARD-NUM-X                     PIC X(16).
           10  CARD-CARD-NUM-N REDEFINES  CARD-CARD-NUM-X
                                                   PIC 9(16).
           10  CARD-NAME-EMBOSSED-X                PIC X(50).
           10  CARD-STATUS-X                       PIC X.
           10  CARD-EXPIRAION-DATE-X               PIC X(10).
           10  FILLER REDEFINES CARD-EXPIRAION-DATE-X.
               20 CARD-EXPIRY-YEAR                 PIC X(4).
               20 FILLER                           PIC X(1).
               20 CARD-EXPIRY-MONTH                PIC X(2).
               20 FILLER                           PIC X(1).
               20 CARD-EXPIRY-DAY                  PIC X(2).
           10  CARD-EXPIRAION-DATE-N REDEFINES
               CARD-EXPIRAION-DATE-X               PIC 9(10).

      ******************************************************************
      *      Output Message Construction
      ******************************************************************
         05  WS-INFO-MSG                           PIC X(40).
           88  WS-NO-INFO-MESSAGE                 VALUES
                                                  SPACES LOW-VALUES.
           88  FOUND-CARDS-FOR-ACCOUNT             VALUE
               '   Displaying requested details'.
           88  WS-PROMPT-FOR-INPUT                 VALUE
               'Please enter Account and Card Number'.

         05  WS-RETURN-MSG                         PIC X(75).
           88  WS-RETURN-MSG-OFF                   VALUE SPACES.
           88  WS-EXIT-MESSAGE                     VALUE
               'PF03 pressed.Exiting              '.
           88  WS-PROMPT-FOR-ACCT                  VALUE
               'Account number not provided'.
           88  WS-PROMPT-FOR-CARD                  VALUE
               'Card number not provided'.
           88  NO-SEARCH-CRITERIA-RECEIVED         VALUE
               'No input received'.
           88  SEARCHED-ACCT-ZEROES                VALUE
               'Account number must be a non zero 11 digit number'.
           88  SEARCHED-ACCT-NOT-NUMERIC           VALUE
               'Account number must be a non zero 11 digit number'.
           88  SEARCHED-CARD-NOT-NUMERIC           VALUE
               'Card number if supplied must be a 16 digit number'.

           88  DID-NOT-FIND-ACCT-IN-CARDXREF       VALUE
               'Did not find this account in cards database'.
           88  DID-NOT-FIND-ACCTCARD-COMBO         VALUE
               'Did not find cards for this search condition'.
           88  XREF-READ-ERROR                     VALUE
               'Error reading Card Data File'.
           88  CODING-TO-BE-DONE                   VALUE
               'Looks Good.... so far'.

      ******************************************************************
      *      Literals and Constants
      ******************************************************************
       01 WS-LITERALS.
          05 LIT-THISPGM                           PIC X(8)
                                                   VALUE 'COCRDSLU'.
          05 LIT-RPCPGM                           PIC X(8)
                                                  VALUE 'COCRDSLA'.
          05 LIT-THISTRANID                        PIC X(4)
                                                   VALUE 'AAS6'.
          05 LIT-THISMAPSET                        PIC X(8)
                                                   VALUE 'COCRDSL '.
          05 LIT-THISMAP                           PIC X(7)
                                                   VALUE 'CCRDSLA'.
          05 LIT-CCLISTPGM                         PIC X(8)
                                                   VALUE 'COCRDLIU'.
          05 LIT-CCLISTTRANID                      PIC X(4)
                                                   VALUE 'AAS4'.
          05 LIT-CCLISTMAPSET                      PIC X(7)
                                                   VALUE 'COCRDLI'.
          05 LIT-CCLISTMAP                         PIC X(7)
                                                   VALUE 'CCRDSLA'.
          05 LIT-MENUPGM                           PIC X(8)
                                                   VALUE 'COMEN01U'.
          05 LIT-MENUTRANID                        PIC X(4)
                                                   VALUE 'AAUM'.
          05 LIT-MENUMAPSET                        PIC X(7)
                                                   VALUE 'COMEN01'.
          05 LIT-MENUMAP                           PIC X(7)
                                                   VALUE 'COMEN1A'.

      ******************************************************************
      *Other common working storage Variables
      ******************************************************************
       COPY CVCRD01Y.

      ******************************************************************
      *Application Commmarea Copybook
       COPY COCOM01Y.

       01 WS-THIS-PROGCOMMAREA.
          05 CA-CALL-CONTEXT.
             10 CA-FROM-PROGRAM                    PIC X(08).
             10 CA-FROM-TRANID                     PIC X(04).

       01  WS-COMMAREA                             PIC X(2000).

      * RPC Communication Area for card lookup
       01 WS-RPC-COMMAREA.
           05 LK-OPERATION               PIC X(01).
               88 LK-OP-LOOKUP-CARD      VALUE 'L'.
               88 LK-OP-LOOKUP-ACCT      VALUE 'A'.
           05 LK-INPUT-CRITERIA.
               10 LK-IN-ACCT-ID             PIC 9(11).
               10 LK-IN-CARD-NUM            PIC 9(16).
           05 LK-OUTPUT-STATUS.
               10 LK-OUT-RETURN-CODE        PIC 9(02).
                   88 RC-SUCCESS             VALUE 00.
                   88 RC-NOT-FOUND           VALUE 01.
                   88 RC-VALIDATION-ERROR    VALUE 10.
                   88 RC-DATABASE-ERROR      VALUE 99.
               10 LK-OUT-MESSAGE            PIC X(80).
           05 LK-OUTPUT-CARD-DATA.
               10 LK-OUT-CARD-NUM           PIC X(16).
               10 LK-OUT-CARD-ACCT-ID       PIC 9(11).
               10 LK-OUT-CARD-CVV-CD        PIC 9(03).
               10 LK-OUT-CARD-EMBOSSED-NAME PIC X(50).
               10 LK-OUT-CARD-EXPIRY-DATE   PIC X(10).
               10 LK-OUT-CARD-STATUS        PIC X(01).

      *IBM SUPPLIED COPYBOOKS
       COPY DFHBMSCA.
       COPY DFHAID.

      *COMMON COPYBOOKS
      *Screen Titles
       COPY COTTL01Y.
      *Credit Card Search Screen Layout
       COPY COCRDSL.

      *Current Date
       COPY CSDAT01Y.

      *Common Messages
       COPY CSMSG01Y.

      *Abend Variables
       COPY CSMSG02Y.

      *Signed on user data
       COPY CSUSR01Y.

      *Dataset layouts
      *CARD RECORD LAYOUT
       COPY CVACT02Y.

      *CUSTOMER LAYOUT
       COPY CVCUS01Y.

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
           SET ERR-FLG-OFF TO TRUE
      *****************************************************************
      * Store passed data if  any                *
      *****************************************************************
           IF EIBCALEN IS EQUAL TO 0
               OR (CDEMO-FROM-PROGRAM = LIT-MENUPGM
               AND NOT CDEMO-PGM-REENTER)
              INITIALIZE CARDDEMO-COMMAREA
                         WS-THIS-PROGCOMMAREA
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
      * Enter show screen again
      *****************************************************************
           SET PFK-INVALID TO TRUE
           IF CCARD-AID-ENTER OR
              CCARD-AID-PFK03
              SET PFK-VALID TO TRUE
           END-IF

           IF PFK-INVALID
              SET CCARD-AID-ENTER TO TRUE
           END-IF

      *****************************************************************
      * Decide what to do based on inputs received
      *****************************************************************
           EVALUATE TRUE
              WHEN CCARD-AID-PFK03
      ******************************************************************
      *            XCTL TO CALLING PROGRAM OR MAIN MENU
      ******************************************************************
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
      *
                   EXEC CICS XCTL
                             PROGRAM (CDEMO-TO-PROGRAM)
                             COMMAREA(CARDDEMO-COMMAREA)
                   END-EXEC
      ******************************************************************
      *            COMING FROM CREDIT CARD LIST SCREEN
      *            SELECTION CRITERIA ALREADY VALIDATED
      ******************************************************************
              WHEN CDEMO-PGM-ENTER
               AND CDEMO-FROM-PROGRAM  EQUAL LIT-CCLISTPGM
                   SET INPUT-OK TO TRUE
                   MOVE CDEMO-ACCT-ID       TO CC-ACCT-ID-N
                   MOVE CDEMO-CARD-NUM      TO CC-CARD-NUM-N
                   PERFORM LOOKUP-CARD-VIA-RPC
                   PERFORM 1000-SEND-MAP
                     THRU 1000-SEND-MAP-EXIT
                   GO TO COMMON-RETURN
              WHEN CDEMO-PGM-ENTER
      ******************************************************************
      *            COMING FROM SOME OTHER CONTEXT
      *            SELECTION CRITERIA TO BE GATHERED
      ******************************************************************
                   PERFORM 1000-SEND-MAP THRU
                           1000-SEND-MAP-EXIT
                   GO TO COMMON-RETURN
              WHEN CDEMO-PGM-REENTER
                   PERFORM 2000-PROCESS-INPUTS
                      THRU 2000-PROCESS-INPUTS-EXIT
                   IF INPUT-ERROR
                      PERFORM 1000-SEND-MAP
                         THRU 1000-SEND-MAP-EXIT
                      GO TO COMMON-RETURN
                   ELSE
                      PERFORM LOOKUP-CARD-VIA-RPC
                      PERFORM 1000-SEND-MAP
                         THRU 1000-SEND-MAP-EXIT
                      GO TO COMMON-RETURN

                   END-IF

              WHEN OTHER
                   MOVE LIT-THISPGM    TO ABEND-CULPRIT
                   MOVE '0001'         TO ABEND-CODE
                   MOVE SPACES         TO ABEND-REASON
                   MOVE 'UNEXPECTED DATA SCENARIO'
                                       TO WS-RETURN-MSG
                   PERFORM SEND-PLAIN-TEXT
                      THRU SEND-PLAIN-TEXT-EXIT
           END-EVALUATE


      * If we had an error setup error message that slipped through
      * Display and return
           IF INPUT-ERROR
              MOVE WS-RETURN-MSG  TO CCARD-ERROR-MSG
              PERFORM 1000-SEND-MAP
                 THRU 1000-SEND-MAP-EXIT
              GO TO COMMON-RETURN
           END-IF
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

      *----------------------------------------------------------------*
      *                      LOOKUP-CARD-VIA-RPC
      *----------------------------------------------------------------*
       LOOKUP-CARD-VIA-RPC.

           MOVE SPACES TO LK-INPUT-CRITERIA LK-OUTPUT-STATUS
                          LK-OUTPUT-CARD-DATA
           SET LK-OP-LOOKUP-CARD TO TRUE
           MOVE CC-ACCT-ID-N TO LK-IN-ACCT-ID
           MOVE CC-CARD-NUM-N TO LK-IN-CARD-NUM

           PERFORM CALL-RPC-PROGRAM

           IF NOT ERR-FLG-ON
               EVALUATE TRUE
                   WHEN RC-SUCCESS
                       SET FOUND-CARDS-FOR-ACCOUNT TO TRUE
                       MOVE LK-OUT-CARD-EMBOSSED-NAME TO
                            CARD-EMBOSSED-NAME
                       MOVE LK-OUT-CARD-EXPIRY-DATE TO
                            CARD-EXPIRAION-DATE
                       MOVE LK-OUT-CARD-STATUS TO
                            CARD-ACTIVE-STATUS
                   WHEN RC-NOT-FOUND
                       SET DID-NOT-FIND-ACCTCARD-COMBO TO TRUE
                   WHEN OTHER
                       MOVE LK-OUT-MESSAGE TO WS-RETURN-MSG
               END-EVALUATE
           END-IF.

      *----------------------------------------------------------------*
      *                      CALL-RPC-PROGRAM
      *----------------------------------------------------------------*
       CALL-RPC-PROGRAM.

           EXEC CICS LINK
                PROGRAM(LIT-RPCPGM)
                COMMAREA(WS-RPC-COMMAREA)
                LENGTH(LENGTH OF WS-RPC-COMMAREA)
                RESP(WS-RESP-CD)
           END-EXEC.

           MOVE WS-RESP-CD TO WS-RESP-DISP
           MOVE WS-REAS-CD TO WS-REAS-DISP.


           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(PGMIDERR)
                   MOVE 'COCRDSLA program not found' TO WS-RETURN-MSG
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   MOVE 'Error calling RPC program' TO WS-RETURN-MSG
           END-EVALUATE.

       1000-SEND-MAP.
           PERFORM 1100-SCREEN-INIT
              THRU 1100-SCREEN-INIT-EXIT
           PERFORM 1200-SETUP-SCREEN-VARS
              THRU 1200-SETUP-SCREEN-VARS-EXIT
           PERFORM 1300-SETUP-SCREEN-ATTRS
              THRU 1300-SETUP-SCREEN-ATTRS-EXIT
           PERFORM 1400-SEND-SCREEN
              THRU 1400-SEND-SCREEN-EXIT
           .

       1000-SEND-MAP-EXIT.
           EXIT
           .

       1100-SCREEN-INIT.
           MOVE LOW-VALUES TO CCRDSLAO

           MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA

           MOVE CCDA-TITLE01           TO TITLE01O OF CCRDSLAO
           MOVE CCDA-TITLE02           TO TITLE02O OF CCRDSLAO
           MOVE LIT-THISTRANID         TO TRNNAMEO OF CCRDSLAO
           MOVE LIT-THISPGM            TO PGMNAMEO OF CCRDSLAO

           MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA

           MOVE WS-CURDATE-MONTH       TO WS-CURDATE-MM
           MOVE WS-CURDATE-DAY         TO WS-CURDATE-DD
           MOVE WS-CURDATE-YEAR(3:2)   TO WS-CURDATE-YY

           MOVE WS-CURDATE-MM-DD-YY    TO CURDATEO OF CCRDSLAO

           MOVE WS-CURTIME-HOURS       TO WS-CURTIME-HH
           MOVE WS-CURTIME-MINUTE      TO WS-CURTIME-MM
           MOVE WS-CURTIME-SECOND      TO WS-CURTIME-SS

           MOVE WS-CURTIME-HH-MM-SS    TO CURTIMEO OF CCRDSLAO

           .

       1100-SCREEN-INIT-EXIT.
           EXIT
           .

       1200-SETUP-SCREEN-VARS.
      *    INITIALIZE SEARCH CRITERIA
           IF EIBCALEN = 0
              SET  WS-PROMPT-FOR-INPUT TO TRUE
           ELSE
              IF CDEMO-ACCT-ID = 0
                 MOVE LOW-VALUES   TO ACCTSIDO OF CCRDSLAO
              ELSE
                 MOVE CC-ACCT-ID   TO ACCTSIDO OF CCRDSLAO
              END-IF

              IF CDEMO-CARD-NUM = 0
                MOVE LOW-VALUES   TO CARDSIDO OF CCRDSLAO
              ELSE
                MOVE CC-CARD-NUM  TO CARDSIDO OF CCRDSLAO
              END-IF

              IF FOUND-CARDS-FOR-ACCOUNT
                 MOVE CARD-EMBOSSED-NAME
                                        TO CRDNAMEO OF CCRDSLAO
                 MOVE CARD-EXPIRAION-DATE
                                        TO CARD-EXPIRAION-DATE-X

                 MOVE CARD-EXPIRY-MONTH TO EXPMONO  OF CCRDSLAO

                 MOVE CARD-EXPIRY-YEAR  TO EXPYEARO OF CCRDSLAO

                 MOVE CARD-ACTIVE-STATUS TO CRDSTCDO OF CCRDSLAO
              END-IF
            END-IF


      *    SETUP MESSAGE
           IF WS-NO-INFO-MESSAGE
             SET WS-PROMPT-FOR-INPUT TO TRUE
           END-IF

           MOVE WS-RETURN-MSG          TO ERRMSGO OF CCRDSLAO

           MOVE WS-INFO-MSG            TO INFOMSGO OF CCRDSLAO
           .

       1200-SETUP-SCREEN-VARS-EXIT.
           EXIT
           .
       1300-SETUP-SCREEN-ATTRS.

      *    PROTECT OR UNPROTECT BASED ON CONTEXT
           IF  CDEMO-LAST-MAPSET  EQUAL LIT-CCLISTMAPSET
           AND CDEMO-FROM-PROGRAM EQUAL LIT-CCLISTPGM
              MOVE DFHBMPRF     TO ACCTSIDA OF CCRDSLAI
              MOVE DFHBMPRF     TO CARDSIDA OF CCRDSLAI
           ELSE
              MOVE DFHBMFSE      TO ACCTSIDA OF CCRDSLAI
              MOVE DFHBMFSE      TO CARDSIDA OF CCRDSLAI
           END-IF

      *    POSITION CURSOR
           EVALUATE TRUE
              WHEN FLG-ACCTFILTER-NOT-OK
              WHEN FLG-ACCTFILTER-BLANK
                   MOVE -1             TO ACCTSIDL OF CCRDSLAI
              WHEN FLG-CARDFILTER-NOT-OK
              WHEN FLG-CARDFILTER-BLANK
                   MOVE -1             TO CARDSIDL OF CCRDSLAI
              WHEN OTHER
                   MOVE -1             TO ACCTSIDL OF CCRDSLAI
           END-EVALUATE

      *    SETUP COLOR
           IF  CDEMO-LAST-MAPSET   EQUAL LIT-CCLISTMAPSET
           AND CDEMO-FROM-PROGRAM  EQUAL LIT-CCLISTPGM
              MOVE DFHDFCOL     TO ACCTSIDC OF CCRDSLAO
              MOVE DFHDFCOL     TO CARDSIDC OF CCRDSLAO
           END-IF

           IF FLG-ACCTFILTER-NOT-OK
              MOVE DFHRED              TO ACCTSIDC OF CCRDSLAO
           END-IF

           IF FLG-CARDFILTER-NOT-OK
              MOVE DFHRED              TO CARDSIDC OF CCRDSLAO
           END-IF

           IF  FLG-ACCTFILTER-BLANK
           AND CDEMO-PGM-REENTER
               MOVE '*'                TO ACCTSIDO OF CCRDSLAO
               MOVE DFHRED             TO ACCTSIDC OF CCRDSLAO
           END-IF

           IF  FLG-CARDFILTER-BLANK
           AND CDEMO-PGM-REENTER
               MOVE '*'                TO CARDSIDO OF CCRDSLAO
               MOVE DFHRED             TO CARDSIDC OF CCRDSLAO
           END-IF

           IF  WS-NO-INFO-MESSAGE
               MOVE DFHBMDAR           TO INFOMSGC OF CCRDSLAO
           ELSE
               MOVE DFHNEUTR           TO INFOMSGC OF CCRDSLAO
           END-IF
           .
       1300-SETUP-SCREEN-ATTRS-EXIT.
            EXIT.


       1400-SEND-SCREEN.

           MOVE LIT-THISMAPSET         TO CCARD-NEXT-MAPSET
           MOVE LIT-THISMAP            TO CCARD-NEXT-MAP
           SET  CDEMO-PGM-REENTER TO TRUE

           EXEC CICS SEND MAP(CCARD-NEXT-MAP)
                          MAPSET(CCARD-NEXT-MAPSET)
                          FROM(CCRDSLAO)
                          CURSOR
                          ERASE
                          FREEKB
                          RESP(WS-RESP-CD)
           END-EXEC
           .
       1400-SEND-SCREEN-EXIT.
           EXIT
           .

       2000-PROCESS-INPUTS.
           PERFORM 2100-RECEIVE-MAP
              THRU 2100-RECEIVE-MAP-EXIT
           PERFORM 2200-EDIT-MAP-INPUTS
              THRU 2200-EDIT-MAP-INPUTS-EXIT
           MOVE WS-RETURN-MSG  TO CCARD-ERROR-MSG
           MOVE LIT-THISPGM    TO CCARD-NEXT-PROG
           MOVE LIT-THISMAPSET TO CCARD-NEXT-MAPSET
           MOVE LIT-THISMAP    TO CCARD-NEXT-MAP
           .

       2000-PROCESS-INPUTS-EXIT.
           EXIT
           .
       2100-RECEIVE-MAP.
           EXEC CICS RECEIVE MAP(LIT-THISMAP)
                     MAPSET(LIT-THISMAPSET)
                     INTO(CCRDSLAI)
                     RESP(WS-RESP-CD)
                     RESP2(WS-REAS-CD)
           END-EXEC
           .

       2100-RECEIVE-MAP-EXIT.
           EXIT
           .
       2200-EDIT-MAP-INPUTS.

           SET INPUT-OK                  TO TRUE
           SET FLG-CARDFILTER-ISVALID    TO TRUE
           SET FLG-ACCTFILTER-ISVALID    TO TRUE

      *    REPLACE * WITH LOW-VALUES
           IF  ACCTSIDI OF CCRDSLAI = '*'
           OR  ACCTSIDI OF CCRDSLAI = SPACES
               MOVE LOW-VALUES           TO  CC-ACCT-ID
           ELSE
               MOVE ACCTSIDI OF CCRDSLAI TO  CC-ACCT-ID
           END-IF

           IF  CARDSIDI OF CCRDSLAI = '*'
           OR  CARDSIDI OF CCRDSLAI = SPACES
               MOVE LOW-VALUES           TO  CC-CARD-NUM
           ELSE
               MOVE CARDSIDI OF CCRDSLAI TO  CC-CARD-NUM
           END-IF

      *    INDIVIDUAL FIELD EDITS
           PERFORM 2210-EDIT-ACCOUNT
              THRU 2210-EDIT-ACCOUNT-EXIT

           PERFORM 2220-EDIT-CARD
              THRU 2220-EDIT-CARD-EXIT

      *    CROSS FIELD EDITS
           IF  FLG-ACCTFILTER-BLANK
           AND FLG-CARDFILTER-BLANK
               SET NO-SEARCH-CRITERIA-RECEIVED TO TRUE
           END-IF
           .

       2200-EDIT-MAP-INPUTS-EXIT.
           EXIT
           .

       2210-EDIT-ACCOUNT.
           SET FLG-ACCTFILTER-NOT-OK TO TRUE

      *    Not supplied
           IF CC-ACCT-ID   EQUAL LOW-VALUES
           OR CC-ACCT-ID   EQUAL SPACES
           OR CC-ACCT-ID-N EQUAL ZEROS
              SET INPUT-ERROR           TO TRUE
              SET FLG-ACCTFILTER-BLANK  TO TRUE
              IF WS-RETURN-MSG-OFF
                 SET WS-PROMPT-FOR-ACCT TO TRUE
              END-IF
              MOVE ZEROES       TO CDEMO-ACCT-ID
              GO TO  2210-EDIT-ACCOUNT-EXIT
           END-IF
      *
      *    Not numeric
      *    Not 11 characters
           IF CC-ACCT-ID  IS NOT NUMERIC
              SET INPUT-ERROR TO TRUE
              SET FLG-ACCTFILTER-NOT-OK TO TRUE
              IF WS-RETURN-MSG-OFF
                MOVE
              'ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER'
                              TO WS-RETURN-MSG
              END-IF
              MOVE ZERO       TO CDEMO-ACCT-ID
              GO TO 2210-EDIT-ACCOUNT-EXIT
           ELSE
              MOVE CC-ACCT-ID TO CDEMO-ACCT-ID
              SET FLG-ACCTFILTER-ISVALID TO TRUE
           END-IF
           .

       2210-EDIT-ACCOUNT-EXIT.
           EXIT
           .

       2220-EDIT-CARD.
      *    Not numeric
      *    Not 16 characters
           SET FLG-CARDFILTER-NOT-OK TO TRUE

      *    Not supplied
           IF CC-CARD-NUM   EQUAL LOW-VALUES
           OR CC-CARD-NUM   EQUAL SPACES
           OR CC-CARD-NUM-N EQUAL ZEROS
              SET INPUT-ERROR           TO TRUE
              SET FLG-CARDFILTER-BLANK  TO TRUE
              IF WS-RETURN-MSG-OFF
                 SET WS-PROMPT-FOR-CARD TO TRUE
              END-IF

              MOVE ZEROES       TO CDEMO-CARD-NUM
              GO TO  2220-EDIT-CARD-EXIT
           END-IF
      *
      *    Not numeric
      *    Not 16 characters
           IF CC-CARD-NUM  IS NOT NUMERIC
              SET INPUT-ERROR TO TRUE
              SET FLG-CARDFILTER-NOT-OK TO TRUE
              IF WS-RETURN-MSG-OFF
                 MOVE
              'CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER'
                              TO WS-RETURN-MSG
              END-IF
              MOVE ZERO       TO CDEMO-CARD-NUM
              GO TO 2220-EDIT-CARD-EXIT
           ELSE
              MOVE CC-CARD-NUM-N TO CDEMO-CARD-NUM
              SET FLG-CARDFILTER-ISVALID TO TRUE
           END-IF
           .

       2220-EDIT-CARD-EXIT.
           EXIT
           .

      *****************************************************************
      * Plain text exit - Dont use in production                      *
      *****************************************************************
       SEND-PLAIN-TEXT.
           EXEC CICS SEND TEXT
                     FROM(WS-RETURN-MSG)
                     LENGTH(LENGTH OF WS-RETURN-MSG)
                     ERASE
                     FREEKB
           END-EXEC

           EXEC CICS RETURN
           END-EXEC
           .
       SEND-PLAIN-TEXT-EXIT.
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
           END-EXEC

           EXEC CICS HANDLE ABEND
                CANCEL
           END-EXEC

           EXEC CICS ABEND
                ABCODE('9999')
           END-EXEC
           .

      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:12:33 CDT
      *