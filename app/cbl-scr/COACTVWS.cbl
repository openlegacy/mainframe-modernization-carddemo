      ******************************************************************
      * Program:     COACTVWS.CBL                                     *
      * Layer:       Presentation                                     *
      * Function:    Account View Screen - calls COACTVWL RPC         *
      * Transaction: ALS9                                             *
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
           COACTVWS.
       DATE-WRITTEN.
           June 2025.
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
            07 WS-RESP-DISP                          PIC 9(09)
                                                   VALUE ZEROS.
            07 WS-REAS-DISP                        PIC 9(09)
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
         05  WS-PFK-FLAG                           PIC X(1).
           88  PFK-VALID                           VALUE '0'.
           88  PFK-INVALID                         VALUE '1'.
           88  INPUT-PENDING                       VALUE LOW-VALUES.
         05  WS-EDIT-ACCT-FLAG                     PIC X(1).
           88  FLG-ACCTFILTER-NOT-OK               VALUE '0'.
           88  FLG-ACCTFILTER-ISVALID              VALUE '1'.
           88  FLG-ACCTFILTER-BLANK                VALUE ' '.
         05  WS-EDIT-CUST-FLAG                     PIC X(1).
           88  FLG-CUSTFILTER-NOT-OK               VALUE '0'.
           88  FLG-CUSTFILTER-ISVALID              VALUE '1'.
           88  FLG-CUSTFILTER-BLANK                VALUE ' '.
      ******************************************************************
      * Output edits
      ******************************************************************
      *  05  EDIT-FIELD-9-2                PIC +ZZZ,ZZZ,ZZZ.99.
      ******************************************************************
      *      File and data Handling
      ******************************************************************
         05  WS-XREF-RID.
           10  WS-CARD-RID-CARDNUM                 PIC X(16).
           10  WS-CARD-RID-CUST-ID                 PIC 9(09).
           10  WS-CARD-RID-CUST-ID-X REDEFINES
                  WS-CARD-RID-CUST-ID              PIC X(09).
           10  WS-CARD-RID-ACCT-ID                 PIC 9(11).
           10  WS-CARD-RID-ACCT-ID-X REDEFINES
                  WS-CARD-RID-ACCT-ID              PIC X(11).
         05  WS-FILE-READ-FLAGS.
           10 WS-ACCOUNT-MASTER-READ-FLAG          PIC X(1).
              88 FOUND-ACCT-IN-MASTER              VALUE '1'.
           10 WS-CUST-MASTER-READ-FLAG             PIC X(1).
              88 FOUND-CUST-IN-MASTER              VALUE '1'.
         05  WS-FILE-ERROR-MESSAGE.
           10  FILLER                              PIC X(12)
                                                   VALUE 'File Error: '.
           10  ERROR-OPNAME                        PIC X(8)
                                                   VALUE SPACES.
           10  FILLER                              PIC X(4)
                                                   VALUE ' on '.
           10  ERROR-FILE                          PIC X(9)
                                                   VALUE SPACES.
           10  FILLER                              PIC X(15)
                                                   VALUE
                                                   ' returned RESP '.
           10  ERROR-RESP                          PIC X(10)
                                                   VALUE SPACES.
           10  FILLER                              PIC X(7)
                                                   VALUE ',RESP2 '.
           10  ERROR-RESP2                         PIC X(10)
                                                   VALUE SPACES.
          10  FILLER                               PIC X(5)
                                                   VALUE SPACES.
      ******************************************************************
      *      Output Message Construction
      ******************************************************************
         05  WS-LONG-MSG                           PIC X(500).
         05  WS-INFO-MSG                           PIC X(40).
           88  WS-NO-INFO-MESSAGE                 VALUES
                                                  SPACES LOW-VALUES.
           88  WS-PROMPT-FOR-INPUT                 VALUE
               'Enter or update id of account to display'.
           88  WS-INFORM-OUTPUT                    VALUE
               'Displaying details of given Account'.
         05  WS-RETURN-MSG                         PIC X(75).
           88  WS-RETURN-MSG-OFF                   VALUE SPACES.
           88  WS-EXIT-MESSAGE                     VALUE
               'PF03 pressed.Exiting              '.
           88  WS-PROMPT-FOR-ACCT                  VALUE
               'Account number not provided'.
           88  NO-SEARCH-CRITERIA-RECEIVED         VALUE
               'No input received'.
           88  SEARCHED-ACCT-ZEROES                VALUE
               'Account number must be a non zero 11 digit number'.
           88  SEARCHED-ACCT-NOT-NUMERIC           VALUE
               'Account number must be a non zero 11 digit number'.
           88  DID-NOT-FIND-ACCT-IN-CARDXREF       VALUE
               'Did not find this account in account card xref file'.
           88  DID-NOT-FIND-ACCT-IN-ACCTDAT        VALUE
               'Did not find this account in account master file'.
           88  DID-NOT-FIND-CUST-IN-CUSTDAT        VALUE
               'Did not find associated customer in master file'.
           88  XREF-READ-ERROR                     VALUE
               'Error reading account card xref File'.
           88  CODING-TO-BE-DONE                   VALUE
               'Looks Good.... so far'.
      *****************************************************************
      *      Literals and Constants
      ******************************************************************
       01 WS-LITERALS.
          05 LIT-THISPGM                           PIC X(8)
                                                   VALUE 'COACTVWS'.
          05 LIT-THISTRANID                        PIC X(4)
                                                   VALUE 'ALS9'.
          05 LIT-THISMAPSET                        PIC X(8)
                                                   VALUE 'COACTVW '.
          05 LIT-THISMAP                           PIC X(7)
                                                   VALUE 'CACTVWA'.
          05 LIT-CCLISTPGM                         PIC X(8)
                                                   VALUE 'COCRDLIC'.
          05 LIT-CCLISTTRANID                      PIC X(4)
                                                   VALUE 'CCLI'.
          05 LIT-CCLISTMAPSET                      PIC X(7)
                                                   VALUE 'COCRDLI'.
          05 LIT-CCLISTMAP                         PIC X(7)
                                                   VALUE 'CCRDSLA'.
          05 LIT-CARDUPDATEPGM                           PIC X(8)
                                                   VALUE 'COCRDUPC'.
          05 LIT-CARDUDPATETRANID                        PIC X(4)
                                                   VALUE 'CCUP'.
          05 LIT-CARDUPDATEMAPSET                        PIC X(8)
                                                   VALUE 'COCRDUP '.
          05 LIT-CARDUPDATEMAP                           PIC X(7)
                                                   VALUE 'CCRDUPA'.

          05 LIT-MENUPGM                           PIC X(8)
                                                   VALUE 'COMEN01S'.
          05 LIT-MENUTRANID                        PIC X(4)
                                                   VALUE 'ALUM'.
          05 LIT-MENUMAPSET                        PIC X(7)
                                                   VALUE 'COMEN01'.
          05 LIT-MENUMAP                           PIC X(7)
                                                   VALUE 'COMEN1A'.
          05  LIT-CARDDTLPGM                       PIC X(8)
                                                   VALUE 'COCRDSLC'.
          05  LIT-CARDDTLTRANID                    PIC X(4)
                                                   VALUE 'CCDL'.
          05  LIT-CARDDTLMAPSET                    PIC X(7)
                                                   VALUE 'COCRDSL'.
          05  LIT-CARDDTLMAP                       PIC X(7)
                                                   VALUE 'CCRDSLA'.
          05 LIT-RPC-PROGRAM                       PIC X(8)
                                                   VALUE 'COACTVWL'.
          05 LIT-ALL-ALPHA-FROM                    PIC X(52)
             VALUE
             'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'.
          05 LIT-ALL-SPACES-TO                     PIC X(52)
                                                   VALUE SPACES.
          05 LIT-UPPER                             PIC X(26)
                                 VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
          05 LIT-LOWER                             PIC X(26)
                                 VALUE 'abcdefghijklmnopqrstuvwxyz'.

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

      *IBM SUPPLIED COPYBOOKS
       COPY DFHBMSCA.
       COPY DFHAID.

      *COMMON COPYBOOKS
      *Screen Titles
       COPY COTTL01Y.

      *BMS Copybook
       COPY COACTVW.

      *Current Date
       COPY CSDAT01Y.

      *Common Messages
       COPY CSMSG01Y.

      *Abend Variables
       COPY CSMSG02Y.

      *Signed on user data
       COPY CSUSR01Y.

      * RPC Communication Area - MUST MATCH COACTVWL EXACTLY
       01 WS-RPC-COMMAREA.
          05 LK-INPUT-PARMS.
             10 LK-IN-ACCT-ID             PIC X(11).
          05 LK-OUTPUT-STATUS.
             10 LK-OUT-RETURN-CODE        PIC 9(02).
                88 RC-SUCCESS             VALUE 00.
                88 RC-NOT-FOUND           VALUE 01.
                88 RC-INPUT-ERROR         VALUE 03.
                88 RC-DATABASE-ERROR      VALUE 99.
             10 LK-OUT-MESSAGE            PIC X(80).
          05 LK-OUTPUT-DATA.
             10 LK-OUT-ACCT-DATA.
                15 LK-OUT-ACCT-ID              PIC X(11).
                15 LK-OUT-ACCT-ACTIVE-STATUS   PIC X(01).
                15 LK-OUT-ACCT-CREDIT-LIMIT    PIC S9(10)V99.
                15 LK-OUT-ACCT-CASH-LIMIT      PIC S9(10)V99.
                15 LK-OUT-ACCT-CURR-BAL        PIC S9(10)V99.
                15 LK-OUT-ACCT-CURR-CYC-CREDIT PIC S9(10)V99.
                15 LK-OUT-ACCT-CURR-CYC-DEBIT  PIC S9(10)V99.
                15 LK-OUT-ACCT-OPEN-DATE       PIC X(10).
                15 LK-OUT-ACCT-EXPIRATION-DATE PIC X(10).
                15 LK-OUT-ACCT-REISSUE-DATE    PIC X(10).
                15 LK-OUT-ACCT-GROUP-ID        PIC X(10).
                15 LK-OUT-ACCT-CARD-NUM        PIC X(16).
             10 LK-OUT-CUST-DATA.
                15 LK-OUT-CUST-ID              PIC X(9).
                15 LK-OUT-CUST-FIRST-NAME      PIC X(25).
                15 LK-OUT-CUST-MIDDLE-NAME     PIC X(25).
                15 LK-OUT-CUST-LAST-NAME       PIC X(25).
                15 LK-OUT-CUST-SSN             PIC X(9).
                15 LK-OUT-CUST-DOB             PIC X(10).
                15 LK-OUT-CUST-ADDR-LINE-1     PIC X(50).
                15 LK-OUT-CUST-ADDR-LINE-2     PIC X(50).
                15 LK-OUT-CUST-ADDR-LINE-3     PIC X(50).
                15 LK-OUT-CUST-ADDR-STATE-CD   PIC X(2).
                15 LK-OUT-CUST-ADDR-COUNTRY-CD PIC X(3).
                15 LK-OUT-CUST-ADDR-ZIP        PIC X(10).
                15 LK-OUT-CUST-PHONE-NUM-1     PIC X(15).
                15 LK-OUT-CUST-PHONE-NUM-2     PIC X(15).
                15 LK-OUT-CUST-GOVT-ISSUED-ID  PIC X(20).
                15 LK-OUT-CUST-EFT-ACCOUNT-ID  PIC  X(10).
                15 LK-OUT-CUST-PRI-HOLDER-IND  PIC X(1).
                15 LK-OUT-CUST-FICO-SCORE      PIC 9(3).

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
      *****************************************************************
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
                      PERFORM 9000-READ-ACCT
                         THRU 9000-READ-ACCT-EXIT
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
       0000-MAIN-EXIT.
           EXIT
           .


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
           MOVE LOW-VALUES             TO CACTVWAO

           MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA

           MOVE CCDA-TITLE01           TO TITLE01O OF CACTVWAO
           MOVE CCDA-TITLE02           TO TITLE02O OF CACTVWAO
           MOVE LIT-THISTRANID         TO TRNNAMEO OF CACTVWAO
           MOVE LIT-THISPGM            TO PGMNAMEO OF CACTVWAO

           MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA

           MOVE WS-CURDATE-MONTH       TO WS-CURDATE-MM
           MOVE WS-CURDATE-DAY         TO WS-CURDATE-DD
           MOVE WS-CURDATE-YEAR(3:2)   TO WS-CURDATE-YY

           MOVE WS-CURDATE-MM-DD-YY    TO CURDATEO OF CACTVWAO

           MOVE WS-CURTIME-HOURS       TO WS-CURTIME-HH
           MOVE WS-CURTIME-MINUTE      TO WS-CURTIME-MM
           MOVE WS-CURTIME-SECOND      TO WS-CURTIME-SS

           MOVE WS-CURTIME-HH-MM-SS    TO CURTIMEO OF CACTVWAO

           .

       1100-SCREEN-INIT-EXIT.
           EXIT
           .
       1200-SETUP-SCREEN-VARS.
      *    INITIALIZE SEARCH CRITERIA
           IF EIBCALEN = 0
              SET  WS-PROMPT-FOR-INPUT TO TRUE
           ELSE
              IF FLG-ACCTFILTER-BLANK
                 MOVE LOW-VALUES   TO ACCTSIDO OF CACTVWAO
              ELSE
                 MOVE CC-ACCT-ID   TO ACCTSIDO OF CACTVWAO
              END-IF

              IF FOUND-ACCT-IN-MASTER
              OR FOUND-CUST-IN-MASTER
                 MOVE LK-OUT-ACCT-ACTIVE-STATUS  TO ACSTTUSO OF CACTVWAO

                 MOVE LK-OUT-ACCT-CURR-BAL       TO ACURBALO OF CACTVWAO

                 MOVE LK-OUT-ACCT-CREDIT-LIMIT   TO ACRDLIMO OF CACTVWAO

                 MOVE LK-OUT-ACCT-CASH-LIMIT
                                          TO ACSHLIMO OF CACTVWAO

                 MOVE LK-OUT-ACCT-CURR-CYC-CREDIT
                                          TO ACRCYCRO OF CACTVWAO

                 MOVE LK-OUT-ACCT-CURR-CYC-DEBIT TO ACRCYDBO OF CACTVWAO

                 MOVE LK-OUT-ACCT-OPEN-DATE      TO ADTOPENO OF CACTVWAO
                 MOVE LK-OUT-ACCT-EXPIRATION-DATE TO AEXPDTO OF CACTVWAO
                 MOVE LK-OUT-ACCT-REISSUE-DATE   TO AREISDTO OF CACTVWAO
                 MOVE LK-OUT-ACCT-GROUP-ID       TO AADDGRPO OF CACTVWAO
              END-IF

              IF FOUND-CUST-IN-MASTER
                MOVE LK-OUT-CUST-ID              TO ACSTNUMO OF CACTVWAO
      *         MOVE CUST-SSN             TO ACSTSSNO OF CACTVWAO
                STRING
                    LK-OUT-CUST-SSN(1:3)
                    '-'
                    LK-OUT-CUST-SSN(4:2)
                    '-'
                    LK-OUT-CUST-SSN(6:4)
                    DELIMITED BY SIZE
                    INTO ACSTSSNO OF CACTVWAO
                END-STRING
                MOVE LK-OUT-CUST-FICO-SCORE
                                          TO ACSTFCOO OF CACTVWAO
                MOVE LK-OUT-CUST-DOB  TO ACSTDOBO OF CACTVWAO
                MOVE LK-OUT-CUST-FIRST-NAME      TO ACSFNAMO OF CACTVWAO
                MOVE LK-OUT-CUST-MIDDLE-NAME     TO ACSMNAMO OF CACTVWAO
                MOVE LK-OUT-CUST-LAST-NAME       TO ACSLNAMO OF CACTVWAO
                MOVE LK-OUT-CUST-ADDR-LINE-1     TO ACSADL1O OF CACTVWAO
                MOVE LK-OUT-CUST-ADDR-LINE-2     TO ACSADL2O OF CACTVWAO
                MOVE LK-OUT-CUST-ADDR-LINE-3     TO ACSCITYO OF CACTVWAO
                MOVE LK-OUT-CUST-ADDR-STATE-CD   TO ACSSTTEO OF CACTVWAO
                MOVE LK-OUT-CUST-ADDR-ZIP        TO ACSZIPCO OF CACTVWAO
                MOVE LK-OUT-CUST-ADDR-COUNTRY-CD TO ACSCTRYO OF CACTVWAO
                MOVE LK-OUT-CUST-PHONE-NUM-1     TO ACSPHN1O OF CACTVWAO
                MOVE LK-OUT-CUST-PHONE-NUM-2     TO ACSPHN2O OF CACTVWAO
                MOVE LK-OUT-CUST-GOVT-ISSUED-ID  TO ACSGOVTO OF CACTVWAO
                MOVE LK-OUT-CUST-EFT-ACCOUNT-ID  TO ACSEFTCO OF CACTVWAO
                MOVE LK-OUT-CUST-PRI-HOLDER-IND
                                          TO ACSPFLGO OF CACTVWAO
              END-IF

            END-IF

      *    SETUP MESSAGE
           IF WS-NO-INFO-MESSAGE
             SET WS-PROMPT-FOR-INPUT TO TRUE
           END-IF

           MOVE WS-RETURN-MSG          TO ERRMSGO OF CACTVWAO

           MOVE WS-INFO-MSG            TO INFOMSGO OF CACTVWAO
           .

       1200-SETUP-SCREEN-VARS-EXIT.
           EXIT
           .

       1300-SETUP-SCREEN-ATTRS.
      *    PROTECT OR UNPROTECT BASED ON CONTEXT
           MOVE DFHBMFSE               TO ACCTSIDA OF CACTVWAI

      *    POSITION CURSOR
           EVALUATE TRUE
              WHEN FLG-ACCTFILTER-NOT-OK
              WHEN FLG-ACCTFILTER-BLANK
                   MOVE -1             TO ACCTSIDL OF CACTVWAI
              WHEN OTHER
                   MOVE -1             TO ACCTSIDL OF CACTVWAI
           END-EVALUATE

      *    SETUP COLOR
           MOVE DFHDFCOL               TO ACCTSIDC OF CACTVWAO

           IF FLG-ACCTFILTER-NOT-OK
              MOVE DFHRED              TO ACCTSIDC OF CACTVWAO
           END-IF

           IF  FLG-ACCTFILTER-BLANK
           AND CDEMO-PGM-REENTER
               MOVE '*'                TO ACCTSIDO OF CACTVWAO
               MOVE DFHRED             TO ACCTSIDC OF CACTVWAO
           END-IF

           IF  WS-NO-INFO-MESSAGE
               MOVE DFHBMDAR           TO INFOMSGC OF CACTVWAO
           ELSE
               MOVE DFHNEUTR           TO INFOMSGC OF CACTVWAO
           END-IF
           .

       1300-SETUP-SCREEN-ATTRS-EXIT.
           EXIT
           .
       1400-SEND-SCREEN.

           MOVE LIT-THISMAPSET         TO CCARD-NEXT-MAPSET
           MOVE LIT-THISMAP            TO CCARD-NEXT-MAP
           SET  CDEMO-PGM-REENTER TO TRUE

           EXEC CICS SEND MAP(CCARD-NEXT-MAP)
                          MAPSET(CCARD-NEXT-MAPSET)
                          FROM(CACTVWAO)
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
                     INTO(CACTVWAI)
                     RESP(WS-RESP-CD)
                     RESP2(WS-REAS-CD)
           END-EXEC
           .

       2100-RECEIVE-MAP-EXIT.
           EXIT
           .
       2200-EDIT-MAP-INPUTS.

           SET INPUT-OK                  TO TRUE
           SET FLG-ACCTFILTER-ISVALID    TO TRUE

      *    REPLACE * WITH LOW-VALUES
           IF  ACCTSIDI OF CACTVWAI = '*'
           OR  ACCTSIDI OF CACTVWAI = SPACES
               MOVE LOW-VALUES           TO  CC-ACCT-ID
           ELSE
               MOVE ACCTSIDI OF CACTVWAI TO  CC-ACCT-ID
           END-IF

      *    INDIVIDUAL FIELD EDITS
           PERFORM 2210-EDIT-ACCOUNT
              THRU 2210-EDIT-ACCOUNT-EXIT

      *    CROSS FIELD EDITS
           IF  FLG-ACCTFILTER-BLANK
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
           OR CC-ACCT-ID  EQUAL ZEROES
              SET INPUT-ERROR TO TRUE
              SET FLG-ACCTFILTER-NOT-OK TO TRUE
              IF WS-RETURN-MSG-OFF
                MOVE
              'Account Filter must  be a non-zero 11 digit number'
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

       9000-READ-ACCT.

           SET  WS-NO-INFO-MESSAGE  TO TRUE

           MOVE CDEMO-ACCT-ID TO WS-CARD-RID-ACCT-ID

           PERFORM 9100-READ-ACCT-VIA-RPC
              THRU 9100-READ-ACCT-VIA-RPC-EXIT

      *    Check if we got the data successfully
           IF RC-SUCCESS
              SET FOUND-ACCT-IN-MASTER TO TRUE
              SET FOUND-CUST-IN-MASTER TO TRUE
              SET WS-INFORM-OUTPUT     TO TRUE
           END-IF

           .

       9000-READ-ACCT-EXIT.
           EXIT
           .

       9100-READ-ACCT-VIA-RPC.
           INITIALIZE WS-RPC-COMMAREA

           MOVE CDEMO-ACCT-ID TO LK-IN-ACCT-ID

           EXEC CICS LINK
                PROGRAM(LIT-RPC-PROGRAM)
                COMMAREA(WS-RPC-COMMAREA)
                LENGTH(LENGTH OF WS-RPC-COMMAREA)
                RESP(WS-RESP-CD)
                RESP2(WS-REAS-CD)
           END-EXEC.

           MOVE WS-RESP-CD    TO WS-RESP-DISP.
           MOVE WS-REAS-CD    TO WS-REAS-DISP.

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   IF RC-SUCCESS
                       CONTINUE
                   ELSE
                       SET INPUT-ERROR TO TRUE
                       SET FLG-ACCTFILTER-NOT-OK TO TRUE
                       MOVE LK-OUT-MESSAGE TO WS-RETURN-MSG
                   END-IF
               WHEN DFHRESP(PGMIDERR)
                   SET INPUT-ERROR TO TRUE
                   SET FLG-ACCTFILTER-NOT-OK TO TRUE
                   MOVE 'COACTVWL program not found' TO WS-RETURN-MSG
               WHEN OTHER
                   SET INPUT-ERROR TO TRUE
                   SET FLG-ACCTFILTER-NOT-OK TO TRUE
                   STRING 'Error calling COACTVWL. RESP='
                          WS-RESP-DISP
                          ' RESP2='
                          WS-REAS-DISP
                     DELIMITED BY SIZE
                     INTO WS-RETURN-MSG
                   END-STRING
           END-EVALUATE
           .
       9100-READ-ACCT-VIA-RPC-EXIT.
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
      *****************************************************************
      * Display Long text and exit                                    *
      * This is primarily for debugging and should not be used in     *
      * regular course                                                *
      *****************************************************************
       SEND-LONG-TEXT.
           EXEC CICS SEND TEXT
                     FROM(WS-LONG-MSG)
                     LENGTH(LENGTH OF WS-LONG-MSG)
                     ERASE
                     FREEKB
           END-EXEC

           EXEC CICS RETURN
           END-EXEC
           .
       SEND-LONG-TEXT-EXIT.
           EXIT
           .
      *****************************************************************
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
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:12:32 CDT
      *