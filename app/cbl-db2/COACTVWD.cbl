*****************************************************************
      * Program:     COACTVWD.CBL                                     *
      * Layer:       Business logic                                   *
      * Function:    Accept and process Account View request (DB2)    *
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
      * Unless required by applicable law or agreed to in F writing,
      * software distributed under the License is distributed on an
      * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
      * either express or implied. See the License for the specific
      * language governing permissions and limitations under the License
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           COACTVWD.
       DATE-WRITTEN.
           May 2022.
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
                                                   VALUE 'COACTVWD'.
          05 LIT-THISTRANID                        PIC X(4)
                                                   VALUE 'ADS9'.
          05 LIT-THISMAPSET                        PIC X(8)
                                                   VALUE 'COACTVW '.
          05 LIT-THISMAP                           PIC X(7)
                                                   VALUE 'CACTVWA'.
          05 LIT-CCLISTPGM                         PIC X(8)
                                                   VALUE 'COCRDLID'.
          05 LIT-CCLISTTRANID                      PIC X(4)
                                                   VALUE 'CCLI'.
          05 LIT-CCLISTMAPSET                      PIC X(7)
                                                   VALUE 'COCRDLI'.
          05 LIT-CCLISTMAP                         PIC X(7)
                                                   VALUE 'CCRDSLA'.
          05 LIT-CARDUPDATEPGM                           PIC X(8)
                                                   VALUE 'COCRDUPD'.
          05 LIT-CARDUDPATETRANID                        PIC X(4)
                                                   VALUE 'CCUP'.
          05 LIT-CARDUPDATEMAPSET                        PIC X(8)
                                                   VALUE 'COCRDUP '.
          05 LIT-CARDUPDATEMAP                           PIC X(7)
                                                   VALUE 'CCRDUPA'.

          05 LIT-MENUPGM                           PIC X(8)
                                                   VALUE 'COMEN01D'.
          05 LIT-MENUTRANID                        PIC X(4)
                                                   VALUE 'CM00'.
          05 LIT-MENUMAPSET                        PIC X(7)
                                                   VALUE 'COMEN01'.
          05 LIT-MENUMAP                           PIC X(7)
                                                   VALUE 'COMEN1A'.
          05  LIT-CARDDTLPGM                       PIC X(8)
                                                   VALUE 'COCRDSLD'.
          05  LIT-CARDDTLTRANID                    PIC X(4)
                                                   VALUE 'CCDL'.
          05  LIT-CARDDTLMAPSET                    PIC X(7)
                                                   VALUE 'COCRDSL'.
          05  LIT-CARDDTLMAP                       PIC X(7)
                                                   VALUE 'CCRDSLA'.
          05 LIT-ACCTFILENAME                      PIC X(8)
                                                   VALUE 'ACCTDAT '.
          05 LIT-CARDFILENAME                      PIC X(8)
                                                   VALUE 'CARDDAT '.
          05 LIT-CUSTFILENAME                      PIC X(8)
                                                   VALUE 'CUSTDAT '.
          05 LIT-CARDFILENAME-ACCT-PATH            PIC X(8)
                                                   VALUE 'CARDAIX '.
          05 LIT-CARDXREFNAME-ACCT-PATH            PIC X(8)
                                                   VALUE 'CXACAIX '.
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
      *DB2 SQL COMMUNICATION AREA
      ******************************************************************
       EXEC SQL
           INCLUDE SQLCA
       END-EXEC.

      ******************************************************************
      *DB2 HOST VARIABLES
      ******************************************************************
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  HV-ACCOUNT-ID                        PIC X(11).
       01  HV-CUSTOMER-ID                       PIC S9(09) COMP.
       01  HV-CARD-NUMBER                       PIC X(16).
       01  HV-SQLCODE-DISPLAY                   PIC S9(09) DISPLAY.
       01  HV-ACCOUNT-RECORD.
           05  HV-ACCT-ID                       PIC X(11).
           05  HV-ACCT-STATUS                   PIC X(01).
           05  HV-ACCT-CURR-BAL                 PIC S9(10)V99 COMP-3.
           05  HV-ACCT-CREDIT-LMT               PIC S9(10)V99 COMP-3.
           05  HV-ACCT-CASH-LMT                 PIC S9(10)V99 COMP-3.
           05  HV-ACCT-OPEN-DT                  PIC X(10).
           05  HV-ACCT-EXPIRY-DT                PIC X(10).
           05  HV-ACCT-REISSUE-DT               PIC X(10).
           05  HV-ACCT-CYC-CREDIT               PIC S9(10)V99 COMP-3.
           05  HV-ACCT-CYC-DEBIT                PIC S9(10)V99 COMP-3.
           05  HV-ACCT-ZIP                      PIC X(10).
           05  HV-ACCT-GROUP-ID                 PIC X(10).
       01  HV-CUSTOMER-RECORD.
           05  HV-CUST-ID                       PIC S9(09) COMP.
           05  HV-CUST-FNAME                    PIC X(25).
           05  HV-CUST-MNAME                    PIC X(25).
           05  HV-CUST-LNAME                    PIC X(25).
           05  HV-CUST-ADDR1                    PIC X(50).
           05  HV-CUST-ADDR2                    PIC X(50).
           05  HV-CUST-ADDR3                    PIC X(50).
           05  HV-CUST-STATE                    PIC X(02).
           05  HV-CUST-COUNTRY                  PIC X(03).
           05  HV-CUST-ZIP                      PIC X(10).
           05  HV-CUST-PHONE1                   PIC X(15).
           05  HV-CUST-PHONE2                   PIC X(15).
           05  HV-CUST-SSN                      PIC S9(09) COMP.
           05  HV-CUST-GOVT-ID                  PIC X(20).
           05  HV-CUST-DOB                      PIC X(10).
           05  HV-CUST-EFT-ID                   PIC X(10).
           05  HV-CUST-PRI-HOLDER               PIC X(01).
           05  HV-CUST-FICO                     PIC S9(03) COMP.
       EXEC SQL END DECLARE SECTION END-EXEC.

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

      *ACCOUNT RECORD LAYOUT
       COPY CVACT01Y.


      *CUSTOMER RECORD LAYOUT
       COPY CVACT02Y.

      *CARD XREF LAYOUT
       COPY CVACT03Y.

      *CUSTOMER LAYOUT
       COPY CVCUS01Y.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
         05  FILLER                                PIC X(1)
             OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.

       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY 'CRISS'
           EXEC CICS HANDLE ABEND
                     LABEL(ABEND-ROUTINE)
           END-EXEC
           DISPLAY 'CRISS0'
           INITIALIZE CC-WORK-AREA
                      WS-MISC-STORAGE
                      WS-COMMAREA
           DISPLAY 'CRISS1'
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
               DISPLAY 'ALAIN0'
              INITIALIZE CARDDEMO-COMMAREA
                         WS-THIS-PROGCOMMAREA
                         DISPLAY 'ALAIN1'
           ELSE
           DISPLAY 'ALAIN2'
              MOVE DFHCOMMAREA (1:LENGTH OF CARDDEMO-COMMAREA)  TO
                                CARDDEMO-COMMAREA
              MOVE DFHCOMMAREA(LENGTH OF CARDDEMO-COMMAREA + 1:
                               LENGTH OF WS-THIS-PROGCOMMAREA ) TO
                                WS-THIS-PROGCOMMAREA
                                DISPLAY 'ALAIN3'

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
              DISPLAY 'ALAIN5'
      ******************************************************************
      *            COMING FROM SOME OTHER CONTEXT
      *            SELECTION CRITERIA TO BE GATHERED
      ******************************************************************
                   PERFORM 1000-SEND-MAP THRU
                           1000-SEND-MAP-EXIT
                   GO TO COMMON-RETURN
              WHEN CDEMO-PGM-REENTER
              DISPLAY 'ALAIN7'
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
                 MOVE ACCT-ACTIVE-STATUS  TO ACSTTUSO OF CACTVWAO

                 MOVE ACCT-CURR-BAL       TO ACURBALO OF CACTVWAO

                 MOVE ACCT-CREDIT-LIMIT   TO ACRDLIMO OF CACTVWAO

                 MOVE ACCT-CASH-CREDIT-LIMIT
                                          TO ACSHLIMO OF CACTVWAO

                 MOVE ACCT-CURR-CYC-CREDIT
                                          TO ACRCYCRO OF CACTVWAO

                 MOVE ACCT-CURR-CYC-DEBIT TO ACRCYDBO OF CACTVWAO

                 MOVE ACCT-OPEN-DATE      TO ADTOPENO OF CACTVWAO
                 MOVE ACCT-EXPIRAION-DATE TO AEXPDTO  OF CACTVWAO
                 MOVE ACCT-REISSUE-DATE   TO AREISDTO OF CACTVWAO
                 MOVE ACCT-GROUP-ID       TO AADDGRPO OF CACTVWAO
              END-IF

              IF FOUND-CUST-IN-MASTER
                MOVE CUST-ID              TO ACSTNUMO OF CACTVWAO
      *         MOVE CUST-SSN             TO ACSTSSNO OF CACTVWAO
                STRING
                    CUST-SSN(1:3)
                    '-'
                    CUST-SSN(4:2)
                    '-'
                    CUST-SSN(6:4)
                    DELIMITED BY SIZE
                    INTO ACSTSSNO OF CACTVWAO
                END-STRING
                MOVE CUST-FICO-CREDIT-SCORE
                                          TO ACSTFCOO OF CACTVWAO
                MOVE CUST-DOB-YYYY-MM-DD  TO ACSTDOBO OF CACTVWAO
                MOVE CUST-FIRST-NAME      TO ACSFNAMO OF CACTVWAO
                MOVE CUST-MIDDLE-NAME     TO ACSMNAMO OF CACTVWAO
                MOVE CUST-LAST-NAME       TO ACSLNAMO OF CACTVWAO
                MOVE CUST-ADDR-LINE-1     TO ACSADL1O OF CACTVWAO
                MOVE CUST-ADDR-LINE-2     TO ACSADL2O OF CACTVWAO
                MOVE CUST-ADDR-LINE-3     TO ACSCITYO OF CACTVWAO
                MOVE CUST-ADDR-STATE-CD   TO ACSSTTEO OF CACTVWAO
                MOVE CUST-ADDR-ZIP        TO ACSZIPCO OF CACTVWAO
                MOVE CUST-ADDR-COUNTRY-CD TO ACSCTRYO OF CACTVWAO
                MOVE CUST-PHONE-NUM-1     TO ACSPHN1O OF CACTVWAO
                MOVE CUST-PHONE-NUM-2     TO ACSPHN2O OF CACTVWAO
                MOVE CUST-GOVT-ISSUED-ID  TO ACSGOVTO OF CACTVWAO
                MOVE CUST-EFT-ACCOUNT-ID  TO ACSEFTCO OF CACTVWAO
                MOVE CUST-PRI-CARD-HOLDER-IND
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

           PERFORM 9200-GETCARDXREF-BYACCT
              THRU 9200-GETCARDXREF-BYACCT-EXIT

      *    IF DID-NOT-FIND-ACCT-IN-CARDXREF
           IF FLG-ACCTFILTER-NOT-OK
              GO TO 9000-READ-ACCT-EXIT
           END-IF

           PERFORM 9300-GETACCTDATA-BYACCT
              THRU 9300-GETACCTDATA-BYACCT-EXIT

           IF DID-NOT-FIND-ACCT-IN-ACCTDAT
              GO TO 9000-READ-ACCT-EXIT
           END-IF

           MOVE CDEMO-CUST-ID TO WS-CARD-RID-CUST-ID

           PERFORM 9400-GETCUSTDATA-BYCUST
              THRU 9400-GETCUSTDATA-BYCUST-EXIT

           IF DID-NOT-FIND-CUST-IN-CUSTDAT
              GO TO 9000-READ-ACCT-EXIT
           END-IF


           .

       9000-READ-ACCT-EXIT.
           EXIT
           .
       9200-GETCARDXREF-BYACCT.

      *    Read the Card file using DB2 SQL
      *
           MOVE WS-CARD-RID-ACCT-ID TO HV-ACCOUNT-ID

           EXEC SQL
                SELECT CARD_NUM
                INTO :HV-CARD-NUMBER
                FROM CARDDAT
                WHERE CARD_ACCT_ID = :HV-ACCOUNT-ID
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                  MOVE HV-CARD-NUMBER TO CDEMO-CARD-NUM
      *           Customer ID will be obtained from account record
               WHEN 100
                  SET INPUT-ERROR                 TO TRUE
                  SET FLG-ACCTFILTER-NOT-OK       TO TRUE
                  IF WS-RETURN-MSG-OFF
                    MOVE SQLCODE TO HV-SQLCODE-DISPLAY
                    STRING
                    'Account:'
                     WS-CARD-RID-ACCT-ID-X
                    ' not found in'
                    ' Card file.  SQLCODE:'
                    HV-SQLCODE-DISPLAY
                    DELIMITED BY SIZE
                    INTO WS-RETURN-MSG
                    END-STRING
                  END-IF
               WHEN OTHER
                  SET INPUT-ERROR                 TO TRUE
                  SET FLG-ACCTFILTER-NOT-OK                TO TRUE
                  MOVE SQLCODE TO HV-SQLCODE-DISPLAY
                  STRING
                  'Error reading CARDDAT table. SQLCODE:'
                  HV-SQLCODE-DISPLAY
                  DELIMITED BY SIZE
                  INTO WS-RETURN-MSG
                  END-STRING
           END-EVALUATE
           .
       9200-GETCARDXREF-BYACCT-EXIT.
           EXIT
           .
       9300-GETACCTDATA-BYACCT.

           MOVE WS-CARD-RID-ACCT-ID TO HV-ACCOUNT-ID

           EXEC SQL
                SELECT ACCT_ID, ACCT_ACTIVE_STATUS, ACCT_CURR_BAL,
                       ACCT_CREDIT_LIMIT, ACCT_CASH_CREDIT_LIMIT,
                       ACCT_OPEN_DATE, ACCT_EXPIRAION_DATE,
                       ACCT_REISSUE_DATE, ACCT_CURR_CYC_CREDIT,
                       ACCT_CURR_CYC_DEBIT, ACCT_ADDR_ZIP,
                       ACCT_GROUP_ID
                INTO :HV-ACCT-ID, :HV-ACCT-STATUS, :HV-ACCT-CURR-BAL,
                     :HV-ACCT-CREDIT-LMT, :HV-ACCT-CASH-LMT,
                     :HV-ACCT-OPEN-DT, :HV-ACCT-EXPIRY-DT,
                     :HV-ACCT-REISSUE-DT, :HV-ACCT-CYC-CREDIT,
                     :HV-ACCT-CYC-DEBIT, :HV-ACCT-ZIP,
                     :HV-ACCT-GROUP-ID
                FROM ACCTDAT
                WHERE ACCT_ID = :HV-ACCOUNT-ID
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                  SET FOUND-ACCT-IN-MASTER        TO TRUE
                  MOVE HV-ACCT-ID                 TO ACCT-ID
                  MOVE HV-ACCT-STATUS             TO ACCT-ACTIVE-STATUS
                  MOVE HV-ACCT-CURR-BAL           TO ACCT-CURR-BAL
                  MOVE HV-ACCT-CREDIT-LMT         TO ACCT-CREDIT-LIMIT
                  MOVE HV-ACCT-CASH-LMT
                    TO ACCT-CASH-CREDIT-LIMIT
                  MOVE HV-ACCT-OPEN-DT            TO ACCT-OPEN-DATE
                  MOVE HV-ACCT-EXPIRY-DT          TO ACCT-EXPIRAION-DATE
                  MOVE HV-ACCT-REISSUE-DT         TO ACCT-REISSUE-DATE
                  MOVE HV-ACCT-CYC-CREDIT
                    TO ACCT-CURR-CYC-CREDIT
                  MOVE HV-ACCT-CYC-DEBIT          TO ACCT-CURR-CYC-DEBIT
                  MOVE HV-ACCT-ZIP                TO ACCT-ADDR-ZIP
                  MOVE HV-ACCT-GROUP-ID           TO ACCT-GROUP-ID
               WHEN 100
                  SET INPUT-ERROR                 TO TRUE
                  SET FLG-ACCTFILTER-NOT-OK       TO TRUE
                  IF WS-RETURN-MSG-OFF
                    MOVE SQLCODE TO HV-SQLCODE-DISPLAY
                    STRING
                    'Account:'
                     WS-CARD-RID-ACCT-ID-X
                    ' not found in'
                    ' Acct Master file. SQLCODE:'
                    HV-SQLCODE-DISPLAY
                    DELIMITED BY SIZE
                    INTO WS-RETURN-MSG
                    END-STRING
                  END-IF
               WHEN OTHER
                  SET INPUT-ERROR                 TO TRUE
                  SET FLG-ACCTFILTER-NOT-OK                TO TRUE
                  MOVE SQLCODE TO HV-SQLCODE-DISPLAY
                  STRING
                  'Error reading ACCTDAT table. SQLCODE:'
                  HV-SQLCODE-DISPLAY
                  DELIMITED BY SIZE
                  INTO WS-RETURN-MSG
                  END-STRING
           END-EVALUATE
           .
       9300-GETACCTDATA-BYACCT-EXIT.
           EXIT
           .

       9400-GETCUSTDATA-BYCUST.
      *    For now, we'll assume customer ID is derived from account ID
      *    In a real implementation, this would come from a proper
      *    cross-reference table or account-customer relationship
           MOVE WS-CARD-RID-ACCT-ID TO HV-CUSTOMER-ID

           EXEC SQL
                SELECT CUST_ID, CUST_FIRST_NAME, CUST_MIDDLE_NAME,
                       CUST_LAST_NAME, CUST_ADDR_LINE_1,
                       CUST_ADDR_LINE_2, CUST_ADDR_LINE_3,
                       CUST_ADDR_STATE_CD, CUST_ADDR_COUNTRY_CD,
                       CUST_ADDR_ZIP, CUST_PHONE_NUM_1,
                       CUST_PHONE_NUM_2, CUST_SSN,
                       CUST_GOVT_ISSUED_ID, CUST_DOB_YYYY_MM_DD,
                       CUST_EFT_ACCOUNT_ID, CUST_PRI_CARD_HOLDER_IND,
                       CUST_FICO_CREDIT_SCORE
                INTO :HV-CUST-ID, :HV-CUST-FNAME, :HV-CUST-MNAME,
                     :HV-CUST-LNAME, :HV-CUST-ADDR1,
                     :HV-CUST-ADDR2, :HV-CUST-ADDR3,
                     :HV-CUST-STATE, :HV-CUST-COUNTRY,
                     :HV-CUST-ZIP, :HV-CUST-PHONE1,
                     :HV-CUST-PHONE2, :HV-CUST-SSN,
                     :HV-CUST-GOVT-ID, :HV-CUST-DOB,
                     :HV-CUST-EFT-ID, :HV-CUST-PRI-HOLDER,
                     :HV-CUST-FICO
                FROM CUSTDAT
                WHERE CUST_ID = :HV-CUSTOMER-ID
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                  SET FOUND-CUST-IN-MASTER        TO TRUE
                  MOVE HV-CUST-ID                 TO CUST-ID
                  MOVE HV-CUST-FNAME              TO CUST-FIRST-NAME
                  MOVE HV-CUST-MNAME              TO CUST-MIDDLE-NAME
                  MOVE HV-CUST-LNAME              TO CUST-LAST-NAME
                  MOVE HV-CUST-ADDR1              TO CUST-ADDR-LINE-1
                  MOVE HV-CUST-ADDR2              TO CUST-ADDR-LINE-2
                  MOVE HV-CUST-ADDR3              TO CUST-ADDR-LINE-3
                  MOVE HV-CUST-STATE              TO CUST-ADDR-STATE-CD
                  MOVE HV-CUST-COUNTRY
                    TO CUST-ADDR-COUNTRY-CD
                  MOVE HV-CUST-ZIP                TO CUST-ADDR-ZIP
                  MOVE HV-CUST-PHONE1             TO CUST-PHONE-NUM-1
                  MOVE HV-CUST-PHONE2             TO CUST-PHONE-NUM-2
                  MOVE HV-CUST-SSN                TO CUST-SSN
                  MOVE HV-CUST-GOVT-ID            TO CUST-GOVT-ISSUED-ID
                  MOVE HV-CUST-DOB                TO CUST-DOB-YYYY-MM-DD
                  MOVE HV-CUST-EFT-ID             TO CUST-EFT-ACCOUNT-ID
                  MOVE HV-CUST-PRI-HOLDER
                    TO CUST-PRI-CARD-HOLDER-IND
                  MOVE HV-CUST-FICO
                    TO CUST-FICO-CREDIT-SCORE
                  MOVE HV-CUST-ID                 TO CDEMO-CUST-ID
               WHEN 100
                  SET INPUT-ERROR                 TO TRUE
                  SET FLG-CUSTFILTER-NOT-OK       TO TRUE
                  IF WS-RETURN-MSG-OFF
                    MOVE SQLCODE TO HV-SQLCODE-DISPLAY
                    STRING
                    'CustId not found'
                    ' in customer master. SQLCODE: '
                    HV-SQLCODE-DISPLAY
                    DELIMITED BY SIZE
                    INTO WS-RETURN-MSG
                    END-STRING
                  END-IF
               WHEN OTHER
                  SET INPUT-ERROR                 TO TRUE
                  SET FLG-CUSTFILTER-NOT-OK                TO TRUE
                  MOVE SQLCODE TO HV-SQLCODE-DISPLAY
                  STRING
                  'Error reading CUSTDAT table. SQLCODE:'
                  HV-SQLCODE-DISPLAY
                  DELIMITED BY SIZE
                  INTO WS-RETURN-MSG
                  END-STRING
           END-EVALUATE
           .
       9400-GETCUSTDATA-BYCUST-EXIT.
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