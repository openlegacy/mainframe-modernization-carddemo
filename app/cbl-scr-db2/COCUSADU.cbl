******************************************************************
      * Program:     COCUSADU.CBL                                     *
      * Layer:       Presentation                                     *
      * Function:    Customer Creation Screen - calls COCUSADA RPC    *
      * Transaction: AASD                                             *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           COCUSADU.
       DATE-WRITTEN.
           August 2025.
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
            07 WS-RESP-DISP                        PIC 9(09)
                                                   VALUE ZEROS.
            07 WS-REAS-DISP                        PIC 9(09)
                                                   VALUE ZEROS.
            07 WS-TRANID                           PIC X(4)
                                                   VALUE SPACES.

      ******************************************************************
      * Target Customer ID - NEW FOR ACTADU INTEGRATION
      ******************************************************************
         05  WS-TARGET-CUST-ID                    PIC 9(09) VALUE ZEROS.

      ******************************************************************
      * Input validation flags
      ******************************************************************
         05  WS-INPUT-FLAG                         PIC X(1).
           88  INPUT-OK                            VALUE '0'.
           88  INPUT-ERROR                         VALUE '1'.
           88  INPUT-PENDING                       VALUE LOW-VALUES.
         05  WS-PFK-FLAG                           PIC X(1).
           88  PFK-VALID                           VALUE '0'.
           88  PFK-INVALID                         VALUE '1'.

      ******************************************************************
      * Message handling - ENHANCED
      ******************************************************************
         05  WS-MESSAGE                            PIC X(80).
         05  WS-ERR-FLG                            PIC X(01) VALUE 'N'.
           88  ERR-FLG-ON                          VALUE 'Y'.
           88  ERR-FLG-OFF                         VALUE 'N'.

      ******************************************************************
      * Field persistence storage - LIKE ACTU/ACTA PATTERN
      ******************************************************************
         05  WS-FIELD-PERSISTENCE.
           10 WS-PERSIST-FNAME                     PIC X(25).
           10 WS-PERSIST-MNAME                     PIC X(25).
           10 WS-PERSIST-LNAME                     PIC X(25).
           10 WS-PERSIST-ADDR1                     PIC X(50).
           10 WS-PERSIST-ADDR2                     PIC X(50).
           10 WS-PERSIST-CITY                      PIC X(50).
           10 WS-PERSIST-STATE                     PIC X(02).
           10 WS-PERSIST-COUNTRY                   PIC X(03).
           10 WS-PERSIST-ZIP                       PIC X(10).
           10 WS-PERSIST-PHONE1-AREA               PIC X(03).
           10 WS-PERSIST-PHONE1-PREFIX             PIC X(03).
           10 WS-PERSIST-PHONE1-LINE               PIC X(04).
           10 WS-PERSIST-PHONE2-AREA               PIC X(03).
           10 WS-PERSIST-PHONE2-PREFIX             PIC X(03).
           10 WS-PERSIST-PHONE2-LINE               PIC X(04).
           10 WS-PERSIST-SSN                       PIC X(09).
           10 WS-PERSIST-GOVT-ID                   PIC X(20).
           10 WS-PERSIST-DOB                       PIC X(8).
           10 WS-PERSIST-EFT-ID                    PIC X(10).
           10 WS-PERSIST-PRI-HOLDER                PIC X(01).
           10 WS-PERSIST-FICO                      PIC X(03).

      ******************************************************************
      * Customer creation work fields
      ******************************************************************
         05  WS-CUST-CREATE-FIELDS.
           10 WS-NEW-CUST-ID                       PIC 9(9).

      ******************************************************************
      * Message handling
      ******************************************************************
         05  WS-INFO-MSG                           PIC X(40).
           88  WS-NO-INFO-MESSAGE                  VALUES
                                                   SPACES LOW-VALUES.
           88  WS-PROMPT-FOR-INPUT                 VALUE
               'Enter customer creation details         '.
           88  WS-INFORM-SUCCESS                   VALUE
               'Customer created successfully           '.
         05  WS-RETURN-MSG                         PIC X(75).
           88  WS-RETURN-MSG-OFF                   VALUE SPACES.

      ******************************************************************
      * Constants and literals
      ******************************************************************
       01 WS-LITERALS.
          05 LIT-THISPGM                           PIC X(8)
                                                   VALUE 'COCUSADU'.
          05 LIT-THISTRANID                        PIC X(4)
                                                   VALUE 'AASD'.
          05 LIT-THISMAPSET                        PIC X(8)
                                                   VALUE 'COCUSAD '.
          05 LIT-THISMAP                           PIC X(7)
                                                   VALUE 'COCUSAD'.
          05 LIT-RPC-PROGRAM                       PIC X(8)
                                                   VALUE 'COCUSADA'.
          05 LIT-ACCT-CREATE-PGM                   PIC X(8)
                                                   VALUE 'COACTADU'.
          05 LIT-ACCT-CREATE-TRAN                  PIC X(4)
                                                   VALUE 'AASC'.
          05 LIT-MENUPGM                           PIC X(8)
                                                   VALUE 'COMEN01U'.
          05 LIT-MENUTRANID                        PIC X(4)
                                                   VALUE 'AAUM'.

      ******************************************************************
      * Copy common work areas
      ******************************************************************
       COPY CVCRD01Y.

      ******************************************************************
      * Application Commarea Copybook
      ******************************************************************
       COPY COCOM01Y.

       01 WS-THIS-PROGCOMMAREA.
          05 CA-CALL-CONTEXT.
             10 CA-FROM-PROGRAM                    PIC X(08).
             10 CA-FROM-TRANID                     PIC X(04).

       01  WS-COMMAREA                             PIC X(2000).

      ******************************************************************
      * IBM SUPPLIED COPYBOOKS
      ******************************************************************
       COPY DFHBMSCA.
       COPY DFHAID.

      ******************************************************************
      * COMMON COPYBOOKS
      ******************************************************************
       COPY COTTL01Y.
       COPY COCUSAD.
       COPY CSDAT01Y.
       COPY CSMSG01Y.
       COPY CSMSG02Y.
       COPY CSUSR01Y.
       COPY CSLKPCDY.

      ******************************************************************
      * RPC Communication Area for Customer Creation - FIXED PHONE FORMAT
      ******************************************************************
       01 WS-CUST-RPC-COMMAREA.
          05 LK-CUST-INPUT-PARMS.
             10 LK-CUST-IN-TARGET-ID       PIC 9(09).
             10 LK-CUST-IN-FNAME           PIC X(25).
             10 LK-CUST-IN-MNAME           PIC X(25).
             10 LK-CUST-IN-LNAME           PIC X(25).
             10 LK-CUST-IN-ADDR1           PIC X(50).
             10 LK-CUST-IN-ADDR2           PIC X(50).
             10 LK-CUST-IN-CITY            PIC X(50).
             10 LK-CUST-IN-STATE           PIC X(02).
             10 LK-CUST-IN-COUNTRY         PIC X(03).
             10 LK-CUST-IN-ZIP             PIC X(10).
             10 LK-CUST-IN-PHONE1.
                15 LK-CUST-IN-PHONE1-AREA  PIC X(03).
                15 LK-CUST-IN-PHONE1-PREFIX PIC X(03).
                15 LK-CUST-IN-PHONE1-LINE  PIC X(04).
             10 LK-CUST-IN-PHONE2.
                15 LK-CUST-IN-PHONE2-AREA  PIC X(03).
                15 LK-CUST-IN-PHONE2-PREFIX PIC X(03).
                15 LK-CUST-IN-PHONE2-LINE  PIC X(04).
             10 LK-CUST-IN-SSN             PIC 9(09).
             10 LK-CUST-IN-GOVT-ID         PIC X(20).
             10 LK-CUST-IN-DOB             PIC X(8).
             10 LK-CUST-IN-EFT-ID          PIC X(10).
             10 LK-CUST-IN-PRI-HOLDER      PIC X(01).
             10 LK-CUST-IN-FICO            PIC 9(03).
          05 LK-CUST-OUTPUT-STATUS.
             10 LK-CUST-OUT-RETURN-CODE    PIC 9(02).
                88 CUST-RC-SUCCESS         VALUE 00.
                88 CUST-RC-NOT-FOUND       VALUE 01.
                88 CUST-RC-INPUT-ERROR     VALUE 03.
                88 CUST-RC-DATABASE-ERROR  VALUE 99.
                88 CUST-RC-TARGET-ID-EXISTS VALUE 06.
             10 LK-CUST-OUT-MESSAGE        PIC X(80).
             10 LK-CUST-OUT-ERROR-FIELD    PIC X(25).
          05 LK-CUST-OUTPUT-DATA.
             10 LK-CUST-OUT-NEW-CUST-ID    PIC 9(09).

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

      ******************************************************************
      * Store our context
      ******************************************************************
           MOVE LIT-THISTRANID       TO WS-TRANID

      ******************************************************************
      * Clear error message
      ******************************************************************
           SET WS-RETURN-MSG-OFF  TO TRUE
           SET ERR-FLG-OFF TO TRUE
           MOVE SPACES TO WS-MESSAGE

      ******************************************************************
      * Store passed data if any - MODIFIED FOR TARGET CUST-ID
      ******************************************************************
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


      *       SAVE TARGET CUST-ID IF COMING FROM ACCOUNT CREATION
              IF CDEMO-FROM-PROGRAM = 'COACTADU'
              AND CDEMO-CUST-ID NOT = ZEROS
                 MOVE CDEMO-CUST-ID TO WS-TARGET-CUST-ID
              END-IF

           END-IF

      ******************************************************************
      * Remap PFkeys and store mapped PF Key
      ******************************************************************
           PERFORM YYYY-STORE-PFKEY
              THRU YYYY-STORE-PFKEY-EXIT

      ******************************************************************
      * Check the AID to see if its valid at this point
      * F3 - Exit/Return  Enter - Create Customer
      ******************************************************************
           SET PFK-INVALID TO TRUE
           IF CCARD-AID-ENTER OR
              CCARD-AID-PFK03
              SET PFK-VALID TO TRUE
           END-IF

           IF PFK-INVALID
              SET CCARD-AID-ENTER TO TRUE
           END-IF

      ******************************************************************
      * Decide what to do based on inputs received
      ******************************************************************
           EVALUATE TRUE
              WHEN CCARD-AID-PFK03

                  IF CDEMO-FROM-PROGRAM = 'COACTADU'
                  OR CDEMO-FROM-PROGRAM = LIT-ACCT-CREATE-PGM
                     MOVE LIT-ACCT-CREATE-TRAN TO CDEMO-TO-TRANID
                     MOVE LIT-ACCT-CREATE-PGM  TO CDEMO-TO-PROGRAM

      * RESTORE TARGET CUST-ID WHEN RETURNING
                     IF WS-TARGET-CUST-ID NOT = ZEROS
                        MOVE WS-TARGET-CUST-ID TO CDEMO-CUST-ID
                     END-IF
      * CLEAR INFO MESSAGE WHEN RETURNING TO ACTADU
                     SET WS-NO-INFO-MESSAGE TO TRUE
                     MOVE SPACES TO WS-INFO-MSG
                  ELSE
                     MOVE LIT-ACCT-CREATE-TRAN TO CDEMO-TO-TRANID
                     MOVE LIT-ACCT-CREATE-PGM  TO CDEMO-TO-PROGRAM
                     INITIALIZE CDEMO-CUSTOMER-INFO
                  END-IF

                  MOVE LIT-THISTRANID       TO CDEMO-FROM-TRANID
                  MOVE LIT-THISPGM          TO CDEMO-FROM-PROGRAM


                  EXEC CICS XCTL
                        PROGRAM (CDEMO-TO-PROGRAM)
                        COMMAREA(CARDDEMO-COMMAREA)
                  END-EXEC

              WHEN CDEMO-PGM-ENTER
      ******************************************************************
      *            COMING FROM ACCOUNT CREATION SCREEN
      *            INITIAL SCREEN DISPLAY
      ******************************************************************
                   PERFORM 1000-SEND-MAP THRU
                           1000-SEND-MAP-EXIT
                   GO TO COMMON-RETURN

              WHEN CDEMO-PGM-REENTER
                   PERFORM 2000-PROCESS-INPUTS
                      THRU 2000-PROCESS-INPUTS-EXIT
                   PERFORM 9000-CREATE-CUSTOMER
                      THRU 9000-CREATE-CUSTOMER-EXIT
                   IF CUST-RC-SUCCESS

      *               CLEAN COMMAREA BEFORE SETTING NEW CUST-ID
                      INITIALIZE CDEMO-CUSTOMER-INFO
                      MOVE WS-NEW-CUST-ID TO CDEMO-CUST-ID

      *               DON'T CHANGE FROM-PROGRAM - LEAVE AS 'COACTADU'
      *               SET PGM-ENTER TO TRIGGER INITIAL DISPLAY LOGIC
                      SET CDEMO-PGM-ENTER TO TRUE

                      MOVE LIT-ACCT-CREATE-TRAN TO CDEMO-TO-TRANID
                      MOVE LIT-ACCT-CREATE-PGM TO CDEMO-TO-PROGRAM


                      EXEC CICS XCTL
                            PROGRAM (CDEMO-TO-PROGRAM)
                            COMMAREA(CARDDEMO-COMMAREA)
                            LENGTH(LENGTH OF CARDDEMO-COMMAREA)
                      END-EXEC

                   ELSE
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

      ******************************************************************
      * Persist all input fields for screen continuity - FIXED PHONES
      ******************************************************************
       PERSIST-INPUT-FIELDS.
           MOVE CFNAMEI OF COCUSADI TO WS-PERSIST-FNAME
           MOVE CMNAMEI OF COCUSADI TO WS-PERSIST-MNAME
           MOVE CLNAMEI OF COCUSADI TO WS-PERSIST-LNAME
           MOVE CADDR1I OF COCUSADI TO WS-PERSIST-ADDR1
           MOVE CADDR2I OF COCUSADI TO WS-PERSIST-ADDR2
           MOVE CCITYI OF COCUSADI TO WS-PERSIST-CITY
           MOVE CSTATEI OF COCUSADI TO WS-PERSIST-STATE
           MOVE CCNTRYI OF COCUSADI TO WS-PERSIST-COUNTRY
           MOVE CZIPI OF COCUSADI TO WS-PERSIST-ZIP
           MOVE CPH1AI OF COCUSADI TO WS-PERSIST-PHONE1-AREA
           MOVE CPH1BI OF COCUSADI TO WS-PERSIST-PHONE1-PREFIX
           MOVE CPH1CI OF COCUSADI TO WS-PERSIST-PHONE1-LINE
           MOVE CPH2AI OF COCUSADI TO WS-PERSIST-PHONE2-AREA
           MOVE CPH2BI OF COCUSADI TO WS-PERSIST-PHONE2-PREFIX
           MOVE CPH2CI OF COCUSADI TO WS-PERSIST-PHONE2-LINE
           MOVE CSSNI OF COCUSADI TO WS-PERSIST-SSN
           MOVE CGOVTIDI OF COCUSADI TO WS-PERSIST-GOVT-ID
           MOVE CDOBI OF COCUSADI TO WS-PERSIST-DOB
           MOVE CEFTIDI OF COCUSADI TO WS-PERSIST-EFT-ID
           MOVE CPRIHLDI OF COCUSADI TO WS-PERSIST-PRI-HOLDER
           MOVE CFICOI OF COCUSADI TO WS-PERSIST-FICO
           .

      ******************************************************************
      * Send map section
      ******************************************************************
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
           EXIT.

       1100-SCREEN-INIT.
           MOVE LOW-VALUES             TO COCUSADO

           MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA

           MOVE CCDA-TITLE01           TO TITLE01O OF COCUSADO
           MOVE CCDA-TITLE02           TO TITLE02O OF COCUSADO
           MOVE LIT-THISTRANID         TO TRNNAMEO OF COCUSADO
           MOVE LIT-THISPGM            TO PGMNAMEO OF COCUSADO

           MOVE WS-CURDATE-MONTH       TO WS-CURDATE-MM
           MOVE WS-CURDATE-DAY         TO WS-CURDATE-DD
           MOVE WS-CURDATE-YEAR(3:2)   TO WS-CURDATE-YY

           MOVE WS-CURDATE-MM-DD-YY    TO CURDATEO OF COCUSADO

           MOVE WS-CURTIME-HOURS       TO WS-CURTIME-HH
           MOVE WS-CURTIME-MINUTE      TO WS-CURTIME-MM
           MOVE WS-CURTIME-SECOND      TO WS-CURTIME-SS

           MOVE WS-CURTIME-HH-MM-SS    TO CURTIMEO OF COCUSADO
           .
       1100-SCREEN-INIT-EXIT.
           EXIT.

       1200-SETUP-SCREEN-VARS.
           IF EIBCALEN = 0
              SET  WS-PROMPT-FOR-INPUT TO TRUE
           END-IF

           IF WS-NO-INFO-MESSAGE
             SET WS-PROMPT-FOR-INPUT TO TRUE
           END-IF

           MOVE WS-MESSAGE             TO ERRMSGO OF COCUSADO
           MOVE WS-INFO-MSG            TO INFOMSGO OF COCUSADO

      *    POPULATE PERSISTED FIELDS ON SCREEN - LIKE ACTU
           PERFORM POPULATE-PERSISTED-FIELDS
           .
       1200-SETUP-SCREEN-VARS-EXIT.
           EXIT.

      ******************************************************************
      * Populate persisted fields on screen - FIXED PHONE FORMAT
      ******************************************************************
       POPULATE-PERSISTED-FIELDS.
           MOVE WS-PERSIST-FNAME TO CFNAMEO OF COCUSADO
           MOVE WS-PERSIST-MNAME TO CMNAMEO OF COCUSADO
           MOVE WS-PERSIST-LNAME TO CLNAMEO OF COCUSADO
           MOVE WS-PERSIST-ADDR1 TO CADDR1O OF COCUSADO
           MOVE WS-PERSIST-ADDR2 TO CADDR2O OF COCUSADO
           MOVE WS-PERSIST-CITY TO CCITYO OF COCUSADO
           MOVE WS-PERSIST-STATE TO CSTATEO OF COCUSADO
           MOVE WS-PERSIST-COUNTRY TO CCNTRYO OF COCUSADO
           MOVE WS-PERSIST-ZIP TO CZIPO OF COCUSADO
           MOVE WS-PERSIST-PHONE1-AREA TO CPH1AO OF COCUSADO
           MOVE WS-PERSIST-PHONE1-PREFIX TO CPH1BO OF COCUSADO
           MOVE WS-PERSIST-PHONE1-LINE TO CPH1CO OF COCUSADO
           MOVE WS-PERSIST-PHONE2-AREA TO CPH2AO OF COCUSADO
           MOVE WS-PERSIST-PHONE2-PREFIX TO CPH2BO OF COCUSADO
           MOVE WS-PERSIST-PHONE2-LINE TO CPH2CO OF COCUSADO
           MOVE WS-PERSIST-SSN TO CSSNO OF COCUSADO
           MOVE WS-PERSIST-GOVT-ID TO CGOVTIDO OF COCUSADO
           MOVE WS-PERSIST-DOB TO CDOBO OF COCUSADO
           MOVE WS-PERSIST-EFT-ID TO CEFTIDO OF COCUSADO
           MOVE WS-PERSIST-PRI-HOLDER TO CPRIHLDO OF COCUSADO
           MOVE WS-PERSIST-FICO TO CFICOO OF COCUSADO
           .

       1300-SETUP-SCREEN-ATTRS.
      *    POSITION CURSOR BASED ON RPC ERROR MESSAGE
           PERFORM SET-CURSOR-FROM-ERROR-FIELD

      *    SETUP COLOR - ALL GREEN BY DEFAULT
           MOVE DFHGREEN               TO CFNAMEC OF COCUSADO
           MOVE DFHGREEN               TO CLNAMEC OF COCUSADO
           MOVE DFHGREEN               TO CSSNC OF COCUSADO
           MOVE DFHGREEN               TO CDOBC OF COCUSADO

      *    SET ERROR MESSAGE COLOR
           IF ERR-FLG-ON
               MOVE DFHRED             TO ERRMSGC OF COCUSADO
           ELSE
               MOVE DFHGREEN           TO ERRMSGC OF COCUSADO
           END-IF

           IF  WS-NO-INFO-MESSAGE
               MOVE DFHBMDAR           TO INFOMSGC OF COCUSADO
           ELSE
               MOVE DFHNEUTR           TO INFOMSGC OF COCUSADO
           END-IF
           .
       1300-SETUP-SCREEN-ATTRS-EXIT.
           EXIT.

      ******************************************************************
      * Set cursor based on RPC error field - ENHANCED FOR PHONE FIELDS
      ******************************************************************
       SET-CURSOR-FROM-ERROR-FIELD.
           EVALUATE LK-CUST-OUT-ERROR-FIELD
               WHEN 'FNAME'
                   MOVE -1 TO CFNAMEL OF COCUSADI
               WHEN 'LNAME'
                   MOVE -1 TO CLNAMEL OF COCUSADI
               WHEN 'SSN'
                   MOVE -1 TO CSSNL OF COCUSADI
               WHEN 'DOB'
                   MOVE -1 TO CDOBL OF COCUSADI
               WHEN 'ADDR1'
                   MOVE -1 TO CADDR1L OF COCUSADI
               WHEN 'CITY'
                   MOVE -1 TO CCITYL OF COCUSADI
               WHEN 'STATE'
                   MOVE -1 TO CSTATEL OF COCUSADI
               WHEN 'COUNTRY'
                   MOVE -1 TO CCNTRYL OF COCUSADI
               WHEN 'ZIP'
                   MOVE -1 TO CZIPL OF COCUSADI
               WHEN 'PHONE1'
                   MOVE -1 TO CPH1AL OF COCUSADI
               WHEN 'PHONE2'
                   MOVE -1 TO CPH2AL OF COCUSADI
               WHEN 'FICO'
                   MOVE -1 TO CFICOL OF COCUSADI
               WHEN OTHER
                   MOVE -1 TO CFNAMEL OF COCUSADI
           END-EVALUATE
           .

       1400-SEND-SCREEN.
           MOVE LIT-THISMAPSET         TO CCARD-NEXT-MAPSET
           MOVE LIT-THISMAP            TO CCARD-NEXT-MAP
           SET  CDEMO-PGM-REENTER TO TRUE

           EXEC CICS SEND MAP(CCARD-NEXT-MAP)
                          MAPSET(CCARD-NEXT-MAPSET)
                          FROM(COCUSADO)
                          CURSOR
                          ERASE
                          FREEKB
                          RESP(WS-RESP-CD)
           END-EXEC
           .
       1400-SEND-SCREEN-EXIT.
           EXIT.

      ******************************************************************
      * Process input section - SIMPLIFIED TO JUST RECEIVE
      ******************************************************************
       2000-PROCESS-INPUTS.
           PERFORM 2100-RECEIVE-MAP
              THRU 2100-RECEIVE-MAP-EXIT
           .
       2000-PROCESS-INPUTS-EXIT.
           EXIT.

       2100-RECEIVE-MAP.
           EXEC CICS RECEIVE MAP(LIT-THISMAP)
                     MAPSET(LIT-THISMAPSET)
                     INTO(COCUSADI)
                     RESP(WS-RESP-CD)
                     RESP2(WS-REAS-CD)
           END-EXEC

      *    PERSIST INPUT FIELDS AFTER RECEIVING - LIKE ACTU
           PERFORM PERSIST-INPUT-FIELDS
           .
       2100-RECEIVE-MAP-EXIT.
           EXIT.

      ******************************************************************
      * Create customer section - FIXED FOR THREE-PART PHONE FIELDS
      ******************************************************************
       9000-CREATE-CUSTOMER.
           SET  WS-NO-INFO-MESSAGE  TO TRUE

           PERFORM 9100-CREATE-CUST-VIA-RPC
              THRU 9100-CREATE-CUST-VIA-RPC-EXIT

           IF CUST-RC-SUCCESS
              MOVE WS-NEW-CUST-ID   TO CDEMO-CUST-ID
              SET ERR-FLG-OFF TO TRUE
           ELSE
              MOVE LK-CUST-OUT-MESSAGE TO WS-MESSAGE
              SET INPUT-ERROR TO TRUE
              SET ERR-FLG-ON TO TRUE
           END-IF
           .
       9000-CREATE-CUSTOMER-EXIT.
           EXIT.

       9100-CREATE-CUST-VIA-RPC.
           INITIALIZE WS-CUST-RPC-COMMAREA

      *    Pass target customer ID if specified
           MOVE WS-TARGET-CUST-ID TO LK-CUST-IN-TARGET-ID

           MOVE CFNAMEI OF COCUSADI    TO LK-CUST-IN-FNAME
           MOVE CMNAMEI OF COCUSADI    TO LK-CUST-IN-MNAME
           MOVE CLNAMEI OF COCUSADI    TO LK-CUST-IN-LNAME
           MOVE CADDR1I OF COCUSADI    TO LK-CUST-IN-ADDR1
           MOVE CADDR2I OF COCUSADI    TO LK-CUST-IN-ADDR2
           MOVE CCITYI OF COCUSADI     TO LK-CUST-IN-CITY
           MOVE CSTATEI OF COCUSADI    TO LK-CUST-IN-STATE
           MOVE CCNTRYI OF COCUSADI    TO LK-CUST-IN-COUNTRY
           MOVE CZIPI OF COCUSADI      TO LK-CUST-IN-ZIP
           MOVE CPH1AI OF COCUSADI     TO LK-CUST-IN-PHONE1-AREA
           MOVE CPH1BI OF COCUSADI     TO LK-CUST-IN-PHONE1-PREFIX
           MOVE CPH1CI OF COCUSADI     TO LK-CUST-IN-PHONE1-LINE
           MOVE CPH2AI OF COCUSADI     TO LK-CUST-IN-PHONE2-AREA
           MOVE CPH2BI OF COCUSADI     TO LK-CUST-IN-PHONE2-PREFIX
           MOVE CPH2CI OF COCUSADI     TO LK-CUST-IN-PHONE2-LINE
           MOVE CSSNI OF COCUSADI      TO LK-CUST-IN-SSN
           MOVE CGOVTIDI OF COCUSADI   TO LK-CUST-IN-GOVT-ID
           MOVE CDOBI OF COCUSADI      TO LK-CUST-IN-DOB
           MOVE CEFTIDI OF COCUSADI    TO LK-CUST-IN-EFT-ID
           MOVE CPRIHLDI OF COCUSADI   TO LK-CUST-IN-PRI-HOLDER
           MOVE CFICOI OF COCUSADI     TO LK-CUST-IN-FICO

           EXEC CICS LINK
                PROGRAM(LIT-RPC-PROGRAM)
                COMMAREA(WS-CUST-RPC-COMMAREA)
                LENGTH(LENGTH OF WS-CUST-RPC-COMMAREA)
                RESP(WS-RESP-CD)
                RESP2(WS-REAS-CD)
           END-EXEC.

           MOVE WS-RESP-CD    TO WS-RESP-DISP.
           MOVE WS-REAS-CD    TO WS-REAS-DISP.

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   IF CUST-RC-SUCCESS
                       MOVE LK-CUST-OUT-NEW-CUST-ID TO WS-NEW-CUST-ID
                   END-IF
               WHEN DFHRESP(PGMIDERR)
                   MOVE 'COCUSADA program not found' TO
                        LK-CUST-OUT-MESSAGE
                   MOVE 'FNAME' TO LK-CUST-OUT-ERROR-FIELD
               WHEN OTHER
                   STRING 'Error calling COCUSADA. RESP='
                          WS-RESP-DISP
                          ' RESP2='
                          WS-REAS-DISP
                     DELIMITED BY SIZE
                     INTO LK-CUST-OUT-MESSAGE
                   END-STRING
                   MOVE 'FNAME' TO LK-CUST-OUT-ERROR-FIELD
           END-EVALUATE
           .
       9100-CREATE-CUST-VIA-RPC-EXIT.
           EXIT.

      ******************************************************************
      * Utility sections
      ******************************************************************
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
           EXIT.

      ******************************************************************
      * Common code to store PFKey
      ******************************************************************
       COPY 'CSSTRPFY'.

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
           END-EXEC.

           GOBACK.