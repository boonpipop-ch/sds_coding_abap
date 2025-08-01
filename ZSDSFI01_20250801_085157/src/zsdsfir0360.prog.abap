*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0360
*  Creation Date      : 03.07.2024
*  Author             : Jutamas Y.(Eviden)
*  Add-on ID          : ZFIARE004
*  Description        : This is a program to upload text file AR
*                       Copy from T41( program : ZFIAR_INF_COLLECTOR )
*  Purpose            : To upload text file and call program ZSDSFIR0250
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  21.01.2025  F36K910898  Jutamas Y.  420000106 - check line item not
*                                      blank
*-----------------------------------------------------------------------
*  22.01.2025  F36K911501  Jutamas Y.  420000143 - Add Logic for
*                                      transport fee post only full
*                                      -(Hold)
*-----------------------------------------------------------------------
*  17.02.2025 F36K912646  Jutamas Y.  420000378 - Add logic for Service
*-----------------------------------------------------------------------
*  28.02.2025 F36K913267  Jutamas Y.  420000378 - validate amount only
*                                     case 02,02
*-----------------------------------------------------------------------
*  28.02.2025 F36K913267  Jutamas Y.  420000463 - Add Genc for Bank Fee
*-----------------------------------------------------------------------
*  28.03.2025 F36K914905  Jutamas Y.  420000572 Update activity bill
*                                     - sign of variable no need to check
*                                       activity and status
*  15.05.2025             Jutamas Y.  - if done 0202 then upload
*                                       Case 0404 and 0403 will change
*                                       type memo
*-----------------------------------------------------------------------
*  27.06.2025 F36K920171  Jutamas Y.  420000677 Shortdump conv. codepage
*-----------------------------------------------------------------------
*  02.07.2025 F36K920513  Jutamas Y.  420000633 - Partial except.0202
*                                     Set type to MEMO
*-----------------------------------------------------------------------
REPORT ZSDSFIR0360.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*
INCLUDE ZSDSFIR0250_TOP ##INCL_OK.
INCLUDE ZSDSCAI9990 ##INCL_OK.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:
  RLGRAP,
  ADR6.


*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: BEGIN OF TS_SAVE_RESULT,
         FILENAME  TYPE RLGRAP-FILENAME,
         SAVE_FLAG TYPE FLAG, "X = Save success, space = Error
         MSG       TYPE MSGTX,
       END OF TS_SAVE_RESULT.

TYPES: BEGIN OF TS_SUM_BILLPL ,
         BUKRS      TYPE ZSDSFIS131-BUKRS,
         BELNR      TYPE ZSDSFIS131-BELNR,
         GJAHR      TYPE ZSDSFIS131-GJAHR,
         BUZEI      TYPE ZSDSFIS131-BUZEI,
         SEQ        TYPE ZSDSFIS131-SEQ,
         BILLPL_NO  TYPE ZSDSFIS131-BILLPL_NO,
         BILLPL_AMT TYPE ZSDSFIS131-BILLPL_AMT,
       END   OF TS_SUM_BILLPL .

TYPES: BEGIN OF TS_FIDOC,
         BUKRS TYPE BKPF-BUKRS,
         BELNR TYPE BKPF-BELNR,
         GJAHR TYPE BKPF-GJAHR,
         XBLNR TYPE BKPF-XBLNR,
       END OF TS_FIDOC.

TYPES: BEGIN OF TS_PARTIAL_CHEQUE ,
         BUKRS     TYPE ZSDSFIS131-BUKRS,
         BELNR     TYPE ZSDSFIS131-BELNR,
         GJAHR     TYPE ZSDSFIS131-GJAHR,
         BUZEI     TYPE ZSDSFIS131-BUZEI,
         SEQ       TYPE ZSDSFIS131-SEQ,
         WORK_DATE TYPE ZSDSFIS131-WORK_DATE,
         CHEQUE_NO TYPE ZSDSFIS131-CHEQUE_NO,
       END   OF TS_PARTIAL_CHEQUE.

*-420000633 Beg of INS
TYPES: BEGIN OF TS_PARTIAL_MEMO ,
         BUKRS     TYPE ZSDSFIS131-BUKRS,
         BELNR     TYPE ZSDSFIS131-BELNR,
         GJAHR     TYPE ZSDSFIS131-GJAHR,
         BUZEI     TYPE ZSDSFIS131-BUZEI,
         SEQ       TYPE ZSDSFIS131-SEQ,
         WORK_DATE TYPE ZSDSFIS131-WORK_DATE,
         WRBTR     TYPE ZSDSFIS131-WRBTR,
       END   OF TS_PARTIAL_MEMO.
*-420000633 End of INS

**-420000143 Beg of INS
*TYPES: BEGIN OF TS_SUM_AMT ,
*         DATA_TYPE TYPE ZSDSFIS131-DATA_TYPE,
*         BUKRS     TYPE ZSDSFIS131-BUKRS,
*         BELNR     TYPE ZSDSFIS131-BELNR,
*         GJAHR     TYPE ZSDSFIS131-GJAHR,
*         WRBTR     TYPE ZSDSFIS131-WRBTR,
*         CNTNO     TYPE ZSDSFIS131-SEQ,
*       END   OF TS_SUM_AMT ,
*
*       TT_SUM_AMT TYPE STANDARD TABLE OF TS_SUM_AMT.
**-420000143 End of INS

TYPES: TS_COLLECTOR TYPE ZSDSFIS131.

TYPES: TS_RESULT TYPE  ZSDSFIS131,
       TT_RESULT TYPE  STANDARD TABLE OF TS_RESULT.

TYPES: TT_FILE_LIST      TYPE STANDARD TABLE OF EPS2FILI,
       TT_DATA_SPLIT     TYPE STANDARD TABLE OF STRING,
       TT_COLLECTOR      TYPE STANDARD TABLE OF TS_COLLECTOR,
       TT_SAVE_RESULT    TYPE STANDARD TABLE OF TS_SAVE_RESULT,
       TT_SUM_BILLPL     TYPE STANDARD TABLE OF TS_SUM_BILLPL,
       TT_PARTIAL_CHEQUE TYPE STANDARD TABLE OF TS_PARTIAL_CHEQUE,
*-420000633 Beg of INS
       TT_PARTIAL_MEMO   TYPE STANDARD TABLE OF TS_PARTIAL_MEMO,
*-420000633 End of INS
       TT_FIDOC          TYPE STANDARD TABLE OF TS_FIDOC.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
*CONSTANTS:
*  GC_TCODE TYPE  SY-TCODE  VALUE 'ZSDSFI024'.
##NEEDED
CONSTANTS: GC_PATH_SDIRN   TYPE CHAR40   VALUE 'SOURCE_DIR_NORMAL',
           GC_PATH_SDIRR   TYPE CHAR40   VALUE 'SOURCE_DIR_REVISE',
           GC_PATH_BDIR    TYPE CHAR40   VALUE 'BACKUP_DIR',
           GC_PATH_BACKUP  TYPE CHAR40   VALUE 'TARGET_DIR',
           GC_PATH_EMAIL_E TYPE CHAR40   VALUE 'EMAIL_ERROR',
           GC_PATH_EMAIL_S TYPE CHAR40   VALUE 'EMAIL_SUCCESS',
           GC_X            TYPE FLAG     VALUE 'X',
           GC_TRANS        TYPE SY-TCODE VALUE 'ZSDSFI033'.

CONSTANTS: GC_VENDOR     TYPE CHAR2 VALUE '13',
           GC_BILLPLACE  TYPE CHAR2 VALUE '01',
           GC_COLLECTION TYPE CHAR2 VALUE '02',
           GC_OTHER      TYPE CHAR2 VALUE '04',
           GC_FILING     TYPE CHAR2 VALUE '06',
           GC_RECEIVED   TYPE CHAR2 VALUE '02'. "Status Received

CONSTANTS: GC_FOLLOW   TYPE CHAR2 VALUE '03',
           GC_FINISHED TYPE CHAR2 VALUE '04'.


CONSTANTS:
  GC_INBOUND    TYPE  TEXT20 VALUE '10_INBOUND',
  GC_OUTBOUND   TYPE  TEXT20 VALUE '20_OUTBOUND',
  GC_I_NORMAL   TYPE  TEXT20 VALUE 'Inbound_Normal',
  GC_I_REVISE   TYPE  TEXT20 VALUE 'Inbound_Revise',
  GC_O_ARCHIVED TYPE  TEXT20 VALUE 'Archived' ##NO_TEXT,
  GC_O_ERROR    TYPE  TEXT20 VALUE 'Error' ##NO_TEXT,

  GC_INTFNO_I   TYPE  ZSDSCAC004-INTFNO VALUE 'FIARE004' , "Inbound
  GC_INTFNO_A   TYPE  ZSDSCAC004-INTFNO VALUE 'FIARE005' , "Archived
  GC_INTFNO_E   TYPE  ZSDSCAC004-INTFNO VALUE 'FIARE006' . "Error
*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*
##NEEDED
DATA: GT_FILE_LIST    TYPE TT_FILE_LIST,
      GT_LOG          TYPE TT_OUTPUT,
      GT_ZSDSFIT046   TYPE TABLE OF ZSDSFIT046,
      GT_ZSDSFIT037   TYPE TABLE OF ZSDSFIT037,
      GT_SAVE_RESULT  TYPE TT_SAVE_RESULT,
      GT_LOG_DATA     TYPE TT_RESULT,
      GT_RESULT       TYPE TT_RESULT,
      GT_SUM_BILLPL   TYPE TT_SUM_BILLPL,
      GT_VALIDATE_ACT TYPE TT_ACTION_STATUS,
      GT_FIDOC        TYPE TT_FIDOC.
*      GT_SUM_AMT      TYPE TT_SUM_AMT.

##NEEDED
DATA: GF_PATH_NORMAL     TYPE RLGRAP-FILENAME,
      GF_PATH_REVISE     TYPE RLGRAP-FILENAME,
      GF_DATE            TYPE CHAR10,
      GF_TIME            TYPE CHAR8,
      GF_NAME_SIGN_EMAIL TYPE CHAR40,
      GF_HEADER          TYPE STRING.


*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*
##NEEDED
DATA :
  GRT_STATUS      TYPE RANGE OF ZSDSFIT029-STATUS,
  GRT_ACTION_TYPE TYPE RANGE OF ZSDSFIT029-ACTION_TYPE   ##NEEDED,
  GRT_BANK        TYPE RANGE OF ZSDSFIT029-PYMT_METHOD   ##NEEDED,
  GRT_PDC         TYPE RANGE OF ZSDSFIT029-PYMT_METHOD   ##NEEDED.
*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* GENC VARIABLES
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSFIS131'.

CONSTANTS:
  GC_HEADER_HEIGHT_1 TYPE  I                VALUE 0,
  GC_ALV_HEIGHT_1    TYPE  I                VALUE 100.
*----------------------------------------------------------------------*
* MACROS
*----------------------------------------------------------------------*
DEFINE MC_SHOW_PROGRESS.
  IF sy-batch IS INITIAL.
*   Show progress online
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = &1 ##NUMBER_OK
        text       = &2.
  ELSE.
*   Show message step in background
    MESSAGE i000(38) WITH &2 space space space.
  ENDIF.
END-OF-DEFINITION.
*&---------------------------------------------------------------------*
*  SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B01 WITH FRAME TITLE TEXT-T01.
  PARAMETERS: P_BUKRS TYPE BUKRS DEFAULT '1000'.

  SELECTION-SCREEN BEGIN OF BLOCK B02 WITH FRAME TITLE TEXT-T02.
    PARAMETERS: P_NORM RADIOBUTTON GROUP RD1 DEFAULT 'X' USER-COMMAND COMM1,
                P_REVI RADIOBUTTON GROUP RD1 ##NEEDED.
  SELECTION-SCREEN END OF BLOCK B02.

  SELECTION-SCREEN BEGIN OF BLOCK B03 WITH FRAME TITLE TEXT-T03.
    PARAMETERS: P_SDIR TYPE RLGRAP-FILENAME.
    SELECT-OPTIONS: S_SFNAME FOR RLGRAP-FILENAME.
  SELECTION-SCREEN END OF BLOCK B03.

  SELECTION-SCREEN BEGIN OF BLOCK B04 WITH FRAME TITLE TEXT-T04.
    PARAMETERS: P_TDIR TYPE RLGRAP-FILENAME.
  SELECTION-SCREEN END OF BLOCK B04.

  SELECTION-SCREEN BEGIN OF BLOCK B05 WITH FRAME TITLE TEXT-T05.
    PARAMETERS: P_BDIR TYPE RLGRAP-FILENAME.
  SELECTION-SCREEN END OF BLOCK B05.

  SELECTION-SCREEN BEGIN OF BLOCK B06 WITH FRAME TITLE TEXT-T06.
    SELECT-OPTIONS: S_EMAILS FOR ADR6-SMTP_ADDR NO INTERVALS.
    SELECT-OPTIONS: S_EMAILE FOR ADR6-SMTP_ADDR NO INTERVALS .
  SELECTION-SCREEN END OF BLOCK B06.

  SELECTION-SCREEN BEGIN OF BLOCK B07 WITH FRAME TITLE TEXT-T07.
    PARAMETERS: P_TEST AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN END OF BLOCK B07.

SELECTION-SCREEN END OF BLOCK B01.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TRANS.
  PERFORM F_GET_GENC.
  PERFORM F_SET_PATHFILE USING GC_INTFNO_I
                               GC_INBOUND
                               GC_I_NORMAL
                      CHANGING GF_PATH_NORMAL.

  PERFORM F_SET_PATHFILE USING GC_INTFNO_I
                               GC_INBOUND
                               GC_I_REVISE
                      CHANGING GF_PATH_REVISE.

  PERFORM F_SET_PATHFILE USING GC_INTFNO_A
                               GC_OUTBOUND
                               GC_O_ARCHIVED
                      CHANGING P_TDIR.


  PERFORM F_SET_PATHFILE USING GC_INTFNO_E
                               GC_OUTBOUND
                               GC_O_ERROR
                      CHANGING P_BDIR.

  CONCATENATE: SY-DATUM(4) SY-DATUM+4(2) SY-DATUM+6(2) INTO GF_DATE,
               SY-UZEIT(2) SY-UZEIT+2(2) SY-UZEIT+4(2) INTO GF_TIME.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  IF P_NORM IS NOT INITIAL.
    P_SDIR = GF_PATH_NORMAL.
  ELSE.
    P_SDIR = GF_PATH_REVISE.
  ENDIF.
  LOOP AT SCREEN.
    CASE SCREEN-NAME .
      WHEN  'P_BUKRS'       OR
            'S_EMAILS-LOW'  OR
            'S_EMAILS-HIGH' OR
            'S_EMAILE-LOW'  OR
            'S_EMAILE-HIGH' .
        SCREEN-INPUT = 0.
      WHEN 'RB_CRE' OR
           'RB_CHG' OR
           'RB_DIS' .
        SCREEN-INPUT = 0.
        SCREEN-INVISIBLE = 1 .

    ENDCASE.
    MODIFY SCREEN.

  ENDLOOP.

AT SELECTION-SCREEN.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_PROCESS_DATA .

*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
*   Display Processing Result
  PERFORM F_DISPLAY_RESULT USING GT_RESULT .

*----------------------------------------------------------------------*
* SUBROUTINE INCLUDES
*----------------------------------------------------------------------*
*  INCLUDE ZSDSFIR0250_ALV ##INCL_OK.
*  INCLUDE ZSDSFIR0250_F01 ##INCL_OK.

*&---------------------------------------------------------------------*
*& Form f_get_file_from_directory
*&---------------------------------------------------------------------*
FORM F_GET_FILE_FROM_DIRECTORY  USING  UF_SDIRS     TYPE RLGRAP-FILENAME
                              CHANGING CT_FILE_LIST TYPE TT_FILE_LIST.

  DATA: LF_DIR_NAME TYPE EPS2FILNAM.

*-Beg of INS
  DATA: LT_FILE_LIST TYPE TT_FILE_LIST.
*-End of INS
  LF_DIR_NAME = UF_SDIRS.
  CLEAR: CT_FILE_LIST[].
  CALL FUNCTION 'EPS2_GET_DIRECTORY_LISTING'
    EXPORTING
      IV_DIR_NAME            = LF_DIR_NAME
    TABLES
      DIR_LIST               = CT_FILE_LIST
    EXCEPTIONS
      INVALID_EPS_SUBDIR     = 1
      SAPGPARAM_FAILED       = 2
      BUILD_DIRECTORY_FAILED = 3
      NO_AUTHORIZATION       = 4
      READ_DIRECTORY_FAILED  = 5
      TOO_MANY_READ_ERRORS   = 6
      EMPTY_DIRECTORY_LIST   = 7
      OTHERS                 = 8.

  IF SY-SUBRC EQ 0.
    IF S_SFNAME IS NOT INITIAL.
      DELETE CT_FILE_LIST WHERE NAME NOT IN S_SFNAME.
    ELSE.
      DELETE CT_FILE_LIST WHERE NAME CP '*.xls'.
      DELETE CT_FILE_LIST WHERE NAME CP '*.xlsx'.
      DELETE CT_FILE_LIST WHERE NAME CP '*.XLS'.
      DELETE CT_FILE_LIST WHERE NAME CP '*.XLSX'.
      DELETE CT_FILE_LIST WHERE NAME CP '*.doc'.
      DELETE CT_FILE_LIST WHERE NAME CP '*.docx'.
      DELETE CT_FILE_LIST WHERE NAME CP '*.DOC'.
      DELETE CT_FILE_LIST WHERE NAME CP '*.DOCX'.
    ENDIF.
  ENDIF.

*-Beg of INS-Short dump file
  LT_FILE_LIST[] = CT_FILE_LIST[] .
  CLEAR CT_FILE_LIST .
  LOOP AT LT_FILE_LIST ASSIGNING FIELD-SYMBOL(<F_FILE_LIST>) WHERE NAME CP '*.TXT'
                                                                OR NAME CP '*.txt'
                                                                OR NAME CP '*.csv'
                                                                OR NAME CP '*.CSV'.
    APPEND <F_FILE_LIST> TO CT_FILE_LIST .

  ENDLOOP.
*-End of INS

  SORT CT_FILE_LIST BY MTIM .

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_upload_file
*&---------------------------------------------------------------------*
*& Upload file to Archive
*&---------------------------------------------------------------------*
FORM F_UPLOAD_FILE .
  DATA: LS_FILE        TYPE EPS2FILI,
        LF_FILENAME    TYPE EPS2FILNAM,
        LF_DATA        TYPE STRING,
        LT_DATA_SPLIT  TYPE TT_DATA_SPLIT,
*        LS_DATA_SPLIT  LIKE LINE OF LT_DATA_SPLIT,
        LT_COLLECTOR   TYPE TABLE OF TS_COLLECTOR,
        LS_COLLECTOR   TYPE TS_COLLECTOR,
        LF_LINE        TYPE I,
        LS_SAVE_RESULT TYPE TS_SAVE_RESULT.
*        LF_ERROR       TYPE FLAG.

  DATA: LT_ARCHIVED TYPE TT_COLLECTOR,
        LT_SUCCESS  TYPE TT_COLLECTOR,
        LT_ERROR    TYPE TT_COLLECTOR.

  LOOP AT GT_FILE_LIST INTO LS_FILE.
    CLEAR: LT_COLLECTOR, LT_ARCHIVED,
           LT_SUCCESS, LT_ERROR ,
           LF_LINE .

    CONCATENATE P_SDIR LS_FILE-NAME INTO LF_FILENAME.
    TRY.
*        OPEN DATASET LF_FILENAME FOR INPUT IN TEXT MODE ENCODING UTF-8.
        OPEN DATASET LF_FILENAME FOR INPUT IN TEXT MODE ENCODING DEFAULT.
        DO.
          READ DATASET LF_FILENAME INTO LF_DATA.
          IF SY-SUBRC NE 0.
            EXIT.
          ENDIF.
          LF_LINE = LF_LINE + 1 .

          "Keep Header file
          IF LF_LINE = 1 .
            GF_HEADER = LF_DATA .
          ENDIF.
          CHECK LF_LINE GT 1.
          SPLIT LF_DATA AT '","' INTO TABLE LT_DATA_SPLIT.
          LOOP AT LT_DATA_SPLIT ASSIGNING FIELD-SYMBOL(<LF_DATA_SPLIT>).
            CASE SY-TABIX.
              WHEN 1.
                REPLACE ALL OCCURRENCES OF '"' IN <LF_DATA_SPLIT> WITH ''.
                LS_COLLECTOR-BUKRS = <LF_DATA_SPLIT>.
              WHEN 2 ##NUMBER_OK.
                LS_COLLECTOR-GJAHR = <LF_DATA_SPLIT>.
              WHEN 3.
                LS_COLLECTOR-PERNR = <LF_DATA_SPLIT>.
              WHEN 4.
                LS_COLLECTOR-PERNM = <LF_DATA_SPLIT>.
              WHEN 5.
                LS_COLLECTOR-WORK_DATE = <LF_DATA_SPLIT>.
              WHEN 6.
                LS_COLLECTOR-ACTION_TYPE = <LF_DATA_SPLIT>.
              WHEN 7.
                LS_COLLECTOR-STATUS = <LF_DATA_SPLIT>.
              WHEN 8.
                LS_COLLECTOR-KUNNR = <LF_DATA_SPLIT>.
                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    INPUT  = LS_COLLECTOR-KUNNR
                  IMPORTING
                    OUTPUT = LS_COLLECTOR-KUNNR.
              WHEN 9.
                LS_COLLECTOR-CNAMT = <LF_DATA_SPLIT>.
              WHEN 10 ##NUMBER_OK.
                LS_COLLECTOR-CNAME = <LF_DATA_SPLIT>.
              WHEN 11 ##NUMBER_OK.
                LS_COLLECTOR-BELNR = <LF_DATA_SPLIT>.
              WHEN 12 ##NUMBER_OK.
                LS_COLLECTOR-VBELN = <LF_DATA_SPLIT>.
              WHEN 13 ##NUMBER_OK.
                LS_COLLECTOR-XBLNR = <LF_DATA_SPLIT>.
              WHEN 14 ##NUMBER_OK.
                LS_COLLECTOR-BLDAT = <LF_DATA_SPLIT>.
              WHEN 15 ##NUMBER_OK.
                LS_COLLECTOR-FAEDT = <LF_DATA_SPLIT>.
              WHEN 16 ##NUMBER_OK.
                REPLACE ALL OCCURRENCES OF ',' IN <LF_DATA_SPLIT> WITH ''.
                LS_COLLECTOR-WRBTR = <LF_DATA_SPLIT>.
              WHEN 17 ##NUMBER_OK.
                LS_COLLECTOR-WAERS = <LF_DATA_SPLIT>.
              WHEN 18 ##NUMBER_OK.
                LS_COLLECTOR-PAYMENT_DATE = <LF_DATA_SPLIT>.
              WHEN 19 ##NUMBER_OK.
                LS_COLLECTOR-FOLLOW_DATE = <LF_DATA_SPLIT>.
              WHEN 20 ##NUMBER_OK.
                LS_COLLECTOR-RECEIVED_DATE = <LF_DATA_SPLIT>.
              WHEN 21 ##NUMBER_OK.
                REPLACE ALL OCCURRENCES OF ',' IN <LF_DATA_SPLIT> WITH ''.
                LS_COLLECTOR-RECEIVED_AMT = <LF_DATA_SPLIT>.
              WHEN 22 ##NUMBER_OK.
                REPLACE ALL OCCURRENCES OF ',' IN <LF_DATA_SPLIT> WITH ''.
                LS_COLLECTOR-DEDUCT_AMT = <LF_DATA_SPLIT>.
              WHEN 23 ##NUMBER_OK.
                LS_COLLECTOR-PYMT_METHOD = <LF_DATA_SPLIT>.
              WHEN 24 ##NUMBER_OK.
*                LS_COLLECTOR-CHEQUE_DATE = <LF_DATA_SPLIT>.
                LS_COLLECTOR-BANK_DATE = <LF_DATA_SPLIT>.
              WHEN 25 ##NUMBER_OK.
                LS_COLLECTOR-CHEQUE_NO = <LF_DATA_SPLIT>.
              WHEN 26 ##NUMBER_OK.
                LS_COLLECTOR-BANKL = <LF_DATA_SPLIT>.
              WHEN 27 ##NUMBER_OK.
                LS_COLLECTOR-HBKID = <LF_DATA_SPLIT>.
              WHEN 28 ##NUMBER_OK.
                LS_COLLECTOR-HKTID = <LF_DATA_SPLIT>.
              WHEN 29 ##NUMBER_OK.
                LS_COLLECTOR-BANKK = <LF_DATA_SPLIT>.
              WHEN 30 ##NUMBER_OK.
                LS_COLLECTOR-BANKN = <LF_DATA_SPLIT>.
              WHEN 31 ##NUMBER_OK.
                LS_COLLECTOR-RECEIPT_NO = <LF_DATA_SPLIT>.
              WHEN 32 ##NUMBER_OK.
                LS_COLLECTOR-REMARK = <LF_DATA_SPLIT>.
              WHEN 33 ##NUMBER_OK.
                REPLACE ALL OCCURRENCES OF ',' IN <LF_DATA_SPLIT> WITH ''.
                LS_COLLECTOR-EXPS_AMT = ABS( <LF_DATA_SPLIT> ).
              WHEN 34 ##NUMBER_OK.
                REPLACE ALL OCCURRENCES OF ',' IN <LF_DATA_SPLIT> WITH ''.
                LS_COLLECTOR-INCOME_AMT = ABS( <LF_DATA_SPLIT> ).
              WHEN 35 ##NUMBER_OK.
                REPLACE ALL OCCURRENCES OF ',' IN <LF_DATA_SPLIT> WITH ''.
                LS_COLLECTOR-WHT_AMT = ABS( <LF_DATA_SPLIT> ).
              WHEN 36 ##NUMBER_OK.
                REPLACE ALL OCCURRENCES OF ',' IN <LF_DATA_SPLIT> WITH ''.
                LS_COLLECTOR-FEE = ABS( <LF_DATA_SPLIT> ).
              WHEN 37 ##NUMBER_OK.
                REPLACE ALL OCCURRENCES OF ',' IN <LF_DATA_SPLIT> WITH ''.
                LS_COLLECTOR-RETENTION = ABS( <LF_DATA_SPLIT> ).
              WHEN 38 ##NUMBER_OK.
                REPLACE ALL OCCURRENCES OF ',' IN <LF_DATA_SPLIT> WITH ''.
                LS_COLLECTOR-CASH_CON = ABS( <LF_DATA_SPLIT> ).
              WHEN 39 ##NUMBER_OK.
                REPLACE ALL OCCURRENCES OF ',' IN <LF_DATA_SPLIT> WITH ''.
                LS_COLLECTOR-BAL_AMT = ABS( <LF_DATA_SPLIT> ).
              WHEN 40 ##NUMBER_OK.
                LS_COLLECTOR-PAYNO = <LF_DATA_SPLIT>.
              WHEN 41 ##NUMBER_OK.
                LS_COLLECTOR-BRNCH = <LF_DATA_SPLIT>.
              WHEN 42 ##NUMBER_OK.
                LS_COLLECTOR-VTWEG = <LF_DATA_SPLIT>.
              WHEN 43 ##NUMBER_OK.
                LS_COLLECTOR-VKBUR = <LF_DATA_SPLIT>.
              WHEN 44 ##NUMBER_OK.
                LS_COLLECTOR-VKGRP = <LF_DATA_SPLIT>.
              WHEN 45 ##NUMBER_OK.
                LS_COLLECTOR-PSPID = <LF_DATA_SPLIT>.
              WHEN 46 ##NUMBER_OK.
                LS_COLLECTOR-BSTKD = <LF_DATA_SPLIT>.
              WHEN 47 ##NUMBER_OK.
                LS_COLLECTOR-ZTERM =  <LF_DATA_SPLIT>.
              WHEN 48 ##NUMBER_OK.
                LS_COLLECTOR-PRCTR = <LF_DATA_SPLIT>.
*-420000378 Beg of INS
                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    INPUT  = LS_COLLECTOR-PRCTR
                  IMPORTING
                    OUTPUT = LS_COLLECTOR-PRCTR.
*-420000378 End of INS
              WHEN 49 ##NUMBER_OK.
                LS_COLLECTOR-KTEXT = <LF_DATA_SPLIT>.
              WHEN 50 ##NUMBER_OK.
                LS_COLLECTOR-TRANF_NO = <LF_DATA_SPLIT>.
              WHEN 51 ##NUMBER_OK.
                LS_COLLECTOR-BILLPL_NO = <LF_DATA_SPLIT>.
              WHEN 52 ##NUMBER_OK.
*                LS_COLLECTOR-BANKK2 = <LF_DATA_SPLIT>.
              WHEN 53 ##NUMBER_OK.
                LS_COLLECTOR-BANK_ITEM = <LF_DATA_SPLIT>.
              WHEN 54 ##NUMBER_OK.
                REPLACE ALL OCCURRENCES OF ',' IN <LF_DATA_SPLIT> WITH ''.
                LS_COLLECTOR-BILLPL_AMT = <LF_DATA_SPLIT>   .
              WHEN 55 ##NUMBER_OK.
                LS_COLLECTOR-DOCUMENT_STATUS = <LF_DATA_SPLIT>   .
              WHEN 56 ##NUMBER_OK.
                LS_COLLECTOR-INV_STATUS = <LF_DATA_SPLIT>.
              WHEN 57 ##NUMBER_OK.
                LS_COLLECTOR-REMARK_VENDOR = <LF_DATA_SPLIT>.
              WHEN 58 ##NUMBER_OK.
                LS_COLLECTOR-REASON_VENDOR = <LF_DATA_SPLIT>.
              WHEN 59 ##NUMBER_OK.
*                LS_COLLECTOR-BILLING_CYCLE = <LF_DATA_SPLIT>.
              WHEN 60 ##NUMBER_OK.
*                LS_COLLECTOR-COLLECTION_CYCLE = <LF_DATA_SPLIT>.
              WHEN 61 ##NUMBER_OK.
*                "Delete carriage return
                REPLACE CL_ABAP_CHAR_UTILITIES=>CR_LF WITH '' INTO <LF_DATA_SPLIT>.
                REPLACE ALL OCCURRENCES OF '"' IN <LF_DATA_SPLIT> WITH ''.
                REPLACE ALL OCCURRENCES OF '#' IN <LF_DATA_SPLIT> WITH ''.
                LS_COLLECTOR-AWB_NO = <LF_DATA_SPLIT>.

            ENDCASE.
          ENDLOOP.
          "Add filename and file date time
          LS_COLLECTOR-FILENAME    = LS_FILE-NAME .
          LS_COLLECTOR-UPLOAD_DATE = LS_FILE-MTIM+0(10) .
          LS_COLLECTOR-UPLOAD_TIME = LS_FILE-MTIM+11(10) .
          APPEND LS_COLLECTOR TO LT_COLLECTOR.
        ENDDO.
        CLOSE DATASET LF_FILENAME.
      CATCH CX_SY_FILE_OPEN_MODE CX_SY_OPEN_SQL_DB
            CX_SY_CONVERSION_NO_NUMBER .
        LS_SAVE_RESULT-FILENAME = LF_FILENAME.
        LS_SAVE_RESULT-SAVE_FLAG = 'E'.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO LS_SAVE_RESULT-MSG.
        APPEND LS_SAVE_RESULT TO GT_SAVE_RESULT.
        CLEAR LS_SAVE_RESULT.
        CLOSE DATASET LF_FILENAME.
        PERFORM F_MOVEFILE_TO_BACKUP USING LF_FILENAME LS_FILE-NAME.
*-420000677 Beg of INS
      CATCH  CX_SY_CONVERSION_CODEPAGE .
        LS_SAVE_RESULT-FILENAME = LF_FILENAME.
        LS_SAVE_RESULT-SAVE_FLAG = 'E'.
        "Text-E21 : Character set conversion is not possible. FI Doc:
        CONCATENATE TEXT-E21 LS_COLLECTOR-BELNR
               INTO LS_SAVE_RESULT-MSG SEPARATED BY SPACE.

        APPEND LS_SAVE_RESULT TO GT_SAVE_RESULT.
        CLEAR LS_COLLECTOR .
*-420000677 End of INS
    ENDTRY.

    PERFORM F_GET_BP_LOG_OPEN USING LT_COLLECTOR .
    PERFORM F_PREPARE_CHANGE_OUTPUT USING LT_COLLECTOR
                                          LF_FILENAME
                                 CHANGING LT_ARCHIVED
                                          LT_SUCCESS
                                          LT_ERROR.
    IF P_TEST IS INITIAL  .

      "Write all data to Outbound Archive folder
      PERFORM F_WRITE_FILE USING LT_ARCHIVED
                                 P_TDIR
                                 LS_FILE-NAME .

      IF LT_SUCCESS[] IS NOT INITIAL .
        PERFORM F_LOCK_TRAN IN PROGRAM ZSDSFIR0250
          USING  '' GT_OUTPUT.

        PERFORM F_UPDATE_COLL_LOG IN PROGRAM ZSDSFIR0250
          USING GT_OUTPUT
                GT_STATUS_HIST.

        PERFORM F_UNLOCK_TRAN IN PROGRAM ZSDSFIR0250
          USING GT_OUTPUT .

        PERFORM F_UPDATE_FILE_LOG USING LT_SUCCESS
                                        'C'
                                        'S'
                                        LS_FILE-NAME .
      ENDIF.

      IF LT_ERROR[] IS NOT INITIAL .
        "Write error data to Outbound Error folder
        PERFORM F_WRITE_FILE USING LT_ERROR
                                   P_BDIR
                                   LS_FILE-NAME .

        PERFORM F_UPDATE_FILE_LOG USING LT_SUCCESS
                                        'N'
                                        'E'
                                        LS_FILE-NAME .
      ENDIF.

      IF LT_SUCCESS[] IS NOT INITIAL OR
         LT_ERROR[] IS NOT INITIAL .
        DELETE DATASET LF_FILENAME.
      ENDIF.

      CLEAR: LT_COLLECTOR, LF_LINE.

      PERFORM F_PREPARE_SEND_MAIL .

    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_prepare_send_mail
*&---------------------------------------------------------------------*
FORM F_PREPARE_SEND_MAIL  .
  DATA: LT_SAVE_SUCCESS TYPE TT_SAVE_RESULT,
        LT_SAVE_ERROR   TYPE TT_SAVE_RESULT.
  LT_SAVE_SUCCESS = GT_SAVE_RESULT.
  LT_SAVE_ERROR   = GT_SAVE_RESULT.
  DELETE LT_SAVE_SUCCESS WHERE SAVE_FLAG = 'E'.
  DELETE LT_SAVE_ERROR   WHERE SAVE_FLAG = 'S'.
  CLEAR GT_SAVE_RESULT.

  IF LT_SAVE_SUCCESS IS NOT INITIAL.
    PERFORM F_SEND_MAIL USING 'S' LT_SAVE_SUCCESS.
  ENDIF.
  IF LT_SAVE_ERROR IS NOT INITIAL.
    PERFORM F_SEND_MAIL USING 'E' LT_SAVE_ERROR.
  ENDIF.

*  PERFORM F_WRITE_LOG USING LT_SAVE_SUCCESS LT_SAVE_ERROR.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_movefile_to_backup
*&---------------------------------------------------------------------*
FORM F_MOVEFILE_TO_BACKUP  USING  UF_PATHFILENAME TYPE EPS2FILNAM
                                  UF_FILENAME     TYPE EPS2FILNAM.
  DATA: LF_LINE              TYPE I,
        LF_FILENAME_ARCHIVED TYPE EPS2FILNAM,
        LF_DATA              TYPE STRING.
  OPEN DATASET UF_PATHFILENAME FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  DO.
    READ DATASET UF_PATHFILENAME INTO LF_DATA.
    IF SY-SUBRC NE 0.
      EXIT.
    ENDIF.
    LF_LINE = LF_LINE + 1.
    IF LF_LINE = 1.
      CONCATENATE P_TDIR UF_FILENAME INTO LF_FILENAME_ARCHIVED.
      OPEN DATASET LF_FILENAME_ARCHIVED FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    ENDIF.
    TRANSFER LF_DATA TO LF_FILENAME_ARCHIVED.
  ENDDO.
  CLOSE DATASET UF_PATHFILENAME.
  CLOSE DATASET LF_FILENAME_ARCHIVED.
  DELETE DATASET UF_PATHFILENAME.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_send_mail
*&---------------------------------------------------------------------*
FORM F_SEND_MAIL  USING  UF_SAVE_FLAG     TYPE CHAR1
                         UT_SAVE_FILENAME TYPE TT_SAVE_RESULT .

  DATA:
    LF_TITLE   TYPE STRING,
    LF_SUBJECT TYPE STRING,
    LF_DES     TYPE SO_OBJ_DES,
    LF_RESULT  TYPE OS_BOOLEAN ##NEEDED.

  DATA: LT_CONTENT TYPE SOLI_TAB.
*        ls_content TYPE LINE OF soli_tab.

  DATA:
    LO_SEND_REQUEST  TYPE REF TO CL_BCS,
    LO_DOCUMENT      TYPE REF TO CL_DOCUMENT_BCS,
    LO_SENDER        TYPE REF TO IF_SENDER_BCS,
    LO_RECIPIENT     TYPE REF TO IF_RECIPIENT_BCS,
    LX_BCS_EXCEPTION TYPE REF TO CX_BCS ##NEEDED.

  DATA LR_EMAIL TYPE RANGE OF  ADR6-SMTP_ADDR  .
* --- send email ---
  TRY.

*-Create Title
      "Subject
      LF_SUBJECT = 'Daikin/Inf.Collector' ##NO_TEXT.

      IF UF_SAVE_FLAG = 'S'.
        "Text-C01: Completed
        CONCATENATE LF_SUBJECT TEXT-C01 INTO LF_TITLE SEPARATED BY '/'.
      ELSE.
        "Text-C02: Error
        CONCATENATE LF_SUBJECT TEXT-C02 INTO LF_TITLE SEPARATED BY '/'.
      ENDIF.

      LF_DES = LF_TITLE.

      LO_SEND_REQUEST = CL_BCS=>CREATE_PERSISTENT( ).

* Create body text
      IF UF_SAVE_FLAG = 'S'.
        PERFORM F_CONTENT_MAIL USING 'S' UT_SAVE_FILENAME
                             CHANGING LT_CONTENT.


        LR_EMAIL[] =  S_EMAILS[] .
      ENDIF.

      IF UF_SAVE_FLAG = 'E'.
        PERFORM F_CONTENT_MAIL USING 'E' UT_SAVE_FILENAME
                             CHANGING LT_CONTENT.
        LR_EMAIL[] =  S_EMAILE[] .
      ENDIF.

      LO_DOCUMENT = CL_DOCUMENT_BCS=>CREATE_DOCUMENT(
                          I_TYPE    = 'RAW'
                          I_TEXT    = LT_CONTENT
                          I_SUBJECT = LF_DES ).
      LO_SEND_REQUEST->SET_DOCUMENT( LO_DOCUMENT ).


      CALL METHOD LO_SEND_REQUEST->SET_MESSAGE_SUBJECT
        EXPORTING
          IP_SUBJECT = LF_TITLE.


      LO_SENDER = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( 'k2servicedev@daikin.co.th' ) ##NO_TEXT.
      CALL METHOD LO_SEND_REQUEST->SET_SENDER
        EXPORTING
          I_SENDER = LO_SENDER.

      LOOP AT LR_EMAIL ASSIGNING FIELD-SYMBOL(<LF_EMAIL>).

        LO_RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( <LF_EMAIL>-LOW ).
        CALL METHOD LO_SEND_REQUEST->ADD_RECIPIENT
          EXPORTING
            I_RECIPIENT  = LO_RECIPIENT
            I_EXPRESS    = 'X'
            I_BLIND_COPY = 'X'.
      ENDLOOP .

      CALL METHOD LO_SEND_REQUEST->SEND(
        EXPORTING
          I_WITH_ERROR_SCREEN = 'X'
        RECEIVING
          RESULT              = LF_RESULT ).
    CATCH CX_BCS INTO LX_BCS_EXCEPTION.
      RETURN.
  ENDTRY.

  COMMIT WORK.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_content_mail
*&---------------------------------------------------------------------*
FORM F_CONTENT_MAIL USING   UF_SAVE_FLAG TYPE CHAR1
                            UT_FILENAME  TYPE TT_SAVE_RESULT
                   CHANGING CT_CONTENT   TYPE SOLI_TAB.
  DATA: LS_CONTENT  TYPE LINE OF SOLI_TAB.

  DATA LT_FILENAME TYPE TT_SAVE_RESULT .

  LT_FILENAME[] = UT_FILENAME[] .
  SORT LT_FILENAME BY FILENAME .
  DELETE ADJACENT DUPLICATES FROM LT_FILENAME COMPARING FILENAME.

* Text-T08 : Dear All,
  LS_CONTENT-LINE = TEXT-T08 .
  APPEND LS_CONTENT TO CT_CONTENT.
  IF UF_SAVE_FLAG = 'S'.

* Text-T09: Please see file name that the processing is completed as following;
    LS_CONTENT-LINE = TEXT-T09.
    APPEND LS_CONTENT TO CT_CONTENT.
* Text-T10: File name completed:
    LS_CONTENT-LINE = TEXT-T10.
    APPEND LS_CONTENT TO CT_CONTENT.
  ENDIF.

  IF UF_SAVE_FLAG = 'E'.
* Text-T11:  Please see file name that the processing is error as following;
    LS_CONTENT-LINE = TEXT-T11.
    APPEND LS_CONTENT TO CT_CONTENT.
* Text-T12:  File name Error:
    LS_CONTENT-LINE = TEXT-T12.
    APPEND LS_CONTENT TO CT_CONTENT.
  ENDIF.

  LOOP AT LT_FILENAME ASSIGNING FIELD-SYMBOL(<LS_FILENAME>).
    CONCATENATE '   -  ' <LS_FILENAME>-FILENAME
           INTO LS_CONTENT SEPARATED BY SPACE.
    APPEND LS_CONTENT TO CT_CONTENT.
    IF UF_SAVE_FLAG = 'E'.
      "Display message error and FI Document and Year
      LOOP AT UT_FILENAME ASSIGNING FIELD-SYMBOL(<LS_FILE_FI>)
                          WHERE FILENAME = <LS_FILENAME>-FILENAME .
        LS_CONTENT = <LS_FILE_FI>-MSG .
        APPEND LS_CONTENT TO CT_CONTENT.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  APPEND INITIAL LINE TO CT_CONTENT.
* Text-T13: Best regards,
  LS_CONTENT-LINE = TEXT-T13.
  APPEND LS_CONTENT TO CT_CONTENT.
  LS_CONTENT-LINE = GF_NAME_SIGN_EMAIL.
  APPEND LS_CONTENT TO CT_CONTENT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_bp_log_open
*&---------------------------------------------------------------------*
FORM F_GET_BP_LOG_OPEN USING UT_COLLECTOR   TYPE  TT_COLLECTOR .
*  DATA LS_LOG_DATA TYPE TS_LOG .
  DATA LS_LOG_DATA TYPE TS_RESULT .

**-420000143 Beg of INS
*  DATA: LS_SUM_AMT TYPE TS_SUM_AMT .
**-420000143 End of INS
  CLEAR: GT_LOG, GT_PARTIAL  .
  IF UT_COLLECTOR IS NOT INITIAL .

    SELECT FROM ZSDSFIT029 AS Z
            LEFT OUTER JOIN PA0002 AS P
        ON Z~PERNR EQ P~PERNR
*-420000378 Beg of INS
    LEFT OUTER JOIN CEPCT AS PC                        "#EC CI_BUFFJOIN
    ON Z~PRCTR EQ PC~PRCTR
    AND PC~SPRAS EQ 'E'
    LEFT OUTER JOIN ZSDSFIC024 AS Z24
    ON Z~KUNNR EQ Z24~KUNNR
    LEFT OUTER JOIN CVI_CUST_LINK AS CL
    ON Z~KUNNR EQ CL~CUSTOMER
    LEFT OUTER JOIN BUT000 AS BP
    ON CL~PARTNER_GUID EQ BP~PARTNER_GUID
    LEFT OUTER JOIN TSAD3T AS TT
    ON BP~TITLE EQ TT~TITLE
    AND TT~LANGU EQ 'E'
*-420000378 End of INS
*-420000378 Beg of DEL
*     LEFT OUTER JOIN PA0002 AS P
*       ON Z~PERNR EQ P~PERNR
*     LEFT OUTER JOIN ZSDSFIC024 AS Z24
*       ON Z~KUNNR EQ Z24~KUNNR
*     LEFT OUTER JOIN BUT020 AS BP
*       ON Z~KUNNR EQ BP~PARTNER
*     LEFT OUTER JOIN ADRC AS BT
*       ON BP~ADDRNUMBER EQ BT~ADDRNUMBER
*      AND BT~NATION = ''
*     LEFT OUTER JOIN ADRC AS BE
*       ON BP~ADDRNUMBER EQ BE~ADDRNUMBER
*      AND BE~NATION = 'I'
*-420000378 End of DEL
     LEFT OUTER JOIN ZSDSFIT039 AS L                   "#EC CI_BUFFJOIN
       ON Z~BILLPL_NO EQ L~BILLPL_NO
      AND Z~BILLPL_DATE EQ L~BILLPL_DATE
      AND Z~BUKRS EQ L~BUKRS
      AND Z~BELNR EQ L~BELNR
      AND Z~GJAHR EQ L~GJAHR
    FIELDS Z~* ,
           P~VORNA ,
           P~NACHN ,
*-420000378 Beg of INS
      PC~KTEXT AS KTEXT,
      BP~NAME_ORG1 ,
      BP~NAME_ORG2 ,
      BP~NAME_ORG3 ,
      BP~NAME_ORG4 ,
      TT~TITLE_MEDI ,
      BP~NAME_FIRST ,
      BP~NAME_LAST ,

*-420000378 End of INS
*-420000378 Beg of DEL
*           BT~NAME1 ,
*           BT~NAME2 ,
*           BT~NAME3 ,
*           BT~NAME4 ,
*
*           BE~NAME1 AS BE_NAME1,
*           BE~NAME2 AS BE_NAME2,
*           BE~NAME3 AS BE_NAME3,
*           BE~NAME4 AS BE_NAME4,
*-420000378 End of DEL
           Z24~ZBLLCYL ,
           Z24~ZBLLCOL ,
           L~FILENAME AS ZFILE,
           L~STATU AS ZSTAT
       FOR ALL ENTRIES IN @UT_COLLECTOR
       WHERE Z~BUKRS   = @P_BUKRS
         AND Z~BELNR   = @UT_COLLECTOR-BELNR
         AND Z~GJAHR   = @UT_COLLECTOR-GJAHR
         AND Z~DELETE_FLAG = ''
        INTO TABLE @DATA(LT_LOG_DATA) ##TOO_MANY_ITAB_FIELDS.

*----------Case conversion => use tax invoice
    SELECT FROM ZSDSFIT029 AS Z
      LEFT OUTER JOIN PA0002 AS P
        ON Z~PERNR EQ P~PERNR
*-420000378 Beg of INS
      LEFT OUTER JOIN CEPCT AS PC                      "#EC CI_BUFFJOIN
        ON Z~PRCTR EQ PC~PRCTR
       AND PC~SPRAS EQ 'E'
      LEFT OUTER JOIN ZSDSFIC024 AS Z24
        ON Z~KUNNR EQ Z24~KUNNR
      LEFT OUTER JOIN CVI_CUST_LINK AS CL
        ON Z~KUNNR EQ CL~CUSTOMER
      LEFT OUTER JOIN BUT000 AS BP
        ON CL~PARTNER_GUID EQ BP~PARTNER_GUID
      LEFT OUTER JOIN TSAD3T AS TT
        ON BP~TITLE EQ TT~TITLE
       AND TT~LANGU EQ 'E'
*-420000378 End of INS
*-420000378 Beg of DEL
*     LEFT OUTER JOIN ZSDSFIC024 AS Z24
*       ON Z~KUNNR EQ Z24~KUNNR
*     LEFT OUTER JOIN BUT020 AS BP
*       ON Z~KUNNR EQ BP~PARTNER
*     LEFT OUTER JOIN ADRC AS BT
*       ON BP~ADDRNUMBER EQ BT~ADDRNUMBER
*      AND BT~NATION = ''
*     LEFT OUTER JOIN ADRC AS BE
*       ON BP~ADDRNUMBER EQ BE~ADDRNUMBER
*      AND BE~NATION = 'I'
*-420000378 End of DEL
     LEFT OUTER JOIN ZSDSFIT039 AS L                   "#EC CI_BUFFJOIN
       ON Z~BILLPL_NO EQ L~BILLPL_NO
      AND Z~BILLPL_DATE EQ L~BILLPL_DATE
      AND Z~BUKRS EQ L~BUKRS
      AND Z~BELNR EQ L~BELNR
      AND Z~GJAHR EQ L~GJAHR

    FIELDS Z~* ,
           P~VORNA ,
           P~NACHN ,
*-420000378 Beg of INS
           PC~KTEXT AS KTEXT,
           BP~NAME_ORG1 ,
           BP~NAME_ORG2 ,
           BP~NAME_ORG3 ,
           BP~NAME_ORG4 ,
           TT~TITLE_MEDI ,
           BP~NAME_FIRST ,
           BP~NAME_LAST ,
*-420000378 End of INS
*-420000378 Beg of INS
*           BT~NAME1 ,
*           BT~NAME2 ,
*           BT~NAME3 ,
*           BT~NAME4 ,
*           BE~NAME1 AS BE_NAME1,
*           BE~NAME2 AS BE_NAME2,
*           BE~NAME3 AS BE_NAME3,
*           BE~NAME4 AS BE_NAME4,
*-420000378 End of INS
           Z24~ZBLLCYL ,
           Z24~ZBLLCOL ,
           L~FILENAME AS ZFILE,
           L~STATU AS ZSTAT
       FOR ALL ENTRIES IN @UT_COLLECTOR
       WHERE Z~BUKRS   = @P_BUKRS
         AND Z~XBLNR   = @UT_COLLECTOR-XBLNR
         AND Z~DELETE_FLAG = ''
        APPENDING TABLE @LT_LOG_DATA ##TOO_MANY_ITAB_FIELDS.
*----------Case Conversion


    SELECT FROM VBRP AS IV                    "#EC CI_FAE_LINES_ENSURED
      LEFT OUTER JOIN VBAK AS SO
        ON IV~AUBEL EQ SO~VBELN
      LEFT OUTER JOIN VBAP AS SI
        ON SO~VBELN EQ SI~VBELN
       AND IV~AUPOS EQ SI~POSNR
      LEFT OUTER JOIN VBKD AS SB
        ON SO~VBELN EQ SB~VBELN
       AND SB~POSNR EQ '000000'
      LEFT OUTER JOIN VBPA AS SP
        ON SO~VBELN EQ SP~VBELN
       AND SP~PARVW EQ 'VE'
      LEFT OUTER JOIN BUT000 AS BP
        ON SP~ASSIGNED_BP EQ BP~PARTNER
      LEFT OUTER JOIN PRPS AS WBS
        ON SI~PS_PSP_PNR = WBS~PSPNR
      LEFT OUTER JOIN PROJ AS PJ
        ON WBS~PSPHI EQ PJ~PSPNR
      LEFT OUTER JOIN CEPCT AS PC                      "#EC CI_BUFFJOIN
        ON SI~PRCTR EQ PC~PRCTR
       AND PC~SPRAS EQ 'E'
      LEFT OUTER JOIN TVTWT AS SS
        ON SO~VTWEG EQ SS~VTWEG
       AND SS~SPRAS EQ 'E'
      LEFT OUTER JOIN TVGRT AS SG
        ON SO~VKGRP EQ SG~VKGRP
       AND SG~SPRAS EQ 'E'
      LEFT OUTER JOIN TVKBT AS SF
        ON SO~VKBUR EQ SF~VKBUR
       AND SF~SPRAS EQ 'E'

      FIELDS  IV~VBELN AS VBELN_VF,
              SO~VTWEG AS VTWEG,
              SP~PERNR AS KUNNE,
              SO~VKBUR AS VKBUR,
              SO~VKGRP AS VKGRP,
              PJ~POST1 AS POST1,
              SB~BSTKD AS BSTKD,
              SB~ZTERM AS ZTERM,
              PC~KTEXT AS KTEXT,
              IV~AUBEL AS VBELN,
              SI~PRCTR AS PRCTR,
              SS~VTEXT AS VTWET,
              SG~BEZEI AS VKGRT,
              SF~BEZEI AS VKBUT,
              BP~NAME_FIRST AS SP_FIRST,
              BP~NAME_LAST  AS SP_LAST
      FOR ALL ENTRIES IN @LT_LOG_DATA              "#EC CI_NO_TRANSFORM
      WHERE IV~VBELN EQ @LT_LOG_DATA-Z-VBELN_VF
      AND   IV~UEPOS  EQ '000000'
      INTO TABLE @DATA(LT_INV).
    IF SY-SUBRC EQ 0.
      SORT LT_INV BY VBELN_VF.
    ENDIF.


*-420000378 Beg of INS
    IF LT_LOG_DATA[] IS NOT INITIAL .
      SELECT FROM VBRP AS IV                       "#EC CI_NO_TRANSFORM
        INNER JOIN VBRK AS IH
           ON IV~VBELN EQ IH~VBELN
        INNER JOIN CEPCT AS PC                         "#EC CI_BUFFJOIN
           ON IV~PRCTR EQ PC~PRCTR
          AND PC~SPRAS EQ 'E'
        INNER JOIN TVGRT AS SG
           ON IV~VKGRP EQ SG~VKGRP
        AND SG~SPRAS EQ 'E'
        INNER JOIN TVKBT AS SF
        ON IV~VKBUR EQ SF~VKBUR
        AND SF~SPRAS EQ 'E'
        FIELDS  IV~POSNR,
                IH~ZTERM,
                IV~PRCTR AS PRCTR,
                PC~KTEXT AS KTEXT,
                SG~BEZEI AS VKGRT,
                SF~BEZEI AS VKBUT
        FOR ALL ENTRIES IN @LT_LOG_DATA
        WHERE IV~VBELN EQ @LT_LOG_DATA-Z-VBELN "Sales Order No.
        INTO TABLE @DATA(LT_INV_SERVICE).


      SELECT FROM VBPA AS SP                       "#EC CI_NO_TRANSFORM
        INNER JOIN BUT000 AS BP
        ON SP~ASSIGNED_BP EQ BP~PARTNER
      FIELDS
        BP~NAME_FIRST AS SP_FIRST,
        BP~NAME_LAST  AS SP_LAST
      FOR ALL ENTRIES IN @LT_LOG_DATA
      WHERE SP~VBELN EQ @LT_LOG_DATA-Z-VBELN
        AND SP~PARVW EQ 'ZM'
      INTO TABLE @DATA(LT_BP_SERVICE).

      DATA(LT_KUNNR) = LT_LOG_DATA[].
      SORT LT_KUNNR BY Z-KUNNR.
      DELETE ADJACENT DUPLICATES FROM LT_KUNNR COMPARING Z-KUNNR.
      IF LT_KUNNR IS NOT INITIAL.
        SELECT FROM BUT020 AS BP
          INNER JOIN ADRC AS BE
          ON BP~ADDRNUMBER EQ BE~ADDRNUMBER
          AND BE~NATION = 'I'
          FIELDS BP~PARTNER , BE~NAME1 , BE~NAME2 , BE~NAME3 , BE~NAME4
          FOR ALL ENTRIES IN @LT_KUNNR
          WHERE BP~PARTNER EQ @LT_KUNNR-Z-KUNNR
          INTO TABLE @DATA(LT_NAME_ENG).
        IF SY-SUBRC EQ 0.
          SORT LT_NAME_ENG BY PARTNER.
        ENDIF.
      ENDIF.
    ENDIF.
*-420000378 End of INS

    CLEAR GT_LOG_DATA .
    LOOP AT LT_LOG_DATA ASSIGNING FIELD-SYMBOL(<L_LOG_DATA>) .
      CLEAR LS_LOG_DATA.
      MOVE-CORRESPONDING <L_LOG_DATA>-Z TO LS_LOG_DATA .
      CONCATENATE <L_LOG_DATA>-VORNA <L_LOG_DATA>-NACHN
       INTO LS_LOG_DATA-PERNM SEPARATED BY SPACE.
*-420000378 Beg of INS
      IF LS_LOG_DATA-KUNNR+0(1) EQ 'E'.
        CONCATENATE <L_LOG_DATA>-TITLE_MEDI
                    <L_LOG_DATA>-NAME_FIRST
                    <L_LOG_DATA>-NAME_LAST
               INTO LS_LOG_DATA-CNAMT SEPARATED BY SPACE.
      ELSE.
        CONCATENATE <L_LOG_DATA>-NAME_ORG1
                    <L_LOG_DATA>-NAME_ORG2
                    <L_LOG_DATA>-NAME_ORG3
                    <L_LOG_DATA>-NAME_ORG4
         INTO LS_LOG_DATA-CNAMT SEPARATED BY SPACE.
      ENDIF.
*-420000378 End of INS

*-420000378 Beg of DEL
*      CONCATENATE <L_LOG_DATA>-NAME1
*                  <L_LOG_DATA>-NAME2
*                  <L_LOG_DATA>-NAME3
*                  <L_LOG_DATA>-NAME4
*             INTO LS_LOG_DATA-CNAMT SEPARATED BY SPACE.
*
*      CONCATENATE <L_LOG_DATA>-BE_NAME1
*                  <L_LOG_DATA>-BE_NAME2
*                  <L_LOG_DATA>-BE_NAME3
*                  <L_LOG_DATA>-BE_NAME4
*             INTO LS_LOG_DATA-CNAME SEPARATED BY SPACE.
*-420000378 End of DEL

      READ TABLE LT_INV ASSIGNING FIELD-SYMBOL(<LFS_INV>)
        WITH KEY VBELN_VF = LS_LOG_DATA-VBELN_VF
        BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        MOVE-CORRESPONDING <LFS_INV> TO LS_LOG_DATA.

        CONCATENATE <LFS_INV>-SP_FIRST  <LFS_INV>-SP_LAST
               INTO LS_LOG_DATA-SPTXT SEPARATED BY SPACE.
      ENDIF.

*-420000378 Beg of INS
      "Case Service
      IF LS_LOG_DATA-SPTXT IS INITIAL.
        READ TABLE LT_BP_SERVICE                        "#EC CI_NOORDER
          ASSIGNING FIELD-SYMBOL(<LFS_BP_SERVICE>)
          INDEX 1.
        IF SY-SUBRC EQ 0.
          CONCATENATE <LFS_BP_SERVICE>-SP_FIRST <LFS_BP_SERVICE>-SP_LAST
            INTO LS_LOG_DATA-SPTXT SEPARATED BY SPACE.
        ENDIF.
      ENDIF.

*-Beg of INS - Sale doc.case Service job Service MA
      IF LS_LOG_DATA-SPTXT IS INITIAL .
        SELECT SINGLE FROM VBFA                             "#EC WARNOK
          FIELDS VBELV
          WHERE VBELN EQ @LS_LOG_DATA-VBELN
          AND   VBTYP_N EQ 'EBDR'
          AND ( VBTYP_V EQ 'CSVO' OR VBTYP_V EQ 'CSCT' )
          INTO @DATA(LF_VBELN).
        IF SY-SUBRC EQ 0.
          LS_LOG_DATA-VBELN = LF_VBELN.

          SELECT SINGLE FROM CRMS4D_SERV_H                  "#EC WARNOK
            FIELDS ZZ1_CUS_PO
            WHERE ( OBJTYPE_H EQ 'BUS2000116' OR OBJTYPE_H EQ 'BUS2000112' )
            AND   OBJECT_ID EQ @LF_VBELN
            INTO @DATA(LF_CUS_PO).
          IF SY-SUBRC EQ 0.
            LS_LOG_DATA-BSTKD = LF_CUS_PO.
          ENDIF.
        ENDIF.
      ENDIF.
*-End of INS - Sale doc.case Service job Service MA


      IF LS_LOG_DATA-ZTERM IS INITIAL OR
         LS_LOG_DATA-PRCTR IS INITIAL OR
         LS_LOG_DATA-VKGRT IS INITIAL OR
         LS_LOG_DATA-VKBUT IS INITIAL.

        READ TABLE LT_INV_SERVICE                       "#EC CI_NOORDER
          ASSIGNING FIELD-SYMBOL(<LFS_INV_SERVICE>)
          INDEX 1.
        IF SY-SUBRC EQ 0.
          IF LS_LOG_DATA-ZTERM IS INITIAL.
            LS_LOG_DATA-ZTERM = <LFS_INV_SERVICE>-ZTERM.
          ENDIF.

          IF LS_LOG_DATA-PRCTR IS INITIAL.
            LS_LOG_DATA-PRCTR = <LFS_INV_SERVICE>-PRCTR.
            LS_LOG_DATA-KTEXT = <LFS_INV_SERVICE>-KTEXT.
          ENDIF.

          IF LS_LOG_DATA-VKGRT IS INITIAL.
            LS_LOG_DATA-VKGRT = <LFS_INV_SERVICE>-VKGRT.
          ENDIF.

          IF LS_LOG_DATA-VKBUT IS INITIAL.
            LS_LOG_DATA-VKBUT = <LFS_INV_SERVICE>-VKBUT.
          ENDIF.
        ENDIF.

      ENDIF.

      READ TABLE LT_NAME_ENG ASSIGNING FIELD-SYMBOL(<LFS_NAME_ENG>)
        WITH KEY PARTNER = LS_LOG_DATA-KUNNR
          BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        CONCATENATE
          <LFS_NAME_ENG>-NAME1
          <LFS_NAME_ENG>-NAME2
          <LFS_NAME_ENG>-NAME3
          <LFS_NAME_ENG>-NAME4
          INTO LS_LOG_DATA-CNAME SEPARATED BY SPACE.
      ENDIF.
*-420000378 End of INS
      LS_LOG_DATA-BANKK2  = LS_LOG_DATA-BANKK.
*-Beg of MOD
*      LS_LOG_DATA-COLLECTION_CYCLE = <L_LOG_DATA>-ZBLLCYL .
*      LS_LOG_DATA-BILLING_CYCLE = <L_LOG_DATA>-ZBLLCOL .
      LS_LOG_DATA-COLLECTION_CYCLE = <L_LOG_DATA>-ZBLLCOL .
      LS_LOG_DATA-BILLING_CYCLE = <L_LOG_DATA>-ZBLLCYL .
*-End of MOD
      APPEND LS_LOG_DATA TO GT_LOG_DATA.
    ENDLOOP .

    SORT GT_LOG_DATA BY BUKRS ASCENDING
                        BELNR ASCENDING
                        GJAHR ASCENDING
                        BUZEI ASCENDING
                        SEQ   DESCENDING.

*-Beg of INS - Add for fix double lines problem
    DELETE ADJACENT DUPLICATES FROM GT_LOG_DATA
                          COMPARING BUKRS BELNR GJAHR BUZEI SEQ .
*-End of INS

**-420000143 Beg of INS
*    CLEAR: LS_SUM_AMT, GT_SUM_AMT[].
*
*    LOOP AT GT_LOG_DATA ASSIGNING FIELD-SYMBOL(<L_LOG>).
*      IF <L_LOG>-SEQ = '001' .  "Only First record
*        MOVE-CORRESPONDING <L_LOG> TO LS_SUM_AMT .
*        LS_SUM_AMT-CNTNO = 1 .
*        COLLECT  LS_SUM_AMT INTO GT_SUM_AMT .
*        CLEAR LS_SUM_AMT .
*      ENDIF.
*    ENDLOOP .
**-420000143 End of INS

    SELECT * INTO TABLE GT_ZSDSFIT046
      FROM ZSDSFIT046
       FOR ALL ENTRIES IN UT_COLLECTOR
     WHERE BUKRS = UT_COLLECTOR-BUKRS
       AND BELNR = UT_COLLECTOR-BELNR
       AND GJAHR = UT_COLLECTOR-GJAHR
       AND BUZEI = UT_COLLECTOR-BUZEI .
    IF SY-SUBRC EQ 0 .
      SORT GT_ZSDSFIT046 BY BUKRS BELNR GJAHR BUZEI .
    ENDIF .

    SELECT * INTO TABLE GT_ZSDSFIT037
      FROM ZSDSFIT037
       FOR ALL ENTRIES IN UT_COLLECTOR
     WHERE BUKRS = UT_COLLECTOR-BUKRS
       AND BELNR = UT_COLLECTOR-BELNR
       AND GJAHR = UT_COLLECTOR-GJAHR
       AND BUZEI = UT_COLLECTOR-BUZEI.
    IF SY-SUBRC = 0 .
      SORT GT_ZSDSFIT037  BY BUKRS BELNR GJAHR BUZEI SEQ  .
    ENDIF.

    "Get FIDOC for declare Input data type
    CLEAR GT_FIDOC .
    SELECT  BUKRS,
            BELNR,
            GJAHR,
            XBLNR
      FROM  BKPF
       FOR ALL ENTRIES IN @UT_COLLECTOR
     WHERE BUKRS = @UT_COLLECTOR-BUKRS
      AND  BELNR = @UT_COLLECTOR-BELNR
      AND  GJAHR = @UT_COLLECTOR-GJAHR
      INTO TABLE @GT_FIDOC .
    IF SY-SUBRC = 0 .
      SORT GT_FIDOC BY BUKRS BELNR GJAHR .
    ENDIF.
  ENDIF.

  IF GT_LOG_DATA IS NOT INITIAL.
    "Get FIDOC for declare Input data type
    CLEAR GT_FIDOC .
    SELECT  BUKRS,
            BELNR,
            GJAHR,
            XBLNR
      FROM  BKPF
       FOR ALL ENTRIES IN @GT_LOG_DATA
     WHERE BUKRS = @GT_LOG_DATA-BUKRS
      AND  BELNR = @GT_LOG_DATA-BELNR
      AND  GJAHR = @GT_LOG_DATA-GJAHR
      APPENDING TABLE @GT_FIDOC .
    IF SY-SUBRC = 0 .
      SORT GT_FIDOC BY BUKRS BELNR GJAHR .
    ENDIF.

    SELECT  WI~BUKRS,
            WI~BELNR,
            WI~GJAHR,
            WI~BUZEI,
            WI~WITHT,
            WI~WT_WITHCD,
            WI~WT_QSSHB,
            RT~QSATZ,
            RT~QPROZ,
            WI~WT_QBSHB
      FROM WITH_ITEM AS WI INNER JOIN T001  AS COMP
                                   ON WI~BUKRS     = COMP~BUKRS
                           INNER JOIN T059Z AS RT      "#EC CI_BUFFJOIN
                                   ON RT~LAND1     = COMP~LAND1
                                  AND RT~WITHT     = WI~WITHT
                                  AND RT~WT_WITHCD = WI~WT_WITHCD
       FOR ALL ENTRIES IN @GT_LOG_DATA
      WHERE WI~WT_QSSHB IS NOT INITIAL
        AND WI~BUKRS = @GT_LOG_DATA-BUKRS
        AND WI~BELNR = @GT_LOG_DATA-BELNR
        AND WI~GJAHR = @GT_LOG_DATA-GJAHR
        AND WI~BUZEI = @GT_LOG_DATA-BUZEI
        INTO CORRESPONDING FIELDS OF TABLE @GT_WHTAX ##TOO_MANY_ITAB_FIELDS.

    SELECT
      BUKRS,
      BELNR,
      GJAHR,
      BUZEI,
      NAME1,
      NAME2
      FROM BSEC
      FOR ALL ENTRIES IN @GT_LOG_DATA
      WHERE BUKRS = @GT_LOG_DATA-BUKRS
      AND   BELNR = @GT_LOG_DATA-BELNR
      AND   GJAHR = @GT_LOG_DATA-GJAHR
      AND   BUZEI = @GT_LOG_DATA-BUZEI                  "#EC CI_NOORDER
      INTO TABLE @GT_ONETIME.                      "#EC CI_NO_TRANSFORM

    SELECT *
      INTO TABLE @GT_BPLOG
      FROM ZSDSFIT029
      FOR ALL ENTRIES IN @GT_LOG_DATA
      WHERE BUKRS = @GT_LOG_DATA-BUKRS
      AND   BELNR = @GT_LOG_DATA-BELNR
      AND   GJAHR = @GT_LOG_DATA-GJAHR
      AND   BUZEI = @GT_LOG_DATA-BUZEI
      AND   DELETE_FLAG = ''.                      "#EC CI_NO_TRANSFORM
    IF GT_BPLOG IS NOT INITIAL.
*      "Clearing link list
*      SELECT *
*        INTO TABLE @GT_CLR_LINK
*        FROM ZSDSFIT037
*        FOR ALL ENTRIES IN @GT_BPLOG
*        WHERE BUKRS = @GT_BPLOG-BUKRS
*        AND   BELNR = @GT_BPLOG-BELNR
*        AND   GJAHR = @GT_BPLOG-GJAHR
*        AND   BUZEI = @GT_BPLOG-BUZEI
*        AND   SEQ   = @GT_BPLOG-SEQ.               "#EC CI_NO_TRANSFORM

      "Change history
      SELECT *
        INTO TABLE @GT_STATUS_HIST
        FROM ZSDSFIT038
        FOR ALL ENTRIES IN @GT_BPLOG
        WHERE BUKRS = @GT_BPLOG-BUKRS
        AND   BELNR = @GT_BPLOG-BELNR
        AND   GJAHR = @GT_BPLOG-GJAHR
        AND   BUZEI = @GT_BPLOG-BUZEI              "#EC CI_NO_TRANSFORM
        AND   SEQ   = @GT_BPLOG-SEQ.          "#EC CI_ALL_FIELDS_NEEDED
      IF SY-SUBRC = 0 .
        SORT GT_STATUS_HIST BY BUKRS BELNR GJAHR  BUZEI SEQ .
      ENDIF.
    ENDIF.

    SELECT
      FROM BSID_VIEW AS ITM
      INNER JOIN BKPF AS HDR
      ON  HDR~BUKRS = ITM~BUKRS
      AND HDR~BELNR = ITM~BELNR
      AND HDR~GJAHR = ITM~GJAHR
      FIELDS
        ITM~BUKRS,
        ITM~BELNR,
        ITM~GJAHR,
        ITM~BUZEI,
        ITM~WAERS,
        ITM~DMBTR,
        ITM~WRBTR,
        ITM~REBZG,
        ITM~REBZJ,
        ITM~REBZZ,
        HDR~XREF2_HD
      FOR ALL ENTRIES IN @GT_LOG_DATA
      WHERE ITM~BUKRS = @GT_LOG_DATA-BUKRS
      AND   ITM~REBZG = @GT_LOG_DATA-BELNR
      AND   ITM~REBZJ = @GT_LOG_DATA-GJAHR
      AND   ITM~REBZZ = @GT_LOG_DATA-BUZEI
      INTO TABLE @GT_PARTIAL.                      "#EC CI_NO_TRANSFORM

    IF SY-SUBRC = 0 .
      SORT GT_PARTIAL BY BUKRS BELNR GJAHR .
    ENDIF.

  ELSE.
    MESSAGE S000(38) WITH TEXT-E00. "No data found
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_prepare_change_output
*&---------------------------------------------------------------------*
FORM F_PREPARE_CHANGE_OUTPUT USING UT_COLLECTOR TYPE  TT_COLLECTOR
                                   UF_FILENAME  TYPE  EPS2FILNAM
                          CHANGING CT_ARCHIVED  TYPE  TT_COLLECTOR
                                   CT_SUCCESS   TYPE  TT_COLLECTOR
                                   CT_ERROR     TYPE  TT_COLLECTOR .


  DATA: LS_RESULT      LIKE LINE OF GT_RESULT,
        LS_SAVE_RESULT TYPE TS_SAVE_RESULT.

  DATA: LS_OUTPUT     LIKE LINE OF GT_OUTPUT,
        LS_SUM_BILLPL TYPE TS_SUM_BILLPL.

  DATA: LT_PARTIAL_CHEQUE TYPE TT_PARTIAL_CHEQUE,
        LS_PARTIAL_CHEQUE TYPE TS_PARTIAL_CHEQUE.

*-420000633 Beg of INS
  DATA: LT_PARTIAL_MEMO TYPE TT_PARTIAL_MEMO,
        LS_PARTIAL_MEMO TYPE TS_PARTIAL_MEMO.

  CLEAR: LT_PARTIAL_MEMO , LS_PARTIAL_MEMO .
*-420000633 End of INS

**-420000143 Beg of INS
*  DATA: LS_OUTPUT_N LIKE LINE OF GT_OUTPUT,
*        LT_LOG      TYPE TT_RESULT.
*
*  LT_LOG[] = GT_LOG_DATA[] .
**-420000143 End of INS
  CLEAR: CT_ARCHIVED[], CT_ERROR[], GT_OUTPUT[], GT_SUM_BILLPL[].

  "Summary Bill placement
  LOOP AT UT_COLLECTOR ASSIGNING FIELD-SYMBOL(<L_COLLECTOR>).
    LS_SUM_BILLPL-BUKRS = <L_COLLECTOR>-BUKRS.
    LS_SUM_BILLPL-BELNR = <L_COLLECTOR>-BELNR.
    LS_SUM_BILLPL-GJAHR = <L_COLLECTOR>-GJAHR .
    LS_SUM_BILLPL-BUZEI = <L_COLLECTOR>-BUZEI.
    LS_SUM_BILLPL-SEQ   = <L_COLLECTOR>-SEQ.
    LS_SUM_BILLPL-BILLPL_NO  = <L_COLLECTOR>-BILLPL_NO .
    LS_SUM_BILLPL-BILLPL_AMT = <L_COLLECTOR>-WRBTR .  "Summary FI Amount
    COLLECT LS_SUM_BILLPL INTO GT_SUM_BILLPL .
  ENDLOOP .

  SORT LT_PARTIAL_CHEQUE BY BUKRS BELNR GJAHR WORK_DATE CHEQUE_NO.
  DELETE ADJACENT DUPLICATES FROM LT_PARTIAL_CHEQUE COMPARING WORK_DATE CHEQUE_NO .

  LOOP AT UT_COLLECTOR ASSIGNING <L_COLLECTOR>.

    READ TABLE GT_LOG_DATA ASSIGNING FIELD-SYMBOL(<L_LOG_DATA>)
      WITH KEY BUKRS = <L_COLLECTOR>-BUKRS
               BELNR = <L_COLLECTOR>-BELNR
               GJAHR = <L_COLLECTOR>-GJAHR .
    "Addition for case conversion by use XBLNR for check
    IF SY-SUBRC NE 0 .
      READ TABLE GT_LOG_DATA ASSIGNING <L_LOG_DATA>
        WITH KEY BUKRS = <L_COLLECTOR>-BUKRS
                 XBLNR = <L_COLLECTOR>-XBLNR .
*                 GJAHR = <L_COLLECTOR>-GJAHR .

    ENDIF.

    IF SY-SUBRC = 0 .
      CLEAR LS_RESULT .
      MOVE-CORRESPONDING <L_LOG_DATA> TO LS_RESULT .

      PERFORM F_SET_FILE_VALUE :
          USING <L_COLLECTOR>-PERNR          CHANGING LS_RESULT-PERNR  ,
          USING <L_COLLECTOR>-WORK_DATE      CHANGING LS_RESULT-WORK_DATE,
          USING <L_COLLECTOR>-ACTION_TYPE    CHANGING LS_RESULT-ACTION_TYPE,
          USING <L_COLLECTOR>-STATUS         CHANGING LS_RESULT-STATUS.
*-Beg of ADD 09.04.2025
      PERFORM F_SET_FILE_VALUE :
        USING <L_COLLECTOR>-RECEIPT_NO CHANGING LS_RESULT-RECEIPT_NO.
*        USING <L_COLLECTOR>-PRCTR      CHANGING LS_RESULT-PRCTR. "F36K914905--
*-End of ADD 09.04.2025
      IF LS_RESULT-ACTION_TYPE = GC_VENDOR     OR
         LS_RESULT-ACTION_TYPE = GC_BILLPLACE  OR
         LS_RESULT-ACTION_TYPE = GC_COLLECTION OR
         LS_RESULT-ACTION_TYPE = GC_OTHER      OR
         LS_RESULT-ACTION_TYPE = GC_FILING .
        PERFORM F_SET_FILE_VALUE :
          USING <L_COLLECTOR>-PAYNO            CHANGING LS_RESULT-PAYNO,
          USING <L_COLLECTOR>-BRNCH            CHANGING LS_RESULT-BRNCH,
          USING <L_COLLECTOR>-EXPS_AMT         CHANGING LS_RESULT-EXPS_AMT ,
          USING <L_COLLECTOR>-WHT_AMT          CHANGING LS_RESULT-WHT_AMT,
          USING <L_COLLECTOR>-FEE              CHANGING LS_RESULT-FEE,
          USING <L_COLLECTOR>-RETENTION        CHANGING LS_RESULT-RETENTION,
          USING <L_COLLECTOR>-INCOME_AMT       CHANGING LS_RESULT-INCOME_AMT,
          USING <L_COLLECTOR>-CASH_CON         CHANGING LS_RESULT-CASH_CON,
          USING <L_COLLECTOR>-PAYIN_AMT        CHANGING LS_RESULT-PAYIN_AMT,
          USING <L_COLLECTOR>-REMARK           CHANGING LS_RESULT-REMARK    ,
          USING <L_COLLECTOR>-BANK_ITEM        CHANGING LS_RESULT-BANK_ITEM   ,
          USING <L_COLLECTOR>-DOCUMENT_STATUS  CHANGING LS_RESULT-DOCUMENT_STATUS ,
          USING <L_COLLECTOR>-REMARK_VENDOR    CHANGING LS_RESULT-REMARK_VENDOR  ,
          USING <L_COLLECTOR>-BILLING_CYCLE    CHANGING LS_RESULT-BILLING_CYCLE   ,
          USING <L_COLLECTOR>-COLLECTION_CYCLE CHANGING LS_RESULT-COLLECTION_CYCLE ,
          USING <L_COLLECTOR>-AWB_NO           CHANGING LS_RESULT-AWB_NO  ,
          USING <L_COLLECTOR>-BANKK2           CHANGING LS_RESULT-BANKK2  .
      ENDIF.

      IF LS_RESULT-ACTION_TYPE = GC_BILLPLACE  .
        PERFORM F_SET_FILE_VALUE :
            USING <L_COLLECTOR>-PAYMENT_DATE     CHANGING LS_RESULT-PAYMENT_DATE .
      ENDIF.

      IF LS_RESULT-ACTION_TYPE = GC_COLLECTION AND
         LS_RESULT-STATUS      = GC_RECEIVED  .
        PERFORM F_SET_FILE_VALUE :
              USING <L_COLLECTOR>-RECEIVED_DATE CHANGING LS_RESULT-RECEIVED_DATE ,
*-Beg of DEL 09.04.2025
*              USING <L_COLLECTOR>-RECEIVED_AMT  CHANGING LS_RESULT-RECEIVED_AMT,
*-End of DEL 09.04.2025
              USING <L_COLLECTOR>-DEDUCT_AMT    CHANGING LS_RESULT-DEDUCT_AMT,
              USING <L_COLLECTOR>-PYMT_METHOD   CHANGING LS_RESULT-PYMT_METHOD,
*              USING <L_COLLECTOR>-CHEQUE_DATE   CHANGING LS_RESULT-CHEQUE_DATE ,
              USING <L_COLLECTOR>-BANK_DATE     CHANGING LS_RESULT-BANK_DATE,
              USING <L_COLLECTOR>-CHEQUE_NO     CHANGING LS_RESULT-CHEQUE_NO  ,
              USING <L_COLLECTOR>-BANKL         CHANGING LS_RESULT-BANKL,
              USING <L_COLLECTOR>-HBKID         CHANGING LS_RESULT-HBKID,
              USING <L_COLLECTOR>-HKTID         CHANGING LS_RESULT-HKTID,
              USING <L_COLLECTOR>-BANKK         CHANGING LS_RESULT-BANKK,
              USING <L_COLLECTOR>-BANKN         CHANGING LS_RESULT-BANKN.
      ENDIF.

      IF LS_RESULT-ACTION_TYPE = GC_VENDOR OR
         LS_RESULT-ACTION_TYPE = GC_OTHER  .

        PERFORM F_SET_FILE_VALUE :
          USING <L_COLLECTOR>-FOLLOW_DATE      CHANGING LS_RESULT-FOLLOW_DATE .
      ENDIF.

      IF LS_RESULT-ACTION_TYPE = GC_BILLPLACE OR
         LS_RESULT-ACTION_TYPE = GC_COLLECTION .

        PERFORM F_SET_FILE_VALUE :
          USING <L_COLLECTOR>-BILLPL_NO        CHANGING LS_RESULT-BILLPL_NO ,
          USING <L_COLLECTOR>-BILLPL_AMT       CHANGING LS_RESULT-BILLPL_AMT  .

      ENDIF.

      IF LS_RESULT-ACTION_TYPE = GC_BILLPLACE  OR
         LS_RESULT-ACTION_TYPE = GC_COLLECTION OR
         LS_RESULT-ACTION_TYPE = GC_OTHER  .

        PERFORM F_SET_FILE_VALUE :
          USING <L_COLLECTOR>-INV_STATUS       CHANGING LS_RESULT-INV_STATUS ,
          USING <L_COLLECTOR>-REASON_VENDOR    CHANGING LS_RESULT-REASON_VENDOR  .
      ENDIF.

      IF LS_RESULT-PYMT_METHOD IN GR_PDC AND LS_RESULT-CHEQUE_NO IS NOT INITIAL.
        IF LS_RESULT-BANKN IS INITIAL.
          LS_RESULT-BANKN = GV_DF_PDC_BANK_ACCT.
        ENDIF.
      ENDIF.

      IF LS_RESULT-EXPS_AMT IS NOT INITIAL.
        IF LS_RESULT-AUFNR_EXPS IS INITIAL.
          LS_RESULT-AUFNR_EXPS = GV_DF_ORDER_EXP.
        ENDIF.
      ELSE.
        CLEAR LS_RESULT-AUFNR_EXPS.
      ENDIF.

*-420000463 Beg of INS
      IF LS_RESULT-FEE IS NOT INITIAL.
        IF LS_RESULT-AUFNR_FEE IS INITIAL.
          LS_RESULT-AUFNR_FEE = GV_DF_ORDER_FEE.
        ENDIF.
      ELSE.
        CLEAR LS_RESULT-AUFNR_FEE.
      ENDIF.
*-420000463 End of INS
*---No have in structure text file
*LS_RESULT-BILLPL_DATE  = <L_COLLECTOR>-BILLPL_DATE.
*LS_RESULT-UMSKZ  = <L_COLLECTOR>-UMSKZ.
*LS_RESULT-FAEDT  = <L_COLLECTOR>-FAEDT.
*LS_RESULT-AUFNR_FEE  = <L_COLLECTOR>-AUFNR_FEE.
*LS_RESULT-BANKK  = <L_COLLECTOR>-BANKK.
*LS_RESULT-BANK_DATE  = <L_COLLECTOR>-BANK_DATE.
*LS_RESULT-FKDAT  = <L_COLLECTOR>-FKDAT.

*--------------------------------------------------------------------

      LS_RESULT-STATU =  ICON_LED_GREEN.
      "Text-T18: Test run sucessful.
      IF P_TEST IS NOT INITIAL .
        LS_RESULT-MSGTX = TEXT-T18.
      ELSE.
        "Updated complete
        LS_RESULT-MSGTX = TEXT-T20.
      ENDIF.
    ELSE.
      MOVE-CORRESPONDING <L_COLLECTOR> TO LS_RESULT .
      MOVE-CORRESPONDING <L_COLLECTOR> TO LS_OUTPUT .

      LS_RESULT-SEL = GC_X .
      LS_RESULT-STATU =  ICON_LED_RED.
      "Text-t19 : Data not found in Table log : ZSDSFIT029
      LS_RESULT-MSGTX = TEXT-T19.

    ENDIF.

*  Logic from Perform f_on_data_changed_finished (ZSDSFIR250_F01)
    LS_RESULT-BAL_AMT = LS_RESULT-DEDUCT_AMT .
    LS_RESULT-REMAIN_AMT = LS_RESULT-WRBTR.  "Copy Case Create log
*    "Witholding tax amount
*    READ TABLE GT_WHTAX
*      INTO DATA(LS_WHTAX)
*      WITH KEY BUKRS = LS_RESULT-BUKRS
*               BELNR = LS_RESULT-BELNR
*               GJAHR = LS_RESULT-GJAHR
*               BUZEI = LS_RESULT-BUZEI.
*    IF SY-SUBRC EQ 0.
*      LS_RESULT-WHT_AMT = ( LS_RESULT-BAL_AMT * ( LS_WHTAX-QSATZ / LS_WHTAX-QPROZ ) ) * -1.
*    ENDIF.
*-420000106 Beg of INS
*-Validate Account Line no.
    IF LS_RESULT-BUZEI IS INITIAL AND
       LS_RESULT-DATA_TYPE IS INITIAL.
      LS_RESULT-STATU =  ICON_LED_RED.
      "Text-t21 : Line Item is initial.
      LS_RESULT-MSGTX = TEXT-T21.
    ENDIF.
*-420000106 End of INS

    IF ( LS_RESULT-STATU NE ICON_LED_RED OR
         LS_RESULT-STATU = '@0A@' ). "Error
      PERFORM F_VALIDATE_MANDATORY CHANGING LS_RESULT.
      PERFORM F_VALIDATE_INPUT     CHANGING LS_RESULT .

      LS_RESULT-REMAIN_C_AMT = LS_RESULT-REMAIN_AMT - LS_RESULT-BAL_AMT.

      "Set input data type (FI Doc or Credit memo)
      READ TABLE GT_FIDOC ASSIGNING FIELD-SYMBOL(<LFS_FIDOC>)
      WITH KEY BUKRS = LS_RESULT-BUKRS
               BELNR = LS_RESULT-BELNR
               GJAHR = LS_RESULT-GJAHR .
      IF SY-SUBRC NE 0.
        LS_RESULT-DATA_TYPE =  'M' .
      ELSE.
        CLEAR LS_RESULT-DATA_TYPE .
*-F36K914905 Case 0404 0403 Begin of INS
        IF ( LS_RESULT-ACTION_TYPE = GC_OTHER AND LS_RESULT-STATUS = GC_FOLLOW ) OR
           ( LS_RESULT-ACTION_TYPE = GC_OTHER AND LS_RESULT-STATUS = GC_FINISHED ) .

          LS_RESULT-DATA_TYPE =  'M' .

        ENDIF.
*-F36K914905 Case 0404 0403 End of INS

      ENDIF.

      IF ( LS_RESULT-STATU NE ICON_LED_RED OR
           LS_RESULT-STATU = '@0A@' ). "Error "420000106++
        "Set data from Log table to output
        MOVE-CORRESPONDING LS_RESULT TO LS_OUTPUT .

        LS_OUTPUT-SEL = GC_X .
      ELSE.
        CLEAR LS_OUTPUT-SEL .
      ENDIF."420000106++
    ENDIF.

    "Set data for display in email and write file
    IF ( LS_RESULT-STATU = '@0A@' OR "420000106++
         LS_RESULT-STATU = ICON_LED_RED ). "Error
      CLEAR LS_SAVE_RESULT .
      LS_SAVE_RESULT-FILENAME = UF_FILENAME.
      LS_SAVE_RESULT-SAVE_FLAG = 'E'.
      CONCATENATE LS_RESULT-BUKRS
                  LS_RESULT-BELNR
                  LS_RESULT-GJAHR
                  LS_RESULT-MSGTX
      INTO LS_SAVE_RESULT-MSG SEPARATED BY SPACE .

      APPEND LS_SAVE_RESULT TO GT_SAVE_RESULT.
      APPEND <L_COLLECTOR> TO CT_ERROR .
    ELSE.
      CLEAR LS_SAVE_RESULT .
      LS_SAVE_RESULT-FILENAME = UF_FILENAME.
      LS_SAVE_RESULT-SAVE_FLAG = 'S'.
      LS_SAVE_RESULT-MSG = LS_RESULT-MSGTX.
      APPEND LS_SAVE_RESULT TO GT_SAVE_RESULT.
      APPEND <L_COLLECTOR> TO CT_SUCCESS .

**-420000143 Beg of INS
*      READ TABLE GT_SUM_AMT ASSIGNING FIELD-SYMBOL(<L_SUM_AMT>)
*        WITH KEY DATA_TYPE = LS_OUTPUT-DATA_TYPE
*                 BUKRS = LS_OUTPUT-BUKRS
*                 BELNR = LS_OUTPUT-BELNR
*                 GJAHR = LS_OUTPUT-GJAHR  .
*      IF <L_SUM_AMT> IS ASSIGNED .
*        IF <L_SUM_AMT>-CNTNO > 1 .
*
*          IF LS_RESULT-ACTION_TYPE = GC_COLLECTION AND
*             LS_RESULT-STATUS      = GC_RECEIVED  .
*            LOOP AT LT_LOG ASSIGNING FIELD-SYMBOL(<L_LOG>)
*                           WHERE DATA_TYPE = <L_SUM_AMT>-DATA_TYPE
*                             AND BUKRS     = <L_SUM_AMT>-BUKRS
*                             AND BELNR     = <L_SUM_AMT>-BELNR
*                             AND GJAHR     = <L_SUM_AMT>-GJAHR
*                             AND SEQ       = '0001' .
*              "case > 1 line Set amount =
*              IF <L_LOG>-BUZEI NE '0001' .
*                CLEAR LS_OUTPUT_N .
*                MOVE-CORRESPONDING <L_LOG> TO LS_OUTPUT_N .
*                LS_OUTPUT_N-BAL_AMT    = <L_LOG>-WRBTR .
*                LS_OUTPUT_N-DEDUCT_AMT = <L_LOG>-WRBTR .
*
*                CLEAR: LS_OUTPUT_N-REMAIN_C_AMT, LS_OUTPUT_N-REMAIN_AMT .
*                "Set Action, Status
*                LS_OUTPUT_N-ACTION_TYPE = LS_OUTPUT-ACTION_TYPE .
*                LS_OUTPUT_N-STATUS = LS_OUTPUT-STATUS.
*                LS_OUTPUT_N-SEL = GC_X .
*                APPEND LS_OUTPUT_N TO GT_OUTPUT .
*              ELSE.
*                LS_OUTPUT-DEDUCT_AMT = <L_LOG>-WRBTR .
*                LS_OUTPUT-BAL_AMT    = <L_LOG>-WRBTR .
*                CLEAR: LS_OUTPUT-REMAIN_C_AMT, LS_OUTPUT-REMAIN_AMT .
*              ENDIF.
*            ENDLOOP.
*
*
*            "Set for update in table ZSDSFIT029
*            APPEND LS_OUTPUT TO GT_OUTPUT .
*
*          ENDIF.
*        ENDIF.
*      ELSE.
**-420000143 End of INS

*-420000633 Beg of INS
*-Find Case Partial not equal 02 02
      IF LS_RESULT-ACTION_TYPE NE GC_COLLECTION AND
          LS_RESULT-STATUS     NE GC_RECEIVED  .

        LS_OUTPUT-WRBTR = <L_COLLECTOR>-WRBTR . "Amount
        LS_RESULT-WRBTR = <L_COLLECTOR>-WRBTR .
        CLEAR LS_PARTIAL_MEMO.
        MOVE-CORRESPONDING LS_RESULT TO LS_PARTIAL_MEMO.
        APPEND LS_PARTIAL_MEMO TO LT_PARTIAL_MEMO .
      ENDIF.
*-420000633 End of INS
      "Set for update in table ZSDSFIT029
      APPEND LS_OUTPUT TO GT_OUTPUT .

**-420000143 Beg of INS
*      ENDIF.
**-420000143 End of INS


    ENDIF.

    APPEND <L_COLLECTOR> TO CT_ARCHIVED .

    "Add filename and File date/time
    LS_RESULT-FILENAME = <L_COLLECTOR>-FILENAME .
    LS_RESULT-UPLOAD_DATE = <L_COLLECTOR>-UPLOAD_DATE .
    LS_RESULT-UPLOAD_TIME = <L_COLLECTOR>-UPLOAD_TIME .

    APPEND LS_RESULT TO GT_RESULT .

*-Find Case Partial cheque( 1 FIDOC per cheque > 1 )
    CLEAR LS_PARTIAL_CHEQUE.
    MOVE-CORRESPONDING LS_RESULT TO LS_PARTIAL_CHEQUE .
    APPEND LS_PARTIAL_CHEQUE TO LT_PARTIAL_CHEQUE .

  ENDLOOP.

  SORT:
    GT_RESULT BY BUKRS BELNR GJAHR BUZEI SEQ ,
    GT_OUTPUT BY BUKRS BELNR GJAHR BUZEI SEQ .

  PERFORM F_PARTIAL_CASE USING    LT_PARTIAL_CHEQUE
                         CHANGING GT_OUTPUT .


*-420000633 Beg of INS
  SORT LT_PARTIAL_MEMO BY BUKRS BELNR GJAHR BUZEI SEQ WORK_DATE .
  PERFORM F_PARTIAL_MEMO USING    LT_PARTIAL_MEMO
                                  UF_FILENAME
                                  UT_COLLECTOR
                         CHANGING GT_OUTPUT
                                  GT_RESULT.
*-420000633 End of INS
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_genc
*&---------------------------------------------------------------------*
*& Get Constants from table ZSDSCAC001
*&---------------------------------------------------------------------*
FORM F_GET_GENC .

  DATA:
    LT_GENC   TYPE ZCL_SDSCA_UTILITIES=>TT_GEN_C.

  DATA :
    LR_BANKN     TYPE RANGE OF ZSDSFIT029-BANKN,
    LR_ORDER_EXP TYPE RANGE OF ZSDSFIT029-AUFNR_EXPS.
*-420000463 Beg of INS
  DATA:
    LR_ORDER_FEE TYPE RANGE OF ZSDSFIT029-AUFNR_FEE .
*-420000463 End of INS
*    LF_SYSTEM TYPE CHAR3.
  ZCL_SDSCA_UTILITIES=>GET_GEN_C(
     EXPORTING
       IF_REPID  = SY-REPID
     IMPORTING
       ET_GEN_C  = LT_GENC
   ).

  LOOP AT LT_GENC ASSIGNING FIELD-SYMBOL(<L_GENC>).
    IF <L_GENC>-PARAM CS 'EMAIL_ERROR'.
      <L_GENC>-PARAM = 'EMAIL_ERROR'.
    ELSEIF <L_GENC>-PARAM CS 'EMAIL_SUCCESS'.
      <L_GENC>-PARAM = 'EMAIL_SUCCESS'.
    ENDIF.
    CASE <L_GENC>-PARAM.
      WHEN 'EMAIL_ERROR'.
        APPEND VALUE #( SIGN   = 'I'
                        OPTION = 'EQ'
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = '' )
                  TO S_EMAILE.

      WHEN 'EMAIL_SUCCESS'.
        APPEND VALUE #( SIGN   = 'I'
                        OPTION = 'EQ'
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = '' )
                  TO S_EMAILS.
      WHEN 'NAME_SIGN_EMAIL'.
        GF_NAME_SIGN_EMAIL = <L_GENC>-VALUE_LOW.

      WHEN 'ACTION_TYPE' .
        APPEND VALUE #( SIGN   = 'I'
                        OPTION = 'EQ'
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = '' )
                  TO GRT_ACTION_TYPE.
      WHEN 'STATUS' .
        APPEND VALUE #( SIGN   = 'I'
                        OPTION = 'EQ'
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = '' )
                  TO GRT_STATUS.
      WHEN 'PYMT_METHOD_BANK' .
        APPEND VALUE #( SIGN   = 'I'
                       OPTION = 'EQ'
                       LOW    = <L_GENC>-VALUE_LOW
                       HIGH   = '' )
                 TO GRT_BANK.
      WHEN 'PYMT_METHOD_PDC' .
        APPEND VALUE #( SIGN   = 'I'
                        OPTION = 'EQ'
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = '' )
                  TO GRT_PDC.

      WHEN 'DOCUMENT_ACTION_STATUS' .

        READ TABLE GT_ACTION_STATUS ASSIGNING FIELD-SYMBOL(<LS_ACTION_STATUS>)
                                    WITH KEY SEQ = <L_GENC>-SEQUENCE.
        IF SY-SUBRC EQ 0.
          IF <L_GENC>-PARAM_EXT = GC_GENC_PARAM_EXT-ACTION_TYPE.
            <LS_ACTION_STATUS>-ACTION_TYPE = <L_GENC>-VALUE_LOW.
          ELSEIF <L_GENC>-PARAM_EXT = GC_GENC_PARAM_EXT-STATUS.
            <LS_ACTION_STATUS>-STATUS = <L_GENC>-VALUE_LOW.
          ENDIF.
        ELSE.
          APPEND INITIAL LINE TO GT_ACTION_STATUS ASSIGNING <LS_ACTION_STATUS>.
          <LS_ACTION_STATUS>-SEQ = <L_GENC>-SEQUENCE.
          IF <L_GENC>-PARAM_EXT = GC_GENC_PARAM_EXT-ACTION_TYPE.
            <LS_ACTION_STATUS>-ACTION_TYPE = <L_GENC>-VALUE_LOW.
          ELSEIF <L_GENC>-PARAM_EXT = GC_GENC_PARAM_EXT-STATUS.
            <LS_ACTION_STATUS>-STATUS = <L_GENC>-VALUE_LOW.
          ENDIF.
        ENDIF.

      WHEN 'VALIDATE_ACTION_STATUS' .

        READ TABLE GT_VALIDATE_ACT ASSIGNING FIELD-SYMBOL(<LS_VALIDATE_ACT>)
                                    WITH KEY SEQ = <L_GENC>-SEQUENCE.
        IF SY-SUBRC EQ 0.
          IF <L_GENC>-PARAM_EXT = GC_GENC_PARAM_EXT-ACTION_TYPE.
            <LS_VALIDATE_ACT>-ACTION_TYPE = <L_GENC>-VALUE_LOW.
          ELSEIF <L_GENC>-PARAM_EXT  = GC_GENC_PARAM_EXT-STATUS.
            <LS_VALIDATE_ACT>-STATUS = <L_GENC>-VALUE_LOW.
          ENDIF.
        ELSE.
          APPEND INITIAL LINE TO GT_VALIDATE_ACT ASSIGNING <LS_VALIDATE_ACT>.
          <LS_VALIDATE_ACT>-SEQ = <L_GENC>-SEQUENCE.
          IF <L_GENC>-PARAM_EXT = GC_GENC_PARAM_EXT-ACTION_TYPE.
            <LS_VALIDATE_ACT>-ACTION_TYPE = <L_GENC>-VALUE_LOW.
          ELSEIF <L_GENC>-PARAM_EXT = GC_GENC_PARAM_EXT-STATUS.
            <LS_VALIDATE_ACT>-STATUS = <L_GENC>-VALUE_LOW.
          ENDIF.
        ENDIF.

      WHEN 'DEFAULT_PDC_BANK_ACCT' .

        APPEND VALUE #( SIGN   = 'I'
                        OPTION = 'EQ'
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = '' )
                  TO LR_BANKN.

      WHEN 'DEFAULT_ORDER_EXPENSE' .
        APPEND VALUE #( SIGN   = 'I'
                        OPTION = 'EQ'
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = '' )
                  TO LR_ORDER_EXP.
*-420000463 Beg of INS
      WHEN 'DEFAULT_ORDER_BANK_FEE' .
        APPEND VALUE #( SIGN   = 'I'
                        OPTION = 'EQ'
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = '' )
                  TO LR_ORDER_FEE.
*-420000463 End of INS
    ENDCASE.
  ENDLOOP.

  CLEAR GV_DF_PDC_BANK_ACCT .

  "Default bank and order expense
  IF LR_BANKN IS NOT INITIAL.
    GV_DF_PDC_BANK_ACCT = LR_BANKN[ 1 ]-LOW.
  ENDIF.


  IF LR_ORDER_EXP IS NOT INITIAL.
    GV_DF_ORDER_EXP = LR_ORDER_EXP[ 1 ]-LOW.
  ENDIF.
*-420000463 Beg of INS
  IF LR_ORDER_FEE IS NOT INITIAL.
    GV_DF_ORDER_FEE = LR_ORDER_FEE[ 1 ]-LOW.
  ENDIF.
*-420000463 End of INS
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_process_data
*&---------------------------------------------------------------------*
FORM F_PROCESS_DATA .
  PERFORM F_GET_FILE_FROM_DIRECTORY USING P_SDIR
                                 CHANGING GT_FILE_LIST.

  IF GT_FILE_LIST IS NOT INITIAL.
    PERFORM F_UPLOAD_FILE .
  ELSE.
*- Text-E11: Not found any file
    MESSAGE S000(38) WITH TEXT-E11 DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_write_file
*&---------------------------------------------------------------------*
FORM F_WRITE_FILE  USING  UT_DATA         TYPE TT_COLLECTOR
                          UF_PATHFILENAME TYPE RLGRAP-FILENAME
                          UF_FILENAME     TYPE EPS2FILNAM.

  DATA:
    LF_FILENAME_ARCHIVED TYPE EPS2FILNAM,
    LF_DATA              TYPE STRING.

  DATA: LF_WRBTR        TYPE CHAR20,
        LF_RECEIVED_AMT TYPE CHAR20,
        LF_DEDUCT_AMT   TYPE CHAR20,
        LF_INCOME_AMT   TYPE CHAR20,
        LF_WHT_AMT      TYPE CHAR20,
        LF_FEE          TYPE CHAR20,
        LF_BAL_AMT      TYPE CHAR20,
        LF_BILLPL_AMT   TYPE CHAR20,
        LF_RETENTION    TYPE CHAR20,
        LF_CASH_CON     TYPE CHAR20,
        LF_EXPS_AMT     TYPE CHAR20.

  LF_FILENAME_ARCHIVED =  UF_PATHFILENAME &&  UF_FILENAME ##NO_TEXT.

  OPEN DATASET LF_FILENAME_ARCHIVED FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

  LF_DATA = GF_HEADER .
  TRANSFER LF_DATA TO  LF_FILENAME_ARCHIVED.

  LOOP AT UT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>).
    WRITE:
       <L_DATA>-WRBTR        TO LF_WRBTR CURRENCY <L_DATA>-WAERS,
       <L_DATA>-RECEIVED_AMT TO LF_RECEIVED_AMT CURRENCY <L_DATA>-WAERS,
       <L_DATA>-DEDUCT_AMT   TO LF_DEDUCT_AMT CURRENCY <L_DATA>-WAERS,
       <L_DATA>-EXPS_AMT     TO LF_EXPS_AMT CURRENCY <L_DATA>-WAERS,
       <L_DATA>-INCOME_AMT   TO LF_INCOME_AMT CURRENCY <L_DATA>-WAERS,
       <L_DATA>-WHT_AMT      TO LF_WHT_AMT CURRENCY <L_DATA>-WAERS,
       <L_DATA>-FEE          TO LF_FEE CURRENCY <L_DATA>-WAERS,
       <L_DATA>-BAL_AMT      TO LF_BAL_AMT CURRENCY <L_DATA>-WAERS ,
       <L_DATA>-BILLPL_AMT   TO LF_BILLPL_AMT CURRENCY <L_DATA>-WAERS,
       <L_DATA>-RETENTION    TO LF_RETENTION CURRENCY <L_DATA>-WAERS,
       <L_DATA>-CASH_CON     TO LF_CASH_CON CURRENCY <L_DATA>-WAERS.

    CONDENSE: LF_WRBTR        NO-GAPS ,
              LF_RECEIVED_AMT NO-GAPS ,
              LF_DEDUCT_AMT   NO-GAPS ,
              LF_EXPS_AMT     NO-GAPS ,
              LF_INCOME_AMT   NO-GAPS ,
              LF_WHT_AMT      NO-GAPS ,
              LF_FEE          NO-GAPS ,
              LF_BAL_AMT      NO-GAPS ,
              LF_BILLPL_AMT   NO-GAPS ,
              LF_RETENTION    NO-GAPS ,
              LF_CASH_CON     NO-GAPS .

    CONCATENATE:
      <L_DATA>-BUKRS
      <L_DATA>-GJAHR
      <L_DATA>-PERNR
      <L_DATA>-PERNM
      <L_DATA>-WORK_DATE
      <L_DATA>-ACTION_TYPE
      <L_DATA>-STATUS
      <L_DATA>-KUNNR
      <L_DATA>-CNAMT
      <L_DATA>-CNAME
      <L_DATA>-BELNR
      <L_DATA>-VBELN
      <L_DATA>-XBLNR
      <L_DATA>-BLDAT
      <L_DATA>-FAEDT
      LF_WRBTR
      <L_DATA>-WAERS
      <L_DATA>-PAYMENT_DATE
      <L_DATA>-FOLLOW_DATE
      <L_DATA>-RECEIVED_DATE
      LF_RECEIVED_AMT
      LF_DEDUCT_AMT
      <L_DATA>-PYMT_METHOD
*      <L_DATA>-CHEQUE_DATE
      <L_DATA>-BANK_DATE
      <L_DATA>-CHEQUE_NO
      <L_DATA>-BANKL
      <L_DATA>-HBKID
      <L_DATA>-HKTID
      <L_DATA>-BANKK
      <L_DATA>-BANKN
      <L_DATA>-RECEIPT_NO
      <L_DATA>-REMARK
      LF_EXPS_AMT
      LF_INCOME_AMT
      LF_WHT_AMT
      LF_FEE
      LF_RETENTION
      LF_CASH_CON
      LF_BAL_AMT
      <L_DATA>-PAYNO
      <L_DATA>-BRNCH
      <L_DATA>-VTWEG
      <L_DATA>-VKBUR
      <L_DATA>-VKGRP
      <L_DATA>-PSPID
      <L_DATA>-BSTKD
      <L_DATA>-ZTERM
      <L_DATA>-PRCTR
      <L_DATA>-KTEXT
      <L_DATA>-TRANF_NO
      <L_DATA>-BILLPL_NO
      <L_DATA>-BANKK2
      <L_DATA>-BANK_ITEM
      LF_BILLPL_AMT
      <L_DATA>-DOCUMENT_STATUS
      <L_DATA>-INV_STATUS
      <L_DATA>-REMARK_VENDOR
      <L_DATA>-REASON_VENDOR
      <L_DATA>-BILLING_CYCLE
      <L_DATA>-COLLECTION_CYCLE
      <L_DATA>-AWB_NO
    INTO LF_DATA   SEPARATED BY '","' .

    CONCATENATE '"' LF_DATA  '"' INTO LF_DATA.

    TRANSFER LF_DATA TO  LF_FILENAME_ARCHIVED.
  ENDLOOP.

  CLOSE DATASET LF_FILENAME_ARCHIVED.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DISPLAY_RESULT
*&---------------------------------------------------------------------*
*& Display Processing Result
*&---------------------------------------------------------------------*
FORM F_DISPLAY_RESULT  USING  UT_RESULT TYPE TT_RESULT.

* Show progress
* Text-p99 : Generating ALV Report . . .
  MC_SHOW_PROGRESS 99 TEXT-P99.

* Set Container name
  GF_CONTAINER_1 = 'CTL_ALV_1'.
* Disable Header area
  GF_ALV_HEADER_1 = SPACE.
* Enable Soft Refresh only in display mode
  GF_SOFT_REFRESH_1 = GC_TRUE.
** No auto refresh in edit mode
*  GF_NO_AUTO_REFRESH_1 = GC_TRUE.

* ALV Layout
  PERFORM F_ALV_LAYOUT CHANGING GS_LAYOUT_1
                                GS_VARIANT_1
                                GS_PRINT_1.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_1.
  GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_1.
  ASSIGN UT_RESULT TO <G_LIST_1>.            "#EC CI_FLDEXT_OK[2610650]
* Build Field cat
  PERFORM F_ALV_BUILD_FIELDCAT CHANGING GT_FIELDCAT_1.
* Sort data
  PERFORM F_ALV_SORT_RESULT CHANGING GT_SORT_1.
* Call ALV Screen
  CALL SCREEN 9000.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ALV_BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT  CHANGING CT_FIELDCAT  TYPE LVC_T_FCAT.

  DATA:
    LT_FIELDCAT TYPE  LVC_T_FCAT,
    LS_FIELDCAT TYPE  LVC_S_FCAT,
    LF_COLPOS   TYPE  LVC_S_FCAT-COL_POS.

  FIELD-SYMBOLS:
    <L_FIELDCAT>   TYPE  LVC_S_FCAT.

  DEFINE MC_FIELDCAT.

    READ TABLE LT_FIELDCAT ASSIGNING <L_FIELDCAT>
      WITH KEY FIELDNAME = &1.
    IF SY-SUBRC EQ 0.
      LS_FIELDCAT =  <L_FIELDCAT>.
    ELSE.
      LS_FIELDCAT-FIELDNAME = &1.
      LS_FIELDCAT-DATATYPE = &3.
    ENDIF.

    CASE LS_FIELDCAT-DATATYPE.
      WHEN 'CURR'.
        LS_FIELDCAT-DO_SUM = GC_TRUE.
        LS_FIELDCAT-NO_ZERO = GC_TRUE.
    ENDCASE.

    IF &1 EQ 'ZSEL'.
      LS_FIELDCAT-CHECKBOX  = GC_TRUE.
      LS_FIELDCAT-EDIT   = GC_TRUE.
    ENDIF.

    IF &2 IS NOT INITIAL.
      LS_FIELDCAT-SCRTEXT_S = &2.
      LS_FIELDCAT-SCRTEXT_M = &2.
      LS_FIELDCAT-SCRTEXT_L = &2.
      LS_FIELDCAT-REPTEXT = &2.
      LS_FIELDCAT-COLTEXT = &2.
    ENDIF.

    IF &4 IS NOT INITIAL.
      LS_FIELDCAT-OUTPUTLEN = &4.
    ENDIF.

    LF_COLPOS = LF_COLPOS + 1.
    LS_FIELDCAT-COL_POS = LF_COLPOS.
    APPEND LS_FIELDCAT TO CT_FIELDCAT.
    CLEAR LS_FIELDCAT.

  END-OF-DEFINITION.

* Build Field cat from Structure.
*  CLEAR CT_FIELDCAT[].
*  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
*                              CHANGING CT_FIELDCAT.

  CLEAR :
    CT_FIELDCAT[],
    LT_FIELDCAT[].
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
                              CHANGING LT_FIELDCAT.

  MC_FIELDCAT :
    "FIELDNAME      "REPTEXT  "TYPE   "LEN
*                    "Select
*    'ZSEL'          TEXT-A01  ''      '5',
*                    "NO
*    'ZSEQ'          TEXT-A02  ''      '5',
                     "Status
    'STATU'          TEXT-A65  ''     '',
                     "Msgtxt
    'MSGTX'          TEXT-A66  ''     '100',
                     "File name
    'FILENAME'       TEXT-A75  ''     '100',
                     "Upload Date
    'UPLOAD_DATE'    TEXT-A76  ''     '30',
                     "Upload Time
    'UPLOAD_DATE'    TEXT-A77  ''     '30',
                    "Company
    'BUKRS'         TEXT-A34  ''      '4',
                    "Pers.No.
    'PERNR'         TEXT-A35  ''      '8',
                    "Collector Name
    'PERNM'         TEXT-A03  'CHAR'  '20',

    'WORK_DATE'     ''        ''      '',

    'ACTION_TYPE'   ''        ''      '8',
                    "STATUS
    'STATUS'        TEXT-A51  ''      '5',
                    "Customer Code
    'KUNNR'         TEXT-A04  ''      '10',
                    "Customer Name [TH]
    'CNAMT'         TEXT-A05  ''      '20',
                    "Customer Name [EN]
    'CNAME'         TEXT-A06  ''      '20',
                    "DocumentNo
    'BELNR'         TEXT-A07  ''      '',
                    "Fiscal Year
    'GJAHR'         TEXT-A36  ''      '',
                    "Sales Order No.
    'VBELN'         TEXT-A08  ''      '',
                    "Tax Invoice No.
    'XBLNR'         TEXT-A70  ''      '',
                    "Doc. Date
    'BLDAT'         TEXT-A52  ''      '',
                    "Net Due Date
    'FAEDT'         TEXT-A09  ''      '',
                    "Amount
    'WRBTR'         TEXT-A10  ''      '15',
                    "Currency
    'WAERS'         TEXT-A53  ''      '',
                    "Payment Date
    'PAYMENT_DATE'  TEXT-A54  ''      '',

    'FOLLOW_DATE'   ''        ''      '',

    'RECEIVED_DATE' ''        ''      '',

    'RECEIVED_AMT'  ''        ''      '15',
                    "Deduct Amount
    'DEDUCT_AMT'    TEXT-A55  ''      '15',
                    "Payment Method
    'PYMT_METHOD'   TEXT-A11  ''      '12',
                    "TF/CHQ. Date
*    'CHEQUE_DATE'   TEXT-A12  ''      '15',
    'BANK_DATE'     TEXT-A12  ''      '15',
                    "Cheque Number
    'CHEQUE_NO'     TEXT-A13   ''      '',
                    "Bank Name
    'BANKL'         TEXT-A56   ''      '',
                    "House Bank
    'HBKID'         TEXT-A57   ''      '',
                    "Account ID
    'HKTID'         TEXT-A58   ''      '',
                    "Bank Key
    'BANKK'        TEXT-A59   ''      '',

    'BANKN'         ''         ''      '',
                    "Receipt No.
    'RECEIPT_NO'    TEXT-A71   ''      '',

    'REMARK'        ''         ''      '20',
                    "Expense Amout
    'EXPS_AMT'      TEXT-A69   ''      '15',
                    "Income Amout
    'INCOME_AMT'    TEXT-A17   ''      '15',

    'WHT_AMT'       ''         ''      '15',

    'FEE'           ''         ''      '15',

    'RETENTION'     ''         ''      '15',
                    "PDC Contra Amount
    'CASH_CON'      TEXT-A61   ''      '15',

    'BAL_AMT'       ''         ''      '15',
                    "Pay-In No
    'PAYNO'         TEXT-A62   ''      '',

    'BRNCH'         ''         ''      '',
                    "Sale Person
*    'VTWEG'         TEXT-A20   'CHAR'  '2',
    'SPTXT'         TEXT-A20   'CHAR'  '2',
                    "Sale office
*    'VKBUR'         TEXT-A21   'CHAR'  '',
    'VKBUT'         TEXT-A21   'CHAR'  '',
                    "Sale group
*    'VKGRP'         TEXT-A22   'CHAR'  '',
    'VKGRT'         TEXT-A22   'CHAR'  '',
                    "Project Name
    'PSPID'         TEXT-A23   'CHAR'  '',
                    "PO. No.
    'BSTKD'         TEXT-A24   ''      '',
                    "Payment Term
    'ZTERM'         TEXT-A25   'CHAR'  '',
                    "Profit Center
    'PRCTR'         TEXT-A72   ''      '',
                    "Profit Center Description
    'KTEXT'         TEXT-A26   'CHAR'  '',
                    "Transfer No.
    'TRANF_NO'      TEXT-A27   ''      '10',

    'BILLPL_NO'     ''         ''      '',
                    "Mapping Bank
    'BANKK2'        TEXT-A32   ''      '',
                    "Mapping Bank item
    'BANK_ITEM'     TEXT-A33   ''      '',

    'BILLPL_AMT'    ''         ''      '15',
                     "Document Status
    'DOCUMENT_STATUS' TEXT-A64 ''      '10',

    'INV_STATUS'    ''         ''      '10',

    'REMARK_VENDOR' ''         ''      '20',

    'REASON_VENDOR' ''         ''      '20',
                       "Billing Cycle
    'BILLING_CYCLE'    TEXT-A67      ''   '20',
                       "Collection Cycle
    'COLLECTION_CYCLE' TEXT-A68      ''   '20',
                    "AWB no.
    'AWB_NO'        TEXT-A37   'CHAR'  '10'.

  LOOP AT LT_FIELDCAT ASSIGNING FIELD-SYMBOL(<F_FIELDCAT>) .
    CASE <F_FIELDCAT>-FIELDNAME  .
      WHEN 'ZSEL'  OR
           'ZSEQ'  .
        <L_FIELDCAT>-NO_OUT     = GC_TRUE.
    ENDCASE.
  ENDLOOP .


ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ALV_LAYOUT
*&---------------------------------------------------------------------*
*  ALV Layout for Report
*----------------------------------------------------------------------*
FORM F_ALV_LAYOUT  CHANGING CS_LAYOUT  TYPE  LVC_S_LAYO
                           CS_VARIANT TYPE  DISVARIANT
                           CS_PRINT   TYPE  LVC_S_PRNT.

* Initialize Output
  CLEAR:  CS_LAYOUT, CS_VARIANT, CS_PRINT.

* determine layout
  CS_LAYOUT-SEL_MODE   = 'A'.
  CS_LAYOUT-CWIDTH_OPT = GC_TRUE.
  CS_LAYOUT-ZEBRA      = SPACE.
  CS_LAYOUT-INFO_FNAME = 'LINECOLOR'.

* For Variant Saving
  CS_VARIANT-REPORT  = SY-REPID.
*  CS_VARIANT-VARIANT = P_VARI.
  CS_PRINT-NO_COLWOPT = GC_TRUE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ALV_SORT_RESULT
*&---------------------------------------------------------------------*
*  Maintain SORT data for Result ALV
*----------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT  CHANGING CT_SORT TYPE LVC_T_SORT.

* Initialize Output
  CLEAR: CT_SORT.


** Sort by CARRID
*  CLEAR LS_SORT.
*  LS_SORT-SPOS      = 1.
*  LS_SORT-FIELDNAME = LC_SORT1.
*  LS_SORT-UP        = GC_TRUE.
*  LS_SORT-SUBTOT    = SPACE.
*  APPEND LS_SORT TO CT_SORT.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_set_pathfile
*&---------------------------------------------------------------------*
FORM F_SET_PATHFILE  USING UF_INTFNO     TYPE ZSDSCAC004-INTFNO
                           UF_FILENAME_I TYPE TEXT20
                           UF_SUB        TYPE TEXT20
                  CHANGING CF_PATHNAME   TYPE RLGRAP-FILENAME .
  CONSTANTS:
    LC_INBOUND  TYPE  TEXT20 VALUE '10_INBOUND',
    LC_OUTBOUND TYPE  TEXT20 VALUE '20_OUTBOUND'.

  DATA:
    LF_FILENAME   TYPE STRING,  "Filename
    LF_FILENAME_I TYPE STRING.

* Get Interface Setting
  SELECT SINGLE *
    FROM ZSDSCAC004
   WHERE INTFNO   EQ @UF_INTFNO
     AND SYSID    EQ @SY-SYSID
     AND ZDEL_FLG EQ @SPACE
    INTO @DATA(LS_SETTING).
  IF SY-SUBRC NE 0.
    SELECT SINGLE *
      FROM ZSDSCAC004
     WHERE INTFNO   EQ @UF_INTFNO
       AND SYSID    EQ @SPACE
       AND ZDEL_FLG EQ @SPACE
      INTO @LS_SETTING.
  ENDIF.

  CASE LS_SETTING-INOUT_FLAG.
    WHEN 'I'.
      LF_FILENAME_I = LC_INBOUND.
    WHEN 'O'.
      LF_FILENAME_I = LC_OUTBOUND.
  ENDCASE.
  LF_FILENAME_I = UF_FILENAME_I      && '/' &&
                  LS_SETTING-SYSNAME && '/' &&
                  LS_SETTING-WRICEF  && '/' &&
                  UF_SUB && '/' .

  CALL FUNCTION 'FILE_GET_NAME_USING_PATH'
    EXPORTING
      LOGICAL_PATH               = LS_SETTING-PATHINTERN
      OPERATING_SYSTEM           = SY-OPSYS
*     PARAMETER_1                = IF_PARAM1
*     PARAMETER_2                = IF_PARAM2
*     PARAMETER_3                = IF_PARAM3
      FILE_NAME                  = LF_FILENAME_I
      ELEMINATE_BLANKS           = 'X'
    IMPORTING
      FILE_NAME_WITH_PATH        = LF_FILENAME
    EXCEPTIONS
      PATH_NOT_FOUND             = 1
      MISSING_PARAMETER          = 2
      OPERATING_SYSTEM_NOT_FOUND = 3
      FILE_SYSTEM_NOT_FOUND      = 4
      OTHERS                     = 5.
  IF SY-SUBRC EQ 0 .
    CF_PATHNAME = LF_FILENAME  .
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_mandatory
*&---------------------------------------------------------------------*
FORM F_VALIDATE_MANDATORY  CHANGING CS_RESULT TYPE TS_RESULT.

  "Company Code
  IF CS_RESULT-BUKRS IS INITIAL.
    PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '017' TEXT-A34 SPACE SPACE SPACE
                       CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
    RETURN.
  ENDIF.

  "Fiscal Year
  IF CS_RESULT-GJAHR IS INITIAL.
    PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '017' TEXT-A36 SPACE SPACE SPACE
                       CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
    RETURN.
  ENDIF.

  "Pers.No.
  IF CS_RESULT-PERNR IS INITIAL.
    PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '017' TEXT-A35 SPACE SPACE SPACE
                       CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
    RETURN.
  ENDIF.

  "Work Date
  IF CS_RESULT-WORK_DATE IS INITIAL.
    PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '017' TEXT-A38 SPACE SPACE SPACE
                       CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
    RETURN.
  ENDIF.

  "Action Type
  IF CS_RESULT-ACTION_TYPE IS INITIAL.
    PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '017' TEXT-A39 SPACE SPACE SPACE
                       CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
    RETURN.
  ENDIF.

  "STATUS
  IF CS_RESULT-STATUS IS INITIAL.
    PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '017' TEXT-E08 SPACE SPACE SPACE
                       CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
    RETURN.
  ENDIF.

  "DocumentNo
  IF CS_RESULT-BELNR IS INITIAL.
    PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '017' TEXT-A07 SPACE SPACE SPACE
                       CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
    RETURN.
  ENDIF.

  "Reference
  IF CS_RESULT-XBLNR IS INITIAL.
    PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '017' TEXT-A40 SPACE SPACE SPACE
                       CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
    RETURN.
  ENDIF.

  "Amount
*-420000439 Beg of INS
  IF CS_RESULT-ACTION_TYPE = GC_COLLECTION AND
     CS_RESULT-STATUS      = GC_RECEIVED  .
*-420000439 End of INS
    IF CS_RESULT-WRBTR IS INITIAL.
      PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '017' TEXT-A45 SPACE SPACE SPACE
                         CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
      RETURN.
    ENDIF.
*-420000439 Beg of INS
  ENDIF.
*-420000439 End of INS
  "Pmnt Date
  IF CS_RESULT-ACTION_TYPE = GC_BILLPLACE  .
    IF CS_RESULT-PAYMENT_DATE IS INITIAL.
      PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '017' TEXT-A46 SPACE SPACE SPACE
                         CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
      RETURN.
    ENDIF.
  ENDIF.

  "Deduct Amount
  IF CS_RESULT-ACTION_TYPE = GC_COLLECTION AND
     CS_RESULT-STATUS      = GC_RECEIVED  .


**-420000143 Beg of INS
*    READ TABLE GT_SUM_AMT ASSIGNING FIELD-SYMBOL(<L_SUM_AMT>)
*      WITH KEY DATA_TYPE = CS_RESULT-DATA_TYPE
*               BUKRS = CS_RESULT-BUKRS
*               BELNR = CS_RESULT-BELNR
*               GJAHR = CS_RESULT-GJAHR  .
*    IF <L_SUM_AMT> IS ASSIGNED .
*      IF <L_SUM_AMT>-CNTNO > 1 .
*
*        IF CS_RESULT-DEDUCT_AMT IS INITIAL.
*          PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '017' TEXT-A50 SPACE SPACE SPACE
*                             CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
*          RETURN.
*        ELSEIF CS_RESULT-DEDUCT_AMT > 0 AND CS_RESULT-DEDUCT_AMT > <L_SUM_AMT>-WRBTR.
*          "Text E01 : Original Invoice Amount Over Limit
*          PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '999' TEXT-E01 SPACE SPACE SPACE
*                             CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
*          RETURN.
*
*        ELSEIF CS_RESULT-DEDUCT_AMT < 0 AND CS_RESULT-DEDUCT_AMT < <L_SUM_AMT>-WRBTR.
*          "Text E02 : Original Invoice Amount Lower Limit
*          PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '999' TEXT-E02 SPACE SPACE SPACE
*                             CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
*          RETURN.
*        ENDIF.
*      ELSE.
**-420000143 End of INS
    IF CS_RESULT-DEDUCT_AMT IS INITIAL.
      PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '017' TEXT-A50 SPACE SPACE SPACE
                         CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
      RETURN.
    ELSEIF CS_RESULT-DEDUCT_AMT > 0 AND CS_RESULT-DEDUCT_AMT > CS_RESULT-WRBTR.
      "Text E01 : Original Invoice Amount Over Limit
      PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '999' TEXT-E01 SPACE SPACE SPACE
                         CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
      RETURN.

    ELSEIF CS_RESULT-DEDUCT_AMT < 0 AND CS_RESULT-DEDUCT_AMT < CS_RESULT-WRBTR.
      "Text E02 : Original Invoice Amount Lower Limit
      PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '999' TEXT-E02 SPACE SPACE SPACE
                         CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
      RETURN.

    ENDIF.

**-420000143 Beg of INS
*      ENDIF.
*    ENDIF.
**-420000143 End of INS

  ENDIF.

  IF CS_RESULT-PYMT_METHOD IS INITIAL.

  ELSEIF CS_RESULT-PYMT_METHOD IN GRT_BANK.  "Bank transfer
    READ TABLE GT_ACTION_STATUS INTO DATA(LS_ACTION_STATUS)
                                WITH KEY ACTION_TYPE = CS_RESULT-ACTION_TYPE
                                         STATUS      = CS_RESULT-STATUS.
    IF LS_ACTION_STATUS IS NOT INITIAL AND CS_RESULT-HBKID IS INITIAL.
      "HBKID : House Bank
      PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '017' TEXT-A57 SPACE SPACE SPACE
                 CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
      RETURN.
    ENDIF.

    IF LS_ACTION_STATUS IS NOT INITIAL AND CS_RESULT-HKTID IS INITIAL.
      "HKTID : ID for Account
      PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '017' TEXT-A60 SPACE SPACE SPACE
                 CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
      RETURN.

    ENDIF.

  ELSEIF CS_RESULT-PYMT_METHOD IN GRT_PDC. "PDC

    READ TABLE GT_ACTION_STATUS
      WITH KEY ACTION_TYPE = CS_RESULT-ACTION_TYPE
               STATUS      = CS_RESULT-STATUS
      TRANSPORTING NO FIELDS.
    IF SY-SUBRC EQ 0.

*      IF CS_RESULT-BANK_DATE IS INITIAL.
*
*        PERFORM F_ADD_MSG USING GC_MSG_TY-ERR_REQUIRED LV_ROW_ID 'BANK_DATE' '' '' CHANGING CREF_MSG.
*      ENDIF.

      IF CS_RESULT-CHEQUE_NO IS INITIAL.
        "Text-A62: Cheque No.
        PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '017' TEXT-A62 SPACE SPACE SPACE
                           CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
      ENDIF.

      IF CS_RESULT-BANKK IS INITIAL.
        "Text-A32: Mapping Bank
        PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '017' TEXT-A32 SPACE SPACE SPACE
                           CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
      ENDIF.

*      IF CS_RESULT-BANKN IS INITIAL.
*        "Text-A65 : Bank Account No.
*        PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '017' TEXT-A65 SPACE SPACE SPACE
*                           CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
*      ENDIF.
    ENDIF.

  ENDIF.

  "Follow up Date
*  IF CS_RESULT-ACTION_TYPE = GC_VENDOR OR
*     CS_RESULT-ACTION_TYPE = GC_OTHER  .
*    IF CS_RESULT-FOLLOW_DATE IS INITIAL.
*      PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '017' TEXT-A47 SPACE SPACE SPACE
*                         CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
*      RETURN.
*    ENDIF.
*  ENDIF.

  IF CS_RESULT-ACTION_TYPE = GC_COLLECTION AND
     CS_RESULT-STATUS      = GC_RECEIVED  .
    "Date received
    IF CS_RESULT-RECEIVED_DATE IS INITIAL.
      PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '017' TEXT-A48 SPACE SPACE SPACE
                         CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
      RETURN.
    ENDIF.

    "Received Amount
*    IF CS_RESULT-RECEIVED_AMT IS INITIAL.
*      PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '017' TEXT-A49 SPACE SPACE SPACE
*                         CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
*      RETURN.
*    ENDIF.

    "Deduct Amount
    IF CS_RESULT-DEDUCT_AMT IS INITIAL.
      PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '017' TEXT-A50 SPACE SPACE SPACE
                         CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
      RETURN.
    ENDIF.

    "Order for expense
    IF CS_RESULT-EXPS_AMT IS NOT INITIAL.
      IF CS_RESULT-AUFNR_EXPS IS INITIAL.

        PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '017' TEXT-A73 SPACE SPACE SPACE
                           CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
        RETURN.
      ENDIF.
    ENDIF.

    IF CS_RESULT-FEE IS NOT INITIAL.
      IF CS_RESULT-AUFNR_FEE IS INITIAL.
        PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '017' TEXT-A74 SPACE SPACE SPACE
                           CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
        RETURN.
      ENDIF.
    ENDIF.
  ENDIF.

  READ TABLE GT_VALIDATE_ACT WITH KEY ACTION_TYPE = CS_RESULT-ACTION_TYPE
                                      STATUS      = CS_RESULT-STATUS
                                      TRANSPORTING NO FIELDS.
  IF SY-SUBRC NE 0 .
    " Text-E20: Action type and Status not correct.
    PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '999' TEXT-E20 SPACE SPACE SPACE
                       CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
    RETURN.
  ENDIF.

*    IF CS_RESULT-exps_amt IS NOT INITIAL.
*      IF CS_RESULT-aufnr_exps IS INITIAL.
*        "Text-A63: Order Number for expense
*        PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '017' TEXT-A63 SPACE SPACE SPACE
*                           CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
*        RETURN.
*      ENDIF.
*    ENDIF.

*    IF CS_RESULT-fee IS NOT INITIAL.
*      IF CS_RESULT-aufnr_fee IS INITIAL.
*        "Text-A: Order Number for bank fee
*        PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '017' TEXT-A SPACE SPACE SPACE
*                           CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
*        RETURN.
*      ENDIF.
*    ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_set_message
*&---------------------------------------------------------------------*
FORM F_SET_MESSAGE  USING UF_TYPE    TYPE BAPI_MTYPE
                          UF_ID      TYPE SYMSGID
                          UF_NUMBER  TYPE SYMSGNO
                          UF_MSG1    TYPE ANY
                          UF_MSG2    TYPE ANY
                          UF_MSG3    TYPE ANY
                          UF_MSG4    TYPE ANY
                 CHANGING CF_STATUS  TYPE ICON_D
                          CF_MESSAGE TYPE MSGTX.

  MESSAGE ID UF_ID TYPE UF_TYPE NUMBER UF_NUMBER
          INTO CF_MESSAGE
          WITH UF_MSG1 UF_MSG2 UF_MSG3 UF_MSG4.

  CASE UF_TYPE.
    WHEN 'E'.
      CF_STATUS      = ICON_RED_LIGHT.
    WHEN 'W'.
      CF_STATUS      = ICON_YELLOW_LIGHT.
    WHEN 'S'.
      CF_STATUS      = ICON_GREEN_LIGHT.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_input
*&---------------------------------------------------------------------*
*& Validate input data in text file
*&---------------------------------------------------------------------*
FORM F_VALIDATE_INPUT  CHANGING CS_RESULT TYPE TS_RESULT.
  ##NEEDED
  DATA: LF_RECEIVED_AMT TYPE ZSDSFIS085-RECEIVED_AMT,
        LF_PAYIN_AMT    TYPE ZSDSFIS085-PAYIN_AMT.

**-420000143 Beg of INS
*  READ TABLE GT_SUM_AMT ASSIGNING FIELD-SYMBOL(<L_SUM_AMT>)
*    WITH KEY DATA_TYPE = CS_RESULT-DATA_TYPE
*             BUKRS = CS_RESULT-BUKRS
*             BELNR = CS_RESULT-BELNR
*             GJAHR = CS_RESULT-GJAHR  .
*  IF <L_SUM_AMT> IS ASSIGNED .
*    IF <L_SUM_AMT>-CNTNO > 1 .
*      IF  <L_SUM_AMT>-WRBTR > CS_RESULT-DEDUCT_AMT.
*        "Text-E17: Deduction Amount > Remain Amount
*        PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '999' TEXT-E17 SPACE SPACE SPACE
*                          CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
*        RETURN.
*      ENDIF.
*
*
*    ELSE.
**-420000143 End of INS
  IF CS_RESULT-REMAIN_AMT > 0 AND
     CS_RESULT-DEDUCT_AMT > CS_RESULT-REMAIN_AMT.
    "Text-E17: Deduction Amount > Remain Amount
    PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '999' TEXT-E17 SPACE SPACE SPACE
                      CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
    RETURN.
  ENDIF.

**-420000143 Beg of INS
*    ENDIF.
*  ENDIF.
**-420000143 End of INS

*-420000572 Beg of DEL
*  READ TABLE GT_ACTION_STATUS
*    WITH KEY ACTION_TYPE = CS_RESULT-ACTION_TYPE
*             STATUS      = CS_RESULT-STATUS
*    TRANSPORTING NO FIELDS.
*  IF SY-SUBRC EQ 0.
*-420000572 End of DEL
  DATA: LF_WHT_AMT  TYPE ZSDSFIS085-WHT_AMT,
        LF_EXPS_AMT TYPE ZSDSFIS085-EXPS_AMT,
        LF_FEE      TYPE ZSDSFIS085-FEE.
*    "Witholding tax amount
*    READ TABLE GT_WHTAX
*      INTO DATA(LS_WHTAX)
*      WITH KEY BUKRS = LS_RESULT-BUKRS
*               BELNR = LS_RESULT-BELNR
*               GJAHR = LS_RESULT-GJAHR
*               BUZEI = LS_RESULT-BUZEI.
*    IF SY-SUBRC EQ 0.
*      LS_RESULT-WHT_AMT = ( LS_RESULT-BAL_AMT * ( LS_WHTAX-QSATZ / LS_WHTAX-QPROZ ) ) * -1.
*    ENDIF.

  LF_EXPS_AMT = CS_RESULT-EXPS_AMT * -1.
  LF_WHT_AMT  = CS_RESULT-WHT_AMT * -1.
  LF_FEE      = CS_RESULT-FEE * -1.
  LF_RECEIVED_AMT =  CS_RESULT-BAL_AMT +
                     LF_EXPS_AMT +
                     LF_WHT_AMT +
                     LF_FEE.

*    IF LF_RECEIVED_AMT <> CS_RESULT-RECEIVED_AMT .
*      "Text-E07: Receive Amount not equal detail.
*      PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '999' TEXT-E07 SPACE SPACE SPACE
*                         CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
*      RETURN.
*    ELSE.
*-Beg of INS 09.04.2025
  CS_RESULT-RECEIVED_AMT = LF_RECEIVED_AMT .
*-End of INS 09.04.2025
  CS_RESULT-EXPS_AMT  = LF_EXPS_AMT.
  CS_RESULT-WHT_AMT   = LF_WHT_AMT .
  CS_RESULT-FEE       = LF_FEE  .
  CS_RESULT-PAYIN_AMT = CS_RESULT-RECEIVED_AMT +
                        CS_RESULT-RETENTION +
                        CS_RESULT-INCOME_AMT +
                        CS_RESULT-CASH_CON.

*    ENDIF.

*    IF CS_RESULT-PAYIN_AMT <> CS_RESULT-RECEIVED_AMT .
*      "Text-E07: Payin Amount not equal detail.
*      PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '999' TEXT-E07 SPACE SPACE SPACE
*                         CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
*      RETURN.
*    ENDIF.


*    LF_PAYIN_AMT = CS_RESULT-RECEIVED_AMT +
*                   CS_RESULT-RETENTION +
*                   CS_RESULT-INCOME_AMT +
*                   CS_RESULT-CASH_CON.
*    IF LF_PAYIN_AMT <> CS_RESULT-PAYIN_AMT .
*      "Text-E09: Payin Amount not equal detail.
*      PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '999' TEXT-E09 SPACE SPACE SPACE
*                         CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
*      RETURN.
*    ENDIF.
*-420000572 Beg of DEL
*  ENDIF.
*-420000572 End of DEL
  " For case action type 01  Status 01
*  IF CS_RESULT-BILLPL_AMT > 0  .
*    READ TABLE GT_SUM_BILLPL ASSIGNING FIELD-SYMBOL(<F_SUM_BILLPL>)
*                             WITH KEY BILLPL_NO = CS_RESULT-BILLPL_NO .
*    IF CS_RESULT-BILLPL_AMT NE <F_SUM_BILLPL>-BILLPL_AMT .
*      "Text-E17: Summary of Bill placement Amount not correct.
*      PERFORM F_SET_MESSAGE USING 'E' 'ZSDSFI01' '999' TEXT-E21 SPACE SPACE SPACE
*                        CHANGING CS_RESULT-STATU CS_RESULT-MSGTX.
*    ENDIF.
*
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_update_file_log
*&---------------------------------------------------------------------*
*& Update data in table
*&---------------------------------------------------------------------*
FORM F_UPDATE_FILE_LOG
       USING UT_DATA        TYPE TT_COLLECTOR
             UF_STATUS_DOC  TYPE ZSDSFIT046-STATUS_DOC_UPDATE
             UF_STATUS_FILE TYPE ZSDSFIT046-STATUS_FILE
             UF_FILENAME    TYPE EPS2FILNAM.

  DATA: LS_STATUS_FILE TYPE ZSDSFIT046,
        LT_STATUS_FILE TYPE TABLE OF ZSDSFIT046.

  SORT GT_ZSDSFIT046 BY BUKRS BELNR GJAHR BUZEI SEQ DESCENDING.

  LOOP AT UT_DATA ASSIGNING FIELD-SYMBOL(<F_DATA>) .
    "Get lastest file log data
    LOOP AT GT_ZSDSFIT046  ASSIGNING FIELD-SYMBOL(<F_LOG>)
      WHERE BUKRS = <F_DATA>-BUKRS
      AND   BELNR = <F_DATA>-BELNR
      AND   GJAHR = <F_DATA>-GJAHR
      AND   BUZEI = <F_DATA>-BUZEI
      AND   SEQ   = <F_DATA>-SEQ.
      EXIT.
    ENDLOOP.

    LS_STATUS_FILE = CORRESPONDING #( <F_DATA> ).

    IF <F_LOG> IS ASSIGNED.
      LS_STATUS_FILE-SEQ = <F_LOG>-SEQ + 1.
    ELSE.
      LS_STATUS_FILE-SEQ = 1.
    ENDIF.
    LS_STATUS_FILE-STATUS_DOC_UPDATE = UF_STATUS_DOC .
    LS_STATUS_FILE-STATUS_FILE = UF_STATUS_FILE.
    LS_STATUS_FILE-FILENAME    = UF_FILENAME .
    LS_STATUS_FILE-ERNAM = SY-UNAME.
    LS_STATUS_FILE-ERDAT = GF_DATE.
    LS_STATUS_FILE-ERZMT = GF_TIME.

    APPEND LS_STATUS_FILE TO LT_STATUS_FILE.
  ENDLOOP .

  IF LT_STATUS_FILE IS NOT INITIAL .
    MODIFY ZSDSFIT046 FROM TABLE LT_STATUS_FILE .
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_Set_file_value
*&---------------------------------------------------------------------*
FORM F_SET_FILE_VALUE  USING UF_FILE_DATA TYPE ANY
                    CHANGING CF_DATA TYPE ANY.

  IF UF_FILE_DATA IS NOT INITIAL .
    CF_DATA = UF_FILE_DATA .
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_AUTHORIZE_CHECK
*&---------------------------------------------------------------------*
*& Check Authorization on t-code
*&---------------------------------------------------------------------*
FORM F_AUTHORIZE_CHECK  USING UF_TCODE  TYPE  SY-TCODE.

  AUTHORITY-CHECK OBJECT 'S_TCODE'     "Transaction Code Check
  ID 'TCD' FIELD UF_TCODE.

  IF SY-SUBRC <> 0.
*   Error You are not authorized to use transaction &
    MESSAGE S172(00) WITH UF_TCODE.
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_partial_case
*&---------------------------------------------------------------------*
FORM F_PARTIAL_CASE  USING  UT_PARTIAL_CHEQUE TYPE TT_PARTIAL_CHEQUE
                   CHANGING CT_OUTPUT         TYPE TT_OUTPUT.

  DATA:
    LT_CLR_LINK TYPE STANDARD TABLE OF ZSDSFIT037,

    LT_OUTPUT   TYPE TT_OUTPUT,
    LT_PARTIAL  TYPE TT_PARTIAL.


  DATA:
    LF_READONLY TYPE ABAP_BOOL,
    LF_PARTIAL  TYPE ABAP_BOOL,
    LF_SEQ      TYPE ZSDSFIT029-SEQ.


  LT_OUTPUT[] =  CT_OUTPUT[] .

  IF LT_OUTPUT IS NOT INITIAL.
    "Clearing link list
    SELECT *
      INTO TABLE @LT_CLR_LINK
      FROM ZSDSFIT037
      FOR ALL ENTRIES IN @LT_OUTPUT
      WHERE BUKRS = @LT_OUTPUT-BUKRS
      AND   BELNR = @LT_OUTPUT-BELNR
      AND   GJAHR = @LT_OUTPUT-GJAHR
      AND   BUZEI = @LT_OUTPUT-BUZEI .
*      AND   SEQ   = @LT_OUTPUT-SEQ.
    IF SY-SUBRC = 0 .
      SORT LT_CLR_LINK BY BUKRS BELNR GJAHR BUZEI SEQ .
    ENDIF.

    SELECT
      FROM BSID_VIEW AS ITM
      INNER JOIN BKPF AS HDR
      ON  HDR~BUKRS = ITM~BUKRS
      AND HDR~BELNR = ITM~BELNR
      AND HDR~GJAHR = ITM~GJAHR
      FIELDS
        ITM~BUKRS,
        ITM~BELNR,
        ITM~GJAHR,
        ITM~BUZEI,
        ITM~WAERS,
        ITM~DMBTR,
        ITM~WRBTR,
        ITM~REBZG,
        ITM~REBZJ,
        ITM~REBZZ,
        HDR~XREF2_HD
      FOR ALL ENTRIES IN @LT_OUTPUT
      WHERE ITM~BUKRS = @LT_OUTPUT-BUKRS
      AND   ITM~REBZG = @LT_OUTPUT-BELNR
      AND   ITM~REBZJ = @LT_OUTPUT-GJAHR
      AND   ITM~REBZZ = @LT_OUTPUT-BUZEI
      INTO TABLE @LT_PARTIAL.
    IF SY-SUBRC = 0 .
      SORT LT_PARTIAL BY BUKRS BELNR GJAHR BUZEI .
    ENDIF.
  ENDIF.

  LOOP AT CT_OUTPUT ASSIGNING FIELD-SYMBOL(<LS_OUTPUT>) WHERE ACTION_TYPE = GC_COLLECTION
                                                          AND STATUS      = GC_RECEIVED  .

    CLEAR:
      LF_READONLY,
      LF_PARTIAL.

    <LS_OUTPUT>-FULL_NAME = VALUE #( GT_FULLNAME[ PERNR = <LS_OUTPUT>-PERNR ]-FULL_NAME OPTIONAL ).

    LOOP AT GT_CLR_LINK INTO DATA(LS_LIST)      ##INTO_OK
       WHERE BUKRS = <LS_OUTPUT>-BUKRS
       AND   BELNR = <LS_OUTPUT>-BELNR
       AND   GJAHR = <LS_OUTPUT>-GJAHR
       AND   BUZEI = <LS_OUTPUT>-BUZEI
       AND   SEQ   = <LS_OUTPUT>-SEQ.

      APPEND INITIAL LINE TO <LS_OUTPUT>-BILLPL_LIST ASSIGNING FIELD-SYMBOL(<LS_BILLPL_LIST>).
      <LS_BILLPL_LIST>-TRANF_NO     = <LS_OUTPUT>-TRANF_NO.
      <LS_BILLPL_LIST>-BILLPL_NO    = LS_LIST-BILLPL_NO.
      <LS_BILLPL_LIST>-BILLPL_DATE  = LS_LIST-BILLPL_DATE.
      <LS_BILLPL_LIST>-WRBTR        = LS_LIST-WRBTR.
      <LS_BILLPL_LIST>-BELNR        = LS_LIST-BELNR_CLR.
      <LS_BILLPL_LIST>-GJAHR        = LS_LIST-GJAHR_CLR.
      <LS_BILLPL_LIST>-RECEIVED     = GC_TRUE.

      LF_PARTIAL = GC_TRUE.

      DELETE LT_PARTIAL WHERE BUKRS = LS_LIST-BUKRS
                        AND   BELNR = LS_LIST-BELNR_CLR
                        AND   GJAHR = LS_LIST-GJAHR_CLR.

      "Already received - do not change data
      LF_READONLY = GC_TRUE.

    ENDLOOP.

*   Check with case partial refer perform f_prepare_change_output
*   progrm ZSDSFIR0250_F00
    LOOP AT LT_PARTIAL INTO DATA(LS_PARTIAL)    ##INTO_OK
      WHERE BUKRS = <LS_OUTPUT>-BUKRS
      AND   REBZG = <LS_OUTPUT>-BELNR
      AND   REBZJ = <LS_OUTPUT>-GJAHR
      AND   REBZZ = <LS_OUTPUT>-BUZEI.

      DATA(LF_TABIX) = SY-TABIX.
      LF_PARTIAL = GC_TRUE.

      APPEND INITIAL LINE TO <LS_OUTPUT>-BILLPL_LIST ASSIGNING <LS_BILLPL_LIST>.
      <LS_BILLPL_LIST>-BELNR    = LS_PARTIAL-BELNR.
      <LS_BILLPL_LIST>-GJAHR    = LS_PARTIAL-GJAHR.
      <LS_BILLPL_LIST>-WRBTR    = LS_PARTIAL-WRBTR.
      <LS_BILLPL_LIST>-RECEIVED = GC_TRUE.

      <LS_OUTPUT>-REMAIN_AMT -= LS_PARTIAL-WRBTR.
      <LS_OUTPUT>-TOTAL_BIL  += LS_PARTIAL-WRBTR.

      DELETE LT_PARTIAL INDEX LF_TABIX.
    ENDLOOP.

    <LS_OUTPUT>-REMAIN_C_AMT = <LS_OUTPUT>-REMAIN_AMT - <LS_OUTPUT>-BAL_AMT.

*    "Check is editable
    IF LF_PARTIAL = GC_TRUE. "New partial found
      LF_READONLY = GC_TRUE.
    ENDIF.

    IF LF_READONLY = GC_TRUE.


    ELSE.

      LOOP AT UT_PARTIAL_CHEQUE ASSIGNING FIELD-SYMBOL(<L_PARTIAL_CHEQUE>)
                                    WHERE BUKRS = <LS_OUTPUT>-BUKRS
                                      AND BELNR = <LS_OUTPUT>-BELNR
                                      AND GJAHR = <LS_OUTPUT>-GJAHR
                                      AND BUZEI = <LS_OUTPUT>-BUZEI
                                      AND SEQ   = <LS_OUTPUT>-SEQ
                                      AND CHEQUE_NO NE <LS_OUTPUT>-CHEQUE_NO .
        IF LF_SEQ IS INITIAL .
          LF_SEQ = <LS_OUTPUT>-SEQ .
        ELSE.
          LF_SEQ = <LS_OUTPUT>-SEQ + 1 .
          <LS_OUTPUT>-SEQ = LF_SEQ .
        ENDIF.



      ENDLOOP.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_PARTIAL_memo
*&---------------------------------------------------------------------*
FORM F_PARTIAL_MEMO  USING  UT_PARTIAL_MEMO TYPE TT_PARTIAL_MEMO
                            UF_FILENAME  TYPE  EPS2FILNAM
                            UT_COLLECTOR TYPE  TT_COLLECTOR
                   CHANGING CT_OUTPUT    TYPE  TT_OUTPUT
                            CT_RESULT    TYPE  TT_RESULT.

  DATA:
    LT_OUTPUT  TYPE TT_OUTPUT,
    LT_PARTIAL TYPE TT_PARTIAL_MEMO.


  DATA:
    LF_READONLY TYPE ABAP_BOOL,
    LF_PARTIAL  TYPE ABAP_BOOL,
    LF_SEQ      TYPE ZSDSFIT029-SEQ,
    LF_WRBTR    TYPE BSEG-WRBTR.

  LOOP AT UT_PARTIAL_MEMO INTO DATA(LS_PARTIAL)
    GROUP BY   ( BUKRS     = LS_PARTIAL-BUKRS
                 BELNR     = LS_PARTIAL-BELNR
                 GJAHR     = LS_PARTIAL-GJAHR
                 BUZEI     = LS_PARTIAL-BUZEI
                 WORK_DATE = LS_PARTIAL-WORK_DATE )
      ASCENDING
      REFERENCE INTO DATA(GROUP_PARTIAL) ##INTO_OK.

    LOOP AT GROUP GROUP_PARTIAL ASSIGNING FIELD-SYMBOL(<LFS_PARTIAL>).
      MOVE-CORRESPONDING <LFS_PARTIAL> TO LS_PARTIAL .
      LF_WRBTR = LF_WRBTR + <LFS_PARTIAL>-WRBTR .
    ENDLOOP .

    LS_PARTIAL-WRBTR = LF_WRBTR .
    APPEND LS_PARTIAL TO LT_PARTIAL .
    CLEAR LF_WRBTR.
  ENDLOOP.

  "Get Amount
  IF LT_PARTIAL IS NOT INITIAL.
    SELECT
      FROM BSID_VIEW AS ITM
      INNER JOIN BKPF AS HDR
      ON  HDR~BUKRS = ITM~BUKRS
      AND HDR~BELNR = ITM~BELNR
      AND HDR~GJAHR = ITM~GJAHR
      FIELDS
        ITM~BUKRS,
        ITM~BELNR,
        ITM~GJAHR,
        ITM~BUZEI,
        ITM~WAERS,
        ITM~DMBTR,
        ITM~WRBTR
      FOR ALL ENTRIES IN @LT_PARTIAL
      WHERE ITM~BUKRS = @LT_PARTIAL-BUKRS
      AND   ITM~BELNR = @LT_PARTIAL-BELNR
      AND   ITM~GJAHR = @LT_PARTIAL-GJAHR
      AND   ITM~BUZEI = @LT_PARTIAL-BUZEI
      INTO TABLE @DATA(LT_FIDOC).
    IF SY-SUBRC = 0 .
      SORT LT_FIDOC BY BUKRS BELNR GJAHR BUZEI .
    ENDIF.
  ENDIF.

  LOOP AT LT_PARTIAL ASSIGNING FIELD-SYMBOL(<LS_PARTIAL>).
    "Validate amount in FIDOC
    READ TABLE LT_FIDOC ASSIGNING FIELD-SYMBOL(<LS_FIDOC>)
    WITH KEY BUKRS =  <LS_PARTIAL>-BUKRS
             BELNR =  <LS_PARTIAL>-BELNR
             GJAHR = <LS_PARTIAL>-GJAHR
             BUZEI = <LS_PARTIAL>-BUZEI.
    IF SY-SUBRC = 0 .
      CLEAR LF_SEQ .
      LOOP AT CT_OUTPUT ASSIGNING FIELD-SYMBOL(<LS_OUTPUT>)
                                         WHERE BUKRS = <LS_PARTIAL>-BUKRS
                                           AND BELNR = <LS_PARTIAL>-BELNR
                                           AND GJAHR = <LS_PARTIAL>-GJAHR
                                           AND BUZEI = <LS_PARTIAL>-BUZEI.

        <LS_OUTPUT>-DATA_TYPE =  'M' .

        IF LF_SEQ IS INITIAL .
          LF_SEQ = <LS_OUTPUT>-SEQ .
        ELSE.
          LF_SEQ = <LS_OUTPUT>-SEQ + 1 .
          <LS_OUTPUT>-SEQ = LF_SEQ .
        ENDIF.

        "Validate Original Invoice Amount <> summary amount upload file.
        IF <LS_FIDOC>-WRBTR <> LS_PARTIAL-WRBTR .
          CLEAR <LS_OUTPUT>-SEL .

          LOOP AT CT_RESULT ASSIGNING FIELD-SYMBOL(<LS_RESULT>)
                                             WHERE BUKRS = <LS_PARTIAL>-BUKRS
                                               AND BELNR = <LS_PARTIAL>-BELNR
                                               AND GJAHR = <LS_PARTIAL>-GJAHR
                                               AND BUZEI = <LS_PARTIAL>-BUZEI.

            "Text E22 : Original Invoice Amount <> summary amount upload file.
            PERFORM F_SET_MESSAGE USING 'W' 'ZSDSFI01' '999' TEXT-E22 SPACE SPACE SPACE
                               CHANGING <LS_RESULT>-STATU <LS_RESULT>-MSGTX.

          ENDLOOP.
        ENDIF.
      ENDLOOP .
    ENDIF.
  ENDLOOP.

ENDFORM.
