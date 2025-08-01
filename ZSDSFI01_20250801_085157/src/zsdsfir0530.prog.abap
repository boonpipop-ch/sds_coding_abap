*&---------------------------------------------------------------------*
*& Report ZSDSFIR0530
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSDSFIR0530.
*---------------------------------------------------------*
*                       TABLE                        *
*---------------------------------------------------------*
TABLES: BNKA,ZSDSFIT042.
*---------------------------------------------------------*
*                       CONSTANTS                         *
*---------------------------------------------------------*
CONSTANTS: GC_SAVE(1)   TYPE C      VALUE 'A',
           GC_EQTYP     TYPE EQTYP  VALUE 'M'.

************************************************************************
*      D E C L A R E  T A B L E & S T R U C T  U R E & V A R I A B L E *
************************************************************************
TYPE-POOLS: SLIS,VRM,TRUXS.
TYPES: BEGIN OF TY_ZSDSFIT042,
          MANDT             TYPE  ZSDSFIT042-MANDT,    "CLIENT
          HBKID             TYPE  ZSDSFIT042-HBKID,    "BANK KEYS
          ZBANK_ITEM        TYPE  ZSDSFIT042-ZBANK_ITEM,    "STATEMENT BANK ITEM
          CHEQUE_DATE       TYPE  ZSDSFIT042-CHEQUE_DATE,    "DATE FOR PAYMENT BY CHEQUE
          DEBIT_AMT         TYPE  ZSDSFIT042-DEBIT_AMT,    "AMOUNT
          RECEIVED_AMOUNT   TYPE  ZSDSFIT042-RECEIVED_AMOUNT,    "AMOUNT
          FLAG_BANK_FEE     TYPE  ZSDSFIT042-FLAG_BANK_FEE,    "FLAG BANK FEE
          LINE_MAP_BANK_FE  TYPE  ZSDSFIT042-LINE_MAP_BANK_FE,    "STATEMENT BANK ITEM
          BANK_DESC         TYPE  ZSDSFIT042-BANK_DESC,    "BANK DESCRIPTION
          BAL_AMT           TYPE  ZSDSFIT042-BAL_AMT,    "AMOUNT
          CHANNEL           TYPE  ZSDSFIT042-CHANNEL,    "CHANNEL
          BRANCH_CODE       TYPE  ZSDSFIT042-BRANCH_CODE,    "BRANCH CODE
          BRANCH_NAME       TYPE  ZSDSFIT042-BRANCH_NAME,    "BRANCH NAME
          TERMINAL_ID       TYPE  ZSDSFIT042-TERMINAL_ID,    "TERMINAL ID
          CHEQUE_NO         TYPE  ZSDSFIT042-CHEQUE_NO,    "CHEQUE NO
          TRAN_CODE         TYPE  ZSDSFIT042-TRAN_CODE,    "TRAN CODE
          SENDER_BANK       TYPE  ZSDSFIT042-SENDER_BANK,    "SENDER BANK
          SENDER_BRANCH     TYPE  ZSDSFIT042-SENDER_BRANCH,    "SENDER BRANCH
          SENDER_ACC_NO     TYPE  ZSDSFIT042-SENDER_ACC_NO,    "SENDER ACCOUNT NUMBER
          SENDER_ACC_NAME   TYPE  ZSDSFIT042-SENDER_ACC_NAME,    "SENDER ACCOUNT NAME
          SDS_ACC_NO        TYPE  ZSDSFIT042-SDS_ACC_NO,    "SDS ACCOUNT NO
          TRAN_DATE         TYPE  ZSDSFIT042-TRAN_DATE,    "TRANSECTION DATE OR BOOKING DATE
          TRAN_DATE_TO      TYPE  ZSDSFIT042-TRAN_DATE_TO,    "TRANSECTION DATE TO OR BOOKING DATE TO
          SMBC_REMARK       TYPE  ZSDSFIT042-SMBC_REMARK,    "SMBC REMARK
          SMBC_BAL_AMT      TYPE  ZSDSFIT042-SMBC_BAL_AMT,    "AMOUNT
          RECORD_TYPE       TYPE  ZSDSFIT042-RECORD_TYPE,    "RECORD TYPE
          SWIFT_BIC         TYPE  ZSDSFIT042-SWIFT_BIC,    "SWIFT BIC
          ACC_TYPE          TYPE  ZSDSFIT042-ACC_TYPE,    "ACCOUNT TYPE
          CURRENCY          TYPE  ZSDSFIT042-CURRENCY,    "CURRENCY
          ACC_NAME          TYPE  ZSDSFIT042-ACC_NAME,    "ACCOUNT NAME
          TRAN_TYPE_NAME    TYPE  ZSDSFIT042-TRAN_TYPE_NAME,    "TRANS TYPE NAME
          CUST_REFER        TYPE  ZSDSFIT042-CUST_REFER,    "CUST REFER
          BANK_REFER        TYPE  ZSDSFIT042-BANK_REFER,    "BANK REFER
          ERDAT             TYPE  ZSDSFIT042-ERDAT,    "DATE ON WHICH RECORD WAS CREATED
          ERZET             TYPE  ZSDSFIT042-ERZET,    "ENTRY TIME
          USNAM             TYPE  ZSDSFIT042-USNAM,    "USER NAME
          TRANF             TYPE  ZSDSFIT042-TRANF,    "TRANSFER NO.
          RUN_ID            TYPE  ZSDSFIT042-RUN_ID,    "RUNNING ID
          PAYIN             TYPE  ZSDSFIT042-PAYIN,    "PAY IN
          MAP_STATUS        TYPE  ZSDSFIT042-MAP_STATUS,    "MAPPING STATUS WITH COLLECTOR
          MAP_DATE          TYPE  ZSDSFIT042-MAP_DATE,    "MAPPING DATE WITH COLLECTOR
          MAP_TIME          TYPE  ZSDSFIT042-MAP_TIME,    "MAPPING TIME WITH COLLECTOR
          STATUS_LOCK       TYPE  ZSDSFIT042-STATUS_LOCK,    "STATUS LOCK DATA
          LOCK_ERDAT        TYPE  ZSDSFIT042-LOCK_ERDAT,    "DATE ON WHICH RECORD WAS CREATED
          LOCK_ERZET        TYPE  ZSDSFIT042-LOCK_ERZET,    "ENTRY TIME
          LOCK_USNAM        TYPE  ZSDSFIT042-LOCK_USNAM,    "USER NAME
          DELETE_FLAG       TYPE  ZSDSFIT042-DELETE_FLAG,    "DELETED FLAG
          DEL_ERDAT         TYPE  ZSDSFIT042-DEL_ERDAT,    "DATE ON WHICH RECORD WAS CREATED
          DEL_ERZET         TYPE  ZSDSFIT042-DEL_ERZET,    "ENTRY TIME
          DEL_USNAM         TYPE  ZSDSFIT042-DEL_USNAM,    "USER NAME
          CUST_NAME         TYPE  ZSDSFIT042-CUST_NAME,    "CUSTOMER NAME
          B_VALUE_TIME      TYPE  ZSDSFIT042-B_VALUE_TIME,    "BANK VALUE TIME
          MAP_USNAM         TYPE  ZSDSFIT042-MAP_USNAM,    "USER NAME
          RESERVE_MAP       TYPE  ZSDSFIT042-RESERVE_MAP,    "STATUS RESERVE BY USER
          RESERVE_DATE      TYPE  ZSDSFIT042-RESERVE_DATE,    "DATE ON WHICH RECORD WAS CREATED
          RESERVE_TIME      TYPE  ZSDSFIT042-RESERVE_TIME,    "ENTRY TIME
          RESERVE_USNAM     TYPE  ZSDSFIT042-RESERVE_USNAM,    "USER NAME
          RESERVE_DESC      TYPE  ZSDSFIT042-RESERVE_DESC,    "RESEVE DETAIL
          FI_CONFIRM_MAP    TYPE  ZSDSFIT042-FI_CONFIRM_MAP,    "FI CONFRIM
          FI_CONFIRM_DATE   TYPE  ZSDSFIT042-FI_CONFIRM_DATE,    "DATE ON WHICH RECORD WAS CREATED
          FI_CONFIRM_TIME   TYPE  ZSDSFIT042-FI_CONFIRM_TIME,    "ENTRY TIME
          FI_CONFIRM_USNAM  TYPE  ZSDSFIT042-FI_CONFIRM_USNAM,    "USER NAME
          STATUS_AD         TYPE  ZSDSFIT042-STATUS_AD,    "STATUS CLEAR EXPERIMENT
          STATUS_ACC_RECON     TYPE  ZSDSFIT042-STATUS_ACC_RECON,    "STATUS ACCOUNT RECONCILE
          RECON_DATE        TYPE  ZSDSFIT042-RECON_DATE,    "DATE ON WHICH RECORD WAS CREATED
          RECON_TIME        TYPE  ZSDSFIT042-RECON_TIME,    "ENTRY TIME
          RECON_USER        TYPE  ZSDSFIT042-RECON_USER,    "USER NAME
          KBANK_DETAIL      TYPE  ZSDSFIT042-KBANK_DETAIL,    "KBANK_DETAIL  "CH2  ADD BY WANTANEE 20210610
          TRNFER_NUMBER     TYPE  ZSDSFIT042-TRNFER_NUMBER,    "TRNFER_NUMBER
          FYEAR_TRNFERNO    TYPE  ZSDSFIT042-FYEAR_TRNFERNO,    "FYEAR_TRNFERNO
          FI_CLEARING_NO    TYPE  ZSDSFIT042-FI_CLEARING_NO,    "FI_CLEARING_NO
          FYEAR_CLEARING    TYPE  ZSDSFIT042-FYEAR_CLEARING,    "FYEAR_CLEARING
          DATE_CLEAR        TYPE  ZSDSFIT042-DATE_CLEAR,    "DATE_CLEAR
          TIME_CLEAR        TYPE  ZSDSFIT042-TIME_CLEAR,    "TIME_CLEAR
          MT940_DOC         TYPE  ZSDSFIT042-MT940_DOC,    "MT940_DOC
          FYEAR_MT940       TYPE  ZSDSFIT042-FYEAR_MT940,    "FYEAR_MT940
          CLEARING_USER     TYPE  ZSDSFIT042-CLEARING_USER,    "CLEARING_USER
        END OF TY_ZSDSFIT042.


TYPES: BEGIN OF TYP_OUTPUT,
          HBKID             TYPE  ZSDSFIT042-HBKID,    "BANK KEYS
          ZBANK_ITEM        TYPE  ZSDSFIT042-ZBANK_ITEM,    "STATEMENT BANK ITEM
          CHEQUE_DATE       TYPE  ZSDSFIT042-CHEQUE_DATE,    "DATE FOR PAYMENT BY CHEQUE
          DEBIT_AMT         TYPE  ZSDSFIT042-DEBIT_AMT,    "AMOUNT
          RECEIVED_AMOUNT   TYPE  ZSDSFIT042-RECEIVED_AMOUNT,    "AMOUNT
          FLAG_BANK_FEE     TYPE  ZSDSFIT042-FLAG_BANK_FEE,    "FLAG BANK FEE
          LINE_MAP_BANK_FE  TYPE  ZSDSFIT042-LINE_MAP_BANK_FE,    "STATEMENT BANK ITEM
*          BANK_FEE          TYPE  ZSDSFIT042-FEE,    "AMOUNT
          BANK_DESC         TYPE  ZSDSFIT042-BANK_DESC,    "BANK DESCRIPTION
          BAL_AMT           TYPE  ZSDSFIT042-BAL_AMT,    "AMOUNT
          CHANNEL           TYPE  ZSDSFIT042-CHANNEL,    "CHANNEL
          BRANCH_CODE       TYPE  ZSDSFIT042-BRANCH_CODE,    "BRANCH CODE
          BRANCH_NAME       TYPE  ZSDSFIT042-BRANCH_NAME,    "BRANCH NAME
          TERMINAL_ID       TYPE  ZSDSFIT042-TERMINAL_ID,    "TERMINAL ID
          CHEQUE_NO         TYPE  ZSDSFIT042-CHEQUE_NO,    "CHEQUE NO
          TRAN_CODE         TYPE  ZSDSFIT042-TRAN_CODE,    "TRAN CODE
          SENDER_BANK       TYPE  ZSDSFIT042-SENDER_BANK,    "SENDER BANK
          SENDER_BRANCH     TYPE  ZSDSFIT042-SENDER_BRANCH,    "SENDER BRANCH
          SENDER_ACC_NO     TYPE  ZSDSFIT042-SENDER_ACC_NO,    "SENDER ACCOUNT NUMBER
          SENDER_ACC_NAME   TYPE  ZSDSFIT042-SENDER_ACC_NAME,    "SENDER ACCOUNT NAME
          SDS_ACC_NO        TYPE  ZSDSFIT042-SDS_ACC_NO,    "SDS ACCOUNT NO
          TRAN_DATE         TYPE  ZSDSFIT042-TRAN_DATE,    "TRANSECTION DATE OR BOOKING DATE
          TRAN_DATE_TO      TYPE  ZSDSFIT042-TRAN_DATE_TO,    "TRANSECTION DATE TO OR BOOKING DATE TO
          SMBC_REMARK       TYPE  ZSDSFIT042-SMBC_REMARK,    "SMBC REMARK
          SMBC_BAL_AMT      TYPE  ZSDSFIT042-SMBC_BAL_AMT,    "AMOUNT
          RECORD_TYPE       TYPE  ZSDSFIT042-RECORD_TYPE,    "RECORD TYPE
          SWIFT_BIC         TYPE  ZSDSFIT042-SWIFT_BIC,    "SWIFT BIC
          ACC_TYPE          TYPE  ZSDSFIT042-ACC_TYPE,    "ACCOUNT TYPE
          CURRENCY          TYPE  ZSDSFIT042-CURRENCY,    "CURRENCY
          ACC_NAME          TYPE  ZSDSFIT042-ACC_NAME,    "ACCOUNT NAME
          TRAN_TYPE_NAME    TYPE  ZSDSFIT042-TRAN_TYPE_NAME,    "TRANS TYPE NAME
          CUST_REFER        TYPE  ZSDSFIT042-CUST_REFER,    "CUST REFER
          BANK_REFER        TYPE  ZSDSFIT042-BANK_REFER,    "BANK REFER
          ERDAT             TYPE  ZSDSFIT042-ERDAT,    "DATE ON WHICH RECORD WAS CREATED
          ERZET             TYPE  ZSDSFIT042-ERZET,    "ENTRY TIME
          USNAM             TYPE  ZSDSFIT042-USNAM,    "USER NAME
          TRANF             TYPE  ZSDSFIT042-TRANF,    "TRANSFER NO.
          RUN_ID            TYPE  ZSDSFIT042-RUN_ID,    "RUNNING ID
          PAYIN             TYPE  ZSDSFIT042-PAYIN,    "PAY IN
          MAP_STATUS        TYPE  ZSDSFIT042-MAP_STATUS,    "MAPPING STATUS WITH COLLECTOR
          MAP_DATE          TYPE  ZSDSFIT042-MAP_DATE,    "MAPPING DATE WITH COLLECTOR
          MAP_TIME          TYPE  ZSDSFIT042-MAP_TIME,    "MAPPING TIME WITH COLLECTOR
          STATUS_LOCK       TYPE  ZSDSFIT042-STATUS_LOCK,    "STATUS LOCK DATA
          LOCK_ERDAT        TYPE  ZSDSFIT042-LOCK_ERDAT,    "DATE ON WHICH RECORD WAS CREATED
          LOCK_ERZET        TYPE  ZSDSFIT042-LOCK_ERZET,    "ENTRY TIME
          LOCK_USNAM        TYPE  ZSDSFIT042-LOCK_USNAM,    "USER NAME
          DELETE_FLAG       TYPE  ZSDSFIT042-DELETE_FLAG,    "DELETED FLAG
          DEL_ERDAT         TYPE  ZSDSFIT042-DEL_ERDAT,    "DATE ON WHICH RECORD WAS CREATED
          DEL_ERZET         TYPE  ZSDSFIT042-DEL_ERZET,    "ENTRY TIME
          DEL_USNAM         TYPE  ZSDSFIT042-DEL_USNAM,    "USER NAME
          CUST_NAME         TYPE  ZSDSFIT042-CUST_NAME,    "CUSTOMER NAME
          B_VALUE_TIME      TYPE  ZSDSFIT042-B_VALUE_TIME,    "BANK VALUE TIME
          MAP_USNAM         TYPE  ZSDSFIT042-MAP_USNAM,    "USER NAME
          RESERVE_MAP       TYPE  ZSDSFIT042-RESERVE_MAP,    "STATUS RESERVE BY USER
          RESERVE_DATE      TYPE  ZSDSFIT042-RESERVE_DATE,    "DATE ON WHICH RECORD WAS CREATED
          RESERVE_TIME      TYPE  ZSDSFIT042-RESERVE_TIME,    "ENTRY TIME
          RESERVE_USNAM     TYPE  ZSDSFIT042-RESERVE_USNAM,    "USER NAME
          RESERVE_DESC      TYPE  ZSDSFIT042-RESERVE_DESC,    "RESEVE DETAIL
          FI_CONFIRM_MAP    TYPE  ZSDSFIT042-FI_CONFIRM_MAP,    "FI CONFRIM
          FI_CONFIRM_DATE   TYPE  ZSDSFIT042-FI_CONFIRM_DATE,    "DATE ON WHICH RECORD WAS CREATED
          FI_CONFIRM_TIME   TYPE  ZSDSFIT042-FI_CONFIRM_TIME,    "ENTRY TIME
          FI_CONFIRM_USNAM  TYPE  ZSDSFIT042-FI_CONFIRM_USNAM,    "USER NAME
          STATUS_AD         TYPE  ZSDSFIT042-STATUS_AD,    "STATUS CLEAR EXPERIMENT
          UNCHECK           TYPE C,  "UNCHECK
          KBANK_DETAIL      TYPE  ZSDSFIT042-KBANK_DETAIL,    "KBANK_DETAIL  "CH2  ADD BY WANTANEE 20210610
          TRNFER_NUMBER     TYPE  ZSDSFIT042-TRNFER_NUMBER,    "TRNFER_NUMBER
          FYEAR_TRNFERNO    TYPE  ZSDSFIT042-FYEAR_TRNFERNO,    "FYEAR_TRNFERNO
          FI_CLEARING_NO    TYPE  ZSDSFIT042-FI_CLEARING_NO,    "FI_CLEARING_NO
          FYEAR_CLEARING    TYPE  ZSDSFIT042-FYEAR_CLEARING,    "FYEAR_CLEARING
          DATE_CLEAR        TYPE  ZSDSFIT042-DATE_CLEAR,    "DATE_CLEAR
          TIME_CLEAR        TYPE  ZSDSFIT042-TIME_CLEAR,    "TIME_CLEAR
          MT940_DOC         TYPE  ZSDSFIT042-MT940_DOC,    "MT940_DOC
          FYEAR_MT940       TYPE  ZSDSFIT042-FYEAR_MT940,    "FYEAR_MT940
          CLEARING_USER     TYPE  ZSDSFIT042-CLEARING_USER,    "CLEARING_USER

       END OF TYP_OUTPUT.

TYPES: BEGIN OF TYP_BANK,
         HBKID             TYPE  ZSDSFIT044-HBKID,  "Bank code
         BANK_NAME         TYPE  ZSDSFIT044-BANK_NAME,  "Bank code
END OF TYP_BANK.
TYPES: BEGIN OF TYP_USGRP_USER,
       BNAME   TYPE USGRP_USER-BNAME,
       USERGROUP TYPE USGRP_USER-USERGROUP,
END OF TYP_USGRP_USER.



DATA: GT_ZSDSFIT042   TYPE TABLE OF TY_ZSDSFIT042,
      GW_ZSDSFIT042   TYPE TY_ZSDSFIT042,
      WA_ZSDSFIT042_FEE   TYPE TY_ZSDSFIT042,
      GT_OUTPUT TYPE TABLE OF TYP_OUTPUT,
      GS_OUTPUT TYPE TYP_OUTPUT,
      WA_OUTPUT TYPE TYP_OUTPUT,
      GW_OUTPUT TYPE TYP_OUTPUT,
      GT_BANK   TYPE STANDARD TABLE OF TYP_BANK,
      GS_BANK   TYPE  TYP_BANK.

DATA: GT_USGRP_USER   TYPE STANDARD TABLE OF TYP_USGRP_USER,
      GW_USGRP_USER   TYPE TYP_USGRP_USER,
      WA_USGRP_USER   TYPE TYP_USGRP_USER.


DATA: GV_TRNFER_NUMBER TYPE ZSDSFIT042-TRNFER_NUMBER.


DATA: GT_FIELDCAT      TYPE SLIS_T_FIELDCAT_ALV,
      GS_FIELDCAT      TYPE SLIS_FIELDCAT_ALV,
      GS_LAYOUT        TYPE SLIS_LAYOUT_ALV.

DATA: BDCDATA TYPE TABLE OF BDCDATA WITH HEADER LINE.

DATA: NAME  TYPE VRM_ID,
      LIST  TYPE VRM_VALUES,
      VALUE LIKE LINE OF LIST.
DATA: FLAG_AUTHO_USER  TYPE C. "CHECK AUTHORIZE USER USE REPORT
CONTROLS: TC_100 TYPE TABLEVIEW USING SCREEN 0100.
DATA: XSCREEN(1)   TYPE C.                "OUTPUT ON PRINTER OR SCREEN

DEFINE DEF_LIST_HEAD.
  CLEAR: LW_LISTLINE.
  LW_LISTLINE-TYP  = &1.
  LW_LISTLINE-KEY  = &2.
  LW_LISTLINE-INFO = &3.
  APPEND LW_LISTLINE TO LT_LISTHEAD.
END-OF-DEFINITION.
*&---------------------------------------------------------------------*
*&  M A C R O   C O M M A N D   D E F I N I T I O N  ( %_ )            *
*&---------------------------------------------------------------------*
DEFINE %SHOW. " SHOW INPUT FIELD
  IF SCREEN-GROUP1 = &1.
    SCREEN-INVISIBLE = 0.
    SCREEN-ACTIVE = 1.
  ENDIF.
END-OF-DEFINITION.
DEFINE %HIDE. " HIDE INPUT FIELD
  IF SCREEN-GROUP1 = &1.
    SCREEN-INVISIBLE = 1.
    SCREEN-ACTIVE = 0.
  ENDIF.
END-OF-DEFINITION.

DEFINE M_FILL_CAT.
  GS_FIELDCAT-TABNAME    = &1.
  GS_FIELDCAT-FIELDNAME  = &2.
  GS_FIELDCAT-COL_POS    = &3.
  GS_FIELDCAT-SELTEXT_L  = &4.
  GS_FIELDCAT-NO_OUT     = &5.
  GS_FIELDCAT-OUTPUTLEN  = &6.
  GS_FIELDCAT-EDIT  = &7.
  GS_FIELDCAT-CHECKBOX = &8.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
  CLEAR GS_FIELDCAT.

END-OF-DEFINITION.

*
*END-OF-DEFINITION.
************************************************************************
*      S E L E C T I O N  S C R E E N                                  *
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-B01.

PARAMETERS: R_REV RADIOBUTTON GROUP GR1 DEFAULT 'X',
            R_FICON  RADIOBUTTON GROUP GR1,
            R_REPT  RADIOBUTTON GROUP GR1,
            R_UNREV RADIOBUTTON GROUP GR1,
            R_UNFICO RADIOBUTTON GROUP GR1,
            R_UNMAP  RADIOBUTTON GROUP GR1.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-B02.

PARAMETERS : P_BANK TYPE ZSDSFIT042-HBKID AS LISTBOX VISIBLE LENGTH 35   DEFAULT SPACE." MODIF ID SC1.
SELECT-OPTIONS : S_BDATE FOR ZSDSFIT042-ERDAT MODIF ID SC1,
                 S_BVDAT FOR ZSDSFIT042-CHEQUE_DATE MODIF ID SC1,
                 S_ITEM  FOR ZSDSFIT042-ZBANK_ITEM  MODIF ID SC1.

SELECTION-SCREEN END OF BLOCK B2.



************************************************************************
*      A T     S E L E C T I O N     S C R E E N                       *
************************************************************************
INITIALIZATION.
   PERFORM F_ADD_LIST_BANK.

AT SELECTION-SCREEN OUTPUT.
*   PERFORM F_MODIFY_SCREEN.
************************************************************************
*      B E G I N      S E L E C T I O N                                *
************************************************************************
START-OF-SELECTION .
   PERFORM F_CHECK_AUTHORIZE_USER.

       PERFORM F_GET_DATA.
       PERFORM F_MAP_DATA.


************************************************************************
*      E N D      S E L E C T I O N                                    *
************************************************************************
END-OF-SELECTION .

  IF GT_OUTPUT[] IS NOT INITIAL.
    PERFORM F_DISPLAY_REPORT.
  ELSE.
    MESSAGE I000(38) WITH TEXT-E04 DISPLAY LIKE 'E'.
  ENDIF.


************************************************************************
*      FORM F_GET_PATH_NAME                                              *
*----------------------------------------------------------------------*
*      DESCRIPTION: THIS FORM IS USED FOR GET PC PATH.                 *
************************************************************************
FORM F_GET_PATH_NAME  CHANGING P_FILEPATH.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      MASK             = ',*.XLS.'
      MODE             = 'O'
      TITLE            = 'BROWSED FILE'
    IMPORTING
      FILENAME         = P_FILEPATH
    EXCEPTIONS
      INV_WINSYS       = 01
      NO_BATCH         = 02
      SELECTION_CANCEL = 03
      SELECTION_ERROR  = 04.

ENDFORM.                    " F_GET_PATH_NAME



*&---------------------------------------------------------------------*
*&      FORM  F_GET_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM F_GET_DATA .

  DATA: LV_ADRNR1 TYPE KNA1-ADRNR.
  DATA: LV_VBELN TYPE VBRK-VBELN.

  CLEAR: GT_ZSDSFIT042.

    IF R_REPT IS INITIAL.



              IF P_BANK IS INITIAL.
                     IF R_FICON IS NOT INITIAL.
                            SELECT MANDT HBKID ZBANK_ITEM CHEQUE_DATE DEBIT_AMT RECEIVED_AMOUNT
                                   FLAG_BANK_FEE LINE_MAP_BANK_FE BANK_DESC BAL_AMT CHANNEL BRANCH_CODE
                                   BRANCH_NAME TERMINAL_ID CHEQUE_NO TRAN_CODE SENDER_BANK SENDER_BRANCH
                                   SENDER_ACC_NO SENDER_ACC_NAME SDS_ACC_NO TRAN_DATE TRAN_DATE_TO
                                   SMBC_REMARK SMBC_BAL_AMT RECORD_TYPE SWIFT_BIC ACC_TYPE CURRENCY
                                   ACC_NAME TRAN_TYPE_NAME CUST_REFER BANK_REFER ERDAT ERZET
                                   USNAM TRANF RUN_ID PAYIN MAP_STATUS MAP_DATE MAP_TIME
                                   STATUS_LOCK LOCK_ERDAT LOCK_ERZET LOCK_USNAM DELETE_FLAG
                                   DEL_ERDAT DEL_ERZET DEL_USNAM CUST_NAME B_VALUE_TIME
                                   MAP_USNAM RESERVE_MAP RESERVE_DATE RESERVE_TIME RESERVE_USNAM
                                   RESERVE_DESC FI_CONFIRM_MAP FI_CONFIRM_DATE FI_CONFIRM_TIME
                                   FI_CONFIRM_USNAM STATUS_AD
                                   STATUS_ACC_RECON
                                   RECON_DATE
                                   RECON_TIME
                                   RECON_USER
                                   KBANK_DETAIL
                                   TRNFER_NUMBER
                                   FYEAR_TRNFERNO
                                   FI_CLEARING_NO
                                   FYEAR_CLEARING
                                   DATE_CLEAR
                                   TIME_CLEAR
                                   MT940_DOC
                                   FYEAR_MT940
                                   CLEARING_USER
                              INTO TABLE GT_ZSDSFIT042
                              FROM ZSDSFIT042
                              WHERE CHEQUE_DATE IN S_BVDAT
                              AND   ERDAT  IN S_BDATE
                              AND   DELETE_FLAG NE 'X'
                              AND   TRANF EQ ''
                              AND   FI_CONFIRM_MAP NE 'X'
                              AND   ZBANK_ITEM IN S_ITEM.
                        ELSEIF R_REV IS NOT INITIAL.
                           SELECT MANDT HBKID ZBANK_ITEM CHEQUE_DATE DEBIT_AMT RECEIVED_AMOUNT
                                   FLAG_BANK_FEE LINE_MAP_BANK_FE BANK_DESC BAL_AMT CHANNEL BRANCH_CODE
                                   BRANCH_NAME TERMINAL_ID CHEQUE_NO TRAN_CODE SENDER_BANK SENDER_BRANCH
                                   SENDER_ACC_NO SENDER_ACC_NAME SDS_ACC_NO TRAN_DATE TRAN_DATE_TO
                                   SMBC_REMARK SMBC_BAL_AMT RECORD_TYPE SWIFT_BIC ACC_TYPE CURRENCY
                                   ACC_NAME TRAN_TYPE_NAME CUST_REFER BANK_REFER ERDAT ERZET
                                   USNAM TRANF RUN_ID PAYIN MAP_STATUS MAP_DATE MAP_TIME
                                   STATUS_LOCK LOCK_ERDAT LOCK_ERZET LOCK_USNAM DELETE_FLAG
                                   DEL_ERDAT DEL_ERZET DEL_USNAM CUST_NAME B_VALUE_TIME
                                   MAP_USNAM RESERVE_MAP RESERVE_DATE RESERVE_TIME RESERVE_USNAM
                                   RESERVE_DESC FI_CONFIRM_MAP FI_CONFIRM_DATE FI_CONFIRM_TIME
                                   FI_CONFIRM_USNAM STATUS_AD
                                   STATUS_ACC_RECON
                                   RECON_DATE
                                   RECON_TIME
                                   RECON_USER
                                   KBANK_DETAIL
                                    TRNFER_NUMBER
                                    FYEAR_TRNFERNO
                                    FI_CLEARING_NO
                                    FYEAR_CLEARING
                                    DATE_CLEAR
                                    TIME_CLEAR
                                    MT940_DOC
                                    FYEAR_MT940
                                    CLEARING_USER
                              INTO TABLE GT_ZSDSFIT042
                              FROM ZSDSFIT042
                              WHERE CHEQUE_DATE IN S_BVDAT
                              AND   ERDAT  IN S_BDATE
                              AND   DELETE_FLAG NE 'X'
                              AND   TRANF EQ ''
                              AND   ( RESERVE_MAP NE 'X' AND FI_CONFIRM_MAP NE 'X' )
                              AND   ZBANK_ITEM IN S_ITEM.

                        ELSEIF R_UNREV IS NOT INITIAL.
                              SELECT MANDT HBKID ZBANK_ITEM CHEQUE_DATE DEBIT_AMT RECEIVED_AMOUNT
                                   FLAG_BANK_FEE LINE_MAP_BANK_FE BANK_DESC BAL_AMT CHANNEL BRANCH_CODE
                                   BRANCH_NAME TERMINAL_ID CHEQUE_NO TRAN_CODE SENDER_BANK SENDER_BRANCH
                                   SENDER_ACC_NO SENDER_ACC_NAME SDS_ACC_NO TRAN_DATE TRAN_DATE_TO
                                   SMBC_REMARK SMBC_BAL_AMT RECORD_TYPE SWIFT_BIC ACC_TYPE CURRENCY
                                   ACC_NAME TRAN_TYPE_NAME CUST_REFER BANK_REFER ERDAT ERZET
                                   USNAM TRANF RUN_ID PAYIN MAP_STATUS MAP_DATE MAP_TIME
                                   STATUS_LOCK LOCK_ERDAT LOCK_ERZET LOCK_USNAM DELETE_FLAG
                                   DEL_ERDAT DEL_ERZET DEL_USNAM CUST_NAME B_VALUE_TIME
                                   MAP_USNAM RESERVE_MAP RESERVE_DATE RESERVE_TIME RESERVE_USNAM
                                   RESERVE_DESC FI_CONFIRM_MAP FI_CONFIRM_DATE FI_CONFIRM_TIME
                                   FI_CONFIRM_USNAM STATUS_AD
                                   STATUS_ACC_RECON
                                   RECON_DATE
                                   RECON_TIME
                                   RECON_USER
                                   KBANK_DETAIL
                                    TRNFER_NUMBER
                                    FYEAR_TRNFERNO
                                    FI_CLEARING_NO
                                    FYEAR_CLEARING
                                    DATE_CLEAR
                                    TIME_CLEAR
                                    MT940_DOC
                                    FYEAR_MT940
                                    CLEARING_USER
                              INTO TABLE GT_ZSDSFIT042
                              FROM ZSDSFIT042
                              WHERE CHEQUE_DATE IN S_BVDAT
                              AND   ERDAT  IN S_BDATE
                              AND   DELETE_FLAG NE 'X'
                              AND   TRANF EQ ''
                              AND   RESERVE_MAP EQ 'X'
                              AND   ZBANK_ITEM IN S_ITEM.

                        ELSEIF R_UNFICO IS NOT INITIAL.
                              SELECT MANDT HBKID ZBANK_ITEM CHEQUE_DATE DEBIT_AMT RECEIVED_AMOUNT
                                   FLAG_BANK_FEE LINE_MAP_BANK_FE BANK_DESC BAL_AMT CHANNEL BRANCH_CODE
                                   BRANCH_NAME TERMINAL_ID CHEQUE_NO TRAN_CODE SENDER_BANK SENDER_BRANCH
                                   SENDER_ACC_NO SENDER_ACC_NAME SDS_ACC_NO TRAN_DATE TRAN_DATE_TO
                                   SMBC_REMARK SMBC_BAL_AMT RECORD_TYPE SWIFT_BIC ACC_TYPE CURRENCY
                                   ACC_NAME TRAN_TYPE_NAME CUST_REFER BANK_REFER ERDAT ERZET
                                   USNAM TRANF RUN_ID PAYIN MAP_STATUS MAP_DATE MAP_TIME
                                   STATUS_LOCK LOCK_ERDAT LOCK_ERZET LOCK_USNAM DELETE_FLAG
                                   DEL_ERDAT DEL_ERZET DEL_USNAM CUST_NAME B_VALUE_TIME
                                   MAP_USNAM RESERVE_MAP RESERVE_DATE RESERVE_TIME RESERVE_USNAM
                                   RESERVE_DESC FI_CONFIRM_MAP FI_CONFIRM_DATE FI_CONFIRM_TIME
                                   FI_CONFIRM_USNAM STATUS_AD
                                   STATUS_ACC_RECON
                                   RECON_DATE
                                   RECON_TIME
                                   RECON_USER
                                   KBANK_DETAIL
                                    TRNFER_NUMBER
                                    FYEAR_TRNFERNO
                                    FI_CLEARING_NO
                                    FYEAR_CLEARING
                                    DATE_CLEAR
                                    TIME_CLEAR
                                    MT940_DOC
                                    FYEAR_MT940
                                    CLEARING_USER
                              INTO TABLE GT_ZSDSFIT042
                              FROM ZSDSFIT042
                              WHERE CHEQUE_DATE IN S_BVDAT
                              AND   ERDAT  IN S_BDATE
                              AND   DELETE_FLAG NE 'X'
                              AND   TRANF EQ ''
                              AND   FI_CONFIRM_MAP EQ 'X'
                               AND   ZBANK_ITEM IN S_ITEM.
                        ELSEIF R_UNMAP IS NOT INITIAL.
                          SELECT MANDT HBKID ZBANK_ITEM CHEQUE_DATE DEBIT_AMT RECEIVED_AMOUNT
                                 FLAG_BANK_FEE LINE_MAP_BANK_FE BANK_DESC BAL_AMT CHANNEL BRANCH_CODE
                                 BRANCH_NAME TERMINAL_ID CHEQUE_NO TRAN_CODE SENDER_BANK SENDER_BRANCH
                                 SENDER_ACC_NO SENDER_ACC_NAME SDS_ACC_NO TRAN_DATE TRAN_DATE_TO
                                 SMBC_REMARK SMBC_BAL_AMT RECORD_TYPE SWIFT_BIC ACC_TYPE CURRENCY
                                 ACC_NAME TRAN_TYPE_NAME CUST_REFER BANK_REFER ERDAT ERZET
                                 USNAM TRANF RUN_ID PAYIN MAP_STATUS MAP_DATE MAP_TIME
                                 STATUS_LOCK LOCK_ERDAT LOCK_ERZET LOCK_USNAM DELETE_FLAG
                                 DEL_ERDAT DEL_ERZET DEL_USNAM CUST_NAME B_VALUE_TIME
                                 MAP_USNAM RESERVE_MAP RESERVE_DATE RESERVE_TIME RESERVE_USNAM
                                 RESERVE_DESC FI_CONFIRM_MAP FI_CONFIRM_DATE FI_CONFIRM_TIME
                                 FI_CONFIRM_USNAM STATUS_AD
                                 STATUS_ACC_RECON
                                   RECON_DATE
                                   RECON_TIME
                                   RECON_USER
                                   KBANK_DETAIL
                                    TRNFER_NUMBER
                                    FYEAR_TRNFERNO
                                    FI_CLEARING_NO
                                    FYEAR_CLEARING
                                    DATE_CLEAR
                                    TIME_CLEAR
                                    MT940_DOC
                                    FYEAR_MT940
                                    CLEARING_USER
                            INTO TABLE GT_ZSDSFIT042
                            FROM ZSDSFIT042
                            WHERE HBKID  EQ P_BANK
                            AND   CHEQUE_DATE IN S_BVDAT
                            AND   ERDAT  IN S_BDATE
                            AND   DELETE_FLAG NE 'X'
*                            AND   MAP_STATUS EQ 'X'
                            AND   ZBANK_ITEM IN S_ITEM.
                        ENDIF.

               ELSE.
                  IF R_FICON IS NOT INITIAL.
                          SELECT MANDT HBKID ZBANK_ITEM CHEQUE_DATE DEBIT_AMT RECEIVED_AMOUNT
                                 FLAG_BANK_FEE LINE_MAP_BANK_FE BANK_DESC BAL_AMT CHANNEL BRANCH_CODE
                                 BRANCH_NAME TERMINAL_ID CHEQUE_NO TRAN_CODE SENDER_BANK SENDER_BRANCH
                                 SENDER_ACC_NO SENDER_ACC_NAME SDS_ACC_NO TRAN_DATE TRAN_DATE_TO
                                 SMBC_REMARK SMBC_BAL_AMT RECORD_TYPE SWIFT_BIC ACC_TYPE CURRENCY
                                 ACC_NAME TRAN_TYPE_NAME CUST_REFER BANK_REFER ERDAT ERZET
                                 USNAM TRANF RUN_ID PAYIN MAP_STATUS MAP_DATE MAP_TIME
                                 STATUS_LOCK LOCK_ERDAT LOCK_ERZET LOCK_USNAM DELETE_FLAG
                                 DEL_ERDAT DEL_ERZET DEL_USNAM CUST_NAME B_VALUE_TIME
                                 MAP_USNAM RESERVE_MAP RESERVE_DATE RESERVE_TIME RESERVE_USNAM
                                 RESERVE_DESC FI_CONFIRM_MAP FI_CONFIRM_DATE FI_CONFIRM_TIME
                                 FI_CONFIRM_USNAM STATUS_AD
                                 STATUS_ACC_RECON
                                   RECON_DATE
                                   RECON_TIME
                                   RECON_USER
                                   KBANK_DETAIL
                                    TRNFER_NUMBER
                                    FYEAR_TRNFERNO
                                    FI_CLEARING_NO
                                    FYEAR_CLEARING
                                    DATE_CLEAR
                                    TIME_CLEAR
                                    MT940_DOC
                                    FYEAR_MT940
                                    CLEARING_USER
                            INTO TABLE GT_ZSDSFIT042
                            FROM ZSDSFIT042
                            WHERE HBKID  EQ P_BANK
                            AND   CHEQUE_DATE IN S_BVDAT
                            AND   ERDAT  IN S_BDATE
                            AND   DELETE_FLAG NE 'X'
                            AND   TRANF EQ ' '
                            AND   FI_CONFIRM_MAP NE 'X'
                            AND   ZBANK_ITEM IN S_ITEM.
                   ELSEIF R_REV IS NOT INITIAL.
                         SELECT MANDT HBKID ZBANK_ITEM CHEQUE_DATE DEBIT_AMT RECEIVED_AMOUNT
                                 FLAG_BANK_FEE LINE_MAP_BANK_FE BANK_DESC BAL_AMT CHANNEL BRANCH_CODE
                                 BRANCH_NAME TERMINAL_ID CHEQUE_NO TRAN_CODE SENDER_BANK SENDER_BRANCH
                                 SENDER_ACC_NO SENDER_ACC_NAME SDS_ACC_NO TRAN_DATE TRAN_DATE_TO
                                 SMBC_REMARK SMBC_BAL_AMT RECORD_TYPE SWIFT_BIC ACC_TYPE CURRENCY
                                 ACC_NAME TRAN_TYPE_NAME CUST_REFER BANK_REFER ERDAT ERZET
                                 USNAM TRANF RUN_ID PAYIN MAP_STATUS MAP_DATE MAP_TIME
                                 STATUS_LOCK LOCK_ERDAT LOCK_ERZET LOCK_USNAM DELETE_FLAG
                                 DEL_ERDAT DEL_ERZET DEL_USNAM CUST_NAME B_VALUE_TIME
                                 MAP_USNAM RESERVE_MAP RESERVE_DATE RESERVE_TIME RESERVE_USNAM
                                 RESERVE_DESC FI_CONFIRM_MAP FI_CONFIRM_DATE FI_CONFIRM_TIME
                                 FI_CONFIRM_USNAM STATUS_AD
                                 STATUS_ACC_RECON
                                   RECON_DATE
                                   RECON_TIME
                                   RECON_USER
                                   KBANK_DETAIL
                                  TRNFER_NUMBER
                                  FYEAR_TRNFERNO
                                  FI_CLEARING_NO
                                  FYEAR_CLEARING
                                  DATE_CLEAR
                                  TIME_CLEAR
                                  MT940_DOC
                                  FYEAR_MT940
                                  CLEARING_USER
                            INTO TABLE GT_ZSDSFIT042
                            FROM ZSDSFIT042
                            WHERE HBKID  EQ P_BANK
                            AND   CHEQUE_DATE IN S_BVDAT
                            AND   ERDAT  IN S_BDATE
                            AND   DELETE_FLAG NE 'X'
                            AND   TRANF EQ ' '
                            AND   ( RESERVE_MAP NE 'X' AND FI_CONFIRM_MAP NE 'X' )
                            AND   ZBANK_ITEM IN S_ITEM.
                   ELSEIF R_UNREV IS NOT INITIAL.
                           SELECT MANDT HBKID ZBANK_ITEM CHEQUE_DATE DEBIT_AMT RECEIVED_AMOUNT
                                 FLAG_BANK_FEE LINE_MAP_BANK_FE BANK_DESC BAL_AMT CHANNEL BRANCH_CODE
                                 BRANCH_NAME TERMINAL_ID CHEQUE_NO TRAN_CODE SENDER_BANK SENDER_BRANCH
                                 SENDER_ACC_NO SENDER_ACC_NAME SDS_ACC_NO TRAN_DATE TRAN_DATE_TO
                                 SMBC_REMARK SMBC_BAL_AMT RECORD_TYPE SWIFT_BIC ACC_TYPE CURRENCY
                                 ACC_NAME TRAN_TYPE_NAME CUST_REFER BANK_REFER ERDAT ERZET
                                 USNAM TRANF RUN_ID PAYIN MAP_STATUS MAP_DATE MAP_TIME
                                 STATUS_LOCK LOCK_ERDAT LOCK_ERZET LOCK_USNAM DELETE_FLAG
                                 DEL_ERDAT DEL_ERZET DEL_USNAM CUST_NAME B_VALUE_TIME
                                 MAP_USNAM RESERVE_MAP RESERVE_DATE RESERVE_TIME RESERVE_USNAM
                                 RESERVE_DESC FI_CONFIRM_MAP FI_CONFIRM_DATE FI_CONFIRM_TIME
                                 FI_CONFIRM_USNAM STATUS_AD
                                 STATUS_ACC_RECON
                                   RECON_DATE
                                   RECON_TIME
                                   RECON_USER
                                   KBANK_DETAIL
                                    TRNFER_NUMBER
                                    FYEAR_TRNFERNO
                                    FI_CLEARING_NO
                                    FYEAR_CLEARING
                                    DATE_CLEAR
                                    TIME_CLEAR
                                    MT940_DOC
                                    FYEAR_MT940
                                    CLEARING_USER
                            INTO TABLE GT_ZSDSFIT042
                            FROM ZSDSFIT042
                            WHERE HBKID  EQ P_BANK
                            AND   CHEQUE_DATE IN S_BVDAT
                            AND   ERDAT  IN S_BDATE
                            AND   DELETE_FLAG NE 'X'
                            AND   TRANF EQ ' '
                            AND   RESERVE_MAP EQ 'X'
                            AND   ZBANK_ITEM IN S_ITEM.
                   ELSEIF R_UNFICO IS NOT INITIAL.
                          SELECT MANDT HBKID ZBANK_ITEM CHEQUE_DATE DEBIT_AMT RECEIVED_AMOUNT
                                 FLAG_BANK_FEE LINE_MAP_BANK_FE BANK_DESC BAL_AMT CHANNEL BRANCH_CODE
                                 BRANCH_NAME TERMINAL_ID CHEQUE_NO TRAN_CODE SENDER_BANK SENDER_BRANCH
                                 SENDER_ACC_NO SENDER_ACC_NAME SDS_ACC_NO TRAN_DATE TRAN_DATE_TO
                                 SMBC_REMARK SMBC_BAL_AMT RECORD_TYPE SWIFT_BIC ACC_TYPE CURRENCY
                                 ACC_NAME TRAN_TYPE_NAME CUST_REFER BANK_REFER ERDAT ERZET
                                 USNAM TRANF RUN_ID PAYIN MAP_STATUS MAP_DATE MAP_TIME
                                 STATUS_LOCK LOCK_ERDAT LOCK_ERZET LOCK_USNAM DELETE_FLAG
                                 DEL_ERDAT DEL_ERZET DEL_USNAM CUST_NAME B_VALUE_TIME
                                 MAP_USNAM RESERVE_MAP RESERVE_DATE RESERVE_TIME RESERVE_USNAM
                                 RESERVE_DESC FI_CONFIRM_MAP FI_CONFIRM_DATE FI_CONFIRM_TIME
                                 FI_CONFIRM_USNAM STATUS_AD
                                 STATUS_ACC_RECON
                                   RECON_DATE
                                   RECON_TIME
                                   RECON_USER
                                   KBANK_DETAIL
                                    TRNFER_NUMBER
                                    FYEAR_TRNFERNO
                                    FI_CLEARING_NO
                                    FYEAR_CLEARING
                                    DATE_CLEAR
                                    TIME_CLEAR
                                    MT940_DOC
                                    FYEAR_MT940
                                    CLEARING_USER
                            INTO TABLE GT_ZSDSFIT042
                            FROM ZSDSFIT042
                            WHERE HBKID  EQ P_BANK
                            AND   CHEQUE_DATE IN S_BVDAT
                            AND   ERDAT  IN S_BDATE
                            AND   DELETE_FLAG NE 'X'
                            AND   TRANF EQ ' '
                            AND   FI_CONFIRM_MAP EQ 'X'
                            AND   ZBANK_ITEM IN S_ITEM.

                   ELSEIF R_UNMAP IS NOT INITIAL.
                          SELECT MANDT HBKID ZBANK_ITEM CHEQUE_DATE DEBIT_AMT RECEIVED_AMOUNT
                                 FLAG_BANK_FEE LINE_MAP_BANK_FE BANK_DESC BAL_AMT CHANNEL BRANCH_CODE
                                 BRANCH_NAME TERMINAL_ID CHEQUE_NO TRAN_CODE SENDER_BANK SENDER_BRANCH
                                 SENDER_ACC_NO SENDER_ACC_NAME SDS_ACC_NO TRAN_DATE TRAN_DATE_TO
                                 SMBC_REMARK SMBC_BAL_AMT RECORD_TYPE SWIFT_BIC ACC_TYPE CURRENCY
                                 ACC_NAME TRAN_TYPE_NAME CUST_REFER BANK_REFER ERDAT ERZET
                                 USNAM TRANF RUN_ID PAYIN MAP_STATUS MAP_DATE MAP_TIME
                                 STATUS_LOCK LOCK_ERDAT LOCK_ERZET LOCK_USNAM DELETE_FLAG
                                 DEL_ERDAT DEL_ERZET DEL_USNAM CUST_NAME B_VALUE_TIME
                                 MAP_USNAM RESERVE_MAP RESERVE_DATE RESERVE_TIME RESERVE_USNAM
                                 RESERVE_DESC FI_CONFIRM_MAP FI_CONFIRM_DATE FI_CONFIRM_TIME
                                 FI_CONFIRM_USNAM STATUS_AD
                                 STATUS_ACC_RECON
                                   RECON_DATE
                                   RECON_TIME
                                   RECON_USER
                                   KBANK_DETAIL
                                  TRNFER_NUMBER
                                  FYEAR_TRNFERNO
                                  FI_CLEARING_NO
                                  FYEAR_CLEARING
                                  DATE_CLEAR
                                  TIME_CLEAR
                                  MT940_DOC
                                  FYEAR_MT940
                                  CLEARING_USER
                            INTO TABLE GT_ZSDSFIT042
                            FROM ZSDSFIT042
                            WHERE HBKID  EQ P_BANK
                            AND   CHEQUE_DATE IN S_BVDAT
                            AND   ERDAT  IN S_BDATE
                            AND   DELETE_FLAG NE 'X'
                            AND   ( MAP_STATUS EQ 'X' OR FI_CLEARING_NO NE ' ' )
                            AND   ZBANK_ITEM IN S_ITEM.


                   ENDIF.
               ENDIF.
      ELSE.
            IF P_BANK IS INITIAL.
                     SELECT MANDT HBKID ZBANK_ITEM CHEQUE_DATE DEBIT_AMT RECEIVED_AMOUNT
                            FLAG_BANK_FEE LINE_MAP_BANK_FE BANK_DESC BAL_AMT CHANNEL BRANCH_CODE
                            BRANCH_NAME TERMINAL_ID CHEQUE_NO TRAN_CODE SENDER_BANK SENDER_BRANCH
                            SENDER_ACC_NO SENDER_ACC_NAME SDS_ACC_NO TRAN_DATE TRAN_DATE_TO
                            SMBC_REMARK SMBC_BAL_AMT RECORD_TYPE SWIFT_BIC ACC_TYPE CURRENCY
                            ACC_NAME TRAN_TYPE_NAME CUST_REFER BANK_REFER ERDAT ERZET
                            USNAM TRANF RUN_ID PAYIN MAP_STATUS MAP_DATE MAP_TIME
                            STATUS_LOCK LOCK_ERDAT LOCK_ERZET LOCK_USNAM DELETE_FLAG
                            DEL_ERDAT DEL_ERZET DEL_USNAM CUST_NAME B_VALUE_TIME
                            MAP_USNAM RESERVE_MAP RESERVE_DATE RESERVE_TIME RESERVE_USNAM
                            RESERVE_DESC FI_CONFIRM_MAP FI_CONFIRM_DATE FI_CONFIRM_TIME
                            FI_CONFIRM_USNAM STATUS_AD
                            STATUS_ACC_RECON
                            RECON_DATE
                            RECON_TIME
                            RECON_USER
                            KBANK_DETAIL
                            TRNFER_NUMBER
                            FYEAR_TRNFERNO
                            FI_CLEARING_NO
                            FYEAR_CLEARING
                            DATE_CLEAR
                            TIME_CLEAR
                            MT940_DOC
                            FYEAR_MT940
                            CLEARING_USER
                       INTO TABLE GT_ZSDSFIT042
                       FROM ZSDSFIT042
                       WHERE CHEQUE_DATE IN S_BVDAT
                       AND   ERDAT  IN S_BDATE
                       AND   DELETE_FLAG NE 'X'
                       AND   TRANF EQ ''
                        AND   ZBANK_ITEM IN S_ITEM.


               ELSE.
                     SELECT MANDT HBKID ZBANK_ITEM CHEQUE_DATE DEBIT_AMT RECEIVED_AMOUNT
                            FLAG_BANK_FEE LINE_MAP_BANK_FE BANK_DESC BAL_AMT CHANNEL BRANCH_CODE
                            BRANCH_NAME TERMINAL_ID CHEQUE_NO TRAN_CODE SENDER_BANK SENDER_BRANCH
                            SENDER_ACC_NO SENDER_ACC_NAME SDS_ACC_NO TRAN_DATE TRAN_DATE_TO
                            SMBC_REMARK SMBC_BAL_AMT RECORD_TYPE SWIFT_BIC ACC_TYPE CURRENCY
                            ACC_NAME TRAN_TYPE_NAME CUST_REFER BANK_REFER ERDAT ERZET
                            USNAM TRANF RUN_ID PAYIN MAP_STATUS MAP_DATE MAP_TIME
                            STATUS_LOCK LOCK_ERDAT LOCK_ERZET LOCK_USNAM DELETE_FLAG
                            DEL_ERDAT DEL_ERZET DEL_USNAM CUST_NAME B_VALUE_TIME
                            MAP_USNAM RESERVE_MAP RESERVE_DATE RESERVE_TIME RESERVE_USNAM
                            RESERVE_DESC FI_CONFIRM_MAP FI_CONFIRM_DATE FI_CONFIRM_TIME
                            FI_CONFIRM_USNAM STATUS_AD
                            STATUS_ACC_RECON
                            RECON_DATE
                            RECON_TIME
                            RECON_USER
                            KBANK_DETAIL
                            TRNFER_NUMBER
                            FYEAR_TRNFERNO
                            FI_CLEARING_NO
                            FYEAR_CLEARING
                            DATE_CLEAR
                            TIME_CLEAR
                            MT940_DOC
                            FYEAR_MT940
                            CLEARING_USER
                       INTO TABLE GT_ZSDSFIT042
                       FROM ZSDSFIT042
                       WHERE HBKID  EQ P_BANK
                       AND   CHEQUE_DATE IN S_BVDAT
                       AND   ERDAT  IN S_BDATE
                       AND   DELETE_FLAG NE 'X'
                       AND   TRANF EQ ' '
                        AND   ZBANK_ITEM IN S_ITEM.
*                       AND   RESERVE_MAP EQ ' '.
               ENDIF.
      ENDIF.



ENDFORM.                    " F_PROCESS_DATA


*&---------------------------------------------------------------------*
*&      FORM  F_MAP_DATE
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM F_MAP_DATA .
  DATA: LV_RECEIVE_AMT TYPE ZSDSFIT042-RECEIVED_AMOUNT.
  DATA: LV_CHECK_RECIVE TYPE C.
  DATA: LV_TABIX TYPE SY-TABIX.
  DATA: LV_ADRNR1 TYPE KNA1-ADRNR.
  DATA: LV_VBELN TYPE VBRK-VBELN.
  DATA: LV_CHECK_COLLECTOR TYPE C.
  CLEAR: GT_OUTPUT.

     LOOP AT GT_ZSDSFIT042 INTO GW_ZSDSFIT042 WHERE FLAG_BANK_FEE NE 'X'.
             CLEAR: GW_OUTPUT.
             GW_OUTPUT-HBKID = GW_ZSDSFIT042-HBKID.
             GW_OUTPUT-ZBANK_ITEM = GW_ZSDSFIT042-ZBANK_ITEM.
             GW_OUTPUT-CHEQUE_DATE = GW_ZSDSFIT042-CHEQUE_DATE.
             GW_OUTPUT-DEBIT_AMT = GW_ZSDSFIT042-DEBIT_AMT.

             GW_OUTPUT-RECEIVED_AMOUNT = GW_ZSDSFIT042-RECEIVED_AMOUNT.
                READ TABLE GT_ZSDSFIT042 INTO WA_ZSDSFIT042_FEE WITH KEY FLAG_BANK_FEE = 'X'
                                                                                    HBKID = GW_ZSDSFIT042-HBKID
                                                                                    LINE_MAP_BANK_FE = GW_ZSDSFIT042-ZBANK_ITEM.
                      IF SY-SUBRC EQ 0.
                          GW_OUTPUT-LINE_MAP_BANK_FE = WA_ZSDSFIT042_FEE-ZBANK_ITEM.
*                          GW_OUTPUT-BANK_FEE = WA_ZSDSFIT042_FEE-RECEIVED_AMOUNT.
                      ENDIF.



             GW_OUTPUT-BANK_DESC = GW_ZSDSFIT042-BANK_DESC.
             GW_OUTPUT-BAL_AMT = GW_ZSDSFIT042-BAL_AMT.
             GW_OUTPUT-CHANNEL = GW_ZSDSFIT042-CHANNEL.
             GW_OUTPUT-BRANCH_CODE = GW_ZSDSFIT042-BRANCH_CODE.
             GW_OUTPUT-BRANCH_NAME = GW_ZSDSFIT042-BRANCH_NAME.
             GW_OUTPUT-TERMINAL_ID = GW_ZSDSFIT042-TERMINAL_ID.
             GW_OUTPUT-CHEQUE_NO = GW_ZSDSFIT042-CHEQUE_NO.
             GW_OUTPUT-TRAN_CODE = GW_ZSDSFIT042-TRAN_CODE.
             GW_OUTPUT-SENDER_BANK = GW_ZSDSFIT042-SENDER_BANK.
             GW_OUTPUT-SENDER_BRANCH = GW_ZSDSFIT042-SENDER_BRANCH.
             GW_OUTPUT-SENDER_ACC_NO = GW_ZSDSFIT042-SENDER_ACC_NO.
             GW_OUTPUT-SENDER_ACC_NAME = GW_ZSDSFIT042-SENDER_ACC_NAME.
             GW_OUTPUT-SDS_ACC_NO = GW_ZSDSFIT042-SDS_ACC_NO.
             GW_OUTPUT-TRAN_DATE = GW_ZSDSFIT042-TRAN_DATE.
             GW_OUTPUT-TRAN_DATE_TO = GW_ZSDSFIT042-TRAN_DATE_TO.
             GW_OUTPUT-SMBC_REMARK = GW_ZSDSFIT042-SMBC_REMARK.
             GW_OUTPUT-SMBC_BAL_AMT = GW_ZSDSFIT042-SMBC_BAL_AMT.
             GW_OUTPUT-RECORD_TYPE = GW_ZSDSFIT042-RECORD_TYPE.
             GW_OUTPUT-SWIFT_BIC = GW_ZSDSFIT042-SWIFT_BIC.
             GW_OUTPUT-ACC_TYPE = GW_ZSDSFIT042-ACC_TYPE.
             GW_OUTPUT-CURRENCY = GW_ZSDSFIT042-CURRENCY.
             GW_OUTPUT-ACC_NAME = GW_ZSDSFIT042-ACC_NAME.
             GW_OUTPUT-TRAN_TYPE_NAME = GW_ZSDSFIT042-TRAN_TYPE_NAME.
             GW_OUTPUT-CUST_REFER = GW_ZSDSFIT042-CUST_REFER.
             GW_OUTPUT-BANK_REFER = GW_ZSDSFIT042-BANK_REFER.
             GW_OUTPUT-ERDAT = GW_ZSDSFIT042-ERDAT.
             GW_OUTPUT-ERZET = GW_ZSDSFIT042-ERZET.
             GW_OUTPUT-USNAM = GW_ZSDSFIT042-USNAM.
             GW_OUTPUT-TRANF = GW_ZSDSFIT042-TRANF.
             GW_OUTPUT-PAYIN = GW_ZSDSFIT042-PAYIN.
             GW_OUTPUT-MAP_STATUS = GW_ZSDSFIT042-MAP_STATUS.
             GW_OUTPUT-MAP_DATE = GW_ZSDSFIT042-MAP_DATE.
             GW_OUTPUT-MAP_TIME = GW_ZSDSFIT042-MAP_TIME.
             GW_OUTPUT-MAP_DATE = GW_ZSDSFIT042-MAP_DATE.
             GW_OUTPUT-STATUS_LOCK = GW_ZSDSFIT042-STATUS_LOCK.
             GW_OUTPUT-LOCK_ERDAT = GW_ZSDSFIT042-LOCK_ERDAT.
             GW_OUTPUT-LOCK_ERZET = GW_ZSDSFIT042-LOCK_ERZET.
             GW_OUTPUT-LOCK_USNAM = GW_ZSDSFIT042-LOCK_USNAM.
             GW_OUTPUT-RESERVE_MAP = GW_ZSDSFIT042-RESERVE_MAP.
             GW_OUTPUT-RESERVE_DESC = GW_ZSDSFIT042-RESERVE_DESC.
             GW_OUTPUT-STATUS_AD = GW_ZSDSFIT042-STATUS_AD.
             GW_OUTPUT-FI_CONFIRM_MAP = GW_ZSDSFIT042-FI_CONFIRM_MAP.
             GW_OUTPUT-RESERVE_DATE = GW_ZSDSFIT042-RESERVE_DATE.
             GW_OUTPUT-RESERVE_TIME = GW_ZSDSFIT042-RESERVE_TIME.
             GW_OUTPUT-RESERVE_USNAM = GW_ZSDSFIT042-RESERVE_USNAM.
             GW_OUTPUT-FI_CONFIRM_DATE = GW_ZSDSFIT042-FI_CONFIRM_DATE.
             GW_OUTPUT-FI_CONFIRM_TIME = GW_ZSDSFIT042-FI_CONFIRM_TIME.
             GW_OUTPUT-FI_CONFIRM_USNAM = GW_ZSDSFIT042-FI_CONFIRM_USNAM.
             GW_OUTPUT-KBANK_DETAIL = GW_ZSDSFIT042-KBANK_DETAIL.

            GW_OUTPUT-TRNFER_NUMBER = GW_ZSDSFIT042-TRNFER_NUMBER.
            GW_OUTPUT-FYEAR_TRNFERNO = GW_ZSDSFIT042-FYEAR_TRNFERNO.
            GW_OUTPUT-FI_CLEARING_NO = GW_ZSDSFIT042-FI_CLEARING_NO.
            GW_OUTPUT-FYEAR_CLEARING = GW_ZSDSFIT042-FYEAR_CLEARING.
            GW_OUTPUT-DATE_CLEAR = GW_ZSDSFIT042-DATE_CLEAR.
            GW_OUTPUT-TIME_CLEAR = GW_ZSDSFIT042-TIME_CLEAR.
            GW_OUTPUT-MT940_DOC = GW_ZSDSFIT042-MT940_DOC.
            GW_OUTPUT-FYEAR_MT940 = GW_ZSDSFIT042-FYEAR_MT940.
            GW_OUTPUT-CLEARING_USER = GW_ZSDSFIT042-CLEARING_USER.



             APPEND GW_OUTPUT TO GT_OUTPUT.






     ENDLOOP.

     SORT GT_OUTPUT BY ZBANK_ITEM.





ENDFORM.                    " F_PROCESS_DATA

*&---------------------------------------------------------------------*
*&      FORM  F_DISPLAY_REPORT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM F_DISPLAY_REPORT .

  PERFORM F_FILL_FIELDCAT.

  PERFORM F_PREPARE_LAYOUT.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-REPID
      I_SAVE                   = GC_SAVE
*     IS_VARIANT               = G_VARIANT
      I_DEFAULT                = 'X'
      IT_FIELDCAT              = GT_FIELDCAT
      IS_LAYOUT                = GS_LAYOUT
      I_CALLBACK_PF_STATUS_SET = 'STATUS_SET'
      I_CALLBACK_USER_COMMAND  = 'USERCOMMAND'
    TABLES
      T_OUTTAB                 = GT_OUTPUT
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " F_DISPLAY_REPORT
* ----------------------------------------------------
* STATUS
* ----------------------------------------------------
FORM STATUS_SET USING RT_EXTAB TYPE SLIS_T_EXTAB.
*BREAK-POINT.
  SET PF-STATUS 'ZSTANDARD530' EXCLUDING RT_EXTAB.
ENDFORM.                    "STATUS_SET
*&---------------------------------------------------------------------*
*&      FORM  USERCOMMAND
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->I_UCOMM    TEXT
*      -->I_SELFIELD TEXT
*----------------------------------------------------------------------*
FORM USERCOMMAND USING I_UCOMM I_SELFIELD TYPE SLIS_SELFIELD.

  DATA: LT_MESS_TAB      TYPE TAB_BDCMSGCOLL,
        LW_MESS_TAB      TYPE BDCMSGCOLL.

  DATA: LV_MODE    TYPE C VALUE 'N',
        LV_UPD     TYPE C VALUE 'S',
        LV_MSGTYP  TYPE C.
BREAK WANTANEE.
*&---------------------------------------------------------------------*
*&FOR CHECK = 'X' WHEN TICK CHECK BOX
*&---------------------------------------------------------------------*
  DATA : REF_GRID TYPE REF TO CL_GUI_ALV_GRID.

  IF REF_GRID IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        E_GRID = REF_GRID.
    CALL METHOD REF_GRID->CHECK_CHANGED_DATA.
  ENDIF.
*&---------------------------------------------------------------------*




  CASE I_UCOMM.
    WHEN '&UPDATE1'.
         BREAK WANTANEE.

*PARAMETERS: R_REV RADIOBUTTON GROUP GR1 DEFAULT 'X',
*            R_FICON  RADIOBUTTON GROUP GR1,
         IF R_REV IS NOT INITIAL.
*           BREAK-POINT.
                  LOOP AT GT_OUTPUT INTO WA_OUTPUT WHERE RESERVE_DESC IS NOT  INITIAL.


                              "CASE BANK NORMAL
                              UPDATE ZSDSFIT042
                              SET RESERVE_MAP = 'X'
                                  STATUS_AD  = WA_OUTPUT-STATUS_AD
                                  RESERVE_DATE = SY-DATUM
                                  RESERVE_TIME =  SY-TIMLO
                                  RESERVE_USNAM = SY-UNAME
                                  RESERVE_DESC = WA_OUTPUT-RESERVE_DESC
                              WHERE HBKID = WA_OUTPUT-HBKID
                                AND ZBANK_ITEM = WA_OUTPUT-ZBANK_ITEM.
                              COMMIT WORK.

                      ENDLOOP.
                      MESSAGE S000(38) WITH 'RESERVED COMPLETE'.

                      PERFORM REFERSH.
                      CALL METHOD REF_GRID->REFRESH_TABLE_DISPLAY.
                      CLEAR : REF_GRID.
         ELSEIF R_FICON IS NOT INITIAL.
               IF FLAG_AUTHO_USER EQ 'X'.
                       LOOP AT GT_OUTPUT INTO WA_OUTPUT WHERE ( RESERVE_DESC IS NOT  INITIAL OR STATUS_AD IS NOT INITIAL OR FI_CONFIRM_MAP IS NOT INITIAL ).
*BREAK-POINT.
                                UPDATE ZSDSFIT042
                                SET FI_CONFIRM_MAP = WA_OUTPUT-FI_CONFIRM_MAP
                                    RESERVE_MAP = 'X'
                                    TRNFER_NUMBER  = WA_OUTPUT-ZBANK_ITEM
                                    FYEAR_TRNFERNO = SY-DATUM+0(4)
                                    FI_CONFIRM_DATE = SY-DATUM
                                    FI_CONFIRM_TIME =  SY-TIMLO
                                    FI_CONFIRM_USNAM = SY-UNAME
                                    RESERVE_DESC = WA_OUTPUT-RESERVE_DESC
                                    STATUS_AD = WA_OUTPUT-STATUS_AD
                                WHERE HBKID = WA_OUTPUT-HBKID
                                  AND ZBANK_ITEM = WA_OUTPUT-ZBANK_ITEM.
                                COMMIT WORK.

                        ENDLOOP.
                        MESSAGE S000(38) WITH 'FI CONFIRM COMPLETE'.

                        PERFORM REFERSH.
                        CALL METHOD REF_GRID->REFRESH_TABLE_DISPLAY.
                        CLEAR : REF_GRID.
               ENDIF.
         ELSEIF R_UNREV IS NOT INITIAL.
                   LOOP AT GT_OUTPUT INTO WA_OUTPUT WHERE UNCHECK IS NOT INITIAL.


                              "CASE BANK NORMAL
                              UPDATE ZSDSFIT042
                              SET RESERVE_MAP = ''
                                  STATUS_AD  = ''
                                  RESERVE_DATE = '00000000'
                                  RESERVE_TIME =  '000000'
                                  RESERVE_USNAM = ''
                                  RESERVE_DESC = ''
                                  TRNFER_NUMBER = ''
                                  FYEAR_TRNFERNO = ''
                              WHERE HBKID = WA_OUTPUT-HBKID
                                AND ZBANK_ITEM = WA_OUTPUT-ZBANK_ITEM.
                              COMMIT WORK.

                      ENDLOOP.
                      MESSAGE S000(38) WITH 'CANCEL RESERVED COMPLETE'.

                      PERFORM REFERSH.
                      CALL METHOD REF_GRID->REFRESH_TABLE_DISPLAY.
                      CLEAR : REF_GRID.

         ELSEIF R_UNFICO IS NOT INITIAL.
                      LOOP AT GT_OUTPUT INTO WA_OUTPUT WHERE UNCHECK IS NOT INITIAL.

                                UPDATE ZSDSFIT042
                                SET FI_CONFIRM_MAP = ''
                                    FI_CONFIRM_DATE = '00000000'
                                    FI_CONFIRM_TIME =  '000000'
                                    FI_CONFIRM_USNAM = ''
                                    TRNFER_NUMBER = ''
                                    FYEAR_TRNFERNO = ''
                                    RESERVE_DESC = ''
                                WHERE HBKID = WA_OUTPUT-HBKID
                                  AND ZBANK_ITEM = WA_OUTPUT-ZBANK_ITEM.
                                COMMIT WORK.

                        ENDLOOP.
                        MESSAGE S000(38) WITH 'CANCEL FI CONFIRM COMPLETE'.

                        PERFORM REFERSH.
                        CALL METHOD REF_GRID->REFRESH_TABLE_DISPLAY.
                        CLEAR : REF_GRID.
         ELSEIF R_UNMAP IS NOT INITIAL.
                      LOOP AT GT_OUTPUT INTO WA_OUTPUT WHERE MAP_STATUS IS INITIAL.

                                UPDATE ZSDSFIT042
                                SET MAP_STATUS = ''
                                    MAP_TIME = '000000'
                                    FI_CONFIRM_TIME =  '000000'
                                    CLEARING_USER = ''
                                    TRNFER_NUMBER = ''
                                    FYEAR_TRNFERNO = ''
                                    FYEAR_CLEARING = ''
                                    FI_CLEARING_NO = ''
                                    DATE_CLEAR = '00000000'
                                    TIME_CLEAR =  '000000'
                                WHERE HBKID = WA_OUTPUT-HBKID
                                  AND ZBANK_ITEM = WA_OUTPUT-ZBANK_ITEM.
                                COMMIT WORK.

                        ENDLOOP.
                        MESSAGE S000(38) WITH 'CANCEL FI CONFIRM COMPLETE'.

                        PERFORM REFERSH.
                        CALL METHOD REF_GRID->REFRESH_TABLE_DISPLAY.
                        CLEAR : REF_GRID.
         ENDIF.

    WHEN OTHERS.

  ENDCASE.
*
*  I_SELFIELD-REFRESH = 'X'.
*  I_SELFIELD-COL_STABLE = 'X'.
*  I_SELFIELD-ROW_STABLE = 'X'.

ENDFORM.                    "USERCOMMAND

*&---------------------------------------------------------------------*
*&      FORM  F_FILL_FIELDCAT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM F_FILL_FIELDCAT .
  DATA: LV_COLUMN TYPE I.



IF R_REPT IS  INITIAL.

        IF R_FICON IS NOT INITIAL.
              IF FLAG_AUTHO_USER EQ 'X'.
*               -FI_CONFIRM_MAP
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'FI_CONFIRM_MAP' LV_COLUMN 'FI CONFIRM' SPACE 10 'X' 'X'.

              ELSE.
*               -FI_CONFIRM_MAP
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'FI_CONFIRM_MAP' LV_COLUMN 'FI CONFIRM' SPACE 10 SPACE 'X'.
              ENDIF.
        ENDIF.

*         -RESERVE MAP
          ADD 1 TO LV_COLUMN.
          M_FILL_CAT 'GT_OUTPUT' 'RESERVE_MAP' LV_COLUMN 'RESERVE' SPACE 10 SPACE 'X'.

ELSE.

        IF FLAG_AUTHO_USER EQ 'X'.
*         -FI_CONFIRM_MAP
          ADD 1 TO LV_COLUMN.
          M_FILL_CAT 'GT_OUTPUT' 'FI_CONFIRM_MAP' LV_COLUMN 'FI CONFIRM' SPACE 10 SPACE 'X'.

        ELSE.
*         -FI_CONFIRM_MAP
          ADD 1 TO LV_COLUMN.
          M_FILL_CAT 'GT_OUTPUT' 'FI_CONFIRM_MAP' LV_COLUMN 'FI CONFIRM' SPACE 10 SPACE 'X'.
        ENDIF.

*         -RESERVE MAP
          ADD 1 TO LV_COLUMN.
          M_FILL_CAT 'GT_OUTPUT' 'RESERVE_MAP' LV_COLUMN 'RESERVE' SPACE 10 SPACE 'X'.

ENDIF.

IF R_UNREV IS NOT INITIAL.
*        -UN REVERSE UNCHECK
          ADD 1 TO LV_COLUMN.
          M_FILL_CAT 'GT_OUTPUT' 'UNCHECK' LV_COLUMN 'UN-RESERVE' SPACE 10 'X' 'X'.
ELSEIF R_UNFICO IS NOT INITIAL.
*        -UN REVERSE UNCHECK
          ADD 1 TO LV_COLUMN.
          M_FILL_CAT 'GT_OUTPUT' 'UNCHECK' LV_COLUMN 'UN-FI CONFIRM' SPACE 10 'X' 'X'.

ENDIF.

**-STATUS MAP
          ADD 1 TO LV_COLUMN.
          M_FILL_CAT 'GT_OUTPUT' 'MAP_STATUS' LV_COLUMN 'STATUS MAP' SPACE 10 'X' 'X'.


 IF P_BANK IS NOT INITIAL.

               IF P_BANK = 'BAY01'.
*              -BANK CODE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'HBKID' LV_COLUMN 'BANK CODE' SPACE 10 SPACE SPACE.
*              *-ITEM
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'ZBANK_ITEM' LV_COLUMN 'ITEM' SPACE 10 SPACE SPACE.
*              *-DATE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'CHEQUE_DATE' LV_COLUMN 'วันที่' SPACE 10 SPACE SPACE.
*              *TRAN_CODE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'TRAN_CODE' LV_COLUMN 'รหัส' SPACE 15 SPACE SPACE.
*              *-DESCRIPTION
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'BANK_DESC' LV_COLUMN 'หมายเลขอ้างอิง' SPACE 20 SPACE SPACE.
*              *-DEBIT
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'DEBIT_AMT' LV_COLUMN 'เดบิต' SPACE 20 SPACE SPACE.

*              *-CREDIT
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'RECEIVED_AMOUNT' LV_COLUMN 'เครดิต' SPACE 20 SPACE SPACE.
*              *-BALANCE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'BAL_AMT' LV_COLUMN 'BALANCE' SPACE 20 SPACE SPACE.
*              *-CHANNEL
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'CHANNEL' LV_COLUMN 'ช่องทาง' SPACE 20 SPACE SPACE.
*              *-BRANCH CODE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'BRANCH_CODE' LV_COLUMN 'รหัสสาขา' SPACE 20 SPACE SPACE.


                ELSEIF P_BANK = 'BBL01'.

*              -BANK CODE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'HBKID' LV_COLUMN 'BANK CODE' SPACE 10 SPACE SPACE.
*              *-ITEM
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'ZBANK_ITEM' LV_COLUMN 'ITEM' SPACE 10 SPACE SPACE.
*              *-TRAN DATE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'TRAN_DATE' LV_COLUMN 'TRAN DATE' SPACE 10 SPACE SPACE.
*              *-VALUE DATE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'CHEQUE_DATE' LV_COLUMN 'VALUE DATE' SPACE 10 SPACE SPACE.
*              *-DESCRIPTION
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'BANK_DESC' LV_COLUMN 'DESCRIPTION' SPACE 20 SPACE SPACE.
*              *TRAN_CODE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'TRAN_CODE' LV_COLUMN 'TRAN CODE' SPACE 15 SPACE SPACE.
*              *CHEQUE NO.
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'CHEQUE_NO' LV_COLUMN 'CHEQUE NO.' SPACE 15 SPACE SPACE.
*              *-DEBIT
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'DEBIT_AMT' LV_COLUMN 'DEBIT' SPACE 20 SPACE SPACE.

*              *-CREDIT
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'RECEIVED_AMOUNT' LV_COLUMN 'CREDIT' SPACE 20 SPACE SPACE.
*              *-BALANCE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'BAL_AMT' LV_COLUMN 'BALANCE' SPACE 20 SPACE SPACE.
*              *-CHANNEL
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'CHANNEL' LV_COLUMN 'CHANNEL' SPACE 20 SPACE SPACE.
*              *-TERMINAL ID
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'TERMINAL_ID' LV_COLUMN 'TERMINAL ID' SPACE 20 SPACE SPACE.
*              *-BRANCH
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'BRANCH_NAME' LV_COLUMN 'BRANCH' SPACE 20 SPACE SPACE.
*              *-SENDER BANK
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'SENDER_BANK' LV_COLUMN 'SENDER BANK' SPACE 20 SPACE SPACE.
*              *-SENDER BRANCH
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'SENDER_BRANCH' LV_COLUMN 'SENDER BRANCH' SPACE 20 SPACE SPACE.
*              *-SENDER ACCOUNT NUMBER
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'SENDER_ACC_NO' LV_COLUMN 'SENDER ACC.NO.' SPACE 20 SPACE SPACE.



                ELSEIF P_BANK = 'BTMU'.

*              -BANK CODE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'HBKID' LV_COLUMN 'BANK CODE' SPACE 10 SPACE SPACE.
*              *-ITEM
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'ZBANK_ITEM' LV_COLUMN 'ITEM' SPACE 10 SPACE SPACE.
*              *-RECORD TYPE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'RECORD_TYPE' LV_COLUMN 'RECORD TYPE' SPACE 10 SPACE SPACE.
*              *-BRANCH
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'BRANCH_NAME' LV_COLUMN 'BRANCH NAME' SPACE 20 SPACE SPACE.
*              *-SWIFT_BIC
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'SWIFT_BIC' LV_COLUMN 'SWIFT BIC' SPACE 20 SPACE SPACE.
*              *-ACCOUNT TYPE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'ACC_TYPE' LV_COLUMN 'ACCOUNT TYPE' SPACE 20 SPACE SPACE.
*              *-CURRENCY
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'CURRENCY' LV_COLUMN 'CURRENCY' SPACE 20 SPACE SPACE.
*              *-ACCOUNT NO.
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'SDS_ACC_NO' LV_COLUMN 'SDS ACCOUNT NO.' SPACE 20 SPACE SPACE.
*              *-ACC_NAME
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'ACC_NAME' LV_COLUMN 'SDS ACCOUNT NAME' SPACE 20 SPACE SPACE.
*              *-BOOKING DATE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'TRAN_DATE' LV_COLUMN 'BOOKING DATE' SPACE 10 SPACE SPACE.
*              *-VALUE DATE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'CHEQUE_DATE' LV_COLUMN 'VALUE DATE' SPACE 10 SPACE SPACE.
*              *-TRANSACTION TYPE NAME
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'TRAN_TYPE_NAME' LV_COLUMN 'TRANS. TYPE NAME' SPACE 10 SPACE SPACE.
*              *-DEBIT
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'DEBIT_AMT' LV_COLUMN 'DEBIT' SPACE 20 SPACE SPACE.
*              *-CREDIT
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'RECEIVED_AMOUNT' LV_COLUMN 'CREDIT' SPACE 20 SPACE SPACE.
*              *-OPENING / CLOSING BALANCE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'BAL_AMT' LV_COLUMN 'OPENING / CLOSING BALANCE' SPACE 20 SPACE SPACE.

*              *-CUSTOMER REFERENCE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'CUST_REFER' LV_COLUMN 'CUSTOMER REFER.' SPACE 10 SPACE SPACE.
*              *-DETAIL INFORMATION
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'BANK_DESC' LV_COLUMN 'DETAIL INFORMATION' SPACE 20 SPACE SPACE.
*              *-BANK REFERENCE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT' 'BANK_REFER' LV_COLUMN 'BANK REFERENCE' SPACE 20 SPACE SPACE.


                ELSEIF P_BANK = 'KBK01'.



*                  -BANK CODE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'HBKID' LV_COLUMN 'BANK CODE' SPACE 10 SPACE SPACE.
*                  *-ITEM
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'ZBANK_ITEM' LV_COLUMN 'ITEM' SPACE 10 SPACE SPACE.
*                  *-VALUE DATE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'CHEQUE_DATE' LV_COLUMN 'วันที่รายการมีผล' SPACE 10 SPACE SPACE.
*                  *-DETAIL INFORMATION
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'BANK_DESC' LV_COLUMN 'รายละเอียดรายการ' SPACE 20 SPACE SPACE.
*                  *-CHEQUE NO
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'CHEQUE_NO' LV_COLUMN 'เลขที่เช็ค' SPACE 10 SPACE SPACE.
*                  *-DEBIT
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'DEBIT_AMT' LV_COLUMN 'จำนวนเงินหักบัญชี' SPACE 20 SPACE SPACE.
*                  *-CREDIT
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'RECEIVED_AMOUNT' LV_COLUMN 'จำนวนเงินนำฝากเข้าบัญชี' SPACE 20 SPACE SPACE.
*                  *-OPENING / CLOSING BALANCE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'BAL_AMT' LV_COLUMN 'ยอดคงเหลือ' SPACE 20 SPACE SPACE.
*                   *-BRANCH CODE
                     ADD 1 TO LV_COLUMN.
                     M_FILL_CAT 'GT_OUTPUT' 'BRANCH_CODE' LV_COLUMN 'สาขา' SPACE 20 SPACE SPACE.
*                  *-BOOKING DATE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'TRAN_DATE' LV_COLUMN 'วันที่ทำรายการ' SPACE 10 SPACE SPACE.
*                  *-DETAIL
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'KBANK_DETAIL' LV_COLUMN 'DETAIL' SPACE 10 SPACE SPACE.


                ELSEIF P_BANK = 'KTB01'.

*                  -BANK CODE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'HBKID' LV_COLUMN 'BANK CODE' SPACE 10 SPACE SPACE.
*                  *-ITEM
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'ZBANK_ITEM' LV_COLUMN 'ITEM' SPACE 10 SPACE SPACE.
*                  *-DATE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'CHEQUE_DATE' LV_COLUMN 'DATE' SPACE 10 SPACE SPACE.
*                  *-TELLER ID
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'TERMINAL_ID' LV_COLUMN 'TELLER ID' SPACE 10 SPACE SPACE.
*                   *TRAN_CODE
                     ADD 1 TO LV_COLUMN.
                     M_FILL_CAT 'GT_OUTPUT' 'TRAN_CODE' LV_COLUMN 'TRAN CODE' SPACE 15 SPACE SPACE.
*                  *-DESCRIPTION
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'BANK_DESC' LV_COLUMN 'DESCRIPTION' SPACE 20 SPACE SPACE.
*                  *-CHEQUE NO
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'CHEQUE_NO' LV_COLUMN 'CHEQUE NO.' SPACE 10 SPACE SPACE.
*                  *-AMOUNT
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'RECEIVED_AMOUNT' LV_COLUMN 'AMOUNT' SPACE 20 SPACE SPACE.
*                  *-BALANCE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'BAL_AMT' LV_COLUMN 'BALANCE' SPACE 20 SPACE SPACE.
*                   *-INIT BR
                     ADD 1 TO LV_COLUMN.
                     M_FILL_CAT 'GT_OUTPUT' 'BRANCH_CODE' LV_COLUMN 'INIT BR' SPACE 20 SPACE SPACE.



                ELSEIF P_BANK = 'SCB01'.

*                  -BANK CODE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'HBKID' LV_COLUMN 'BANK CODE' SPACE 10 SPACE SPACE.
*                  *-ITEM
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'ZBANK_ITEM' LV_COLUMN 'ITEM' SPACE 10 SPACE SPACE.
*                  *-SDS ACC. NO.
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'SDS_ACC_NO' LV_COLUMN 'SDS ACC. NO.' SPACE 10 SPACE SPACE.
*                  *-DATE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'CHEQUE_DATE' LV_COLUMN 'DATE' SPACE 10 SPACE SPACE.
*                   *TRAN_CODE
                     ADD 1 TO LV_COLUMN.
                     M_FILL_CAT 'GT_OUTPUT' 'TRAN_CODE' LV_COLUMN 'TRAN CODE' SPACE 10 SPACE SPACE.
*                   *CHANNEL
                     ADD 1 TO LV_COLUMN.
                     M_FILL_CAT 'GT_OUTPUT' 'CHANNEL' LV_COLUMN 'CHANNEL' SPACE 10 SPACE SPACE.
*                  *-CHEQUE NO
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'CHEQUE_NO' LV_COLUMN 'CHEQUE NO.' SPACE 10 SPACE SPACE.
*                  *-DEBIT
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'DEBIT_AMT' LV_COLUMN 'DEBIT' SPACE 20 SPACE SPACE.
*                  *-CREDIT
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'RECEIVED_AMOUNT' LV_COLUMN 'CREDIT' SPACE 20 SPACE SPACE.
*                  *-BALANCE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'BAL_AMT' LV_COLUMN 'BALANCE' SPACE 20 SPACE SPACE.
*                  *-DESCRIPTION
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'BANK_DESC' LV_COLUMN 'DESCRIPTION' SPACE 20 SPACE SPACE.



                ELSEIF P_BANK = 'TMB01'.

*                  -BANK CODE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'HBKID' LV_COLUMN 'BANK CODE' SPACE 10 SPACE SPACE.
*                  *-ITEM
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'ZBANK_ITEM' LV_COLUMN 'ITEM' SPACE 10 SPACE SPACE.
*                  *-SDS ACC. NO.
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'SDS_ACC_NO' LV_COLUMN 'SDS เลขที่บัญชี' SPACE 10 SPACE SPACE.
*                  *-BOOKING DATE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'TRAN_DATE' LV_COLUMN 'วันที่ทำรายการ' SPACE 10 SPACE SPACE.
*                  *-DATE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'CHEQUE_DATE' LV_COLUMN 'วันที่มีผล' SPACE 10 SPACE SPACE.
*                   *TRAN_CODE
                     ADD 1 TO LV_COLUMN.
                     M_FILL_CAT 'GT_OUTPUT' 'TRAN_CODE' LV_COLUMN 'รหัสประเภทรายการ' SPACE 10 SPACE SPACE.
*                  *-CHEQUE NO
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'CHEQUE_NO' LV_COLUMN 'เลขที่เช็ค' SPACE 10 SPACE SPACE.

*                  *-DEBIT
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'DEBIT_AMT' LV_COLUMN 'ถอน' SPACE 20 SPACE SPACE.
*                  *-CREDIT
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'RECEIVED_AMOUNT' LV_COLUMN 'ฝาก' SPACE 20 SPACE SPACE.
*                  *-BALANCE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'BAL_AMT' LV_COLUMN 'ยอดคงเหลือ' SPACE 20 SPACE SPACE .
*                  *-TELLER ID
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'TERMINAL_ID' LV_COLUMN 'ช่องทางทำรายการ' SPACE 10 SPACE SPACE.
*                  *-DESCRIPTION
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'BANK_DESC' LV_COLUMN 'รายละเอียดการทำธุรกรรม' SPACE 20 SPACE SPACE.

                ELSEIF P_BANK = 'UOB'.

*                  -BANK CODE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'HBKID' LV_COLUMN 'BANK CODE' SPACE 10 SPACE SPACE.
*                  *-ITEM
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'ZBANK_ITEM' LV_COLUMN 'ITEM' SPACE 10 SPACE SPACE.
*                  *-SDS ACC. NO.
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'SDS_ACC_NO' LV_COLUMN 'SDS ACC.NO.' SPACE 10 SPACE SPACE.
*                  *-DATE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'CHEQUE_DATE' LV_COLUMN 'VALUE DATE' SPACE 10 SPACE SPACE.
*                  *-BOOKING DATE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'TRAN_DATE' LV_COLUMN 'DATE' SPACE 10 SPACE SPACE.
*                  *-DESCRIPTION
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'BANK_DESC' LV_COLUMN 'DESCRIPTION' SPACE 20 SPACE SPACE.
*                  *-CHEQUE NO
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'CHEQUE_NO' LV_COLUMN 'CHEQUE NO' SPACE 10 SPACE SPACE.
*                  *-CREDIT
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'RECEIVED_AMOUNT' LV_COLUMN 'DEPOSIT' SPACE 20 SPACE SPACE.
*                  *-BALANCE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'BAL_AMT' LV_COLUMN 'LEDGER BALANCE' SPACE 20 SPACE SPACE.


                ELSEIF P_BANK = 'SMB01'.

*                  -BANK CODE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'HBKID' LV_COLUMN 'BANK CODE' SPACE 10 SPACE SPACE.
*                  *-ITEM
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'ZBANK_ITEM' LV_COLUMN 'ITEM' SPACE 10 SPACE SPACE.
*                   *-BRANCH CODE
                     ADD 1 TO LV_COLUMN.
                     M_FILL_CAT 'GT_OUTPUT' 'BRANCH_CODE' LV_COLUMN 'BRANCH CODE' SPACE 20 SPACE SPACE.

*                  *-ACC_NAME
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'ACC_NAME' LV_COLUMN 'SDS ACCOUNT NAME' SPACE 20 SPACE SPACE.
*                  *-DATE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'TRAN_DATE' LV_COLUMN 'DATE FROM' SPACE 10 SPACE SPACE.
*                  *-DATE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'TRAN_DATE_TO' LV_COLUMN 'DATE TO' SPACE 10 SPACE SPACE.
*                  *-SDS ACC. NO.
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'SDS_ACC_NO' LV_COLUMN 'SDS ACC. NO.' SPACE 10 SPACE SPACE.
*                  *-DATE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'CHEQUE_DATE' LV_COLUMN 'DATE' SPACE 10 SPACE SPACE.
*                  *-DEBIT
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'DEBIT_AMT' LV_COLUMN 'DEBIT' SPACE 20 SPACE SPACE.
*                  *-CREDIT
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'RECEIVED_AMOUNT' LV_COLUMN 'CREDIT' SPACE 20 SPACE SPACE.
*                  *-BALANCE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'SMBC_BAL_AMT' LV_COLUMN 'BALANCE' SPACE 20 SPACE SPACE.
*                  *-DESCRIPTION
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'BANK_DESC' LV_COLUMN 'DESCRIPTION' SPACE 20 SPACE SPACE.
*                  *-CHEQUE NO
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'CHEQUE_NO' LV_COLUMN 'CHEQUE NO.' SPACE 10 SPACE SPACE.
*                  *-REMARKS
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'SMBC_REMARK' LV_COLUMN 'REMARKS' SPACE 20 SPACE SPACE.

*                  *-AVAILABLEBALANCE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT' 'BAL_AMT' LV_COLUMN 'AVAILABLEBALANCE' SPACE 20 SPACE SPACE.




                ENDIF.
    ELSE.

*             -BANK CODE
               ADD 1 TO LV_COLUMN.
               M_FILL_CAT 'GT_OUTPUT' 'HBKID' LV_COLUMN 'BANK CODE' SPACE 10 SPACE SPACE.
*             *-ITEM
               ADD 1 TO LV_COLUMN.
               M_FILL_CAT 'GT_OUTPUT' 'ZBANK_ITEM' LV_COLUMN 'ITEM' SPACE 10 SPACE SPACE.
*             *-DATE
               ADD 1 TO LV_COLUMN.
               M_FILL_CAT 'GT_OUTPUT' 'CHEQUE_DATE' LV_COLUMN 'DATE' SPACE 10 SPACE SPACE.
*             *-CREDIT
               ADD 1 TO LV_COLUMN.
               M_FILL_CAT 'GT_OUTPUT' 'RECEIVED_AMOUNT' LV_COLUMN 'CREDIT' SPACE 20 SPACE SPACE.
*             *-DESCRIPTION
               ADD 1 TO LV_COLUMN.
               M_FILL_CAT 'GT_OUTPUT' 'BANK_DESC' LV_COLUMN 'DESCRIPTION' SPACE 20 SPACE SPACE.
*              *-UPLOAD DATE
               ADD 1 TO LV_COLUMN.
               M_FILL_CAT 'GT_OUTPUT' 'ERDAT' LV_COLUMN 'UPLOAD DATE' SPACE 20 SPACE SPACE.



    ENDIF.

 IF R_REPT IS  INITIAL.
*        *-RESERVE DESC
        ADD 1 TO LV_COLUMN.
        M_FILL_CAT 'GT_OUTPUT' 'RESERVE_DESC' LV_COLUMN 'RESERVE DETAIL' SPACE 20 'X' SPACE.
        ADD 1 TO LV_COLUMN.
        M_FILL_CAT 'GT_OUTPUT' 'STATUS_AD' LV_COLUMN 'STATUS ADVANCE' SPACE 20 'X' 'X'.

 ELSE.
*        *-RESERVE DESC
        ADD 1 TO LV_COLUMN.
        M_FILL_CAT 'GT_OUTPUT' 'RESERVE_DESC' LV_COLUMN 'RESERVE DETAIL' SPACE 20 SPACE SPACE.
        ADD 1 TO LV_COLUMN.
        M_FILL_CAT 'GT_OUTPUT' 'STATUS_AD' LV_COLUMN 'STATUS ADVANCE' SPACE 20 SPACE 'X'.
 ENDIF.

* *-RESERVE_DATE
  ADD 1 TO LV_COLUMN.
  M_FILL_CAT 'GT_OUTPUT' 'RESERVE_DATE' LV_COLUMN 'RESERVE DATE' SPACE 20 SPACE SPACE.

* *-RESERVE_TIME
  ADD 1 TO LV_COLUMN.
  M_FILL_CAT 'GT_OUTPUT' 'RESERVE_TIME' LV_COLUMN 'RESERVE TIME' SPACE 20 SPACE SPACE.

* *-RESERVE_USNAM
  ADD 1 TO LV_COLUMN.
  M_FILL_CAT 'GT_OUTPUT' 'RESERVE_USNAM' LV_COLUMN 'RESERVE USER' SPACE 20 SPACE SPACE.
* *-FI_CONFIRM_DATE
  ADD 1 TO LV_COLUMN.
  M_FILL_CAT 'GT_OUTPUT' 'FI_CONFIRM_DATE' LV_COLUMN 'FI CONFIRM DATE' SPACE 20 SPACE SPACE.
* *-FI_CONFIRM_TIME
  ADD 1 TO LV_COLUMN.
  M_FILL_CAT 'GT_OUTPUT' 'FI_CONFIRM_TIME' LV_COLUMN 'FI CONFIRM TIME' SPACE 20 SPACE SPACE.
* *-FI_CONFIRM_USNAM
  ADD 1 TO LV_COLUMN.
  M_FILL_CAT 'GT_OUTPUT' 'FI_CONFIRM_USNAM' LV_COLUMN 'FI CONFIRM USER' SPACE 20 SPACE SPACE.


**-MAP DATE
 ADD 1 TO LV_COLUMN.
 M_FILL_CAT 'GT_OUTPUT' 'MAP_DATE' LV_COLUMN 'MAP DATE' SPACE 20 SPACE SPACE.

**-MAP TIME
 ADD 1 TO LV_COLUMN.
 M_FILL_CAT 'GT_OUTPUT' 'MAP_TIME' LV_COLUMN 'MAP TIME' SPACE 20 SPACE SPACE.
* *-TRNFER_NUMBER
  ADD 1 TO LV_COLUMN.
  M_FILL_CAT 'GT_OUTPUT' 'TRNFER_NUMBER' LV_COLUMN 'TF Number' SPACE 20 SPACE SPACE.
* *-FYEAR_TRNFERNO
  ADD 1 TO LV_COLUMN.
  M_FILL_CAT 'GT_OUTPUT' 'FYEAR_TRNFERNO' LV_COLUMN 'TF Year' SPACE 20 SPACE SPACE.
* *-FI_CLEARING_NO
  ADD 1 TO LV_COLUMN.
  M_FILL_CAT 'GT_OUTPUT' 'FI_CLEARING_NO' LV_COLUMN 'Clearing no' SPACE 20 SPACE SPACE.
* *-FYEAR_CLEARING
  ADD 1 TO LV_COLUMN.
  M_FILL_CAT 'GT_OUTPUT' 'FYEAR_CLEARING' LV_COLUMN 'Clearing FY' SPACE 20 SPACE SPACE.
* *-DATE_CLEAR
  ADD 1 TO LV_COLUMN.
  M_FILL_CAT 'GT_OUTPUT' 'DATE_CLEAR' LV_COLUMN 'Clearing Date' SPACE 20 SPACE SPACE.
* *-TIME_CLEAR
  ADD 1 TO LV_COLUMN.
  M_FILL_CAT 'GT_OUTPUT' 'TIME_CLEAR' LV_COLUMN 'Clearing Time' SPACE 20 SPACE SPACE.

* *-TIME_CLEAR
  ADD 1 TO LV_COLUMN.
  M_FILL_CAT 'GT_OUTPUT' 'MT940_DOC' LV_COLUMN 'MT940 Doc' SPACE 20 SPACE SPACE.
* *-FYEAR_MT940
  ADD 1 TO LV_COLUMN.
  M_FILL_CAT 'GT_OUTPUT' 'FYEAR_MT940' LV_COLUMN 'MT940 FY' SPACE 20 SPACE SPACE.
* *-CLEARING_USER
  ADD 1 TO LV_COLUMN.
  M_FILL_CAT 'GT_OUTPUT' 'CLEARING_USER' LV_COLUMN 'Clearing User' SPACE 20 SPACE SPACE.


ENDFORM.                    " F_FILL_FIELDCAT
*&---------------------------------------------------------------------*
*&      FORM  F_PREPARE_LAYOUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM F_PREPARE_LAYOUT .
*  GS_LAYOUT-BOX_FIELDNAME      = 'SEL'.
  GS_LAYOUT-ZEBRA              = 'X'.     " STRIPED PATTERN
  GS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  GS_LAYOUT-INFO_FIELDNAME    = 'LINE_COLOR'.
ENDFORM.                    " F_PREPARE_LAYOUT

*&---------------------------------------------------------------------*
*&      FORM  F_ADD_LIST_BANK
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_ADD_LIST_BANK.
*   HBKID             TYPE  ZSDSFIT044-HBKID1,  "BANK CODE
*         BANKA             TYPE  BNKA-BANKA,  "BANK CODE
*  BREAK WANTANEE.

       SELECT HBKID BANK_NAME
       INTO TABLE GT_BANK
       FROM ZSDSFIT044 .

  LOOP AT GT_BANK INTO GS_BANK.. "GM ONLY
    VALUE-KEY = GS_BANK-HBKID.
    VALUE-TEXT = GS_BANK-BANK_NAME.
*    CONCATENATE GS_BANK-BANKL1 GS_BANK-BANKA  INTO VALUE-TEXT SEPARATED BY '-'.
    APPEND VALUE TO LIST.
  ENDLOOP.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID     = 'P_BANK'
      VALUES = LIST.

ENDFORM.                    " F_CHECK_ERROR


*&---------------------------------------------------------------------*
*&      FORM  REFERSH
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM REFERSH .

   PERFORM F_GET_DATA.
   PERFORM F_MAP_DATA.

ENDFORM.                    " REFERSH

*&---------------------------------------------------------------------*
*&      FORM  F_CHECK_AUTHORIZE_USER
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM F_CHECK_AUTHORIZE_USER.
       SELECT BNAME USERGROUP
       INTO TABLE GT_USGRP_USER
       FROM USGRP_USER
       WHERE USERGROUP = 'MAP_BANK'.

       READ TABLE GT_USGRP_USER INTO GW_USGRP_USER WITH KEY BNAME = SY-UNAME.
           IF SY-SUBRC = 0.
              FLAG_AUTHO_USER = 'X'.
           ENDIF.
ENDFORM.
