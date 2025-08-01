*&---------------------------------------------------------------------*
*& Report ZSDSFIR0510
*  Author             : Wantanee Prateep na thalang
*  Add-on ID          :
*  Description        : Report checkk statement bank
*  Purpose            :
*  Copied from        : ZFIAP_CHK_BANK_MAP_COLLECTOR
*  Restriction        :
*&---------------------------------------------------------------------*
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
REPORT ZSDSFIR0510.
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

TYPES: BEGIN OF GY_ZSDSFIT042,
      MANDT               TYPE  ZSDSFIT042-MANDT,    "CLIENT
      HBKID               TYPE  ZSDSFIT042-HBKID,    "BANK KEYS
      ZBANK_ITEM          TYPE  ZSDSFIT042-ZBANK_ITEM,    "STATEMENT BANK ITEM
      CHEQUE_DATE         TYPE  ZSDSFIT042-CHEQUE_DATE,    "DATE FOR PAYMENT BY CHEQUE
      DEBIT_AMT           TYPE  ZSDSFIT042-DEBIT_AMT,    "AMOUNT
      RECEIVED_AMOUNT     TYPE  ZSDSFIT042-RECEIVED_AMOUNT,    "AMOUNT
      FLAG_BANK_FEE       TYPE  ZSDSFIT042-FLAG_BANK_FEE,    "FLAG BANK FEE
      LINE_MAP_BANK_FE    TYPE  ZSDSFIT042-LINE_MAP_BANK_FE,    "STATEMENT BANK ITEM
      BANK_DESC           TYPE  ZSDSFIT042-BANK_DESC,    "BANK DESCRIPTION
      BAL_AMT             TYPE  ZSDSFIT042-BAL_AMT,    "AMOUNT
      CHANNEL             TYPE  ZSDSFIT042-CHANNEL,    "CHANNEL
      BRANCH_CODE         TYPE  ZSDSFIT042-BRANCH_CODE,    "BRANCH CODE
      BRANCH_NAME         TYPE  ZSDSFIT042-BRANCH_NAME,    "BRANCH NAME
      TERMINAL_ID         TYPE  ZSDSFIT042-TERMINAL_ID,    "TERMINAL ID
      CHEQUE_NO           TYPE  ZSDSFIT042-CHEQUE_NO,    "CHEQUE NO
      TRAN_CODE           TYPE  ZSDSFIT042-TRAN_CODE,    "TRAN CODE
      SENDER_BANK         TYPE  ZSDSFIT042-SENDER_BANK,    "SENDER BANK
      SENDER_BRANCH       TYPE  ZSDSFIT042-SENDER_BRANCH,    "SENDER BRANCH
      SENDER_ACC_NO       TYPE  ZSDSFIT042-SENDER_ACC_NO,    "SENDER ACCOUNT NUMBER
      SENDER_ACC_NAME     TYPE  ZSDSFIT042-SENDER_ACC_NAME,    "SENDER ACCOUNT NAME
      SDS_ACC_NO          TYPE  ZSDSFIT042-SDS_ACC_NO,    "SDS ACCOUNT NO
      TRAN_DATE           TYPE  ZSDSFIT042-TRAN_DATE,    "TRANSECTION DATE OR BOOKING DATE
      TRAN_DATE_TO        TYPE  ZSDSFIT042-TRAN_DATE_TO,    "TRANSECTION DATE TO OR BOOKING DATE TO
      SMBC_REMARK         TYPE  ZSDSFIT042-SMBC_REMARK,    "SMBC REMARK
      SMBC_BAL_AMT        TYPE  ZSDSFIT042-SMBC_BAL_AMT,    "AMOUNT
      RECORD_TYPE         TYPE  ZSDSFIT042-RECORD_TYPE,    "RECORD TYPE
      SWIFT_BIC           TYPE  ZSDSFIT042-SWIFT_BIC,    "SWIFT BIC
      ACC_TYPE            TYPE  ZSDSFIT042-ACC_TYPE,    "ACCOUNT TYPE
      CURRENCY            TYPE  ZSDSFIT042-CURRENCY,    "CURRENCY
      ACC_NAME            TYPE  ZSDSFIT042-ACC_NAME,    "ACCOUNT NAME
      TRAN_TYPE_NAME      TYPE  ZSDSFIT042-TRAN_TYPE_NAME,    "TRANS TYPE NAME
      CUST_REFER          TYPE  ZSDSFIT042-CUST_REFER,    "CUST REFER
      BANK_REFER          TYPE  ZSDSFIT042-BANK_REFER,    "BANK REFER
      ERDAT               TYPE  ZSDSFIT042-ERDAT,    "DATE ON WHICH RECORD WAS CREATED
      ERZET               TYPE  ZSDSFIT042-ERZET,    "ENTRY TIME
      USNAM               TYPE  ZSDSFIT042-USNAM,    "USER NAME
      TRANF               TYPE  ZSDSFIT042-TRANF,    "TRANSFER NO.
      RUN_ID              TYPE  ZSDSFIT042-RUN_ID,    "RUN_ID
      PAYIN               TYPE  ZSDSFIT042-PAYIN,    "PAYIN
      MAP_STATUS          TYPE  ZSDSFIT042-MAP_STATUS,    "MAPPING STATUS WITH COLLECTOR
      MAP_DATE            TYPE  ZSDSFIT042-MAP_DATE,    "MAPPING DATE WITH COLLECTOR
      MAP_TIME            TYPE  ZSDSFIT042-MAP_TIME,    "MAPPING TIME WITH COLLECTOR
      STATUS_LOCK         TYPE  ZSDSFIT042-STATUS_LOCK,    "STATUS LOCK DATA
      LOCK_ERDAT          TYPE  ZSDSFIT042-LOCK_ERDAT,    "DATE ON WHICH RECORD WAS CREATED
      LOCK_ERZET          TYPE  ZSDSFIT042-LOCK_ERZET,    "ENTRY TIME
      LOCK_USNAM          TYPE  ZSDSFIT042-LOCK_USNAM,    "USER NAME
      B_VALUE_TIME        TYPE  ZSDSFIT042-B_VALUE_TIME,    "USER NAME
      MAP_USNAM           TYPE  ZSDSFIT042-MAP_USNAM,    "USER NAME
      RESERVE_MAP         TYPE  ZSDSFIT042-RESERVE_MAP,    "STATUS RESERVE BY USER
      RESERVE_DATE        TYPE  ZSDSFIT042-RESERVE_DATE,    "DATE ON WHICH RECORD WAS CREATED
      RESERVE_TIME        TYPE  ZSDSFIT042-RESERVE_TIME,    "ENTRY TIME
      RESERVE_USNAM       TYPE  ZSDSFIT042-RESERVE_USNAM,    "USER NAME
      RESERVE_DESC        TYPE  ZSDSFIT042-RESERVE_DESC,    "RESEVE DETAIL
      FI_CONFIRM_MAP      TYPE  ZSDSFIT042-FI_CONFIRM_MAP,    "FI CONFRIM
      FI_CONFIRM_DATE     TYPE  ZSDSFIT042-FI_CONFIRM_DATE,    "DATE ON WHICH RECORD WAS CREATED
      FI_CONFIRM_TIME     TYPE  ZSDSFIT042-FI_CONFIRM_TIME,    "ENTRY TIME
      FI_CONFIRM_USNAM    TYPE  ZSDSFIT042-FI_CONFIRM_USNAM,    "USER NAME
      STATUS_AD           TYPE  ZSDSFIT042-STATUS_AD,    "STATUS CLEAR EXPERIMENT
      STATUS_ACC_RECON    TYPE  ZSDSFIT042-STATUS_ACC_RECON,    "STATUS ACCOUNT RECONCILE
      RECON_DATE          TYPE  ZSDSFIT042-RECON_DATE,    "DATE ON WHICH RECORD WAS CREATED
      RECON_TIME          TYPE  ZSDSFIT042-RECON_TIME,    "ENTRY TIME
      RECON_USER          TYPE  ZSDSFIT042-RECON_USER,    "USER NAME
      KBANK_DETAIL        TYPE  ZSDSFIT042-KBANK_DETAIL,    "KBANK_DETAIL "CH1  ADD BY WANTANEE 20210610
      TRNFER_NUMBER       TYPE  ZSDSFIT042-TRNFER_NUMBER,
      FYEAR_TRNFERNO      TYPE  ZSDSFIT042-FYEAR_TRNFERNO,
      FI_CLEARING_NO      TYPE  ZSDSFIT042-FI_CLEARING_NO,
      FYEAR_CLEARING      TYPE  ZSDSFIT042-FYEAR_CLEARING,
      DATE_CLEAR          TYPE  ZSDSFIT042-DATE_CLEAR,
      TIME_CLEAR          TYPE  ZSDSFIT042-TIME_CLEAR,
      MT940_DOC           TYPE  ZSDSFIT042-MT940_DOC,
      FYEAR_MT940         TYPE  ZSDSFIT042-FYEAR_MT940,
      CLEARING_USER       TYPE  ZSDSFIT042-CLEARING_USER,
      LINE_MAP_BANK_FE_AMT TYPE ZSDSFIT042-DEBIT_AMT,

END OF GY_ZSDSFIT042.
TYPES: BEGIN OF GY_ZSDSFIT044,
        HBKID1        TYPE  ZSDSFIT044-HBKID,
        BANKL         TYPE  ZSDSFIT044-BANKL,
        BANK_CODE     TYPE  ZSDSFIT044-BANK_CODE,
        SAKNR         TYPE  ZSDSFIT044-SAKNR,
        BANKL1        TYPE  ZSDSFIT044-BANKL1,
        BANK_NAME     TYPE  ZSDSFIT044-BANK_NAME,
END OF GY_ZSDSFIT044.
TYPES: BEGIN OF GY_BANK,
         HBKID             TYPE  ZSDSFIT044-HBKID,  "Bank code
         BANK_NAME         TYPE  ZSDSFIT044-BANK_NAME,  "Bank code
END OF GY_BANK.

DATA:
      GT_OUTPUT_BANK_DETAIL TYPE STANDARD TABLE OF GY_ZSDSFIT042,
      GW_OUTPUT_BANK_DETAIL TYPE GY_ZSDSFIT042,
      WA_OUTPUT_BANK_DETAIL TYPE GY_ZSDSFIT042,
      GT_ZSDSFIT042         TYPE STANDARD TABLE OF GY_ZSDSFIT042,
      GW_ZSDSFIT042         TYPE GY_ZSDSFIT042,
      WA_ZSDSFIT042         TYPE GY_ZSDSFIT042,
      GW_ZSDSFIT042_FEE     TYPE GY_ZSDSFIT042,
      GT_ZSDSFIT044         TYPE STANDARD TABLE OF GY_ZSDSFIT044,
      GW_ZSDSFIT044         TYPE GY_ZSDSFIT044,
      WA_ZSDSFIT044         TYPE GY_ZSDSFIT044,
      GT_BANK               TYPE STANDARD TABLE OF GY_BANK,
      GW_BANK               TYPE GY_BANK,
      GS_BANK               TYPE GY_BANK,
      WA_BANK               TYPE GY_BANK.




DATA: GT_FIELDCAT      TYPE SLIS_T_FIELDCAT_ALV,
      GS_FIELDCAT      TYPE SLIS_FIELDCAT_ALV,
      GS_LAYOUT        TYPE SLIS_LAYOUT_ALV.

DATA: BDCDATA TYPE TABLE OF BDCDATA WITH HEADER LINE.

DATA: NAME  TYPE VRM_ID,
      LIST  TYPE VRM_VALUES,
      VALUE LIKE LINE OF LIST.

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
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
  CLEAR GS_FIELDCAT.

END-OF-DEFINITION.

*
*END-OF-DEFINITION.
************************************************************************
*      S E L E C T I O N  S C R E E N                                  *
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-B02.

PARAMETERS : P_BANK TYPE BNKA-BANKL AS LISTBOX VISIBLE LENGTH 35   DEFAULT SPACE." MODIF ID SC1.
SELECT-OPTIONS : S_BDATE FOR ZSDSFIT042-ERDAT MODIF ID SC1,
                 S_BVDAT FOR ZSDSFIT042-CHEQUE_DATE MODIF ID SC1.
*                 S_TRANF FOR ZSDSFIT042-TRNFER_NUMBER MODIF ID SC1," NO-EXTENSION NO INTERVALS DEFAULT SPACE,
*                 S_WORKD FOR ZSDSFIT042-DATE_CLEAR MODIF ID SC1.
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


   PERFORM F_GET_DATA_VIEWBANK.
   PERFORM F_MAP_DATA_VIEWBANK.


************************************************************************
*      E N D      S E L E C T I O N                                    *
************************************************************************
END-OF-SELECTION .

  IF GT_OUTPUT_BANK_DETAIL[] IS NOT INITIAL.
    PERFORM F_DISPLAY_REPORT_BANK_DETAIL.
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
*&      FORM  F_GET_DATA_UNMAP
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM F_GET_DATA_VIEWBANK .


           SELECT MANDT HBKID ZBANK_ITEM CHEQUE_DATE DEBIT_AMT
                  RECEIVED_AMOUNT FLAG_BANK_FEE LINE_MAP_BANK_FE BANK_DESC
                  BAL_AMT CHANNEL BRANCH_CODE BRANCH_NAME TERMINAL_ID
                  CHEQUE_NO TRAN_CODE SENDER_BANK SENDER_BRANCH
                  SENDER_ACC_NO SENDER_ACC_NAME SDS_ACC_NO TRAN_DATE
                  TRAN_DATE_TO SMBC_REMARK SMBC_BAL_AMT RECORD_TYPE
                  SWIFT_BIC ACC_TYPE CURRENCY ACC_NAME TRAN_TYPE_NAME
                  CUST_REFER BANK_REFER ERDAT ERZET USNAM TRANF
                  RUN_ID PAYIN
                  MAP_STATUS MAP_DATE MAP_TIME STATUS_LOCK LOCK_ERDAT
                  LOCK_ERZET LOCK_USNAM
                  B_VALUE_TIME MAP_USNAM  RESERVE_MAP RESERVE_DATE RESERVE_TIME
                  RESERVE_USNAM RESERVE_DESC  FI_CONFIRM_MAP FI_CONFIRM_DATE
                  FI_CONFIRM_TIME FI_CONFIRM_USNAM STATUS_AD STATUS_ACC_RECON
                  RECON_DATE RECON_TIME RECON_USER
                  KBANK_DETAIL TRNFER_NUMBER FYEAR_TRNFERNO FI_CLEARING_NO
                  FYEAR_CLEARING DATE_CLEAR TIME_CLEAR MT940_DOC FYEAR_MT940 CLEARING_USER
             INTO TABLE GT_ZSDSFIT042
             FROM ZSDSFIT042
             WHERE HBKID EQ P_BANK
             AND CHEQUE_DATE IN S_BVDAT
             AND   ERDAT  IN S_BDATE
             AND   DELETE_FLAG NE 'X'.



ENDFORM.                    " F_PROCESS_DATA


*&---------------------------------------------------------------------*
*&      FORM  F_MAP_DATE
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM F_MAP_DATA_VIEWBANK .
  DATA: LV_RECEIVE_AMT TYPE ZSDSFIT042-RECEIVED_AMOUNT.
  DATA: LV_CHECK_RECIVE TYPE C.
  DATA: LV_TABIX TYPE SY-TABIX.
  DATA: LV_ADRNR1 TYPE KNA1-ADRNR.
  DATA: LV_VBELN TYPE VBRK-VBELN.
  DATA: LV_CHECK_COLLECTOR TYPE C.
  DATA: LV_CHECK_ITEM_BANK TYPE ZSDSFIT042-ZBANK_ITEM.

     CLEAR : LV_CHECK_ITEM_BANK.
     LOOP AT GT_ZSDSFIT042 INTO GW_ZSDSFIT042 WHERE FLAG_BANK_FEE NE 'X'.
             CLEAR: GW_OUTPUT_BANK_DETAIL.



             GW_OUTPUT_BANK_DETAIL-HBKID = GW_ZSDSFIT042-HBKID.
             GW_OUTPUT_BANK_DETAIL-ZBANK_ITEM = GW_ZSDSFIT042-ZBANK_ITEM.
             GW_OUTPUT_BANK_DETAIL-CHEQUE_DATE = GW_ZSDSFIT042-CHEQUE_DATE.
             GW_OUTPUT_BANK_DETAIL-DEBIT_AMT = GW_ZSDSFIT042-DEBIT_AMT.

                READ TABLE GT_ZSDSFIT042 INTO GW_ZSDSFIT042_FEE WITH KEY FLAG_BANK_FEE = 'X'
                                                                                    HBKID = GW_ZSDSFIT042-HBKID
                                                                                    LINE_MAP_BANK_FE = GW_ZSDSFIT042-ZBANK_ITEM.
                      IF SY-SUBRC EQ 0.
                          GW_OUTPUT_BANK_DETAIL-LINE_MAP_BANK_FE = GW_ZSDSFIT042_FEE-ZBANK_ITEM.
                          GW_OUTPUT_BANK_DETAIL-LINE_MAP_BANK_FE_AMT = GW_ZSDSFIT042_FEE-RECEIVED_AMOUNT.
                      ENDIF.



             GW_OUTPUT_BANK_DETAIL-BANK_DESC = GW_ZSDSFIT042-BANK_DESC.
             GW_OUTPUT_BANK_DETAIL-BAL_AMT = GW_ZSDSFIT042-BAL_AMT.
             GW_OUTPUT_BANK_DETAIL-CHANNEL = GW_ZSDSFIT042-CHANNEL.
             GW_OUTPUT_BANK_DETAIL-BRANCH_CODE = GW_ZSDSFIT042-BRANCH_CODE.
             GW_OUTPUT_BANK_DETAIL-BRANCH_NAME = GW_ZSDSFIT042-BRANCH_NAME.
             GW_OUTPUT_BANK_DETAIL-TERMINAL_ID = GW_ZSDSFIT042-TERMINAL_ID.
             GW_OUTPUT_BANK_DETAIL-CHEQUE_NO = GW_ZSDSFIT042-CHEQUE_NO.
             GW_OUTPUT_BANK_DETAIL-TRAN_CODE = GW_ZSDSFIT042-TRAN_CODE.
             GW_OUTPUT_BANK_DETAIL-SENDER_BANK = GW_ZSDSFIT042-SENDER_BANK.
             GW_OUTPUT_BANK_DETAIL-SENDER_BRANCH = GW_ZSDSFIT042-SENDER_BRANCH.
             GW_OUTPUT_BANK_DETAIL-SENDER_ACC_NO = GW_ZSDSFIT042-SENDER_ACC_NO.
             GW_OUTPUT_BANK_DETAIL-SENDER_ACC_NAME = GW_ZSDSFIT042-SENDER_ACC_NAME.
             GW_OUTPUT_BANK_DETAIL-SDS_ACC_NO = GW_ZSDSFIT042-SDS_ACC_NO.
             GW_OUTPUT_BANK_DETAIL-TRAN_DATE = GW_ZSDSFIT042-TRAN_DATE.
             GW_OUTPUT_BANK_DETAIL-TRAN_DATE_TO = GW_ZSDSFIT042-TRAN_DATE_TO.
             GW_OUTPUT_BANK_DETAIL-SMBC_REMARK = GW_ZSDSFIT042-SMBC_REMARK.
             GW_OUTPUT_BANK_DETAIL-SMBC_BAL_AMT = GW_ZSDSFIT042-SMBC_BAL_AMT.
             GW_OUTPUT_BANK_DETAIL-RECORD_TYPE = GW_ZSDSFIT042-RECORD_TYPE.
             GW_OUTPUT_BANK_DETAIL-SWIFT_BIC = GW_ZSDSFIT042-SWIFT_BIC.
             GW_OUTPUT_BANK_DETAIL-ACC_TYPE = GW_ZSDSFIT042-ACC_TYPE.
             GW_OUTPUT_BANK_DETAIL-CURRENCY = GW_ZSDSFIT042-CURRENCY.
             GW_OUTPUT_BANK_DETAIL-ACC_NAME = GW_ZSDSFIT042-ACC_NAME.
             GW_OUTPUT_BANK_DETAIL-TRAN_TYPE_NAME = GW_ZSDSFIT042-TRAN_TYPE_NAME.
             GW_OUTPUT_BANK_DETAIL-CUST_REFER = GW_ZSDSFIT042-CUST_REFER.
             GW_OUTPUT_BANK_DETAIL-BANK_REFER = GW_ZSDSFIT042-BANK_REFER.
             GW_OUTPUT_BANK_DETAIL-ERDAT = GW_ZSDSFIT042-ERDAT.
             GW_OUTPUT_BANK_DETAIL-ERZET = GW_ZSDSFIT042-ERZET.
             GW_OUTPUT_BANK_DETAIL-USNAM = GW_ZSDSFIT042-USNAM.
             GW_OUTPUT_BANK_DETAIL-TRANF = GW_ZSDSFIT042-TRANF.
             GW_OUTPUT_BANK_DETAIL-PAYIN = GW_ZSDSFIT042-PAYIN.
             GW_OUTPUT_BANK_DETAIL-MAP_STATUS = GW_ZSDSFIT042-MAP_STATUS.
             GW_OUTPUT_BANK_DETAIL-MAP_DATE = GW_ZSDSFIT042-MAP_DATE.
             GW_OUTPUT_BANK_DETAIL-MAP_TIME = GW_ZSDSFIT042-MAP_TIME.
             GW_OUTPUT_BANK_DETAIL-MAP_DATE = GW_ZSDSFIT042-MAP_DATE.
             GW_OUTPUT_BANK_DETAIL-STATUS_LOCK = GW_ZSDSFIT042-STATUS_LOCK.
             GW_OUTPUT_BANK_DETAIL-LOCK_ERDAT = GW_ZSDSFIT042-LOCK_ERDAT.
             GW_OUTPUT_BANK_DETAIL-LOCK_ERZET = GW_ZSDSFIT042-LOCK_ERZET.
             GW_OUTPUT_BANK_DETAIL-LOCK_USNAM = GW_ZSDSFIT042-LOCK_USNAM.
             GW_OUTPUT_BANK_DETAIL-RECEIVED_AMOUNT = GW_ZSDSFIT042-RECEIVED_AMOUNT.

             GW_OUTPUT_BANK_DETAIL-MAP_USNAM = GW_ZSDSFIT042-MAP_USNAM.
             GW_OUTPUT_BANK_DETAIL-RESERVE_MAP = GW_ZSDSFIT042-RESERVE_MAP.
             GW_OUTPUT_BANK_DETAIL-RESERVE_DATE = GW_ZSDSFIT042-RESERVE_DATE.
             GW_OUTPUT_BANK_DETAIL-RESERVE_TIME = GW_ZSDSFIT042-RESERVE_TIME.
             GW_OUTPUT_BANK_DETAIL-RESERVE_USNAM = GW_ZSDSFIT042-RESERVE_USNAM.
             GW_OUTPUT_BANK_DETAIL-RESERVE_DESC = GW_ZSDSFIT042-RESERVE_DESC.
             GW_OUTPUT_BANK_DETAIL-FI_CONFIRM_MAP = GW_ZSDSFIT042-FI_CONFIRM_MAP.
             GW_OUTPUT_BANK_DETAIL-FI_CONFIRM_DATE = GW_ZSDSFIT042-FI_CONFIRM_DATE.
             GW_OUTPUT_BANK_DETAIL-FI_CONFIRM_TIME = GW_ZSDSFIT042-FI_CONFIRM_TIME.
             GW_OUTPUT_BANK_DETAIL-FI_CONFIRM_USNAM = GW_ZSDSFIT042-FI_CONFIRM_USNAM.
             GW_OUTPUT_BANK_DETAIL-STATUS_AD = GW_ZSDSFIT042-STATUS_AD.
             GW_OUTPUT_BANK_DETAIL-KBANK_DETAIL = GW_ZSDSFIT042-KBANK_DETAIL.
             GW_OUTPUT_BANK_DETAIL-TRNFER_NUMBER = GW_ZSDSFIT042-TRNFER_NUMBER.
             GW_OUTPUT_BANK_DETAIL-FYEAR_TRNFERNO = GW_ZSDSFIT042-FYEAR_TRNFERNO.
             GW_OUTPUT_BANK_DETAIL-FI_CLEARING_NO = GW_ZSDSFIT042-FI_CLEARING_NO.
             GW_OUTPUT_BANK_DETAIL-FYEAR_CLEARING = GW_ZSDSFIT042-FYEAR_CLEARING.
             GW_OUTPUT_BANK_DETAIL-DATE_CLEAR = GW_ZSDSFIT042-DATE_CLEAR.
             GW_OUTPUT_BANK_DETAIL-TIME_CLEAR = GW_ZSDSFIT042-TIME_CLEAR.
             GW_OUTPUT_BANK_DETAIL-MT940_DOC = GW_ZSDSFIT042-MT940_DOC.
             GW_OUTPUT_BANK_DETAIL-FYEAR_MT940 = GW_ZSDSFIT042-FYEAR_MT940.
             GW_OUTPUT_BANK_DETAIL-CLEARING_USER = GW_ZSDSFIT042-CLEARING_USER.
             GW_OUTPUT_BANK_DETAIL-LINE_MAP_BANK_FE_AMT = GW_ZSDSFIT042-LINE_MAP_BANK_FE_AMT.

             IF LV_CHECK_COLLECTOR = ' '.
                APPEND GW_OUTPUT_BANK_DETAIL TO GT_OUTPUT_BANK_DETAIL.
             ENDIF.

     ENDLOOP.





ENDFORM.                    " F_PROCESS_DATA

**&---------------------------------------------------------------------*
**&      FORM  F_PROCESS_DATA
**&---------------------------------------------------------------------*
**       TEXT
**----------------------------------------------------------------------*
**  -->  P1        TEXT
**  <--  P2        TEXT
**----------------------------------------------------------------------*
*FORM F_PROCESS_DATA .
*  LOOP AT GT_DATA INTO GS_DATA.
*    MOVE-CORRESPONDING GS_DATA TO GS_OUTPUT.
*    APPEND GS_OUTPUT TO GT_OUTPUT.
*  ENDLOOP.
*ENDFORM.                    " F_PROCESS_DATA


*&---------------------------------------------------------------------*
*&      FORM  F_DISPLAY_REPORT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM F_DISPLAY_REPORT_BANK_DETAIL .

  PERFORM F_FILL_FIELDCAT_BANK_DETAIL.

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
      T_OUTTAB                 = GT_OUTPUT_BANK_DETAIL
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
*DATA FCODE TYPE TABLE OF SY-UCOMM.
*APPEND 'MAPPING' TO FCODE.
*APPEND 'UNMAPPING' TO FCODE.
*
*SET PF-STATUS 'STATUS_0100' EXCLUDING FCODE.
*  IF R_MAP IS NOT INITIAL.
*
*  ELSEIF R_UNMAP IS NOT INITIAL.
*
*  ENDIF.

  SET PF-STATUS 'ZSTANDARD' EXCLUDING RT_EXTAB.
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


ENDFORM.                    "USERCOMMAND

*&---------------------------------------------------------------------*
*&      FORM  F_FILL_FIELDCAT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM F_FILL_FIELDCAT_BANK_DETAIL .
  DATA: LV_COLUMN TYPE I.

 IF P_BANK IS NOT INITIAL.

               IF P_BANK = 'BAY01'.
*              -BANK CODE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'HBKID' LV_COLUMN 'BANK CODE' SPACE 10.
*              *-ITEM
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'ZBANK_ITEM' LV_COLUMN 'ITEM' SPACE 10.
*              *-DATE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'CHEQUE_DATE' LV_COLUMN 'วันที่' SPACE 10.
*              *TRAN_CODE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'TRAN_CODE' LV_COLUMN 'รหัส' SPACE 15.
*              *-DESCRIPTION
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'BANK_DESC' LV_COLUMN 'หมายเลขอ้างอิง' SPACE 20.
*              *-DEBIT
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'DEBIT_AMT' LV_COLUMN 'เดบิต' SPACE 20.

*              *-CREDIT
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'RECEIVED_AMOUNT' LV_COLUMN 'เครดิต' SPACE 20.
*              *-BALANCE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'BAL_AMT' LV_COLUMN 'BALANCE' SPACE 20.
*              *-CHANNEL
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'CHANNEL' LV_COLUMN 'ช่องทาง' SPACE 20.
*              *-BRANCH CODE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'BRANCH_CODE' LV_COLUMN 'รหัสสาขา' SPACE 20.

            ELSEIF P_BANK = 'BBL01'.

*              -BANK CODE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'HBKID' LV_COLUMN 'BANK CODE' SPACE 10.
*              *-ITEM
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'ZBANK_ITEM' LV_COLUMN 'ITEM' SPACE 10.
*              *-TRAN DATE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'TRAN_DATE' LV_COLUMN 'TRAN DATE' SPACE 10.
*              *-VALUE DATE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'CHEQUE_DATE' LV_COLUMN 'VALUE DATE' SPACE 10.
*              *-DESCRIPTION
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'BANK_DESC' LV_COLUMN 'DESCRIPTION' SPACE 20.
*              *TRAN_CODE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'TRAN_CODE' LV_COLUMN 'TRAN CODE' SPACE 15.
*              *CHEQUE NO.
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'CHEQUE_NO' LV_COLUMN 'CHEQUE NO.' SPACE 15.
*              *-DEBIT
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'DEBIT_AMT' LV_COLUMN 'DEBIT' SPACE 20.

*              *-CREDIT
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'RECEIVED_AMOUNT' LV_COLUMN 'CREDIT' SPACE 20.
*              *-BALANCE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'BAL_AMT' LV_COLUMN 'BALANCE' SPACE 20.
*              *-CHANNEL
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'CHANNEL' LV_COLUMN 'CHANNEL' SPACE 20.
*              *-TERMINAL ID
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'TERMINAL_ID' LV_COLUMN 'TERMINAL ID' SPACE 20.
*              *-BRANCH
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'BRANCH_NAME' LV_COLUMN 'BRANCH' SPACE 20.
*              *-SENDER BANK
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'SENDER_BANK' LV_COLUMN 'SENDER BANK' SPACE 20.
*              *-SENDER BRANCH
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'SENDER_BRANCH' LV_COLUMN 'SENDER BRANCH' SPACE 20.
*              *-SENDER ACCOUNT NUMBER
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'SENDER_ACC_NO' LV_COLUMN 'SENDER ACC.NO.' SPACE 20.
*              *-SENDER ACCOUNT NAME
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'SENDER_ACC_NAME' LV_COLUMN 'SENDER NAME' SPACE 20.



                ELSEIF P_BANK = 'BTMU'.

*              -BANK CODE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'HBKID' LV_COLUMN 'BANK CODE' SPACE 10.
*              *-ITEM
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'ZBANK_ITEM' LV_COLUMN 'ITEM' SPACE 10.
*              *-RECORD TYPE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'RECORD_TYPE' LV_COLUMN 'RECORD TYPE' SPACE 10.
*              *-BRANCH
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'BRANCH_NAME' LV_COLUMN 'BRANCH NAME' SPACE 20.
*              *-SWIFT_BIC
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'SWIFT_BIC' LV_COLUMN 'SWIFT BIC' SPACE 20.
*              *-ACCOUNT TYPE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'ACC_TYPE' LV_COLUMN 'ACCOUNT TYPE' SPACE 20.
*              *-CURRENCY
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'CURRENCY' LV_COLUMN 'CURRENCY' SPACE 20.
*              *-ACCOUNT NO.
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'SDS_ACC_NO' LV_COLUMN 'SDS ACCOUNT NO.' SPACE 20.
*              *-ACC_NAME
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'ACC_NAME' LV_COLUMN 'SDS ACCOUNT NAME' SPACE 20.
*              *-BOOKING DATE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'TRAN_DATE' LV_COLUMN 'BOOKING DATE' SPACE 10.
*              *-VALUE DATE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'CHEQUE_DATE' LV_COLUMN 'VALUE DATE' SPACE 10.
*              *-TRANSACTION TYPE NAME
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'TRAN_TYPE_NAME' LV_COLUMN 'TRANS. TYPE NAME' SPACE 10.
*              *-DEBIT
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'DEBIT_AMT' LV_COLUMN 'DEBIT' SPACE 20.
*              *-CREDIT
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'RECEIVED_AMOUNT' LV_COLUMN 'CREDIT' SPACE 20.
*              *-OPENING / CLOSING BALANCE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'BAL_AMT' LV_COLUMN 'OPENING / CLOSING BALANCE' SPACE 20.

*              *-CUSTOMER REFERENCE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'CUST_REFER' LV_COLUMN 'CUSTOMER REFER.' SPACE 10.
*              *-DETAIL INFORMATION
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'BANK_DESC' LV_COLUMN 'DETAIL INFORMATION' SPACE 20.
*              *-BANK REFERENCE
                ADD 1 TO LV_COLUMN.
                M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'BANK_REFER' LV_COLUMN 'BANK REFERENCE' SPACE 20.

                ELSEIF P_BANK = 'KBK01'.

*                  -BANK CODE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'HBKID' LV_COLUMN 'BANK CODE' SPACE 10.
*                  *-ITEM
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'ZBANK_ITEM' LV_COLUMN 'ITEM' SPACE 10.
*                  *-VALUE DATE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'CHEQUE_DATE' LV_COLUMN 'วันที่รายการมีผล' SPACE 10.
*                  *-DETAIL INFORMATION
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'BANK_DESC' LV_COLUMN 'รายละเอียดรายการ' SPACE 20.
*                  *-CHEQUE NO
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'CHEQUE_NO' LV_COLUMN 'เลขที่เช็ค' SPACE 10.
*                  *-DEBIT
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'DEBIT_AMT' LV_COLUMN 'จำนวนเงินหักบัญชี' SPACE 20.
*                  *-CREDIT
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'RECEIVED_AMOUNT' LV_COLUMN 'จำนวนเงินนำฝากเข้าบัญชี' SPACE 20.
*                  *-OPENING / CLOSING BALANCE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'BAL_AMT' LV_COLUMN 'ยอดคงเหลือ' SPACE 20.
*                                     *-หมายเลข
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'TERMINAL_ID' LV_COLUMN 'หมายเลข' SPACE 20.
*                   *-BRANCH CODE
                     ADD 1 TO LV_COLUMN.
                     M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'BRANCH_CODE' LV_COLUMN 'สาขา' SPACE 20.
*                  *-BOOKING DATE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'TRAN_DATE' LV_COLUMN 'วันที่ทำรายการ' SPACE 10.
*                  *-BOOKING DATE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'KBANK_DETAIL' LV_COLUMN 'DETAIL' SPACE 10.  "CH1 ADD BY WANTANEE 20210610

                ELSEIF P_BANK = 'KTB01'.

*                  -BANK CODE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'HBKID' LV_COLUMN 'BANK CODE' SPACE 10.
*                  *-ITEM
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'ZBANK_ITEM' LV_COLUMN 'ITEM' SPACE 10.
*                  *-DATE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'CHEQUE_DATE' LV_COLUMN 'DATE' SPACE 10.
*                  *-TELLER ID
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'TERMINAL_ID' LV_COLUMN 'TELLER ID' SPACE 10.
*                   *TRAN_CODE
                     ADD 1 TO LV_COLUMN.
                     M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'TRAN_CODE' LV_COLUMN 'TRAN CODE' SPACE 15.
*                  *-DESCRIPTION
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'BANK_DESC' LV_COLUMN 'DESCRIPTION' SPACE 20.
*                  *-CHEQUE NO
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'CHEQUE_NO' LV_COLUMN 'CHEQUE NO.' SPACE 10.
*                  *-AMOUNT
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'RECEIVED_AMOUNT' LV_COLUMN 'AMOUNT' SPACE 20.
*                  *-BALANCE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'BAL_AMT' LV_COLUMN 'BALANCE' SPACE 20.
*                   *-INIT BR
                     ADD 1 TO LV_COLUMN.
                     M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'BRANCH_CODE' LV_COLUMN 'INIT BR' SPACE 20.



                ELSEIF P_BANK = 'SCB01'.

*                  -BANK CODE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'HBKID' LV_COLUMN 'BANK CODE' SPACE 10.
*                  *-ITEM
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'ZBANK_ITEM' LV_COLUMN 'ITEM' SPACE 10.
*                  *-SDS ACC. NO.
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'SDS_ACC_NO' LV_COLUMN 'SDS ACC. NO.' SPACE 10.
*                  *-DATE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'CHEQUE_DATE' LV_COLUMN 'DATE' SPACE 10.
*                   *TRAN_CODE
                     ADD 1 TO LV_COLUMN.
                     M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'TRAN_CODE' LV_COLUMN 'TRAN CODE' SPACE 10.
*                   *CHANNEL
                     ADD 1 TO LV_COLUMN.
                     M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'CHANNEL' LV_COLUMN 'CHANNEL' SPACE 10.
*                  *-CHEQUE NO
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'CHEQUE_NO' LV_COLUMN 'CHEQUE NO.' SPACE 10.
*                  *-DEBIT
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'DEBIT_AMT' LV_COLUMN 'DEBIT' SPACE 20.
*                  *-CREDIT
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'RECEIVED_AMOUNT' LV_COLUMN 'CREDIT' SPACE 20.
*                  *-BALANCE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'BAL_AMT' LV_COLUMN 'BALANCE' SPACE 20.
*                  *-DESCRIPTION
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'BANK_DESC' LV_COLUMN 'DESCRIPTION' SPACE 20.



                ELSEIF P_BANK = 'TMB01'.

*                  -BANK CODE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'HBKID' LV_COLUMN 'BANK CODE' SPACE 10.
*                  *-ITEM
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'ZBANK_ITEM' LV_COLUMN 'ITEM' SPACE 10.
*                  *-SDS ACC. NO.
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'SDS_ACC_NO' LV_COLUMN 'SDS เลขที่บัญชี' SPACE 10.
*                  *-BOOKING DATE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'TRAN_DATE' LV_COLUMN 'วันที่ทำรายการ' SPACE 10.
*                  *-DATE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'CHEQUE_DATE' LV_COLUMN 'วันที่มีผล' SPACE 10.
*                   *TRAN_CODE
                     ADD 1 TO LV_COLUMN.
                     M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'TRAN_CODE' LV_COLUMN 'รหัสประเภทรายการ' SPACE 10.
*                  *-CHEQUE NO
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'CHEQUE_NO' LV_COLUMN 'เลขที่เช็ค' SPACE 10.

*                  *-DEBIT
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'DEBIT_AMT' LV_COLUMN 'ถอน' SPACE 20.
*                  *-CREDIT
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'RECEIVED_AMOUNT' LV_COLUMN 'ฝาก' SPACE 20.
*                  *-BALANCE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'BAL_AMT' LV_COLUMN 'ยอดคงเหลือ' SPACE 20.
*                  *-TELLER ID
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'TERMINAL_ID' LV_COLUMN 'ช่องทางทำรายการ' SPACE 10.
*                  *-DESCRIPTION
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'BANK_DESC' LV_COLUMN 'รายละเอียดการทำธุรกรรม' SPACE 20.


                ELSEIF P_BANK = 'UOB'.

*                  -BANK CODE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'HBKID' LV_COLUMN 'BANK CODE' SPACE 10.
*                  *-ITEM
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'ZBANK_ITEM' LV_COLUMN 'ITEM' SPACE 10.
*                  *-SDS ACC. NO.
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'SDS_ACC_NO' LV_COLUMN 'SDS ACC.NO.' SPACE 10.
*                  *-DATE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'CHEQUE_DATE' LV_COLUMN 'VALUE DATE' SPACE 10.
*                  *-BOOKING DATE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'TRAN_DATE' LV_COLUMN 'DATE' SPACE 10.
*                  *-DESCRIPTION
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'BANK_DESC' LV_COLUMN 'DESCRIPTION' SPACE 20.
*                  *-CHEQUE NO
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'CHEQUE_NO' LV_COLUMN 'CHEQUE NO' SPACE 10.
*                  *-CREDIT
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'RECEIVED_AMOUNT' LV_COLUMN 'DEPOSIT' SPACE 20.
*                  *-BALANCE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'BAL_AMT' LV_COLUMN 'LEDGER BALANCE' SPACE 20.

                ELSEIF P_BANK = 'SMB01'.

*                  -BANK CODE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'HBKID' LV_COLUMN 'BANK CODE' SPACE 10.
*                  *-ITEM
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'ZBANK_ITEM' LV_COLUMN 'ITEM' SPACE 10.
*                   *-BRANCH CODE
                     ADD 1 TO LV_COLUMN.
                     M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'BRANCH_CODE' LV_COLUMN 'BRANCH CODE' SPACE 20.

*                  *-ACC_NAME
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'ACC_NAME' LV_COLUMN 'SDS ACCOUNT NAME' SPACE 20.
*                  *-DATE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'TRAN_DATE' LV_COLUMN 'DATE FROM' SPACE 10.
*                  *-DATE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'TRAN_DATE_TO' LV_COLUMN 'DATE TO' SPACE 10.
*                  *-SDS ACC. NO.
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'SDS_ACC_NO' LV_COLUMN 'SDS ACC. NO.' SPACE 10.
*                  *-DATE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'CHEQUE_DATE' LV_COLUMN 'DATE' SPACE 10.
*                  *-DEBIT
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'DEBIT_AMT' LV_COLUMN 'DEBIT' SPACE 20.
*                  *-CREDIT
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'RECEIVED_AMOUNT' LV_COLUMN 'CREDIT' SPACE 20.
*                  *-BALANCE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'SMBC_BAL_AMT' LV_COLUMN 'BALANCE' SPACE 20.
*                  *-DESCRIPTION
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'BANK_DESC' LV_COLUMN 'DESCRIPTION' SPACE 20.
*                  *-CHEQUE NO
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'CHEQUE_NO' LV_COLUMN 'CHEQUE NO.' SPACE 10.
*                  *-REMARKS
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'SMBC_REMARK' LV_COLUMN 'REMARKS' SPACE 20.

*                  *-AVAILABLEBALANCE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'BAL_AMT' LV_COLUMN 'AVAILABLEBALANCE' SPACE 20.


                ELSE.
*                   -BANK CODE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'HBKID' LV_COLUMN 'BANK CODE' SPACE 10.
*                  *-ITEM
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'ZBANK_ITEM' LV_COLUMN 'ITEM' SPACE 10.
*                  *-DATE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'CHEQUE_DATE' LV_COLUMN 'DATE' SPACE 10.
*                  *-CREDIT
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'RECEIVED_AMOUNT' LV_COLUMN 'CREDIT' SPACE 20.
*                  *-DESCRIPTION
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'BANK_DESC' LV_COLUMN 'DESCRIPTION' SPACE 20.
*                   *-UPLOAD DATE
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_SUMMARY' 'ERDAT' LV_COLUMN 'UPLOAD DATE' SPACE 20.
*                   *-STATUS MAP
                    ADD 1 TO LV_COLUMN.
                    M_FILL_CAT 'GT_OUTPUT_BANK_SUMMARY' 'MAP_STATUS' LV_COLUMN 'STATUS MAP' SPACE 20.
                ENDIF.
    ELSE.

*             -BANK CODE
               ADD 1 TO LV_COLUMN.
               M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'HBKID' LV_COLUMN 'BANK CODE' SPACE 10.
*             *-ITEM
               ADD 1 TO LV_COLUMN.
               M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'ZBANK_ITEM' LV_COLUMN 'ITEM' SPACE 10.
*             *-DATE
               ADD 1 TO LV_COLUMN.
               M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'CHEQUE_DATE' LV_COLUMN 'DATE' SPACE 10.
*             *-CREDIT
               ADD 1 TO LV_COLUMN.
               M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'RECEIVED_AMOUNT' LV_COLUMN 'CREDIT' SPACE 20.
*             *-DESCRIPTION
               ADD 1 TO LV_COLUMN.
               M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'BANK_DESC' LV_COLUMN 'DESCRIPTION' SPACE 20.
*              *-UPLOAD DATE
               ADD 1 TO LV_COLUMN.
               M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'ERDAT' LV_COLUMN 'UPLOAD DATE' SPACE 20.
**              *-COLLECTOR MAP
*               ADD 1 TO LV_COLUMN.
*               M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'TRANF' LV_COLUMN 'COLLECTOR MAP' SPACE 20.
**              *-PAY IN
*               ADD 1 TO LV_COLUMN.
*               M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'PAYIN' LV_COLUMN 'PAY IN' SPACE 20.
*              *-STATUS MAP
               ADD 1 TO LV_COLUMN.
               M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'MAP_STATUS' LV_COLUMN 'STATUS MAP' SPACE 20.
*              *-MAP DATE
               ADD 1 TO LV_COLUMN.
               M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'MAP_DATE' LV_COLUMN 'MAP DATE' SPACE 20.

*              *-MAP TIME
               ADD 1 TO LV_COLUMN.
               M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'MAP_TIME' LV_COLUMN 'MAP TIME' SPACE 20.

    ENDIF.
    ADD 1 TO LV_COLUMN.
    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'TRANF' LV_COLUMN 'TRANSFER NO.' SPACE 20.
    ADD 1 TO LV_COLUMN.
    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'PAYIN' LV_COLUMN 'PAY IN' SPACE 20.

    ADD 1 TO LV_COLUMN.
    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'LINE_MAP_BANK_FE' LV_COLUMN 'LINE BANK FEE' SPACE 20.
    ADD 1 TO LV_COLUMN.
    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'LINE_MAP_BANK_FE_AMT' LV_COLUMN 'LINE BANK FEE AMT' SPACE 20.

    ADD 1 TO LV_COLUMN.
    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'MAP_USNAM' LV_COLUMN 'MAPPING USERS' SPACE 20.
    ADD 1 TO LV_COLUMN.
    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'RESERVE_MAP' LV_COLUMN 'RESERVE STATUS' SPACE 20.
    ADD 1 TO LV_COLUMN.
    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'RESERVE_DATE' LV_COLUMN 'RESERVE DATE' SPACE 20.
    ADD 1 TO LV_COLUMN.
    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'RESERVE_TIME' LV_COLUMN 'RESERVE TIME' SPACE 20.
    ADD 1 TO LV_COLUMN.
    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'RESERVE_USNAM' LV_COLUMN 'RESERVE USERS' SPACE 20.
    ADD 1 TO LV_COLUMN.
    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'RESERVE_DESC' LV_COLUMN 'RESERVE DESC.' SPACE 20.
    ADD 1 TO LV_COLUMN.
    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'FI_CONFIRM_MAP' LV_COLUMN 'FI CONFIRM STATUS' SPACE 20.
    ADD 1 TO LV_COLUMN.
    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'FI_CONFIRM_DATE' LV_COLUMN 'FI CONFIRM DATE' SPACE 20.
    ADD 1 TO LV_COLUMN.
    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'FI_CONFIRM_TIME' LV_COLUMN 'FI CONFIRM TIME' SPACE 20.
    ADD 1 TO LV_COLUMN.
    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'FI_CONFIRM_USNAM' LV_COLUMN 'FI CONFIRM USERS' SPACE 20.
    ADD 1 TO LV_COLUMN.
    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'STATUS_AD' LV_COLUMN 'STATUS CLEAR ทดลอง' SPACE 20.
    ADD 1 TO LV_COLUMN.
    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'STATUS_ACC_RECON' LV_COLUMN 'Status Account Reconcile' SPACE 20.
    ADD 1 TO LV_COLUMN.
    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'RECON_DATE' LV_COLUMN 'Recon Created On' SPACE 20.
    ADD 1 TO LV_COLUMN.
    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'RECON_TIME' LV_COLUMN 'Recon Entry time' SPACE 20.
    ADD 1 TO LV_COLUMN.
    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'RECON_USER' LV_COLUMN 'Recon User Name' SPACE 20.

    ADD 1 TO LV_COLUMN.
    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'TRNFER_NUMBER' LV_COLUMN 'Transfer Number' SPACE 20.
    ADD 1 TO LV_COLUMN.
    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'FYEAR_TRNFERNO' LV_COLUMN 'Fiscal year transfer number' SPACE 20.
    ADD 1 TO LV_COLUMN.
    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'FI_CLEARING_NO' LV_COLUMN 'FI Clearing number' SPACE 20.
    ADD 1 TO LV_COLUMN.
    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'FYEAR_CLEARING' LV_COLUMN 'FY clearing' SPACE 20.
    ADD 1 TO LV_COLUMN.
    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'DATE_CLEAR' LV_COLUMN 'Date Clearing' SPACE 20.
    ADD 1 TO LV_COLUMN.
    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'TIME_CLEAR' LV_COLUMN 'Time clearing' SPACE 20.
    ADD 1 TO LV_COLUMN.
    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'MT940_DOC' LV_COLUMN 'MT940 Doc.' SPACE 20.
    ADD 1 TO LV_COLUMN.
    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'FYEAR_MT940' LV_COLUMN 'Fiscal year MT940' SPACE 20.
    ADD 1 TO LV_COLUMN.
    M_FILL_CAT 'GT_OUTPUT_BANK_DETAIL' 'CLEARING_USER' LV_COLUMN 'User Clearing' SPACE 20.





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

*
***&---------------------------------------------------------------------*
***&      FORM  F_MODIFY_SCREEN
***&---------------------------------------------------------------------*
*
*
*FORM F_MODIFY_SCREEN.
*  IF NOT R_COLL IS INITIAL.
*   LOOP AT SCREEN.
*      %HIDE 'SC1'.
*      %SHOW 'SC2'.
*      MODIFY SCREEN.
*
*    ENDLOOP.
*  ELSE.
*    LOOP AT SCREEN.
*      %SHOW 'SC1'.
*      %HIDE 'SC2'.
*      MODIFY SCREEN.
*
*    ENDLOOP.
*  ENDIF.
*
*ENDFORM.
