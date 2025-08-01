*-----------------------------------------------------------------------
*  Program ID         : ZSDSMMR0510
*  Creation Date      : 19.09.2024
*  Author             : Jakarin S.
*  Add-on ID          : <<Refer WRICEF List)
*  Description        : Upload Purchase Requisition
*  Purpose            :
*  Copied from        : <<Reference Program>>
*  Restriction        :
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

REPORT ZSDSMMR0510 NO STANDARD PAGE HEADING LINE-SIZE 200.
*----------------------------------------------------------------------*
*  Definition area
*----------------------------------------------------------------------*
TABLES: LFA1.
TABLES: MARA.
TABLES: T001W.
*-----  Parameters  ---------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME.
  PARAMETERS PA_BSART LIKE MEREQ_TOPLINE-BSART OBLIGATORY
                      DEFAULT 'ZPR '.
  PARAMETERS PA_WERKS LIKE EBAN-WERKS OBLIGATORY
                      DEFAULT '1000'.
  PARAMETERS PA_AFNAM TYPE EBAN-AFNAM OBLIGATORY.
  PARAMETERS PA_BEDNR TYPE EBAN-BEDNR OBLIGATORY.
  PARAMETERS PA_LIFNR TYPE EBAN-LIFNR OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
  PARAMETERS PA_FILE  TYPE DSVASDOCID OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B1.

*-----  Inside Table  -------------------------------------------------*
DATA: BEGIN OF REC_CHECK,
        TEXT  TYPE TEXT1000,
      END   OF REC_CHECK.
DATA: TBL_CHECK LIKE TABLE OF REC_CHECK.
*
DATA: BEGIN OF I_FILE    OCCURS 0,
    DELIV_DATE LIKE EBAN-LFDAT,   "Item delivery date
    MATERIAL   LIKE EBAN-MATNR,   "Material number
    QUANTITY   LIKE EBAN-MENGE,   "Purchase requisition quantity
    ETD        LIKE EBAN-LFDAT,   "ETd
  END OF I_FILE.
*
DATA: BEGIN OF O_MESSAGE OCCURS 0,
    DELIV_DATE   TYPE DATUM,
    MESSAGE      TYPE BAPI_MSG,
  END OF O_MESSAGE.
*
DATA: BEGIN OF I_ITEM   OCCURS 0.
        INCLUDE STRUCTURE BAPIEBANC.
DATA: END OF I_ITEM.
*
DATA: SO_RETURN            LIKE BAPIRETURN,
      T_SO_RETURN          LIKE BAPIRETURN  OCCURS 0 WITH HEADER LINE.
*
DATA: BEGIN OF O_ERR    OCCURS 0,
    TABIX        LIKE SY-TABIX,
    REC(100)     TYPE C,
    MESSAGE(100) TYPE C,
  END OF O_ERR.
*
*-----  Constants  ----------------------------------------------------*
CONSTANTS:
  CST_FILETYPE     TYPE RLGRAP-FILETYPE  VALUE 'ASC'.
*-----  Work Definition   ---------------------------------------------*
DATA:
  WK_HTYPE         TYPE DATATYPE_D.      "Data Type
*----------------------------------------------------------------------*
*  Program
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INIT_RTN.

AT SELECTION-SCREEN ON BLOCK B1.
  PERFORM INPUT_CHECK_RTN.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR PA_FILE.
  PERFORM GET_FILENAME_RTN CHANGING PA_FILE.

AT SELECTION-SCREEN.
  PERFORM USER_COMMAND.

START-OF-SELECTION.

END-OF-SELECTION.
  PERFORM END_RTN.
*&---------------------------------------------------------------------*
*&      Form  INIT_RTN
*&---------------------------------------------------------------------*
FORM INIT_RTN.
*
*
ENDFORM.                    " INIT_RTN
*&---------------------------------------------------------------------*
*&      Form  INPUT_CHECK_RTN
*&---------------------------------------------------------------------*
FORM INPUT_CHECK_RTN.
DATA: LO_RETURN     TYPE C,
      LO_ANSWER(01) TYPE C.
*
  PERFORM FILE_CHECK  USING  PA_FILE
                            'FE'
                   CHANGING  LO_RETURN.
*
  IF SY-SUBRC = 0.
    IF  LO_RETURN = '0'.
      SET CURSOR FIELD 'PA_FILE'.
      MESSAGE E058(00) WITH PA_FILE '' ''.
    ENDIF.
  ELSE.
    SET CURSOR FIELD 'PA_FILE'.
    MESSAGE E058(00) WITH PA_FILE '' ''.
  ENDIF.
*
ENDFORM.                    " INPUT_CHECK_RTN
*&---------------------------------------------------------------------*
*&      Form  FILE_CHECK
*&---------------------------------------------------------------------*
FORM FILE_CHECK USING    L_FILE
                         L_QUERY
                CHANGING L_RETURN.
*
  CLEAR L_RETURN.
*
  CALL FUNCTION 'WS_QUERY'
    EXPORTING
*     ENVIRONMENT          =
      FILENAME             = L_FILE
      QUERY                = L_QUERY
*     WINID                =
    IMPORTING
      RETURN               = L_RETURN
    EXCEPTIONS
      INV_QUERY            = 1
      NO_BATCH             = 2
      FRONTEND_ERROR       = 3
      OTHERS               = 4.
*
ENDFORM.                    " FILE_CHECK
*&---------------------------------------------------------------------*
*&      Form  GET_FILENAME_RTN
*&---------------------------------------------------------------------*
FORM GET_FILENAME_RTN CHANGING P_NAME.
*
  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            DEF_FILENAME     = ' '
            DEF_PATH         = 'C:\TEMP\'
            MASK             = ',*.*,*.*.'
            MODE             = 'O'
       IMPORTING
            FILENAME         = P_NAME
       EXCEPTIONS
            INV_WINSYS       = 01
            NO_BATCH         = 02
            SELECTION_CANCEL = 03
            SELECTION_ERROR  = 04.
*
ENDFORM.                    " GET_FILENAME_RTN
*&---------------------------------------------------------------------*
*&      Form  MAIN_RTN
*&---------------------------------------------------------------------*
FORM MAIN_RTN.
*
DATA: L_LINE TYPE I.
*
  PERFORM GET_FILE_RTN.
  PERFORM ERR_CHECK_RTN.
*
ENDFORM.                    " MAIN_RTN
*&---------------------------------------------------------------------*
*&      Form  GET_FILE_RTN
*&---------------------------------------------------------------------*
FORM GET_FILE_RTN.
*
DATA L_FILE LIKE RLGRAP-FILENAME.
*
  L_FILE = PA_FILE.
*
  CALL FUNCTION 'WS_UPLOAD'
    EXPORTING
*     CODEPAGE                      = ' '
      FILENAME                      = L_FILE
      FILETYPE                      = CST_FILETYPE
*     HEADLEN                       = ' '
*     LINE_EXIT                     = ' '
*     TRUNCLEN                      = ' '
*     USER_FORM                     = ' '
*     USER_PROG                     = ' '
*     DAT_D_FORMAT                  = ' '
*   IMPORTING
*     FILELENGTH                    =
    TABLES
      DATA_TAB                      = TBL_CHECK
    EXCEPTIONS
     CONVERSION_ERROR              = 1
     FILE_OPEN_ERROR               = 2
     FILE_READ_ERROR               = 3
     INVALID_TYPE                  = 4
     NO_BATCH                      = 5
     UNKNOWN_ERROR                 = 6
     INVALID_TABLE_WIDTH           = 7
     GUI_REFUSE_FILETRANSFER       = 8
     CUSTOMER_ERROR                = 9
     OTHERS                        = 10
            .
  IF SY-SUBRC <> 0.
    MESSAGE E120(CPE).
  ENDIF.
*
ENDFORM.                    " GET_FILE_RTN
*&---------------------------------------------------------------------*
*&      Form  END_RTN
*&---------------------------------------------------------------------*
FORM END_RTN.
*
DATA: L_LINE TYPE I.
*
  DESCRIBE TABLE O_ERR LINES L_LINE.
  IF L_LINE > 0.
    PERFORM ERR_OUTPUT.
  ELSE.
    PERFORM CREATE_PR.
  ENDIF.
*
ENDFORM.                    " END_RTN
*&---------------------------------------------------------------------*
*&      Form  ERR_CHECK_RTN
*&---------------------------------------------------------------------*
FORM ERR_CHECK_RTN.
*
DATA: L_DUMMY1(100) TYPE C.
DATA: L_DUMMY2(100) TYPE C.
DATA: L_DUMMY3(100) TYPE C.
DATA: L_LIFNR(10)   TYPE C.
DATA: L_MATNR(18)   TYPE C.
DATA: L_MENGE(20)   TYPE C.
DATA: L_ETD(08)     TYPE C.
DATA: L_ETA(08)     TYPE C.
DATA: L_WERKS(04)   TYPE C.
DATA: L_AFNAM(12)   TYPE C.
DATA: L_IDNLF(35)   TYPE C.
DATA: L_1000(1000)  TYPE C.
DATA: L_DATE        TYPE D.
*
  REFRESH: I_FILE,
           O_ERR.
*
  LOOP AT TBL_CHECK INTO REC_CHECK.
    SPLIT REC_CHECK-TEXT AT ',' INTO L_DUMMY1
                                     L_MATNR
                                     L_MENGE
                                     L_ETD
                                     L_ETA
                                     L_DUMMY2
                                     L_DUMMY3
                                     L_1000.
*
    CLEAR  : I_FILE,
             O_ERR.
*
*--  MODEL Check
    SELECT SINGLE *
      FROM MARA
     WHERE MATNR = L_MATNR.
    IF SY-SUBRC <> 0.
      O_ERR-TABIX   = SY-TABIX.
      O_ERR-REC     = REC_CHECK.
      O_ERR-MESSAGE = 'Material does not exist'.
      APPEND O_ERR.
      CONTINUE.
    ELSE.
      I_FILE-MATERIAL = L_MATNR.
    ENDIF.
*--  QUANTITY Check
    PERFORM CHECK_NUMERIC_RTN USING L_MENGE.
    IF WK_HTYPE <> 'NUMC'.
      O_ERR-TABIX   = SY-TABIX.
      O_ERR-REC     = REC_CHECK.
      O_ERR-MESSAGE = 'Enter a numeric value (QUANTITY)'.
      APPEND O_ERR.
      CONTINUE.
    ELSE.
      I_FILE-QUANTITY = L_MENGE.
    ENDIF.
*--  ETA
    PERFORM CHECK_NUMERIC_RTN USING L_ETA.
    IF WK_HTYPE <> 'NUMC'.
      O_ERR-TABIX   = SY-TABIX.
      O_ERR-REC     = REC_CHECK.
      O_ERR-MESSAGE = 'Date is not plausible (ETA)'.
      APPEND O_ERR.
      CONTINUE.
    ENDIF.
    L_DATE = L_ETA.
    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        DATE                      = L_DATE
      EXCEPTIONS
        PLAUSIBILITY_CHECK_FAILED = 1
        OTHERS                    = 2.
    IF SY-SUBRC <> 0.
      O_ERR-TABIX   = SY-TABIX.
      O_ERR-REC     = REC_CHECK.
      O_ERR-MESSAGE = 'Date is not plausible (ETA)'.
      APPEND O_ERR.
      CONTINUE.
    ELSE.
      I_FILE-DELIV_DATE = L_ETA.
    ENDIF.
*--  ETD
    PERFORM CHECK_NUMERIC_RTN USING L_ETD.
    IF WK_HTYPE <> 'NUMC'.
      O_ERR-TABIX   = SY-TABIX.
      O_ERR-REC     = REC_CHECK.
      O_ERR-MESSAGE = 'Date is not plausible (Delivery date)'.
      APPEND O_ERR.
      CONTINUE.
    ENDIF.
    L_DATE = L_ETD.
    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        DATE                      = L_DATE
      EXCEPTIONS
        PLAUSIBILITY_CHECK_FAILED = 1
        OTHERS                    = 2.
    IF SY-SUBRC <> 0.
      O_ERR-TABIX   = SY-TABIX.
      O_ERR-REC     = REC_CHECK.
      O_ERR-MESSAGE = 'Date is not plausible (Delivery date)'.
      APPEND O_ERR.
      CONTINUE.
    ELSE.
      I_FILE-ETD = L_ETD.
    ENDIF.
*
    APPEND I_FILE.
*
  ENDLOOP.
*
ENDFORM.                    " ERR_CHECK_RTN
*&---------------------------------------------------------------------*
*&      Form  CHECK_NUMERIC_RTN
*&---------------------------------------------------------------------*
FORM CHECK_NUMERIC_RTN        USING P_IN_STRING.
*
  CALL FUNCTION 'NUMERIC_CHECK'
       EXPORTING
            STRING_IN = P_IN_STRING
       IMPORTING
            HTYPE     = WK_HTYPE.
*
ENDFORM.                    " CHECK_NUMERIC_RTN
*&---------------------------------------------------------------------*
*&      Form  CREATE_PR
*&---------------------------------------------------------------------*
FORM CREATE_PR.
*
DATA: L_WK_NEWFLG    TYPE C,
      L_WK_ENDFLG    TYPE C.
*
DATA: L_NUMBER LIKE BAPIEBANC-PREQ_NO.
DATA: L_EXTENSIONIN TYPE TABLE OF BAPIPAREX WITH HEADER LINE,
      L_PINCR       TYPE T161-PINCR,
      L_ITEMNO      TYPE T161-PINCR.
*
  CLEAR: L_PINCR.
  SELECT SINGLE PINCR
    INTO (L_PINCR)
    FROM T161
   WHERE ( BSTYP = 'B'      )
     AND ( BSART = PA_BSART ).
*
  REFRESH: O_MESSAGE.
*
  SORT I_FILE BY DELIV_DATE.
*
  LOOP AT I_FILE.
*
    AT NEW DELIV_DATE.
      L_WK_NEWFLG = 'X'.
    ENDAT.
*
    IF L_WK_NEWFLG = 'X'.
      CLEAR: L_WK_NEWFLG,
             L_WK_ENDFLG.
      CLEAR: L_ITEMNO.
      REFRESH: I_ITEM.
      REFRESH: L_EXTENSIONIN.
    ENDIF.
*
    CLEAR I_ITEM.
    L_ITEMNO = L_ITEMNO + L_PINCR.
    MOVE-CORRESPONDING I_FILE TO I_ITEM.
    I_ITEM-PREQ_ITEM  = L_ITEMNO.
    I_ITEM-DOC_TYPE   = PA_BSART.
    I_ITEM-PREQ_NAME  = PA_AFNAM.
    I_ITEM-PLANT      = PA_WERKS.
    I_ITEM-TRACKINGNO = PA_BEDNR.
    I_ITEM-DES_VENDOR = PA_LIFNR.
    I_ITEM-FIXED_VEND = PA_LIFNR.
    I_ITEM-C_AMT_BAPI = 1.
    APPEND I_ITEM.
*
*    CLEAR: L_EXTENSIONIN.
*    L_EXTENSIONIN-STRUCTURE  = 'BAPI_TE_REQUISITION_ITEM'.
*    CONCATENATE L_ITEMNO I_FILE-ETD
*           INTO L_EXTENSIONIN-VALUEPART1.
*    APPEND L_EXTENSIONIN.

*---< END >----------------------------------------------*
    AT END OF DELIV_DATE.
      L_WK_ENDFLG = 'X'.
    ENDAT.
*
    IF L_WK_ENDFLG = 'X'.
*     [Save]
      CALL FUNCTION 'BAPI_REQUISITION_CREATE'
*       EXPORTING
*         SKIP_ITEMS_WITH_ERROR          =
        IMPORTING
          NUMBER                         = L_NUMBER
        TABLES
          REQUISITION_ITEMS              = I_ITEM
*         REQUISITION_ACCOUNT_ASSIGNMENT =
*         REQUISITION_ITEM_TEXT          =
*         REQUISITION_LIMITS             =
*         REQUISITION_CONTRACT_LIMITS    =
*         REQUISITION_SERVICES           =
*         REQUISITION_SRV_ACCASS_VALUES  =
          RETURN                         = T_SO_RETURN
*         REQUISITION_SERVICES_TEXT      =
*         REQUISITION_ADDRDELIVERY       =
          EXTENSIONIN                    = L_EXTENSIONIN
          .
*
      LOOP AT T_SO_RETURN INTO SO_RETURN.
        CLEAR O_MESSAGE.
        O_MESSAGE-DELIV_DATE = I_FILE-DELIV_DATE.
        O_MESSAGE-MESSAGE    = SO_RETURN-MESSAGE.
        APPEND O_MESSAGE.
      ENDLOOP.
*
    ENDIF.
*
  ENDLOOP.
*
  FORMAT COLOR COL_HEADING ON.
  WRITE: /(15) 'Delivery date',
          (20) 'Message Text'.
  FORMAT COLOR OFF.
  LOOP AT O_MESSAGE.
    WRITE: /(15) O_MESSAGE-DELIV_DATE,
            (80) O_MESSAGE-MESSAGE.
  ENDLOOP.
*
ENDFORM.                    " CREATE_PR
*&---------------------------------------------------------------------*
*&      Form  ERR_OUTPUT
*&---------------------------------------------------------------------*
FORM ERR_OUTPUT.
*
  FORMAT COLOR COL_TOTAL ON.
  WRITE: / 'Please check!'.
  SKIP.
  FORMAT COLOR COL_HEADING ON.
  WRITE: /07      'Line',
          13      'Data',
          93(107) 'Description'.
*
  FORMAT COLOR OFF.
  LOOP AT O_ERR.
    WRITE: /01 O_ERR-TABIX,
            13 O_ERR-REC,
            93 O_ERR-MESSAGE.
  ENDLOOP.
*
ENDFORM.                    " ERR_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM USER_COMMAND.
*
  IF SY-UCOMM = 'ONLI'.
    PERFORM MAIN_RTN.
  ENDIF.
*
ENDFORM.                    " USER_COMMAND
