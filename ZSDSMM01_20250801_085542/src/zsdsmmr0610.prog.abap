*-----------------------------------------------------------------------
*  Program ID         : ZSDSMMR0610
*  Creation Date      : 01/11/2024
*  Author             : B.CHIEWSARIKIJ (SDS)
*  Add-on ID          : N/A
*  Description        : Upload Serial No. to Inbound Delivery
*  Purpose            : N/A
*  Copied from        : ZMM_UPLOAD_SERIAL (ECC)
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Report ZSDSMMR0610
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSDSMMR0610.
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
*TABLES:

*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
*TYPES:

*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
*DATA:
*-> internal tables
*-> range
*-> work areas
*-> variables
*-> reference
DATA : BEGIN OF IN_TAB OCCURS 0,
         DELIVERY(10),
         ITEM(6),
         SERIAL(18),
       END OF IN_TAB.
*-----------------------------------------------------------------------
* C O N S T A N T S
*-----------------------------------------------------------------------
*CONSTANTS:

*-----------------------------------------------------------------------
* M A C R O S
*-----------------------------------------------------------------------
*DEFINE.
*END-OF-DEFINITION.

*-----------------------------------------------------------------------
* F I E L D – S Y M B O L S
*-----------------------------------------------------------------------
*FIELD-SYMBOLS:

*-----------------------------------------------------------------------
* F I E L D – G R O U P S
*-----------------------------------------------------------------------
*FIELD-GROUPS:

*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-B01.
  SELECTION-SCREEN COMMENT /1(40) TEXT-001.

  PARAMETERS: P_FILE LIKE RLGRAP-FILENAME  DEFAULT
                       'C:\'.

  PARAMETERS: P_CHECK AS CHECKBOX DEFAULT SPACE.
SELECTION-SCREEN END OF BLOCK B1.

*-----------------------------------------------------------------------
* I N I T I A L I Z A T I O N
*-----------------------------------------------------------------------
INITIALIZATION.

*-----------------------------------------------------------------------
* A T   S E L E C T I O N - S C R E E N
*-----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.
  PERFORM F_GET_SELECTION.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM GET_PATH_NAME CHANGING P_FILE.

*-----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*-----------------------------------------------------------------------
START-OF-SELECTION.
*-> read data
* Get data
  IF P_CHECK NE 'X'.
    PERFORM UPLOAD_DATA.
  ELSE.
    PERFORM F_IMPORT_DATA.
  ENDIF.

*Process data
  PERFORM PROCESS_DATA.
*-----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*-----------------------------------------------------------------------
END-OF-SELECTION.
*-> process data

*-----------------------------------------------------------------------
* T O P - O F – P A G E
*-----------------------------------------------------------------------
*TOP-OF-PAGE.

*-----------------------------------------------------------------------
* AT LINE-SELECTION
*-----------------------------------------------------------------------
*AT LINE-SELECTION.

*-----------------------------------------------------------------------
* F O R M S
*-----------------------------------------------------------------------
************************************************************************
*      FORM GET_PATH_NAME                                               *
*----------------------------------------------------------------------*
*      Description: This form is used for get PC path.                          *
************************************************************************
FORM GET_PATH_NAME  CHANGING PATH.

  DATA: L_LENGTH TYPE I.
  DATA: L_MASK(20) TYPE C.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      MASK             = ',*.txt,*.txt.'
      MODE             = 'S'
    IMPORTING
      FILENAME         = PATH
    EXCEPTIONS
      INV_WINSYS       = 01
      NO_BATCH         = 02
      SELECTION_CANCEL = 03
      SELECTION_ERROR  = 04.

ENDFORM.                    " get_path_name
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_DATA .

  CALL FUNCTION 'WS_UPLOAD'
    EXPORTING
      FILENAME        = P_FILE
      FILETYPE        = 'DAT'
    TABLES
      DATA_TAB        = IN_TAB
    EXCEPTIONS
      FILE_OPEN_ERROR = 1
      FILE_READ_ERROR = 2.


ENDFORM.                    " UPLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_DATA .

  DATA : L_DOH      LIKE BAPIIBDLVHDRCHG,
         L_DOHC     LIKE BAPIIBDLVHDRCTRLCHG,
         L_DELIVERY LIKE BAPIIBDLVHDRCHG-DELIV_NUMB.

  DATA : LT_SERIAL LIKE BAPIDLVITMSERNO OCCURS 0 WITH HEADER LINE,
         LT_RET    LIKE BAPIRET2 OCCURS 0 WITH HEADER LINE.

  LOOP AT IN_TAB.

    IF SY-TABIX = '1'.
      L_DOH-DELIV_NUMB = IN_TAB-DELIVERY.
      L_DOHC-DELIV_NUMB = IN_TAB-DELIVERY.
      L_DELIVERY =  IN_TAB-DELIVERY.
    ENDIF.

    LT_SERIAL-DELIV_NUMB = IN_TAB-DELIVERY.
    LT_SERIAL-ITM_NUMBER = IN_TAB-ITEM.
    LT_SERIAL-SERIALNO = IN_TAB-SERIAL.
    APPEND LT_SERIAL.
    CLEAR LT_SERIAL.

  ENDLOOP.

  CALL FUNCTION 'BAPI_INB_DELIVERY_CHANGE'
    EXPORTING
      HEADER_DATA    = L_DOH
      HEADER_CONTROL = L_DOHC
      DELIVERY       = L_DELIVERY
    TABLES
      ITEM_SERIAL_NO = LT_SERIAL
      RETURN         = LT_RET.
  READ TABLE LT_RET WITH KEY TYPE = 'E'.
  IF SY-SUBRC <> 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    IF P_CHECK EQ 'X'.
*      UPDATE ZTSD_STATUS_DO SET SERNF = 'X'
*                          WHERE DONUM EQ L_DOH-DELIV_NUMB.
    ENDIF.
    IF P_CHECK NE 'X'.
      WRITE : 'Upload Complete'.
    ELSE.
      MESSAGE S001(Z_MM) WITH 'Upload Complete'.
    ENDIF.
  ELSE.
    IF P_CHECK NE 'X'.
      WRITE : 'Upload not complete'.
    ELSE.
      MESSAGE S001(Z_MM) WITH 'Upload not complete' DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.

ENDFORM.                    " PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  F_IMPORT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_IMPORT_DATA .
  IMPORT LT_SERIAL_TMP TO IN_TAB[] FROM MEMORY ID 'SER'. " from Program ZP_MM_RETRIEVE_STATUS_DO.
  FREE MEMORY ID 'SER'.
ENDFORM.                    " F_IMPORT_DATA
*&---------------------------------------------------------------------*
*&      Form  F_GET_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_SELECTION .
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'P_CHECK'.
      SCREEN-INTENSIFIED = '1'.
      SCREEN-ACTIVE      = '0'.
      SCREEN-DISPLAY_3D  = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " F_GET_SELECTION
