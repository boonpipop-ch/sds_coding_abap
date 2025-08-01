*-----------------------------------------------------------------------
*  Program ID         : ZSDSMMR0560
*  Creation Date      : 21.10.2024
*  Author             : B.chiewsarikij
*  Add-on ID          : N/A
*  Description        : Upload Material Serial from excel file
*  Purpose            : N/A
*  Copied from        : ZMM_CREATE_SERIAL(ECC)
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT  ZSDSMMR0560.
*---------------------------------------------------------*
*                       CONSTANTS                         *
*---------------------------------------------------------*
CONSTANTS: GC_SAVE(1) TYPE C      VALUE 'A',
           GC_EQTYP   TYPE EQTYP  VALUE 'M'.

************************************************************************
*      D E C L A R E  T A B L E & S T R U C T  U R E & V A R I A B L E *
************************************************************************
TYPE-POOLS: SLIS.

TYPES: BEGIN OF TY_DATA,
         MATNR TYPE MARA-MATNR,           "Material
         SERNR TYPE RISA0-SERNR,          "Serial
       END OF TY_DATA.

TYPES: BEGIN OF TY_OUTPUT,
         SEL    TYPE C,
         MATNR  TYPE MARA-MATNR,           "Material
         SERNR  TYPE RISA0-SERNR,          "Serial
         STATUS TYPE ICON-ID,              "Status
         MSGTYP TYPE C,                    "Message type
         MESSG  TYPE CHAR100,              "Message
       END OF TY_OUTPUT.

DATA: GT_DATA   TYPE TABLE OF TY_DATA,
      GS_DATA   TYPE TY_DATA,
      GT_OUTPUT TYPE TABLE OF TY_OUTPUT,
      GS_OUTPUT TYPE TY_OUTPUT.

DATA: GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      GS_FIELDCAT TYPE SLIS_FIELDCAT_ALV,
      GS_LAYOUT   TYPE SLIS_LAYOUT_ALV.

DATA: BDCDATA TYPE TABLE OF BDCDATA WITH HEADER LINE.
*&---------------------------------------------------------------------*
*&  M A C R O   C O M M A N D   D E F I N I T I O N                    *
*&---------------------------------------------------------------------*
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
************************************************************************
*      S E L E C T I O N  S C R E E N                                  *
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-B01.


  PARAMETERS: P_FILE LIKE RLGRAP-FILENAME DEFAULT
                       'C:\'.
SELECTION-SCREEN END OF BLOCK B1.

************************************************************************
*      A T     S E L E C T I O N     S C R E E N                       *
************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM F_GET_PATH_NAME CHANGING P_FILE.

************************************************************************
*      B E G I N      S E L E C T I O N                                *
************************************************************************
START-OF-SELECTION .

* Get data
  PERFORM F_UPLOAD_DATA.
  PERFORM F_VALIDATE_DATA.

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
*      Description: This form is used for get PC path.                 *
************************************************************************
FORM F_GET_PATH_NAME  CHANGING P_FILEPATH.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      MASK             = ',*.xls.'
      MODE             = 'O'
      TITLE            = 'Browsed file'
    IMPORTING
      FILENAME         = P_FILEPATH
    EXCEPTIONS
      INV_WINSYS       = 01
      NO_BATCH         = 02
      SELECTION_CANCEL = 03
      SELECTION_ERROR  = 04.

ENDFORM.                    " f_get_path_name
*&---------------------------------------------------------------------*
*&      Form  F_UPLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_UPLOAD_DATA .
  DATA: LT_FILE TYPE TABLE OF ALSMEX_TABLINE WITH HEADER LINE,
        LW_FILE TYPE ALSMEX_TABLINE.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = P_FILE
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 1
      I_END_COL               = 9999
      I_END_ROW               = 9999
    TABLES
      INTERN                  = LT_FILE
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.
  IF SY-SUBRC <> 0.
*    MESSAGE 'Cannot upload file. Please check version excel.' TYPE 'E'.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*Process Records and get only records into table IT_DATA
  IF LT_FILE[] IS NOT INITIAL.

    LOOP AT LT_FILE INTO LW_FILE.

*      MOVE the records into final table.
      CASE LW_FILE-COL.
        WHEN '0001'.
          GS_DATA-MATNR = LW_FILE-VALUE.
        WHEN '0002'.
          GS_DATA-SERNR = LW_FILE-VALUE.
        WHEN OTHERS.
      ENDCASE.

      AT END OF ROW.
        APPEND GS_DATA TO GT_DATA .
        CLEAR GS_DATA.
      ENDAT.
      CLEAR : LW_FILE.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " F_UPLOAD_DATA

*&---------------------------------------------------------------------*
*&      Form  F_VALIDATE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_VALIDATE_DATA .
  DATA: LW_EQUI  TYPE EQUI,
        LV_MATNR TYPE MATNR,
        LV_EQTYP TYPE EQTYP.

  LOOP AT GT_DATA INTO GS_DATA.
    CLEAR GS_OUTPUT.
    MOVE-CORRESPONDING GS_DATA TO GS_OUTPUT.

*-Check Category type
    CLEAR LV_EQTYP.
    SELECT SINGLE EQTYP INTO LV_EQTYP
      FROM T370T
      WHERE EQTYP EQ GC_EQTYP.
    IF LV_EQTYP IS INITIAL.
      GS_OUTPUT-MSGTYP = 'E'.
      GS_OUTPUT-STATUS = '@0A@'."Red light
      CONCATENATE TEXT-T02 GC_EQTYP TEXT-E02
             INTO GS_OUTPUT-MESSG SEPARATED BY SPACE.
    ENDIF.

*-Check Material
    IF GS_DATA-MATNR IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          INPUT  = GS_DATA-MATNR
        IMPORTING
          OUTPUT = GS_DATA-MATNR.
      IF SY-SUBRC EQ 0.
        CLEAR LV_MATNR.
        SELECT SINGLE MATNR INTO LV_MATNR
          FROM MARA
          WHERE MATNR = GS_DATA-MATNR.
        IF SY-SUBRC <> 0 .
          GS_OUTPUT-MSGTYP = 'E'.
          GS_OUTPUT-STATUS = '@0A@'."Red light
          CONCATENATE TEXT-T01 GS_DATA-MATNR TEXT-E01
                 INTO GS_OUTPUT-MESSG SEPARATED BY SPACE.
        ENDIF.
      ENDIF.
    ENDIF.

*-Check Serial Number
    IF GS_DATA-SERNR IS NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_GERNR_INPUT'
        EXPORTING
          INPUT  = GS_DATA-SERNR
        IMPORTING
          OUTPUT = GS_DATA-SERNR.

      IF SY-SUBRC EQ 0.
        CLEAR LV_MATNR.
        SELECT SINGLE * INTO LW_EQUI
          FROM EQUI
          WHERE SERNR EQ GS_DATA-SERNR
            AND MATNR EQ GS_DATA-MATNR.
        IF SY-SUBRC EQ 0 .
          GS_OUTPUT-MSGTYP = 'E'.
          GS_OUTPUT-STATUS = '@0A@'."Red light
          CONCATENATE TEXT-T03 GS_DATA-SERNR
                      TEXT-E03 GS_DATA-MATNR
                 INTO GS_OUTPUT-MESSG SEPARATED BY SPACE.
        ENDIF.
      ENDIF.
    ENDIF.

    IF GS_OUTPUT-MSGTYP IS INITIAL.
      GS_OUTPUT-MSGTYP = 'S'.
      GS_OUTPUT-STATUS = '@08@'."Green light
    ENDIF.

    APPEND GS_OUTPUT TO GT_OUTPUT.
  ENDLOOP.
ENDFORM.                    " F_VALIDATE_DATA
*&---------------------------------------------------------------------*
*&      Form  F_PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_PROCESS_DATA .
  LOOP AT GT_DATA INTO GS_DATA.
    MOVE-CORRESPONDING GS_DATA TO GS_OUTPUT.
    APPEND GS_OUTPUT TO GT_OUTPUT.
  ENDLOOP.
ENDFORM.                    " F_PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_DISPLAY_REPORT .

  PERFORM F_FILL_FIELDCAT.

  PERFORM F_PREPARE_LAYOUT.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-REPID
      I_SAVE                   = GC_SAVE
*     is_variant               = g_variant
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
* Status
* ----------------------------------------------------
FORM STATUS_SET USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD' EXCLUDING RT_EXTAB.
ENDFORM.                    "status_set
*&---------------------------------------------------------------------*
*&      Form  usercommand
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_UCOMM    text
*      -->I_SELFIELD text
*----------------------------------------------------------------------*
FORM USERCOMMAND USING I_UCOMM I_SELFIELD TYPE SLIS_SELFIELD.

  DATA: LT_MESS_TAB TYPE TAB_BDCMSGCOLL,
        LW_MESS_TAB TYPE BDCMSGCOLL.

  DATA: LV_MODE   TYPE C VALUE 'N',
        LV_UPD    TYPE C VALUE 'S',
        LV_MSGTYP TYPE C.

  CASE I_UCOMM.
    WHEN '&UPLOAD'.                                         "Call IQ01

      PERFORM F_CHECK_ITEM_ERROR.

      LOOP AT GT_OUTPUT INTO GS_OUTPUT WHERE SEL EQ 'X'
                                         AND MSGTYP EQ 'S'.

        IF NOT GS_OUTPUT-MATNR IS INITIAL.

          PERFORM BDC_DYNPRO      USING 'SAPMIEQ0' '1000'.
          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                        'RISA0-SERNR'.
          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                        '/00'.
          PERFORM BDC_FIELD       USING 'RISA0-MATNR'
                                        GS_OUTPUT-MATNR.
          PERFORM BDC_FIELD       USING 'RISA0-SERNR'
                                        GS_OUTPUT-SERNR.
          PERFORM BDC_FIELD       USING 'RM63E-EQTYP'
                                        GC_EQTYP.
          PERFORM BDC_DYNPRO      USING 'SAPMIEQ0' '0101'.
          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                        '=BU'.

          REFRESH LT_MESS_TAB.
          CALL TRANSACTION 'IQ01' USING BDCDATA
                                UPDATE LV_UPD
                                MODE   LV_MODE
                                MESSAGES INTO LT_MESS_TAB.
          REFRESH BDCDATA.
          CLEAR LV_MSGTYP.
          READ TABLE LT_MESS_TAB INTO LW_MESS_TAB WITH KEY MSGTYP = 'S'.
          IF SY-SUBRC EQ 0.
            LV_MSGTYP = 'S'.
            GS_OUTPUT-STATUS = '@08@'."Green light

          ELSE.
            LOOP AT LT_MESS_TAB INTO LW_MESS_TAB WHERE MSGTYP = 'A'
                                                    OR MSGTYP = 'E'.
              EXIT.
            ENDLOOP.
            IF SY-SUBRC EQ 0.
              LV_MSGTYP = 'E'.
              GS_OUTPUT-STATUS  = '@0A@'."Red light
            ENDIF.
          ENDIF.

          MESSAGE ID LW_MESS_TAB-MSGID TYPE LV_MSGTYP
              NUMBER LW_MESS_TAB-MSGNR
                WITH LW_MESS_TAB-MSGV1
                     LW_MESS_TAB-MSGV2
                     LW_MESS_TAB-MSGV3
                     LW_MESS_TAB-MSGV4
              INTO   GS_OUTPUT-MESSG.

          MODIFY GT_OUTPUT FROM GS_OUTPUT TRANSPORTING STATUS MESSG.
        ENDIF.
      ENDLOOP.

    WHEN OTHERS.

  ENDCASE.

  I_SELFIELD-REFRESH = 'X'.
  I_SELFIELD-COL_STABLE = 'X'.
  I_SELFIELD-ROW_STABLE = 'X'.

ENDFORM.                    "USERCOMMAND
*&---------------------------------------------------------------------*
*&      Form  F_FILL_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_FILL_FIELDCAT .
  DATA: LV_COLUMN TYPE I.
*-Material
  ADD 1 TO LV_COLUMN.
  M_FILL_CAT 'GT_OUTPUT' 'MATNR' LV_COLUMN TEXT-C01 SPACE 18.
*-Serial
  ADD 1 TO LV_COLUMN.
  M_FILL_CAT 'GT_OUTPUT' 'SERNR' LV_COLUMN TEXT-C02 SPACE 18.

  ADD 1 TO LV_COLUMN.
  M_FILL_CAT 'GT_OUTPUT' 'STATUS' LV_COLUMN TEXT-C03 SPACE 6.

  ADD 1 TO LV_COLUMN.
  M_FILL_CAT 'GT_OUTPUT' 'MESSG' LV_COLUMN TEXT-C04 SPACE 70.
ENDFORM.                    " F_FILL_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_PREPARE_LAYOUT .
  GS_LAYOUT-BOX_FIELDNAME      = 'SEL'.
  GS_LAYOUT-ZEBRA              = 'X'.     " striped pattern
ENDFORM.                    " F_PREPARE_LAYOUT
*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
  IF FVAL IS NOT INITIAL.
    CLEAR BDCDATA.
    BDCDATA-FNAM = FNAM.
    BDCDATA-FVAL = FVAL.
    APPEND BDCDATA.
  ENDIF.
ENDFORM.                    "BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_ERROR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_ERROR  text
*----------------------------------------------------------------------*
FORM F_CHECK_ITEM_ERROR.
  LOOP AT GT_OUTPUT INTO GS_OUTPUT WHERE SEL = 'X'
                                     AND MSGTYP = 'E'.
    EXIT.
  ENDLOOP.
  IF SY-SUBRC EQ 0.
    MESSAGE I000(38) WITH TEXT-T04.
  ENDIF.
ENDFORM.                    " F_CHECK_ERROR
