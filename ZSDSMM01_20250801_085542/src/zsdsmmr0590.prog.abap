*-----------------------------------------------------------------------
*  Program ID         : ZSDSMMR0590
*  Creation Date      : 31/10/2024
*  Author             : B. CHIEWSARIKIJ (SDS)
*  Add-on ID          : N/A
*  Description        : Program export Serial (E-DO Sony)
*  Purpose            : N/A
*  Copied from        : ZMM_EXPORT_SERIAL_EDO
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSMMR0590  MESSAGE-ID ZSDSMM01.

*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES: LIKP,
        LIPS,
        OBJK,
        SER01.

*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES: BEGIN OF TY_SER01,
         LIEF_NR TYPE SER01-LIEF_NR,
         POSNR   TYPE SER01-POSNR,
         OBKNR   TYPE SER01-OBKNR,
       END OF TY_SER01.

TYPES: BEGIN OF TY_OBJK,
         OBKNR TYPE OBJK-OBKNR,
         OBZAE TYPE OBJK-OBZAE,
         SERNR TYPE OBJK-SERNR,
         MATNR TYPE OBJK-MATNR,
       END OF TY_OBJK.
TYPES: BEGIN OF TYP_DO_NO,
         VBELN TYPE LIKP-VBELN,
       END OF TYP_DO_NO.

*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
*DATA:
*-> internal tables
DATA: GT_SER01 TYPE STANDARD TABLE OF TY_SER01,
      GW_SER01 TYPE TY_SER01,
      WA_SER01 TYPE TY_SER01,
      GT_OBJK  TYPE STANDARD TABLE OF TY_OBJK,
      GW_OBJK  TYPE TY_OBJK,
      WA_OBJK  TYPE TY_OBJK.
DATA: GT_DO_TEMP TYPE STANDARD TABLE OF TYP_DO_NO,
      WA_DO_TEMP TYPE TYP_DO_NO.

DATA: FILE_NAME(10) TYPE C.
DATA: TEXT_NAME(100) TYPE C.
DATA: BEGIN OF GT_OUTTAB OCCURS 0,
        DOC_NO(10),
        DOC_ITEM(6),
        SERNR(18),
      END OF GT_OUTTAB.
*-> range
*-> work areas
*-> variables
*-> reference

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
  SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
    SELECT-OPTIONS: S_VBELN  FOR LIKP-VBELN.
  SELECTION-SCREEN END OF BLOCK B1.
  SELECTION-SCREEN BEGIN OF BLOCK B5 WITH FRAME TITLE TEXT-005.
*    PARAMETERS: R_SER RADIOBUTTON GROUP GR3 USER-COMMAND UCOM,
*                R_LOC RADIOBUTTON GROUP GR3 DEFAULT 'X'.
    PARAMETER: R_LOC AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN END OF BLOCK B5.

  SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
    PARAMETERS: P_F2 AS CHECKBOX  USER-COMMAND UCOM. "DEFAULT 'X'
    PARAMETERS: P_FILE2 LIKE RLGRAP-FILENAME DEFAULT 'C:\' OBLIGATORY MODIF ID SA2.

  SELECTION-SCREEN END OF BLOCK B2.
*-----------------------------------------------------------------------
* I N I T I A L I Z A T I O N
*-----------------------------------------------------------------------
INITIALIZATION.

*-----------------------------------------------------------------------
* A T   S E L E C T I O N - S C R E E N
*-----------------------------------------------------------------------
*AT SELECTION-SCREEN OUTPUT.
*AT SELECTION-SCREEN ON HELP-REQUEST FOR <field>
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR <field>
*AT SELECTION-SCREEN ON <field>
*AT SELECTION-SCREEN.
AT SELECTION-SCREEN OUTPUT.
  PERFORM MODIFY_SCREEN.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE2.
  PERFORM GET_PATH_NAME USING P_FILE2.

AT SELECTION-SCREEN.
  PERFORM CHECK_SCREEN_FIELD.
*-----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*-----------------------------------------------------------------------
START-OF-SELECTION.START-OF-SELECTION .



  IF P_F2 = 'X'.
    PERFORM GET_DATA_OUT.
  ENDIF.

  IF NOT GT_OUTTAB IS INITIAL .
*    MESSAGE s001 WITH lv_chk_logrt.
    PERFORM DOWNLOAD_DATA.
  ELSE.
    MESSAGE S001 WITH 'Serial is not found'.
*     Download Data

*    IF p_f2 = 'X'.
*      PERFORM insert_ztmm_export_do.
*    ENDIF.
  ENDIF.

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
FORM  GET_DATA_OUT.

  SELECT LIEF_NR POSNR OBKNR
  INTO TABLE GT_SER01
  FROM SER01
  WHERE LIEF_NR IN S_VBELN.

  IF GT_SER01 IS NOT INITIAL.


    SELECT OBKNR OBZAE  SERNR MATNR
      INTO TABLE GT_OBJK
      FROM OBJK
      FOR ALL ENTRIES IN GT_SER01
      WHERE OBKNR EQ GT_SER01-OBKNR.

    IF GT_OBJK IS NOT INITIAL.

      LOOP AT GT_SER01 INTO GW_SER01 .

        LOOP AT GT_OBJK INTO GW_OBJK WHERE OBKNR EQ GW_SER01-OBKNR.
          GT_OUTTAB-DOC_NO =  GW_SER01-LIEF_NR.
          GT_OUTTAB-DOC_ITEM =  GW_SER01-POSNR.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              INPUT  = GW_SER01-POSNR
            IMPORTING
              OUTPUT = GT_OUTTAB-DOC_ITEM.

          GT_OUTTAB-SERNR =  GW_OBJK-SERNR.

          APPEND GT_OUTTAB .
        ENDLOOP.


      ENDLOOP.
    ENDIF.


  ENDIF.
ENDFORM.                    " get_data_outbound
*&---------------------------------------------------------------------*
*&      Form  download_data
*&---------------------------------------------------------------------*
FORM DOWNLOAD_DATA .
  DATA: FILE1        TYPE STRING,
        FILE2        TYPE STRING,
        FILE3        TYPE STRING,
        FILE4        TYPE STRING,
        FILE5        TYPE STRING,
        FILE6        TYPE STRING, "CH29
        L_TEXT(4096) TYPE C OCCURS 0,
        LV_TEXT(100) TYPE C.

* outbound file
  IF P_F2 IS NOT INITIAL.
    "Add by Wantanee 20140807

    IF GT_OUTTAB[] IS NOT INITIAL.


      IF P_F2 = 'X' .

        SELECT VBELN
        INTO TABLE GT_DO_TEMP
        FROM LIKP
        WHERE VBELN IN S_VBELN
        ORDER BY VBELN.

        LOOP AT GT_DO_TEMP INTO WA_DO_TEMP.
          IF SY-TABIX EQ 1.
            LV_TEXT = WA_DO_TEMP-VBELN.
            TEXT_NAME = WA_DO_TEMP-VBELN.
          ELSE.
            CONCATENATE TEXT_NAME WA_DO_TEMP-VBELN+6(4) INTO TEXT_NAME SEPARATED BY ','.
          ENDIF.
        ENDLOOP.
        FILE_NAME = TEXT_NAME.

*        IF NOT R_SER IS INITIAL.
*
*          IF SY-SYSID EQ 'T41'.
*
*            CONCATENATE '\\172.31.136.37\wh_sony\OUT\QAS\SDS_SERIAL\' 'SER_' TEXT_NAME '.txt' INTO P_FILE2.
*          ELSEIF SY-SYSID EQ 'T44'.
*            CONCATENATE '\\172.31.136.37\wh_sony\OUT\QAS\SDS_SERIAL\' 'SER_' TEXT_NAME '.txt' INTO P_FILE2.
*          ELSE.
*            CONCATENATE '\\172.31.136.37\wh_sony\OUT\PRD\SDS_SERIAL\' 'SER_' TEXT_NAME '.txt' INTO P_FILE2.
*          ENDIF.
*
*        ENDIF.

      ENDIF.
      FILE2 = P_FILE2.

      IF R_LOC IS INITIAL.
        CALL FUNCTION 'GUI_DOWNLOAD'
          EXPORTING
            FILENAME              = FILE2
            FILETYPE              = 'ASC'
            CODEPAGE              = '8600'
            TRUNC_TRAILING_BLANKS = SPACE
          TABLES
            DATA_TAB              = GT_OUTTAB[]
          EXCEPTIONS
            FILE_OPEN_ERROR       = 1
            FILE_READ_ERROR       = 2.

      ELSE.
        CALL FUNCTION 'GUI_DOWNLOAD'
          EXPORTING
            FILENAME              = FILE2
            FILETYPE              = 'ASC'
            CODEPAGE              = '8600'
            TRUNC_TRAILING_BLANKS = SPACE
          TABLES
            DATA_TAB              = GT_OUTTAB[]
          EXCEPTIONS
            FILE_OPEN_ERROR       = 1
            FILE_READ_ERROR       = 2.
      ENDIF.
      MESSAGE S001 WITH 'Download Outbound data Completed !'.
    ELSE.
      MESSAGE S001 WITH 'No Data serial For SONY!'.
    ENDIF.
  ENDIF.

ENDFORM.                    " download_data

*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
FORM MODIFY_SCREEN .

  DATA: LV_TEXT(100) TYPE C.


  IF P_F2 = 'X'.

    SELECT VBELN
    INTO TABLE GT_DO_TEMP
    FROM LIKP
    WHERE VBELN IN S_VBELN
    ORDER BY VBELN.

    LOOP AT GT_DO_TEMP INTO WA_DO_TEMP.
      IF SY-TABIX EQ 1.
        LV_TEXT = WA_DO_TEMP-VBELN.
        TEXT_NAME = WA_DO_TEMP-VBELN.
      ELSE.
        CONCATENATE TEXT_NAME WA_DO_TEMP-VBELN+6(4) INTO TEXT_NAME SEPARATED BY ','.
      ENDIF.
    ENDLOOP.
    FILE_NAME = TEXT_NAME.

    IF NOT R_LOC IS  INITIAL.

      CONCATENATE 'C:\O-'    SY-DATUM '.txt' INTO P_FILE2.
    ELSE.

      IF SY-SYSID EQ 'T41'.
        CONCATENATE '\\172.31.136.37\wh_sony\OUT\QAS\SDS_SERIAL\' 'SER_' TEXT_NAME '.txt' INTO P_FILE2.

      ELSEIF SY-SYSID EQ 'T44'.
        CONCATENATE '\\172.31.136.37\wh_sony\OUT\QAS\SDS_SERIAL\' 'SER_' TEXT_NAME '.txt' INTO P_FILE2.
      ELSE.
        CONCATENATE '\\172.31.136.37\wh_sony\OUT\PRD\SDS_SERIAL\' 'SER_' TEXT_NAME '.txt' INTO P_FILE2.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " MODIFY_SCREEN
*&---------------------------------------------------------------------*
*&      Form  GET_PATH_NAME
*&---------------------------------------------------------------------*
FORM GET_PATH_NAME  USING    PATH.
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

ENDFORM.                    " GET_PATH_NAME

*&---------------------------------------------------------------------*
*&      Form  CHECK_SCREEN_FIELD
*&---------------------------------------------------------------------*
FORM CHECK_SCREEN_FIELD .
  IF P_F2 IS INITIAL.
    MESSAGE E001 WITH 'Please, Select checkbox at least 1 !'.
  ENDIF.

ENDFORM.                    " CHECK_SCREEN_FIELD
