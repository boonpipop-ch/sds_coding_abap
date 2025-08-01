*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0450
*  Creation Date      : 30.10.2024
*  Author             : Boontip R. (Eviden)
*  Add-on ID          : SDE035
*  Description        : Billing Due List for POB
*  Copied from        : Function spec is referred to VF04 and add field POB
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  31.07.2025  420000714   Wuthichai L. - Fix logic checking delete status
*-----------------------------------------------------------------------
REPORT ZSDSSDR0370.
*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
INCLUDE ZSDSCAI9990 ##INCL_OK.
INCLUDE <ICON>      ##INCL_OK.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES : VBCO7,
         VBAK.
*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES:
  TT_VKDFIF TYPE STANDARD TABLE OF VKDFIF,
  TT_DATA   TYPE STANDARD TABLE OF ZSDSSDS112.
*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_VBTYP_SO    TYPE VBAK-VBTYP VALUE 'C',
  GC_VBTYP_DO    TYPE VBAK-VBTYP VALUE 'J',
  GC_EXIT_GENC   TYPE SY-REPID   VALUE 'ZCL_SDSSD_RV60B900',
  GC_STRUCTURE_1 TYPE TABNAME   VALUE 'ZSDSSDS112',
  GC_TCODE       TYPE SY-TCODE  VALUE 'ZSDSSD032'.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
DATA: GT_FVKDFI TYPE TT_VKDFIF ##NEEDED,
      GT_DATA   TYPE TT_DATA ##NEEDED.
*----------------------------------------------------------------------*
* RANGES
*----------------------------------------------------------------------*
DATA:
  GRT_ZZPOB_VALIDATE      TYPE RANGE OF VBAK-ZZPOB ##NEEDED,
  GRT_STAT_LIFECYCLE_PASS TYPE RANGE OF CRMS4D_SERV_H-STAT_LIFECYCLE ##NEEDED,
  GRT_AUART               TYPE RANGE OF VBAK-AUART ##NEEDED,
  GRT_VBTYP               TYPE RANGE OF VBRK-VBTYP ##NEEDED.
*----------------------------------------------------------------------*
* ALV
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
DEFINE %FCAT.
  <l_fcat>-scrtext_s = <l_fcat>-scrtext_m =
  <l_fcat>-scrtext_l = <l_fcat>-coltext =
  <l_fcat>-seltext   = <l_fcat>-tooltip =
  <l_fcat>-reptext   = &1.
END-OF-DEFINITION.

DATA:   GF_EDIT    TYPE  CHAR1 ##NEEDED.
CONSTANTS:
  GC_TRUE            TYPE FLAG VALUE 'X',
  GC_HEADER_HEIGHT_1 TYPE  I                VALUE 0,
  GC_ALV_HEIGHT_1    TYPE  I                VALUE 100.
*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN SKIP 1.
*Block : Billin data  ---------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BELEG WITH FRAME TITLE TEXT-S01.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(31) TEXT-P01 FOR FIELD P_FKDAT.
    PARAMETERS: P_FKDAT LIKE VBCO7-FKDAT.
    SELECTION-SCREEN COMMENT 52(5) TEXT-P02 FOR FIELD P_FKDAB.
    PARAMETERS: P_FKDAB LIKE VBCO7-FKDAT_BIS DEFAULT SY-DATLO.
  SELECTION-SCREEN END OF LINE.

  SELECT-OPTIONS S_FKART FOR VBCO7-FKART. "P_FKART
  SELECT-OPTIONS S_VBELN FOR VBCO7-VBELN.
SELECTION-SCREEN END   OF BLOCK BELEG.




*Block : Selection  ---------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME TITLE TEXT-S02.

*Block : Organizational Data-------------------------------------------*
  SELECTION-SCREEN BEGIN OF BLOCK ORGAN WITH FRAME TITLE TEXT-S03.
    PARAMETERS: P_VKORG   TYPE VBCO7-VKORG MEMORY ID FKO.
    SELECT-OPTIONS S_VTWEG FOR VBCO7-VTWEG.
    SELECT-OPTIONS S_SPART FOR VBCO7-SPART.
    SELECT-OPTIONS S_VSTEL FOR VBCO7-VSTEL.
    SELECT-OPTIONS S_POB   FOR VBAK-ZZPOB.
  SELECTION-SCREEN END   OF BLOCK ORGAN.

*Block : Customer Data---------------------------------------------------*
  SELECTION-SCREEN BEGIN OF BLOCK KUNDE WITH FRAME TITLE TEXT-S04.
    SELECT-OPTIONS S_KUNNR FOR VBCO7-KUNNR MATCHCODE OBJECT DEBI. "P_KUNNR
    SELECT-OPTIONS S_LLAND FOR VBCO7-LLAND.                       "P_LLAND
    SELECT-OPTIONS S_SORT FOR VBCO7-SORTKRI.                      "P_SORT
  SELECTION-SCREEN END   OF BLOCK KUNDE.

*Block : Zu selektierende Belege---------------------------------------*
  SELECTION-SCREEN BEGIN OF BLOCK CHOICE WITH FRAME TITLE TEXT-S05.
    PARAMETERS: P_ALLEA LIKE VBCO7-ALLEA AS CHECKBOX  MEMORY ID SNA.
    PARAMETERS: P_ALLEL LIKE VBCO7-ALLEL  AS CHECKBOX MEMORY ID SNL.
  SELECTION-SCREEN END OF BLOCK CHOICE.

SELECTION-SCREEN END OF BLOCK BLK2.
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  SET PARAMETER ID 'SNA' FIELD P_ALLEA.
  SET PARAMETER ID 'SNL' FIELD P_ALLEL.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_INIT_VALUE .
  PERFORM F_SUBMIT_VF04 CHANGING GT_FVKDFI.
  PERFORM F_PREPARE_DATA USING GT_FVKDFI
                         CHANGING GT_DATA.
*----------------------------------------------------------------------*
* END-OF-SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM F_DISPLAY_RESULT USING GT_DATA.
*----------------------------------------------------------------------*
*  Form F_AUTHORIZE_CHECK
*----------------------------------------------------------------------*
*  Check Authorization on t-code
*----------------------------------------------------------------------*
FORM F_AUTHORIZE_CHECK USING UF_TCODE  TYPE  SY-TCODE.

  AUTHORITY-CHECK OBJECT 'S_TCODE'     "Transaction Code Check
    ID 'TCD' FIELD UF_TCODE.

  IF SY-SUBRC <> 0.
*   Error You are not authorized to use transaction &
    MESSAGE S172(00) WITH UF_TCODE.
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_submit_vf04
*&---------------------------------------------------------------------*
FORM F_SUBMIT_VF04 CHANGING CT_FVKDFI TYPE TT_VKDFIF.
  DATA: LO_DATA TYPE REF TO DATA.
  FIELD-SYMBOLS: <L_DATA> TYPE ANY TABLE.

  CL_SALV_BS_RUNTIME_INFO=>SET( EXPORTING DISPLAY  = ABAP_FALSE
                                          METADATA = ABAP_FALSE
                                          DATA     = ABAP_TRUE ).
  SUBMIT SDBILLDL WITH P_FKDAT EQ P_FKDAT                "#EC CI_SUBMIT
                  WITH P_FKDAB EQ P_FKDAB
                  WITH P_FKART IN S_FKART
                  WITH S_VBELN IN S_VBELN
                  WITH P_VKORG EQ P_VKORG
                  WITH S_VTWEG IN S_VTWEG
                  WITH S_SPART IN S_SPART
                  WITH S_VSTEL IN S_VSTEL
                  WITH P_KUNNR IN S_KUNNR
                  WITH P_LLAND IN S_LLAND
                  WITH P_SORT  IN S_SORT
                  WITH P_ALLEA EQ P_ALLEA
                  WITH P_ALLEL EQ P_ALLEL
                  AND RETURN.

  TRY.
      CL_SALV_BS_RUNTIME_INFO=>GET_DATA_REF(
        IMPORTING
          R_DATA = LO_DATA ).
      IF LO_DATA IS BOUND.
        ASSIGN LO_DATA->* TO  <L_DATA>.
        MOVE-CORRESPONDING <L_DATA> TO CT_FVKDFI[].
      ENDIF.
    CATCH CX_SALV_BS_SC_RUNTIME_INFO.
      MESSAGE E000(ZSDSCA01) WITH 'Unable to retrieve ALV data'(m02).
  ENDTRY.
  CL_SALV_BS_RUNTIME_INFO=>CLEAR_ALL( ) .
  DELETE CT_FVKDFI WHERE VBTYP NOT IN GRT_VBTYP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_prepare_data
*&---------------------------------------------------------------------*
FORM F_PREPARE_DATA  USING    UT_FVKDFI TYPE TT_VKDFIF
                     CHANGING CT_DATA TYPE TT_DATA.

  DATA: LT_KEY TYPE TT_DATA.
  IF UT_FVKDFI[] IS INITIAL.
    RETURN.
  ENDIF.
  MOVE-CORRESPONDING UT_FVKDFI TO CT_DATA.

  "===== NOT DELIVERY=====
  LT_KEY = CT_DATA.
  DELETE LT_KEY WHERE VBTYP EQ GC_VBTYP_DO.
  IF LT_KEY[] IS NOT INITIAL.
    SELECT VBELN,
           ZZPOB
    FROM VBAK
    FOR ALL ENTRIES IN @LT_KEY
    WHERE VBELN = @LT_KEY-VBELN
    INTO TABLE @DATA(LT_SO).

    SORT LT_SO BY VBELN.
  ENDIF.


  "===== DELVERY ===
  LT_KEY = CT_DATA .
  DELETE LT_KEY WHERE VBTYP NE GC_VBTYP_DO.
  IF LT_KEY[] IS NOT INITIAL.
    SELECT B~VBELN,
           B~VBELV AS SO,
           A~AUART AS SO_AUART,
           A~ZZPOB
    FROM VBAK AS A INNER JOIN VBFA AS B
    ON  A~VBELN = B~VBELV
    FOR ALL ENTRIES IN @LT_KEY
    WHERE B~VBELN = @LT_KEY-VBELN
    AND   B~VBTYP_N = @GC_VBTYP_DO
    AND   B~VBTYP_V = @GC_VBTYP_SO
    INTO TABLE @DATA(LT_DO).

    SORT LT_DO BY  VBELN.
  ENDIF.
  IF LT_DO IS NOT INITIAL.
    SELECT ZZ1_DELIVERY_ORD,                            "#EC CI_NOFIELD
           OBJTYPE_H,
           OBJECT_ID,
           STAT_LIFECYCLE
    FROM CRMS4D_SERV_H
    FOR ALL ENTRIES IN @LT_DO
    WHERE ZZ1_DELIVERY_ORD = @LT_DO-VBELN
      AND STAT_CANCELLED   = @SPACE                         "+420000714
    INTO TABLE @DATA(LT_SERV).

    SORT LT_SERV BY ZZ1_DELIVERY_ORD.
  ENDIF.

* =====================PREPARE DATA ==============
  LOOP AT CT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>).
    IF <L_DATA>-VBTYP <> GC_VBTYP_DO.
      READ TABLE LT_SO INTO DATA(LS_SO) WITH KEY VBELN = <L_DATA>-VBELN
                                                 BINARY SEARCH.
      IF SY-SUBRC = 0.
        <L_DATA>-ZZPOB = LS_SO-ZZPOB.
      ENDIF.
      <L_DATA>-STAT_POB = ICON_GREEN_LIGHT.
    ELSE.
      READ TABLE LT_DO INTO DATA(LS_DO) WITH KEY VBELN = <L_DATA>-VBELN
                                                 BINARY SEARCH.
      IF SY-SUBRC <> 0.
        <L_DATA>-STAT_POB = ICON_GREEN_LIGHT.
      ELSE.
        <L_DATA>-ZZPOB = LS_DO-ZZPOB.
        IF LS_DO-ZZPOB NOT IN GRT_ZZPOB_VALIDATE
        OR LS_DO-SO_AUART NOT IN GRT_AUART.
          <L_DATA>-STAT_POB = ICON_GREEN_LIGHT.
        ELSE.
          READ TABLE LT_SERV INTO DATA(LS_SERV) WITH KEY ZZ1_DELIVERY_ORD = <L_DATA>-VBELN
                                                     BINARY SEARCH.
          IF SY-SUBRC <> 0
          OR LS_SERV-STAT_LIFECYCLE NOT IN GRT_STAT_LIFECYCLE_PASS .
            <L_DATA>-STAT_POB = ICON_RED_LIGHT.
          ELSE.
            <L_DATA>-STAT_POB = ICON_GREEN_LIGHT.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

  IF S_POB[] IS NOT INITIAL.
    DELETE CT_DATA WHERE ZZPOB NOT IN S_POB.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_INIT_VALUE
*&---------------------------------------------------------------------*
FORM F_INIT_VALUE .
* -- from exit
  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE
    EXPORTING
      IF_REPID = GC_EXIT_GENC
      IF_PARAM = 'ZZPOB'
    IMPORTING
      ET_RANGE = GRT_ZZPOB_VALIDATE.

  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE
    EXPORTING
      IF_REPID = GC_EXIT_GENC
      IF_PARAM = 'STAT_LIFECYCLE_PASS'
    IMPORTING
      ET_RANGE = GRT_STAT_LIFECYCLE_PASS.

* -- from this program
  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE
    EXPORTING
      IF_REPID = SY-REPID
      IF_PARAM = 'AUART'
    IMPORTING
      ET_RANGE = GRT_AUART.


  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE
    EXPORTING
      IF_REPID = SY-REPID
      IF_PARAM = 'DOC_CAT'
    IMPORTING
      ET_RANGE = GRT_VBTYP.
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_DISPLAY_RESULT
*----------------------------------------------------------------------*
*  Display Processing Result
*----------------------------------------------------------------------*
FORM F_DISPLAY_RESULT  USING  UT_DATA TYPE TT_DATA.

* Show progress
* Text-p99 : Generating ALV Report . . .
  MC_SHOW_PROGRESS 99 TEXT-P99.

* Set Container name
  GF_CONTAINER_1 = 'CTL_ALV_1'.
* Disable Header area
  GF_ALV_HEADER_1 = GC_TRUE.

*   No auto refresh in edit mode
  GF_NO_AUTO_REFRESH_1 = GC_TRUE.


* ALV Layout
  PERFORM F_ALV_LAYOUT CHANGING GS_LAYOUT_1
                                GS_VARIANT_1
                                GS_PRINT_1.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_1.
  GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_1.
  ASSIGN UT_DATA TO <G_LIST_1>.              "#EC CI_FLDEXT_OK[2610650]
* Build Field cat
  PERFORM F_ALV_BUILD_FIELDCAT CHANGING GT_FIELDCAT_1.
** Sort data
*  PERFORM F_ALV_SORT_RESULT CHANGING GT_SORT_1.
* Call ALV Screen
  CALL SCREEN 9000.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_BUILD_FIELDCAT
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT CHANGING CT_FIELDCAT  TYPE LVC_T_FCAT.

*  DATA:
*    LF_TEXT         TYPE  TEXT50.

  FIELD-SYMBOLS:
    <L_FCAT>   TYPE  LVC_S_FCAT.


* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
                              CHANGING CT_FIELDCAT.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FCAT>.

    CASE <L_FCAT>-FIELDNAME.
      WHEN 'SELKZ'
      OR   'MANDT'.
        <L_FCAT>-TECH = ABAP_TRUE.
      WHEN 'SEL'.
        %FCAT 'Select'(c01).
        <L_FCAT>-CHECKBOX = GC_TRUE.
        <L_FCAT>-EDIT     = GC_TRUE.
      WHEN 'STAT_PROCESS'.
        %FCAT 'Process'(c04).
      WHEN 'STAT_POB'.
        %FCAT 'POB status'(c02).
        <L_FCAT>-ICON = ABAP_TRUE.
      WHEN 'ZZPOB'.
        %FCAT 'POB'(c03).
        <L_FCAT>-ICON = ABAP_TRUE.
    ENDCASE.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ALV_LAYOUT
*----------------------------------------------------------------------*
*  ALV Layout for Report
*----------------------------------------------------------------------*
FORM F_ALV_LAYOUT CHANGING CS_LAYOUT  TYPE  LVC_S_LAYO
                           CS_VARIANT TYPE  DISVARIANT
                           CS_PRINT   TYPE  LVC_S_PRNT.

* Initialize Output
  CLEAR:  CS_LAYOUT, CS_VARIANT, CS_PRINT.

* determine layout
  CS_LAYOUT-SEL_MODE   = 'B'. "Multiple Selection with Push Box
  CS_LAYOUT-CWIDTH_OPT = ABAP_TRUE.
  CS_LAYOUT-ZEBRA      = GC_TRUE.

* For Variant Saving
  CS_VARIANT-REPORT  = SY-REPID.

  CS_PRINT-NO_COLWOPT = GC_TRUE.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_HANDLE_TOOLBAR_1
*----------------------------------------------------------------------*
*  Event ALV 1 Toolbar
*----------------------------------------------------------------------*
FORM F_HANDLE_TOOLBAR_1 USING UF_OBJECT TYPE REF TO	CL_ALV_EVENT_TOOLBAR_SET ##CALLED
                              UF_INTERACTIVE TYPE	CHAR01 ##NEEDED.

  INSERT VALUE #( FUNCTION = 'ZALL'
                  ICON = ICON_SELECT_ALL
                  QUICKINFO = 'Select All'(001)
                )
         INTO TABLE UF_OBJECT->MT_TOOLBAR.

  INSERT VALUE #( FUNCTION = 'ZSAL'
                  ICON = ICON_DESELECT_ALL
                  QUICKINFO = 'Deselect All'(002)
                 )
         INTO TABLE UF_OBJECT->MT_TOOLBAR.

  INSERT VALUE #( FUNCTION = 'ZPOST'
                  ICON = ICON_EXECUTE_OBJECT
                  QUICKINFO = 'Create Billing document'(003)
                  TEXT = 'Create Billing document'(003)
                )
         INTO TABLE UF_OBJECT->MT_TOOLBAR.

* Handle Toolbar as needed
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&CHECK'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&REFRESH'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP01'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&CUT'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&COPY'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&PASTE'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&UNDO'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP02'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&APPEND'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&INSERT_ROW'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&DELETE_ROW'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&COPY_ROW'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP03'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&MB_VIEW'.
  DELETE UF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&INFO'.


ENDFORM.
*----------------------------------------------------------------------*
*  Form F_USER_COMMAND_1
*----------------------------------------------------------------------*
*  User Command Processing
*----------------------------------------------------------------------*
FORM F_USER_COMMAND_1 USING UF_UCOMM  TYPE  SY-UCOMM ##CALLED.
  DATA: LS_STABLE  TYPE LVC_S_STBL.

  CASE UF_UCOMM.
    WHEN 'ZALL'.
      PERFORM F_SELECT_CHECK_BOX USING 'X'.
    WHEN 'ZSAL'.
      PERFORM F_SELECT_CHECK_BOX USING ''.
    WHEN 'ZPOST'.
      PERFORM F_CREATE_BILLING .
  ENDCASE.

  LS_STABLE-ROW = 'X'.
  LS_STABLE-COL = 'X'.

  CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = LS_STABLE
      I_SOFT_REFRESH = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form select_check_box
*&---------------------------------------------------------------------*
FORM F_SELECT_CHECK_BOX  USING UF_X TYPE C.
  LOOP AT GT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>) .
    <L_DATA>-SEL = UF_X.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CREATE_BILLING
*&---------------------------------------------------------------------*
* REFER LOGIC SUBROUTINE VKDFS_FCODE_SAMQ IN SAPLV60P
*&---------------------------------------------------------------------*
FORM F_CREATE_BILLING .
  TYPES: BEGIN OF LTS_POSTAB.
           INCLUDE TYPE VKDFI.
  TYPES:   V_FKDAT  TYPE VKDFI-FKDAT,
           V_FKART  TYPE VKDFI-FKART,
           ACTIV(1) TYPE N,
         END OF LTS_POSTAB.
  DATA: LF_SUBRC TYPE SY-SUBRC .

  DATA: LS_POSTAB TYPE LTS_POSTAB,
        LT_POSTAB TYPE STANDARD TABLE OF LTS_POSTAB,
        LS_RV60A  TYPE RV60A.
  DATA: LF_OKCODE(5)          TYPE C.

  PERFORM AUTHORITY_CHECK USING '01' 'X' CHANGING LF_SUBRC.

  LOOP AT GT_DATA ASSIGNING FIELD-SYMBOL(<L_DATA>) WHERE SEL = ABAP_TRUE.

    LF_OKCODE = 'SAMQ'. "REFER FROM VKDFS_FCODE_SAMQ
    CLEAR: LS_POSTAB,
           LT_POSTAB.


    LS_POSTAB-FKDAT = <L_DATA>-FKDAT.
    LS_POSTAB-FKART = <L_DATA>-FKART.
    "-- COMMENT OUT -- NO DATA DEFAULT FROM SELECTION SCREEN --
*    RV60A-FKART  = P_GD_DEFAULT_DATA-FKART.
*    RV60A-FKDAT  = P_GD_DEFAULT_DATA-FKDAT.
*    RV60A-FBUDA  = P_GD_DEFAULT_DATA-FBUDA.
*    RV60A-PRSDT  = P_GD_DEFAULT_DATA-PRSDT.
*    RV60A-RFBFK  = P_GD_DEFAULT_DATA-RFBFK.
    MOVE-CORRESPONDING <L_DATA> TO LS_POSTAB ##ENH_OK.
    APPEND LS_POSTAB TO LT_POSTAB.

    SET PARAMETER ID 'VF' FIELD SPACE.
    SET PARAMETER ID 'VFR' FIELD <L_DATA>-VBELN.
    EXPORT OK-CODE      = LF_OKCODE
           POSTAB-FKDAT = LS_POSTAB-FKDAT
           POSTAB-FKART = LS_POSTAB-FKART
           POSTAB-VKORG = LS_POSTAB-VKORG
           RV60A-FKART  = LS_RV60A-FKART
           RV60A-FKDAT  = LS_RV60A-FKDAT
           RV60A-FBUDA  = LS_RV60A-FBUDA
           RV60A-PRSDT  = LS_RV60A-PRSDT
           RV60A-RFBFK  = LS_RV60A-RFBFK
           POSTAB       = LT_POSTAB
           TO MEMORY ID 'VF04'.
    CALL TRANSACTION 'VF01' AND SKIP FIRST SCREEN.       "#EC CI_CALLTA
*    IMPORT OK-CODE
*           POSTAB-FKDAT
*           POSTAB-FKART
*           POSTAB-VKORG
*           RV60A-FKART
*           RV60A-FKDAT
*           RV60A-FBUDA
*           RV60A-PRSDT
*           RV60A-RFBFK
*           POSTAB
*           FROM MEMORY ID 'VF04'.
    FREE MEMORY ID 'VF04'.

    DATA : LF_VF01_PARAMETER TYPE VBRK-VBELN.
    GET PARAMETER ID 'VF' FIELD LF_VF01_PARAMETER.
    IF LF_VF01_PARAMETER IS INITIAL.
      <L_DATA>-STATF = '@0W@'.
    ELSE.
      <L_DATA>-STAT_PROCESS = '1'.
      <L_DATA>-STATF = '@0V@'.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
* REFER LOGIC SUBROUTINE AUTHORITY_CHECK IN SAPLV60P
*&---------------------------------------------------------------------*
FORM AUTHORITY_CHECK USING AKTIVITAET TYPE ANY
                           NACHRICHT TYPE ANY
                     CHANGING RCODE TYPE ANY.
  AUTHORITY-CHECK OBJECT 'V_VBRK_VKO'
     ID 'VKORG' DUMMY
     ID 'ACTVT' FIELD AKTIVITAET.
  CLEAR RCODE.
  IF SY-SUBRC <> 0.
    RCODE = SY-SUBRC.
    IF NACHRICHT <> SPACE.
      IF AKTIVITAET = '03'.
        MESSAGE E124(VR).
      ELSE.
        MESSAGE E123(VR).
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
