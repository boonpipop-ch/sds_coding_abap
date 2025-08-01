*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0230_F01
*&---------------------------------------------------------------------*
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
*& Form F_GET_SN_iv
*&---------------------------------------------------------------------*
FORM F_GET_SN_IV  CHANGING CT_SN_IV TYPE TT_SN_IV.
  DATA: LS_SN_IV    TYPE TS_SN_IV,
        LS_STYLEROW TYPE LVC_S_STYL.
  DATA: LF_MAIN TYPE FLAG.
  SELECT A~VBELN AS BILNO,
         C~VBELN AS SO,
         B~VGBEL AS DO,
         A~KUNAG,
         A~KUNRG,   "CH01 420000161+
         A~FKART,
         D~XCPDK AS KUNAG_XCPDK, "CH03 420000161+
         D~NAME1 AS KUNAG_NM,
         D2~XCPDK AS KUNRG_XCPDK, "CH03 420000161+
         D2~NAME1 AS KUNRG_NM, "CH01 420000161+
         A~FKDAT,
         A~ZTERM,
         F~PSPHI,
         B~VKBUR,
         A~NETWR,
         A~MWSBK,
         A~WAERK,
         E~VBELN,
         E~ZCHECK,
         E~ZSTATUS,
         E~ZBLLST,
         E~ZCLLST,
         E~ZREMARK,
         E~ZADMIN,
         E~ZSEND_N,
         E~ZSEND_D,
         E~ZSEND_T,
         E~ZRCVD_N,
         E~ZRCVD_D,
         E~ZRCVD_T,
         E~ZRCVD_PERNR "CH05+
  INTO TABLE @DATA(LT_DATA)
  FROM VBRK AS A INNER JOIN VBRP AS B
  ON A~VBELN = B~VBELN
                 LEFT OUTER JOIN VBAK AS C
  ON B~AUBEL = C~VBELN
                 INNER JOIN KNA1 AS D
  ON A~KUNAG = D~KUNNR
                 INNER JOIN KNA1 AS D2    "CH01 420000161+
  ON A~KUNRG = D2~KUNNR                   "CH01 420000161+
                 LEFT OUTER JOIN ZSDSFIT027 AS E
  ON A~VBELN = E~VBELN
                 LEFT OUTER JOIN PRPS AS F
  ON B~PS_PSP_PNR = F~PSPNR
*  WHERE A~KUNAG IN @S_KUNNR "CH01 420000161-
  WHERE A~KUNRG IN @S_KUNNR "CH01 420000161+
  AND   A~BUKRS IN @S_BUKRS
  AND   A~FKDAT IN @S_FKDAT
  AND   A~VBELN IN @S_BILNO
  AND   A~VKORG IN @S_VKORG
  AND   A~VTWEG IN @S_VTWEG
  AND   A~SPART IN @S_SPART
  AND   B~VKGRP IN @S_VKGRP
  AND   B~VKBUR IN @S_VKBUR
  AND   A~ERNAM IN @S_ERNAM
  AND   A~FKSTO = ''
  AND   A~GBSTK = @GC_GBSTK_COMPLETE.

  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

  DELETE LT_DATA WHERE ZCHECK = GC_ZCHECK-RECEIVED
                 OR    ZCHECK = GC_ZCHECK-SENT.

  IF S_SO[] IS NOT INITIAL.
    DELETE LT_DATA WHERE SO NOT IN S_SO.
  ENDIF.
  IF LT_DATA IS INITIAL.
    RETURN.
  ENDIF.

  SORT LT_DATA BY BILNO SO DO .
  DELETE ADJACENT DUPLICATES FROM LT_DATA COMPARING BILNO SO DO.

*BOD CH02  420000145 Do not check found in ZSDSFIT028 then remove it
*  "---get DO in ZSDSFIT028 for excluding
*  DATA(LT_DO_KEY) = LT_DATA[].
*  SORT LT_DO_KEY BY DO.
*  DELETE LT_DO_KEY WHERE DO IS INITIAL.
*  DELETE ADJACENT DUPLICATES FROM LT_DO_KEY COMPARING DO.
*  IF LT_DO_KEY[] IS NOT INITIAL.
*    SELECT VBELN
*    INTO TABLE @DATA(LT_ZSDSFIT028)
*    FROM ZSDSFIT028
*    FOR ALL ENTRIES IN @LT_DO_KEY
*    WHERE VBELN = @LT_DO_KEY-DO.
*    SORT LT_ZSDSFIT028 BY VBELN.
*  ENDIF.
*  LOOP AT LT_DATA INTO DATA(LS_DATA) ##INTO_OK.
*    READ TABLE LT_ZSDSFIT028 TRANSPORTING NO FIELDS WITH KEY VBELN = LS_DATA-DO
*                                                    BINARY SEARCH.
*    IF SY-SUBRC = 0.
*      DELETE LT_DATA WHERE BILNO = LS_DATA-BILNO.
*    ENDIF.
*  ENDLOOP.
*EOD CH02 420000145
  SORT LT_DATA BY BILNO SO.
  DELETE ADJACENT DUPLICATES FROM LT_DATA COMPARING BILNO SO.

  IF LT_DATA[] IS NOT INITIAL.
    "--- select fi doc
    SELECT B~VBELN,
           A~BUKRS,
           A~BELNR,
           A~GJAHR,
           A~XBLNR
*    FROM BKPF AS A INNER JOIN BSID_VIEW AS B "CH01 420000161-
     FROM BKPF AS A INNER JOIN BSEG AS B      "CH01 420000161+
    ON  A~BUKRS = B~BUKRS
    AND A~GJAHR = B~GJAHR
    AND A~BELNR = B~BELNR
    FOR ALL ENTRIES IN @LT_DATA
    WHERE B~VBELN = @LT_DATA-BILNO
    INTO TABLE @DATA(LT_FIDOC).

    SORT LT_FIDOC BY VBELN.

* BOI CH01 420000147
* -- get wbs from vbrp
    SELECT A~VBELN,
           POSNR,
           PS_PSP_PNR,
           B~PSPHI
    FROM VBRP AS A INNER JOIN @LT_DATA AS DATA
    ON A~VBELN = DATA~BILNO
                   INNER JOIN PRPS AS B
    ON A~PS_PSP_PNR = B~PSPNR
    INTO TABLE @DATA(LT_VBRP_WBS).

    SORT LT_VBRP_WBS BY VBELN .
* EOI CH01 420000147

    IF LT_VBRP_WBS IS NOT INITIAL.  "CH01 420000147+
      SELECT  PSPNR,
              POST1
      INTO TABLE @DATA(LT_PROJ)
      FROM PROJ
*      FOR ALL ENTRIES IN @LT_DATA    "CH01 420000147-
      FOR ALL ENTRIES IN @LT_VBRP_WBS "CH01 420000147+
      WHERE PSPNR = @LT_VBRP_WBS-PSPHI.
      SORT LT_PROJ BY PSPNR.
    ENDIF.                            "CH01 420000147+

* BOI CH03 420000161
    SELECT VBPA~VBELN,
           PARVW,
           NATION,
           KUNNR,
           ADRNR,
           NAME1
    FROM VBPA INNER JOIN ADRC
    ON VBPA~ADRNR = ADRC~ADDRNUMBER
              INNER JOIN @LT_DATA AS ITAB
    ON VBPA~VBELN = ITAB~BILNO
    WHERE PARVW IN ( @GC_SOLD_TO , @GC_PAYER )
    AND ( ITAB~KUNAG_XCPDK = 'X'  OR ITAB~KUNRG_XCPDK = 'X' )
    INTO TABLE @DATA(LT_VBPA).

    SORT LT_VBPA BY VBELN PARVW NATION.
* EOI CH03 420000161
  ENDIF.

  "----prepare data
  LOOP AT LT_DATA INTO DATA(LS_DATA) ##INTO_OK.
    CLEAR: LS_SN_IV,
           LF_MAIN.
    AT NEW BILNO.
      LF_MAIN = GC_TRUE.
    ENDAT.
    READ TABLE LT_FIDOC INTO DATA(LS_FIDOC) WITH KEY VBELN = LS_DATA-BILNO
                                                     BINARY SEARCH.
    IF SY-SUBRC = 0.
      LS_SN_IV-BELNR = LS_FIDOC-BELNR.
      LS_SN_IV-XBLNR = LS_FIDOC-XBLNR.
    ELSE.
      IF  GR_FKART_NO_FIDOC IS INITIAL
      OR  LS_DATA-FKART NOT IN GR_FKART_NO_FIDOC .
        CONTINUE.
      ENDIF.
    ENDIF.
    LS_SN_IV = CORRESPONDING #( BASE ( LS_SN_IV ) LS_DATA ).
*   BOI CH03 420000161
    IF LS_DATA-KUNAG_XCPDK = 'X'."ONE TIME
      READ TABLE LT_VBPA INTO DATA(LS_VBPA) WITH KEY VBELN = LS_SN_IV-BILNO
                                                     PARVW = GC_SOLD_TO.
      LS_SN_IV-KUNAG_NM = LS_VBPA-NAME1.
    ENDIF.
    IF LS_DATA-KUNRG_XCPDK = 'X'."ONE TIME
      READ TABLE LT_VBPA INTO LS_VBPA WITH KEY VBELN = LS_SN_IV-BILNO
                                               PARVW = GC_PAYER.
      LS_SN_IV-KUNRG_NM = LS_VBPA-NAME1.
    ENDIF.
*   EOI CH03 420000161
*   BOI CH01 420000147
    IF LS_SN_IV-PSPHI IS INITIAL.
      READ TABLE LT_VBRP_WBS INTO DATA(LS_VBRP_WBS) WITH KEY VBELN = LS_DATA-BILNO
                                                             BINARY SEARCH.
      IF SY-SUBRC = 0.
        LS_SN_IV-PSPHI = LS_VBRP_WBS-PSPHI.
      ENDIF.
    ENDIF.
*   EOI CH01 420000147
    IF LS_SN_IV-PSPHI IS NOT INITIAL.
      READ TABLE LT_PROJ INTO DATA(LS_PROJ) WITH KEY PSPNR = LS_SN_IV-PSPHI
                                                     BINARY SEARCH.
      IF SY-SUBRC = 0.
        LS_SN_IV-POST1 = LS_PROJ-POST1.
      ENDIF.
    ENDIF.
    LS_SN_IV-MAIN = LF_MAIN.
    LS_SN_IV-TOTAL = LS_SN_IV-NETWR + LS_SN_IV-MWSBK.
    IF LS_SN_IV-ZSTATUS IS NOT INITIAL.
      READ TABLE GT_ZSTATUS INTO DATA(LS_ZSTATUS) WITH KEY VALUE = LS_SN_IV-ZSTATUS.
      IF SY-SUBRC = 0.
        LS_SN_IV-ZSTATUS_TX = LS_ZSTATUS-TEXT.
      ENDIF.
    ENDIF.
    CASE LS_SN_IV-ZCHECK.
      WHEN GC_ZCHECK-SENT.
        LS_SN_IV-SENT = GC_TRUE.
      WHEN GC_ZCHECK-REJECTED.
        LS_SN_IV-REJECTED = GC_TRUE.
    ENDCASE.
    IF LF_MAIN = ''.
      CLEAR: LS_SN_IV-NETWR,
             LS_SN_IV-MWSBK,
             LS_SN_IV-TOTAL.

      LS_STYLEROW-FIELDNAME = 'SENT' .
      LS_STYLEROW-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      INSERT LS_STYLEROW  INTO TABLE LS_SN_IV-FIELD_STYLE.
      LS_STYLEROW-FIELDNAME = 'ZSTATUS' .
      LS_STYLEROW-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      INSERT LS_STYLEROW  INTO TABLE LS_SN_IV-FIELD_STYLE.
      LS_STYLEROW-FIELDNAME = 'ZADMIN' .
      LS_STYLEROW-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      INSERT LS_STYLEROW  INTO TABLE LS_SN_IV-FIELD_STYLE.
    ENDIF.
    APPEND LS_SN_IV TO CT_SN_IV.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DISPLAY_RESULT
*&---------------------------------------------------------------------*
FORM F_DISPLAY_RESULT  USING  UT_DATA TYPE ANY TABLE.
* Show progress
* Text-p99 : Generating ALV Report . . .
  MC_SHOW_PROGRESS 99 TEXT-P99.

* Set Container name
  GF_CONTAINER_1 = 'CTL_ALV_1'.
* Disable Header area
  GF_ALV_HEADER_1 = GC_TRUE.
  IF GF_EDIT EQ SPACE.
*   Enable Soft Refresh only in display mode
    GF_SOFT_REFRESH_1 = GC_TRUE.
  ELSE.
*   No auto refresh in edit mode
    GF_NO_AUTO_REFRESH_1 = GC_TRUE.
  ENDIF.

* ALV Layout
  PERFORM F_ALV_LAYOUT CHANGING GS_LAYOUT_1
                                GS_VARIANT_1
                                GS_PRINT_1.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_1.
  GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_1.
  ASSIGN UT_DATA TO <G_LIST_1>.              "#EC CI_FLDEXT_OK[2610650]
* Build Field cat, sort
  PERFORM F_ALV_BUILD_FIELDCAT    USING UT_DATA
                                  CHANGING GT_FIELDCAT_1.
  CASE GF_EXEC_TY.
    WHEN GC_EXEC_TY-SN_IV.
      PERFORM F_ALV_MODIFY_FC_SN_IV   CHANGING GT_FIELDCAT_1.
      PERFORM F_ALV_SORT_RESULT_SN_IV CHANGING GT_SORT_1.
    WHEN GC_EXEC_TY-RC_IV.
      PERFORM F_ALV_MODIFY_FC_RC_IV   CHANGING GT_FIELDCAT_1.
      PERFORM F_ALV_SORT_RESULT_RC_IV CHANGING GT_SORT_1.
    WHEN GC_EXEC_TY-DP_IV.
      PERFORM F_ALV_MODIFY_FC_DP_IV   CHANGING GT_FIELDCAT_1.
      PERFORM F_ALV_SORT_RESULT_DP_IV CHANGING GT_SORT_1.
    WHEN GC_EXEC_TY-SN_PJ
      OR GC_EXEC_TY-RC_PJ
      OR GC_EXEC_TY-DP_PJ.
      PERFORM F_ALV_MODIFY_FC_PJ   CHANGING GT_FIELDCAT_1.
      PERFORM F_ALV_SORT_RESULT_PJ CHANGING GT_SORT_1.
  ENDCASE.
* Call ALV Screen
  CALL SCREEN 9000.
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
  CS_LAYOUT-SEL_MODE   = 'B'.
  CS_LAYOUT-CWIDTH_OPT = GC_TRUE.
  CASE GF_EXEC_TY .
    WHEN GC_EXEC_TY-SN_IV.
      CS_LAYOUT-STYLEFNAME = 'FIELD_STYLE'.
  ENDCASE.

* For Variant Saving
  CS_VARIANT-REPORT  = SY-REPID.
  CS_VARIANT-LOG_GROUP = GF_EXEC_TY.
  CS_PRINT-NO_COLWOPT = GC_TRUE.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_BUILD_FIELDCAT
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT USING UT_DATA TYPE ANY TABLE
                       CHANGING CT_FIELDCAT TYPE LVC_T_FCAT.
  DATA: LF_COLUMNS      TYPE REF TO CL_SALV_COLUMNS_TABLE,
        LF_AGGREGATIONS TYPE REF TO CL_SALV_AGGREGATIONS,
        LF_SALV_TABLE   TYPE REF TO CL_SALV_TABLE,
        LF_TABLE        TYPE REF TO DATA.

  FIELD-SYMBOLS: <TABLE> TYPE ANY TABLE.

* create unprotected table from import data
  CREATE DATA LF_TABLE LIKE  UT_DATA.
  ASSIGN LF_TABLE->* TO <TABLE>.

*...New ALV Instance ...............................................
  TRY.
      CL_SALV_TABLE=>FACTORY(
        EXPORTING
          LIST_DISPLAY = ABAP_FALSE
        IMPORTING
          R_SALV_TABLE = LF_SALV_TABLE
        CHANGING
          T_TABLE      = <TABLE> ).
    CATCH CX_SALV_MSG ##NO_HANDLER.
  ENDTRY.
  LF_COLUMNS  = LF_SALV_TABLE->GET_COLUMNS( ).
  LF_AGGREGATIONS = LF_SALV_TABLE->GET_AGGREGATIONS( ).

  CALL METHOD CL_SALV_CONTROLLER_METADATA=>GET_LVC_FIELDCATALOG
    EXPORTING
      R_COLUMNS      = LF_COLUMNS
      R_AGGREGATIONS = LF_AGGREGATIONS
    RECEIVING
      T_FIELDCATALOG = CT_FIELDCAT[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_init_value
*&---------------------------------------------------------------------*
FORM F_INIT_VALUE .
  CONSTANTS: LC_ACTIVATE TYPE DD07T-AS4LOCAL VALUE 'A'.
  DATA: LS_ZSTATUS TYPE TS_ZSTATUS.

  CASE GC_TRUE.
    WHEN RB_SN_IV.
      GF_EXEC_TY = GC_EXEC_TY-SN_IV.
      GF_EDIT = GC_TRUE.
    WHEN RB_RC_IV.
      GF_EXEC_TY = GC_EXEC_TY-RC_IV.
      GF_EDIT = GC_TRUE.
    WHEN RB_DP_IV.
      GF_EXEC_TY = GC_EXEC_TY-DP_IV.
    WHEN RB_SN_PJ.
      GF_EXEC_TY = GC_EXEC_TY-SN_PJ.
      GF_EDIT = GC_TRUE.
    WHEN RB_RC_PJ.
      GF_EXEC_TY = GC_EXEC_TY-RC_PJ.
      GF_EDIT = GC_TRUE.
    WHEN RB_DP_PJ.
      GF_EXEC_TY = GC_EXEC_TY-DP_PJ.
  ENDCASE.

  SELECT VALPOS,
         DOMVALUE_L,
         DDTEXT
  INTO TABLE @DATA(LT_DD07T)
  FROM DD07T
  WHERE DOMNAME = @GC_DOMNAME_ZSTATUS
  AND   DDLANGUAGE = @SY-LANGU
  AND   AS4LOCAL = @LC_ACTIVATE.

  SORT  LT_DD07T BY VALPOS.

  LOOP AT LT_DD07T INTO DATA(LS_DD07T) ##INTO_OK.
    CLEAR LS_ZSTATUS.
    LS_ZSTATUS-VALUE =  LS_DD07T-DOMVALUE_L.
    LS_ZSTATUS-TEXT = LS_DD07T-DDTEXT.
    APPEND LS_ZSTATUS TO GT_ZSTATUS.
  ENDLOOP.


  SELECT *
  INTO TABLE GT_STAT_ACT
  FROM ZSDSFIC028.                                      "#EC CI_NOWHERE

  LOOP AT GT_STAT_ACT INTO DATA(LS_STAT_ACT) ##INTO_OK.
    IF LS_STAT_ACT-BANK_VENDOR = ABAP_TRUE.
      INSERT VALUE #( SIGN = 'I'
                      OPTION = 'EQ'
                      LOW = LS_STAT_ACT-STAT_ACT
                      HIGH = '' )
      INTO TABLE GR_BANK_VENDOR.
    ENDIF.
  ENDLOOP.

  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE
    EXPORTING
      IF_REPID = SY-REPID
      IF_PARAM = 'BILL_TYPE_NO_FIDOC'
    IMPORTING
      ET_RANGE = GR_FKART_NO_FIDOC.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ALV_MODIFY_FC_SN_IV
*&---------------------------------------------------------------------*
FORM F_ALV_MODIFY_FC_SN_IV  CHANGING CT_FIELDCAT TYPE LVC_T_FCAT.

  LOOP AT CT_FIELDCAT ASSIGNING FIELD-SYMBOL(<FCAT>) .
    CASE <FCAT>-FIELDNAME.
      WHEN 'MAIN'
        OR 'ZCHECK'
        OR 'VBELN'
        OR 'ZREMARK'
        OR 'ZBLLST'
        OR 'ZCLLST'.
        <FCAT>-TECH = GC_TRUE.
      WHEN 'SENT'.
        %FCAT 'Send'(c01).
        <FCAT>-CHECKBOX = GC_TRUE.
        <FCAT>-COL_POS = 1.
        <FCAT>-EDIT    = GC_TRUE.
        <FCAT>-FIX_COLUMN = ABAP_TRUE.
      WHEN 'REJECTED'.
        %FCAT 'Reject'(c02).
        <FCAT>-CHECKBOX = GC_TRUE.
        <FCAT>-COL_POS = 2.
        <FCAT>-FIX_COLUMN = ABAP_TRUE.
      WHEN 'BILNO'.
        <FCAT>-HOTSPOT = GC_TRUE.
        <FCAT>-COL_POS = <FCAT>-COL_POS + 2.
        <FCAT>-FIX_COLUMN = ABAP_TRUE.
      WHEN 'SO'.
        <FCAT>-HOTSPOT = GC_TRUE.
        <FCAT>-COL_POS = <FCAT>-COL_POS + 2.
        <FCAT>-FIX_COLUMN = ABAP_TRUE.
      WHEN 'XBLNR'.
        %FCAT 'Tax Invoice'(c08).
        <FCAT>-COL_POS = <FCAT>-COL_POS + 2.
        <FCAT>-FIX_COLUMN = ABAP_TRUE.
      WHEN 'POST1'.
        %FCAT 'Project Description'(c10).
        <FCAT>-COL_POS = <FCAT>-COL_POS + 2.
      WHEN 'BELNR'.
        %FCAT 'Receipt No.'(c09).
        <FCAT>-COL_POS = <FCAT>-COL_POS + 2.
        <FCAT>-FIX_COLUMN = ABAP_TRUE.
      WHEN 'ZSTATUS'.
        <FCAT>-EDIT       = GC_TRUE.
        <FCAT>-REF_TABLE  = 'ZSDSFIT027'.
        <FCAT>-REF_FIELD  = 'ZSTATUS'.
        <FCAT>-COL_POS = <FCAT>-COL_POS + 2.
      WHEN 'ZADMIN'.
        <FCAT>-EDIT       = GC_TRUE.
        <FCAT>-COL_POS    = <FCAT>-COL_POS + 2.
      WHEN 'NETWR'
      OR   'MWSBK'.
        <FCAT>-CFIELDNAME = 'WAERK'.
        <FCAT>-COL_POS    = <FCAT>-COL_POS + 2.
      WHEN 'TOTAL'.
        <FCAT>-CFIELDNAME = 'WAERK'.
        %FCAT 'Total Amount'(c05).
        <FCAT>-COL_POS    = <FCAT>-COL_POS + 2.
      WHEN 'ZSTATUS_TX'.
        %FCAT 'Status Description'(c03).
        <FCAT>-COL_POS = <FCAT>-COL_POS + 2.
*BOI CH01 420000161
      WHEN 'KUNAG_NM'.
        %FCAT 'Sold-to Name'(c11).
        <FCAT>-COL_POS = <FCAT>-COL_POS + 2.
      WHEN 'KUNRG_NM'.
        %FCAT 'Payer Name'(c12).
        <FCAT>-COL_POS = <FCAT>-COL_POS + 2.
*EOI CH01 420000161
      WHEN OTHERS.
        <FCAT>-COL_POS = <FCAT>-COL_POS + 2.
    ENDCASE.
  ENDLOOP.
ENDFORM.
*----------------------------------------------------------------------*
*       Form  f_top_of_page_1
*----------------------------------------------------------------------*
*       ALV TOP-OF-PAGE Event
*----------------------------------------------------------------------*
FORM F_TOP_OF_PAGE_1 USING UF_DYNDOC_ID  TYPE  REF TO CL_DD_DOCUMENT. "#EC CALLED

  CONSTANTS:
    LC_SIZE_KEY TYPE  SDYDO_VALUE  VALUE '18%',
    LC_SIZE_VAL TYPE  SDYDO_VALUE  VALUE '82%'.

  DATA:
    LF_TEXT    TYPE  SDYDO_TEXT_ELEMENT,
    LF_TABLE   TYPE  REF TO CL_DD_TABLE_ELEMENT,
    LF_COL_KEY TYPE  REF TO CL_DD_AREA,
    LF_COL_VAL TYPE  REF TO CL_DD_AREA.

* Create table
  CALL METHOD UF_DYNDOC_ID->ADD_TABLE
    EXPORTING
      NO_OF_COLUMNS = 2
      BORDER        = '0'
      WIDTH         = '50%'
    IMPORTING
      TABLE         = LF_TABLE.
* Get Column Element
  CALL METHOD LF_TABLE->ADD_COLUMN
    EXPORTING
      WIDTH  = LC_SIZE_KEY
    IMPORTING
      COLUMN = LF_COL_KEY.
* Get Column Element
  CALL METHOD LF_TABLE->ADD_COLUMN
    EXPORTING
      WIDTH  = LC_SIZE_VAL
    IMPORTING
      COLUMN = LF_COL_VAL.
* Set Key column style
  CALL METHOD LF_TABLE->SET_COLUMN_STYLE
    EXPORTING
      COL_NO       = 1
      SAP_EMPHASIS = CL_DD_AREA=>STRONG.

*-----------------------
* Add value in Line1
*-----------------------
  CALL METHOD LF_TABLE->NEW_ROW.
* Text-h01 : Operation Selection:
  LF_TEXT = TEXT-H01.
  CALL METHOD LF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  CASE GC_TRUE.
    WHEN RB_SN_IV.
      LF_TEXT = TEXT-H11.
    WHEN RB_RC_IV.
      LF_TEXT = TEXT-H12.
    WHEN RB_DP_IV.
      LF_TEXT = TEXT-H13.
    WHEN RB_SN_PJ.
      LF_TEXT = TEXT-H14.
    WHEN RB_RC_PJ.
      LF_TEXT = TEXT-H15.
    WHEN RB_DP_PJ.
      LF_TEXT = TEXT-H16.
  ENDCASE.
  CALL METHOD LF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_SORT_RESULT
*----------------------------------------------------------------------*
*  Maintain SORT data for Result ALV
*----------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT_SN_IV  CHANGING CT_SORT TYPE LVC_T_SORT.

  CONSTANTS:
    LC_SORT1 TYPE  LVC_S_SORT-FIELDNAME VALUE 'BILNO',
    LC_SORT2 TYPE  LVC_S_SORT-FIELDNAME VALUE 'SO'.

  DATA:
    LS_SORT  TYPE  LVC_S_SORT.


* Initialize Output
  CLEAR: CT_SORT.


  CLEAR LS_SORT.
  LS_SORT-SPOS      = 1.
  LS_SORT-FIELDNAME = LC_SORT1.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO CT_SORT.


  CLEAR LS_SORT.
  LS_SORT-SPOS      = 2.
  LS_SORT-FIELDNAME = LC_SORT2.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO CT_SORT.


ENDFORM.
*----------------------------------------------------------------------*
*  Form F_HANDLE_TOOLBAR_1
*----------------------------------------------------------------------*
*  Event ALV 1 Toolbar
*----------------------------------------------------------------------*
FORM F_HANDLE_TOOLBAR_1 USING UF_OBJECT TYPE REF TO	CL_ALV_EVENT_TOOLBAR_SET ##CALLED
                              UF_INTERACTIVE TYPE	CHAR01 ##NEEDED.

* Handle Toolbar as needed
  IF GF_EDIT EQ GC_TRUE.
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
  ENDIF.
*  BOI CH01 420000145
  IF GF_EXEC_TY = GC_EXEC_TY-SN_IV
  OR GF_EXEC_TY =  GC_EXEC_TY-SN_PJ.
    INSERT VALUE #( FUNCTION = 'ZALL'
                    ICON = '@4B@'
                    QUICKINFO = 'Select All'(001) )
           INTO TABLE UF_OBJECT->MT_TOOLBAR.

    INSERT VALUE #( FUNCTION = 'ZSAL'
                    ICON = '@4D@'
                    QUICKINFO = 'Deselect All'(002) )
           INTO TABLE UF_OBJECT->MT_TOOLBAR.
  ENDIF.
* EOI CH01 420000145
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ON_DATA_CHANGED_1
*----------------------------------------------------------------------*
*  Event ALV 1 Data is changed
*----------------------------------------------------------------------*
FORM F_ON_DATA_CHANGED_1 USING UF_DATA_CHANGED TYPE REF TO  CL_ALV_CHANGED_DATA_PROTOCOL ##CALLED
                               UF_ONF4 TYPE  CHAR01          ##NEEDED
                               UF_ONF4_BEFORE TYPE  CHAR01   ##NEEDED
                               UF_ONF4_AFTER TYPE  CHAR01    ##NEEDED
                               UF_UCOMM TYPE  SY-UCOMM       ##NEEDED.
  CASE GF_EXEC_TY.
    WHEN GC_EXEC_TY-SN_IV.
      PERFORM F_ON_DATA_CHANGED_SN_IV
              USING UF_DATA_CHANGED
                    UF_ONF4
                    UF_ONF4_BEFORE
                    UF_ONF4_AFTER
                    UF_UCOMM .
    WHEN GC_EXEC_TY-RC_IV.
      PERFORM F_ON_DATA_CHANGED_RC_IV
              USING UF_DATA_CHANGED
                    UF_ONF4
                    UF_ONF4_BEFORE
                    UF_ONF4_AFTER
                    UF_UCOMM .
    WHEN GC_EXEC_TY-SN_PJ
     OR GC_EXEC_TY-RC_PJ .
      PERFORM F_ON_DATA_CHANGED_PJ
              USING UF_DATA_CHANGED
                    UF_ONF4
                    UF_ONF4_BEFORE
                    UF_ONF4_AFTER
                    UF_UCOMM .
  ENDCASE.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ON_DATA_CHANGED_SN_IV
*----------------------------------------------------------------------*
*  Event ALV 1 Data is changed
*----------------------------------------------------------------------*
FORM F_ON_DATA_CHANGED_SN_IV  USING UF_DATA_CHANGED TYPE REF TO  CL_ALV_CHANGED_DATA_PROTOCOL ##CALLED
                                    UF_ONF4 TYPE  CHAR01          ##NEEDED
                                    UF_ONF4_BEFORE TYPE  CHAR01   ##NEEDED
                                    UF_ONF4_AFTER TYPE  CHAR01    ##NEEDED
                                    UF_UCOMM TYPE  SY-UCOMM       ##NEEDED.
  DATA: LS_STABLE  TYPE LVC_S_STBL,
        LF_REFRESH TYPE FLAG.

* For all valid changing cells
  LOOP AT UF_DATA_CHANGED->MT_GOOD_CELLS ASSIGNING FIELD-SYMBOL(<L_GOOD_CELL>).

*   Read Row
    READ TABLE GT_SN_IV ASSIGNING FIELD-SYMBOL(<L_SN_IV>)
                         INDEX <L_GOOD_CELL>-ROW_ID.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    CASE <L_GOOD_CELL>-FIELDNAME.

      WHEN 'ZSTATUS'.
        IF  <L_GOOD_CELL>-VALUE IS NOT INITIAL.
          READ TABLE GT_ZSTATUS INTO DATA(LS_ZSTATUS)
                                WITH KEY VALUE = <L_GOOD_CELL>-VALUE ##WARN_OK.
          IF SY-SUBRC = 0.
            <L_SN_IV>-ZSTATUS_TX = LS_ZSTATUS-TEXT.
            LF_REFRESH = GC_TRUE.
          ENDIF.
        ENDIF.

    ENDCASE.

  ENDLOOP.
  IF LF_REFRESH = GC_TRUE.
    LS_STABLE-ROW = 'X'.
    LS_STABLE-COL = ''.

    CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE      = LS_STABLE
        I_SOFT_REFRESH = ''.
  ENDIF.
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ON_DATA_CHANGED_RC_IV
*----------------------------------------------------------------------*
*  Event ALV 1 Data is changed
*----------------------------------------------------------------------*
FORM F_ON_DATA_CHANGED_RC_IV  USING UF_DATA_CHANGED TYPE REF TO  CL_ALV_CHANGED_DATA_PROTOCOL ##CALLED
                                    UF_ONF4 TYPE  CHAR01          ##NEEDED
                                    UF_ONF4_BEFORE TYPE  CHAR01   ##NEEDED
                                    UF_ONF4_AFTER TYPE  CHAR01    ##NEEDED
                                    UF_UCOMM TYPE  SY-UCOMM       ##NEEDED.
  DATA: LS_STABLE  TYPE LVC_S_STBL,
        LF_REFRESH TYPE FLAG.
* For all valid changing cells
  LOOP AT UF_DATA_CHANGED->MT_GOOD_CELLS ASSIGNING FIELD-SYMBOL(<L_GOOD_CELL>).

*   Read Row
    READ TABLE GT_RC_IV ASSIGNING FIELD-SYMBOL(<L_RC_IV>)
                         INDEX <L_GOOD_CELL>-ROW_ID.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    CASE <L_GOOD_CELL>-FIELDNAME.
      WHEN 'RECEIVED'.
        IF  <L_GOOD_CELL>-VALUE = GC_TRUE.
          IF <L_RC_IV>-REJECTED = GC_TRUE.
            CLEAR <L_RC_IV>-REJECTED .
            LF_REFRESH = GC_TRUE.
          ENDIF.
        ENDIF.
      WHEN 'REJECTED'.
        PERFORM F_CHECK_DELETE_BANK_VENDOR USING <L_RC_IV>-VBELN
                                                 'X'.

        IF <L_GOOD_CELL>-VALUE = GC_TRUE.
          IF <L_RC_IV>-RECEIVED = GC_TRUE.
            CLEAR <L_RC_IV>-RECEIVED .
            LF_REFRESH = GC_TRUE.
          ENDIF.
          IF <L_RC_IV>-STAT_ACT01 = ABAP_TRUE
          OR <L_RC_IV>-STAT_ACT02 = ABAP_TRUE
          OR <L_RC_IV>-STAT_ACT03 = ABAP_TRUE
          OR <L_RC_IV>-STAT_ACT04 = ABAP_TRUE
          OR <L_RC_IV>-STAT_ACT05 = ABAP_TRUE.
            CLEAR: <L_RC_IV>-STAT_ACT01,
                   <L_RC_IV>-STAT_ACT02,
                   <L_RC_IV>-STAT_ACT03,
                   <L_RC_IV>-STAT_ACT04,
                   <L_RC_IV>-STAT_ACT05.
          ENDIF.
        ENDIF.
      WHEN 'STAT_ACT01'
      OR   'STAT_ACT02'
      OR   'STAT_ACT03'
      OR   'STAT_ACT04'
      OR   'STAT_ACT05'.
*       ----IF change from bank vendor to other status action -----

        "new value not bank vendor
        IF <L_GOOD_CELL>-FIELDNAME+8(2) IN GR_BANK_VENDOR .
          PERFORM F_CHECK_DELETE_BANK_VENDOR USING <L_RC_IV>-VBELN
                                                   ''.


        ENDIF.
*       ----- set related value
        PERFORM F_SET_STAT_ACT USING <L_GOOD_CELL>
                                         CHANGING <L_RC_IV>.
        LF_REFRESH = GC_TRUE.
    ENDCASE.

  ENDLOOP.
  IF LF_REFRESH = GC_TRUE.
    LS_STABLE-ROW = 'X'.
    LS_STABLE-COL = ''.

    CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE      = LS_STABLE
        I_SOFT_REFRESH = ''.
  ENDIF.
ENDFORM.
*----------------------------------------------------------------------*
*       Form  F_ALV_DISPLAY
*----------------------------------------------------------------------*
*       Display ALV Grid
*----------------------------------------------------------------------*
FORM F_ALV_DISPLAY.

* --------------------------------------------------
* Create First ALV Container
* --------------------------------------------------
  IF GREF_GRID_1 IS INITIAL.

*   Get First ALV Container
    PERFORM F_GET_CONTAINER_1  USING  'FIRST'
                                      GF_ALV_HEIGHT_1
                             CHANGING GREF_CONTAINER_GRID_1.

*   Create First ALV Object
    CREATE OBJECT GREF_GRID_1
      EXPORTING
        I_PARENT = GREF_CONTAINER_GRID_1.

*   Set Event Handler for First ALV
    CREATE OBJECT GREF_EVENT_RECEIVER_1.
    SET HANDLER:
      GREF_EVENT_RECEIVER_1->ON_PRINT_TOP_OF_PAGE_1  FOR GREF_GRID_1,
      GREF_EVENT_RECEIVER_1->ON_HOTSPOT_CLICK_1      FOR GREF_GRID_1,
      GREF_EVENT_RECEIVER_1->ON_DOUBLE_CLICK_1       FOR GREF_GRID_1,
      GREF_EVENT_RECEIVER_1->HANDLE_TOOLBAR_1        FOR GREF_GRID_1,
      GREF_EVENT_RECEIVER_1->HANDLE_USER_COMMAND_1   FOR GREF_GRID_1,
      GREF_EVENT_RECEIVER_1->ON_DATA_CHANGED_1       FOR GREF_GRID_1.


*   Show First ALV
    CALL METHOD GREF_GRID_1->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        I_BUFFER_ACTIVE               = 'X'
        IS_VARIANT                    = GS_VARIANT_1
        IS_LAYOUT                     = GS_LAYOUT_1
        IS_PRINT                      = GS_PRINT_1
        I_SAVE                        = GC_SAVE_ALL
        IT_TOOLBAR_EXCLUDING          = GT_TOOL_EXC_1
      CHANGING
        IT_OUTTAB                     = <G_LIST_1>
        IT_FIELDCATALOG               = GT_FIELDCAT_1
        IT_SORT                       = GT_SORT_1
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      CALL METHOD GREF_GRID_1->REGISTER_EDIT_EVENT
        EXPORTING
          I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.
      CALL METHOD GREF_GRID_1->REGISTER_EDIT_EVENT
        EXPORTING
          I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
    ENDIF.
  ELSE.

    IF GF_NO_AUTO_REFRESH_1 = SPACE.
      IF GF_SOFT_REFRESH_1 IS INITIAL.
        CALL METHOD GREF_GRID_1->SET_FRONTEND_FIELDCATALOG
          EXPORTING
            IT_FIELDCATALOG = GT_FIELDCAT_1.
        CALL METHOD GREF_GRID_1->SET_FRONTEND_LAYOUT
          EXPORTING
            IS_LAYOUT = GS_LAYOUT_1.
        CALL METHOD GREF_GRID_1->SET_FRONTEND_PRINT
          EXPORTING
            IS_PRINT = GS_PRINT_1.
        CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
          EXPORTING
            I_SOFT_REFRESH = ' '.
      ELSE.
        CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
          EXPORTING
            I_SOFT_REFRESH = 'X'.
      ENDIF.
    ENDIF.

  ENDIF.

* --------------------------------------------------
* Create HTML Header Container if flag is marked
* --------------------------------------------------
  IF GF_ALV_HEADER_1 EQ 'X' AND
     CL_GUI_ALV_GRID=>OFFLINE( ) IS INITIAL.

    IF GREF_CONTAINER_HTML_1 IS INITIAL.

*     Get HTML Header Container
      PERFORM F_GET_CONTAINER_1  USING  'HEADER'
                                        GF_HEADER_HIGHT_1
                               CHANGING GREF_CONTAINER_HTML_1.

*     Bind TOP-OF-PAGE Event
      SET HANDLER:
        GREF_EVENT_RECEIVER_1->ON_TOP_OF_PAGE_1        FOR GREF_GRID_1.
*     Create TOP-Document
      CREATE OBJECT GREF_DYNDOC_ID_1
        EXPORTING
          STYLE = 'ALV_GRID'.
    ENDIF.

*   Initializing document
    CALL METHOD GREF_DYNDOC_ID_1->INITIALIZE_DOCUMENT.
*   Processing events
    CALL METHOD GREF_GRID_1->LIST_PROCESSING_EVENTS
      EXPORTING
        I_EVENT_NAME = 'TOP_OF_PAGE'
        I_DYNDOC_ID  = GREF_DYNDOC_ID_1.
*   Merge all setting
    CALL METHOD GREF_DYNDOC_ID_1->MERGE_DOCUMENT.
*   Display TOP document
    CALL METHOD GREF_DYNDOC_ID_1->DISPLAY_DOCUMENT
      EXPORTING
        REUSE_CONTROL      = 'X'
        PARENT             = GREF_CONTAINER_HTML_1
      EXCEPTIONS
        HTML_DISPLAY_ERROR = 1.
    IF SY-SUBRC NE 0 ##NEEDED.
      "Do nothing
    ENDIF.

  ENDIF.

ENDFORM.                    " F_ALV_DISPLAY
*&---------------------------------------------------------------------*
*& Form F_USER_COMMAND_SAVE
*&---------------------------------------------------------------------*
FORM F_USER_COMMAND_SAVE .
  DATA: LS_STABLE  TYPE LVC_S_STBL.
  DATA:
    LF_VALID   TYPE CHAR01  ##NEEDED,
    LF_REFRESH TYPE CHAR01  ##NEEDED.

  GREF_GRID_1->CHECK_CHANGED_DATA( IMPORTING E_VALID = LF_VALID
                                   CHANGING C_REFRESH = LF_REFRESH ).
  CASE GF_EXEC_TY .
    WHEN GC_EXEC_TY-SN_IV.
      PERFORM F_USER_COMMAND_SAVE_SN_IV.
*      PERFORM F_UNLOCK_ZSDSFIT027 USING GT_SN_IV. "CH03 420000360-
      CLEAR: GT_SN_IV,
             GT_SN_IV_OLD.
      PERFORM F_GET_SN_IV CHANGING GT_SN_IV.
*      PERFORM F_LOCK_ZSDSFIT027 USING GT_SN_IV. "CH03 420000360-
      GT_SN_IV_OLD[] = GT_SN_IV[].
    WHEN GC_EXEC_TY-RC_IV.
      PERFORM F_USER_COMMAND_SAVE_RC_IV.
*      PERFORM F_UNLOCK_ZSDSFIT027 USING GT_SN_IV. "CH03 420000360-
      CLEAR: GT_RC_IV,
               GT_RC_IV_OLD.
      PERFORM F_GET_RC_IV CHANGING GT_RC_IV.
*      PERFORM F_LOCK_ZSDSFIT027 USING GT_SN_IV. "CH03 420000360-
      GT_RC_IV_OLD[] = GT_RC_IV[].
    WHEN GC_EXEC_TY-SN_PJ
    OR   GC_EXEC_TY-RC_PJ.
      PERFORM F_USER_COMMAND_SAVE_PJ.
      CLEAR: GT_PJ,
               GT_PJ_OLD.
      PERFORM F_GET_PJ CHANGING GT_PJ.
      GT_PJ_OLD[] = GT_PJ[].
  ENDCASE.
  LS_STABLE-ROW = 'X'.
  LS_STABLE-COL = ''.

  CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = LS_STABLE
      I_SOFT_REFRESH = ''.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_USER_COMMAND_EXIT
*&---------------------------------------------------------------------*
FORM F_USER_COMMAND_EXIT .
  DATA:
    LF_VALID   TYPE CHAR01,
    LF_REFRESH TYPE CHAR01,
    LF_ANS     TYPE CHAR01,
    LF_CHANGED TYPE FLAG.

  GREF_GRID_1->CHECK_CHANGED_DATA( IMPORTING E_VALID = LF_VALID
                                   CHANGING C_REFRESH = LF_REFRESH ).
*     Continue processing only when valid
  IF LF_VALID IS INITIAL.
    LEAVE TO SCREEN 0.
  ENDIF.
  PERFORM F_CHECK_CHANGED_DATA CHANGING LF_CHANGED.
  IF LF_CHANGED = ''.
    LEAVE TO SCREEN 0.
  ENDIF.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = 'Please confirm'(m02)
      TEXT_QUESTION         = 'Data was changed. Save before exiting editor ?'(m03)
      TEXT_BUTTON_1         = 'Yes'
      ICON_BUTTON_1         = 'ICON_CHECKED'
      TEXT_BUTTON_2         = 'No'
      ICON_BUTTON_2         = 'ICON_CANCEL'
      DISPLAY_CANCEL_BUTTON = ' '
      POPUP_TYPE            = 'ICON_MESSAGE_QUESTION'
    IMPORTING
      ANSWER                = LF_ANS
    EXCEPTIONS
      TEXT_NOT_FOUND        = 1
      OTHERS                = 2.
  IF SY-SUBRC <> 0.
    LEAVE TO SCREEN 0.
  ELSE.
    IF LF_ANS = 1.
      PERFORM F_USER_COMMAND_SAVE.
      LEAVE TO SCREEN 0 .
    ELSE.
      LEAVE TO SCREEN 0 .
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_USER_COMMAND_SAVE_SN_IV
*&---------------------------------------------------------------------*
FORM F_USER_COMMAND_SAVE_SN_IV .
  DATA: LT_UPDATE TYPE STANDARD TABLE OF ZSDSFIT027,
        LT_INSERT TYPE STANDARD TABLE OF ZSDSFIT027,
        LS_RECORD TYPE ZSDSFIT027.
  DATA: LF_DATUM TYPE SY-DATUM,
        LF_UZEIT TYPE SY-UZEIT.
  LF_DATUM = SY-DATUM.
  LF_UZEIT = SY-UZEIT.

  SORT GT_SN_IV_OLD BY BILNO MAIN .
  LOOP AT GT_SN_IV INTO DATA(LS_SN_IV) WHERE MAIN = GC_TRUE ##INTO_OK.
    READ TABLE GT_SN_IV_OLD INTO DATA(LS_SN_IV_OLD) WITH KEY BILNO = LS_SN_IV-BILNO
                                                             MAIN = GC_TRUE
                                                             BINARY SEARCH.
    IF SY-SUBRC = 0.
      IF LS_SN_IV <> LS_SN_IV_OLD. "SOME CHANGE
        LS_RECORD = CORRESPONDING #( BASE ( LS_RECORD ) LS_SN_IV ).
        LS_RECORD-VBELN = LS_SN_IV-BILNO.
        IF LS_SN_IV-SENT = GC_TRUE.
          LS_RECORD-ZCHECK = GC_ZCHECK-SENT.
        ELSEIF LS_SN_IV-SENT = ''.
          IF LS_SN_IV-REJECTED = GC_TRUE.
            LS_RECORD-ZCHECK = GC_ZCHECK-REJECTED.
          ELSE.
            LS_RECORD-ZCHECK = ''.
          ENDIF.
        ENDIF.
        LS_RECORD-ZSEND_N = SY-UNAME.
        LS_RECORD-ZSEND_D = LF_DATUM.
        LS_RECORD-ZSEND_T = LF_UZEIT.
        IF  LS_SN_IV-VBELN IS NOT INITIAL.
          APPEND LS_RECORD TO LT_UPDATE.
        ELSE.
          APPEND LS_RECORD TO LT_INSERT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF LT_UPDATE IS NOT INITIAL.
    UPDATE  ZSDSFIT027 FROM TABLE LT_UPDATE.
  ENDIF.
  IF LT_INSERT IS NOT INITIAL.
    INSERT ZSDSFIT027 FROM TABLE LT_INSERT.
  ENDIF.
  COMMIT WORK AND WAIT.
  MESSAGE S000(ZSDSCA01) WITH 'Data has been changed'(m01).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CHECK_CHANGED_DATA
*&---------------------------------------------------------------------*
FORM F_CHECK_CHANGED_DATA CHANGING CF_CHANGED TYPE FLAG.

  CASE GF_EXEC_TY.
    WHEN GC_EXEC_TY-SN_IV.
      IF GT_SN_IV <> GT_SN_IV_OLD.
        CF_CHANGED = GC_TRUE.
      ENDIF.
    WHEN GC_EXEC_TY-RC_IV.
      IF GT_RC_IV <> GT_RC_IV_OLD.
        CF_CHANGED = GC_TRUE.
      ENDIF.
    WHEN GC_EXEC_TY-SN_PJ
    OR   GC_EXEC_TY-RC_PJ.
      IF GT_PJ <> GT_PJ_OLD.
        CF_CHANGED = GC_TRUE.
      ENDIF.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_RC_IV
*&---------------------------------------------------------------------*
FORM F_GET_RC_IV  CHANGING CT_RC_IV TYPE TT_RC_IV.
  DATA: LS_RC_IV    TYPE TS_RC_IV.
  DATA: LRT_KUNRG TYPE RANGE OF VBRK-KUNRG.
  DATA: LF_MINUS_ONE TYPE P DECIMALS 0 VALUE -1,
        LF_FIELDNAME TYPE TEXT20.

  SELECT A~VBELN,
         C~VBELN AS SO,
         A~KUNAG,
         D~XCPDK AS KUNAG_XCPDK, "CH03 420000161+
         D~NAME1 AS KUNAG_NM,
         A~KUNRG,
         A~FKART,
         E~XCPDK AS KUNRG_XCPDK, "CH03 420000161+
         E~NAME1 AS KUNRG_NM,
         A~FKDAT,
         A~ZTERM,
         B~VKBUR,
         A~NETWR,
         A~MWSBK,
         A~WAERK,
         F~ZCHECK,
         F~ZSTATUS,
         F~ZBLLST,
         F~ZCLLST,
         F~ZREMARK,
         F~ZADMIN,
         F~ZSTAT_ACT,
         F~ZSEND_N,
         F~ZSEND_D,
         F~ZSEND_T,
         F~ZRCVD_N,
         F~ZRCVD_D,
         F~ZRCVD_T,
         F~ZRCVD_PERNR, "CH05+
         G~PSPHI
  INTO TABLE @DATA(LT_DATA)
  FROM VBRK AS A INNER JOIN VBRP AS B
  ON A~VBELN = B~VBELN
                 LEFT OUTER JOIN VBAK AS C
  ON B~AUBEL = C~VBELN
                 INNER JOIN KNA1 AS D
  ON A~KUNAG = D~KUNNR
                 INNER JOIN KNA1 AS E
  ON A~KUNRG = E~KUNNR
                 INNER JOIN ZSDSFIT027 AS F
  ON A~VBELN = F~VBELN
                 LEFT OUTER JOIN PRPS AS G
  ON B~PS_PSP_PNR = G~PSPNR
*  WHERE A~KUNAG IN @S_KUNNR "CH01 420000161-
  WHERE A~KUNRG IN @S_KUNNR "CH01 420000161+
  AND   A~BUKRS IN @S_BUKRS
  AND   A~FKDAT IN @S_FKDAT
  AND   A~VBELN IN @S_BILNO
  AND   B~AUBEL IN @S_SO
  AND   C~VBELN IN @S_SO
  AND   A~VKORG IN @S_VKORG
  AND   A~VTWEG IN @S_VTWEG
  AND   A~SPART IN @S_SPART
  AND   B~VKGRP IN @S_VKGRP
  AND   B~VKBUR IN @S_VKBUR
  AND   F~ZCHECK <> @SPACE
  AND   A~ERNAM IN @S_ERNAM
  AND   A~FKSTO = ''.

  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

  IF P_INC_RC = ABAP_FALSE.
    DELETE LT_DATA WHERE ZCHECK = GC_ZCHECK-RECEIVED.
  ENDIF.
  IF P_INC_RJ = ABAP_FALSE.
    DELETE LT_DATA WHERE ZCHECK = GC_ZCHECK-REJECTED.
  ENDIF.

  SORT LT_DATA BY VBELN SO.
  DELETE ADJACENT DUPLICATES FROM LT_DATA COMPARING VBELN.

  DATA(LT_KUNRG) = LT_DATA.
  SORT LT_KUNRG BY KUNRG.
  DELETE ADJACENT DUPLICATES FROM LT_KUNRG COMPARING KUNRG.


  LRT_KUNRG = VALUE #( FOR LS_KUNRG IN LT_KUNRG
                       ( SIGN = 'I'
                         OPTION = 'EQ'
                         LOW = LS_KUNRG-KUNRG )
                      ).
* -- get advance amount
  IF LRT_KUNRG[] IS NOT INITIAL.
    SELECT FROM BSID_VIEW
    FIELDS KUNNR,
    SUM( CASE WHEN SHKZG = 'S'
              THEN DMBTR
              ELSE DMBTR * CAST( @LF_MINUS_ONE AS DEC( 1 ) )
          END  ) AS AMOUNT
    WHERE KUNNR IN @LRT_KUNRG
    AND   UMSKZ = 'S'
    GROUP BY KUNNR
    INTO TABLE @DATA(LT_SUM_ADV).
    SORT LT_SUM_ADV BY KUNNR.

* -- get ZSDSFIC024
    SELECT KUNNR,
          ZBLLCYL,
          ZBLLCOL
    FROM ZSDSFIC024
    INTO TABLE @DATA(LT_ZSDSFIC024)
    WHERE KUNNR IN @LRT_KUNRG.
    SORT LT_ZSDSFIC024 BY KUNNR.
  ENDIF.

  IF LT_DATA[] IS NOT INITIAL.
    SELECT B~VBELN,
           A~BUKRS,
           A~BELNR,
           A~GJAHR,
           A~XBLNR
*    FROM BKPF AS A INNER JOIN BSID_VIEW AS B "CH01 420000161-
     FROM BKPF AS A INNER JOIN BSEG AS B      "CH01 420000161+
    ON  A~BUKRS = B~BUKRS
    AND A~GJAHR = B~GJAHR
    AND A~BELNR = B~BELNR
    FOR ALL ENTRIES IN @LT_DATA
    WHERE B~VBELN = @LT_DATA-VBELN
    INTO TABLE @DATA(LT_FIDOC).

    SORT LT_FIDOC BY VBELN.

* BOI CH01 420000147
* -- get wbs from vbrp
    SELECT A~VBELN,
           POSNR,
           PS_PSP_PNR,
           B~PSPHI
    FROM VBRP AS A INNER JOIN @LT_DATA AS DATA
    ON A~VBELN = DATA~VBELN
                   INNER JOIN PRPS AS B
    ON A~PS_PSP_PNR = B~PSPNR
    INTO TABLE @DATA(LT_VBRP_WBS).

    SORT LT_VBRP_WBS BY VBELN .
* EOI CH01 420000147

    IF LT_VBRP_WBS IS NOT INITIAL.  "CH01 420000147+
      SELECT  PSPNR,
              POST1
      INTO TABLE @DATA(LT_PROJ)
      FROM PROJ
*      FOR ALL ENTRIES IN @LT_DATA    "CH01 420000147-
      FOR ALL ENTRIES IN @LT_VBRP_WBS "CH01 420000147+
      WHERE PSPNR = @LT_VBRP_WBS-PSPHI.
      SORT LT_PROJ BY PSPNR.
    ENDIF.                            "CH01 420000147+

* BOI CH03 420000161
    SELECT VBPA~VBELN,
           PARVW,
           NATION,
           KUNNR,
           ADRNR,
           NAME1
    FROM VBPA INNER JOIN ADRC
    ON VBPA~ADRNR = ADRC~ADDRNUMBER
              INNER JOIN @LT_DATA AS ITAB
    ON VBPA~VBELN = ITAB~VBELN
    WHERE PARVW IN ( @GC_SOLD_TO , @GC_PAYER )
    AND ( ITAB~KUNAG_XCPDK = 'X'  OR ITAB~KUNRG_XCPDK = 'X' )
    INTO TABLE @DATA(LT_VBPA).

    SORT LT_VBPA BY VBELN PARVW NATION.
* EOI CH03 420000161

  ENDIF.
  LOOP AT LT_DATA INTO DATA(LS_DATA) ##INTO_OK.
    CLEAR: LS_RC_IV.
    LS_RC_IV = CORRESPONDING #( BASE ( LS_RC_IV ) LS_DATA ).
    LS_RC_IV-TOTAL = LS_RC_IV-NETWR + LS_RC_IV-MWSBK.
    READ TABLE LT_FIDOC INTO DATA(LS_FIDOC) WITH KEY VBELN = LS_DATA-VBELN
                                                     BINARY SEARCH.
    IF SY-SUBRC = 0.
      LS_RC_IV-BELNR = LS_FIDOC-BELNR.
      LS_RC_IV-XBLNR = LS_FIDOC-XBLNR.
    ELSE.
      IF  GR_FKART_NO_FIDOC IS INITIAL
      OR  LS_DATA-FKART NOT IN GR_FKART_NO_FIDOC .
        CONTINUE.
      ENDIF.
    ENDIF.
*   BOI CH03 420000161
    IF LS_DATA-KUNAG_XCPDK = 'X'."ONE TIME
      READ TABLE LT_VBPA INTO DATA(LS_VBPA) WITH KEY VBELN = LS_DATA-VBELN
                                                     PARVW = GC_SOLD_TO.
      LS_RC_IV-KUNAG_NM = LS_VBPA-NAME1.
    ENDIF.
    IF LS_DATA-KUNRG_XCPDK = 'X'."ONE TIME
      READ TABLE LT_VBPA INTO LS_VBPA WITH KEY VBELN = LS_DATA-VBELN
                                               PARVW = GC_PAYER.
      LS_RC_IV-KUNRG_NM = LS_VBPA-NAME1.
    ENDIF.
*   EOI CH03 420000161
*   BOI CH01 420000147
    IF LS_RC_IV-PSPHI IS INITIAL.
      READ TABLE LT_VBRP_WBS INTO DATA(LS_VBRP_WBS) WITH KEY VBELN = LS_DATA-VBELN
                                                             BINARY SEARCH.
      IF SY-SUBRC = 0.
        LS_RC_IV-PSPHI = LS_VBRP_WBS-PSPHI.
      ENDIF.
    ENDIF.
*   EOI CH01 420000147
    IF LS_RC_IV-PSPHI IS NOT INITIAL.
      READ TABLE LT_PROJ INTO DATA(LS_PROJ) WITH KEY PSPNR = LS_RC_IV-PSPHI
                                                     BINARY SEARCH.
      IF SY-SUBRC = 0.
        LS_RC_IV-POST1 = LS_PROJ-POST1.
      ENDIF.
    ENDIF.

    IF LS_RC_IV-ZSTATUS IS NOT INITIAL.
      READ TABLE GT_ZSTATUS INTO DATA(LS_ZSTATUS) WITH KEY VALUE = LS_RC_IV-ZSTATUS.
      IF SY-SUBRC = 0.
        LS_RC_IV-ZSTATUS_TX = LS_ZSTATUS-TEXT.
      ENDIF.
    ENDIF.
    CASE LS_RC_IV-ZCHECK.
      WHEN GC_ZCHECK-SENT.
        LS_RC_IV-RECEIVED = ''.
        LS_RC_IV-REJECTED = ''.
      WHEN GC_ZCHECK-RECEIVED.
        LS_RC_IV-RECEIVED = GC_TRUE.
      WHEN GC_ZCHECK-REJECTED.
        LS_RC_IV-REJECTED = GC_TRUE.
    ENDCASE.
    IF LS_DATA-ZSTAT_ACT IS NOT INITIAL.
      LF_FIELDNAME = 'STAT_ACT' && LS_DATA-ZSTAT_ACT.
      ASSIGN LS_RC_IV-(LF_FIELDNAME) TO FIELD-SYMBOL(<L_STAT_ACT>).
      IF <L_STAT_ACT> IS ASSIGNED.
        <L_STAT_ACT> = ABAP_TRUE.
      ENDIF.
    ENDIF.

    READ TABLE LT_SUM_ADV INTO DATA(LS_SUM_ADV) WITH KEY KUNNR = LS_RC_IV-KUNRG
                                                         BINARY SEARCH.
    IF SY-SUBRC = 0.
      LS_RC_IV-ADV_AMT = LS_SUM_ADV-AMOUNT.
    ENDIF.
    READ TABLE LT_ZSDSFIC024 INTO DATA(LS_ZSDSFIC024) WITH KEY KUNNR = LS_RC_IV-KUNRG
                                                      BINARY SEARCH.
    IF SY-SUBRC = 0.
      LS_RC_IV-ZBLLCYL = LS_ZSDSFIC024-ZBLLCYL.
      LS_RC_IV-ZBLLCOL = LS_ZSDSFIC024-ZBLLCOL.
    ENDIF.
    PERFORM F_GET_NET_DUE_DATE USING LS_RC_IV-FKDAT
                                     LS_RC_IV-ZTERM
                             CHANGING LS_RC_IV-NETDU.

    APPEND LS_RC_IV TO CT_RC_IV.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_net_due_date
*&---------------------------------------------------------------------*
FORM F_GET_NET_DUE_DATE  USING  UF_FKDAT TYPE VBRK-FKDAT
                                UF_ZTERM TYPE VBRK-ZTERM
                       CHANGING CF_NETDU TYPE FAEDE-NETDT.
  DATA: LF_BUKRS TYPE T001-BUKRS,
        LS_FAEDE TYPE FAEDE.
  READ TABLE S_BUKRS INTO DATA(LS_BUKRS) INDEX 1.
  LF_BUKRS = LS_BUKRS-LOW.
  CALL FUNCTION 'FI_TERMS_OF_PAYMENT_PROPOSE'
    EXPORTING
      I_BLDAT         = UF_FKDAT
      I_BUDAT         = UF_FKDAT
      I_ZTERM         = UF_ZTERM
      I_BUKRS         = LF_BUKRS
    IMPORTING
      E_ZBD1T         = LS_FAEDE-ZBD1T
      E_ZBD2T         = LS_FAEDE-ZBD2T
      E_ZBD3T         = LS_FAEDE-ZBD3T
      E_ZFBDT         = LS_FAEDE-ZFBDT
    EXCEPTIONS
      TERMS_NOT_FOUND = 1
      OTHERS          = 2.
  IF SY-SUBRC = 0.
    LS_FAEDE-SHKZG = 'S'.
    LS_FAEDE-KOART = 'D'.
    LS_FAEDE-BLDAT = UF_FKDAT.
    CALL FUNCTION 'DETERMINE_DUE_DATE'
      EXPORTING
        I_FAEDE                    = LS_FAEDE
      IMPORTING
        E_FAEDE                    = LS_FAEDE
      EXCEPTIONS
        ACCOUNT_TYPE_NOT_SUPPORTED = 1
        OTHERS                     = 2.
    IF SY-SUBRC <> 0.
      CF_NETDU = LS_FAEDE-ZFBDT.
    ELSE.
      CF_NETDU = LS_FAEDE-NETDT.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ALV_MODIFY_FC_RC_IV
*&---------------------------------------------------------------------*
FORM F_ALV_MODIFY_FC_RC_IV  CHANGING CT_FIELDCAT TYPE LVC_T_FCAT.
  DATA: LF_STAT_ACT TYPE ZSDSFIC028-STAT_ACT.
  LOOP AT CT_FIELDCAT ASSIGNING FIELD-SYMBOL(<FCAT>) .
    CASE <FCAT>-FIELDNAME.
      WHEN 'ZCHECK'.
        <FCAT>-TECH = GC_TRUE.
      WHEN 'RECEIVED'.
        %FCAT 'Receive'(c04).
        <FCAT>-CHECKBOX = GC_TRUE.
        <FCAT>-COL_POS = 1.
      WHEN 'REJECTED'.
        %FCAT 'Reject'(c02).
        <FCAT>-CHECKBOX = GC_TRUE.
        <FCAT>-EDIT    = GC_TRUE.
        <FCAT>-COL_POS = 2.
      WHEN 'VBELN'.
        <FCAT>-HOTSPOT = GC_TRUE.
        <FCAT>-COL_POS = <FCAT>-COL_POS + 2.
      WHEN 'BELNR'.
        %FCAT 'Receipt No.'(c09).
        <FCAT>-COL_POS = <FCAT>-COL_POS + 2.
      WHEN 'XBLNR'.
        %FCAT 'Tax Invoice'(c08).
        <FCAT>-COL_POS = <FCAT>-COL_POS + 2.
      WHEN 'POST1'.
        %FCAT 'Project Description'(c10).
        <FCAT>-COL_POS = <FCAT>-COL_POS + 2.
      WHEN 'ZSTATUS'.
        <FCAT>-EDIT       = GC_TRUE.
        <FCAT>-REF_TABLE  = 'ZSDSFIT027'.
        <FCAT>-REF_FIELD  = 'ZSTATUS'.
        <FCAT>-COL_POS = <FCAT>-COL_POS + 2.
      WHEN 'ZREMARK'.
        <FCAT>-EDIT       = GC_TRUE.
        <FCAT>-COL_POS = <FCAT>-COL_POS + 2.
      WHEN 'ZADMIN'.
        <FCAT>-EDIT       = GC_TRUE.
        <FCAT>-COL_POS    = <FCAT>-COL_POS + 2.
      WHEN 'ZSTATUS_TX'.
        %FCAT 'Status Description'(c03).
        <FCAT>-COL_POS = <FCAT>-COL_POS + 2.
      WHEN 'NETWR'
      OR   'MWSBK'.
        <FCAT>-CFIELDNAME = 'WAERK'.
        <FCAT>-COL_POS    = <FCAT>-COL_POS + 2.
      WHEN 'TOTAL'.
        %FCAT 'Total Amount'(c05).
        <FCAT>-CFIELDNAME = 'WAERK'.
        <FCAT>-COL_POS    = <FCAT>-COL_POS + 2.
      WHEN 'ADV_AMT'.
        %FCAT 'Advance Amount'(c06).
        <FCAT>-CFIELDNAME = 'WAERK'.
        <FCAT>-COL_POS    = <FCAT>-COL_POS + 2.
*BOI CH01 420000161
      WHEN 'KUNAG_NM'.
        %FCAT 'Sold-to Name'(c11).
        <FCAT>-COL_POS = <FCAT>-COL_POS + 2.
      WHEN 'KUNRG_NM'.
        %FCAT 'Payer Name'(c12).
        <FCAT>-COL_POS = <FCAT>-COL_POS + 2.
*EOI CH01 420000161
      WHEN 'STAT_ACT01'
      OR  'STAT_ACT02'
      OR  'STAT_ACT03'
      OR  'STAT_ACT04'
      OR  'STAT_ACT05'.
        LF_STAT_ACT = <FCAT>-FIELDNAME+8(2).
        READ TABLE GT_STAT_ACT INTO DATA(LS_STAT_ACT) WITH KEY STAT_ACT = LF_STAT_ACT.
        IF SY-SUBRC = 0.
          %FCAT LS_STAT_ACT-FLD_DESC.
        ENDIF.
        <FCAT>-EDIT       = GC_TRUE.
        <FCAT>-CHECKBOX   = ABAP_TRUE.
        <FCAT>-COL_POS    = <FCAT>-COL_POS + 2.
      WHEN OTHERS.
        <FCAT>-COL_POS = <FCAT>-COL_POS + 2.
    ENDCASE.
  ENDLOOP.
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_SORT_RESULT
*----------------------------------------------------------------------*
*  Maintain SORT data for Result ALV
*----------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT_RC_IV  CHANGING CT_SORT TYPE LVC_T_SORT.

  CONSTANTS:
    LC_SORT1 TYPE  LVC_S_SORT-FIELDNAME VALUE 'VBELN'.

  DATA:
    LS_SORT  TYPE  LVC_S_SORT.


* Initialize Output
  CLEAR: CT_SORT.

  CLEAR LS_SORT.
  LS_SORT-SPOS      = 1.
  LS_SORT-FIELDNAME = LC_SORT1.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO CT_SORT.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_USER_COMMAND_SAVE_RC_IV
*&---------------------------------------------------------------------*
FORM F_USER_COMMAND_SAVE_RC_IV .


  DATA: LT_UPDATE     TYPE STANDARD TABLE OF ZSDSFIT027,
        LT_MAIL       TYPE TT_RC_IV,
        LS_RECORD     TYPE ZSDSFIT027,
        LS_RECORD_OLD TYPE ZSDSFIT027.
  DATA: LF_DATUM     TYPE SY-DATUM,
        LF_UZEIT     TYPE SY-UZEIT,
        LF_STAT_ACT  TYPE ZSDSFIC028-STAT_ACT,
        LF_FIELDNAME TYPE TEXT20.
  DATA: LT_BANK_VENDOR_CREATE TYPE ZCL_SDSFI_COLL_LOG=>TT_BILLING,
        LT_BANK_VENDOR_DELETE TYPE ZCL_SDSFI_COLL_LOG=>TT_BILLING.
  LF_DATUM = SY-DATUM.
  LF_UZEIT = SY-UZEIT.
  SORT GT_RC_IV_OLD BY VBELN .
  LOOP AT GT_RC_IV INTO DATA(LS_RC_IV) ##INTO_OK.
    READ TABLE GT_RC_IV_OLD INTO DATA(LS_RC_IV_OLD) WITH KEY VBELN = LS_RC_IV-VBELN
                                                             BINARY SEARCH.

    IF SY-SUBRC = 0.
      IF LS_RC_IV <> LS_RC_IV_OLD. "SOME CHANGE
        CLEAR: LS_RECORD,
               LS_RECORD_OLD.
        LS_RECORD = CORRESPONDING #( BASE ( LS_RECORD ) LS_RC_IV ).
        LS_RECORD_OLD = CORRESPONDING #( BASE ( LS_RECORD_OLD ) LS_RC_IV_OLD ).
        IF LS_RC_IV-RECEIVED = GC_TRUE.
          LS_RECORD-ZCHECK = GC_ZCHECK-RECEIVED.
        ELSEIF LS_RC_IV-RECEIVED = ''.
          IF LS_RC_IV-REJECTED = GC_TRUE.
            LS_RECORD-ZCHECK = GC_ZCHECK-REJECTED.
          ELSE.
            LS_RECORD-ZCHECK = GC_ZCHECK-SENT.
          ENDIF.
        ENDIF.

        "--new--
        CLEAR LF_STAT_ACT.
        DO 5 TIMES.
          LF_STAT_ACT =  LF_STAT_ACT + 1.
          LF_FIELDNAME = 'STAT_ACT' && LF_STAT_ACT.
          ASSIGN LS_RC_IV-(LF_FIELDNAME) TO FIELD-SYMBOL(<L_STAT_ACT>).
          IF <L_STAT_ACT> IS ASSIGNED.
            IF <L_STAT_ACT> = ABAP_TRUE.
              LS_RECORD-ZSTAT_ACT = LF_STAT_ACT .
              EXIT.
            ENDIF.
          ENDIF.
          UNASSIGN <L_STAT_ACT>.
        ENDDO.

        "--old
        CLEAR LF_STAT_ACT.
        DO 5 TIMES.
          LF_STAT_ACT =  LF_STAT_ACT + 1.
          LF_FIELDNAME = 'STAT_ACT' && LF_STAT_ACT.
          ASSIGN LS_RC_IV_OLD-(LF_FIELDNAME) TO <L_STAT_ACT>.
          IF <L_STAT_ACT> IS ASSIGNED.
            IF <L_STAT_ACT> = ABAP_TRUE.
              LS_RECORD_OLD-ZSTAT_ACT = LF_STAT_ACT .
              EXIT.
            ENDIF.
          ENDIF.
          UNASSIGN <L_STAT_ACT>.
        ENDDO.

        IF LS_RECORD-ZSTAT_ACT IN GR_BANK_VENDOR
        AND LS_RECORD_OLD-ZSTAT_ACT <> LS_RECORD-ZSTAT_ACT.
          READ TABLE GT_STAT_ACT INTO DATA(LS_STAT_ACT_BV) WITH KEY STAT_ACT = LS_RECORD-ZSTAT_ACT.
          INSERT VALUE #( VBELN = LS_RC_IV-VBELN
                          PERNR = P_PERNR
                          ACTION_TYPE = LS_STAT_ACT_BV-BL_ACTTY
                          STATUS = LS_STAT_ACT_BV-BL_STAT )
          INTO TABLE  LT_BANK_VENDOR_CREATE.
        ELSEIF LS_RECORD-ZSTAT_ACT NOT IN  GR_BANK_VENDOR
        AND LS_RECORD_OLD-ZSTAT_ACT IN GR_BANK_VENDOR.
          READ TABLE GT_STAT_ACT INTO LS_STAT_ACT_BV WITH KEY STAT_ACT = LS_RECORD_OLD-ZSTAT_ACT.
          INSERT VALUE #( VBELN = LS_RC_IV-VBELN
                          PERNR = P_PERNR
                          ACTION_TYPE = LS_STAT_ACT_BV-BL_ACTTY
                          STATUS = LS_STAT_ACT_BV-BL_STAT )
          INTO TABLE  LT_BANK_VENDOR_DELETE.
        ENDIF.

        LS_RECORD-ZRCVD_N = SY-UNAME.
        LS_RECORD-ZRCVD_D = LF_DATUM.
        LS_RECORD-ZRCVD_T = LF_UZEIT.
        LS_RECORD-ZRCVD_PERNR = P_PERNR. "CH05+
        APPEND LS_RECORD TO LT_UPDATE.

        IF LS_RC_IV-REJECTED = GC_TRUE.
          APPEND LS_RC_IV TO LT_MAIL.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR LS_STAT_ACT_BV.
  ENDLOOP.
  IF LT_UPDATE IS NOT INITIAL.
    UPDATE  ZSDSFIT027 FROM TABLE LT_UPDATE.
  ENDIF.
  IF LT_BANK_VENDOR_DELETE IS NOT INITIAL.
    CALL METHOD ZCL_SDSFI_COLL_LOG=>DELETE_BANK_VENDOR_LOG
      EXPORTING
        IT_VBELN   = LT_BANK_VENDOR_DELETE
        IV_TESTRUN = ''
      IMPORTING
        EV_RC      = DATA(LV_RC) ##NEEDED
        ET_RETURN  = DATA(LT_RETURN) ##NEEDED.
  ENDIF.
  IF LT_BANK_VENDOR_CREATE IS NOT INITIAL.
    CALL METHOD ZCL_SDSFI_COLL_LOG=>CREATE_BANK_VENDOR_LOG
      EXPORTING
        IT_VBELN  = LT_BANK_VENDOR_CREATE
      IMPORTING
        EV_RC     = LV_RC
        ET_RETURN = LT_RETURN.
  ENDIF.
  IF LT_MAIL[] IS NOT INITIAL.
    PERFORM F_SEND_MAIL_RC_IV USING LT_MAIL.
  ENDIF.
  COMMIT WORK AND WAIT.
  MESSAGE S000(ZSDSCA01) WITH 'Data has been changed'(m01).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SEL_SCRN_SET_PROPERTIES
*&---------------------------------------------------------------------*
FORM F_SEL_SCRN_SET_PROPERTIES .
  LOOP AT SCREEN.
    CASE SCREEN-GROUP1.
      WHEN 'IV'.
        IF RB_SN_IV = GC_TRUE
        OR RB_RC_IV = GC_TRUE
        OR RB_DP_IV = GC_TRUE.
          SCREEN-INPUT = 1.
          SCREEN-OUTPUT = 1.
          SCREEN-ACTIVE = 1.
        ELSE.
          SCREEN-INPUT = 0.
          SCREEN-OUTPUT = 0.
          SCREEN-ACTIVE = 0.
        ENDIF.
        MODIFY SCREEN.
      WHEN 'PJ'.
        IF RB_SN_PJ = GC_TRUE
        OR RB_RC_PJ = GC_TRUE
        OR RB_DP_PJ = GC_TRUE.
          SCREEN-ACTIVE = 1.
        ELSE.
          SCREEN-ACTIVE = 0.
        ENDIF.
        MODIFY SCREEN.
      WHEN 'RC'.
        IF RB_RC_IV = ABAP_TRUE
        OR RB_RC_PJ = ABAP_TRUE.
          SCREEN-ACTIVE = 1.
        ELSE.
          SCREEN-ACTIVE = 0.
        ENDIF.
        MODIFY SCREEN.
    ENDCASE.
    IF SCREEN-NAME = 'S_FKDAT-LOW'.
      IF RB_SN_IV = GC_TRUE
      OR RB_RC_IV = GC_TRUE
      OR RB_DP_IV = GC_TRUE.
        SCREEN-REQUIRED = '2'. "RECOMMEND
      ELSE.
        SCREEN-REQUIRED = '0'.
      ENDIF.
      MODIFY SCREEN.
    ELSEIF SCREEN-NAME = 'S_LFDAT-LOW'.
      IF RB_SN_PJ = GC_TRUE
      OR RB_RC_PJ = GC_TRUE
      OR RB_DP_PJ = GC_TRUE.
        SCREEN-REQUIRED = '2'. "RECOMMEND
      ELSE.
        SCREEN-REQUIRED = '0'.
      ENDIF.
      MODIFY SCREEN.
    ELSEIF SCREEN-NAME = 'P_PERNR'.
      IF RB_RC_IV = GC_TRUE.
        SCREEN-REQUIRED = '2'. "RECOMMEND
      ELSE.
        SCREEN-REQUIRED = '0'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SEL_SCRN_validate
*&---------------------------------------------------------------------*
FORM F_SEL_SCRN_VALIDATE .
  IF RB_SN_IV = GC_TRUE
  OR RB_RC_IV = GC_TRUE
  OR RB_DP_IV = GC_TRUE.
    IF S_FKDAT[] IS INITIAL.
      MESSAGE E000(ZSDSCA01) WITH 'Billing Date is required'(m04).
    ENDIF.
    IF RB_RC_IV = ABAP_TRUE.
      IF P_PERNR IS INITIAL.
        MESSAGE E000(ZSDSCA01) WITH 'Personal ID(for Bank Vendor) is required'(m10).
      ELSE.
        SELECT PERNR
        INTO @DATA(LF_PERNR)
        UP TO 1 ROWS
        FROM PA0002
        WHERE PERNR = @P_PERNR ##NEEDED.
        ENDSELECT.
        IF SY-SUBRC <> 0.
          MESSAGE E000(ZSDSCA01) WITH 'Personal ID(for Bank Vendor) is invalid'(m11).
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    IF S_LFDAT[] IS INITIAL.
      MESSAGE E000(ZSDSCA01) WITH 'Delivery Date is required'(m05).
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_send_Mail_rc_iv
*&---------------------------------------------------------------------*
FORM F_SEND_MAIL_RC_IV  USING UT_MAIL TYPE TT_RC_IV.
  DATA: LT_USRID TYPE TT_RC_IV,
        LT_DATA  TYPE TT_RC_IV,
        LF_TABIX TYPE SY-TABIX.
  DATA: LS_DOC_DATA     TYPE SODOCCHGI1,
        LT_RECEIVERS    TYPE TABLE OF SOMLRECI1,
        LS_RECEIVER     LIKE LINE OF LT_RECEIVERS,
        LT_MAILBODY     TYPE TABLE OF SOLISTI1,
        LS_MAILBODY     LIKE LINE OF LT_MAILBODY,
        LT_PACKING_LIST TYPE TABLE OF SOPCKLSTI1,
        LS_PACKING_LIST LIKE LINE OF LT_PACKING_LIST.

  LT_USRID[] = UT_MAIL[].
  SORT LT_USRID BY ZSEND_N VBELN.
  LT_DATA[]  = LT_USRID.
  DELETE ADJACENT DUPLICATES FROM LT_USRID COMPARING ZSEND_N.

  LS_DOC_DATA-OBJ_DESCR = 'Rejected Billing Document'(n01).
  LOOP AT LT_USRID INTO DATA(LS_USRID) ##INTO_OK.
    CLEAR: LT_RECEIVERS,
           LT_MAILBODY,
           LT_PACKING_LIST.

    LS_RECEIVER-REC_TYPE  = 'B'.
    LS_RECEIVER-RECEIVER  = LS_USRID-ZSEND_N.
    LS_RECEIVER-EXPRESS   = 'X'.
    APPEND LS_RECEIVER TO LT_RECEIVERS.

    LS_MAILBODY-LINE = 'Please find rejected billing document as bellow.'(n02).
    APPEND LS_MAILBODY TO LT_MAILBODY.

    CLEAR LS_MAILBODY.
    APPEND LS_MAILBODY TO LT_MAILBODY.

    LS_MAILBODY-LINE    = 'Billno'(n03)     .
    LS_MAILBODY-LINE+20 = 'Reason for Rejection'(n04) .
    APPEND LS_MAILBODY TO LT_MAILBODY.

    READ TABLE LT_DATA TRANSPORTING NO FIELDS WITH KEY ZSEND_N = LS_USRID-ZSEND_N.
    IF SY-SUBRC = 0.
      LF_TABIX = SY-TABIX.
      LOOP AT LT_DATA INTO DATA(LS_DATA) FROM LF_TABIX ##INTO_OK.
        IF LS_DATA-ZSEND_N <> LS_USRID-ZSEND_N.
          EXIT.
        ENDIF.

        CLEAR LS_MAILBODY.
        LS_MAILBODY-LINE    = LS_DATA-VBELN.
        LS_MAILBODY-LINE+15 = LS_DATA-ZADMIN.
        APPEND LS_MAILBODY TO LT_MAILBODY.

      ENDLOOP.

*     Describe the body of the message
      CLEAR LS_PACKING_LIST.
      LS_PACKING_LIST-TRANSF_BIN = SPACE.
      LS_PACKING_LIST-HEAD_START = 1.
      LS_PACKING_LIST-HEAD_NUM = 0.
      LS_PACKING_LIST-BODY_START = 1.
      LS_PACKING_LIST-DOC_TYPE = 'RAW'.
      LS_PACKING_LIST-BODY_NUM = LINES( LT_MAILBODY ).
      APPEND LS_PACKING_LIST TO LT_PACKING_LIST.

      CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
        EXPORTING
          DOCUMENT_DATA              = LS_DOC_DATA
        TABLES
          PACKING_LIST               = LT_PACKING_LIST
          CONTENTS_TXT               = LT_MAILBODY
          RECEIVERS                  = LT_RECEIVERS
        EXCEPTIONS
          TOO_MANY_RECEIVERS         = 1
          DOCUMENT_NOT_SENT          = 2
          DOCUMENT_TYPE_NOT_EXIST    = 3
          OPERATION_NO_AUTHORIZATION = 4
          PARAMETER_ERROR            = 5
          X_ERROR                    = 6
          ENQUEUE_ERROR              = 7
          OTHERS                     = 8.

      IF SY-SUBRC <> 0.
        MESSAGE E000(ZSDSCA01) WITH 'Error sending mail'(m06).
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_DP_IV
*&---------------------------------------------------------------------*
FORM F_GET_DP_IV  CHANGING CT_DP_IV TYPE TT_DP_IV.
  DATA: LS_DP_IV    TYPE TS_DP_IV.
  DATA: LRT_KUNRG TYPE RANGE OF VBRK-KUNRG.
  DATA: LF_MINUS_ONE TYPE P DECIMALS 0 VALUE -1 .

  SELECT A~VBELN,
         C~VBELN AS SO,
         A~KUNAG,
         D~XCPDK AS KUNAG_XCPDK, "CH03 420000161+
         D~NAME1 AS KUNAG_NM,
         A~KUNRG,
         A~FKART,
         E~XCPDK AS KUNRG_XCPDK, "CH03 420000161+
         E~NAME1 AS KUNRG_NM,
         A~FKDAT,
         A~ZTERM,
         B~VKBUR,
         A~NETWR,
         A~MWSBK,
         A~WAERK,
         F~ZCHECK,
         F~ZSTATUS,
         F~ZSTAT_ACT,
         F~ZBLLST,
         F~ZCLLST,
         F~ZREMARK,
         F~ZADMIN,
         F~ZSEND_N,
         F~ZSEND_D,
         F~ZSEND_T,
         F~ZRCVD_N,
         F~ZRCVD_D,
         F~ZRCVD_T,
         F~ZRCVD_PERNR, "CH05+
         G~PSPHI,
         H~FLD_DESC
  INTO TABLE @DATA(LT_DATA)
  FROM VBRK AS A INNER JOIN VBRP AS B
  ON A~VBELN = B~VBELN
*                 INNER JOIN VBAK AS C "CH01 420000161-
                 LEFT OUTER JOIN VBAK AS C "CH01 420000161+
  ON B~AUBEL = C~VBELN
                 INNER JOIN KNA1 AS D
  ON A~KUNAG = D~KUNNR
                 INNER JOIN KNA1 AS E
  ON A~KUNRG = E~KUNNR
*                 INNER JOIN ZSDSFIT027 AS F "CH04-
                LEFT OUTER JOIN ZSDSFIT027 AS F "CH04+
  ON A~VBELN = F~VBELN
                 LEFT OUTER JOIN PRPS AS G
  ON B~PS_PSP_PNR = G~PSPNR
                 LEFT OUTER JOIN ZSDSFIC028 AS H
  ON F~ZSTAT_ACT = H~STAT_ACT
*  WHERE A~KUNAG IN @S_KUNNR
  WHERE A~KUNRG IN @S_KUNNR "CH01 420000161+
  AND   A~BUKRS IN @S_BUKRS
  AND   A~FKDAT IN @S_FKDAT
  AND   A~VBELN IN @S_BILNO
  AND   B~AUBEL IN @S_SO
  AND   C~VBELN IN @S_SO
  AND   A~VKORG IN @S_VKORG
  AND   A~VTWEG IN @S_VTWEG
  AND   A~SPART IN @S_SPART
  AND   B~VKGRP IN @S_VKGRP
  AND   B~VKBUR IN @S_VKBUR
  AND   A~ERNAM IN @S_ERNAM
*  AND   F~ZCHECK <> @SPACE.  "CH04-
  AND   A~FKSTO = ''                    "CH04+
  AND   A~GBSTK = @GC_GBSTK_COMPLETE .   "CH04+

  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

  SORT LT_DATA BY VBELN SO.
  DELETE ADJACENT DUPLICATES FROM LT_DATA COMPARING VBELN.

  DATA(LT_KUNRG) = LT_DATA.
  SORT LT_KUNRG BY KUNRG.
  DELETE ADJACENT DUPLICATES FROM LT_KUNRG COMPARING KUNRG.


  LRT_KUNRG = VALUE #( FOR LS_KUNRG IN LT_KUNRG
                       ( SIGN = 'I'
                         OPTION = 'EQ'
                         LOW = LS_KUNRG-KUNRG )
                      ).
* -- get advance amount
  IF LRT_KUNRG[] IS NOT INITIAL.
    SELECT FROM BSID_VIEW
    FIELDS KUNNR,
    SUM( CASE WHEN SHKZG = 'S'
              THEN DMBTR
              ELSE DMBTR * CAST( @LF_MINUS_ONE AS DEC( 1 ) )
          END  ) AS AMOUNT
    WHERE KUNNR IN @LRT_KUNRG
    AND   UMSKZ = 'S'
    GROUP BY KUNNR
    INTO TABLE @DATA(LT_SUM_ADV).
    SORT LT_SUM_ADV BY KUNNR.

* -- get ZSDSFIT027
    SELECT KUNNR,
          ZBLLCYL,
          ZBLLCOL
    FROM ZSDSFIC024
    INTO TABLE @DATA(LT_ZSDSFIC024)
    WHERE KUNNR IN @LRT_KUNRG.
    SORT LT_ZSDSFIC024 BY KUNNR.
  ENDIF.

  IF LT_DATA[] IS NOT INITIAL.
    SELECT B~VBELN,
           A~BUKRS,
           A~BELNR,
           A~GJAHR,
           A~XBLNR
*    FROM BKPF AS A INNER JOIN BSID_VIEW AS B "CH01 420000161-
     FROM BKPF AS A INNER JOIN BSEG AS B      "CH01 420000161+
    ON  A~BUKRS = B~BUKRS
    AND A~GJAHR = B~GJAHR
    AND A~BELNR = B~BELNR
    FOR ALL ENTRIES IN @LT_DATA
    WHERE B~VBELN = @LT_DATA-VBELN
    INTO TABLE @DATA(LT_FIDOC).

    SORT LT_FIDOC BY VBELN.

* BOI CH01 420000147
* -- get wbs from vbrp
    SELECT A~VBELN,
           POSNR,
           PS_PSP_PNR,
           B~PSPHI
    FROM VBRP AS A INNER JOIN @LT_DATA AS DATA
    ON A~VBELN = DATA~VBELN
                   INNER JOIN PRPS AS B
    ON A~PS_PSP_PNR = B~PSPNR
    INTO TABLE @DATA(LT_VBRP_WBS).

    SORT LT_VBRP_WBS BY VBELN .
* EOI CH01 420000147

    IF LT_VBRP_WBS IS NOT INITIAL.  "CH01 420000147+
      SELECT  PSPNR,
              POST1
      INTO TABLE @DATA(LT_PROJ)
      FROM PROJ
*      FOR ALL ENTRIES IN @LT_DATA    "CH01 420000147-
      FOR ALL ENTRIES IN @LT_VBRP_WBS "CH01 420000147+
      WHERE PSPNR = @LT_VBRP_WBS-PSPHI.
      SORT LT_PROJ BY PSPNR.
    ENDIF.                            "CH01 420000147+

* BOI CH03 420000161
    SELECT VBPA~VBELN,
           PARVW,
           NATION,
           KUNNR,
           ADRNR,
           NAME1
    FROM VBPA INNER JOIN ADRC
    ON VBPA~ADRNR = ADRC~ADDRNUMBER
              INNER JOIN @LT_DATA AS ITAB
    ON VBPA~VBELN = ITAB~VBELN
    WHERE PARVW IN ( @GC_SOLD_TO , @GC_PAYER )
    AND ( ITAB~KUNAG_XCPDK = 'X'  OR ITAB~KUNRG_XCPDK = 'X' )
    INTO TABLE @DATA(LT_VBPA).

    SORT LT_VBPA BY VBELN PARVW NATION.
* EOI CH03 420000161

  ENDIF.
  LOOP AT LT_DATA INTO DATA(LS_DATA) ##INTO_OK.
    CLEAR: LS_DP_IV.
    LS_DP_IV = CORRESPONDING #( BASE ( LS_DP_IV ) LS_DATA ).
    LS_DP_IV-TOTAL = LS_DP_IV-NETWR + LS_DP_IV-MWSBK.
    LS_DP_IV-ZSTAT_ACT_TX = LS_DATA-FLD_DESC.
    READ TABLE LT_FIDOC INTO DATA(LS_FIDOC) WITH KEY VBELN = LS_DATA-VBELN
                                                     BINARY SEARCH.
    IF SY-SUBRC = 0.
      LS_DP_IV-BELNR = LS_FIDOC-BELNR.
      LS_DP_IV-XBLNR = LS_FIDOC-XBLNR.
    ELSE.
      IF  GR_FKART_NO_FIDOC IS INITIAL
      OR  LS_DATA-FKART NOT IN GR_FKART_NO_FIDOC .
        CONTINUE.
      ENDIF.
    ENDIF.
*   BOI CH03 420000161
    IF LS_DATA-KUNAG_XCPDK = 'X'."ONE TIME
      READ TABLE LT_VBPA INTO DATA(LS_VBPA) WITH KEY VBELN = LS_DATA-VBELN
                                                     PARVW = GC_SOLD_TO.
      LS_DP_IV-KUNAG_NM = LS_VBPA-NAME1.
    ENDIF.
    IF LS_DATA-KUNRG_XCPDK = 'X'."ONE TIME
      READ TABLE LT_VBPA INTO LS_VBPA WITH KEY VBELN = LS_DATA-VBELN
                                               PARVW = GC_PAYER.
      LS_DP_IV-KUNRG_NM = LS_VBPA-NAME1.
    ENDIF.
*   EOI CH03 420000161
*   BOI CH01 420000147
    IF LS_DP_IV-PSPHI IS INITIAL.
      READ TABLE LT_VBRP_WBS INTO DATA(LS_VBRP_WBS) WITH KEY VBELN = LS_DATA-VBELN
                                                             BINARY SEARCH.
      IF SY-SUBRC = 0.
        LS_DP_IV-PSPHI = LS_VBRP_WBS-PSPHI.
      ENDIF.
    ENDIF.
*   EOI CH01 420000147
    IF LS_DP_IV-PSPHI IS NOT INITIAL.
      READ TABLE LT_PROJ INTO DATA(LS_PROJ) WITH KEY PSPNR = LS_DP_IV-PSPHI
                                                     BINARY SEARCH.
      IF SY-SUBRC = 0.
        LS_DP_IV-POST1 = LS_PROJ-POST1.
      ENDIF.
    ENDIF.
    IF LS_DP_IV-ZSTATUS IS NOT INITIAL.
      READ TABLE GT_ZSTATUS INTO DATA(LS_ZSTATUS) WITH KEY VALUE = LS_DP_IV-ZSTATUS.
      IF SY-SUBRC = 0.
        LS_DP_IV-ZSTATUS_TX = LS_ZSTATUS-TEXT.
      ENDIF.
    ENDIF.
    CASE LS_DP_IV-ZCHECK.
      WHEN GC_ZCHECK-SENT.
        LS_DP_IV-SENT = GC_TRUE.
      WHEN GC_ZCHECK-RECEIVED.
        LS_DP_IV-RECEIVED = GC_TRUE.
      WHEN GC_ZCHECK-REJECTED.
        LS_DP_IV-REJECTED = GC_TRUE.
    ENDCASE.

    READ TABLE LT_SUM_ADV INTO DATA(LS_SUM_ADV) WITH KEY KUNNR = LS_DP_IV-KUNRG
                                                         BINARY SEARCH.
    IF SY-SUBRC = 0.
      LS_DP_IV-ADV_AMT = LS_SUM_ADV-AMOUNT.
    ENDIF.
    READ TABLE LT_ZSDSFIC024 INTO DATA(LS_ZSDSFIC024) WITH KEY KUNNR = LS_DP_IV-KUNRG
                                                      BINARY SEARCH.
    IF SY-SUBRC = 0.
      LS_DP_IV-ZBLLCYL = LS_ZSDSFIC024-ZBLLCYL.
      LS_DP_IV-ZBLLCOL = LS_ZSDSFIC024-ZBLLCOL.
    ENDIF.
    PERFORM F_GET_NET_DUE_DATE USING LS_DP_IV-FKDAT
                                   LS_DP_IV-ZTERM
                             CHANGING LS_DP_IV-NETDU.

    APPEND LS_DP_IV TO CT_DP_IV.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ALV_MODIFY_FC_DP_IV
*&---------------------------------------------------------------------*
FORM F_ALV_MODIFY_FC_DP_IV  CHANGING CT_FIELDCAT TYPE LVC_T_FCAT.

  LOOP AT CT_FIELDCAT ASSIGNING FIELD-SYMBOL(<FCAT>) .
    CASE <FCAT>-FIELDNAME.
      WHEN 'ZCHECK'
      OR  'ZSTATUS'
      OR  'ZSTATUS_TX'
      OR  'ZSTAT_ACT'.
        <FCAT>-TECH = GC_TRUE.
      WHEN 'SENT'.
        %FCAT 'Send'(c01).
        <FCAT>-CHECKBOX = GC_TRUE.
        <FCAT>-COL_POS = 1.
      WHEN 'RECEIVED'.
        %FCAT 'Receive'(c04).
        <FCAT>-CHECKBOX = GC_TRUE.
        <FCAT>-COL_POS = 2.
      WHEN 'REJECTED'.
        %FCAT 'Reject'(c02).
        <FCAT>-CHECKBOX = GC_TRUE.
        <FCAT>-COL_POS = 3.
      WHEN 'ZSTAT_ACT_TX'.
        %FCAT 'Status + action'(c07).
        <FCAT>-COL_POS = <FCAT>-COL_POS + 3.
      WHEN 'VBELN'.
        <FCAT>-HOTSPOT = GC_TRUE.
        <FCAT>-COL_POS = <FCAT>-COL_POS + 3.
      WHEN 'XBLNR'.
        %FCAT 'Tax Invoice'(c08).
        <FCAT>-COL_POS = <FCAT>-COL_POS + 2.
      WHEN 'POST1'.
        %FCAT 'Project Description'(c10).
        <FCAT>-COL_POS = <FCAT>-COL_POS + 2.
      WHEN 'BELNR'.
        %FCAT 'Receipt No.'(c09).
        <FCAT>-COL_POS = <FCAT>-COL_POS + 2.
      WHEN 'ZREMARK'.
        <FCAT>-COL_POS = <FCAT>-COL_POS + 3.
      WHEN 'ZADMIN'.
        <FCAT>-COL_POS    = <FCAT>-COL_POS + 3.
      WHEN 'NETWR'
      OR   'MWSBK'
      OR   'TOTAL'.
        <FCAT>-CFIELDNAME = 'WAERK'.
        <FCAT>-COL_POS    = <FCAT>-COL_POS + 3.
*BOI CH01 420000161
      WHEN 'KUNAG_NM'.
        %FCAT 'Sold-to Name'(c11).
        <FCAT>-COL_POS = <FCAT>-COL_POS + 3.
      WHEN 'KUNRG_NM'.
        %FCAT 'Payer Name'(c12).
        <FCAT>-COL_POS = <FCAT>-COL_POS + 3.
*EOI CH01 420000161
      WHEN OTHERS.
        <FCAT>-COL_POS = <FCAT>-COL_POS + 3.
    ENDCASE.
  ENDLOOP.
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_SORT_RESULT
*----------------------------------------------------------------------*
*  Maintain SORT data for Result ALV
*----------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT_DP_IV  CHANGING CT_SORT TYPE LVC_T_SORT.

  CONSTANTS:
    LC_SORT1 TYPE  LVC_S_SORT-FIELDNAME VALUE 'VBELN'.

  DATA:
    LS_SORT  TYPE  LVC_S_SORT.


* Initialize Output
  CLEAR: CT_SORT.

  CLEAR LS_SORT.
  LS_SORT-SPOS      = 1.
  LS_SORT-FIELDNAME = LC_SORT1.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO CT_SORT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_PJ
*&---------------------------------------------------------------------*
FORM F_GET_PJ  CHANGING CT_PJ TYPE TT_PJ.
  CONSTANTS: LC_FKSTK_NOT_PROCESSED TYPE LIKP-FKSTK VALUE 'A'.
  DATA: LS_PJ        TYPE TS_PJ,
        LF_FIELDNAME TYPE TEXT20.

  "--- get possible do list , filter criteria ( some do item contains wbs )
  SELECT A~VBELN AS DO,                                "#EC CI_BUFFJOIN
         A~FKSTK,
         B~POSNR AS DO_ITEM,
         C2~VBELN AS SO,
         C2~POSNR AS SO_ITEM,
         C2~PS_PSP_PNR,
         F~PSPHI,
         G~VBELN AS VBELN,
         G~ZCHECK,
         C~KUNNR AS KUNAG,  "sold to
         C3~KUNNR AS KUNRG "payer
  INTO TABLE @DATA(LT_DO)
  FROM LIKP AS A INNER JOIN LIPS AS B
  ON   A~VBELN = B~VBELN
                 INNER JOIN VBAK AS C
  ON   B~VGBEL = C~VBELN
                 INNER JOIN VBAP AS C2
  ON   B~VGBEL = C2~VBELN
  AND  B~VGPOS = C2~POSNR
                 LEFT OUTER JOIN VBPA AS C3
  ON   C2~VBELN = C3~VBELN                  "CH01 420000161+
  AND  C3~PARVW = 'RG' "payer               "CH01 420000161+
                 INNER JOIN TVKO AS E
  ON   A~VKORG = E~VKORG
                 INNER JOIN PRPS AS F
  ON   C2~PS_PSP_PNR =  F~PSPNR
                 LEFT OUTER JOIN ZSDSFIT028 AS G
  ON   A~VBELN = G~VBELN
*  WHERE A~KUNAG IN @S_KUNNR   "CH01 420000161-
  WHERE A~LFDAT IN @S_LFDAT
  AND   A~VBELN IN @S_DO
  AND   C~VBELN IN @S_SO
  AND   A~VKORG IN @S_VKORG
  AND   C~VTWEG IN @S_VTWEG
  AND   C~SPART IN @S_SPART
  AND   B~VKGRP IN @S_VKGRP
  AND   B~VKBUR IN @S_VKBUR
  AND   E~BUKRS IN @S_BUKRS
  AND   F~PSPHI IN @S_PSPHI
  AND   A~ERNAM IN @S_ERNAM.
*  AND   ( A~FKSTK = @LC_FKSTK_NOT_PROCESSED  "CH01 420000161-
*       OR A~FKSTK = '' ).                    "CH01 420000161-

* BOI CH01 420000161
  IF S_KUNNR IS NOT INITIAL.
    DELETE LT_DO WHERE KUNRG NOT IN S_KUNNR.
  ENDIF.
* EOI CH01 420000161

  IF GF_EXEC_TY = GC_EXEC_TY-SN_PJ.
    DELETE LT_DO WHERE ZCHECK = GC_ZCHECK-RECEIVED
                 OR    ZCHECK = GC_ZCHECK-SENT.
    DELETE LT_DO WHERE FKSTK <> LC_FKSTK_NOT_PROCESSED "CH01 420000161+
                 AND   FKSTK <> '' .                   "CH01 420000161+
  ELSEIF GF_EXEC_TY = GC_EXEC_TY-RC_PJ.
    DELETE LT_DO WHERE ZCHECK = ''.
    IF P_INC_RC = ABAP_FALSE.
      DELETE LT_DO WHERE ZCHECK = GC_ZCHECK-RECEIVED.
    ENDIF.
    IF P_INC_RJ = ABAP_FALSE.
      DELETE LT_DO WHERE ZCHECK = GC_ZCHECK-REJECTED.
    ENDIF.
  ELSEIF GF_EXEC_TY = GC_EXEC_TY-DP_PJ.
*    DELETE LT_DO WHERE VBELN = ''.  "CH04-
  ENDIF.

  IF LT_DO[] IS INITIAL.
    RETURN.
  ENDIF.
  SORT LT_DO BY DO.

* --- get data by do (LT_DO)
  SELECT A~VBELN AS DO,
         B~POSNR AS DO_ITEM,
         C~VBELN AS SO,
         C2~POSNR AS SO_ITEM,
         B~PS_PSP_PNR,
         C2~NETWR,
         C2~MWSBP,
         C3~ZTERM,
         A~KUNAG,
         D~NAME1 AS KUNAG_NM,
         A~LFDAT,
         G~VBELN,
         G~ZCHECK,
         G~ZSTATUS,
         G~ZSTAT_ACT,
         G~ZADMIN,
         G~ZSEND_N,
         G~ZSEND_D,
         G~ZSEND_T,
         G~ZRCVD_N,
         G~ZRCVD_D,
         G~ZRCVD_T
  INTO TABLE @DATA(LT_DATA)
  FROM LIKP AS A INNER JOIN LIPS AS B
  ON   A~VBELN = B~VBELN
                 INNER JOIN VBAK AS C
  ON   B~VGBEL = C~VBELN
                 INNER JOIN VBAP AS C2
  ON   B~VGBEL = C2~VBELN
  AND  B~VGPOS = C2~POSNR
                 INNER JOIN VBKD AS C3
  ON   C~VBELN = C3~VBELN
  AND  C3~POSNR = '000000'
                 INNER JOIN KNA1 AS D
  ON   A~KUNAG = D~KUNNR
                 LEFT OUTER JOIN ZSDSFIT028 AS G
  ON   A~VBELN = G~VBELN
  FOR ALL ENTRIES IN @LT_DO
  WHERE A~VBELN = @LT_DO-DO.


  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

  IF LT_DO[] IS NOT INITIAL.
    SELECT  PSPNR,
            POST1
    INTO TABLE @DATA(LT_PROJ)
    FROM PROJ
    FOR ALL ENTRIES IN @LT_DO
    WHERE PSPNR = @LT_DO-PSPHI.
    SORT LT_PROJ BY PSPNR.

    SELECT KUNNR,
           NAME1
    INTO TABLE @DATA(LT_KNA1)
    FROM KNA1
    FOR ALL ENTRIES IN @LT_DO
    WHERE KUNNR = @LT_DO-KUNRG.
    SORT LT_KNA1 BY KUNNR.

    SELECT VBELV,
           VBELN
    INTO TABLE @DATA(LT_VBFA_INV)
    FROM VBFA
    FOR ALL ENTRIES IN @LT_DO
    WHERE VBELV = @LT_DO-VBELN
    AND VBTYP_V = 'J' "DO
    AND VBTYP_N = 'M' ."INV
    SORT LT_VBFA_INV BY VBELV.
  ENDIF.

  "---prepare main data
  LOOP AT LT_DATA INTO DATA(LS_DATA) ##INTO_OK.
    CLEAR: LS_PJ.
    LS_PJ = CORRESPONDING #( BASE ( LS_PJ ) LS_DATA ).
    LS_PJ-TOTAL = LS_PJ-NETWR + LS_PJ-MWSBP.
    READ TABLE LT_DO INTO DATA(LS_DO) WITH KEY DO = LS_DATA-DO
                                           BINARY SEARCH.
    IF SY-SUBRC = 0.
      LS_PJ-PSPHI = LS_DO-PSPHI.
      IF LS_PJ-PSPHI IS NOT INITIAL.
        READ TABLE LT_PROJ INTO DATA(LS_PROJ) WITH KEY PSPNR = LS_PJ-PSPHI
                                                       BINARY SEARCH.
        IF SY-SUBRC = 0.
          LS_PJ-POST1 = LS_PROJ-POST1.
        ENDIF.
      ENDIF.
*BOI CH01 420000161
      IF LS_DO-KUNRG IS NOT INITIAL.
        LS_PJ-KUNRG = LS_DO-KUNRG.
        READ TABLE LT_KNA1 INTO DATA(LS_KNA1) WITH KEY KUNNR = LS_DO-KUNRG
                                              BINARY SEARCH.
        IF SY-SUBRC = 0.
          LS_PJ-KUNRG_NM = LS_KNA1-NAME1.
        ENDIF.
      ENDIF.
      READ TABLE LT_VBFA_INV INTO DATA(LS_VBFA_INV) WITH KEY VBELV = LS_DO-VBELN
                                                    BINARY SEARCH.
      IF SY-SUBRC = 0.
        LS_PJ-BILNO = LS_VBFA_INV-VBELN.
      ENDIF.
*EOI CH01 420000161
    ENDIF.
    IF LS_PJ-ZSTATUS IS NOT INITIAL.
      READ TABLE GT_ZSTATUS INTO DATA(LS_ZSTATUS) WITH KEY VALUE = LS_PJ-ZSTATUS.
      IF SY-SUBRC = 0.
        LS_PJ-ZSTATUS_TX = LS_ZSTATUS-TEXT.
      ENDIF.
    ENDIF.
*   ---  ZCHECK
    CASE LS_PJ-ZCHECK.
      WHEN GC_ZCHECK-SENT.
        IF GF_EXEC_TY = GC_EXEC_TY-SN_PJ
        OR GF_EXEC_TY = GC_EXEC_TY-DP_PJ.
          LS_PJ-SENT = GC_TRUE.
        ELSEIF GF_EXEC_TY = GC_EXEC_TY-RC_PJ.
          CLEAR LS_PJ-SENT.
        ENDIF.
      WHEN GC_ZCHECK-RECEIVED.
        LS_PJ-RECEIVED = GC_TRUE.
      WHEN GC_ZCHECK-REJECTED.
        LS_PJ-REJECTED = GC_TRUE.
    ENDCASE.

*   ---- ZSTAT_ACT
    IF LS_DATA-ZSTAT_ACT IS NOT INITIAL.
      IF GF_EXEC_TY = GC_EXEC_TY-DP_PJ.
        READ TABLE GT_STAT_ACT INTO DATA(LS_STAT_ACT) WITH KEY STAT_ACT = LS_DATA-ZSTAT_ACT.
        IF SY-SUBRC = 0.
          LS_PJ-ZSTAT_ACT_TX = LS_STAT_ACT-FLD_DESC.
        ENDIF.
      ELSEIF GF_EXEC_TY = GC_EXEC_TY-RC_PJ.
        LF_FIELDNAME = 'STAT_ACT' && LS_DATA-ZSTAT_ACT.
        ASSIGN LS_PJ-(LF_FIELDNAME) TO FIELD-SYMBOL(<L_STAT_ACT>).
        IF <L_STAT_ACT> IS ASSIGNED.
          <L_STAT_ACT> = ABAP_TRUE.
        ENDIF.
      ENDIF.
    ENDIF.
    COLLECT LS_PJ INTO CT_PJ.
  ENDLOOP.
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_SORT_RESULT
*----------------------------------------------------------------------*
*  Maintain SORT data for Result ALV
*----------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT_PJ  CHANGING CT_SORT TYPE LVC_T_SORT.

  CONSTANTS:
    LC_SORT1 TYPE  LVC_S_SORT-FIELDNAME VALUE 'DO',
    LC_SORT2 TYPE  LVC_S_SORT-FIELDNAME VALUE 'SO'.

  DATA:
    LS_SORT  TYPE  LVC_S_SORT.


* Initialize Output
  CLEAR: CT_SORT.


  CLEAR LS_SORT.
  LS_SORT-SPOS      = 1.
  LS_SORT-FIELDNAME = LC_SORT1.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO CT_SORT.


  CLEAR LS_SORT.
  LS_SORT-SPOS      = 2.
  LS_SORT-FIELDNAME = LC_SORT2.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO CT_SORT.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ALV_MODIFY_FC_PJ
*&---------------------------------------------------------------------*
FORM F_ALV_MODIFY_FC_PJ  CHANGING CT_FIELDCAT TYPE LVC_T_FCAT.
  DATA: LF_STAT_ACT TYPE ZSDSFIC028-STAT_ACT.
  LOOP AT CT_FIELDCAT ASSIGNING FIELD-SYMBOL(<FCAT>) .
    CASE <FCAT>-FIELDNAME.
      WHEN 'ZCHECK'
      OR   'VBELN'.
        <FCAT>-TECH = GC_TRUE.
      WHEN 'SENT'.
        IF GF_EXEC_TY = GC_EXEC_TY-RC_PJ.
          <FCAT>-TECH = GC_TRUE.
        ELSE.
          %FCAT 'Send'(c01).
          <FCAT>-CHECKBOX = GC_TRUE.
          <FCAT>-COL_POS = 1.
          <FCAT>-EDIT = GF_EDIT.
        ENDIF.
      WHEN 'RECEIVED'.
        IF GF_EXEC_TY = GC_EXEC_TY-SN_PJ.
          <FCAT>-TECH = GC_TRUE.
        ELSE.
          %FCAT 'Receive'(c04).
          <FCAT>-CHECKBOX = GC_TRUE.
          <FCAT>-COL_POS = 2.
        ENDIF.
      WHEN 'REJECTED'.
        %FCAT 'Reject'(c02).
        <FCAT>-CHECKBOX = GC_TRUE.
        <FCAT>-COL_POS = 3.
        IF GF_EXEC_TY = GC_EXEC_TY-SN_PJ.
          <FCAT>-EDIT = ''.
        ELSE.
          <FCAT>-EDIT = GF_EDIT.
        ENDIF.
      WHEN 'ZSTAT_ACT_TX'.
        IF GF_EXEC_TY = GC_EXEC_TY-DP_PJ.
          %FCAT 'Status + action'(c07).
          <FCAT>-COL_POS = <FCAT>-COL_POS + 3.
        ELSE.
          <FCAT>-TECH = GC_TRUE.
        ENDIF.
      WHEN 'STAT_ACT01'
      OR  'STAT_ACT02'
      OR  'STAT_ACT03'
      OR  'STAT_ACT04'
      OR  'STAT_ACT05'.
        IF GF_EXEC_TY = GC_EXEC_TY-RC_PJ.
          LF_STAT_ACT = <FCAT>-FIELDNAME+8(2).
          READ TABLE GT_STAT_ACT INTO DATA(LS_STAT_ACT) WITH KEY STAT_ACT = LF_STAT_ACT.
          IF SY-SUBRC = 0.
            %FCAT LS_STAT_ACT-FLD_DESC.
          ENDIF.
          <FCAT>-EDIT       = GC_TRUE.
          <FCAT>-CHECKBOX   = ABAP_TRUE.
          <FCAT>-COL_POS    = <FCAT>-COL_POS + 3.
        ELSE.
          <FCAT>-TECH = GC_TRUE.
        ENDIF.
      WHEN 'DO'.
        <FCAT>-HOTSPOT = GC_TRUE.
        <FCAT>-COL_POS = <FCAT>-COL_POS + 3.
      WHEN 'SO'.
        <FCAT>-HOTSPOT = GC_TRUE.
        <FCAT>-COL_POS = <FCAT>-COL_POS + 3.
      WHEN 'BILNO'.
        IF GF_EXEC_TY = GC_EXEC_TY-SN_PJ.
          <FCAT>-TECH = GC_TRUE.
        ELSE.
          <FCAT>-COL_POS = <FCAT>-COL_POS + 3.
        ENDIF.
      WHEN 'POST1'.
        %FCAT 'Project Description'(c10).
        <FCAT>-COL_POS = <FCAT>-COL_POS + 3.
      WHEN 'ZSTATUS'.
        <FCAT>-REF_TABLE  = 'ZSDSFIT028'.
        <FCAT>-REF_FIELD  = 'ZSTATUS'.
        <FCAT>-COL_POS = <FCAT>-COL_POS + 3.
        <FCAT>-EDIT = GF_EDIT.
      WHEN 'ZADMIN'.
        <FCAT>-EDIT = GF_EDIT.
        <FCAT>-COL_POS    = <FCAT>-COL_POS + 3.
      WHEN 'ZSTATUS_TX'.
        %FCAT 'Status Description'(c03).
        <FCAT>-COL_POS = <FCAT>-COL_POS + 3.
      WHEN 'NETWR'
      OR   'MWSBK'.
        <FCAT>-CFIELDNAME = 'WAERK'.
        <FCAT>-COL_POS    = <FCAT>-COL_POS + 3.
      WHEN 'TOTAL'.
        %FCAT 'Total Amount'(c05).
        <FCAT>-COL_POS    = <FCAT>-COL_POS + 3.
*BOI CH01 420000161
      WHEN 'KUNAG_NM'.
        %FCAT 'Sold-to Name'(c11).
        <FCAT>-COL_POS = <FCAT>-COL_POS + 2.
      WHEN 'KUNRG_NM'.
        %FCAT 'Payer Name'(c12).
        <FCAT>-COL_POS = <FCAT>-COL_POS + 2.
*EOI CH01 420000161
      WHEN OTHERS.
        <FCAT>-COL_POS = <FCAT>-COL_POS + 3.

    ENDCASE.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_USER_COMMAND_SAVE_pj
*&---------------------------------------------------------------------*
FORM F_USER_COMMAND_SAVE_PJ .
  DATA: LT_UPDATE TYPE STANDARD TABLE OF ZSDSFIT028,
        LT_INSERT TYPE STANDARD TABLE OF ZSDSFIT028,
        LT_MAIL   TYPE TT_PJ,
        LS_RECORD TYPE ZSDSFIT028.
  DATA: LF_DATUM TYPE SY-DATUM,
        LF_UZEIT TYPE SY-UZEIT.
  DATA: LF_STAT_ACT  TYPE ZSDSFIC028-STAT_ACT,
        LF_FIELDNAME TYPE TEXT20.

  LF_DATUM = SY-DATUM.
  LF_UZEIT = SY-UZEIT.

  SORT GT_PJ_OLD BY DO .
  LOOP AT GT_PJ INTO DATA(LS_PJ) ##INTO_OK.
    READ TABLE GT_PJ_OLD INTO DATA(LS_PJ_OLD) WITH KEY DO = LS_PJ-DO
                                                       BINARY SEARCH.
    IF SY-SUBRC = 0.
      IF LS_PJ <> LS_PJ_OLD. "SOME CHANGE
        LS_RECORD = CORRESPONDING #( BASE ( LS_RECORD ) LS_PJ ).
        LS_RECORD-VBELN = LS_PJ-DO.

        IF LS_PJ-SENT = GC_TRUE.
          LS_RECORD-ZCHECK = GC_ZCHECK-SENT.
        ELSEIF LS_PJ-RECEIVED = GC_TRUE.
          LS_RECORD-ZCHECK = GC_ZCHECK-RECEIVED.
        ELSEIF LS_PJ-REJECTED = GC_TRUE.
          LS_RECORD-ZCHECK = GC_ZCHECK-REJECTED.
        ELSE.
          IF GF_EXEC_TY = GC_EXEC_TY-SN_PJ.
            LS_RECORD-ZCHECK = ''.
          ELSEIF GF_EXEC_TY = GC_EXEC_TY-RC_PJ.
            LS_RECORD-ZCHECK = GC_ZCHECK-SENT.
          ENDIF.
        ENDIF.
        CLEAR LF_STAT_ACT.

        IF GF_EXEC_TY = GC_EXEC_TY-RC_PJ.
          DO 5 TIMES.
            LF_STAT_ACT =  LF_STAT_ACT + 1.
            LF_FIELDNAME = 'STAT_ACT' && LF_STAT_ACT.
            ASSIGN LS_PJ-(LF_FIELDNAME) TO FIELD-SYMBOL(<L_STAT_ACT>).
            IF <L_STAT_ACT> IS ASSIGNED.
              IF <L_STAT_ACT> = ABAP_TRUE.
                LS_RECORD-ZSTAT_ACT = LF_STAT_ACT .
                EXIT.
              ENDIF.
            ENDIF.
            UNASSIGN <L_STAT_ACT>.
          ENDDO.
        ENDIF.

        IF GF_EXEC_TY = GC_EXEC_TY-SN_PJ.
          LS_RECORD-ZSEND_N = SY-UNAME.
          LS_RECORD-ZSEND_D = LF_DATUM.
          LS_RECORD-ZSEND_T = LF_UZEIT.
        ELSEIF GF_EXEC_TY = GC_EXEC_TY-RC_PJ.
          LS_RECORD-ZRCVD_N = SY-UNAME.
          LS_RECORD-ZRCVD_D = LF_DATUM.
          LS_RECORD-ZRCVD_T = LF_UZEIT.
        ENDIF.
        IF  LS_PJ-VBELN IS NOT INITIAL.
          APPEND LS_RECORD TO LT_UPDATE.
        ELSE.
          APPEND LS_RECORD TO LT_INSERT.
        ENDIF.
        IF GF_EXEC_TY = GC_EXEC_TY-RC_PJ
        AND LS_PJ-REJECTED = GC_TRUE.
          APPEND LS_PJ TO LT_MAIL.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF LT_UPDATE IS NOT INITIAL.
    UPDATE  ZSDSFIT028 FROM TABLE LT_UPDATE.
  ENDIF.
  IF LT_INSERT IS NOT INITIAL.
    INSERT ZSDSFIT028 FROM TABLE LT_INSERT.
  ENDIF.
  IF LT_MAIL IS NOT INITIAL.
    PERFORM F_SEND_MAIL_RC_PJ USING LT_MAIL.
  ENDIF.
  COMMIT WORK AND WAIT.
  MESSAGE S000(ZSDSCA01) WITH 'Data has been changed'(m01).
ENDFORM.

*----------------------------------------------------------------------*
*  Form F_ON_DATA_CHANGED_PJ
*----------------------------------------------------------------------*
*  Event ALV 1 Data is changed
*----------------------------------------------------------------------*
FORM F_ON_DATA_CHANGED_PJ  USING UF_DATA_CHANGED TYPE REF TO  CL_ALV_CHANGED_DATA_PROTOCOL ##CALLED
                                 UF_ONF4 TYPE  CHAR01          ##NEEDED
                                 UF_ONF4_BEFORE TYPE  CHAR01   ##NEEDED
                                 UF_ONF4_AFTER TYPE  CHAR01    ##NEEDED
                                 UF_UCOMM TYPE  SY-UCOMM       ##NEEDED.
  DATA: LS_STABLE  TYPE LVC_S_STBL,
        LF_REFRESH TYPE FLAG.

* For all valid changing cells
  LOOP AT UF_DATA_CHANGED->MT_GOOD_CELLS ASSIGNING FIELD-SYMBOL(<L_GOOD_CELL>).

*   Read Row
    READ TABLE GT_PJ ASSIGNING FIELD-SYMBOL(<L_PJ>)
                         INDEX <L_GOOD_CELL>-ROW_ID.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    CASE <L_GOOD_CELL>-FIELDNAME.

*     Example on validation error
      WHEN 'ZSTATUS'.
        IF  <L_GOOD_CELL>-VALUE IS NOT INITIAL.
          READ TABLE GT_ZSTATUS INTO DATA(LS_ZSTATUS)
                                WITH KEY VALUE = <L_GOOD_CELL>-VALUE ##WARN_OK.
          IF SY-SUBRC = 0.
            <L_PJ>-ZSTATUS_TX = LS_ZSTATUS-TEXT.
            LF_REFRESH = GC_TRUE.
          ENDIF.
        ENDIF.
      WHEN 'RECEIVED'.
        IF  <L_GOOD_CELL>-VALUE = GC_TRUE.
          IF <L_PJ>-REJECTED = GC_TRUE.
            CLEAR <L_PJ>-REJECTED .
            LF_REFRESH = GC_TRUE.
          ENDIF.
        ENDIF.
      WHEN 'REJECTED'.
        IF <L_GOOD_CELL>-VALUE = GC_TRUE.
          IF <L_PJ>-RECEIVED = GC_TRUE.
            CLEAR <L_PJ>-RECEIVED .
            LF_REFRESH = GC_TRUE.
          ENDIF.
          IF <L_PJ>-STAT_ACT01 = ABAP_TRUE
          OR <L_PJ>-STAT_ACT02 = ABAP_TRUE
          OR <L_PJ>-STAT_ACT03 = ABAP_TRUE
          OR <L_PJ>-STAT_ACT04 = ABAP_TRUE
          OR <L_PJ>-STAT_ACT05 = ABAP_TRUE.
            CLEAR: <L_PJ>-STAT_ACT01,
                   <L_PJ>-STAT_ACT02,
                   <L_PJ>-STAT_ACT03,
                   <L_PJ>-STAT_ACT04,
                   <L_PJ>-STAT_ACT05.
          ENDIF.
        ENDIF.
      WHEN 'STAT_ACT01'
      OR   'STAT_ACT02'
      OR   'STAT_ACT03'
      OR   'STAT_ACT04'
      OR   'STAT_ACT05'.
        PERFORM F_SET_AND_VALID_STAT_ACT_PJ USING <L_GOOD_CELL>
                                            CHANGING <L_PJ>.
        LF_REFRESH = GC_TRUE.
    ENDCASE.

  ENDLOOP.
  IF LF_REFRESH = GC_TRUE.
    LS_STABLE-ROW = 'X'.
    LS_STABLE-COL = ''.

    CALL METHOD GREF_GRID_1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE      = LS_STABLE
        I_SOFT_REFRESH = ''.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_send_Mail_rc_PJ
*&---------------------------------------------------------------------*
FORM F_SEND_MAIL_RC_PJ  USING UT_MAIL TYPE TT_PJ.
  DATA: LT_USRID TYPE TT_PJ,
        LT_DATA  TYPE TT_PJ,
        LF_TABIX TYPE SY-TABIX.
  DATA: LS_DOC_DATA     TYPE SODOCCHGI1,
        LT_RECEIVERS    TYPE TABLE OF SOMLRECI1,
        LS_RECEIVER     LIKE LINE OF LT_RECEIVERS,
        LT_MAILBODY     TYPE TABLE OF SOLISTI1,
        LS_MAILBODY     LIKE LINE OF LT_MAILBODY,
        LT_PACKING_LIST TYPE TABLE OF SOPCKLSTI1,
        LS_PACKING_LIST LIKE LINE OF LT_PACKING_LIST.
  LT_USRID[] = UT_MAIL[].
  SORT LT_USRID BY ZSEND_N VBELN.
  LT_DATA[]  = LT_USRID.
  DELETE ADJACENT DUPLICATES FROM LT_USRID COMPARING ZSEND_N.


  LS_DOC_DATA-OBJ_DESCR = 'Rejected Delivery Document'(n07).

  LOOP AT LT_USRID INTO DATA(LS_USRID) ##INTO_OK.
    CLEAR: LT_RECEIVERS,
           LT_MAILBODY,
           LT_PACKING_LIST.

    LS_RECEIVER-REC_TYPE  = 'B'.
    LS_RECEIVER-RECEIVER  = LS_USRID-ZSEND_N.
    LS_RECEIVER-EXPRESS   = 'X'.
    APPEND LS_RECEIVER TO LT_RECEIVERS.

    LS_MAILBODY-LINE = 'Please find rejected delivery document as bellow.'(n05).
    APPEND LS_MAILBODY TO LT_MAILBODY.

    CLEAR LS_MAILBODY.
    APPEND LS_MAILBODY TO LT_MAILBODY.

    LS_MAILBODY-LINE    = 'Delivery no.'(n06)     .
    LS_MAILBODY-LINE+20 = 'Reason for Rejection'(n04) .
    APPEND LS_MAILBODY TO LT_MAILBODY.

    READ TABLE LT_DATA TRANSPORTING NO FIELDS WITH KEY ZSEND_N = LS_USRID-ZSEND_N.
    IF SY-SUBRC = 0.
      LF_TABIX = SY-TABIX.
      LOOP AT LT_DATA INTO DATA(LS_DATA) FROM LF_TABIX ##INTO_OK.
        IF LS_DATA-ZSEND_N <> LS_USRID-ZSEND_N.
          EXIT.
        ENDIF.

        CLEAR LS_MAILBODY.
        LS_MAILBODY-LINE    = LS_DATA-VBELN.
        LS_MAILBODY-LINE+15 = LS_DATA-ZADMIN.
        APPEND LS_MAILBODY TO LT_MAILBODY.

      ENDLOOP.

*     Describe the body of the message
      CLEAR LS_PACKING_LIST.
      LS_PACKING_LIST-TRANSF_BIN = SPACE.
      LS_PACKING_LIST-HEAD_START = 1.
      LS_PACKING_LIST-HEAD_NUM = 0.
      LS_PACKING_LIST-BODY_START = 1.
      LS_PACKING_LIST-DOC_TYPE = 'RAW'.
      LS_PACKING_LIST-BODY_NUM = LINES( LT_MAILBODY ).
      APPEND LS_PACKING_LIST TO LT_PACKING_LIST.

      CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
        EXPORTING
          DOCUMENT_DATA              = LS_DOC_DATA
        TABLES
          PACKING_LIST               = LT_PACKING_LIST
          CONTENTS_TXT               = LT_MAILBODY
          RECEIVERS                  = LT_RECEIVERS
        EXCEPTIONS
          TOO_MANY_RECEIVERS         = 1
          DOCUMENT_NOT_SENT          = 2
          DOCUMENT_TYPE_NOT_EXIST    = 3
          OPERATION_NO_AUTHORIZATION = 4
          PARAMETER_ERROR            = 5
          X_ERROR                    = 6
          ENQUEUE_ERROR              = 7
          OTHERS                     = 8.

      IF SY-SUBRC <> 0.
        MESSAGE E000(ZSDSCA01) WITH 'Error sending mail'(m06).
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.
*BOD CH03 420000360
**&---------------------------------------------------------------------*
**& Form F_LOCK_ZSDSFIT027
**&---------------------------------------------------------------------*
*FORM F_LOCK_ZSDSFIT027  USING    UT_DATA TYPE ANY TABLE .
*  TYPES: BEGIN OF LTS_LOCK,
*           BILNO TYPE VBRK-VBELN,
*           VBELN TYPE VBRK-VBELN,
*         END OF LTS_LOCK.
*  DATA: LT_LOCK  TYPE STANDARD TABLE OF  LTS_LOCK,
*        LF_VBELN TYPE VBRK-VBELN.
*  LT_LOCK = CORRESPONDING #( BASE ( LT_LOCK ) UT_DATA ).
*  SORT LT_LOCK BY BILNO VBELN.
*  DELETE ADJACENT DUPLICATES FROM LT_LOCK COMPARING BILNO VBELN.
*
*  LOOP AT LT_LOCK INTO DATA(LS_LOCK).
*    IF LS_LOCK-BILNO IS NOT INITIAL.
*      LF_VBELN = LS_LOCK-BILNO.
*    ELSE.
*      LF_VBELN = LS_LOCK-VBELN.
*    ENDIF.
*    CALL FUNCTION 'ENQUEUE_EZSDSFIT027'
*      EXPORTING
*        MODE_ZSDSFIT027 = 'E'
*        MANDT           = SY-MANDT
*        VBELN           = LF_VBELN
*      EXCEPTIONS
*        FOREIGN_LOCK    = 1
*        SYSTEM_FAILURE  = 2
*        OTHERS          = 3.
*
*    IF SY-SUBRC <> 0.
*      DATA(LF_USRID) = SY-MSGV1.
*      MESSAGE S000(ZSDSCA01) WITH 'Billing document'(m07) LF_VBELN 'is locked by'(m08) LF_USRID
*      DISPLAY LIKE 'E'.
*      LEAVE LIST-PROCESSING.
*    ENDIF.
*  ENDLOOP.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_unlock_ZSDSFIT027
**&---------------------------------------------------------------------*
*FORM F_UNLOCK_ZSDSFIT027  USING    UT_DATA TYPE ANY TABLE .
*  TYPES: BEGIN OF LTS_LOCK,
*           BILNO TYPE VBRK-VBELN,
*           VBELN TYPE VBRK-VBELN,
*         END OF LTS_LOCK.
*  DATA: LT_LOCK  TYPE STANDARD TABLE OF  LTS_LOCK,
*        LF_VBELN TYPE VBRK-VBELN.
*  LT_LOCK = CORRESPONDING #( BASE ( LT_LOCK ) UT_DATA ).
*  SORT LT_LOCK BY BILNO VBELN.
*  DELETE ADJACENT DUPLICATES FROM LT_LOCK COMPARING BILNO VBELN.
*
*  LOOP AT LT_LOCK INTO DATA(LS_LOCK).
*    IF LS_LOCK-BILNO IS NOT INITIAL.
*      LF_VBELN = LS_LOCK-BILNO.
*    ELSE.
*      LF_VBELN = LS_LOCK-VBELN.
*    ENDIF.
*    CALL FUNCTION 'DEQUEUE_EZSDSFIT027'
*      EXPORTING
*        MODE_ZSDSFIT027 = 'E'
*        MANDT           = SY-MANDT
*        VBELN           = LF_VBELN.
*
*  ENDLOOP.
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_lock_ZSDSFIT028
**&---------------------------------------------------------------------*
*FORM F_LOCK_ZSDSFIT028  USING    UT_DATA TYPE ANY TABLE .
*  TYPES: BEGIN OF LTS_LOCK,
*           DO    TYPE LIKP-VBELN,
*           VBELN TYPE LIKP-VBELN,
*         END OF LTS_LOCK.
*  DATA: LT_LOCK  TYPE STANDARD TABLE OF  LTS_LOCK,
*        LF_VBELN TYPE VBRK-VBELN.
*  LT_LOCK = CORRESPONDING #( BASE ( LT_LOCK ) UT_DATA ).
*  SORT LT_LOCK BY DO VBELN.
*  DELETE ADJACENT DUPLICATES FROM LT_LOCK COMPARING DO VBELN.
*
*  LOOP AT LT_LOCK INTO DATA(LS_LOCK).
*    IF LS_LOCK-DO IS NOT INITIAL.
*      LF_VBELN = LS_LOCK-DO.
*    ELSE.
*      LF_VBELN = LS_LOCK-VBELN.
*    ENDIF.
*    CALL FUNCTION 'ENQUEUE_EZSDSFIT028'
*      EXPORTING
*        MODE_ZSDSFIT028 = 'E'
*        MANDT           = SY-MANDT
*        VBELN           = LF_VBELN
*      EXCEPTIONS
*        FOREIGN_LOCK    = 1
*        SYSTEM_FAILURE  = 2
*        OTHERS          = 3.
*
*    IF SY-SUBRC <> 0.
*      DATA(LF_USRID) = SY-MSGV1.
*      MESSAGE S000(ZSDSCA01) WITH 'Delivery document'(m09) LF_VBELN 'is locked by'(m08) LF_USRID
*      DISPLAY LIKE 'E'.
*      LEAVE LIST-PROCESSING.
*
*    ENDIF.
*  ENDLOOP.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form f_unlock_ZSDSFIT028
**&---------------------------------------------------------------------*
*FORM F_UNLOCK_ZSDSFIT028  USING    UT_DATA TYPE ANY TABLE .
*  TYPES: BEGIN OF LTS_LOCK,
*           DO    TYPE VBRK-VBELN,
*           VBELN TYPE VBRK-VBELN,
*         END OF LTS_LOCK.
*  DATA: LT_LOCK  TYPE STANDARD TABLE OF  LTS_LOCK,
*        LF_VBELN TYPE VBRK-VBELN.
*  LT_LOCK = CORRESPONDING #( BASE ( LT_LOCK ) UT_DATA ).
*  SORT LT_LOCK BY DO VBELN.
*  DELETE ADJACENT DUPLICATES FROM LT_LOCK COMPARING DO VBELN.
*
*  LOOP AT LT_LOCK INTO DATA(LS_LOCK).
*    IF LS_LOCK-DO IS NOT INITIAL.
*      LF_VBELN = LS_LOCK-DO.
*    ELSE.
*      LF_VBELN = LS_LOCK-VBELN.
*    ENDIF.
*    CALL FUNCTION 'DEQUEUE_EZSDSFIT028'
*      EXPORTING
*        MODE_ZSDSFIT028 = 'E'
*        MANDT           = SY-MANDT
*        VBELN           = LF_VBELN.
*
*  ENDLOOP.
*ENDFORM.
*EOD CH03 420000360
*&---------------------------------------------------------------------*
*& Form f_unlock_ZSDSFIT028
*&---------------------------------------------------------------------*
FORM F_HOTSPOT_CLICK_1 USING US_ROW_ID TYPE LVC_S_ROW
                             US_COLUMN_ID TYPE LVC_S_COL ##CALLED.
  CASE GF_EXEC_TY.
    WHEN GC_EXEC_TY-SN_IV.
      READ TABLE GT_SN_IV ASSIGNING FIELD-SYMBOL(<L_SN_IV>) INDEX US_ROW_ID-INDEX.
      IF SY-SUBRC = 0.
        IF US_COLUMN_ID-FIELDNAME = 'BILNO'.
          IF <L_SN_IV>-BILNO IS NOT INITIAL.
            SET PARAMETER ID 'VF' FIELD <L_SN_IV>-BILNO.
            CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
          ENDIF.
        ELSEIF US_COLUMN_ID-FIELDNAME = 'SO'.
          IF <L_SN_IV>-SO IS NOT INITIAL.
            SET PARAMETER ID 'AUN' FIELD <L_SN_IV>-SO.
            CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
          ENDIF.
        ENDIF.
      ENDIF.
    WHEN GC_EXEC_TY-RC_IV.
      READ TABLE GT_RC_IV ASSIGNING FIELD-SYMBOL(<L_RC_IV>) INDEX US_ROW_ID-INDEX.
      IF SY-SUBRC = 0.
        IF US_COLUMN_ID-FIELDNAME = 'VBELN'.
          IF <L_RC_IV>-VBELN IS NOT INITIAL.
            SET PARAMETER ID 'VF' FIELD <L_RC_IV>-VBELN.
            CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
          ENDIF.
        ENDIF.
      ENDIF.
    WHEN GC_EXEC_TY-DP_IV.
      READ TABLE GT_DP_IV ASSIGNING FIELD-SYMBOL(<L_DP_IV>) INDEX US_ROW_ID-INDEX.
      IF SY-SUBRC = 0.
        IF US_COLUMN_ID-FIELDNAME = 'VBELN'.
          IF <L_DP_IV>-VBELN IS NOT INITIAL.
            SET PARAMETER ID 'VF' FIELD <L_DP_IV>-VBELN.
            CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
          ENDIF.
        ENDIF.
      ENDIF.
    WHEN GC_EXEC_TY-SN_PJ
    OR   GC_EXEC_TY-RC_PJ
    OR   GC_EXEC_TY-DP_PJ .
      READ TABLE GT_PJ ASSIGNING FIELD-SYMBOL(<L_PJ>) INDEX US_ROW_ID-INDEX.
      IF SY-SUBRC = 0.
        IF US_COLUMN_ID-FIELDNAME = 'DO'.
          SET PARAMETER ID 'VL' FIELD <L_PJ>-DO.
          CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
        ENDIF.
      ENDIF.
      READ TABLE GT_PJ ASSIGNING <L_PJ> INDEX US_ROW_ID-INDEX.
      IF SY-SUBRC = 0.
        IF US_COLUMN_ID-FIELDNAME = 'SO'.
          SET PARAMETER ID 'AUN' FIELD <L_PJ>-SO.
          CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
        ENDIF.
      ENDIF.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_STAT_ACT
*&---------------------------------------------------------------------*
FORM F_SET_STAT_ACT  USING    US_GOOD_CELL TYPE LVC_S_MODI
                               CHANGING CS_RC_IV TYPE TS_RC_IV.
  DATA: LF_FIELDNAME    TYPE TEXT20,
        LF_CUR_STAT_ACT TYPE ZSDSFIC028-STAT_ACT,
        LF_STAT_ACT     TYPE ZSDSFIC028-STAT_ACT.

  LF_CUR_STAT_ACT = US_GOOD_CELL-FIELDNAME+8(2).

  IF US_GOOD_CELL-VALUE = ABAP_TRUE . "MARK FIELD
    CS_RC_IV-RECEIVED = ABAP_TRUE.
    CLEAR CS_RC_IV-REJECTED.
    DO 5 TIMES.
      LF_STAT_ACT = LF_STAT_ACT + 1.
      IF LF_STAT_ACT = LF_CUR_STAT_ACT.
        CONTINUE.
      ENDIF.
      LF_FIELDNAME = 'STAT_ACT' && LF_STAT_ACT.
      ASSIGN CS_RC_IV-(LF_FIELDNAME) TO FIELD-SYMBOL(<L_STAT_ACT>).
      IF <L_STAT_ACT>  IS ASSIGNED.
        CLEAR <L_STAT_ACT>.
      ENDIF.
    ENDDO.
*   Clear other feild 'STAT_ACT<NN>'
  ELSE. "UNMARK FIELD
    CLEAR CS_RC_IV-RECEIVED .
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_AND_VALID_STAT_ACT_PJ
*&---------------------------------------------------------------------*
FORM F_SET_AND_VALID_STAT_ACT_PJ  USING US_GOOD_CELL TYPE LVC_S_MODI
                               CHANGING CS_PJ TYPE TS_PJ.
  DATA: LF_FIELDNAME    TYPE TEXT20,
        LF_CUR_STAT_ACT TYPE ZSDSFIC028-STAT_ACT,
        LF_STAT_ACT     TYPE ZSDSFIC028-STAT_ACT.

  LF_CUR_STAT_ACT = US_GOOD_CELL-FIELDNAME+8(2).

  IF US_GOOD_CELL-VALUE = ABAP_TRUE . "MARK FIELD
    CS_PJ-RECEIVED = ABAP_TRUE.
    CLEAR CS_PJ-REJECTED.
    DO 5 TIMES.
      LF_STAT_ACT = LF_STAT_ACT + 1.
      IF LF_STAT_ACT = LF_CUR_STAT_ACT.
        CONTINUE.
      ENDIF.
      LF_FIELDNAME = 'STAT_ACT' && LF_STAT_ACT.
      ASSIGN CS_PJ-(LF_FIELDNAME) TO FIELD-SYMBOL(<L_STAT_ACT>).
      IF <L_STAT_ACT>  IS ASSIGNED.
        CLEAR <L_STAT_ACT>.
      ENDIF.
    ENDDO.
*   Clear other feild 'STAT_ACT<NN>'
  ELSE. "UNMARK FIELD
    CLEAR CS_PJ-RECEIVED .
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_check_delete_bank_vendor
*&---------------------------------------------------------------------*
FORM F_CHECK_DELETE_BANK_VENDOR  USING   UF_VBELN TYPE VBRK-VBELN
                                         UF_REJECT TYPE FLAG.
  DATA: LT_BANK_VENDOR_DELETE TYPE ZCL_SDSFI_COLL_LOG=>TT_BILLING,
        LF_STAT_ACT_NEW       TYPE ZSDSFIC028-STAT_ACT,
        LF_STAT_ACT_OLD       TYPE ZSDSFIC028-STAT_ACT,
        LF_FIELDNAME          TYPE TEXT20.

  IF UF_REJECT = ABAP_FALSE.
    READ TABLE GT_RC_IV INTO DATA(LS_NEW) WITH KEY VBELN = UF_VBELN.
    CLEAR LF_STAT_ACT_NEW.
    DO 5 TIMES.
      LF_STAT_ACT_NEW =  LF_STAT_ACT_NEW + 1.
      LF_FIELDNAME = 'STAT_ACT' && LF_STAT_ACT_NEW.
      ASSIGN LS_NEW-(LF_FIELDNAME) TO FIELD-SYMBOL(<L_STAT_ACT>).
      IF <L_STAT_ACT> IS ASSIGNED.
        IF <L_STAT_ACT> = ABAP_TRUE.
          EXIT.
        ENDIF.
      ENDIF.
      UNASSIGN <L_STAT_ACT>.
    ENDDO.
  ENDIF.

  READ TABLE GT_RC_IV_OLD INTO DATA(LS_OLD) WITH KEY VBELN = UF_VBELN.
  CLEAR LF_STAT_ACT_OLD.
  DO 5 TIMES.
    LF_STAT_ACT_OLD =  LF_STAT_ACT_OLD + 1.
    LF_FIELDNAME = 'STAT_ACT' && LF_STAT_ACT_OLD.
    ASSIGN LS_OLD-(LF_FIELDNAME) TO <L_STAT_ACT>.
    IF <L_STAT_ACT> IS ASSIGNED.
      IF <L_STAT_ACT> = ABAP_TRUE.
        EXIT.
      ENDIF.
    ENDIF.
    UNASSIGN <L_STAT_ACT>.
  ENDDO.
  READ TABLE GT_STAT_ACT INTO DATA(LS_STAT_ACT_NEW) WITH KEY STAT_ACT = LF_STAT_ACT_NEW.
  READ TABLE GT_STAT_ACT INTO DATA(LS_STAT_ACT_OLD) WITH KEY STAT_ACT = LF_STAT_ACT_OLD.

  "change from not bank vendor -> bank vendor
  IF LS_STAT_ACT_OLD-BANK_VENDOR = ABAP_TRUE
  AND ( LS_STAT_ACT_NEW-BANK_VENDOR = ABAP_FALSE OR UF_REJECT = ABAP_TRUE ).
    " check whether can be delete data from table ZSDST029

    INSERT VALUE #( VBELN = UF_VBELN
                    ACTION_TYPE = LS_STAT_ACT_OLD-BL_ACTTY
                    STATUS = LS_STAT_ACT_OLD-BL_STAT )
    INTO TABLE  LT_BANK_VENDOR_DELETE.

    CALL METHOD ZCL_SDSFI_COLL_LOG=>DELETE_BANK_VENDOR_LOG
      EXPORTING
        IT_VBELN   = LT_BANK_VENDOR_DELETE
        IV_TESTRUN = 'X'
      IMPORTING
        EV_RC      = DATA(LV_RC) ##NEEDED
        ET_RETURN  = DATA(LT_RETURN).
    READ TABLE LT_RETURN INTO DATA(LS_RETURN) WITH KEY TYPE = 'E'.
    IF SY-SUBRC = 0.
      MESSAGE ID LS_RETURN-ID TYPE LS_RETURN-TYPE NUMBER LS_RETURN-NUMBER
      WITH LS_RETURN-MESSAGE_V1 LS_RETURN-MESSAGE_V2 LS_RETURN-MESSAGE_V3 LS_RETURN-MESSAGE_V4.
    ENDIF.
  ENDIF.

ENDFORM.
*BOI CH01 420000145
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
  IF GF_EXEC_TY = GC_EXEC_TY-SN_IV.
    LOOP AT GT_SN_IV ASSIGNING FIELD-SYMBOL(<L_RC_IV>) .
      <L_RC_IV>-SENT = UF_X.
    ENDLOOP.
  ELSEIF GF_EXEC_TY =  GC_EXEC_TY-SN_PJ.
    LOOP AT GT_PJ ASSIGNING FIELD-SYMBOL(<L_PJ>) .
      <L_PJ>-SENT = UF_X.
    ENDLOOP.
  ENDIF.

ENDFORM.
*EOI CH01 420000145
