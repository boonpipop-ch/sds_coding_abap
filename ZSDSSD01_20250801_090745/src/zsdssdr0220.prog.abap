*-----------------------------------------------------------------------
*  Program ID         : ZSDSSDR0220
*  Creation Date      : 19.07.2024
*  Author             : Boonpipop Ch.
*  Add-on ID          : ZSDF008
*  Description        : Packing List Form
*  Purpose            : N/A
*  Copied from        : N/A
*  Restriction        :
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Report ZSDSSDR0220
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSDSSDR0220 MESSAGE-ID ZSDSSD02.
TYPE-POOLS : TRUXS,SLIS,ICON.
*-----------------------------------------------------------------------
* T A B L E S
*-----------------------------------------------------------------------
TABLES: LIKP,
        ZSDSSDT017,
        SSCRFIELDS.

*-----------------------------------------------------------------------
* T Y P E S
*-----------------------------------------------------------------------
TYPES:
  BEGIN OF GTYP_ALV,
    VBELN TYPE LIKP-VBELN,    "Delivery Number
    POSNR TYPE LIPS-POSNR,    "Item Number
    MATNR TYPE LIPS-MATNR,    "Material Number
    ARKTX TYPE LIPS-ARKTX,    "Material Description
    LFIMG TYPE LIPS-LFIMG,    "Quantity
    VRKME TYPE LIPS-VRKME,    "Sales Unit
    ZPACK TYPE LIPS-LFIMG,    "Package
    NTGEW TYPE LIPS-NTGEW,    "Net Weight
    BRGEW TYPE LIPS-BRGEW,    "Net Weight
    GEWEI TYPE LIPS-GEWEI,    "Unit
    GROES TYPE MARA-GROES,    "Dimension
    ERDAT TYPE LIPS-ERDAT,    "Creation Date
  END OF GTYP_ALV.


*-----------------------------------------------------------------------
* D A T A
*-----------------------------------------------------------------------
DATA:
*-> internal tables
  GT_ALV        TYPE STANDARD TABLE OF ZSDSSDS057,
  GT_ZSDSSDT017 TYPE STANDARD TABLE OF ZSDSSDT017,
*gt_alv     type standard table of gtyp_alv,
*-> range
*-> work areas
  GS_PACKING    TYPE ZSDSSDS050,
*-> variables
  GV_VERSION    TYPE AD_NATION.
*-> reference

DATA:
  GT_FIELDCAT   TYPE SLIS_T_FIELDCAT_ALV,                   "#EC NEEDED
  GS_LAYOUT     TYPE SLIS_LAYOUT_ALV,                       "#EC NEEDED
  GT_SORT       TYPE SLIS_T_SORTINFO_ALV,                   "#EC NEEDED
  GT_FILTER     TYPE SLIS_T_FILTER_ALV,                     "#EC NEEDED
  GT_EXCLUDING  TYPE SLIS_T_EXTAB,                          "#EC NEEDED
  GT_LISTHEADER TYPE SLIS_T_LISTHEADER,                     "#EC NEEDED
  GS_PRINT      TYPE SLIS_PRINT_ALV,                        "#EC NEEDED
  GS_KEYINFO    TYPE SLIS_KEYINFO_ALV,                      "#EC NEEDED
  GV_PF_STATUS  LIKE SY-PFKEY,                              "#EC NEEDED
  GS_VARIANT    LIKE DISVARIANT,                            "#EC NEEDED
  GS_ALV        TYPE ZSDSSDS057
  .


*-----------------------------------------------------------------------
* C O N S T A N T S
*-----------------------------------------------------------------------
CONSTANTS:
  GC_TCODE  TYPE  SY-TCODE  VALUE 'ZSDSMM004',
  GC_TDDEST TYPE SSFCOMPOP-TDDEST VALUE 'SDLC'.

CONSTANTS:
  GC_HEADER_HEIGHT_1 TYPE  I  VALUE 0,
  GC_ALV_HEIGHT_1    TYPE  I  VALUE 100.
*-----------------------------------------------------------------------
* M A C R O S
*-----------------------------------------------------------------------
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
SELECTION-SCREEN BEGIN OF BLOCK S03 WITH FRAME TITLE TEXT-003.
  PARAMETERS : R_CRET RADIOBUTTON GROUP RAD2 DEFAULT 'X' USER-COMMAND C01,
               R_RPRT RADIOBUTTON GROUP RAD2.
SELECTION-SCREEN END OF BLOCK S03.


SELECTION-SCREEN BEGIN OF BLOCK S01 WITH FRAME TITLE TEXT-001.
* PARAMETERS:
  SELECT-OPTIONS: S_VBELN FOR LIKP-VBELN OBLIGATORY MODIF ID NEW,
                  S_ERDAT FOR LIKP-ERDAT MODIF ID NEW.

  PARAMETERS: P_LFART TYPE LIKP-LFART DEFAULT 'ZD03' MODIF ID NEW.

  SELECT-OPTIONS: S_KUNNR FOR LIKP-KUNNR MODIF ID NEW.

  SELECT-OPTIONS: S_ZPACK FOR ZSDSSDT017-PACKING_NO NO-EXTENSION MODIF ID OLD.
SELECTION-SCREEN END OF BLOCK S01.

SELECTION-SCREEN BEGIN OF BLOCK S02 WITH FRAME TITLE TEXT-002.
  PARAMETERS : R_TH RADIOBUTTON GROUP RAD1 DEFAULT 'X',
               R_EN RADIOBUTTON GROUP RAD1.
SELECTION-SCREEN END OF BLOCK S02.

*-----------------------------------------------------------------------
* I N I T I A L I Z A T I O N
*-----------------------------------------------------------------------
INITIALIZATION.
*  PERFORM f_authorize_check USING gc_tcode.
  PERFORM SET_FN_KEY.


*-----------------------------------------------------------------------
* A T   S E L E C T I O N - S C R E E N
*-----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF SCREEN-GROUP1 EQ 'NEW'.
      IF R_CRET IS NOT INITIAL.
        SCREEN-ACTIVE = 1.
      ELSE.
        SCREEN-ACTIVE = 0.
      ENDIF.
    ELSEIF SCREEN-GROUP1 EQ 'OLD'.
      IF R_RPRT IS NOT INITIAL.
        SCREEN-ACTIVE     = 1.
        CLEAR: S_ZPACK-LOW.
      ELSE.
        SCREEN-ACTIVE     = 0.
      ENDIF.
    ENDIF.

    IF SCREEN-NAME EQ 'S_ZPACK-LOW'.
      IF R_RPRT IS NOT INITIAL.
        SCREEN-REQUIRED   = 2.
      ELSE.
        SCREEN-REQUIRED   = 0.
      ENDIF.
    ENDIF.

    IF SCREEN-NAME EQ 'S_ZPACK-HIGH'.
      SCREEN-ACTIVE     = 0.
    ENDIF.

    IF SCREEN-NAME EQ 'P_LFART'.
      SCREEN-INPUT = 0.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

*AT SELECTION-SCREEN ON HELP-REQUEST FOR <field>
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR <field>
AT SELECTION-SCREEN.
  CASE SSCRFIELDS-UCOMM.
    WHEN 'ONLI'.
      "Check Require field
      PERFORM CHK_INPUT_FIELD.
  ENDCASE.

*-----------------------------------------------------------------------
* S T A R T - O F - S E L E C T I O N
*-----------------------------------------------------------------------
START-OF-SELECTION.

  IF R_EN IS NOT INITIAL. "en = checked
    GV_VERSION = 'I'.
    GS_PACKING-LANGU = 'E'.
  ELSE.
    GV_VERSION = SPACE.
    GS_PACKING-LANGU = '2'.
  ENDIF.

*-> read data
  IF R_CRET IS NOT INITIAL.
    CLEAR S_ZPACK-LOW.
  ENDIF.
  PERFORM GET_ALV_DATA.
  IF GT_ALV[] IS INITIAL.
*   Message: No data found.
    MESSAGE S001(ZSDSCA01).
    RETURN.
  ENDIF.

*-----------------------------------------------------------------------
* E N D - O F - S E L E C T I O N
*-----------------------------------------------------------------------
END-OF-SELECTION.
*-> process data
  PERFORM F_DISPLAY_RESULT.

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
*&---------------------------------------------------------------------*
*& Form update_log
*&---------------------------------------------------------------------*
*& update table log
*&---------------------------------------------------------------------*
FORM UPDATE_LOG .

  DATA(LV_DATE) = SY-DATUM.
  DATA(LV_TIME) = SY-UZEIT.

  DATA: LS_ZSDSSDT017 TYPE ZSDSSDT017,
        LT_ZSDSSDT017 TYPE STANDARD TABLE OF ZSDSSDT017.

  DATA(LT_DATA) = GT_ALV[].

  DELETE LT_DATA WHERE ZCHECK IS INITIAL.
  SORT LT_DATA BY VBELN POSNR.

  LOOP AT LT_DATA ASSIGNING FIELD-SYMBOL(<LFS_ITEM>).
    LS_ZSDSSDT017-PACKING_NO  = GS_PACKING-HEADER-DOC_NO+0(10).
    LS_ZSDSSDT017-VBELN       = <LFS_ITEM>-VBELN.
    LS_ZSDSSDT017-POSNR       = <LFS_ITEM>-POSNR.
    LS_ZSDSSDT017-MATNR       = <LFS_ITEM>-MATNR.
    LS_ZSDSSDT017-ZPACK       = <LFS_ITEM>-ZPACK.

    IF R_RPRT IS INITIAL.
      LS_ZSDSSDT017-ZCRT_DATE   = LV_DATE.
      LS_ZSDSSDT017-ZCRT_TIME   = LV_TIME.
      LS_ZSDSSDT017-ZCRT_USER   = SY-UNAME.
      LS_ZSDSSDT017-ZCRT_PGM    = SY-CPROG.
    ELSE.
      READ TABLE GT_ZSDSSDT017 INTO DATA(LS_LOG)
                               INDEX 1.
      IF SY-SUBRC = 0.
        LS_ZSDSSDT017-ZCRT_DATE   = LS_LOG-ZCRT_DATE.
        LS_ZSDSSDT017-ZCRT_TIME   = LS_LOG-ZCRT_TIME.
        LS_ZSDSSDT017-ZCRT_USER   = LS_LOG-ZCRT_USER.
        LS_ZSDSSDT017-ZCRT_PGM    = LS_LOG-ZCRT_PGM.
      ENDIF.
    ENDIF.

    LS_ZSDSSDT017-ZUPD_DATE     = LV_DATE.
    LS_ZSDSSDT017-ZUPD_TIME     = LV_TIME.
    LS_ZSDSSDT017-ZUPD_USER     = SY-UNAME.
    LS_ZSDSSDT017-ZUPD_PGM      = SY-CPROG.

    APPEND LS_ZSDSSDT017 TO LT_ZSDSSDT017.
  ENDLOOP.

  IF LT_ZSDSSDT017 IS NOT INITIAL.
    MODIFY ZSDSSDT017 FROM TABLE LT_ZSDSSDT017[].
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_form_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM GET_FORM_DATA .

  DATA:
*lv_total_exw TYPE
*lv_total_fob
    LV_TOTAL_QTY  TYPE P DECIMALS 2, "lips-lfimg,
    LV_TOTAL_PACK TYPE ZSDSSDS057-ZPACK,
    LV_TOTAL_NW   TYPE LIPS-NTGEW,
    LV_TOTAL_GW   TYPE LIPS-BRGEW,
    LV_RUN_DOC    TYPE N LENGTH 4,
    LV_ITEM       TYPE N,
    LV_DATE_TXT   TYPE CHAR10.

  CONSTANTS: LC_SDS TYPE T001-BUKRS VALUE '1000'.
  DATA: LT_SELECTED LIKE GT_ALV.

  CLEAR: GS_PACKING.

  IF NOT LINE_EXISTS( GT_ALV[ ZCHECK = 'X' ] ).
    MESSAGE W000(ZSDSCA01) WITH TEXT-W01.
    RETURN.
  ENDIF.

  LT_SELECTED[] = GT_ALV[].
  DELETE LT_SELECTED WHERE ZCHECK = SPACE.

  "Packing No.
  IF R_RPRT IS INITIAL.
    SELECT MAX( PACKING_NO )
      FROM ZSDSSDT017
      INTO @DATA(LV_DOC).
    IF SY-SUBRC = 0 AND LV_DOC+8(2) = SY-DATUM+2(2).
      LV_RUN_DOC = LV_DOC+3(4) + 1.
      GS_PACKING-HEADER-DOC_NO = |EX-{ LV_RUN_DOC }-{ SY-DATUM+2(2) }|.
    ELSE.
      GS_PACKING-HEADER-DOC_NO = |EX-0001-{ SY-DATUM+2(2) }|.
    ENDIF.
  ELSE.
    GS_PACKING-HEADER-DOC_NO = S_ZPACK-LOW.
  ENDIF.

  "=>Document Name
  GS_PACKING-HEADER-DOC_NAME = 'Packing List'.
*  gs_packing-header-doc_no = 'EX-0001-24'.
  GS_PACKING-HEADER-DOC_DATE = |{ SY-DATUM+6(2) }/{ SY-DATUM+4(2) }/{ SY-DATUM+0(4) }|.

  "=>Company Data
  SELECT SINGLE
    ADRNR,
    STCEG
    FROM T001
    INTO @DATA(LS_T001)
    WHERE BUKRS = @LC_SDS.
  IF SY-SUBRC = 0.

    SELECT SINGLE
      ADDRNUMBER,
      NAME1,
      STREET,
      CITY1,
      CITY2,
      POST_CODE1,
      TEL_NUMBER,
      FAX_NUMBER
    FROM ADRC
    INTO @DATA(LS_ADRC)
    WHERE ADDRNUMBER = @LS_T001-ADRNR
      AND NATION     = @GV_VERSION.
    IF SY-SUBRC = 0.

      "Company name
      GS_PACKING-HEADER-COM_NAME = LS_ADRC-NAME1.

      "Address
      CONCATENATE LS_ADRC-STREET
                  LS_ADRC-CITY2
                  LS_ADRC-CITY1
                  LS_ADRC-POST_CODE1
                  INTO GS_PACKING-HEADER-COM_ADDR SEPARATED BY ''.

      CONCATENATE 'TEL' LS_ADRC-TEL_NUMBER
                  'FAX' LS_ADRC-FAX_NUMBER
                  INTO GS_PACKING-HEADER-COM_CONTACT SEPARATED BY ''.

      "Tax ID
      GS_PACKING-HEADER-TAX_ID = LS_T001-STCEG.
      REPLACE ALL OCCURRENCES OF 'TH' IN GS_PACKING-HEADER-TAX_ID WITH SPACE.
      CONDENSE GS_PACKING-HEADER-TAX_ID.
    ENDIF.

  ENDIF.

  "Get Delivery Data
  SELECT
    H~VBELN,
    I~POSNR,
    H~KUNNR,
    H~INCO1,
    I~MATNR
    FROM LIKP AS H
    INNER JOIN LIPS AS I
    ON H~VBELN = I~VBELN
    INTO TABLE @DATA(LT_DELIVERY)
    FOR ALL ENTRIES IN @GT_ALV
    WHERE ( H~VBELN = @GT_ALV-VBELN   "selected item
        AND I~POSNR = @GT_ALV-POSNR ).
*      AND h~lfart EQ @p_lfart.
  IF SY-SUBRC = 0.
    SORT LT_DELIVERY BY VBELN POSNR.
  ENDIF.

  "Ship-to
  READ TABLE LT_DELIVERY INTO DATA(LS_DELIVERY)
                         INDEX 1.
  IF SY-SUBRC = 0.
    GS_PACKING-HEADER-SHIP_TO = LS_DELIVERY-KUNNR.
    GS_PACKING-HEADER-INCO_TERM = LS_DELIVERY-INCO1.

    SELECT SINGLE
      KNA1~KUNNR,
      KNA1~ADRNR,
      ADRC~NAME1,
      ADRC~STREET,
      ADRC~CITY1,
      ADRC~CITY2,
      ADRC~POST_CODE1,
      ADRC~TEL_NUMBER,
      ADRC~FAX_NUMBER
      FROM KNA1
      INNER JOIN ADRC
      ON KNA1~ADRNR = ADRC~ADDRNUMBER
      INTO @DATA(LS_SHIPTO)
      WHERE KNA1~KUNNR = @LS_DELIVERY-KUNNR.
    IF SY-SUBRC = 0.
      GS_PACKING-HEADER-SHIP_NAME = LS_SHIPTO-NAME1.

      "Address
      CONCATENATE LS_SHIPTO-STREET
                  LS_SHIPTO-CITY2
                  LS_SHIPTO-CITY1
                  LS_SHIPTO-POST_CODE1
                  INTO GS_PACKING-HEADER-SHIP_ADDR SEPARATED BY ''.

      "Contact
      CONCATENATE 'TEL' LS_SHIPTO-TEL_NUMBER
                  'FAX' LS_SHIPTO-FAX_NUMBER
                  INTO GS_PACKING-HEADER-SHIP_CONTACT SEPARATED BY ''.

    ENDIF.
  ENDIF.

*  gs_packing-header
  IF LT_DELIVERY[] IS NOT INITIAL.
    SELECT
      SER01~OBKNR,
      SER01~LIEF_NR,
      SER01~POSNR,
      OBJK~SERNR,
      OBJK~MATNR
      FROM SER01
      INNER JOIN OBJK
      ON SER01~OBKNR = OBJK~OBKNR
      INTO TABLE @DATA(LT_SERIAL)
      FOR ALL ENTRIES IN @LT_DELIVERY[]
      WHERE ( LIEF_NR = @LT_DELIVERY-VBELN AND
              POSNR   = @LT_DELIVERY-POSNR )
        AND OBJK~TASER = 'SER01'
        AND OBJK~MATNR = @LT_DELIVERY-MATNR.
    IF SY-SUBRC = 0.
      SORT LT_SERIAL BY LIEF_NR POSNR.
    ENDIF.

    SELECT
      VBFA~VBELN,   "Current Document
      VBFA~POSNN,
      VBFA~VBELV,   "Previous Document
      VBFA~POSNV,
      VBFA~VBTYP_V,
      VBAK~ERDAT AS SO_CREATE
    INTO TABLE @DATA(LT_VBFA)
    FROM VBFA
    INNER JOIN VBAK ON VBFA~VBELV = VBAK~VBELN
    FOR ALL ENTRIES IN @LT_DELIVERY
    WHERE VBFA~VBELN = @LT_DELIVERY-VBELN
      AND VBFA~POSNN = @LT_DELIVERY-POSNR
      AND VBFA~VBTYP_V = 'C'.
    IF SY-SUBRC = 0.
      SORT LT_VBFA BY VBELN POSNN.
      "Ref. = Quotaion doc/Quo DAte , ...... ,.....
      LOOP AT LT_VBFA INTO DATA(LS_VBFA).

        CONCATENATE LS_VBFA-SO_CREATE+6(2)
                    LS_VBFA-SO_CREATE+4(2)
                    LS_VBFA-SO_CREATE+0(4)
                    INTO LV_DATE_TXT
                    SEPARATED BY '.'.
        IF SY-INDEX = 1.
          GS_PACKING-HEADER-REFERENCE = |{ LS_VBFA-VBELV }/{ LV_DATE_TXT }|.
        ELSE.
          GS_PACKING-HEADER-REFERENCE = |{ GS_PACKING-HEADER-REFERENCE }, { LS_VBFA-VBELV }/{ LV_DATE_TXT }|.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDIF.


  CLEAR LV_ITEM.
  LOOP AT  LT_SELECTED INTO DATA(LS_SELECTED).

    LV_ITEM = LV_ITEM = 1.

    APPEND INITIAL LINE TO GS_PACKING-ITEM ASSIGNING FIELD-SYMBOL(<LFS_ITEM>).
    <LFS_ITEM>-ITEM_NO          = LV_ITEM.
    <LFS_ITEM>-MATNR            = LS_SELECTED-MATNR.
    <LFS_ITEM>-MATNR_DESC       = LS_SELECTED-ARKTX.

*    <lfs_item>-UNIT_PRICE       =
*    <lfs_item>-VALUE            =

    WRITE: LS_SELECTED-LFIMG TO <LFS_ITEM>-QTY          DECIMALS 0,
           LS_SELECTED-ZPACK TO <LFS_ITEM>-ZPACKAGE     DECIMALS 0,
           LS_SELECTED-NTGEW TO <LFS_ITEM>-NET_WEIGHT   DECIMALS 2,
           LS_SELECTED-BRGEW TO <LFS_ITEM>-GROSS_WEIGHT DECIMALS 2.



*    <lfs_item>-qty              = ls_selected-lfimg.
*    <lfs_item>-zpackage         = ls_selected-zpack.
*    <lfs_item>-net_weight       = ls_selected-ntgew.
*    <lfs_item>-gross_weight     = ls_selected-brgew.
    <LFS_ITEM>-DIMENSION        = LS_SELECTED-GROES.
*    <lfs_item>-SERIAL_NUMBER    =

    CONDENSE: <LFS_ITEM>-QTY,
              <LFS_ITEM>-ZPACKAGE,
              <LFS_ITEM>-NET_WEIGHT,
              <LFS_ITEM>-GROSS_WEIGHT,
              <LFS_ITEM>-DIMENSION.

    "Total
    LV_TOTAL_QTY  = LV_TOTAL_QTY  + LS_SELECTED-LFIMG.
    LV_TOTAL_PACK = LV_TOTAL_PACK + LS_SELECTED-ZPACK.
    LV_TOTAL_NW   = LV_TOTAL_NW   + LS_SELECTED-NTGEW.
    LV_TOTAL_GW   = LV_TOTAL_GW   + LS_SELECTED-BRGEW.

    READ TABLE LT_SERIAL TRANSPORTING NO FIELDS
                         WITH KEY LIEF_NR = LS_SELECTED-VBELN
                                  POSNR   = LS_SELECTED-POSNR
                                  BINARY SEARCH.
    IF SY-SUBRC = 0.
      LOOP AT LT_SERIAL INTO DATA(LS_SERIAL) FROM SY-TABIX.
        IF LS_SERIAL-LIEF_NR <> LS_SELECTED-VBELN AND
           LS_SERIAL-POSNR   <> LS_SELECTED-POSNR.
          EXIT.
        ENDIF.

        IF SY-INDEX = 1.
          <LFS_ITEM>-SERIAL_NUMBER = LS_SERIAL-SERNR.
        ELSE.
          <LFS_ITEM>-SERIAL_NUMBER = |{ <LFS_ITEM>-SERIAL_NUMBER }, { LS_SERIAL-SERNR }|.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDLOOP.

  "Write to footer
  WRITE: LV_TOTAL_QTY     TO GS_PACKING-FOOTER-TOTAL_QTY    DECIMALS 0,
         LV_TOTAL_PACK    TO GS_PACKING-FOOTER-TOTAL_PACK   DECIMALS 0,
         LV_TOTAL_NW      TO GS_PACKING-FOOTER-TOTAL_NW     DECIMALS 2,
         LV_TOTAL_GW      TO GS_PACKING-FOOTER-TOTAL_GW     DECIMALS 2.

  CONDENSE: GS_PACKING-FOOTER-TOTAL_QTY,
            GS_PACKING-FOOTER-TOTAL_PACK,
            GS_PACKING-FOOTER-TOTAL_NW,
            GS_PACKING-FOOTER-TOTAL_GW.




ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_alv_data
*&---------------------------------------------------------------------*
*& Get ALV Data
*&---------------------------------------------------------------------*
FORM GET_ALV_DATA .

  IF S_ZPACK[] IS INITIAL.
    SELECT
      LIPS~VBELN,
      LIPS~POSNR,
      LIPS~MATNR,
      LIPS~ARKTX,
      LIPS~LFIMG,
      LIPS~VRKME,
*      lips~vrkme AS zpack,
      LIPS~NTGEW,
      LIPS~BRGEW,
      LIPS~GEWEI,
      MARA~GROES,
      LIPS~ERDAT
    FROM LIKP
    INNER JOIN LIPS ON LIPS~VBELN = LIKP~VBELN
    INNER JOIN MARA ON LIPS~MATNR = MARA~MATNR
    INTO TABLE @DATA(LT_DELIVERY)
    WHERE LIKP~VBELN IN @S_VBELN[]
      AND LIKP~LFART = @P_LFART.

  ELSE.
    REFRESH S_VBELN[].
    SELECT
        *
      FROM ZSDSSDT017
      INTO TABLE @GT_ZSDSSDT017
      WHERE PACKING_NO IN @S_ZPACK[].
    IF SY-SUBRC = 0.
      SORT GT_ZSDSSDT017 BY VBELN POSNR.
      SELECT
        LIPS~VBELN,
        LIPS~POSNR,
        LIPS~MATNR,
        LIPS~ARKTX,
        LIPS~LFIMG,
        LIPS~VRKME,
*      lips~vrkme AS zpack,
        LIPS~NTGEW,
        LIPS~BRGEW,
        LIPS~GEWEI,
        MARA~GROES,
        LIPS~ERDAT
      FROM LIKP
      INNER JOIN LIPS ON LIPS~VBELN = LIKP~VBELN
      INNER JOIN MARA ON LIPS~MATNR = MARA~MATNR
      INTO TABLE @LT_DELIVERY
      FOR ALL ENTRIES IN @GT_ZSDSSDT017
      WHERE LIKP~VBELN = @GT_ZSDSSDT017-VBELN
        AND LIPS~POSNR = @GT_ZSDSSDT017-POSNR.
    ENDIF.
  ENDIF.


  IF LT_DELIVERY[] IS NOT INITIAL.
    SORT LT_DELIVERY[] BY VBELN POSNR.

*    MOVE-CORRESPONDING lt_delivery[] TO gt_alv[].

    LOOP AT LT_DELIVERY INTO DATA(LS_DELIVERY).

      APPEND INITIAL LINE TO GT_ALV ASSIGNING FIELD-SYMBOL(<LFS_ALV>).
      MOVE-CORRESPONDING LS_DELIVERY TO <LFS_ALV>.

      READ TABLE GT_ZSDSSDT017 INTO DATA(LS_ZSDSSDT017)
                               WITH KEY VBELN = LS_DELIVERY-VBELN
                                        POSNR = LS_DELIVERY-POSNR
                                        BINARY SEARCH.
      IF SY-SUBRC = 0.
        <LFS_ALV>-ZPACK = LS_ZSDSSDT017-ZPACK.
      ELSE.
        <LFS_ALV>-ZPACK = <LFS_ALV>-LFIMG.
      ENDIF.

    ENDLOOP.
  ENDIF.


  SORT GT_ALV BY VBELN POSNR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_display_result
*&---------------------------------------------------------------------*
*& display alv
*&---------------------------------------------------------------------*
FORM F_DISPLAY_RESULT .

  MC_SHOW_PROGRESS 99 TEXT-P99.

* set layout attribute
  PERFORM LAYOUT_BUILD.
* build field catalog
  PERFORM FIELDCAT_BUILD.

  PERFORM ALV_GRID_DISPLAY.



ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_PREPARE_FIELDCAT_O
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FIELDCAT
*&---------------------------------------------------------------------*
FORM F_PREPARE_FIELDCAT_O CHANGING CT_FIELDCAT  TYPE SLIS_T_FIELDCAT_ALV ##CALLED.


  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME         = SY-REPID
      I_INTERNAL_TABNAME     = 'ZSDSSDS057'
*     I_STRUCTURE_NAME       =
*     I_CLIENT_NEVER_DISPLAY = ' '
      I_INCLNAME             = SY-REPID
*     I_BYPASSING_BUFFER     =
*     I_BUFFER_ACTIVE        =
    CHANGING
      CT_FIELDCAT            = CT_FIELDCAT[]
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  LAYOUT_BUILD
*&---------------------------------------------------------------------*
*       set layout attribute
*----------------------------------------------------------------------*
FORM LAYOUT_BUILD .

* Initialize Output
  CLEAR:  GS_LAYOUT, GS_VARIANT, GS_PRINT.

* determine layout
  GS_LAYOUT-COLWIDTH_OPTIMIZE = ABAP_TRUE.
  GS_LAYOUT-ZEBRA             = ABAP_TRUE.

* For Variant Saving
  GS_VARIANT-REPORT   = SY-REPID.
*  gs_print-no_colwopt = abap_true.

  GS_LAYOUT-DETAIL_POPUP           = 'X'.   " display detailed pop-up
  GS_LAYOUT-DETAIL_INITIAL_LINES   = 'X'.   " display initial value

ENDFORM.                    " LAYOUT_BUILD
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_BUILD
*&---------------------------------------------------------------------*
*       build field catalog
*----------------------------------------------------------------------*
FORM FIELDCAT_BUILD.
  FIELD-SYMBOLS :
    <LS_FCAT>   TYPE SLIS_FIELDCAT_ALV.

* build field catalog
  PERFORM ALV_FIELDCAT_BUILD  USING 'ZSDSSDS057'.

* Edit Field catalog
  LOOP AT GT_FIELDCAT ASSIGNING <LS_FCAT>.

* --- non-display
    CASE <LS_FCAT>-FIELDNAME.
      WHEN 'ZZZZZZZZZ'
      OR   'XZZZZZZZZ'
      .
        <LS_FCAT>-NO_OUT  = 'X'.
      WHEN OTHERS.
        <LS_FCAT>-NO_OUT  = ' '.
    ENDCASE.

** --- icon
*    CASE <ls_fcat>-fieldname.
*      WHEN 'ICON_STATUS'.
*        <ls_fcat>-icon    = 'X'.
*    ENDCASE.

* --- column text
    CASE <LS_FCAT>-FIELDNAME.
      WHEN 'ZCHECK'.
        <LS_FCAT>-SELTEXT_L    = TEXT-C01.
        <LS_FCAT>-SELTEXT_M    = TEXT-C01.
        <LS_FCAT>-SELTEXT_S    = TEXT-C01.
        <LS_FCAT>-REPTEXT_DDIC = TEXT-C01.
      WHEN 'ZPACK'.
        <LS_FCAT>-SELTEXT_L    = TEXT-C02.
        <LS_FCAT>-SELTEXT_M    = TEXT-C02.
        <LS_FCAT>-SELTEXT_S    = TEXT-C02.
        <LS_FCAT>-REPTEXT_DDIC = TEXT-C02.
    ENDCASE.

* --- Hot Spot
    CASE <LS_FCAT>-FIELDNAME.
      WHEN 'VBELN_VL'
      OR   'VBELN_VL_OLD'
      OR   'VBELN_VA'
      OR   'VBELN_VF'
      OR   'VBELN_F8'
      OR   'VBELN_IV'
      .
        <LS_FCAT>-HOTSPOT  = 'X'.
    ENDCASE.


    "Editable
    CASE <LS_FCAT>-FIELDNAME.
      WHEN 'ZCHECK'.
        <LS_FCAT>-CHECKBOX    = 'X'.
        <LS_FCAT>-INPUT       = 'X'.
        <LS_FCAT>-EDIT        = 'X'.
      WHEN 'ZPACK'.
        <LS_FCAT>-INPUT       = 'X'.
        <LS_FCAT>-EDIT        = 'X'.
      WHEN OTHERS.
    ENDCASE.

  ENDLOOP.

ENDFORM.                    " FIELDCAT_BUILD
*&---------------------------------------------------------------------*
*&      Form  ALV_FIELDCAT_BUILD
*&---------------------------------------------------------------------*
*       build field catalog
*----------------------------------------------------------------------*
FORM ALV_FIELDCAT_BUILD USING   IV_TABNAME TYPE SLIS_TABNAME.


  DATA : LT_FIELDCAT   TYPE SLIS_T_FIELDCAT_ALV.
  REFRESH : LT_FIELDCAT.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME         = SY-REPID
*     i_internal_tabname     = iv_tabname
      I_STRUCTURE_NAME       = IV_TABNAME
      I_CLIENT_NEVER_DISPLAY = 'X'
      I_INCLNAME             = SY-REPID
      I_BYPASSING_BUFFER     = 'X'
      I_BUFFER_ACTIVE        = ' '
    CHANGING
      CT_FIELDCAT            = LT_FIELDCAT[]
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE 'E' NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    APPEND LINES OF LT_FIELDCAT TO GT_FIELDCAT.
  ENDIF.
ENDFORM.                    " ALV_FIELDCAT_BUILD
*&---------------------------------------------------------------------*
*& Form alv_grid_display
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM ALV_GRID_DISPLAY .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_PF_STATUS_SET = 'PF_STATUS_1'
      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
*     I_GRID_SETTINGS          =
      IS_LAYOUT                = GS_LAYOUT
      IT_FIELDCAT              = GT_FIELDCAT
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS        =
      IT_SORT                  = GT_SORT
*     IT_FILTER                =
*     IS_SEL_HIDE              =
      I_DEFAULT                = 'X'
      I_SAVE                   = 'X'
*     IS_VARIANT               =
*     IT_EVENTS                =
*     IT_EVENT_EXIT            =
*     IS_PRINT                 =
*     IS_REPREP_ID             =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        = 0
*     I_HTML_HEIGHT_END        = 0
*     IT_ALV_GRAPHICS          =
*     IT_HYPERLINK             =
*     IT_ADD_FIELDCAT          =
*     IT_EXCEPT_QINFO          =
*     IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      T_OUTTAB                 = GT_ALV
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_1
*&---------------------------------------------------------------------*
*       Status
*----------------------------------------------------------------------*
FORM PF_STATUS_1 USING US_EXTAB TYPE SLIS_T_EXTAB.

  SET PF-STATUS 'ZSTANDARD'. "EXCLUDING us_extab.

ENDFORM.                    "PF_STATUS_1
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_UCOMM    text
*      -->P_SELFLD   text
*----------------------------------------------------------------------*
FORM USER_COMMAND USING P_UCOMM TYPE SY-UCOMM
                        P_SELFLD TYPE SLIS_SELFIELD.
*&---------------------------------------------------------------------*
*&for Check = 'X' when tick Check Box
*&---------------------------------------------------------------------*
  DATA : REF_GRID TYPE REF TO CL_GUI_ALV_GRID.

  IF REF_GRID IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        E_GRID = REF_GRID.
    CALL METHOD REF_GRID->CHECK_CHANGED_DATA.
  ENDIF.
*&---------------------------------------------------------------------*

  CASE P_UCOMM.
    WHEN '&IC1'.
*      PERFORM f_call_transection.
    WHEN 'SAVE'.
*      PERFORM f_save_data.
    WHEN '&ZALL'.
      PERFORM F_SELECT USING 'X'.
    WHEN '&ZSAL'.
      PERFORM F_SELECT USING SPACE.
    WHEN '&ZPRINT'.
      PERFORM PRINT_PROCESS.
      PERFORM UPDATE_LOG.
  ENDCASE.

  CALL METHOD REF_GRID->REFRESH_TABLE_DISPLAY.
  CLEAR : REF_GRID.

ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  F_SELECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0654   text
*----------------------------------------------------------------------*
FORM F_SELECT USING LV_CHECK.

  GS_ALV-ZCHECK = LV_CHECK.
  MODIFY GT_ALV FROM GS_ALV  TRANSPORTING ZCHECK
                              WHERE ZCHECK NE GS_ALV-ZCHECK.

ENDFORM.                    " F_SELECT
*&---------------------------------------------------------------------*
*& Form print_process
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM PRINT_PROCESS .

  PERFORM GET_FORM_DATA.
  PERFORM PRINT_FORM.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form print_form
*&---------------------------------------------------------------------*
FORM PRINT_FORM .

  DATA: FM_NAME     TYPE RS38L_FNAM,
        LS_JOB_INFO TYPE SSFCRESCL.

  CONSTANTS: LC_FORM_NAME TYPE TDSFNAME VALUE 'ZSDSMM008'.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = LC_FORM_NAME
    IMPORTING
      FM_NAME            = FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    RETURN.
  ENDIF.

  ">set output options (optional)
  DATA(LS_OPTIONS) = VALUE SSFCOMPOP( TDDEST   = GC_TDDEST
                                      TDNEWID  = ABAP_TRUE
                                      TDIMMED  = ABAP_TRUE ).

  DATA(LS_CONTROL) = VALUE SSFCTRLOP( "no_open   = abap_true
                                      "no_close  = abap_true
                                      PREVIEW   = ABAP_TRUE
                                      NO_DIALOG = ABAP_TRUE ).

  CALL FUNCTION FM_NAME
    EXPORTING
      USER_SETTINGS      = SPACE
      OUTPUT_OPTIONS     = LS_OPTIONS
      CONTROL_PARAMETERS = LS_CONTROL
      PACKING_DATA       = GS_PACKING
    IMPORTING
*     document_output_info =
*     job_output_options =
      JOB_OUTPUT_INFO    = LS_JOB_INFO
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.
  IF SY-SUBRC <> 0.
    "Update data to log table
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.


  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form chk_input_field
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CHK_INPUT_FIELD .

  IF R_RPRT IS NOT INITIAL.
    IF S_ZPACK IS INITIAL.
      SET CURSOR FIELD 'S_ZPACK-LOW'.
      MESSAGE E055(00).
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_fn_key
*&---------------------------------------------------------------------*
FORM SET_FN_KEY .


ENDFORM.
*&---------------------------------------------------------------------*
*& Form save_data
*&---------------------------------------------------------------------*
FORM SAVE_DATA .

ENDFORM.
