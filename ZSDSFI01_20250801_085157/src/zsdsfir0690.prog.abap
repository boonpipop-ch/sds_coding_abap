*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0690
*  Creation Date      : 20.02.2025
*  Author             : Boonpipop Ch.
*  Add-on ID          : N/A
*  Description        : Print AR Tax Invoice CN DN
*  Purpose            : Print AR Tax Invoice CN DN
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSFIR0690.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: BKPF, ACDOCA.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE            TYPE  CHAR1     VALUE 'X',
  GC_HEADER_HEIGHT_1 TYPE  I                VALUE 0,
  GC_ALV_HEIGHT_1    TYPE  I                VALUE 100,
  GC_INTFNO          TYPE  ZSDSCAC004-INTFNO VALUE 'FIARI018'.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*
DATA: BEGIN OF GS_BSID ##NEEDED,
        KUNNR    TYPE BSEG-KUNNR,
        NAME1    TYPE KNA1-NAME1,
        GJAHR    TYPE BSEG-GJAHR,
        BELNR    TYPE BSEG-BELNR,
        BUZEI    TYPE BSEG-BUZEI,
        BLART    TYPE BKPF-BLART,
        BKTXT    TYPE BKPF-BKTXT,
        XBLNR    TYPE BKPF-XBLNR,
        BUDAT    TYPE BKPF-BUDAT,
        BLDAT    TYPE BKPF-BLDAT,
        UMSKZ    TYPE BSEG-UMSKZ,
        DMBTR    TYPE BSEG-DMBTR,
        WAERS    TYPE BSEG-H_WAERS, "BSID-WAERS
        MWSKZ    TYPE BSEG-MWSKZ,
        ZTERM    TYPE BSEG-ZTERM,
        ZUONR    TYPE BSEG-ZUONR,
        SGTXT    TYPE BSEG-SGTXT,
        ZFBDT    TYPE BSEG-ZFBDT,
        ZBD1T    TYPE BSEG-ZBD1T,
        ZBD2T    TYPE BSEG-ZBD2T,
        ZBD3T    TYPE BSEG-ZBD3T,
        SHKZG    TYPE BSEG-SHKZG,
        REBZG    TYPE BSEG-REBZG,
        AUGBL    TYPE BSEG-AUGBL,
        OT_ANRED TYPE BSEC-ANRED,
        OT_NAME1 TYPE BSEC-NAME1,
        OT_NAME2 TYPE BSEC-NAME2,
        OT_NAME3 TYPE BSEC-NAME3,
        OT_NAME4 TYPE BSEC-NAME4,
        OT_STCD3 TYPE BSEC-STCD3,
        OT_STRAS TYPE BSEC-STRAS,
        OT_ORT01 TYPE BSEC-ORT01,
        OT_PSTLZ TYPE BSEC-PSTLZ,
        FAEDT    TYPE SY-DATUM, "Net Due Date
        CK_BOX   TYPE XFELD,
        ANRED    TYPE ANRED,

      END OF GS_BSID,
      GT_BSID LIKE TABLE OF GS_BSID ##NEEDED,

      BEGIN OF GS_COMPANY ##NEEDED,
        BUKRS      TYPE T001-BUKRS,
        BUTXT      TYPE T001-BUTXT,
        STCEG      TYPE T001-STCEG,
        ADRNR      TYPE T001-ADRNR,
        DATE_FROM  TYPE ADRC-DATE_FROM,
        NATION     TYPE ADRC-NATION,
        DATE_TO    TYPE ADRC-DATE_TO,
        NAME1      TYPE ADRC-NAME1,
        STREET     TYPE ADRC-STREET,
        CITY2      TYPE ADRC-CITY2,
        CITY1      TYPE ADRC-CITY1,
        POST_CODE1 TYPE ADRC-POST_CODE1,
        TEL_NUMBER TYPE ADRC-TEL_NUMBER,
        FAX_NUMBER TYPE ADRC-FAX_NUMBER,
        LANDX      TYPE T005T-LANDX,
      END OF GS_COMPANY,
      GT_COMPANY LIKE TABLE OF GS_COMPANY ##NEEDED,

      BEGIN OF GS_CUSTOMER ##NEEDED,
        KUNNR      TYPE KNA1-KUNNR,
        STCD3      TYPE KNA1-STCD3,
        ADRNR      TYPE KNA1-ADRNR,
        DATE_FROM  TYPE ADRC-DATE_FROM,
        NATION     TYPE ADRC-NATION,
        DATE_TO    TYPE ADRC-DATE_TO,
        HOUSE_NUM1 TYPE ADRC-HOUSE_NUM1,
        NAME1      TYPE ADRC-NAME1,
        NAME2      TYPE ADRC-NAME2,
        STREET     TYPE ADRC-STREET,
        CITY2      TYPE ADRC-CITY2,
        CITY1      TYPE ADRC-CITY1,
        POST_CODE1 TYPE ADRC-POST_CODE1,
        STR_SUPPL1 TYPE ADRC-STR_SUPPL1,
        STR_SUPPL2 TYPE ADRC-STR_SUPPL2,
        STR_SUPPL3 TYPE ADRC-STR_SUPPL3,
        LOCATION   TYPE ADRC-LOCATION,
        TEL_NUMBER TYPE ADRC-TEL_NUMBER,
        FAX_NUMBER TYPE ADRC-FAX_NUMBER,
      END OF GS_CUSTOMER,
      GT_CUSTOMER LIKE TABLE OF GS_CUSTOMER ##NEEDED,

      BEGIN OF GS_BSEG ##NEEDED,
        BUKRS       TYPE BSEG-BUKRS,
        BELNR       TYPE BSEG-BELNR,
        GJAHR       TYPE BSEG-GJAHR,
        BUZEI       TYPE BSEG-BUZEI,
        SGTXT       TYPE BSEG-SGTXT,
        MENGE       TYPE BSEG-MENGE,
        WRBTR       TYPE BSEG-WRBTR,
        PRCTR       TYPE BSEG-PRCTR,
        HKONT       TYPE BSEG-HKONT,
        SHKZG       TYPE BSEG-SHKZG,
        KUNNR       TYPE BSEG-KUNNR,
        KOART       TYPE BSEG-KOART,
        MWART       TYPE BSEG-MWART,
        UMSKZ       TYPE BSEG-UMSKZ,
        MWSKZ       TYPE BSEG-MWSKZ,
        J_1TPBUPL   TYPE BSEG-J_1TPBUPL,
        AUGBL       TYPE BSEG-AUGBL,
        FWBAS       TYPE BSEG-FWBAS,
        DESCRIPTION TYPE FITHA_PBUPL_D_T-DESCRIPTION,
      END OF GS_BSEG,
      GT_BSEG      LIKE TABLE OF GS_BSEG ##NEEDED,
      GT_BSEG_ACC4 LIKE TABLE OF GS_BSEG ##NEEDED,

      BEGIN OF GS_WITH_ITEM ##NEEDED,
        BUKRS    TYPE WITH_ITEM-BUKRS,
        BELNR    TYPE WITH_ITEM-BELNR,
        GJAHR    TYPE WITH_ITEM-GJAHR,
        BUZEI    TYPE WITH_ITEM-BUZEI,
        WITHT    TYPE WITH_ITEM-WITHT,
        QSATZ    TYPE WITH_ITEM-QSATZ,
        WT_QBSHB TYPE WITH_ITEM-WT_QBSHB,
      END OF GS_WITH_ITEM,
      GT_WITH_ITEM LIKE TABLE OF GS_WITH_ITEM ##NEEDED,

      BEGIN OF GS_BRANCH ##NEEDED,
        BELNR       TYPE BSEG-BELNR,
        GJAHR       TYPE BSEG-GJAHR,
        SPRAS       TYPE FITHA_PBUPL_D_T-SPRAS,
        KUNNR       TYPE FITHA_PBUPL_D_T-KUNNR,
        J_1TPBUPL   TYPE FITHA_PBUPL_D_T-J_1TPBUPL,
        DESCRIPTION TYPE FITHA_PBUPL_D_T-DESCRIPTION,
      END OF GS_BRANCH,
      GT_BRANCH LIKE TABLE OF GS_BRANCH ##NEEDED,

      BEGIN OF GS_GENC ##NEEDED,
        REPID        TYPE ZSDSCAC001-REPID,
        PARAM        TYPE ZSDSCAC001-PARAM,
        PARAM_EXT    TYPE ZSDSCAC001-PARAM_EXT,
        SEQUENCE     TYPE ZSDSCAC001-SEQUENCE,
        PARAM_SIGN   TYPE ZSDSCAC001-PARAM_SIGN,
        PARAM_OPTION TYPE ZSDSCAC001-PARAM_OPTION,
        VALUE_LOW    TYPE ZSDSCAC001-VALUE_LOW,
        VALUE_HIGH   TYPE ZSDSCAC001-VALUE_HIGH,
      END OF GS_GENC,
      GT_GENC LIKE TABLE OF GS_GENC ##NEEDED,

      BEGIN OF GS_DOCTY_RD ##NEEDED,
        BLART TYPE ZSDSSDC026-BLART,
        MWSKZ TYPE ZSDSSDC026-MWSKZ,
        DOCTY TYPE ZSDSSDC026-DOCTY,
      END OF GS_DOCTY_RD,
      GT_DOCTY_RD LIKE TABLE OF GS_DOCTY_RD ##NEEDED.

DATA: GT_T247 TYPE TABLE OF T247 ##NEEDED.

*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*
DATA: GRT_BLART_INV TYPE RANGE OF BKPF-BLART ##NEEDED,
      GRT_BLART_CN  TYPE RANGE OF BKPF-BLART ##NEEDED,
      GRT_BLART_DN  TYPE RANGE OF BKPF-BLART ##NEEDED.
*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* GENC VARIABLES
*----------------------------------------------------------------------*
DATA: GF_ISO TYPE ZSDSV_GEN_C-VALUE_LOW ##NEEDED.

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* MACROS
*----------------------------------------------------------------------*
DEFINE MC_SHOW_PROGRESS.
  IF SY-BATCH IS INITIAL.
*   Show progress online
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        PERCENTAGE = &1 ##NUMBER_OK
        TEXT       = &2.
  ELSE.
*   Show message step in background
    MESSAGE I000(38) WITH &2 SPACE SPACE SPACE.
  ENDIF.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
* Text-s01: Selection Criteria
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
  PARAMETERS: P_BUKRS TYPE BKPF-BUKRS DEFAULT '1000' OBLIGATORY,
              P_GJAHR TYPE BKPF-GJAHR OBLIGATORY,
              P_SPRAS TYPE SPRAS DEFAULT '2' OBLIGATORY.
  SELECT-OPTIONS:
    S_BELNR  FOR  BKPF-BELNR OBLIGATORY,
    S_KUNNR  FOR  ACDOCA-KUNNR,
    S_BUDAT  FOR  BKPF-BUDAT,
    S_XBLNR  FOR  BKPF-XBLNR.
  PARAMETERS: R_INV RADIOBUTTON GROUP G1 DEFAULT 'X',
              R_CN  RADIOBUTTON GROUP G1,
              R_DN  RADIOBUTTON GROUP G1.
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM SET_DEFAULT.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM GET_DATA.
  PERFORM PREPARE_DATA.

*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  IF GT_BSID IS NOT INITIAL.
*   Display Processing Result
    PERFORM F_DISPLAY_RESULT.
  ELSE.
    MESSAGE 'No data' ##NO_TEXT
       TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

*----------------------------------------------------------------------*
* SUBROUTINE INCLUDES
*----------------------------------------------------------------------*
  INCLUDE ZSDSCAI9990 ##INCL_OK.

*----------------------------------------------------------------------*
* SUBROUTINES
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form get_data
*&---------------------------------------------------------------------*
FORM GET_DATA .

  DATA: LRT_BLART    TYPE RANGE OF BLART,
        LS_BSEG_ACC4 LIKE GS_BSEG,
        LS_BLART     LIKE LINE OF LRT_BLART.

  SELECT REPID
         PARAM
         PARAM_EXT
         SEQUENCE
         PARAM_SIGN
         PARAM_OPTION
         VALUE_LOW
         VALUE_HIGH
    FROM ZSDSCAC001
    INTO TABLE GT_GENC
    WHERE REPID = SY-CPROG.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

  LOOP AT GT_GENC ASSIGNING FIELD-SYMBOL(<L_GENC>).
    LS_BLART-SIGN = <L_GENC>-PARAM_SIGN.
    LS_BLART-OPTION = <L_GENC>-PARAM_OPTION.
    LS_BLART-LOW = <L_GENC>-VALUE_LOW.
    LS_BLART-HIGH = <L_GENC>-VALUE_HIGH.
    CASE <L_GENC>-PARAM.
      WHEN 'INV_DOC_TYPE'.
        APPEND LS_BLART TO GRT_BLART_INV.
        IF R_INV IS NOT INITIAL.
          APPEND LS_BLART TO LRT_BLART.
        ENDIF.
      WHEN 'CN_DOC_TYPE'.
        APPEND LS_BLART TO GRT_BLART_CN.
        IF R_CN IS NOT INITIAL.
          APPEND LS_BLART TO LRT_BLART.
        ENDIF.
      WHEN 'DN_DOC_TYPE'.
        APPEND LS_BLART TO GRT_BLART_DN.
        IF R_DN IS NOT INITIAL.
          APPEND LS_BLART TO LRT_BLART.
        ENDIF.
    ENDCASE.

  ENDLOOP.
*  IF R_INV IS NOT INITIAL.
*    LRT_BLART = VALUE #( ( SIGN = 'I'
*                           OPTION = 'EQ'
*                           LOW = 'DR' ) ).
*  ELSE.
*    LRT_BLART = VALUE #( ( SIGN = 'I'
*                           OPTION = 'EQ'
*                           LOW = 'DG' )
*                         ( SIGN = 'I'
*                           OPTION = 'EQ'
*                           LOW = 'DF' ) ).
*  ENDIF.
  IF LRT_BLART[] IS INITIAL.
    MESSAGE 'Please maintain Doc.Type in GENC' TYPE 'E' ##NO_TEXT.
  ENDIF.

  SELECT *
    FROM T247
    INTO TABLE GT_T247
    WHERE SPRAS = P_SPRAS.

  SELECT BLART
         MWSKZ
         DOCTY
    FROM ZSDSSDC026
    INTO TABLE GT_DOCTY_RD
    WHERE BLART IN LRT_BLART.

  SELECT A~KUNNR,
         D~NAME1,
         A~GJAHR,
         A~BELNR,
         A~BUZEI,
         A~BLART,
         B~BKTXT,
         A~XBLNR,
         A~BUDAT,
         A~BLDAT,
         A~UMSKZ,
         A~DMBTR,
         A~WAERS,
         A~MWSKZ,
         A~ZTERM,
         A~ZUONR,
         A~SGTXT,
         A~ZFBDT,
         A~ZBD1T,
         A~ZBD2T,
         A~ZBD3T,
         A~SHKZG,
         A~REBZG,
         A~AUGBL,
         E~ANRED AS OT_ANRED  ,
         E~NAME1 AS OT_NAME1  ,
         E~NAME2 AS OT_NAME2  ,
         E~NAME3 AS OT_NAME3  ,
         E~NAME4 AS OT_NAME4  ,
         E~STCD3 AS OT_STCD3  ,
         E~STRAS AS OT_STRAS  ,
         E~ORT01 AS OT_ORT01  ,
         E~PSTLZ AS OT_PSTLZ
    FROM  BSID_VIEW AS A
    INNER JOIN BKPF AS B
      ON  A~BUKRS = B~BUKRS
      AND A~BELNR = B~BELNR
      AND A~GJAHR = B~GJAHR
    LEFT JOIN KNA1 AS D
      ON A~KUNNR = D~KUNNR
    LEFT JOIN BSEC AS E
       ON  A~BUKRS = E~BUKRS
      AND A~BELNR = E~BELNR
      AND A~GJAHR = E~GJAHR
    WHERE A~BUKRS = @P_BUKRS
      AND A~GJAHR = @P_GJAHR
      AND A~BELNR IN @S_BELNR
      AND A~BLART IN @LRT_BLART
      AND A~KUNNR IN @S_KUNNR
      AND A~BUDAT IN @S_BUDAT
      AND ( A~UMSKZ = '' OR A~UMSKZ = 'S' OR ( A~BLART IN @GRT_BLART_CN AND A~UMSKZ = 'A' ) )
      AND A~MWSKZ <> ''
      AND B~XBLNR IN @S_XBLNR
      AND B~XREVERSAL = ''
    INTO TABLE @GT_BSID  ##TOO_MANY_ITAB_FIELDS.

  SELECT A~KUNNR,
         D~NAME1,
         A~GJAHR,
         A~BELNR,
         A~BUZEI,
         A~BLART,
         B~BKTXT,
         A~XBLNR,
         A~BUDAT,
         A~BLDAT,
         A~UMSKZ,
         A~DMBTR,
         A~WAERS,
         A~MWSKZ,
         A~ZTERM,
         A~ZUONR,
         A~SGTXT,
         A~ZFBDT,
         A~ZBD1T,
         A~ZBD2T,
         A~ZBD3T,
         A~SHKZG,
         A~REBZG,
         A~AUGBL,
         E~ANRED AS OT_ANRED  ,
         E~NAME1 AS OT_NAME1  ,
         E~NAME2 AS OT_NAME2  ,
         E~NAME3 AS OT_NAME3  ,
         E~NAME4 AS OT_NAME4  ,
         E~STCD3 AS OT_STCD3  ,
         E~STRAS AS OT_STRAS  ,
         E~ORT01 AS OT_ORT01  ,
         E~PSTLZ AS OT_PSTLZ
    FROM  BSAD_VIEW AS A
    INNER JOIN BKPF AS B
      ON  A~BUKRS = B~BUKRS
      AND A~BELNR = B~BELNR
      AND A~GJAHR = B~GJAHR
    LEFT JOIN KNA1 AS D
      ON A~KUNNR = D~KUNNR
    LEFT JOIN BSEC AS E
       ON  A~BUKRS = E~BUKRS
      AND A~BELNR = E~BELNR
      AND A~GJAHR = E~GJAHR
    WHERE A~BUKRS = @P_BUKRS
      AND A~GJAHR = @P_GJAHR
      AND A~BELNR IN @S_BELNR
      AND A~BLART IN @LRT_BLART
      AND A~KUNNR IN @S_KUNNR
      AND A~BUDAT IN @S_BUDAT
      AND ( A~UMSKZ = '' OR A~UMSKZ = 'S' OR ( A~BLART IN @GRT_BLART_CN AND A~UMSKZ = 'A' AND A~AUGBL = A~BELNR ) )
*                                          OR ( A~BLART IN @GRT_BLART_CN AND A~UMSKZ = 'S' AND A~AUGBL = A~BELNR ) )
      AND A~MWSKZ <> ''
      AND B~XBLNR IN @S_XBLNR
      AND B~XREVERSAL = ''
    APPENDING TABLE @GT_BSID  ##TOO_MANY_ITAB_FIELDS.

  IF GT_BSID[] IS NOT INITIAL.
    DATA(LT_BSID) = GT_BSID[].
    LOOP AT LT_BSID ASSIGNING FIELD-SYMBOL(<L_BSID>) WHERE UMSKZ = 'S'.
      DELETE GT_BSID WHERE BELNR = <L_BSID>-BELNR
                       AND GJAHR = <L_BSID>-GJAHR
                       AND UMSKZ = ''.
    ENDLOOP.

    SELECT A~BUKRS                                 "#EC CI_NO_TRANSFORM
           A~BELNR                                      "#EC CI_NOORDER
           A~GJAHR
           A~BUZEI
           A~SGTXT
           A~MENGE
           A~WRBTR
           A~PRCTR
           A~HKONT
           A~SHKZG
           A~KUNNR
           A~KOART
           A~MWART
           A~UMSKZ
           A~MWSKZ
           A~J_1TPBUPL
           A~AUGBL
           A~FWBAS
           B~DESCRIPTION
      FROM BSEG AS A
*      INNER JOIN @GT_BSID AS C
*         ON A~GJAHR = C~GJAHR
*        AND A~BELNR = C~BELNR
      LEFT JOIN FITHA_PBUPL_D_T AS B
      ON  A~KUNNR     = B~KUNNR
      AND A~J_1TPBUPL = B~J_1TPBUPL
      INTO TABLE GT_BSEG
      FOR ALL ENTRIES IN GT_BSID
      WHERE A~BUKRS = P_BUKRS
        AND A~GJAHR = GT_BSID-GJAHR
        AND A~BELNR = GT_BSID-BELNR
        AND A~KTOSL = ''.
*        AND A~KOART = 'S'
*        AND A~HKONT LIKE '6%'.
    SORT GT_BSEG BY BUKRS BELNR GJAHR BUZEI.
    LOOP AT GT_BSEG INTO DATA(LS_BSEG) ##INTO_OK.
      IF LS_BSEG-KOART = 'S' AND LS_BSEG-HKONT CP '4*'.
        LS_BSEG_ACC4-BUKRS = LS_BSEG-BUKRS.
        LS_BSEG_ACC4-BELNR = LS_BSEG-BELNR.
        LS_BSEG_ACC4-GJAHR = LS_BSEG-GJAHR.
        LS_BSEG_ACC4-PRCTR = LS_BSEG-PRCTR.
        IF LS_BSEG-SHKZG = 'H'.
          LS_BSEG_ACC4-WRBTR = LS_BSEG-WRBTR * -1.
        ELSE.
          LS_BSEG_ACC4-WRBTR = LS_BSEG-WRBTR.
        ENDIF.
        COLLECT LS_BSEG_ACC4 INTO GT_BSEG_ACC4.
        CLEAR LS_BSEG_ACC4.
*        DELETE GT_BSEG.
      ENDIF.

    ENDLOOP.


    SELECT A~KUNNR                                 "#EC CI_NO_TRANSFORM
           A~STCD3
           A~ADRNR
           B~DATE_FROM
           B~NATION
           B~DATE_TO
           B~HOUSE_NUM1
           B~NAME1
           B~NAME2
           B~STREET
           B~CITY2
           B~CITY1
           B~POST_CODE1
           B~STR_SUPPL1
           B~STR_SUPPL2
           B~STR_SUPPL3
           B~LOCATION
           B~TEL_NUMBER
           B~FAX_NUMBER
      FROM KNA1 AS A
      LEFT JOIN ADRC AS B
      ON A~ADRNR = B~ADDRNUMBER
      INTO TABLE GT_CUSTOMER
      FOR ALL ENTRIES IN GT_BSID
      WHERE KUNNR = GT_BSID-KUNNR.
    SORT GT_CUSTOMER BY DATE_TO DESCENDING.

    SELECT A~BELNR                                 "#EC CI_NO_TRANSFORM
           A~GJAHR                                      "#EC CI_NOORDER
           B~SPRAS
           A~KUNNR
           A~J_1TPBUPL
           B~DESCRIPTION
      FROM BSEG AS A
      INNER JOIN FITHA_PBUPL_D_T AS B
      ON A~KUNNR = B~KUNNR
      AND A~J_1TPBUPL = B~J_1TPBUPL
      INTO TABLE GT_BRANCH
      FOR ALL ENTRIES IN GT_BSID
      WHERE A~BUKRS = P_BUKRS
        AND A~BELNR = GT_BSID-BELNR
        AND A~GJAHR = GT_BSID-GJAHR
        AND A~BUZEI = GT_BSID-BUZEI.
*        AND B~SPRAS = P_SPRAS.

    IF R_CN IS NOT INITIAL OR R_DN IS NOT INITIAL.
      SELECT BUKRS                                 "#EC CI_NO_TRANSFORM
             BELNR
             GJAHR
             BUZEI
             WITHT
             QSATZ
             WT_QBSHB
        FROM WITH_ITEM
        INTO TABLE GT_WITH_ITEM
        FOR ALL ENTRIES IN GT_BSID
        WHERE BUKRS = P_BUKRS
          AND BELNR = GT_BSID-BELNR
          AND GJAHR = GT_BSID-GJAHR
          AND WT_WITHCD <> ''.
    ENDIF.
  ENDIF.

  SELECT A~BUKRS,
         A~BUTXT,
         A~STCEG,
         A~ADRNR,
         B~DATE_FROM,
         B~NATION,
         B~DATE_TO,
         B~NAME1,
         B~STREET,
         B~CITY2,
         B~CITY1,
         B~POST_CODE1,
         B~TEL_NUMBER,
         B~FAX_NUMBER,
         C~LANDX
    FROM T001 AS A
    LEFT JOIN ADRC AS B
    ON A~ADRNR = B~ADDRNUMBER
        LEFT OUTER JOIN T005T AS C ON C~SPRAS = B~LANGU "#EC CI_BUFFJOIN
                                  AND C~LAND1 = B~COUNTRY
    INTO TABLE @GT_COMPANY
    BYPASSING BUFFER
    WHERE A~BUKRS = @P_BUKRS
    ORDER BY DATE_TO DESCENDING.

  SELECT SINGLE VALUE_LOW ##WARN_OK
    FROM ZSDSCAC001
    INTO GF_ISO
    WHERE REPID = 'ZSDSSDR0100'
      AND PARAM = 'ISO_NO'
      AND PARAM_OPTION = 'EQ'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form prepare_data
*&---------------------------------------------------------------------*
FORM PREPARE_DATA .

  LOOP AT GT_BSID ASSIGNING FIELD-SYMBOL(<L_BSID>).
    IF <L_BSID>-BLART = 'DV'.
      IF NOT ( <L_BSID>-MWSKZ = 'O0' OR <L_BSID>-MWSKZ = 'O7' ).
        DELETE GT_BSID.
        CONTINUE.
      ENDIF.
    ENDIF.
    CALL FUNCTION 'NET_DUE_DATE_GET'
      EXPORTING
        I_ZFBDT = <L_BSID>-ZFBDT
        I_ZBD1T = <L_BSID>-ZBD1T
        I_ZBD2T = <L_BSID>-ZBD2T
        I_ZBD3T = <L_BSID>-ZBD3T
        I_SHKZG = <L_BSID>-SHKZG
        I_REBZG = <L_BSID>-REBZG
      IMPORTING
        E_FAEDT = <L_BSID>-FAEDT.
    IF <L_BSID>-BLART IN GRT_BLART_CN.
      LOOP AT GT_BSEG INTO DATA(LS_BSEG) WHERE BELNR = <L_BSID>-BELNR ##INTO_OK
                                           AND GJAHR = <L_BSID>-GJAHR.
        IF ( <L_BSID>-UMSKZ = 'A' "AND <L_BSID>-AUGBL = <L_BSID>-BELNR
          AND LS_BSEG-KOART = 'S' AND LS_BSEG-MWART = 'A' AND LS_BSEG-MWSKZ = 'O7' )
        OR ( <L_BSID>-UMSKZ = 'S' "AND <L_BSID>-AUGBL = <L_BSID>-BELNR
          AND LS_BSEG-KOART = 'S' AND LS_BSEG-MWART = 'A' AND LS_BSEG-MWSKZ = 'O7' ).
          <L_BSID>-MWSKZ = LS_BSEG-MWSKZ.
*          EXIT.
        ENDIF.

        IF LS_BSEG-KOART = 'D' AND LS_BSEG-UMSKZ = ''.
          <L_BSID>-SGTXT = LS_BSEG-SGTXT.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDLOOP.
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_DISPLAY_RESULT
*----------------------------------------------------------------------*
*  Display Processing Result
*----------------------------------------------------------------------*
FORM F_DISPLAY_RESULT.

* Show progress
* Text-p99 : Generating ALV Report . . .
  MC_SHOW_PROGRESS 99 TEXT-P99.

* Set Container name
  GF_CONTAINER_1 = 'CTL_ALV_1'.
* Disable Header area
  GF_ALV_HEADER_1 = SPACE.
* Enable Soft Refresh only in display mode
  GF_SOFT_REFRESH_1 = GC_TRUE.
* No auto refresh in edit mode
*  GF_NO_AUTO_REFRESH_1 = GC_TRUE.

* ALV Layout
  PERFORM F_ALV_LAYOUT CHANGING GS_LAYOUT_1
                                GS_VARIANT_1
                                GS_PRINT_1.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_1.
  GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_1.
  ASSIGN GT_BSID TO <G_LIST_1>.              "#EC CI_FLDEXT_OK[2610650]
* Build Field cat
  PERFORM F_ALV_BUILD_FIELDCAT. "CHANGING GT_FIELDCAT_1.
** Sort data
  PERFORM F_ALV_SORT_RESULT CHANGING GT_SORT_1.
* Call ALV Screen
  CALL SCREEN 9000.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_SORT_RESULT
*----------------------------------------------------------------------*
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
  CS_LAYOUT-SEL_MODE   = 'B'."'A'.
  CS_LAYOUT-CWIDTH_OPT = GC_TRUE.
  CS_LAYOUT-ZEBRA      = SPACE.
  CS_LAYOUT-INFO_FNAME = 'LINECOLOR'.
  CS_LAYOUT-NO_ROWMARK = 'X'.

* For Variant Saving
  CS_VARIANT-REPORT  = SY-REPID.
*  CS_VARIANT-VARIANT = P_VARI.
  CS_PRINT-NO_COLWOPT = GC_TRUE.


ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_BUILD_FIELDCAT
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT. "CHANGING CT_FIELDCAT  TYPE LVC_T_FCAT ##NEEDED..

  PERFORM ADD_FCAT USING 'CK_BOX' TEXT-001 '' ''."'Select' '' ''.
  PERFORM ADD_FCAT USING 'KUNNR' TEXT-002 '' ''."'Customer' 'BSID' 'KUNNR'.
  PERFORM ADD_FCAT USING 'NAME1' TEXT-003 '' ''."'Customer name' 'KNA1' 'NAME1'.
*  PERFORM add_fcat USING 'GJAHR' '' 'BSID' 'GJAHR'.
  PERFORM ADD_FCAT USING 'BELNR' TEXT-004 '' ''."'Document Number' 'BSID' 'BELNR'.
*  PERFORM add_fcat USING 'BUZEI' '' 'BSID' 'BUZEI'.
  PERFORM ADD_FCAT USING 'BLART' TEXT-005 '' ''."'Document Type' 'BSID' 'BLART'.
  PERFORM ADD_FCAT USING 'BKTXT' TEXT-006 '' ''."'Doc Header Text' 'BKPF' 'BKTXT'.
  PERFORM ADD_FCAT USING 'XBLNR' TEXT-007 '' ''."'Reference' 'BSID' 'XBLNR'.
  PERFORM ADD_FCAT USING 'BUDAT' TEXT-008 '' ''."'Posting Date' 'BSID' 'BUDAT'.
  PERFORM ADD_FCAT USING 'BLDAT' TEXT-009 '' ''."'Document Date' 'BSID' 'BLDAT'.
  PERFORM ADD_FCAT USING 'UMSKZ' TEXT-010 '' ''."'Special G/L Ind.' 'BSID' 'UMSKZ'.
  PERFORM ADD_FCAT USING 'DMBTR' TEXT-011 '' ''."'Amount in Local Currency' 'BSID' 'DMBTR'.
  PERFORM ADD_FCAT USING 'WAERS' TEXT-012 '' ''."'Local Currency' 'BSID' 'WAERS'.
  PERFORM ADD_FCAT USING 'MWSKZ' TEXT-013 '' ''."'Tax Code' 'BSID' 'MWSKZ'.
  PERFORM ADD_FCAT USING 'ZTERM' TEXT-014 '' ''."'Terms of Payment' 'BSID' 'ZTERM'.
  PERFORM ADD_FCAT USING 'FAEDT' TEXT-015 '' ''."'Net Due Date' '' ''.
  PERFORM ADD_FCAT USING 'ZUONR' TEXT-016 '' ''."'Assignment' 'BSID' 'ZUONR'.
  PERFORM ADD_FCAT USING 'SGTXT' TEXT-017 '' ''."'Text' 'BSID' 'SGTXT'.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADD_FCAT
*&---------------------------------------------------------------------*
FORM ADD_FCAT  USING UV_FNAME ##PERF_NO_TYPE
                     UV_TEXT
                     UV_REF_TABLE
                     UV_REF_FIELD.
  CLEAR GS_FIELDCAT_1.
  GS_FIELDCAT_1-FIELDNAME = UV_FNAME.
  GS_FIELDCAT_1-REPTEXT   = UV_TEXT.
  GS_FIELDCAT_1-COLTEXT   = UV_TEXT.
  GS_FIELDCAT_1-REF_TABLE = UV_REF_TABLE.
  GS_FIELDCAT_1-REF_FIELD = UV_REF_FIELD.
  IF UV_FNAME = 'CK_BOX'.
    GS_FIELDCAT_1-CHECKBOX = 'X'.
    GS_FIELDCAT_1-EDIT = 'X'.
  ENDIF.
  APPEND GS_FIELDCAT_1 TO GT_FIELDCAT_1.
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_USER_COMMAND_1
*----------------------------------------------------------------------*
*  User Command Processing
*----------------------------------------------------------------------*
FORM F_USER_COMMAND_1 USING UF_UCOMM  TYPE  SY-UCOMM ##CALLED.
  DATA: LS_JOB_INFO TYPE SSFCRESCL,
        LS_JOB_OPT  TYPE SSFCRESOP.

  CASE UF_UCOMM.
    WHEN 'ZALL'.
      PERFORM SELECT_CHECK_BOX USING 'X'.
    WHEN 'ZSAL'.
      PERFORM SELECT_CHECK_BOX USING ''.

    WHEN '&RNT'.
      PERFORM PRINT_FORM USING ''
                         CHANGING LS_JOB_INFO
                                  LS_JOB_OPT .
      IF LS_JOB_INFO-OUTPUTDONE IS NOT INITIAL.
        PERFORM PRINT_FORM USING 'X'
                           CHANGING LS_JOB_INFO
                                    LS_JOB_OPT .
      ENDIF.
  ENDCASE.
  SUPPRESS DIALOG.
ENDFORM.
*----------------------------------------------------------------------*
*  Form F_HANDLE_TOOLBAR_1
*----------------------------------------------------------------------*
*  Event ALV 1 Toolbar
*----------------------------------------------------------------------*
FORM F_HANDLE_TOOLBAR_1 USING UREF_OBJECT TYPE REF TO	CL_ALV_EVENT_TOOLBAR_SET ##CALLED
                              UF_INTERACTIVE TYPE	CHAR01 ##NEEDED.

  DATA:
    LS_BUTTON TYPE STB_BUTTON.


* Add Refresh Button
  CLEAR LS_BUTTON.
  LS_BUTTON-FUNCTION = 'ZALL'.
  LS_BUTTON-ICON     = '@4B@'.
  LS_BUTTON-QUICKINFO = 'Select All' ##NO_TEXT.
  INSERT LS_BUTTON INTO TABLE UREF_OBJECT->MT_TOOLBAR.

  CLEAR LS_BUTTON.
  LS_BUTTON-FUNCTION = 'ZSAL'.
  LS_BUTTON-ICON     = '@4D@'.
  LS_BUTTON-QUICKINFO = 'Deselect All' ##NO_TEXT.
  INSERT LS_BUTTON INTO TABLE UREF_OBJECT->MT_TOOLBAR.

  CLEAR LS_BUTTON.
  LS_BUTTON-FUNCTION = '&RNT'.
  LS_BUTTON-ICON     = '@0X@'.
  LS_BUTTON-QUICKINFO = 'Print' ##NO_TEXT.
  INSERT LS_BUTTON INTO TABLE UREF_OBJECT->MT_TOOLBAR.


  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&CHECK'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&REFRESH'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP01'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&CUT'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&COPY'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&PASTE'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&UNDO'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP02'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&APPEND'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&INSERT_ROW'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&DELETE_ROW'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&LOCAL&COPY_ROW'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&&SEP03'.
  DELETE UREF_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&PRINT_BACK'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form select_check_box
*&---------------------------------------------------------------------*
FORM SELECT_CHECK_BOX  USING UF_X ##PERF_NO_TYPE.
  FIELD-SYMBOLS: <L_BSID> LIKE GS_BSID.
  LOOP AT <G_LIST_1> ASSIGNING <L_BSID> ##GEN_OK.
    <L_BSID>-CK_BOX = UF_X.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form print_form
*&---------------------------------------------------------------------*
FORM PRINT_FORM USING UF_ETAX ##PERF_NO_TYPE
                CHANGING CS_JOB_INFO TYPE SSFCRESCL
                         CS_JOB_OPT     TYPE SSFCRESOP.
  DATA: LS_HEADER   TYPE ZSDSFIS079,
        LS_FOOTER   TYPE ZSDSFIS081,
        LT_ITEM     TYPE TABLE OF ZSDSFIS080,
        LT_DOC      LIKE GT_BSID,
        LS_JOB_INFO TYPE SSFCRESCL.

  DATA: LT_LINE         TYPE STANDARD TABLE OF TLINE,
        LF_BIN_FILESIZE TYPE I ##NEEDED,
        LF_BIN_FILE     TYPE XSTRING.

  DATA: LT_DATABIN  TYPE ZCL_SDSCA_FILE_INTERFACE=>TT_DATABIN,
        LF_FILENAME TYPE STRING,
        LS_RETURN   TYPE BAPIRET2.

  DATA: LF_SPRAS(1),
        LF_TEXT(50) ##NEEDED,
        LF_NO       TYPE I,
        LF_ITEM(1),
        LF_FM_NAME  TYPE RS38L_FNAM,
        LF_FORMNAME TYPE TDSFNAME,
        LS_CONTROL  TYPE SSFCTRLOP,
        LS_OPTION   TYPE SSFCOMPOP,
        LS_WORDS    TYPE SPELL,
        LF_BUDAT    TYPE BKPF-BUDAT,
        LF_DEC(2) ##NEEDED.

  DATA: LS_JOB_OPT     TYPE SSFCRESOP.
*        LS_CONTROL_PDF TYPE SSFCTRLOP,
*        LS_OPTION_PDF  TYPE SSFCOMPOP.

  FIELD-SYMBOLS: <L_BSID> LIKE GS_BSID.

  CASE 'X'.
    WHEN R_INV.
      LF_FORMNAME = 'ZSDSFI007'.
    WHEN R_CN.
      LF_FORMNAME = 'ZSDSFI008'.
    WHEN R_DN.
      LF_FORMNAME = 'ZSDSFI014'.
  ENDCASE.
*  IF R_INV IS NOT INITIAL.
*    LF_FORMNAME = 'ZSDSFI007'.
*  ELSE.
*    LF_FORMNAME = 'ZSDSFI008'.
*  ENDIF.

  IF P_SPRAS = 'E'.
    LF_SPRAS = 'I'.
  ENDIF.
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = LF_FORMNAME
*     VARIANT            = ' '
*     DIRECT_CALL        = ' '
    IMPORTING
      FM_NAME            = LF_FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
  IF SY-SUBRC <> 0 ##NEEDED.
* Implement suitable error handling here
  ENDIF.

  LOOP AT <G_LIST_1> ASSIGNING <L_BSID> ##GEN_OK.
    CHECK <L_BSID>-CK_BOX = 'X'.
    APPEND <L_BSID> TO LT_DOC.
  ENDLOOP.

*  DATA(LT_BSEG_ACC4) = GT_BSEG_ACC4[].
*  SORT LT_BSEG_ACC4 by BUKRS BELNR GJAHR PRCTR.
*  DELETE ADJACENT DUPLICATES FROM LT_BSEG_ACC4 COMPARING BUKRS BELNR GJAHR PRCTR.

  READ TABLE GT_COMPANY INTO DATA(LS_COMPANY_TH) WITH KEY NATION = ''.
  READ TABLE GT_COMPANY INTO DATA(LS_COMPANY_EN) WITH KEY NATION = 'I'.

  LOOP AT LT_DOC ASSIGNING FIELD-SYMBOL(<L_DOC>). "WHERE CK_BOX = 'X'.
    "Call Form ------------------------------------------------------
    LS_CONTROL-NO_OPEN  = ABAP_TRUE.
    LS_CONTROL-NO_CLOSE = ABAP_TRUE.

    AT FIRST.
      LS_CONTROL-NO_OPEN  = ABAP_FALSE.
    ENDAT.

    AT LAST.
      LS_CONTROL-NO_CLOSE = ABAP_FALSE.
    ENDAT.

    IF UF_ETAX IS NOT INITIAL .
      CLEAR: LS_CONTROL-NO_OPEN, LS_CONTROL-NO_CLOSE.
    ENDIF.

    LS_HEADER-COM_NAME_TH = LS_COMPANY_TH-NAME1.
    LS_HEADER-COM_NAME_EN = LS_COMPANY_EN-NAME1.
    CONCATENATE LS_COMPANY_TH-STREET LS_COMPANY_TH-CITY2 LS_COMPANY_TH-CITY1
                LS_COMPANY_TH-POST_CODE1 'TEL.' LS_COMPANY_TH-TEL_NUMBER
                'Cool Line 1271'
*                'FAX' LS_Company_TH-FAX_NUMBER
                INTO LS_HEADER-COM_ADDR_TH SEPARATED BY SPACE ##NO_TEXT.

    CONCATENATE LS_COMPANY_EN-STREET LS_COMPANY_EN-CITY2 LS_COMPANY_EN-CITY1
                LS_COMPANY_EN-POST_CODE1 LS_COMPANY_TH-LANDX
*                'FAX' LS_Company_EN-FAX_NUMBER
                INTO LS_HEADER-COM_ADDR_EN SEPARATED BY SPACE.

    LS_HEADER-TAX_ID = LS_COMPANY_TH-STCEG+2.
    LS_HEADER-COM_REGI = '09-41-05533'.

    IF <L_DOC>-BLART IN GRT_BLART_INV.
      CASE <L_DOC>-MWSKZ.
        WHEN 'O7' OR 'O0'.
          LS_HEADER-DOC_NAME_TH = 'ต้นฉบับ ใบแจ้งหนี้/ใบกำกับภาษี' ##NO_TEXT.
          LS_HEADER-DOC_NAME_EN = 'ORIGINAL INVOICE/TAX INVOICE' ##NO_TEXT.
        WHEN 'OX' OR ''.
          LS_HEADER-DOC_NAME_TH = 'ต้นฉบับ ใบแจ้งหนี้' ##NO_TEXT.
          LS_HEADER-DOC_NAME_EN = 'ORIGINAL INVOICE' ##NO_TEXT.
      ENDCASE.
    ELSEIF <L_DOC>-BLART IN GRT_BLART_CN.
      CASE <L_DOC>-MWSKZ.
        WHEN 'O7' OR 'O0'.
          LS_HEADER-DOC_NAME_TH = 'ใบลดหนี้/ใบกำกับภาษี' ##NO_TEXT.
          LS_HEADER-DOC_NAME_EN = 'CREDIT NOTE/TAX INVOICE' ##NO_TEXT.
        WHEN 'OX' OR ''.
          LS_HEADER-DOC_NAME_TH = 'ใบลดหนี้' ##NO_TEXT.
          LS_HEADER-DOC_NAME_EN = 'CREDIT NOTE' ##NO_TEXT.
      ENDCASE.
      LS_FOOTER-REDU_AMT_TXT = 'ราคาสินค้าที่ลดลง' ##NO_TEXT.
      LS_FOOTER-REDU_AMT_TXT_EN = 'Price Reduction' ##NO_TEXT.
    ELSEIF <L_DOC>-BLART IN GRT_BLART_DN.
      CASE <L_DOC>-MWSKZ.
        WHEN 'O7' OR 'O0'.
          LS_HEADER-DOC_NAME_TH = 'ใบเพิ่มหนี้/ใบกำกับภาษี' ##NO_TEXT.
          LS_HEADER-DOC_NAME_EN = 'DEBIT NOTE/TAX INVOICE' ##NO_TEXT.
        WHEN 'OX' OR ''.
          LS_HEADER-DOC_NAME_TH = 'ใบเพิ่มหนี้' ##NO_TEXT.
          LS_HEADER-DOC_NAME_EN = 'DEBIT NOTE' ##NO_TEXT.
      ENDCASE.
      LS_FOOTER-REDU_AMT_TXT = 'ราคาสินค้าที่เพิ่มขึ้น' ##NO_TEXT.
      LS_FOOTER-REDU_AMT_TXT_EN = 'Price Increase' ##NO_TEXT.
    ENDIF.

*    CASE <L_DOC>-BLART.
*      WHEN 'DR'.
*        CASE <L_DOC>-MWSKZ.
*          WHEN 'O7' OR 'O0'.
*            LS_HEADER-DOC_NAME_TH = 'ต้นฉบับ ใบแจ้งหนี้/ใบกำกับภาษี' ##NO_TEXT.
*            LS_HEADER-DOC_NAME_EN = 'ORIGINAL INVOICE/ TAX INVOICE' ##NO_TEXT.
*          WHEN 'OX' OR ''.
*            LS_HEADER-DOC_NAME_TH = 'ต้นฉบับ ใบแจ้งหนี้' ##NO_TEXT.
*            LS_HEADER-DOC_NAME_EN = 'ORIGINAL INVOICE' ##NO_TEXT.
*        ENDCASE.
*      WHEN 'DG'.
*        CASE <L_DOC>-MWSKZ.
*          WHEN 'O7' OR 'O0'.
*            LS_HEADER-DOC_NAME_TH = 'ใบลดหนี้/ใบกำกับภาษี' ##NO_TEXT.
*            LS_HEADER-DOC_NAME_EN = 'Credit Note / Tax Invoice' ##NO_TEXT.
**          WHEN 'OX' OR 'DS' OR ''.
*          WHEN 'OX' OR ''.
*            LS_HEADER-DOC_NAME_TH = 'ใบลดหนี้' ##NO_TEXT.
*            LS_HEADER-DOC_NAME_EN = 'Credit Note' ##NO_TEXT.
*        ENDCASE.
*
*      WHEN 'DF'.
*        CASE <L_DOC>-MWSKZ.
*          WHEN 'O7' OR 'O0'.
*            LS_HEADER-DOC_NAME_TH = 'ใบเพิ่มหนี้/ใบกำกับภาษี' ##NO_TEXT.
*            LS_HEADER-DOC_NAME_EN = 'Debit Note / Tax Invoice' ##NO_TEXT.
**          WHEN 'OX' OR 'DS' OR ''.
*          WHEN 'OX' OR ''.
*            LS_HEADER-DOC_NAME_TH = 'ใบเพิ่มหนี้' ##NO_TEXT.
*            LS_HEADER-DOC_NAME_EN = 'Debit Note' ##NO_TEXT.
*        ENDCASE.
*
*    ENDCASE.

*-Check one-time or Normal customer
    IF <L_DOC>-KUNNR CP 'OT*' .
*      READ TABLE GT_BSEC INTO DATA(LS_BSEC) WITH KEY BELNR = <L_DOC>-BELNR
*                                                     GJAHR = <L_DOC>-GJAHR
*                                                     BUZEI = <L_DOC>-BUZEI .
*      IF SY-SUBRC = 0 .
      LS_HEADER-CUST_NAME = |{ <L_DOC>-OT_ANRED } { <L_DOC>-OT_NAME1 } { <L_DOC>-OT_NAME2 }|.
      IF <L_DOC>-OT_STCD3 <> '0000000000000'.
        IF <L_DOC>-OT_STCD3(2) = 'TH'.
          LS_HEADER-CUST_TAX_ID = <L_DOC>-OT_STCD3+2.
        ELSE.
          LS_HEADER-CUST_TAX_ID = <L_DOC>-OT_STCD3.
        ENDIF.
      ENDIF.
      LS_HEADER-CUST_ADDR1 =  <L_DOC>-OT_STRAS.
      LS_HEADER-CUST_ADDR2 = |{ <L_DOC>-OT_NAME3 } { <L_DOC>-OT_NAME4 }|.
      LS_HEADER-CUST_ADDR3 = |{ <L_DOC>-OT_ORT01 } { <L_DOC>-OT_PSTLZ }|.
*    ENDIF.
    ELSE.
      READ TABLE GT_CUSTOMER INTO DATA(LS_CUSTOMER) WITH KEY KUNNR = <L_DOC>-KUNNR
                                                             NATION = LF_SPRAS.
      IF SY-SUBRC = 0.
        LS_HEADER-CUST_NAME   = |{ LS_CUSTOMER-NAME1 } { LS_CUSTOMER-NAME2 }|.
        IF LS_CUSTOMER-STCD3 <> '0000000000000'.
          IF LS_CUSTOMER-STCD3(2) = 'TH'.
            LS_HEADER-CUST_TAX_ID = LS_CUSTOMER-STCD3+2.
          ELSE.
            LS_HEADER-CUST_TAX_ID = LS_CUSTOMER-STCD3.
          ENDIF.
        ENDIF.
        LS_HEADER-CUST_ADDR1 = LS_CUSTOMER-STREET.
        LS_HEADER-CUST_ADDR2 = |{ LS_CUSTOMER-STR_SUPPL3 } { LS_CUSTOMER-LOCATION }|.
        LS_HEADER-CUST_ADDR3 = |{ LS_CUSTOMER-STR_SUPPL1 } { LS_CUSTOMER-STR_SUPPL2 } { LS_CUSTOMER-CITY2 }|.
        LS_HEADER-CUST_ADDR4 = |{ LS_CUSTOMER-CITY1 } { LS_CUSTOMER-POST_CODE1 }|.
*      CONCATENATE LS_CUSTOMER-STREET LS_CUSTOMER-STR_SUPPL3 LS_CUSTOMER-LOCATION
*                  LS_CUSTOMER-CITY2 LS_CUSTOMER-CITY1 LS_CUSTOMER-POST_CODE1
*                  INTO LS_HEADER-CUST_ADDR SEPARATED BY ''.
*      CONCATENATE LS_CUSTOMER-HOUSE_NUM1 LS_CUSTOMER-STREET LS_CUSTOMER-CITY2 LS_CUSTOMER-CITY1
*                  LS_CUSTOMER-POST_CODE1 'TEL' LS_CUSTOMER-TEL_NUMBER
*                  'FAX' LS_CUSTOMER-FAX_NUMBER
*                  INTO LS_HEADER-CUST_ADDR SEPARATED BY ''.

      ENDIF.
    ENDIF .
    CONDENSE: LS_HEADER-CUST_ADDR1, LS_HEADER-CUST_ADDR2, LS_HEADER-CUST_ADDR3, LS_HEADER-CUST_ADDR4.

    IF <L_DOC>-XBLNR IS NOT INITIAL.
      LS_HEADER-DOC_NO = <L_DOC>-XBLNR.
    ELSE.
      LS_HEADER-DOC_NO = <L_DOC>-BELNR.
    ENDIF.
*    WRITE <L_DOC>-BUDAT TO LS_HEADER-DOC_DATE DD/MM/YYYY.
    READ TABLE GT_T247 ASSIGNING FIELD-SYMBOL(<L_T247>) WITH KEY MNR = <L_DOC>-BUDAT+4(2).
    IF SY-SUBRC = 0.
      LS_HEADER-DOC_DATE = |{ <L_DOC>-BUDAT+6(2) }-{ <L_T247>-KTX }-{ <L_DOC>-BUDAT(4) }|.
    ENDIF.
    LS_HEADER-CUST_NO = |{ <L_DOC>-KUNNR ALPHA = OUT }|."<L_DOC>-KUNNR.
*    WRITE <L_DOC>-FAEDT TO LS_HEADER-DUE_DATE DD/MM/YYYY.
    READ TABLE GT_T247 ASSIGNING <L_T247> WITH KEY MNR = <L_DOC>-FAEDT+4(2).
    IF SY-SUBRC = 0.
      LS_HEADER-DUE_DATE = |{ <L_DOC>-FAEDT+6(2) }-{ <L_T247>-KTX }-{ <L_DOC>-FAEDT(4) }|.
    ENDIF.
    LS_HEADER-XBLNR = <L_DOC>-XBLNR.
    LS_HEADER-KUNNR = <L_DOC>-KUNNR.
    LS_HEADER-FI_DOC = <L_DOC>-BELNR.

    READ TABLE GT_BRANCH ASSIGNING FIELD-SYMBOL(<L_BRANCH>) WITH KEY BELNR = <L_DOC>-BELNR
                                                                     GJAHR = <L_DOC>-GJAHR.
    IF SY-SUBRC = 0.
      IF <L_BRANCH>-J_1TPBUPL = '00000' OR <L_BRANCH>-J_1TPBUPL = 'NVAT'.
        SPLIT <L_BRANCH>-DESCRIPTION AT '/' INTO DATA(L_EN) DATA(L_TH).
        CASE P_SPRAS.
          WHEN 'E'.
            LS_HEADER-BRANCH = L_EN.
          WHEN '2'.
            LS_HEADER-BRANCH = L_TH.
          WHEN OTHERS.
        ENDCASE.
      ELSE.
        LS_HEADER-BRANCH = <L_Branch>-DESCRIPTION.
      ENDIF.
*      LS_HEADER-BRANCH = <L_Branch>-DESCRIPTION.

      IF LS_HEADER-CUST_TAX_ID = '0000000000000' OR
         LS_HEADER-CUST_TAX_ID IS INITIAL.
        CLEAR: LS_HEADER-BRANCH ,
               LS_HEADER-CUST_TAX_ID .
      ELSE.
        IF <L_BRANCH>-J_1TPBUPL = 'NVAT' .
          CLEAR LS_HEADER-BRANCH .
        ELSE.
          IF  <L_BRANCH>-J_1TPBUPL <> '00000'.

            CASE P_SPRAS.
              WHEN 'E'.
                "Text-T02 :  Branch no.
*                CONCATENATE LS_HEADER-BRANCH TEXT-T02 <L_BRANCH>-J_1TPBUPL
                CONCATENATE TEXT-T02 <L_BRANCH>-J_1TPBUPL
                       INTO LS_HEADER-BRANCH SEPARATED BY SPACE.
              WHEN '2'.
                "Text-T01: สาขาที่  :
*                CONCATENATE LS_HEADER-BRANCH TEXT-T01 <L_BRANCH>-J_1TPBUPL
                CONCATENATE TEXT-T01 <L_BRANCH>-J_1TPBUPL
                       INTO LS_HEADER-BRANCH SEPARATED BY SPACE.
              WHEN OTHERS.
            ENDCASE.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

    IF LS_HEADER-CUST_TAX_ID = '0000000000000' OR
       LS_HEADER-CUST_TAX_ID IS INITIAL.
      CLEAR: LS_HEADER-CUST_TAX_ID .
    ELSE.
      "Set Tax id no.
      CASE P_SPRAS.
        WHEN 'E'.
          "Text-T03 : TAX ID No.
          CONCATENATE TEXT-T03  LS_HEADER-CUST_TAX_ID
                 INTO LS_HEADER-CUST_TAX_ID  SEPARATED BY SPACE.

        WHEN '2'.
          "Text-t04: เลขประจำตัวผู้เสียภาษี
          CONCATENATE TEXT-T04  LS_HEADER-CUST_TAX_ID
                 INTO LS_HEADER-CUST_TAX_ID SEPARATED BY SPACE  .
        WHEN OTHERS.
      ENDCASE.
    ENDIF.

    LOOP AT GT_BSEG INTO DATA(LS_BSEG) WHERE BELNR = <L_DOC>-BELNR ##INTO_OK
                                         AND GJAHR = <L_DOC>-GJAHR.
      CLEAR: LF_ITEM.
      IF LS_BSEG-KOART = 'S' AND LS_BSEG-HKONT CP '6*'.
        LF_ITEM = 'X'.
      ELSEIF LS_BSEG-UMSKZ <> '' AND LS_BSEG-KOART = 'D' AND LS_BSEG-HKONT CP '2*' AND LS_BSEG-MWSKZ = 'O7'.
        LF_ITEM = 'X'.
      ELSEIF LS_BSEG-KOART = 'S' AND LS_BSEG-SHKZG = 'H' AND LS_BSEG-HKONT CP '1*'.
        LF_ITEM = 'X'.
      ELSE.
*        IF <L_DOC>-BLART = 'DR' OR <L_DOC>-BLART = 'DG'.
        IF <L_DOC>-BLART IN GRT_BLART_INV OR <L_DOC>-BLART IN GRT_BLART_CN.
          IF LS_BSEG-UMSKZ = '' AND LS_BSEG-KOART = 'S' AND LS_BSEG-HKONT CP '2*' AND LS_BSEG-MWART <> 'A'.
            LF_ITEM = 'X'.
*          ELSEIF LS_BSEG-KOART = 'S' AND LS_BSEG-HKONT CP '4*'.
*            LF_ITEM = 'X'.
          ELSEIF LS_BSEG-UMSKZ <> '' AND LS_BSEG-KOART = 'D' AND LS_BSEG-HKONT CP '1*' AND LS_BSEG-MWSKZ = 'O7'.
            LF_ITEM = 'X'.
          ELSEIF ( <L_DOC>-BLART IN GRT_BLART_CN AND <L_DOC>-UMSKZ = 'A' "AND <L_DOC>-AUGBL = <L_DOC>-BELNR
                 AND LS_BSEG-KOART = 'S' AND LS_BSEG-MWART = 'A' AND LS_BSEG-MWSKZ = 'O7' )
              OR ( <L_DOC>-BLART IN GRT_BLART_CN AND <L_DOC>-UMSKZ = 'S' "AND <L_DOC>-AUGBL = <L_DOC>-BELNR
                 AND LS_BSEG-KOART = 'S' AND LS_BSEG-MWART = 'A' AND LS_BSEG-MWSKZ = 'O7' ) .
            LF_ITEM = 'M'.
          ENDIF.
        ENDIF.
      ENDIF.

      CHECK LF_ITEM IS NOT INITIAL."= 'X'.

      APPEND INITIAL LINE TO LT_ITEM ASSIGNING FIELD-SYMBOL(<L_ITEM>).

      LF_NO = LF_NO + 1.
      <L_ITEM>-ITEM_NO = LF_NO.
      <L_ITEM>-SGTXT = LS_BSEG-SGTXT.

      IF LF_ITEM = 'M'.
        LS_BSEG-WRBTR = LS_BSEG-FWBAS.
      ENDIF.

      IF LS_BSEG-MENGE IS INITIAL.
        <L_ITEM>-MENGE = '1'.
        <L_ITEM>-UNIT_PRICE = LS_BSEG-WRBTR.
      ELSE.
        <L_ITEM>-MENGE = LS_BSEG-MENGE.
        <L_ITEM>-UNIT_PRICE = LS_BSEG-WRBTR / LS_BSEG-MENGE.
      ENDIF.

      <L_ITEM>-NET_PRICE = <L_ITEM>-UNIT_PRICE.
      <L_ITEM>-AMOUNT = LS_BSEG-WRBTR.
      IF LS_FOOTER-PRCTR IS INITIAL. "AND LS_BSEG-HKONT CP '6*'.
        LS_FOOTER-PRCTR = LS_BSEG-PRCTR.
      ENDIF.

      LS_FOOTER-TOTAL = LS_FOOTER-TOTAL + LS_BSEG-WRBTR.
    ENDLOOP.

*    IF <L_DOC>-BLART = 'DR' OR <L_DOC>-BLART = 'DG'.
    IF <L_DOC>-BLART IN GRT_BLART_INV OR <L_DOC>-BLART IN GRT_BLART_CN.
      LOOP AT GT_BSEG_ACC4 INTO DATA(LS_BSEG_ACC4) WHERE BELNR = <L_DOC>-BELNR ##INTO_OK
                                                     AND GJAHR = <L_DOC>-GJAHR.
        IF LS_BSEG_ACC4-WRBTR IS NOT INITIAL.
          APPEND INITIAL LINE TO LT_ITEM ASSIGNING <L_ITEM>.
          LF_NO = LF_NO + 1.
          <L_ITEM>-ITEM_NO = LF_NO.
          LOOP AT GT_BSEG INTO LS_BSEG WHERE BUKRS = LS_BSEG_ACC4-BUKRS ##INTO_OK
                                         AND BELNR = LS_BSEG_ACC4-BELNR
                                         AND GJAHR = LS_BSEG_ACC4-GJAHR
                                         AND PRCTR = LS_BSEG_ACC4-PRCTR
                                         AND KOART = 'S'
                                         AND HKONT CP '4*'.
            IF <L_ITEM>-AMOUNT IS INITIAL OR <L_ITEM>-AMOUNT < LS_BSEG-WRBTR.
              <L_ITEM>-SGTXT = LS_BSEG-SGTXT.
              IF LS_BSEG-MENGE IS INITIAL.
                <L_ITEM>-MENGE = '1'.
                <L_ITEM>-UNIT_PRICE = LS_BSEG-WRBTR.
              ELSE.
                <L_ITEM>-MENGE = LS_BSEG-MENGE.
                <L_ITEM>-UNIT_PRICE = LS_BSEG-WRBTR / LS_BSEG-MENGE.
              ENDIF.
              <L_ITEM>-NET_PRICE = <L_ITEM>-UNIT_PRICE.
              IF <L_ITEM>-AMOUNT IS INITIAL.
                LS_FOOTER-TOTAL = LS_FOOTER-TOTAL + LS_BSEG-WRBTR.
              ELSE.
                LS_FOOTER-TOTAL = LS_FOOTER-TOTAL - <L_ITEM>-AMOUNT + LS_BSEG-WRBTR.
              ENDIF.
              <L_ITEM>-AMOUNT = LS_BSEG-WRBTR.
            ENDIF.
          ENDLOOP.
        ENDIF.
        IF LS_FOOTER-PRCTR IS INITIAL.
          LS_FOOTER-PRCTR = LS_BSEG-PRCTR.
        ENDIF.
      ENDLOOP.
    ENDIF.

    <L_DOC>-SGTXT = <L_DOC>-SGTXT+1.
    SPLIT <L_DOC>-SGTXT AT '|' INTO: LS_HEADER-INV_NO LS_FOOTER-TAX_AMT
                                     LS_FOOTER-TOT_AMT LS_FOOTER-REDU_AMT LF_TEXT.

    IF R_CN IS NOT INITIAL OR R_DN IS NOT INITIAL.
      SELECT SINGLE BUDAT
        INTO LF_BUDAT
        FROM BKPF
        WHERE BELNR = LS_HEADER-INV_NO
          AND BUKRS = P_BUKRS
          AND GJAHR = <L_DOC>-GJAHR.
      IF SY-SUBRC = 0.
*        WRITE LF_BUDAT TO LS_HEADER-INV_DATE DD/MM/YYYY.
        READ TABLE GT_T247 ASSIGNING <L_T247> WITH KEY MNR = LF_BUDAT+4(2).
        IF SY-SUBRC = 0.
          LS_HEADER-INV_DATE = |{ LF_BUDAT+6(2) }-{ <L_T247>-KTX }-{ LF_BUDAT(4) }|.
        ENDIF.
      ENDIF.
      CLEAR LF_BUDAT.
    ENDIF.


    LS_FOOTER-TOTAL_AMT = LS_FOOTER-TOTAL.
    IF <L_DOC>-MWSKZ = 'O7'.
      LS_FOOTER-TAX = LS_FOOTER-TOTAL * ( 7 / 100 ).
    ENDIF.
    LS_FOOTER-NET_TOTAL = LS_FOOTER-TOTAL + LS_FOOTER-TAX.

    CONCATENATE <L_DOC>-XBLNR <L_DOC>-KUNNR INTO LS_FOOTER-QR_CODE.
    CONCATENATE LS_COMPANY_TH-STCEG+2 <L_DOC>-XBLNR <L_DOC>-KUNNR INTO LS_FOOTER-BARCODE.
    IF <L_DOC>-MWSKZ = 'O7' OR <L_DOC>-MWSKZ = 'O0'.
      LS_FOOTER-ISO_NO = GF_ISO.
    ENDIF.

    IF GT_WITH_ITEM[] IS NOT INITIAL.
      READ TABLE GT_WITH_ITEM INTO DATA(LS_WITH_ITEM) WITH KEY BELNR = <L_DOC>-BELNR
                                                               GJAHR = <L_DOC>-GJAHR.
      IF SY-SUBRC = 0.
        LS_FOOTER-QSATZ = LS_WITH_ITEM-QSATZ.
        LS_FOOTER-WT_QBSHB = LS_WITH_ITEM-WT_QBSHB.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'SPELL_AMOUNT'
      EXPORTING
        AMOUNT    = LS_FOOTER-NET_TOTAL
        CURRENCY  = 'THB'
        LANGUAGE  = P_SPRAS
      IMPORTING
        IN_WORDS  = LS_WORDS
      EXCEPTIONS
        NOT_FOUND = 1
        TOO_LARGE = 2
        OTHERS    = 3.
    IF SY-SUBRC = 0.
      LS_FOOTER-GRAND_TOTAL = LS_WORDS-WORD.

*      IF P_SPRAS = 'E'.
*        CONCATENATE LS_FOOTER-GRAND_TOTAL 'Baht' ##NO_TEXT
*                    INTO LS_FOOTER-GRAND_TOTAL SEPARATED BY ''.
*      ELSE.
*        IF LS_WORDS-DECIMAL IS NOT INITIAL.
*          CONCATENATE LS_FOOTER-GRAND_TOTAL 'บาท' ##NO_TEXT
*                      INTO LS_FOOTER-GRAND_TOTAL.
*        ELSE.
*          CONCATENATE LS_FOOTER-GRAND_TOTAL 'บาทถ้วน' ##NO_TEXT
*                      INTO LS_FOOTER-GRAND_TOTAL.
*        ENDIF.
*      ENDIF.
*
*      IF LS_WORDS-DECIMAL IS NOT INITIAL.
*        LF_DEC = LS_WORDS-DECIMAL.
*        LS_FOOTER-GRAND_TOTAL = |{ LS_FOOTER-GRAND_TOTAL } { LF_DEC }/100|.
*      ENDIF.

      IF LS_WORDS-DECIMAL IS NOT INITIAL.
        IF P_SPRAS = 'E'.
          CONCATENATE LS_FOOTER-GRAND_TOTAL 'Baht and' ##NO_TEXT
                      LS_WORDS-DECWORD 'Satang' ##NO_TEXT
                      INTO LS_FOOTER-GRAND_TOTAL SEPARATED BY ''.
        ELSE.
          CONCATENATE LS_FOOTER-GRAND_TOTAL 'บาท'  ##NO_TEXT
                      LS_WORDS-DECWORD 'สตางค์'  ##NO_TEXT
                      INTO LS_FOOTER-GRAND_TOTAL.
        ENDIF.

      ELSE.
        IF P_SPRAS = 'E'.
          CONCATENATE LS_FOOTER-GRAND_TOTAL 'Baht only' ##NO_TEXT
                      INTO LS_FOOTER-GRAND_TOTAL SEPARATED BY ''.
        ELSE.
          CONCATENATE LS_FOOTER-GRAND_TOTAL 'บาทถ้วน' ##NO_TEXT
                      INTO LS_FOOTER-GRAND_TOTAL.
        ENDIF.

      ENDIF.
    ENDIF.

    IF UF_ETAX IS NOT INITIAL.
      MOVE-CORRESPONDING CS_JOB_OPT TO LS_OPTION.
      LS_CONTROL-GETOTF = 'X'.
      LS_CONTROL-NO_DIALOG = 'X'.
      LS_CONTROL-PREVIEW = SPACE.
    ENDIF.

    CALL FUNCTION LF_FM_NAME
      EXPORTING
        CONTROL_PARAMETERS = LS_CONTROL
        OUTPUT_OPTIONS     = LS_OPTION
        HEADER             = LS_HEADER
        FOOTER             = LS_FOOTER
      IMPORTING
*       DOCUMENT_OUTPUT_INFO       = LS_DOC_INFO
        JOB_OUTPUT_INFO    = LS_JOB_INFO
        JOB_OUTPUT_OPTIONS = LS_JOB_OPT
      TABLES
        ITEM               = LT_ITEM
      EXCEPTIONS
        FORMATTING_ERROR   = 1
        INTERNAL_ERROR     = 2
        SEND_ERROR         = 3
        USER_CANCELED      = 4
        OTHERS             = 5.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'E'.
      EXIT.
    ELSEIF UF_ETAX IS NOT INITIAL.
*      MOVE-CORRESPONDING LS_JOB_OPT TO LS_OPTION_PDF.
*      LS_CONTROL_PDF-GETOTF = 'X'.
*      LS_CONTROL_PDF-NO_DIALOG = 'X'.
*      LS_CONTROL_PDF-PREVIEW = SPACE.
*
*      CALL FUNCTION LF_FM_NAME
*        EXPORTING
*          CONTROL_PARAMETERS = LS_CONTROL_PDF
*          OUTPUT_OPTIONS     = LS_OPTION_PDF
*          HEADER             = LS_HEADER
*          FOOTER             = LS_FOOTER
*        IMPORTING
*          JOB_OUTPUT_INFO    = LS_JOB_INFO
*        TABLES
*          ITEM               = LT_ITEM
*        EXCEPTIONS
*          FORMATTING_ERROR   = 1
*          INTERNAL_ERROR     = 2
*          SEND_ERROR         = 3
*          USER_CANCELED      = 4
*          OTHERS             = 5.
*      IF SY-SUBRC <> 0.
*        MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
*          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'E'.
*        EXIT.
*      ENDIF.

      "PDF E-Tax
      CALL FUNCTION 'CONVERT_OTF'
        EXPORTING
          FORMAT                = 'PDF'
        IMPORTING
          BIN_FILESIZE          = LF_BIN_FILESIZE
          BIN_FILE              = LF_BIN_FILE
        TABLES
          OTF                   = LS_JOB_INFO-OTFDATA
          LINES                 = LT_LINE
        EXCEPTIONS
          ERR_MAX_LINEWIDTH     = 1
          ERR_FORMAT            = 2
          ERR_CONV_NOT_POSSIBLE = 3
          ERR_BAD_OTF           = 4
          OTHERS                = 5.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'E'.
        EXIT.
      ELSEIF LF_BIN_FILE IS NOT INITIAL.
*        IF <L_DOC>-BLART = 'DR' AND <L_DOC>-MWSKZ = 'OX'.
*          LF_FILENAME = 'B380'.
*        ELSEIF <L_DOC>-BLART = 'DF' AND <L_DOC>-MWSKZ = 'OX'.
*          LF_FILENAME = 'B80'.
*        ELSEIF <L_DOC>-BLART = 'DG' AND <L_DOC>-MWSKZ = 'OX'.
*          LF_FILENAME = 'B81'.
*        ELSE.
*          LF_FILENAME = 'B388'.
*        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
          EXPORTING
            INPUT  = P_SPRAS
          IMPORTING
            OUTPUT = LF_FILENAME.

        READ TABLE GT_DOCTY_RD ASSIGNING FIELD-SYMBOL(<L_DOCTY_RD>) WITH KEY BLART = <L_DOC>-BLART
                                                                             MWSKZ = <L_DOC>-MWSKZ.
        IF SY-SUBRC = 0.
          LF_FILENAME = |B{ <L_DOCTY_RD>-DOCTY }_{ LF_FILENAME }|.
        ELSE.
          LF_FILENAME = |BXXX_{ LF_FILENAME }|.
        ENDIF.



        LF_FILENAME = |{ LF_FILENAME }_{ SY-DATUM }_{ SY-UZEIT }_{ P_BUKRS }{ LS_HEADER-DOC_NO }{ <L_DOC>-GJAHR }.PDF| ##NO_TEXT.

        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            BUFFER     = LF_BIN_FILE
          TABLES
            BINARY_TAB = LT_DATABIN.

        CALL METHOD ZCL_SDSCA_FILE_INTERFACE=>CREATE_INTERFACE_FILE
          EXPORTING
            IF_INTFNO   = GC_INTFNO
            IF_FILENAME = LF_FILENAME
            IT_DATABIN  = LT_DATABIN
          IMPORTING
            ES_RETURN   = LS_RETURN.

        IF LS_RETURN-TYPE NE 'S'.
*         Error
          MESSAGE ID LS_RETURN-ID TYPE 'I'
                  NUMBER LS_RETURN-NUMBER
                  DISPLAY LIKE LS_RETURN-TYPE
                  WITH LS_RETURN-MESSAGE_V1 LS_RETURN-MESSAGE_V2
                       LS_RETURN-MESSAGE_V3 LS_RETURN-MESSAGE_V4.
          EXIT.
        ENDIF.

      ENDIF.
    ENDIF.
    CS_JOB_INFO = LS_JOB_INFO.
    IF CS_JOB_OPT IS INITIAL.
      CS_JOB_OPT  = LS_JOB_OPT.
      CLEAR: CS_JOB_OPT-TDPREVIEW.
    ENDIF.
    CLEAR : LS_HEADER, LS_FOOTER, LT_ITEM[], LF_NO, LS_WORDS,
            LS_JOB_INFO, LF_BIN_FILESIZE, LF_BIN_FILE, LT_LINE[],
            LF_FILENAME, LT_DATABIN[], LS_RETURN, LS_JOB_OPT, LS_JOB_INFO.
*            LS_CONTROL_PDF, LS_OPTION_PDF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_default
*&---------------------------------------------------------------------*
FORM SET_DEFAULT .
  CALL FUNCTION 'GET_CURRENT_YEAR'
    EXPORTING
      BUKRS = P_BUKRS
    IMPORTING
      CURRY = P_GJAHR.
ENDFORM.
