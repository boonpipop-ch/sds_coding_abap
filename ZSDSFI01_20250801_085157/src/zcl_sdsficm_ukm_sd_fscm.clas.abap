class ZCL_SDSFICM_UKM_SD_FSCM definition
  public
  final
  create public .

public section.

  class-methods CHECK_ACTIVE_NORMAL_ORDER_TYPE
    importing
      !IF_AUART type VBAK-AUART
    returning
      value(RF_ACTIVE) type FLAG .
  class-methods CHECK_ACTIVE_WBS_ORDER_TYPE
    importing
      !IF_AUART type VBAK-AUART
    returning
      value(RF_ACTIVE) type FLAG .
  class-methods IGNORE_CREDIT_CHECK
    importing
      !IF_VKORG type VBAK-AUART
      !IF_VBTYP type VBAK-VBTYP
      !IF_ZTERM type VBKD-ZTERM
    returning
      value(RF_IGNORE) type FLAG .
  class-methods CREDIT_CHECK_NORMAL_ORDER
    importing
      !IS_XVBAK type VBAK
      !IT_XVBAP type VA_VBAPVB_T
      !IF_TRTYP type TRTYP optional
    changing
      !CT_MESSAGE type UKM_T_PI_CREDIT_MESSAGE optional
      !CF_FSCM_TE_CHECK type SY-SUBRC optional
      !CF_FSCM_SET type T691F-USR2SET optional
      !CF_DATA_VALUE type ZSDSFIS133 optional .
  class-methods CREDIT_CHECK_WBS_ORDER
    importing
      !IF_TRTYP type TRTYP optional
      !IS_XVBAK type VBAK
      !IT_XVBAP type VA_VBAPVB_T
    changing
      !CT_MESSAGE type UKM_T_PI_CREDIT_MESSAGE optional
      !CF_FSCM_TE_CHECK type SY-SUBRC optional
      !CF_FSCM_SET type T691F-USR2SET optional
      !CF_DATA_VALUE type ZSDSFIS133 optional .
  class-methods CREDIT_GET_VALUE_NORMAL_ORDER
    importing
      !IS_XVBAK type VBAK
      !IT_XVBAP type VA_VBAPVB_T
    changing
      !CF_FSCM_TE_CHECK type SY-SUBRC optional
      !CF_DATA_VALUE type ZSDSFIS133 optional .
  class-methods CREDIT_SET_MESSAGE_SHOW_ERROR
    changing
      !CT_FSCM_CREDIT_MESSAGES type UKM_T_PI_CREDIT_MESSAGE .
  PROTECTED SECTION.
private section.

  constants GC_REPID type SY-REPID value 'ZCL_SDSFICM_UKM_SD_FSCM' ##NO_TEXT.
  class-data GR_AUART_NORMAL type TMS_T_AUART_RANGE .
  class-data GR_AUART_WBS type TMS_T_AUART_RANGE .
  class-data GR_VKORG_NO_CHECK_CREDIT type RANGE_T_VKORG .
  class-data GR_ZTERM_NO_CHECK_CREDIT type PFM_T_RANGE_ZTERM .
  class-data GR_VBTYP_NO_CHECK_CREDIT type SACO_VBTYP_RANGES_TAB .
  class-data GR_OVDUE_BLART type BKK_R_BLART .
  class-data GR_OVDUE_UMSKZ type FIAPPT_T_GL_RANGE .
  class-data GR_OVDUE_ZTERM type PFM_T_RANGE_ZTERM .

  class-methods GET_GENC .
  class-methods CHECK_COPY_DOC_NOT_LAST_VBAP
    returning
      value(RF_NOT_LAST) type FLAG .
ENDCLASS.



CLASS ZCL_SDSFICM_UKM_SD_FSCM IMPLEMENTATION.


  METHOD CHECK_ACTIVE_WBS_ORDER_TYPE.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSFICM_UKM_SD_FSCM/ CHECK_ACTIVE_WBS_ORDER_TYPE
*  Creation Date      : 20.08.2024
*  Author             : Boontip R. (Eviden)
*  Add-on ID          : FSCME035
*  Description        : check order type whether it is wbs order type or not
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
    CLEAR RF_ACTIVE.

    IF GR_AUART_WBS IS NOT INITIAL
    AND IF_AUART IN GR_AUART_WBS.
      RF_ACTIVE = ABAP_TRUE.
    ENDIF.

  ENDMETHOD.


  METHOD CREDIT_CHECK_WBS_ORDER.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSFICM_UKM_SD_FSCM/ CREDIT_CHECK_WBS_ORDER
*  Creation Date      : 20.08.2024
*  Author             : Boontip R. (Eviden)
*  Add-on ID          : FSCME035
*  Description        : check credit limit for wbs order type
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  22/01/2025  F36K911483  Boontip R.  CH01 IMS#420000174
*  add checking wbs status
*-----------------------------------------------------------------------
*  24/06/2025  F36K919976  Boontip R.  CH02 IMS#420000664
*  Change the overdue checking logic by checking at the customer level only
*-----------------------------------------------------------------------

    DATA: LRT_BP           TYPE RANGE OF ZSDSFIT036-KUNNR,
          LRT_WBS          TYPE RANGE OF ZSDSFIT036-PSPHI,
          LF_CREDIT_EXCEED TYPE ZSDSFIT036-CREDIT_LIMIT,
          LF_OVDUE_AMOUNT  TYPE BSID_VIEW-DMBTR,
          LF_NETDUE        TYPE SY-DATUM,
          LS_MESSAGE       LIKE LINE OF CT_MESSAGE,
          LF_AMOUNT_TXT    TYPE TEXT100,
          LF_DATE_TXT      TYPE TEXT100,
          LF_AMOUNT        TYPE VBAK-NETWR,
          LF_EXIST_AMOUNT  TYPE VBAK-NETWR.
    CLEAR: CT_MESSAGE,
           CF_FSCM_TE_CHECK,
           CF_FSCM_SET,
           CF_DATA_VALUE.

    IF IS_XVBAK-KNKLI IS INITIAL
    OR IS_XVBAK-KKBER IS INITIAL.
      RETURN.
    ENDIF.

*   -- case reference ( copy doc ) ,check all entries of copied VBAP completely
*   '(SAPFV45C)CVBAP-POSNR'  must be equal to last line of '(SAPFV45C)CVBAP[]'
    IF IF_TRTYP = 'H'
    AND IS_XVBAK-VGBEL IS NOT INITIAL.  "copy from reference
      IF CHECK_COPY_DOC_NOT_LAST_VBAP( ) = ABAP_TRUE.
        RETURN.
      ENDIF.
    ENDIF.

*   --- get wbs in vbap
    LOOP AT IT_XVBAP INTO DATA(LS_VBAP) WHERE PS_PSP_PNR <> ''.
      EXIT.
    ENDLOOP.
    IF LS_VBAP IS INITIAL.
      RETURN.
    ENDIF.


    SELECT A~PSPHI ,                                    "#EC CI_NOORDER
           B~PSPID AS PROJ_POSID
    INTO @DATA(LS_PROJECT)
    UP TO 1 ROWS
    FROM PRPS AS A INNER JOIN PROJ AS B
    ON A~PSPHI = B~PSPNR
    WHERE A~PSPNR = @LS_VBAP-PS_PSP_PNR.
    ENDSELECT.
* =================== OVER CREDIT LIMIT ===============
*   ---- Get currency
    SELECT A~CREDIT_SGMNT, A~CURRENCY                  "#EC CI_BUFFJOIN
    FROM UKMCRED_SGM0C AS A  INNER JOIN UKM_KKBER2SGM AS B "#EC CI_NOORDER
    ON A~CREDIT_SGMNT = B~CREDIT_SGMNT
    WHERE KKBER = @IS_XVBAK-KKBER
    INTO @DATA(LS_CREDIT)
    UP TO 1 ROWS.
    ENDSELECT .

*BOD CH01
**   ---- get credit limit from table ZSDSFIT036
*    SELECT KUNNR,
*           PSPHI,
*           CREDIT_LIMIT,
*           STARTDATE,
*           ENDDATE,
*           SEQ
*    INTO TABLE @DATA(LT_036)
*    FROM ZSDSFIT036  AS A
*    WHERE A~KUNNR = @IS_XVBAK-KNKLI
*    AND   A~PSPHI = @LS_PROJECT-PSPHI.
*    IF SY-SUBRC <> 0.
*      RETURN.
*    ENDIF.
*    SORT LT_036 BY STARTDATE DESCENDING ENDDATE DESCENDING SEQ DESCENDING.
*    READ TABLE LT_036 INTO DATA(LS_036) INDEX 1.
*EOD CH01
*   ---- get credit exprosure from program ZSDSFIR0320
    INSERT VALUE #( SIGN = 'I'
                    OPTION = 'EQ'
                    LOW = IS_XVBAK-KNKLI )
    INTO TABLE LRT_BP.
    INSERT VALUE #( SIGN = 'I'
                    OPTION = 'EQ'
                    LOW = LS_PROJECT-PSPHI )
    INTO TABLE LRT_WBS.

    ZCL_SDSFI_CUSTOMER_CREDIT=>GET_DATA(
      EXPORTING
        IT_PARTNER      = LRT_BP[]           " Table Type EDM_PARTN_RANGE
        IT_WBS          = LRT_WBS[]          " Selection Options for POSID (WBS Element)
        IF_CREDIT_SGMNT = LS_CREDIT-CREDIT_SGMNT  " SAP Credit Management: Ranges Table for Credit Segments
        IF_ALL_CUSTOMER = ABAP_TRUE        " All Customer
        IF_SEL_CREDIT   = 'P'              " Selection (C: Customer Credit, P: Project Credit, *: All )
        IF_LANGU        = SY-LANGU         " ABAP System Field: Language Key of Text Environment
      IMPORTING
        ET_RESULT       = DATA(LT_RESULT)
    ).
    IF IS_XVBAK-VBELN IS NOT INITIAL. "CHANGE MODE
*     if change mode and not block -> must sum exist amount to deduct from exposure
      SELECT SINGLE CMGST
      INTO @DATA(LF_CMGST)
      FROM VBAK
      WHERE VBELN = @IS_XVBAK-VBELN.
      IF LF_CMGST <> 'B'.
        SELECT VBELN ,                                  "#EC CI_NOORDER
               SUM( NETWR )  AS NETWR,
               SUM( MWSBP )  AS MWSBP
        INTO @DATA(LS_VBAP_EXIST)
        UP TO 1 ROWS
        FROM VBAP
        WHERE VBELN = @IS_XVBAK-VBELN
        GROUP BY VBELN.

        ENDSELECT.
        LF_EXIST_AMOUNT = LS_VBAP_EXIST-NETWR + LS_VBAP_EXIST-MWSBP.

      ENDIF.
    ENDIF.
    LOOP AT IT_XVBAP INTO DATA(LS_XVBAP) ##INTO_OK.
      LF_AMOUNT = LF_AMOUNT + LS_XVBAP-NETWR + LS_XVBAP-MWSBP .
    ENDLOOP.

    READ TABLE LT_RESULT INTO DATA(LS_RESULT) INDEX 1.
    IF SY-SUBRC = 0.
      READ TABLE LS_RESULT-SUMMARY_PROJ INTO DATA(LS_SUM_PROJ) WITH KEY PSPID = LS_PROJECT-PROJ_POSID.
      IF SY-SUBRC = 0.
*BOI CH01
        IF LS_SUM_PROJ-STATUS_TXT <> 'Open'.
          CLEAR LS_SUM_PROJ-CREDIT_LIMIT.
        ENDIF.
*EOI CH01
        LF_CREDIT_EXCEED = ( LS_SUM_PROJ-CREDIT_EXPOSURE - LF_EXIST_AMOUNT +  LF_AMOUNT ) -  LS_SUM_PROJ-CREDIT_LIMIT.
      ENDIF.
    ENDIF.

* =================== AR OVERDUE LOGIC ===============
*   ---- Get currency
    SELECT SINGLE WAERS
    INTO @DATA(LF_WAERS)
    FROM T001
    WHERE BUKRS = @IS_XVBAK-BUKRS_VF.

*   --- get spec gl open item
    SELECT BELNR,
           KUNNR,
           SGTXT,
           SHKZG,
           DMBTR,
           ZTERM,
           BLART,
           UMSKZ
    FROM BSID_VIEW
    WHERE KUNNR = @IS_XVBAK-KNKLI
    AND   KKBER = @IS_XVBAK-KKBER
    AND   UMSKZ IN @GR_OVDUE_UMSKZ
    INTO TABLE @DATA(LT_BSID_SP).
    LOOP AT LT_BSID_SP INTO DATA(LS_BSID_SP) ##INTO_OK.
      IF LS_BSID_SP-SHKZG = 'H'.
        LF_OVDUE_AMOUNT = LF_OVDUE_AMOUNT - LS_BSID_SP-DMBTR.
      ELSE.
        LF_OVDUE_AMOUNT = LF_OVDUE_AMOUNT + LS_BSID_SP-DMBTR.
      ENDIF.
    ENDLOOP.

    IF LF_OVDUE_AMOUNT IS INITIAL.
*   ---- get open item
      SELECT BELNR,
             KUNNR,
             SGTXT,
             SHKZG,
             DMBTR,
             ZFBDT,
             ZBD1T,
             ZTERM,
             BLART,
             UMSKZ
      FROM BSID_VIEW
      WHERE KUNNR = @IS_XVBAK-KNKLI
      AND   KKBER = @IS_XVBAK-KKBER
*      AND   SGTXT = @LS_PROJECT-PROJ_POSID  "CH02-
      AND   ZTERM IN @GR_OVDUE_ZTERM
      AND   BLART IN @GR_OVDUE_BLART
      AND   UMSKZ = ''                       "CH02+
      INTO TABLE @DATA(LT_BSID).

      SELECT SINGLE ZDAYS
      INTO @DATA(LF_ZDAYS)
      FROM ZSDSFIC024
      WHERE KUNNR = @IS_XVBAK-KNKLI.

      LOOP AT LT_BSID INTO DATA(LS_BSID) ##INTO_OK.
        LF_NETDUE = LS_BSID-ZFBDT + LS_BSID-ZBD1T + LF_ZDAYS.
        IF LF_NETDUE <= SY-DATUM.
          IF LS_BSID-SHKZG = 'H'.
            LF_OVDUE_AMOUNT = LF_OVDUE_AMOUNT - LS_BSID-DMBTR.
          ELSE.
            LF_OVDUE_AMOUNT = LF_OVDUE_AMOUNT + LS_BSID-DMBTR.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
* =================== SET MESSAGE ===============
    IF LF_CREDIT_EXCEED > 0.
      LS_MESSAGE-LANGU = SY-LANGU.
      LS_MESSAGE-MSG_TEXT = '=====Over Credit======' ##NO_TEXT.
      APPEND LS_MESSAGE TO CT_MESSAGE.

      WRITE LF_CREDIT_EXCEED CURRENCY LS_CREDIT-CURRENCY TO LF_AMOUNT_TXT .
      CONDENSE LF_AMOUNT_TXT NO-GAPS.
      LS_MESSAGE-MSG_TEXT = |Credit Exceed : { LF_AMOUNT_TXT }| ##NO_TEXT.
      APPEND LS_MESSAGE TO CT_MESSAGE.

      WRITE LS_SUM_PROJ-ENDDATE TO LF_DATE_TXT.
      LS_MESSAGE-MSG_TEXT = |Credit Expire date : { LF_DATE_TXT }| ##NO_TEXT.
      APPEND LS_MESSAGE TO CT_MESSAGE.
      CF_FSCM_TE_CHECK  = 4.
      CF_FSCM_SET = ABAP_TRUE.
      CF_DATA_VALUE-CREDIT_EXCEED = LF_CREDIT_EXCEED.
      CF_DATA_VALUE-CREDIT_EXCEED_WAERS = LS_CREDIT-CURRENCY.

      ASSIGN ('(SAPLVKMP)T691F-PMREA') TO FIELD-SYMBOL(<LFS_PMREA>).
      IF <LFS_PMREA> IS ASSIGNED.
        <LFS_PMREA> = 'B' . "error
      ENDIF.
    ENDIF.

    IF LF_OVDUE_AMOUNT > 0.
      LS_MESSAGE-LANGU = SY-LANGU.
      LS_MESSAGE-MSG_TEXT = '=====AR Overdue======' ##NO_TEXT.
      APPEND LS_MESSAGE TO CT_MESSAGE.

      WRITE LF_OVDUE_AMOUNT CURRENCY LF_WAERS TO LF_AMOUNT_TXT .
      CONDENSE LF_AMOUNT_TXT NO-GAPS.
      LS_MESSAGE-MSG_TEXT = |AR Overdue: { LF_AMOUNT_TXT }| ##NO_TEXT.
      APPEND LS_MESSAGE TO CT_MESSAGE.
      CF_FSCM_TE_CHECK  = 4.
      CF_FSCM_SET = ABAP_TRUE.
      CF_DATA_VALUE-OVDUE_AMOUNT = LF_OVDUE_AMOUNT.
      CF_DATA_VALUE-OVDUE_AMOUNT_WAERS = LF_WAERS.
    ENDIF.

  ENDMETHOD.


  METHOD CREDIT_GET_VALUE_NORMAL_ORDER.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSFICM_UKM_SD_FSCM/ CREDIT_GET_VALUE_NORMAL_ORDER
*  Creation Date      : 20.08.2024
*  Author             : Boontip R. (Eviden)
*  Add-on ID          : FSCME035
*  Description        : get credit value , over due value for normal order
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
    DATA:
*      LS_CHECK_RESULT TYPE UKM_S_QUERY_RESULT.
*      LF_PARTNER      TYPE BU_PARTNER,
*      LF_SEGMENT      TYPE UKM_CREDIT_SGMNT,
      LF_OVDUE_AMOUNT TYPE BSID_VIEW-DMBTR,
      LF_NETDUE       TYPE SY-DATUM.
    DATA:
      LRT_BP          TYPE RANGE OF ZSDSFIT036-KUNNR,
      LF_EXIST_AMOUNT TYPE VBAK-NETWR,
      LF_AMOUNT       TYPE VBAK-NETWR.

    CLEAR: CF_FSCM_TE_CHECK,
           CF_DATA_VALUE.

    IF IS_XVBAK-KNKLI IS INITIAL
    OR IS_XVBAK-KKBER IS INITIAL.
      RETURN.
    ENDIF.


* =================== OVER CREDIT LIMIT ===============
*    LF_PARTNER = IS_XVBAK-KNKLI.
*    LF_SEGMENT = IS_XVBAK-KKBER.
*    CALL FUNCTION 'UKM_CREDIT_CHECK_SIMU'
*      EXPORTING
*        I_PARTNER      = LF_PARTNER
*        I_SEGMENT      = LF_SEGMENT
*        I_NO_DIALOG    = ABAP_TRUE
*      IMPORTING
*        E_CHECK_RESULT = LS_CHECK_RESULT.
*    LS_CHECK_RESULT-COMM_TOTAL = LS_CHECK_RESULT-COMM_TOTAL + IS_XVBAK-NETWR.
*    CF_DATA_VALUE-CREDIT_EXCEED = LS_CHECK_RESULT-COMM_TOTAL - LS_CHECK_RESULT-CREDIT_LIMIT.
    INSERT VALUE #( SIGN = 'I'
                    OPTION = 'EQ'
                    LOW = IS_XVBAK-KNKLI )
    INTO TABLE LRT_BP.
*   ---- Get currency
    SELECT A~CREDIT_SGMNT, A~CURRENCY                  "#EC CI_BUFFJOIN
    FROM UKMCRED_SGM0C AS A  INNER JOIN UKM_KKBER2SGM AS B "#EC CI_NOORDER
    ON A~CREDIT_SGMNT = B~CREDIT_SGMNT
    WHERE KKBER = @IS_XVBAK-KKBER
    INTO @DATA(LS_CREDIT)
    UP TO 1 ROWS.
    ENDSELECT .

    ZCL_SDSFI_CUSTOMER_CREDIT=>GET_DATA(
      EXPORTING
        IT_PARTNER      = LRT_BP[]         " Table Type EDM_PARTN_RANGE
        IF_CREDIT_SGMNT = LS_CREDIT-CREDIT_SGMNT  " SAP Credit Management: Ranges Table for Credit Segments
        IF_ALL_CUSTOMER = ABAP_TRUE        " All Customer
        IF_SEL_CREDIT   = 'C'              " Selection (C: Customer Credit, P: Project Credit, *: All )
        IF_LANGU        = SY-LANGU         " ABAP System Field: Language Key of Text Environment
      IMPORTING
        ET_RESULT       = DATA(LT_RESULT)
    ).
    IF IS_XVBAK-VBELN IS NOT INITIAL. "CHANGE MODE
*     if change mode and not block -> must sum exist amount to deduct from exposure
      SELECT SINGLE CMGST
      INTO @DATA(LF_CMGST)
      FROM VBAK
      WHERE VBELN = @IS_XVBAK-VBELN.
      IF LF_CMGST <> 'B'.
        SELECT VBELN ,                     "#EC CI_NOORDER
               SUM( NETWR )  AS NETWR,
               SUM( MWSBP )  AS MWSBP
        INTO @DATA(LS_VBAP_EXIST)
        UP TO 1 ROWS
        FROM VBAP
        WHERE VBELN = @IS_XVBAK-VBELN
        GROUP BY VBELN.
        ENDSELECT.
        LF_EXIST_AMOUNT = LS_VBAP_EXIST-NETWR + LS_VBAP_EXIST-MWSBP.
      ENDIF.
    ENDIF.
    LOOP AT IT_XVBAP INTO DATA(LS_XVBAP) ##INTO_OK.
      LF_AMOUNT = LF_AMOUNT + LS_XVBAP-NETWR + LS_XVBAP-MWSBP .
    ENDLOOP.
    READ TABLE LT_RESULT INTO DATA(LS_RESULT) WITH KEY PARTNER = IS_XVBAK-KNKLI.
    IF SY-SUBRC = 0.
      CF_DATA_VALUE-CREDIT_EXCEED = ( LS_RESULT-CREDIT_EXPOSURE - LF_EXIST_AMOUNT +  LF_AMOUNT ) -  LS_RESULT-CREDIT_LIMIT.
    ENDIF.

    IF CF_DATA_VALUE-CREDIT_EXCEED > 0.
      CF_FSCM_TE_CHECK  = 4.
      CF_DATA_VALUE-CREDIT_EXCEED_WAERS = LS_CREDIT-CURRENCY .
    ENDIF.
* =================== AR OVERDUE LOGIC ===============

*   ---- Get currency
    SELECT SINGLE WAERS
    INTO @DATA(LF_WAERS)
    FROM T001
    WHERE BUKRS = @IS_XVBAK-BUKRS_VF.
*   --- get spec gl open item
    SELECT BELNR,
           KUNNR,
           SGTXT,
           SHKZG,
           DMBTR,
           ZTERM,
           BLART,
           UMSKZ
    FROM BSID_VIEW
    WHERE KUNNR = @IS_XVBAK-KNKLI
    AND   KKBER = @IS_XVBAK-KKBER
    AND   UMSKZ IN @GR_OVDUE_UMSKZ
    INTO TABLE @DATA(LT_BSID_SP).
    LOOP AT LT_BSID_SP INTO DATA(LS_BSID_SP) ##INTO_OK.
      IF LS_BSID_SP-SHKZG = 'H'.
        LF_OVDUE_AMOUNT = LF_OVDUE_AMOUNT - LS_BSID_SP-DMBTR.
      ELSE.
        LF_OVDUE_AMOUNT = LF_OVDUE_AMOUNT + LS_BSID_SP-DMBTR.
      ENDIF.
    ENDLOOP.
    IF LF_OVDUE_AMOUNT IS INITIAL.
*   ---- get open item
      SELECT BELNR,
             KUNNR,
             SGTXT,
             SHKZG,
             DMBTR,
             ZFBDT,
             ZBD1T,
             ZTERM,
             BLART,
             UMSKZ
      FROM BSID_VIEW
      WHERE KUNNR = @IS_XVBAK-KNKLI
      AND   KKBER = @IS_XVBAK-KKBER
      AND   ZTERM IN @GR_OVDUE_ZTERM
      AND   BLART IN @GR_OVDUE_BLART
      AND   UMSKZ = ''
      INTO TABLE @DATA(LT_BSID).

      SELECT SINGLE ZDAYS
      INTO @DATA(LF_ZDAYS)
      FROM ZSDSFIC024
      WHERE KUNNR = @IS_XVBAK-KNKLI.

      LOOP AT LT_BSID INTO DATA(LS_BSID) ##INTO_OK.
        LF_NETDUE = LS_BSID-ZFBDT + LS_BSID-ZBD1T + LF_ZDAYS.
        IF LF_NETDUE <= SY-DATUM.
          IF LS_BSID-SHKZG = 'H'.
            LF_OVDUE_AMOUNT = LF_OVDUE_AMOUNT - LS_BSID-DMBTR.
          ELSE.
            LF_OVDUE_AMOUNT = LF_OVDUE_AMOUNT + LS_BSID-DMBTR.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
    IF LF_OVDUE_AMOUNT > 0.
      CF_FSCM_TE_CHECK  = 4.
      CF_DATA_VALUE-OVDUE_AMOUNT = LF_OVDUE_AMOUNT.
      CF_DATA_VALUE-OVDUE_AMOUNT_WAERS = LF_WAERS.
    ENDIF.
  ENDMETHOD.


  METHOD CHECK_ACTIVE_NORMAL_ORDER_TYPE.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSFICM_UKM_SD_FSCM/ CHECK_ACTIVE_NORMAL_ORDER_TYPE
*  Creation Date      : 20.08.2024
*  Author             : Boontip R. (Eviden)
*  Add-on ID          : FSCME035
*  Description        : check order type whether it is found in genc or not
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
    CLEAR RF_ACTIVE.

    IF GR_AUART_NORMAL IS NOT INITIAL
    AND IF_AUART IN GR_AUART_NORMAL.
      RF_ACTIVE = ABAP_TRUE.
    ENDIF.

  ENDMETHOD.


  METHOD CHECK_COPY_DOC_NOT_LAST_VBAP.
    FIELD-SYMBOLS <L_T_CVBAP> TYPE STANDARD TABLE.
    CLEAR RF_NOT_LAST.
*   -- case reference ( copy doc ) ,check all entries of copied VBAP completely
*   '(SAPFV45C)CVBAP-POSNR'  must be equal to last line of '(SAPFV45C)CVBAP[]'
    ASSIGN ('(SAPFV45C)CVBAP-POSNR') TO FIELD-SYMBOL(<LF_CVBAP_POSNR>).
    ASSIGN ('(SAPFV45C)CVBAP[]') TO <L_T_CVBAP>.
    IF <L_T_CVBAP> IS ASSIGNED
    AND <LF_CVBAP_POSNR> IS ASSIGNED.

      IF <L_T_CVBAP> IS INITIAL
      OR <LF_CVBAP_POSNR> IS INITIAL.
        RETURN.
      ENDIF.

      DATA(LF_LINE) = LINES( <L_T_CVBAP> ).

      READ TABLE <L_T_CVBAP> ASSIGNING FIELD-SYMBOL(<LS_CVBAP>) INDEX LF_LINE.
      IF <LS_CVBAP> IS ASSIGNED.

        ASSIGN COMPONENT 'POSNR' OF STRUCTURE <LS_CVBAP> TO FIELD-SYMBOL(<L_POSNR>).

        IF <L_POSNR> IS ASSIGNED
        AND <L_POSNR> <> <LF_CVBAP_POSNR> .
          RF_NOT_LAST = ABAP_TRUE.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD CREDIT_CHECK_NORMAL_ORDER.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSFICM_UKM_SD_FSCM/ CREDIT_CHECK_WBS_ORDER
*  Creation Date      : 20.08.2024
*  Author             : Boontip R. (Eviden)
*  Add-on ID          : FSCME035
*  Description        : check credit limit for normal order type
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
    DATA: LS_MESSAGE      LIKE LINE OF CT_MESSAGE,
          LF_AMOUNT_TXT   TYPE TEXT100,
          LF_SUBRC        TYPE SY-SUBRC,
          LS_CREDIT_VALUE TYPE ZSDSFIS133.

    CLEAR: CT_MESSAGE,
           CF_FSCM_TE_CHECK,
           CF_FSCM_SET,
           CF_DATA_VALUE.

    IF IS_XVBAK-KNKLI IS INITIAL
    OR IS_XVBAK-KKBER IS INITIAL.
      RETURN.
    ENDIF.


*   -- case reference ( copy doc ) ,check all entries of copied VBAP completely
*   '(SAPFV45C)CVBAP-POSNR'  must be equal to last line of '(SAPFV45C)CVBAP[]'
    IF IF_TRTYP = 'H'  "create
    AND IS_XVBAK-VGBEL IS NOT INITIAL.  "copy from reference
      IF CHECK_COPY_DOC_NOT_LAST_VBAP( ) = ABAP_TRUE.
        RETURN.
      ENDIF.
    ENDIF.


    CALL METHOD ZCL_SDSFICM_UKM_SD_FSCM=>CREDIT_GET_VALUE_NORMAL_ORDER
      EXPORTING
        IS_XVBAK         = IS_XVBAK
        IT_XVBAP         = IT_XVBAP
      CHANGING
        CF_FSCM_TE_CHECK = LF_SUBRC
        CF_DATA_VALUE    = LS_CREDIT_VALUE.

    CF_DATA_VALUE = LS_CREDIT_VALUE.

* =================== SET MESSAGE ===============
    IF LS_CREDIT_VALUE-CREDIT_EXCEED > 0.
      LS_MESSAGE-LANGU = SY-LANGU.
      LS_MESSAGE-MSG_TEXT = '=====Over Credit======' ##NO_TEXT.
      APPEND LS_MESSAGE TO CT_MESSAGE.

      WRITE LS_CREDIT_VALUE-CREDIT_EXCEED CURRENCY LS_CREDIT_VALUE-CREDIT_EXCEED_WAERS TO LF_AMOUNT_TXT .
      CONDENSE LF_AMOUNT_TXT NO-GAPS.
      LS_MESSAGE-MSG_TEXT = |Credit Exceed : { LF_AMOUNT_TXT }| ##NO_TEXT.
      APPEND LS_MESSAGE TO CT_MESSAGE.

      CF_FSCM_TE_CHECK  = 4.
      CF_FSCM_SET = ABAP_TRUE.

      ASSIGN ('(SAPLVKMP)T691F-PMREA') TO FIELD-SYMBOL(<LFS_PMREA>).
      IF <LFS_PMREA> IS ASSIGNED.
        <LFS_PMREA> = 'B' . "error
      ENDIF.
    ENDIF.

    IF  LS_CREDIT_VALUE-OVDUE_AMOUNT > 0.
      LS_MESSAGE-LANGU = SY-LANGU.
      LS_MESSAGE-MSG_TEXT = '=====AR Overdue======' ##NO_TEXT.
      APPEND LS_MESSAGE TO CT_MESSAGE.

      WRITE LS_CREDIT_VALUE-OVDUE_AMOUNT CURRENCY LS_CREDIT_VALUE-OVDUE_AMOUNT_WAERS TO LF_AMOUNT_TXT .
      CONDENSE LF_AMOUNT_TXT NO-GAPS.
      LS_MESSAGE-MSG_TEXT = |AR Overdue: { LF_AMOUNT_TXT }| ##NO_TEXT.
      APPEND LS_MESSAGE TO CT_MESSAGE.
      CF_FSCM_TE_CHECK  = 4.
      CF_FSCM_SET = ABAP_TRUE.


    ENDIF.

  ENDMETHOD.


  METHOD CREDIT_SET_MESSAGE_SHOW_ERROR.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSFICM_UKM_SD_FSCM/ CREDIT_GET_VALUE_NORMAL_ORDER
*  Creation Date      : 20.08.2024
*  Author             : Boontip R. (Eviden)
*  Add-on ID          : FSCME035
*  Description        : set message
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
    DATA: LF_MSG_TEXT TYPE UKM_S_PI_CREDIT_MESSAGE-MSG_TEXT.
    LOOP AT CT_FSCM_CREDIT_MESSAGES ASSIGNING FIELD-SYMBOL(<L_MSG>).
      IF <L_MSG>-MSG_TEXT CP '====*'.
        CONTINUE.
      ENDIF.
      IF LF_MSG_TEXT IS INITIAL.
        LF_MSG_TEXT = <L_MSG>-MSG_TEXT.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF LF_MSG_TEXT IS NOT INITIAL.
      CLEAR CT_FSCM_CREDIT_MESSAGES.
      INSERT VALUE #( LANGU = SY-LANGU
                      MSG_TEXT = LF_MSG_TEXT )
      INTO TABLE CT_FSCM_CREDIT_MESSAGES.
    ENDIF.
  ENDMETHOD.


  METHOD GET_GENC.
    IF GR_VKORG_NO_CHECK_CREDIT IS INITIAL.
      CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE
        EXPORTING
          IF_REPID = GC_REPID
          IF_PARAM = 'NO_CHECK_CREDIT'
          IF_EXT   = 'SALES_ORG'
        IMPORTING
          ET_RANGE = GR_VKORG_NO_CHECK_CREDIT.
    ENDIF.
    IF GR_ZTERM_NO_CHECK_CREDIT IS INITIAL.
      CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE
        EXPORTING
          IF_REPID = GC_REPID
          IF_PARAM = 'NO_CHECK_CREDIT'
          IF_EXT   = 'PAYM_TERM'
        IMPORTING
          ET_RANGE = GR_ZTERM_NO_CHECK_CREDIT.
    ENDIF.
    IF GR_VBTYP_NO_CHECK_CREDIT IS INITIAL.
      CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE
        EXPORTING
          IF_REPID = GC_REPID
          IF_PARAM = 'NO_CHECK_CREDIT'
          IF_EXT   = 'DOC_CAT'
        IMPORTING
          ET_RANGE = GR_VBTYP_NO_CHECK_CREDIT.
    ENDIF.
    IF GR_AUART_WBS IS INITIAL.
      CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE
        EXPORTING
          IF_REPID = GC_REPID
          IF_PARAM = 'SALES_DOC_TYPE_WBS'
        IMPORTING
          ET_RANGE = GR_AUART_WBS.
    ENDIF.
    IF GR_AUART_NORMAL IS INITIAL.
      CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE
        EXPORTING
          IF_REPID = GC_REPID
          IF_PARAM = 'SALES_DOC_TYPE_NORMAL'
        IMPORTING
          ET_RANGE = GR_AUART_NORMAL.
    ENDIF.
    IF GR_OVDUE_ZTERM IS INITIAL.
      CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE
        EXPORTING
          IF_REPID = GC_REPID
          IF_PARAM = 'OVDUE_SELECT_FI_DOC'
          IF_EXT   = 'PAYM_TERM'
        IMPORTING
          ET_RANGE = GR_OVDUE_ZTERM.
    ENDIF.

    IF GR_OVDUE_UMSKZ IS INITIAL.
      CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE
        EXPORTING
          IF_REPID = GC_REPID
          IF_PARAM = 'OVDUE_SELECT_FI_DOC'
          IF_EXT   = 'SPEC_GL_IND'
        IMPORTING
          ET_RANGE = GR_OVDUE_UMSKZ.
    ENDIF.

    IF GR_OVDUE_BLART IS INITIAL.
      CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE
        EXPORTING
          IF_REPID = GC_REPID
          IF_PARAM = 'OVDUE_SELECT_FI_DOC'
          IF_EXT   = 'DOC_TYPE'
        IMPORTING
          ET_RANGE = GR_OVDUE_BLART.
    ENDIF.
  ENDMETHOD.


  METHOD IGNORE_CREDIT_CHECK.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSFICM_UKM_SD_FSCM/ IGNORE_CREDIT_CHECK
*  Creation Date      : 24.10.2024
*  Author             : Boontip R. (Eviden)
*  Add-on ID          : FSCME035
*  Description        : check criteria whether credit should not ignore or not
*                       - by sales org
*                       - by payment term
*                       - by sales doc cat
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
    CLEAR RF_IGNORE.
    CALL METHOD GET_GENC.
    IF GR_VKORG_NO_CHECK_CREDIT IS NOT INITIAL  AND IF_VKORG IN GR_VKORG_NO_CHECK_CREDIT
    AND GR_ZTERM_NO_CHECK_CREDIT IS NOT INITIAL AND IF_ZTERM IN GR_ZTERM_NO_CHECK_CREDIT
    AND GR_VBTYP_NO_CHECK_CREDIT IS NOT INITIAL AND IF_VBTYP IN GR_VBTYP_NO_CHECK_CREDIT.
      RF_IGNORE = ABAP_TRUE.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
