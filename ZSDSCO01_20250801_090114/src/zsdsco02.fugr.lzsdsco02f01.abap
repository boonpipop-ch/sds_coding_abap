*----------------------------------------------------------------------*
***INCLUDE LZSDSCO02F01.
*----------------------------------------------------------------------*
*---------------------------------------------------------------------*
* Form F_DERIVE_BY_ACTIVITY_TYPE
*---------------------------------------------------------------------*
* Derive characteristics for project sales
* 1. Reference Procedure = VBRK
*    - Derive from Sale Quotation of WBS x-x-xxxxxxxx-01-01-xxx
* 2. Reference Procedure <> VBRK
*    - Derive from Service Order
*&---------------------------------------------------------------------*
FORM F_DERIVE_BY_ACTIVITY_TYPE USING UF_PROJTYPE TYPE CE11000-ZZ1_PROJTYPE_MSE
                               CHANGING CS_COPA_ITEM TYPE CE11000
                                        CF_ADRNR_SHIPTO TYPE CRMS4D_PARTNER-ADDR_NR
                                        CF_FAILED TYPE ABAP_BOOL.

  CONSTANTS: LC_ACTIVITY_TYPE        TYPE ZSDSCAC001-PARAM_EXT VALUE 'ACTIVITY_TYPE',
             LC_ACTIVITY_TYPE_SUBWBS TYPE ZSDSCAC001-PARAM_EXT VALUE 'ACTIVITY_TYPE_SUBWBS',
             LC_PROJ_TYPE            TYPE ZSDSCAC001-PARAM VALUE 'PROJECT_TYPE_',
             LC_AWTYP_VBRK           TYPE ACDOCA-AWTYP VALUE 'VBRK',
             LC_RLDNR                TYPE ACDOCA-RLDNR VALUE '0L'.

  DATA: LF_DERIVE_OK    TYPE ABAP_BOOL,
        LF_PARAM        TYPE ZSDSCAC001-PARAM,
        LF_PSPNR_0101   TYPE CE11000-PSPNR,
        LF_SUBWBS_FOUND TYPE C,
        LF_VBRK         TYPE FLAG.

  CLEAR: LF_DERIVE_OK, LF_VBRK, LF_SUBWBS_FOUND, CF_ADRNR_SHIPTO.

  CONCATENATE LC_PROJ_TYPE UF_PROJTYPE INTO LF_PARAM.
  CONDENSE LF_PARAM.

* Check Activity Type in scope? (found in ZSDSCAC001?)
  SELECT COUNT(*)
    FROM ZSDSCAC001
    UP TO 1 ROWS
    WHERE REPID EQ SY-REPID
      AND PARAM EQ LF_PARAM
      AND PARAM_EXT EQ LC_ACTIVITY_TYPE
      AND VALUE_LOW EQ CS_COPA_ITEM-ZZ1_ACTTYPE_MSE
      AND ZDEL_FLG EQ SPACE.
  IF SY-SUBRC NE 0.
    CASE UF_PROJTYPE.
      WHEN '1'.
        CF_FAILED = ABAP_TRUE.
      WHEN '2'.
        "Do not set fail flag
    ENDCASE.

    RETURN.
  ENDIF.

* Check Activity type to get suffix WBS from matching PRPS-POST1
  SELECT COUNT(*)
    FROM ZSDSCAC001
    UP TO 1 ROWS
    WHERE REPID EQ SY-REPID
      AND PARAM EQ LF_PARAM
      AND PARAM_EXT EQ LC_ACTIVITY_TYPE_SUBWBS
      AND VALUE_LOW EQ CS_COPA_ITEM-ZZ1_ACTTYPE_MSE
      AND ZDEL_FLG EQ SPACE.
  IF SY-SUBRC EQ 0.
    LF_SUBWBS_FOUND = 'X'.
  ENDIF.

* Check ACDOCA if reference procedure = VBRK found for this WBS?
  CASE UF_PROJTYPE.
    WHEN '1'.     "Project type 1
      SELECT BELNR
        FROM ACDOCA
        UP TO 1 ROWS
        INTO @DATA(LS_ACDOCA)                 ##NEEDED
        WHERE RLDNR EQ @LC_RLDNR
          AND AWREF_REV EQ @SPACE
          AND AWTYP EQ @LC_AWTYP_VBRK
          AND ( GLACCOUNT_TYPE EQ 'P' OR GLACCOUNT_TYPE EQ 'X' )
          AND PS_PSP_PNR EQ @CS_COPA_ITEM-PSPNR.
      ENDSELECT.
      IF SY-SUBRC EQ 0.
        LF_VBRK = ABAP_TRUE.
      ELSE.
        CLEAR: LF_VBRK.
      ENDIF.

    WHEN '2'.     "Project type 2
*   Project type 2, always get from service order
      CLEAR: LF_VBRK.
  ENDCASE.

* Reference Procedure = VBRK
  IF LF_VBRK EQ ABAP_TRUE.
    IF LF_SUBWBS_FOUND EQ SPACE.
* Begin of deletion - CH01 -  420000223 - 17.02.2025
**     Derive from Sale Quotation of WBS x-x-xxxxxxxx-01-01-001
*      PERFORM F_REPLACE_WBS_ACTIVITY USING CS_COPA_ITEM-PSPNR
*                                           '01-01'
*                                           '-001'
*                                   CHANGING LF_PSPNR_0101.
* End of deletion - CH01 -  420000223 - 17.02.2025

* Begin of insertion - CH01 - 420000223 - 17.02.2025
* First, to derive from sales quotation of WBS itself
* If not found, derive from sales quotation of WBS x-x-xxxxxxxx-01-01-yyyy
      PERFORM F_DERIVE_QUOTATION_FROM_WBS USING CS_COPA_ITEM-PSPNR
                                                '01-01'
                                       CHANGING CS_COPA_ITEM
                                                LF_DERIVE_OK.
* End of insertion - CH01 - 420000223 - 17.02.2025

    ELSE.
*     Derive from Sale Quotation of WBS x-x-xxxxxxxx-01-01-yyy
*     by matching with WBS Short text (PRPS-POST1)
      PERFORM F_REPLACE_WBS_SUBSTITUTION USING CS_COPA_ITEM-PSPNR
                                               '01-01'
                                         CHANGING LF_PSPNR_0101.

*     Begin of insertion - CH01 -  420000223 - 17.02.2025
      IF LF_PSPNR_0101 IS INITIAL.
        CF_FAILED = ABAP_TRUE.
        RETURN.
      ENDIF.

      PERFORM F_GET_FROM_QUOTATION USING LF_PSPNR_0101
                                CHANGING CS_COPA_ITEM
                                         LF_DERIVE_OK.
*     End of insertion - CH01 -  420000223 - 17.02.2025
    ENDIF.

* Begin of deletion - CH01 -  420000223 - 17.02.2025
*    IF LF_PSPNR_0101 IS INITIAL.
*      CF_FAILED = ABAP_TRUE.
*      RETURN.
*    ENDIF.
*
*    PERFORM F_GET_FROM_QUOTATION USING LF_PSPNR_0101
*                              CHANGING CS_COPA_ITEM
*                                       LF_DERIVE_OK.
* End of deletion - CH01 -  420000223 - 17.02.2025

  ELSE.
*   Reference Procedure <> VBRK
*   Then derive from Service Order
    PERFORM F_GET_FROM_SERVICE_ORDER USING CS_COPA_ITEM-PSPNR
                                  CHANGING CS_COPA_ITEM
                                           LF_DERIVE_OK
                                           CF_ADRNR_SHIPTO.
  ENDIF.

  IF LF_DERIVE_OK EQ ABAP_TRUE.
    CLEAR: CF_FAILED.
  ENDIF.

ENDFORM.
* Begin of deletion - CH01 -  420000223 - 03.03.2025
**---------------------------------------------------------------------*
** Form F_REPLACE_WBS_ACTIVITY
**---------------------------------------------------------------------*
** Replace WBS activity with value YY-YY
** from WBS x-x-xxxxxxxx-XX-XX-xxx
** to   WBS x-x-xxxxxxxx-YY-YY-xxx
**---------------------------------------------------------------------*
*FORM F_REPLACE_WBS_ACTIVITY  USING    UF_PSPNR TYPE CE11000-PSPNR
*                                      UF_ACTIVITY TYPE CHAR5
*                                      UF_SUFFIX TYPE CHAR5
*                             CHANGING CF_PSPNR TYPE CE11000-PSPNR.
*  DATA: LF_POSID TYPE PRPS-POSID.
*
*  CLEAR: CF_PSPNR.
*
*  IF UF_PSPNR IS INITIAL.
*    RETURN.
*  ENDIF.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
*    EXPORTING
*      INPUT  = UF_PSPNR
*    IMPORTING
*      OUTPUT = LF_POSID.
*
*  IF LF_POSID IS NOT INITIAL.
*    LF_POSID+13(5) = UF_ACTIVITY.
*
*    IF UF_SUFFIX IS NOT INITIAL.
*      LF_POSID+18(4) = UF_SUFFIX.
*    ENDIF.
*
*    SELECT PSPNR,
*           POSID
*      FROM PRPS
*      WHERE POSID EQ @LF_POSID
*      ORDER BY PRIMARY KEY
*      INTO @DATA(LS_PRPS)
*      UP TO 1 ROWS.
*    ENDSELECT.
*
*    IF SY-SUBRC EQ 0.
*      CF_PSPNR = LS_PRPS-PSPNR.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.
* End of deletion - CH01 -  420000223 - 03.03.2025
*---------------------------------------------------------------------*
* Form F_GET_FROM_QUOTATION
*---------------------------------------------------------------------*
* Get sales data from quotation
*---------------------------------------------------------------------*
FORM F_GET_FROM_QUOTATION  USING    UF_PSPNR_0101 TYPE CE11000-PSPNR
                           CHANGING CS_COPA_ITEM TYPE CE11000
                                    CF_DERIVE_OK TYPE FLAG.
  CLEAR: CF_DERIVE_OK.

* Get from WBS activity 01-01-XXX
  IF UF_PSPNR_0101 IS NOT INITIAL.
    SELECT VBAP~VBELN,
           VBAP~POSNR,
           VBAP~MATNR,                                  "#EC CI_NOORDER
           VBAP~PERVE_ANA,
           VBAP~KUNNR_ANA,
           VBAP~KUNWE_ANA,
           VBAP~PRODH,
           VBAP~VKBUR_ANA,
           VBAP~MVGR1,
           VBAK~VKORG,
           VBAK~VTWEG,
           VBAK~VKGRP
      INTO @DATA(LS_VBAP) UP TO 1 ROWS
      FROM VBAP
      INNER JOIN VBAK
        ON VBAK~VBELN EQ VBAP~VBELN
      WHERE VBAP~PS_PSP_PNR EQ @UF_PSPNR_0101
        AND VBAP~ABGRU EQ @SPACE
        AND VBAK~VBTYP EQ 'B'.   "B-Quotation, C-Order
    ENDSELECT.

    IF SY-SUBRC EQ 0.
      CF_DERIVE_OK = ABAP_TRUE.

      CS_COPA_ITEM-ARTNR     = LS_VBAP-MATNR.
      CS_COPA_ITEM-KMVTNR    = LS_VBAP-PERVE_ANA.
      CS_COPA_ITEM-KNDNR     = LS_VBAP-KUNNR_ANA.
      CS_COPA_ITEM-KUNWE     = LS_VBAP-KUNWE_ANA.
      CS_COPA_ITEM-PRODH     = LS_VBAP-PRODH.
      CS_COPA_ITEM-VKBUR     = LS_VBAP-VKBUR_ANA.
      CS_COPA_ITEM-VKGRP     = LS_VBAP-VKGRP.
      CS_COPA_ITEM-VKORG     = LS_VBAP-VKORG.
      CS_COPA_ITEM-VTWEG     = LS_VBAP-VTWEG.
      CS_COPA_ITEM-ZZ1_MVGR1 = LS_VBAP-MVGR1.

      CS_COPA_ITEM-KAUFN = LS_VBAP-VBELN.
      CS_COPA_ITEM-KDPOS = LS_VBAP-POSNR.

      RETURN.
    ENDIF.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_GET_FROM_SERVICE_ORDER
*---------------------------------------------------------------------*
* Get sales data from service order
*---------------------------------------------------------------------*
FORM F_GET_FROM_SERVICE_ORDER  USING    UF_PSPNR TYPE CE11000-PSPNR
                               CHANGING CS_COPA_ITEM TYPE CE11000
                                        CF_DERIVE_OK TYPE FLAG
                                        CF_ADRNR_SHIPTO TYPE CRMS4D_PARTNER-ADDR_NR.

  DATA: LF_POSID TYPE PRPS-POSID.

  CLEAR: CF_DERIVE_OK, CF_ADRNR_SHIPTO.

  PERFORM F_CONVERT_WBS_OUTPUT USING UF_PSPNR
                               CHANGING LF_POSID.

* Get service order the latest one
  SELECT CRMS4D_SERV_H~OBJECT_ID,
         CRMS4D_SERV_I~NUMBER_INT,
         CRMS4D_SERV_I~SOLD_TO_PARTY,                   "#EC CI_NOORDER
         CRMS4D_SERV_I~PRODUCT_ID,
         CRMS4D_SERV_I~SALES_ORG_SD,
         CRMS4D_SERV_I~DIS_CHANNEL,
         CRMS4D_SERV_I~SALES_OFFICE_SD,
         CRMS4D_SERV_I~SALES_GROUP_SD,
         CRMS4D_SERV_I~SHIP_TO_PARTY,
         CRMS4D_SERV_I~PERSON_RESP,
         CRMS4D_SERV_I~PROD_HIERARCHY,
         CRMS4D_SERV_I~PRC_GROUP1
    FROM CRMS4D_SERV_I
    INNER JOIN CRMS4D_SERV_H
      ON ( CRMS4D_SERV_H~OBJTYPE_H EQ CRMS4D_SERV_I~OBJTYPE_H AND
           CRMS4D_SERV_H~OBJECT_ID EQ CRMS4D_SERV_I~OBJECT_ID )
    UP TO 1 ROWS
    INTO @DATA(LS_SERVICE_ORD)
    WHERE CRMS4D_SERV_I~OBJTYPE_H EQ 'BUS2000116'
      AND CRMS4D_SERV_I~CANCPROC EQ @SPACE
      AND ( CRMS4D_SERV_H~STAT_LIFECYCLE EQ 'D' OR
*            CRMS4D_SERV_H~STAT_LIFECYCLE EQ 'C' )           "-420000714
            ( CRMS4D_SERV_H~STAT_LIFECYCLE EQ 'C' AND       "+420000714
              CRMS4D_SERV_H~STAT_CANCELLED EQ @SPACE ) )    "+420000714
      AND ( CRMS4D_SERV_I~STAT_LIFECYCLE EQ 'D' OR
*            CRMS4D_SERV_I~STAT_LIFECYCLE EQ 'C' )           "-420000714
            ( CRMS4D_SERV_I~STAT_LIFECYCLE EQ 'C' AND       "+420000714
              CRMS4D_SERV_I~STAT_CANCELLED EQ @SPACE ) )    "+420000714
      AND ( ( CRMS4D_SERV_I~AC_OBJECT_TYPE EQ '01' AND   "ORDER
              CRMS4D_SERV_I~ZZ1_POSID EQ @LF_POSID ) OR
            ( CRMS4D_SERV_I~AC_OBJECT_TYPE EQ '03' AND   "PSPEL
              CRMS4D_SERV_I~AC_ASSIGNMENT EQ @LF_POSID ) )
    ORDER BY CRMS4D_SERV_H~OBJECT_ID DESCENDING,
             CRMS4D_SERV_I~NUMBER_INT DESCENDING.
  ENDSELECT.


  IF SY-SUBRC EQ 0.
    CF_DERIVE_OK = ABAP_TRUE.

    CS_COPA_ITEM-ARTNR     = LS_SERVICE_ORD-PRODUCT_ID.
    CS_COPA_ITEM-KNDNR     = LS_SERVICE_ORD-SOLD_TO_PARTY.
    CS_COPA_ITEM-KUNWE     = LS_SERVICE_ORD-SHIP_TO_PARTY.
    CS_COPA_ITEM-PRODH     = LS_SERVICE_ORD-PROD_HIERARCHY.
    CS_COPA_ITEM-VKBUR     = LS_SERVICE_ORD-SALES_OFFICE_SD.
    CS_COPA_ITEM-VKGRP     = LS_SERVICE_ORD-SALES_GROUP_SD.
    CS_COPA_ITEM-VKORG     = LS_SERVICE_ORD-SALES_ORG_SD.
    CS_COPA_ITEM-VTWEG     = LS_SERVICE_ORD-DIS_CHANNEL.
    CS_COPA_ITEM-ZZ1_MVGR1 = LS_SERVICE_ORD-PRC_GROUP1.

*   Get sale employee from partner responsible person
    SELECT IDNUMBER
      FROM BUT0ID
      INTO @DATA(LF_IDNUMBER)
      UP TO 1 ROWS
      WHERE PARTNER EQ @LS_SERVICE_ORD-PERSON_RESP
      ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF SY-SUBRC EQ 0.
      CS_COPA_ITEM-KMVTNR = LF_IDNUMBER.
    ENDIF.

*   Get address number of ship-to from partner function 55
    IF LS_SERVICE_ORD-SHIP_TO_PARTY IS NOT INITIAL.
      SELECT ADDR_NR
        FROM CRMS4D_PARTNER                             "#EC CI_NOORDER
        WHERE OBJTYPE_H EQ 'BUS2000116'
          AND OBJECT_ID EQ @LS_SERVICE_ORD-OBJECT_ID
           AND NUMBER_INT EQ '000000'              "Service Order Header
           AND PARTNER_FCT EQ '00000055'           "Ship-To Party/ServiceRecipient
         INTO @CF_ADRNR_SHIPTO
        UP TO 1 ROWS.
      ENDSELECT.
      IF SY-SUBRC NE 0.
        CLEAR CF_ADRNR_SHIPTO.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form f_convert_wbs_output
*---------------------------------------------------------------------*
* Conversion WBS to output format
*---------------------------------------------------------------------*
FORM F_CONVERT_WBS_OUTPUT  USING    UF_PSPNR TYPE CE11000-PSPNR
                           CHANGING CF_POSID TYPE PRPS-POSID.

  IF UF_PSPNR IS NOT INITIAL.
    CLEAR: CF_POSID.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
      EXPORTING
        INPUT  = UF_PSPNR
      IMPORTING
        OUTPUT = CF_POSID.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_GET_SERVICE_ORD_INFO
*----------------------------------------------------------------------*
*  Get Service Order Information
*----------------------------------------------------------------------*
FORM F_GET_SERVICE_ORD_INFO  USING  UF_AUFNR  TYPE AUFNR
                           CHANGING CS_SERV   TYPE TS_SERV
                                    CF_ADRNR_WE   TYPE CRMS4D_PARTNER-ADDR_NR.

  CONSTANTS:
    LC_SOLDTO      TYPE CRMT_PARTNER_FCT VALUE '00000001',
    LC_SHIPTO      TYPE CRMT_PARTNER_FCT VALUE '00000055',
    LC_PERSON_RESP TYPE CRMT_PARTNER_FCT VALUE '00000014'.

  DATA:
    LT_GUID       TYPE  CRMT_OBJECT_GUID_TAB,
    LT_PARTNER    TYPE  CRMT_PARTNER_EXTERNAL_WRKT,
    LT_ORDERADM_I TYPE  CRMT_ORDERADM_I_WRKT.

  DATA:
    LS_ORGMAN_WRK TYPE  CRMT_ORGMAN_WRK,
    LS_PRODUCT_I  TYPE CRMT_PRODUCT_I_WRK.       "CH01+

  DATA:
    LF_FNAME        TYPE  CHAR80 VALUE '(SAPLGCC_OR)GS_CALLBACK-CLASS->EXTERNAL_OBJECT->ATTRIBUTES',
    LF_PROCESS_TYPE TYPE  CRMS4D_SERV_H-PROCESS_TYPE,
    LF_OBJTYPE_H    TYPE  CRMS4D_SERV_H-OBJTYPE_H,
    LF_OBJECT_ID    TYPE  CRMS4D_SERV_H-OBJECT_ID,
    LF_NUMBER_INT   TYPE  CRMS4D_SERV_I-NUMBER_INT.

  FIELD-SYMBOLS:
    <L_ATTRIBUTES> TYPE IAOMT_OBJECT_ATTRIBUTE_TAB,
    <L_ATTRIBUTE>  TYPE IAOM_OBJECT_ATTRIBUTE.


* Initialize Output
  CLEAR: CS_SERV,
         CF_ADRNR_WE.

  DO 1 TIMES.
    TRY.
*       Get Attributes
        ASSIGN (LF_FNAME) TO <L_ATTRIBUTES>.
        IF SY-SUBRC NE 0.
*         Cannot read Attributes
          EXIT.
        ENDIF.

      CATCH CX_ROOT ##CATCH_ALL.
        UNASSIGN <L_ATTRIBUTES>.
*       Cannot read Attributes
        EXIT.
    ENDTRY.
  ENDDO.

* -------------------------------
* Read from buffer functions
* -------------------------------
  IF <L_ATTRIBUTES> IS ASSIGNED.
*   Get Object ID
    READ TABLE <L_ATTRIBUTES> ASSIGNING <L_ATTRIBUTE>
                              WITH KEY DATA_ELEMENT = 'CRMT_OBJECT_ID_CO'.
    IF SY-SUBRC EQ 0.
      LF_OBJECT_ID = <L_ATTRIBUTE>-VALUE.
    ENDIF.

*   Get Item No
    READ TABLE <L_ATTRIBUTES> ASSIGNING <L_ATTRIBUTE>
                              WITH KEY DATA_ELEMENT = 'CRMT_ITEM_NO_CO'.
    IF SY-SUBRC EQ 0.
      LF_NUMBER_INT = <L_ATTRIBUTE>-VALUE.
    ENDIF.

*   Get Process Type
    READ TABLE <L_ATTRIBUTES> ASSIGNING <L_ATTRIBUTE>
                              WITH KEY DATA_ELEMENT = 'CRMT_PROCESS_TYPE_CO'.
    IF SY-SUBRC EQ 0.
      LF_PROCESS_TYPE = <L_ATTRIBUTE>-VALUE.
    ENDIF.

*   Get Business Object type
    SELECT SINGLE OBJECT_TYPE
      FROM CRMC_PROC_TYPE
     WHERE PROCESS_TYPE EQ @LF_PROCESS_TYPE
      INTO @LF_OBJTYPE_H.
    IF SY-SUBRC NE 0.
      RETURN.
    ENDIF.

*   Get Object GUID
    CALL FUNCTION 'CRM_ORDERADM_H_GUID_GET_OB'
      EXPORTING
        IV_OBJECT_ID       = LF_OBJECT_ID
        IV_OBJECT_TYPE     = LF_OBJTYPE_H
      IMPORTING
        ET_ORDERADM_H_GUID = LT_GUID
      EXCEPTIONS
        RECORD_NOT_FOUND   = 1
        OTHERS             = 2.
    IF SY-SUBRC <> 0.
      RETURN.
    ENDIF.

    READ TABLE LT_GUID ASSIGNING FIELD-SYMBOL(<L_GUID>)
                       INDEX 1.
    IF SY-SUBRC NE 0.
      RETURN.
    ENDIF.

*   Read ORGMAN Object
    CALL FUNCTION 'CRM_ORGMAN_READ_OB'
      EXPORTING
        IV_REF_GUID          = <L_GUID>
        IV_REF_KIND          = 'A'
      IMPORTING
        ES_ORGMAN_WRK        = LS_ORGMAN_WRK
      EXCEPTIONS
        ENTRY_DOES_NOT_EXIST = 1
        PARAMETER_ERROR      = 2
        OTHERS               = 3.
    IF SY-SUBRC <> 0.
      CLEAR LS_ORGMAN_WRK.
    ENDIF.

*   Read Partner Data
    CALL FUNCTION 'CRM_PARTNER_READ_SINGLE_OB'
      EXPORTING
        IV_REF_GUID                   = <L_GUID>
        IV_REF_KIND                   = 'A'
      IMPORTING
        ET_PARTNER_EXTERNAL_WRK       = LT_PARTNER
      EXCEPTIONS
        ENTRY_DOES_NOT_EXIST          = 1
        RECORD_NOT_FOUND              = 2
        AT_LEAST_ONE_RECORD_NOT_FOUND = 3
        PARAMETER_ERROR               = 4
        OTHERS                        = 5.
    IF SY-SUBRC <> 0.
      CLEAR LT_PARTNER.
    ENDIF.

*   Read Items
    CALL FUNCTION 'CRM_ORDERADM_I_READ_OB'
      EXPORTING
        IV_HEADER           = <L_GUID>
      IMPORTING
        ET_ORDERADM_I_WRK   = LT_ORDERADM_I
      EXCEPTIONS
        ITEM_DOES_NOT_EXIST = 1
        ERROR_OCCURRED      = 2
        OTHERS              = 3.
    IF SY-SUBRC <> 0.
      CLEAR LT_ORDERADM_I.
    ENDIF.

*   Assign Output
    CS_SERV-OBJECT_ID        = LF_OBJECT_ID.
    CS_SERV-PROCESS_TYPE     = LF_PROCESS_TYPE.
    CS_SERV-SALES_ORG_SD     = LS_ORGMAN_WRK-SALES_ORG_SD.
    CS_SERV-DIS_CHANNEL      = LS_ORGMAN_WRK-DIS_CHANNEL.
    CS_SERV-DIVISION         = LS_ORGMAN_WRK-DIVISION.
    CS_SERV-SALES_OFFICE_SD  = LS_ORGMAN_WRK-SALES_OFFICE_SD.
    CS_SERV-SALES_GROUP_SD   = LS_ORGMAN_WRK-SALES_GROUP_SD.
    LOOP AT LT_PARTNER ASSIGNING FIELD-SYMBOL(<L_PARTNER>).
      CASE <L_PARTNER>-PARTNER_FCT.
        WHEN LC_SOLDTO.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = <L_PARTNER>-PARTNER_NO
            IMPORTING
              OUTPUT = CS_SERV-SOLD_TO_PARTY.
        WHEN LC_SHIPTO.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = <L_PARTNER>-PARTNER_NO
            IMPORTING
              OUTPUT = CS_SERV-SHIP_TO_PARTY.
          CF_ADRNR_WE = <L_PARTNER>-ADDR_NR.
        WHEN LC_PERSON_RESP.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = <L_PARTNER>-PARTNER_NO
            IMPORTING
              OUTPUT = CS_SERV-PERSON_RESP.
      ENDCASE.
    ENDLOOP.
    READ TABLE LT_ORDERADM_I ASSIGNING FIELD-SYMBOL(<L_ORDERADM_I>) "#EC CI_SORTSEQ
                             WITH KEY NUMBER_INT = LF_NUMBER_INT.
    IF SY-SUBRC NE 0.
      READ TABLE LT_ORDERADM_I ASSIGNING <L_ORDERADM_I>
                               INDEX 1.
    ENDIF.
    IF SY-SUBRC EQ 0.
      CS_SERV-PRODUCT_ID = <L_ORDERADM_I>-ORDERED_PROD.

* Begin of insertion - CH01 - 420000223 - 03.03.2025
*     Get product hierarchy
      CLEAR: LS_PRODUCT_I.
      CALL FUNCTION 'CRM_PRODUCT_I_READ_OW'
        EXPORTING
          IV_GUID                   = <L_ORDERADM_I>-GUID
        IMPORTING
          ES_PRODUCT_I              = LS_PRODUCT_I
        EXCEPTIONS
          PRODUCT_I_NOT_IN_WORKAREA = 1
          OTHERS                    = 2.

      IF SY-SUBRC EQ 0.
        CS_SERV-PROD_HIERARCHY = LS_PRODUCT_I-PROD_HIERARCHY.
      ENDIF.
* End of insertion - CH01 - 420000223 - 03.03.2025

    ENDIF.

* -------------------------------
* Read From tables
* -------------------------------
  ELSE.
* Begin of deletion - CH01 - 420000223 - 03.03.2025
*    SELECT CRMS4D_SERV_H~OBJECT_ID,
*           CRMS4D_SERV_H~PROCESS_TYPE,
*           CRMS4D_SERV_H~SALES_ORG_SD,
*           CRMS4D_SERV_H~DIS_CHANNEL,
*           CRMS4D_SERV_H~DIVISION,
*           CRMS4D_SERV_H~SALES_OFFICE_SD,
*           CRMS4D_SERV_H~SALES_GROUP_SD,
*           CRMS4D_SERV_H~SOLD_TO_PARTY,
*           CRMS4D_SERV_H~SHIP_TO_PARTY,
*           CRMS4D_SERV_H~PERSON_RESP,
*           CRMS4D_SERV_I~PRODUCT_ID
*      FROM CRMS4D_SERV_H
*             INNER JOIN CRMS4D_SERV_I
*               ON  CRMS4D_SERV_H~AC_ASSIGNMENT EQ CRMS4D_SERV_I~AC_ASSIGNMENT
*               AND CRMS4D_SERV_H~OBJECT_ID EQ CRMS4D_SERV_I~OBJECT_ID
*     WHERE CRMS4D_SERV_H~AC_ASSIGNMENT EQ @UF_AUFNR
*       AND CRMS4D_SERV_H~OBJTYPE_H EQ 'BUS2000116'
*       AND CRMS4D_SERV_H~AC_OBJECT_TYPE EQ '01'
*       AND CRMS4D_SERV_I~AC_ASSIGNMENT EQ @UF_AUFNR
*       AND CRMS4D_SERV_I~OBJTYPE_H EQ 'BUS2000116'
*       AND CRMS4D_SERV_I~AC_OBJECT_TYPE EQ '01'
*     ORDER BY CRMS4D_SERV_H~OBJECT_ID ASCENDING,
*              CRMS4D_SERV_I~NUMBER_INT ASCENDING
*      INTO @CS_SERV
*        UP TO 1 ROWS.
*    ENDSELECT.
* End of deletion - CH01 - 420000223 - 03.03.2025

* Begin of insertion  - CH01 - 420000223 - 03.03.2025
    SELECT CRMS4D_SERV_H~OBJECT_ID,
           CRMS4D_SERV_H~PROCESS_TYPE,
           CRMS4D_SERV_H~SALES_ORG_SD,
           CRMS4D_SERV_H~DIS_CHANNEL,
           CRMS4D_SERV_H~DIVISION,
           CRMS4D_SERV_H~SALES_OFFICE_SD,
           CRMS4D_SERV_H~SALES_GROUP_SD,
           CRMS4D_SERV_H~SOLD_TO_PARTY,
           CRMS4D_SERV_H~SHIP_TO_PARTY,
           CRMS4D_SERV_H~PERSON_RESP,
           CRMS4D_SERV_I~PRODUCT_ID,
           CRMS4D_SERV_I~PROD_HIERARCHY
      FROM CRMS4D_SERV_I
      INNER JOIN CRMS4D_SERV_H
         ON CRMS4D_SERV_I~OBJTYPE_H EQ CRMS4D_SERV_H~OBJTYPE_H
        AND CRMS4D_SERV_I~OBJECT_ID EQ CRMS4D_SERV_H~OBJECT_ID
     WHERE CRMS4D_SERV_I~AC_ASSIGNMENT EQ @UF_AUFNR
       AND CRMS4D_SERV_I~OBJTYPE_H EQ 'BUS2000116'
       AND CRMS4D_SERV_I~AC_OBJECT_TYPE EQ '01'
     ORDER BY CRMS4D_SERV_I~OBJECT_ID ASCENDING,
              CRMS4D_SERV_I~NUMBER_INT ASCENDING
      INTO @CS_SERV
        UP TO 1 ROWS.
    ENDSELECT.
* End of insertion - CH01 - 420000223 - 03.03.2025

    IF SY-SUBRC NE 0.
      RETURN.
    ENDIF.

    IF CS_SERV-SHIP_TO_PARTY IS NOT INITIAL.
      SELECT ADDR_NR
        FROM CRMS4D_PARTNER                             "#EC CI_NOORDER
        WHERE OBJTYPE_H EQ 'BUS2000116'
          AND OBJECT_ID EQ @CS_SERV-OBJECT_ID
           AND NUMBER_INT EQ '000000'              "Service Order Header
           AND PARTNER_FCT EQ '00000055'           "Ship-To Party/ServiceRecipient
         INTO @CF_ADRNR_WE
        UP TO 1 ROWS.
      ENDSELECT.
      IF SY-SUBRC NE 0.
        CLEAR CF_ADRNR_WE.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_DERIVE_FROM_SALESORDER
*---------------------------------------------------------------------*
* Derive COPA Characteristic from sales order
*---------------------------------------------------------------------*
FORM F_DERIVE_FROM_SALESORDER  CHANGING CS_COPA_ITEM TYPE CE11000.
  DATA:
*        LF_PSPNR_0410 TYPE CE11000-PSPNR,                 "CH01-
    LF_ACTIVITY TYPE CHAR5 VALUE '04-10',            "CH01+
    LT_PRPS     TYPE TT_PRPS.                        "CH01+

* Begin of deletion  - CH01 - 420000223 - 03.03.2025
*  PERFORM F_REPLACE_WBS_ACTIVITY USING CS_COPA_ITEM-PSPNR
*                                       '04-10'
*                                       SPACE
*                               CHANGING LF_PSPNR_0410.
*  IF LF_PSPNR_0410 IS INITIAL.
*    RETURN.
*  ENDIF.
*
**------------------------------------------
** Get from sales order of WBS itself: Field:
** 1. Product number,
** 2. Product Hierarchy
** 3. Material Group 1
**------------------------------------------
*  SELECT VBAP~VBELN,
*         VBAP~POSNR,
*         VBAP~MATNR,                                    "#EC CI_NOORDER
*         VBAP~PERVE_ANA,
*         VBAP~KUNNR_ANA,
*         VBAP~KUNWE_ANA,
*         VBAP~PRODH,
*         VBAP~VKBUR_ANA,
*         VBAP~MVGR1,
*         VBAK~VKORG,
*         VBAK~VTWEG,
*         VBAK~VKGRP
*    INTO @DATA(LS_VBAP) UP TO 1 ROWS
*    FROM VBAP
*    INNER JOIN VBAK
*    ON VBAK~VBELN EQ VBAP~VBELN
*    WHERE VBAP~PS_PSP_PNR EQ @CS_COPA_ITEM-PSPNR
*      AND VBAK~VBTYP EQ 'C'.      "Sales Order
*  ENDSELECT.
*  IF SY-SUBRC EQ 0.
*    CS_COPA_ITEM-ARTNR     = LS_VBAP-MATNR.
*    CS_COPA_ITEM-PRODH     = LS_VBAP-PRODH.
*    CS_COPA_ITEM-ZZ1_MVGR1 = LS_VBAP-MVGR1.
*  ENDIF.
* End of deletion  - CH01 - 420000223 - 03.03.2025

*------------------------------------------
* Get from sales order of WBS activity 0410
*------------------------------------------
* Begin of insertion  - CH01 - 420000223 - 03.03.2025
* Get list of WBS where activity = 04-10
  PERFORM F_GET_WBS_LIST_BY_PREFIX USING CS_COPA_ITEM-PSPNR
                                         LF_ACTIVITY   "04-10
                                   CHANGING LT_PRPS.

  IF LT_PRPS IS NOT INITIAL.

    LOOP AT LT_PRPS ASSIGNING FIELD-SYMBOL(<L_PRPS>).
* End of insertion  - CH01 - 420000223 - 03.03.2025
      SELECT VBAP~VBELN,
             VBAP~POSNR,
             VBAP~MATNR,                                "#EC CI_NOORDER
             VBAP~PERVE_ANA,
             VBAP~KUNNR_ANA,
             VBAP~KUNWE_ANA,
             VBAP~PRODH,
             VBAP~VKBUR_ANA,
             VBAP~MVGR1,
             VBAK~VKORG,
             VBAK~VTWEG,
             VBAK~VKGRP
        INTO @DATA(LS_VBAP) UP TO 1 ROWS
        FROM VBAP
        INNER JOIN VBAK
        ON VBAK~VBELN EQ VBAP~VBELN
*        WHERE VBAP~PS_PSP_PNR EQ @LF_PSPNR_0410     "CH01-
        WHERE VBAP~PS_PSP_PNR EQ @<L_PRPS>-PSPNR     "CH01+
          AND VBAK~VBTYP EQ 'C'.      "Sales Order
      ENDSELECT.
      IF SY-SUBRC EQ 0.
        CS_COPA_ITEM-KMVTNR    = LS_VBAP-PERVE_ANA.
        CS_COPA_ITEM-KNDNR     = LS_VBAP-KUNNR_ANA.
        CS_COPA_ITEM-KUNWE     = LS_VBAP-KUNWE_ANA.
        CS_COPA_ITEM-VKBUR     = LS_VBAP-VKBUR_ANA.
        CS_COPA_ITEM-VKGRP     = LS_VBAP-VKGRP.
        CS_COPA_ITEM-VKORG     = LS_VBAP-VKORG.
        CS_COPA_ITEM-VTWEG     = LS_VBAP-VTWEG.

        CS_COPA_ITEM-ARTNR     = LS_VBAP-MATNR.
        CS_COPA_ITEM-PRODH     = LS_VBAP-PRODH.
        CS_COPA_ITEM-ZZ1_MVGR1 = LS_VBAP-MVGR1.

        IF CS_COPA_ITEM-KAUFN IS INITIAL.
          CS_COPA_ITEM-KAUFN = LS_VBAP-VBELN.
          CS_COPA_ITEM-KDPOS = LS_VBAP-POSNR.
        ENDIF.

        EXIT.                                         "CH01+
      ENDIF.
    ENDLOOP.                                          "CH01+
  ENDIF.                                              "CH01+

ENDFORM.
*---------------------------------------------------------------------*
* Form F_REPLACE_WBS_SUBSTITUTION
*---------------------------------------------------------------------*
* Replace WBS activity with the same value PRPS-POST1 of current WBS
* Prefix of WBS will be
* Current WBS: x-x-xxxxxxxx-XX-XX-xxx
* Finding WBS: x-x-xxxxxxxx-01-01-yyy
*---------------------------------------------------------------------*
FORM F_REPLACE_WBS_SUBSTITUTION  USING UF_PSPNR TYPE CE11000-PSPNR
                                       UF_ACTIVITY TYPE CHAR5
                              CHANGING CF_PSPNR TYPE CE11000-PSPNR.
  DATA: LF_TEMP  TYPE PRPS-POSID.

  CLEAR: CF_PSPNR.

  IF UF_PSPNR IS INITIAL.
    RETURN.
  ENDIF.

* Get WBS Detail of WBS itself
  SELECT SINGLE POSID,
                POST1,
                PSPHI
    FROM PRPS
   WHERE PSPNR EQ @UF_PSPNR
    INTO @DATA(LS_PRPS_CURRENT).
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Assign WBS Prefix for searching
  WRITE LS_PRPS_CURRENT-PSPHI TO LF_TEMP.
  CONCATENATE LF_TEMP '-' UF_ACTIVITY '%'
    INTO LF_TEMP.

* Find Substitute WBS
  SELECT PSPNR,
         POSID,
         POST1
    FROM PRPS
    WHERE PSPHI EQ @LS_PRPS_CURRENT-PSPHI
      AND POST1 EQ @LS_PRPS_CURRENT-POST1
      AND POSID LIKE @LF_TEMP
    ORDER BY PRIMARY KEY
    INTO @DATA(LS_PRPS_SUB)
    UP TO 1 ROWS.
  ENDSELECT.
  IF SY-SUBRC EQ 0.
    CF_PSPNR = LS_PRPS_SUB-PSPNR.
  ENDIF.

ENDFORM.
* Begin of insertion - CH01 - 420000223 - 17.02.2025
*---------------------------------------------------------------------*
* Form F_DERIVE_QUOTATION_FROM_WBS
*---------------------------------------------------------------------*
* To get sales data from quotation
* First, get sales quotation of WBS itself
* If not found then get sales quotation of WBS x-x-xxxxxxxx-01-01-yyyy
* where x = same as WBS itself, y = any value
*---------------------------------------------------------------------*
FORM F_DERIVE_QUOTATION_FROM_WBS  USING    UF_PSPNR TYPE CE11000-PSPNR
                                           UF_ACTIVITY TYPE CHAR5
                                  CHANGING CS_COPA_ITEM TYPE CE11000
                                           CF_DERIVE_OK TYPE FLAG.

  DATA: LF_ACTIVITY TYPE CHAR5,
        LT_PRPS     TYPE TT_PRPS.

  CLEAR: CF_DERIVE_OK.

  LF_ACTIVITY = UF_ACTIVITY.

  IF UF_PSPNR IS INITIAL.
    RETURN.
  ENDIF.

* First, get from sales quotation of WBS itself
  PERFORM F_GET_FROM_QUOTATION USING UF_PSPNR
                            CHANGING CS_COPA_ITEM
                                     CF_DERIVE_OK.
  IF CF_DERIVE_OK EQ ABAP_TRUE.
*   If found, then return the derived COPA Characteristics
    RETURN.
  ENDIF.

* If not found then get sales quotation of WBS x-x-xxxxxxxx-01-01-yyyy
  PERFORM F_GET_WBS_LIST_BY_PREFIX USING UF_PSPNR
                                         LF_ACTIVITY
                                   CHANGING LT_PRPS.

  IF LT_PRPS IS NOT INITIAL.
    LOOP AT LT_PRPS ASSIGNING FIELD-SYMBOL(<L_PRPS>).

      PERFORM F_GET_FROM_QUOTATION USING <L_PRPS>-PSPNR
                                CHANGING CS_COPA_ITEM
                                         CF_DERIVE_OK.
      IF CF_DERIVE_OK EQ ABAP_TRUE.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_GET_WBS_LIST_BY_PREFIX
*---------------------------------------------------------------------*
* Get list of WBS where activity = specify activity
*---------------------------------------------------------------------*
FORM F_GET_WBS_LIST_BY_PREFIX  USING    UF_PSPNR TYPE CE11000-PSPNR
                                        UF_ACTIVITY TYPE CHAR5
                               CHANGING CT_PRPS TYPE TT_PRPS.
  DATA: LF_TEMP TYPE PRPS-POSID.

  CLEAR: CT_PRPS.

  IF UF_PSPNR IS INITIAL OR
     UF_ACTIVITY IS INITIAL.
    RETURN.
  ENDIF.

* Get WBS Detail of WBS itself
  SELECT SINGLE POSID,
                POST1,
                PSPHI
    FROM PRPS
    WHERE PSPNR EQ @UF_PSPNR
    INTO @DATA(LS_PRPS_CURRENT).

  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Assign WBS Prefix for searching
  WRITE LS_PRPS_CURRENT-PSPHI TO LF_TEMP.
  CONCATENATE LF_TEMP '-' UF_ACTIVITY '%'
    INTO LF_TEMP.

* Find list of WBS which activity = UF_ACTIVITY
  SELECT PSPNR,
         POSID,
         POST1
    FROM PRPS
    WHERE PSPHI EQ @LS_PRPS_CURRENT-PSPHI
      AND POSID LIKE @LF_TEMP
    ORDER BY PRIMARY KEY
    INTO TABLE @DATA(LT_PRPS).

  IF SY-SUBRC EQ 0.
    CT_PRPS[] = LT_PRPS[].
  ENDIF.

ENDFORM.
* End of insertion - CH01 - 420000223 - 17.02.2025
