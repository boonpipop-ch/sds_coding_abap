*-----------------------------------------------------------------------
*  Program ID         : Z_SDSCO_KEDR_Z002
*  Creation Date      : 06.06.2024
*  Author             : Nadtaya / Waraporn (Eviden)
*  Add-on ID          : COPAE001
*  Description        : Derive characteristic for normal service
*                       (Order <> Blank)
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  03.03.2025  F36K913347  Waraporn S. Ticket#420000223
*                                      Search key: CH01
*                                      Add to derive PROD_HIERARCHY
*                                      from service order item data
*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
FUNCTION Z_SDSCO_KEDR_Z002 ##FM_NO_TYPE .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_OPERATING_CONCERN) LIKE  TKEB-ERKRS
*"     VALUE(I_DERIVATION_DATE) LIKE  SY-DATUM
*"     VALUE(I_STEP_ID) LIKE  TKEDRS-STEPID
*"     VALUE(I_COPA_ITEM)
*"     VALUE(I_GLOBAL) LIKE  KEDRCOPA STRUCTURE  KEDRCOPA
*"  EXPORTING
*"     REFERENCE(E_COPA_ITEM)
*"     REFERENCE(E_GLOBAL)
*"     REFERENCE(E_EXIT_IS_ACTIVE)
*"     REFERENCE(E_FAILED)
*"----------------------------------------------------------------------
  DATA: LS_COPA_ITEM  TYPE CE11000,
        LT_HREMPLOYEE TYPE HREMPLOYEET,
        LS_SERV       TYPE TS_SERV,
        LF_ADRNR_WE   TYPE CRMS4D_PARTNER-ADDR_NR.

*..Set export param
  E_COPA_ITEM      = I_COPA_ITEM.
  E_GLOBAL         = I_GLOBAL.
  E_EXIT_IS_ACTIVE = ABAP_TRUE.
  E_FAILED         = ABAP_FALSE.

  MOVE-CORRESPONDING I_COPA_ITEM TO LS_COPA_ITEM.

* Get Service Order Data
  PERFORM F_GET_SERVICE_ORD_INFO  USING  LS_COPA_ITEM-RKAUFNR
                                CHANGING LS_SERV
                                         LF_ADRNR_WE.
  IF LS_SERV IS INITIAL.
    E_FAILED = ABAP_TRUE.
    RETURN.
  ENDIF.

  ASSIGN ('E_GLOBAL-USERTEMP8') TO FIELD-SYMBOL(<L_USERTEMP8>).
  IF SY-SUBRC EQ 0.
    <L_USERTEMP8> = LS_SERV-PROCESS_TYPE.
  ENDIF.

  LS_COPA_ITEM-KNDNR  = LS_SERV-SOLD_TO_PARTY.
  LS_COPA_ITEM-KUNWE  = LS_SERV-SHIP_TO_PARTY.
  LS_COPA_ITEM-ARTNR  = LS_SERV-PRODUCT_ID.
  LS_COPA_ITEM-VKBUR  = LS_SERV-SALES_OFFICE_SD.
  LS_COPA_ITEM-VKGRP  = LS_SERV-SALES_GROUP_SD.
  LS_COPA_ITEM-VKORG  = LS_SERV-SALES_ORG_SD.
  LS_COPA_ITEM-VTWEG  = LS_SERV-DIS_CHANNEL.

  LS_COPA_ITEM-PRODH = LS_SERV-PROD_HIERARCHY.    "CH01+

  IF LS_SERV-PERSON_RESP IS NOT INITIAL.
    SELECT SINGLE PARTNER_GUID INTO @DATA(LF_PARTNER_GUID)
      FROM BUT000
      WHERE PARTNER EQ @LS_SERV-PERSON_RESP.
    IF SY-SUBRC EQ 0.
      CALL FUNCTION 'HR_CENTRALPERSON_GET_NUMBERS'
        EXPORTING
          IV_BU_PARTNER_GUID  = LF_PARTNER_GUID
        IMPORTING
          ET_EMPLOYEE_ID      = LT_HREMPLOYEE
        EXCEPTIONS
          NO_CENTRAL_PERSON   = 1
          NO_BUSINESS_PARTNER = 2
          NO_ID               = 3
          OTHERS              = 4.
      IF SY-SUBRC EQ 0.
        READ TABLE LT_HREMPLOYEE INTO DATA(LS_HREMPLOYEE) INDEX 1.
        IF SY-SUBRC EQ 0.
          LS_COPA_ITEM-KMVTNR = LS_HREMPLOYEE-EMPLOYEEID.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDIF.

*...Update address no. of Ship-to
  IF LF_ADRNR_WE IS NOT INITIAL.
    ASSIGN ('E_GLOBAL-USERTEMP5') TO FIELD-SYMBOL(<L_USERTEMP5>).
    IF SY-SUBRC EQ 0.
      <L_USERTEMP5> = LF_ADRNR_WE.
    ENDIF.
  ENDIF.

  MOVE-CORRESPONDING LS_COPA_ITEM TO E_COPA_ITEM.

ENDFUNCTION.                                             "#EC CI_VALPAR
