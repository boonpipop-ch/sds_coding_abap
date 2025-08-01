*-----------------------------------------------------------------------
*  Program ID         : Z_SDSCO_KEDR_Z001
*  Creation Date      : 06.06.2024
*  Author             : Nadtaya / Waraporn (Eviden)
*  Add-on ID          : COPAE001
*  Description        : Derive characteristic for project sales
*                       (WBS <> Blank)
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  17.02.2025  F36K912598  Waraporn S. Ticket#420000223
*                                      Search key: CH01
*                                      To fix case derive from quotation
*                                      in F_DERIVE_QUOTATION_FROM_WBS
*-----------------------------------------------------------------------
*  31.07.2025  420000714   Wuthichai L. - Fix logic checking delete status
*-----------------------------------------------------------------------
FUNCTION Z_SDSCO_KEDR_Z001 ##FM_NO_TYPE .
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
*Project Sales: Project Type(1,2)=”1” use Quoation:
*Project Overhaul: Project Type(1,2)=”2”: No Sale Quotation use Sales Order

  DATA: LS_COPA_ITEM    TYPE CE11000,
        LF_POSID        TYPE PRPS-POSID,
        LF_ADRNR_SHIPTO TYPE CRMS4D_PARTNER-ADDR_NR,
*        LF_PSPNR_0101   TYPE CE11000-PSPNR,             "CH01-
        LF_DERIVE_OK    TYPE FLAG.

*..Set export param
  E_COPA_ITEM      = I_COPA_ITEM.
  E_GLOBAL         = I_GLOBAL.
  E_EXIT_IS_ACTIVE = ABAP_TRUE.
  E_FAILED         = ABAP_FALSE.

  MOVE-CORRESPONDING I_COPA_ITEM TO LS_COPA_ITEM.

  CASE LS_COPA_ITEM-ZZ1_PROJTYPE_MSE.
    WHEN '1'.                 "Project Type 1
*..Get sales data from Quotation
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
        WHERE VBAP~PS_PSP_PNR EQ @LS_COPA_ITEM-PSPNR
          AND VBAP~ABGRU EQ @SPACE
          AND VBAK~VBTYP EQ 'B'.
      ENDSELECT.
      IF SY-SUBRC NE 0.
        IF LS_COPA_ITEM-ZZ1_ACTTYPE_MSE(2) = '05'.

* Begin of deletion - CH01 - 420000223 - 17.02.2025
*          PERFORM F_REPLACE_WBS_ACTIVITY USING LS_COPA_ITEM-PSPNR
*                                               '01-01'
*                                               '-001'
*                                       CHANGING LF_PSPNR_0101.
*
*          IF LF_PSPNR_0101 IS INITIAL.
*            E_FAILED = ABAP_TRUE.
*            RETURN.
*          ENDIF.
*
*          PERFORM F_GET_FROM_QUOTATION USING LF_PSPNR_0101
**                                             LF_PSPNR_0102
*                                    CHANGING LS_COPA_ITEM
*                                             LF_DERIVE_OK.
* End of deletion - CH01 - 420000223 - 17.02.2025

* Begin of insertion - CH01 - 420000223 - 17.02.2025
* First, to derive from sales quotation of WBS itself
* If not found, derive from sales quotation of WBS x-x-xxxxxxxx-01-01-yyy
          PERFORM F_DERIVE_QUOTATION_FROM_WBS USING LS_COPA_ITEM-PSPNR
                                                    '01-01'
                                           CHANGING LS_COPA_ITEM
                                                    LF_DERIVE_OK.

          IF LF_DERIVE_OK EQ ABAP_FALSE.
* Derive from sales quotation of WBS x-x-xxxxxxxx-04-yy-zzz
            PERFORM F_DERIVE_QUOTATION_FROM_WBS USING LS_COPA_ITEM-PSPNR
                                                      '04'
                                             CHANGING LS_COPA_ITEM
                                                      LF_DERIVE_OK.
          ENDIF.
* End of insertion - CH01 - 420000223 - 17.02.2025

          IF LF_DERIVE_OK EQ ABAP_TRUE.
            CLEAR: E_FAILED.
          ENDIF.

        ELSE.    "Activity Type <> 05
          PERFORM F_DERIVE_BY_ACTIVITY_TYPE USING LS_COPA_ITEM-ZZ1_PROJTYPE_MSE
                                         CHANGING LS_COPA_ITEM
                                                  LF_ADRNR_SHIPTO
                                                  E_FAILED.
        ENDIF.

        IF E_FAILED EQ ABAP_TRUE.
          RETURN.
        ENDIF.
      ELSE.
        LS_COPA_ITEM-ARTNR     = LS_VBAP-MATNR.
        LS_COPA_ITEM-KMVTNR    = LS_VBAP-PERVE_ANA.
        LS_COPA_ITEM-KNDNR     = LS_VBAP-KUNNR_ANA.
        LS_COPA_ITEM-KUNWE     = LS_VBAP-KUNWE_ANA.
        LS_COPA_ITEM-PRODH     = LS_VBAP-PRODH.
        LS_COPA_ITEM-VKBUR     = LS_VBAP-VKBUR_ANA.
        LS_COPA_ITEM-VKGRP     = LS_VBAP-VKGRP.
        LS_COPA_ITEM-VKORG     = LS_VBAP-VKORG.
        LS_COPA_ITEM-VTWEG     = LS_VBAP-VTWEG.
        LS_COPA_ITEM-ZZ1_MVGR1 = LS_VBAP-MVGR1.

        IF LS_COPA_ITEM-KAUFN IS INITIAL.
          LS_COPA_ITEM-KAUFN = LS_VBAP-VBELN.
          LS_COPA_ITEM-KDPOS = LS_VBAP-POSNR.
        ENDIF.
      ENDIF.

    WHEN '2'.               "Project Type 2
*..Get sales data from Standard
      IF LS_COPA_ITEM-ZZ1_ACTTYPE_MSE(2) = '05'.
        PERFORM F_DERIVE_FROM_SALESORDER CHANGING LS_COPA_ITEM.

      ELSE.
        PERFORM F_DERIVE_BY_ACTIVITY_TYPE USING LS_COPA_ITEM-ZZ1_PROJTYPE_MSE
                                          CHANGING LS_COPA_ITEM
                                                   LF_ADRNR_SHIPTO
                                                   E_FAILED.
        IF E_FAILED EQ ABAP_TRUE.
          RETURN.
        ENDIF.
      ENDIF.
    WHEN OTHERS.

  ENDCASE.

*...Get Transaction type from Service order
  IF LS_COPA_ITEM-ZZ1_ITMTYPE_MSE NE '05'.

    PERFORM F_CONVERT_WBS_OUTPUT USING LS_COPA_ITEM-PSPNR
                                 CHANGING LF_POSID.

    SELECT CRMS4D_SERV_H~PROCESS_TYPE
      FROM CRMS4D_SERV_H                                "#EC CI_NOORDER
      WHERE OBJTYPE_H EQ 'BUS2000116'
        AND AC_OBJECT_TYPE EQ '03'
        AND AC_ASSIGNMENT EQ @LF_POSID
      INTO @DATA(LV_PROCESS_TYPE)
      UP TO 1 ROWS.
    ENDSELECT.
    IF SY-SUBRC EQ 0.

      ASSIGN ('E_GLOBAL-USERTEMP8') TO FIELD-SYMBOL(<L_USERTEMP8>).
      IF SY-SUBRC EQ 0.
        <L_USERTEMP8> = LV_PROCESS_TYPE.
      ENDIF.

    ENDIF.
  ENDIF.

*...Update address no. of Ship-to
  IF LF_ADRNR_SHIPTO IS NOT INITIAL.
*   Set address no. of Ship-to from service order header partner function = 55
    ASSIGN ('E_GLOBAL-USERTEMP5') TO FIELD-SYMBOL(<L_USERTEMP5>).
    IF SY-SUBRC EQ 0.
      <L_USERTEMP5> = LF_ADRNR_SHIPTO.
    ENDIF.

  ELSEIF LS_COPA_ITEM-KUNWE IS NOT INITIAL.
*   Set address no. of Ship-to from sales order partner
    SELECT ADRNR
      FROM VBPA                                         "#EC CI_NOORDER
      WHERE VBELN EQ @LS_COPA_ITEM-KAUFN
        AND ( POSNR EQ @LS_COPA_ITEM-KDPOS OR POSNR EQ '000000' )
        AND PARVW EQ 'WE'
      INTO @DATA(LF_ADRNR_WE)
      UP TO 1 ROWS.
    ENDSELECT.
    IF SY-SUBRC EQ 0.
      ASSIGN ('E_GLOBAL-USERTEMP5') TO <L_USERTEMP5>.
      IF SY-SUBRC EQ 0.
        <L_USERTEMP5> = LF_ADRNR_WE.
      ENDIF.
    ENDIF.
  ENDIF.

*..Get Employee
  SELECT PERNR
    FROM VBPA                                           "#EC CI_NOORDER
    WHERE VBELN EQ @LS_COPA_ITEM-KAUFN
      AND ( POSNR EQ @LS_COPA_ITEM-KDPOS OR POSNR EQ '000000' )
      AND PARVW EQ 'VE'
    INTO @DATA(LF_PERNR_VE)
    UP TO 1 ROWS.
  ENDSELECT.
  IF SY-SUBRC EQ 0.
    LS_COPA_ITEM-KMVTNR = LF_PERNR_VE.
  ENDIF.

  MOVE-CORRESPONDING LS_COPA_ITEM TO E_COPA_ITEM.

ENDFUNCTION.                                             "#EC CI_VALPAR
