*&---------------------------------------------------------------------*
*& Include          LZSDSFI08F01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Function module : Z_SDSFI_POSTING_INTF_CLR_PART
* Function  Desc  : COPY FM POSTING_INTERFACE_CLEARING
*                   AND ADD MORE BDC STEPS FOR PARTIAL CLEARING
*                   ->Additional part will be marked with CH00
* Start Date      : 07.06.2024
* Developer       : Apichat C.
* SAP Version     : S/4 HANA
*----------------------------------------------------------------------*
* Modification History
***********************************************************************
* Author        :
* Date          :
* Change Request:
* Transport no. :
* Search term   : CH01
* Description   :
***********************************************************************

***INCLUDE LFIPIF00 .
* BOI CH00+
*&---------------------------------------------------------------------*
*& Form F_PARTIAL_CLEARING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_partial_clearing .
  DATA lv_lineno TYPE numc2 ##NEEDED.
  DATA lv_have_wht TYPE c.
  DATA lv_first    TYPE c.

  CLEAR ft.
  ft-fnam     = 'BDC_OKCODE'.
  ft-fval     = 'PA'.
  APPEND ft.
  CLEAR ft.


*\\\\\\\\\\\\\\\\\\\\ Start Withholding tax \\\\\\\\\\\\\\\\
  ft-program  = 'SAPDF05X'.
  ft-dynpro   = '3100'.
  ft-dynbegin = 'X'.
  APPEND ft.
  CLEAR ft.
  LOOP AT t_partial WHERE wt_entered IS NOT INITIAL.
    lv_have_wht = 'X'.

    IF lv_first IS INITIAL.
      lv_first = 'X'.
      ft-fnam     = 'BDC_OKCODE'.
      ft-fval     = '=WITH'.
      APPEND ft.
      CLEAR ft.
    ENDIF.

    CLEAR lv_lineno.
    ft-program  = 'SAPDF05X'.
    ft-dynpro   = '3100'.
    ft-dynbegin = 'X'.
    APPEND ft.
    CLEAR ft.
*>Fix bug add finding position (filter Document no.)
    READ TABLE t_ftclear INDEX t_partial-line_no.
    ft-fnam     = 'BDC_OKCODE'.
    ft-fval     = '=OSU'.               "Field content search
    APPEND ft.
    CLEAR ft.

*----------------------------
*   Search by Document Nnumber
*----------------------------
    ft-program  = 'SAPDF05X'.
    ft-dynpro   = '2000'.               "Select search criteria
    ft-dynbegin = 'X'.
    APPEND ft.
    CLEAR ft.
    ft-fnam     = 'RF05A-XPOS1(01)'.    "Unselect Document Date
    ft-fval     = ' '.
    APPEND ft.
    CLEAR ft.
    ft-fnam     = 'RF05A-XPOS1(03)'.    "Select Document Number
    ft-fval     = 'X'.
    APPEND ft.
    CLEAR ft.
    ft-fnam     = 'BDC_OKCODE'.
    ft-fval     = '=GO'.
    APPEND ft.
    CLEAR ft.
    ft-program  = 'SAPDF05X'.
    ft-dynpro   = '0731'.                   "Search for Document Number
    ft-dynbegin = 'X'.
    APPEND ft.
    CLEAR ft.
    ft-fnam     = 'RF05A-SEL01(01)'.        "Document from
    ft-fval     = t_ftclear-selvon+0(10).  "Document Number with Partial payment
    APPEND ft.
    CLEAR ft.
    ft-fnam     = 'BDC_OKCODE'.
    ft-fval     = '=GO'.
    APPEND ft.
    CLEAR ft.
    ft-program  = 'SAPDF05X'.
    ft-dynpro   = '3100'.
    ft-dynbegin = 'X'.
    APPEND ft.
    CLEAR ft.
    ft-fnam     = 'BDC_OKCODE'.
    ft-fval     = '=OSU'.               "Field content search
    APPEND ft.
    CLEAR ft.
*----------------------------
*   Search by Document Item
*----------------------------
    ft-program  = 'SAPDF05X'.
    ft-dynpro   = '2000'.               "Select search criteria
    ft-dynbegin = 'X'.
    APPEND ft.
    CLEAR ft.
    ft-fnam     = 'RF05A-XPOS1(01)'.    "Unselect Document Date
    ft-fval     = ' '.
    APPEND ft.
    CLEAR ft.
    ft-fnam     = 'RF05A-XPOS1(10)'.    "Select Document Item
    ft-fval     = 'X'.
    APPEND ft.
    CLEAR ft.
    ft-fnam     = 'BDC_OKCODE'.
    ft-fval     = '=GO'.
    APPEND ft.
    CLEAR ft.
    ft-program  = 'SAPDF05X'.
    ft-dynpro   = '0731'.                   "Search for Document Item
    ft-dynbegin = 'X'.
    APPEND ft.
    CLEAR ft.
    ft-fnam     = 'RF05A-SEL01(01)'.        "Document Item from
    ft-fval     = t_ftclear-selvon+14(3).  "Document Item with Partial payment
    APPEND ft.
    CLEAR ft.
    ft-fnam     = 'BDC_OKCODE'.
    ft-fval     = '=GO'.
    APPEND ft.
    CLEAR ft.
    ft-program  = 'SAPDF05X'.
    ft-dynpro   = '3100'.
    ft-dynbegin = 'X'.
    APPEND ft.
    CLEAR ft.
*<End of finding position of document no.
    ft-fnam     = 'BDC_CURSOR'.
    ft-fval     = 'DF05B-PSQST(01)'.
    APPEND ft.
    CLEAR ft.
    ft-fnam     = 'BDC_OKCODE'.
    ft-fval     = '=PI'.
    APPEND ft.
    CLEAR ft.
    ft-program  = 'SAPLFWTD'.
    ft-dynpro   = '0100'.
    ft-dynbegin = 'X'.
    APPEND ft.
    CLEAR ft.
    ft-fnam     = 'WITH_DIALG-WT_AMNTGVN(01)'.
    ft-fval     = t_partial-wt_entered.
    APPEND ft.
    CLEAR ft.
    ft-fnam     = 'BDC_OKCODE'.
    ft-fval     = '=GO'.
    APPEND ft.
    CLEAR ft.
****Add OKCODE BACK , so that filter document will be refreshed
    ft-program  = 'SAPDF05X'.
    ft-dynpro   = '3100'.
    ft-dynbegin = 'X'.
    APPEND ft.
    CLEAR ft.
    ft-fnam     = 'BDC_OKCODE'.
    ft-fval     = '=OSE'.               "Press BACK
    APPEND ft.
    CLEAR ft.
****Add OKCODE BACK , so that filter document will be refreshed
    ft-program  = 'SAPDF05X'.
    ft-dynpro   = '3100'.
    ft-dynbegin = 'X'.
    APPEND ft.
    CLEAR ft.
    ft-fnam     = 'BDC_OKCODE'.
    ft-fval     = '=OSE'.               "Press BACK
    APPEND ft.
    CLEAR ft.
  ENDLOOP.

  IF lv_have_wht = 'X'.
    ft-fnam     = 'BDC_OKCODE'.
    ft-fval     = '=OSE'.               "Press BACK
    APPEND ft.
    CLEAR ft.

    ft-program  = 'SAPDF05X'.
    ft-dynpro   = '3100'.
    ft-dynbegin = 'X'.
    APPEND ft.
    CLEAR ft.

  ENDIF.

  ft-fnam     = 'BDC_OKCODE'.
  ft-fval     = '/00'.
  APPEND ft.
  CLEAR ft.


*\\\\\\\\\\\\\\\\\\\\ Start Tab Partial \\\\\\\\\\\\\\\\

  ft-program  = 'SAPDF05X'.
  ft-dynpro   = '3100'.
  ft-dynbegin = 'X'.
  APPEND ft.
  CLEAR ft.

  ft-fnam     = 'BDC_OKCODE'.
  ft-fval     = '=PART'.
  APPEND ft.
  CLEAR ft.
*  ft-program  = 'SAPDF05X'.
*  ft-dynpro   = '3100'.
*  ft-dynbegin = 'X'.
*  APPEND ft.
*  CLEAR ft.
  LOOP AT t_partial WHERE payment_amount IS NOT INITIAL.
    ft-program  = 'SAPDF05X'.
    ft-dynpro   = '3100'.
    ft-dynbegin = 'X'.
    APPEND ft.
    CLEAR ft.

    READ TABLE t_ftclear INDEX t_partial-line_no. "Clear item has been resorted!
*>Fix bug add finding position
    ft-fnam     = 'BDC_OKCODE'.
    ft-fval     = '=OSU'.               "Field content search
    APPEND ft.
    CLEAR ft.

*----------------------------
*   Search by Document No
*----------------------------
    ft-program  = 'SAPDF05X'.
    ft-dynpro   = '2000'.               "Select search criteria
    ft-dynbegin = 'X'.
    APPEND ft.
    CLEAR ft.
    ft-fnam     = 'RF05A-XPOS1(01)'.    "Unselect Document Date
    ft-fval     = ' '.
    APPEND ft.
    CLEAR ft.
    ft-fnam     = 'RF05A-XPOS1(03)'.    "Select Document Number
    ft-fval     = 'X'.
    APPEND ft.
    CLEAR ft.
    ft-fnam     = 'BDC_OKCODE'.
    ft-fval     = '=GO'.
    APPEND ft.
    CLEAR ft.
    ft-program  = 'SAPDF05X'.
    ft-dynpro   = '0731'.               "Search for Document Number
    ft-dynbegin = 'X'.
    APPEND ft.
    CLEAR ft.
    ft-fnam     = 'RF05A-SEL01(01)'.        "Document from
    ft-fval     = t_ftclear-selvon+0(10).  "Document number with Partial payment
    APPEND ft.
    CLEAR ft.
    ft-fnam     = 'BDC_OKCODE'.
    ft-fval     = '=GO'.
    APPEND ft.
    CLEAR ft.
*    ft-program  = 'SAPDF05X'.
*    ft-dynpro   = '3100'.
*    ft-dynbegin = 'X'.
*    APPEND ft.
*    CLEAR ft.
*    ft-fnam     = 'BDC_OKCODE'.
*    ft-fval     = '=OSU'.               "Field content search
*    APPEND ft.
*    CLEAR ft.
**----------------------------
**   Search by Document Item
**----------------------------
*    ft-program  = 'SAPDF05X'.
*    ft-dynpro   = '2000'.               "Select search criteria
*    ft-dynbegin = 'X'.
*    APPEND ft.
*    CLEAR ft.
*    ft-fnam     = 'RF05A-XPOS1(01)'.    "Unselect Document Date
*    ft-fval     = ' '.
*    APPEND ft.
*    CLEAR ft.
*    ft-fnam     = 'RF05A-XPOS1(10)'.    "Select Line Item
*    ft-fval     = 'X'.
*    APPEND ft.
*    CLEAR ft.
*    ft-fnam     = 'BDC_OKCODE'.
*    ft-fval     = '=GO'.
*    APPEND ft.
*    CLEAR ft.
*    ft-program  = 'SAPDF05X'.
*    ft-dynpro   = '0731'.               "Search for Item No
*    ft-dynbegin = 'X'.
*    APPEND ft.
*    CLEAR ft.
*    ft-fnam     = 'RF05A-SEL01(01)'.
*    ft-fval     = t_ftclear-selvon+14(3).  "Line item with Partial payment
*    APPEND ft.
*    CLEAR ft.
*    ft-fnam     = 'BDC_OKCODE'.
*    ft-fval     = '=GO'.
*    APPEND ft.
*    CLEAR ft.
    ft-program  = 'SAPDF05X'.
    ft-dynpro   = '3100'.
    ft-dynbegin = 'X'.
    APPEND ft.
    CLEAR ft.
    ft-fnam       = 'DF05B-PSZAH(01)'.
    ft-fval       = t_partial-payment_amount.
    APPEND ft.
    CLEAR: ft, lv_lineno.
****Add OKCODE BACK , so that filter document will be refreshed
    ft-fnam     = 'BDC_OKCODE'.
    ft-fval     = '=OSE'.               "Press BACK
    APPEND ft.
    CLEAR ft.
****Add OKCODE BACK , so that filter document will be refreshed
    ft-program  = 'SAPDF05X'.
    ft-dynpro   = '3100'.
    ft-dynbegin = 'X'.
    APPEND ft.
    CLEAR ft.
*    ft-fnam     = 'BDC_OKCODE'.
*    ft-fval     = '=OSE'.               "Press BACK
*    APPEND ft.
*    CLEAR ft.
*    ft-program  = 'SAPDF05X'.
*    ft-dynpro   = '3100'.
*    ft-dynbegin = 'X'.
*    APPEND ft.
*    CLEAR ft.
  ENDLOOP.

  ft-fnam     = 'BDC_OKCODE'.
  ft-fval     = '=OSE'.               "Press BACK
  APPEND ft.
  CLEAR ft.

*\\\\\\\\\\\\\\\\\\\\ End Tab Partial\\\\\\\\\\\\\\\\

*  ft-program  = 'SAPDF05X'.
*  ft-dynpro   = '3100'.
*  ft-dynbegin = 'X'.
*  APPEND ft.
*  CLEAR ft.

  ft-program  = 'SAPMF05A'.
  ft-dynpro   = '0700'.
  ft-dynbegin = 'X'.
  APPEND ft.
  CLEAR ft.

  DESCRIBE TABLE ft LINES index.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_adj_fcode
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_adj_fcode .
  DATA:
    lv_okcode TYPE char01,
    ls_add_ft LIKE LINE OF ft.


  LOOP AT ft INTO DATA(ls_ft).
    IF ls_ft-dynbegin = 'X' AND
       sy-tabix <> 1.
      IF lv_okcode IS INITIAL.
        CLEAR ls_add_ft.
        ls_add_ft-fnam = 'BDC_OKCODE'.
        ls_add_ft-fval = '/00'.
        INSERT ls_add_ft INTO ft INDEX sy-tabix.
      ENDIF.
      CLEAR lv_okcode.
    ENDIF.
    IF ls_ft-fnam = 'BDC_OKCODE'.
      lv_okcode = 'X'.
    ENDIF.
  ENDLOOP.
ENDFORM.

* EOI CH00+
*&---------------------------------------------------------------------*
*& Form f_adjust_fb01
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- FT
*&---------------------------------------------------------------------*
FORM f_adjust_fb01 .

  CHECK tcode EQ 'FB01'.

  READ TABLE t_ftpost
    TRANSPORTING NO FIELDS
    WITH KEY
      stype = 'P'
      fnam = 'RF05A-NEWUM'
      fval = 'D'.
  IF sy-subrc EQ 0.
    READ TABLE ft
      INTO DATA(ls_ft)
      WITH KEY program = 'SAPLFWTD'.
    IF sy-subrc EQ 0.
      DATA(lv_from) = sy-tabix.
      LOOP AT ft INTO ls_ft FROM lv_from.
        IF ls_ft-program = 'SAPLFWTD' OR
           ls_ft-program IS INITIAL.
          DELETE ft INDEX sy-tabix.
        ELSE.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.

* BOI CH01+

*&---------------------------------------------------------------------*
*& Form f_accept_popup
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_accept_popup .

  "Adjust last ft line
  READ TABLE ft
    INDEX lines( ft[] ).
  IF sy-subrc = 0.
    IF ft-program  = 'SAPMF05A' AND
       ft-dynpro   = '0700'.
      DELETE ft INDEX lines( ft[] ).
    ENDIF.
  ENDIF.

  ft-program  = 'SAPMF05A'.
  ft-dynpro   = '0614'.
  ft-dynbegin = 'X'.
  APPEND ft.
  CLEAR ft.

  ft-fnam     = 'BDC_OKCODE'.
  ft-fval     = 'AKZP'.               "Press BACK
  APPEND ft.
  CLEAR ft.

  ft-program  = 'SAPMF05A'.
  ft-dynpro   = '0700'.
  ft-dynbegin = 'X'.
  APPEND ft.
  CLEAR ft.

ENDFORM.

* EOI CH01+
