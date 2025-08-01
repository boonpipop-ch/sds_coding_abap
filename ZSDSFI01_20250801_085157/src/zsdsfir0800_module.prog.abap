*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0800_MODULE
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0101_EXIT INPUT.

  CASE SY-UCOMM.
    WHEN 'EXIT'.
      LEAVE PROGRAM  .
    WHEN 'CANCEL'.
      LEAVE PROGRAM   .
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0101_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0101 INPUT.
  CASE  SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'PRINT'.
      LCL_DATA=>PRINT( ).
    WHEN 'DEL_LINE'.
      LCL_DATA=>DEL_LINE( ).
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0101 OUTPUT.
  SET PF-STATUS 'Z_101'.
*  SET TITLEBAR 'xxx'.Z

ENDMODULE.                 " STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  M_GET_DOCUMENT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_GET_DOCUMENT INPUT.

*  SELECT VBRK~VBELN,
*         VBRK~KUNAG,
*         ADRC~NAME1,
*         ADRC~STREET,
*         ADRC~CITY2,
*         ADRC~CITY1,
*         ADRC~POST_CODE1
*    FROM VBRK
*    INNER JOIN KNA1 ON VBRK~KUNAG EQ KNA1~KUNNR
**                 AND KNA1~LAND1 EQ 'TH'
*    INNER JOIN ADRC ON KNA1~ADRNR EQ ADRC~ADDRNUMBER
**                 AND ADRC~NATION NE 'i'
*    WHERE VBRK~VBELN = @GS_HEADER-DOC_NUMBER
*    INTO TABLE @DATA(LT_TMP).
  DATA : LV_NATION TYPE ADRC-NATION.

  DATA : LV_LANGU TYPE SY-LANGU.

  IF GS_HEADER-LANGUAGE EQ 'TH'.
    LV_NATION = SPACE.
  ELSE.
    LV_NATION = 'I'.
  ENDIF.



  SELECT SINGLE VBRK~VBELN,
         VBRK~FKDAT,
         VBRK~KUNAG,
         ADRC~NAME1,
         ADRC~STREET,
         ADRC~CITY2,
         ADRC~CITY1,
         ADRC~POST_CODE1,
         ADRC~NAME2,
         ADRC~NAME3,
         ADRC~NAME4
*         VBRP~MWSBP
    FROM VBRK
*    INNER JOIN VBRP ON VBRK~VBELN EQ VBRP~VBELN
    INNER JOIN KNA1 ON VBRK~KUNAG EQ KNA1~KUNNR
    INNER JOIN ADRC ON KNA1~ADRNR  EQ ADRC~ADDRNUMBER
                   AND ADRC~NATION EQ @LV_NATION
    WHERE VBRK~VBELN = @GS_HEADER-DOC_NUMBER
    INTO @DATA(LS_TMP).

  GS_HEADER-DATE              = LS_TMP-FKDAT.
*  GS_HEADER-CUSTOMER          = LS_TMP-NAME1.

  CONCATENATE LS_TMP-NAME1 LS_TMP-NAME2 INTO GS_HEADER-CUSTOMER SEPARATED BY SPACE.

  GS_HEADER-ADRESS1           = LS_TMP-STREET.
  GS_HEADER-ADRESS2           = LS_TMP-CITY2.
  GS_HEADER-ADRESS3           = LS_TMP-CITY1.
  GS_HEADER-ADRESS4           = LS_TMP-POST_CODE1.
*  GS_HEADER-LANGUAGE          = 'TH'.
  GS_HEADER-INVOICE_TAX       = ' '.
  GS_HEADER-INVOICE_DATE      = ' '.
  GS_HEADER-CUST_CODE         = LS_TMP-KUNAG.
  GS_HEADER-REMARK1           = ' '.
  GS_HEADER-REMARK2           = ' '.
  GS_HEADER-REMARK3           = ' '.
  GS_HEADER-REMARK4           = ' '.
  GS_HEADER-TOTAL_TEXT        = ' '.
  GS_HEADER-PRICE_ORG_INV     = ' '.
  GS_HEADER-ADV_REC           = ' '.
  GS_HEADER-TOTAL_AMT         = ' '.
  GS_HEADER-PRICE_DUC         = ' '.
*  GS_HEADER-VAL_ADDTAX        = LS_TMP-MWSBP.
*  GS_HEADER-NET_TOTAL         = ' '.
*   GS_HEADER-TOTAL_AMT         = ' '.
*   GS_HEADER-PRICE_DUC         = ' '.
*  GS_HEADER-CUSTOMER2          = LS_TMP-NAME3.
    CONCATENATE LS_TMP-NAME3 LS_TMP-NAME4 INTO GS_HEADER-CUSTOMER2 SEPARATED BY SPACE.



*  CLEAR : GT_HEADER[].
*  LOOP AT  LT_TMP INTO LS_tmp.
*    GS_HEADER-CUSTOMER          = LS_TMP-NAME1.
*    GS_HEADER-ADRESS1           = LS_TMP-STREET.
*    GS_HEADER-ADRESS2           = LS_TMP-CITY2.
*    GS_HEADER-ADRESS3           = LS_TMP-CITY1.
*    GS_HEADER-ADRESS4           = LS_TMP-POST_CODE1.
*    GS_HEADER-DATE              = ' '.
*    GS_HEADER-LANGUAGE          = 'TH'.
*    GS_HEADER-INVOICE_TAX       = ' '.
*    GS_HEADER-INVOICE_DATE      = ' '.
*    GS_HEADER-CUST_CODE         = LS_TMP-KUNAG.
*    GS_HEADER-REMARK1           = ' '.
*    GS_HEADER-REMARK2           = ' '.
*    GS_HEADER-REMARK3           = ' '.
*    GS_HEADER-REMARK4           = ' '.
*    GS_HEADER-TOTAL_TEXT        = ' '.
*    GS_HEADER-PRICE_ORG_INV     = ' '.
*    GS_HEADER-ADV_REC           = ' '.
*    GS_HEADER-TOTAL_AMT         = ' '.
*    GS_HEADER-PRICE_DUC         = ' '.
*    GS_HEADER-VAL_ADDTAX        = ' '.
*    GS_HEADER-NET_TOTAL         = ' '.
*
*    APPEND GS_HEADER TO Gt_HEADER.
*  ENDLOOP.


  SELECT VBRP~POSNR,
         VBRP~MATNR,
         VBRP~ARKTX,
         VBRP~FKLMG,
         VBRP~KZWI1,
         VBRP~NETWR,
         VBRP~MWSBP
   FROM VBRP
   WHERE VBRP~VBELN = @GS_HEADER-DOC_NUMBER
   INTO TABLE @DATA(LT_TMP2).

*  DATA LV_TABIX TYPE SY-TABIX.
  DATA LS_TMP2 LIKE LINE OF LT_TMP2.

  CLEAR : GT_DETAIL[].
  CLEAR : GS_HEADER-NET_TOTAL,GS_HEADER-VAL_ADDTAX.


  LOOP AT LT_TMP2 INTO LS_TMP2.
    GS_DETAIL-ITEM          = LS_TMP2-POSNR.
    GS_DETAIL-MODEL         = LS_TMP2-MATNR.
    GS_DETAIL-DES           = LS_TMP2-ARKTX.
    GS_DETAIL-QTY           = LS_TMP2-FKLMG.
*    GS_DETAIL-UNIT_PRICE    = LS_TMP2-KZWI1.
*    GS_DETAIL-DISCOUNT =
*    GS_DETAIL-NET_UNIT      = ''.
    GS_DETAIL-NET_AMOUNT    = LS_TMP2-NETWR.


    GS_DETAIL-UNIT_PRICE  = LS_TMP2-KZWI1 / LS_TMP2-FKLMG.
*    GS_DETAIL-DISCOUNT    = ( GS_DETAIL-NET_AMOUNT - GS_DETAIL-UNIT_PRICE ) * GS_DETAIL-QTY.


    GS_DETAIL-DISCOUNT    = ( LS_TMP2-KZWI1 - LS_TMP2-NETWR ) / LS_TMP2-FKLMG.

    GS_DETAIL-NET_UNIT    = GS_DETAIL-UNIT_PRICE - GS_DETAIL-DISCOUNT .
    GS_DETAIL-VAT_TMP     = LS_TMP2-MWSBP .

    APPEND GS_DETAIL TO GT_DETAIL.



*GS_HEADER-NET_TOTAL = LS_TMP2-NETWR + GS_HEADER-VAL_ADDTAX.

    ADD LS_TMP2-NETWR TO GS_HEADER-NET_TOTAL.
    ADD LS_TMP2-MWSBP TO GS_HEADER-VAL_ADDTAX.

*APPEND  LS_TMP2-NETWR +  GS_HEADER-VAL_ADDTAX  TO GS_HEADER-NET_TOTAL.

  ENDLOOP.


  GS_HEADER-NET_TOTAL = GS_HEADER-NET_TOTAL + GS_HEADER-VAL_ADDTAX.


*  GS_HEADER-NET_TOTAL = GS_HEADER-NET_TOTAL + VAT.

  IF GS_HEADER-LANGUAGE EQ 'TH'.
    LV_LANGU = '2'.
  ELSE.
    LV_LANGU = 'E'.
  ENDIF.

  CALL FUNCTION 'Z_SDSCA_SPELL_AMOUNT'
    EXPORTING
      AMOUNT     = GS_HEADER-NET_TOTAL
      CURRENCY   = 'THB'
      LANGUAGE   = LV_LANGU
    IMPORTING
      SPELL_WORD = GS_HEADER-TOTAL_TEXT
    EXCEPTIONS
      NOT_FOUND  = 1
      TOO_LARGE  = 2
      OTHERS     = 3.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.




*GS_HEADER-ADRESS1 = '123123'.
*
*GS_DETAIL-ITEM = 'a '.
*GS_DETAIL-MODEL = ' b'.
*GS_DETAIL-DES = ' c'.
*GS_DETAIL-QTY = 'd '.
*GS_DETAIL-UNIT_PRICE = ' e'.
*GS_DETAIL-DISCOUNT = 'f'.
*GS_DETAIL-NET_UNIT = ' g'.




*  CONCATENATE 'aaaa' 'bbbb' 'cccc' INTO GS_HEADER-CUSTOMER SEPARATED BY SPACE.

*  GS_HEADER-ADRESS1 = '123123'.
*  GS_HEADER-ADRESS2 = 'asdsad'.
*
*  CONCATENATE GS_HEADER-ADRESS1 GS_HEADER-ADRESS2 INTO GS_HEADER-CUSTOMER SEPARATED BY SPACE.



*   GS_HEADER-CUSTOMER = '123213'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  M_UPDATE_DETAIL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_UPDATE_DETAIL INPUT.
  MODIFY GT_DETAIL FROM GS_DETAIL INDEX TC_DETAIL-CURRENT_LINE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  M_CAL_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_CAL_DATA INPUT.

  GS_DETAIL-NET_AMOUNT = GS_DETAIL-QTY * ( GS_DETAIL-UNIT_PRICE  - GS_DETAIL-DISCOUNT ).
  GS_DETAIL-NET_UNIT   = GS_DETAIL-UNIT_PRICE - GS_DETAIL-DISCOUNT.





ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  M_CLEAR_SUM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_CLEAR_SUM INPUT.
  CLEAR : GS_HEADER-NET_TOTAL, GS_HEADER-VAL_ADDTAX.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  M_SUM_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_SUM_DATA INPUT.

  ADD GS_DETAIL-NET_AMOUNT TO  GS_HEADER-NET_TOTAL.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  M_CAL_VAT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE M_CAL_VAT INPUT.
  GS_HEADER-VAL_ADDTAX = GS_HEADER-NET_TOTAL * 7 / 100.
  GS_HEADER-NET_TOTAL = GS_HEADER-VAL_ADDTAX + GS_HEADER-NET_TOTAL.

  IF GS_HEADER-LANGUAGE EQ 'TH'.
    LV_LANGU = '2'.
  ELSE.
    LV_LANGU = 'E'.
  ENDIF.

  CALL FUNCTION 'Z_SDSCA_SPELL_AMOUNT'
    EXPORTING
      AMOUNT     = GS_HEADER-NET_TOTAL
      CURRENCY   = 'THB'
      LANGUAGE   = LV_LANGU
    IMPORTING
      SPELL_WORD = GS_HEADER-TOTAL_TEXT
    EXCEPTIONS
      NOT_FOUND  = 1
      TOO_LARGE  = 2
      OTHERS     = 3.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.


ENDMODULE.
