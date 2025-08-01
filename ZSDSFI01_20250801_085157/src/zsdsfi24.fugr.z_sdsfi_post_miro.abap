FUNCTION Z_SDSFI_POST_MIRO.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(GV_POST_DT) LIKE  BKPF-BUDAT
*"     REFERENCE(GV_DOC_DT) LIKE  BKPF-BLDAT
*"     REFERENCE(GV_REF_DOC_NO) TYPE  XBLNR OPTIONAL
*"     REFERENCE(GV_VENDOR) TYPE  LIFNR
*"     REFERENCE(GV_ITEM_TEXT) TYPE  BKTXT OPTIONAL
*"  EXPORTING
*"     REFERENCE(GV_OUTPUT) TYPE  BELNR_D
*"     REFERENCE(GV_MESSAGE) TYPE  CHAR255
*"  TABLES
*"      GT_DETAIL STRUCTURE  ZSDSFIS193
*"      GT_RETURN STRUCTURE  BAPIRETURN OPTIONAL
*"----------------------------------------------------------------------

  DATA : GS_DETAIL LIKE LINE OF GT_DETAIL.

  DATA : LS_HEADERDATA  TYPE  BAPI_INCINV_CREATE_HEADER,
         LS_ADDRESSDATA TYPE  BAPI_INCINV_CREATE_ADDRESSDATA,
         LS_OILDATA     TYPE  BAPI_INCINV_CREATE_OIL.

  DATA : LS_INVOICEDOCNUMBER TYPE BAPI_INCINV_FLD-INV_DOC_NO,
         LS_FISCALYEAR       TYPE BAPI_INCINV_FLD-FISC_YEAR.

  DATA : LT_ITEMDATA            TYPE TABLE OF BAPI_INCINV_CREATE_ITEM,
         LT_ACCOUNTINGDATA      TYPE TABLE OF BAPI_INCINV_CREATE_ACCOUNT,
         LT_GLACCOUNTDATA	      TYPE TABLE OF BAPI_INCINV_CREATE_GL_ACCOUNT,
         LT_MATERIALDATA        TYPE TABLE OF BAPI_INCINV_CREATE_MATERIAL,
         LT_TAXDATA	            TYPE TABLE OF BAPI_INCINV_CREATE_TAX,
         LT_WITHTAXDATA	        TYPE TABLE OF BAPI_INCINV_CREATE_WITHTAX,
         LT_VENDORITEMSPLITDATA	TYPE TABLE OF BAPI_INCINV_CREATE_VENDORSPLIT,
         LT_RETURN              TYPE TABLE OF BAPIRET2,
         LT_NFMETALLITMS        TYPE TABLE OF /NFM/BAPIDOCITM.

  DATA : LS_ITEMDATA            LIKE LINE OF LT_ITEMDATA,
         LS_TAXDATA             LIKE LINE OF LT_TAXDATA,
         LS_VENDORITEMSPLITDATA LIKE LINE OF LT_VENDORITEMSPLITDATA,
         LS_GLACCOUNTDATA	      LIKE LINE OF LT_GLACCOUNTDATA,
         LS_WITHTAXDATA	        LIKE LINE OF LT_WITHTAXDATA,
         LS_ACCOUNTINGDATA      LIKE LINE OF LT_ACCOUNTINGDATA.

  DATA : LV_RUNING TYPE I.

  DATA : LV_GROSS LIKE LS_HEADERDATA-GROSS_AMOUNT.

  DATA : LV_AMT LIKE LS_HEADERDATA-GROSS_AMOUNT.

  DATA : LV_IVAT LIKE GS_DETAIL-MWSBP,
         LV_DVAT LIKE GS_DETAIL-MWSBP.

  DATA : BEGIN OF LS_MSEG,
           MBLNR TYPE MKPF-MBLNR,
           MJAHR TYPE MKPF-MJAHR,
           ZEILE TYPE MSEG-ZEILE,
           XBLNR TYPE MKPF-XBLNR,
           MENGE TYPE MSEG-MENGE,
           MEINS TYPE MSEG-MEINS,
           EBELN TYPE MSEG-EBELN,
           EBELP TYPE MSEG-EBELP,
         END OF LS_MSEG.
  DATA LT_MSEG LIKE TABLE OF LS_MSEG.

  DATA : BEGIN OF LS_ESSR,
           LBLNI TYPE ESSR-LBLNI,
           EBELN TYPE ESSR-EBELN,
           EBELP TYPE ESSR-EBELP,
         END OF LS_ESSR.
  DATA LT_ESSR LIKE TABLE OF LS_ESSR.

  DATA : BEGIN OF LS_EKBE,
           EBELN TYPE EKBE-EBELN,
           EBELP TYPE EKBE-EBELP,
           LFBNR TYPE EKBE-LFBNR,
           LFGJA TYPE EKBE-LFGJA,
           LFPOS TYPE EKBE-LFPOS,
           MEINS TYPE EKPO-MEINS,
         END OF LS_EKBE.
  DATA LT_EKBE LIKE TABLE OF LS_EKBE.

  DATA : BEGIN OF LS_EKKN,
           EBELN TYPE EKKN-EBELN,
           EBELP TYPE EKKN-EBELP,
           SAKTO TYPE EKKN-SAKTO,
           KOKRS TYPE EKKN-KOKRS,
           VBELN TYPE EKKN-VBELN,
           VBELP TYPE EKKN-VBELP,
           VETEN TYPE EKKN-VETEN,
         END OF LS_EKKN.
  DATA LT_EKKN LIKE TABLE OF LS_EKKN.

  DATA : LS_TMP LIKE GS_DETAIL.

  DATA : LV_VAT     TYPE C,
         LV_TAXCODE LIKE LS_ITEMDATA-TAX_CODE.

  DATA LV_EBELN TYPE EKKO-EBELN.

  IF GT_DETAIL[] IS NOT INITIAL.
    SELECT MKPF~MBLNR
           MKPF~MJAHR
           MSEG~ZEILE
           MKPF~XBLNR
           MSEG~MENGE
           MSEG~ERFME
           MSEG~EBELN
           MSEG~EBELP
      FROM MSEG
      INNER JOIN MKPF ON MSEG~MBLNR EQ MKPF~MBLNR AND
                         MSEG~MJAHR EQ MKPF~MJAHR
      INTO TABLE LT_MSEG
      FOR ALL ENTRIES IN GT_DETAIL
      WHERE EBELN EQ GT_DETAIL-EBELN
        AND EBELP EQ GT_DETAIL-EBELP.

    SELECT LBLNI
           EBELN
           EBELP
      FROM ESSR
      INTO TABLE LT_ESSR
      FOR ALL ENTRIES IN GT_DETAIL
      WHERE EBELN EQ GT_DETAIL-EBELN
        AND EBELP EQ GT_DETAIL-EBELP.

    SELECT EKBE~EBELN
           EKBE~EBELP
           EKBE~LFBNR
           EKBE~LFGJA
           EKBE~LFPOS
*           ekpo~meins
      FROM EKBE
*      INNER JOIN ekpo ON ekbe~ebeln EQ ekpo~ebeln AND
*                         ekbe~ebelp EQ ekpo~ebelp
      INTO TABLE LT_EKBE
      FOR ALL ENTRIES IN GT_DETAIL
      WHERE EKBE~EBELN EQ GT_DETAIL-EBELN
        AND EKBE~EBELP EQ GT_DETAIL-EBELP.

*    SELECT ebeln
*           ebelp
*           sakto
*           kokrs
*           vbeln
*           vbelp
*           veten
*      FROM ekkn
*      INTO TABLE lt_ekkn
*      FOR ALL ENTRIES IN gt_detail
*      WHERE ebeln EQ gt_detail-ebeln
*        AND ebelp EQ gt_detail-ebelp.
  ENDIF.

*  READ TABLE gt_detail INTO ls_tmp INDEX 1.
*
**--------------------------------------------------------------------*
** Header
**--------------------------------------------------------------------*
  LS_HEADERDATA-INVOICE_IND  = 'X'.
  LS_HEADERDATA-DOC_TYPE     = 'RE'.
  LS_HEADERDATA-DOC_DATE     = GV_DOC_DT.
  LS_HEADERDATA-PSTNG_DATE   = GV_POST_DT.
  LS_HEADERDATA-REF_DOC_NO   = GV_REF_DOC_NO.
  LS_HEADERDATA-COMP_CODE    = '1000'.
  LS_HEADERDATA-DIFF_INV     = GV_VENDOR.
  LS_HEADERDATA-CURRENCY     = 'THB'.
  LS_HEADERDATA-CURRENCY_ISO = 'THB'.
  LS_HEADERDATA-EXCH_RATE    = 1.
  LS_HEADERDATA-ITEM_TEXT    = GV_ITEM_TEXT.
**  ls_headerdata-dsct_days1   = gs_header-payment_terms.
**  ls_headerdata-header_txt   = gs_header-bktxt.
  LS_HEADERDATA-BLINE_DATE   = GV_DOC_DT.
*--------------------------------------------------------------------*
* Item
*--------------------------------------------------------------------*
  CLEAR : LV_VAT,LV_VAT,LV_DVAT.
  LOOP AT GT_DETAIL INTO GS_DETAIL.

    ADD 1 TO LV_RUNING.
    LS_ITEMDATA-INVOICE_DOC_ITEM = LV_RUNING.
    LS_ITEMDATA-PO_NUMBER        = GS_DETAIL-EBELN.
    LS_ITEMDATA-PO_ITEM          = GS_DETAIL-EBELP.


    READ TABLE LT_MSEG INTO LS_MSEG
    WITH KEY EBELN = GS_DETAIL-EBELN
             EBELP = GS_DETAIL-EBELP.
    IF SY-SUBRC = 0.
      LS_ITEMDATA-REF_DOC          = LS_MSEG-MBLNR.
      LS_ITEMDATA-REF_DOC_YEAR     = LS_MSEG-MJAHR.
      LS_ITEMDATA-REF_DOC_IT       = LS_MSEG-ZEILE.
*      ls_itemdata-ref_doc_no       = ls_mseg-xblnr.
      LS_ITEMDATA-PO_UNIT          = LS_MSEG-MEINS.
    ENDIF.
*    lv_amt = gs_detail-dmbtr.
    IF GS_DETAIL-FLAGV EQ 'X'.
      LS_ITEMDATA-TAX_CODE         = 'I7'.
*      lv_amt                       = ( gs_detail-dmbtr * 100 ) / 107.
      LV_AMT                       = GS_DETAIL-DMBTR.
*      ls_headerdata-calc_tax_ind   = 'X'.
*      ls_itemdata-item_amount      = gs_detail-dmbtr.
      LS_ITEMDATA-ITEM_AMOUNT      = GS_DETAIL-DMBTR." + ( ( gs_detail-dmbtr * 7 ) / 100 ).
      ADD GS_DETAIL-MWSBP TO LV_IVAT.
      LV_VAT     = 'X'.
      LV_TAXCODE = LS_ITEMDATA-TAX_CODE.
    ELSEIF GS_DETAIL-FLAGV EQ 'Y'.
      LS_ITEMDATA-TAX_CODE         = 'D7'.
*      lv_amt                       = ( gs_detail-dmbtr * 100 ) / 107.
      LV_AMT                       = GS_DETAIL-DMBTR.
*      ls_headerdata-calc_tax_ind   = 'X'.
*      ls_itemdata-item_amount      = gs_detail-dmbtr.
      LS_ITEMDATA-ITEM_AMOUNT      = GS_DETAIL-DMBTR." + ( ( gs_detail-dmbtr * 7 ) / 100 ).
      ADD GS_DETAIL-MWSBP TO LV_DVAT.
      LV_VAT     = 'X'.
      LV_TAXCODE = LS_ITEMDATA-TAX_CODE.
    ELSE.
      LS_ITEMDATA-TAX_CODE         = 'IX'.
      LV_AMT                       = GS_DETAIL-DMBTR.
      LS_ITEMDATA-ITEM_AMOUNT      = GS_DETAIL-DMBTR.
      LV_TAXCODE                   = LS_ITEMDATA-TAX_CODE.
      CLEAR : LV_VAT.
    ENDIF.
    LS_ITEMDATA-QUANTITY         = GS_DETAIL-MENGE.

    IF LS_ITEMDATA-QUANTITY LE 0.
      LS_ITEMDATA-QUANTITY = 1.
    ENDIF.

    ADD LV_AMT TO LV_GROSS.
    LS_HEADERDATA-GROSS_AMOUNT   = LV_GROSS." * ls_itemdata-quantity.

    READ TABLE LT_ESSR INTO LS_ESSR
    WITH KEY EBELN = GS_DETAIL-EBELN
             EBELP = GS_DETAIL-EBELP.
    IF SY-SUBRC EQ 0.
      LS_ITEMDATA-SHEET_NO   = LS_ESSR-LBLNI.
      CLEAR : LS_ITEMDATA-QUANTITY,LS_ITEMDATA-PO_UNIT.
*      ls_itemdata-sheet_item = lv_runing.
*      READ TABLE lt_ekkn INTO ls_ekkn
*      WITH KEY ebeln = gs_detail-ebeln
*               ebelp = gs_detail-ebelp.
*      IF sy-subrc EQ 0.
*        ls_accountingdata-invoice_doc_item = ls_itemdata-invoice_doc_item.
*        ls_accountingdata-co_area          = ls_ekkn-kokrs.
*        ls_accountingdata-gl_account       = ls_ekkn-sakto.
*        ls_accountingdata-quantity         = ls_itemdata-quantity.
*        ls_accountingdata-po_unit          = ls_itemdata-po_unit.
*        ls_accountingdata-sd_doc           = ls_ekkn-vbeln.
*        ls_accountingdata-sdoc_item        = ls_ekkn-vbelp.
*        APPEND ls_accountingdata TO lt_accountingdata.
*    ENDIF.
*      CLEAR : ls_accountingdata.
    ELSE.
      CLEAR : LS_ITEMDATA-SHEET_NO,LS_ITEMDATA-SHEET_ITEM.
    ENDIF.

    READ TABLE LT_EKBE INTO LS_EKBE
    WITH KEY EBELN = GS_DETAIL-EBELN
             EBELP = GS_DETAIL-EBELP.
    IF SY-SUBRC EQ 0.
      LS_ITEMDATA-REF_DOC	      = LS_EKBE-LFBNR.
      LS_ITEMDATA-REF_DOC_YEAR  = LS_EKBE-LFGJA.
      LS_ITEMDATA-REF_DOC_IT    = LS_EKBE-LFPOS.
*      ls_itemdata-po_unit       = ls_ekbe-meins.
    ELSE.
      CLEAR : LS_ITEMDATA-REF_DOC,LS_ITEMDATA-REF_DOC_YEAR,LS_ITEMDATA-REF_DOC_IT.
    ENDIF.


*    lv_ebeln = gs_detail-ebeln.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*      EXPORTING
*        input  = lv_ebeln
*      IMPORTING
*        output = lv_ebeln.

    APPEND LS_ITEMDATA TO LT_ITEMDATA.
    CLEAR LV_AMT.


  ENDLOOP.
*--------------------------------------------------------------------*
* Tax
*--------------------------------------------------------------------*
**  IF gs_detail-flagv EQ 'X'.
*  IF lv_ivat IS NOT INITIAL.
*    ls_taxdata-tax_code   = 'I7'.
*    ls_taxdata-tax_amount = lv_ivat." * 100."gs_detail-dmbtr - ls_headerdata-gross_amount.
*    APPEND ls_taxdata TO lt_taxdata.
*  ENDIF.
**  ELSEIF gs_detail-flagv EQ 'Y'.
*  IF lv_dvat IS NOT INITIAL.
*    ls_taxdata-tax_code   = 'D7'.
*    ls_taxdata-tax_amount = lv_dvat." * 100."gs_detail-dmbtr - ls_headerdata-gross_amount.
*    APPEND ls_taxdata TO lt_taxdata.
*  ENDIF.
**  ENDIF.
  IF GS_DETAIL-FLAGV EQ 'X'.
    LS_TAXDATA-TAX_CODE        = 'I7'.
    LS_TAXDATA-TAX_AMOUNT      = GS_DETAIL-MWSBP." * 100."gs_detail-dmbtr - ls_headerdata-gross_amount.
    LS_TAXDATA-TAX_BASE_AMOUNT = GS_DETAIL-DMBTR.
    APPEND LS_TAXDATA TO LT_TAXDATA.
  ELSEIF GS_DETAIL-FLAGV EQ 'Y'.
    LS_TAXDATA-TAX_CODE        = 'D7'.
    LS_TAXDATA-TAX_AMOUNT      = GS_DETAIL-MWSBP." * 100."gs_detail-dmbtr - ls_headerdata-gross_amount.
    LS_TAXDATA-TAX_BASE_AMOUNT = GS_DETAIL-DMBTR.
    APPEND LS_TAXDATA TO LT_TAXDATA.
  ENDIF.
*--------------------------------------------------------------------*
* vendoritemsplitdata
*--------------------------------------------------------------------*
  IF LV_VAT EQ 'X'.
    LS_VENDORITEMSPLITDATA-SPLIT_KEY    = 1.
    LS_VENDORITEMSPLITDATA-SPLIT_AMOUNT = LS_HEADERDATA-GROSS_AMOUNT +  LS_TAXDATA-TAX_AMOUNT.
    LS_VENDORITEMSPLITDATA-TAX_CODE     = LV_TAXCODE.
    "( ( ls_headerdata-gross_amount * 7 ) / 100 ).
  ELSE.
    LS_VENDORITEMSPLITDATA-SPLIT_KEY    = 1.
    LS_VENDORITEMSPLITDATA-SPLIT_AMOUNT = LS_HEADERDATA-GROSS_AMOUNT." + ls_taxdata-tax_amount.
    LS_VENDORITEMSPLITDATA-TAX_CODE     = LV_TAXCODE.
  ENDIF.
  APPEND LS_VENDORITEMSPLITDATA TO LT_VENDORITEMSPLITDATA.
*--------------------------------------------------------------------*
* WHT
*--------------------------------------------------------------------*
  LS_WITHTAXDATA-SPLIT_KEY    = LS_VENDORITEMSPLITDATA-SPLIT_KEY.
  SELECT SINGLE WITHT
                WT_WITHCD
    FROM LFBW
    INTO (LS_WITHTAXDATA-WI_TAX_TYPE,LS_WITHTAXDATA-WI_TAX_CODE)
    WHERE LIFNR EQ GV_VENDOR
      AND BUKRS EQ '1000'.

  APPEND LS_WITHTAXDATA TO LT_WITHTAXDATA.

*  ls_withtaxdata-WI_TAX_TYPE
*  ls_withtaxdata-WI_TAX_CODE
*  ls_withtaxdata-WI_TAX_BASE
*  ls_withtaxdata-WI_TAX_AMT
*  ls_withtaxdata-WI_TAX_WITHHELD_AMT
*--------------------------------------------------------------------*
* Post
*--------------------------------------------------------------------*
  IF LV_VAT EQ 'X'.
    LS_HEADERDATA-GROSS_AMOUNT = LS_HEADERDATA-GROSS_AMOUNT + LS_TAXDATA-TAX_AMOUNT."( ( ls_headerdata-gross_amount * 7 ) / 100 ).
  ELSE.
    LS_HEADERDATA-GROSS_AMOUNT = LS_HEADERDATA-GROSS_AMOUNT." + ls_taxdata-tax_amount.
  ENDIF.

  CLEAR : LV_GROSS,LV_RUNING.
  CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE'
    EXPORTING
      HEADERDATA          = LS_HEADERDATA
      ADDRESSDATA         = LS_ADDRESSDATA
    IMPORTING
      INVOICEDOCNUMBER    = LS_INVOICEDOCNUMBER
      FISCALYEAR          = LS_FISCALYEAR
    TABLES
      ITEMDATA            = LT_ITEMDATA
      ACCOUNTINGDATA      = LT_ACCOUNTINGDATA
      GLACCOUNTDATA       = LT_GLACCOUNTDATA
      MATERIALDATA        = LT_MATERIALDATA
      TAXDATA             = LT_TAXDATA
      WITHTAXDATA         = LT_WITHTAXDATA
      VENDORITEMSPLITDATA = LT_VENDORITEMSPLITDATA
      RETURN              = LT_RETURN
    EXCEPTIONS
      ERROR_MESSAGE       = 1
      OTHERS              = 2.

  IF LS_INVOICEDOCNUMBER IS NOT INITIAL.
    PERFORM F_COMMIT.

    GV_OUTPUT  = LS_INVOICEDOCNUMBER.
    GV_MESSAGE = TEXT-101.

  ELSE.
    PERFORM F_GET_MESSAGE TABLES LT_RETURN
                        CHANGING GV_MESSAGE.
  ENDIF.

  CLEAR : LT_ITEMDATA,LT_ACCOUNTINGDATA,LT_GLACCOUNTDATA,LT_MATERIALDATA,
          LT_TAXDATA,LT_WITHTAXDATA,LT_VENDORITEMSPLITDATA,LT_RETURN,
          LS_TAXDATA,LS_VENDORITEMSPLITDATA.

  CLEAR : LS_HEADERDATA,LS_FISCALYEAR,LS_INVOICEDOCNUMBER,LS_ADDRESSDATA.




ENDFUNCTION.
