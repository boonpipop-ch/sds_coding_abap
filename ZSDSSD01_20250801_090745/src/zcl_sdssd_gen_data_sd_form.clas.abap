class ZCL_SDSSD_GEN_DATA_SD_FORM definition
  public
  final
  create public .

public section.

  methods MAPPING_DATA_RECEIPT
    importing
      !I_BILL_INFO type LBBIL_INVOICE
      value(I_NATION) type ADRC-NATION optional
    exporting
      !E_DATA type ZSDSFIS005
      !ET_DATA type ZSDSFIS006_TT .
  methods MAPPING_DATA_PICKING
    importing
      !IS_PICKING_INFO type LEDLV_DELNOTE
      value(I_NATION) type ADRC-NATION optional
      value(I_CHECK_NO_SERIAL) type FLAG optional
    exporting
      !ES_DATA type ZSDSSDS036
      !ET_DETAIL type ZSDSSDS037_TT .
  methods GEN_PDF .
  methods GET_LINE_ITEM_WARRANTY
    importing
      value(I_DO) type LIKP-VBELN optional
    changing
      value(CT_DATA) type TT_VBELN optional
    returning
      value(RT_RETURN) type ZSDSSDS113_TT .
  methods MAPPING_DATA_INV
    importing
      !I_BILL_INFO type LBBIL_INVOICE
      value(I_NATION) type ADRC-NATION optional
    exporting
      !E_DATA type ZSDSFIS005
      !ET_DATA type ZSDSFIS006_TT .
  methods GEN_QRCODE
    importing
      value(I_TAX_ID_RECEIVER) type ANY optional
      value(I_REF1) type ANY optional
      value(I_REF2) type ANY optional
    returning
      value(R_QR) type CHAR255 .
  methods GEN_BARCODE
    importing
      value(I_TAX_ID_RECEIVER) type ANY optional
      value(I_REF1) type ANY optional
      value(I_REF2) type ANY optional
    returning
      value(R_BARCODE) type CHAR255 .
  methods GEN_BARCODE_DESC
    importing
      value(I_TAX_ID_RECEIVER) type ANY optional
      value(I_REF1) type ANY optional
      value(I_REF2) type ANY optional
    returning
      value(R_BARCODE) type CHAR255 .
protected section.
PRIVATE SECTION.

  CONSTANTS:
    BEGIN OF GC_CON,
      I                    TYPE C LENGTH 1     VALUE 'I',
      PARTN_ROLE           TYPE C LENGTH 2     VALUE 'VE',
      AG                   TYPE C LENGTH 2     VALUE 'AG',
      WE                   TYPE C LENGTH 2     VALUE 'WE',
      RE                   TYPE C LENGTH 2     VALUE 'RE',
      REMARK_ID            TYPE THEAD-TDID     VALUE 'ZH09',
      REMARK_OBJECT        TYPE THEAD-TDOBJECT VALUE 'VBBK',
      THB                  TYPE C LENGTH 3     VALUE 'THB',
      ZZZZ                 TYPE C LENGTH 4     VALUE 'ZZZZ',
      DO_REMARK_ID         TYPE THEAD-TDID     VALUE 'ZH10',
      DO_REMARK_OBJECT     TYPE THEAD-TDOBJECT VALUE 'VBBK',
      DO_INV_REMARK_ID     TYPE THEAD-TDID     VALUE 'ZH09',
      DO_INV_REMARK_OBJECT TYPE THEAD-TDOBJECT VALUE 'VBBK',
      DO_PROJECT_ID        TYPE THEAD-TDID     VALUE 'ZH06',
      DO_PROJECT_OBJECT    TYPE THEAD-TDOBJECT VALUE 'VBBK',
      DO_CN_REMARK         TYPE THEAD-TDID     VALUE 'ZH13',
      DO_CONTACT_PERSON    TYPE THEAD-TDID     VALUE 'ZH19',
      DO_LAND_ID           TYPE THEAD-TDID     VALUE 'ZH11',
      INV_PROJECT_ID       TYPE THEAD-TDID     VALUE 'ZH06',
      INV_PROJECT_OBJECT   TYPE THEAD-TDOBJECT VALUE 'VBBK',
      INV_LAND_ID          TYPE THEAD-TDID     VALUE 'ZH11',
      INV_LAND_OBJECT      TYPE THEAD-TDOBJECT VALUE 'VBBK',
      CHECK_SUM            TYPE THEAD-TDID     VALUE 'ZH16',    "Additional Text
      ITEM_DETAIL          TYPE THEAD-TDID     VALUE 'ZI06',
      ITEM_OBJECT          TYPE THEAD-TDOBJECT VALUE 'VBBP',
      COMMENT              TYPE THEAD-TDID     VALUE 'ZH09',
      THAI                 TYPE SY-LANGU       VALUE '2',
      ENG                  TYPE SY-LANGU       VALUE 'E',
    END OF GC_CON .
ENDCLASS.



CLASS ZCL_SDSSD_GEN_DATA_SD_FORM IMPLEMENTATION.


  METHOD GEN_BARCODE.
    DATA : LV_BARCODE TYPE C LENGTH 255.

    DATA : LV_TAX TYPE C LENGTH 255.

    IF I_TAX_ID_RECEIVER IS INITIAL.
      ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = 'ZSDSSDR0100'
                                                    I_PARAM             = 'BARCODE'
                                                    I_PARAM_EXT         = 'TAX_ID'
                                                    I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                          CHANGING  C_RETURN            = LV_TAX ).
    ELSE.
      LV_TAX = I_TAX_ID_RECEIVER.
    ENDIF.

    CONCATENATE '|' LV_TAX INTO LV_BARCODE.
    CONCATENATE LV_BARCODE
                I_REF1
                I_REF2
                '0'
    INTO R_BARCODE SEPARATED BY CL_ABAP_CHAR_UTILITIES=>NEWLINE.
  ENDMETHOD.


  METHOD GEN_BARCODE_DESC.
    DATA : LV_BARCODE TYPE C LENGTH 255.

    DATA : LV_TAX TYPE C LENGTH 255.

    IF I_TAX_ID_RECEIVER IS INITIAL.
      ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = 'ZSDSSDR0100'
                                                    I_PARAM             = 'BARCODE'
                                                    I_PARAM_EXT         = 'TAX_ID'
                                                    I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                          CHANGING  C_RETURN            = LV_TAX ).
    ELSE.
      LV_TAX = I_TAX_ID_RECEIVER.
    ENDIF.

    CONCATENATE '|' LV_TAX I_REF1 I_REF2 '0' INTO R_BARCODE SEPARATED BY '  '.
  ENDMETHOD.


  method GEN_PDF.
  endmethod.


  METHOD GEN_QRCODE.
    DATA : LV_QR_CODE TYPE C LENGTH 255.

    DATA : LV_TAX TYPE C LENGTH 255.

    IF I_TAX_ID_RECEIVER IS INITIAL.
      ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = 'ZSDSSDR0100'
                                                    I_PARAM             = 'QR_CODE'
                                                    I_PARAM_EXT         = 'TAX_ID'
                                                    I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                          CHANGING  C_RETURN           = LV_TAX ).
    ELSE.
      LV_TAX = I_TAX_ID_RECEIVER.
    ENDIF.

    CONCATENATE '|' LV_TAX INTO LV_QR_CODE.
    CONCATENATE LV_QR_CODE
                I_REF1
                I_REF2
                '0'
    INTO R_QR SEPARATED BY '\c013\'.
  ENDMETHOD.


  METHOD GET_LINE_ITEM_WARRANTY.

    DATA : LS_RETURN LIKE LINE OF RT_RETURN.

    DATA : LR_VBELN TYPE RANGE OF LIKP-VBELN.

    CONSTANTS : BEGIN OF LC_CON,
                  CDU TYPE C LENGTH 5 VALUE 'CDU',
                END OF LC_CON.

    IF CT_DATA IS NOT INITIAL.
      SORT CT_DATA.
      DELETE ADJACENT DUPLICATES FROM CT_DATA.

      LR_VBELN  = VALUE #(
                FOR LS_TMP IN CT_DATA INDEX INTO LV_INDEX
                  ( SIGN   = 'I' OPTION = 'EQ' LOW    = LS_TMP )
                 ).

      SELECT LIKP~VBELN,
             LIKP~KUNNR,
             LIPS~POSNR,
             LIPS~MATNR,
             LIPS~PRODH,
             VBAK~VKGRP,
             VBAK~AUGRU,
             VBAK~AUART
        FROM LIKP
        INNER JOIN LIPS ON LIKP~VBELN EQ LIPS~VBELN
        INNER JOIN VBAP ON LIPS~VGBEL EQ VBAP~VBELN AND
                           LIPS~VGPOS EQ VBAP~POSNR
        INNER JOIN VBAK ON VBAP~VBELN EQ VBAK~VBELN
        WHERE LIKP~VBELN IN @LR_VBELN[]
          AND LIPS~KOWRR EQ @SPACE
        INTO TABLE @DATA(LT_DO).
    ELSE.
      SELECT LIKP~VBELN,
             LIKP~KUNNR,
             LIPS~POSNR,
             LIPS~MATNR,
             LIPS~PRODH,
             VBAK~VKGRP,
             VBAK~AUGRU,
             VBAK~AUART
        FROM LIKP
        INNER JOIN LIPS ON LIKP~VBELN EQ LIPS~VBELN
        INNER JOIN VBAP ON LIPS~VGBEL EQ VBAP~VBELN AND
                           LIPS~VGPOS EQ VBAP~POSNR
        INNER JOIN VBAK ON VBAP~VBELN EQ VBAK~VBELN
        WHERE LIKP~VBELN EQ @I_DO
          AND LIPS~KOWRR EQ @SPACE
        INTO TABLE @LT_DO.
    ENDIF.

    IF LT_DO IS NOT INITIAL.
      SELECT MATNR,
             AUGRU
        FROM ZSDSMMC003
        FOR ALL ENTRIES IN @LT_DO
        WHERE MATNR EQ @LT_DO-MATNR
          AND AUGRU EQ @LT_DO-AUGRU
        INTO TABLE @DATA(LT_CHECK_MAT).

      SELECT VKGRP,
             PH1,
             PH2,
             PH3
       FROM ZSDSMMC004
       FOR ALL ENTRIES IN @LT_DO
       WHERE VKGRP EQ @LT_DO-VKGRP
         AND PH1   EQ @LT_DO-PRODH+0(5)
         AND PH2   EQ @LT_DO-PRODH+5(5)
*         AND PH3   EQ @LT_DO-PRODH+10(8)
       INTO TABLE @DATA(LT_CONDITION).

      SELECT KUNNR,
             VKGRP
      FROM ZSDSMMC005
      FOR ALL ENTRIES IN @LT_DO
       WHERE KUNNR EQ @LT_DO-KUNNR
         AND VKGRP EQ @LT_DO-VKGRP
       INTO TABLE @DATA(LT_CUST).

      SELECT MATNR
      FROM ZSDSMMC006
      FOR ALL ENTRIES IN @LT_DO
       WHERE MATNR EQ @LT_DO-MATNR
       INTO TABLE @DATA(LT_EX_MAT).

      SELECT AUART
      FROM ZSDSMMC007
      FOR ALL ENTRIES IN @LT_DO
       WHERE AUART EQ @LT_DO-AUART
       INTO TABLE @DATA(LT_ORDER_TYPE).
    ENDIF.

    DATA : LS_CHECK_MAT  LIKE LINE OF LT_CHECK_MAT,
           LS_CONDITION  LIKE LINE OF LT_CONDITION,
           LS_CUST       LIKE LINE OF LT_CUST,
           LS_EX_MAT     LIKE LINE OF LT_EX_MAT,
           LS_ORDER_TYPE LIKE LINE OF LT_ORDER_TYPE.

    LOOP AT LT_DO INTO DATA(LS_DO) WHERE PRODH+5(5) EQ LC_CON-CDU.
      READ TABLE LT_EX_MAT INTO LS_EX_MAT
      WITH KEY MATNR = LS_DO-MATNR.
      IF SY-SUBRC EQ 0.
        CONTINUE.
      ENDIF.

      READ TABLE LT_ORDER_TYPE INTO LS_ORDER_TYPE
      WITH KEY AUART = LS_DO-AUART.
      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.

      READ TABLE LT_CHECK_MAT INTO LS_CHECK_MAT
      WITH KEY MATNR = LS_DO-MATNR
               AUGRU = LS_DO-AUGRU.
      IF SY-SUBRC EQ 0.
        LS_RETURN-VBELN = LS_DO-VBELN.
        LS_RETURN-POSNR = LS_DO-POSNR.
        APPEND LS_RETURN TO RT_RETURN.
        CONTINUE.
      ENDIF.

      READ TABLE LT_CONDITION INTO LS_CONDITION
      WITH KEY VKGRP = LS_DO-VKGRP
               PH1   = LS_DO-PRODH+0(5)
               PH2   = LS_DO-PRODH+5(5).
*               PH3   = LS_DO-PRODH+10(8).
      IF SY-SUBRC EQ 0.
        LS_RETURN-VBELN = LS_DO-VBELN.
        LS_RETURN-POSNR = LS_DO-POSNR.
        APPEND LS_RETURN TO RT_RETURN.
        CONTINUE.
      ENDIF.

      READ TABLE LT_CUST  INTO LS_CUST
      WITH KEY KUNNR = LS_DO-KUNNR
               VKGRP = LS_DO-VKGRP.
      IF SY-SUBRC EQ 0.
        LS_RETURN-VBELN = LS_DO-VBELN.
        LS_RETURN-POSNR = LS_DO-POSNR.
        APPEND LS_RETURN TO RT_RETURN.
        CONTINUE.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD MAPPING_DATA_INV.
    DATA : LS_DATA     LIKE E_DATA,
           LS_DATA_TAB LIKE LINE OF ET_DATA.

    DATA : LS_ADR LIKE LINE OF I_BILL_INFO-HD_ADR.

    DATA : LS_PART_ADD LIKE LINE OF I_BILL_INFO-HD_PART_ADD.

    DATA : LS_GEN LIKE LINE OF I_BILL_INFO-IT_GEN.

    DATA : LS_PRICE LIKE LINE OF I_BILL_INFO-IT_PRICE.

    DATA : LCL_UTIL TYPE REF TO ZCL_SDSCA_UTIL_SDS.

    DATA : LT_SERIAL_TAB LIKE ET_DATA,
           LS_SERIAL_TAB LIKE LINE OF ET_DATA.

    DATA : LS_KOND LIKE LINE OF I_BILL_INFO-HD_KOND.

    DATA : LV_LINE_ITEM TYPE I.

    DATA : LV_LANGU TYPE SY-LANGU.

    DATA : LV_BRANCH TYPE STRING.

    DATA : LV_XBLNR TYPE VBRK-XBLNR.

    DATA : LV_TEXT1 TYPE STRING,
           LV_TEXT2 TYPE STRING.

    DATA : LV_SPECIAL TYPE C.

    DATA : LV_AMOUNT_SPECIAL TYPE P DECIMALS 2.

    DATA : LV_FIX_BRANCH LIKE I_BILL_INFO-HD_GEN-BUPLA.

    CONSTANTS : BEGIN OF LC_CON,
                  E      TYPE C LENGTH 1 VALUE 'E',
                  TWO    TYPE C LENGTH 1 VALUE '2',
                  TAX_TH TYPE C LENGTH 100 VALUE 'เลขประจำตัวผู้เสียภาษี',
                  TAX_EN TYPE C LENGTH 100 VALUE 'Tax ID No.',
                  INITL  TYPE C LENGTH 6 VALUE '000000',
                END OF LC_CON.

    IF LCL_UTIL IS NOT BOUND.
      CREATE OBJECT LCL_UTIL.
    ENDIF.

    IF I_NATION EQ SPACE.
      LV_LANGU = LC_CON-TWO.
    ELSE.
      LV_LANGU = LC_CON-E.
    ENDIF.

    ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = 'MAPPING_DATA_INV'
                                                  I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                  I_PARAM             = 'BRANCH'
                                         CHANGING C_RETURN            = LV_FIX_BRANCH ).

    IF LV_FIX_BRANCH IS INITIAL.
      IF I_BILL_INFO-HD_GEN-BUPLA IS INITIAL.
        DATA(LV_BUPLA) = LCL_DATA=>GET_BRANCH( I_BILL_INFO-HD_GEN-BIL_NUMBER ).
      ELSE.
        LV_BUPLA = I_BILL_INFO-HD_GEN-BUPLA.
      ENDIF.
    ELSE.
      LV_BUPLA = LV_FIX_BRANCH.
    ENDIF.

    LCL_UTIL->GET_SDS_ADDRESS( EXPORTING I_BRANCH     = LV_BUPLA "I_BILL_INFO-HD_GEN-BUPLA
                               IMPORTING E_ADDRESS_TH = E_DATA-SDS_ADDRESS_TH
                                         E_ADDRESS_EN = E_DATA-SDS_ADDRESS_EN ).

    E_DATA-BILNO  = I_BILL_INFO-HD_GEN-BIL_NUMBER.
    E_DATA-DO_NO  = LCL_DATA=>GEN_DO_NO( I_BILL_INFO-IT_REFDLV ).
    E_DATA-SO_NO  = LCL_DATA=>GEN_SO_NO( I_BILL_INFO-IT_REFORD ).
*    E_DATA-WBS_NO = LCL_DATA=>GET_WBS( I_BILL_INFO-HD_GEN-BIL_NUMBER ).
*    E_DATA-PROFIT = LCL_DATA=>GET_PROFIT( I_BILL_INFO-HD_GEN-BIL_NUMBER ).
    LCL_DATA=>GET_SINGLE_VBRP( EXPORTING I_DATA = I_BILL_INFO-HD_GEN-BIL_NUMBER
                                CHANGING C_DATA = E_DATA ).

    E_DATA-PO_NO  = I_BILL_INFO-HD_REF-PURCH_NO_C.

    E_DATA-SIGNC = 'ZSDS_6913'.


    E_DATA-PROJECT_NAME = LCL_UTIL->GET_TEXT( EXPORTING I_ID       = GC_CON-INV_PROJECT_ID
                                                        I_NAME     = I_BILL_INFO-HD_REF-ORDER_NUMB
                                                        I_OBJECT   = GC_CON-INV_PROJECT_OBJECT
                                                        I_LANGUAGE = SY-LANGU ).

    E_DATA-LAND = LCL_UTIL->GET_TEXT( EXPORTING I_ID       = GC_CON-INV_LAND_ID
                                                I_NAME     = I_BILL_INFO-HD_REF-ORDER_NUMB
                                                I_OBJECT   = GC_CON-INV_LAND_OBJECT
                                                I_LANGUAGE = SY-LANGU ).

    SELECT SINGLE XBLNR
      FROM VBRK
      INTO LV_XBLNR
      WHERE VBELN EQ I_BILL_INFO-HD_GEN-BIL_NUMBER.

    LV_XBLNR = |{ LV_XBLNR ALPHA = OUT }|.  CONDENSE LV_XBLNR.
    E_DATA-QRCOD = GEN_QRCODE( I_REF1 = LV_XBLNR
                               I_REF2 = I_BILL_INFO-HD_GEN-PAYER ).

    E_DATA-BARCD = GEN_BARCODE( I_REF1 = LV_XBLNR
                                I_REF2 = I_BILL_INFO-HD_GEN-PAYER ).

    E_DATA-BARDS = GEN_BARCODE_DESC( I_REF1 = LV_XBLNR
                                     I_REF2 = I_BILL_INFO-HD_GEN-PAYER ).

    SELECT SINGLE BELNR,
                  XBLNR
    FROM BKPF
    INTO (@DATA(LV_FI_DOC),@DATA(LV_TAX_INV))
    WHERE AWKEY EQ @I_BILL_INFO-HD_GEN-BIL_NUMBER.
    IF SY-SUBRC EQ 0.
      E_DATA-VBELN   = LV_FI_DOC.
      E_DATA-TAX_INV = LV_TAX_INV.
    ELSE.
      E_DATA-VBELN = I_BILL_INFO-HD_GEN-BIL_NUMBER.
    ENDIF.
    E_DATA-FKDAT = I_BILL_INFO-HD_GEN-BIL_DATE.
    E_DATA-CREDT = LCL_DATA=>CONVERT_DATE( I_BILL_INFO-HD_GEN-BIL_DATE ).
*    WRITE I_BILL_INFO-HD_GEN-BIL_EDATE TO E_DATA-CREDT.
    E_DATA-KUNNR = I_BILL_INFO-HD_GEN-PAYER.

    LCL_UTIL->GET_CUST_NAME( EXPORTING I_CUST_NO  = E_DATA-KUNNR
                                       I_NATION   = I_NATION
                             IMPORTING E_NAME1    = E_DATA-NAME1
                                       E_NAME2    = E_DATA-NAME2
                                       E_NAME3    = E_DATA-NAME3
                                       E_NAME4    = E_DATA-NAME4 ).

    LV_BRANCH = LCL_UTIL->GET_BRANCH( I_KUNNR = E_DATA-KUNNR
                                      I_LANGU = LV_LANGU ).

    CONCATENATE E_DATA-NAME1
                E_DATA-NAME2
                E_DATA-NAME3
                E_DATA-NAME4
                LV_BRANCH
           INTO E_DATA-NAMEALL SEPARATED BY SPACE.

    SELECT SINGLE STCD3
      FROM KNA1
      INTO @DATA(LV_TAXNO)
      WHERE KUNNR EQ @E_DATA-KUNNR.
    IF SY-SUBRC = 0 AND LV_TAXNO <> '0000000000000'.
      IF I_NATION EQ SPACE.
        CONCATENATE LC_CON-TAX_TH
                    LV_TAXNO
               INTO E_DATA-TAX_NO SEPARATED BY SPACE.
      ELSE.
        CONCATENATE LC_CON-TAX_EN
                    LV_TAXNO
               INTO E_DATA-TAX_NO SEPARATED BY SPACE.
      ENDIF.
    ENDIF.

*    IF E_DATA-NAME1 IS INITIAL.
*      LCL_UTIL->GET_CUST_NAME( EXPORTING I_CUST_NO = E_DATA-KUNNR
*                                         I_NATION   = GC_CON-I
*                               IMPORTING E_NAME1    = E_DATA-NAME1
*                                         E_NAME2    = E_DATA-NAME2
*                                         E_NAME3    = E_DATA-NAME3
*                                         E_NAME4    = E_DATA-NAME4 ).
*    ENDIF.

    READ TABLE I_BILL_INFO-HD_PART_ADD INTO LS_PART_ADD
    WITH KEY PARTN_ROLE = GC_CON-PARTN_ROLE.
    IF SY-SUBRC EQ 0.
      E_DATA-PERNR = LS_PART_ADD-PARTN_NUMB.
*      LCL_UTIL->GET_SALE_NAME( EXPORTING I_SALE_NO   = E_DATA-PERNR
*                               IMPORTING E_SALE_NAEM = E_DATA-SALEN ).
    ENDIF.
    E_DATA-SALEN = LCL_DATA=>GET_SALES_GROUP_DESC( I_BILL_INFO-HD_REF-ORDER_NUMB ).

    IF I_BILL_INFO-HD_GEN-TERMS_PAYM EQ GC_CON-ZZZZ.
      E_DATA-DUEDT = I_BILL_INFO-HD_GEN-VAL_DATE.
    ELSE.
      E_DATA-DUEDT = LCL_UTIL->GET_PAYMENT_DATE( I_DATE = E_DATA-FKDAT
                                                 I_TERM = I_BILL_INFO-HD_GEN-TERMS_PAYM ).
    ENDIF.
*    WRITE E_DATA-DUEDT TO E_DATA-DUETT.
    E_DATA-DUETT = LCL_DATA=>CONVERT_DATE( E_DATA-DUEDT ).
    READ TABLE I_BILL_INFO-HD_ADR INTO LS_ADR
    WITH KEY PARTN_ROLE = GC_CON-AG.
    IF SY-SUBRC EQ 0.
      LCL_UTIL->GET_ADDRESS( EXPORTING I_ADDRESS_NO           = LS_ADR-ADDR_NO
                                       I_NATION               = I_NATION
                             IMPORTING E_ADDRESS_CONCAT_LINE1 = E_DATA-ADDR1
                                       E_ADDRESS_CONCAT_LINE2 = E_DATA-ADDR2
                                       E_ADDRESS_CONCAT_LINE3 = E_DATA-ADDR3
                                       E_ADDRESS_CONCAT_LINE4 = E_DATA-ADDR4 ).
*      IF E_DATA-ADDR1 IS INITIAL.
*        LCL_UTIL->GET_ADDRESS( EXPORTING I_ADDRESS_NO           = LS_ADR-ADDR_NO
*                                         I_NATION               = GC_CON-I
*                               IMPORTING E_ADDRESS_CONCAT_LINE1 = E_DATA-ADDR1
*                                         E_ADDRESS_CONCAT_LINE2 = E_DATA-ADDR2 ).
*      ENDIF.
      IF E_DATA-KUNNR CP 'OT*'.
        CLEAR E_DATA-NAMEALL.
        LCL_UTIL->GET_CUST_NAME( EXPORTING I_CUST_NO      = E_DATA-KUNNR
                                           I_NATION       = I_NATION
                                           I_ADDRNUMBER   = LS_ADR-ADDR_NO
                                 IMPORTING E_NAME1    = E_DATA-NAME1
                                           E_NAME2    = E_DATA-NAME2
                                           E_NAME3    = E_DATA-NAME3
                                           E_NAME4    = E_DATA-NAME4 ).
        CONCATENATE E_DATA-NAME1
                    E_DATA-NAME2
                    E_DATA-NAME3
                    E_DATA-NAME4
                    INTO E_DATA-NAMEALL SEPARATED BY SPACE.
        SELECT SINGLE STCD3
          FROM VBPA3
          INTO LV_TAXNO
          WHERE VBELN = I_BILL_INFO-HD_GEN-BIL_NUMBER
            AND PARVW = GC_CON-AG.
        IF SY-SUBRC = 0 AND LV_TAXNO <> '0000000000000'.
          IF I_NATION EQ SPACE.
            CONCATENATE LC_CON-TAX_TH
                        LV_TAXNO
                   INTO E_DATA-TAX_NO SEPARATED BY SPACE.
          ELSE.
            CONCATENATE LC_CON-TAX_EN
                        LV_TAXNO
                   INTO E_DATA-TAX_NO SEPARATED BY SPACE.
          ENDIF.
        ENDIF.

      ENDIF.
    ENDIF.

    E_DATA-REMAK = LCL_UTIL->GET_TEXT( EXPORTING I_ID       = GC_CON-REMARK_ID
                                                 I_NAME     = E_DATA-VBELN
                                                 I_OBJECT   = GC_CON-REMARK_OBJECT
                                                 I_LANGUAGE = SY-LANGU ).

    E_DATA-LANNO = LCL_UTIL->GET_TEXT( EXPORTING I_ID       = GC_CON-INV_LAND_ID
                                                 I_NAME     = I_BILL_INFO-HD_GEN-BIL_NUMBER
                                                 I_OBJECT   = GC_CON-REMARK_OBJECT
                                                 I_LANGUAGE = SY-LANGU ).

    DATA(LV_CHECK_SUM) = LCL_UTIL->GET_TEXT( EXPORTING I_ID       = GC_CON-CHECK_SUM
                                                       I_NAME     = I_BILL_INFO-HD_GEN-BIL_NUMBER
                                                       I_OBJECT   = GC_CON-REMARK_OBJECT
                                                       I_LANGUAGE = SY-LANGU ).

    DATA(LV_ADVANCE) = LCL_UTIL->GET_TEXT( EXPORTING I_ID       = GC_CON-COMMENT
                                                     I_NAME     = I_BILL_INFO-HD_GEN-BIL_NUMBER
                                                     I_OBJECT   = GC_CON-REMARK_OBJECT
                                                     I_LANGUAGE = SY-LANGU ).

    SPLIT LV_ADVANCE AT '|' INTO LV_TEXT1
                                 LV_TEXT2.

    TRANSLATE LV_ADVANCE TO UPPER CASE.

    IF LV_TEXT1 = '$ADVANCE'.
      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LV_TEXT2 WITH ''.
      REPLACE ALL OCCURRENCES OF PCRE ',' IN LV_TEXT2 WITH ''.
      E_DATA-ADVAN = LV_TEXT2.
      LV_SPECIAL   = ABAP_TRUE.
    ELSE.
      CLEAR : LV_SPECIAL.
    ENDIF.

*  E_DATA-SIGNC = ''.
*  E_DATA-SIGNR = ''.



*    LT_SERIAL_TAB = LCL_DATA=>GET_SERAIL_TAB( I_DO      = I_BILL_INFO-HD_REF-DELIV_NUMB
*                                              IT_DO_TAB = I_BILL_INFO-IT_REF ).

    DATA : LS_ITEM  LIKE LINE OF I_BILL_INFO-IT_GEN,
           LV_QTY   TYPE I,
           LV_QTY_T TYPE C LENGTH 20,
           LV_BOM   TYPE STRING.
    IF LV_CHECK_SUM EQ ABAP_TRUE OR
           LV_CHECK_SUM EQ 'x'.
      DATA : LV_SUM_TEXT TYPE STRING.
      LOOP AT I_BILL_INFO-IT_GEN INTO LS_GEN.
        LS_DATA_TAB-POSTT = '1'.
        LV_QTY   = LS_GEN-FKIMG.
        IF LV_SPECIAL EQ ABAP_TRUE.
          LS_DATA_TAB-MATNR = LS_GEN-MATERIAL.
          LS_DATA_TAB-MAKTX = LS_GEN-SHORT_TEXT.
          LS_DATA_TAB-FKIMG = LS_GEN-FKIMG.

          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
            EXPORTING
              INPUT          = LS_GEN-SALES_UNIT
              LANGUAGE       = SY-LANGU
            IMPORTING
              OUTPUT         = LS_DATA_TAB-VRKME
            EXCEPTIONS
              UNIT_NOT_FOUND = 1
              OTHERS         = 2.
          IF SY-SUBRC <> 0.
* Implement suitable error handling here
          ENDIF.

          WRITE LS_DATA_TAB-FKIMG TO LS_DATA_TAB-QTYTT.
          REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DATA_TAB-QTYTT WITH ''.
*          WRITE LS_DATA_TAB-FKIMG TO LS_DATA_TAB-QTYTT.
*          REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DATA_TAB-QTYTT WITH ''.
        ELSE.
          CONCATENATE : LS_GEN-BIL_NUMBER LS_GEN-ITM_NUMBER INTO LV_SUM_TEXT.

          DATA(LV_TEXT_SUM) = LCL_UTIL->GET_TEXT( EXPORTING I_ID       = GC_CON-ITEM_DETAIL
                                                            I_NAME     = LV_SUM_TEXT
                                                            I_OBJECT   = GC_CON-ITEM_OBJECT
                                                            I_LANGUAGE = SY-LANGU ).
          IF LV_TEXT_SUM IS NOT INITIAL.
            LS_DATA_TAB-MAKTX = LV_TEXT_SUM.
          ENDIF.
        ENDIF.

        READ TABLE I_BILL_INFO-IT_PRICE INTO LS_PRICE
          WITH KEY BIL_NUMBER = LS_GEN-BIL_NUMBER
                   ITM_NUMBER = LS_GEN-ITM_NUMBER.
        IF SY-SUBRC EQ 0.
          LS_DATA_TAB-PRICE = LS_DATA_TAB-PRICE + ( LS_PRICE-KZWI1 / LV_QTY ).
          LS_DATA_TAB-NETWR = LS_DATA_TAB-NETWR + ( LS_PRICE-NETWR  / LV_QTY ).
          LS_DATA_TAB-DISCT = LS_DATA_TAB-PRICE - LS_DATA_TAB-NETWR.
          LS_DATA_TAB-TOTAL = LS_DATA_TAB-TOTAL + LS_PRICE-NETWR .
        ENDIF.
      ENDLOOP.

      ADD LS_DATA_TAB-TOTAL TO E_DATA-AMONT.

      IF LV_SPECIAL EQ ABAP_TRUE.
        LV_AMOUNT_SPECIAL = E_DATA-AMONT + E_DATA-ADVAN.
        WRITE LV_AMOUNT_SPECIAL TO LS_DATA_TAB-PRITT.
        WRITE LS_DATA_TAB-DISCT TO LS_DATA_TAB-DISTT.
        WRITE LV_AMOUNT_SPECIAL TO LS_DATA_TAB-NETTT.
        WRITE LV_AMOUNT_SPECIAL TO LS_DATA_TAB-TOTTT.
      ELSE.
        WRITE LS_DATA_TAB-PRICE TO LS_DATA_TAB-PRITT.
        WRITE LS_DATA_TAB-DISCT TO LS_DATA_TAB-DISTT.
        WRITE LS_DATA_TAB-NETWR TO LS_DATA_TAB-NETTT.
        WRITE LS_DATA_TAB-TOTAL TO LS_DATA_TAB-TOTTT.
      ENDIF.
      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DATA_TAB-PRITT WITH ''.
      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DATA_TAB-DISTT WITH ''.
      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DATA_TAB-NETTT WITH ''.
      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DATA_TAB-TOTTT WITH ''.

      APPEND LS_DATA_TAB TO ET_DATA.

    ELSE.
      LOOP AT I_BILL_INFO-IT_GEN INTO LS_GEN.
*
        IF LS_GEN-UEPOS NE LC_CON-INITL.
          READ TABLE I_BILL_INFO-IT_GEN
          WITH KEY ITM_NUMBER = LS_GEN-UEPOS TRANSPORTING NO FIELDS.
          IF SY-SUBRC EQ 0.
            CONTINUE.
          ENDIF.
        ENDIF.
        LS_DATA_TAB-VBELN = LS_GEN-BIL_NUMBER.
*      LS_DATA_TAB-POSNR = LS_GEN-ITM_NUMBER.
*    LS_DATA_TAB-POSTT = LS_DATA_TAB-POSNR.
        ADD 1 TO LV_LINE_ITEM.
        LS_DATA_TAB-POSNR = LV_LINE_ITEM.

        LCL_DATA=>CONVERT_ALPHA_OUT( EXPORTING I_DATA = LS_DATA_TAB-POSNR
                                     IMPORTING E_DATA = LS_DATA_TAB-POSTT ).

        LS_DATA_TAB-MATNR = LS_GEN-MATERIAL.
        IF LS_GEN-MATERIAL EQ 'FIDOC'.
          CLEAR : LS_GEN-MATERIAL.
          CONCATENATE LS_GEN-SHORT_TEXT LS_GEN-MAT_ENTRD
                 INTO LS_DATA_TAB-MAKTX.
        ELSE.
          LS_DATA_TAB-MAKTX = LS_GEN-SHORT_TEXT.
        ENDIF.
        LS_DATA_TAB-FKIMG = LS_GEN-FKIMG.

        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
          EXPORTING
            INPUT          = LS_GEN-SALES_UNIT
            LANGUAGE       = SY-LANGU
          IMPORTING
            OUTPUT         = LS_DATA_TAB-VRKME
          EXCEPTIONS
            UNIT_NOT_FOUND = 1
            OTHERS         = 2.
        IF SY-SUBRC <> 0.
* Implement suitable error handling here
        ENDIF.

        WRITE LS_DATA_TAB-FKIMG TO LS_DATA_TAB-QTYTT.
        REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DATA_TAB-QTYTT WITH ''.
*      IF LS_GEN-UEPOS NE LC_CON-INITL.
*
*
*      ELSE.
        LOOP AT I_BILL_INFO-IT_GEN INTO LS_ITEM WHERE UEPOS EQ LS_GEN-ITM_NUMBER.
          LV_QTY   = LS_ITEM-FKIMG.
          LV_QTY_T = LV_QTY.
          REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LV_QTY_T WITH ''.
          IF LV_BOM IS INITIAL.
            CONCATENATE LS_ITEM-MATERIAL '(' LV_QTY_T ')' INTO LV_BOM.
          ELSE.
            CONCATENATE LV_BOM '+' LS_ITEM-MATERIAL '(' LV_QTY_T ')' INTO LV_BOM.
          ENDIF.

          READ TABLE I_BILL_INFO-IT_PRICE INTO LS_PRICE
          WITH KEY BIL_NUMBER = LS_ITEM-BIL_NUMBER
                   ITM_NUMBER = LS_ITEM-ITM_NUMBER.
          IF SY-SUBRC EQ 0.
            LS_DATA_TAB-PRICE = LS_DATA_TAB-PRICE + ( LS_PRICE-KZWI1 / LV_QTY ).
            LS_DATA_TAB-NETWR = LS_DATA_TAB-NETWR + ( LS_PRICE-NETWR  / LV_QTY ).
            LS_DATA_TAB-DISCT = LS_DATA_TAB-PRICE - LS_DATA_TAB-NETWR.
            LS_DATA_TAB-TOTAL = LS_DATA_TAB-TOTAL + LS_PRICE-NETWR .
          ENDIF.
          SY-SUBRC = 0.
        ENDLOOP.
        IF SY-SUBRC NE 0.
          LV_QTY   = LS_GEN-FKIMG.
          LV_QTY_T = LV_QTY.

          READ TABLE I_BILL_INFO-IT_PRICE INTO LS_PRICE
          WITH KEY BIL_NUMBER = LS_GEN-BIL_NUMBER
                   ITM_NUMBER = LS_GEN-ITM_NUMBER.
          IF SY-SUBRC EQ 0.
*          LS_DATA_TAB-PRICE = LS_PRICE-KZWI1.
*          LS_DATA_TAB-DISCT = ABS( LS_PRICE-KZWI4 ).
*          LS_DATA_TAB-NETWR = LS_PRICE-NETWR.
*          LS_DATA_TAB-TOTAL = ( LS_PRICE-NETWR * LS_DATA_TAB-FKIMG ) - LS_DATA_TAB-DISCT.
            LS_DATA_TAB-PRICE =  ( LS_PRICE-KZWI1 / LV_QTY ).
            LS_DATA_TAB-NETWR =  ( LS_PRICE-NETWR  / LV_QTY ).
            LS_DATA_TAB-DISCT =  LS_DATA_TAB-PRICE - LS_DATA_TAB-NETWR.
            LS_DATA_TAB-TOTAL =  LS_PRICE-NETWR .
          ENDIF.
        ENDIF.
*      ENDIF.

        ADD LS_DATA_TAB-TOTAL TO E_DATA-AMONT.

        WRITE LS_DATA_TAB-PRICE TO LS_DATA_TAB-PRITT.
        WRITE LS_DATA_TAB-DISCT TO LS_DATA_TAB-DISTT.
        WRITE LS_DATA_TAB-NETWR TO LS_DATA_TAB-NETTT.
        WRITE LS_DATA_TAB-TOTAL TO LS_DATA_TAB-TOTTT.
        REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DATA_TAB-PRITT WITH ''.
        REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DATA_TAB-DISTT WITH ''.
        REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DATA_TAB-NETTT WITH ''.
        REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DATA_TAB-TOTTT WITH ''.

        APPEND LS_DATA_TAB TO ET_DATA.
        CLEAR : LS_DATA_TAB.
        LOOP AT LT_SERIAL_TAB INTO LS_SERIAL_TAB WHERE VBELN EQ LS_GEN-BIL_NUMBER AND
                                                       POSNR EQ LS_GEN-ITM_NUMBER.

          LS_DATA_TAB-MATNR = LS_SERIAL_TAB-MATNR.
          LS_DATA_TAB-MAKTX = LS_SERIAL_TAB-MAKTX.
          APPEND LS_DATA_TAB TO ET_DATA.
          CLEAR : LS_DATA_TAB.
        ENDLOOP.

        IF LS_GEN-UEPOS EQ LS_GEN-UEPOS.
*        LOOP AT I_BILL_INFO-IT_GEN INTO LS_ITEM WHERE UEPOS EQ LS_GEN-ITM_NUMBER.
*          LV_QTY   = LS_GEN-FKIMG.
*          LV_QTY_T = LV_QTY.
*          REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LV_QTY_T WITH ''.
*          IF LS_DATA_TAB-MAKTX IS INITIAL.
*            CONCATENATE LS_ITEM-MATERIAL '(' LV_QTY_T ')' INTO LV_BOM.
*          ELSE.
*            CONCATENATE LV_BOM '+' LS_ITEM-MATERIAL '(' LV_QTY_T ')' INTO LV_BOM.
*          ENDIF.
*        ENDLOOP.
          IF LV_BOM IS NOT INITIAL.
            LS_DATA_TAB-MAKTX = LV_BOM.
            APPEND LS_DATA_TAB TO ET_DATA.
            CLEAR : LS_DATA_TAB,LV_BOM.
          ENDIF.
        ENDIF.

      ENDLOOP.
    ENDIF.

    READ TABLE I_BILL_INFO-HD_KOND INTO LS_KOND
    WITH KEY BIL_NUMBER = I_BILL_INFO-HD_GEN-BIL_NUMBER.
    IF SY-SUBRC EQ 0.
      E_DATA-TAXAM = LS_KOND-KWERT.
    ENDIF.

    IF LV_SPECIAL EQ ABAP_TRUE.
      E_DATA-TOTAL = E_DATA-AMONT.
      E_DATA-NETWR = E_DATA-AMONT + E_DATA-TAXAM.
      E_DATA-AMONT = E_DATA-AMONT + E_DATA-ADVAN.
    ELSE.
      E_DATA-TOTAL = E_DATA-AMONT + E_DATA-ADVAN.
      E_DATA-NETWR = E_DATA-TOTAL + E_DATA-TAXAM.
    ENDIF.

    E_DATA-TXTAM = LCL_DATA=>CONVERT_AMOUNT_TO_TEXT( I_NETWR = E_DATA-NETWR
                                                     I_CURR  = GC_CON-THB
                                                     I_LANGU = LV_LANGU ).

    WRITE E_DATA-AMONT TO E_DATA-AMOTT.
    WRITE E_DATA-ADVAN TO E_DATA-ADVTT.
    WRITE E_DATA-TOTAL TO E_DATA-TOTTT.
    WRITE E_DATA-TAXAM TO E_DATA-TAXTT.
    WRITE E_DATA-NETWR TO E_DATA-NETTT.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN E_DATA-AMOTT WITH ''.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN E_DATA-ADVTT WITH ''.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN E_DATA-TOTTT WITH ''.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN E_DATA-TAXTT WITH ''.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN E_DATA-NETTT WITH ''.


  ENDMETHOD.


  METHOD MAPPING_DATA_PICKING.
    CONSTANTS : LC_SPACE TYPE C LENGTH 2 VALUE '  ',
                LC_SLASH TYPE C LENGTH 1 VALUE '/'.

    DATA : LS_DETAIL LIKE LINE OF ET_DETAIL.

    DATA : LS_GEN LIKE LINE OF IS_PICKING_INFO-IT_GEN.

    DATA : LV_TEXT TYPE C LENGTH 255.

    DATA : LCL_UTIL TYPE REF TO ZCL_SDSCA_UTIL_SDS.

    DATA : LS_ADR LIKE LINE OF IS_PICKING_INFO-HD_ADR.

    DATA : LS_PART_ADD LIKE LINE OF IS_PICKING_INFO-HD_PART_ADD.

    DATA : LV_PERSNO TYPE PERSNO.

    DATA : LV_SALES_GROUP TYPE STRING.

    DATA : LV_QTY TYPE I.

    DATA : LV_CN TYPE C.

    DATA : LV_NO_SERIAL TYPE C.

    DATA : LV_BRANCH TYPE STRING.

    DATA : LV_LINE TYPE I VALUE 1.

    DATA : LV_CHECK_BOM TYPE CHAR1.

    DATA : LR_WARANTY_GROUP TYPE RANGE OF ZSDSCAC001-VALUE_LOW.

    DATA : LT_WARRANTY TYPE ZSDSSDS113_TT,
           LS_WARRANTY TYPE ZSDSSDS113.

    DATA : LV_LINE_DO TYPE LIPS-POSNR.

    CONSTANTS : BEGIN OF LC_CON,
                  E      TYPE C LENGTH 1   VALUE 'E',
                  TWO    TYPE C LENGTH 1   VALUE '2',
                  TAX_TH TYPE C LENGTH 100 VALUE 'เลขประจำตัวผู้เสียภาษี',
                  TAX_EN TYPE C LENGTH 100 VALUE 'Taxpayer Identification No.',
                  INITL  TYPE C LENGTH 6   VALUE '000000',
                  SL     TYPE C LENGTH 1   VALUE '/',
                END OF LC_CON.

    IF LCL_UTIL IS NOT BOUND.
      CREATE OBJECT LCL_UTIL.
    ENDIF.

    ES_DATA-DO_NO          = IS_PICKING_INFO-HD_GEN-DELIV_NUMB.
    ES_DATA-DOC_REF        = IS_PICKING_INFO-HD_REF-REF_DOC.

    SELECT SINGLE ERNAM
      FROM LIKP
      INTO @ES_DATA-CREATE_BY
      WHERE VBELN = @IS_PICKING_INFO-HD_GEN-DELIV_NUMB.

    DATA(LV_BNAME) = LCL_DATA=>GET_BNAME( IS_PICKING_INFO-HD_REF-REF_DOC ).
    IF LV_BNAME IS NOT INITIAL.
      CONCATENATE ES_DATA-DOC_REF LC_CON-SL LV_BNAME INTO ES_DATA-DOC_REF.
    ENDIF.

    LV_CN = LCL_DATA=>CHECK_RETURN( IS_PICKING_INFO-HD_GEN-DELIV_NUMB ).
    IF I_CHECK_NO_SERIAL EQ ABAP_TRUE.
      LV_NO_SERIAL = LCL_DATA=>CHECK_NO_SERIAL( I_CUST = IS_PICKING_INFO-HD_GEN-SOLD_TO_PARTY
                                                I_SO   = IS_PICKING_INFO-HD_REF-REF_DOC ).
    ENDIF.

    IF LV_CN EQ ABAP_TRUE.
      ES_DATA-REF_DOC_NO = LCL_DATA=>GET_REF_DOC( IS_PICKING_INFO-HD_REF-REF_DOC ).
      LV_SALES_GROUP = LCL_DATA=>GET_SALES_GROUP_DESC( IS_PICKING_INFO-HD_REF-REF_DOC ).
      ES_DATA-SALES_EMP = LCL_DATA=>GET_SALES_EMP_BY_SO( IS_PICKING_INFO-HD_REF-REF_DOC ).
*      ES_DATA-CONTRACT_NAME =
*      ES_DATA-TEL           =
    ENDIF.


    CLEAR : LV_TEXT.
    WRITE IS_PICKING_INFO-HD_GEN-DLV_DATE TO LV_TEXT.
    ES_DATA-DO_DATE        = LV_TEXT.
    ES_DATA-CUSTOMER_CODE  = IS_PICKING_INFO-HD_GEN-SOLD_TO_PARTY.

    READ TABLE IS_PICKING_INFO-HD_PART_ADD INTO LS_PART_ADD
    WITH KEY PARTN_ROLE = GC_CON-PARTN_ROLE.
    IF SY-SUBRC NE 0.
      SELECT SINGLE PERNR
        FROM VBPA
        INTO LV_PERSNO
        WHERE VBELN EQ IS_PICKING_INFO-HD_REF-REF_DOC
          AND POSNR EQ 0
          AND PARVW EQ GC_CON-PARTN_ROLE.
    ELSE.
      LV_PERSNO =  LS_PART_ADD-PARTN_NUMB.
    ENDIF.

    ES_DATA-PAYMENT_TERM      = LCL_DATA=>GET_PAYTERM( IS_PICKING_INFO-HD_REF-REF_DOC ).
    ES_DATA-PAYMENT_TERM_TEXT = LCL_DATA=>GET_PAYTERM_TEXT( ES_DATA-PAYMENT_TERM ).
    ES_DATA-MHA_MTD           = LCL_DATA=>GET_MTD_MHA( IS_PICKING_INFO-HD_REF-REF_DOC ).

    LCL_DATA=>GET_SALES_GROUP_OFFICE( EXPORTING I_DATA = IS_PICKING_INFO-HD_REF-REF_DOC
                                      CHANGING  C_OFF  = ES_DATA-SALES_GROUP
                                                C_GRO  = ES_DATA-SALES_OFFICE ).

    IF LV_CN EQ ABAP_TRUE.
      LCL_UTIL->GET_SALE_NAME( EXPORTING I_SALE_NO   = LV_PERSNO
                               IMPORTING E_NAME_FIRST = ES_DATA-SALES_EMP ).
      IF LV_SALES_GROUP IS NOT INITIAL.
        CONCATENATE ES_DATA-SALES_EMP LC_SLASH LV_SALES_GROUP INTO ES_DATA-SALES_EMP.
      ENDIF.
    ELSE.
      LCL_UTIL->GET_SALE_NAME( EXPORTING I_SALE_NO   = LV_PERSNO
                               IMPORTING E_SALE_NAEM = ES_DATA-SALES_EMP ).
    ENDIF.

    LCL_UTIL->GET_CUST_NAME( EXPORTING I_CUST_NO  = ES_DATA-CUSTOMER_CODE
                                       I_NATION   = SPACE"GC_CON-I
                             IMPORTING E_NAME1    = ES_DATA-CUSTOMER_NAME1
                                       E_NAME2    = ES_DATA-CUSTOMER_NAME2
                                       E_NAME3    = ES_DATA-CUSTOMER_NAME3
                                       E_NAME4    = ES_DATA-CUSTOMER_NAME4 ).

    LV_BRANCH = LCL_UTIL->GET_BRANCH( I_KUNNR = ES_DATA-CUSTOMER_CODE
                                      I_LANGU = SY-LANGU ).

    CONCATENATE ES_DATA-CUSTOMER_NAME1
                ES_DATA-CUSTOMER_NAME2
                ES_DATA-CUSTOMER_NAME3
                ES_DATA-CUSTOMER_NAME4
                LV_BRANCH
           INTO ES_DATA-CUSTOMER_NAME_ALL SEPARATED BY SPACE.

    SELECT SINGLE STCD3
      FROM KNA1
      INTO @DATA(LV_TAXNO)
      WHERE KUNNR EQ @ES_DATA-CUSTOMER_CODE.

    IF I_NATION EQ SPACE.
      CONCATENATE LC_CON-TAX_TH
                  LV_TAXNO
             INTO ES_DATA-TAX_NO SEPARATED BY SPACE.
    ELSE.
      CONCATENATE LC_CON-TAX_EN
                  LV_TAXNO
             INTO ES_DATA-TAX_NO SEPARATED BY SPACE.
    ENDIF.

    READ TABLE IS_PICKING_INFO-HD_ADR INTO LS_ADR
    WITH KEY PARTN_ROLE = GC_CON-WE.
    IF SY-SUBRC EQ 0.
      LCL_UTIL->GET_ADDRESS( EXPORTING I_ADDRESS_NO           = LS_ADR-ADDR_NO
                                       I_NATION               = SPACE
                             IMPORTING E_ADDRESS_CONCAT_LINE1 = ES_DATA-CUSTOMER_ADDR1
                                       E_ADDRESS_CONCAT_LINE2 = ES_DATA-CUSTOMER_ADDR2
                                       E_ADDRESS_CONCAT_LINE3 = ES_DATA-CUSTOMER_ADDR3
                                       E_ADDRESS_CONCAT_LINE4 = ES_DATA-CUSTOMER_ADDR4
                                       ).
      IF ES_DATA-CUSTOMER_ADDR1 IS INITIAL.
        LCL_UTIL->GET_ADDRESS( EXPORTING I_ADDRESS_NO           = LS_ADR-ADDR_NO
                                         I_NATION               = GC_CON-I
                               IMPORTING E_ADDRESS_CONCAT_LINE1 = ES_DATA-CUSTOMER_ADDR1
                                         E_ADDRESS_CONCAT_LINE2 = ES_DATA-CUSTOMER_ADDR2
                                         E_ADDRESS_CONCAT_LINE3 = ES_DATA-CUSTOMER_ADDR3
                                         E_ADDRESS_CONCAT_LINE4 = ES_DATA-CUSTOMER_ADDR4
                                         ).
      ENDIF.

      IF ES_DATA-CUSTOMER_CODE CP 'OT*'.
        CLEAR:
          ES_DATA-CUSTOMER_NAME_ALL,
          ES_DATA-TAX_NO.

        LCL_UTIL->GET_CUST_NAME( EXPORTING I_CUST_NO      = ES_DATA-CUSTOMER_CODE
                                           I_NATION       = I_NATION
                                           I_ADDRNUMBER   = LS_ADR-ADDR_NO
                                 IMPORTING E_NAME1    = ES_DATA-CUSTOMER_NAME1
                                           E_NAME2    = ES_DATA-CUSTOMER_NAME2
                                           E_NAME3    = ES_DATA-CUSTOMER_NAME3
                                           E_NAME4    = ES_DATA-CUSTOMER_NAME4 ).
        CONCATENATE ES_DATA-CUSTOMER_NAME1
                    ES_DATA-CUSTOMER_NAME2
                    ES_DATA-CUSTOMER_NAME3
                    ES_DATA-CUSTOMER_NAME4
                    INTO ES_DATA-CUSTOMER_NAME_ALL SEPARATED BY SPACE.

        SELECT SINGLE STCD3
          FROM VBPA3
          INTO ES_DATA-TAX_NO
          WHERE VBELN = ES_DATA-DO_NO
            AND PARVW = GC_CON-AG.

        IF I_NATION EQ SPACE.
          CONCATENATE LC_CON-TAX_TH
                      LV_TAXNO
                 INTO ES_DATA-TAX_NO SEPARATED BY SPACE.
        ELSE.
          CONCATENATE LC_CON-TAX_EN
                      LV_TAXNO
                 INTO ES_DATA-TAX_NO SEPARATED BY SPACE.
        ENDIF.
      ENDIF.
    ENDIF.

    ES_DATA-REMARK = LCL_UTIL->GET_TEXT( EXPORTING I_ID       = GC_CON-DO_REMARK_ID
                                                     I_NAME     = IS_PICKING_INFO-HD_REF-REF_DOC
                                                     I_OBJECT   = GC_CON-DO_REMARK_OBJECT
                                                     I_LANGUAGE = SY-LANGU ).

    ES_DATA-INV_REMARK = LCL_UTIL->GET_TEXT( EXPORTING I_ID       = GC_CON-DO_INV_REMARK_ID
                                                       I_NAME     = IS_PICKING_INFO-HD_REF-REF_DOC
                                                       I_OBJECT   = GC_CON-DO_INV_REMARK_OBJECT
                                                       I_LANGUAGE = SY-LANGU ).
*     = 'TEST'.

*    ES_DATA-CN_REMARK = LCL_UTIL->GET_TEXT( EXPORTING I_ID       = GC_CON-DO_CN_REMARK
*                                                      I_NAME     = IS_PICKING_INFO-HD_REF-REF_DOC
*                                                      I_OBJECT   = GC_CON-DO_INV_REMARK_OBJECT
*                                                      I_LANGUAGE = SY-LANGU ).

    ES_DATA-CN_REMARK = LCL_UTIL->GET_TEXT( EXPORTING I_ID       = GC_CON-REMARK_ID
                                                     I_NAME     = IS_PICKING_INFO-HD_REF-REF_DOC
                                                     I_OBJECT   = GC_CON-DO_REMARK_OBJECT
                                                     I_LANGUAGE = SY-LANGU ).


    ES_DATA-CONTRACT_NAME = LCL_UTIL->GET_TEXT( EXPORTING I_ID       = GC_CON-DO_CONTACT_PERSON
                                                          I_NAME     = IS_PICKING_INFO-HD_REF-REF_DOC
                                                          I_OBJECT   = GC_CON-DO_INV_REMARK_OBJECT
                                                          I_LANGUAGE = SY-LANGU ).

    ES_DATA-LAND_NO = LCL_UTIL->GET_TEXT( EXPORTING I_ID       = GC_CON-DO_LAND_ID
                                                    I_NAME     = IS_PICKING_INFO-HD_REF-REF_DOC
                                                    I_OBJECT   = GC_CON-DO_INV_REMARK_OBJECT
                                                    I_LANGUAGE = SY-LANGU ).

    ES_DATA-SALES_ORDER_NO = IS_PICKING_INFO-HD_REF-REF_DOC.
    ES_DATA-BARCODE        = ES_DATA-DO_NO.

    ES_DATA-LOADING_POINT  = LCL_DATA=>GET_LOADING_POINT_DESC( I_LODING = IS_PICKING_INFO-HD_ORG-LOADING_PT
                                                               I_SHIPPG = IS_PICKING_INFO-HD_ORG-SHIP_POINT ).
    WRITE SY-DATUM TO ES_DATA-CREATE_DATE.
    WRITE SY-UZEIT TO ES_DATA-CREATE_TIME.
    ES_DATA-PROJECT_NO     = IS_PICKING_INFO-HD_REF-PURCH_NO.
    ES_DATA-PROJECT_NAME   = LCL_UTIL->GET_TEXT( EXPORTING I_ID       = GC_CON-DO_PROJECT_ID
                                                           I_NAME     = IS_PICKING_INFO-HD_REF-REF_DOC
                                                           I_OBJECT   = GC_CON-DO_PROJECT_OBJECT
                                                           I_LANGUAGE = SY-LANGU ).

*    LCL_DATA=>GET_CONFIG_WARRANTY( CHANGING CT_DATA = LR_WARANTY_GROUP ).
*    DATA(LV_WARRANTY) = LCL_DATA=>SET_WARRANTY( I_SODOC    = IS_PICKING_INFO-HD_REF-REF_DOC
*                                                I_WARRANTY = LR_WARANTY_GROUP ).

    LT_WARRANTY = GET_LINE_ITEM_WARRANTY( IS_PICKING_INFO-HD_GEN-DELIV_NUMB ).

    LOOP AT IS_PICKING_INFO-IT_GEN INTO LS_GEN.
      CLEAR : LV_CHECK_BOM.
      LV_LINE_DO = LS_GEN-ITM_NUMBER.
      LCL_DATA=>CONVERT_ALPHA_OUT( EXPORTING I_DATA = LS_GEN-ITM_NUMBER
                                   IMPORTING E_DATA = LS_GEN-ITM_NUMBER ).

      IF LS_GEN-HIERARITEM IS INITIAL.
        LS_DETAIL-LINE_ITEM        = LV_LINE."LS_GEN-ITM_NUMBER.
        ADD 1 TO LV_LINE.
        REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DETAIL-LINE_ITEM WITH ''.
        LS_DETAIL-MATERIAL_CODE    = LS_GEN-MATERIAL.
      ELSE.
        READ TABLE IS_PICKING_INFO-IT_GEN
        WITH KEY ITM_NUMBER = LS_GEN-HIERARITEM TRANSPORTING NO FIELDS.
        IF SY-SUBRC EQ 0.
          LS_DETAIL-LINE_ITEM        = SPACE.
          CONCATENATE LC_SPACE
                      LS_GEN-MATERIAL
                 INTO LS_DETAIL-MATERIAL_CODE RESPECTING BLANKS.
          LV_CHECK_BOM = ABAP_TRUE.
        ELSE.
          LS_DETAIL-LINE_ITEM        = LV_LINE."LS_GEN-ITM_NUMBER.
          ADD 1 TO LV_LINE.
          REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DETAIL-LINE_ITEM WITH ''.
          LS_DETAIL-MATERIAL_CODE    = LS_GEN-MATERIAL.
        ENDIF.
      ENDIF.

      IF LV_NO_SERIAL EQ ABAP_TRUE.
        LS_DETAIL-DESCRIPTION      = LS_GEN-SHORT_TEXT.
      ELSE.
        LS_DETAIL-DESCRIPTION      = LCL_DATA=>READ_SERIAL( IT_DATA = IS_PICKING_INFO-IT_SERNR
                                                            I_DO    = LS_GEN-DELIV_NUMB
                                                            I_ITEM  = LS_GEN-ITM_NUMBER
                                                            I_TEXT  = LS_GEN-SHORT_TEXT
                                                            I_CHECK = LV_CHECK_BOM ).

        IF LS_DETAIL-DESCRIPTION IS INITIAL.
          LS_DETAIL-DESCRIPTION      = LS_GEN-SHORT_TEXT.
          CLEAR : LS_DETAIL-MAT_DESC.
        ELSE.
          IF LS_DETAIL-LINE_ITEM IS NOT INITIAL.
            LS_DETAIL-MAT_DESC = LS_GEN-SHORT_TEXT.
          ENDIF.
        ENDIF.
      ENDIF.

      LS_DETAIL-STORAGE_LOCATION = LCL_DATA=>READ_LOCATION( IT_DATA = IS_PICKING_INFO-IT_ORG
                                                            I_DO    = LS_GEN-DELIV_NUMB
                                                            I_ITEM  = LS_GEN-ITM_NUMBER ).

      LV_QTY = LS_GEN-DLV_QTY.
      WRITE LV_QTY TO LS_DETAIL-QTY.
      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DETAIL-QTY WITH ''.

      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING
          INPUT          = LS_GEN-SALES_UNIT
          LANGUAGE       = SY-LANGU
        IMPORTING
          OUTPUT         = LS_DETAIL-BASE_UNIT
        EXCEPTIONS
          UNIT_NOT_FOUND = 1
          OTHERS         = 2.
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.

*      IF LV_WARRANTY EQ ABAP_TRUE.
      READ TABLE LT_WARRANTY INTO LS_WARRANTY
      WITH KEY VBELN = IS_PICKING_INFO-HD_GEN-DELIV_NUMB
               POSNR = LV_LINE_DO.
      IF SY-SUBRC EQ 0.
        LS_DETAIL-WARRANTY = 'WARRANTY CARD'.
      ENDIF.

      APPEND LS_DETAIL TO ET_DETAIL.
      CLEAR : LS_DETAIL.
    ENDLOOP.

  ENDMETHOD.


  METHOD MAPPING_DATA_RECEIPT.
    DATA : LS_DATA     LIKE E_DATA,
           LS_DATA_TAB LIKE LINE OF ET_DATA.

    DATA : LS_ADR LIKE LINE OF I_BILL_INFO-HD_ADR.

    DATA : LS_PART_ADD LIKE LINE OF I_BILL_INFO-HD_PART_ADD.

    DATA : LS_GEN LIKE LINE OF I_BILL_INFO-IT_GEN.

    DATA : LS_PRICE LIKE LINE OF I_BILL_INFO-IT_PRICE.

    DATA : LCL_UTIL TYPE REF TO ZCL_SDSCA_UTIL_SDS.

    DATA : LT_SERIAL_TAB LIKE ET_DATA,
           LS_SERIAL_TAB LIKE LINE OF ET_DATA.

    DATA : LS_KOND LIKE LINE OF I_BILL_INFO-HD_KOND.

    DATA : LV_LINE_ITEM TYPE I.

    DATA : LV_LANGU TYPE SY-LANGU.

    DATA : LV_BRANCH TYPE STRING.

    CONSTANTS : BEGIN OF LC_CON,
                  E      TYPE C LENGTH 1 VALUE 'E',
                  TWO    TYPE C LENGTH 1 VALUE '2',
                  TAX_TH TYPE C LENGTH 100 VALUE 'เลขประจำตัวผู้เสียภาษี',
                  TAX_EN TYPE C LENGTH 100 VALUE 'Tax ID No.',
                  INITL  TYPE C LENGTH 6 VALUE '000000',
                END OF LC_CON.

    IF LCL_UTIL IS NOT BOUND.
      CREATE OBJECT LCL_UTIL.
    ENDIF.

    IF I_NATION EQ SPACE.
      LV_LANGU = LC_CON-TWO.
    ELSE.
      LV_LANGU = LC_CON-E.
    ENDIF.

    IF I_BILL_INFO-HD_GEN-BUPLA IS INITIAL.
      DATA(LV_BUPLA) = LCL_DATA=>GET_BRANCH( I_BILL_INFO-HD_GEN-BIL_NUMBER ).
    ELSE.
      LV_BUPLA = I_BILL_INFO-HD_GEN-BUPLA.
    ENDIF.


    LCL_DATA=>GET_CONDITION_HEADER_COMPANY( EXPORTING I_DATA = I_BILL_INFO-HD_REF-ORDER_NUMB
                                             CHANGING C_DATA = LV_BUPLA ).
    IF LV_BUPLA IS INITIAL.
      LV_BUPLA = I_BILL_INFO-HD_GEN-BUPLA.
    ENDIF.

    LCL_UTIL->GET_SDS_ADDRESS( EXPORTING I_BRANCH     = LV_BUPLA "I_BILL_INFO-HD_GEN-BUPLA
                               IMPORTING E_ADDRESS_TH = E_DATA-SDS_ADDRESS_TH
                                         E_ADDRESS_EN = E_DATA-SDS_ADDRESS_EN ).

    E_DATA-BILNO  = I_BILL_INFO-HD_GEN-BIL_NUMBER.
    E_DATA-DO_NO  = LCL_DATA=>GEN_DO_NO( I_BILL_INFO-IT_REFDLV ).
    E_DATA-SO_NO  = LCL_DATA=>GEN_SO_NO( I_BILL_INFO-IT_REFORD ).
*    E_DATA-WBS_NO = LCL_DATA=>GET_WBS( I_BILL_INFO-HD_GEN-BIL_NUMBER ).
*    E_DATA-PROFIT = LCL_DATA=>GET_PROFIT( I_BILL_INFO-HD_GEN-BIL_NUMBER ).
    LCL_DATA=>GET_SINGLE_VBRP( EXPORTING I_DATA = I_BILL_INFO-HD_GEN-BIL_NUMBER
                                CHANGING C_DATA = E_DATA ).

    E_DATA-PO_NO  = I_BILL_INFO-HD_REF-PURCH_NO_C.

    E_DATA-SIGNC = 'ZSDS_6913'.


    E_DATA-PROJECT_NAME = LCL_UTIL->GET_TEXT( EXPORTING I_ID       = GC_CON-INV_PROJECT_ID
                                                        I_NAME     = I_BILL_INFO-HD_REF-ORDER_NUMB
                                                        I_OBJECT   = GC_CON-INV_PROJECT_OBJECT
                                                        I_LANGUAGE = SY-LANGU ).

    E_DATA-LAND = LCL_UTIL->GET_TEXT( EXPORTING I_ID       = GC_CON-INV_LAND_ID
                                                I_NAME     = I_BILL_INFO-HD_REF-ORDER_NUMB
                                                I_OBJECT   = GC_CON-INV_LAND_OBJECT
                                                I_LANGUAGE = SY-LANGU ).

    SELECT SINGLE BELNR,
                  XBLNR
    FROM BKPF
    INTO (@DATA(LV_FI_DOC),@DATA(LV_TAX_INV))
    WHERE AWKEY EQ @I_BILL_INFO-HD_GEN-BIL_NUMBER.
    IF SY-SUBRC EQ 0.
      E_DATA-VBELN   = LV_FI_DOC.
      E_DATA-TAX_INV = LV_TAX_INV.
    ELSE.
      E_DATA-VBELN = I_BILL_INFO-HD_GEN-BIL_NUMBER.
    ENDIF.
    E_DATA-FKDAT = I_BILL_INFO-HD_GEN-BIL_DATE.
    E_DATA-CREDT = LCL_DATA=>CONVERT_DATE( I_BILL_INFO-HD_GEN-BIL_DATE ).
*    WRITE I_BILL_INFO-HD_GEN-BIL_EDATE TO E_DATA-CREDT.
    E_DATA-KUNNR = I_BILL_INFO-HD_GEN-PAYER.

    LCL_UTIL->GET_CUST_NAME( EXPORTING I_CUST_NO  = E_DATA-KUNNR
                                       I_NATION   = I_NATION
                             IMPORTING E_NAME1    = E_DATA-NAME1
                                       E_NAME2    = E_DATA-NAME2
                                       E_NAME3    = E_DATA-NAME3
                                       E_NAME4    = E_DATA-NAME4 ).

    LV_BRANCH = LCL_UTIL->GET_BRANCH( I_KUNNR = E_DATA-KUNNR
                                      I_LANGU = LV_LANGU ).

    CONCATENATE E_DATA-NAME1
                E_DATA-NAME2
                E_DATA-NAME3
*                E_DATA-NAME4
                LV_BRANCH
           INTO E_DATA-NAMEALL SEPARATED BY SPACE.

    SELECT SINGLE STCD3
      FROM KNA1
      INTO @DATA(LV_TAXNO)
      WHERE KUNNR EQ @E_DATA-KUNNR.
    IF SY-SUBRC = 0 AND LV_TAXNO <> '0000000000000'.
      IF I_NATION EQ SPACE.
        CONCATENATE LC_CON-TAX_TH
                    LV_TAXNO
               INTO E_DATA-TAX_NO SEPARATED BY SPACE.
      ELSE.
        CONCATENATE LC_CON-TAX_EN
                    LV_TAXNO
               INTO E_DATA-TAX_NO SEPARATED BY SPACE.
      ENDIF.
    ENDIF.

    READ TABLE I_BILL_INFO-HD_PART_ADD INTO LS_PART_ADD
    WITH KEY PARTN_ROLE = GC_CON-PARTN_ROLE.
    IF SY-SUBRC EQ 0.
      E_DATA-PERNR = LS_PART_ADD-PARTN_NUMB.
*      LCL_UTIL->GET_SALE_NAME( EXPORTING I_SALE_NO   = E_DATA-PERNR
*                               IMPORTING E_SALE_NAEM = E_DATA-SALEN ).
    ENDIF.
    E_DATA-SALEN = LCL_DATA=>GET_SALES_GROUP_DESC( I_BILL_INFO-HD_REF-ORDER_NUMB ).

    IF I_BILL_INFO-HD_GEN-TERMS_PAYM EQ GC_CON-ZZZZ.
      E_DATA-DUEDT = I_BILL_INFO-HD_GEN-VAL_DATE.
    ELSE.
      E_DATA-DUEDT = LCL_UTIL->GET_PAYMENT_DATE( I_DATE = E_DATA-FKDAT
                                                 I_TERM = I_BILL_INFO-HD_GEN-TERMS_PAYM ).
    ENDIF.
*    WRITE E_DATA-DUEDT TO E_DATA-DUETT.
    E_DATA-DUETT = LCL_DATA=>CONVERT_DATE( E_DATA-DUEDT ).
    READ TABLE I_BILL_INFO-HD_ADR INTO LS_ADR
    WITH KEY PARTN_ROLE = GC_CON-RE.
    IF SY-SUBRC EQ 0.
      LCL_UTIL->GET_ADDRESS( EXPORTING I_ADDRESS_NO           = LS_ADR-ADDR_NO
                                       I_NATION               = I_NATION
                             IMPORTING E_ADDRESS_CONCAT_LINE1 = E_DATA-ADDR1
                                       E_ADDRESS_CONCAT_LINE2 = E_DATA-ADDR2
                                       E_ADDRESS_CONCAT_LINE3 = E_DATA-ADDR3
                                       E_ADDRESS_CONCAT_LINE4 = E_DATA-ADDR4 ).
    ENDIF.

    IF E_DATA-KUNNR CP 'OT*'.
      CLEAR: E_DATA-NAMEALL,
             E_DATA-TAX_NO.

      SELECT SINGLE ADRNR
        INTO @DATA(LV_ADRNR_RE)
        FROM VBPA
        WHERE VBELN = @I_BILL_INFO-HD_GEN-BIL_NUMBER
          AND PARVW = @GC_CON-RE.
      IF SY-SUBRC = 0.
        LV_ADRNR_RE = LV_ADRNR_RE.
      ELSE.
        LV_ADRNR_RE = LS_ADR-ADDR_NO.
      ENDIF.

      LCL_UTIL->GET_CUST_NAME( EXPORTING I_CUST_NO      = E_DATA-KUNNR
                                         I_NATION       = I_NATION
                                         I_ADDRNUMBER   = LV_ADRNR_RE
                               IMPORTING E_NAME1    = E_DATA-NAME1
                                         E_NAME2    = E_DATA-NAME2
                                         E_NAME3    = E_DATA-NAME3
                                         E_NAME4    = E_DATA-NAME4 ).

      LCL_UTIL->GET_ADDRESS( EXPORTING I_ADDRESS_NO           = LV_ADRNR_RE
                                       I_NATION               = I_NATION
                             IMPORTING E_ADDRESS_CONCAT_LINE1 = E_DATA-ADDR1
                                       E_ADDRESS_CONCAT_LINE2 = E_DATA-ADDR2
                                       E_ADDRESS_CONCAT_LINE3 = E_DATA-ADDR3
                                       E_ADDRESS_CONCAT_LINE4 = E_DATA-ADDR4
                                       E_TAXID                = E_DATA-TAX_NO ).

      CONCATENATE E_DATA-NAME1
                  E_DATA-NAME2
                  E_DATA-NAME3
*                  E_DATA-NAME4
                  INTO E_DATA-NAMEALL SEPARATED BY SPACE.

      IF E_DATA-TAX_NO IS INITIAL.
        SELECT SINGLE STCD3
          FROM VBPA3
          INTO LV_TAXNO
          WHERE VBELN = I_BILL_INFO-HD_GEN-BIL_NUMBER
            AND PARVW = GC_CON-AG.
        IF SY-SUBRC = 0 AND LV_TAXNO <> '0000000000000'.
          IF I_NATION EQ SPACE.
            CONCATENATE LC_CON-TAX_TH
                        LV_TAXNO
                   INTO E_DATA-TAX_NO SEPARATED BY SPACE.
          ELSE.
            CONCATENATE LC_CON-TAX_EN
                        LV_TAXNO
                   INTO E_DATA-TAX_NO SEPARATED BY SPACE.
          ENDIF.
        ENDIF.
      ELSE.
        IF E_DATA-TAX_NO = '0000000000000'.
          CLEAR E_DATA-TAX_NO.
        ENDIF.
      ENDIF.

    ENDIF.


    E_DATA-REMAK = LCL_UTIL->GET_TEXT( EXPORTING I_ID       = GC_CON-REMARK_ID
                                                 I_NAME     = E_DATA-VBELN
                                                 I_OBJECT   = GC_CON-REMARK_OBJECT
                                                 I_LANGUAGE = SY-LANGU ).


    DATA(LV_CHECK_SUM) = LCL_UTIL->GET_TEXT( EXPORTING I_ID      = GC_CON-CHECK_SUM
                                                      I_NAME     = I_BILL_INFO-HD_GEN-BIL_NUMBER
                                                      I_OBJECT   = GC_CON-REMARK_OBJECT
                                                      I_LANGUAGE = GC_CON-ENG ).
    IF LV_CHECK_SUM IS INITIAL.
      LV_CHECK_SUM = LCL_UTIL->GET_TEXT( EXPORTING  I_ID      = GC_CON-CHECK_SUM
                                                    I_NAME     = I_BILL_INFO-HD_GEN-BIL_NUMBER
                                                    I_OBJECT   = GC_CON-REMARK_OBJECT
                                                    I_LANGUAGE = GC_CON-THAI ).
    ENDIF.

    E_DATA-ADVAN = ''.

*  E_DATA-SIGNC = ''.
*  E_DATA-SIGNR = ''.

*    LT_SERIAL_TAB = LCL_DATA=>GET_SERAIL_TAB( I_DO      = I_BILL_INFO-HD_REF-DELIV_NUMB
*                                              IT_DO_TAB = I_BILL_INFO-IT_REF ).

    DATA : LS_ITEM  LIKE LINE OF I_BILL_INFO-IT_GEN,
           LV_QTY   TYPE I,
           LV_QTY_T TYPE C LENGTH 20,
           LV_BOM   TYPE STRING.
    DATA LR_MAT_IN TYPE RANGE OF MARA-MATNR.

    "Remove material INSULATION
    ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = 'ZCL_SDSSD_GEN_DATA_SD_FORM'
                                                    IF_PARAM = 'MAT_INSULATION'
                                          IMPORTING ET_RANGE = LR_MAT_IN ).

*--------------------------------------------------------------------*
* ITEM
*--------------------------------------------------------------------*
*    IF LV_CHECK_SUM EQ ABAP_TRUE OR
*       LV_CHECK_SUM EQ 'x'.
    IF LV_CHECK_SUM IS NOT INITIAL.
      DATA : LV_SUM_TEXT TYPE STRING.
      LOOP AT I_BILL_INFO-IT_GEN INTO LS_GEN.
        LS_DATA_TAB-POSTT = '1'.
        LV_QTY   = 1.
        CONCATENATE : LS_GEN-BIL_NUMBER LS_GEN-ITM_NUMBER INTO LV_SUM_TEXT.

        DATA(LV_TEXT_SUM) = LCL_UTIL->GET_TEXT( EXPORTING I_ID       = GC_CON-ITEM_DETAIL
                                                          I_NAME     = LV_SUM_TEXT
                                                          I_OBJECT   = GC_CON-ITEM_OBJECT
                                                          I_LANGUAGE = SY-LANGU ).
        IF LV_TEXT_SUM IS NOT INITIAL.
          LS_DATA_TAB-MAKTX = LV_TEXT_SUM.      "Text from Item
        ELSE.
          IF LS_DATA_TAB-MAKTX IS INITIAL.
            LS_DATA_TAB-MAKTX = LV_CHECK_SUM.   "Text from Header
          ENDIF.
        ENDIF.

        READ TABLE I_BILL_INFO-IT_PRICE INTO LS_PRICE
          WITH KEY BIL_NUMBER = LS_GEN-BIL_NUMBER
                   ITM_NUMBER = LS_GEN-ITM_NUMBER.
        IF SY-SUBRC EQ 0.
          LS_DATA_TAB-PRICE = LS_DATA_TAB-PRICE + ( LS_PRICE-KZWI1 / LV_QTY ).
          LS_DATA_TAB-NETWR = LS_DATA_TAB-NETWR + ( LS_PRICE-NETWR  / LV_QTY ).
          LS_DATA_TAB-DISCT = LS_DATA_TAB-PRICE - LS_DATA_TAB-NETWR.
          LS_DATA_TAB-TOTAL = LS_DATA_TAB-TOTAL + LS_PRICE-NETWR .
        ENDIF.
      ENDLOOP.
      WRITE LV_QTY TO LS_DATA_TAB-QTYTT.
      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DATA_TAB-QTYTT WITH ''.
      ADD LS_DATA_TAB-TOTAL TO E_DATA-AMONT.

      WRITE LS_DATA_TAB-PRICE TO LS_DATA_TAB-PRITT.
      WRITE LS_DATA_TAB-DISCT TO LS_DATA_TAB-DISTT.
      WRITE LS_DATA_TAB-NETWR TO LS_DATA_TAB-NETTT.
      WRITE LS_DATA_TAB-TOTAL TO LS_DATA_TAB-TOTTT.
      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DATA_TAB-PRITT WITH ''.
      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DATA_TAB-DISTT WITH ''.
      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DATA_TAB-NETTT WITH ''.
      REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DATA_TAB-TOTTT WITH ''.

      APPEND LS_DATA_TAB TO ET_DATA.

    ELSE.
      LOOP AT I_BILL_INFO-IT_GEN INTO LS_GEN.
*
        IF LS_GEN-UEPOS NE LC_CON-INITL.
          READ TABLE I_BILL_INFO-IT_GEN
          WITH KEY ITM_NUMBER = LS_GEN-UEPOS TRANSPORTING NO FIELDS.
          IF SY-SUBRC EQ 0.
            CONTINUE.
          ENDIF.
        ENDIF.

        "Skip material Insulation
        IF LR_MAT_IN[] IS NOT INITIAL.
          IF LS_GEN-MATERIAL IN LR_MAT_IN[].
            READ TABLE I_BILL_INFO-IT_PRICE INTO DATA(LS_PRICE_CHECK)
            WITH KEY BIL_NUMBER = LS_ITEM-BIL_NUMBER
                     ITM_NUMBER = LS_ITEM-ITM_NUMBER.
            IF SY-SUBRC EQ 0.
              IF LS_PRICE_CHECK-NETWR EQ 0.
                CONTINUE.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        LS_DATA_TAB-VBELN = LS_GEN-BIL_NUMBER.
*      LS_DATA_TAB-POSNR = LS_GEN-ITM_NUMBER.
*    LS_DATA_TAB-POSTT = LS_DATA_TAB-POSNR.
        ADD 1 TO LV_LINE_ITEM.
        LS_DATA_TAB-POSNR = LV_LINE_ITEM.

        LCL_DATA=>CONVERT_ALPHA_OUT( EXPORTING I_DATA = LS_DATA_TAB-POSNR
                                     IMPORTING E_DATA = LS_DATA_TAB-POSTT ).

        LS_DATA_TAB-MATNR = LS_GEN-MATERIAL.
        IF LS_GEN-MATERIAL EQ 'FIDOC'.
          CLEAR : LS_DATA_TAB-MATNR.
          CONCATENATE LS_GEN-SHORT_TEXT LS_GEN-MAT_ENTRD
                 INTO LS_DATA_TAB-MAKTX.
        ELSE.
          LS_DATA_TAB-MAKTX = LS_GEN-SHORT_TEXT.
        ENDIF.
        LS_DATA_TAB-FKIMG = LS_GEN-FKIMG.

        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
          EXPORTING
            INPUT          = LS_GEN-SALES_UNIT
            LANGUAGE       = SY-LANGU
          IMPORTING
            OUTPUT         = LS_DATA_TAB-VRKME
          EXCEPTIONS
            UNIT_NOT_FOUND = 1
            OTHERS         = 2.
        IF SY-SUBRC <> 0.
* Implement suitable error handling here
        ENDIF.

        WRITE LS_DATA_TAB-FKIMG TO LS_DATA_TAB-QTYTT.
        REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DATA_TAB-QTYTT WITH ''.
*      IF LS_GEN-UEPOS NE LC_CON-INITL.
*
*
*      ELSE.


        LOOP AT I_BILL_INFO-IT_GEN INTO LS_ITEM WHERE UEPOS EQ LS_GEN-ITM_NUMBER.
          IF LR_MAT_IN[] IS NOT INITIAL.
            IF LS_ITEM-MATERIAL IN LR_MAT_IN[].
              READ TABLE I_BILL_INFO-IT_PRICE INTO DATA(LS_PRICE_CHECK1)
              WITH KEY BIL_NUMBER = LS_ITEM-BIL_NUMBER
                       ITM_NUMBER = LS_ITEM-ITM_NUMBER.
              IF SY-SUBRC EQ 0.
                IF LS_PRICE_CHECK1-NETWR EQ 0.
                  CONTINUE.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

          LV_QTY   = LS_ITEM-FKIMG.
          LV_QTY_T = LV_QTY.
          REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LV_QTY_T WITH ''.
          IF LV_BOM IS INITIAL.
            CONCATENATE LS_ITEM-MATERIAL '(' LV_QTY_T ')' INTO LV_BOM.
          ELSE.
            CONCATENATE LV_BOM '+' LS_ITEM-MATERIAL '(' LV_QTY_T ')' INTO LV_BOM.
          ENDIF.

          READ TABLE I_BILL_INFO-IT_PRICE INTO LS_PRICE
          WITH KEY BIL_NUMBER = LS_ITEM-BIL_NUMBER
                   ITM_NUMBER = LS_ITEM-ITM_NUMBER.
          IF SY-SUBRC EQ 0.
            LS_DATA_TAB-PRICE = LS_DATA_TAB-PRICE + ( LS_PRICE-KZWI1 / LV_QTY ).
            LS_DATA_TAB-NETWR = LS_DATA_TAB-NETWR + ( LS_PRICE-NETWR  / LV_QTY ).
            LS_DATA_TAB-DISCT = LS_DATA_TAB-PRICE - LS_DATA_TAB-NETWR.
            LS_DATA_TAB-TOTAL = LS_DATA_TAB-TOTAL + LS_PRICE-NETWR .
          ENDIF.
          SY-SUBRC = 0.
        ENDLOOP.
        IF SY-SUBRC NE 0.
          LV_QTY   = LS_GEN-FKIMG.
          LV_QTY_T = LV_QTY.

          READ TABLE I_BILL_INFO-IT_PRICE INTO LS_PRICE
          WITH KEY BIL_NUMBER = LS_GEN-BIL_NUMBER
                   ITM_NUMBER = LS_GEN-ITM_NUMBER.
          IF SY-SUBRC EQ 0.
*          LS_DATA_TAB-PRICE = LS_PRICE-KZWI1.
*          LS_DATA_TAB-DISCT = ABS( LS_PRICE-KZWI4 ).
*          LS_DATA_TAB-NETWR = LS_PRICE-NETWR.
*          LS_DATA_TAB-TOTAL = ( LS_PRICE-NETWR * LS_DATA_TAB-FKIMG ) - LS_DATA_TAB-DISCT.
            LS_DATA_TAB-PRICE =  ( LS_PRICE-KZWI1 / LV_QTY ).
            LS_DATA_TAB-NETWR =  ( LS_PRICE-NETWR  / LV_QTY ).
            LS_DATA_TAB-DISCT =  LS_DATA_TAB-PRICE - LS_DATA_TAB-NETWR.
            LS_DATA_TAB-TOTAL =  LS_PRICE-NETWR .
          ENDIF.
        ENDIF.
*      ENDIF.

        ADD LS_DATA_TAB-TOTAL TO E_DATA-AMONT.

        WRITE LS_DATA_TAB-PRICE TO LS_DATA_TAB-PRITT.
        WRITE LS_DATA_TAB-DISCT TO LS_DATA_TAB-DISTT.
        WRITE LS_DATA_TAB-NETWR TO LS_DATA_TAB-NETTT.
        WRITE LS_DATA_TAB-TOTAL TO LS_DATA_TAB-TOTTT.
        REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DATA_TAB-PRITT WITH ''.
        REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DATA_TAB-DISTT WITH ''.
        REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DATA_TAB-NETTT WITH ''.
        REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DATA_TAB-TOTTT WITH ''.

        APPEND LS_DATA_TAB TO ET_DATA.
        CLEAR : LS_DATA_TAB.
        LOOP AT LT_SERIAL_TAB INTO LS_SERIAL_TAB WHERE VBELN EQ LS_GEN-BIL_NUMBER AND
                                                       POSNR EQ LS_GEN-ITM_NUMBER.

          LS_DATA_TAB-MATNR = LS_SERIAL_TAB-MATNR.
          LS_DATA_TAB-MAKTX = LS_SERIAL_TAB-MAKTX.
          APPEND LS_DATA_TAB TO ET_DATA.
          CLEAR : LS_DATA_TAB.
        ENDLOOP.

        IF LS_GEN-UEPOS EQ LS_GEN-UEPOS.
*        LOOP AT I_BILL_INFO-IT_GEN INTO LS_ITEM WHERE UEPOS EQ LS_GEN-ITM_NUMBER.
*          LV_QTY   = LS_GEN-FKIMG.
*          LV_QTY_T = LV_QTY.
*          REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LV_QTY_T WITH ''.
*          IF LS_DATA_TAB-MAKTX IS INITIAL.
*            CONCATENATE LS_ITEM-MATERIAL '(' LV_QTY_T ')' INTO LV_BOM.
*          ELSE.
*            CONCATENATE LV_BOM '+' LS_ITEM-MATERIAL '(' LV_QTY_T ')' INTO LV_BOM.
*          ENDIF.
*        ENDLOOP.
          IF LV_BOM IS NOT INITIAL.
            LS_DATA_TAB-MAKTX = LV_BOM.
            APPEND LS_DATA_TAB TO ET_DATA.
            CLEAR : LS_DATA_TAB,LV_BOM.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    READ TABLE I_BILL_INFO-HD_KOND INTO LS_KOND
    WITH KEY BIL_NUMBER = I_BILL_INFO-HD_GEN-BIL_NUMBER.
    IF SY-SUBRC EQ 0.
      E_DATA-TAXAM = LS_KOND-KWERT.
    ENDIF.

    E_DATA-TOTAL = E_DATA-AMONT + E_DATA-ADVAN.
    E_DATA-NETWR = E_DATA-TOTAL + E_DATA-TAXAM.

    E_DATA-TXTAM = LCL_DATA=>CONVERT_AMOUNT_TO_TEXT( I_NETWR = E_DATA-NETWR
                                                     I_CURR  = GC_CON-THB
                                                     I_LANGU = LV_LANGU ).

    WRITE E_DATA-AMONT TO E_DATA-AMOTT.
    WRITE E_DATA-ADVAN TO E_DATA-ADVTT.
    WRITE E_DATA-TOTAL TO E_DATA-TOTTT.
    WRITE E_DATA-TAXAM TO E_DATA-TAXTT.
    WRITE E_DATA-NETWR TO E_DATA-NETTT.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN E_DATA-AMOTT WITH ''.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN E_DATA-ADVTT WITH ''.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN E_DATA-TOTTT WITH ''.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN E_DATA-TAXTT WITH ''.
    REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN E_DATA-NETTT WITH ''.


  ENDMETHOD.
ENDCLASS.
