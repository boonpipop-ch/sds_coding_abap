FUNCTION Z_SDSSD_IF_SOINV.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_MESTYP) TYPE  EDIDC-MESTYP OPTIONAL
*"     VALUE(IS_PATHNAME) TYPE  RLGRAP-FILENAME OPTIONAL
*"     VALUE(IS_FILENAME) TYPE  RLGRAP-FILENAME OPTIONAL
*"     VALUE(IS_DATE) TYPE  ZRANGE_T_DATS OPTIONAL
*"     VALUE(IS_UNAME) TYPE  ZRANGE_T_ERNAM OPTIONAL
*"     VALUE(I_AL11) TYPE  CHAR255 OPTIONAL
*"  EXPORTING
*"     VALUE(E_STATUS) TYPE  FLAG
*"  CHANGING
*"     VALUE(CT_EDIDC) TYPE  EDIDC_TT OPTIONAL
*"     VALUE(CT_EDIDD) TYPE  EDIDD_TT OPTIONAL
*"     VALUE(CT_RESULT) TYPE  EREC_T_STRING OPTIONAL
*"----------------------------------------------------------------------
  DATA: GV_FIELD(80),
        GV_FIELD_OUT(80),
        GV_DEC             TYPE P DECIMALS 0,
        GCL_TABLEDESCR_REF TYPE REF TO CL_ABAP_TABLEDESCR,
        GCL_DESCR_REF      TYPE REF TO CL_ABAP_STRUCTDESCR,
        GS_IDETAILS        TYPE ABAP_COMPDESCR_TAB,
        GS_XDETAILS        TYPE ABAP_COMPDESCR.
* >> INV data
  DATA: GT_VBAK  TYPE TABLE OF VBAK WITH HEADER LINE,
        GS_VBAK  TYPE VBAK,
        GT_VBRP  TYPE TABLE OF VBRP WITH HEADER LINE,
        GS_VBRP  TYPE VBRP,
        GT_VBRK  TYPE TABLE OF VBRK WITH HEADER LINE,
        GS_VBRK  TYPE VBRK,
        GT_VBPA  TYPE TABLE OF VBPA WITH HEADER LINE,
        GS_VBPA  TYPE VBPA,
        GT_KONV  TYPE TABLE OF PRCD_ELEMENTS WITH HEADER LINE,
        GS_KONV  TYPE PRCD_ELEMENTS,
        GS_T052  TYPE T052,
        GV_ZBD1T TYPE BSID-ZBD1T,
        GV_FAEDT TYPE RFPOS-FAEDT.
* <<
  DATA: GV_ZERO TYPE P DECIMALS 2 VALUE 0.      "CH01++
  FIELD-SYMBOLS: <FS>,<FS_OUT>.

**  CLEAR: gs_idoc_control.
  REFRESH: GT_INV_OUT_H,GT_INV_OUT_I,GS_IDETAILS.
  ",gt_edidc,gt_edidd.

  "Get component GT_OUT
  GCL_TABLEDESCR_REF ?= CL_ABAP_TYPEDESCR=>DESCRIBE_BY_DATA( GT_INV_OUT_H ).
  GCL_DESCR_REF ?= GCL_TABLEDESCR_REF->GET_TABLE_LINE_TYPE( ).
  GS_IDETAILS[] = GCL_DESCR_REF->COMPONENTS[].

* >> Get Data
  REFRESH : GT_VBAK, GT_VBRK, GT_VBRP, GT_VBPA, GT_KONV.

  SELECT * FROM VBRK
    INTO TABLE GT_VBRK
    WHERE FKDAT IN IS_DATE.
*    AND   FKART = 'ZIS4'.
*    AND   ernam IN i_uname.
  IF SY-SUBRC = 0.
    SELECT * FROM VBRP
      INTO TABLE GT_VBRP
      FOR ALL ENTRIES IN GT_VBRK
      WHERE VBELN = GT_VBRK-VBELN.

    SELECT * FROM PRCD_ELEMENTS
      INTO TABLE GT_KONV
      FOR ALL ENTRIES IN GT_VBRK
      WHERE KNUMV = GT_VBRK-KNUMV.
  ENDIF.
* << Get Data

* >> Process Data
  LOOP AT GT_VBRK.

    "Header
    CLEAR GS_INV_OUT_H.
    GS_INV_OUT_H-IND   = 'H'.

    GS_INV_OUT_H-BELNR = GT_VBRK-VBELN. "inv no.
    GS_INV_OUT_H-ID = GT_VBRK-VBELN. "inv no.
    GS_INV_OUT_H-FKDAT = GT_VBRK-FKDAT. "inv date.
    GS_INV_OUT_H-NETPR = GT_VBRK-NETWR. "net amt
    GS_INV_OUT_H-VAT_AMT = GT_VBRK-MWSBK. "vat amt
    GS_INV_OUT_H-DMBTR = GS_INV_OUT_H-NETPR + GS_INV_OUT_H-VAT_AMT. "total amt

    "due date
    SELECT SINGLE * FROM T052 INTO GS_T052
      WHERE ZTERM = GT_VBRK-ZTERM.
    IF SY-SUBRC = 0.

      GV_ZBD1T = GS_T052-ZTAG1.

      CALL FUNCTION 'NET_DUE_DATE_GET'
        EXPORTING
          I_ZFBDT = GT_VBRK-FKDAT
          I_ZBD1T = GV_ZBD1T
          I_ZBD2T = '0'
          I_ZBD3T = '0'
          I_SHKZG = 'S'
          I_REBZG = '1'
*         I_KOART = 'D'
        IMPORTING
          E_FAEDT = GV_FAEDT. "payment due date

      GS_INV_OUT_H-ZFBDT = GV_FAEDT.

    ENDIF.

    CLEAR GS_VBPA.
    SELECT SINGLE * FROM VBPA INTO GS_VBPA
      WHERE VBELN = GT_VBRK-VBELN
      AND   PARVW = 'RE'.
    IF SY-SUBRC = 0.
      GS_INV_OUT_H-KUNUM = GS_VBPA-KUNNR. "bill to party
    ENDIF.

*    CLEAR gs_konv.
*    READ TABLE gt_konv INTO gs_konv
*    WITH KEY knumv = gt_vbrk-knumv
*             kschl = 'PR00'
*             kappl = 'V'.
*    IF sy-subrc = 0.
*      gs_inv_out_h-netpr_tot = gs_konv-kwert. "total net item amt
*    ENDIF.
    CLEAR GS_KONV.
    LOOP AT GT_KONV INTO GS_KONV
                    WHERE KNUMV = GT_VBRK-KNUMV
                      AND KSCHL = 'ZPR0'
                      AND KAPPL = 'V'.
      GS_INV_OUT_H-NETPR_TOT = GS_INV_OUT_H-NETPR_TOT + GS_KONV-KWERT. "total net item amt
    ENDLOOP.

*    CLEAR gs_konv.
*    READ TABLE gt_konv INTO gs_konv
*    WITH KEY knumv = gt_vbrk-knumv
*             kschl = 'ZD03'
*             kappl = 'V'.
*    IF sy-subrc = 0.
*      gs_inv_out_h-discount = gs_konv-kwert. "header discount
*    ENDIF.
    CLEAR GS_KONV.
    LOOP AT GT_KONV INTO GS_KONV
                WHERE KNUMV = GT_VBRK-KNUMV
                  AND KSCHL = 'ZD03'
                  AND KAPPL = 'V'.
      GS_INV_OUT_H-DISCOUNT = GS_INV_OUT_H-DISCOUNT + GS_KONV-KWERT. "header discount
    ENDLOOP.

    CLEAR GS_VBRP.
    READ TABLE GT_VBRP INTO GS_VBRP
    WITH KEY VBELN = GT_VBRK-VBELN.
    IF SY-SUBRC = 0.

      GS_INV_OUT_H-VBELN = GS_VBRP-AUBEL. "so no.
      GS_INV_OUT_H-VBELN_DO = GS_VBRP-VGBEL. "do no.

      SELECT SINGLE * FROM VBAK
        INTO GS_VBAK
        WHERE VBELN = GS_VBRP-AUBEL
        AND   ERNAM IN IS_UNAME.
*        AND   AUART IN ('ZS04','ZS05').
      IF SY-SUBRC = 0.
        GS_INV_OUT_H-WEBNO = GS_VBAK-BNAME. "web no.
      ELSE.
        CLEAR GS_INV_OUT_H.
        CONTINUE.
      ENDIF.

    ENDIF.

    APPEND GS_INV_OUT_H TO GT_INV_OUT_H.

* >> Add to IDOC
*    gs_edidd-segnam = 'ZDCS13H'.
*    gs_edidd-sdata = gs_inv_out_h.
*
*    APPEND gs_edidd TO gt_edidd. CLEAR gs_edidd.
    CLEAR: GS_INV_OUT_H.
* << Add to IDOC

  ENDLOOP.

  LOOP AT GT_VBRP.

    READ TABLE GT_INV_OUT_H WITH KEY BELNR = GT_VBRP-VBELN
    TRANSPORTING NO FIELDS.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    "Item
    CLEAR GS_INV_OUT_I.
    GS_INV_OUT_I-IND   = 'I'.

    GS_INV_OUT_I-NETPR = GT_VBRP-NETWR. "net item amt

    READ TABLE GT_VBRK INTO GS_VBRK
    WITH KEY VBELN = GT_VBRP-VBELN.
    IF SY-SUBRC = 0.
      GS_INV_OUT_I-BELNR = GS_VBRK-VBELN. "inv no.
      GS_INV_OUT_I-ID = GS_VBRK-VBELN. "inv no.
    ENDIF.

    GS_INV_OUT_I-POSNR = GT_VBRP-POSNR. "inv item
    GS_INV_OUT_I-MATNR = GT_VBRP-MATNR. "mat no.
    GS_INV_OUT_I-KWMENG = GT_VBRP-FKIMG. "qty

    CLEAR GS_KONV.
    READ TABLE GT_KONV INTO GS_KONV
    WITH KEY KNUMV = GS_VBRK-KNUMV
             KPOSN = GT_VBRP-POSNR
             KSCHL = 'ZPR0'
             KAPPL = 'V'.
    IF SY-SUBRC = 0.
      GS_INV_OUT_I-KBETR = GS_KONV-KBETR. "unit price
      GS_INV_OUT_I-KWERT = GS_KONV-KWERT. "item amt
*BOI CH01
    ELSE.
*      WRITE: gv_zero TO gs_inv_out_i-kbetr, "DECIMALS 2,
*             gv_zero TO gs_inv_out_i-kwert, "DECIMALS 2.
      GS_INV_OUT_I-KBETR = '0.00'.
      GS_INV_OUT_I-KWERT = '0.00'.
*      CONDENSE: gs_inv_out_i-kbetr,
*                gs_inv_out_i-kwert.
*EOI CH01
    ENDIF.

    CLEAR GS_KONV.
    READ TABLE GT_KONV INTO GS_KONV
    WITH KEY KNUMV = GS_VBRK-KNUMV
             KPOSN = GT_VBRP-POSNR
             KSCHL = 'ZD03'
             KAPPL = 'V'.
    IF SY-SUBRC = 0.
      GS_INV_OUT_I-DISCOUNT = GS_KONV-KWERT. "item discount
*BOI CH01
    ELSE.
*      WRITE: gv_zero TO gs_inv_out_i-discount DECIMALS 2.
*      CONDENSE: gs_inv_out_i-discount.
      GS_INV_OUT_I-DISCOUNT = '0.00'.
*EOI CH01
    ENDIF.

    APPEND GS_INV_OUT_I TO GT_INV_OUT_I.

* >> Add to IDOC
*    gs_edidd-segnam = 'ZDCS13I'.
*    gs_edidd-sdata = gs_inv_out_i.
*
*    APPEND gs_edidd TO gt_edidd. CLEAR gs_edidd.
    CLEAR: GS_INV_OUT_I.
* << Add to IDOC

  ENDLOOP.
*
*  SORT gt_edidd BY segnam ASCENDING.
*
** >> Process Data
*  IF gt_edidd[] IS INITIAL.
*    MESSAGE s000(38) WITH 'No data found'.
*  ELSE.
*    gs_path-pathname = IS_PATHNAME.
*    gs_path-filename = IS_FILENAME.
*    gs_edidd-segnam = ' ZSDSSDS000'.
*    gs_edidd-sdata = gs_path.
*    INSERT gs_edidd INTO gt_edidd INDEX 1.
*
*    CLEAR: gs_idoc_control.
*    SELECT SINGLE * INTO gs_edp13
*           FROM edp13
*           WHERE mestyp = IS_MESTYP.
*    gs_idoc_control-mestyp = gs_edp13-mestyp.
*    gs_idoc_control-idoctp = gs_edp13-idoctyp.
*    gs_idoc_control-rcvpor = gs_edp13-rcvpor.
*    gs_idoc_control-rcvprn = gs_edp13-rcvprn.
*    gs_idoc_control-rcvprt = gs_edp13-rcvprt.
*
*    CALL FUNCTION 'MASTER_IDOC_DISTRIBUTE'
*      EXPORTING
*        master_idoc_control                  = gs_idoc_control
**     OBJ_TYPE                             = ''
**     CHNUM                                = ''
*      TABLES
*        communication_idoc_control           = gt_edidc
*        master_idoc_data                     = gt_edidd
*      EXCEPTIONS
*        error_in_idoc_control                = 1
*        error_writing_idoc_status            = 2
*        error_in_idoc_data                   = 3
*        sending_logical_system_unknown       = 4
*        OTHERS                               = 5 .
*
*    COMMIT WORK AND WAIT.
*
*    READ TABLE gt_edidc INTO gs_edidc INDEX 1.
*    CALL FUNCTION 'EDI_DOCUMENT_DEQUEUE_LATER'
*      EXPORTING
*        docnum                 = gs_edidc-docnum
*      EXCEPTIONS
*        idoc_is_not_to_dequeue = 1.
*
*    ct_edidc[] = gt_edidc[].
*    ct_edidd[] = gt_edidd[].
*
*  ENDIF.
  CT_RESULT = ZCL_SDSCA_GEN_DATA_FILE=>GEN_DATA_FIEL( EXPORTING I_HEADER = GT_INV_OUT_H
                                                              I_ITEM      = GT_INV_OUT_I
                                                              I_GROUP_BY_DETAIL_FILED1 = 'X'
                                                              I_SEPARATOR = '","'
                                                              I_START_END_VALUE = '"').



  DATA : LCL_FTP TYPE REF TO ZCL_SDSCA_GEN_DATA_FILE.

  DATA : LV_STATUS TYPE C.

  DATA : LV_FILE TYPE EVE_TT_STRING.

  DATA : LT_FILE TYPE EREC_T_STRING,
         LS_FILE LIKE LINE OF LT_FILE.
  DATA: LV_PATH(100) TYPE C.
  DATA: LV_PATH_FILE TYPE STRING.

*  ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING   I_REPID             = 'Z_SDSSD_IF_SOINV' "LC_CON-REPID
*                                                  I_SINGLE_VALUE_FLAG = ABAP_TRUE "--> ต้องการค่าเดียว ใส่ ABAP_TRUE ต้องการหลายค่าให้ Comment
*                                                  I_PARAM             = SY-SYSID "LC_CON-SEPARATOR
**                                                  I_PARAM_EXT      =
*                                                  CHANGING  C_RETURN  = LV_PATH_FILE ).

* IF sy-sysid = 'F36'.
*     LV_PATH_FILE = '/interface/Z_DS_Dev/SDS_Dev/20_OUTBOUND/DSS/ZSDI030'.
*  ELSEIF sy-sysid = 'F46'.
*     LV_PATH_FILE = '/interface/Z_DS/SDS/20_OUTBOUND/DSS/ZSDI030'.
*  ELSE.
*     LV_PATH_FILE_SF = '/interface/Z_DS_Dev/SDS_Dev/20_OUTBOUND/DSS/ZMMI008'.
*     LV_PATH_FILE_SONY_SP = '/interface/Z_DS_Dev/SDS_Dev/20_OUTBOUND/Sony_Logi/ZMMI014'.
*  ENDIF.



  IF LCL_FTP IS NOT BOUND.
    CREATE OBJECT LCL_FTP.
  ENDIF.

*  LS_FILE = 'Hello Wold'.
*  APPEND LS_FILE TO LT_FILE.
*  LS_FILE = 'Hello Wold1'.
*  APPEND LS_FILE TO LT_FILE.
    LV_PATH_FILE = I_AL11.
  CONCATENATE 'inv_' SY-DATUM SY-TIMLO '.csv' INTO LV_PATH.
  LV_STATUS = LCL_FTP->FTP_FILE_PUT("I_WINDOW_PATH = 'DEALER_CLAIM/DEV/OUT'
*                                     I_AL11_PATH   = '/tmp'
*                                      I_AL11_PATH   = '/interface/Z_DS_Dev/SDS_Dev/20_OUTBOUND/DSS/ZMMI008'
                                   I_AL11_PATH = LV_PATH_FILE
                                   I_FILE_NAME   = LV_PATH "'TEST_FTP_FILE.txt'
                                  " I_USER        = 'ds'
                                   "I_PASS        = 'ds=20240521'
                                  " I_IP          = '172.31.136.250'
                                  " I_PORT        = '21'
                                    I_DATA_SPIDER = 'X'
                                   IT_DATA       = CT_RESULT ).


*  LV_STATUS = LCL_FTP->FTP_FILE_PUT( I_WINDOW_PATH = 'DEALER_CLAIM/DEV/OUT'
*                                     I_AL11_PATH   = '/tmp'
*                                     I_FILE_NAME   = LV_PATH "'TEST_FTP_FILE.txt'
*                                     I_USER        = 'ds'
*                                     I_PASS        = 'ds=20240521'
*                                     I_IP          = '172.31.136.250'
*                                     I_PORT        = '21'
*                                     IT_DATA       = LT_FILE ).
  IF LV_STATUS EQ 'S'.
    E_STATUS = 'S'.
  ELSE.
    E_STATUS = 'E'.
  ENDIF.
ENDFUNCTION.
