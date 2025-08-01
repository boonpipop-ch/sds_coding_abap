class ZCL_IM_SDS_MB_MIGO_BADI definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_MB_MIGO_BADI .
protected section.
private section.

  types:
    BEGIN OF GY_SERIAL_HOLD,
      DELIVERY TYPE ZSDSMMT026-DELIVERY,
      ITEM     TYPE ZSDSMMT026-ITEM,
      SERIAL   TYPE ZSDSMMT026-SERIAL,
      MBLNR    TYPE ZSDSMMT026-MBLNR,
      MJAHR    TYPE ZSDSMMT026-MJAHR,
    END OF GY_SERIAL_HOLD .
  types:
    BEGIN OF TY_S_GOSERIAL,
            SELECTED TYPE XFELD,
            SERIALNO TYPE GERNR,
            UII      TYPE UII_CHAR72,              "EHP603 IUID
            SUBRK    TYPE XFELD,                   "EHP604 TT 2007.12.21
          END OF TY_S_GOSERIAL .
  types:
    TY_T_GOSERIAL TYPE STANDARD TABLE OF TY_S_GOSERIAL WITH
                                            NON-UNIQUE DEFAULT KEY .
  types:
    BEGIN OF TY_S_GOSERIAL_KERNEL,
            GLOBAL_COUNTER TYPE MIGO_GLOBAL_COUNTER,
            T_GOSERIAL     TYPE TY_T_GOSERIAL,
          END OF TY_S_GOSERIAL_KERNEL .

  constants GF_CLASS_ID type MIGO_CLASS_ID value 'MIGO_BADI_IMPLEMENTATION1' ##NO_TEXT.
  data G_NO_INPUT type XFELD .
  data G_CANCEL type XFELD .
  data G_LINE_ID type GOITEM-GLOBAL_COUNTER .
  class-data GT_GOITEM type GOITEM_T .
  class-data GS_GOHEADER type GOHEAD .
  class-data GV_REFDOC type STRING .
  class-data:
    GT_SERIAL_HOLD TYPE TABLE OF GY_SERIAL_HOLD .
  class-data:
    GT_GOSERIAL TYPE TABLE OF TY_S_GOSERIAL_KERNEL .
ENDCLASS.



CLASS ZCL_IM_SDS_MB_MIGO_BADI IMPLEMENTATION.


  method IF_EX_MB_MIGO_BADI~CHECK_HEADER.
    DATA : LV_CHECK_SDS LIKE ABAP_TRUE.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
*    LV_CHECK_SDS = LCL_DATA=>CHECK_SDS( SY-MANDT ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*
    IF LV_CHECK_SDS EQ ABAP_TRUE.

    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*
  endmethod.


  METHOD IF_EX_MB_MIGO_BADI~CHECK_ITEM.
    DATA : LV_CHECK_SDS LIKE ABAP_TRUE.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
    LV_CHECK_SDS = LCL_DATA=>CHECK_SDS( SY-MANDT ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*
    IF LV_CHECK_SDS EQ ABAP_TRUE.
      DATA : LS_STORG TYPE LGORT_D,
             LT_STORG TYPE DFPS_LGORT_T.

      DATA : LCL_ENHANCE TYPE REF TO ZCL_SDSMM_ENHANCEMENT.

      IF LCL_ENHANCE IS NOT BOUND.
        CREATE OBJECT LCL_ENHANCE.
      ENDIF.

      LOOP AT GT_GOITEM INTO DATA(LS_DATA).
        IF LS_DATA-SHKZG EQ 'H'.
          LS_STORG = LS_DATA-LGORT.
        ELSE.
          LS_STORG = LS_DATA-UMLGO.
        ENDIF.
        APPEND LS_STORG TO LT_STORG.
      ENDLOOP.

      SORT LT_STORG.
      DELETE ADJACENT DUPLICATES FROM LT_STORG.

      IF LT_STORG IS NOT INITIAL.
        DATA(LV_CHECK) = LCL_ENHANCE->CHECK_USER_STOCK( LT_STORG ).
        IF LV_CHECK EQ ABAP_TRUE.
          MESSAGE E001(ZSDSMM01) WITH TEXT-E01.
        ENDIF.
      ENDIF.

      DATA(LV_ERROR) = LCL_ENHANCE->VALIDATION_MIGO( IT_ITEM  = GT_GOITEM
                                                     I_HEADER = GS_GOHEADER ).

      IF LV_ERROR IS NOT INITIAL.
        MESSAGE E001(ZSDSMM01) WITH LV_ERROR.
      ENDIF.
    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*
  ENDMETHOD.


  METHOD IF_EX_MB_MIGO_BADI~HOLD_DATA_DELETE.

  ENDMETHOD.


  METHOD IF_EX_MB_MIGO_BADI~HOLD_DATA_LOAD.
    DATA : LV_CHECK_SDS LIKE ABAP_TRUE.

    CONSTANTS : BEGIN OF LC_CON,
                  GO_ITEM TYPE CHAR30 VALUE 'PT_GOITEM',
                  GO_HEAD TYPE CHAR30 VALUE 'PS_GOHEAD',
                  GO_SERI TYPE CHAR30 VALUE 'PT_GOSERIAL_KERNEL',
                END OF LC_CON.

    DATA : LS_GOSERIAL LIKE LINE OF GT_GOSERIAL.

    DATA : LS_DATA_SERIAL TYPE TY_S_GOSERIAL.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
    LV_CHECK_SDS  = LCL_DATA=>CHECK_SDS( SY-MANDT ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*
    IF LV_CHECK_SDS EQ ABAP_TRUE.
*      DATA: LOREF_PREDOC   TYPE REF TO CL_MMIM_DATASTORE.
*      IF LOREF_PREDOC IS NOT BOUND.
*        CREATE OBJECT LOREF_PREDOC.
*      ENDIF.
*
      CLEAR : GT_SERIAL_HOLD.
      SELECT SINGLE MMIM_PREDOC_ORG~REFID
        FROM MMIM_PREDOC_ORG
        WHERE GUID = @I_GUID
        INTO @DATA(LV_REFID).

      SELECT DELIVERY,
             ITEM,
             SERIAL,
             MBLNR,
             MJAHR
        FROM ZSDSMMT026
        WHERE DELIVERY EQ @LV_REFID+0(10)
        INTO TABLE @GT_SERIAL_HOLD.

*      CALL METHOD LOREF_PREDOC->READ
*        EXPORTING
*          I_NAME = LC_CON-GO_ITEM
*        IMPORTING
*          E_DATA = GT_GOITEM.
**
*      CALL METHOD LOREF_PREDOC->READ
*        EXPORTING
*          I_NAME = LC_CON-GO_SERI
*        IMPORTING
*          E_DATA = GT_GOSERIAL.


    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*
  ENDMETHOD.


  METHOD IF_EX_MB_MIGO_BADI~HOLD_DATA_SAVE.
    DATA : LV_CHECK_SDS LIKE ABAP_TRUE.

    DATA : LS_GOITEM LIKE LINE OF GT_GOITEM.

    DATA : LS_DATA TYPE ZSDSMMS042,
           LT_DATA TYPE TABLE OF ZSDSMMS042.

    DATA : LS_MIGO_BADI_HOLD TYPE MIGO_BADI_HOLD.

    DATA : LT_DATA_TEXT TYPE EREC_T_STRING.

    DATA : LCL_FTP TYPE REF TO ZCL_SDSCA_GEN_DATA_FILE.

    DATA : LCL_API TYPE REF TO ZCL_SDSSD_SEND_API.

    DATA : LV_STATUS TYPE FLAG.

    DATA: BEGIN OF LS_RETURN,
            SUCCESS      TYPE STRING,
            MESSAGE      TYPE STRING,
            ERROR_CODE   TYPE STRING,
            MESSAGE_CODE TYPE STRING,
            DATA         TYPE STRING,
            NEXT_CONTROL TYPE STRING,
            ORDER_NUMBER TYPE STRING,
            SHOW_MESSAGE TYPE STRING,
            PAGER        TYPE STRING,
          END OF LS_RETURN.

    CONSTANTS : BEGIN OF LC_CON,
                  DBQ      TYPE C LENGTH 1 VALUE '"',
                  SEPARATE TYPE C LENGTH 3 VALUE '","',
                  TXT      TYPE C LENGTH 4 VALUE '.txt',
                  UNDERS   TYPE C LENGTH 1 VALUE '_',
                END OF LC_CON.

    DATA : LV_VENDOR TYPE LFA1-LIFNR.

    DATA : LCL_UTIL TYPE REF TO ZCL_SDSCA_UTIL_SDS.

    DATA : LV_MESSAGE TYPE C LENGTH 255.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
*    LV_CHECK_SDS = LCL_DATA=>CHECK_SDS( SY-MANDT ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*
    IF LV_CHECK_SDS EQ ABAP_TRUE.
*      IF LCL_UTIL IS NOT BOUND.
*        CREATE OBJECT LCL_UTIL.
*      ENDIF.
*
*      LV_VENDOR = LCL_DATA=>GET_VENDOR_HOLD( ).
*      LV_VENDOR = |{ LV_VENDOR ALPHA = IN }|.
*      LCL_UTIL->GET_VEND_NAME( EXPORTING I_VEND_NO  = LV_VENDOR
*                               IMPORTING E_NAME_ALL = DATA(LV_NAME) ).
*
*
*
*      LOOP AT GT_GOITEM INTO LS_GOITEM.
*        LS_DATA-LFART           = '0'.
*        LS_DATA-CUST_CODE       = 'SDS'.
*        LS_DATA-VBELN           = I_GUID.
*        LS_DATA-LFDAT           = GS_GOHEADER-BLDAT.
*        LS_DATA-ERDAT           = SY-DATUM.
*        LS_DATA-POSNR           = LS_GOITEM-ZEILE.
*        LS_DATA-MATNR           = LS_GOITEM-MATNR.
*        LS_DATA-ARKTX           = ''.
*        LS_DATA-LGMNG           = LS_GOITEM-ERFMG.
*        REPLACE ALL OCCURRENCES OF PCRE '[ ]+' IN LS_DATA-LGMNG WITH ''.
*        LS_DATA-KUNNR           = LV_VENDOR.
*        LS_DATA-CUST_NAME       = LV_NAME.
*        LS_DATA-DELI_TO         = ''.
*        LS_DATA-REMARK          = ''.
*        LS_DATA-PICHON          = ''.
*        LS_DATA-REMARK_PICHON   = ''.
*        LS_DATA-LGORT           = LS_GOITEM-LGORT.
*        LS_DATA-WARRANTY        = ''.
*        CONCATENATE LS_GOITEM-UMMAT_KDAUF
*                    LS_GOITEM-UMMAT_KDPOS
*               INTO LS_DATA-SO SEPARATED BY '/'.
*        LS_DATA-REQ_MAP         = ''.
*        LS_DATA-REQ_INV         = ''.
*        LS_DATA-SHIP_ADDR       = ''.
*        LS_DATA-SHIP_PROVINCE   = ''.
*        LS_DATA-POSTCODE        = ''.
*        LS_DATA-AM_PM           = ''.
*        LS_DATA-LOADING_POINT   = ''.
*        LS_DATA-SALESMAN        = ''.
*        LS_DATA-DUE_DATE        = ''.
*        LS_DATA-PO_NO           = ''.
*        LS_DATA-PROJECT         = ''.
*        LS_DATA-TERM_OF_PAYMENT = ''.
*        LS_DATA-CONTACT_NAME    = ''.
*        LS_DATA-REF_INV         = ''.
*        LS_DATA-SALES_DIV       = ''.
*        LS_DATA-SALES_OFFICE    = ''.
*        LS_DATA-SALES_GROUP     = ''.
*        LS_DATA-UOM             = ''.
*        LS_DATA-DOC_FLAG        = ''.
*        LS_DATA-MHA             = ''.
*        LS_DATA-FLAG_BOM        = ''.
*        LS_DATA-REFER_BOM       = ''.
*        LS_DATA-SPSOLC          = ''.
*        APPEND LS_DATA TO LT_DATA.
*      ENDLOOP.
*
*      IF LT_DATA IS NOT INITIAL.
*        IF LCL_API IS NOT BOUND.
*          CREATE OBJECT LCL_API.
*        ENDIF.
*
*        SELECT SINGLE *
*          FROM ZSDSMMT026 AS A
*          WHERE DOCNO EQ ( SELECT MAX( DOCNO )
*                             FROM ZSDSMMT026 AS B
*                             WHERE B~DOCNO EQ A~DOCNO )
*          INTO @DATA(LS_DOC).
*
*        DO.
*          LS_DOC-DOCNO = LS_DOC-DOCNO + 1.
*          LS_DOC-DOCNO = |{ LS_DOC-DOCNO ALPHA = IN }|.
*          CONCATENATE 'C' LS_DOC-DOCNO INTO LS_DOC-DOCTX.
*          LS_DOC-GUID = I_GUID.
*
*          INSERT ZSDSMMT026 FROM LS_DOC.
*          IF SY-SUBRC EQ 0.
*            LS_DATA-VBELN = LS_DOC-DOCTX.
*            MODIFY LT_DATA FROM LS_DATA TRANSPORTING VBELN
*                                        WHERE VBELN IS NOT INITIAL.
*            COMMIT WORK AND WAIT.
*            CONCATENATE 'Document'
*                        LS_DOC-DOCTX
*                        'has been created.'
*                   INTO LV_MESSAGE SEPARATED BY SPACE.
*            MESSAGE I001(ZSDSMM01) WITH LV_MESSAGE.
*            EXIT.
*          ENDIF.
*        ENDDO.
*
*        LCL_API->SEND_DO_FG_TO_SONY( EXPORTING IT_DATA   = LT_DATA
*                                     IMPORTING E_MESTYPE = DATA(E_TYPE)
*                                     CHANGING  C_RETURN  = LS_RETURN ).
*      ENDIF.

*      LT_DATA_TEXT = ZCL_SDSCA_GEN_DATA_FILE=>GEN_DATA_FIEL( I_ITEM            = LT_DATA
*                                                             I_START_END_VALUE = LC_CON-DBQ
*                                                             I_SEPARATOR       = LC_CON-SEPARATE ).
*
*      IF LCL_FTP IS NOT BOUND.
*        CREATE OBJECT LCL_FTP.
*      ENDIF.
*
*      DATA(LV_WINDOW_PATH) = LCL_DATA=>GET_PATH( ).
*      DATA(LV_FILE_NAME) = LCL_DATA=>GET_FILE_NAEM( ).
*      DATA(LV_AL11_PATH) = LCL_DATA=>GET_AL11_PATH( ).
*
*      CONCATENATE LV_FILE_NAME
*                  LC_CON-UNDERS
*                  SY-DATUM
*                  LC_CON-UNDERS
*                  SY-UZEIT
*                  LC_CON-TXT INTO LV_FILE_NAME.
**    CONCATENATE 'mat' SY-DATUM '.txt' INTO LV_PATH.
*      LV_STATUS = LCL_FTP->FTP_FILE_PUT( I_WINDOW_PATH = LV_WINDOW_PATH
*                                         I_AL11_PATH   = LV_AL11_PATH
*                                         I_FILE_NAME   = LV_FILE_NAME
*                                         I_USER        = 'ds'
*                                         I_PASS        = 'ds=20240521'
*                                         I_IP          = '172.31.136.249'
*                                         I_PORT        = '21'
**                                         I_DATA_SPIDER = ABAP_TRUE
*                                         IT_DATA       = LT_DATA_TEXT ).
*      IF LV_STATUS EQ 'S'.
*      ELSE.
*      ENDIF.

    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*
  ENDMETHOD.


  METHOD IF_EX_MB_MIGO_BADI~INIT.
    DATA : LV_CHECK_SDS LIKE ABAP_TRUE.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
    LV_CHECK_SDS = LCL_DATA=>CHECK_SDS( SY-MANDT ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*
    IF LV_CHECK_SDS EQ ABAP_TRUE.
      APPEND GF_CLASS_ID TO CT_INIT.
    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*
  ENDMETHOD.


  method IF_EX_MB_MIGO_BADI~LINE_DELETE.
    DATA : LV_CHECK_SDS LIKE ABAP_TRUE.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
*    LV_CHECK_SDS = LCL_DATA=>CHECK_SDS( SY-MANDT ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*
    IF LV_CHECK_SDS EQ ABAP_TRUE.

    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*
  endmethod.


  METHOD IF_EX_MB_MIGO_BADI~LINE_MODIFY.
    DATA : LV_CHECK_SDS LIKE ABAP_TRUE.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
    LV_CHECK_SDS = LCL_DATA=>CHECK_SDS( SY-MANDT ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*
    IF LV_CHECK_SDS EQ ABAP_TRUE.
      READ TABLE GT_GOITEM
      WITH KEY MBLNR = CS_GOITEM-MBLNR
               MJAHR = CS_GOITEM-MJAHR
               ZEILE = CS_GOITEM-ZEILE TRANSPORTING NO FIELDS.
      IF SY-SUBRC EQ 0.
        MODIFY GT_GOITEM FROM CS_GOITEM INDEX SY-TABIX.
      ELSE.
        APPEND CS_GOITEM TO GT_GOITEM.
      ENDIF.
    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*
  ENDMETHOD.


  method IF_EX_MB_MIGO_BADI~MAA_LINE_ID_ADJUST.
    DATA : LV_CHECK_SDS LIKE ABAP_TRUE.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
*    LV_CHECK_SDS = LCL_DATA=>CHECK_SDS( SY-MANDT ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*
    IF LV_CHECK_SDS EQ ABAP_TRUE.

    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*
  endmethod.


  method IF_EX_MB_MIGO_BADI~MODE_SET.
    DATA : LV_CHECK_SDS LIKE ABAP_TRUE.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
*    LV_CHECK_SDS = LCL_DATA=>CHECK_SDS( SY-MANDT ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*
    IF LV_CHECK_SDS EQ ABAP_TRUE.

    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*
  endmethod.


  method IF_EX_MB_MIGO_BADI~PAI_DETAIL.
    DATA : LV_CHECK_SDS LIKE ABAP_TRUE.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
*    LV_CHECK_SDS = LCL_DATA=>CHECK_SDS( SY-MANDT ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*
    IF LV_CHECK_SDS EQ ABAP_TRUE.

    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*
  endmethod.


  method IF_EX_MB_MIGO_BADI~PAI_HEADER.
    DATA : LV_CHECK_SDS LIKE ABAP_TRUE.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
*    LV_CHECK_SDS = LCL_DATA=>CHECK_SDS( SY-MANDT ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*
    IF LV_CHECK_SDS EQ ABAP_TRUE.

    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*
  endmethod.


  method IF_EX_MB_MIGO_BADI~PBO_DETAIL.
    DATA : LV_CHECK_SDS LIKE ABAP_TRUE.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
*    LV_CHECK_SDS = LCL_DATA=>CHECK_SDS( SY-MANDT ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*
    IF LV_CHECK_SDS EQ ABAP_TRUE.

    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*
  endmethod.


  method IF_EX_MB_MIGO_BADI~PBO_HEADER.
    DATA : LV_CHECK_SDS LIKE ABAP_TRUE.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
*    LV_CHECK_SDS = LCL_DATA=>CHECK_SDS( SY-MANDT ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*
    IF LV_CHECK_SDS EQ ABAP_TRUE.

    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*
  endmethod.


  METHOD IF_EX_MB_MIGO_BADI~POST_DOCUMENT.
    DATA : LV_CHECK_SDS LIKE ABAP_TRUE.

    DATA : LS_SERIAL_HOLD LIKE LINE OF GT_SERIAL_HOLD.

    DATA : S_MJAHR TYPE RANGE OF MKPF-MJAHR,
           S_MBLNR TYPE RANGE OF MKPF-MBLNR.

    DATA : BEGIN OF LS_BWART,
             BWART TYPE MSEG-BWART,
           END OF LS_BWART.
    DATA: LT_BWART LIKE HASHED TABLE OF LS_BWART WITH UNIQUE KEY BWART.

    DATA: LR_BWART TYPE RANGE OF MSEG-BWART.

    DATA: LV_ACTIVE TYPE C.

    DATA: LCL_SEND TYPE REF TO ZCL_SDSMM_ENHANCEMENT.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
    LV_CHECK_SDS = LCL_DATA=>CHECK_SDS( SY-MANDT ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*
    IF LV_CHECK_SDS EQ ABAP_TRUE.
      IF GT_SERIAL_HOLD IS NOT INITIAL.
        LOOP AT GT_SERIAL_HOLD INTO LS_SERIAL_HOLD.
          MODIFY ZSDSMMT026 FROM @(
      VALUE #(  DELIVERY = LS_SERIAL_HOLD-DELIVERY
                ITEM     = LS_SERIAL_HOLD-ITEM
                SERIAL   = LS_SERIAL_HOLD-SERIAL
                MBLNR    = IS_MKPF-MBLNR
                MJAHR    = IS_MKPF-MJAHR
                AENAM    = SY-UNAME
                AEDAT    = SY-DATUM
                AEZET    = SY-UZEIT
                ) ).
          COMMIT WORK AND WAIT.
        ENDLOOP.

      ENDIF.

      LT_BWART = CORRESPONDING #( IT_MSEG DISCARDING DUPLICATES ).

      ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID   = 'ZSDSMMR0650'
                                                    I_PARAM   = 'RESERVATION'
                                          CHANGING  CR_RETURN = LR_BWART ).

      ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = 'ZSDSMMR0650'
                                                    I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                    I_PARAM             = 'ACTIVE'
                                          CHANGING  C_RETURN            = LV_ACTIVE ).

      IF LV_ACTIVE EQ ABAP_TRUE.
        IF LR_BWART[] IS NOT INITIAL.
          LOOP AT LT_BWART INTO LS_BWART.
            IF LS_BWART NOT IN LR_BWART[].
              RETURN.
            ENDIF.
          ENDLOOP.

          IF LCL_SEND IS NOT BOUND.
            CREATE OBJECT LCL_SEND.
          ENDIF.

          LCL_SEND->SEND_DATA_MATDOC( IT_GOITEM   = IT_MSEG
                                      IS_GOHEADER = IS_MKPF ).

*          S_MJAHR =  VALUE #( ( SIGN  = 'I' OPTION = 'EQ' LOW = IS_MKPF-MJAHR ) ).
*          S_MBLNR =  VALUE #( ( SIGN  = 'I' OPTION = 'EQ' LOW = IS_MKPF-MBLNR ) ).
*
*
*          SUBMIT ZSDSMMR0650 USING SELECTION-SCREEN  1000
*                                  WITH S_MJAHR    IN S_MJAHR[]
*                                  WITH S_MBLNR    IN S_MBLNR[]
*                                  WITH P_AUTO     EQ ABAP_TRUE
*                                  AND RETURN.
        ENDIF.
      ENDIF.

      SELECT COUNT( * )
        FROM ZSDSCAC009
        WHERE PROCESS EQ 'SONY'
          AND STATU   EQ ABAP_TRUE.
      IF SY-SUBRC EQ 0.
        DATA(LV_RETURN) =  ZCL_SDSMM_SEND_API=>SEND_MATDOC_TO_SONY( IT_MSEG = IT_MSEG
                                                                    I_MKPF  = IS_MKPF ).
      ENDIF.
    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*

  ENDMETHOD.


  method IF_EX_MB_MIGO_BADI~PROPOSE_SERIALNUMBERS.
    DATA : LV_CHECK_SDS LIKE ABAP_TRUE.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
*    LV_CHECK_SDS = LCL_DATA=>CHECK_SDS( SY-MANDT ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*
    IF LV_CHECK_SDS EQ ABAP_TRUE.

    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*
  endmethod.


  method IF_EX_MB_MIGO_BADI~PUBLISH_MATERIAL_ITEM.
    DATA : LV_CHECK_SDS LIKE ABAP_TRUE.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
*    LV_CHECK_SDS = LCL_DATA=>CHECK_SDS( SY-MANDT ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*
    IF LV_CHECK_SDS EQ ABAP_TRUE.

    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*
  endmethod.


  METHOD IF_EX_MB_MIGO_BADI~RESET.
    DATA : LV_CHECK_SDS LIKE ABAP_TRUE.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
    LV_CHECK_SDS = LCL_DATA=>CHECK_SDS( SY-MANDT ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*
    IF LV_CHECK_SDS EQ ABAP_TRUE.
      CLEAR : GT_GOITEM.
    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*
  ENDMETHOD.


  method IF_EX_MB_MIGO_BADI~STATUS_AND_HEADER.
    DATA : LV_CHECK_SDS LIKE ABAP_TRUE.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
*    LV_CHECK_SDS = LCL_DATA=>CHECK_SDS( SY-MANDT ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*
    IF LV_CHECK_SDS EQ ABAP_TRUE.

    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*
  endmethod.
ENDCLASS.
