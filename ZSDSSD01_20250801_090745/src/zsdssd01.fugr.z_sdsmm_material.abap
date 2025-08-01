FUNCTION Z_SDSMM_MATERIAL.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_MESTYP) TYPE  EDIDC-MESTYP OPTIONAL
*"     VALUE(IS_PATHNAME) TYPE  RLGRAP-FILENAME OPTIONAL
*"     VALUE(IS_FILENAME) TYPE  RLGRAP-FILENAME OPTIONAL
*"     VALUE(IS_MATNR) TYPE  RANGE_T_MATNR OPTIONAL
*"     VALUE(IS_MATKL) TYPE  ZMATKL_R OPTIONAL
*"     VALUE(I_AL11) TYPE  CHAR255 OPTIONAL
*"  EXPORTING
*"     VALUE(E_STATUS) TYPE  FLAG
*"  CHANGING
*"     VALUE(CT_RESULT) TYPE  EREC_T_STRING OPTIONAL
*"----------------------------------------------------------------------
  DATA: GV_FIELD(80),
        GV_FIELD_OUT(80),
        GV_DEC             TYPE P DECIMALS 0,
        GCL_TABLEDESCR_REF TYPE REF TO CL_ABAP_TABLEDESCR,
        GCL_DESCR_REF      TYPE REF TO CL_ABAP_STRUCTDESCR,
        GS_IDETAILS        TYPE ABAP_COMPDESCR_TAB,
        GS_XDETAILS        TYPE ABAP_COMPDESCR. "
* >> Work center master data
  DATA: GT_MARA     TYPE STANDARD TABLE OF MARA,
        GS_MARA     LIKE LINE OF GT_MARA,
        GT_MARA_ZFG TYPE STANDARD TABLE OF MARA,
        GT_MARA_OTH TYPE STANDARD TABLE OF MARA,
        GT_MARC     TYPE STANDARD TABLE OF MARC,
        GS_MARC     LIKE LINE OF GT_MARC,
        GT_MARD     TYPE STANDARD TABLE OF MARD,
        GS_MARD     LIKE LINE OF GT_MARD,
        GS_A004     TYPE A004,
        GS_VERPR    TYPE MBEW-VERPR,
        GS_STPRS    TYPE MBEW-STPRS,
        GS_KBETR    TYPE KONV-KBETR.
* <<

  DATA: GS_MARA1 TYPE MARA.
  DATA: GS_MARA1_FG  TYPE MARA,
        GS_MARA1_OTH TYPE MARA.


  FIELD-SYMBOLS: <FS>,<FS_OUT>.

*  CLEAR: gs_idoc_control.
  REFRESH: GT_OUT,GS_IDETAILS.",gt_edidc,gt_edidd.

  "Get component GT_OUT
  GCL_TABLEDESCR_REF ?= CL_ABAP_TYPEDESCR=>DESCRIBE_BY_DATA( GT_OUT ).
  GCL_DESCR_REF ?= GCL_TABLEDESCR_REF->GET_TABLE_LINE_TYPE( ).
  GS_IDETAILS[] = GCL_DESCR_REF->COMPONENTS[].

* >> Get Data
  REFRESH: GT_MARA,GT_MARC,GT_MARD.
  SELECT * INTO TABLE GT_MARA
           FROM MARA
           WHERE MATNR IN IS_MATNR
           AND   MATKL IN IS_MATKL
*           AND   prdha IN i_prdha
           AND MTART IN ('ZSP','ZFG','UNBW') .

  REFRESH: GT_MARA_ZFG,GT_MARA_OTH.

  LOOP AT GT_MARA INTO  GS_MARA1.

    IF GS_MARA1-MTART EQ 'ZFG'.
      MOVE-CORRESPONDING GS_MARA1 TO GS_MARA1_FG.
      APPEND GS_MARA1_FG TO GT_MARA_ZFG.
    ELSEIF GS_MARA1-MTART EQ 'UNBW'.
      IF GS_MARA1-MATKL+0(2) = 'FG'.
        MOVE-CORRESPONDING GS_MARA1 TO GS_MARA1_FG.
        APPEND GS_MARA1_FG TO GT_MARA_ZFG.
      ELSE.
        MOVE-CORRESPONDING GS_MARA1 TO GS_MARA1_OTH.
        APPEND GS_MARA1_OTH TO GT_MARA_OTH.
      ENDIF.
    ELSE.
      MOVE-CORRESPONDING GS_MARA1 TO GS_MARA1_OTH.
      APPEND GS_MARA1_OTH TO GT_MARA_OTH.
    ENDIF.
  ENDLOOP.
  "End Add by Wantanee 20201019 T41K937129
  IF NOT GT_MARA_ZFG[] IS INITIAL.
    SELECT * INTO TABLE GT_MARC
             FROM MARC
             FOR ALL ENTRIES IN GT_MARA_ZFG
             WHERE MATNR = GT_MARA_ZFG-MATNR
             AND   WERKS = '1000'
             AND   LVORM = SPACE.
  ENDIF.

  IF NOT GT_MARA_OTH[] IS INITIAL.
    SELECT * APPENDING TABLE GT_MARC
             FROM MARC
             FOR ALL ENTRIES IN GT_MARA_OTH
             WHERE MATNR = GT_MARA_OTH-MATNR
             AND   WERKS = '1000'
             AND   LVORM = SPACE.
  ENDIF.

  IF NOT GT_MARC[] IS INITIAL.
    SELECT * INTO TABLE GT_MARD
             FROM MARD
             FOR ALL ENTRIES IN GT_MARC
             WHERE MATNR = GT_MARC-MATNR
             AND   WERKS = GT_MARC-WERKS
             AND   LGORT = 'B100'.
  ENDIF.
* <<

  LOOP AT GT_MARC INTO GS_MARC.

    CLEAR GS_MARA.
    READ TABLE GT_MARA INTO GS_MARA
               WITH KEY MATNR = GS_MARC-MATNR.

    CLEAR GS_MARD.
    READ TABLE GT_MARD INTO GS_MARD
               WITH KEY MATNR = GS_MARC-MATNR
                        WERKS = GS_MARC-WERKS.

    MOVE-CORRESPONDING GS_MARA TO GS_OUT .

    GS_OUT-PRDHA = GS_MARA-PRDHA+5(3).

    IF  GS_OUT-MATNR EQ 'EWAD265TZ‐XS'.
      GS_OUT-MATNR = 'EWAD265TZ-XS'.
    ENDIF.

    GS_OUT-LGPBE = GS_MARD-LGPBE.
    GS_OUT-EKGRP = GS_MARC-EKGRP.


    SELECT SINGLE MAKTX INTO GS_OUT-MAKTX
           FROM MAKT
           WHERE MATNR = GS_MARA-MATNR
           AND   SPRAS = 'E'.


      REPLACE ALL OCCURRENCES OF '"' IN GS_OUT-MAKTX WITH ''.
      REPLACE ALL OCCURRENCES OF ',' IN GS_OUT-MAKTX WITH ';'.

    CLEAR: GS_VERPR.

    IF GS_MARA-MTART = 'ZSV' .

      "standard price
      SELECT SINGLE STPRS INTO GS_STPRS
             FROM MBEW
             WHERE MATNR = GS_MARA-MATNR
             AND   BWKEY = '1000'."gs_marc-werks.

      GS_OUT-VERPR = GS_STPRS.
      CONDENSE GS_OUT-VERPR NO-GAPS.
    ELSE.
      "moving price
      SELECT SINGLE VERPR INTO GS_VERPR
             FROM MBEW
             WHERE MATNR = GS_MARA-MATNR
             AND   BWKEY = '1000'."gs_marc-werks.

      GS_OUT-VERPR = GS_VERPR.
      CONDENSE GS_OUT-VERPR NO-GAPS.

    ENDIF.



*    IF GS_MARA-MTART = 'ZSP' OR GS_MARA-MTART = 'ZFG' .
      "sales price
      CLEAR GS_A004.
      SELECT SINGLE * INTO GS_A004
             FROM A004
             WHERE MATNR = GS_MARA-MATNR
             AND   VKORG = '1000'
             AND   VTWEG = '00'
             AND   KAPPL = 'V'
             AND   KSCHL = 'ZPR0'
             AND   DATBI >= SY-DATUM
             AND   DATAB <= SY-DATUM.

      CLEAR: GS_KBETR.
      SELECT SINGLE KBETR INTO GS_KBETR
             FROM KONP
             WHERE KNUMH = GS_A004-KNUMH.


      GS_OUT-KBETR = GS_KBETR.
      CONDENSE GS_OUT-KBETR NO-GAPS.
      IF GS_KBETR IS INITIAL.
         "sales price
            CLEAR GS_A004.
            SELECT SINGLE * INTO GS_A004
                   FROM A004
                   WHERE MATNR = GS_MARA-MATNR
                   AND   VKORG = '1000'
                   AND   VTWEG = '00'
                   AND   KAPPL = 'V'
                   AND   KSCHL = 'ZSR1'
                   AND   DATBI >= SY-DATUM
                   AND   DATAB <= SY-DATUM.

            CLEAR: GS_KBETR.
            SELECT SINGLE KBETR INTO GS_KBETR
                   FROM KONP
                   WHERE KNUMH = GS_A004-KNUMH.


            GS_OUT-KBETR = GS_KBETR.
            CONDENSE GS_OUT-KBETR NO-GAPS.
      ENDIF.

*    ENDIF.

*    gs_edidd-segnam = 'ZSDSSDS001'.
*    gs_edidd-sdata = gs_out.
*
*
*
*
*
    APPEND GS_OUT TO GT_OUT.
    CLEAR: GS_OUT.
*     CLEAR gs_edidd.

  ENDLOOP.

  "Z_DEMO_GEN_FILE
  CT_RESULT = ZCL_SDSCA_GEN_DATA_FILE=>GEN_DATA_FIEL( EXPORTING I_ITEM      = GT_OUT
                                                           I_SEPARATOR = '","'
                                                           I_START_END_VALUE = '"').

*  IF gt_edidd[] IS NOT INITIAL.
*
*    gs_path-pathname = IS_PATHNAME.
*    gs_path-filename = IS_FILENAME.
*    gs_edidd-segnam = 'ZSDSSDS000'.
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
  DATA : LCL_FTP TYPE REF TO ZCL_SDSCA_GEN_DATA_FILE.

  DATA : LV_STATUS TYPE C.

  DATA : LV_FILE TYPE EVE_TT_STRING.

  DATA : LT_FILE TYPE EREC_T_STRING,
         LS_FILE LIKE LINE OF LT_FILE.
  DATA: LV_PATH(100) TYPE C.
  DATA: LV_PATH_FILE TYPE STRING.

*  ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING   I_REPID             = 'Z_SDSMM_MATERIAL' "LC_CON-REPID
*                                                  I_SINGLE_VALUE_FLAG = ABAP_TRUE "--> ต้องการค่าเดียว ใส่ ABAP_TRUE ต้องการหลายค่าให้ Comment
*                                                  I_PARAM             = SY-SYSID "LC_CON-SEPARATOR
**                                                  I_PARAM_EXT      =
*                                                  CHANGING  C_RETURN  = LV_PATH_FILE ).

* IF sy-sysid = 'F36'.
*     LV_PATH_FILE = '/interface/Z_DS_Dev/SDS_Dev/20_OUTBOUND/DSS/ZMMI008'.
*  ELSEIF sy-sysid = 'F46'.
*     LV_PATH_FILE = '/interface/Z_DS/SDS/20_OUTBOUND/DSS/ZMMI008'.
*  ELSE.
**     LV_PATH_FILE_SF = '/interface/Z_DS_Dev/SDS_Dev/20_OUTBOUND/DSS/ZMMI008'.
**     LV_PATH_FILE_SONY_SP = '/interface/Z_DS_Dev/SDS_Dev/20_OUTBOUND/Sony_Logi/ZMMI014'.
*  ENDIF.
   LV_PATH_FILE = I_AL11.
  IF LCL_FTP IS NOT BOUND.
    CREATE OBJECT LCL_FTP.
  ENDIF.

  CONCATENATE 'mat_' SY-DATUM SY-TIMLO '.csv' INTO LV_PATH.
  LV_STATUS = LCL_FTP->FTP_FILE_PUT( "I_WINDOW_PATH = 'DEALER_CLAIM/DEV/OUT'
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
  IF LV_STATUS EQ 'S'.
    E_STATUS = 'S'.
  ELSE.
    E_STATUS = 'E'.
  ENDIF.

ENDFUNCTION.
