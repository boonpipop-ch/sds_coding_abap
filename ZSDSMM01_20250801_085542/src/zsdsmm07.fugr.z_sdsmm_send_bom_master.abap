FUNCTION Z_SDSMM_SEND_BOM_MASTER.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IR_DATE) TYPE  TAB_ISUR_DATUM OPTIONAL
*"     VALUE(IR_MATNR) TYPE  TAB_ISUR_RMATNR OPTIONAL
*"     VALUE(I_AL11) TYPE  CHAR255 OPTIONAL
*"  EXPORTING
*"     VALUE(ET_DATA_FILE) TYPE  EREC_T_STRING
*"     VALUE(E_SEPARATOR) TYPE  CHAR10
*"     VALUE(E_FILE_NAME) TYPE  STRING
*"     VALUE(E_FILE_PATH) TYPE  STRING
*"     VALUE(E_STATUS) TYPE  FLAG
*"----------------------------------------------------------------------

  DATA: LV_FIELD(80),
        LV_DEC           TYPE P DECIMALS 0,
        L_TABLEDESCR_REF TYPE REF TO CL_ABAP_TABLEDESCR,
        L_DESCR_REF      TYPE REF TO CL_ABAP_STRUCTDESCR,
        IDETAILS_MASTER  TYPE ABAP_COMPDESCR_TAB,
        IDETAILS_DETAIL  TYPE ABAP_COMPDESCR_TAB,
        XDETAILS         TYPE ABAP_COMPDESCR,
        LV_FILENAME      TYPE STRING,
        LWA_MARA         TYPE MARA,
        LT_CABN          TYPE STANDARD TABLE OF CABN,
        LWA_MVKE         TYPE MVKE.
  DATA: LV_YEAR(4)  TYPE C,
        LV_MONTH(2) TYPE C,
        LV_DAY(2)   TYPE C.
  DATA: LV_LF.
  DATA: LT_EDIDC TYPE STANDARD TABLE OF EDIDC,
        LT_EDIDD TYPE STANDARD TABLE OF EDIDD.
  DATA: LV_GEN_C_CONST TYPE ZSDSCAC002-CONST,
        LV_GEN_C_VALUE TYPE ZSDSCAC002-VALUE.
  FIELD-SYMBOLS: <FS_BOM_MASTER>,<FS_BOM_DETAIL>,<FS>.

  REFRESH: GT_BOM_DETAIL,
           GT_MARA,GT_MAST,GT_STPO,
           IDETAILS_MASTER,IDETAILS_DETAIL.

  REFRESH: GR_ATNAM.

*  DATA : LS_ZSDSMMS013 TYPE ZSDSMMS013,
*         LT_ZSDSMMS013 TYPE TABLE OF ZSDSMMS013.

  DATA : BEGIN OF LS_HEADER,
           FIELD1 TYPE STRING,
           FIELD2 TYPE STRING,
           FIELD3 TYPE STRING,
           FIELD4 TYPE STRING,
           FIELD5 TYPE STRING,
           FIELD6 TYPE STRING,
           FIELD7 TYPE STRING,
           FIELD8 TYPE STRING,
           FIELD9 TYPE STRING,
         END OF LS_HEADER.
  DATA LT_HEADER LIKE TABLE OF LS_HEADER.

  DATA : BEGIN OF LS_DATA.
           INCLUDE TYPE ZSDSMMS013.
DATA END OF LS_DATA.
  DATA LT_DATA LIKE TABLE OF LS_DATA.

  CONSTANTS : BEGIN OF LC_CON,
                REPID     TYPE STRING VALUE 'Z_SDSMM_SEND_BOM_MASTER',
                SEPARATOR TYPE STRING VALUE 'SEPARATOR',
                FILE_NAME TYPE STRING VALUE 'FILE_NAME',
                FILE_PATH TYPE STRING VALUE 'FILE_PATH',
                FIELD1    TYPE STRING VALUE 'BOM_Detail_UID__c',
                FIELD2    TYPE STRING VALUE 'Company__c',
                FIELD3    TYPE STRING VALUE 'Branch__c',
                FIELD4    TYPE STRING VALUE 'Product_Set_Code_UID__c',
                FIELD5    TYPE STRING VALUE 'Product_Code_UID__c',
                FIELD6    TYPE STRING VALUE 'Product_Name__c',
                FIELD7    TYPE STRING VALUE 'Quantity__c',
                FIELD8    TYPE STRING VALUE 'Unit__c',
                FIELD9    TYPE STRING VALUE 'Node__c',
                DBQ       TYPE C      VALUE '"',
              END OF LC_CON.

  SELECT * INTO TABLE GT_MAST
          FROM MAST
          WHERE STLAN = '5'"Sales and Distribution
            AND STLAL = '01'
            AND MATNR IN IR_MATNR
            AND ANDAT IN IR_DATE.

  IF NOT GT_MAST[] IS INITIAL.
    SELECT * INTO TABLE GT_MARA
             FROM MARA
             FOR ALL ENTRIES IN GT_MAST
             WHERE MATNR = GT_MAST-MATNR
               AND ( MATKL LIKE 'FG%'
                     OR MATKL LIKE 'IN%'
                     OR MATKL LIKE 'MO%' ).

    SELECT * INTO TABLE GT_STKO
             FROM STKO
             FOR ALL ENTRIES IN GT_MAST
             WHERE STLTY = 'M'
               AND STLNR = GT_MAST-STLNR
               AND STLST = '01'.

    SELECT * INTO TABLE GT_STAS
             FROM STAS
             FOR ALL ENTRIES IN GT_STKO
             WHERE STLTY = 'M'
               AND STLNR = GT_STKO-STLNR
               AND STLAL = GT_STKO-STLAL.

*    SELECT * INTO TABLE gt_stpo
*             FROM stpo
*             FOR ALL ENTRIES IN gt_mast
*             WHERE stlty = 'M' "Material BOM
*             AND   stlnr = gt_mast-stlnr.
    SELECT * INTO TABLE GT_STPO
             FROM STPO
             FOR ALL ENTRIES IN GT_STAS
             WHERE STLTY = 'M' "Material BOM
             AND   STLNR = GT_STAS-STLNR
             AND   STLKN = GT_STAS-STLKN.

    SELECT * INTO TABLE GT_MAKT
             FROM MAKT
             FOR ALL ENTRIES IN GT_MAST
             WHERE MATNR = GT_MAST-MATNR
             AND   SPRAS = 'E'.

    IF NOT GT_STPO[] IS INITIAL.
      SELECT * APPENDING TABLE GT_MAKT
           FROM MAKT
           FOR ALL ENTRIES IN GT_STPO
           WHERE MATNR = GT_STPO-IDNRK
           AND   SPRAS = 'E'.
    ENDIF.

    SELECT * INTO TABLE GT_CABN
             FROM CABN
             WHERE ATNAM IN GR_ATNAM.

  ENDIF.

  "Get component GT_BOM_MASTER
*  L_TABLEDESCR_REF ?= CL_ABAP_TYPEDESCR=>DESCRIBE_BY_DATA( GT_BOM_MASTER ).
*  L_DESCR_REF ?= L_TABLEDESCR_REF->GET_TABLE_LINE_TYPE( ).
*  IDETAILS_MASTER[] = L_DESCR_REF->COMPONENTS[].

*  DELETE idetails_master WHERE type_kind <> 'C'.

  "Get component LT_DATA
  L_TABLEDESCR_REF ?= CL_ABAP_TYPEDESCR=>DESCRIBE_BY_DATA( LT_DATA ).
  L_DESCR_REF ?= L_TABLEDESCR_REF->GET_TABLE_LINE_TYPE( ).
  IDETAILS_DETAIL[] = L_DESCR_REF->COMPONENTS[].

*  DELETE idetails_detail WHERE type_kind <> 'C'.
  DATA: LV_TABIX TYPE SY-TABIX.
  LOOP AT GT_MAST INTO GS_MAST.
    CLEAR: GS_MARA.
    CLEAR: LV_YEAR,LV_MONTH,LV_DAY. "Add by Wantanee 20150616
    LT_CABN[] = GT_CABN[].
    READ TABLE GT_MARA INTO GS_MARA
         WITH KEY MATNR = GS_MAST-MATNR.
    CLEAR: LWA_MVKE.
    SELECT SINGLE * INTO LWA_MVKE
           FROM MVKE
           WHERE MATNR = GS_MARA-MATNR.
***********************************************************
***> PRODUCT SET
***********************************************************
*    "Company
*    GS_BOM_MASTER-COMPANY = 'SDS'.
*    "Branch
*    GS_BOM_MASTER-BRANCH  = 'BKK'.
*    "Product Set Code
*    GS_BOM_MASTER-MATNR = GS_MAST-MATNR.

    "Add by Wantanee 20150616 T41K919762
    "Create date
*    LV_YEAR = GS_MAST-ANDAT+0(4).
*    LV_MONTH = GS_MAST-ANDAT+4(2).
*    LV_DAY = GS_MAST-ANDAT+6(2).
*    CONCATENATE LV_DAY  LV_MONTH  LV_YEAR
*    INTO GS_BOM_MASTER-ZCREATE_DATE SEPARATED BY  '/'.


    "Add by Wantanee 20150616 T41K919762
    "Sales Unit
*    GS_BOM_MASTER-VRKME = LWA_MVKE-VRKME.
*    IF GS_BOM_MASTER-VRKME IS INITIAL.
*      GS_BOM_MASTER-VRKME = GS_MARA-MEINS.
*    ENDIF.
    "BOM Master UID
*    CONCATENATE GS_BOM_MASTER-COMPANY
*                GS_BOM_MASTER-BRANCH
*                GS_BOM_MASTER-MATNR
*                INTO GS_BOM_MASTER-BOMMASTER_UID
*                SEPARATED BY '+'.
    "Product Group
*    GS_BOM_MASTER-MATKL = GS_MARA-MATKL.

    "Add by Wantanee 20200721 T41K936145,T41K936153,T41K936157,T41K936165,T41K936217

*    CLEAR:LV_GEN_C_VALUE,LV_GEN_C_CONST.

*    LV_GEN_C_VALUE = GS_BOM_MASTER-MATNR.

*    SELECT SINGLE CONST
*    INTO LV_GEN_C_CONST
*    FROM ZSDS_GEN_C
*    WHERE REPID = 'MV45AFZZ'
*    AND  CONST = LV_GEN_C_VALUE.

*    IF LV_GEN_C_CONST IS NOT INITIAL.
*      GS_BOM_MASTER-ZFOC_FLAG = 'Y'.
*    ELSE.
*      GS_BOM_MASTER-ZFOC_FLAG = 'N'.
*    ENDIF..

    "End Add by Wantanee 20200721 T41K936145,T41K936153,T41K936157,T41K936165,T41K936217

*    CLEAR: GS_MAKT.
*    READ TABLE GT_MAKT INTO GS_MAKT
*               WITH KEY MATNR = GS_MARA-MATNR.
    "Product Name
*    GS_BOM_MASTER-MAKTX = GS_MAKT-MAKTX.

    "BOM No.
*    CALL FUNCTION 'CONVERSION_EXIT_NUMCV_INPUT'
*      EXPORTING
*        INPUT  = GS_MAST-STLNR
*      IMPORTING
*        OUTPUT = GS_BOM_MASTER-STLNR.

    "Material Type
*    GS_BOM_MASTER-MTART = GS_MARA-MTART.

    "Product Group LV1, Product Group LV2, Product Group LV3
*    GS_BOM_MASTER-PRODGRP_1_C = GS_MARA-PRDHA+0(5).
*    GS_BOM_MASTER-PRODGRP_2_C = GS_MARA-PRDHA+5(5).
*    GS_BOM_MASTER-PRODGRP_3_C = GS_MARA-PRDHA+10(8).

    "Delete
*    IF GS_MARA-LVORM = 'X'.
*      GS_BOM_MASTER-DELETE = 'Y'.
*    ELSE.
*      GS_BOM_MASTER-DELETE = 'N'.
*    ENDIF.
    "Active waiting Confirm
*-- Change Logic Check Sales Active
*    IF NOT GS_mara-zeinr IS INITIAL. " Delete By Supoj On 17.09.2014
*    IF NOT GS_MARA-MSTAV IS INITIAL. " Add By Supoj On 17.09.2014
*      GS_BOM_MASTER-ACTIVE = 'N'.
*    ELSE.
*      GS_BOM_MASTER-ACTIVE = 'Y'.
*    ENDIF.
    "Manufacturer
*    GS_BOM_MASTER-MFRNR = GS_MARA-MFRNR.
    "Classification for BOM Set
*    IF NOT LT_CABN[] IS INITIAL.
*      SELECT * INTO TABLE GT_AUSP
*               FROM AUSP
*               FOR ALL ENTRIES IN LT_CABN
*               WHERE OBJEK = GS_MAST-MATNR
*               AND   ATINN = LT_CABN-ATINN.
*      CLEAR GS_CABN.
*      LOOP AT LT_CABN INTO GS_CABN.
*        LV_TABIX = SY-TABIX.
*        CLEAR: GS_AUSP,LV_DEC.
*        READ TABLE GT_AUSP INTO GS_AUSP
*                   WITH KEY ATINN = GS_CABN-ATINN.
*        CONCATENATE 'GS_BOM_MASTER-' GS_CABN-ATNAM
*                    INTO LV_FIELD.
*        ASSIGN (LV_FIELD) TO <FS>.
*
*        IF GS_CABN-ATFOR = 'CHAR'.
*          <FS> = GS_AUSP-ATWRT.
*        ELSE.
*          IF NOT GS_AUSP-ATFLV IS INITIAL.
*            <FS> = LV_DEC = 0 + GS_AUSP-ATFLV.
*          ENDIF.
*        ENDIF.
*        IF NOT <FS> IS INITIAL.
*          DELETE LT_CABN INDEX LV_TABIX.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
    "Component Desc.
*    CLEAR: GS_BOM_MASTER-IDNRK_DESC.
*    REFRESH: GT_AUSP.
*    LOOP AT GT_STPO INTO GS_STPO WHERE STLNR = GS_MAST-STLNR.
**      IF GS_BOM_MASTER-IDNRK_DESC IS INITIAL.
**        GS_BOM_MASTER-IDNRK_DESC = GS_STPO-IDNRK.
**      ELSE.
**        CONCATENATE GS_BOM_MASTER-IDNRK_DESC GS_STPO-IDNRK
**        INTO GS_BOM_MASTER-IDNRK_DESC
**        SEPARATED BY '+'.
**      ENDIF.
*
*      CLEAR: LWA_MARA.
*      SELECT SINGLE * INTO LWA_MARA
*             FROM MARA
*             WHERE MATNR = GS_STPO-IDNRK.
*      IF LWA_MARA-PRDHA+5(5) <> 'ACC'.
*        IF NOT LT_CABN[] IS INITIAL.
*          SELECT * INTO TABLE GT_AUSP
*                   FROM AUSP
*                   FOR ALL ENTRIES IN LT_CABN
*                   WHERE OBJEK = GS_STPO-IDNRK
*                   AND   ATINN = LT_CABN-ATINN.
*          CLEAR GS_CABN.
*          LOOP AT LT_CABN INTO GS_CABN.
*            CLEAR: GS_AUSP,LV_DEC.
*            READ TABLE GT_AUSP INTO GS_AUSP
*                       WITH KEY ATINN = GS_CABN-ATINN.
*            CONCATENATE 'GS_BOM_MASTER-' GS_CABN-ATNAM
*                        INTO LV_FIELD.
*            ASSIGN (LV_FIELD) TO <FS>.
*
*            IF GS_CABN-ATFOR = 'CHAR'.
*              <FS> = GS_AUSP-ATWRT.
*            ELSE.
*              IF NOT GS_AUSP-ATFLV IS INITIAL.
*                <FS> = LV_DEC = 0 + GS_AUSP-ATFLV.
*              ENDIF.
*            ENDIF.
*            IF NOT <FS> IS INITIAL.
*              DELETE LT_CABN INDEX LV_TABIX.
*            ENDIF.
*          ENDLOOP.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.

*    LOOP AT gt_cabn INTO GS_cabn.
*      CLEAR: GS_ausp,lv_dec.
*      READ TABLE gt_ausp INTO GS_ausp
*                 WITH KEY atinn = GS_cabn-atinn.
*      CONCATENATE 'GS_BOM_MASTER-' GS_cabn-atnam
*                  INTO lv_field.
*      ASSIGN (lv_field) TO <fs>.
*
*      IF GS_cabn-atfor = 'CHAR'.
*        <fs> = GS_ausp-atwrt.
*      ELSE.
*        <fs> = lv_dec = 0 + GS_ausp-atflv.
*      ENDIF.
*    ENDLOOP.

*    APPEND GS_BOM_MASTER TO GT_BOM_MASTER.
*    CLEAR: GS_BOM_MASTER.

***********************************************************
***> PRODUCT SET DETAIL
***********************************************************
    LOOP AT GT_STPO INTO GS_STPO
            WHERE STLNR = GS_MAST-STLNR.
      "Company
      LS_DATA-COMPANY = 'SDS'.
      "Branch
      LS_DATA-BRANCH  = 'BKK'.
      "Product Set Code
*      LS_DATA-prodsetcode = GS_MAST-matnr.
      CONCATENATE 'SDS' 'BKK' GS_MAST-MATNR INTO LS_DATA-PRODSETCODE SEPARATED BY '+'.
      "Product Code
*      LS_DATA-idnrk = GS_stpo-idnrk.
      CONCATENATE 'SDS' 'BKK' GS_STPO-IDNRK INTO LS_DATA-IDNRK SEPARATED BY '+'.
      CLEAR: GS_MAKT.
      READ TABLE GT_MAKT INTO GS_MAKT
                 WITH KEY MATNR = GS_STPO-IDNRK.


      REPLACE '"' IN GS_MAKT-MAKTX WITH ''.
      "Product Name
      LS_DATA-MAKTX = GS_MAKT-MAKTX.


      "Quantity
      LS_DATA-MENGE = GS_STPO-MENGE.
      CONDENSE LS_DATA-MENGE NO-GAPS.
      "Unit
      LS_DATA-MEINS = GS_STPO-MEINS.
      "Node
*      LS_DATA-stlkn = GS_stpo-stlkn.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = GS_STPO-STLKN
        IMPORTING
          OUTPUT = LS_DATA-STLKN.

      "BOM Detail UID
      CONCATENATE LS_DATA-COMPANY
                  LS_DATA-BRANCH
*                  LS_DATA-prodsetcode
                  GS_MAST-MATNR
*                  LS_DATA-idnrk
                  GS_STPO-IDNRK
                  LS_DATA-STLKN
                  INTO LS_DATA-BOMDETAIL_UID
                  SEPARATED BY '+'.

      APPEND LS_DATA TO LT_DATA.
      CLEAR: LS_DATA.
    ENDLOOP.
  ENDLOOP.

*  LOOP AT LT_DATA INTO LS_DATA.
*    LS_ZSDSMMS013-BOMDETAIL_UID = LS_DATA.
*    LS_ZSDSMMS013-COMPANY       = LS_DATA.
*    LS_ZSDSMMS013-BRANCH        = LS_DATA.
*    LS_ZSDSMMS013-PRODSETCODE   = LS_DATA.
*    LS_ZSDSMMS013-IDNRK         = LS_DATA.
*    LS_ZSDSMMS013-MAKTX         = LS_DATA.
*    LS_ZSDSMMS013-MENGE         = LS_DATA.
*    LS_ZSDSMMS013-MEINS         = LS_DATA.
*    LS_ZSDSMMS013-STLKN         = LS_DATA.
*    APPEND LS_ZSDSMMS013 TO LT_ZSDSMMS013.
*  ENDLOOP.

  LS_HEADER-FIELD1 = LC_CON-FIELD1.
  LS_HEADER-FIELD2 = LC_CON-FIELD2.
  LS_HEADER-FIELD3 = LC_CON-FIELD3.
  LS_HEADER-FIELD4 = LC_CON-FIELD4.
  LS_HEADER-FIELD5 = LC_CON-FIELD5.
  LS_HEADER-FIELD6 = LC_CON-FIELD6.
  LS_HEADER-FIELD7 = LC_CON-FIELD7.
  LS_HEADER-FIELD8 = LC_CON-FIELD8.
  LS_HEADER-FIELD9 = LC_CON-FIELD9.
  APPEND LS_HEADER TO LT_HEADER.

*  IF LT_DATA IS NOT INITIAL.
    ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_CON-REPID
                                                  I_SINGLE_VALUE_FLAG = ABAP_TRUE
                                                  I_PARAM             = LC_CON-SEPARATOR
                                        CHANGING  C_RETURN            = E_SEPARATOR ).
*
*    ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_CON-REPID
*                                                  I_SINGLE_VALUE_FLAG = ABAP_TRUE
*                                                  I_PARAM             = LC_CON-FILE_NAME
*                                        CHANGING  C_RETURN            = E_FILE_NAME ).
*
*    ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             = LC_CON-REPID
*                                                  I_SINGLE_VALUE_FLAG = ABAP_TRUE
*                                                  I_PARAM             = LC_CON-FILE_PATH
*                                        CHANGING  C_RETURN            = E_FILE_PATH ).

    ET_DATA_FILE = ZCL_SDSCA_GEN_DATA_FILE=>GEN_DATA_FIEL( I_HEADER          = LT_HEADER
                                                           I_ITEM            = LT_DATA
                                                           I_START_END_VALUE = LC_CON-DBQ
                                                           I_SEPARATOR       = E_SEPARATOR ).

*    E_SEPARATOR = CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.
    DATA : LCL_FTP TYPE REF TO ZCL_SDSCA_GEN_DATA_FILE.

    DATA : LV_STATUS TYPE C.

    DATA : LV_FILE TYPE EVE_TT_STRING.

    DATA : LT_FILE TYPE EREC_T_STRING,
           LS_FILE LIKE LINE OF LT_FILE.
    DATA: LV_PATH(100) TYPE C.

    IF LCL_FTP IS NOT BOUND.
      CREATE OBJECT LCL_FTP.
    ENDIF.

    E_FILE_NAME = 'product_set_detail.csv'.
    LV_STATUS = LCL_FTP->FTP_FILE_PUT( I_AL11_PATH   = I_AL11"'/tmp'
                                       I_FILE_NAME   = E_FILE_NAME "'TEST_FTP_FILE.txt'
*                                       I_USER        = 'ds'
*                                       I_PASS        = 'ds=20240521'
*                                       I_IP          = '172.31.136.249'
*                                       I_PORT        = '21'
                                       I_DATA_SPIDER = ABAP_TRUE
                                       IT_DATA       = ET_DATA_FILE ).
    IF LV_STATUS EQ 'S'.
      E_STATUS = 'S'.
    ELSE.
      E_STATUS = 'E'.
    ENDIF.
*  ENDIF.

ENDFUNCTION.
