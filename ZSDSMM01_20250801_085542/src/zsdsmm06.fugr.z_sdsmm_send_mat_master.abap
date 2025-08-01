FUNCTION Z_SDSMM_SEND_MAT_MASTER.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IR_DATE) TYPE  TAB_ISUR_DATUM OPTIONAL
*"     VALUE(IR_MATNR) TYPE  TAB_ISUR_RMATNR OPTIONAL
*"     VALUE(I_AL11) TYPE  CHAR255 OPTIONAL
*"     VALUE(IR_LGORT) TYPE  ZSDSMMS043_TT OPTIONAL
*"  EXPORTING
*"     VALUE(ET_DATA_FILE) TYPE  EREC_T_STRING
*"     VALUE(E_SEPARATOR) TYPE  CHAR10
*"     VALUE(E_FILE_NAME) TYPE  STRING
*"     VALUE(E_FILE_PATH) TYPE  STRING
*"     VALUE(E_STATUS) TYPE  FLAG
*"----------------------------------------------------------------------

  DATA: LV_FIELD(80),
        LV_FIELD_OUT(80),
        LV_DEC           TYPE P DECIMALS 0,
        L_TABLEDESCR_REF TYPE REF TO CL_ABAP_TABLEDESCR,
        L_DESCR_REF      TYPE REF TO CL_ABAP_STRUCTDESCR,
        IDETAILS         TYPE ABAP_COMPDESCR_TAB,
        XDETAILS         TYPE ABAP_COMPDESCR.
  DATA: LV_YEAR(4)  TYPE C,
        LV_MONTH(2) TYPE C,
        LV_DAY(2)   TYPE C.

  DATA: LV_GEN_C_CONST TYPE ZSDSCAC002-CONST,
        LV_GEN_C_VALUE TYPE ZSDSCAC002-VALUE.

  FIELD-SYMBOLS: <FS>,<FS_OUT>.

  REFRESH: GT_MVKE,GT_MARA,GT_MAKT,GT_CABN,
           GT_AUSP,GT_OUT,IDETAILS.

  REFRESH: GR_WERKS,GR_ATNAM.

  DATA : BEGIN OF LS_DATA.
           INCLUDE TYPE ZSDSMMS012.
DATA  END OF LS_DATA.
  DATA LT_DATA LIKE TABLE OF LS_DATA.

  DATA : BEGIN OF LS_MAT,
           MATNR TYPE MARA-MATNR,
         END OF LS_MAT.
  DATA LT_MAT LIKE SORTED TABLE OF LS_MAT WITH UNIQUE KEY MATNR.

  DATA : BEGIN OF LS_HEADER,
           FIELD1  TYPE STRING,
           FIELD2  TYPE STRING,
           FIELD3  TYPE STRING,
           FIELD4  TYPE STRING,
           FIELD5  TYPE STRING,
           FIELD6  TYPE STRING,
           FIELD7  TYPE STRING,
           FIELD8  TYPE STRING,
           FIELD11 TYPE STRING,
           FIELD12 TYPE STRING,
           FIELD13 TYPE STRING,
           FIELD14 TYPE STRING,
           FIELD15 TYPE STRING,
           FIELD16 TYPE STRING,
           FIELD17 TYPE STRING,
           FIELD18 TYPE STRING,
           FIELD19 TYPE STRING,
           FIELD20 TYPE STRING,
           FIELD21 TYPE STRING,
           FIELD22 TYPE STRING,
           FIELD23 TYPE STRING,
           FIELD24 TYPE STRING,
           FIELD25 TYPE STRING,
           FIELD26 TYPE STRING,
           FIELD27 TYPE STRING,
           FIELD28 TYPE STRING,
           FIELD29 TYPE STRING,
           FIELD30 TYPE STRING,
           FIELD31 TYPE STRING,
           FIELD32 TYPE STRING,
           FIELD33 TYPE STRING,
           FIELD34 TYPE STRING,
           FIELD35 TYPE STRING,
           FIELD36 TYPE STRING,
           FIELD37 TYPE STRING,
           FIELD38 TYPE STRING,
           FIELD39 TYPE STRING,
           FIELD40 TYPE STRING,
           FIELD41 TYPE STRING,
           FIELD42 TYPE STRING,
           FIELD43 TYPE STRING,
           FIELD44 TYPE STRING,
           FIELD45 TYPE STRING,
           FIELD46 TYPE STRING,
           FIELD47 TYPE STRING,
           FIELD48 TYPE STRING,
           FIELD49 TYPE STRING,
           FIELD50 TYPE STRING,
           FIELD51 TYPE STRING,
           FIELD52 TYPE STRING,
           FIELD53 TYPE STRING,
           FIELD54 TYPE STRING,
           FIELD55 TYPE STRING,
           FIELD56 TYPE STRING,
           FIELD57 TYPE STRING,
           FIELD58 TYPE STRING,
           FIELD59 TYPE STRING,
           FIELD60 TYPE STRING,
         END OF LS_HEADER.
  DATA LT_HEADER LIKE TABLE OF LS_HEADER.

  CONSTANTS : BEGIN OF LC_CON,
                REPID     TYPE STRING VALUE 'Z_SDSMM_SEND_MAT_MASTER',
                SEPARATOR TYPE STRING VALUE 'SEPARATOR',
                FILE_NAME TYPE STRING VALUE 'FILE_NAME',
                FILE_PATH TYPE STRING VALUE 'FILE_PATH',
                FIELD1    TYPE STRING VALUE 'Product_UID__c',
                FIELD2    TYPE STRING VALUE 'Company__c',
                FIELD3    TYPE STRING VALUE 'Branch__c',
                FIELD4    TYPE STRING VALUE 'ProductCode',
                FIELD5    TYPE STRING VALUE 'Sales_Unit__c',
                FIELD6    TYPE STRING VALUE 'Description',
                FIELD7    TYPE STRING VALUE 'Material_Type__c',
                FIELD8    TYPE STRING VALUE 'Product_Group__c',
                FIELD11   TYPE STRING VALUE 'Product_Group_LV1__c',
                FIELD12   TYPE STRING VALUE 'Product_Group_LV2__c',
                FIELD13   TYPE STRING VALUE 'Product_Group_LV3__c',
                FIELD14   TYPE STRING VALUE 'Manufacturer__c',
                FIELD15   TYPE STRING VALUE 'Item_Category_Group__c',
                FIELD16   TYPE STRING VALUE 'System__c',
                FIELD17   TYPE STRING VALUE 'Type1__c',
                FIELD18   TYPE STRING VALUE 'Refrigerant__c',
                FIELD19   TYPE STRING VALUE 'Power_Supply_Phase__c',
                FIELD20   TYPE STRING VALUE 'Power_Supply_Hertz__c',
                FIELD21   TYPE STRING VALUE 'Electricity_Volt__c',
                FIELD22   TYPE STRING VALUE 'Capacity_Btu_Hr__c',
                FIELD23   TYPE STRING VALUE 'Capacity_Kcal_h__c',
                FIELD24   TYPE STRING VALUE 'KW__c',
                FIELD25   TYPE STRING VALUE 'Usrt_For_Chiller__c',
                FIELD26   TYPE STRING VALUE 'Air_Flow_cmh__c',
                FIELD27   TYPE STRING VALUE 'Multi__c',
                FIELD28   TYPE STRING VALUE 'Flag_for_Ukeharai__c',
                FIELD29   TYPE STRING VALUE 'SCM_Flag__c',
                FIELD30   TYPE STRING VALUE 'SPIC__c',
                FIELD31   TYPE STRING VALUE 'Static__c',
                FIELD32   TYPE STRING VALUE 'Is_Product__c',
                FIELD33   TYPE STRING VALUE 'Sales_Active__c',
                FIELD34   TYPE STRING VALUE 'Mark_Delete__c',
                FIELD35   TYPE STRING VALUE 'Moving_Average_Cost__c',
                FIELD36   TYPE STRING VALUE 'ERP_Created_Date__c',
                FIELD37   TYPE STRING VALUE 'SDS_Component_Description__c',
                FIELD38   TYPE STRING VALUE 'SDS_FOC_FLAG__c',
                FIELD39   TYPE STRING VALUE 'SDS_Lab_Office__c',
                FIELD40   TYPE STRING VALUE 'SDS_Lab_Office_Description__c',
                FIELD41   TYPE STRING VALUE 'SDS_BTU_From__c',
                FIELD42   TYPE STRING VALUE 'SDS_BTU_To__c',
                FIELD43   TYPE STRING VALUE 'SDS_BIN1__c',
                FIELD44   TYPE STRING VALUE 'SDS_BIN2__c',
                FIELD45   TYPE STRING VALUE 'SDS_BIN3__c',
                FIELD46   TYPE STRING VALUE 'SDS_BIN4__c',
                FIELD47   TYPE STRING VALUE 'SDS_BIN5__c',
                FIELD48   TYPE STRING VALUE 'SDS_BIN6__c',
                FIELD49   TYPE STRING VALUE 'SDS_BIN7__c',
                FIELD50   TYPE STRING VALUE 'SDS_BIN8__c',
                FIELD51   TYPE STRING VALUE 'SDS_BIN9__c',
                FIELD52   TYPE STRING VALUE 'SDS_BIN10__c',
                FIELD53   TYPE STRING VALUE 'SDS_BIN11__c',
                FIELD54   TYPE STRING VALUE 'SDS_BIN12__c',
                FIELD55   TYPE STRING VALUE 'RecordTypeId',
                FIELD56   TYPE STRING VALUE 'Item_Type__c',
                FIELD57   TYPE STRING VALUE 'Activity__c',
                FIELD58   TYPE STRING VALUE 'Indoor_Outdoor_Type__c',
                FIELD59   TYPE STRING VALUE 'Material_Group__c',
                FIELD60   TYPE STRING VALUE 'Material_Group2__c',
                DBQ       TYPE C      LENGTH 1 VALUE '"',
                5         TYPE C      LENGTH 1 VALUE '5',
                01        TYPE C      LENGTH 2 VALUE '01',
                M         TYPE C      LENGTH 1 VALUE 'M',
              END OF LC_CON.

  SELECT *
    FROM MARA
    INTO TABLE GT_MARA
    WHERE ERSDA IN IR_DATE
      AND MATNR IN IR_MATNR.

  LT_MAT = VALUE #(
             FOR LS_MARA IN GT_MARA INDEX INTO LV_INDEX
             ( MATNR = LS_MARA-MATNR )
             ).

  IF LT_MAT IS NOT INITIAL.
    SELECT *
      FROM MARC
      INNER JOIN @LT_MAT AS A ON MARC~MATNR EQ A~MATNR
      INTO CORRESPONDING FIELDS OF TABLE @GT_MARC.

    SELECT *
      FROM MVKE
      INNER JOIN @LT_MAT AS A ON MVKE~MATNR EQ A~MATNR
      INTO CORRESPONDING FIELDS OF TABLE @GT_MVKE.

    SELECT *
      FROM MBEW
      INNER JOIN @LT_MAT AS A ON MBEW~MATNR EQ A~MATNR
      INTO CORRESPONDING FIELDS OF TABLE @GT_MBEW.

    SELECT *
      FROM MAKT
      INNER JOIN @LT_MAT AS A ON MAKT~MATNR EQ A~MATNR
      WHERE SPRAS EQ @SY-LANGU
      INTO CORRESPONDING FIELDS OF TABLE @GT_MAKT.

    SELECT *
      FROM MARD
      INNER JOIN @LT_MAT AS A ON MARD~MATNR EQ A~MATNR
      WHERE WERKS EQ '1000' " CHECK STORAGE FOR SPARE PART
      AND MARD~LGORT IN @IR_LGORT
      INTO CORRESPONDING FIELDS OF TABLE @GT_MARD..
  ENDIF.

  L_TABLEDESCR_REF ?= CL_ABAP_TYPEDESCR=>DESCRIBE_BY_DATA( GT_OUT ).
  L_DESCR_REF ?= L_TABLEDESCR_REF->GET_TABLE_LINE_TYPE( ).
  IDETAILS[] = L_DESCR_REF->COMPONENTS[].

  SELECT * INTO TABLE GT_CABN
           FROM CABN
           WHERE ATNAM IN GR_ATNAM.

  SELECT *
  FROM MAST
  WHERE STLAN EQ @LC_CON-5 "Sales and Distribution
    AND STLAL EQ @LC_CON-01
  INTO TABLE @GT_MAST.

  IF GT_MAST IS NOT INITIAL.
    SELECT *
    FROM STKO
    FOR ALL ENTRIES IN @GT_MAST
    WHERE STLTY = @LC_CON-M
      AND STLNR = @GT_MAST-STLNR
      AND STLST = @LC_CON-01
    INTO TABLE @GT_STKO.

    SELECT *
    FROM STAS
    FOR ALL ENTRIES IN @GT_STKO
    WHERE STLTY = @LC_CON-M
      AND STLNR = @GT_STKO-STLNR
      AND STLAL = @GT_STKO-STLAL
    INTO TABLE @GT_STAS.

    SELECT *
    FROM STPO
    FOR ALL ENTRIES IN @GT_STAS
    WHERE STLTY = @LC_CON-M "Material BOM
      AND STLNR = @GT_STAS-STLNR
      AND STLKN = @GT_STAS-STLKN
    INTO TABLE @GT_STPO.
  ENDIF.

  SELECT LABOR,
         LBTXT
    FROM T024X
    WHERE SPRAS EQ @SY-LANGU
     INTO TABLE @GT_T024X.

  SELECT *
    FROM ZDSMMC013
    INTO TABLE @GT_ZDSMMC013.

  SELECT *
    FROM ZDSMMC002
    INTO TABLE @GT_ZDSMMC002.

  SELECT *
    FROM ZDSMMC010
    INTO TABLE @GT_ZDSMMC010.

  SELECT *
    FROM ZDSMMC009
    INTO TABLE @GT_ZDSMMC009.

  SELECT *
    FROM ZDSMMC028
    INTO TABLE @GT_ZDSMMC028.

  SELECT *
    FROM ZDSMMC015
    INTO TABLE @GT_ZDSMMC015.

  SELECT *
    FROM ZDSMMC001
    INTO TABLE @GT_ZDSMMC001.



  LOOP AT GT_MARA INTO GS_MARA.
    REFRESH: GT_AUSP.
    CLEAR: GS_MVKE.
    CLEAR: LV_YEAR,LV_MONTH,LV_DAY. "Add by Wantanee 20150616

    READ TABLE GT_MVKE INTO GS_MVKE
                        WITH KEY MATNR = GS_MARA-MATNR.
    IF NOT GT_CABN[] IS INITIAL.
      SELECT * INTO TABLE GT_AUSP
               FROM AUSP
               FOR ALL ENTRIES IN GT_CABN
               WHERE OBJEK = GS_MARA-MATNR
               AND   ATINN = GT_CABN-ATINN.
    ENDIF.



    "Company
    LS_DATA-COMPANY = 'SDS'.
    "Branch
    LS_DATA-BRANCH  = 'BKK'.
    "Product Code
    LS_DATA-MATNR = GS_MARA-MATNR.
    "Sales Unit
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input                = GS_MVKE-VRKME
        LANGUAGE              = 'E'
     IMPORTING
*       LONG_TEXT            =
       OUTPUT               = LS_DATA-VRKME
*       SHORT_TEXT           =
*     EXCEPTIONS
*       UNIT_NOT_FOUND       = 1
*       OTHERS               = 2
              .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

*    LS_DATA-VRKME = GS_MVKE-VRKME.

    IF LS_DATA-VRKME IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input                = GS_MARA-MEINS
       LANGUAGE              = 'E'
     IMPORTING
*       LONG_TEXT            =
       OUTPUT               = LS_DATA-VRKME
*       SHORT_TEXT           =
*     EXCEPTIONS
*       UNIT_NOT_FOUND       = 1
*       OTHERS               = 2
              .

*      LS_DATA-VRKME = GS_MARA-MEINS.
    ENDIF.
    "Product UID
    CONCATENATE LS_DATA-COMPANY
                LS_DATA-BRANCH
                LS_DATA-MATNR
                INTO LS_DATA-PRODUCT_UID
                SEPARATED BY '+'.
    CLEAR: GS_MAKT.
    READ TABLE GT_MAKT INTO GS_MAKT
               WITH KEY MATNR = GS_MARA-MATNR.
    "Product Name
    LS_DATA-MAKTX = GS_MAKT-MAKTX.
    REPLACE ALL OCCURRENCES OF '"' IN
        LS_DATA-MAKTX WITH ' ' .
    "Material Type
    LS_DATA-MTART = GS_MARA-MTART.
    "Product Group
    LS_DATA-MATKL = GS_MARA-MATKL.
    "Product Group LV1, Product Group LV2, Product Group LV3
*    SHIFT GS_mara-prdha RIGHT DELETING TRAILING space.
*    TRANSLATE GS_mara-prdha USING ' @'.
*    SHIFT GS_mara-prdha LEFT DELETING LEADING '@'.
*    SPLIT GS_mara-prdha
*          AT '@@'
*          INTO: LS_DATA-prodgrp_1_c
*                LS_DATA-prodgrp_2_c
*                LS_DATA-prodgrp_3_c.
    LS_DATA-PRODGRP_1_C = GS_MARA-PRDHA+0(5).
    LS_DATA-PRODGRP_2_C = GS_MARA-PRDHA+5(5).
    LS_DATA-PRODGRP_3_C = GS_MARA-PRDHA+10(8).

    "Is Product
    IF GS_MARA-MTART = 'ZFG' OR GS_MARA-MTART = 'ZAC' OR GS_MARA-MTART = 'ZSV'.
      LS_DATA-IS_PRODUCT = 'Y'.
    ELSE.
      LS_DATA-IS_PRODUCT = 'N'.
    ENDIF.
    "Delete
    IF GS_MARA-LVORM = 'X'.
      LS_DATA-DELETE = 'Y'.
      LS_DATA-ACTIVE = 'N'.
    ELSE.
      LS_DATA-DELETE = 'N'.
      LS_DATA-ACTIVE = 'Y'.
    ENDIF.
    "Active waiting Confirm
*    IF NOT GS_MARA-ZEINR IS INITIAL.
*      LS_DATA-ACTIVE = 'N'.
*    ELSE.
*      LS_DATA-ACTIVE = 'Y'.
*    ENDIF.
    "Manufacturer
    LS_DATA-MFRNR = GS_MARA-MFRNR.
    "Add by Wantanee 20150616 T41K919762
    "Create date
    LV_YEAR = GS_MARA-ERSDA+0(4).
    LV_MONTH = GS_MARA-ERSDA+4(2).
    LV_DAY = GS_MARA-ERSDA+6(2).
    CONCATENATE LV_DAY  LV_MONTH  LV_YEAR
    INTO LS_DATA-ZCREATE_DATE SEPARATED BY  '/'.

    "Item category group
    LS_DATA-MTPOS = GS_MVKE-MTPOS.

    "Add costing
    READ TABLE GT_MBEW INTO GS_MBEW WITH KEY MATNR = GS_MARA-MATNR.
    IF SY-SUBRC EQ 0.
      LS_DATA-VERPR = GS_MBEW-VERPR.
*      IF GS_mbew-vprsv EQ 'V'.
**          LS_DATA-cost_sp = GS_mbew-verpr.  "Edit by Wantanee 20210122
*           LS_DATA-cost_repair = GS_mbew-verpr.  "Edit by Wantanee 20210122
*
*      ELSE.
*           LS_DATA-cost_repair = GS_mbew-verpr.  "Add by Wantanee 20170703
*      ENDIF.
    ENDIF.
"BREAK-POINT.
    LOOP AT GT_CABN INTO GS_CABN.
      CLEAR: GS_AUSP,LV_DEC.
      IF GS_CABN-ATNAM EQ 'ZKCAL'.
           READ TABLE GT_AUSP INTO GS_AUSP
                      WITH KEY ATINN = GS_CABN-ATINN.
           LS_DATA-ZKCAL = GS_AUSP-ATWRT.
      ENDIF.

      IF GS_CABN-ATNAM EQ 'ZKW'.
           READ TABLE GT_AUSP INTO GS_AUSP
                      WITH KEY ATINN = GS_CABN-ATINN.
           LS_DATA-ZKW = GS_AUSP-ATWRT.
      ENDIF.

      IF GS_CABN-ATNAM EQ 'ZUSRT'.
           READ TABLE GT_AUSP INTO GS_AUSP
                      WITH KEY ATINN = GS_CABN-ATINN.
           LS_DATA-ZUSRT = GS_AUSP-ATWRT.
      ENDIF.

      IF GS_CABN-ATNAM EQ 'ZAIRFLOW'.
           READ TABLE GT_AUSP INTO GS_AUSP
                      WITH KEY ATINN = GS_CABN-ATINN.
           LS_DATA-ZAIRFLOW = GS_AUSP-ATWRT.
      ENDIF.

      IF GS_CABN-ATNAM EQ 'ZUKEHARAI'.
           READ TABLE GT_AUSP INTO GS_AUSP
                      WITH KEY ATINN = GS_CABN-ATINN.
           LS_DATA-ZUKEHARAI = GS_AUSP-ATWRT.
      ENDIF.

      IF GS_CABN-ATNAM EQ 'ZSCM'.
           READ TABLE GT_AUSP INTO GS_AUSP
                      WITH KEY ATINN = GS_CABN-ATINN.
           LS_DATA-ZSCM = GS_AUSP-ATWRT.
      ENDIF.

      IF GS_CABN-ATNAM EQ 'ZSPIC'.
           READ TABLE GT_AUSP INTO GS_AUSP
                      WITH KEY ATINN = GS_CABN-ATINN.
           LS_DATA-ZSPIC = GS_AUSP-ATWRT.
      ENDIF.

      IF GS_CABN-ATNAM EQ 'ZSTATIC'.
           READ TABLE GT_AUSP INTO GS_AUSP
                      WITH KEY ATINN = GS_CABN-ATINN.
           LS_DATA-ZSTATIC = GS_AUSP-ATWRT.
      ENDIF.

      IF GS_CABN-ATNAM EQ 'ZSDS_BTUFROM'.
           READ TABLE GT_AUSP INTO GS_AUSP
                      WITH KEY ATINN = GS_CABN-ATINN.
           LS_DATA-BTU_MA_FROM = GS_AUSP-ATWRT.
      ENDIF.
      IF GS_CABN-ATNAM EQ 'ZSDS_BTUTO'.
           READ TABLE GT_AUSP INTO GS_AUSP
                      WITH KEY ATINN = GS_CABN-ATINN.
           LS_DATA-BTU_MA_TO = GS_AUSP-ATWRT.
      ENDIF.





*      CONCATENATE 'LS_DATA-' GS_CABN-ATNAM
*                  INTO LV_FIELD.
*      ASSIGN (LV_FIELD) TO <FS>.
*
*      IF GS_CABN-ATFOR = 'CHAR'.
*        <FS> = GS_AUSP-ATWRT.
*      ELSE.
*        <FS> = LV_DEC = 0 + GS_AUSP-ATFLV.
*      ENDIF.

    ENDLOOP.

    "REFRIGERATION
       LS_DATA-ZREFRIGERATION = GS_MARA-ZZREFT.
    "BTU
       LS_DATA-ZBTU = GS_MARA-ZZCAV.
    "SYSTEM
       READ TABLE GT_ZDSMMC013 INTO GS_ZDSMMC013 WITH KEY ZZCOHP =  GS_MARA-ZZCOHP.
            IF SY-SUBRC EQ 0.
               LS_DATA-ZSYSTEM = GS_ZDSMMC013-DESCRIPTION.
            ENDIF.
    "TYPE
       READ TABLE GT_ZDSMMC002 INTO GS_ZDSMMC002 WITH KEY INV_NONINV =  GS_MARA-ZZINNI.
            IF SY-SUBRC EQ 0.
               LS_DATA-ZTYPE1 = GS_ZDSMMC002-DESCRIPTION.
            ENDIF.
    "POWER PHASE
*       READ TABLE GT_ZDSMMC010 INTO GS_ZDSMMC010 WITH KEY ZZPHA =  GS_MARA-ZZPHA.
*            IF SY-SUBRC EQ 0.
*               LS_DATA-ZPOWER_PHASE = GS_ZDSMMC010-DESCRIPTION.
*            ENDIF.
            LS_DATA-ZPOWER_PHASE = GS_MARA-ZZPHA.
    "PPOWER HERTZ
       READ TABLE GT_ZDSMMC009 INTO GS_ZDSMMC009 WITH KEY ZZFRE =  GS_MARA-ZZFRE.
            IF SY-SUBRC EQ 0.
               LS_DATA-ZPOWER_HERTZ = GS_ZDSMMC009-DESCRIPTION.
            ENDIF.
    "ELECTRICITY
       READ TABLE GT_ZDSMMC028 INTO GS_ZDSMMC028 WITH KEY ZZVOL =  GS_MARA-ZZVOL.
            IF SY-SUBRC EQ 0.
               LS_DATA-ZELECTRICITY = GS_ZDSMMC028-DESCRIPTION.
            ENDIF.
    "MULTI
       READ TABLE GT_ZDSMMC015 INTO GS_ZDSMMC015 WITH KEY ZZPMT =  GS_MARA-ZZPMT.
            IF SY-SUBRC EQ 0.
               LS_DATA-ZMULTI = GS_ZDSMMC015-DESCRIPTION.
            ENDIF.

    "Item type for project WBS and "Activity for project WBS

            IF GS_MARA-SPART EQ 10.
               IF GS_MARA-PRDHA+0(5) EQ 'EQ   '.
                  LS_DATA-ITEM_TYPE__C = '01'.
                  LS_DATA-ACTIVITY__C = '01-02'.
               ELSE.
                  LS_DATA-ITEM_TYPE__C = '01'.
                  LS_DATA-ACTIVITY__C = '01-01'.
               ENDIF.

            ELSEIF  GS_MARA-SPART EQ 20.
               LS_DATA-ITEM_TYPE__C = '02'.
               LS_DATA-ACTIVITY__C = '02-01'.
            ELSEIF  GS_MARA-SPART EQ 30.

               IF GS_MARA-MTART EQ 'ZSV'.

                  IF GS_MARA-PRDHA+5(5) EQ 'COM  '.
                     LS_DATA-ITEM_TYPE__C = '04'.
                     LS_DATA-ACTIVITY__C = '04-03'.
                  ELSEIF GS_MARA-PRDHA+5(5) EQ 'MO   '.
                     LS_DATA-ITEM_TYPE__C = '04'.
                     LS_DATA-ACTIVITY__C = '04-04'.
                  ELSEIF GS_MARA-PRDHA+0(5) EQ 'INS  '.
                     LS_DATA-ITEM_TYPE__C = '04'.
                     LS_DATA-ACTIVITY__C = '04-01'.
                  ELSEIF GS_MARA-PRDHA+0(5) EQ 'COM  '.
                     LS_DATA-ITEM_TYPE__C = '04'.
                     LS_DATA-ACTIVITY__C = '04-03'.
                  ELSEIF GS_MARA-PRDHA+0(5) EQ 'MA  '.
                     LS_DATA-ITEM_TYPE__C = '04'.
                     LS_DATA-ACTIVITY__C = '04-05'.
                  ELSEIF GS_MARA-PRDHA+0(5) EQ 'SB  '.
                     LS_DATA-ITEM_TYPE__C = '04'.
                     LS_DATA-ACTIVITY__C = '04-08'.
                  ELSEIF GS_MARA-PRDHA+0(5) EQ 'SVEXT'.
                     LS_DATA-ITEM_TYPE__C = '04'.
                     LS_DATA-ACTIVITY__C = '04-07'.
                  ELSEIF GS_MARA-PRDHA+0(5) EQ 'MO   '.
                     LS_DATA-ITEM_TYPE__C = '04'.
                     LS_DATA-ACTIVITY__C = '04-04'.
                  ELSEIF GS_MARA-PRDHA+0(5) EQ 'OVH  '.
                     LS_DATA-ITEM_TYPE__C = '04'.
                     LS_DATA-ACTIVITY__C = '04-10'.

                  ENDIF.
               ENDIF.
            ENDIF.

    "Indoor Outdoor Type
*      READ TABLE GT_ZDSMMC001 INTO GS_ZDSMMC001 WITH KEY ZZIOD =  GS_MARA-ZZIOD.
*            IF SY-SUBRC EQ 0.
*               LS_DATA-INDOOR_OUTDOOR_TYPE__C = GS_ZDSMMC001-DESCRIPTION.
*            ENDIF.
       LS_DATA-INDOOR_OUTDOOR_TYPE__C = GS_MARA-ZZIUT.
    "Material Group
      LS_DATA-MATERIAL_GROUP__C = GS_MARA-MATKL.

    "Material Group2
     LS_DATA-MATERIAL_GROUP2__C = GS_MVKE-MVGR2.

    CLEAR: GS_MAST,GS_STKO,GS_STAS,GS_STPO.

    READ TABLE GT_MAST INTO GS_MAST WITH KEY MATNR = GS_MARA-MATNR.
    IF SY-SUBRC EQ 0.

      LOOP AT GT_STPO INTO GS_STPO WHERE STLNR = GS_MAST-STLNR.
        CONCATENATE LS_DATA-IDNRK_DESC GS_STPO-IDNRK
          INTO LS_DATA-IDNRK_DESC
          SEPARATED BY '+'.
      ENDLOOP.

    ENDIF.
    CLEAR:LV_GEN_C_VALUE,LV_GEN_C_CONST.

    LV_GEN_C_VALUE = GS_MARA-MATNR.

    SELECT SINGLE CONST
    INTO LV_GEN_C_CONST
    FROM ZSDSCAC002
    WHERE REPID = 'MV45AFZZ'
    AND  CONST = LV_GEN_C_VALUE.

    IF LV_GEN_C_CONST IS NOT INITIAL.
      LS_DATA-ZFOC_FLAG = 'Y'.
    ELSE.
      LS_DATA-ZFOC_FLAG = 'N'.
    ENDIF..
    LS_DATA-LABOF = GS_MARA-LABOR.


    READ TABLE GT_T024X INTO GS_T024X WITH KEY LABOR =  LS_DATA-LABOF.
    IF SY-SUBRC EQ 0.
      CONCATENATE LS_DATA-LABOF '_' GS_T024X-LBTXT INTO LS_DATA-LABOF_DESC.
    ENDIF.

    CLEAR: GS_MARD.
    LOOP AT GT_MARD INTO GS_MARD WHERE MATNR = GS_MARA-MATNR.
      CASE GS_MARD-LGORT.
        WHEN 'B100'.
          LS_DATA-BIN2100 = GS_MARD-LGPBE.
*                 MODIFY: gt_mara FROM GS_mara INDEX sy-tabix.
        WHEN 'C100'.
          LS_DATA-BIN2200 = GS_MARD-LGPBE.
*                 MODIFY: gt_mara FROM GS_mara INDEX sy-tabix.
        WHEN 'D100'.
          LS_DATA-BIN2300 = GS_MARD-LGPBE.
*                 MODIFY: gt_mara FROM GS_mara INDEX sy-tabix.
        WHEN 'F100'.
          LS_DATA-BIN2400 = GS_MARD-LGPBE.
*                 MODIFY: gt_mara FROM GS_mara INDEX sy-tabix.
        WHEN 'E100'.
          LS_DATA-BIN2500 = GS_MARD-LGPBE.
*                 MODIFY: gt_mara FROM GS_mara INDEX sy-tabix.
        WHEN 'J100'.
          LS_DATA-BIN2600 = GS_MARD-LGPBE.
*                 MODIFY: gt_mara FROM GS_mara INDEX sy-tabix.
        WHEN 'G100'.
          LS_DATA-BIN2700 = GS_MARD-LGPBE.
*                 MODIFY: gt_mara FROM GS_mara INDEX sy-tabix.
        WHEN 'C188'.
          LS_DATA-BIN2288 = GS_MARD-LGPBE.
*                 MODIFY: gt_mara FROM GS_mara INDEX sy-tabix.
        WHEN 'G188'.
          LS_DATA-BIN2788 = GS_MARD-LGPBE.
*                 MODIFY: gt_mara FROM GS_mara INDEX sy-tabix.
        WHEN 'H100'.  "Add by Wantanee 20151021
          LS_DATA-BIN3100 = GS_MARD-LGPBE.
*                 MODIFY: gt_mara FROM GS_mara INDEX sy-tabix.
        WHEN 'L100'.
          LS_DATA-BIN3200 = GS_MARD-LGPBE.
*                 MODIFY: gt_mara FROM GS_mara INDEX sy-tabix.
        WHEN 'I100'.
          LS_DATA-BIN3300 = GS_MARD-LGPBE.
*                 MODIFY: gt_mara FROM GS_mara INDEX sy-tabix.

      ENDCASE.

    ENDLOOP.

    IF SY-SYSID EQ 'T41' .
      IF LS_DATA-VRKME EQ 'SET'.
        LS_DATA-RECORD_TYPD_ID = '012O0000000Z61iIAC'.
      ELSEIF LS_DATA-VRKME EQ 'UNT'.
        LS_DATA-RECORD_TYPD_ID = '012O0000000Z61iIAC'.
      ELSEIF LS_DATA-VRKME EQ 'LOT'.
        LS_DATA-RECORD_TYPD_ID = '012O0000000Z61sIAC'.
      ELSEIF LS_DATA-VRKME EQ 'PC'.
        LS_DATA-RECORD_TYPD_ID = '012O0000000Z61sIAC'.
      ELSEIF LS_DATA-VRKME EQ 'JOB'.
        LS_DATA-RECORD_TYPD_ID = '012O0000000Z61sIAC'.
      ELSEIF LS_DATA-VRKME EQ 'TST'.
        LS_DATA-RECORD_TYPD_ID = '012O0000000Z61sIAC'.
      ELSEIF LS_DATA-VRKME EQ 'L'.
        LS_DATA-RECORD_TYPD_ID = '012O0000000Z61sIAC'.
      ENDIF.
    ELSEIF  SY-SYSID EQ 'T44'.

      "012O0000000Z61iIAC => 0121s000000D133AAC
      "012O0000000Z61sIAC => 0121s000000D132AAC
      IF LS_DATA-VRKME EQ 'SET'.
        LS_DATA-RECORD_TYPD_ID = '0121s000000D133AAC'.
      ELSEIF LS_DATA-VRKME EQ 'UNT'.
        LS_DATA-RECORD_TYPD_ID = '0121s000000D133AAC'.
      ELSEIF LS_DATA-VRKME EQ 'LOT'.
        LS_DATA-RECORD_TYPD_ID = '0121s000000D132AAC'.
      ELSEIF LS_DATA-VRKME EQ 'PC'.
        LS_DATA-RECORD_TYPD_ID = '0121s000000D132AAC'.
      ELSEIF LS_DATA-VRKME EQ 'JOB'.
        LS_DATA-RECORD_TYPD_ID = '0121s000000D132AAC'.
      ELSEIF LS_DATA-VRKME EQ 'TST'.
        LS_DATA-RECORD_TYPD_ID = '0121s000000D132AAC'.
      ELSEIF LS_DATA-VRKME EQ 'L'.
        LS_DATA-RECORD_TYPD_ID = '0121s000000D132AAC'.
      ENDIF.


    ELSEIF SY-SYSID EQ 'T51'.
      IF LS_DATA-VRKME EQ 'SET'.
        LS_DATA-RECORD_TYPD_ID = '0122s000000Gqp7AAC'.
      ELSEIF LS_DATA-VRKME EQ 'UNT'.
        LS_DATA-RECORD_TYPD_ID = '0122s000000Gqp7AAC'.
      ELSEIF LS_DATA-VRKME EQ 'LOT'.
        LS_DATA-RECORD_TYPD_ID = '0122s000000Gqp6AAC'.
      ELSEIF LS_DATA-VRKME EQ 'PC'.
        LS_DATA-RECORD_TYPD_ID = '0122s000000Gqp6AAC'.
      ELSEIF LS_DATA-VRKME EQ 'JOB'.
        LS_DATA-RECORD_TYPD_ID = '0122s000000Gqp6AAC'.
      ELSEIF LS_DATA-VRKME EQ 'TST'.
        LS_DATA-RECORD_TYPD_ID = '0122s000000Gqp6AAC'.
      ELSEIF LS_DATA-VRKME EQ 'L'.
        LS_DATA-RECORD_TYPD_ID = '0122s000000Gqp6AAC'.
      ENDIF.
    ENDIF.

    APPEND LS_DATA TO LT_DATA.
    CLEAR: LS_DATA.
  ENDLOOP.

*  IF LT_DATA IS NOT INITIAL.

    LS_HEADER-FIELD1  = LC_CON-FIELD1.
    LS_HEADER-FIELD2  = LC_CON-FIELD2.
    LS_HEADER-FIELD3  = LC_CON-FIELD3.
    LS_HEADER-FIELD4  = LC_CON-FIELD4.
    LS_HEADER-FIELD5  = LC_CON-FIELD5.
    LS_HEADER-FIELD6  = LC_CON-FIELD6.
    LS_HEADER-FIELD7  = LC_CON-FIELD7.
    LS_HEADER-FIELD8  = LC_CON-FIELD8.
    LS_HEADER-FIELD11 = LC_CON-FIELD11.
    LS_HEADER-FIELD12 = LC_CON-FIELD12.
    LS_HEADER-FIELD13 = LC_CON-FIELD13.
    LS_HEADER-FIELD14 = LC_CON-FIELD14.
    LS_HEADER-FIELD15 = LC_CON-FIELD15.
    LS_HEADER-FIELD16 = LC_CON-FIELD16.
    LS_HEADER-FIELD17 = LC_CON-FIELD17.
    LS_HEADER-FIELD18 = LC_CON-FIELD18.
    LS_HEADER-FIELD19 = LC_CON-FIELD19.
    LS_HEADER-FIELD20 = LC_CON-FIELD20.
    LS_HEADER-FIELD21 = LC_CON-FIELD21.
    LS_HEADER-FIELD22 = LC_CON-FIELD22.
    LS_HEADER-FIELD23 = LC_CON-FIELD23.
    LS_HEADER-FIELD24 = LC_CON-FIELD24.
    LS_HEADER-FIELD25 = LC_CON-FIELD25.
    LS_HEADER-FIELD26 = LC_CON-FIELD26.
    LS_HEADER-FIELD27 = LC_CON-FIELD27.
    LS_HEADER-FIELD28 = LC_CON-FIELD28.
    LS_HEADER-FIELD29 = LC_CON-FIELD29.
    LS_HEADER-FIELD30 = LC_CON-FIELD30.
    LS_HEADER-FIELD31 = LC_CON-FIELD31.
    LS_HEADER-FIELD32 = LC_CON-FIELD32.
    LS_HEADER-FIELD33 = LC_CON-FIELD33.
    LS_HEADER-FIELD34 = LC_CON-FIELD34.
    LS_HEADER-FIELD35 = LC_CON-FIELD35.
    LS_HEADER-FIELD36 = LC_CON-FIELD36.
    LS_HEADER-FIELD37 = LC_CON-FIELD37.
    LS_HEADER-FIELD38 = LC_CON-FIELD38.
    LS_HEADER-FIELD39 = LC_CON-FIELD39.
    LS_HEADER-FIELD40 = LC_CON-FIELD40.
    LS_HEADER-FIELD41 = LC_CON-FIELD41.
    LS_HEADER-FIELD42 = LC_CON-FIELD42.
    LS_HEADER-FIELD43 = LC_CON-FIELD43.
    LS_HEADER-FIELD44 = LC_CON-FIELD44.
    LS_HEADER-FIELD45 = LC_CON-FIELD45.
    LS_HEADER-FIELD46 = LC_CON-FIELD46.
    LS_HEADER-FIELD47 = LC_CON-FIELD47.
    LS_HEADER-FIELD48 = LC_CON-FIELD48.
    LS_HEADER-FIELD49 = LC_CON-FIELD49.
    LS_HEADER-FIELD50 = LC_CON-FIELD50.
    LS_HEADER-FIELD51 = LC_CON-FIELD51.
    LS_HEADER-FIELD52 = LC_CON-FIELD52.
    LS_HEADER-FIELD53 = LC_CON-FIELD53.
    LS_HEADER-FIELD54 = LC_CON-FIELD54.
    LS_HEADER-FIELD55 = LC_CON-FIELD55.
    LS_HEADER-FIELD56 = LC_CON-FIELD56.
    LS_HEADER-FIELD57 = LC_CON-FIELD57.
    LS_HEADER-FIELD58 = LC_CON-FIELD58.
    LS_HEADER-FIELD59 = LC_CON-FIELD59.
    LS_HEADER-FIELD60 = LC_CON-FIELD60.
    APPEND LS_HEADER TO LT_HEADER.
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
*--------------------------------------------------------------------*
* TMP DELETE AFTER DATA SPIDER FINISHED
*--------------------------------------------------------------------*
    DATA : LCL_FTP TYPE REF TO ZCL_SDSCA_GEN_DATA_FILE.

    DATA : LV_STATUS TYPE C.

    DATA : LV_FILE TYPE EVE_TT_STRING.

    DATA : LT_FILE TYPE EREC_T_STRING,
           LS_FILE LIKE LINE OF LT_FILE.
    DATA: LV_PATH(100) TYPE C.

    IF LCL_FTP IS NOT BOUND.
      CREATE OBJECT LCL_FTP.
    ENDIF.

*    CONCATENATE 'mat' SY-DATUM '.txt' INTO LV_PATH.
    E_FILE_NAME = 'Product.csv'.
    LV_STATUS = LCL_FTP->FTP_FILE_PUT( "I_WINDOW_PATH = 'SALESFORCE/DEV/OUT/PRODUCT'
                                       I_AL11_PATH   = I_AL11"'/tmp'
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
*--------------------------------------------------------------------*
* TMP DELETE AFTER DATA SPIDER FINISHED
*--------------------------------------------------------------------*


*  ENDIF.
ENDFUNCTION.
