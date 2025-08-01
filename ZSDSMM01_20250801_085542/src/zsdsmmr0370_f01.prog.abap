*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0370_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_SHOW_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SHOW_ALV .

  TRY.
      CL_SALV_TABLE=>FACTORY(
        IMPORTING
          R_SALV_TABLE = GR_TABLE
        CHANGING
          T_TABLE      = <FS_TABLE> ).
    CATCH CX_SALV_MSG.                                  "#EC NO_HANDLER
  ENDTRY.


*  gr_table->set_screen_status(
*    pfstatus      =  'STATUS001'
*    report        =  sy-repid
*    set_functions = gr_table->c_functions_all ).

  GR_LAYOUT = GR_TABLE->GET_LAYOUT( ).
  GR_KEY-REPORT = SY-REPID.
  GR_LAYOUT->SET_KEY( GR_KEY ).
  GR_LAYOUT->SET_SAVE_RESTRICTION( IF_SALV_C_LAYOUT=>RESTRICT_NONE ).

  IF S_VARI IS NOT INITIAL.
    G_VARIANT = S_VARI.
  ELSE.
    G_VARIANT =  GV_VARIANT.    "'DEFAULT'.
  ENDIF.
  GR_LAYOUT->SET_INITIAL_LAYOUT( G_VARIANT ).


  GR_FUNCTIONS = GR_TABLE->GET_FUNCTIONS( ).
  GR_FUNCTIONS->SET_ALL( 'X' ).


  GR_DISPLAY = GR_TABLE->GET_DISPLAY_SETTINGS( ).
  GR_DISPLAY->SET_STRIPED_PATTERN('X').
*
*
  GR_SELECTIONS = GR_TABLE->GET_SELECTIONS( ).
**... §7.1 set selection mode
  GR_SELECTIONS->SET_SELECTION_MODE( IF_SALV_C_SELECTION_MODE=>ROW_COLUMN ).
*

  GR_AGGRS = GR_TABLE->GET_AGGREGATIONS( ).

  GR_AGGRS->ADD_AGGREGATION(
    EXPORTING
     COLUMNNAME  = 'VERPR'
     AGGREGATION = IF_SALV_C_AGGREGATION=>TOTAL ).

  GR_EVENTS = GR_TABLE->GET_EVENT( ).
  CREATE OBJECT GR_EVENTS_HANDLE.
*
**... §6.1 register to the event USER_COMMAND
  SET HANDLER GR_EVENTS_HANDLE->ON_USER_COMMAND FOR GR_EVENTS.
**... §6.2 register to the event DOUBLE_CLICK
  SET HANDLER GR_EVENTS_HANDLE->ON_DOUBLE_CLICK FOR GR_EVENTS.
**... §6.3 register to the event LINK_CLICK
*  set handler gr_events_handle->on_link_click for gr_events.
*
*

  GR_TABLE->GET_SORTS( "get sorts
   RECEIVING
     VALUE = GR_SORT ).

*  gr_sort->ADD_SORT(
*   exporting
*      COLUMNNAME = 'MATNR'  "sort column always keyfield
**     POSITION   =
**     SEQUENCE   = IF_SALV_C_SORT=>SORT_UP
**     SUBTOTAL   = IF_SALV_C_BOOL_SAP=>FALSE
**     GROUP      = IF_SALV_C_SORT=>GROUP_NONE
**     OBLIGATORY = IF_SALV_C_BOOL_SAP=>FALSE
*    receiving
*      VALUE      = gr_SORT_COLUMN ).
*
**
*  GR_SORT_COLUMN->SET_SUBTOTAL( "add subtotal
* EXPORTING
*   VALUE = IF_SALV_C_BOOL_SAP=>TRUE ).


  GR_COLUMNS = GR_TABLE->GET_COLUMNS( ).
  GR_COLUMNS->SET_OPTIMIZE( 'X' ).


  GR_COLUMNS = GR_TABLE->GET_COLUMNS( ).
  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'MATNR' ).
  GR_COLUMN->SET_LONG_TEXT( 'Material' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Material' ).
  GR_COLUMN->SET_SHORT_TEXT( 'Material' ).


  DATA: LW_AUSP LIKE LINE OF GT_AUSP,
        LV_FNAME TYPE LVC_FNAME,
        LV_SCRTEXT_L TYPE SCRTEXT_L,
        LV_SCRTEXT_M  TYPE SCRTEXT_M,
        LV_SCRTEXT_S TYPE SCRTEXT_S.

  LOOP AT GT_COL_AUSP INTO LW_AUSP.
    MOVE:  LW_AUSP-ATINN TO LV_FNAME,
           LW_AUSP-ATBEZ TO LV_SCRTEXT_L,
           LV_SCRTEXT_L TO LV_SCRTEXT_S,
           LV_SCRTEXT_L TO LV_SCRTEXT_M.
    GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( LV_FNAME ).
    GR_COLUMN->SET_LONG_TEXT( LV_SCRTEXT_L  ).
    GR_COLUMN->SET_MEDIUM_TEXT( LV_SCRTEXT_M ).
    GR_COLUMN->SET_SHORT_TEXT( LV_SCRTEXT_S  ).
  ENDLOOP.

  GR_COLUMNS = GR_TABLE->GET_COLUMNS( ).
  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'ROTAT' ).
  GR_COLUMN->SET_LONG_TEXT( 'Rotation Code' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Rotation Code' ).
  GR_COLUMN->SET_SHORT_TEXT( 'RotaCode' ).

  GR_COLUMNS = GR_TABLE->GET_COLUMNS( ).
  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'ZEINR' ).
  GR_COLUMN->SET_LONG_TEXT( 'Drawing Number' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Drawing Number' ).
  GR_COLUMN->SET_SHORT_TEXT( 'Drawing' ).

  GR_COLUMNS = GR_TABLE->GET_COLUMNS( ).
  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'MAKTX' ).
  GR_COLUMN->SET_LONG_TEXT( 'Material Description' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Material Description' ).
  GR_COLUMN->SET_SHORT_TEXT( 'MatDescrip' ).


  GR_COLUMNS = GR_TABLE->GET_COLUMNS( ).
  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'PRDHA' ).
  GR_COLUMN->SET_LONG_TEXT( 'Product Hierarchy Basic' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'PrdHrarchyBasic' ).
  GR_COLUMN->SET_SHORT_TEXT( 'PrdHrchBas' ).


  GR_COLUMNS = GR_TABLE->GET_COLUMNS( ).
  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'PRODH' ).
  GR_COLUMN->SET_LONG_TEXT( 'Product Hierarchy Sales Org' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'PrdHrarchySlesOrg' ).
  GR_COLUMN->SET_SHORT_TEXT( 'PrdHrchOrg' ).



  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'VENDOR_NAME' ).
  GR_COLUMN->SET_LONG_TEXT( 'Manufacturer Name' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Manufacturer Name' ).
  GR_COLUMN->SET_SHORT_TEXT( 'MnufacName' ).




  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'INSPEC_STPRS' ).
  GR_COLUMN->SET_LONG_TEXT( 'Standard Price Inspcection' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Std Price Inspcect' ).
  GR_COLUMN->SET_SHORT_TEXT( 'StdPrcInsp' ).

  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'CLEAN_STPRS' ).
  GR_COLUMN->SET_LONG_TEXT( 'Standard Price Cleaning' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Std Price Cleaning' ).
  GR_COLUMN->SET_SHORT_TEXT( 'StdPrcCln' ).

  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'BRGEW' ).
  GR_COLUMN->SET_LONG_TEXT( 'Gross Weight' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Gross Weight' ).
  GR_COLUMN->SET_SHORT_TEXT( 'Gross' ).


  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'NTGEW' ).
  GR_COLUMN->SET_LONG_TEXT( 'Net Weight' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Net Weight' ).
  GR_COLUMN->SET_SHORT_TEXT( 'Net Weight' ).

*  gr_column ?= gr_columns->get_column( 'ROTAT' ).
*  gr_column->set_long_text( 'Rotation Code' ).
*  gr_column->set_medium_text( 'Rotation Code' ).
*  gr_column->set_short_text( 'Rotation Code' ).

*
*      USING 'BRGEW'  'BRGEW' 'MARA' space,
*                       USING 'NTGEW'  'NTGEW' 'MARA' space,

"Add by Wantanee 20180302

  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'MSTAE' ).
  GR_COLUMN->SET_LONG_TEXT( 'X-plant matl status Block' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'matl status Block' ).
  GR_COLUMN->SET_SHORT_TEXT( 'matl Block' ).

  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'MSTAV' ).
  GR_COLUMN->SET_LONG_TEXT( 'X-distr.chain status Block' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Mat status Block' ).
  GR_COLUMN->SET_SHORT_TEXT( 'Mat Block' ).

  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'MTVFP' ).
  GR_COLUMN->SET_LONG_TEXT( 'Availability check' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Avail. check' ).
  GR_COLUMN->SET_SHORT_TEXT( 'AvailCheck' ).

"--> Add by wantanee 20250715
  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'ZZCAV' ).
  GR_COLUMN->SET_LONG_TEXT( 'Capacity（Value）' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Capacity（Value）' ).
  GR_COLUMN->SET_SHORT_TEXT( 'Capa(Val)' ).

  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'ZZCAU' ).
  GR_COLUMN->SET_LONG_TEXT( 'Capacity（Unit）' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Capacity（Unit）' ).
  GR_COLUMN->SET_SHORT_TEXT( 'Capa(UNT)' ).

  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'ZZCAU_DES' ).
  GR_COLUMN->SET_LONG_TEXT( 'Capacity（Unit） Desc' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Capacity（Unit） Desc' ).
  GR_COLUMN->SET_SHORT_TEXT( 'Cap（UNT)De' ).


  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'ZZIOD' ).
  GR_COLUMN->SET_LONG_TEXT( 'Indoor / Outdoor' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Indoor / Outdoor' ).
  GR_COLUMN->SET_SHORT_TEXT( 'In/Out' ).

  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'ZZIOD_DES' ).
  GR_COLUMN->SET_LONG_TEXT( 'Indoor / Outdoor Desc.' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Indoor/OutdoorDesc.' ).
  GR_COLUMN->SET_SHORT_TEXT( 'In/Out.Des' ).


  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'ZZIUT' ).
  GR_COLUMN->SET_LONG_TEXT( 'Indoor Unit Type' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Indoor Unit Type' ).
  GR_COLUMN->SET_SHORT_TEXT( 'In UNT Typ' ).


  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'ZZIUT_DES' ).
  GR_COLUMN->SET_LONG_TEXT( 'Indoor Unit Type Desc.' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'In Unit Type Desc.' ).
  GR_COLUMN->SET_SHORT_TEXT( 'InUntTyDes' ).




  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'MVGR1' ).
  GR_COLUMN->SET_LONG_TEXT( 'Material Group 1' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Material Group 1' ).
  GR_COLUMN->SET_SHORT_TEXT( 'MatGrp 1' ).

  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'MVGR1_DES' ).
  GR_COLUMN->SET_LONG_TEXT( 'Material Group 1 Desc.' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Mat Group 1 Desc' ).
  GR_COLUMN->SET_SHORT_TEXT( 'MatGrp1Des' ).

  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'MVGR2_DES' ).
  GR_COLUMN->SET_LONG_TEXT( 'Material Group 2 Desc' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Mat Group 2 Desc' ).
  GR_COLUMN->SET_SHORT_TEXT( 'MatGrp2Des' ).

  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'MVGR3_DES' ).
  GR_COLUMN->SET_LONG_TEXT( 'Material Group 3 Desc' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Mat Group 3 Desc' ).
  GR_COLUMN->SET_SHORT_TEXT( 'MatGrp3Des' ).

  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'MVGR4_DES' ).
  GR_COLUMN->SET_LONG_TEXT( 'Material Group 4 Desc' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Mat Group 4 Desc' ).
  GR_COLUMN->SET_SHORT_TEXT( 'MatGrp4Des' ).

  GR_COLUMN ?= GR_COLUMNS->GET_COLUMN( 'MVGR5_DES' ).
  GR_COLUMN->SET_LONG_TEXT( 'Material Group 5 Desc' ).
  GR_COLUMN->SET_MEDIUM_TEXT( 'Mat Group 5 Desc' ).
  GR_COLUMN->SET_SHORT_TEXT( 'MatGrp5Des' ).
"--> End Add by wantanee 20250715


**-- Top Of Page
*
  DATA: LO_HEADER  TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
          LO_H_LABEL TYPE REF TO CL_SALV_FORM_LABEL,
          LO_H_FLOW  TYPE REF TO CL_SALV_FORM_LAYOUT_FLOW,
          LV_STR(50).

  CREATE OBJECT LO_HEADER.
*
  LO_H_LABEL = LO_HEADER->CREATE_LABEL( ROW = 1 COLUMN = 1 ).
  LO_H_LABEL->SET_TEXT( SY-TITLE ).
  LO_H_FLOW = LO_HEADER->CREATE_FLOW( ROW = 5  COLUMN = 1 ).
  CLEAR LV_STR.
  CONCATENATE 'Date: ' SY-DATUM  '  Time: ' SY-UZEIT INTO LV_STR RESPECTING BLANKS.
  LO_H_FLOW->CREATE_TEXT( TEXT = LV_STR ).
*    lo_h_flow = lo_header->create_flow( row = 3  column = 1 ).
*    lo_h_flow->create_text( text = 'Number of Records in the output' ).
*    lo_h_flow = lo_header->create_flow( row = 3  column = 2 ).
*    lo_h_flow->create_text( text = 20 ).

  GR_TABLE->SET_TOP_OF_LIST( LO_HEADER ).
*   set the top of list using the header for Print.
  GR_TABLE->SET_TOP_OF_LIST_PRINT( LO_HEADER ).
*
  GR_TABLE->DISPLAY( ).

ENDFORM.                    " F_SHOW_ALV
*&---------------------------------------------------------------------*
*&      Form  F_F4_FOR_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_F4_FOR_VARIANT .

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      IS_VARIANT = GV_VARIANT
      I_SAVE     = GV_SAVE
    IMPORTING
      E_EXIT     = GV_EXIT
      ES_VARIANT = GV_X_VARIANT
    EXCEPTIONS
      NOT_FOUND  = 2.

  IF SY-SUBRC = 2.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    IF GV_EXIT = SPACE.
      S_VARI = GV_X_VARIANT-VARIANT.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_F4_FOR_VARIANT
*&---------------------------------------------------------------------*
*&      Form  F_INITIALIZE_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_INITIALIZE_VARIANT .

  GV_REPNAME = SY-REPID.


  CLEAR GV_VARIANT.
  GV_SAVE           = 'X'.
  GV_VARIANT-REPORT = GV_REPNAME.
  GV_X_VARIANT      = GV_VARIANT.

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      I_SAVE     = GV_SAVE
    CHANGING
      CS_VARIANT = GV_X_VARIANT
    EXCEPTIONS
      NOT_FOUND  = 2.

  IF SY-SUBRC = 0.

    S_VARI = GV_X_VARIANT-VARIANT.

  ENDIF.

ENDFORM.                    " F_INITIALIZE_VARIANT
*&---------------------------------------------------------------------*
*&      Form  F_AT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_AT_SELECTION_SCREEN .


  IF NOT S_VARI IS INITIAL.

    MOVE GV_VARIANT TO GV_X_VARIANT.
    MOVE S_VARI    TO GV_X_VARIANT-VARIANT.

    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        I_SAVE     = GV_SAVE
      CHANGING
        CS_VARIANT = GV_X_VARIANT.
    GV_VARIANT = GV_X_VARIANT.

  ELSE.

*    PERFORM f_initialize_variant.

  ENDIF.
ENDFORM.                    " F_AT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_DATA .

  IF S_SPRAS[] IS INITIAL.
    S_SPRAS-LOW = 'EN'.
    S_SPRAS-SIGN = 'I'.
    S_SPRAS-OPTION = 'EQ'.
    APPEND  S_SPRAS.
  ENDIF.

  SELECT A~MATNR
         ERSDA
        ERNAM
        LAEDA
        AENAM
        MTART
        MATKL
        SPART
        PRDHA
        MFRNR
        EAN11
        MEINS
        A~MATNR
        MAKTX
        A~LABOR
        BRGEW
        NTGEW
        GROES
        MSTAE
        MSTAV
        C~LBTXT
        GEWEI
        VOLEH
        ZZFUCP
        ZZSBCT
        ZZCAV
        ZZCAU
        ZZIOD
        ZZIUT
  INTO TABLE GT_MARA
  FROM  MARA AS A INNER JOIN MAKT AS B
  ON A~MATNR EQ B~MATNR
  LEFT OUTER JOIN T024X AS C
  ON A~LABOR EQ  C~LABOR
  AND C~SPRAS  EQ  S_SPRAS-LOW
  WHERE A~MATNR IN S_MATNR
  AND  A~AENAM IN S_AENAM
  AND  A~ERNAM IN S_ERNAM
  AND  A~MATKL IN S_MATKL
  AND  A~MTART IN S_MTART
  AND  A~PRDHA IN S_PRDHA
  AND  A~SPART IN S_SPART
  AND  B~SPRAS IN S_SPRAS
  AND MTART IN S_MTART.


  CHECK GT_MARA[] IS NOT INITIAL.

*-- Supoj Add on 02.03.2015
  SELECT MATNR
           MEINH
           LAENG   "Length
           BREIT    " Width
          HOEHE "Height
          VOLUM  "Volumn
          BRGEW "Gross Weight
  FROM MARM
  INTO TABLE GT_MARM
  FOR ALL ENTRIES IN GT_MARA
  WHERE MATNR EQ GT_MARA-MATNR.
*-- End Add


*-- Vendor Name
  SELECT LIFNR
         NAME1
         NAME2
    FROM LFA1
    INTO TABLE GT_LFA1
    FOR ALL ENTRIES IN GT_MARA
    WHERE LIFNR EQ GT_MARA-MFRNR.

*-- Product Heirachy Sale Org
  SELECT MATNR
         VKORG
         VTWEG
         PRODH
         MVGR1
         MVGR2
         MVGR3
         MVGR4
         MVGR5
   FROM  MVKE
   INTO TABLE GT_MVKE
   FOR ALL ENTRIES IN GT_MARA
   WHERE MATNR EQ GT_MARA-MATNR.


*-- GR Processing Time
  SELECT WERKS
   MATNR
   WEBAZ
   SERNP
   LADGR
   FROM MARC
   INTO TABLE GT_MARC
   FOR ALL ENTRIES IN GT_MARA
   WHERE MATNR EQ GT_MARA-MATNR.



*-- Moving Price
  SELECT
       MATNR
       BWKEY
       BWTAR
       VERPR
       STPRS
   FROM  MBEW
   APPENDING TABLE GT_MBEW
   FOR ALL ENTRIES IN GT_MARA
   WHERE  MATNR EQ GT_MARA-MATNR.
*   and  bwkey eq gt_vbap-werks.
*   and bwtar eq gt_vbap-lgort.



  DATA: LT_COL_AUSP TYPE TABLE OF TY_AUSP.
*        lw_ausp like line of lt_col_ausp.

  SELECT OBJEK
        KLART
        ATWRT
        A~ATINN
        ATBEZ
   FROM AUSP AS A INNER JOIN  CABNT AS B
   ON A~ATINN EQ B~ATINN
*   and b~spras in s_spras
   INTO TABLE GT_AUSP
   FOR ALL ENTRIES IN GT_MARA
   WHERE  OBJEK EQ GT_MARA-OBJEK
   AND KLART EQ '001'
   AND SPRAS EQ 'E'.    "in s_spras.

  GT_COL_AUSP[]  = GT_AUSP[].
  SORT GT_COL_AUSP BY ATINN.
  DELETE ADJACENT DUPLICATES FROM GT_COL_AUSP COMPARING ATINN.


*-- MA Product

*  SELECT *
*  FROM ZTCS_MA_PRODUCT
*  INTO TABLE GT_ZTCS_MA_PRODUCT.
*
*  IF  GT_ZTCS_MA_PRODUCT[] IS NOT INITIAL.
*
**-- Standard Price
*    SELECT
*         MATNR
*         BWKEY
*         BWTAR
*         VERPR
*         STPRS
*     FROM  MBEW
*     APPENDING TABLE GT_MBEW_MA_PRODUCT
*     FOR ALL ENTRIES IN GT_ZTCS_MA_PRODUCT
*     WHERE  MATNR EQ GT_ZTCS_MA_PRODUCT-ZMAT_MA_INSPEC.
*
*    SELECT
*      MATNR
*      BWKEY
*      BWTAR
*      VERPR
*      STPRS
*   FROM  MBEW
*   APPENDING TABLE GT_MBEW_MA_PRODUCT
*   FOR ALL ENTRIES IN GT_ZTCS_MA_PRODUCT
*   WHERE  MATNR EQ GT_ZTCS_MA_PRODUCT-ZMAT_MA_CLEANING.
*
*  ENDIF.

     SELECT ZZIUT , DESCRIPTION
       INTO TABLE @GT_ZDSMMC026
       FROM ZDSMMC026.

     SELECT ZZIOD , DESCRIPTION
       INTO TABLE @GT_ZDSMMC001
       FROM ZDSMMC001.

     SELECT ZZCAU , DESCRIPTION
       INTO TABLE @GT_ZDSMMC027
       FROM ZDSMMC027.

     SELECT MVGR1 , BEZEI
       INTO TABLE @GT_TVM1T
       FROM TVM1T
       WHERE SPRAS = 'E'.

     SELECT MVGR2 , BEZEI
       INTO TABLE @GT_TVM2T
       FROM TVM2T
       WHERE SPRAS = 'E'.

     SELECT MVGR3 , BEZEI
       INTO TABLE @GT_TVM3T
       FROM TVM3T
       WHERE SPRAS = 'E'.

     SELECT MVGR4 , BEZEI
       INTO TABLE @GT_TVM4T
       FROM TVM4T
       WHERE SPRAS = 'E'.

     SELECT MVGR5 , BEZEI
       INTO TABLE @GT_TVM5T
       FROM TVM5T
       WHERE SPRAS = 'E'.


ENDFORM.                    " F_GET_DATA
*&---------------------------------------------------------------------*
*&      Form  F_CREATE_DYN_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CREATE_DYN_TABLE .

  DATA: LT_FCAT  TYPE LVC_T_FCAT,
        LW_FCAT LIKE LINE OF LT_FCAT,
        LW_FLD   TYPE STRING .

  DATA: LT_ITAB TYPE REF TO DATA.

  PERFORM F_APPEND_FCAT TABLES LT_FCAT
                      :USING 'MATNR'  'MATNR' 'MARA' SPACE,
                      USING 'ERSDA' 'ERSDA' 'MARA' SPACE,
                      USING 'ERNAM' 'ERNAM'  'MARA' SPACE,
                      USING 'LAEDA' 'LAEDA'  'MARA' SPACE,
                      USING 'AENAM' 'AENAM'  'MARA' SPACE,
                      USING 'MTART'  'MTART' 'MARA' SPACE,
                      USING 'MATKL'  'MATKL'  'MARA' SPACE,
                      USING 'SPART'  'SPART'  'MARA' SPACE,
                      USING 'PRDHA'  'PRDHA' 'MARA' SPACE,
                      USING 'MFRNR' 'MFRNR'  'MARA' SPACE,
                      USING 'VENDOR_NAME' 'LFURL' 'LFA1' SPACE,
                      USING 'EAN11'  'EAN11' 'MARA'  SPACE,
                      USING 'MEINS'  'MEINS' 'MARA' SPACE,
                      USING 'VERPR'   'VERPR' 'MBEW' SPACE,
                      USING 'STPRS'   'STPRS' 'MBEW' SPACE,
                      USING 'SERNP'   'SERNP' 'MARC' SPACE,
                      USING 'PRODH'  'PRDHA' 'MARA' SPACE,
                      USING 'MAKTX'  'MAKTX' 'MAKT' SPACE,
                      USING 'WEBAZ'  'WEBAZ' 'MARC' SPACE,
*                      USING 'ZMAT_MA_INSPEC' 'ZMAT_MA_INSPEC' 'ZTCS_MA_PRODUCT' SPACE,
                       USING 'INSPEC_STPRS' 'STPRS' 'MBEW' SPACE,
*                      USING 'ZMAT_MA_CLEANING' 'ZMAT_MA_CLEANING' 'ZTCS_MA_PRODUCT' SPACE,
                      USING 'CLEAN_STPRS' 'STPRS' 'MBEW' SPACE,
*                      --Supoj Add on 02.03.2015
                        USING 'LAENG'  'LAENG' 'MARA' SPACE,
                       USING 'BREIT'  'BREIT' 'MARA' SPACE,
                       USING 'HOEHE'  'HOEHE' 'MARA' SPACE,
                       USING 'VOLUM'  'VOLUM' 'MARA' SPACE,
                        USING 'BRGEW'  'BRGEW' 'MARA' SPACE,
                       USING 'NTGEW'  'NTGEW' 'MARA' SPACE,
                       USING 'GROES'  'GROES' 'MARA' SPACE,
                       USING 'LABOR'  'LABOR' 'MARA' SPACE,
                       USING 'LBTXT'  'LBTXT' 'T024X' SPACE,
                       USING 'ROTAT'  'ATWRT' 'AUSP' SPACE,
                       USING 'WRKST'  'WRKST' 'MARA' SPACE,
                       USING 'MSTAE'  'MSTAE' 'MARA' SPACE,
                       USING 'MSTAV'  'MSTAV' 'MARA' SPACE,
                       USING 'ZEINR'  'ZEINR' 'MARA' SPACE,
                       USING 'MTVFP'  'MTVFP' 'MARC' SPACE,
                       USING 'GEWEI'    'GEWEI'   'MARA'  SPACE,
                       USING 'VOLEH'    'VOLEH'   'MARA'  SPACE,
                       USING 'ZZFUCP'   'ZZFUCP'  'MARA'  SPACE,
                       USING 'ZZSBCT'   'ZZSBCT'  'MARA'  SPACE,
                       USING 'LADGR'    'LADGR'   'MARC'  SPACE,
                       USING 'ZZCAV'    'ZZCAV'   'MARA'  SPACE, "Add 20250715
                       USING 'ZZCAU'    'ZZCAU'   'MARA'  SPACE, "Add 20250715
                       USING 'ZZCAU_DES' 'DESCRIPTION' 'ZDSMMC027' SPACE, "Add 20250715
                       USING 'ZZIOD'    'ZZIOD'   'MARA'  SPACE, "Add 20250715
                       USING 'ZZIOD_DES' 'DESCRIPTION' 'ZDSMMC001'  SPACE, "Add 20250715
                       USING 'ZZIUT'    'ZZIUT'   'MARA'  SPACE, "Add 20250715
                       USING 'ZZIUT_DES' 'DESCRIPTION' 'ZDSMMC026'  SPACE, "Add 20250715
                       USING 'MVGR1'    'MVGR1'   'MVKE'  SPACE, "Add 20250715
                       USING 'MVGR1_DES' 'BEZEI'  'TVM1T'  SPACE, "Add 20250715
                       USING 'MVGR2'    'MVGR2'   'MVKE'  SPACE, "Add 20250715
                       USING 'MVGR2_DES'  'BEZEI'  'TVM2T'  SPACE, "Add 20250715
                       USING 'MVGR3'    'MVGR3'   'MVKE'  SPACE, "Add 20250715
                       USING 'MVGR3_DES'  'BEZEI'  'TVM3T'  SPACE, "Add 20250715
                       USING 'MVGR4'    'MVGR4'   'MVKE'  SPACE, "Add 20250715
                       USING 'MVGR4_DES'  'BEZEI'  'TVM4T'  SPACE, "Add 20250715
                       USING 'MVGR5'    'MVGR5'   'MVKE'  SPACE, "Add 20250715
                       USING 'MVGR5_DES'   'BEZEI'  'TVM5T'  SPACE. "Add 20250715




*                       -- End Add

  DATA: LW_AUSP LIKE LINE OF GT_COL_AUSP.


  LOOP AT GT_COL_AUSP INTO LW_AUSP.

    PERFORM F_APPEND_FCAT TABLES LT_FCAT
                        :USING LW_AUSP-ATINN  'ATWRT' 'AUSP' LW_AUSP-ATWRT.

  ENDLOOP.

* Create Dynamic Table
  CALL METHOD CL_ALV_TABLE_CREATE=>CREATE_DYNAMIC_TABLE
    EXPORTING
      IT_FIELDCATALOG           = LT_FCAT
    IMPORTING
      EP_TABLE                  = LT_ITAB
    EXCEPTIONS
      GENERATE_SUBPOOL_DIR_FULL = 1
      OTHERS                    = 2.
  ASSIGN LT_ITAB->* TO <FS_TABLE>.
  CREATE DATA LT_ITAB LIKE LINE OF <FS_TABLE>.
  ASSIGN LT_ITAB->* TO <FS_LINE>.


ENDFORM.                    " F_CREATE_DYN_TABLE
*&---------------------------------------------------------------------*
*&      Form  F_MAP_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_MAP_DATA .

  DATA: LW_MARA LIKE LINE OF GT_MARA,
        LW_LFA1 LIKE LINE OF GT_LFA1,
        LW_MVKE LIKE LINE OF GT_MVKE,
        LW_AUSP LIKE LINE OF GT_AUSP,
        LW_MARC LIKE LINE OF GT_MARC,
        LW_MBEW LIKE LINE OF GT_MBEW,
        LW_MARM LIKE LINE OF GT_MARM.
*        LW_ZTCS_MA_PRODUCT LIKE LINE OF GT_ZTCS_MA_PRODUCT.


  SORT: GT_LFA1 BY LIFNR,
        GT_MVKE BY MATNR,
        GT_AUSP BY OBJEK,
        GT_MBEW BY MATNR,
        GT_MARC BY MATNR,
        GT_MARA BY MATNR,
        GT_COL_AUSP BY ATBEZ,
*        GT_ZTCS_MA_PRODUCT BY ZPH1 ZMA_TYPE_CODE ZBTU_FROM ZBTU_TO,
        GT_MBEW_MA_PRODUCT BY MATNR,
        GT_MARM BY MATNR.  "Supoj Add on 02.03.2015

*      "Add field
*      GEWEI TYPE  MARA-GEWEI,
*      VOLEH TYPE MARA-VOLEH,
*      ZZFUCP TYPE MARA-ZZFUCP,
*      ZZSBCT TYPE MARA-ZZSBCT,
*      LADGR  TYPE MARC-LADGR,


  LOOP AT GT_MARA INTO LW_MARA.

    CLEAR <FS_LINE>.

    CLEAR GR_OBJEK.
    GR_OBJEK-SIGN   = 'I'.
    GR_OBJEK-OPTION = 'EQ'.
    GR_OBJEK-LOW    = LW_MARA-MATNR.
    APPEND GR_OBJEK.

    ASSIGN COMPONENT 'MATNR' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
    MOVE LW_MARA-MATNR TO <FS_FIELD>.

    ASSIGN COMPONENT 'ERSDA' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
    MOVE LW_MARA-ERSDA TO <FS_FIELD>.

    ASSIGN COMPONENT 'ERNAM' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
    MOVE LW_MARA-ERNAM TO <FS_FIELD>.

    ASSIGN COMPONENT 'LAEDA' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
    MOVE LW_MARA-LAEDA TO <FS_FIELD>.

    ASSIGN COMPONENT 'AENAM' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
    MOVE LW_MARA-AENAM TO <FS_FIELD>.

    ASSIGN COMPONENT 'MTART' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
    MOVE LW_MARA-MTART TO <FS_FIELD>.

    ASSIGN COMPONENT 'MATKL' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
    MOVE LW_MARA-MATKL TO <FS_FIELD>.

    ASSIGN COMPONENT 'SPART' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
    MOVE LW_MARA-SPART TO <FS_FIELD>.

    ASSIGN COMPONENT 'PRDHA' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
    MOVE LW_MARA-PRDHA TO <FS_FIELD>.

    ASSIGN COMPONENT 'MFRNR' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
    MOVE LW_MARA-MFRNR TO <FS_FIELD>.

    ASSIGN COMPONENT 'EAN11' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
    MOVE LW_MARA-EAN11 TO <FS_FIELD>.

    ASSIGN COMPONENT 'MEINS' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
    MOVE LW_MARA-MEINS TO <FS_FIELD>.


    ASSIGN COMPONENT 'MAKTX' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
    MOVE LW_MARA-MAKTX TO <FS_FIELD>.

*-- Supoj add on 02.03.2015

    ASSIGN COMPONENT 'LABOR' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
    MOVE LW_MARA-LABOR TO <FS_FIELD>.

    ASSIGN COMPONENT 'LBTXT' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
    MOVE LW_MARA-LBTXT TO <FS_FIELD>.



    ASSIGN COMPONENT 'GROES' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
    MOVE LW_MARA-GROES TO <FS_FIELD>.

    ASSIGN COMPONENT 'NTGEW' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
    MOVE LW_MARA-NTGEW TO <FS_FIELD>.
"Add by Wantanee     20180302
    ASSIGN COMPONENT 'MSTAE' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
    MOVE LW_MARA-MSTAE TO <FS_FIELD>.

    ASSIGN COMPONENT 'MSTAV' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
    MOVE LW_MARA-MSTAV TO <FS_FIELD>.
"End Add by Wantanee  20180302

    ASSIGN COMPONENT 'GEWEI' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
    MOVE LW_MARA-GEWEI TO <FS_FIELD>.

    ASSIGN COMPONENT 'VOLEH' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
    MOVE LW_MARA-VOLEH TO <FS_FIELD>.

    ASSIGN COMPONENT 'ZZFUCP' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
    MOVE LW_MARA-ZZFUCP TO <FS_FIELD>.

    ASSIGN COMPONENT 'ZZSBCT' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
    MOVE LW_MARA-ZZFUCP TO <FS_FIELD>.


    "Add 20250715
    ASSIGN COMPONENT 'ZZCAV' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
    MOVE LW_MARA-ZZCAV TO <FS_FIELD>.

    ASSIGN COMPONENT 'ZZCAU' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
    MOVE LW_MARA-ZZCAU TO <FS_FIELD>.

    ASSIGN COMPONENT 'ZZIOD' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
    MOVE LW_MARA-ZZIOD TO <FS_FIELD>.

    ASSIGN COMPONENT 'ZZIUT' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
    MOVE LW_MARA-ZZIUT TO <FS_FIELD>.


    "End Add 20250715
    READ TABLE GT_MARM INTO LW_MARM WITH KEY MATNR = LW_MARA-MATNR
                                                                BINARY SEARCH.
    IF SY-SUBRC EQ 0.


      ASSIGN COMPONENT 'LAENG' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
      MOVE LW_MARM-LAENG TO <FS_FIELD>.

      ASSIGN COMPONENT 'BREIT' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
      MOVE LW_MARM-BREIT TO <FS_FIELD>.

      ASSIGN COMPONENT 'HOEHE' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
      MOVE LW_MARM-HOEHE TO <FS_FIELD>.

      ASSIGN COMPONENT 'VOLUM' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
      MOVE LW_MARM-VOLUM TO <FS_FIELD>.


      ASSIGN COMPONENT 'BRGEW' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
      MOVE LW_MARM-BRGEW TO <FS_FIELD>.


    ENDIF.

*-- End add

    READ TABLE GT_LFA1 INTO LW_LFA1 WITH KEY LIFNR = LW_MARA-MFRNR
                                             BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      ASSIGN COMPONENT 'VENDOR_NAME' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
      CONCATENATE LW_LFA1-NAME1 LW_LFA1-NAME2 INTO  <FS_FIELD>.
    ENDIF.

    READ TABLE GT_MVKE INTO LW_MVKE WITH KEY MATNR = LW_MARA-MATNR
                                             BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      ASSIGN COMPONENT 'PRODH' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
      MOVE LW_MVKE-PRODH TO <FS_FIELD>.
*      concatenate lw_lfa1-name1 lw_lfa1-name2 into  <FS_FIELD>.

      "Add 20250715
      ASSIGN COMPONENT 'MVGR1' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
      MOVE LW_MVKE-MVGR1 TO <FS_FIELD>.

      ASSIGN COMPONENT 'MVGR2' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
      MOVE LW_MVKE-MVGR2 TO <FS_FIELD>.

      ASSIGN COMPONENT 'MVGR3' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
      MOVE LW_MVKE-MVGR3 TO <FS_FIELD>.

      ASSIGN COMPONENT 'MVGR4' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
      MOVE LW_MVKE-MVGR4 TO <FS_FIELD>.

      ASSIGN COMPONENT 'MVGR5' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
      MOVE LW_MVKE-MVGR5 TO <FS_FIELD>.
      "End Add 20250715

         READ TABLE GT_TVM1T INTO GS_TVM1T WITH KEY MVGR1 = LW_MVKE-MVGR1
                                             BINARY SEARCH.
             IF sy-subrc EQ 0.
                 ASSIGN COMPONENT 'MVGR1_DES' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
                 MOVE GS_TVM1T-BEZEI TO <FS_FIELD>.
             ENDIF.

         READ TABLE GT_TVM2T INTO GS_TVM2T WITH KEY MVGR2 = LW_MVKE-MVGR2
                                                   BINARY SEARCH.
            IF sy-subrc EQ 0.
                ASSIGN COMPONENT 'MVGR2_DES' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
                MOVE GS_TVM2T-BEZEI TO <FS_FIELD>.
            ENDIF.
         READ TABLE GT_TVM3T INTO GS_TVM3T WITH KEY MVGR3 = LW_MVKE-MVGR3
                                                   BINARY SEARCH.
            IF sy-subrc EQ 0.
                ASSIGN COMPONENT 'MVGR3_DES' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
                MOVE GS_TVM3T-BEZEI TO <FS_FIELD>.
            ENDIF.
         READ TABLE GT_TVM4T INTO GS_TVM4T WITH KEY MVGR4 = LW_MVKE-MVGR4
                                                   BINARY SEARCH.
            IF sy-subrc EQ 0.
                ASSIGN COMPONENT 'MVGR4_DES' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
                MOVE GS_TVM4T-BEZEI TO <FS_FIELD>.
            ENDIF.
         READ TABLE GT_TVM5T INTO GS_TVM5T WITH KEY MVGR5 = LW_MVKE-MVGR5
                                                   BINARY SEARCH.
            IF sy-subrc EQ 0.
                ASSIGN COMPONENT 'MVGR5_DES' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
                MOVE GS_TVM5T-BEZEI TO <FS_FIELD>.
            ENDIF.



    ENDIF.

    READ TABLE GT_MBEW INTO LW_MBEW WITH KEY MATNR = LW_MARA-MATNR
                                             BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      ASSIGN COMPONENT 'VERPR' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
      MOVE LW_MBEW-VERPR TO <FS_FIELD>.

      ASSIGN COMPONENT 'STPRS' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
      MOVE LW_MBEW-STPRS TO <FS_FIELD>.
    ENDIF.


    READ TABLE GT_MARC INTO LW_MARC WITH KEY MATNR = LW_MARA-MATNR
                                             BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      ASSIGN COMPONENT 'WEBAZ' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
      MOVE LW_MARC-WEBAZ TO <FS_FIELD>.

      ASSIGN COMPONENT 'SERNP' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
      MOVE LW_MARC-SERNP TO <FS_FIELD>.

      ASSIGN COMPONENT 'LADGR' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
      MOVE LW_MARC-LADGR TO <FS_FIELD>.

    ENDIF.
*BREAK-POINT.

   READ TABLE GT_ZDSMMC026 INTO GS_ZDSMMC026 WITH KEY ZZIUT = LW_MARA-ZZIUT
                                             BINARY SEARCH.
      IF sy-subrc EQ 0.
          ASSIGN COMPONENT 'ZZIUT_DES' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
          MOVE GS_ZDSMMC026-DESCRIPTION TO <FS_FIELD>.
      ENDIF.

   READ TABLE GT_ZDSMMC001 INTO GS_ZDSMMC001 WITH KEY ZZIOD = LW_MARA-ZZIOD
                                             BINARY SEARCH.
      IF sy-subrc EQ 0.
          ASSIGN COMPONENT 'ZZIOD_DES' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
          MOVE GS_ZDSMMC001-DESCRIPTION TO <FS_FIELD>.
      ENDIF.

   READ TABLE GT_ZDSMMC027 INTO GS_ZDSMMC027 WITH KEY ZZCAU = LW_MARA-ZZCAU
                                             BINARY SEARCH.
      IF sy-subrc EQ 0.
          ASSIGN COMPONENT 'ZZCAU_DES' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
          MOVE GS_ZDSMMC027-DESCRIPTION TO <FS_FIELD>.
      ENDIF.




    DATA:LT_CLOBJDAT TYPE TABLE OF CLOBJDAT,
         LW_CLOBJDAT LIKE LINE OF LT_CLOBJDAT,
         LT_SCLASS  TYPE TABLE OF SCLASS.


    CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
      EXPORTING
*   CLASS                      = ' '
*   CLASSTEXT                  = 'X'
        CLASSTYPE                  = '001'
*   CLINT                      = 0
*   FEATURES                   = 'X'
*   LANGUAGE                   = SY-LANGU
        OBJECT                     = LW_MARA-OBJEK
*   OBJECTTABLE                = ' '
*   KEY_DATE                   = SY-DATUM
*   INITIAL_CHARACT            = 'X'
*   NO_VALUE_DESCRIPT          =
*   CHANGE_SERVICE_CLF         = 'X'
*   INHERITED_CHAR             = ' '
*   CHANGE_NUMBER              = ' '
      TABLES
        T_CLASS                    =  LT_SCLASS
        T_OBJECTDATA               = LT_CLOBJDAT
*   I_SEL_CHARACTERISTIC       =
*   T_NO_AUTH_CHARACT          =
 EXCEPTIONS
   NO_CLASSIFICATION          = 1
   NO_CLASSTYPES              = 2
   INVALID_CLASS_TYPE         = 3
   OTHERS                     = 4
              .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    DATA: LV_INT(2) TYPE N,
          LV_CHAR(20),
          LV_ATWTB TYPE CAWNT-ATWTB.
    LOOP AT  LT_CLOBJDAT INTO LW_CLOBJDAT.
      READ TABLE GT_COL_AUSP INTO LW_AUSP  WITH KEY ATBEZ = LW_CLOBJDAT-SMBEZ
                                                    BINARY SEARCH.
      IF SY-SUBRC EQ 0 AND LW_CLOBJDAT-AUSP1 IS NOT INITIAL AND  LW_CLOBJDAT-AUSP1 NE '?'.
        MOVE  LW_AUSP-ATINN TO LV_CHAR.
*        unassign <FS_FIELD>.
        ASSIGN COMPONENT LV_CHAR OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
*        if <FS_FIELD> is assigned.
        MOVE LW_CLOBJDAT-AUSP1 TO <FS_FIELD>.
*        endif.
      ENDIF.
    ENDLOOP.

*    loop at  gt_ausp into lw_ausp where objek = lw_mara-objek.
*      move  lw_ausp-atinn to lv_char.
*      ASSIGN COMPONENT lv_char OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
*      MOVE lw_ausp-atwrt to <FS_FIELD>.
*      clear lv_atwtb.
*      select single atwtb
*        from cawnt
*        into lv_atwtb
*        where atzhl eq lw_ausp-atwrt
*        and  atinn eq lw_ausp-atinn
*        and spras eq sy-langu.
*      if sy-subrc eq 0.
*        MOVE lv_atwtb to <FS_FIELD>.
*      endif.
*    endloop.



*    FIELD-SYMBOLS <FS_FIELD_CHECK> TYPE ANY.
*
*    UNASSIGN  <FS_FIELD_CHECK>.
*    LOOP AT  GT_ZTCS_MA_PRODUCT INTO LW_ZTCS_MA_PRODUCT WHERE
*                                        ZPH1 = LW_MARA-PRDHA+0(5)
*                                  AND   ZMA_TYPE_CODE = LW_MARA-LABOR.
*      ASSIGN COMPONENT '0000000823' OF STRUCTURE <FS_LINE> TO <FS_FIELD_CHECK>.
*
*      IF <FS_FIELD_CHECK> IS ASSIGNED.
*        REPLACE ALL OCCURENCES OF ',' IN <FS_FIELD_CHECK> WITH SPACE.
*        REPLACE ALL OCCURENCES OF '-' IN <FS_FIELD_CHECK> WITH SPACE.
*        CALL FUNCTION 'CATS_NUMERIC_INPUT_CHECK'
*          EXPORTING
*            INPUT            = <FS_FIELD_CHECK>
*           INTERNAL         = 'X'
**         IMPORTING
**           OUTPUT           =
*         EXCEPTIONS
*           NO_NUMERIC       = 1
*           ERROR_MESSAGE    = 2
*           OTHERS           = 3
*                  .
*        IF SY-SUBRC <> 0.
*          CONTINUE.
*        ENDIF.
*
*        IF <FS_FIELD_CHECK> BETWEEN  LW_ZTCS_MA_PRODUCT-ZBTU_FROM AND LW_ZTCS_MA_PRODUCT-ZBTU_TO.
*
*          UNASSIGN  <FS_FIELD>.
*          ASSIGN COMPONENT 'ZMAT_MA_INSPEC' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
*          MOVE LW_ZTCS_MA_PRODUCT-ZMAT_MA_INSPEC TO <FS_FIELD>.
*
*
*          ASSIGN COMPONENT 'ZMAT_MA_CLEANING' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
*          MOVE LW_ZTCS_MA_PRODUCT-ZMAT_MA_CLEANING TO <FS_FIELD>.
*
*
*          READ TABLE GT_MBEW_MA_PRODUCT INTO LW_MBEW WITH KEY MATNR = LW_ZTCS_MA_PRODUCT-ZMAT_MA_INSPEC
*                                                                        BINARY SEARCH.
*          IF SY-SUBRC EQ 0.
*            ASSIGN COMPONENT 'INSPEC_STPRS' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
*            MOVE LW_MBEW-STPRS TO <FS_FIELD>.
*          ENDIF.
*          READ TABLE GT_MBEW_MA_PRODUCT INTO LW_MBEW WITH KEY MATNR = LW_ZTCS_MA_PRODUCT-ZMAT_MA_CLEANING
*                                                                   BINARY SEARCH.
*          IF SY-SUBRC EQ 0.
*            ASSIGN COMPONENT 'CLEAN_STPRS' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.
*            MOVE LW_MBEW-STPRS TO <FS_FIELD>.
*          ENDIF.
*
*          EXIT.
*
*
*        ENDIF.
*      ENDIF.
*
*    ENDLOOP.

    APPEND <FS_LINE> TO <FS_TABLE>.

  ENDLOOP.

ENDFORM.                    " F_MAP_DATA

*&---------------------------------------------------------------------*
*&      Form  F_APPEND_FCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_FCAT  text
*      -->P_0499   text
*      -->P_0500   text
*      -->P_0501   text
*      -->P_APPEND  text
*      -->P_LW_FCAT  text
*      -->P_TO  text
*      -->P_LT_FCAT  text
*----------------------------------------------------------------------*
FORM F_APPEND_FCAT  TABLES   P_TAB TYPE   LVC_T_FCAT
                    USING    P_FIELD
                             P_REF_FIELD
                             P_REF_TAB
                             P_SELECT_TEXT.
  CLEAR  P_TAB.

  P_TAB-FIELDNAME = P_FIELD.
  P_TAB-REF_FIELD = P_REF_FIELD.
  P_TAB-REF_TABLE = P_REF_TAB.

  IF P_SELECT_TEXT IS NOT INITIAL.
    P_TAB-SCRTEXT_L  =  P_SELECT_TEXT.
    P_TAB-SCRTEXT_M  =  P_SELECT_TEXT.
    P_TAB-SCRTEXT_S  =  P_SELECT_TEXT.
    P_TAB-REPTEXT    = P_SELECT_TEXT.
    P_TAB-COLDDICTXT = P_SELECT_TEXT.
    P_TAB-SELDDICTXT = P_SELECT_TEXT.
    P_TAB-TIPDDICTXT = P_SELECT_TEXT.
    P_TAB-TXT_FIELD = P_SELECT_TEXT.
  ENDIF.

  APPEND P_TAB.

ENDFORM.                    " F_APPEND_FCAT
*&---------------------------------------------------------------------*
*&      Form  F_ATINN_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_AUSP_ATINN  text
*      <--P_LV_SCRTEXT_L  text
*----------------------------------------------------------------------*
FORM F_ATINN_OUTPUT  USING    P_ATINN
                     CHANGING  CH_TXT.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ATINN_OUTPUT'
*    EXPORTING
*      INPUT  = P_ATINN
*    IMPORTING
*      OUTPUT = CH_TXT.

ENDFORM.                    " F_ATINN_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  F_CALL_MM03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ROW  text
*      -->P_COLUMN  text
*----------------------------------------------------------------------*
FORM F_CALL_MM03  USING    P_ROW
                           P_COLUMN.


  READ TABLE <FS_TABLE> ASSIGNING <FS_LINE> INDEX P_ROW.

  ASSIGN COMPONENT 'MATNR' OF STRUCTURE <FS_LINE> TO <FS_FIELD>.

  SET PARAMETER ID 'MAT'  FIELD <FS_FIELD>.
  CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.



ENDFORM.                    " F_CALL_MM03
