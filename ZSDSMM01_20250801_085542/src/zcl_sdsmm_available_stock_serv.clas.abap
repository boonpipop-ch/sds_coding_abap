class ZCL_SDSMM_AVAILABLE_STOCK_SERV definition
  public
  inheriting from ZCL_SDSCA_REST_SERVICE
  final
  create public .

public section.

  class-data ET_WERKS type WSRS_T_WERKS_RANGE .
  class-data ET_LGORT type TDT_RG_LGORT .
  constants GC_TRUE type FLAG value 'X' ##NO_TEXT.

  methods PROCESS_DATA
    redefinition .
protected section.
private section.

  class-methods GET_CONSTANTS
    exporting
      !ET_WERKS type WSRS_T_WERKS_RANGE
      !ET_KAPPL type APPLICATION_RANGE_ERP_T
      !ET_KSCHL type FARR_TT_COND_TYPE_RANGE
      !ET_VKORG type RANGE_T_VKORG
      !ET_VTWEG type TMS_T_VTWEG_RANGE .
  class-methods AVAILABLE_STOCK_PROCESS
    importing
      !IF_MATNR type MATNR optional
    exporting
      !ES_RESPONSE type ZSDSMMS032 .
  class-methods GET_STOCK_DATA
    importing
      !IT_WERKS type WSRS_T_WERKS_RANGE
      !IT_PARTGRP_MATNR type ZMATNR_R
      !IT_KAPPL type APPLICATION_RANGE_ERP_T
      !IT_KSCHL type FARR_TT_COND_TYPE_RANGE
      !IT_VKORG type RANGE_T_VKORG
      !IT_VTWEG type TMS_T_VTWEG_RANGE
    exporting
      !ET_RESULT type ZSDSMMS026_TT
      !EF_MESSAGE type ZSDSDE_REST_MESSAGE .
  class-methods GET_MATERIAL_BY_PARTGROUP
    importing
      !IF_MATNR type MATNR
    exporting
      !ET_PARTGRP_MATNR type ZMATNR_R .
ENDCLASS.



CLASS ZCL_SDSMM_AVAILABLE_STOCK_SERV IMPLEMENTATION.


  METHOD AVAILABLE_STOCK_PROCESS.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSMM_AVAILABLE_STOCK_SERV /
*                       AVAILABLE_STOCK_PROCESS
*  Creation Date      : 28.05.2024
*  Author             : Jutamas Y. (Eviden)
*  Add-on ID          : MMI005
*  Description        : Get available stock for spare parts
*  Purpose            : Get constants in table ZSDSCAC001 ->
*                       Get material same as part group ->
*                       Get available stock
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
    DATA: LRT_WERKS         TYPE WSRS_T_WERKS_RANGE,
          LRT_LGORT	        TYPE TDT_RG_LGORT ##NEEDED,
          LRT_KAPPL	        TYPE APPLICATION_RANGE_ERP_T,
          LRT_KSCHL	        TYPE FARR_TT_COND_TYPE_RANGE,
          LRT_VKORG	        TYPE RANGE_T_VKORG,
          LRT_VTWEG	        TYPE TMS_T_VTWEG_RANGE,
          LRT_PARTGRP_MATNR TYPE ZMATNR_R,
          LT_RESULT         TYPE ZSDSMMS026_TT.

    CLEAR ES_RESPONSE .

    "Get constants from GENC
    CALL METHOD ZCL_SDSMM_AVAILABLE_STOCK_SERV=>GET_CONSTANTS
      IMPORTING
        ET_WERKS = LRT_WERKS
        ET_KAPPL = LRT_KAPPL
        ET_KSCHL = LRT_KSCHL
        ET_VKORG = LRT_VKORG
        ET_VTWEG = LRT_VTWEG.


    CALL METHOD ZCL_SDSMM_AVAILABLE_STOCK_SERV=>GET_MATERIAL_BY_PARTGROUP
      EXPORTING
        IF_MATNR         = IF_MATNR
      IMPORTING
        ET_PARTGRP_MATNR = LRT_PARTGRP_MATNR.


    CALL METHOD ZCL_SDSMM_AVAILABLE_STOCK_SERV=>GET_STOCK_DATA
      EXPORTING
        IT_WERKS         = LRT_WERKS
        IT_PARTGRP_MATNR = LRT_PARTGRP_MATNR
        IT_KAPPL         = LRT_KAPPL
        IT_KSCHL         = LRT_KSCHL
        IT_VKORG         = LRT_VKORG
        IT_VTWEG         = LRT_VTWEG
      IMPORTING
        ET_RESULT        = LT_RESULT
        EF_MESSAGE       = ES_RESPONSE-RESP_MESSAGE.

    ES_RESPONSE-STOCK = LT_RESULT[] .

    "Set reponse status and message
    ES_RESPONSE-RESP_STATUS  = 'S'.

    CLEAR: LRT_WERKS[], LT_RESULT[] .


  ENDMETHOD.


  METHOD GET_CONSTANTS.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSMM_AVAILABLE_STOCK_SERV / GET_CONSTANTS
*  Creation Date      : 28.05.2024
*  Author             : Jutamas Y. (Eviden)
*  Add-on ID          : MMI005
*  Description        : Get general constants from table ZSDSCAC001
*  Purpose            : Get general constants from table ZSDSCAC001
*                       Maintenance view: ZSDSV_GEN_C
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
    CONSTANTS:
      LC_PLANT TYPE  ZSDSDE_PARAM_NAME VALUE 'PLANT',
      LC_KAPPL TYPE  ZSDSDE_PARAM_NAME VALUE 'APPLICATION',
      LC_KSCHL TYPE  ZSDSDE_PARAM_NAME VALUE 'CONDITION_TYPE',
      LC_VKORG TYPE  ZSDSDE_PARAM_NAME VALUE 'SALESORG',
      LC_VTWEG TYPE  ZSDSDE_PARAM_NAME VALUE 'DISTRIBUTE_CHANNEL'.

    DATA:
      LT_GENC       TYPE  ZCL_SDSCA_UTILITIES=>TT_GEN_C.

    DATA:
      LF_REPID   TYPE  PROGRAMM VALUE 'ZCL_SDSMM_AVAILABLE_STOCK_SERV'.

* Initialize Output
    CLEAR:  ET_WERKS[],
            ET_KAPPL[],
            ET_KSCHL[],
            ET_VKORG[],
            ET_VTWEG[].


* Read All GenC constants for program
    CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
      EXPORTING
        IF_REPID = LF_REPID
      IMPORTING
        ET_GEN_C = LT_GENC.

    CLEAR: ET_WERKS[],
           ET_KAPPL[],
           ET_KSCHL[],
           ET_VKORG[],
           ET_VTWEG[].

* Assign GenC Constants
    LOOP AT LT_GENC ASSIGNING FIELD-SYMBOL(<L_GENC>).

      CASE <L_GENC>-PARAM.
        WHEN LC_PLANT.
          "Plant
          INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                          OPTION = <L_GENC>-PARAM_OPTION
                          LOW    = <L_GENC>-VALUE_LOW
                          HIGH   = <L_GENC>-VALUE_HIGH )
                  INTO TABLE ET_WERKS.


        WHEN LC_KAPPL  .
          "Application
          INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                          OPTION = <L_GENC>-PARAM_OPTION
                          LOW    = <L_GENC>-VALUE_LOW
                          HIGH   = <L_GENC>-VALUE_HIGH )
                  INTO TABLE ET_KAPPL.
        WHEN LC_KSCHL  .
          "Condition Type
          INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                          OPTION = <L_GENC>-PARAM_OPTION
                          LOW    = <L_GENC>-VALUE_LOW
                          HIGH   = <L_GENC>-VALUE_HIGH )
                  INTO TABLE ET_KSCHL.

        WHEN LC_VKORG  .
          "Sales Org
          INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                          OPTION = <L_GENC>-PARAM_OPTION
                          LOW    = <L_GENC>-VALUE_LOW
                          HIGH   = <L_GENC>-VALUE_HIGH )
                  INTO TABLE ET_KAPPL.
        WHEN LC_VTWEG .
          "Distribution Chanel
          INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                          OPTION = <L_GENC>-PARAM_OPTION
                          LOW    = <L_GENC>-VALUE_LOW
                          HIGH   = <L_GENC>-VALUE_HIGH )
                  INTO TABLE ET_VTWEG.
        WHEN OTHERS.

      ENDCASE.

    ENDLOOP.


  ENDMETHOD.


  METHOD GET_MATERIAL_BY_PARTGROUP.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSMM_AVAILABLE_STOCK_SERV / GET_CONSTANTS
*  Creation Date      : 28.05.2024
*  Author             : Jutamas Y. (Eviden)
*  Add-on ID          : MMI005
*  Description        : Get material same as part group from ZRTCAC002
*  Purpose            : Get material same as part group
*                       from table ZRTCAC002
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
    DATA LF_PART_GROUP TYPE ZRTCAC002-PART_GROUP .

    CLEAR ET_PARTGRP_MATNR[] .
    "Get Spare parts material
    SELECT  PART_GROUP INTO LF_PART_GROUP UP TO 1 ROWS
      FROM  ZRTCAC002                                   "#EC CI_GENBUFF
      WHERE MATNR = IF_MATNR
      ORDER BY PART_GROUP.
    ENDSELECT.

    IF SY-SUBRC = 0 .
      "Get Material same as material inbound.
      SELECT PART_GROUP, MATNR
        FROM ZRTCAC002                                  "#EC CI_GENBUFF
       WHERE PART_GROUP = @LF_PART_GROUP
*         AND ACTIVE = @GC_TRUE
        INTO TABLE @DATA(LT_ZRTCAC002).

      LOOP AT LT_ZRTCAC002 INTO DATA(LS_ZRTCAC002)    .
        APPEND VALUE #( SIGN   = 'I'
                        OPTION = 'EQ'
                        LOW    = LS_ZRTCAC002-MATNR
                        HIGH   = '' )
                  TO ET_PARTGRP_MATNR.
      ENDLOOP.
    ELSE.
      APPEND VALUE #( SIGN   = 'I'
                      OPTION = 'EQ'
                      LOW    = IF_MATNR
                      HIGH   = '' )
                TO ET_PARTGRP_MATNR.
    ENDIF.

  ENDMETHOD.


  METHOD GET_STOCK_DATA.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSMM_AVAILABLE_STOCK_SERV / GET_STOCK_DATA
*  Creation Date      : 28.05.2024
*  Author             : Jutamas Y. (Eviden)
*  Add-on ID          : MMI005
*  Description        : Get stock data
*  Purpose            : Get stock data and call transaction MMBE for get
*                       quantity and set stock data.
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

    DATA:
      LF_STOP_FLG TYPE CHAR1,
      LT_OUTTAB   TYPE STANDARD TABLE OF RMMMBESTN_DATEN,
      LT_OUTTAB_L TYPE STANDARD TABLE OF RMMMBESTN_DATEN,
      LS_STOCK    TYPE ZSDSMMS026.

    DATA:
      LRT_MATNR TYPE RANGE OF MATNR,
      LRT_WERKS TYPE RANGE OF WERKS.
*
*    DATA LF_AVAILABLE_QTY   TYPE BAPIWMDVE-COM_QTY.
    DATA LF_RECORDS TYPE CHAR5 .

    DATA: LF_REQD_QTY       TYPE BAPI_MRP_IND_LINES-REC_REQD_QTY,
          LF_ORDER_STCK     TYPE BAPI_MRP_IND_LINES-REC_REQD_QTY,
          LF_CONFIRM_SO     TYPE BAPI_MRP_IND_LINES-REC_REQD_QTY,
          LF_DELIVERY_STCK  TYPE BAPI_MRP_IND_LINES-REC_REQD_QTY,
          LF_PUR_ORDER_STCK TYPE BAPI_MRP_IND_LINES-REC_REQD_QTY,
          LF_RESERVED_STCK  TYPE BAPI_MRP_IND_LINES-REC_REQD_QTY,
          LF_FREE_STOCK     TYPE BAPI_MRP_IND_LINES-REC_REQD_QTY.


    CLEAR: ET_RESULT, EF_MESSAGE.
    SELECT MARC~MATNR , MARC~WERKS , MAKT~MAKTX, MARA~MEINS
      FROM MARA
     INNER JOIN MARC ON MARA~MATNR = MARC~MATNR
     INNER JOIN MAKT ON MAKT~MATNR = MARA~MATNR
      INTO TABLE @DATA(LT_MARC)
     WHERE MARA~MATNR IN @IT_PARTGRP_MATNR
       AND MARC~WERKS IN @IT_WERKS
       AND MARC~LVORM = ''
       AND MAKT~SPRAS = @SY-LANGU.

    IF SY-SUBRC <> 0.
      "Text-T01: Data doesn't exist.
      EF_MESSAGE = TEXT-T01 .
      RETURN.
    ELSE.
      SORT LT_MARC BY MATNR WERKS .
    ENDIF.

    IF LT_MARC[] IS NOT INITIAL .
      SELECT MARD~MATNR , MARD~WERKS, MARD~LGORT , "#EC CI_NO_TRANSFORM
             MARD~LGPBE , MARD~UMLME
        FROM MARD
        INTO TABLE @DATA(LT_MARD)
         FOR ALL ENTRIES IN @LT_MARC
       WHERE MATNR = @LT_MARC-MATNR
         AND WERKS = @LT_MARC-WERKS.
      IF SY-SUBRC EQ 0 .
        SORT LT_MARD BY MATNR WERKS LGORT .
      ENDIF.

      SELECT A004~MATNR, KONP~KBETR, KONP~KONWA    "#EC CI_NO_TRANSFORM
        FROM A004 INNER JOIN KONP ON KONP~KNUMH = A004~KNUMH "#EC CI_BUFFJOIN
                                 AND KONP~KAPPL = A004~KAPPL
                                 AND KONP~KSCHL = A004~KSCHL
         FOR ALL ENTRIES IN @LT_MARC
       WHERE A004~MATNR = @LT_MARC-MATNR
         AND A004~KAPPL IN @IT_KAPPL
         AND A004~KSCHL IN @IT_KSCHL
         AND A004~VKORG IN @IT_VKORG
         AND A004~VTWEG IN @IT_VTWEG
         AND A004~DATBI = '99991231'
         INTO TABLE @DATA(LT_KONP).                     "#EC CI_NOORDER
      IF SY-SUBRC = 0 .
        SORT LT_KONP BY MATNR .
      ENDIF.

      SELECT MATNR, BWKEY, BWTAR, VERPR            "#EC CI_NO_TRANSFORM
        FROM MBEW
         FOR ALL ENTRIES IN @LT_MARC
       WHERE MBEW~MATNR = @LT_MARC-MATNR
         AND MBEW~BWKEY IN @IT_WERKS
         AND MBEW~BWTAR = ''
        INTO TABLE @DATA(LT_MBEW).
      IF SY-SUBRC EQ 0 .
        SORT LT_MBEW BY MATNR BWKEY BWTAR .
      ENDIF.


    ENDIF.

    DATA: LF_MATNR         TYPE BAPI_MRP_MAT_PARAM-MATERIAL,
          LF_WERKS         TYPE BAPI_MRP_MAT_PARAM-PLANT,
          LS_MRP_STOCK_DET TYPE BAPI_MRP_STOCK_DETAIL  ##NEEDED,
          LS_RETURN        TYPE BAPIRET2,
          LT_MRP_IND       TYPE TABLE OF BAPI_MRP_IND_LINES.

    LOOP AT LT_MARC ASSIGNING FIELD-SYMBOL(<L_MARC>).

      LF_MATNR = <L_MARC>-MATNR.
      LF_WERKS = <L_MARC>-WERKS.

      "Get Free stock
      CALL FUNCTION 'BAPI_MATERIAL_STOCK_REQ_LIST'
        EXPORTING
          MATERIAL         = LF_MATNR
          PLANT            = LF_WERKS
        IMPORTING
          MRP_STOCK_DETAIL = LS_MRP_STOCK_DET
          RETURN           = LS_RETURN
        TABLES
          MRP_IND_LINES    = LT_MRP_IND.

      IF LS_RETURN-TYPE EQ 'S'.
        DELETE LT_MRP_IND WHERE PLNGSEGMT NE '02' AND
                                PLNGSEGMT NE '20'.
      ENDIF.

      CLEAR : LRT_MATNR[] , LRT_WERKS[] .
      LRT_MATNR = VALUE #( ( SIGN   = 'I'
                            OPTION = 'EQ'
                            LOW    = <L_MARC>-MATNR ) ).
      LRT_WERKS = VALUE #( ( SIGN   = 'I'
                            OPTION = 'EQ'
                            LOW    = <L_MARC>-WERKS ) ).

      LF_STOP_FLG = 'X'.

      "export memory in program RMMMBESTN
      EXPORT
        STOP_FLG = LF_STOP_FLG  TO MEMORY ID 'ZSTOP_FLG'.

      SUBMIT RMMMBESTN WITH MS_MATNR  IN LRT_MATNR
                       WITH MS_WERKS  IN LRT_WERKS
                       WITH VERNU     EQ '01'
                       AND RETURN.                       "#EC CI_SUBMIT

      CLEAR: LT_OUTTAB[].

      IMPORT GT_OUTTAB = LT_OUTTAB FROM MEMORY ID 'ZGT_OUTTAB'.

      LT_OUTTAB_L[] = LT_OUTTAB[].
      DELETE LT_OUTTAB_L WHERE LGORT IS INITIAL.
      DELETE ADJACENT DUPLICATES FROM LT_OUTTAB_L COMPARING LGORT.

      "Set data
      LOOP AT LT_OUTTAB_L ASSIGNING FIELD-SYMBOL(<L_OUTTAB>). "#EC CI_NESTED
        CLEAR: LS_STOCK.
        LS_STOCK-SLOC                = <L_OUTTAB>-LGORT .
        LS_STOCK-SLOC_DESC           = <L_OUTTAB>-LGOBE .
        LS_STOCK-MATERIAL_NO         = <L_MARC>-MATNR .
        LS_STOCK-MATERIAL_DESC       = <L_MARC>-MAKTX .
        LS_STOCK-UNRESTRICTED_USE    = <L_OUTTAB>-LABST .
        LS_STOCK-QUALITY_INSPECTION  = <L_OUTTAB>-INSME .
*        ls_STOCK-REQ_QTY            = <L_OUTTAB>-BDMNG .
*        LS_STOCK-OPEN_QTY_STOCK      = <L_OUTTAB>-OMENG .
        LS_STOCK-BLOCK_STK_RETURN    = <L_OUTTAB>-RETME .
        LS_STOCK-STK_IN_TRANSIT      = <L_OUTTAB>-TRAME .
        LS_STOCK-BLOCK_STK_GR        = <L_OUTTAB>-WESPB .
*        LS_STOCK-SALES_ORDERS        = <L_OUTTAB>-VBMNC .
        LS_STOCK-TIED_EMPTIES_STK    = <L_OUTTAB>-GLGMG .
        LS_STOCK-ON_ORDER_STK        = <L_OUTTAB>-MENGE .

        READ TABLE LT_MARD INTO DATA(LS_MARD)
                           WITH KEY MATNR = <L_MARC>-MATNR
                                    WERKS = <L_MARC>-WERKS
                                    LGORT = <L_OUTTAB>-LGORT
                           BINARY SEARCH.
        IF SY-SUBRC = 0 .
          LS_STOCK-STORAGE_BIN   =  LS_MARD-LGPBE .
          LS_STOCK-STK_IN_PP     =  LS_MARD-UMLME .
        ENDIF.

        READ TABLE LT_KONP INTO DATA(LS_KONP)
                           WITH KEY MATNR = <L_MARC>-MATNR
                           BINARY SEARCH .
        IF SY-SUBRC = 0 .
          LS_STOCK-RATE_COND_AMT   =  LS_KONP-KBETR .
        ENDIF.

        CLEAR: LF_REQD_QTY, LF_ORDER_STCK, LF_CONFIRM_SO,
               LF_DELIVERY_STCK, LF_PUR_ORDER_STCK,
               LF_RESERVED_STCK, LF_FREE_STOCK .

        DELETE LT_MRP_IND WHERE STORAGE_LOC IS INITIAL .
        LOOP AT LT_MRP_IND ASSIGNING FIELD-SYMBOL(<L_MRP_IND>) "#EC CI_NESTED.
                                            WHERE STORAGE_LOC EQ LS_STOCK-SLOC .

          LF_REQD_QTY = <L_MRP_IND>-REC_REQD_QTY.
          IF <L_MRP_IND>-PLUS_MINUS EQ '-'.
            LF_REQD_QTY = LF_REQD_QTY * -1.
          ENDIF.

          CASE <L_MRP_IND>-MRP_ELEMENT_IND.
            WHEN 'VC'.
              LF_ORDER_STCK  = LF_ORDER_STCK + LF_REQD_QTY.
              LF_CONFIRM_SO  = LF_CONFIRM_SO + LF_REQD_QTY.
            WHEN 'VJ'.
              LF_DELIVERY_STCK = LF_DELIVERY_STCK + LF_REQD_QTY.
            WHEN 'BE' OR 'LA'.
              LF_PUR_ORDER_STCK  = LF_PUR_ORDER_STCK + LF_REQD_QTY.
            WHEN 'MR'.
              LF_RESERVED_STCK = LF_RESERVED_STCK + LF_REQD_QTY.
          ENDCASE.
        ENDLOOP.

        LF_FREE_STOCK = LS_STOCK-UNRESTRICTED_USE
                         - LF_RESERVED_STCK
                         - LF_CONFIRM_SO
                         - LF_DELIVERY_STCK.

        LS_STOCK-FREE_STK_QTY   = LF_FREE_STOCK .
        LS_STOCK-SALES_ORDERS   = LF_CONFIRM_SO.
        LS_STOCK-OPEN_QTY_STOCK = LF_DELIVERY_STCK.
        LS_STOCK-REQ_QTY        = LF_RESERVED_STCK.

        READ TABLE LT_MBEW INTO DATA(LS_MBEW)
        WITH KEY MATNR =  <L_MARC>-MATNR
                 BWKEY =  <L_MARC>-WERKS
                 BINARY SEARCH .
        IF SY-SUBRC = 0 .
          LS_STOCK-MOVING_AVG_PUP = LS_MBEW-VERPR .
        ENDIF.

        APPEND LS_STOCK TO ET_RESULT.

      ENDLOOP.

    ENDLOOP."#CI_NESTED

    IF  ET_RESULT[] IS INITIAL.
      "Text-T04: Stock= 0.
      EF_MESSAGE = TEXT-T04 .
    ELSE.
      SORT ET_RESULT BY MATERIAL_NO SLOC STORAGE_BIN.

      LF_RECORDS = LINES( ET_RESULT ).
      CONDENSE LF_RECORDS NO-GAPS.
      "Text-t02 :  Found data , Text-t03 : Records.
      CONCATENATE TEXT-T02 LF_RECORDS TEXT-T03
             INTO EF_MESSAGE SEPARATED BY SPACE.


    ENDIF.
  ENDMETHOD.


  METHOD PROCESS_DATA.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSMM_AVAILABLE_STOCK_SERV / PROCESS_DATA
*  Creation Date      : 28.05.2024
*  Author             : Jutamas Y. (Eviden)
*  Add-on ID          : MMI005
*  Description        : Get available stock
*  Purpose            : Inbound interface from salesforce system to
*                       get available stock for spare parts
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
    DATA: LS_REQUEST TYPE ZSDSMMS032.

    DATA: LF_PARTIAL_ERROR TYPE FLAG.

*    FIELD-SYMBOLS: <L_RESPONSE> TYPE ZSDSMMS026.
    FIELD-SYMBOLS: <L_RESPONSE> TYPE ZSDSMMS032.

* Initialize Output
    CLEAR: EF_STATUS,
           EF_MESSAGE,
           EF_HTTP_ERROR.

    LS_REQUEST = IREF_REQUEST_DATA->*.

    ASSIGN EREF_RESPONSE_DATA->* TO <L_RESPONSE>.
    IF SY-SUBRC NE 0.
*   Critical error
      RETURN.
    ENDIF.

    <L_RESPONSE> = LS_REQUEST.

    TRANSLATE LS_REQUEST-MATERIAL_NO TO UPPER CASE .

    CALL METHOD ZCL_SDSMM_AVAILABLE_STOCK_SERV=>AVAILABLE_STOCK_PROCESS
      EXPORTING
        IF_MATNR    = LS_REQUEST-MATERIAL_NO
      IMPORTING
        ES_RESPONSE = <L_RESPONSE>.


* If not partial error, error for whole service
    IF LF_PARTIAL_ERROR IS INITIAL AND
       EF_STATUS EQ ZCL_SDSCA_REST_INTF_UTILITY=>GC_ERROR.
      EF_HTTP_ERROR = 'X'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
