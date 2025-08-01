*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS LCL_DATA DEFINITION.
  PUBLIC SECTION.
    CONSTANTS : BEGIN OF GC_CON,
                  E  TYPE C LENGTH 1 VALUE 'E',
                  S  TYPE C LENGTH 1 VALUE 'S',
                  I  TYPE C LENGTH 1 VALUE 'I',
                  EQ TYPE C LENGTH 2 VALUE 'EQ',
                END OF GC_CON.

    TYPES : GY_ZSDSVC_CHKSTK_V  TYPE TABLE OF ZSDSVC_SUM_STOCK WITH EMPTY KEY.
    TYPES : GY_ZSDSVC_CHKSTK_SP TYPE TABLE OF ZSDSVC_SUM_STOCK_SP WITH EMPTY KEY.
    TYPES : GY_DETAIL           TYPE TABLE OF ZSDSVC_DETAIL_STOCK WITH EMPTY KEY.
    TYPES : GTY_MRP_IND         TYPE TABLE OF BAPI_MRP_IND_LINES WITH EMPTY KEY.

    TYPES : BEGIN OF GY_STOCK_REQ,
              MRP_STOCK_DET TYPE BAPI_MRP_STOCK_DETAIL,
              MRP_IND       TYPE GTY_MRP_IND,
            END OF GY_STOCK_REQ.

    METHODS :
      CONSTRUCTOR.

    CLASS-METHODS :
      GET_BOM IMPORTING I_DATA  TYPE WERKS_D
              CHANGING  CT_DATA TYPE ZSDSMMS050_TT,
      GET_STOCK_INFO IMPORTING I_WERKS  TYPE WERKS_D
                               I_VTWEG  TYPE VTWEG
                               I_DATA   TYPE ZSDSMMS056
                     RETURNING VALUE(R) TYPE GY_ZSDSVC_CHKSTK_V,
      GET_STOCK_INFO_SP IMPORTING I_WERKS  TYPE WERKS_D
                                  I_VTWEG  TYPE VTWEG
                                  I_DATA   TYPE ZSDSMMS056
                        RETURNING VALUE(R) TYPE GY_ZSDSVC_CHKSTK_SP,
      GEN_DATA_AVAILABLE_STOCK IMPORTING I_DATA   TYPE GY_ZSDSVC_CHKSTK_V
                                         I_WERKS  TYPE WERKS_D
                               RETURNING VALUE(R) TYPE ZSDSMMS055_TT,
      GEN_DATA_AVAILABLE_STOCK_SP IMPORTING I_DATA      TYPE GY_ZSDSVC_CHKSTK_SP
                                            I_WERKS     TYPE WERKS_D
                                            I_SELECTION TYPE ZSDSMMS056
                                  RETURNING VALUE(R)    TYPE ZSDSMMS055_TT,
      GET_STOCL_REQ IMPORTING I_MAT    TYPE MARA-MATNR
                              I_PAT    TYPE WERKS_D
                    RETURNING VALUE(R) TYPE GY_STOCK_REQ,
      GET_STOCK_INFO_DETAIL IMPORTING I_WERKS  TYPE WERKS_D
                                      I_VTWEG  TYPE VTWEG
                                      I_DATA   TYPE ZSDSMMS049
                            RETURNING VALUE(R) TYPE GY_DETAIL,
      GET_STOCK_INFO_DETAIL_SP IMPORTING I_WERKS  TYPE WERKS_D
                                         I_VTWEG  TYPE VTWEG
                                         I_DATA   TYPE ZSDSMMS049
                               RETURNING VALUE(R) TYPE GY_DETAIL,
      GEN_DATA_AVAILABLE_STOCK_D IMPORTING I_DATA   TYPE GY_DETAIL
                                           I_WERKS  TYPE WERKS_D
                                 RETURNING VALUE(R) TYPE ZSDSMMS055_TT,
      GEN_DATA_AVAILABLE_STOCK_D_SP IMPORTING I_DATA   TYPE GY_DETAIL
                                              I_WERKS  TYPE WERKS_D
                                    RETURNING VALUE(R) TYPE ZSDSMMS055_TT,
      GET_GROUP_PART CHANGING CT_DATA TYPE ZSDSMMS050_TT.
ENDCLASS.
CLASS LCL_DATA IMPLEMENTATION.
  METHOD CONSTRUCTOR.

  ENDMETHOD.
  METHOD GET_BOM.
    DATA : LV_MATERIAL  TYPE RC29L-MATNR,
           LV_PLANT     TYPE RC29L-WERKS,
           LV_BOM_USAGE TYPE RC29L-STLAN.

    DATA : LS_FL_WARNING TYPE	CAPIFLAG-FLWARNING,
           LS_RETURN     TYPE BAPIRET2.

    DATA : LT_BOM_HEADER                  TYPE TABLE OF STKO_API02,
           LT_BOM_ITEM                    TYPE TABLE OF STPO_API02,
           LT_DOCUMENTDATA                TYPE TABLE OF DMU_DOCUMENT,
           LT_DOCUMENTFILES	              TYPE TABLE OF DMU_FILE,
           LT_DMUPOSITIONOBJECTS          TYPE TABLE OF DMU_POSOBJ,
           LT_DMUMATRICES	                TYPE TABLE OF DMU_MATRIX,
           LT_MULTIPLE_APPLICATION_FILTER	TYPE TABLE OF BAPI_DOC_APPLICATIONS.

    DATA : LS_BOM_ITEM TYPE STPO_API02.

    DATA : LV_LINE TYPE I.

    DATA : LS_DATA LIKE LINE OF CT_DATA,
           LT_TMP  LIKE CT_DATA.

    DATA : LV_TABIX TYPE SY-TABIX.

    LOOP AT CT_DATA INTO LS_DATA.
      LV_TABIX     = SY-TABIX.
      LV_MATERIAL  = LS_DATA-LOW.
      LV_PLANT     = I_DATA.
      LV_BOM_USAGE = '5'.

      CALL FUNCTION 'DMU_MAT_BOM_READ'
        EXPORTING
          MATERIAL                    = LV_MATERIAL
          PLANT                       = LV_PLANT
          BOM_USAGE                   = LV_BOM_USAGE
          GET_SINGLE_FILE             = ABAP_TRUE
        IMPORTING
          FL_WARNING                  = LS_FL_WARNING
          RETURN                      = LS_RETURN
        TABLES
          BOM_HEADER                  = LT_BOM_HEADER
          BOM_ITEM                    = LT_BOM_ITEM
          DOCUMENTDATA                = LT_DOCUMENTDATA
          DOCUMENTFILES               = LT_DOCUMENTFILES
          DMUPOSITIONOBJECTS          = LT_DMUPOSITIONOBJECTS
          DMUMATRICES                 = LT_DMUMATRICES
          MULTIPLE_APPLICATION_FILTER = LT_MULTIPLE_APPLICATION_FILTER.

      IF LS_RETURN-TYPE   = ZCL_SDSMM_AVAILABLE_STOCK_NEW=>GC_CON-E  AND
         LS_RETURN-ID     = 'M3'      AND
         LS_RETURN-NUMBER = '305'.
*      MESSAGE e001 WITH ls_return-message.
      ELSEIF LS_RETURN IS INITIAL.
        DELETE CT_DATA[] INDEX LV_TABIX.
        LOOP AT LT_BOM_ITEM INTO LS_BOM_ITEM.
          CLEAR LS_DATA.
          LS_DATA-SIGN   = GC_CON-I.
          LS_DATA-OPTION = GC_CON-EQ.
          LS_DATA-LOW    = LS_BOM_ITEM-COMPONENT.
          APPEND LS_DATA TO LT_TMP.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    IF LT_TMP IS NOT INITIAL.
      APPEND LINES OF LT_TMP TO CT_DATA.
      SORT CT_DATA.
      DELETE ADJACENT DUPLICATES FROM CT_DATA.
    ENDIF.

  ENDMETHOD.
  METHOD GET_STOCK_INFO.
    SELECT *
      FROM ZSDSVC_SUM_STOCK( P_PLANT = @I_WERKS , P_CHANL = @I_VTWEG )
      WHERE MATNR IN @I_DATA-MATNR[]
        AND MTART EQ 'ZFG'
*        AND LGORT IN @I_DATA-LGORT[]
        AND PH1   IN @I_DATA-PH1[]
        AND PH2   IN @I_DATA-PH2[]
        AND PH3   IN @I_DATA-PH3[]
      INTO TABLE @R.
  ENDMETHOD.
  METHOD GET_STOCK_INFO_SP.
    SELECT *
      FROM ZSDSVC_SUM_STOCK_SP( P_PLANT = @I_WERKS , P_CHANL = @I_VTWEG )
      WHERE MATNR IN @I_DATA-MATNR[]
        AND ( MTART EQ 'ZSP' OR
              MTART EQ 'UNBW' )
*        AND LGORT IN @I_DATA-LGORT[]
        AND PH1   IN @I_DATA-PH1[]
        AND PH2   IN @I_DATA-PH2[]
        AND PH3   IN @I_DATA-PH3[]
      INTO TABLE @R.
  ENDMETHOD.
  METHOD GEN_DATA_AVAILABLE_STOCK.

    DATA : LS_STOCK_REQ TYPE GY_STOCK_REQ.

    DATA : LS_R LIKE LINE OF R.

    LOOP AT I_DATA INTO DATA(LS_DATA).
      LS_STOCK_REQ = GET_STOCL_REQ( I_MAT = LS_DATA-MATNR
                                    I_PAT = I_WERKS ).

      LS_R-MATNR         = LS_DATA-MATNR.
      LS_R-MAKTX         = LS_DATA-MAKTX.
      LS_R-ZZCAV         = LS_DATA-ZZCAV.
      LS_R-PH1           = LS_DATA-PH1.
      LS_R-PH2           = LS_DATA-PH2.
      LS_R-PH3           = LS_DATA-PH3.
      LS_R-KBETR         = LS_DATA-KBETR.
      LS_R-VERPR         = LS_DATA-VERPR.
      LS_R-STPRS         = LS_DATA-STPRS.
      LS_R-UR_QTY        = LS_DATA-LABST.
      LS_R-SO_QTY        = LS_STOCK_REQ-MRP_STOCK_DET-ORDERS.
      LS_R-DO_QTY        = LS_STOCK_REQ-MRP_STOCK_DET-DELIVERY.
      LS_R-PO_QTY        = LS_STOCK_REQ-MRP_STOCK_DET-PUR_ORDERS.
      LS_R-QT_QTY        = LS_STOCK_REQ-MRP_STOCK_DET-SALES_REQS.
      LS_R-PR_QTY        = LS_STOCK_REQ-MRP_STOCK_DET-PUR_REQ.
      LS_R-BLOCK_QTY     = LS_DATA-SPEME.
      LS_R-TRANSFERT_QTY = LS_DATA-UMLME.

      LS_R-QTY           = ( LS_DATA-LABST ) -
                           ( LS_R-DO_QTY + LS_R-SO_QTY ).

      APPEND LS_R TO R.
      CLEAR : LS_R.
    ENDLOOP.

  ENDMETHOD.
  METHOD GEN_DATA_AVAILABLE_STOCK_SP.

    DATA : LS_STOCK_REQ TYPE GY_STOCK_REQ.

    DATA : LS_R LIKE LINE OF R.

    DATA : LS_MRP_IND LIKE LINE OF LS_STOCK_REQ-MRP_IND.

    DATA : LV_TABIX TYPE SY-TABIX.

    DATA : LR_TYPE LIKE RANGE OF LS_MRP_IND-MRP_ELEMENT_IND.

    DATA : LS_DATA LIKE LINE OF I_DATA.

    DATA : LV_SO  TYPE I,
           LV_DO  TYPE I,
           LV_RES TYPE I.

    LR_TYPE =  VALUE #( ( SIGN  = 'I' OPTION = 'EQ' LOW = 'VC' ) "SO Order
                        ( SIGN  = 'I' OPTION = 'EQ' LOW = 'VJ' ) "Delivery Order
                        ( SIGN  = 'I' OPTION = 'EQ' LOW = 'BE' ) "Purchase Order
                        ( SIGN  = 'I' OPTION = 'EQ' LOW = 'LA' ) "Purchase Order
                        ( SIGN  = 'I' OPTION = 'EQ' LOW = 'MR' ) "Reserve
                       ).

    DATA(LT_TMP) = I_DATA.

    SORT LT_TMP BY MATNR.

    LOOP AT LT_TMP INTO DATA(LS_TMP).
      MOVE-CORRESPONDING LS_TMP TO LS_DATA.
      AT NEW MATNR.
        LS_STOCK_REQ = GET_STOCL_REQ( I_MAT = LS_DATA-MATNR
                                      I_PAT = I_WERKS ).
      ENDAT.

      LS_R-MATNR         = LS_DATA-MATNR.
      LS_R-MAKTX         = LS_DATA-MAKTX.
      LS_R-ZZCAV         = LS_DATA-ZZCAV.
      LS_R-PH1           = LS_DATA-PH1.
      LS_R-PH2           = LS_DATA-PH2.
      LS_R-PH3           = LS_DATA-PH3.
      LS_R-LGORT         = LS_DATA-LGORT.
      LS_R-LGOBE         = LS_DATA-LGOBE.
      LS_R-KBETR         = LS_DATA-KBETR.
      LS_R-VERPR         = LS_DATA-VERPR.
      LS_R-STPRS         = LS_DATA-STPRS.
      LS_R-UR_QTY        = LS_DATA-LABST.

      LOOP AT LS_STOCK_REQ-MRP_IND INTO LS_MRP_IND WHERE STORAGE_LOC     EQ LS_R-LGORT
                                                     AND MRP_ELEMENT_IND IN LR_TYPE[].
        LV_TABIX   = SY-TABIX.

        LS_MRP_IND-REC_REQD_QTY = ABS( LS_MRP_IND-REC_REQD_QTY ).
        IF     LS_MRP_IND-MRP_ELEMENT_IND EQ 'VC'.
          LS_R-SO_QTY  = LS_R-SO_QTY + LS_MRP_IND-REC_REQD_QTY.
        ELSEIF LS_MRP_IND-MRP_ELEMENT_IND EQ 'VJ'.
          LS_R-DO_QTY  = LS_R-DO_QTY + LS_MRP_IND-REC_REQD_QTY.
        ELSEIF LS_MRP_IND-MRP_ELEMENT_IND EQ 'MR'.
          LS_R-RES_QTY = LS_R-RES_QTY + LS_MRP_IND-REC_REQD_QTY.
        ENDIF.
        DELETE LS_STOCK_REQ-MRP_IND INDEX LV_TABIX.
      ENDLOOP.

      IF LS_R-LGORT  EQ 'B100'.
        LS_R-PO_QTY        = LS_STOCK_REQ-MRP_STOCK_DET-PUR_ORDERS.
        LS_R-QT_QTY        = LS_STOCK_REQ-MRP_STOCK_DET-SALES_REQS.
        LS_R-PR_QTY        = LS_STOCK_REQ-MRP_STOCK_DET-PUR_REQ.
*        LS_R-RES_QTY       = LS_STOCK_REQ-MRP_STOCK_DET-RESERVATIONS.
        LS_R-BLOCK_QTY     = LS_DATA-SPEME.
        LS_R-TRANSFERT_QTY = LS_DATA-UMLME.
      ENDIF.

      LS_R-QTY           = ( LS_DATA-LABST ) -
                           ( LS_R-DO_QTY + LS_R-SO_QTY + LS_R-RES_QTY ).
      APPEND LS_R TO R.

      AT END OF MATNR.
        CLEAR : LV_SO,LV_DO.
        LOOP AT LS_STOCK_REQ-MRP_IND INTO LS_MRP_IND WHERE MRP_ELEMENT_IND IN LR_TYPE[].

          LS_MRP_IND-REC_REQD_QTY = ABS( LS_MRP_IND-REC_REQD_QTY ).

          IF     LS_MRP_IND-MRP_ELEMENT_IND EQ 'VC'.
            ADD LS_MRP_IND-REC_REQD_QTY TO LV_SO.
          ELSEIF LS_MRP_IND-MRP_ELEMENT_IND EQ 'VJ'.
            ADD LS_MRP_IND-REC_REQD_QTY TO LV_DO.
          ELSEIF LS_MRP_IND-MRP_ELEMENT_IND EQ 'MR'.
            ADD LS_MRP_IND-REC_REQD_QTY TO LV_RES.
          ENDIF.
        ENDLOOP.

        READ TABLE R ASSIGNING FIELD-SYMBOL(<LFS_DATA>)
        WITH KEY MATNR = LS_R-MATNR
                 LGORT = 'B100'.
        IF SY-SUBRC EQ 0.
          <LFS_DATA>-SO_QTY  = <LFS_DATA>-SO_QTY  + LV_SO.
          <LFS_DATA>-DO_QTY  = <LFS_DATA>-DO_QTY  + LV_DO.
          <LFS_DATA>-RES_QTY = <LFS_DATA>-RES_QTY + LV_RES.
          <LFS_DATA>-QTY     = ( <LFS_DATA>-UR_QTY ) -
                               ( <LFS_DATA>-DO_QTY + <LFS_DATA>-SO_QTY + <LFS_DATA>-RES_QTY ).
        ENDIF.
      ENDAT.

      CLEAR : LS_R.
    ENDLOOP.


    SELECT MARA~MATNR,
           MAKT~MAKTX,
           MARA~ZZCAV,
           SUBSTRING( MARA~PRDHA, 1, 5 )   AS PH1,
           SUBSTRING( MARA~PRDHA, 6, 5 )   AS PH2,
           SUBSTRING( MARA~PRDHA, 11, 8 )  AS PH3,
           MBEW~VERPR,
           MBEW~STPRS
      FROM MARA
      INNER JOIN MAKT ON MARA~MATNR EQ MAKT~MATNR AND
                         MAKT~SPRAS EQ @SY-LANGU
      LEFT JOIN MBEW  ON MARA~MATNR EQ MBEW~MATNR AND
                         MBEW~BWKEY EQ '1000' AND
                         MBEW~BWTAR EQ ''
      WHERE MARA~MATNR IN @I_SELECTION-MATNR[]
      INTO TABLE @DATA(LT_MARA).

    LOOP AT LT_MARA INTO DATA(LS_MARA).
      READ TABLE R INTO LS_R
      WITH KEY MATNR = LS_MARA-MATNR.
      IF SY-SUBRC NE 0.
        CLEAR : LS_R.
        LS_R-MATNR         = LS_MARA-MATNR.
        LS_R-MAKTX         = LS_MARA-MAKTX.
        LS_R-ZZCAV         = LS_MARA-ZZCAV.
        LS_R-PH1           = LS_MARA-PH1.
        LS_R-PH2           = LS_MARA-PH2.
        LS_R-PH3           = LS_MARA-PH3.
        LS_R-VERPR         = LS_MARA-VERPR.
        LS_R-STPRS         = LS_MARA-STPRS.
        APPEND LS_R TO R.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD GET_STOCL_REQ.
    DATA: LT_MRP_IND       TYPE STANDARD TABLE OF BAPI_MRP_IND_LINES,
          LS_MRP_STOCK_DET TYPE BAPI_MRP_STOCK_DETAIL,
          LS_RETURN        TYPE BAPIRET2,
          LV_MATERIAL      TYPE BAPI_MRP_MAT_PARAM-MATERIAL,
          LV_PLANT         TYPE BAPI_MRP_MAT_PARAM-PLANT,
          LV_MATERIAL_LONG TYPE BAPI_MRP_MAT_PARAM-MATERIAL_LONG.

    DATA : LV_LEN TYPE I.

    LV_LEN = STRLEN( I_MAT ).

    IF LV_LEN GT 18.
      LV_MATERIAL_LONG   = I_MAT.
    ELSE.
      LV_MATERIAL        = I_MAT.
    ENDIF.
    LV_PLANT             = I_PAT.

    CALL FUNCTION 'BAPI_MATERIAL_STOCK_REQ_LIST'
      EXPORTING
        MATERIAL         = LV_MATERIAL
        PLANT            = LV_PLANT
        MATERIAL_LONG    = LV_MATERIAL_LONG
      IMPORTING
        MRP_STOCK_DETAIL = LS_MRP_STOCK_DET
        RETURN           = LS_RETURN
      TABLES
        MRP_IND_LINES    = LT_MRP_IND.

    IF LS_RETURN-TYPE EQ GC_CON-S.
      R-MRP_STOCK_DET = LS_MRP_STOCK_DET.
      R-MRP_IND       = LT_MRP_IND.
      DELETE R-MRP_IND WHERE PLNGSEGMT NE '02' AND
                             PLNGSEGMT NE '20'.
    ENDIF.
  ENDMETHOD.
  METHOD GET_STOCK_INFO_DETAIL.
    SELECT *
      FROM ZSDSVC_DETAIL_STOCK( P_PLANT = @I_WERKS , P_CHANL = @I_VTWEG )
      WHERE MATNR IN @I_DATA-MATNR[]
        AND MTART EQ 'ZFG'
        AND LGORT IN @I_DATA-LGORT[]
        AND PH1   IN @I_DATA-PH1[]
        AND PH2   IN @I_DATA-PH2[]
        AND PH3   IN @I_DATA-PH3[]
      INTO TABLE @R.
  ENDMETHOD.
  METHOD GET_STOCK_INFO_DETAIL_SP.
    SELECT *
      FROM ZSDSVC_DETAIL_STOCK_SP( P_PLANT = @I_WERKS , P_CHANL = @I_VTWEG )
      WHERE MATNR IN @I_DATA-MATNR[]
        AND ( MTART EQ 'ZSP' OR
              MTART EQ 'UNBW' )
        AND LGORT IN @I_DATA-LGORT[]
        AND PH1   IN @I_DATA-PH1[]
        AND PH2   IN @I_DATA-PH2[]
        AND PH3   IN @I_DATA-PH3[]
      INTO TABLE @R.
  ENDMETHOD.
  METHOD GEN_DATA_AVAILABLE_STOCK_D.
    DATA : LS_STOCK_REQ TYPE GY_STOCK_REQ.

    DATA : LS_R LIKE LINE OF R.

    DATA : LS_DATA LIKE LINE OF I_DATA.

    DATA : LS_MRP_IND LIKE LINE OF LS_STOCK_REQ-MRP_IND.

    DATA : LV_TABIX TYPE SY-TABIX.

    DATA : LR_TYPE LIKE RANGE OF LS_MRP_IND-MRP_ELEMENT_IND.

    LR_TYPE =  VALUE #( ( SIGN  = 'I' OPTION = 'EQ' LOW = 'VC' ) "SO Order
                        ( SIGN  = 'I' OPTION = 'EQ' LOW = 'VJ' ) "Delivery Order
                        ( SIGN  = 'I' OPTION = 'EQ' LOW = 'BE' ) "Purchase Order
                        ( SIGN  = 'I' OPTION = 'EQ' LOW = 'LA' ) "Purchase Order
                       ).

    LOOP AT I_DATA INTO DATA(LS_TMP).
      MOVE-CORRESPONDING LS_TMP TO LS_DATA.

      AT NEW MATNR.
        LS_STOCK_REQ = GET_STOCL_REQ( I_MAT = LS_DATA-MATNR
                                      I_PAT = I_WERKS ).
      ENDAT.

      LS_R-MATNR         = LS_DATA-MATNR.
      LS_R-MAKTX         = LS_DATA-MAKTX.
      LS_R-ZZCAV         = LS_DATA-ZZCAV.
      LS_R-PH1           = LS_DATA-PH1.
      LS_R-PH2           = LS_DATA-PH2.
      LS_R-PH3           = LS_DATA-PH3.
      LS_R-KBETR         = LS_DATA-KBETR.
      LS_R-VERPR         = LS_DATA-VERPR.
      LS_R-STPRS         = LS_DATA-STPRS.
      LS_R-QTY           = LS_DATA-LABST.
      LS_R-LGORT         = LS_DATA-LGORT.
      LS_R-LGOBE         = LS_DATA-LGOBE.
      LS_R-BLOCK_QTY     = LS_DATA-SPEME.
      LS_R-TRANSFERT_QTY = LS_DATA-UMLME.
      LS_R-TYPE          = 'UrStrock'.

      APPEND LS_R TO R.

      LOOP AT LS_STOCK_REQ-MRP_IND INTO LS_MRP_IND WHERE STORAGE_LOC     EQ LS_R-LGORT
                                                     AND MRP_ELEMENT_IND IN LR_TYPE[].
        LV_TABIX   = SY-TABIX.
        LS_R-TYPE  = LS_MRP_IND-MRP_ELEMNT.
        WRITE LS_MRP_IND-FINISH_DATE TO LS_R-DATEI.
        LS_R-DOCNO = LS_MRP_IND-ELEMNT_DATA.
        LS_R-QTY   = LS_MRP_IND-REC_REQD_QTY.
        APPEND LS_R TO R.
        DELETE LS_STOCK_REQ-MRP_IND INDEX LV_TABIX.
      ENDLOOP.

      AT END OF MATNR.
        LOOP AT LS_STOCK_REQ-MRP_IND INTO LS_MRP_IND WHERE MRP_ELEMENT_IND IN LR_TYPE[].
          CLEAR : LS_R-LGORT,LS_R-LGOBE.
          LS_R-TYPE  = LS_MRP_IND-MRP_ELEMNT.
          WRITE LS_MRP_IND-FINISH_DATE TO LS_R-DATEI.
          LS_R-DOCNO = LS_MRP_IND-ELEMNT_DATA.
          LS_R-QTY   = LS_MRP_IND-REC_REQD_QTY.
          APPEND LS_R TO R.
        ENDLOOP.
        CLEAR : LS_STOCK_REQ-MRP_IND.
      ENDAT.

      CLEAR : LS_R.
    ENDLOOP.

  ENDMETHOD.
  METHOD GEN_DATA_AVAILABLE_STOCK_D_SP.
    DATA : LS_STOCK_REQ TYPE GY_STOCK_REQ.

    DATA : LS_R LIKE LINE OF R.

    DATA : LS_DATA LIKE LINE OF I_DATA.

    DATA : LS_MRP_IND LIKE LINE OF LS_STOCK_REQ-MRP_IND.

    DATA : LV_TABIX TYPE SY-TABIX.

    DATA : LR_TYPE LIKE RANGE OF LS_MRP_IND-MRP_ELEMENT_IND.

    LR_TYPE =  VALUE #( ( SIGN  = 'I' OPTION = 'EQ' LOW = 'VC' ) "SO Order
                        ( SIGN  = 'I' OPTION = 'EQ' LOW = 'VJ' ) "Delivery Order
                        ( SIGN  = 'I' OPTION = 'EQ' LOW = 'BE' ) "Purchase Order
                        ( SIGN  = 'I' OPTION = 'EQ' LOW = 'LA' ) "Purchase Order
                        ( SIGN  = 'I' OPTION = 'EQ' LOW = 'MR' ) "Reserve
                       ).

    LOOP AT I_DATA INTO DATA(LS_TMP).
      MOVE-CORRESPONDING LS_TMP TO LS_DATA.

      AT NEW MATNR.
        LS_STOCK_REQ = GET_STOCL_REQ( I_MAT = LS_DATA-MATNR
                                      I_PAT = I_WERKS ).
      ENDAT.

      LS_R-MATNR         = LS_DATA-MATNR.
      LS_R-MAKTX         = LS_DATA-MAKTX.
      LS_R-ZZCAV         = LS_DATA-ZZCAV.
      LS_R-PH1           = LS_DATA-PH1.
      LS_R-PH2           = LS_DATA-PH2.
      LS_R-PH3           = LS_DATA-PH3.
      LS_R-KBETR         = LS_DATA-KBETR.
      LS_R-VERPR         = LS_DATA-VERPR.
      LS_R-STPRS         = LS_DATA-STPRS.
      LS_R-QTY           = LS_DATA-LABST.
      LS_R-LGORT         = LS_DATA-LGORT.
      LS_R-LGOBE         = LS_DATA-LGOBE.
      LS_R-BLOCK_QTY     = LS_DATA-SPEME.
      LS_R-TRANSFERT_QTY = LS_DATA-UMLME.
      LS_R-TYPE          = 'UrStrock'.

      APPEND LS_R TO R.

      LOOP AT LS_STOCK_REQ-MRP_IND INTO LS_MRP_IND WHERE STORAGE_LOC     EQ LS_R-LGORT
                                                     AND MRP_ELEMENT_IND IN LR_TYPE[].
        LV_TABIX   = SY-TABIX.
        LS_R-TYPE  = LS_MRP_IND-MRP_ELEMNT.
        WRITE LS_MRP_IND-FINISH_DATE TO LS_R-DATEI.
        LS_R-DOCNO = LS_MRP_IND-ELEMNT_DATA.
        LS_R-QTY   = LS_MRP_IND-REC_REQD_QTY.
        APPEND LS_R TO R.
        DELETE LS_STOCK_REQ-MRP_IND INDEX LV_TABIX.
      ENDLOOP.

      AT END OF MATNR.
        LOOP AT LS_STOCK_REQ-MRP_IND INTO LS_MRP_IND WHERE MRP_ELEMENT_IND IN LR_TYPE[].
          CLEAR : LS_R-LGORT,LS_R-LGOBE.
          LS_R-TYPE  = LS_MRP_IND-MRP_ELEMNT.
          WRITE LS_MRP_IND-FINISH_DATE TO LS_R-DATEI.
*          LS_R-DATEI = LS_MRP_IND-FINISH_DATE.
          LS_R-DOCNO = LS_MRP_IND-ELEMNT_DATA.
          LS_R-QTY   = LS_MRP_IND-REC_REQD_QTY.
          APPEND LS_R TO R.
        ENDLOOP.
        CLEAR : LS_STOCK_REQ-MRP_IND.
      ENDAT.

      CLEAR : LS_R.
    ENDLOOP.

  ENDMETHOD.
  METHOD GET_GROUP_PART.

    DATA : LS_DATA LIKE LINE OF CT_DATA.

    SELECT A~PART_GROUP
      FROM ZRTCAC002 AS A
      WHERE A~MATNR    IN @CT_DATA
    INTO TABLE @DATA(LT_PART_GROUP).
    IF LT_PART_GROUP IS NOT INITIAL.
      SELECT B~MATNR
        FROM @LT_PART_GROUP AS A
        INNER JOIN ZRTCAC002 AS B ON A~PART_GROUP EQ B~PART_GROUP
      INTO TABLE @DATA(LT_MAT).

      LOOP AT LT_MAT INTO DATA(LS_MAT).
        CLEAR LS_DATA.
        LS_DATA-SIGN   = GC_CON-I.
        LS_DATA-OPTION = GC_CON-EQ.
        LS_DATA-LOW    = LS_MAT-MATNR.
        APPEND LS_DATA TO CT_DATA.
      ENDLOOP.
      IF SY-SUBRC EQ 0.
        SORT CT_DATA.
        DELETE ADJACENT DUPLICATES FROM CT_DATA.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
