class ZCL_ZSDSVC_CDS_VIEW_DPC_EXT definition
  public
  inheriting from ZCL_ZSDSVC_CDS_VIEW_DPC
  create public .

public section.
protected section.

  methods GETSOHEADERSET_GET_ENTITY
    redefinition .
  methods ZSDSVC_TB003_GET_ENTITYSET
    redefinition .
  methods GETDODATASET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZSDSVC_CDS_VIEW_DPC_EXT IMPLEMENTATION.


  METHOD zsdsvc_tb003_get_entityset.
    IF 1 = 2.
    ELSE.

      if_sadl_gw_dpc_util~get_dpc( )->get_entityset( EXPORTING io_tech_request_context = io_tech_request_context
                                                     IMPORTING et_data                 = et_entityset
                                                               es_response_context     = es_response_context ).

    ENDIF.
**TRY.
*CALL METHOD SUPER->ZSDSVC_TB003_GET_ENTITYSET
*  EXPORTING
*    IV_ENTITY_NAME           =
*    IV_ENTITY_SET_NAME       =
*    IV_SOURCE_NAME           =
*    IT_FILTER_SELECT_OPTIONS =
*    IS_PAGING                =
*    IT_KEY_TAB               =
*    IT_NAVIGATION_PATH       =
*    IT_ORDER                 =
*    IV_FILTER_STRING         =
*    IV_SEARCH_STRING         =
**    io_tech_request_context  =
**  IMPORTING
**    et_entityset             =
**    es_response_context      =
*    .
**  CATCH /iwbep/cx_mgw_busi_exception.
**  CATCH /iwbep/cx_mgw_tech_exception.
**ENDTRY.
  ENDMETHOD.


  METHOD GETSOHEADERSET_GET_ENTITY.

    DATA: LS_DATA TYPE ZCL_ZSDSVC_CDS_VIEW_MPC=>TS_GETSOHEADER,
          LS_HEADER TYPE ZSDSSDS129,
          LV_VBELN  TYPE VBAK-VBELN.

    READ TABLE IT_KEY_TAB INTO DATA(LS_INPUT)
                          WITH KEY NAME = 'salesOrder'.
    IF SY-SUBRC = 0 AND LS_INPUT-VALUE IS NOT INITIAL.
      LV_VBELN = |{ LS_INPUT-VALUE ALPHA = IN }|.
      CALL FUNCTION 'Z_SDSSD_GET_SO_HEADER'
        EXPORTING
          IV_VBELN  = LV_VBELN
        IMPORTING
          ES_HEADER = LS_HEADER.

      LS_DATA-ES_HEADER = LS_HEADER.
      LS_DATA-IV_VBELN  = LS_INPUT-VALUE.

      ER_ENTITY = LS_DATA.

    ENDIF.

  ENDMETHOD.


  METHOD GETDODATASET_GET_ENTITYSET.

    TYPES:
      BEGIN OF LTYP_TEXT,
        VBELN TYPE LIKP-VBELN,
        LAND  TYPE TEXT255,
      END OF LTYP_TEXT.

    DATA:
      LR_VBELN TYPE RANGE OF LIKP-VBELN,
      LT_TEXT  TYPE STANDARD TABLE OF LTYP_TEXT.

    DATA: LT_LINE     TYPE STANDARD TABLE OF TLINE.
    DATA: LS_LINE     LIKE LINE OF LT_LINE.
    DATA: LV_TDID     TYPE THEAD-TDID.
    DATA: LV_TDNAME   TYPE THEAD-TDNAME.
    DATA: LV_TDOBJECT TYPE THEAD-TDOBJECT.

    LOOP AT IT_FILTER_SELECT_OPTIONS INTO DATA(LS_FILTER).

      CASE LS_FILTER-PROPERTY.
        WHEN 'DoNum'.
          READ TABLE LS_FILTER-SELECT_OPTIONS INTO DATA(LS_RANGE)
               INDEX 1.
          IF SY-SUBRC = 0 AND LS_RANGE-LOW IS NOT INITIAL.
            APPEND VALUE #( OPTION = 'EQ'
                  SIGN   = 'I'
                  LOW    = LS_RANGE-LOW
                  HIGH   = LS_RANGE-HIGH ) TO LR_VBELN.
          ENDIF.
      ENDCASE.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM LR_VBELN COMPARING ALL FIELDS.
    SELECT *
      FROM ZSDSV_DO_GET
      INTO TABLE @DATA(LT_DO_GET)
      WHERE DO_NUM IN @LR_VBELN.
    IF SY-SUBRC = 0.
      SORT LT_DO_GET BY DO_NUM DO_ITM.
    ENDIF.

    "Get Text
    DATA(LT_TEMP) = LT_DO_GET.
    DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING DO_NUM.
    LOOP AT LT_TEMP INTO DATA(LS_TEMP).

      REFRESH: LT_LINE.
      LV_TDID     = 'ZH11'.     "Land No.
      LV_TDNAME   = |{ LS_TEMP-DO_NUM }|.
      LV_TDOBJECT = 'VBBK'.

      CALL FUNCTION 'READ_TEXT' ##FM_SUBRC_OK
        EXPORTING
          ID                      = LV_TDID
          LANGUAGE                = SY-LANGU
          NAME                    = LV_TDNAME
          OBJECT                  = LV_TDOBJECT
        TABLES
          LINES                   = LT_LINE
        EXCEPTIONS
          ID                      = 1
          LANGUAGE                = 2
          NAME                    = 3
          NOT_FOUND               = 4
          OBJECT                  = 5
          REFERENCE_CHECK         = 6
          WRONG_ACCESS_TO_ARCHIVE = 7
          OTHERS                  = 8.
      IF SY-SUBRC NE 0.
        CALL FUNCTION 'READ_TEXT' ##FM_SUBRC_OK
          EXPORTING
            ID                      = LV_TDID
            LANGUAGE                = '2'
            NAME                    = LV_TDNAME
            OBJECT                  = LV_TDOBJECT
          TABLES
            LINES                   = LT_LINE
          EXCEPTIONS
            ID                      = 1
            LANGUAGE                = 2
            NAME                    = 3
            NOT_FOUND               = 4
            OBJECT                  = 5
            REFERENCE_CHECK         = 6
            WRONG_ACCESS_TO_ARCHIVE = 7
            OTHERS                  = 8.
      ENDIF.

      READ TABLE LT_LINE INTO LS_LINE
                         INDEX 1.
      IF SY-SUBRC = 0.
        CONDENSE LS_LINE-TDLINE.
        IF LS_LINE-TDLINE IS NOT INITIAL.
          APPEND INITIAL LINE TO LT_TEXT ASSIGNING FIELD-SYMBOL(<LFS_TEXT>).
          <LFS_TEXT>-VBELN = LS_TEMP-DO_NUM.
          <LFS_TEXT>-LAND  = LS_LINE-TDLINE.
        ENDIF.
      ENDIF.
    ENDLOOP.

    SORT LT_TEXT BY VBELN.

    LOOP AT LT_DO_GET INTO DATA(LS_DO_GET).
      APPEND INITIAL LINE TO ET_ENTITYSET ASSIGNING FIELD-SYMBOL(<LFS_ENTITYSET>).
      MOVE-CORRESPONDING LS_DO_GET TO <LFS_ENTITYSET>.

      READ TABLE LT_TEXT ASSIGNING <LFS_TEXT>
                         WITH KEY VBELN = LS_DO_GET-DO_NUM
                         BINARY SEARCH.
      IF SY-SUBRC = 0.
        <LFS_ENTITYSET>-LAND_NO = <LFS_TEXT>-LAND.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
