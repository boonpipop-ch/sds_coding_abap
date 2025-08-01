class ZCL_SDSSD_ENHANCE_REPORT definition
  public
  final
  create public .

public section.

  methods VL10E
    changing
      !CT_DATA type SHP_VL10_POSTAB_T .
protected section.
PRIVATE SECTION.

  CONSTANTS  : BEGIN OF GC_CON,
                 REMARK_ID     TYPE THEAD-TDID     VALUE 'ZH09',
                 REMARK_REQ    TYPE THEAD-TDID     VALUE 'ZH10',
                 REMARK_OBJECT TYPE THEAD-TDOBJECT VALUE 'VBBK',
                 MHA           TYPE THEAD-TDID     VALUE 'ZH18',
                 TWO           TYPE THEAD-TDSPRAS  VALUE '2',
               END OF GC_CON .
ENDCLASS.



CLASS ZCL_SDSSD_ENHANCE_REPORT IMPLEMENTATION.


  METHOD VL10E.
    DATA : LV_CHECK_SDS LIKE ABAP_TRUE.

    DATA : BEGIN OF LS_OFFICE_NAME,
             VKBUR TYPE TVKBT-VKBUR,
             BEZEI TYPE TVKBT-BEZEI,
           END OF LS_OFFICE_NAME.
    DATA LT_OFFICE_NAME LIKE TABLE OF LS_OFFICE_NAME.

    FIELD-SYMBOLS <LFS_DATA> LIKE LINE OF CT_DATA.

    DATA : LCL_UTIL TYPE REF TO ZCL_SDSCA_UTIL_SDS.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
*    LV_CHECK_SDS = LCL_DATA=>CHECK_SDS( SY-MANDT ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*
*    IF LV_CHECK_SDS EQ ABAP_TRUE.
    IF LCL_UTIL  IS NOT BOUND.
      CREATE OBJECT LCL_UTIL.
    ENDIF.

    DATA(LT_ADDRESS)     = LCL_DATA=>GET_ADDRESS( CT_DATA ).
    LCL_DATA=>GET_SALES_OFFICE_NAME( EXPORTING I_DATA  = CT_DATA
                                      CHANGING CT_DATA = LT_OFFICE_NAME ).

    LOOP AT CT_DATA ASSIGNING <LFS_DATA>.

      READ TABLE LT_ADDRESS INTO DATA(LS_ADDRESS)
      WITH KEY ADDRNUMBER = <LFS_DATA>-ADRNR_WE.
      IF SY-SUBRC = 0.
        <LFS_DATA>-ZZCITY1 = LS_ADDRESS-CITY1.
        CONCATENATE LS_ADDRESS-STREET LS_ADDRESS-STR_SUPPL3 LS_ADDRESS-LOCATION LS_ADDRESS-CITY1 INTO <LFS_DATA>-ZZADDRS SEPARATED BY SPACE.
        CONCATENATE LS_ADDRESS-NAME1  LS_ADDRESS-NAME2 INTO <LFS_DATA>-ZZFNAME SEPARATED BY SPACE.
        CONCATENATE LS_ADDRESS-NAME2  LS_ADDRESS-NAME3 LS_ADDRESS-NAME4 INTO <LFS_DATA>-ZZNAME_CUST SEPARATED BY SPACE.
      ENDIF.

      READ TABLE LT_OFFICE_NAME INTO LS_OFFICE_NAME
      WITH KEY VKBUR = <LFS_DATA>-VKBUR.
      IF SY-SUBRC EQ 0.
        <LFS_DATA>-ZZSALEOFFICE_NAME = LS_OFFICE_NAME-BEZEI.
      ENDIF.

      <LFS_DATA>-ZZREQ_REMARK = LCL_UTIL->GET_TEXT( EXPORTING I_ID       = GC_CON-REMARK_REQ
                                                            I_NAME     = <LFS_DATA>-VBELV
                                                            I_OBJECT   = GC_CON-REMARK_OBJECT
                                                            I_LANGUAGE = SY-LANGU ).

      IF <LFS_DATA>-ZZREQ_REMARK IS INITIAL.
        <LFS_DATA>-ZZREQ_REMARK = LCL_UTIL->GET_TEXT( EXPORTING I_ID     = GC_CON-REMARK_REQ
                                                            I_NAME     = <LFS_DATA>-VBELV
                                                            I_OBJECT   = GC_CON-REMARK_OBJECT
                                                            I_LANGUAGE = GC_CON-TWO ).
      ENDIF.

      <LFS_DATA>-ZZINV_REMARK = LCL_UTIL->GET_TEXT( EXPORTING I_ID       = GC_CON-REMARK_ID
                                                            I_NAME     = <LFS_DATA>-VBELV
                                                            I_OBJECT   = GC_CON-REMARK_OBJECT
                                                            I_LANGUAGE = SY-LANGU ).
      IF <LFS_DATA>-ZZINV_REMARK IS INITIAL.
        <LFS_DATA>-ZZINV_REMARK = LCL_UTIL->GET_TEXT( EXPORTING I_ID       = GC_CON-REMARK_ID
                                                              I_NAME     = <LFS_DATA>-VBELV
                                                              I_OBJECT   = GC_CON-REMARK_OBJECT
                                                              I_LANGUAGE = GC_CON-TWO ).
      ENDIF.

      <LFS_DATA>-ZZMHA = LCL_UTIL->GET_TEXT( EXPORTING I_ID       = GC_CON-MHA
                                                       I_NAME     = <LFS_DATA>-VBELV
                                                       I_OBJECT   = GC_CON-REMARK_OBJECT
                                                       I_LANGUAGE = SY-LANGU ).
      IF <LFS_DATA>-ZZMHA IS INITIAL.
        <LFS_DATA>-ZZMHA = LCL_UTIL->GET_TEXT( EXPORTING I_ID       = GC_CON-MHA
                                                         I_NAME     = <LFS_DATA>-VBELV
                                                         I_OBJECT   = GC_CON-REMARK_OBJECT
                                                         I_LANGUAGE = GC_CON-TWO ).
      ENDIF.
    ENDLOOP.
*    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*

  ENDMETHOD.
ENDCLASS.
