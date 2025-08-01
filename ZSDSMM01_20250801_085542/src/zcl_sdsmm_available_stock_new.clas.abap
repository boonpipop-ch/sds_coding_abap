class ZCL_SDSMM_AVAILABLE_STOCK_NEW definition
  public
  final
  create public .

public section.

  constants:
    BEGIN OF GC_CON,
                  E TYPE C LENGTH 1 VALUE 'E',
                  S TYPE C LENGTH 1 VALUE 'S',
                END OF GC_CON .

  methods FG
    importing
      !I_WERKS type WERKS_D
      value(I_VTWEG) type VTWEG default '00'
      !I_DATA_SELECTION type ZSDSMMS056
    returning
      value(R_RESULT) type ZSDSMMS055_TT .
  methods FG_DETAIL
    importing
      !I_WERKS type WERKS_D
      !I_VTWEG type VTWEG default '00'
      !I_DATA_SELECTION type ZSDSMMS049
    returning
      value(R_RESULT) type ZSDSMMS055_TT .
  methods SP
    importing
      !I_WERKS type WERKS_D
      value(I_VTWEG) type VTWEG default '00'
      !I_DATA_SELECTION type ZSDSMMS056
    returning
      value(R_RESULT) type ZSDSMMS055_TT .
  methods SP_DETAIL
    importing
      !I_WERKS type WERKS_D
      value(I_VTWEG) type VTWEG default '00'
      !I_DATA_SELECTION type ZSDSMMS049
    returning
      value(R_RESULT) type ZSDSMMS055_TT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SDSMM_AVAILABLE_STOCK_NEW IMPLEMENTATION.


  METHOD FG.

    DATA : LS_SELECTION LIKE I_DATA_SELECTION.

    LS_SELECTION = I_DATA_SELECTION.
    LCL_DATA=>GET_BOM( EXPORTING I_DATA  = I_WERKS
                        CHANGING CT_DATA = LS_SELECTION-MATNR ).

    IF LS_SELECTION-MATNR IS INITIAL AND
       LS_SELECTION-PH1   IS INITIAL AND
       LS_SELECTION-PH2   IS INITIAL AND
       LS_SELECTION-PH3   IS INITIAL.
    ELSE.
      DATA(LT_STOCK_INFO) = LCL_DATA=>GET_STOCK_INFO( I_WERKS = I_WERKS
                                                      I_VTWEG = I_VTWEG
                                                      I_DATA  = LS_SELECTION ).

      R_RESULT = LCL_DATA=>GEN_DATA_AVAILABLE_STOCK( I_DATA  = LT_STOCK_INFO
                                                     I_WERKS = I_WERKS ).
    ENDIF.
  ENDMETHOD.


  METHOD FG_DETAIL.
    DATA : LS_SELECTION LIKE I_DATA_SELECTION.

    LS_SELECTION = I_DATA_SELECTION.
    LCL_DATA=>GET_BOM( EXPORTING I_DATA  = I_WERKS
                        CHANGING CT_DATA = LS_SELECTION-MATNR ).

    IF LS_SELECTION-MATNR IS NOT INITIAL.
      DATA(LT_STOCK_INFO) = LCL_DATA=>GET_STOCK_INFO_DETAIL( I_WERKS = I_WERKS
                                                             I_VTWEG = I_VTWEG
                                                             I_DATA  = LS_SELECTION ).
      SORT LT_STOCK_INFO BY MATNR.

      R_RESULT = LCL_DATA=>GEN_DATA_AVAILABLE_STOCK_D( I_DATA  = LT_STOCK_INFO
                                                       I_WERKS = I_WERKS ).
    ENDIF.
  ENDMETHOD.


  METHOD SP.
    DATA : LS_SELECTION LIKE I_DATA_SELECTION.

    LS_SELECTION = I_DATA_SELECTION.
    LCL_DATA=>GET_GROUP_PART( CHANGING CT_DATA = LS_SELECTION-MATNR ).

    IF LS_SELECTION-MATNR IS NOT INITIAL.
      DATA(LT_STOCK_INFO) = LCL_DATA=>GET_STOCK_INFO_SP( I_WERKS = I_WERKS
                                                         I_VTWEG = I_VTWEG
                                                         I_DATA  = LS_SELECTION ).

      R_RESULT = LCL_DATA=>GEN_DATA_AVAILABLE_STOCK_SP( I_DATA      = LT_STOCK_INFO
                                                        I_WERKS     = I_WERKS
                                                        I_SELECTION = LS_SELECTION ).
    ENDIF.

  ENDMETHOD.


  METHOD SP_DETAIL.
    DATA : LS_SELECTION LIKE I_DATA_SELECTION.

    LS_SELECTION = I_DATA_SELECTION.
    LCL_DATA=>GET_BOM( EXPORTING I_DATA  = I_WERKS
                        CHANGING CT_DATA = LS_SELECTION-MATNR ).

    IF LS_SELECTION-MATNR IS NOT INITIAL.
      DATA(LT_STOCK_INFO) = LCL_DATA=>GET_STOCK_INFO_DETAIL_SP( I_WERKS = I_WERKS
                                                                I_VTWEG = I_VTWEG
                                                                I_DATA  = LS_SELECTION ).
      SORT LT_STOCK_INFO BY MATNR.

      R_RESULT = LCL_DATA=>GEN_DATA_AVAILABLE_STOCK_D_SP( I_DATA  = LT_STOCK_INFO
                                                          I_WERKS = I_WERKS ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
