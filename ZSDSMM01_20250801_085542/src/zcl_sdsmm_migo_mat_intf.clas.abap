class ZCL_SDSMM_MIGO_MAT_INTF definition
  public
  final
  create public .

public section.

  class-methods CREATE_INTERFACE_FILE
    importing
      !IS_MKPF type MKPF
      !IT_MSEG type TY_T_MSEG .
  class-methods GET_CONSTANT .
protected section.
private section.

  class-data GRT_VGART type ZVGART_R .
  class-data GRT_BWART type ZBWART_R .
ENDCLASS.



CLASS ZCL_SDSMM_MIGO_MAT_INTF IMPLEMENTATION.


  METHOD CREATE_INTERFACE_FILE.

    CALL METHOD ZCL_SDSMM_MIGO_MAT_INTF=>GET_CONSTANT.

    IF IS_MKPF-VGART IN GRT_VGART AND
          GRT_VGART[] IS NOT INITIAL .

      PERFORM F_CALL_B2P_INTERFACE
           IN PROGRAM ZSDSMMR0230 IF FOUND USING IS_MKPF .

    ENDIF.

  ENDMETHOD.


  METHOD GET_CONSTANT.

    IF GRT_VGART[] IS INITIAL.
      ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = 'ZSDSMMR0230'
                                                      IF_PARAM = 'TRANSACTION_TYPE'
                                            IMPORTING ET_RANGE = GRT_VGART ).
    ENDIF.


   IF GRT_BWART[] IS INITIAL.
      ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = 'ZSDSMMR0230'
                                                      IF_PARAM = 'MOVEMENT_TYPE'
                                            IMPORTING ET_RANGE = GRT_BWART ).
    ENDIF.




  ENDMETHOD.
ENDCLASS.
