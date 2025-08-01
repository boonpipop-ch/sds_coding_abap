FUNCTION Z_SDSMM_PHYSICAL_COUNT_FIX.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_INPUT) TYPE  ZSDSFIT050 OPTIONAL
*"     VALUE(I_MODE) TYPE  CHAR10 OPTIONAL
*"  EXPORTING
*"     VALUE(E_MESTYPE) TYPE  CHAR1
*"     VALUE(E_MESSAGE) TYPE  CHAR255
*"     VALUE(ET_RETURN) TYPE  ZSDSFIS142_TT
*"     VALUE(ET_REPORT) TYPE  ZSDSFIS182_TT
*"     VALUE(E_SEARCH) TYPE  CHAR255
*"     VALUE(E_KOSTL) TYPE  KOSTL
*"     VALUE(E_PERNR) TYPE  PERNR_D
*"     VALUE(E_ASSET) TYPE  ANLN1
*"     VALUE(E_SUBST) TYPE  ANLN2
*"----------------------------------------------------------------------
  IF     I_MODE EQ GC_CON-SEARCH_COSTCENTER.
    ET_RETURN = LCL_DATA=>SEARCH_COSTCENTER( I_INPUT ).
  ELSEIF I_MODE EQ GC_CON-SEARCH_LOCATION.
    ET_RETURN = LCL_DATA=>SEARCH_LOCATION( I_INPUT ).
  ELSEIF I_MODE EQ GC_CON-SEARCH_PERSON.
    ET_RETURN = LCL_DATA=>SEARCH_PERSONAL_NUMBER( I_INPUT ).
  ELSEIF I_MODE EQ GC_CON-INSERT_DATA.
    LCL_DATA=>INSERT_DATA( EXPORTING I_DATA    = I_INPUT
                            CHANGING C_MESSTYP = E_MESTYPE
                                     C_MESSAGE = E_MESSAGE ).
  ELSEIF I_MODE EQ GC_CON-GET_DESC.
    LCL_DATA=>GET_ASSET_DESC( EXPORTING I_DATA = I_INPUT
                              IMPORTING C_DESC = E_SEARCH
                                        C_COST = E_KOSTL
                                        C_EMPC = E_PERNR ).
  ELSEIF I_MODE EQ GC_CON-REPORT.
    ET_REPORT = LCL_DATA=>GET_REPORT( I_INPUT ).
  ELSEIF I_MODE EQ GC_CON-GET_ECC.
    LCL_DATA=>GET_ECC_ASSET( EXPORTING I_DATA = I_INPUT
                              CHANGING C_DESC = E_SEARCH
                                       C_COST = E_KOSTL
                                       C_EMPC = E_PERNR
                                       C_ASST = E_ASSET
                                       C_SUBA = E_SUBST ).
  ELSEIF I_MODE EQ GC_CON-GET_AUTO.
    LCL_DATA=>GET_AUTO( EXPORTING I_DATA = I_INPUT
                         CHANGING C_AUTO = E_SEARCH ).
  ENDIF.
ENDFUNCTION.
