FUNCTION Z_SDSCO_KEDR_ENH ##FM_NO_TYPE .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_OPERATING_CONCERN) LIKE  TKEB-ERKRS
*"     VALUE(I_DERIVATION_DATE) LIKE  SY-DATUM
*"     VALUE(I_STEP_ID) LIKE  TKEDRS-STEPID
*"     VALUE(I_COPA_ITEM)
*"     VALUE(I_GLOBAL) LIKE  KEDRCOPA STRUCTURE  KEDRCOPA
*"  EXPORTING
*"     REFERENCE(E_COPA_ITEM)
*"     REFERENCE(E_GLOBAL)
*"     REFERENCE(E_EXIT_IS_ACTIVE)
*"     REFERENCE(E_FAILED)
*"----------------------------------------------------------------------
*..Initial export param
  E_COPA_ITEM      = I_COPA_ITEM.
  E_GLOBAL         = I_GLOBAL.
  E_EXIT_IS_ACTIVE = ABAP_FALSE.
  E_FAILED         = ABAP_FALSE.

  CASE I_STEP_ID.
    WHEN 'Z001'. "Project Sales (Step ID - Z001)
      CALL FUNCTION 'Z_SDSCO_KEDR_Z001'
        EXPORTING
          I_OPERATING_CONCERN = I_OPERATING_CONCERN
          I_DERIVATION_DATE   = I_DERIVATION_DATE
          I_STEP_ID           = I_STEP_ID
          I_COPA_ITEM         = I_COPA_ITEM
          I_GLOBAL            = I_GLOBAL
        IMPORTING
          E_COPA_ITEM         = E_COPA_ITEM
          E_GLOBAL            = E_GLOBAL
          E_EXIT_IS_ACTIVE    = E_EXIT_IS_ACTIVE
          E_FAILED            = E_FAILED.

    WHEN 'Z002'. "Normal Service (Step ID Z002)
      CALL FUNCTION 'Z_SDSCO_KEDR_Z002'
        EXPORTING
          I_OPERATING_CONCERN = I_OPERATING_CONCERN
          I_DERIVATION_DATE   = I_DERIVATION_DATE
          I_STEP_ID           = I_STEP_ID
          I_COPA_ITEM         = I_COPA_ITEM
          I_GLOBAL            = I_GLOBAL
        IMPORTING
          E_COPA_ITEM         = E_COPA_ITEM
          E_GLOBAL            = E_GLOBAL
          E_EXIT_IS_ACTIVE    = E_EXIT_IS_ACTIVE
          E_FAILED            = E_FAILED.

    WHEN OTHERS.

      RETURN.

  ENDCASE.

ENDFUNCTION.                                             "#EC CI_VALPAR
