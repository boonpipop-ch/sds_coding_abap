FUNCTION Z_SDSSD_GET_CM_DOC.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_DOC) TYPE  VBELN OPTIONAL
*"  EXPORTING
*"     VALUE(E_DO) TYPE  VBELN_VL
*"     VALUE(E_INV) TYPE  VBELN_VF
*"----------------------------------------------------------------------

  SELECT SINGLE VBELN
    FROM VBFA
    WHERE VBELV   EQ @I_DOC
      AND VBTYP_N EQ 'T'
    INTO @E_DO.

  SELECT SINGLE VBELN
    FROM VBFA
    WHERE VBELV   EQ @I_DOC
      AND ( VBTYP_N EQ 'O' OR
            VBTYP_N EQ 'P' )
    INTO @E_INV.


ENDFUNCTION.
