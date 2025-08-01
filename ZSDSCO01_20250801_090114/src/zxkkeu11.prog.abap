*&---------------------------------------------------------------------*
*& Include          ZXKKEU11
*&---------------------------------------------------------------------*
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(I_OPERATING_CONCERN) LIKE  TKEB-ERKRS
*"             VALUE(I_DERIVATION_DATE) LIKE  SY-DATUM
*"             VALUE(I_STEP_ID) LIKE  TKEDRS-STEPID
*"             VALUE(I_COPA_ITEM)
*"             VALUE(I_GLOBAL) LIKE  KEDRCOPA STRUCTURE  KEDRCOPA
*"       EXPORTING
*"             REFERENCE(E_COPA_ITEM)
*"             REFERENCE(E_GLOBAL)
*"             REFERENCE(E_EXIT_IS_ACTIVE)
*"             REFERENCE(E_FAILED)
*"       EXCEPTIONS
*"              DERIVATION_FAILED
*"----------------------------------------------------------------------
*-----------------------------------------------------------------------
*  Program            : ZXKKEU11
*  Creation Date      : 30.05.2024
*  Author             : Nadtaya T.(Eviden)
*  Add-on ID          : N/A
*  Description        : Enhancement Characteristic Derivation
*  Purpose            : Derive Sale/Service datato CO-PA Characteristic
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
*& Version/T.P No.: 1.0 (F36K913291)
*& Changed On/By: 28.02.2025 TWSG (2TW012)
*& Description:  Remove the current validation
*&---------------------------------------------------------------------*
  CONSTANTS LC_ERKRS_1000 TYPE ERKRS VALUE '1000'.

*...Check SDS Operating Concern
  IF I_OPERATING_CONCERN EQ LC_ERKRS_1000.
    CALL FUNCTION 'Z_SDSCO_KEDR_ENH'
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
  ENDIF.

*DHOS not allow blank product.hierarchy-------------------------------------------------
** >>>>> v1.0 INSERT START >>>>> *
*  CONSTANTS: lc_z001 TYPE kedrstepid VALUE 'Z001'. " only trigger by DHOS Z001 , NO IMPACT TO OTHER CLIENT
*
*  CASE i_step_id.
*
*  WHEN lc_z001." Blank product.hierarchy
*    IF sy-tcode EQ 'ZDHOSFI100'
*    OR sy-cprog EQ 'ZDHOSFIR0100'
*    OR sy-tcode EQ 'FB60'.
*      "do nothing.
*    ELSE.

*      e_exit_is_active = 'X'.
*      e_failed = 'X'.
*      MESSAGE e000(02) WITH 'Pls. enter value for Product hierarchy'.
*    ENDIF.
*
*ENDCASE.
*
** >>>>> v1.0 INSERT END >>>>> *
**DHOS not allow blank product.hierarchy-------------------------------------------------
