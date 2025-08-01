*&---------------------------------------------------------------------*
*& Include          ZXACKU01
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSSD_CREATE_CHANGE_SO_SRV
*  Creation Date      : 24.10.2024
*  Author             : Zulkiff B. (Eviden)
*  Add-on ID          : ZSDI023
*  Description        : perform the ATP check with requirement quantities
*  Purpose            : BAPI will carry out the ATP check with requirement quantities
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
*CMOD: ZSDSSD01
*Enhancement: W61V0001

*SAP Note 1751389 - BAPI_MATERIAL_AVAILABILITY does not check with requirement quantities
DATA: GRT_PLANT TYPE T_RANGE_WERKS.

ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = 'ZCL_SDSSD_CREATE_CHANGE_SO_SRV'
                                                IF_PARAM = 'PLANT'
                                      IMPORTING ET_RANGE = GRT_PLANT ).
IF GRT_PLANT IS NOT INITIAL.
  IF PLANT IN GRT_PLANT.
    CUSTOMER_TRTYP = 'H'.
  ENDIF.
ENDIF.
