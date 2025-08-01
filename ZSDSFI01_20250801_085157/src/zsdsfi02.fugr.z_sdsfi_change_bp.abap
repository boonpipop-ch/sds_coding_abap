*-----------------------------------------------------------------------
*  Program ID         : Z_SDSFI_CREATE_BP
*  Creation Date      : 23.01.2024
*  Author             : B.CHIEWSARIKIJ (SDS)
*  Add-on ID          : ZFIAPI002
*  Description        : K2 Interface : Business Partner Creation
*  Purpose            : N/A
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*
*-----------------------------------------------------------------------
FUNCTION Z_SDSFI_CHANGE_BP.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_MODE) TYPE  ZSDSDE_INF_MODE OPTIONAL
*"     VALUE(IV_BP) TYPE  BU_PARTNER OPTIONAL
*"     VALUE(IV_TEST) TYPE  BOOLE_D OPTIONAL
*"     VALUE(IS_CENTRAL) TYPE  ZSDSFIS003 OPTIONAL
*"     VALUE(IT_BP_ROLE) TYPE  ZSDSFIS089_TT OPTIONAL
*"     VALUE(IT_ADDRESS) TYPE  ZSDSFIS010_TT OPTIONAL
*"     VALUE(IS_CUSTOMER) TYPE  ZSDSFIS078 OPTIONAL
*"     VALUE(IT_PARTNER) TYPE  ZSDSFIS083_TT OPTIONAL
*"     VALUE(IT_CONTACT) TYPE  ZSDSFIS082_TT OPTIONAL
*"     VALUE(IS_VENDOR) TYPE  ZSDSFIS084 OPTIONAL
*"     VALUE(IV_PARTN_CAT) TYPE  BU_TYPE OPTIONAL
*"     VALUE(IV_SHIPTO) TYPE  BOOLE_D OPTIONAL
*"     VALUE(IV_CONTACT) TYPE  BOOLE_D OPTIONAL
*"     VALUE(IT_SALES_AREA) TYPE  ZSDSSDS123_TT OPTIONAL
*"  CHANGING
*"     VALUE(ES_CENTRAL) TYPE  ZSDSFIS003 OPTIONAL
*"     VALUE(ET_BP_ROLE) TYPE  ZSDSFIS089_TT OPTIONAL
*"     VALUE(ET_ADDRESS) TYPE  ZSDSFIS010_TT OPTIONAL
*"     VALUE(ES_CUSTOMER) TYPE  ZSDSFIS078 OPTIONAL
*"     VALUE(ET_PARTNER) TYPE  ZSDSFIS083_TT OPTIONAL
*"     VALUE(ET_CONTACT) TYPE  ZSDSFIS082_TT OPTIONAL
*"     VALUE(ES_VENDOR) TYPE  ZSDSFIS084 OPTIONAL
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_T OPTIONAL
*"----------------------------------------------------------------------

  CLEAR: GV_FLAG_ERR,
         GV_PARTNER.

  CHECK IV_MODE = 'U'.
  GV_PARTNER = |{ IV_BP ALPHA = IN }|.
  ET_PARTNER[] = IT_PARTNER[].

  READ TABLE IT_ADDRESS INTO DATA(LS_ADDRESS)
                        INDEX 1.
  IF SY-SUBRC = 0 AND LS_ADDRESS-COUNTRY = 'TH'.
    GV_LOCAL = ABAP_TRUE.
  ENDIF.
  PERFORM GET_CONSTANTS.

  IF IV_SHIPTO IS NOT INITIAL.      "Change ship-to only
    PERFORM CHANGE_SHIP_TO USING GV_PARTNER
                                 IV_TEST
                           CHANGING ET_PARTNER
                                    ET_RETURN.
  ELSEIF IV_CONTACT IS NOT INITIAL. "Change Contact only
    PERFORM CHANGE_CONTACT_PERSON USING GV_PARTNER
                                        IV_TEST
                              CHANGING ET_CONTACT
                                       ET_RETURN.


  ELSE.                               "Normal Process

    PERFORM VALIDATE_CHG_DATA USING IT_ADDRESS
                              CHANGING ET_CONTACT
                                       ET_PARTNER
                                       ET_RETURN.
    IF GV_FLAG_ERR IS INITIAL.
      PERFORM CHANGE_BP_DATA USING
                                IV_TEST
                                IS_CENTRAL
                                IT_BP_ROLE
                                IT_ADDRESS
                                IS_CUSTOMER
                                IT_PARTNER
                                IT_CONTACT
                                IS_VENDOR
                                IV_PARTN_CAT
                                IT_SALES_AREA
                              CHANGING
                                ES_CENTRAL
                                ET_BP_ROLE
                                ET_ADDRESS
                                ES_CUSTOMER
                                ET_PARTNER
                                ET_CONTACT
                                ES_VENDOR
                                ET_RETURN.

      IF GV_FLAG_ERR <> ABAP_TRUE.
        PERFORM UPDATE_BILL_CYCLE USING IV_BP
                                        ES_CUSTOMER.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.

  ENDIF.


ENDFUNCTION.
