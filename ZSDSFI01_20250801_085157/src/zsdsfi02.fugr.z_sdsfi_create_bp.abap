*-----------------------------------------------------------------------
*  Program ID         : Z_SDSFI_CREATE_BP
*  Creation Date      : 23.01.2024
*  Author             : B.CHIEWSARIKIJ
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
FUNCTION Z_SDSFI_CREATE_BP.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_MODE) TYPE  ZSDSDE_INF_MODE OPTIONAL
*"     VALUE(IV_BP) TYPE  BU_PARTNER OPTIONAL
*"     VALUE(IV_PARTN_CAT) TYPE  BU_TYPE OPTIONAL
*"     VALUE(IV_BU_GROUP) TYPE  BU_GROUP OPTIONAL
*"     VALUE(IV_K2_REFNO) TYPE  BU_BPEXT OPTIONAL
*"     VALUE(IV_SFDC_REFNO) TYPE  BU_BPEXT OPTIONAL
*"     VALUE(IV_TEST) TYPE  BOOLE_D OPTIONAL
*"     VALUE(IV_CREATED_BY) TYPE  PERSNO OPTIONAL
*"     VALUE(IS_CENTRAL) TYPE  ZSDSFIS003 OPTIONAL
*"     VALUE(IT_BP_ROLE) TYPE  ZSDSFIS089_TT OPTIONAL
*"     VALUE(IT_ADDRESS) TYPE  ZSDSFIS010_TT OPTIONAL
*"     VALUE(IS_CUSTOMER) TYPE  ZSDSFIS078 OPTIONAL
*"     VALUE(IT_PARTNER) TYPE  ZSDSFIS083_TT OPTIONAL
*"     VALUE(IT_CONTACT) TYPE  ZSDSFIS082_TT OPTIONAL
*"     VALUE(IS_VENDOR) TYPE  ZSDSFIS084 OPTIONAL
*"  EXPORTING
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_T
*"     VALUE(ET_PARTNER) TYPE  ZSDSFIS083_TT
*"     VALUE(ET_CONTACT) TYPE  ZSDSFIS082_TT
*"     VALUE(EV_PARTNER) TYPE  BU_PARTNER
*"----------------------------------------------------------------------

*  PERFORM mapping_field USING.
*  PERFORM validate_data.

  DATA: LV_BP_NUMBER  TYPE BU_PARTNER.
  CLEAR: GV_FLAG_ERR,
         GV_CUSTOMER_FLAG.

  CHECK IV_MODE = 'I'.      "I = Insert, U = Update and V = View

  READ TABLE IT_ADDRESS INTO DATA(LS_ADDRESS)
                        INDEX 1.
  IF SY-SUBRC = 0 AND LS_ADDRESS-COUNTRY = 'TH'.
    GV_LOCAL = ABAP_TRUE.
  ENDIF.

  PERFORM GET_CONSTANTS.

  PERFORM VALIDATE_DATA USING IV_PARTN_CAT
                              IS_CENTRAL
                              IT_ADDRESS
                         CHANGING IS_CUSTOMER
                                  IS_VENDOR
                                  IT_PARTNER
                                  IT_CONTACT
                                  ET_RETURN.

  IF GV_FLAG_ERR <> ABAP_TRUE.
    PERFORM CREATE_BUSINESSPARTNER USING IV_BP
                                         IT_BP_ROLE
                                         IV_PARTN_CAT
                                         IV_BU_GROUP
                                         IV_K2_REFNO
                                         IV_SFDC_REFNO
                                         IS_CENTRAL
                                         IS_CUSTOMER
                                         IS_VENDOR
                                         IT_ADDRESS
                                         IV_TEST
                                     CHANGING
                                         LV_BP_NUMBER
                                         IT_PARTNER
                                         IT_CONTACT
                                         ET_RETURN.
  ENDIF.

  LOOP AT ET_RETURN INTO DATA(LS_RETURN)
                    WHERE TYPE = 'E' OR TYPE = 'A'.
    GV_FLAG_ERR = ABAP_TRUE.
    EXIT.
  ENDLOOP.
  IF SY-SUBRC <> 0.
    "Return Result Data
    ET_PARTNER = IT_PARTNER.
    ET_CONTACT = IT_CONTACT.
    EV_PARTNER = LV_BP_NUMBER.
  ENDIF.

  IF IV_TEST IS INITIAL.
    "Update to log table
    READ TABLE ET_RETURN INTO LS_RETURN
                         INDEX 1.
    IF SY-SUBRC = 0.
      PERFORM UPDATE_LOG_DATA USING IV_K2_REFNO
                                    IV_SFDC_REFNO
                                    EV_PARTNER
                                    IV_CREATED_BY
                                    LS_RETURN.
    ENDIF.

    IF GV_FLAG_ERR <> ABAP_TRUE AND GV_CUSTOMER_FLAG = ABAP_TRUE.
      PERFORM UPDATE_BILL_CYCLE USING EV_PARTNER
                                      IS_CUSTOMER.

    ENDIF.

  ENDIF.

ENDFUNCTION.
