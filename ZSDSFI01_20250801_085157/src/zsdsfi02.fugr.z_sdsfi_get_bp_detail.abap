*-----------------------------------------------------------------------
*  Program ID         : Z_SDSFI_GET_BP_DETAIL
*  Creation Date      : 15.02.2024
*  Author             : b.chiewsarikij
*  Add-on ID          : ZFIAPI002
*  Description        : Function for get Business partner detail
*  Purpose            : N/A
*  Copied from        : <<Reference Program>>
*  Restriction        :
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
FUNCTION Z_SDSFI_GET_BP_DETAIL.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_PARTNER) TYPE  BU_PARTNER OPTIONAL
*"     VALUE(IV_MODE) TYPE  ZSDSDE_INF_MODE OPTIONAL
*"  EXPORTING
*"     VALUE(EV_PARTN_CAT) TYPE  BU_TYPE
*"     VALUE(EV_BU_GROUP) TYPE  BU_GROUP
*"     VALUE(EV_K2_REFNO) TYPE  BU_BPEXT
*"     VALUE(EV_SFDC_REFNO) TYPE  BU_BPEXT
*"     VALUE(ES_CENTRAL) TYPE  ZSDSFIS003
*"     VALUE(ET_BP_ROLE) TYPE  ZSDSFIS089_TT
*"     VALUE(ET_ADDRESS) TYPE  ZSDSFIS010_TT
*"     VALUE(ES_CUSTOMER) TYPE  ZSDSFIS078
*"     VALUE(ET_PARTNER) TYPE  ZSDSFIS083_TT
*"     VALUE(ET_CONTACT) TYPE  ZSDSFIS082_TT
*"     VALUE(ES_VENDOR) TYPE  ZSDSFIS084
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_T
*"     VALUE(ES_BLOCK) TYPE  ZSDSFIS116
*"----------------------------------------------------------------------

  DATA: LT_RETURN     TYPE STANDARD TABLE OF BAPIRET2,
        LV_BP_ROLE    TYPE BU_PARTNERROLE,

        "Change Parameter
        LV_PARTN_CAT  TYPE BU_TYPE,
        LV_BU_GROUP   TYPE BU_GROUP,
        LV_K2_REFNO   TYPE BU_BPEXT,
        LV_SFDC_REFNO TYPE BU_BPEXT,
        LS_CENTRAL    TYPE ZSDSFIS003,
        LT_BP_ROLE    TYPE ZSDSFIS089_TT,
        LT_ADDRESS    TYPE ZSDSFIS010_TT,
        LS_CUSTOMER   TYPE ZSDSFIS078,
        LT_PARTNER    TYPE ZSDSFIS083_TT,
        LT_CONTACT    TYPE ZSDSFIS082_TT,
        LS_VENDOR     TYPE ZSDSFIS084,
        LS_BLOCK      TYPE ZSDSFIS116.

  "Display Mode
  CHECK IV_MODE = 'V'.
  CLEAR GV_FLAG_ERR.

  GV_PARTNER = |{ IV_PARTNER ALPHA = IN }|.

  PERFORM CHK_BP_NUMBER USING GV_PARTNER
                        CHANGING LT_RETURN.

  IF GV_FLAG_ERR IS INITIAL.

    PERFORM GET_BP_DETAIL CHANGING LV_PARTN_CAT
                                   LV_BU_GROUP
                                   LV_K2_REFNO
                                   LV_SFDC_REFNO
                                   LS_CENTRAL
                                   LT_BP_ROLE
                                   LT_ADDRESS
                                   LS_CUSTOMER
                                   LT_PARTNER
                                   LT_CONTACT
                                   LS_VENDOR
                                   LS_BLOCK
                                   LT_RETURN.

  ELSE.
    ET_RETURN[] = LT_RETURN[].
  ENDIF.

  EV_PARTN_CAT     = LV_PARTN_CAT.
  EV_BU_GROUP      = LV_BU_GROUP.
  EV_K2_REFNO      = LV_K2_REFNO.
  EV_SFDC_REFNO    = LV_SFDC_REFNO.
  ES_CENTRAL       = LS_CENTRAL.
  ET_BP_ROLE       = LT_BP_ROLE.
  ET_ADDRESS       = LT_ADDRESS.
  ES_CUSTOMER      = LS_CUSTOMER.
  ET_PARTNER       = LT_PARTNER.
  ET_CONTACT       = LT_CONTACT.
  ES_VENDOR        = LS_VENDOR.
  ES_BLOCK         = LS_BLOCK.



ENDFUNCTION.
