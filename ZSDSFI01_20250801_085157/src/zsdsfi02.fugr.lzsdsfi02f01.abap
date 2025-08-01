*----------------------------------------------------------------------*
***INCLUDE LZSDSFI02F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form validate_data
*&---------------------------------------------------------------------*
*& Perform for validate data
*&---------------------------------------------------------------------*
FORM VALIDATE_DATA USING UV_PARTN_CAT    TYPE BU_TYPE
                         US_CENTRAL      TYPE ZSDSFIS003
                         UT_ADDRESS      TYPE ZSDSFIS010_TT
                   CHANGING CS_CUSTOMER  TYPE ZSDSFIS078
                            CS_VENDOR    TYPE ZSDSFIS084
                            CT_PARTNER   TYPE ZSDSFIS083_TT
                            CT_CONTACT   TYPE ZSDSFIS082_TT
                            CT_RETURN    TYPE BAPIRET2_T.

  CLEAR GV_FLAG_ERR.

  IF US_CENTRAL-TAXNUMBER = GC_VIP_TAX.
    CS_CUSTOMER-VIP = ABAP_TRUE.
  ENDIF.

  IF CS_CUSTOMER-VIP IS INITIAL.
    SELECT
        DFKKBPTAXNUM~PARTNER,
        DFKKBPTAXNUM~TAXTYPE,
        DFKKBPTAXNUM~TAXNUM,
        BUT000~XDELE
      FROM DFKKBPTAXNUM
      INNER JOIN BUT000
      ON DFKKBPTAXNUM~PARTNER = BUT000~PARTNER
      INTO TABLE @DATA(LT_BPTAXNUM)
      WHERE TAXTYPE = @GC_TAX_TH3
        AND TAXNUM  = @US_CENTRAL-TAXNUMBER.
*        AND XDELE  <> @ABAP_TRUE.
    IF SY-SUBRC = 0.
      IF UV_PARTN_CAT = '1'.          "Person
        GV_FLAG_ERR = ABAP_TRUE.
      ELSEIF UV_PARTN_CAT = '2'.      "Org.

        "Branch Code (customer)
        SELECT
          KUNNR,
          J_1TPBUPL,
          DESCRIPTION
        FROM FITHA_PBUPL_D_T
        INTO TABLE @DATA(LT_TAX_CUST)
        FOR ALL ENTRIES IN @LT_BPTAXNUM
        WHERE KUNNR = @LT_BPTAXNUM-PARTNER
          AND SPRAS = 'E'.
        IF SY-SUBRC = 0.
          SORT LT_TAX_CUST BY KUNNR.
        ENDIF.

        "Branch Code (vendor)
        SELECT
          LIFNR,
          J_1TPBUPL,
          DESCRIPTION
          FROM FITHA_PBUPL_K_T
          INTO TABLE @DATA(LT_TAX_VEND)
          FOR ALL ENTRIES IN @LT_BPTAXNUM
          WHERE LIFNR = @LT_BPTAXNUM-PARTNER
            AND SPRAS = 'E'.
        IF SY-SUBRC = 0.
          SORT LT_TAX_VEND BY LIFNR.
        ENDIF.

        LOOP AT LT_BPTAXNUM INTO DATA(LS_BPTAXNUM).
          "Customer
          READ TABLE LT_TAX_CUST INTO DATA(LS_TAX_CUST)
                                 WITH KEY KUNNR = LS_BPTAXNUM-PARTNER
                                 BINARY SEARCH.
          IF SY-SUBRC = 0 AND LS_TAX_CUST-J_1TPBUPL = US_CENTRAL-BCODE.
            GV_FLAG_ERR = ABAP_TRUE.
            EXIT.
          ELSE.
            "Vendor
            READ TABLE LT_TAX_VEND INTO DATA(LS_TAX_VEND)
                                   WITH KEY LIFNR = LS_BPTAXNUM-PARTNER
                                   BINARY SEARCH.
            IF SY-SUBRC = 0 AND LS_TAX_VEND-J_1TPBUPL = US_CENTRAL-BCODE.
              GV_FLAG_ERR = ABAP_TRUE.
              EXIT.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF GV_FLAG_ERR = ABAP_TRUE.
        APPEND INITIAL LINE TO CT_RETURN ASSIGNING FIELD-SYMBOL(<LFS_RETURN>).
        <LFS_RETURN>-TYPE     = 'E'.
        <LFS_RETURN>-MESSAGE  = TEXT-E01.
*     Another business partner already has this tax number (tax number :  &1)
        REPLACE '&1' IN <LFS_RETURN>-MESSAGE WITH US_CENTRAL-TAXNUMBER.
      ENDIF.

    ENDIF.
  ENDIF.

  "Contact Person (Pre-execute->chk err)
  PERFORM CRE_CONTACT_PERSON USING SPACE
                                   ABAP_TRUE
                             CHANGING CT_CONTACT
                                      CT_RETURN.

  "Shipto (Pre-execute->chk error)
  PERFORM CRE_SHIPTO_PFN     USING SPACE        "BU_PARTNER
                                   ABAP_TRUE    "TEST RUN
                             CHANGING CT_PARTNER
                                      CT_RETURN.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form create_businesspartner
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CREATE_BUSINESSPARTNER USING
                                  UV_BP           TYPE BU_PARTNER
                                  UT_BP_ROLE      TYPE ZSDSFIS089_TT
                                  UV_PARTN_CAT    TYPE BU_TYPE
                                  UV_BU_GROUP     TYPE BU_GROUP
                                  UV_K2_REFNO     TYPE CHAR20
                                  UV_SFDC_REFNO   TYPE CHAR20
                                  US_CENTRAL      TYPE ZSDSFIS003
                                  US_CUSTOMER     TYPE ZSDSFIS078
                                  US_VENDOR       TYPE ZSDSFIS084
                                  UT_ADDRESS      TYPE ZSDSFIS010_TT
                                  UV_TEST         TYPE CHAR01
                             CHANGING
                                  CV_BP           TYPE BU_PARTNER
                                  CT_PARTNER      TYPE ZSDSFIS083_TT
                                  CT_CONTACT      TYPE ZSDSFIS082_TT
                                  CT_RETURN       TYPE BAPIRET2_T.

  CONSTANTS:
    LC_VALIDTO TYPE SY-DATUM           VALUE '99991231'.

  DATA:
    LS_BP            TYPE CVIS_EI_EXTERN,
    LT_BP            TYPE CVIS_EI_EXTERN_T,
    LT_ADDRESS       TYPE BUS_EI_BUPA_ADDRESS_T,
    LT_ROLE          TYPE BUS_EI_BUPA_ROLES_T,
    LS_ROLE          TYPE BUS_EI_BUPA_ROLES,
    LT_IDENT_NUMBERS TYPE BUS_EI_BUPA_IDENTIFICATION_T,
    LT_TAXNUMBERS    TYPE BUS_EI_BUPA_TAXNUMBER_T,
    LT_RETURN        TYPE BAPIRETM,
    LV_BU_PARTNER    TYPE BU_PARTNER,
    LV_ERROR         TYPE ABAP_BOOL,
    LT_TAX           TYPE BUS_EI_BUPA_TAXNUMBER_T,
    LT_ADDR_VER      TYPE BUS_EI_BUPA_VERSION_T,
    LT_CUS_SALES     TYPE CMDS_EI_SALES_T,
    LT_CUS_COMPANY   TYPE CMDS_EI_COMPANY_T,
    LT_CUS_DUNNING   TYPE CMDS_EI_DUNNING_T,
    LT_VEN_BANK      TYPE CVIS_EI_BANKDETAIL_T,
    LT_VEN_PUR       TYPE VMDS_EI_PURCHASING_T,
    LT_CRE_SEGMENT   TYPE UKMT_EI_BP_CMS_SGM,
    LS_RETURN        TYPE BAPIRET2,
    LT_TAX_IND       TYPE CMDS_EI_TAX_IND_T,
    LT_SMTP          TYPE BUS_EI_BUPA_SMTP_T,
    LT_PHONE         TYPE BUS_EI_BUPA_TELEPHONE_T,
    LT_FAX           TYPE BUS_EI_BUPA_FAX_T,
    LT_VEN_COMPANY   TYPE VMDS_EI_COMPANY_T,
    LT_PARTNER_FUNC  TYPE CMDS_EI_FUNCTIONS_T,
    LT_TAX_CUST      TYPE CMDS_EI_WTAX_TYPE_T,
    LT_TAX_VEND      TYPE VMDS_EI_WTAX_TYPE_T,
    LV_WHT_NO        TYPE N.

*------------------------------------------------------------------------------
* Create GUID for new BP
*------------------------------------------------------------------------------
  TRY.
      DATA(V_GUID) = CL_SYSTEM_UUID=>IF_SYSTEM_UUID_STATIC~CREATE_UUID_C32( ).
    CATCH CX_UUID_ERROR INTO DATA(R_UUID_EXC).
      MESSAGE R_UUID_EXC->GET_TEXT( ) TYPE 'E'.
  ENDTRY.

*------------------------------------------------------------------------------
* Header and common central data
*------------------------------------------------------------------------------
  LS_BP-PARTNER-HEADER-OBJECT_TASK                      = GC_TASK_INSERT. "'I' for new BP
  LS_BP-PARTNER-HEADER-OBJECT_INSTANCE-BPARTNERGUID     = V_GUID.
  IF UV_BP IS NOT INITIAL.
    LS_BP-PARTNER-HEADER-OBJECT_INSTANCE-BPARTNER       = |{ UV_BP ALPHA = IN }|.
    LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CENTRALDATA-PARTNEREXTERNAL = UV_BP.
    LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_CENTRALDATA-PARTNEREXTERNAL = ABAP_TRUE.

    IF UV_BU_GROUP = GC_GROUP-DKI.
      LS_BP-PARTNER-FINSERV_DATA-COMMON-DATA-FSBP_CENTRL-VBUND    = |{ UV_BP ALPHA = IN }|.
      LS_BP-PARTNER-FINSERV_DATA-COMMON-DATAX-FSBP_CENTRL-VBUND   = ABAP_TRUE.
    ENDIF.
  ENDIF.

  "VIP Customer
  IF US_CUSTOMER-VIP IS NOT INITIAL.
    LS_BP-PARTNER-FINSERV_DATA-COMMON-DATA-FSBP_CENTRL-VIP  = ABAP_TRUE.
    LS_BP-PARTNER-FINSERV_DATA-COMMON-DATAX-FSBP_CENTRL-VIP = ABAP_TRUE.
  ENDIF.

* Category: 1 for Person, 2 for Organization, 3 for Group
  LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CONTROL-CATEGORY = UV_PARTN_CAT.

* The grouping depends on the system settings
  LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CONTROL-GROUPING = UV_BU_GROUP.         "Business Partner Group

  "Central Data
  LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CENTRALDATA-SEARCHTERM1 = US_CENTRAL-SEARCHTERM1.
  LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CENTRALDATA-SEARCHTERM2 = UV_SFDC_REFNO.
*  ls_bp-partner-central_data-common-data-bp_centraldata-title_key   = us_central-title_key.

  "Mark X
  LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_CENTRALDATA-SEARCHTERM1 = ABAP_TRUE.
  LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_CENTRALDATA-SEARCHTERM2 = ABAP_TRUE.
*  ls_bp-partner-central_data-common-datax-bp_centraldata-title_key   = abap_true.

  LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CENTRALDATA-PARTNEREXTERNAL   = US_CENTRAL-TAXNUMBER.
  LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_CENTRALDATA-PARTNEREXTERNAL  = ABAP_TRUE.

  "Tax Data
  APPEND INITIAL LINE TO LT_TAX ASSIGNING FIELD-SYMBOL(<LFS_TAX>).
  <LFS_TAX>-DATA_KEY-TAXTYPE     = GC_TAX_TH3.
  <LFS_TAX>-DATA_KEY-TAXNUMBER   = US_CENTRAL-TAXNUMBER.
  <LFS_TAX>-DATA_KEY-TAXNUMXL    = US_CENTRAL-TAXNUMBER.
  LS_BP-PARTNER-CENTRAL_DATA-TAXNUMBER-TAXNUMBERS = LT_TAX[].

  "Thai version
  READ TABLE UT_ADDRESS INTO DATA(LS_ADDRESS_TH)
                        WITH KEY INTERNATIONAL_VERSION = ''.
  IF SY-SUBRC = 0.
    IF UV_PARTN_CAT = '1'. "Person
      "Person
*      ls_bp-partner-central_data-common-data-bp_person-prefix1      =
      LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_PERSON-FIRSTNAME            = LS_ADDRESS_TH-NAME1.
      LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_PERSON-LASTNAME             = LS_ADDRESS_TH-NAME2.
      LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_PERSON-CORRESPONDLANGUAGE   = US_CENTRAL-PARTNERLANGUAGE.

      "Mark X
      LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_PERSON-FIRSTNAME           = ABAP_TRUE.
      LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_PERSON-LASTNAME            = ABAP_TRUE.
      LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_PERSON-CORRESPONDLANGUAGE  = ABAP_TRUE.

    ELSEIF UV_PARTN_CAT = '2'. "Organization
      LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_ORGANIZATION-NAME1  = LS_ADDRESS_TH-NAME1.
      LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_ORGANIZATION-NAME2  = LS_ADDRESS_TH-NAME2.
      LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_ORGANIZATION-NAME3  = LS_ADDRESS_TH-NAME3.
      LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_ORGANIZATION-NAME4  = LS_ADDRESS_TH-NAME4.

      "Mark X
      LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_ORGANIZATION-NAME1 = ABAP_TRUE.
      LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_ORGANIZATION-NAME2 = ABAP_TRUE.
      LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_ORGANIZATION-NAME3 = ABAP_TRUE.
      LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_ORGANIZATION-NAME4 = ABAP_TRUE.
    ENDIF.

    "Communication
    "Mobile Phone
    REFRESH: LT_PHONE[].
    APPEND INITIAL LINE TO LT_PHONE ASSIGNING FIELD-SYMBOL(<LFS_PHONE>).
    <LFS_PHONE>-CONTACT-TASK  =  GC_TASK_INSERT.
    <LFS_PHONE>-CONTACT-DATA-TELEPHONE        = LS_ADDRESS_TH-MOBILE_PHONE.
    <LFS_PHONE>-CONTACT-DATA-R_3_USER         = '3'.
    <LFS_PHONE>-CONTACT-DATAX-TELEPHONE       = ABAP_TRUE.
    <LFS_PHONE>-CONTACT-DATAX-R_3_USER        = ABAP_TRUE.

    "Phone
    APPEND INITIAL LINE TO LT_PHONE ASSIGNING <LFS_PHONE>.
    <LFS_PHONE>-CONTACT-TASK                 = GC_TASK_INSERT.
    <LFS_PHONE>-CONTACT-DATA-TELEPHONE       = LS_ADDRESS_TH-TEL_NUMBER.
    <LFS_PHONE>-CONTACT-DATAX-TELEPHONE      = ABAP_TRUE.

    "Fax
    APPEND INITIAL LINE TO LT_FAX ASSIGNING FIELD-SYMBOL(<LFS_FAX>).
    <LFS_FAX>-CONTACT-TASK                  = GC_TASK_INSERT.
    <LFS_FAX>-CONTACT-DATA-FAX              = LS_ADDRESS_TH-FAX_NUMBER.
    <LFS_FAX>-CONTACT-DATAX-FAX             = ABAP_TRUE.

    "Email
    APPEND INITIAL LINE TO LT_SMTP ASSIGNING FIELD-SYMBOL(<LFS_SMTP>).
    <LFS_SMTP>-CONTACT-TASK                 = GC_TASK_INSERT.
    <LFS_SMTP>-CONTACT-DATA-E_MAIL          = LS_ADDRESS_TH-EMAIL.
    <LFS_SMTP>-CONTACT-DATAX-E_MAIL         = ABAP_TRUE.

    LS_BP-PARTNER-CENTRAL_DATA-COMMUNICATION-PHONE-PHONE = LT_PHONE[].
    LS_BP-PARTNER-CENTRAL_DATA-COMMUNICATION-FAX-FAX     = LT_FAX[].
    LS_BP-PARTNER-CENTRAL_DATA-COMMUNICATION-SMTP-SMTP   = LT_SMTP[].

*------------------------------------------------------------------------------
* Address data
*------------------------------------------------------------------------------
    APPEND INITIAL LINE TO LT_ADDRESS ASSIGNING FIELD-SYMBOL(<LFS_ADDRESS>).
    <LFS_ADDRESS>-TASK = GC_TASK_INSERT.
* Operations are store in table TB008S
    <LFS_ADDRESS>-DATA_KEY-OPERATION           = 'XXDFLT'. "Standard operation
    <LFS_ADDRESS>-DATA-POSTAL-DATA-POSTL_COD1  = LS_ADDRESS_TH-POSTL_COD1.
    <LFS_ADDRESS>-DATA-POSTAL-DATA-STREET      = LS_ADDRESS_TH-STREET.
    <LFS_ADDRESS>-DATA-POSTAL-DATA-COUNTRY     = LS_ADDRESS_TH-COUNTRY.
    <LFS_ADDRESS>-DATA-POSTAL-DATA-LANGU       = 'E'.     "TH
    <LFS_ADDRESS>-DATA-POSTAL-DATA-REGION      = LS_ADDRESS_TH-REGION.
    <LFS_ADDRESS>-DATA-POSTAL-DATA-CITY        = LS_ADDRESS_TH-CITY.
    <LFS_ADDRESS>-DATA-POSTAL-DATA-DISTRICT    = LS_ADDRESS_TH-DISTRICT.
    <LFS_ADDRESS>-DATA-POSTAL-DATA-STR_SUPPL1  = LS_ADDRESS_TH-STR_SUPPL1.
    <LFS_ADDRESS>-DATA-POSTAL-DATA-STR_SUPPL2  = LS_ADDRESS_TH-STR_SUPPL2.
    <LFS_ADDRESS>-DATA-POSTAL-DATA-STR_SUPPL3  = LS_ADDRESS_TH-STR_SUPPL3.
    <LFS_ADDRESS>-DATA-POSTAL-DATA-LOCATION    = LS_ADDRESS_TH-LOCATION.
    <LFS_ADDRESS>-DATA-POSTAL-DATA-TRANSPZONE  = LS_ADDRESS_TH-TRANSPZONE.

    "Phone
    <LFS_ADDRESS>-DATA-COMMUNICATION-PHONE-PHONE  = LT_PHONE[].
    <LFS_ADDRESS>-DATA-COMMUNICATION-FAX-FAX      = LT_FAX[].
    <LFS_ADDRESS>-DATA-COMMUNICATION-SMTP-SMTP    = LT_SMTP[].

* Mark as changed
    <LFS_ADDRESS>-DATA-POSTAL-DATAX-CITY          = ABAP_TRUE.
    <LFS_ADDRESS>-DATA-POSTAL-DATAX-POSTL_COD1    = ABAP_TRUE.
    <LFS_ADDRESS>-DATA-POSTAL-DATAX-STREET        = ABAP_TRUE.
    <LFS_ADDRESS>-DATA-POSTAL-DATAX-COUNTRY       = ABAP_TRUE.
    <LFS_ADDRESS>-DATA-POSTAL-DATAX-REGION        = ABAP_TRUE.
    <LFS_ADDRESS>-DATA-POSTAL-DATAX-LANGU         = ABAP_TRUE.
    <LFS_ADDRESS>-DATA-POSTAL-DATAX-DISTRICT      = ABAP_TRUE.
    <LFS_ADDRESS>-DATA-POSTAL-DATAX-STR_SUPPL1    = ABAP_TRUE.
    <LFS_ADDRESS>-DATA-POSTAL-DATAX-STR_SUPPL2    = ABAP_TRUE.
    <LFS_ADDRESS>-DATA-POSTAL-DATAX-STR_SUPPL3    = ABAP_TRUE.
    <LFS_ADDRESS>-DATA-POSTAL-DATAX-LOCATION      = ABAP_TRUE.
    <LFS_ADDRESS>-DATA-POSTAL-DATAX-TRANSPZONE    = ABAP_TRUE.

  ENDIF.

  "Address (International version)
  READ TABLE UT_ADDRESS INTO DATA(LS_ADDRESS_EN)
                      WITH KEY INTERNATIONAL_VERSION = 'X'.
  IF SY-SUBRC = 0.
    IF <LFS_ADDRESS> IS NOT ASSIGNED.
      APPEND INITIAL LINE TO LT_ADDRESS ASSIGNING <LFS_ADDRESS>.
    ENDIF.

    APPEND INITIAL LINE TO LT_ADDR_VER ASSIGNING FIELD-SYMBOL(<LFS_ADDR_VER>).

    IF UV_PARTN_CAT = '1'.            "Person.
      <LFS_ADDR_VER>-DATA-PERSON-FIRSTNAME    = LS_ADDRESS_EN-NAME1.
      <LFS_ADDR_VER>-DATA-PERSON-LASTNAME     = LS_ADDRESS_EN-NAME2.
      <LFS_ADDR_VER>-DATA-PERSON-ADDR_VERS    = 'I'.
      <LFS_ADDR_VER>-DATA-PERSON-CITY         = LS_ADDRESS_EN-CITY.
      <LFS_ADDR_VER>-DATA-PERSON-STREET       = LS_ADDRESS_EN-STREET.
      <LFS_ADDR_VER>-DATA-PERSON-SORT1_P      = US_CENTRAL-SEARCHTERM1.
      <LFS_ADDR_VER>-DATA-PERSON-DISTRICT     = LS_ADDRESS_EN-DISTRICT.
      <LFS_ADDR_VER>-DATA-PERSON-STR_SUPPL1   = LS_ADDRESS_EN-STR_SUPPL1.
      <LFS_ADDR_VER>-DATA-PERSON-STR_SUPPL2   = LS_ADDRESS_EN-STR_SUPPL2.
      <LFS_ADDR_VER>-DATA-PERSON-STR_SUPPL3   = LS_ADDRESS_EN-STR_SUPPL3.
      <LFS_ADDR_VER>-DATA-PERSON-LOCATION     = LS_ADDRESS_EN-LOCATION.

      <LFS_ADDR_VER>-DATAX-PERSON-FIRSTNAME         = ABAP_TRUE.
      <LFS_ADDR_VER>-DATAX-PERSON-LASTNAME          = ABAP_TRUE.
      <LFS_ADDR_VER>-DATAX-PERSON-ADDR_VERS         = ABAP_TRUE.
      <LFS_ADDR_VER>-DATAX-PERSON-CITY              = ABAP_TRUE.
      <LFS_ADDR_VER>-DATAX-PERSON-STREET            = ABAP_TRUE.
      <LFS_ADDR_VER>-DATAX-PERSON-SORT1_P           = ABAP_TRUE.
      <LFS_ADDR_VER>-DATAX-PERSON-DISTRICT          = ABAP_TRUE.
      <LFS_ADDR_VER>-DATAX-PERSON-STR_SUPPL1        = ABAP_TRUE.
      <LFS_ADDR_VER>-DATAX-PERSON-STR_SUPPL2        = ABAP_TRUE.
      <LFS_ADDR_VER>-DATAX-PERSON-STR_SUPPL3        = ABAP_TRUE.
      <LFS_ADDR_VER>-DATAX-PERSON-LOCATION          = ABAP_TRUE.

    ELSEIF UV_PARTN_CAT = '2'.        "Org.
      <LFS_ADDR_VER>-DATA-ORGANIZATION-ADDR_VERS    = 'I'.
      <LFS_ADDR_VER>-DATA-ORGANIZATION-NAME         = LS_ADDRESS_EN-NAME1.
      <LFS_ADDR_VER>-DATA-ORGANIZATION-NAME_2       = LS_ADDRESS_EN-NAME2.
      <LFS_ADDR_VER>-DATA-ORGANIZATION-NAME_3       = LS_ADDRESS_EN-NAME3.
      <LFS_ADDR_VER>-DATA-ORGANIZATION-NAME_4       = LS_ADDRESS_EN-NAME4.
      <LFS_ADDR_VER>-DATA-ORGANIZATION-CITY         = LS_ADDRESS_EN-CITY.
      <LFS_ADDR_VER>-DATA-ORGANIZATION-STREET       = LS_ADDRESS_EN-STREET.
      <LFS_ADDR_VER>-DATA-ORGANIZATION-LOCATION     = LS_ADDRESS_EN-LOCATION.
      <LFS_ADDR_VER>-DATA-ORGANIZATION-SORT1        = US_CENTRAL-SEARCHTERM1.
      <LFS_ADDR_VER>-DATA-ORGANIZATION-DISTRICT     = LS_ADDRESS_EN-DISTRICT.
      <LFS_ADDR_VER>-DATA-ORGANIZATION-STR_SUPPL1   = LS_ADDRESS_EN-STR_SUPPL1.
      <LFS_ADDR_VER>-DATA-ORGANIZATION-STR_SUPPL2   = LS_ADDRESS_EN-STR_SUPPL2.
      <LFS_ADDR_VER>-DATA-ORGANIZATION-STR_SUPPL3   = LS_ADDRESS_EN-STR_SUPPL3.
      <LFS_ADDR_VER>-DATA-ORGANIZATION-LOCATION     = LS_ADDRESS_EN-LOCATION.

      <LFS_ADDR_VER>-DATAX-ORGANIZATION-ADDR_VERS     = ABAP_TRUE.
      <LFS_ADDR_VER>-DATAX-ORGANIZATION-NAME          = ABAP_TRUE.
      <LFS_ADDR_VER>-DATAX-ORGANIZATION-NAME_2        = ABAP_TRUE.
      <LFS_ADDR_VER>-DATAX-ORGANIZATION-NAME_3        = ABAP_TRUE.
      <LFS_ADDR_VER>-DATAX-ORGANIZATION-NAME_4        = ABAP_TRUE.
      <LFS_ADDR_VER>-DATAX-ORGANIZATION-CITY          = ABAP_TRUE.
      <LFS_ADDR_VER>-DATAX-ORGANIZATION-STREET        = ABAP_TRUE.
      <LFS_ADDR_VER>-DATAX-ORGANIZATION-LOCATION      = ABAP_TRUE.
      <LFS_ADDR_VER>-DATAX-ORGANIZATION-SORT1         = ABAP_TRUE.
      <LFS_ADDR_VER>-DATAX-ORGANIZATION-DISTRICT      = ABAP_TRUE.
      <LFS_ADDR_VER>-DATAX-ORGANIZATION-STR_SUPPL1    = ABAP_TRUE.
      <LFS_ADDR_VER>-DATAX-ORGANIZATION-STR_SUPPL2    = ABAP_TRUE.
      <LFS_ADDR_VER>-DATAX-ORGANIZATION-STR_SUPPL3    = ABAP_TRUE.
      <LFS_ADDR_VER>-DATAX-ORGANIZATION-LOCATION      = ABAP_TRUE.

    ENDIF.
    <LFS_ADDR_VER>-TASK                          = GC_TASK_INSERT.
    <LFS_ADDRESS>-DATA-VERSION-VERSIONS          = LT_ADDR_VER.
  ENDIF.

* Add address to main structure
  LS_BP-PARTNER-CENTRAL_DATA-ADDRESS-ADDRESSES = LT_ADDRESS.

*------------------------------------------------------------------------------
* Roles
*------------------------------------------------------------------------------
  "BP > CUSTOMER > FI CUSTOMER > VENDOR > FI VENDOR
  CLEAR: GV_CUSTOMER_FLAG, GV_VENDOR_FLAG.
  LOOP AT UT_BP_ROLE INTO DATA(LS_BP_ROLE).

    "Append Main role first
    CLEAR LS_ROLE.
    MOVE LS_BP_ROLE-ROLE TO LS_ROLE-DATA_KEY.
    APPEND LS_ROLE TO LT_ROLE.

    CLEAR LS_ROLE.
    IF LS_BP_ROLE-ROLE = GC_ROLE-VENDOR.        "Vendor case
      GV_VENDOR_FLAG = ABAP_TRUE.
      LS_ROLE-DATA_KEY = GC_ROLE-VENDOR_FI.           "FI Vendor
      APPEND LS_ROLE TO LT_ROLE.
    ELSEIF LS_BP_ROLE-ROLE = GC_ROLE-CUSTOMER.  "Customer Case
      GV_CUSTOMER_FLAG = ABAP_TRUE.
      LS_ROLE-DATA_KEY = GC_ROLE-CUSTOMER_FI.         "FI Customer
      APPEND LS_ROLE TO LT_ROLE.

      IF UV_BU_GROUP <> GC_GROUP-EMP AND
         US_CUSTOMER <> ABAP_TRUE.
        LS_ROLE-DATA_KEY = GC_ROLE-CREDIT.              "Credit Management
        APPEND LS_ROLE TO LT_ROLE.
      ENDIF.

    ENDIF.
  ENDLOOP.

* Add role to main structure
  LS_BP-PARTNER-CENTRAL_DATA-ROLE-ROLES = LT_ROLE.

  "Partner relation ship
  DATA: LT_RELATION TYPE BURS_EI_EXTERN_T.
  DATA(LT_CONTACT_EXISTS) = CT_CONTACT[].
  DELETE LT_CONTACT_EXISTS WHERE PARTNER2 IS INITIAL.
  LOOP AT LT_CONTACT_EXISTS INTO DATA(LS_CONTACT_EXISTS).
*    ls_contact_exists =
    APPEND INITIAL LINE TO LT_RELATION ASSIGNING FIELD-SYMBOL(<LFS_RELATION>).
    <LFS_RELATION>-HEADER-OBJECT                          = GC_TASK_INSERT.
    <LFS_RELATION>-HEADER-OBJECT_INSTANCE-PARTNER2        = LS_CONTACT_EXISTS-PARTNER2.
    <LFS_RELATION>-HEADER-OBJECT_INSTANCE-RELAT_CATEGORY  = 'BUR001'.
    <LFS_RELATION>-HEADER-OBJECT_INSTANCE-DATE_TO        = '99991231'.
  ENDLOOP.

  LS_BP-PARTNER_RELATION = LT_RELATION[].

*------------------------------------------------------------------------------
* Customer Data
*------------------------------------------------------------------------------
  "CUSTOMER
  IF GV_CUSTOMER_FLAG = ABAP_TRUE.

    LOOP AT GT_VTWEG INTO DATA(LS_VTWEG).

      REFRESH: LT_PARTNER_FUNC[].

      "sale data
      APPEND INITIAL LINE TO LT_CUS_SALES ASSIGNING FIELD-SYMBOL(<LFS_CUS_SALES>).

      <LFS_CUS_SALES>-TASK           = GC_TASK_INSERT.
      <LFS_CUS_SALES>-DATA_KEY-VKORG = GC_SALES_AREA-VKORG.
      <LFS_CUS_SALES>-DATA_KEY-VTWEG = LS_VTWEG-LOW.
      <LFS_CUS_SALES>-DATA_KEY-SPART = GC_SALES_AREA-SPART.

      <LFS_CUS_SALES>-DATA-KVGR1    = US_CUSTOMER-KVGR1.    "Customer Group 1
      <LFS_CUS_SALES>-DATA-KVGR2    = US_CUSTOMER-KVGR2.    "Customer Group 2
      <LFS_CUS_SALES>-DATA-KVGR3    = US_CUSTOMER-KVGR3.    "Customer Group 3
      <LFS_CUS_SALES>-DATA-KVGR4    = US_CUSTOMER-KVGR4.    "Customer Group 4
      <LFS_CUS_SALES>-DATA-KVGR5    = US_CUSTOMER-KVGR5.    "Customer Group 5
      <LFS_CUS_SALES>-DATA-ZTERM    = US_CUSTOMER-ZTERM.    "Terms of Payment Key
      <LFS_CUS_SALES>-DATA-KTGRD    = US_CUSTOMER-KTGRD.    "Account Assignment Group for Customer
      <LFS_CUS_SALES>-DATA-KALKS    = '1'.                  "Customer Classification for Pricing Procedure Determination    #FIX
      <LFS_CUS_SALES>-DATA-KONDA    = '01'.                 "Price Group    #FIX
      <LFS_CUS_SALES>-DATA-WAERS    = US_CENTRAL-CURRENCY.  "Currency
      <LFS_CUS_SALES>-DATA-VWERK    = GC_SDS.               "Delivering Plant (Own or External) #FIX
      <LFS_CUS_SALES>-DATA-AWAHR    = '100'.                "Order Probability of the Item  #FIX
      <LFS_CUS_SALES>-DATA-KKBER    = GC_SDS.               "Credit Control Area #FIX
      <LFS_CUS_SALES>-DATA-VKGRP    = US_CUSTOMER-VKGRP.    "sales group
      <LFS_CUS_SALES>-DATA-VKBUR    = US_CUSTOMER-VKBUR.    "sales office
      <LFS_CUS_SALES>-DATA-VSBED    = US_CUSTOMER-VSBED.    "Shipping Conditions
      <LFS_CUS_SALES>-DATA-INCO1    = US_CUSTOMER-INCO1.    "Incoterms
      <LFS_CUS_SALES>-DATA-INCO2_L  = US_CUSTOMER-INCO2_L.  "Incoterms
      <LFS_CUS_SALES>-DATA-KDGRP    = US_CUSTOMER-KDGRP.    "Customer Group
      <LFS_CUS_SALES>-DATA-VERSG    = '1'.                  "Customer Statistics Group
      <LFS_CUS_SALES>-DATA-AGREL    = ABAP_TRUE.            "Relevant for Settlement Management
      <LFS_CUS_SALES>-DATA-KZAZU    = ABAP_TRUE.            "Order Combination Indicator
      <LFS_CUS_SALES>-DATA-VERSG    = '1'.                  "Customer Statistics Group

      "mark x
      <LFS_CUS_SALES>-DATAX-KVGR1     = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-KVGR2     = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-KVGR3     = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-KVGR4     = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-KVGR5     = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-ZTERM     = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-KTGRD     = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-KALKS     = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-KONDA     = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-WAERS     = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-VKBUR     = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-VKGRP     = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-AWAHR     = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-VSBED     = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-INCO1     = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-KKBER     = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-INCO2_L   = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-KDGRP     = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-VWERK     = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-AGREL     = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-KZAZU     = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-VERSG     = ABAP_TRUE.

      "Partner Function
      "--Sale Emp.
      APPEND INITIAL LINE TO LT_PARTNER_FUNC ASSIGNING FIELD-SYMBOL(<LFS_PARTNER_FUNC>).
      <LFS_PARTNER_FUNC>-TASK = GC_TASK_INSERT.
      <LFS_PARTNER_FUNC>-DATA_KEY-PARVW = GC_PARTNER_FN-S_EMP.
      <LFS_PARTNER_FUNC>-DATA_KEY-PARZA = '001'.
      <LFS_PARTNER_FUNC>-DATA-PARTNER   = US_CUSTOMER-SALES_EMP.
      <LFS_PARTNER_FUNC>-DATAX-PARTNER  = ABAP_TRUE.
      <LFS_CUS_SALES>-FUNCTIONS-FUNCTIONS = LT_PARTNER_FUNC[].

    ENDLOOP.

    LS_BP-CUSTOMER-SALES_DATA-SALES = LT_CUS_SALES[].

    APPEND INITIAL LINE TO LT_TAX_IND ASSIGNING FIELD-SYMBOL(<LFS_TAX_IND>).
    <LFS_TAX_IND>-TASK            = GC_TASK_INSERT.
    <LFS_TAX_IND>-DATA_KEY-ALAND  = 'TH'.
    <LFS_TAX_IND>-DATA_KEY-TATYP  = 'MWST'.
    <LFS_TAX_IND>-DATA-TAXKD      = US_CUSTOMER-TAXKD.
    <LFS_TAX_IND>-DATAX-TAXKD     = ABAP_TRUE.
    LS_BP-CUSTOMER-CENTRAL_DATA-TAX_IND-TAX_IND    = LT_TAX_IND[].

*    "Customer Classification
*    ls_bp-customer-central_data-central-data-kukla    = us_customer-kukla.
*    ls_bp-customer-central_data-central-datax-kukla   = abap_true.

    "Customer Company
    APPEND INITIAL LINE TO LT_CUS_COMPANY[] ASSIGNING FIELD-SYMBOL(<LFS_CUS_COMPANY>).
    <LFS_CUS_COMPANY>-TASK            = GC_TASK_INSERT.
    <LFS_CUS_COMPANY>-DATA_KEY-BUKRS  = GC_SDS.

    <LFS_CUS_COMPANY>-DATA-ZTERM     = US_CUSTOMER-ZTERM.      "Term of Payment
    <LFS_CUS_COMPANY>-DATA-XZVER     = ABAP_TRUE.              "Indicator: Record Payment History ?
    <LFS_CUS_COMPANY>-DATA-BUSAB     = US_CUSTOMER-BUSAB.      "Accounting Clerk Abbreviation
    <LFS_CUS_COMPANY>-DATA-AKONT     = US_CUSTOMER-AKONT.      "Reconciliation Account in General Ledger
*    <lfs_cus_company>-data-fdgrv     = us_customer-fdgrv.     "Planning Group
    SELECT SINGLE *
      FROM ZSDSFIT032
      INTO @DATA(LS_MAPPLING_GL)
      WHERE AKONT = @<LFS_CUS_COMPANY>-DATA-AKONT.
    IF SY-SUBRC = 0.
      <LFS_CUS_COMPANY>-DATA-FDGRV = LS_MAPPLING_GL-FDGRV.    "Planning Group
    ENDIF.
    <LFS_CUS_COMPANY>-DATA-XAUSZ     = '2'.                   "Indicator for Periodic Account Statements
    <LFS_CUS_COMPANY>-DATA-ZUAWA     = US_CUSTOMER-ZUAWA.     "Key for sorting according to assignment numbers

    "markX
    <LFS_CUS_COMPANY>-DATAX-FDGRV     = ABAP_TRUE.
    <LFS_CUS_COMPANY>-DATAX-ZTERM     = ABAP_TRUE.
    <LFS_CUS_COMPANY>-DATAX-XZVER     = ABAP_TRUE.
    <LFS_CUS_COMPANY>-DATAX-BUSAB     = ABAP_TRUE.
    <LFS_CUS_COMPANY>-DATAX-AKONT     = ABAP_TRUE.
    <LFS_CUS_COMPANY>-DATAX-XAUSZ     = ABAP_TRUE.
    <LFS_CUS_COMPANY>-DATAX-ZUAWA     = ABAP_TRUE.

    "Withholding Tax [ R1 & R2 ]
    CLEAR LV_WHT_NO. "set 0
    WHILE LV_WHT_NO < 2.
      LV_WHT_NO = LV_WHT_NO + 1.
      APPEND INITIAL LINE TO LT_TAX_CUST[] ASSIGNING FIELD-SYMBOL(<LFS_TAX_CUST>).
      <LFS_TAX_CUST>-TASK             = GC_TASK_INSERT.
      <LFS_TAX_CUST>-DATA_KEY-WITHT   = |R{ LV_WHT_NO }|.
      <LFS_TAX_CUST>-DATA-WT_AGENT    = ABAP_TRUE.
      <LFS_TAX_CUST>-DATA-QSREC       = US_CENTRAL-QSREC.
      <LFS_TAX_CUST>-DATA-WT_AGTDF    = '20210101'.
      <LFS_TAX_CUST>-DATA-WT_AGTDT    = LC_VALIDTO.

      "markX
      <LFS_TAX_CUST>-DATAX-WT_AGENT   = ABAP_TRUE.
      <LFS_TAX_CUST>-DATAX-QSREC      = ABAP_TRUE.
      <LFS_TAX_CUST>-DATAX-WT_AGTDF   = ABAP_TRUE.
      <LFS_TAX_CUST>-DATAX-WT_AGTDT   = ABAP_TRUE.
    ENDWHILE.
    <LFS_CUS_COMPANY>-WTAX_TYPE-WTAX_TYPE     = LT_TAX_CUST[].

    "Dunning
    APPEND INITIAL LINE TO LT_CUS_DUNNING[] ASSIGNING FIELD-SYMBOL(<LFS_CUS_DUNNING>).
    <LFS_CUS_DUNNING>-TASK              = GC_TASK_INSERT.
    <LFS_CUS_DUNNING>-DATA-MAHNA        = US_CUSTOMER-MAHNA.
    <LFS_CUS_DUNNING>-DATAX-MAHNA       = ABAP_TRUE.
    <LFS_CUS_COMPANY>-DUNNING-DUNNING   = LT_CUS_DUNNING[].
    LS_BP-CUSTOMER-COMPANY_DATA-COMPANY = LT_CUS_COMPANY.
  ENDIF.

*------------------------------------------------------------------------------
* Vendor :
*------------------------------------------------------------------------------
  IF GV_VENDOR_FLAG = ABAP_TRUE.

    "Purchase DATA
    APPEND INITIAL LINE TO LT_VEN_PUR ASSIGNING FIELD-SYMBOL(<LFS_VEN_PUR>).

    <LFS_VEN_PUR>-TASK             = GC_TASK_INSERT.
    <LFS_VEN_PUR>-DATA_KEY-EKORG   = GC_SDS.
    <LFS_VEN_PUR>-DATA-ZTERM       = US_VENDOR-ZTERM.
    <LFS_VEN_PUR>-DATA-WAERS       = US_CENTRAL-CURRENCY.

    LS_BP-VENDOR-PURCHASING_DATA-PURCHASING = LT_VEN_PUR[].

    APPEND INITIAL LINE TO LT_VEN_COMPANY ASSIGNING FIELD-SYMBOL(<LFS_VEN_COMPANY>).

    "insert
    <LFS_VEN_COMPANY>-TASK            = GC_TASK_INSERT.
    <LFS_VEN_COMPANY>-DATA_KEY-BUKRS  = GC_SDS.

    "Data
    <LFS_VEN_COMPANY>-DATA-ZTERM      = US_VENDOR-ZTERM.    "Terms of Payment Key
    <LFS_VEN_COMPANY>-DATA-AKONT      = US_VENDOR-AKONT.    "Reconciliation Account in General Ledger
    <LFS_VEN_COMPANY>-DATA-ZUAWA      = US_VENDOR-ZUAWA.    "SortKey
    <LFS_VEN_COMPANY>-DATA-REPRF      = ABAP_TRUE.          "Check Flag for Double Invoices
    <LFS_VEN_COMPANY>-DATA-FDGRV      = US_VENDOR-FDGRV.    "Planning Group

    "Mark X
    <LFS_VEN_COMPANY>-DATAX-ZTERM     = ABAP_TRUE.
    <LFS_VEN_COMPANY>-DATAX-AKONT     = ABAP_TRUE.
    <LFS_VEN_COMPANY>-DATAX-FDGRV     = ABAP_TRUE.
    <LFS_VEN_COMPANY>-DATAX-ZUAWA     = ABAP_TRUE.
    <LFS_VEN_COMPANY>-DATAX-REPRF     = ABAP_TRUE.

    "WHT
    "11
    APPEND INITIAL LINE TO LT_TAX_VEND ASSIGNING FIELD-SYMBOL(<LFS_TAX_VEND>).
    <LFS_TAX_VEND>-TASK             = GC_TASK_INSERT.
    <LFS_TAX_VEND>-DATA_KEY-WITHT   = '11'.
    <LFS_TAX_VEND>-DATA-QSREC       = US_CENTRAL-QSREC.
    <LFS_TAX_VEND>-DATA-WT_SUBJCT   = ABAP_TRUE.
    <LFS_TAX_VEND>-DATAX-WT_SUBJCT  = ABAP_TRUE.
    <LFS_TAX_VEND>-DATAX-QSREC      = ABAP_TRUE.

    "12
    APPEND INITIAL LINE TO LT_TAX_VEND ASSIGNING <LFS_TAX_VEND>.
    <LFS_TAX_VEND>-TASK             = GC_TASK_INSERT.
    <LFS_TAX_VEND>-DATA_KEY-WITHT   = '12'.
    <LFS_TAX_VEND>-DATA-QSREC       = US_CENTRAL-QSREC.
    <LFS_TAX_VEND>-DATA-WT_SUBJCT   = ABAP_TRUE.
    <LFS_TAX_VEND>-DATAX-WT_SUBJCT  = ABAP_TRUE.
    <LFS_TAX_VEND>-DATAX-QSREC      = ABAP_TRUE.

    "Vendor
    <LFS_VEN_COMPANY>-WTAX_TYPE-WTAX_TYPE   = LT_TAX_VEND[].

    LS_BP-VENDOR-COMPANY_DATA-COMPANY       = LT_VEN_COMPANY[].   "Assing Vendor Company Data

*    ls_bp-vendor-central_data-central-data-vbund  = uv_bp+4(6).
*    ls_bp-vendor-central_data-central-datax-vbund = abap_true.

  ENDIF.

*------------------------------------------------------------------------------
* SAP Credit Management:
*------------------------------------------------------------------------------
  IF GV_CUSTOMER_FLAG = ABAP_TRUE AND UV_BU_GROUP <> GC_GROUP-EMP.

    IF NOT ( UV_BU_GROUP = GC_GROUP-NAF AND US_CUSTOMER-BUSAB <> 'ZZ' ).
      "-Profile
      LS_BP-PARTNER-UKMBP_DATA-PROFILE-DATA-CHECK_RULE    = 'Z1'.
      LS_BP-PARTNER-UKMBP_DATA-PROFILE-DATA-LIMIT_RULE    = 'B2B-EXIST'.
      LS_BP-PARTNER-UKMBP_DATA-PROFILE-DATA-CREDIT_GROUP  = '1'.
      LS_BP-PARTNER-UKMBP_DATA-PROFILE-DATA-RISK_CLASS    = 'B'.

      "MarkX
      LS_BP-PARTNER-UKMBP_DATA-PROFILE-DATAX-CHECK_RULE   = ABAP_TRUE.
      LS_BP-PARTNER-UKMBP_DATA-PROFILE-DATAX-LIMIT_RULE   = ABAP_TRUE.
      LS_BP-PARTNER-UKMBP_DATA-PROFILE-DATAX-CREDIT_GROUP = ABAP_TRUE.
      LS_BP-PARTNER-UKMBP_DATA-PROFILE-DATAX-RISK_CLASS   = ABAP_TRUE.

      "-Credit segment Data
      APPEND INITIAL LINE TO LT_CRE_SEGMENT ASSIGNING FIELD-SYMBOL(<LFS_CRE_SEGMENT>).
      <LFS_CRE_SEGMENT>-TASK                      = GC_TASK_INSERT.
      <LFS_CRE_SEGMENT>-DATA_KEY-CREDIT_SGMNT     = GC_SDS.

      "DATA
      <LFS_CRE_SEGMENT>-DATA-CREDIT_LIMIT         = 1.
      <LFS_CRE_SEGMENT>-DATA-LIMIT_VALID_DATE     = LC_VALIDTO.

      "MARKX
      <LFS_CRE_SEGMENT>-DATAX-CREDIT_LIMIT      = ABAP_TRUE.

      LS_BP-PARTNER-UKMBP_DATA-SEGMENTS-SEGMENTS = LT_CRE_SEGMENT[].
    ENDIF.

  ENDIF.

*------------------------------------------------------------------------------
* Validate data
*------------------------------------------------------------------------------
  CL_MD_BP_MAINTAIN=>VALIDATE_SINGLE(
    EXPORTING
      I_DATA        = LS_BP
    IMPORTING
      ET_RETURN_MAP = DATA(LT_RETURN_MAP)
  ).

  "Filter message out ->Errors occurred during call of function module BUPA_CREATE_FROM_DATA
  PERFORM FILTER_ERROR_OUT  CHANGING LT_RETURN_MAP.

  IF LINE_EXISTS( LT_RETURN_MAP[ TYPE = 'E' ] ) OR
     LINE_EXISTS( LT_RETURN_MAP[ TYPE = 'A' ] ).

    LOOP AT LT_RETURN_MAP INTO DATA(LS_RETURN_MAP)
                          WHERE TYPE = 'E'
                             OR TYPE = 'A'.

      MOVE-CORRESPONDING LS_RETURN_MAP TO LS_RETURN.
      APPEND LS_RETURN TO CT_RETURN.
      EXIT. "exit loop

    ENDLOOP.

    EXIT. "Exit program
  ENDIF.

*------------------------------------------------------------------------------
* Call API
*------------------------------------------------------------------------------
* Add single BP to IMPORTING table
  INSERT LS_BP INTO TABLE LT_BP.

  CL_MD_BP_MAINTAIN=>MAINTAIN(
    EXPORTING
      I_DATA     = LT_BP
      I_TEST_RUN = UV_TEST
    IMPORTING
      E_RETURN   = LT_RETURN
  ).

  "Error case
  READ TABLE LT_RETURN INTO DATA(LS_RETURN2)
                       INDEX 1.
  LOOP AT LS_RETURN2-OBJECT_MSG INTO DATA(LS_OBJ_MSG).
    IF LS_OBJ_MSG-TYPE = 'E' OR LS_OBJ_MSG-TYPE = 'A'.
*       Error occurred
      LV_ERROR = ABAP_TRUE.
      MOVE-CORRESPONDING LS_OBJ_MSG TO LS_RETURN.
      APPEND LS_RETURN TO CT_RETURN.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF LV_ERROR IS INITIAL.
    CASE UV_TEST.
      WHEN ABAP_TRUE.
*       Test mode
        LS_RETURN-MESSAGE = |BP data is ok.|.
        LS_RETURN-TYPE    = 'S'.
        APPEND LS_RETURN TO CT_RETURN.
      WHEN ABAP_FALSE.
*       Non-test mode => Perform COMMIT
        PERFORM COMMIT_WORK.
*       Get number of new BP (it's not returned by the API)
        IMPORT LV_PARTNER TO LV_BU_PARTNER FROM MEMORY ID 'BUP_MEMORY_PARTNER'.

        LS_RETURN-MESSAGE       = |Business Partner { LV_BU_PARTNER } has been created.|.
        LS_RETURN-TYPE          = 'S'.
        LS_RETURN-MESSAGE_V1    = LV_BU_PARTNER.
        APPEND LS_RETURN TO CT_RETURN.

        CV_BP = LV_BU_PARTNER.

        PERFORM UPDATE_BRANCH     USING LV_BU_PARTNER
                                        UV_PARTN_CAT
                                        US_CENTRAL.

        PERFORM ADD_BANK_DETAIL   USING LV_BU_PARTNER
                                        US_VENDOR.

        PERFORM UPDATE_CUST_OTHER USING LV_BU_PARTNER
                                        US_CUSTOMER.

        "Create contact person
        PERFORM CRE_CONTACT_PERSON USING LV_BU_PARTNER
                                         SPACE
                                   CHANGING CT_CONTACT
                                            CT_RETURN.

        "Create Shipto
        PERFORM CRE_SHIPTO_PFN     USING LV_BU_PARTNER
                                         SPACE
                                   CHANGING CT_PARTNER
                                            CT_RETURN.

        "Update Email to table ztax
        READ TABLE UT_ADDRESS INTO DATA(LS_ADDRESS)
                              INDEX 1.
        IF SY-SUBRC = 0.
          PERFORM UPD_ZSDSFIC001 USING LV_BU_PARTNER
                                       LS_ADDRESS-EMAIL.
        ENDIF.

    ENDCASE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form update_log_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM UPDATE_LOG_DATA USING UV_K2_REF   TYPE CHAR20
                           UV_SFDC_REF TYPE CHAR20
                           UV_PARTNER  TYPE BU_PARTNER
                           UV_CREATE   TYPE PERSNO
                           US_RETURN   TYPE BAPIRET2.


  DATA: LS_LOG_TABLE TYPE ZSDSFIT031.

  SELECT MAX( TRAN_ID )
    INTO @DATA(LV_TRAN_ID)
    FROM ZSDSFIT031.
  IF SY-SUBRC = 0.
    LV_TRAN_ID = LV_TRAN_ID + 1.
  ELSE.
    LV_TRAN_ID = 1.
  ENDIF.

  "Move data to structure
  LS_LOG_TABLE-K2_REFNO   = UV_K2_REF.
  LS_LOG_TABLE-SFDC_REFNO = UV_SFDC_REF.
  LS_LOG_TABLE-BPARTNER   = UV_PARTNER.
  LS_LOG_TABLE-TRAN_ID    = LV_TRAN_ID.
  LS_LOG_TABLE-ZDATE      = SY-DATUM.
  LS_LOG_TABLE-ZTIME      = SY-UZEIT.
  LS_LOG_TABLE-MSGTY      = US_RETURN-TYPE.
  LS_LOG_TABLE-MESSAGE    = US_RETURN-MESSAGE.
  LS_LOG_TABLE-PERNR      = UV_CREATE.

  SELECT SINGLE VORNA,
                NACHN
    FROM PA0002
    INTO @DATA(LS_PA0002)
    WHERE PERNR EQ @UV_CREATE.
  IF SY-SUBRC EQ 0.
    CONCATENATE LS_PA0002-VORNA LS_PA0002-NACHN INTO LS_LOG_TABLE-NAME SEPARATED BY SPACE.
  ENDIF.

  INSERT INTO ZSDSFIT031 VALUES LS_LOG_TABLE.

  IF SY-SUBRC = 0.
    PERFORM COMMIT_WORK.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form update_branch
*&---------------------------------------------------------------------*
FORM UPDATE_BRANCH      USING    UV_PARTNER   TYPE BU_PARTNER
                                 UV_PARTN_CAT TYPE BU_TYPE
                                 US_CENTRAL   TYPE ZSDSFIS003.


  DATA: LCL_FINLOC_CUSTOMER_UPDATE TYPE REF TO CL_FINLOC_CUSTOMER_UPDATE,
        LCL_FINLOC_VENDOR_UPDATE   TYPE REF TO CL_FINLOC_VENDOR_UPDATE,
        LS_FITHA_PBUPL_D           TYPE FITHA_PBUPL_D,
        LT_FITHA_PBUPL_D           TYPE TABLE OF FITHA_PBUPL_D,
        LS_FITHA_PBUPL_D_T         TYPE FITHA_PBUPL_D_T,
        LT_FITHA_PBUPL_D_T         TYPE TABLE OF FITHA_PBUPL_D_T,
        LS_FITHA_PBUPL_K           TYPE FITHA_PBUPL_K,
        LT_FITHA_PBUPL_K           TYPE TABLE OF FITHA_PBUPL_K,
        LS_FITHA_PBUPL_K_T         TYPE FITHA_PBUPL_K_T,
        LT_FITHA_PBUPL_K_T         TYPE TABLE OF FITHA_PBUPL_K_T.


** Customer *************************************************
  CREATE OBJECT LCL_FINLOC_CUSTOMER_UPDATE.

  "Update branch code
  LS_FITHA_PBUPL_D-KUNNR = UV_PARTNER.     "Customer

  IF US_CENTRAL-BCODE IS INITIAL.
    IF UV_PARTN_CAT = '1'. "Person
      LS_FITHA_PBUPL_D-J_1TPBUPL      = 'NoVat'.      "Branch Code
      LS_FITHA_PBUPL_D-DEFAULT_BRANCH = ABAP_TRUE.    "Default Branch
    ELSEIF UV_PARTN_CAT = '2'. "Org
      LS_FITHA_PBUPL_D-J_1TPBUPL      = '00000'.      "Branch Code
      LS_FITHA_PBUPL_D-DEFAULT_BRANCH   = ABAP_TRUE.    "Default Branch
    ENDIF.
  ELSE.
    LS_FITHA_PBUPL_D-J_1TPBUPL        = US_CENTRAL-BCODE.   "Branch Code
    LS_FITHA_PBUPL_D-DEFAULT_BRANCH   = ABAP_TRUE.          "Default Branch
  ENDIF.

  APPEND LS_FITHA_PBUPL_D TO LT_FITHA_PBUPL_D.

  CALL METHOD CL_FINLOC_CVI_PERSIST_DATA=>SET_FITHA_PBUPL_D(
    EXPORTING
      IT_FITHA_PBUPL_D = LT_FITHA_PBUPL_D
  ).
  CALL METHOD LCL_FINLOC_CUSTOMER_UPDATE->CS_UPDATE_FITHA_PBUPL_D.

  "Update Branch Code Description
  LS_FITHA_PBUPL_D_T-KUNNR = UV_PARTNER.     "Customer
  IF US_CENTRAL-BCODE IS INITIAL.
    IF UV_PARTN_CAT = '1'. "Person
      LS_FITHA_PBUPL_D_T-J_1TPBUPL        = 'NoVat'.  "Branch Code
      LS_FITHA_PBUPL_D_T-DESCRIPTION      = 'NoVat'.  "Branch Description
    ELSEIF UV_PARTN_CAT = '2'. "Org
      LS_FITHA_PBUPL_D_T-J_1TPBUPL        = '00000'.      "Branch Code
      LS_FITHA_PBUPL_D_T-DESCRIPTION      = GC_HEADOFF.     "Branch Description
    ENDIF.
  ELSE.
    LS_FITHA_PBUPL_D_T-J_1TPBUPL          = US_CENTRAL-BCODE.  "Branch Code
    LS_FITHA_PBUPL_D_T-DESCRIPTION        = US_CENTRAL-BDESC.  "Branch Description
    IF US_CENTRAL-BCODE = '00000'.
      LS_FITHA_PBUPL_D_T-DESCRIPTION      = GC_HEADOFF.
    ENDIF.
  ENDIF.
  LS_FITHA_PBUPL_D_T-SPRAS                = 'E'.

  APPEND LS_FITHA_PBUPL_D_T TO LT_FITHA_PBUPL_D_T.
  CALL METHOD CL_FINLOC_CVI_PERSIST_DATA=>SET_FITHA_PBUPL_D_T(
    EXPORTING
      IT_FITHA_PBUPL_D_T = LT_FITHA_PBUPL_D_T
  ).
  CALL METHOD LCL_FINLOC_CUSTOMER_UPDATE->CS_UPDATE_FITHA_PBUPL_D_T.


** Vendor ***************************************************
  CREATE OBJECT LCL_FINLOC_VENDOR_UPDATE.

  "Update branch code
  LS_FITHA_PBUPL_K-LIFNR = UV_PARTNER.     "Vendor

  IF US_CENTRAL-BCODE IS INITIAL.
    IF UV_PARTN_CAT = '1'. "Person
      LS_FITHA_PBUPL_K-J_1TPBUPL      = 'NoVat'.
      LS_FITHA_PBUPL_K-DEFAULT_BRANCH = ABAP_TRUE.
    ELSEIF UV_PARTN_CAT = '2'. "Org
      LS_FITHA_PBUPL_K-J_1TPBUPL      = '00000'.
      LS_FITHA_PBUPL_K-DEFAULT_BRANCH   = ABAP_TRUE.
    ENDIF.
  ELSE.
    LS_FITHA_PBUPL_K-J_1TPBUPL        = US_CENTRAL-BCODE.
    LS_FITHA_PBUPL_K-DEFAULT_BRANCH   = ABAP_TRUE.
  ENDIF.

  APPEND LS_FITHA_PBUPL_K TO LT_FITHA_PBUPL_K.
  CALL METHOD CL_FINLOC_CVI_PERSIST_DATA=>SET_FITHA_PBUPL_K(
    EXPORTING
      IT_FITHA_PBUPL_K = LT_FITHA_PBUPL_K
  ).
  CALL METHOD LCL_FINLOC_VENDOR_UPDATE->CS_UPDATE_FITHA_PBUPL_K.

  "Update Branch Code Description
  LS_FITHA_PBUPL_K_T-LIFNR                = UV_PARTNER.     "Vendor
  IF US_CENTRAL-BCODE IS INITIAL.
    IF UV_PARTN_CAT = '1'. "Person
      LS_FITHA_PBUPL_K_T-J_1TPBUPL        = 'NoVat'.
      LS_FITHA_PBUPL_K_T-DESCRIPTION      = 'NoVat'.
    ELSEIF UV_PARTN_CAT = '2'. "Org
      LS_FITHA_PBUPL_K_T-J_1TPBUPL        = '00000'.
      LS_FITHA_PBUPL_K_T-DESCRIPTION      = GC_HEADOFF.
    ENDIF.
  ELSE.
    LS_FITHA_PBUPL_K_T-J_1TPBUPL          = US_CENTRAL-BCODE.
    LS_FITHA_PBUPL_K_T-DESCRIPTION        = US_CENTRAL-BDESC.
    IF US_CENTRAL-BCODE = '00000'.
      LS_FITHA_PBUPL_K_T-DESCRIPTION      = GC_HEADOFF.
    ENDIF.
  ENDIF.
  LS_FITHA_PBUPL_K_T-SPRAS                = 'E'.
  APPEND LS_FITHA_PBUPL_K_T TO LT_FITHA_PBUPL_K_T.

  CALL METHOD CL_FINLOC_CVI_PERSIST_DATA=>SET_FITHA_PBUPL_K_T(
    EXPORTING
      IT_FITHA_PBUPL_K_T = LT_FITHA_PBUPL_K_T
  ).
  CALL METHOD LCL_FINLOC_VENDOR_UPDATE->CS_UPDATE_FITHA_PBUPL_K_T.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form add_bank_detail
*&---------------------------------------------------------------------*
FORM ADD_BANK_DETAIL  USING UV_PARTNER TYPE BU_PARTNER
                            US_VENDOR  TYPE ZSDSFIS084.

  DATA: LS_BANK_DETAIL TYPE BAPIBUS1006_BANKDETAIL,
        LS_BANK_OUT    TYPE BAPIBUS1006_HEAD-BANKDETAILID,
        LT_RETURN      TYPE STANDARD TABLE OF BAPIRET2.

  SELECT SINGLE *
    FROM BNKA
    INTO @DATA(LS_BNKA)
    WHERE BANKS = @US_VENDOR-BANKS
      AND BANKL = @US_VENDOR-BANKL.
  IF SY-SUBRC = 0.
    LS_BANK_DETAIL-BANK_CTRY = LS_BNKA-BANKS.
    LS_BANK_DETAIL-BANK_KEY  = LS_BNKA-BANKL.
    LS_BANK_DETAIL-BANK_ACCT = US_VENDOR-BANKN.
    LS_BANK_DETAIL-BANK_REF  = US_VENDOR-BKREF.

  ENDIF.

  CALL FUNCTION 'BAPI_BUPA_BANKDETAIL_ADD'
    EXPORTING
      BUSINESSPARTNER = UV_PARTNER
      BANKDETAILDATA  = LS_BANK_DETAIL
    IMPORTING
      BANKDETAILIDOUT = LS_BANK_OUT
    TABLES
      RETURN          = LT_RETURN.

  LOOP AT LT_RETURN INTO DATA(LS_RETURN) WHERE TYPE = 'E'.
    EXIT.
  ENDLOOP.
  IF SY-SUBRC <> 0.
    PERFORM COMMIT_WORK.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form cre_contact_person
*&---------------------------------------------------------------------*
FORM CRE_CONTACT_PERSON  USING UV_PARTNER  TYPE BU_PARTNER
                               UV_TEST     TYPE CHAR01
                         CHANGING CT_CONTACT  TYPE ZSDSFIS082_TT
                                  CT_RETURN   TYPE BAPIRET2_T.

  DATA: LV_CONTACT_PERSON TYPE BU_PARTNER,
        LS_BP             TYPE CVIS_EI_EXTERN,
        LT_BP             TYPE CVIS_EI_EXTERN_T,
        LS_CONTACT        TYPE ZSDSFIS082,
        LT_SMTP           TYPE BUS_EI_BUPA_SMTP_T,
        LT_PHONE          TYPE BUS_EI_BUPA_TELEPHONE_T,
        LS_ROLE           TYPE BUS_EI_BUPA_ROLES,
        LT_ROLE           TYPE BUS_EI_BUPA_ROLES_T,
        LT_ADDRESS        TYPE BUS_EI_BUPA_ADDRESS_T,
        LS_RETURN         TYPE BAPIRET2,
        LV_ERROR          TYPE CHAR01.

  DATA: LT_RETURN         TYPE BAPIRET2_T.

  LOOP AT CT_CONTACT ASSIGNING FIELD-SYMBOL(<LFS_CONTACT>).

    CHECK <LFS_CONTACT>-PARTNER2 IS INITIAL.

    REFRESH LT_ROLE.
*------------------------------------------------------------------------------
* Create GUID for new BP
*------------------------------------------------------------------------------
    TRY.
        DATA(V_GUID) = CL_SYSTEM_UUID=>IF_SYSTEM_UUID_STATIC~CREATE_UUID_C32( ).
      CATCH CX_UUID_ERROR INTO DATA(R_UUID_EXC).
        MESSAGE R_UUID_EXC->GET_TEXT( ) TYPE 'E'.
    ENDTRY.


    LS_BP-PARTNER-HEADER-OBJECT_TASK                                      = GC_TASK_INSERT. "'I' for new BP
    LS_BP-PARTNER-HEADER-OBJECT_INSTANCE-BPARTNERGUID                     = V_GUID.

    LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CONTROL-CATEGORY            = '1'.       "Person
    LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CONTROL-GROUPING            = 'Z070'.    "Contact Person

    LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_PERSON-FIRSTNAME            = <LFS_CONTACT>-FIRSTNAME.
    LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_PERSON-LASTNAME             = <LFS_CONTACT>-LASTNAME.
    LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CENTRALDATA-SEARCHTERM1     = <LFS_CONTACT>-FIRSTNAME.
    LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CENTRALDATA-SEARCHTERM2     = <LFS_CONTACT>-SFDC_REF_ID.

    "Mark X
    LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_PERSON-FIRSTNAME           = ABAP_TRUE.
    LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_PERSON-LASTNAME            = ABAP_TRUE.
    LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_CENTRALDATA-SEARCHTERM1    = ABAP_TRUE.
    LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_CENTRALDATA-SEARCHTERM2    = ABAP_TRUE.

    "Phone
    APPEND INITIAL LINE TO LT_PHONE ASSIGNING FIELD-SYMBOL(<LFS_PHONE>).
    <LFS_PHONE>-CONTACT-TASK                 = GC_TASK_INSERT.
    <LFS_PHONE>-CONTACT-DATA-TELEPHONE       = <LFS_CONTACT>-TEL1_NUMBR.
    <LFS_PHONE>-CONTACT-DATA-EXTENSION       = <LFS_CONTACT>-TEL1_EXT.
    <LFS_PHONE>-CONTACT-DATAX-TELEPHONE      = ABAP_TRUE.
    <LFS_PHONE>-CONTACT-DATAX-EXTENSION      = ABAP_TRUE.

    "Email
    APPEND INITIAL LINE TO LT_SMTP ASSIGNING FIELD-SYMBOL(<LFS_SMTP>).
    <LFS_SMTP>-CONTACT-TASK                 = GC_TASK_INSERT.
    <LFS_SMTP>-CONTACT-DATA-E_MAIL          = <LFS_CONTACT>-E_MAIL.
    <LFS_SMTP>-CONTACT-DATAX-E_MAIL         = ABAP_TRUE.

    LS_BP-PARTNER-CENTRAL_DATA-COMMUNICATION-PHONE-PHONE = LT_PHONE[].
    LS_BP-PARTNER-CENTRAL_DATA-COMMUNICATION-SMTP-SMTP   = LT_SMTP[].

    "Contact person role
    LS_ROLE-DATA_KEY = GC_ROLE-CONTACT_P.
    APPEND LS_ROLE TO LT_ROLE.
    LS_BP-PARTNER-CENTRAL_DATA-ROLE-ROLES = LT_ROLE.

    APPEND INITIAL LINE TO LT_ADDRESS ASSIGNING FIELD-SYMBOL(<LFS_ADDRESS>).
    <LFS_ADDRESS>-TASK = GC_TASK_INSERT.
* Operations are store in table TB008S
    <LFS_ADDRESS>-DATA_KEY-OPERATION                = 'XXDFLT'. "Standard operation
    <LFS_ADDRESS>-DATA-POSTAL-DATA-COUNTRY          = 'TH'.
    <LFS_ADDRESS>-DATA-POSTAL-DATA-LANGU            = 'E'.
    <LFS_ADDRESS>-DATA-COMMUNICATION-PHONE-PHONE    = LT_PHONE[].
    <LFS_ADDRESS>-DATA-COMMUNICATION-SMTP-SMTP      = LT_SMTP[].

* Mark as changed
    <LFS_ADDRESS>-DATA-POSTAL-DATAX-COUNTRY    = ABAP_TRUE.
    <LFS_ADDRESS>-DATA-POSTAL-DATAX-LANGU      = ABAP_TRUE.

* Add address to main structure
    LS_BP-PARTNER-CENTRAL_DATA-ADDRESS-ADDRESSES = LT_ADDRESS.

*------------------------------------------------------------------------------
* Validate data
*------------------------------------------------------------------------------
    CL_MD_BP_MAINTAIN=>VALIDATE_SINGLE(
      EXPORTING
        I_DATA        = LS_BP
      IMPORTING
        ET_RETURN_MAP = DATA(LT_RETURN_MAP)
    ).

    "Filter message out ->Errors occurred during call of function module BUPA_CREATE_FROM_DATA
    PERFORM FILTER_ERROR_OUT  CHANGING LT_RETURN_MAP.

    IF LINE_EXISTS( LT_RETURN_MAP[ TYPE = 'E' ] ) OR
       LINE_EXISTS( LT_RETURN_MAP[ TYPE = 'A' ] ).

      LOOP AT LT_RETURN_MAP INTO DATA(LS_RETURN_MAP)
                            WHERE TYPE = 'E'
                               OR TYPE = 'A'.

        MOVE-CORRESPONDING LS_RETURN_MAP TO LS_RETURN.
        APPEND LS_RETURN TO CT_RETURN.
        EXIT. "exit loop

      ENDLOOP.
      REFRESH: LT_RETURN_MAP.

      GV_FLAG_ERR = ABAP_TRUE.

      EXIT. "Exit program
    ENDIF.

    INSERT LS_BP INTO TABLE LT_BP.

    CL_MD_BP_MAINTAIN=>MAINTAIN(
      EXPORTING
        I_DATA     = LT_BP
        I_TEST_RUN = UV_TEST
      IMPORTING
        E_RETURN   = DATA(LT_RETURN2)
    ).

    "Error case
    READ TABLE LT_RETURN2 INTO DATA(LS_RETURN2)
                          INDEX 1.
    LOOP AT LS_RETURN2-OBJECT_MSG INTO DATA(LS_OBJ_MSG).
      IF LS_OBJ_MSG-TYPE = 'E' OR LS_OBJ_MSG-TYPE = 'A'.
*         Error occurred
        LV_ERROR = ABAP_TRUE.
        MOVE-CORRESPONDING LS_OBJ_MSG TO LS_RETURN.
        APPEND LS_RETURN TO CT_RETURN.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF LV_ERROR = ABAP_TRUE.
      GV_FLAG_ERR = ABAP_TRUE.
      REFRESH: LT_RETURN_MAP,
               LT_RETURN2.
      EXIT.
    ELSE.
      CHECK UV_TEST IS INITIAL.
      PERFORM COMMIT_WORK.
      IMPORT LV_PARTNER TO LV_CONTACT_PERSON FROM MEMORY ID 'BUP_MEMORY_PARTNER'.

      <LFS_CONTACT>-PARTNER1 = UV_PARTNER.
      <LFS_CONTACT>-PARTNER2 = LV_CONTACT_PERSON.

      CALL FUNCTION 'BAPI_BUPR_RELATIONSHIP_CREATE'
        EXPORTING
          BUSINESSPARTNER1     = UV_PARTNER
          BUSINESSPARTNER2     = LV_CONTACT_PERSON
          RELATIONSHIPCATEGORY = 'BUR001'
          VALIDFROMDATE        = SY-DATLO
          VALIDUNTILDATE       = '99991231'
        TABLES
          RETURN               = LT_RETURN.

      PERFORM COMMIT_WORK.

    ENDIF.
    REFRESH: LT_RETURN_MAP,
             LT_RETURN2.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form commit_work
*&---------------------------------------------------------------------*
FORM COMMIT_WORK .

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      WAIT = ABAP_TRUE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form roll_back
*&---------------------------------------------------------------------*
FORM ROLL_BACK .

  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form update_cust_other
*&---------------------------------------------------------------------*
FORM UPDATE_CUST_OTHER  USING UV_PARTNER TYPE BU_PARTNER
                              US_CUSTOMER TYPE ZSDSFIS078.

  DATA: LS_KNA1    TYPE KNA1,
        LV_KUNNR   TYPE KUNNR,
        LS_KNA1_EX TYPE KNA1,
        LV_CHAR01  TYPE CHAR01,
        LT_KNVI    TYPE STANDARD TABLE OF FKNVI,
        LS_KNVI    LIKE LINE OF LT_KNVI.

  SELECT SINGLE *
    FROM KNA1
    INTO LS_KNA1
    WHERE KUNNR = UV_PARTNER.

  "Add field to update
  LS_KNA1-KUKLA = US_CUSTOMER-KUKLA.

  SELECT SINGLE *
    FROM KNVI
    INTO CORRESPONDING FIELDS OF LS_KNVI
    WHERE KUNNR = UV_PARTNER
      AND ALAND = 'TH'.
  IF SY-SUBRC = 0.
    LS_KNVI-TAXKD = US_CUSTOMER-TAXKD.
    APPEND LS_KNVI TO LT_KNVI.
  ENDIF.
*  ls_kna1-vbund = uv_partner.

  CALL FUNCTION 'SD_CUSTOMER_MAINTAIN_ALL'
    EXPORTING
      I_KNA1                  = LS_KNA1
      PI_POSTFLAG             = ABAP_TRUE
    IMPORTING
      E_KUNNR                 = LV_KUNNR
      O_KNA1                  = LS_KNA1_EX
      E_SD_CUST_1321_DONE     = LV_CHAR01
    TABLES
      T_XKNVI                 = LT_KNVI
    EXCEPTIONS
      CLIENT_ERROR            = 1
      KNA1_INCOMPLETE         = 2
      KNB1_INCOMPLETE         = 3
      KNB5_INCOMPLETE         = 4
      KNVV_INCOMPLETE         = 5
      KUNNR_NOT_UNIQUE        = 6
      SALES_AREA_NOT_UNIQUE   = 7
      SALES_AREA_NOT_VALID    = 8
      INSERT_UPDATE_CONFLICT  = 9
      NUMBER_ASSIGNMENT_ERROR = 10
      NUMBER_NOT_IN_RANGE     = 11
      NUMBER_RANGE_NOT_EXTERN = 12
      NUMBER_RANGE_NOT_INTERN = 13
      ACCOUNT_GROUP_NOT_VALID = 14
      PARNR_INVALID           = 15
      BANK_ADDRESS_INVALID    = 16
      TAX_DATA_NOT_VALID      = 17
      NO_AUTHORITY            = 18
      COMPANY_CODE_NOT_UNIQUE = 19
      DUNNING_DATA_NOT_VALID  = 20
      KNB1_REFERENCE_INVALID  = 21
      CAM_ERROR               = 22
      OTHERS                  = 23.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ELSE.
    PERFORM COMMIT_WORK.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form chk_bp_number
*&---------------------------------------------------------------------*
FORM CHK_BP_NUMBER  USING UV_PARTNER TYPE BU_PARTNER
                    CHANGING CT_RETURN TYPE BAPIRET2_TT.


  DATA: LT_RETURN TYPE BAPIRET2_TT.


  CALL FUNCTION 'BAPI_BUPA_GET_NUMBERS'
    EXPORTING
      BUSINESSPARTNER             = UV_PARTNER
    IMPORTING
      BUSINESSPARTNEROUT          = GV_PARTNER
      BUSINESSPARTNERGUIDOUT      = GV_PARTNER_GUID
      EXTBUSINESSPARTNERNUMBEROUT = GV_EXT
    TABLES
      RETURN                      = LT_RETURN.


  IF LINE_EXISTS( LT_RETURN[ TYPE = 'E' ] ) OR
   LINE_EXISTS( LT_RETURN[ TYPE = 'A' ] ).

    GV_FLAG_ERR = ABAP_TRUE.
    CT_RETURN[] = LT_RETURN[].

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form cre_shipto_pfn
*&---------------------------------------------------------------------*
FORM CRE_SHIPTO_PFN  USING    UV_PARTNER TYPE BU_PARTNER
                              UV_TEST    TYPE CHAR01
                     CHANGING CT_PARTNER TYPE ZSDSFIS083_TT
                              CT_RETURN  TYPE BAPIRET2_T.

  DATA: LV_SHIPTO    TYPE BU_PARTNER,
        LS_BP        TYPE CVIS_EI_EXTERN,
        LT_BP        TYPE CVIS_EI_EXTERN_T,
        LT_SMTP      TYPE BUS_EI_BUPA_SMTP_T,
        LT_PHONE     TYPE BUS_EI_BUPA_TELEPHONE_T,
        LS_ROLE      TYPE BUS_EI_BUPA_ROLES,
        LT_ROLE      TYPE BUS_EI_BUPA_ROLES_T,
        LT_ADDRESS   TYPE BUS_EI_BUPA_ADDRESS_T,
        LT_FAX       TYPE BUS_EI_BUPA_FAX_T,
        LT_CUS_SALES TYPE CMDS_EI_SALES_T,
        LT_TAX_IND   TYPE CMDS_EI_TAX_IND_T,
        LS_RETURN    TYPE BAPIRET2,
        LV_ERROR     TYPE CHAR01.

  DATA: LT_RETURN         TYPE BAPIRET2_T.

  LOOP AT CT_PARTNER ASSIGNING FIELD-SYMBOL(<LFS_PARTNER>).

    REFRESH LT_TAX_IND[].

    CLEAR: LV_SHIPTO,
           LS_BP,
           LS_ROLE,
           LS_RETURN.

    REFRESH:  LT_BP[],
              LT_SMTP[],
              LT_PHONE[],
              LT_ROLE[],
              LT_ADDRESS[],
              LT_FAX[],
              LT_CUS_SALES[].

    IF <LFS_PARTNER>-PARVW <> GC_PARTNER_FN-SHIPTO.
      CONTINUE.
    ENDIF.

*------------------------------------------------------------------------------
* Create GUID for new BP
*------------------------------------------------------------------------------
    TRY.
        DATA(V_GUID) = CL_SYSTEM_UUID=>IF_SYSTEM_UUID_STATIC~CREATE_UUID_C32( ).
      CATCH CX_UUID_ERROR INTO DATA(R_UUID_EXC).
        MESSAGE R_UUID_EXC->GET_TEXT( ) TYPE 'E'.
    ENDTRY.

    LS_BP-PARTNER-HEADER-OBJECT_TASK                                      = GC_TASK_INSERT. "'I' for new BP
    LS_BP-PARTNER-HEADER-OBJECT_INSTANCE-BPARTNERGUID                     = V_GUID.

    LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CONTROL-CATEGORY            = <LFS_PARTNER>-BU_TYPE.       "Person
    LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CONTROL-GROUPING            = 'Z040'.                       "Ship-to

    IF <LFS_PARTNER>-BU_TYPE = '1'. "Person
      "Person
*      ls_bp-partner-central_data-common-data-bp_person-prefix1      =
      LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_PERSON-FIRSTNAME            = <LFS_PARTNER>-NAME1.
      LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_PERSON-LASTNAME             = <LFS_PARTNER>-NAME2.
      LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_PERSON-CORRESPONDLANGUAGE   = '2'.

      "Mark X
      LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_PERSON-FIRSTNAME           = ABAP_TRUE.
      LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_PERSON-LASTNAME            = ABAP_TRUE.
      LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_PERSON-CORRESPONDLANGUAGE  = ABAP_TRUE.

    ELSEIF <LFS_PARTNER>-BU_TYPE = '2'. "Organization
      LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_ORGANIZATION-NAME1  = <LFS_PARTNER>-NAME1.
      LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_ORGANIZATION-NAME2  = <LFS_PARTNER>-NAME2.
      LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_ORGANIZATION-NAME3  = <LFS_PARTNER>-NAME3.
      LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_ORGANIZATION-NAME4  = <LFS_PARTNER>-NAME4.

      "Mark X
      LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_ORGANIZATION-NAME1 = ABAP_TRUE.
      LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_ORGANIZATION-NAME2 = ABAP_TRUE.
      LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_ORGANIZATION-NAME3 = ABAP_TRUE.
      LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_ORGANIZATION-NAME4 = ABAP_TRUE.
    ENDIF.

    "Central Data
    LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CENTRALDATA-SEARCHTERM1 = <LFS_PARTNER>-NAME1.
    LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CENTRALDATA-SEARCHTERM2 = <LFS_PARTNER>-SFDC_REF_ID.

    "Mark X
    LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_CENTRALDATA-SEARCHTERM1 = ABAP_TRUE.
    LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_CENTRALDATA-SEARCHTERM2 = ABAP_TRUE.


*   "Contact person role
    LS_ROLE-DATA_KEY = GC_ROLE-CUSTOMER.
    APPEND LS_ROLE TO LT_ROLE.
    LS_BP-PARTNER-CENTRAL_DATA-ROLE-ROLES = LT_ROLE.

    "Communication
    "Mobile Phone
    REFRESH: LT_PHONE[].
    APPEND INITIAL LINE TO LT_PHONE ASSIGNING FIELD-SYMBOL(<LFS_PHONE>).
    <LFS_PHONE>-CONTACT-TASK  =  GC_TASK_INSERT.
    <LFS_PHONE>-CONTACT-DATA-TELEPHONE        = <LFS_PARTNER>-MOBILE_PHONE.
    <LFS_PHONE>-CONTACT-DATA-R_3_USER         = '3'.
    <LFS_PHONE>-CONTACT-DATAX-TELEPHONE       = ABAP_TRUE.
    <LFS_PHONE>-CONTACT-DATAX-R_3_USER        = ABAP_TRUE.

    "Phone
    APPEND INITIAL LINE TO LT_PHONE ASSIGNING <LFS_PHONE>.
    <LFS_PHONE>-CONTACT-TASK                 =  GC_TASK_INSERT.
    <LFS_PHONE>-CONTACT-DATA-TELEPHONE       = <LFS_PARTNER>-TEL_NUMBER.
    <LFS_PHONE>-CONTACT-DATAX-TELEPHONE      = ABAP_TRUE.


    "Fax
    APPEND INITIAL LINE TO LT_FAX ASSIGNING FIELD-SYMBOL(<LFS_FAX>).
    <LFS_FAX>-CONTACT-TASK                  = GC_TASK_INSERT.
    <LFS_FAX>-CONTACT-DATA-FAX              = <LFS_PARTNER>-FAX_NUMBER.
    <LFS_FAX>-CONTACT-DATAX-FAX             = ABAP_TRUE.

    "Email
    APPEND INITIAL LINE TO LT_SMTP ASSIGNING FIELD-SYMBOL(<LFS_SMTP>).
    <LFS_SMTP>-CONTACT-TASK                 = GC_TASK_INSERT.
    <LFS_SMTP>-CONTACT-DATA-E_MAIL          = <LFS_PARTNER>-EMAIL.
    <LFS_SMTP>-CONTACT-DATAX-E_MAIL         = ABAP_TRUE.

    LS_BP-PARTNER-CENTRAL_DATA-COMMUNICATION-PHONE-PHONE = LT_PHONE[].
    LS_BP-PARTNER-CENTRAL_DATA-COMMUNICATION-FAX-FAX     = LT_FAX[].
    LS_BP-PARTNER-CENTRAL_DATA-COMMUNICATION-SMTP-SMTP   = LT_SMTP[].

*------------------------------------------------------------------------------
* Address data
*------------------------------------------------------------------------------
    APPEND INITIAL LINE TO LT_ADDRESS ASSIGNING FIELD-SYMBOL(<LFS_ADDRESS>).
    <LFS_ADDRESS>-TASK = GC_TASK_INSERT.
* Operations are store in table TB008S
    <LFS_ADDRESS>-DATA_KEY-OPERATION           = 'XXDFLT'. "Standard operation
    <LFS_ADDRESS>-DATA-POSTAL-DATA-POSTL_COD1  = <LFS_PARTNER>-POSTL_COD1.
    <LFS_ADDRESS>-DATA-POSTAL-DATA-STREET      = <LFS_PARTNER>-STREET.
    <LFS_ADDRESS>-DATA-POSTAL-DATA-COUNTRY     = <LFS_PARTNER>-COUNTRY.
    <LFS_ADDRESS>-DATA-POSTAL-DATA-LANGU       = '2'.     "TH
    <LFS_ADDRESS>-DATA-POSTAL-DATA-REGION      = <LFS_PARTNER>-REGION.
    <LFS_ADDRESS>-DATA-POSTAL-DATA-CITY        = <LFS_PARTNER>-CITY.
    <LFS_ADDRESS>-DATA-POSTAL-DATA-DISTRICT    = <LFS_PARTNER>-DISTRICT.
    <LFS_ADDRESS>-DATA-POSTAL-DATA-STR_SUPPL1  = <LFS_PARTNER>-STR_SUPPL1.
    <LFS_ADDRESS>-DATA-POSTAL-DATA-STR_SUPPL2  = <LFS_PARTNER>-STR_SUPPL2.
    <LFS_ADDRESS>-DATA-POSTAL-DATA-STR_SUPPL3  = <LFS_PARTNER>-STR_SUPPL3.
    <LFS_ADDRESS>-DATA-POSTAL-DATA-LOCATION    = <LFS_PARTNER>-LOCATION.
    <LFS_ADDRESS>-DATA-POSTAL-DATA-TRANSPZONE  = <LFS_PARTNER>-TRANSPZONE.

    "Phone
    <LFS_ADDRESS>-DATA-COMMUNICATION-PHONE-PHONE  = LT_PHONE[].
    <LFS_ADDRESS>-DATA-COMMUNICATION-FAX-FAX      = LT_FAX[].
    <LFS_ADDRESS>-DATA-COMMUNICATION-SMTP-SMTP    = LT_SMTP[].

* Mark as changed
    <LFS_ADDRESS>-DATA-POSTAL-DATAX-CITY          = ABAP_TRUE.
    <LFS_ADDRESS>-DATA-POSTAL-DATAX-POSTL_COD1    = ABAP_TRUE.
    <LFS_ADDRESS>-DATA-POSTAL-DATAX-STREET        = ABAP_TRUE.
    <LFS_ADDRESS>-DATA-POSTAL-DATAX-COUNTRY       = ABAP_TRUE.
    <LFS_ADDRESS>-DATA-POSTAL-DATAX-REGION        = ABAP_TRUE.
    <LFS_ADDRESS>-DATA-POSTAL-DATAX-LANGU         = ABAP_TRUE.
    <LFS_ADDRESS>-DATA-POSTAL-DATAX-DISTRICT      = ABAP_TRUE.
    <LFS_ADDRESS>-DATA-POSTAL-DATAX-STR_SUPPL1    = ABAP_TRUE.
    <LFS_ADDRESS>-DATA-POSTAL-DATAX-STR_SUPPL2    = ABAP_TRUE.
    <LFS_ADDRESS>-DATA-POSTAL-DATAX-STR_SUPPL3    = ABAP_TRUE.
    <LFS_ADDRESS>-DATA-POSTAL-DATAX-LOCATION      = ABAP_TRUE.
    <LFS_ADDRESS>-DATA-POSTAL-DATAX-TRANSPZONE    = ABAP_TRUE.

* Add address to main structure
    LS_BP-PARTNER-CENTRAL_DATA-ADDRESS-ADDRESSES = LT_ADDRESS.

    LOOP AT GT_VTWEG INTO DATA(LS_VTWEG).
      "Sale data
      APPEND INITIAL LINE TO LT_CUS_SALES ASSIGNING FIELD-SYMBOL(<LFS_CUS_SALES>).
      <LFS_CUS_SALES>-TASK           = 'I'.
      <LFS_CUS_SALES>-DATA_KEY-VKORG = GC_SALES_AREA-VKORG.
      <LFS_CUS_SALES>-DATA_KEY-VTWEG = LS_VTWEG-LOW.
      <LFS_CUS_SALES>-DATA_KEY-SPART = GC_SALES_AREA-SPART.
      <LFS_CUS_SALES>-DATA-KALKS     = '1'.
      <LFS_CUS_SALES>-DATA-KONDA     = '01'.
      <LFS_CUS_SALES>-DATA-WAERS     = 'THB'.
      <LFS_CUS_SALES>-DATA-VWERK     = GC_SDS.
      <LFS_CUS_SALES>-DATA-AWAHR     = '100'.
      <LFS_CUS_SALES>-DATA-VSBED    = <LFS_PARTNER>-VSBED.    "Shipping Conditions
      <LFS_CUS_SALES>-DATA-INCO1    = <LFS_PARTNER>-INCO1.    "Incoterms
      <LFS_CUS_SALES>-DATA-INCO2_L  = <LFS_PARTNER>-INCO2_L.  "Incoterms Loca

      "mark X
      <LFS_CUS_SALES>-DATAX-KALKS    = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-KONDA    = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-WAERS    = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-VWERK    = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-AWAHR    = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-VSBED    = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-INCO1    = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-INCO2_L  = ABAP_TRUE.
    ENDLOOP.

    LS_BP-CUSTOMER-SALES_DATA-SALES = LT_CUS_SALES[].

    APPEND INITIAL LINE TO LT_TAX_IND ASSIGNING FIELD-SYMBOL(<LFS_TAX_IND>).
    <LFS_TAX_IND>-TASK            = GC_TASK_INSERT.
    <LFS_TAX_IND>-DATA_KEY-ALAND  = 'TH'.
    <LFS_TAX_IND>-DATA_KEY-TATYP  = 'MWST'.
    <LFS_TAX_IND>-DATA-TAXKD      = '1'.
    <LFS_TAX_IND>-DATAX-TAXKD     = ABAP_TRUE.
    LS_BP-CUSTOMER-CENTRAL_DATA-TAX_IND-TAX_IND    = LT_TAX_IND[].

*------------------------------------------------------------------------------
* Validate data
*------------------------------------------------------------------------------
    CL_MD_BP_MAINTAIN=>VALIDATE_SINGLE(
      EXPORTING
        I_DATA        = LS_BP
      IMPORTING
        ET_RETURN_MAP = DATA(LT_RETURN_MAP)
    ).

    "Filter message out ->Errors occurred during call of function module BUPA_CREATE_FROM_DATA
    PERFORM FILTER_ERROR_OUT  CHANGING LT_RETURN_MAP.

    IF LINE_EXISTS( LT_RETURN_MAP[ TYPE = 'E' ] ) OR
       LINE_EXISTS( LT_RETURN_MAP[ TYPE = 'A' ] ).

      LOOP AT LT_RETURN_MAP INTO DATA(LS_RETURN_MAP)
                            WHERE TYPE = 'E'
                               OR TYPE = 'A'.

        MOVE-CORRESPONDING LS_RETURN_MAP TO LS_RETURN.
        APPEND LS_RETURN TO CT_RETURN.
        EXIT. "exit loop

      ENDLOOP.
      REFRESH: LT_RETURN_MAP.

      GV_FLAG_ERR = ABAP_TRUE.
      EXIT. "Exit loop
    ENDIF.

    INSERT LS_BP INTO TABLE LT_BP.

    CL_MD_BP_MAINTAIN=>MAINTAIN(
      EXPORTING
        I_DATA     = LT_BP
        I_TEST_RUN = UV_TEST
      IMPORTING
        E_RETURN   = DATA(LT_RETURN2)
    ).

    "Error case
    READ TABLE LT_RETURN2 INTO DATA(LS_RETURN2)
                          INDEX 1.
    LOOP AT LS_RETURN2-OBJECT_MSG INTO DATA(LS_OBJ_MSG).
      IF LS_OBJ_MSG-TYPE = 'E' OR LS_OBJ_MSG-TYPE = 'A'.
*         Error occurred
        LV_ERROR = ABAP_TRUE.
        MOVE-CORRESPONDING LS_OBJ_MSG TO LS_RETURN.
        APPEND LS_RETURN TO CT_RETURN.
        EXIT.
      ENDIF.
    ENDLOOP.


    IF LV_ERROR = ABAP_TRUE.
      GV_FLAG_ERR = ABAP_TRUE.
      REFRESH: LT_RETURN_MAP,
               LT_RETURN2.
      EXIT.
    ELSE.
      CHECK UV_TEST IS INITIAL.
      PERFORM COMMIT_WORK.
      IMPORT LV_PARTNER TO LV_SHIPTO FROM MEMORY ID 'BUP_MEMORY_PARTNER'.

      <LFS_PARTNER>-BPARTNER = LV_SHIPTO. "<- error bpartner must be blank

    ENDIF.
    REFRESH: LT_RETURN_MAP,
             LT_RETURN2.
  ENDLOOP.

  PERFORM ASSIGN_PARTFN USING UV_PARTNER
                              CT_PARTNER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form assign_partfn
*&---------------------------------------------------------------------*
FORM ASSIGN_PARTFN  USING UV_PARTNER TYPE BU_PARTNER      "original
                          UT_PARTNER TYPE ZSDSFIS083_TT.  "Shipto

  DATA: LT_PARTNER TYPE ZSDSFIS083_TT,
        LV_GUID    TYPE BU_PARTNER_GUID.

  DATA:
    LS_BP           TYPE CVIS_EI_EXTERN,
    LT_BP           TYPE CVIS_EI_EXTERN_T,
    LT_PARTNER_FUNC TYPE CMDS_EI_FUNCTIONS_T,
    LT_CUS_SALES    TYPE CMDS_EI_SALES_T,
    LV_PARZA        TYPE KNVP-PARZA.

  SELECT
    KUNNR,
    VKORG,
    VTWEG,
    SPART
    FROM KNVV
    INTO TABLE @DATA(LT_KNVV)
    WHERE KUNNR = @UV_PARTNER.
  IF SY-SUBRC = 0.
    SORT LT_KNVV BY VKORG VTWEG SPART.
  ENDIF.

  LT_PARTNER[] = UT_PARTNER[].

  DELETE LT_PARTNER WHERE BPARTNER IS INITIAL.

  CALL FUNCTION 'BUPA_NUMBERS_GET'
    EXPORTING
      IV_PARTNER      = UV_PARTNER
    IMPORTING
      EV_PARTNER_GUID = LV_GUID.

  LS_BP-PARTNER-HEADER-OBJECT_TASK  = 'U'.
  LS_BP-PARTNER-HEADER-OBJECT_INSTANCE-BPARTNERGUID = LV_GUID.
  LS_BP-PARTNER-HEADER-OBJECT_INSTANCE-BPARTNER     = UV_PARTNER.



  LOOP AT LT_PARTNER INTO DATA(LS_PARTNER).   "Partner Function
    LV_PARZA = LV_PARZA + 1.
    APPEND INITIAL LINE TO LT_PARTNER_FUNC ASSIGNING FIELD-SYMBOL(<LFS_PARTNER_FUNC>).
    <LFS_PARTNER_FUNC>-TASK            = GC_TASK_INSERT.
    <LFS_PARTNER_FUNC>-DATA_KEY-PARVW  = LS_PARTNER-PARVW.    "WE
    <LFS_PARTNER_FUNC>-DATA_KEY-PARZA  = LV_PARZA.
    <LFS_PARTNER_FUNC>-DATA-PARTNER    = LS_PARTNER-BPARTNER.
    <LFS_PARTNER_FUNC>-DATAX-PARTNER   = ABAP_TRUE.
  ENDLOOP.

  "Sale data
  LOOP AT GT_VTWEG INTO DATA(LS_VTWEG).
    READ TABLE LT_KNVV INTO DATA(LS_KNVV)
                       WITH KEY VKORG = GC_SALES_AREA-VKORG
                                VTWEG = LS_VTWEG-LOW
                                SPART = GC_SALES_AREA-SPART
                                BINARY SEARCH.
    IF SY-SUBRC = 0.
      APPEND INITIAL LINE TO LT_CUS_SALES ASSIGNING FIELD-SYMBOL(<LFS_CUS_SALES>).
      <LFS_CUS_SALES>-TASK           = GC_TASK_UPDATE.
      <LFS_CUS_SALES>-DATA_KEY-VKORG = GC_SALES_AREA-VKORG.
      <LFS_CUS_SALES>-DATA_KEY-VTWEG = LS_VTWEG-LOW.
      <LFS_CUS_SALES>-DATA_KEY-SPART = GC_SALES_AREA-SPART.
      <LFS_CUS_SALES>-FUNCTIONS-FUNCTIONS = LT_PARTNER_FUNC[].
    ENDIF.
  ENDLOOP.

  LS_BP-CUSTOMER-SALES_DATA-SALES = LT_CUS_SALES[].

*------------------------------------------------------------------------------
* Validate data
*------------------------------------------------------------------------------
  CL_MD_BP_MAINTAIN=>VALIDATE_SINGLE(
    EXPORTING
      I_DATA        = LS_BP
    IMPORTING
      ET_RETURN_MAP = DATA(LT_RETURN_MAP)
  ).

  "Filter message out ->Errors occurred during call of function module BUPA_CREATE_FROM_DATA
  PERFORM FILTER_ERROR_OUT  CHANGING LT_RETURN_MAP.

  IF LINE_EXISTS( LT_RETURN_MAP[ TYPE = 'E' ] ) OR
     LINE_EXISTS( LT_RETURN_MAP[ TYPE = 'A' ] ).

    LOOP AT LT_RETURN_MAP INTO DATA(LS_RETURN_MAP)
                          WHERE TYPE = 'E'
                             OR TYPE = 'A'.

      EXIT. "exit loop
    ENDLOOP.

    EXIT. "Exit program
  ENDIF.

  INSERT LS_BP INTO TABLE LT_BP.

  CL_MD_BP_MAINTAIN=>MAINTAIN(
    EXPORTING
      I_DATA     = LT_BP
      I_TEST_RUN = ''
    IMPORTING
      E_RETURN   = DATA(LT_RETURN)
  ).

  TRY.
      DATA(LT_OBJ_MSG) = LT_RETURN[ 1 ]-OBJECT_MSG.
    CATCH CX_SY_ITAB_LINE_NOT_FOUND.
      "ok to error
  ENDTRY.
  IF LINE_EXISTS( LT_OBJ_MSG[ TYPE = 'E' ] ) OR
     LINE_EXISTS( LT_OBJ_MSG[ TYPE = 'A' ] ).
    "Error case
  ELSE.
    PERFORM COMMIT_WORK.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form filter_error_out
*&---------------------------------------------------------------------*
*& Remove some message error from return tab
*&---------------------------------------------------------------------*
FORM FILTER_ERROR_OUT  CHANGING CT_RETURN_MAP TYPE  MDG_BS_BP_MSGMAP_T.

  DELETE CT_RETURN_MAP WHERE TYPE = 'A'
                         AND ID = 'R11'
                         AND NUMBER = '335'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_constants
*&---------------------------------------------------------------------*
*& get constant data
*&---------------------------------------------------------------------*
FORM GET_CONSTANTS .

  CHECK GV_CONST IS INITIAL.

  REFRESH GT_VTWEG[].

  IF GV_LOCAL = ABAP_TRUE.
    ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = 'Z_SDSFI_CREATE_BP'
                                                    IF_PARAM = 'DISTR_CHANNEL'
                                          IMPORTING ET_RANGE = GT_VTWEG ).
  ELSE.
    ZCL_SDSCA_UTILITIES=>GET_GEN_C_RANGE( EXPORTING IF_REPID = 'Z_SDSFI_CREATE_BP'
                                                    IF_PARAM = 'DISTR_CHANNEL_EX'
                                          IMPORTING ET_RANGE = GT_VTWEG ).
  ENDIF.



  GV_CONST = ABAP_TRUE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form update_bill_cycle
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM UPDATE_BILL_CYCLE  USING  UV_PARTNER TYPE BU_PARTNER
                               US_CUSTOMER TYPE ZSDSFIS078.

  DATA: LS_ZSDSFIC024 TYPE ZSDSFIC024.

  LS_ZSDSFIC024-KUNNR     = UV_PARTNER.
  LS_ZSDSFIC024-ZBLLCOL   = US_CUSTOMER-ZBLLCOL.
  LS_ZSDSFIC024-ZBLLCYL   = US_CUSTOMER-ZBLLCYL.
  LS_ZSDSFIC024-ZDAYS     = US_CUSTOMER-ZDAYS.             "us_customer-zterm+2(2).
**  INSERT INTO zsdsfic024 VALUES ls_zsdsfic024.
  MODIFY ZSDSFIC024 FROM LS_ZSDSFIC024.

  IF SY-SUBRC = 0.
    PERFORM COMMIT_WORK.
  ENDIF.

ENDFORM.
