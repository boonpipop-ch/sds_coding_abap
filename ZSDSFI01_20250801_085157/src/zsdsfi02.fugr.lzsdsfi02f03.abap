*----------------------------------------------------------------------*
***INCLUDE LZSDSFI02F03.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form validate_chg_data
*&---------------------------------------------------------------------*
*& validate change data
*&---------------------------------------------------------------------*
FORM VALIDATE_CHG_DATA   USING UT_ADDRESS TYPE ZSDSFIS010_TT
                         CHANGING CT_CONTACT TYPE ZSDSFIS082_TT
                                  CT_PARTNER TYPE ZSDSFIS083_TT
                                  CT_RETURN  TYPE BAPIRET2_T.

*  READ TABLE ut_address INTO DATA(ls_address)
*                        INDEX 1.
*  IF sy-subrc = 0 AND ls_address-country = 'TH'.
*    gv_local = abap_true.
*  ENDIF.

  PERFORM CHK_BP_NUMBER USING GV_PARTNER
                        CHANGING CT_RETURN.

  CHECK GV_FLAG_ERR IS INITIAL.


  "update contact person
  PERFORM UPD_CONTACT_PERSON USING GV_PARTNER
                                   ABAP_TRUE
                             CHANGING CT_CONTACT
                                      CT_RETURN.

  "update Shipto
  PERFORM UPD_SHIPTO_PFN  USING GV_PARTNER
                                ABAP_TRUE
                         CHANGING CT_PARTNER
                                  CT_RETURN.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form change_bp_data
*&---------------------------------------------------------------------*
*& change bp process
*&---------------------------------------------------------------------*
FORM CHANGE_BP_DATA  USING    UV_TEST       TYPE CHAR01
                              US_CENTRAL    TYPE ZSDSFIS003
                              UT_BP_ROLE    TYPE ZSDSFIS089_TT
                              UT_ADDRESS    TYPE ZSDSFIS010_TT
                              US_CUSTOMER   TYPE ZSDSFIS078
                              UT_PARTNER    TYPE ZSDSFIS083_TT
                              UT_CONTACT    TYPE ZSDSFIS082_TT
                              US_VENDOR     TYPE ZSDSFIS084
                              UV_PARTN_CAT  TYPE BU_TYPE
                              UT_SALES_AREA TYPE ZSDSSDS123_TT
                     CHANGING CS_CENTRAL    TYPE ZSDSFIS003
                              CT_BP_ROLE    TYPE ZSDSFIS089_TT
                              CT_ADDRESS    TYPE ZSDSFIS010_TT
                              CS_CUSTOMER   TYPE ZSDSFIS078
                              CT_PARTNER    TYPE ZSDSFIS083_TT
                              CT_CONTACT    TYPE ZSDSFIS082_TT
                              CS_VENDOR     TYPE ZSDSFIS084
                              CT_RETURN     TYPE BAPIRET2_T.

  DATA: LT_BP_ROLE      TYPE ZSDSFIS089_TT,
        LV_GUID         TYPE BU_PARTNER_GUID,
        LS_BP           TYPE CVIS_EI_EXTERN,
        LT_BP           TYPE CVIS_EI_EXTERN_T,
        LT_TAX          TYPE BUS_EI_BUPA_TAXNUMBER_T,
        LT_SMTP         TYPE BUS_EI_BUPA_SMTP_T,
        LT_PHONE        TYPE BUS_EI_BUPA_TELEPHONE_T,
        LT_FAX          TYPE BUS_EI_BUPA_FAX_T,
        LT_ADDRESS      TYPE BUS_EI_BUPA_ADDRESS_T,
        LT_ADDR_VER     TYPE BUS_EI_BUPA_VERSION_T,
        LT_ROLE         TYPE BUS_EI_BUPA_ROLES_T,
        LS_ROLE         TYPE BUS_EI_BUPA_ROLES,
        LT_CUS_SALES    TYPE CMDS_EI_SALES_T,
        LV_TASK_ACT     TYPE BUS_EI_OBJECT_TASK,
        LT_PARTNER_FUNC TYPE CMDS_EI_FUNCTIONS_T,
        LT_TAX_IND      TYPE CMDS_EI_TAX_IND_T,
        LT_CUS_COMPANY  TYPE CMDS_EI_COMPANY_T,
        LT_CUS_DUNNING  TYPE CMDS_EI_DUNNING_T,
        LT_VEN_PUR      TYPE VMDS_EI_PURCHASING_T,
        LT_VEN_COMPANY  TYPE VMDS_EI_COMPANY_T,
        LS_RETURN       TYPE BAPIRET2,
        LT_RETURN       TYPE BAPIRETM,
        LV_ERROR        TYPE ABAP_BOOL,
        LV_BU_PARTNER   TYPE BU_PARTNER,
        LV_CUST_UPD     TYPE CHAR01,
        LV_VEND_UPD     TYPE CHAR01,
        LV_EMP_FLAG     TYPE CHAR01,
        LS_BUT000       TYPE BUT000.

  MOVE-CORRESPONDING:
    US_CENTRAL         TO CS_CENTRAL,
    US_CUSTOMER        TO CS_CUSTOMER,
    US_VENDOR          TO CS_VENDOR,
    UT_PARTNER[]       TO CT_PARTNER[],
    UT_BP_ROLE[]       TO CT_BP_ROLE[],
    UT_ADDRESS[]       TO CT_ADDRESS[],
    UT_CONTACT[]       TO CT_CONTACT[].

  CALL FUNCTION 'BUPA_NUMBERS_GET'
    EXPORTING
      IV_PARTNER      = GV_PARTNER
    IMPORTING
      EV_PARTNER_GUID = LV_GUID
      ES_BUT000       = LS_BUT000.

  IF LS_BUT000-BU_GROUP = GC_GROUP-EMP.
    LV_EMP_FLAG = ABAP_TRUE.
  ENDIF.

  LS_BP-PARTNER-HEADER-OBJECT_TASK  = 'U'.
  LS_BP-PARTNER-HEADER-OBJECT_INSTANCE-BPARTNERGUID = LV_GUID.
  LS_BP-PARTNER-HEADER-OBJECT_INSTANCE-BPARTNER     = GV_PARTNER.

  "Edit central data
  LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CENTRALDATA-SEARCHTERM1       = US_CENTRAL-SEARCHTERM1.
  LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CENTRALDATA-PARTNEREXTERNAL   = US_CENTRAL-TAXNUMBER.
  LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_CENTRALDATA-PARTNEREXTERNAL  = ABAP_TRUE.

  IF LV_EMP_FLAG <> ABAP_TRUE.
    "Tax Data
    APPEND INITIAL LINE TO LT_TAX ASSIGNING FIELD-SYMBOL(<LFS_TAX>).
    <LFS_TAX>-TASK                 = GC_TASK_UPDATE.
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
        LS_BP-PARTNER-CENTRAL_DATA-COMMON-DATA-BP_PERSON-CORRESPONDLANGUAGE   = '2'. "us_central-partnerlanguage.

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
      <LFS_PHONE>-CONTACT-TASK  =  GC_TASK_MODIFY.
      <LFS_PHONE>-CONTACT-DATA-TELEPHONE        = LS_ADDRESS_TH-MOBILE_PHONE.
      <LFS_PHONE>-CONTACT-DATA-R_3_USER         = '3'.
      <LFS_PHONE>-CONTACT-DATAX-TELEPHONE       = ABAP_TRUE.
      <LFS_PHONE>-CONTACT-DATAX-R_3_USER        = ABAP_TRUE.

      "Phone
      APPEND INITIAL LINE TO LT_PHONE ASSIGNING <LFS_PHONE>.
      <LFS_PHONE>-CONTACT-TASK                 = GC_TASK_MODIFY.
      <LFS_PHONE>-CONTACT-DATA-TELEPHONE       = LS_ADDRESS_TH-TEL_NUMBER.
      <LFS_PHONE>-CONTACT-DATAX-TELEPHONE      = ABAP_TRUE.

      "Fax
      APPEND INITIAL LINE TO LT_FAX ASSIGNING FIELD-SYMBOL(<LFS_FAX>).
      <LFS_FAX>-CONTACT-TASK                  = GC_TASK_MODIFY.
      <LFS_FAX>-CONTACT-DATA-FAX              = LS_ADDRESS_TH-FAX_NUMBER.
      <LFS_FAX>-CONTACT-DATAX-FAX             = ABAP_TRUE.

      "Email
      APPEND INITIAL LINE TO LT_SMTP ASSIGNING FIELD-SYMBOL(<LFS_SMTP>).
      <LFS_SMTP>-CONTACT-TASK                 = GC_TASK_MODIFY.
      <LFS_SMTP>-CONTACT-DATA-E_MAIL          = LS_ADDRESS_TH-EMAIL.
      <LFS_SMTP>-CONTACT-DATAX-E_MAIL         = ABAP_TRUE.


      LS_BP-PARTNER-CENTRAL_DATA-COMMUNICATION-PHONE-PHONE = LT_PHONE[].
      LS_BP-PARTNER-CENTRAL_DATA-COMMUNICATION-FAX-FAX     = LT_FAX[].
      LS_BP-PARTNER-CENTRAL_DATA-COMMUNICATION-SMTP-SMTP   = LT_SMTP[].

*------------------------------------------------------------------------------
* Address data
*------------------------------------------------------------------------------
      APPEND INITIAL LINE TO LT_ADDRESS ASSIGNING FIELD-SYMBOL(<LFS_ADDRESS>).
      <LFS_ADDRESS>-TASK = GC_TASK_UPDATE.
* Operations are store in table TB008S
      <LFS_ADDRESS>-DATA_KEY-OPERATION           = 'XXDFLT'. "Standard operation
      SELECT SINGLE ADDRESS_GUID FROM BUT020 INTO @DATA(LV_ADDR_GUI) WHERE PARTNER = @GV_PARTNER.
      IF SY-SUBRC = 0.
        <LFS_ADDRESS>-DATA_KEY-GUID              = LV_ADDR_GUI.
      ENDIF.

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

        "Mark X
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

      ENDIF.

      <LFS_ADDR_VER>-TASK                          = GC_TASK_MODIFY.
      <LFS_ADDRESS>-DATA-VERSION-VERSIONS          = LT_ADDR_VER.

    ENDIF.

* Add address to main structure
    LS_BP-PARTNER-CENTRAL_DATA-ADDRESS-ADDRESSES = LT_ADDRESS.

  ENDIF.


*  DATA: LT_ROLE_EXIST TYPE ZSDSFIS089_TT.
*  "Get Business partner role
*  PERFORM GET_ROLE    CHANGING LT_ROLE_EXIST.
*  SORT LT_ROLE_EXIST BY ROLE.

  "Get Business partner role
  PERFORM GET_ROLE    CHANGING LT_BP_ROLE.  "Get current role

  LOOP AT UT_BP_ROLE INTO DATA(LS_BP_ROLE).
    "Append Main role first
    READ TABLE LT_BP_ROLE TRANSPORTING NO FIELDS WITH KEY ROLE = LS_BP_ROLE-ROLE BINARY SEARCH.
    IF SY-SUBRC <> 0.
      CLEAR LS_ROLE.
      LS_ROLE-TASK = GC_TASK_INSERT.
      MOVE LS_BP_ROLE-ROLE TO LS_ROLE-DATA_KEY.
      APPEND LS_ROLE TO LT_ROLE.
    ENDIF.

    IF LS_BP_ROLE-ROLE = GC_ROLE-VENDOR.        "Vendor case
      READ TABLE LT_BP_ROLE TRANSPORTING NO FIELDS WITH KEY ROLE = GC_ROLE-VENDOR_FI BINARY SEARCH.
      IF SY-SUBRC <> 0.
        CLEAR LS_ROLE.
        LS_ROLE-TASK      = GC_TASK_INSERT.
        LS_ROLE-DATA_KEY  = GC_ROLE-VENDOR_FI.           "FI Vendor
        APPEND LS_ROLE TO LT_ROLE.
      ENDIF.
    ELSEIF LS_BP_ROLE-ROLE = GC_ROLE-CUSTOMER.  "Customer Case
      READ TABLE LT_BP_ROLE TRANSPORTING NO FIELDS WITH KEY ROLE = GC_ROLE-CUSTOMER_FI BINARY SEARCH.
      IF SY-SUBRC <> 0.
        CLEAR LS_ROLE.
        LS_ROLE-TASK      = GC_TASK_INSERT.
        LS_ROLE-DATA_KEY  = GC_ROLE-CUSTOMER_FI.         "FI Customer
        APPEND LS_ROLE TO LT_ROLE.
      ENDIF.
      READ TABLE LT_BP_ROLE TRANSPORTING NO FIELDS WITH KEY ROLE = GC_ROLE-CREDIT BINARY SEARCH.
      IF SY-SUBRC <> 0.
        CLEAR LS_ROLE.
        LS_ROLE-TASK      = GC_TASK_INSERT.
        LS_ROLE-DATA_KEY  = GC_ROLE-CREDIT.              "Credit Management
        APPEND LS_ROLE TO LT_ROLE.
      ENDIF.
    ENDIF.
  ENDLOOP.

* Add role to main structure
  LS_BP-PARTNER-CENTRAL_DATA-ROLE-ROLES = LT_ROLE.
*------------------------------------------------------------------------------
* Customer Data
*------------------------------------------------------------------------------
  "Customer change
  IF LINE_EXISTS( UT_BP_ROLE[ ROLE = GC_ROLE-CUSTOMER ] ).
    LV_CUST_UPD = ABAP_TRUE.
    "check action insert/update
    CLEAR LV_TASK_ACT.
*    IF LINE_EXISTS( LT_BP_ROLE[ ROLE = GC_ROLE-CUSTOMER ] ).
*      "Change
*      LV_TASK_ACT                   = GC_TASK_UPDATE.
*    ELSE.
*      "Create
*      LV_TASK_ACT                   = GC_TASK_INSERT.
*    ENDIF.

    SELECT
      KNVV~VKORG,
      KNVV~VTWEG,
      KNVV~SPART
     FROM KNVV
     INTO TABLE @DATA(LT_KNVV)
     WHERE KUNNR = @GV_PARTNER.
    IF SY-SUBRC = 0 .
      SORT LT_KNVV BY VTWEG.
    ENDIF.

    LOOP AT GT_VTWEG INTO DATA(LS_VTWEG).   "Loop from const. table

      "Check selected sales area ( SalesOrg,Distribute,Division )
      IF UT_SALES_AREA[] IS NOT INITIAL.
        READ TABLE UT_SALES_AREA TRANSPORTING NO FIELDS
                                 WITH KEY VTWEG = LS_VTWEG-LOW.
        IF SY-SUBRC <> 0.
          CONTINUE.
        ENDIF.
      ENDIF.

      READ TABLE LT_KNVV INTO DATA(LS_KNVV)
                         WITH KEY VTWEG = LS_VTWEG-LOW
                         BINARY SEARCH.
      IF SY-SUBRC = 0.
        LV_TASK_ACT = GC_TASK_UPDATE.
      ELSE.
        LV_TASK_ACT = GC_TASK_INSERT.
      ENDIF.

      "sales data
      APPEND INITIAL LINE TO LT_CUS_SALES ASSIGNING FIELD-SYMBOL(<LFS_CUS_SALES>).
      <LFS_CUS_SALES>-TASK          = LV_TASK_ACT.
      IF LV_TASK_ACT = GC_TASK_INSERT.
        <LFS_CUS_SALES>-DATA-KALKS    = '1'.                  "Customer Classification for Pricing Procedure Determination    #FIX
        <LFS_CUS_SALES>-DATA-KONDA    = '01'.                 "Price Group    #FIX
        <LFS_CUS_SALES>-DATA-WAERS    = US_CENTRAL-CURRENCY.  "Currency
        <LFS_CUS_SALES>-DATA-VWERK    = GC_SDS.               "Delivering Plant (Own or External) #FIX
        <LFS_CUS_SALES>-DATA-AWAHR    = '100'.                "Order Probability of the Item  #FIX
        <LFS_CUS_SALES>-DATA-ZTERM    = US_CUSTOMER-ZTERM.    "TERMS OF PAYMENT KEY
        <LFS_CUS_SALES>-DATA-INCO1    = US_CUSTOMER-INCO1.    "INCOTERMS
        <LFS_CUS_SALES>-DATA-KZAZU    = ABAP_TRUE.            "Order Combination Indicator
        <LFS_CUS_SALES>-DATA-VERSG    = '1'.                  "Customer Statistics Group

        <LFS_CUS_SALES>-DATAX-KALKS     = ABAP_TRUE.
        <LFS_CUS_SALES>-DATAX-KONDA     = ABAP_TRUE.
        <LFS_CUS_SALES>-DATAX-WAERS     = ABAP_TRUE.
        <LFS_CUS_SALES>-DATAX-VWERK     = ABAP_TRUE.
        <LFS_CUS_SALES>-DATAX-AWAHR     = ABAP_TRUE.
        <LFS_CUS_SALES>-DATAX-ZTERM     = ABAP_TRUE.
        <LFS_CUS_SALES>-DATAX-INCO1     = ABAP_TRUE.
        <LFS_CUS_SALES>-DATAX-KZAZU     = ABAP_TRUE.
        <LFS_CUS_SALES>-DATAX-VERSG     = ABAP_TRUE.
      ENDIF.

      <LFS_CUS_SALES>-DATA_KEY-VKORG = GC_SALES_AREA-VKORG.
      <LFS_CUS_SALES>-DATA_KEY-VTWEG = LS_VTWEG-LOW.
      <LFS_CUS_SALES>-DATA_KEY-SPART = GC_SALES_AREA-SPART.
      <LFS_CUS_SALES>-DATA-KVGR1    = US_CUSTOMER-KVGR1.    "Customer Group 1
      <LFS_CUS_SALES>-DATA-KVGR2    = US_CUSTOMER-KVGR2.    "Customer Group 2
      <LFS_CUS_SALES>-DATA-KVGR3    = US_CUSTOMER-KVGR3.    "Customer Group 3
      <LFS_CUS_SALES>-DATA-KVGR4    = US_CUSTOMER-KVGR4.    "Customer Group 4
      <LFS_CUS_SALES>-DATA-KVGR5    = US_CUSTOMER-KVGR5.    "Customer Group 5
      <LFS_CUS_SALES>-DATA-KTGRD    = US_CUSTOMER-KTGRD.    "Account Assignment Group for Customer
      <LFS_CUS_SALES>-DATA-KKBER    = GC_SDS.               "Credit Control Area #FIX
      <LFS_CUS_SALES>-DATA-VKGRP    = US_CUSTOMER-VKGRP.    "sales group
      <LFS_CUS_SALES>-DATA-VKBUR    = US_CUSTOMER-VKBUR.    "sales office
      <LFS_CUS_SALES>-DATA-VSBED    = US_CUSTOMER-VSBED.    "Shipping Conditions
      <LFS_CUS_SALES>-DATA-INCO2_L  = US_CUSTOMER-INCO2_L.  "Incoterms
      <LFS_CUS_SALES>-DATA-KDGRP    = US_CUSTOMER-KDGRP.    "Customer Group

      "mark x
      <LFS_CUS_SALES>-DATAX-KVGR1     = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-KVGR2     = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-KVGR3     = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-KVGR4     = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-KVGR5     = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-KTGRD     = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-VKBUR     = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-VKGRP     = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-VSBED     = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-KKBER     = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-INCO2_L   = ABAP_TRUE.
      <LFS_CUS_SALES>-DATAX-KDGRP     = ABAP_TRUE.

      "Partner Function
      REFRESH: LT_PARTNER_FUNC[].
      "--Sale Emp.
      SELECT SINGLE *
        FROM KNVP
        INTO @DATA(LS_S_EMP)
        WHERE KUNNR = @GV_PARTNER
          AND PARVW = @GC_PARTNER_FN-S_EMP.
      IF SY-SUBRC = 0.
        IF LS_S_EMP-PERNR = US_CUSTOMER-SALES_EMP.
          "dont do anything
        ELSE.
          "delete old sales employee
          APPEND INITIAL LINE TO LT_PARTNER_FUNC ASSIGNING FIELD-SYMBOL(<LFS_PARTNER_FUNC>).
          <LFS_PARTNER_FUNC>-TASK = GC_TASK_DELETE.
          <LFS_PARTNER_FUNC>-DATA_KEY-PARVW = LS_S_EMP-PARVW.
          <LFS_PARTNER_FUNC>-DATA_KEY-PARZA = LS_S_EMP-PARZA.
          <LFS_PARTNER_FUNC>-DATA-PARTNER   = LS_S_EMP-PERNR.
          <LFS_PARTNER_FUNC>-DATAX-PARTNER  = ABAP_TRUE.
          <LFS_CUS_SALES>-FUNCTIONS-FUNCTIONS = LT_PARTNER_FUNC[].

          "append new once
          APPEND INITIAL LINE TO LT_PARTNER_FUNC ASSIGNING <LFS_PARTNER_FUNC>.
          <LFS_PARTNER_FUNC>-TASK = GC_TASK_INSERT.
          <LFS_PARTNER_FUNC>-DATA_KEY-PARVW = GC_PARTNER_FN-S_EMP.
          <LFS_PARTNER_FUNC>-DATA_KEY-PARZA = '001'.
          <LFS_PARTNER_FUNC>-DATA-PARTNER   = US_CUSTOMER-SALES_EMP.
          <LFS_PARTNER_FUNC>-DATAX-PARTNER  = ABAP_TRUE.
          <LFS_CUS_SALES>-FUNCTIONS-FUNCTIONS = LT_PARTNER_FUNC[].
        ENDIF.
      ELSE.
        "New sales emp
        IF US_CUSTOMER-SALES_EMP IS NOT INITIAL.
          APPEND INITIAL LINE TO LT_PARTNER_FUNC ASSIGNING <LFS_PARTNER_FUNC>.
          <LFS_PARTNER_FUNC>-TASK = GC_TASK_INSERT.
          <LFS_PARTNER_FUNC>-DATA_KEY-PARVW = 'VE'.
          <LFS_PARTNER_FUNC>-DATA_KEY-PARZA = '001'.
          <LFS_PARTNER_FUNC>-DATA-PARTNER   = US_CUSTOMER-SALES_EMP.
          <LFS_PARTNER_FUNC>-DATAX-PARTNER  = ABAP_TRUE.
          <LFS_CUS_SALES>-FUNCTIONS-FUNCTIONS = LT_PARTNER_FUNC[].
        ENDIF.
      ENDIF.

      LS_BP-CUSTOMER-SALES_DATA-SALES = LT_CUS_SALES[].

    ENDLOOP.

    APPEND INITIAL LINE TO LT_TAX_IND ASSIGNING FIELD-SYMBOL(<LFS_TAX_IND>).
    <LFS_TAX_IND>-TASK            = GC_TASK_MODIFY.
    <LFS_TAX_IND>-DATA_KEY-ALAND  = 'TH'.
    <LFS_TAX_IND>-DATA_KEY-TATYP  = 'MWST'.
    <LFS_TAX_IND>-DATA-TAXKD      = US_CUSTOMER-TAXKD.
    <LFS_TAX_IND>-DATAX-TAXKD     = ABAP_TRUE.
    LS_BP-CUSTOMER-CENTRAL_DATA-TAX_IND-TAX_IND    = LT_TAX_IND[].

    "Customer Classification
    LS_BP-CUSTOMER-CENTRAL_DATA-CENTRAL-DATA-KUKLA    = US_CUSTOMER-KUKLA.
    LS_BP-CUSTOMER-CENTRAL_DATA-CENTRAL-DATAX-KUKLA   = ABAP_TRUE.

    "Customer Company
    APPEND INITIAL LINE TO LT_CUS_COMPANY[] ASSIGNING FIELD-SYMBOL(<LFS_CUS_COMPANY>).
    <LFS_CUS_COMPANY>-TASK            = GC_TASK_MODIFY.
    <LFS_CUS_COMPANY>-DATA_KEY-BUKRS  = GC_SDS.

    <LFS_CUS_COMPANY>-DATA-XZVER     = ABAP_TRUE.              "Indicator: Record Payment History ?
    <LFS_CUS_COMPANY>-DATA-BUSAB     = US_CUSTOMER-BUSAB.      "Accounting Clerk Abbreviation
    <LFS_CUS_COMPANY>-DATA-AKONT     = US_CUSTOMER-AKONT.      "Reconciliation Account in General Ledger
    <LFS_CUS_COMPANY>-DATA-FDGRV     = US_CUSTOMER-FDGRV.     "Planning Group
    <LFS_CUS_COMPANY>-DATA-XAUSZ     = '2'.                   "Indicator for Periodic Account Statements
    <LFS_CUS_COMPANY>-DATA-ZUAWA     = US_CUSTOMER-ZUAWA.     "Key for sorting according to assignment numbers
    <LFS_CUS_COMPANY>-DATA-ZTERM     = US_CUSTOMER-ZTERM.

    "markX
    <LFS_CUS_COMPANY>-DATAX-FDGRV     = ABAP_TRUE.
    <LFS_CUS_COMPANY>-DATAX-XZVER     = ABAP_TRUE.
    <LFS_CUS_COMPANY>-DATAX-BUSAB     = ABAP_TRUE.
    <LFS_CUS_COMPANY>-DATAX-AKONT     = ABAP_TRUE.
    <LFS_CUS_COMPANY>-DATAX-XAUSZ     = ABAP_TRUE.
    <LFS_CUS_COMPANY>-DATAX-ZUAWA     = ABAP_TRUE.
    <LFS_CUS_COMPANY>-DATAX-ZTERM     = ABAP_TRUE.

    "Dunning
    APPEND INITIAL LINE TO LT_CUS_DUNNING[] ASSIGNING FIELD-SYMBOL(<LFS_CUS_DUNNING>).

    <LFS_CUS_DUNNING>-TASK              = GC_TASK_MODIFY.
    <LFS_CUS_DUNNING>-DATA-MAHNA        = US_CUSTOMER-MAHNA.
    <LFS_CUS_DUNNING>-DATAX-MAHNA       = ABAP_TRUE.
    <LFS_CUS_COMPANY>-DUNNING-DUNNING   = LT_CUS_DUNNING[].
    LS_BP-CUSTOMER-COMPANY_DATA-COMPANY = LT_CUS_COMPANY.

  ENDIF.

*------------------------------------------------------------------------------
* Vendor :
*------------------------------------------------------------------------------
  "Get Vendor Data
  IF LINE_EXISTS( UT_BP_ROLE[ ROLE = GC_ROLE-VENDOR ] ).
    LV_VEND_UPD = ABAP_TRUE.
    CLEAR LV_TASK_ACT.
*    IF LINE_EXISTS( LT_BP_ROLE[ ROLE = GC_ROLE-VENDOR ] ).
*      "edit mode
*      LV_TASK_ACT                   = GC_TASK_UPDATE.
*    ELSE.
*      "Create mode
*      LV_TASK_ACT                   = GC_TASK_INSERT.
*    ENDIF.
    IF LINE_EXISTS( LT_BP_ROLE[ ROLE = GC_ROLE-VENDOR ] ) OR
       LINE_EXISTS( LT_BP_ROLE[ ROLE = GC_ROLE-VENDOR_FI ] ).
      "edit mode
      LV_TASK_ACT                   = GC_TASK_UPDATE.
    ELSE.
      "Create mode
      LV_TASK_ACT                   = GC_TASK_INSERT.
    ENDIF.

    LS_BP-VENDOR-HEADER-OBJECT_TASK             = LV_TASK_ACT.
    LS_BP-VENDOR-HEADER-OBJECT_INSTANCE-LIFNR   = GV_PARTNER.

    "Purchase DATA
    APPEND INITIAL LINE TO LT_VEN_PUR ASSIGNING FIELD-SYMBOL(<LFS_VEN_PUR>).
    <LFS_VEN_PUR>-TASK             = GC_TASK_MODIFY.
    <LFS_VEN_PUR>-DATA_KEY-EKORG   = GC_SDS.
    <LFS_VEN_PUR>-DATA-ZTERM       = US_VENDOR-ZTERM.
    <LFS_VEN_PUR>-DATA-WAERS       = US_CENTRAL-CURRENCY.

    LS_BP-VENDOR-PURCHASING_DATA-PURCHASING = LT_VEN_PUR[].

    APPEND INITIAL LINE TO LT_VEN_COMPANY ASSIGNING FIELD-SYMBOL(<LFS_VEN_COMPANY>).

    "insert
    <LFS_VEN_COMPANY>-TASK            = GC_TASK_MODIFY.
    <LFS_VEN_COMPANY>-DATA_KEY-BUKRS  = GC_SDS.

    "Data
    <LFS_VEN_COMPANY>-DATA-ZTERM      = US_VENDOR-ZTERM.    "Terms of Payment Key
    <LFS_VEN_COMPANY>-DATA-AKONT      = US_VENDOR-AKONT.    "Reconciliation Account in General Ledger
    <LFS_VEN_COMPANY>-DATA-ZUAWA      = US_VENDOR-ZUAWA.    "SortKey
    <LFS_VEN_COMPANY>-DATA-FDGRV      = US_VENDOR-FDGRV.    "Planning Group

    "Mark X
    <LFS_VEN_COMPANY>-DATAX-ZTERM     = ABAP_TRUE.
    <LFS_VEN_COMPANY>-DATAX-AKONT     = ABAP_TRUE.
    <LFS_VEN_COMPANY>-DATAX-FDGRV     = ABAP_TRUE.
    <LFS_VEN_COMPANY>-DATAX-ZUAWA     = ABAP_TRUE.

    LS_BP-VENDOR-COMPANY_DATA-COMPANY       = LT_VEN_COMPANY[].   "Assing Vendor Company Data
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

        LS_RETURN-MESSAGE       = |Business Partner { GV_PARTNER } has been updated.|.
        LS_RETURN-TYPE          = 'S'.
        LS_RETURN-MESSAGE_V1    = GV_PARTNER.
        APPEND LS_RETURN TO CT_RETURN.

        LV_BU_PARTNER = CS_CENTRAL-BPARTNER = GV_PARTNER.


        PERFORM UPDATE_BRANCH     USING LV_BU_PARTNER
                                        UV_PARTN_CAT
                                        US_CENTRAL.

        IF LV_VEND_UPD = ABAP_TRUE.   "vendor update mark x
          PERFORM CHG_BANK_DETAIL   USING LV_BU_PARTNER
                                          US_VENDOR.
        ENDIF.


        IF LV_CUST_UPD = ABAP_TRUE.   "Customer update mark x
          PERFORM UPDATE_CUST_OTHER USING LV_BU_PARTNER
                                          US_CUSTOMER.

          "update contact person
          PERFORM UPD_CONTACT_PERSON USING LV_BU_PARTNER
                                           SPACE
                                    CHANGING CT_CONTACT
                                             CT_RETURN.

*        "update Shipto
          PERFORM UPD_SHIPTO_PFN     USING LV_BU_PARTNER
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


        ENDIF.

        IF LV_EMP_FLAG = ABAP_TRUE.
          "Update Address of Employee
          PERFORM UPD_EMP_ADDR     USING LV_BU_PARTNER
                                         UT_ADDRESS[]
                                   CHANGING CT_RETURN[].
        ENDIF.



    ENDCASE.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form upd_contact_person
*&---------------------------------------------------------------------*
*& Update contact person logic
*&---------------------------------------------------------------------*
FORM UPD_CONTACT_PERSON  USING    UV_BU_PARTNER TYPE BU_PARTNER
                                  UV_TEST       TYPE CHAR01
                         CHANGING CT_CONTACT    TYPE ZSDSFIS082_TT
                                  CT_RETURN     TYPE BAPIRET2_T.

  DATA: LT_CONTACT_NEW TYPE ZSDSFIS082_TT,
        LT_CONTACT_OLD TYPE ZSDSFIS082_TT,
        LT_CONTACT_DEL TYPE ZSDSFIS082_TT.

  CHECK CT_CONTACT IS NOT INITIAL.

  SELECT *
    FROM BUT050
    INTO TABLE @DATA(LT_BUT050)
    WHERE PARTNER1 = @GV_PARTNER
      AND RELTYP = 'BUR001'.    "Has contact person
  IF SY-SUBRC = 0.
    SORT LT_BUT050 BY PARTNER2.
  ENDIF.


  LOOP AT CT_CONTACT ASSIGNING FIELD-SYMBOL(<LFS_CONTACT>).

    READ TABLE LT_BUT050 TRANSPORTING NO FIELDS
                         WITH KEY PARTNER2 = <LFS_CONTACT>-PARTNER2
                         BINARY SEARCH.
    IF SY-SUBRC = 0.
      IF <LFS_CONTACT>-FLAG_DELETE = ABAP_TRUE.
        APPEND <LFS_CONTACT> TO LT_CONTACT_DEL.     "Delete
      ELSE.
        APPEND <LFS_CONTACT> TO LT_CONTACT_OLD.     "Change
      ENDIF.
    ELSE.
      APPEND <LFS_CONTACT> TO LT_CONTACT_NEW.       "Create
    ENDIF.
  ENDLOOP.


  IF LT_CONTACT_OLD IS NOT INITIAL.
    PERFORM CHG_CONTACT_PERSON USING UV_BU_PARTNER
                                     UV_TEST
                                CHANGING LT_CONTACT_OLD
                                         CT_RETURN.
  ENDIF.

  IF LT_CONTACT_NEW IS NOT INITIAL.
    PERFORM CRE_CONTACT_PERSON USING UV_BU_PARTNER
                                     UV_TEST
                               CHANGING LT_CONTACT_NEW
                                        CT_RETURN.
  ENDIF.

  IF LT_CONTACT_DEL[] IS NOT INITIAL.
    PERFORM DEL_CONTACT_PERSON USING UV_BU_PARTNER
                                     UV_TEST
                               CHANGING LT_CONTACT_DEL
                                        CT_RETURN.
  ENDIF.

  REFRESH CT_CONTACT.
  APPEND LINES OF LT_CONTACT_OLD TO CT_CONTACT.
  APPEND LINES OF LT_CONTACT_NEW TO CT_CONTACT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form upd_shipto_pfn
*&---------------------------------------------------------------------*
*& Update Shipto Party
*&---------------------------------------------------------------------*
FORM UPD_SHIPTO_PFN  USING    UV_PARTNER TYPE BU_PARTNER
                              UV_TEST    TYPE CHAR01
                     CHANGING CT_PARTNER TYPE ZSDSFIS083_TT
                              CT_RETURN  TYPE BAPIRET2_T.

  DATA: LS_SHIPTO      TYPE ZSDSFIS083,
        LT_PARTNER_NEW TYPE ZSDSFIS083_TT,    "New Shipto
        LT_PARTNER_OLD TYPE ZSDSFIS083_TT,    "Change Mode
        LT_PARTNER_DEL TYPE ZSDSFIS083_TT,    "Delete(Unassign)
        LT_PARTNER_RE  TYPE ZSDSFIS083_TT.    "Re-New(exist & assign)

  CHECK CT_PARTNER IS NOT INITIAL.

  DATA(LT_SHIPTO) = CT_PARTNER.

  DELETE LT_SHIPTO
  WHERE PARVW <> GC_PARTNER_FN-SHIPTO.

  "Partner Function (Sales Emp and Shipto)
  SELECT *
    FROM KNVP
    INTO TABLE @DATA(LT_KNVP)
    WHERE KUNNR = @GV_PARTNER
      AND PARVW = @GC_PARTNER_FN-SHIPTO
      AND KUNN2 <> @GV_PARTNER.
  IF SY-SUBRC = 0.
    SORT LT_KNVP BY KUNN2 ASCENDING.
*                    parza ASCENDING.
  ENDIF.

  IF LT_SHIPTO IS NOT INITIAL.
    DATA(LT_SHIPTO_EXT) = LT_SHIPTO[].
    DELETE LT_SHIPTO_EXT WHERE BPARTNER = SPACE.

    IF LT_SHIPTO_EXT[] IS NOT INITIAL.
      SELECT PARTNER,
             MC_NAME1,
             MC_NAME2
        FROM BUT000
        INTO TABLE @DATA(LT_BUT000)
        FOR ALL ENTRIES IN @LT_SHIPTO_EXT
        WHERE PARTNER = @LT_SHIPTO_EXT-BPARTNER.
      IF SY-SUBRC = 0.
        SORT LT_BUT000 BY PARTNER.
      ENDIF.
    ENDIF.
  ENDIF.

  LOOP AT LT_SHIPTO INTO LS_SHIPTO.
    READ TABLE LT_KNVP INTO DATA(LS_KNVP)
                         WITH KEY KUNN2 = LS_SHIPTO-BPARTNER
*                                  parza = ls_shipto-parza
                         BINARY SEARCH.
    IF SY-SUBRC = 0.
      LS_SHIPTO-PARZA = LS_KNVP-PARZA.
      IF LS_SHIPTO-FLAG_DELETE = ABAP_TRUE.
        APPEND LS_SHIPTO TO LT_PARTNER_DEL.
      ELSE.
*      old
        APPEND LS_SHIPTO TO LT_PARTNER_OLD.
      ENDIF.
    ELSE.
      READ TABLE LT_BUT000 INTO DATA(LS_BUT000)
                           WITH KEY PARTNER = LS_SHIPTO-BPARTNER
                           BINARY SEARCH.
      IF SY-SUBRC = 0.
        APPEND LS_SHIPTO TO LT_PARTNER_RE.
      ELSE.
*        new(abs)
        APPEND LS_SHIPTO TO LT_PARTNER_NEW.
      ENDIF.

    ENDIF.
  ENDLOOP.

  IF LT_PARTNER_OLD IS NOT INITIAL.
    PERFORM CHG_SHIPTO_PFN     USING UV_PARTNER
                                     UV_TEST
                               CHANGING LT_PARTNER_OLD
                                        CT_RETURN.
  ENDIF.

  IF LT_PARTNER_NEW IS NOT INITIAL.
    PERFORM CRE_SHIPTO_PFN     USING UV_PARTNER
                                     UV_TEST
                               CHANGING LT_PARTNER_NEW
                                        CT_RETURN.
  ENDIF.

  IF LT_PARTNER_DEL IS NOT INITIAL.
    PERFORM DEL_SHIPTO_PFN     USING UV_PARTNER
                                     UV_TEST
                               CHANGING LT_PARTNER_DEL
                                        CT_RETURN.
  ENDIF.

  IF LT_PARTNER_RE IS NOT INITIAL.
    PERFORM RE_SHIPTO_PFN     USING UV_PARTNER
                                     UV_TEST
                               CHANGING LT_PARTNER_RE
                                        CT_RETURN.
  ENDIF.

  DELETE CT_PARTNER WHERE PARVW = GC_PARTNER_FN-SHIPTO.
  DELETE LT_PARTNER_OLD WHERE FLAG_DELETE = ABAP_TRUE.

  APPEND LINES OF LT_PARTNER_OLD TO CT_PARTNER.
  APPEND LINES OF LT_PARTNER_NEW TO CT_PARTNER.
  APPEND LINES OF LT_PARTNER_RE TO CT_PARTNER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form chg_shipto_pfn
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM CHG_SHIPTO_PFN  USING    UV_PARTNER TYPE BU_PARTNER
                              UV_TEST    TYPE CHAR01
                     CHANGING CT_PARTNER TYPE ZSDSFIS083_TT
                              CT_RETURN  TYPE BAPIRET2_T.

  DATA: LV_GUID      TYPE BU_PARTNER_GUID,
        LS_BP        TYPE CVIS_EI_EXTERN,
        LT_BP        TYPE CVIS_EI_EXTERN_T,
        LT_SMTP      TYPE BUS_EI_BUPA_SMTP_T,
        LT_PHONE     TYPE BUS_EI_BUPA_TELEPHONE_T,
        LT_ADDRESS   TYPE BUS_EI_BUPA_ADDRESS_T,
        LT_FAX       TYPE BUS_EI_BUPA_FAX_T,
        LS_RETURN    TYPE BAPIRET2,
        LV_ERROR     TYPE CHAR01,
        LV_SHIPTO    TYPE BU_PARTNER,
        LS_ROLE      TYPE BUS_EI_BUPA_ROLES,
        LT_ROLE      TYPE BUS_EI_BUPA_ROLES_T,
        LT_CUS_SALES TYPE CMDS_EI_SALES_T,
        LT_TAX_IND   TYPE CMDS_EI_TAX_IND_T.

  DATA: LV_GUID_ADDR TYPE BAPIBUS1006_ADDRESSES_INT-ADDRGUID.


  LOOP AT CT_PARTNER ASSIGNING FIELD-SYMBOL(<LFS_PARTNER>).

    REFRESH LT_TAX_IND[].
    CLEAR: LV_GUID_ADDR.

    CALL FUNCTION 'BUPA_NUMBERS_GET'
      EXPORTING
        IV_PARTNER      = <LFS_PARTNER>-BPARTNER
      IMPORTING
        EV_PARTNER_GUID = LV_GUID.

    IF <LFS_PARTNER>-FLAG_DELETE = ABAP_TRUE.
      LS_BP-PARTNER-HEADER-OBJECT_TASK  = GC_TASK_DELETE.
    ELSE.
      LS_BP-PARTNER-HEADER-OBJECT_TASK  = GC_TASK_UPDATE.
    ENDIF.

    LS_BP-PARTNER-HEADER-OBJECT_INSTANCE-BPARTNERGUID = LV_GUID.
    LS_BP-PARTNER-HEADER-OBJECT_INSTANCE-BPARTNER     = <LFS_PARTNER>-BPARTNER.

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
    <LFS_PHONE>-CONTACT-TASK  =  GC_TASK_MODIFY.
    <LFS_PHONE>-CONTACT-DATA-TELEPHONE        = <LFS_PARTNER>-MOBILE_PHONE.
    <LFS_PHONE>-CONTACT-DATA-R_3_USER         = '3'.
    <LFS_PHONE>-CONTACT-DATA-CONSNUMBER       = '001'.
    <LFS_PHONE>-CONTACT-DATA-COUNTRY          = 'TH'.
    "Mobile(markX)
    <LFS_PHONE>-CONTACT-DATAX-TELEPHONE       = ABAP_TRUE.
    <LFS_PHONE>-CONTACT-DATAX-R_3_USER        = ABAP_TRUE.
    <LFS_PHONE>-CONTACT-DATAX-CONSNUMBER      = ABAP_TRUE.
    <LFS_PHONE>-CONTACT-DATAX-COUNTRY         = ABAP_TRUE.

    "Phone
    APPEND INITIAL LINE TO LT_PHONE ASSIGNING <LFS_PHONE>.
    <LFS_PHONE>-CONTACT-TASK                 =  GC_TASK_MODIFY.
    <LFS_PHONE>-CONTACT-DATA-CONSNUMBER      = '002'.
    <LFS_PHONE>-CONTACT-DATA-TELEPHONE       = <LFS_PARTNER>-TEL_NUMBER.
    <LFS_PHONE>-CONTACT-DATA-COUNTRY         = 'TH'.
    "Phone(markX)
    <LFS_PHONE>-CONTACT-DATAX-TELEPHONE      = ABAP_TRUE.
    <LFS_PHONE>-CONTACT-DATAX-CONSNUMBER     = ABAP_TRUE.
    <LFS_PHONE>-CONTACT-DATAX-COUNTRY        = ABAP_TRUE.

    "Fax
*    APPEND INITIAL LINE TO lt_fax ASSIGNING FIELD-SYMBOL(<lfs_fax>).
*    <lfs_fax>-contact-task                  = gc_task_modify.
*    <lfs_fax>-contact-data-fax              = <lfs_partner>-fax_number.
*    <lfs_fax>-contact-datax-fax             = abap_true.

    "Email
    APPEND INITIAL LINE TO LT_SMTP ASSIGNING FIELD-SYMBOL(<LFS_SMTP>).
    <LFS_SMTP>-CONTACT-TASK                 = GC_TASK_MODIFY.
    <LFS_SMTP>-CONTACT-DATA-E_MAIL          = <LFS_PARTNER>-EMAIL.
    <LFS_SMTP>-CONTACT-DATAX-E_MAIL         = ABAP_TRUE.

    LS_BP-PARTNER-CENTRAL_DATA-COMMUNICATION-PHONE-PHONE = LT_PHONE[].
*    ls_bp-partner-central_data-communication-fax-fax     = lt_fax[].
    LS_BP-PARTNER-CENTRAL_DATA-COMMUNICATION-SMTP-SMTP   = LT_SMTP[].
*------------------------------------------------------------------------------
* Address data
*------------------------------------------------------------------------------
    APPEND INITIAL LINE TO LT_ADDRESS ASSIGNING FIELD-SYMBOL(<LFS_ADDRESS>).
    <LFS_ADDRESS>-TASK = GC_TASK_MODIFY.
* Operations are store in table TB008S
    <LFS_ADDRESS>-DATA_KEY-OPERATION           = 'XXDFLT'. "Standard operation
    CALL FUNCTION 'BAPI_BUPA_ADDRESSES_GET'
      EXPORTING
        BUSINESSPARTNER     = <LFS_PARTNER>-BPARTNER
      IMPORTING
        STANDARDADDRESSGUID = LV_GUID_ADDR.
    <LFS_ADDRESS>-DATA_KEY-GUID                = LV_GUID_ADDR.
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
*    <LFS_ADDRESS>-DATA-COMMUNICATION-FAX-FAX      = LT_FAX[].
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

    "Customer
    LS_BP-CUSTOMER-HEADER-OBJECT_INSTANCE-KUNNR  = <LFS_PARTNER>-BPARTNER.
    LS_BP-CUSTOMER-HEADER-OBJECT_TASK            = GC_TASK_UPDATE.

    SELECT
      VKORG,
      VTWEG,
      SPART
     FROM KNVV
     INTO TABLE @DATA(LT_KNVV)
     WHERE KUNNR = @GV_PARTNER.
    LOOP AT LT_KNVV INTO DATA(LS_KNVV).
      "Sale data
      APPEND INITIAL LINE TO LT_CUS_SALES ASSIGNING FIELD-SYMBOL(<LFS_CUS_SALES>).
      <LFS_CUS_SALES>-TASK           = GC_TASK_MODIFY.
      <LFS_CUS_SALES>-DATA_KEY-VKORG = LS_KNVV-VKORG.
      <LFS_CUS_SALES>-DATA_KEY-VTWEG = LS_KNVV-VTWEG.
      <LFS_CUS_SALES>-DATA_KEY-SPART = LS_KNVV-SPART.
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
    <LFS_TAX_IND>-TASK            = GC_TASK_MODIFY.
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

ENDFORM.
*&---------------------------------------------------------------------*
*& Form chg_contact_person
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM CHG_CONTACT_PERSON  USING UV_PARTNER  TYPE BU_PARTNER
                               UV_TEST     TYPE CHAR01
                         CHANGING CT_CONTACT  TYPE ZSDSFIS082_TT
                                  CT_RETURN   TYPE BAPIRET2_T.

  DATA: LV_GUID           TYPE BU_PARTNER_GUID,
        LS_BP             TYPE CVIS_EI_EXTERN,
        LT_BP             TYPE CVIS_EI_EXTERN_T,
        LT_SMTP           TYPE BUS_EI_BUPA_SMTP_T,
        LT_PHONE          TYPE BUS_EI_BUPA_TELEPHONE_T,
        LT_ADDRESS        TYPE BUS_EI_BUPA_ADDRESS_T,
        LS_RETURN         TYPE BAPIRET2,
        LV_ERROR          TYPE CHAR01,
        LV_CONTACT_PERSON TYPE BU_PARTNER.

  LOOP AT CT_CONTACT ASSIGNING FIELD-SYMBOL(<LFS_CONTACT>).

    CALL FUNCTION 'BUPA_NUMBERS_GET'
      EXPORTING
        IV_PARTNER      = <LFS_CONTACT>-PARTNER2
      IMPORTING
        EV_PARTNER_GUID = LV_GUID.

    IF <LFS_CONTACT>-FLAG_DELETE = ABAP_TRUE.
      LS_BP-PARTNER-HEADER-OBJECT_TASK  = GC_TASK_DELETE.
    ELSE.
      LS_BP-PARTNER-HEADER-OBJECT_TASK  = GC_TASK_UPDATE.
    ENDIF.

    LS_BP-PARTNER-HEADER-OBJECT_INSTANCE-BPARTNERGUID = LV_GUID.
    LS_BP-PARTNER-HEADER-OBJECT_INSTANCE-BPARTNER     = <LFS_CONTACT>-PARTNER2.

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
    <LFS_PHONE>-CONTACT-TASK                 = GC_TASK_MODIFY.
    <LFS_PHONE>-CONTACT-DATA-TELEPHONE       = <LFS_CONTACT>-TEL1_NUMBR.
    <LFS_PHONE>-CONTACT-DATA-EXTENSION       = <LFS_CONTACT>-TEL1_EXT.
    <LFS_PHONE>-CONTACT-DATAX-TELEPHONE      = ABAP_TRUE.
    <LFS_PHONE>-CONTACT-DATAX-EXTENSION      = ABAP_TRUE.

    "Email
    APPEND INITIAL LINE TO LT_SMTP ASSIGNING FIELD-SYMBOL(<LFS_SMTP>).
    <LFS_SMTP>-CONTACT-TASK                 = GC_TASK_MODIFY.
    <LFS_SMTP>-CONTACT-DATA-E_MAIL          = <LFS_CONTACT>-E_MAIL.
    <LFS_SMTP>-CONTACT-DATAX-E_MAIL         = ABAP_TRUE.

    LS_BP-PARTNER-CENTRAL_DATA-COMMUNICATION-PHONE-PHONE = LT_PHONE[].
    LS_BP-PARTNER-CENTRAL_DATA-COMMUNICATION-SMTP-SMTP   = LT_SMTP[].

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
    ENDIF.
    REFRESH: LT_RETURN_MAP,
             LT_RETURN2.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form del_contact_person
*&---------------------------------------------------------------------*
*& Delete contact person
*&---------------------------------------------------------------------*
FORM DEL_CONTACT_PERSON  USING UV_PARTNER  TYPE BU_PARTNER
                               UV_TEST     TYPE CHAR01
                         CHANGING CT_CONTACT  TYPE ZSDSFIS082_TT
                                  CT_RETURN   TYPE BAPIRET2_T.

  DATA: LT_RETURN TYPE BAPIRET2_TT.

  CHECK UV_TEST IS INITIAL.

  LOOP AT CT_CONTACT ASSIGNING FIELD-SYMBOL(<LFS_CONTACT>).

    CALL FUNCTION 'BAPI_BUPR_RELATIONSHIP_DELETE'
      EXPORTING
        BUSINESSPARTNER1     = GV_PARTNER
        BUSINESSPARTNER2     = <LFS_CONTACT>-PARTNER2
        RELATIONSHIPCATEGORY = 'BUR001'
      TABLES
        RETURN               = LT_RETURN.

    IF LINE_EXISTS( LT_RETURN[ TYPE = 'E' ] ) OR
     LINE_EXISTS( LT_RETURN[ TYPE = 'A' ] ).

      GV_FLAG_ERR = ABAP_TRUE.
      LOOP AT LT_RETURN INTO DATA(LS_RETURN)
                        WHERE TYPE = 'E'
                           OR TYPE = 'A'.
        APPEND LS_RETURN TO CT_RETURN.
        EXIT.
      ENDLOOP.
    ELSE.
      PERFORM COMMIT_WORK.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form del_shipto_pfn
*&---------------------------------------------------------------------*
*& Delete Shipto Party
*&---------------------------------------------------------------------*
FORM DEL_SHIPTO_PFN  USING    UV_PARTNER TYPE BU_PARTNER
                              UV_TEST    TYPE CHAR01
                     CHANGING CT_PARTNER TYPE ZSDSFIS083_TT
                              CT_RETURN  TYPE BAPIRET2_T.

  PERFORM UNASSIGN_PARTFN USING UV_PARTNER
                                UV_TEST
                                CT_PARTNER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form unassign_partfn
*&---------------------------------------------------------------------*
*& Unassign Shipto Party
*&---------------------------------------------------------------------*
FORM UNASSIGN_PARTFN  USING UV_PARTNER TYPE BU_PARTNER      "original
                            UV_TEST    TYPE CHAR01
                            UT_PARTNER TYPE ZSDSFIS083_TT.  "Shipto

  DATA: LT_PARTNER TYPE ZSDSFIS083_TT,
        LV_GUID    TYPE BU_PARTNER_GUID.

  DATA:
    LS_BP           TYPE CVIS_EI_EXTERN,
    LT_BP           TYPE CVIS_EI_EXTERN_T,
    LT_PARTNER_FUNC TYPE CMDS_EI_FUNCTIONS_T,
    LT_CUS_SALES    TYPE CMDS_EI_SALES_T,
    LV_PARZA        TYPE KNVP-PARZA.

  LT_PARTNER[] = UT_PARTNER[].

  DELETE LT_PARTNER WHERE BPARTNER IS INITIAL.

  CALL FUNCTION 'BUPA_NUMBERS_GET'
    EXPORTING
      IV_PARTNER      = UV_PARTNER
    IMPORTING
      EV_PARTNER_GUID = LV_GUID.

  LS_BP-PARTNER-HEADER-OBJECT_TASK  = GC_TASK_UPDATE.
  LS_BP-PARTNER-HEADER-OBJECT_INSTANCE-BPARTNERGUID = LV_GUID.
  LS_BP-PARTNER-HEADER-OBJECT_INSTANCE-BPARTNER     = UV_PARTNER.

  "Sale data
  REFRESH: LT_PARTNER_FUNC[].
  LOOP AT GT_VTWEG INTO DATA(LS_VTWEG).
    APPEND INITIAL LINE TO LT_CUS_SALES ASSIGNING FIELD-SYMBOL(<LFS_CUS_SALES>).

    LOOP AT LT_PARTNER INTO DATA(LS_PARTNER).   "Partner Function
      LV_PARZA = LV_PARZA + 1.
      APPEND INITIAL LINE TO LT_PARTNER_FUNC ASSIGNING FIELD-SYMBOL(<LFS_PARTNER_FUNC>).
      <LFS_PARTNER_FUNC>-TASK            = GC_TASK_DELETE.
      <LFS_PARTNER_FUNC>-DATA_KEY-PARVW  = LS_PARTNER-PARVW.    "WE
      <LFS_PARTNER_FUNC>-DATA_KEY-PARZA  = LS_PARTNER-PARZA.
      <LFS_PARTNER_FUNC>-DATA-PARTNER    = LS_PARTNER-BPARTNER.
      <LFS_PARTNER_FUNC>-DATAX-PARTNER   = ABAP_TRUE.

    ENDLOOP.

    <LFS_CUS_SALES>-TASK           = GC_TASK_UPDATE.
    <LFS_CUS_SALES>-DATA_KEY-VKORG = GC_SALES_AREA-VKORG.
    <LFS_CUS_SALES>-DATA_KEY-VTWEG = LS_VTWEG-LOW.
    <LFS_CUS_SALES>-DATA_KEY-SPART = GC_SALES_AREA-SPART.

    <LFS_CUS_SALES>-FUNCTIONS-FUNCTIONS = LT_PARTNER_FUNC[].
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
      I_TEST_RUN = UV_TEST
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
    GV_FLAG_ERR = ABAP_TRUE.
  ELSE.
    PERFORM COMMIT_WORK.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form chg_bank_detail
*&---------------------------------------------------------------------*
*& Change Bank deatil
*&---------------------------------------------------------------------*
FORM CHG_BANK_DETAIL  USING UV_PARTNER TYPE BU_PARTNER
                            US_VENDOR  TYPE ZSDSFIS084.


  DATA: LS_BANK_DETAIL  TYPE BAPIBUS1006_BANKDETAIL,
        LS_BANK_DETAILX TYPE BAPIBUS1006_BANKDETAIL_X,
        LS_BANK_OUT     TYPE BAPIBUS1006_HEAD-BANKDETAILID,
        LT_RETURN       TYPE BAPIRET2_TT.

  DATA: LT_BANK_DETAILS  TYPE STANDARD TABLE OF BAPIBUS1006_BANKDETAILS.


  SELECT SINGLE *
    FROM BNKA
    INTO @DATA(LS_BNKA)
    WHERE BANKS = @US_VENDOR-BANKS
      AND BANKL = @US_VENDOR-BANKL.
  IF SY-SUBRC = 0.

    CALL FUNCTION 'BAPI_BUPA_BANKDETAILS_GET'
      EXPORTING
        BUSINESSPARTNER = UV_PARTNER
      TABLES
        BANKDETAILS     = LT_BANK_DETAILS
        RETURN          = LT_RETURN.

    READ TABLE LT_BANK_DETAILS  INTO DATA(LS_BANK_DETAILS)
                                INDEX 1.
    IF SY-SUBRC = 0.
      IF LS_BANK_DETAILS-BANK_CTRY = LS_BNKA-BANKS AND
         LS_BANK_DETAILS-BANK_KEY  = LS_BNKA-BANKL AND
         LS_BANK_DETAILS-BANK_ACCT = US_VENDOR-BANKN AND
         LS_BANK_DETAILS-BANK_REF  = US_VENDOR-BKREF.
        "Same bank (dont do anything)
      ELSE.
        LS_BANK_DETAIL-BANK_CTRY = LS_BNKA-BANKS.
        LS_BANK_DETAIL-BANK_KEY  = LS_BNKA-BANKL.
        LS_BANK_DETAIL-BANK_ACCT = US_VENDOR-BANKN.
        LS_BANK_DETAIL-BANK_REF  = US_VENDOR-BKREF.

        "Mark x
        LS_BANK_DETAILX-BANK_CTRY = ABAP_TRUE.
        LS_BANK_DETAILX-BANK_KEY  = ABAP_TRUE.
        LS_BANK_DETAILX-BANK_ACCT = ABAP_TRUE.
        LS_BANK_DETAILX-BANK_REF  = ABAP_TRUE.

        CALL FUNCTION 'BAPI_BUPA_BANKDETAIL_CHANGE'
          EXPORTING
            BUSINESSPARTNER  = UV_PARTNER
            BANKDETAILID     = LS_BANK_DETAILS-BANKDETAILID
            BANKDETAILDATA   = LS_BANK_DETAIL
            BANKDETAILDATA_X = LS_BANK_DETAILX
          TABLES
            RETURN           = LT_RETURN.

        LOOP AT LT_RETURN INTO DATA(LS_RETURN) WHERE TYPE = 'E'.
          EXIT.
        ENDLOOP.
        IF SY-SUBRC <> 0.
          PERFORM COMMIT_WORK.
        ENDIF.

      ENDIF.
    ELSE.

      LS_BANK_DETAIL-BANK_CTRY = US_VENDOR-BANKS.
      LS_BANK_DETAIL-BANK_KEY  = US_VENDOR-BANKL.
      LS_BANK_DETAIL-BANK_ACCT = US_VENDOR-BANKN.
      LS_BANK_DETAIL-BANK_REF  = US_VENDOR-BKREF.

      CALL FUNCTION 'BAPI_BUPA_BANKDETAIL_ADD'
        EXPORTING
          BUSINESSPARTNER = UV_PARTNER
          BANKDETAILDATA  = LS_BANK_DETAIL
        IMPORTING
          BANKDETAILIDOUT = LS_BANK_OUT
        TABLES
          RETURN          = LT_RETURN.

      LOOP AT LT_RETURN INTO LS_RETURN WHERE TYPE = 'E'.
        EXIT.
      ENDLOOP.
      IF SY-SUBRC <> 0.
        PERFORM COMMIT_WORK.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form re_shipto_pfn
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM RE_SHIPTO_PFN  USING UV_PARTNER TYPE BU_PARTNER
                          UV_TEST    TYPE CHAR01
                    CHANGING CT_PARTNER TYPE ZSDSFIS083_TT
                             CT_RETURN  TYPE BAPIRET2_T.

  DATA: LV_GUID      TYPE BU_PARTNER_GUID,
        LS_BP        TYPE CVIS_EI_EXTERN,
        LT_BP        TYPE CVIS_EI_EXTERN_T,
        LT_SMTP      TYPE BUS_EI_BUPA_SMTP_T,
        LT_PHONE     TYPE BUS_EI_BUPA_TELEPHONE_T,
        LT_ADDRESS   TYPE BUS_EI_BUPA_ADDRESS_T,
        LT_FAX       TYPE BUS_EI_BUPA_FAX_T,
        LS_RETURN    TYPE BAPIRET2,
        LV_ERROR     TYPE CHAR01,
        LV_SHIPTO    TYPE BU_PARTNER,
        LS_ROLE      TYPE BUS_EI_BUPA_ROLES,
        LT_ROLE      TYPE BUS_EI_BUPA_ROLES_T,
        LT_CUS_SALES TYPE CMDS_EI_SALES_T,
        LT_TAX_IND   TYPE CMDS_EI_TAX_IND_T.

  LOOP AT CT_PARTNER ASSIGNING FIELD-SYMBOL(<LFS_PARTNER>).

    REFRESH LT_TAX_IND[].

    CALL FUNCTION 'BUPA_NUMBERS_GET'
      EXPORTING
        IV_PARTNER      = <LFS_PARTNER>-BPARTNER
      IMPORTING
        EV_PARTNER_GUID = LV_GUID.

    IF <LFS_PARTNER>-FLAG_DELETE = ABAP_TRUE.
      LS_BP-PARTNER-HEADER-OBJECT_TASK  = GC_TASK_DELETE.
    ELSE.
      LS_BP-PARTNER-HEADER-OBJECT_TASK  = GC_TASK_UPDATE.
    ENDIF.

    LS_BP-PARTNER-HEADER-OBJECT_INSTANCE-BPARTNERGUID = LV_GUID.
    LS_BP-PARTNER-HEADER-OBJECT_INSTANCE-BPARTNER     = <LFS_PARTNER>-BPARTNER.

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
    <LFS_PHONE>-CONTACT-TASK  =  GC_TASK_MODIFY.
    <LFS_PHONE>-CONTACT-DATA-TELEPHONE        = <LFS_PARTNER>-MOBILE_PHONE.
    <LFS_PHONE>-CONTACT-DATA-R_3_USER         = '3'.
    <LFS_PHONE>-CONTACT-DATAX-TELEPHONE       = ABAP_TRUE.
    <LFS_PHONE>-CONTACT-DATAX-R_3_USER        = ABAP_TRUE.

    "Phone
    APPEND INITIAL LINE TO LT_PHONE ASSIGNING <LFS_PHONE>.
    <LFS_PHONE>-CONTACT-TASK                 =  GC_TASK_MODIFY.
    <LFS_PHONE>-CONTACT-DATA-TELEPHONE       = <LFS_PARTNER>-TEL_NUMBER.
    <LFS_PHONE>-CONTACT-DATAX-TELEPHONE      = ABAP_TRUE.


    "Fax
    APPEND INITIAL LINE TO LT_FAX ASSIGNING FIELD-SYMBOL(<LFS_FAX>).
    <LFS_FAX>-CONTACT-TASK                  = GC_TASK_MODIFY.
    <LFS_FAX>-CONTACT-DATA-FAX              = <LFS_PARTNER>-FAX_NUMBER.
    <LFS_FAX>-CONTACT-DATAX-FAX             = ABAP_TRUE.

    "Email
    APPEND INITIAL LINE TO LT_SMTP ASSIGNING FIELD-SYMBOL(<LFS_SMTP>).
    <LFS_SMTP>-CONTACT-TASK                 = GC_TASK_MODIFY.
    <LFS_SMTP>-CONTACT-DATA-E_MAIL          = <LFS_PARTNER>-EMAIL.
    <LFS_SMTP>-CONTACT-DATAX-E_MAIL         = ABAP_TRUE.

    LS_BP-PARTNER-CENTRAL_DATA-COMMUNICATION-PHONE-PHONE = LT_PHONE[].
    LS_BP-PARTNER-CENTRAL_DATA-COMMUNICATION-FAX-FAX     = LT_FAX[].
    LS_BP-PARTNER-CENTRAL_DATA-COMMUNICATION-SMTP-SMTP   = LT_SMTP[].
*------------------------------------------------------------------------------
* Address data
*------------------------------------------------------------------------------
    APPEND INITIAL LINE TO LT_ADDRESS ASSIGNING FIELD-SYMBOL(<LFS_ADDRESS>).
    <LFS_ADDRESS>-TASK = GC_TASK_MODIFY.
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

    SELECT
      VKORG,
      VTWEG,
      SPART
     FROM KNVV
     INTO TABLE @DATA(LT_KNVV)
     WHERE KUNNR = @GV_PARTNER.
    LOOP AT LT_KNVV INTO DATA(LS_KNVV).
      "Sale data
      APPEND INITIAL LINE TO LT_CUS_SALES ASSIGNING FIELD-SYMBOL(<LFS_CUS_SALES>).
      <LFS_CUS_SALES>-TASK           = GC_TASK_MODIFY.
      <LFS_CUS_SALES>-DATA_KEY-VKORG = LS_KNVV-VKORG.
      <LFS_CUS_SALES>-DATA_KEY-VTWEG = LS_KNVV-VTWEG.
      <LFS_CUS_SALES>-DATA_KEY-SPART = LS_KNVV-SPART.
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
    <LFS_TAX_IND>-TASK            = GC_TASK_MODIFY.
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
*& Form change_ship_to
*&---------------------------------------------------------------------*
*& Update shipto party only
*&---------------------------------------------------------------------*
FORM CHANGE_SHIP_TO  USING UV_PARTNER   TYPE BU_PARTNER
                           UV_TEST      TYPE CHAR01
                     CHANGING CT_PARTNER TYPE ZSDSFIS083_TT
                              CT_RETURN  TYPE BAPIRET2_T.

  CLEAR: GV_FLAG_ERR.

  "update Shipto
  PERFORM UPD_SHIPTO_PFN     USING UV_PARTNER
                                   UV_TEST
                             CHANGING CT_PARTNER
                                      CT_RETURN.

  "Check error
  LOOP AT CT_RETURN INTO DATA(LS_RETURN)
                    WHERE TYPE = 'E'
                      OR  TYPE = 'A'.

    GV_FLAG_ERR = ABAP_TRUE.
    EXIT.
  ENDLOOP.

  IF GV_FLAG_ERR <> ABAP_TRUE.
    REFRESH: CT_RETURN[].
    APPEND INITIAL LINE TO CT_RETURN ASSIGNING FIELD-SYMBOL(<LFS_RETURN>).
    <LFS_RETURN>-TYPE       = 'S'.
    <LFS_RETURN>-MESSAGE    = TEXT-S01.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHANGE_CONTACT_PERSON
*&---------------------------------------------------------------------*
*& Change contact person infomation
*&---------------------------------------------------------------------*
FORM CHANGE_CONTACT_PERSON  USING UV_BU_PARTNER TYPE BU_PARTNER
                                  UV_TEST       TYPE CHAR01
                         CHANGING CT_CONTACT    TYPE ZSDSFIS082_TT
                                  CT_RETURN     TYPE BAPIRET2_T.

  CLEAR: GV_FLAG_ERR.

  "update contact person
  PERFORM UPD_CONTACT_PERSON USING GV_PARTNER
                                   UV_TEST
                            CHANGING CT_CONTACT
                                     CT_RETURN.

  "Check error
  LOOP AT CT_RETURN INTO DATA(LS_RETURN)
                    WHERE TYPE = 'E'
                      OR  TYPE = 'A'.

    GV_FLAG_ERR = ABAP_TRUE.
    EXIT.
  ENDLOOP.

  IF GV_FLAG_ERR <> ABAP_TRUE.
    REFRESH: CT_RETURN[].
    APPEND INITIAL LINE TO CT_RETURN ASSIGNING FIELD-SYMBOL(<LFS_RETURN>).
    <LFS_RETURN>-TYPE       = 'S'.
    <LFS_RETURN>-MESSAGE    = TEXT-S01.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPD_EMP_ADDR
*&---------------------------------------------------------------------*
*& Update Employee address
*&---------------------------------------------------------------------*
FORM UPD_EMP_ADDR  USING  UV_PARTNER TYPE BUT000-PARTNER
                          UT_ADDRESS TYPE ZSDSFIS010_TT
                   CHANGING CT_RETURN TYPE BAPIRET2_T.

  CONSTANTS: LC_MODIFY TYPE PSPAR-ACTIO VALUE 'MOD', " Operation Mode (MOD, INS, DEL)
             LC_INSERT TYPE PSPAR-ACTIO VALUE 'INS'.
  CONSTANTS: LC_NDAT        TYPE SY-DATUM    VALUE '99991231',
             LC_INFTYP_0006 TYPE INFTY       VALUE '0006',
             LC_INFTYP_0105 TYPE INFTY       VALUE '0105',
             LC_INFTYP_0002 TYPE INFTY       VALUE '0002'.

  CONSTANTS: BEGIN OF LC_SUBTY,
               MOBILE TYPE PA0105-SUBTY VALUE 'CELL',
               EMAIL  TYPE PA0105-SUBTY VALUE '0010',
             END OF LC_SUBTY.

  DATA: LV_PERNR  TYPE PERNR-PERNR,     " Employee Number
        LS_P0006  TYPE P0006,           " Structure for Infotype 0006
        LS_P0105  TYPE P0105,
        LS_P0002  TYPE P0002,
        LV_TEL    TYPE P0105-USRID,       "Telephone
        LV_EMAIL  TYPE P0105-USRID_LONG,  "Email.
        LS_RET1   TYPE BAPIRETURN1,
        LT_RET1   TYPE STANDARD TABLE OF BAPIRETURN1,
        LS_RETURN TYPE BAPIRET2,
        LV_ACTION TYPE PSPAR-ACTIO,
        LV_FNAME  TYPE P0002-VORNA,
        LV_LNAME  TYPE P0002-NACHN.

  DATA: LT_INTER_ADDR TYPE STANDARD TABLE OF ZSDSFIT059.

  " กำหนดหมายเลขพนักงานที่ต้องการแก้ไข
  LV_PERNR = UV_PARTNER.   " หมายเลขพนักงาน
  REPLACE 'E' IN LV_PERNR WITH SPACE.
  CONDENSE LV_PERNR.

  CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
    EXPORTING
      NUMBER = LV_PERNR
    IMPORTING
      RETURN = LS_RET1.
  IF LS_RET1-TYPE = GC_MSGTYP-ERROR OR
     LS_RET1-TYPE = GC_MSGTYP-ABORT.
    MOVE-CORRESPONDING LS_RET1 TO LS_RETURN.
    APPEND LS_RETURN TO CT_RETURN.
    EXIT.
  ENDIF.

  CLEAR: LV_ACTION.
  SELECT SINGLE *
    INTO @DATA(LS_PA0006)
    FROM  PA0006
    WHERE PERNR = @LV_PERNR.
  IF SY-SUBRC = 0.
    LV_ACTION = LC_MODIFY.
    MOVE-CORRESPONDING LS_PA0006 TO LS_P0006.
  ELSE.
    LV_ACTION      = LC_INSERT.
    LS_P0006-BEGDA = SY-DATUM.
    LS_P0006-ENDDA = LC_NDAT.
  ENDIF.

  "International version
  READ TABLE UT_ADDRESS INTO DATA(LS_ADDRESS_EN)
                    WITH KEY INTERNATIONAL_VERSION = 'X'.
  IF SY-SUBRC = 0.
    SELECT SINGLE *
      FROM ZSDSFIT059
      INTO @DATA(LS_ZSDSFIT059)
      WHERE PERNR     = @LV_PERNR
        AND BPARTNER  = @UV_PARTNER.
    IF SY-SUBRC = 0.
      LS_ZSDSFIT059-ZUPD_DATE = SY-DATUM.
      LS_ZSDSFIT059-ZUPD_TIME = SY-UZEIT.
      LS_ZSDSFIT059-ZUPD_USER = SY-UNAME.
      LS_ZSDSFIT059-ZUPD_PGM  = 'Z_SDSFI_CHANGE_BP'.
    ELSE.
      LS_ZSDSFIT059-ZUPD_DATE =  LS_ZSDSFIT059-ZCRT_DATE = SY-DATUM.
      LS_ZSDSFIT059-ZUPD_TIME =  LS_ZSDSFIT059-ZCRT_TIME = SY-UZEIT.
      LS_ZSDSFIT059-ZUPD_USER =  LS_ZSDSFIT059-ZCRT_USER = SY-UNAME.
      LS_ZSDSFIT059-ZUPD_PGM  =  LS_ZSDSFIT059-ZCRT_PGM  = 'Z_SDSFI_CHANGE_BP'.
    ENDIF.
    LV_TEL                    = LS_ADDRESS_EN-TEL_NUMBER.
    LV_EMAIL                  = LS_ADDRESS_EN-EMAIL.
    LV_FNAME                  = LS_ADDRESS_EN-NAME1.
    LV_LNAME                  = LS_ADDRESS_EN-NAME2.
    MOVE-CORRESPONDING LS_ADDRESS_EN TO LS_ZSDSFIT059.
    MODIFY ZSDSFIT059 FROM LS_ZSDSFIT059.
  ENDIF.

  "Thai version
  READ TABLE UT_ADDRESS INTO DATA(LS_ADDRESS_TH)
                        WITH KEY INTERNATIONAL_VERSION = ''.
  IF SY-SUBRC = 0.
    " เตรียมข้อมูลสำหรับแก้ไขที่อยู่
    CLEAR: LS_P0006-BLDNG, LS_P0006-STRAS.
    LS_P0006-INFTY = LC_INFTYP_0006.
    LS_P0006-LOCAT = LS_ADDRESS_TH-STREET.
    LS_P0006-CONKK = LS_ADDRESS_TH-LOCATION.
    LS_P0006-ORT02 = LS_ADDRESS_TH-DISTRICT.
    LS_P0006-STATE = LS_ADDRESS_TH-REGION.
    LS_P0006-PSTLZ = LS_ADDRESS_TH-POSTL_COD1.
    LS_P0006-ORT01 = LS_ADDRESS_TH-CITY.
    LS_P0006-LAND1 = LS_ADDRESS_TH-COUNTRY.
    LV_TEL         = LS_ADDRESS_TH-TEL_NUMBER.
    LV_EMAIL       = LS_ADDRESS_TH-EMAIL.
    IF LV_FNAME IS INITIAL OR LV_LNAME IS INITIAL.
      LV_FNAME                  = LS_ADDRESS_TH-NAME1.
      LV_LNAME                  = LS_ADDRESS_TH-NAME2.
    ENDIF.
  ENDIF.

  " เรียกใช้ Function Module
  CALL FUNCTION 'HR_INFOTYPE_OPERATION'
    EXPORTING
      INFTY         = LC_INFTYP_0006  " Infotype 0006: Address
      NUMBER        = LV_PERNR        " หมายเลขพนักงาน
      SUBTYPE       = '1'             " Subtype (เช่น Permanent = '1', Temporary = '2')
      VALIDITYBEGIN = LS_P0006-BEGDA  " วันที่เริ่มต้น
      VALIDITYEND   = LS_P0006-ENDDA  " วันที่สิ้นสุด
      RECORD        = LS_P0006        " ข้อมูลที่ต้องการแก้ไข
      OPERATION     = LV_ACTION       " ประเภทการดำเนินการ (MOD, INS, DEL)
    IMPORTING
      RETURN        = LS_RET1.      " ตารางผลลัพธ์
  IF LS_RET1-TYPE = GC_MSGTYP-ERROR OR
     LS_RET1-TYPE = GC_MSGTYP-ABORT.
    MOVE-CORRESPONDING LS_RET1 TO LS_RETURN.
    APPEND LS_RETURN TO CT_RETURN.
    EXIT.
  ENDIF.

  "Update Telephone number & Email
  SELECT *
    FROM PA0105
    INTO TABLE @DATA(LT_PA0105)
    WHERE PERNR = @LV_PERNR.
  IF SY-SUBRC = 0.
    SORT LT_PA0105 BY SUBTY.
  ENDIF.

  "Telephone
  "----------------------------------------------------------------"
  CLEAR: LV_ACTION.
  READ TABLE LT_PA0105 INTO DATA(LS_PA0105)
                       WITH KEY SUBTY = LC_SUBTY-MOBILE
                       BINARY SEARCH.
  IF SY-SUBRC = 0.
    MOVE-CORRESPONDING LS_PA0105 TO LS_P0105.
    LS_P0105-AEDTM   = SY-UNAME.
    LV_ACTION        = LC_MODIFY.
  ELSE.
    LS_P0105-SUBTY   = LC_SUBTY-MOBILE.
    LS_P0105-USRTY   = LC_SUBTY-MOBILE.
    LS_P0105-BEGDA   = SY-DATUM.
    LS_P0105-ENDDA   = LC_NDAT.
    LS_P0105-AEDTM   = SY-UNAME.
    LS_P0105-UNAME   = SY-UNAME.
    LV_ACTION         = LC_INSERT.
  ENDIF.

  LS_P0105-INFTY = LC_INFTYP_0105.
  LS_P0105-USRID = LV_TEL.

  "Update Tel & Email
  CALL FUNCTION 'HR_INFOTYPE_OPERATION'
    EXPORTING
      INFTY         = LC_INFTYP_0105  " Infotype 0105: Communicate
      NUMBER        = LV_PERNR        " หมายเลขพนักงาน
      SUBTYPE       = LS_P0105-SUBTY   "Subtype
      VALIDITYBEGIN = LS_P0105-BEGDA  " วันที่เริ่มต้น
      VALIDITYEND   = LS_P0105-ENDDA  " วันที่สิ้นสุด
      RECORD        = LS_P0105        " ข้อมูลที่ต้องการแก้ไข
      OPERATION     = LV_ACTION       " ประเภทการดำเนินการ (MOD, INS, DEL)
    IMPORTING
      RETURN        = LS_RET1.
  IF LS_RET1-TYPE = GC_MSGTYP-ERROR OR
     LS_RET1-TYPE = GC_MSGTYP-ABORT.
    MOVE-CORRESPONDING LS_RET1 TO LS_RETURN.
    APPEND LS_RETURN TO CT_RETURN.
    EXIT.
  ENDIF.

  "eMAIL
  "----------------------------------------------------------------"
  CLEAR: LS_PA0105,
         LV_ACTION.
  READ TABLE LT_PA0105 INTO LS_PA0105
                       WITH KEY SUBTY = LC_SUBTY-EMAIL
                       BINARY SEARCH.
  IF SY-SUBRC = 0.
    MOVE-CORRESPONDING LS_PA0105 TO LS_P0105.
    LS_P0105-AEDTM   = SY-UNAME.
    LV_ACTION        = LC_MODIFY.
  ELSE.
    LS_P0105-SUBTY   = LC_SUBTY-EMAIL.
    LS_P0105-USRTY   = LC_SUBTY-EMAIL.
    LS_P0105-BEGDA   = SY-DATUM.
    LS_P0105-ENDDA   = LC_NDAT.
    LS_P0105-AEDTM   = SY-UNAME.
    LS_P0105-UNAME   = SY-UNAME.
    LV_ACTION        = LC_INSERT.
  ENDIF.

  LS_P0105-INFTY      = LC_INFTYP_0105.
  LS_P0105-USRID_LONG = LV_EMAIL.

  "Update address info type 0006
  CALL FUNCTION 'HR_INFOTYPE_OPERATION'
    EXPORTING
      INFTY         = LC_INFTYP_0105  " Infotype 0006: Address
      NUMBER        = LV_PERNR        " หมายเลขพนักงาน
      SUBTYPE       = LS_P0105-SUBTY   "Subtype
      VALIDITYBEGIN = LS_P0105-BEGDA  " วันที่เริ่มต้น
      VALIDITYEND   = LS_P0105-ENDDA  " วันที่สิ้นสุด
      RECORD        = LS_P0105        " ข้อมูลที่ต้องการแก้ไข
      OPERATION     = LV_ACTION       " ประเภทการดำเนินการ (MOD, INS, DEL)
    IMPORTING
      RETURN        = LS_RET1.      " ตารางผลลัพธ์
  IF LS_RET1-TYPE = GC_MSGTYP-ERROR OR
     LS_RET1-TYPE = GC_MSGTYP-ABORT.
    MOVE-CORRESPONDING LS_RET1 TO LS_RETURN.
    APPEND LS_RETURN TO CT_RETURN.
    EXIT.
  ENDIF.

  "Name
  CLEAR: LV_ACTION.
  SELECT SINGLE *
    INTO @DATA(LS_PA0002)
    FROM  PA0002
    WHERE PERNR = @LV_PERNR.
  IF SY-SUBRC = 0.
    LV_ACTION = LC_MODIFY.
    MOVE-CORRESPONDING LS_PA0002 TO LS_P0002.
  ELSE.
    LV_ACTION      = LC_INSERT.
    LS_PA0002-BEGDA = SY-DATUM.
    LS_PA0002-ENDDA = LC_NDAT.
  ENDIF.

  LS_P0002-INFTY = LC_INFTYP_0002.
  LS_P0002-VORNA = LV_FNAME.
  LS_P0002-NACHN = LV_LNAME.

  "Update info type 0002
  CALL FUNCTION 'HR_INFOTYPE_OPERATION'
    EXPORTING
      INFTY         = LC_INFTYP_0002  " Infotype 0002: Personel data
      NUMBER        = LV_PERNR        " หมายเลขพนักงาน
      SUBTYPE       = LS_P0002-SUBTY   "Subtype
      VALIDITYBEGIN = LS_P0002-BEGDA  " วันที่เริ่มต้น
      VALIDITYEND   = LS_P0002-ENDDA  " วันที่สิ้นสุด
      RECORD        = LS_P0002        " ข้อมูลที่ต้องการแก้ไข
      OPERATION     = LV_ACTION        " ประเภทการดำเนินการ (MOD, INS, DEL)
    IMPORTING
      RETURN        = LS_RET1.      " ตารางผลลัพธ์
  IF LS_RET1-TYPE = GC_MSGTYP-ERROR OR
     LS_RET1-TYPE = GC_MSGTYP-ABORT.
    MOVE-CORRESPONDING LS_RET1 TO LS_RETURN.
    APPEND LS_RETURN TO CT_RETURN.
    EXIT.
  ENDIF.

  CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
    EXPORTING
      NUMBER = LV_PERNR
    IMPORTING
      RETURN = LS_RET1.
  IF LS_RET1-TYPE = GC_MSGTYP-ERROR OR
     LS_RET1-TYPE = GC_MSGTYP-ABORT.
    MOVE-CORRESPONDING LS_RET1 TO LS_RETURN.
    APPEND LS_RETURN TO CT_RETURN.
    EXIT.
  ENDIF.

  IF LS_RET1-TYPE <> GC_MSGTYP-ERROR.
    PERFORM COMMIT_WORK.
*    PERFORM SYNC_TO_BP USING LV_PERNR.
  ELSE.
    PERFORM ROLL_BACK.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPD_ZSDSFIC001
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_BU_PARTNER
*&      --> LS_ADDRESS_EMAIL
*&---------------------------------------------------------------------*
FORM UPD_ZSDSFIC001  USING UV_PARTNER TYPE BU_PARTNER
                           UV_EMAIL   TYPE AD_SMTPADR.

  SELECT SINGLE *
    FROM ZSDSFIC001
    INTO @DATA(LS_FIC001)
    WHERE KUNNR = @UV_PARTNER.
  IF SY-SUBRC <> 0.
    LS_FIC001-KUNNR   = UV_PARTNER.
    LS_FIC001-RUNNO   = 1.
    LS_FIC001-E_MAIL  = ''.
    LS_FIC001-CREATED_DATE     = SY-DATUM.
    LS_FIC001-CREATED_TIME     = SY-UZEIT.
    LS_FIC001-CREATED_BY       = SY-UNAME.
    LS_FIC001-UPDATED_DATE     = SY-DATUM.
    LS_FIC001-UPDATED_TIME     = SY-UZEIT.
    LS_FIC001-UPDATED_BY       = SY-UNAME.

    MODIFY ZSDSFIC001 FROM LS_FIC001.
    PERFORM COMMIT_WORK.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SYNC_TO_BP
*&---------------------------------------------------------------------*
*& Sync data to BP
*&---------------------------------------------------------------------*
FORM SYNC_TO_BP  USING UV_PERNR TYPE P0030-PERNR.

  DATA LO_MESSAGE_HANDLER TYPE REF TO CL_HRPA_MESSAGE_LIST.
  DATA: lv_is_ok           TYPE boole_d,
        lt_pernr_tab       TYPE /shcm/t_pernr_change.

  CREATE OBJECT LO_MESSAGE_HANDLER.

  CLEAR LV_IS_OK.
  "Lock the employee
  CALL METHOD CL_HRPA_MASTERDATA_ENQ_DEQ=>ENQUEUE_BY_PERNR
    EXPORTING
      TCLAS           = 'A'
      PERNR           = UV_PERNR
      MESSAGE_HANDLER = LO_MESSAGE_HANDLER
    IMPORTING
      IS_OK           = LV_IS_OK.
  IF LV_IS_OK = ABAP_TRUE.

    LT_PERNR_TAB = VALUE #( ( PERNR = UV_PERNR ) ).
    "Synchronous call: Calling the wrapper function module
    CALL FUNCTION '/SHCM/TRIGGER_BUPA_SYNC'
      EXPORTING
        IT_PERNR_TAB          = LT_PERNR_TAB
        IV_SUPPRESS_LOG       = ABAP_FALSE
        IV_OVERRIDE_DATA      = ''
        IV_DEFAULTING_ADDRESS = ''
        IV_DEFAULTING_BANK    = ''
        IV_DEFAULTING_ROLES   = ''
        IV_VENDOR_OVRDATA     = ''.

    "Release the employee
    CALL METHOD CL_HRPA_MASTERDATA_ENQ_DEQ=>DEQUEUE_BY_PERNR
      EXPORTING
        TCLAS = 'A'
        PERNR = UV_PERNR.

    "Personnel Number is processed. Check Logs -
*    WRITE:/ TEXT-001, PERNR-PERNR.
  ELSE.
    "Personnel Number is locked. Skipped -
*    WRITE:/ TEXT-002, PERNR-PERNR.
  ENDIF.


ENDFORM.
