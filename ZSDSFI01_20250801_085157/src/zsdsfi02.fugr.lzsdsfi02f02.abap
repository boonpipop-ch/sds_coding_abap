*----------------------------------------------------------------------*
***INCLUDE LZSDSFI02F02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form get_bp_detail
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM GET_BP_DETAIL  CHANGING CV_PARTN_CAT   TYPE BU_TYPE
                             CV_BU_GROUP    TYPE BU_GROUP
                             CV_K2_REFNO    TYPE BU_BPEXT
                             CV_SFDC_REFNO  TYPE BU_BPEXT
                             CS_CENTRAL     TYPE ZSDSFIS003
                             CT_BP_ROLE     TYPE ZSDSFIS089_TT
                             CT_ADDRESS     TYPE ZSDSFIS010_TT
                             CS_CUSTOMER    TYPE ZSDSFIS078
                             CT_PARTNER     TYPE ZSDSFIS083_TT
                             CT_CONTACT     TYPE ZSDSFIS082_TT
                             CS_VENDOR      TYPE ZSDSFIS084
                             CS_BLOCK       TYPE ZSDSFIS116
                             CT_RETURN      TYPE BAPIRET2_T.

  CHECK GV_PARTNER IS NOT INITIAL.

  DATA: LT_RETURN   TYPE BAPIRET2_TT,
        LS_CENTRAL  TYPE BAPIBUS1006_CENTRAL,
        LS_PERSON   TYPE BAPIBUS1006_CENTRAL_PERSON,
        LS_ORG      TYPE BAPIBUS1006_CENTRAL_ORGAN,
        LS_GROUP    TYPE BAPIBUS1006_CENTRAL_GROUP,
        LS_VALIDITY TYPE BAPIBUS1006_CENTRAL_VALIDITY.

  "Get Business partner detail
  CALL FUNCTION 'BAPI_BUPA_CENTRAL_GETDETAIL'
    EXPORTING
      BUSINESSPARTNER         = GV_PARTNER
      VALID_DATE              = SY-DATLO
      IV_REQ_MASK             = 'X'
    IMPORTING
      CENTRALDATA             = LS_CENTRAL
      CENTRALDATAPERSON       = LS_PERSON
      CENTRALDATAORGANIZATION = LS_ORG
      CENTRALDATAGROUP        = LS_GROUP
      CENTRALDATAVALIDITY     = LS_VALIDITY
    TABLES
      RETURN                  = LT_RETURN.

  IF LINE_EXISTS( LT_RETURN[ TYPE = 'E' ] ) OR
      LINE_EXISTS( LT_RETURN[ TYPE = 'A' ] ).
    GV_FLAG_ERR = ABAP_TRUE.
    CT_RETURN[] = LT_RETURN[].
    EXIT.
  ELSE.
    REFRESH LT_RETURN[].

    CS_CENTRAL-SEARCHTERM1 = LS_CENTRAL-SEARCHTERM1.  "Search term 1
    CS_CENTRAL-SEARCHTERM2 = LS_CENTRAL-SEARCHTERM2.  "Search term 2
    CV_SFDC_REFNO          = LS_CENTRAL-SEARCHTERM2.

    PERFORM GET_MORE_DATA CHANGING CS_CENTRAL
                                   CV_PARTN_CAT
                                   CV_BU_GROUP.
  ENDIF.

  "Get Business partner role
  PERFORM GET_ROLE    CHANGING CT_BP_ROLE.

  "Get Address Data
  PERFORM GET_ADDRESS   USING CV_PARTN_CAT
                              CT_BP_ROLE
                              LS_PERSON
                        CHANGING CT_ADDRESS.

  "Get Contact Data
  PERFORM GET_CONTACT_DATA CHANGING CT_CONTACT.

  "Get Customer Data (include fi_customer and credit)
  IF LINE_EXISTS( CT_BP_ROLE[ ROLE = GC_ROLE-CUSTOMER ] ).
    PERFORM GET_CUSTOMER_INFO CHANGING CS_CUSTOMER
                                       CT_PARTNER
                                       CS_BLOCK
                                       CS_CENTRAL.
  ENDIF.

  "Get Vendor Data
  IF LINE_EXISTS( CT_BP_ROLE[ ROLE = GC_ROLE-VENDOR ] ).
    PERFORM GET_VENDOR_INFO CHANGING CS_VENDOR
                                     CS_CENTRAL.
  ENDIF.

  CT_RETURN[] = LT_RETURN[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_address
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_ADDRESS
*&---------------------------------------------------------------------*
FORM GET_ADDRESS  USING UV_PARTN_CAT   TYPE BU_TYPE
                        UT_BP_ROLE     TYPE ZSDSFIS089_TT
                        US_PERSON      TYPE BAPIBUS1006_CENTRAL_PERSON
                  CHANGING CT_ADDRESS   TYPE ZSDSFIS010_TT.

  DATA: LV_ADDRNUMBER TYPE ADRC-ADDRNUMBER,
        LS_ADDRL_COM  TYPE SZADR_ADDR1_COMPLETE.

  "Get Address number
  PERFORM GET_ADDRESS_NUMBER USING GV_PARTNER
                                   UT_BP_ROLE
                                 CHANGING LV_ADDRNUMBER.
  IF LV_ADDRNUMBER IS NOT INITIAL.

    "Get Person data
    SELECT
      BUT000~PARTNER,
      BUT000~PERSNUMBER,
      ADRP~NATION,
      ADRP~NAME_FIRST,
      ADRP~NAME_LAST
      FROM BUT000
      INNER JOIN ADRP
      ON BUT000~PERSNUMBER = ADRP~PERSNUMBER
      INTO TABLE @DATA(LT_PERSON)
      WHERE BUT000~PARTNER = @GV_PARTNER.
    IF SY-SUBRC = 0.
      SORT LT_PERSON BY PARTNER NATION.
    ENDIF.

    PERFORM ADDR_GET_COMPLETE USING LV_ADDRNUMBER
                              CHANGING LS_ADDRL_COM.

    LOOP AT LS_ADDRL_COM-ADDR1_TAB INTO DATA(LS_ADDRL_TAB).

      IF LS_ADDRL_TAB-NATION = SPACE    "Thai lang
      OR LS_ADDRL_TAB-NATION = 'I'.     "Inter version lang

        APPEND INITIAL LINE TO CT_ADDRESS ASSIGNING FIELD-SYMBOL(<LFS_ADDRSS>).
        <LFS_ADDRSS>-BPARTNER             = GV_PARTNER.
        IF LS_ADDRL_TAB-NATION = 'I'.
          <LFS_ADDRSS>-INTERNATIONAL_VERSION = ABAP_TRUE.
        ENDIF.

        <LFS_ADDRSS>-TITLE_KEY      = LS_ADDRL_TAB-DATA-TITLE.
        <LFS_ADDRSS>-NAME1          = LS_ADDRL_TAB-DATA-NAME1.
        <LFS_ADDRSS>-NAME2          = LS_ADDRL_TAB-DATA-NAME2.
        <LFS_ADDRSS>-NAME3          = LS_ADDRL_TAB-DATA-NAME3.
        <LFS_ADDRSS>-NAME4          = LS_ADDRL_TAB-DATA-NAME4.
        <LFS_ADDRSS>-CITY           = LS_ADDRL_TAB-DATA-CITY1.
        <LFS_ADDRSS>-DISTRICT       = LS_ADDRL_TAB-DATA-CITY2.
        <LFS_ADDRSS>-POSTL_COD1     = LS_ADDRL_TAB-DATA-POST_CODE1.
        <LFS_ADDRSS>-STREET         = LS_ADDRL_TAB-DATA-STREET.
        <LFS_ADDRSS>-STR_SUPPL1     = LS_ADDRL_TAB-DATA-STR_SUPPL1.
        <LFS_ADDRSS>-STR_SUPPL2     = LS_ADDRL_TAB-DATA-STR_SUPPL2.
        <LFS_ADDRSS>-STR_SUPPL3     = LS_ADDRL_TAB-DATA-STR_SUPPL3.
        <LFS_ADDRSS>-LOCATION       = LS_ADDRL_TAB-DATA-LOCATION.
        <LFS_ADDRSS>-REGION         = LS_ADDRL_TAB-DATA-REGION.
        <LFS_ADDRSS>-TRANSPZONE     = LS_ADDRL_TAB-DATA-TRANSPZONE.
        <LFS_ADDRSS>-COUNTRY        = LS_ADDRL_TAB-DATA-COUNTRY.

        READ TABLE LT_PERSON INTO DATA(LS_PERSON)
                             WITH KEY PARTNER = GV_PARTNER
                                      NATION  = LS_ADDRL_TAB-NATION
                                      BINARY SEARCH.
        IF SY-SUBRC = 0 AND LS_PERSON-PERSNUMBER IS NOT INITIAL.
          CLEAR:<LFS_ADDRSS>-NAME1,
                <LFS_ADDRSS>-NAME2,
                <LFS_ADDRSS>-NAME3,
                <LFS_ADDRSS>-NAME4.
          <LFS_ADDRSS>-NAME1 = LS_PERSON-NAME_FIRST.
          <LFS_ADDRSS>-NAME2 = LS_PERSON-NAME_LAST.
        ENDIF.

        "Communication
        PERFORM ASSIGN_COMMUNICATE  USING LS_ADDRL_COM
                                    CHANGING  <LFS_ADDRSS>-TEL_NUMBER
                                              <LFS_ADDRSS>-MOBILE_PHONE
                                              <LFS_ADDRSS>-EMAIL
                                              <LFS_ADDRSS>-TEL_NUMBER.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_role
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_BP_ROLE
*&---------------------------------------------------------------------*
FORM GET_ROLE  CHANGING CT_BP_ROLE TYPE ZSDSFIS089_TT.

  DATA: LT_ROLE   TYPE STANDARD TABLE OF BAPIBUS1006_BPROLES,
        LT_RETURN TYPE BAPIRET2_TT.

  "Get role
  CALL FUNCTION 'BAPI_BUPA_ROLES_GET_2'
    EXPORTING
      BUSINESSPARTNER      = GV_PARTNER
      VALIDDATE            = SY-DATLO
    TABLES
      BUSINESSPARTNERROLES = LT_ROLE
      RETURN               = LT_RETURN.

  LOOP AT LT_ROLE INTO DATA(LS_ROLE).
    APPEND INITIAL LINE TO CT_BP_ROLE ASSIGNING FIELD-SYMBOL(<LFS_BP_ROLE>).
    <LFS_BP_ROLE>-BPARTNER    = GV_PARTNER.
    <LFS_BP_ROLE>-ROLE        = LS_ROLE-PARTNERROLE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_more_data
*&---------------------------------------------------------------------*
FORM GET_MORE_DATA  CHANGING CS_CENTRAL     TYPE ZSDSFIS003
                             CV_PARTN_CAT   TYPE BU_TYPE
                             CV_BU_GROUP    TYPE BU_GROUP.

  SELECT SINGLE *
    FROM BUT000
    INTO @DATA(LS_BUT000)
    WHERE PARTNER = @GV_PARTNER.
  IF SY-SUBRC = 0.
    CV_PARTN_CAT            = LS_BUT000-TYPE.
    CV_BU_GROUP             = LS_BUT000-BU_GROUP.
    CS_CENTRAL-PARTNERTYPE  = LS_BUT000-BPKIND.   "Not use
  ENDIF.

  "Tax Data
  CS_CENTRAL-TAXNUMBER = GC_TAX_TH3.

  CALL FUNCTION 'BAPI_BUPA_TAX_GETDETAIL'
    EXPORTING
      BUSINESSPARTNER = GV_PARTNER
      TAXTYPE         = GC_TAX_TH3
    IMPORTING
      TAXNUMBER       = CS_CENTRAL-TAXNUMBER.

  "Branch Code (customer)
  SELECT SINGLE
    J_1TPBUPL,
    DESCRIPTION
  FROM FITHA_PBUPL_D_T
  INTO ( @CS_CENTRAL-BCODE, @CS_CENTRAL-BDESC )
  WHERE KUNNR = @GV_PARTNER
    AND SPRAS = 'E'.
  IF SY-SUBRC <> 0.
    SELECT SINGLE
      J_1TPBUPL,
      DESCRIPTION
      FROM FITHA_PBUPL_K_T
      INTO ( @CS_CENTRAL-BCODE, @CS_CENTRAL-BDESC )
      WHERE LIFNR = @GV_PARTNER
        AND SPRAS = 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_customer_info
*&---------------------------------------------------------------------*
*& Get Customer Information
*&---------------------------------------------------------------------*
FORM GET_CUSTOMER_INFO  CHANGING CS_CUSTOMER TYPE ZSDSFIS078
                                 CT_PARTNER  TYPE ZSDSFIS083_TT
                                 CS_BLOCK    TYPE ZSDSFIS116
                                 CS_CENTRAL  TYPE ZSDSFIS003.

  DATA: LS_ADDRL_COM  TYPE SZADR_ADDR1_COMPLETE.

  "Partner Function (Sales Emp and Shipto)
  SELECT
    KUNNR,
    PARVW,
    PARZA,
    KUNN2,
    PERNR,
    VKORG,
    VTWEG,
    SPART
    FROM KNVP
    INTO TABLE @DATA(LT_KNVP)
    WHERE KUNNR = @GV_PARTNER
      AND VKORG = @GC_SALES_AREA-VKORG
*      AND vtweg = @gc_sales_area-vtweg
      AND SPART = @GC_SALES_AREA-SPART.
  IF SY-SUBRC = 0.
    SORT LT_KNVP BY KUNNR ASCENDING
                    PARVW ASCENDING.
    DELETE ADJACENT DUPLICATES FROM LT_KNVP
    COMPARING KUNNR PARVW PARZA KUNN2 PERNR.
  ENDIF.

  "Sales Emp (Can be only 1 record)
  READ TABLE LT_KNVP INTO DATA(LS_KNVP)
                     WITH KEY KUNNR = GV_PARTNER
                              PARVW = GC_PARTNER_FN-S_EMP
                              BINARY SEARCH.
  IF SY-SUBRC = 0.
    CS_CUSTOMER-SALES_EMP = LS_KNVP-PERNR.
  ENDIF.

  DATA(LT_SHIPTO) = LT_KNVP[].
  DELETE LT_SHIPTO WHERE PARVW <> GC_PARTNER_FN-SHIPTO      "Keep only shipto
                     OR  KUNN2 = GV_PARTNER.                "Remove when partner = itself
  SORT LT_SHIPTO BY KUNN2 PARZA.
  DELETE ADJACENT DUPLICATES FROM LT_SHIPTO COMPARING KUNN2.

  "Shipto Party Data
  IF LT_SHIPTO IS NOT INITIAL.
    SELECT KNA1~KUNNR,
           KNVV~VKORG,
           KNVV~VTWEG,
           KNVV~SPART,
           KNA1~ADRNR,
           BUT000~TYPE,
           BUT000~NAME_ORG1,
           BUT000~NAME_ORG2,
           BUT000~NAME_ORG3,
           BUT000~NAME_ORG4,
           BUT000~NAME_FIRST,
           BUT000~NAME_LAST,
           BUT000~BU_SORT2,
           KNVV~INCO1,
           KNVV~INCO2_L,
           KNVV~VSBED
      FROM KNA1
      INNER JOIN BUT000
      ON KNA1~KUNNR = BUT000~PARTNER
      INNER JOIN KNVV
      ON KNA1~KUNNR = KNVV~KUNNR
      INTO TABLE @DATA(LT_KNA1_SHIPTO)
      FOR ALL ENTRIES IN @LT_SHIPTO
      WHERE KNA1~KUNNR = @LT_SHIPTO-KUNN2.
    IF SY-SUBRC = 0.
      SORT LT_KNA1_SHIPTO BY KUNNR.
    ENDIF.
  ENDIF.

  IF LT_KNA1_SHIPTO IS NOT INITIAL.
    SELECT *
      FROM ADRC
      INTO TABLE @DATA(LT_ADRC_SHIPTO)
      FOR ALL ENTRIES IN @LT_KNA1_SHIPTO
      WHERE ADDRNUMBER = @LT_KNA1_SHIPTO-ADRNR
        AND NATION     = @SPACE.
    IF SY-SUBRC = 0.
      SORT LT_ADRC_SHIPTO BY ADDRNUMBER.
    ENDIF.
  ENDIF.

  DATA: LS_PARTNER TYPE ZSDSFIS083.

  LOOP AT LT_SHIPTO INTO DATA(LS_SHIPTO).

    CLEAR: LS_ADDRL_COM,
           LS_PARTNER.

    LS_PARTNER-BPARTNER = LS_SHIPTO-KUNN2.


    CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
      EXPORTING
        INPUT  = LS_SHIPTO-PARVW
      IMPORTING
        OUTPUT = LS_PARTNER-PARVW.

    LS_PARTNER-PARZA    = LS_SHIPTO-PARZA.

    "Read Master
    READ TABLE LT_KNA1_SHIPTO INTO DATA(LS_KNA1_SHIPTO)
                              WITH KEY KUNNR = LS_SHIPTO-KUNN2
                              BINARY SEARCH.
    IF SY-SUBRC = 0.
      LS_PARTNER-BU_TYPE      = LS_KNA1_SHIPTO-TYPE.
      LS_PARTNER-SFDC_REF_ID  = LS_KNA1_SHIPTO-BU_SORT2.
      LS_PARTNER-INCO1        = LS_KNA1_SHIPTO-INCO1.
      LS_PARTNER-INCO2_L      = LS_KNA1_SHIPTO-INCO2_L.
      LS_PARTNER-VSBED        = LS_KNA1_SHIPTO-VSBED.

      IF LS_PARTNER-BU_TYPE = '1'.
        LS_PARTNER-NAME1      = LS_KNA1_SHIPTO-NAME_FIRST.
        LS_PARTNER-NAME2      = LS_KNA1_SHIPTO-NAME_LAST.
      ELSE.
        LS_PARTNER-NAME1      = LS_KNA1_SHIPTO-NAME_ORG1.
        LS_PARTNER-NAME2      = LS_KNA1_SHIPTO-NAME_ORG2.
        LS_PARTNER-NAME3      = LS_KNA1_SHIPTO-NAME_ORG3.
        LS_PARTNER-NAME4      = LS_KNA1_SHIPTO-NAME_ORG4.
      ENDIF.

      "Read Address of Shipto
      READ TABLE LT_ADRC_SHIPTO INTO DATA(LS_ADRC_SHIPTO)
                                WITH KEY ADDRNUMBER = LS_KNA1_SHIPTO-ADRNR
                                BINARY SEARCH.
      IF SY-SUBRC = 0.
*        ls_partner-name1      = ls_adrc_shipto-name1.
*        ls_partner-name2      = ls_adrc_shipto-name2.
*        ls_partner-name3      = ls_adrc_shipto-name3.
*        ls_partner-name4      = ls_adrc_shipto-name4.
        LS_PARTNER-STREET     = LS_ADRC_SHIPTO-STREET.
        LS_PARTNER-STREET     = LS_ADRC_SHIPTO-STREET.
        LS_PARTNER-STR_SUPPL1 = LS_ADRC_SHIPTO-STR_SUPPL1.
        LS_PARTNER-STR_SUPPL2 = LS_ADRC_SHIPTO-STR_SUPPL2.
        LS_PARTNER-STR_SUPPL3 = LS_ADRC_SHIPTO-STR_SUPPL3.
        LS_PARTNER-CITY       = LS_ADRC_SHIPTO-CITY1.
        LS_PARTNER-DISTRICT   = LS_ADRC_SHIPTO-CITY2.
        LS_PARTNER-LOCATION   = LS_ADRC_SHIPTO-LOCATION.
        LS_PARTNER-COUNTRY    = LS_ADRC_SHIPTO-COUNTRY.
        LS_PARTNER-POSTL_COD1 = LS_ADRC_SHIPTO-POST_CODE1.
        LS_PARTNER-REGION     = LS_ADRC_SHIPTO-REGION.
        LS_PARTNER-TRANSPZONE = LS_ADRC_SHIPTO-TRANSPZONE.

        "Get Comunication
        CLEAR: LS_ADDRL_COM.
        PERFORM ADDR_GET_COMPLETE USING LS_ADRC_SHIPTO-ADDRNUMBER
                                  CHANGING LS_ADDRL_COM.
        IF SY-SUBRC = 0.
          PERFORM ASSIGN_COMMUNICATE USING LS_ADDRL_COM
                                      CHANGING  LS_PARTNER-TEL_NUMBER
                                                LS_PARTNER-MOBILE_PHONE
                                                LS_PARTNER-EMAIL
                                                LS_PARTNER-FAX_NUMBER.

        ENDIF.
      ENDIF.
    ENDIF.

    APPEND LS_PARTNER TO CT_PARTNER.
  ENDLOOP.


  "Customer Data
  SELECT SINGLE *
    FROM KNA1
    LEFT OUTER JOIN KNVV
    ON KNA1~KUNNR = KNVV~KUNNR
    LEFT OUTER JOIN KNB5
    ON KNA1~KUNNR = KNB5~KUNNR
    AND KNB5~BUKRS = @GC_SDS
    LEFT OUTER JOIN KNB1
    ON KNA1~KUNNR = KNB1~KUNNR
    AND KNB1~BUKRS = @GC_SDS
    LEFT OUTER JOIN KNVI
    ON KNA1~KUNNR = KNVI~KUNNR
    AND KNVI~ALAND = @GC_TH
    AND KNVI~TATYP = 'MWST'
    LEFT OUTER JOIN KNBW
    ON KNBW~KUNNR = KNA1~KUNNR
    AND KNBW~BUKRS = @GC_SDS
    INTO @DATA(LS_CUSTOMER)
    WHERE KNA1~KUNNR = @GV_PARTNER
      AND ( KNVV~VKORG = @GC_SALES_AREA-VKORG AND
            "knvv~vtweg = @gc_sales_area-vtweg AND
            KNVV~SPART = @GC_SALES_AREA-SPART ).
  IF SY-SUBRC = 0.
    MOVE-CORRESPONDING: LS_CUSTOMER-KNA1 TO CS_CUSTOMER,
                        LS_CUSTOMER-KNVV TO CS_CUSTOMER,
                        LS_CUSTOMER-KNB5 TO CS_CUSTOMER,
                        LS_CUSTOMER-KNB1 TO CS_CUSTOMER,
                        LS_CUSTOMER-KNVI TO CS_CUSTOMER.


    "Customer block ALL : KNA1
    CS_BLOCK-SO_ALL   = LS_CUSTOMER-KNA1-AUFSD.
    CS_BLOCK-DO_ALL   = LS_CUSTOMER-KNA1-LIFSD.
    CS_BLOCK-BILL_ALL = LS_CUSTOMER-KNA1-FAKSD.

    "Customer block sales area : KNVV
    CS_BLOCK-SO_SEL   = LS_CUSTOMER-KNVV-AUFSD.
    CS_BLOCK-DO_SEL   = LS_CUSTOMER-KNVV-LIFSD.
    CS_BLOCK-BILL_SEL = LS_CUSTOMER-KNVV-FAKSD.

    "Central Data
    CS_CENTRAL-CURRENCY = LS_CUSTOMER-KNVV-WAERS.   "Currency Key
    CS_CENTRAL-QSREC    = LS_CUSTOMER-KNBW-QSREC.   "Recipint type

  ENDIF.

  SELECT SINGLE *
    FROM ZSDSFIC024
    INTO @DATA(LS_ZSDSFIC024)
    WHERE KUNNR = @GV_PARTNER.
  IF SY-SUBRC = 0.
    CS_CUSTOMER-ZBLLCOL = LS_ZSDSFIC024-ZBLLCOL.
    CS_CUSTOMER-ZBLLCYL = LS_ZSDSFIC024-ZBLLCYL.
    CS_CUSTOMER-ZDAYS   = LS_ZSDSFIC024-ZDAYS.
  ENDIF.

  "Credit data
  SELECT SINGLE *
    FROM UKMBP_CMS_SGM
    INTO @DATA(LS_UKMBP_CMS_SGM)
    WHERE PARTNER = @GV_PARTNER
      AND LIMIT_VALID_DATE >= @SY-DATUM.
  IF SY-SUBRC = 0.
    CS_CUSTOMER-CREDIT_LIMIT = LS_UKMBP_CMS_SGM-CREDIT_LIMIT.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_address_number
*&---------------------------------------------------------------------*
*& get adadress number
*&---------------------------------------------------------------------*
FORM GET_ADDRESS_NUMBER  USING    UV_PARTNER  TYPE BU_PARTNER
                                  UT_BP_ROLE  TYPE ZSDSFIS089_TT
                         CHANGING CV_ADDRNR   TYPE ADRC-ADDRNUMBER.

  "Get Address number
*  CALL FUNCTION 'BAPI_BUPA_ADDRESSES_GET'
*    EXPORTING
*      businesspartner       = uv_partner
*      valid_date            = sy-datlo
*    IMPORTING
*      standardaddressnumber = cv_addrnr.

*  SELECT SINGLE addrcomm
*    FROM but000
*    INTO @cv_addrnr
*    WHERE partner = @uv_partner.

  IF LINE_EXISTS( UT_BP_ROLE[ ROLE = GC_ROLE-CUSTOMER ] ).
    SELECT SINGLE
      ADRNR
      FROM KNA1
      INTO @CV_ADDRNR
      WHERE KUNNR = @UV_PARTNER.
  ELSEIF LINE_EXISTS( UT_BP_ROLE[ ROLE = GC_ROLE-VENDOR ] ).
    SELECT SINGLE
      ADRNR
      FROM LFA1
      INTO @CV_ADDRNR
      WHERE LIFNR = @UV_PARTNER.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form addr_get_complete
*&---------------------------------------------------------------------*
*& addr_get_complete
*&---------------------------------------------------------------------*
FORM ADDR_GET_COMPLETE  USING    UV_ADDRESS    	TYPE ADRC-ADDRNUMBER
                        CHANGING CS_ADDRL_COM   TYPE SZADR_ADDR1_COMPLETE.

  CALL FUNCTION 'ADDR_GET_COMPLETE'
    EXPORTING
      ADDRNUMBER              = UV_ADDRESS
    IMPORTING
      ADDR1_COMPLETE          = CS_ADDRL_COM
    EXCEPTIONS
      PARAMETER_ERROR         = 1
      ADDRESS_NOT_EXIST       = 2
      INTERNAL_ERROR          = 3
      WRONG_ACCESS_TO_ARCHIVE = 4
      ADDRESS_BLOCKED         = 5
      OTHERS                  = 6.
  IF SY-SUBRC = 0.
    "Return value
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form assign_communicate
*&---------------------------------------------------------------------*
*& assign comunicate data from address data
*&---------------------------------------------------------------------*
FORM ASSIGN_COMMUNICATE  USING    US_ADDRESS TYPE SZADR_ADDR1_COMPLETE
                        CHANGING  CV_TEL     TYPE ANY   "telephone
                                  CV_MOBILE  TYPE ANY   "Mobile
                                  CV_EMAIL   TYPE ANY   "Email
                                  CV_FAX     TYPE ANY.  "Fax number

  "Communication
  LOOP AT US_ADDRESS-ADTEL_TAB INTO DATA(LS_ADTEL).
    IF LS_ADTEL-ADTEL-R3_USER = '3'.      "mobile
      CV_MOBILE   = LS_ADTEL-ADTEL-TEL_NUMBER.
    ELSE.
      CV_TEL      = LS_ADTEL-ADTEL-TEL_NUMBER.
    ENDIF.
  ENDLOOP.

  "Fax
  READ TABLE US_ADDRESS-ADFAX_TAB INTO DATA(LS_ADFAX)
                                    INDEX 1.
  IF SY-SUBRC = 0.
    CV_FAX = LS_ADFAX-ADFAX-FAX_NUMBER.
  ENDIF.

  "Email
  READ TABLE US_ADDRESS-ADSMTP_TAB INTO DATA(LS_ADSMTP_TAB)
                                     INDEX 1.
  IF SY-SUBRC = 0.
    CV_EMAIL = LS_ADSMTP_TAB-ADSMTP-SMTP_ADDR.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_vendor_info
*&---------------------------------------------------------------------*
*& Get vendor information
*&---------------------------------------------------------------------*
FORM GET_VENDOR_INFO  CHANGING CS_VENDOR  TYPE ZSDSFIS084
                               CS_CENTRAL TYPE ZSDSFIS003.

  DATA: LS_GENERAL TYPE BAPIVENDOR_04,
        LS_COMPANY TYPE BAPIVENDOR_05,
        LS_RETURN  TYPE BAPIRET1,
        LT_BANK    TYPE STANDARD TABLE OF BAPIVENDOR_06,
        LT_VENDOR  TYPE STANDARD TABLE OF BAPIVENDOR_02.

  CALL FUNCTION 'BAPI_VENDOR_GETDETAIL'
    EXPORTING
      VENDORNO         = GV_PARTNER
    IMPORTING
      GENERALDETAIL    = LS_GENERAL
      COMPANYDETAIL    = LS_COMPANY
      RETURN           = LS_RETURN
    TABLES
      BANKDETAIL       = LT_BANK
      VENDORIBANDETAIL = LT_VENDOR.

  IF LS_RETURN-TYPE <> 'E' AND LS_RETURN-TYPE <> 'A'.
    CS_VENDOR-LIFNR = GV_PARTNER.
    CS_VENDOR-ZTERM = LS_COMPANY-PMNTTRMS.

    SELECT SINGLE
      LFB1~LIFNR,
      LFB1~AKONT,
      LFB1~BUKRS,
      LFB1~ZUAWA,
      LFB1~ZTERM,
      LFM1~WAERS,
      LFBW~QSREC,
      LFB1~FDGRV
    FROM LFB1
    LEFT OUTER JOIN LFBW
    ON LFBW~LIFNR = LFB1~LIFNR
    AND LFBW~BUKRS = @GC_SDS
    LEFT OUTER JOIN LFM1
    ON LFB1~LIFNR = LFM1~LIFNR
    AND LFM1~EKORG = @GC_SDS
    INTO @DATA(LS_VENDOR_MASTER)
    WHERE LFB1~LIFNR = @GV_PARTNER
      AND LFB1~BUKRS = @GC_SDS.
    IF SY-SUBRC = 0.
      CS_VENDOR-AKONT     = LS_VENDOR_MASTER-AKONT.
      CS_VENDOR-ZUAWA     = LS_VENDOR_MASTER-ZUAWA.
      CS_VENDOR-ZTERM     = LS_VENDOR_MASTER-ZTERM.
      CS_VENDOR-FDGRV     = LS_VENDOR_MASTER-FDGRV.
      CS_CENTRAL-CURRENCY = LS_VENDOR_MASTER-WAERS.
      CS_CENTRAL-QSREC    = LS_VENDOR_MASTER-QSREC.
    ENDIF.

    READ TABLE LT_BANK INTO DATA(LS_BANKS)
                       INDEX 1.
    IF SY-SUBRC = 0.

      SELECT SINGLE *
        FROM T012
        INTO @DATA(LS_T012)
        WHERE BUKRS = @GC_SDS
          AND BANKL = @LS_BANKS-BANK_KEY.
      IF SY-SUBRC = 0.
        CS_VENDOR-HBKID = LS_T012-HBKID.
      ENDIF.
      CS_VENDOR-BANKN = LS_BANKS-BANK_ACCT.
      CS_VENDOR-BKREF = LS_BANKS-BANK_REF.
      CS_VENDOR-BANKL = LS_BANKS-BANK_KEY.
      CS_VENDOR-BANKS = LS_BANKS-BANK_CTRY.
    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_contact_data
*&---------------------------------------------------------------------*
*& PERFORM FOR GET CONTACT
*&---------------------------------------------------------------------*
FORM GET_CONTACT_DATA CHANGING CT_CONTACT  TYPE ZSDSFIS082_TT.

  DATA: LS_CONTACT    TYPE ZSDSFIS082,
        LS_ADDRL_COM  TYPE SZADR_ADDR1_COMPLETE,
        LV_AD_CONTACT TYPE ADRC-ADDRNUMBER,
        LV_MOBILE_TMP TYPE AD_TLNMBR,
        LV_FAX_TMP    TYPE AD_FXNMBR.

  "Contact Person
  SELECT *
    FROM BUT050
    INTO TABLE @DATA(LT_BUT050)
    WHERE PARTNER1 = @GV_PARTNER
      AND RELTYP = 'BUR001'.    "Has contact person
  IF SY-SUBRC = 0.
    SORT LT_BUT050 BY PARTNER2.
    SELECT *
      FROM BUT000
      INTO TABLE @DATA(LT_BUT000_CONTACT)
      FOR ALL ENTRIES IN @LT_BUT050[]
      WHERE PARTNER = @LT_BUT050-PARTNER2.
    IF SY-SUBRC = 0.
      SORT LT_BUT000_CONTACT BY PARTNER.
    ENDIF.

    LOOP AT LT_BUT050 INTO DATA(LS_BUT050).
      CLEAR: LS_CONTACT,
             LV_AD_CONTACT,
             LS_ADDRL_COM.

      LS_CONTACT-PARTNER1 = LS_BUT050-PARTNER1.
      LS_CONTACT-PARTNER2 = LS_BUT050-PARTNER2.
      LS_CONTACT-RELTYP   = LS_BUT050-RELTYP.

      READ TABLE LT_BUT000_CONTACT INTO DATA(LS_BUT000_CONTACT)
                                   WITH KEY PARTNER = LS_BUT050-PARTNER2
                                  BINARY SEARCH.
      IF SY-SUBRC = 0.
        LV_AD_CONTACT           = LS_BUT000_CONTACT-ADDRCOMM.
        LS_CONTACT-FIRSTNAME    = LS_BUT000_CONTACT-NAME_FIRST.
        LS_CONTACT-LASTNAME     = LS_BUT000_CONTACT-NAME_LAST.
        LS_CONTACT-SFDC_REF_ID  = LS_BUT000_CONTACT-BU_SORT2.
      ENDIF.

      IF LV_AD_CONTACT IS NOT INITIAL.
        PERFORM ADDR_GET_COMPLETE USING LV_AD_CONTACT
                                  CHANGING LS_ADDRL_COM.
        IF SY-SUBRC = 0.
          PERFORM ASSIGN_COMMUNICATE  USING LS_ADDRL_COM
                                      CHANGING LS_CONTACT-TEL1_NUMBR
                                               LV_MOBILE_TMP
                                               LS_CONTACT-E_MAIL
                                               LV_FAX_TMP.
        ENDIF.
      ELSE.
        "Get comunicate from ADR2,ADR6
        SELECT SINGLE TEL_NUMBER
          FROM ADR2
          INTO @LS_CONTACT-TEL1_NUMBR
          WHERE PERSNUMBER = @LS_BUT000_CONTACT-PERSNUMBER.

        SELECT SINGLE SMTP_ADDR
          FROM ADR6
          INTO @LS_CONTACT-E_MAIL
          WHERE PERSNUMBER = @LS_BUT000_CONTACT-PERSNUMBER.
      ENDIF.

      APPEND LS_CONTACT TO CT_CONTACT.
    ENDLOOP.

  ENDIF.

ENDFORM.
