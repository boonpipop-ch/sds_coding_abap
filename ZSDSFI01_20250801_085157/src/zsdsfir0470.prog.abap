*&---------------------------------------------------------------------*
*& Report ZSDSFIR0470
*  Author             : Wantanee Prateep na thalang
*  Add-on ID          : ZFIARI034
*  Description        : Export interface Account BP master to SF
*  Purpose            :
*  Copied from        :  ZR_SFDC_SV_ACCOUNT
*  Restriction        :
*&---------------------------------------------------------------------*
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
REPORT ZSDSFIR0470.
TYPE-POOLS : truxs,slis,icon.
*&-----------------------------------------------------------------------------------*
*& T A B L E S
*&-----------------------------------------------------------------------------------*
TABLES: BUT000,BUT100,UKMBP_CMS_SGM.

*&---------------------------------------------------------------------*
*  TYPE
*&---------------------------------------------------------------------*
TYPES : BEGIN OF gy_result,
      ERP_CUSTOMER_ID(20)         TYPE C,
      PARTNER_ROLE(20)            TYPE C,
      PARTNER_CATEGORY            TYPE C,
      ACCOUNT_NAME_LOCAL(255)     TYPE C,
      ERP_NAME1(40)               TYPE C,
      ERP_NAME2(40)               TYPE C,
      STREET1_LOCAL(60)           TYPE C,
      STREET2_LOCAL(40)           TYPE C,
      STREET3_LOCAL(40)           TYPE C,
      DISTRICT_LOCAL(100)         TYPE C,
      CITY_LOCAL(40)              TYPE C,
      SDS_SUBDISTRICT(100)        TYPE C,
      ACCOUNT_NAME_INTER(255)     TYPE C,
      NAME1(40)                   TYPE C,
      NAME2(40)                   TYPE C,
      STREET1_INTER(60)           TYPE C,
      STREET2_INTER(40)           TYPE C,
      STREET3_INTER(40)           TYPE C,
      DISTRICT_INTER(100)         TYPE C,
      CITY_INTER(40)              TYPE C,
      SDS_SUBDISTRICT_INTER(100)  TYPE C,
      POSTAL_CODE(20)             TYPE C,
      TRANSPORTATION_ZONE(10)     TYPE C,
      SHIPPING_CONDITIONS(2)      TYPE C,
      COUNTRY(2)                  TYPE C,
      REGION(2)                   TYPE C,
      IDENTITYNO(255)             TYPE C,
      BRANCH(255)                 TYPE C,
      CREDITLIMIT(18)             TYPE C,
      ACCOUNT_GROUP(20)           TYPE C,
      BU_GROUP(60)                 TYPE C,
      STATUS                      TYPE C,
      BLOCK_STATUS(10)            TYPE C,
      VAT(3)                      TYPE C,
      SDS_ARBALANCE(18)           TYPE C,
      SDS_YTDCREDITBALANCE(13)    TYPE C,
      PHONE(10)                   TYPE C,
      MOBILE_PHONE(10)            TYPE C,
      FAX(15)                     TYPE C,
      EMAIL(100)                  TYPE C,
      PAYMENT_TERM(4)             TYPE C,
      PAYMENT_TERMS_VENDOR(4)     TYPE C,
      BANK_KEY(20)                TYPE C,
      BANK_NAME(40)               TYPE C,
      BANK_BRANCH(255)            TYPE C,
      BANK_ACCOUNT(255)           TYPE C,
      BILLING_CYCLE(255)          TYPE C,
      PAYMENT_CYCLE(255)          TYPE C,
      SDS_STATUS(255)             TYPE C,
      ACCOUNT_ASSIGNMENT_GROUP(255)   TYPE C,
      PREVIOUS_ACCOUNT_NO(10)     TYPE C,


END OF gy_result.
TYPES : BEGIN OF GY_BUT000,
      PARTNER      TYPE BUT000-PARTNER,
      TYPE         TYPE BUT000-TYPE,
      XBLCK        TYPE BUT000-XBLCK,
END OF GY_BUT000.
TYPES : BEGIN OF gy_bp_kna1,
      PARTNER      TYPE BUT000-PARTNER,
      TYPE         TYPE BUT000-TYPE,
      XBLCK        TYPE BUT000-XBLCK,
      ADRNR        TYPE KNA1-ADRNR,
      FAKSD        TYPE KNA1-FAKSD,       "CENTRAL BILLING BLOCK
      NODEL        TYPE KNA1-NODEL,       "CENTRAL DEL.BLOCK
      LOEVM        TYPE KNA1-LOEVM,       "CENTRAL DELETION FLAG
      LIFSD        TYPE KNA1-LIFSD,       "CENTRAL DELIVERY BLOCK
      AUFSD        TYPE KNA1-AUFSD,       "CENTRAL ORDER BLOCK
      SPERR        TYPE KNA1-SPERR,       "CENTRAL POSTING BLOCK
      CASSD        TYPE KNA1-CASSD,       "CENTRAL SALE BLOCK

END OF gy_bp_kna1.
TYPES : BEGIN OF gy_bp_lfa1,
     PARTNER      TYPE BUT000-PARTNER,
     TYPE         TYPE BUT000-TYPE,
     XBLCK        TYPE BUT000-XBLCK,
     ADRNR        TYPE LFA1-ADRNR,
     SPERR        TYPE LFA1-SPERR,  "BLOCK ALL COMPANY CODE
     SPERM        TYPE LFA1-SPERM,  "BLOCK ALL PURCHASING  ORGANIZE
     SPERQ        TYPE LFA1-SPERQ,  "BLOCK TOTAL BLOCK
     SPERR_BS     TYPE LFB1-SPERR,  "BLOCK SELECT CO. CODE
     SPERM_BS     TYPE LFM1-SPERM,  "BLOCK SELECT PURCHASING  ORGANIZE


END OF gy_bp_lfa1.
TYPES: BEGIN OF GY_LFA1_BANK,
     PARTNER      TYPE BUT000-PARTNER,
     BANKL        TYPE LFBK-BANKL,   "BANK
     BANKN        TYPE LFBK-BANKN,  "BANK ACCOUNT
     BKREF        TYPE LFBK-BKREF,  "
END OF GY_LFA1_BANK.

TYPES: BEGIN OF GY_ADRC,
       ADDRNUMBER     TYPE ADRC-ADDRNUMBER,      "ADDRESS NUMBER
       NAME1          TYPE ADRC-NAME1,      "NAME
       NAME2          TYPE ADRC-NAME2,      "NAME2
       STREET         TYPE ADRC-STREET,     "STREET
       STR_SUPPL3     TYPE ADRC-STR_SUPPL3, " STREET4
       LOCATION       TYPE ADRC-LOCATION, "STREET 5
       CITY2          TYPE ADRC-CITY2,      "DISTRICT
       CITY1          TYPE ADRC-CITY1,      "CITY
       POST_CODE1     TYPE ADRC-POST_CODE1, "POST CODE
       TEL_NUMBER     TYPE ADRC-TEL_NUMBER, "TELEPHONE
       NATION         TYPE ADRC-NATION,
END OF GY_ADRC.

TYPES : BEGIN OF GY_BNKA,
       BANKL  TYPE BNKA-BANKL,
       BANKA  TYPE BNKA-BANKA,
       BRNCH  TYPE BNKA-BRNCH,
END OF GY_BNKA.


TYPES : BEGIN OF  GY_ZSDSFIC024,
      KUNNR     TYPE ZSDSFIC024-KUNNR,
      ZBLLCYL   TYPE ZSDSFIC024-ZBLLCYL,
      ZBLLCOL   TYPE ZSDSFIC024-ZBLLCOL,
      ZDAYS     TYPE ZSDSFIC024-ZDAYS,

END OF GY_ZSDSFIC024.

TYPES: BEGIN OF GY_BSID,
       KUNNR      TYPE BSID-KUNNR,
       BELNR      TYPE BSID-BELNR,
       GJAHR      TYPE BSID-GJAHR,
       BUZEI      TYPE BSID-BUZEI,
       BLART      TYPE BSID-BLART,
       SHKZG      TYPE BSID-SHKZG,
       DMBTR      TYPE BSID-DMBTR,
END OF GY_BSID.

TYPES: BEGIN OF GY_TB002,
       BU_GROUP  TYPE TB002-BU_GROUP,
       TXT15     TYPE TB002-TXT15,
END OF GY_TB002.

TYPES: BEGIN OF GY_KNB1,
       KUNNR     TYPE KNB1-KUNNR,
       ALTKN     TYPE KNB1-ALTKN,
END OF GY_KNB1.

*&---------------------------------------------------------------------*
*  VARIABLE
*&---------------------------------------------------------------------*
DATA : gt_result TYPE TABLE OF gy_result,
       gs_result TYPE gy_result.

DATA : gt_bp_kna1 TYPE STANDARD TABLE OF gy_bp_kna1,
       gs_bp_kna1 TYPE gy_bp_kna1,
       gt_bp_lfa1 TYPE STANDARD TABLE OF gy_bp_lfa1,
       gs_bp_lfa1 TYPE gy_bp_lfa1,
       gt_adrc_kna1 TYPE STANDARD TABLE OF GY_ADRC,
       gs_adrc_kna1 TYPE GY_ADRC,
       gt_adrc_lfa1 TYPE STANDARD TABLE OF GY_ADRC,
       gs_adrc_lfa1 TYPE GY_ADRC,
       gt_bank TYPE STANDARD TABLE OF GY_LFA1_BANK,
       gs_bank TYPE GY_LFA1_BANK,
       gt_but000 TYPE STANDARD TABLE OF GY_BUT000,
       gs_but000 TYPE GY_BUT000,
       GT_BAKN TYPE STANDARD TABLE OF GY_BNKA,
       GS_BAKN TYPE GY_BNKA,
       GT_ZSDSFIC024 TYPE STANDARD TABLE OF GY_ZSDSFIC024,
       GS_ZSDSFIC024 TYPE  GY_ZSDSFIC024,
       GT_BSID TYPE STANDARD TABLE OF GY_BSID,
       GS_BSID TYPE GY_BSID,
       GT_TB002 TYPE STANDARD TABLE OF GY_TB002,
       GS_TB002 TYPE GY_TB002,
       GT_KNB1  TYPE STANDARD TABLE OF GY_KNB1,
       GS_KNB1 TYPE GY_KNB1.


DATA : gt_file_list TYPE TABLE OF rsfillst,
       gs_file_list TYPE rsfillst.

DATA : gt_fcat   TYPE slis_t_fieldcat_alv,
       gs_layout TYPE slis_layout_alv,
       gt_sort TYPE slis_t_sortinfo_alv,
       gs_sort TYPE slis_sortinfo_alv.
*&---------------------------------------------------------------------*
*&      Conect to FTP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
TYPES: BEGIN OF gy_files,
        filename TYPE char128,
END OF gy_files.

TYPES: BEGIN OF gy_cmdout,
  line(100) TYPE c,
END OF gy_cmdout.

DATA : gt_files TYPE gy_files OCCURS 0 WITH HEADER LINE,
       gs_files TYPE gy_files.

DATA : gs_file_move TYPE gy_files,
       gt_file_move TYPE TABLE OF gy_files.

DATA: path TYPE string,
      ls_txt_data TYPE string,
      lv_rows TYPE i,
      zlen TYPE i,
      file_n TYPE string,
      gs_file_name TYPE c LENGTH 255.

DATA: w_cmd(255) TYPE c,
      w_hdl TYPE i,
      w_key TYPE i VALUE 26101957,
      w_slen TYPE i,
      it_cmdout TYPE STANDARD TABLE OF gy_cmdout,
      wa_cmdout TYPE gy_cmdout.

DATA: gs_ftp_path TYPE e_dexcommfilepath.

DATA : BEGIN OF LS_HEADER,
           FIELD1 TYPE STRING,
           FIELD2 TYPE STRING,
           FIELD3 TYPE STRING,
           FIELD4 TYPE STRING,
           FIELD5 TYPE STRING,
           FIELD6 TYPE STRING,
           FIELD7 TYPE STRING,
           FIELD8 TYPE STRING,
           FIELD9 TYPE STRING,
           FIELD10 TYPE STRING,
           FIELD11 TYPE STRING,
           FIELD12 TYPE STRING,
           FIELD13 TYPE STRING,
           FIELD14 TYPE STRING,
           FIELD15 TYPE STRING,
           FIELD16 TYPE STRING,
           FIELD17 TYPE STRING,
           FIELD18 TYPE STRING,
           FIELD19 TYPE STRING,
           FIELD20 TYPE STRING,
           FIELD21 TYPE STRING,
           FIELD22 TYPE STRING,
           FIELD23 TYPE STRING,
           FIELD24 TYPE STRING,
           FIELD25 TYPE STRING,
           FIELD26 TYPE STRING,
           FIELD27 TYPE STRING,
           FIELD28 TYPE STRING,
           FIELD29 TYPE STRING,
           FIELD30 TYPE STRING,
           FIELD31 TYPE STRING,
           FIELD32 TYPE STRING,
           FIELD33 TYPE STRING,
           FIELD34 TYPE STRING,
           FIELD35 TYPE STRING,
           FIELD36 TYPE STRING,
           FIELD37 TYPE STRING,
           FIELD38 TYPE STRING,
           FIELD39 TYPE STRING,
           FIELD40 TYPE STRING,
           FIELD41 TYPE STRING,
           FIELD42 TYPE STRING,
           FIELD43 TYPE STRING,
           FIELD44 TYPE STRING,
           FIELD45 TYPE STRING,
           FIELD46 TYPE STRING,
           FIELD47 TYPE STRING,
           FIELD48 TYPE STRING,
           FIELD49 TYPE STRING,
           FIELD50 TYPE STRING,
           FIELD51 TYPE STRING,


         END OF LS_HEADER.
DATA LT_HEADER LIKE TABLE OF LS_HEADER.
DATA: CT_RESULT TYPE TABLE OF STRING.
  CONSTANTS : BEGIN OF LC_CON,
                  FIELD1      TYPE STRING VALUE      'ERP_Customer_ID__c',
                  FIELD2      TYPE STRING VALUE      'Partner_Role__c',
                  FIELD3      TYPE STRING VALUE      'Partner_category__c',
                  FIELD4      TYPE STRING VALUE      'Account_Name_Local__c',
                  FIELD5      TYPE STRING VALUE      'ERP_Name1__c',
                  FIELD6      TYPE STRING VALUE      'ERP_Name2__c',
                  FIELD7      TYPE STRING VALUE      'Street1_Local__c',
                  FIELD8      TYPE STRING VALUE      'Street2_Local__c',
                  FIELD9      TYPE STRING VALUE      'Street3_Local__c',
                  FIELD10     TYPE STRING VALUE      'District_Local__c',
                  FIELD11     TYPE STRING VALUE      'City_Local__c',
                  FIELD12     TYPE STRING VALUE      'SDS_SubDistrict__c',
                  FIELD13     TYPE STRING VALUE      'Account_Name_Inter__c',
                  FIELD14     TYPE STRING VALUE      'Name1__c',
                  FIELD15     TYPE STRING VALUE      'Name2__c',
                  FIELD16     TYPE STRING VALUE      'Street1_Inter__c',
                  FIELD17     TYPE STRING VALUE      'Street2_Inter__c',
                  FIELD18     TYPE STRING VALUE      'Street3_Inter__c',
                  FIELD19     TYPE STRING VALUE      'District_Inter__c',
                  FIELD20     TYPE STRING VALUE      'City_Inter__c',
                  FIELD21     TYPE STRING VALUE      'SDS_SubDistrict_Inter__c',
                  FIELD22     TYPE STRING VALUE      'Postal_Code__c',
                  FIELD23     TYPE STRING VALUE      'Transportation_zone__c',
                  FIELD24     TYPE STRING VALUE      'Shipping_Conditions__c',
                  FIELD25     TYPE STRING VALUE      'Country__c',
                  FIELD26     TYPE STRING VALUE      'Region__c',
                  FIELD27     TYPE STRING VALUE      'IdentityNo__c',
                  FIELD28     TYPE STRING VALUE      'Branch__c',
                  FIELD29     TYPE STRING VALUE      'CreditLimit__c',
                  FIELD30     TYPE STRING VALUE      'Account_Group__c',
                  FIELD31     TYPE STRING VALUE      'BU_Group__c',
                  FIELD32     TYPE STRING VALUE      'Status__c',
                  FIELD33     TYPE STRING VALUE      'Block_Status__c',
                  FIELD34     TYPE STRING VALUE      'Vat__c',
                  FIELD35     TYPE STRING VALUE      'SDS_ARBalance__c',
                  FIELD36     TYPE STRING VALUE      'SDS_YTDCreditBalance__c',
                  FIELD37     TYPE STRING VALUE      'Phone',
                  FIELD38     TYPE STRING VALUE      'Mobile_Phone__c',
                  FIELD39     TYPE STRING VALUE      'Fax',
                  FIELD40     TYPE STRING VALUE      'Email__c',
                  FIELD41     TYPE STRING VALUE      'Payment_Term__c',
                  FIELD42     TYPE STRING VALUE      'Payment_Terms_Vendor__c',
                  FIELD43     TYPE STRING VALUE      'Bank_Key__c',
                  FIELD44     TYPE STRING VALUE      'Bank_Name__c',
                  FIELD45     TYPE STRING VALUE      'Bank_Branch__c',
                  FIELD46     TYPE STRING VALUE      'Bank_Account__c',
                  FIELD47     TYPE STRING VALUE      'Billing_Cycle__c',
                  FIELD48     TYPE STRING VALUE      'Payment_Cycle__c',
                  FIELD49     TYPE STRING VALUE      'SDS_Status__c',
                  FIELD50     TYPE STRING VALUE      'Account_Assignment_Group__c',
                  FIELD51     TYPE STRING VALUE      'Previous_Account_No',
              END OF LC_CON.


*&-----------------------------------------------------------------------------------*
*& V A R I A B L E
*&-----------------------------------------------------------------------------------*
DATA : v_pos TYPE i .

DATA : gt_fieldcat TYPE slis_t_fieldcat_alv,
       gt_layout   TYPE slis_layout_alv,
       gt_events   TYPE slis_t_event.
*        gt_heading  TYPE slis_t_listheader.
*&---------------------------------------------------------------------*
*  RANGES
*&---------------------------------------------------------------------*
RANGES : gr_stat FOR jest-stat.
*&---------------------------------------------------------------------*
*  CONSTANTS
*&---------------------------------------------------------------------*
CONSTANTS : gc_true      TYPE c VALUE 'X',
            gc_repid       TYPE repid         VALUE 'ZSDSFIR0470',
            gc_top_of_page TYPE slis_formname VALUE 'TOP_OF_PAGE'.
*&---------------------------------------------------------------------*
*  SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK block3 WITH FRAME TITLE text-003.
SELECT-OPTIONS : S_PARTNE   FOR BUT000-PARTNER,
                 S_BU_GRP   FOR BUT000-BU_GROUP,
                 S_CRDAT    FOR BUT000-CRDAT.
PARAMETERS: p_chk AS CHECKBOX DEFAULT 'X'.
PARAMETERS: p_disp RADIOBUTTON GROUP gr1 ,                   " Download to WEB Server
*            p_local RADIOBUTTON GROUP gr1,
            p_idoc RADIOBUTTON GROUP gr1 DEFAULT 'X',
            p_path LIKE rlgrap-filename  LOWER CASE..
SELECTION-SCREEN END OF BLOCK block3.



*&---------------------------------------------------------------------*
*  INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION.
*&---------------------------------------------------------------------*
*  AT SELECTION-SCREEN OUTPUT
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM f_set_selection.
*&---------------------------------------------------------------------*
*  AT SELECTION-SCREEN OUTPUT
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.

*&---------------------------------------------------------------------*
*  START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM f_get_data.
  PERFORM F_MAP_DATA.
*&---------------------------------------------------------------------*
*  END-OF-SELECTION
*&---------------------------------------------------------------------*
END-OF-SELECTION.



  IF gt_result[] IS NOT INITIAL.
     IF p_disp IS NOT INITIAL.
        PERFORM display_reprot.
     ELSE.
       PERFORM F_GET_RESULT.
     ENDIF.


  ELSE.
*    MESSAGE s000 DISPLAY LIKE 'E'.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_get_data.

 IF p_chk EQ 'X'.

         SELECT PARTNER TYPE XBLCK
           INTO TABLE GT_BUT000
           FROM BUT000
           WHERE PARTNER IN S_PARTNE
             AND BU_GROUP IN  S_BU_GRP
             AND CRDAT IN S_CRDAT
             AND XDELE NE 'X'.


        SELECT A~PARTNER A~TYPE A~XBLCK
               B~ADRNR B~FAKSD B~NODEL B~LOEVM
               B~LIFSD B~AUFSD B~SPERR B~CASSD
          INTO TABLE gt_bp_kna1
          FROM BUT000 AS A INNER JOIN KNA1 AS B
                           ON ( A~PARTNER EQ B~KUNNR )
          WHERE A~PARTNER IN S_PARTNE
            AND A~BU_GROUP IN  S_BU_GRP
            AND A~CRDAT IN S_CRDAT
            AND A~XDELE NE 'X'.

          IF gt_bp_kna1 IS NOT INITIAL.
            SELECT ADDRNUMBER NAME1 NAME2 STREET STR_SUPPL3
                   LOCATION CITY2 CITY1 POST_CODE1 TEL_NUMBER NATION
              INTO TABLE GT_ADRC_KNA1
              FROM ADRC
              FOR ALL ENTRIES IN GT_BP_KNA1
              WHERE ADDRNUMBER EQ GT_BP_KNA1-ADRNR.
          ENDIF.



          SELECT A~PARTNER A~TYPE A~XBLCK
                B~ADRNR B~SPERR B~SPERM B~SPERQ
                D~SPERR E~SPERM
          INTO TABLE gt_bp_lfa1
          FROM BUT000 AS A INNER JOIN LFA1 AS B
                           ON ( A~PARTNER EQ B~LIFNR )
                           INNER JOIN LFB1 AS D
                           ON ( A~PARTNER EQ D~LIFNR )
                           INNER JOIN LFM1 AS E
                           ON ( A~PARTNER EQ E~LIFNR )
          WHERE A~PARTNER IN S_PARTNE
            AND A~BU_GROUP IN  S_BU_GRP
            AND A~CRDAT IN S_CRDAT
            AND A~XDELE NE 'X'.

            IF GT_BP_LFA1 IS NOT INITIAL.
                  SELECT ADDRNUMBER NAME1 NAME2 STREET STR_SUPPL3
                         LOCATION CITY2 CITY1 POST_CODE1 TEL_NUMBER NATION
                    INTO TABLE GT_ADRC_LFA1
                    FROM ADRC
                    FOR ALL ENTRIES IN GT_BP_LFA1
                    WHERE ADDRNUMBER EQ GT_BP_LFA1-ADRNR.
            ENDIF.


          SELECT A~PARTNER
               B~BANKL B~BANKN B~BKREF
          INTO TABLE GT_BANK
          FROM BUT000 AS A INNER JOIN LFBK AS B
                           ON ( A~PARTNER EQ B~LIFNR )
          WHERE A~PARTNER IN S_PARTNE
            AND A~BU_GROUP IN  S_BU_GRP
            AND A~CRDAT IN S_CRDAT
            AND A~XDELE NE 'X'.
   ELSE.  "All

        SELECT PARTNER TYPE XBLCK
           INTO TABLE GT_BUT000
           FROM BUT000
           WHERE PARTNER IN S_PARTNE
             AND BU_GROUP IN  S_BU_GRP
             AND CRDAT IN S_CRDAT.


        SELECT A~PARTNER A~TYPE A~XBLCK
               B~ADRNR B~FAKSD B~NODEL B~LOEVM
               B~LIFSD B~AUFSD B~SPERR B~CASSD
          INTO TABLE gt_bp_kna1
          FROM BUT000 AS A INNER JOIN KNA1 AS B
                           ON ( A~PARTNER EQ B~KUNNR )
          WHERE A~PARTNER IN S_PARTNE
            AND A~BU_GROUP IN  S_BU_GRP
            AND A~CRDAT IN S_CRDAT.

          IF gt_bp_kna1 IS NOT INITIAL.
            SELECT ADDRNUMBER NAME1 NAME2 STREET STR_SUPPL3
                   LOCATION CITY2 CITY1 POST_CODE1 TEL_NUMBER NATION
              INTO TABLE GT_ADRC_KNA1
              FROM ADRC
              FOR ALL ENTRIES IN GT_BP_KNA1
              WHERE ADDRNUMBER EQ GT_BP_KNA1-ADRNR.
          ENDIF.



          SELECT A~PARTNER A~TYPE A~XBLCK
                B~ADRNR B~SPERR B~SPERM B~SPERQ
                D~SPERR E~SPERM
          INTO TABLE gt_bp_lfa1
          FROM BUT000 AS A INNER JOIN LFA1 AS B
                           ON ( A~PARTNER EQ B~LIFNR )
                           INNER JOIN LFB1 AS D
                           ON ( A~PARTNER EQ D~LIFNR )
                           INNER JOIN LFM1 AS E
                           ON ( A~PARTNER EQ E~LIFNR )
          WHERE A~PARTNER IN S_PARTNE
            AND A~BU_GROUP IN  S_BU_GRP
            AND A~CRDAT IN S_CRDAT.

            IF GT_BP_LFA1 IS NOT INITIAL.
                  SELECT ADDRNUMBER NAME1 NAME2 STREET STR_SUPPL3
                         LOCATION CITY2 CITY1 POST_CODE1 TEL_NUMBER NATION
                    INTO TABLE GT_ADRC_LFA1
                    FROM ADRC
                    FOR ALL ENTRIES IN GT_BP_LFA1
                    WHERE ADDRNUMBER EQ GT_BP_LFA1-ADRNR.
            ENDIF.


          SELECT A~PARTNER
               B~BANKL B~BANKN B~BKREF
          INTO TABLE GT_BANK
          FROM BUT000 AS A INNER JOIN LFBK AS B
                           ON ( A~PARTNER EQ B~LIFNR )
          WHERE A~PARTNER IN S_PARTNE
            AND A~BU_GROUP IN  S_BU_GRP
            AND A~CRDAT IN S_CRDAT.
   ENDIF.

    SELECT BANKL BANKA BRNCH
      INTO TABLE GT_BAKN
      FROM BNKA.

    SELECT KUNNR ZBLLCYL ZBLLCOL ZDAYS
      INTO TABLE GT_ZSDSFIC024
      FROM ZSDSFIC024.


      SELECT A~KUNNR A~BELNR A~GJAHR A~BUZEI A~BLART
             A~SHKZG A~DMBTR
      INTO TABLE GT_BSID
      FROM BSID AS A
      FOR ALL ENTRIES IN GT_BUT000
      WHERE A~KUNNR EQ GT_BUT000-PARTNER.

      SELECT BU_GROUP TXT15
        INTO TABLE GT_TB002
        FROM TB002
        WHERE SPRAS EQ 'EN'.

        SELECT KUNNR ALTKN
          INTO TABLE GT_KNB1
          FROM KNB1.




ENDFORM.                    "f_get_data

*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_MAP_DATA.

  DATA : LV_FLAG_CUST TYPE C,
         LV_FLAG_VENDOR TYPE C.
  DATA: GS_ADDRESS TYPE ZSDSFIS010.
  DATA: LV_KTGRD TYPE KNVV-KTGRD.
  DATA: LV_CREDIT_LIMIT TYPE UKMBP_CMS_SGM-CREDIT_LIMIT.
  DATA: LV_VSBED TYPE KNVV-VSBED.
  DATA: LV_TOTALAMT TYPE BSID-DMBTR.
  DATA: LV_YTDCREDIT_BAL TYPE BSID-DMBTR.
  DATA: LV_DATATYPE TYPE DD01V-DATATYPE.
  DATA: GV_PARTN_CAT  TYPE  BU_TYPE,
        GV_BU_GROUP TYPE  BU_GROUP,
        GV_K2_REFNO TYPE  BU_BPEXT,
        GV_SFDC_REFNO TYPE  BU_BPEXT,
        GS_CENTRAL  TYPE  ZSDSFIS003,
        GT_BP_ROLE  TYPE  ZSDSFIS089_TT,
        GT_ADDRESS  TYPE  ZSDSFIS010_TT,
        GS_CUSTOMER TYPE  ZSDSFIS078,
        GT_PARTNER  TYPE  ZSDSFIS083_TT,
        GT_CONTACT  TYPE  ZSDSFIS082_TT,
        GS_VENDOR TYPE  ZSDSFIS084,
        GT_RETURN TYPE  BAPIRET2_T,
        GS_BLOCK  TYPE  ZSDSFIS116.
  DATA:  TEXT1(1) TYPE C,
         VALUE(30) TYPE C.

    LOOP AT GT_BUT000 INTO GS_BUT000 WHERE TYPE NE '3'.
         CLEAR: GS_BP_KNA1,GS_BP_LFA1,GS_ADRC_KNA1,GS_ADRC_LFA1,LV_KTGRD,LV_FLAG_CUST,LV_FLAG_VENDOR,
                LV_CREDIT_LIMIT,LV_TOTALAMT,LV_VSBED,LV_YTDCREDIT_BAL,LV_DATATYPE.

          GS_RESULT-ERP_CUSTOMER_ID    =  GS_BUT000-PARTNER.

          CALL FUNCTION 'Z_SDSFI_GET_BP_DETAIL'
           EXPORTING
             IV_PARTNER          = GS_BUT000-PARTNER
             IV_MODE             = 'V'
           IMPORTING
             EV_PARTN_CAT        = GV_PARTN_CAT
             EV_BU_GROUP         = GV_BU_GROUP
             EV_K2_REFNO         = GV_K2_REFNO
             EV_SFDC_REFNO       = GV_SFDC_REFNO
             ES_CENTRAL          = GS_CENTRAL
             ET_BP_ROLE          = GT_BP_ROLE
             ET_ADDRESS          = GT_ADDRESS
             ES_CUSTOMER         = GS_CUSTOMER
             ET_PARTNER          = GT_PARTNER
             ET_CONTACT          = GT_CONTACT
             ES_VENDOR           = GS_VENDOR
             ET_RETURN           = GT_RETURN
             ES_BLOCK            = GS_BLOCK
                    .
           READ TABLE GT_BP_KNA1 INTO GS_BP_KNA1 WITH KEY PARTNER = GS_BUT000-PARTNER.
               IF SY-SUBRC EQ 0.
                  LV_FLAG_CUST = 'X'.
               ENDIF.
           READ TABLE GT_BP_LFA1 INTO GS_BP_LFA1 WITH KEY PARTNER = GS_BUT000-PARTNER.
               IF SY-SUBRC EQ 0.
                  LV_FLAG_VENDOR = 'X'.
               ENDIF.


           IF LV_FLAG_CUST = 'X' AND LV_FLAG_VENDOR = 'X'.
             GS_RESULT-PARTNER_ROLE    =  'ALL'.
           ELSEIF LV_FLAG_CUST = 'X'.
             GS_RESULT-PARTNER_ROLE    =  'FLCU01' . "Customer
           ELSEIF LV_FLAG_VENDOR = 'X'.
             GS_RESULT-PARTNER_ROLE    =  'Vendor'.
           ENDIF.

           READ TABLE GT_ADDRESS INTO GS_ADDRESS WITH KEY BPARTNER = GS_BUT000-PARTNER
                                                          INTERNATIONAL_VERSION = ' '.
                   .
                IF SY-SUBRC EQ 0.
                     CONCATENATE  GS_ADDRESS-NAME1 GS_ADDRESS-NAME2 INTO GS_RESULT-ACCOUNT_NAME_LOCAL.
                     GS_RESULT-ERP_NAME1 = GS_ADDRESS-NAME1.
                     GS_RESULT-ERP_NAME2 = GS_ADDRESS-NAME2.
                     GS_RESULT-STREET1_LOCAL = GS_ADDRESS-STREET.
                     GS_RESULT-STREET2_LOCAL = GS_ADDRESS-STR_SUPPL3.
                     GS_RESULT-STREET3_LOCAL = GS_ADDRESS-STR_SUPPL1.
                     GS_RESULT-SDS_SUBDISTRICT  = GS_ADDRESS-LOCATION.
                     GS_RESULT-DISTRICT_LOCAL = GS_ADDRESS-DISTRICT.
                     GS_RESULT-CITY_LOCAL    = GS_ADDRESS-CITY.
                     GS_RESULT-POSTAL_CODE    = GS_ADDRESS-POSTL_COD1.
                     GS_RESULT-PHONE    = GS_ADDRESS-TEL_NUMBER.
                     GS_RESULT-MOBILE_PHONE    = GS_ADDRESS-MOBILE_PHONE.
                     GS_RESULT-FAX    =   GS_ADDRESS-FAX_NUMBER  .
                     GS_RESULT-EMAIL  =   GS_ADDRESS-EMAIL  .
                     GS_RESULT-COUNTRY    = GS_ADDRESS-COUNTRY.
                     GS_RESULT-REGION    = GS_ADDRESS-REGION.
                     GS_RESULT-TRANSPORTATION_ZONE  = GS_ADDRESS-TRANSPZONE.
                ENDIF.

            READ TABLE GT_ADDRESS INTO GS_ADDRESS WITH KEY BPARTNER = GS_BUT000-PARTNER
                                                          INTERNATIONAL_VERSION = 'X'.
                   .
                IF SY-SUBRC EQ 0.
                     CONCATENATE  GS_ADDRESS-NAME1 GS_ADDRESS-NAME2 INTO GS_RESULT-ACCOUNT_NAME_INTER.
                     GS_RESULT-NAME1 = GS_ADDRESS-NAME1.
                     GS_RESULT-NAME2 = GS_ADDRESS-NAME2.
                     GS_RESULT-STREET1_INTER = GS_ADDRESS-STREET.
                     GS_RESULT-STREET2_INTER = GS_ADDRESS-STR_SUPPL3.
                     GS_RESULT-STREET3_INTER = GS_ADDRESS-STR_SUPPL1.
                     GS_RESULT-SDS_SUBDISTRICT_INTER  = GS_ADDRESS-LOCATION.
                     GS_RESULT-DISTRICT_INTER = GS_ADDRESS-DISTRICT.
*                     GS_RESULT-CITY_INTER    = GS_ADDRESS-CITY.
                     IF GS_ADDRESS-REGION EQ 'S'.
                       GS_RESULT-CITY_INTER = '20'.
                     ELSE.
                       GS_RESULT-CITY_INTER = GS_ADDRESS-REGION.
                     ENDIF.

                ENDIF.

           GS_RESULT-PARTNER_CATEGORY    = GS_BUT000-TYPE.
           GS_RESULT-IDENTITYNO    = GS_CENTRAL-TAXNUMBER.


           CALL FUNCTION 'NUMERIC_CHECK'
             EXPORTING
               string_in        = GS_CENTRAL-BCODE
            IMPORTING
*              STRING_OUT       =
              HTYPE            =  LV_DATATYPE.



           IF LV_DATATYPE EQ  'NUMC'.
              GS_RESULT-BRANCH    = GS_CENTRAL-BCODE.

           ELSE.
             GS_RESULT-BRANCH = ''.
           ENDIF.
           GS_RESULT-ACCOUNT_GROUP    = GV_BU_GROUP.
           IF GV_BU_GROUP EQ 'Z010' OR GV_BU_GROUP EQ 'Z020'.
              GS_RESULT-BU_GROUP    = GV_BU_GROUP.
           ELSE.
             READ TABLE GT_TB002 INTO GS_TB002 WITH KEY BU_GROUP = GV_BU_GROUP.
               IF SY-SUBRC EQ 0.
                    CONCATENATE GV_BU_GROUP  GS_TB002-TXT15 INTO GS_RESULT-BU_GROUP SEPARATED BY SPACE.
               ENDIF.
           ENDIF.


           GS_RESULT-PAYMENT_TERM    = GS_CUSTOMER-ZTERM.
           GS_RESULT-PAYMENT_TERMS_VENDOR    = GS_VENDOR-ZTERM.


           SELECT SINGLE CREDIT_LIMIT
             INTO LV_CREDIT_LIMIT
             FROM  UKMBP_CMS_SGM
             WHERE PARTNER = GS_BUT000-PARTNER.

             GS_RESULT-CREDITLIMIT    = LV_CREDIT_LIMIT.
             CONDENSE GS_RESULT-CREDITLIMIT NO-GAPS.




           IF GS_CUSTOMER-TAXKD IS NOT INITIAL.
               IF GS_CUSTOMER-TAXKD EQ 1.
                  GS_RESULT-VAT    = '7%'.
               ELSEIF GS_CUSTOMER-TAXKD EQ 0.
                  GS_RESULT-VAT    = '0%'.
               ENDIF.
           ELSE.
                 GS_RESULT-VAT    = '7%'.
           ENDIF.

           READ TABLE GT_BANK INTO GS_BANK WITH KEY PARTNER = GS_BUT000-PARTNER.
               IF SY-SUBRC EQ 0.
                  READ TABLE GT_BAKN INTO GS_BAKN WITH KEY BANKL = GS_BANK-BANKL.
                     IF SY-SUBRC EQ 0.
                           GS_RESULT-BANK_KEY    = GS_BANK-BANKL+0(3).
                           GS_RESULT-BANK_NAME    =  GS_BAKN-BANKA.
                           GS_RESULT-BANK_BRANCH    =  GS_BANK-BANKL+3(4)."GS_BANK-BKREF.
                           GS_RESULT-BANK_ACCOUNT    =    GS_BANK-BANKN .
                     ENDIF.

               ENDIF.


           IF GS_BLOCK-PARTNER IS INITIAL.
                GS_RESULT-STATUS    = 'Y'.
                GS_RESULT-BLOCK_STATUS    = 'Not Block'.

           ELSE.
                GS_RESULT-STATUS    = 'N'.
                GS_RESULT-BLOCK_STATUS    = 'Block'.
           ENDIF.

           GS_RESULT-SDS_STATUS    = 'Active'.

             SELECT SINGLE KTGRD VSBED
             INTO (LV_KTGRD,LV_VSBED)
             FROM KNVV
             WHERE KUNNR = GS_BUT000-PARTNER.
          GS_RESULT-SHIPPING_CONDITIONS    = LV_VSBED.
          GS_RESULT-ACCOUNT_ASSIGNMENT_GROUP    = LV_KTGRD.
*
*
*
*


            READ TABLE GT_ZSDSFIC024 INTO GS_ZSDSFIC024 WITH KEY KUNNR = GS_BUT000-PARTNER.
                IF SY-SUBRC EQ 0.
                    GS_RESULT-BILLING_CYCLE    = GS_ZSDSFIC024-ZBLLCYL.
                    REPLACE ALL OCCURRENCES OF '"' IN GS_RESULT-BILLING_CYCLE WITH ''.
                    REPLACE ALL OCCURRENCES OF ',' IN GS_RESULT-BILLING_CYCLE WITH ';'.

                    GS_RESULT-PAYMENT_CYCLE    = GS_ZSDSFIC024-ZBLLCOL.
                    REPLACE ALL OCCURRENCES OF '"' IN GS_RESULT-PAYMENT_CYCLE WITH ''.
                    REPLACE ALL OCCURRENCES OF ',' IN GS_RESULT-PAYMENT_CYCLE WITH ';'.
                ENDIF.

            LOOP AT GT_BSID INTO GS_BSID WHERE KUNNR EQ GS_BUT000-PARTNER.
                 IF GS_BSID-SHKZG EQ 'S'.
                    LV_TOTALAMT = LV_TOTALAMT + GS_BSID-DMBTR.
                 ELSE.
                    LV_TOTALAMT = LV_TOTALAMT + ( GS_BSID-DMBTR * -1 ).
                 ENDIF.
             ENDLOOP.

                VALUE = LV_TOTALAMT.
                SEARCH VALUE FOR '-'.
                IF SY-SUBRC = 0 AND SY-FDPOS <> 0.
                  SPLIT VALUE AT '-' INTO VALUE TEXT1.
                  CONDENSE VALUE.
                  CONCATENATE '-' VALUE INTO VALUE.
                ELSE.
                  CONDENSE VALUE.
                ENDIF.
*                GS_RESULT-SDS_ARBALANCE = LV_TOTALAMT.
                GS_RESULT-SDS_ARBALANCE = VALUE.
                CONDENSE GS_RESULT-SDS_ARBALANCE NO-GAPS.

             LV_YTDCREDIT_BAL  = LV_CREDIT_LIMIT - LV_TOTALAMT.
              VALUE = LV_YTDCREDIT_BAL.
              SEARCH VALUE FOR '-'.
              IF SY-SUBRC = 0 AND SY-FDPOS <> 0.
                SPLIT VALUE AT '-' INTO VALUE TEXT1.
                CONDENSE VALUE.
                CONCATENATE '-' VALUE INTO VALUE.
              ELSE.
                CONDENSE VALUE.
              ENDIF.
*             GS_RESULT-SDS_YTDCREDITBALANCE    = LV_YTDCREDIT_BAL.
              GS_RESULT-SDS_YTDCREDITBALANCE    = VALUE.
             CONDENSE GS_RESULT-SDS_YTDCREDITBALANCE NO-GAPS.


             READ TABLE GT_KNB1 INTO GS_KNB1 WITH KEY KUNNR = GS_BUT000-PARTNER.
                 IF SY-SUBRC EQ 0.
                    GS_RESULT-PREVIOUS_ACCOUNT_NO = GS_KNB1-ALTKN.
                 ENDIF.

             APPEND GS_RESULT TO GT_RESULT.

    ENDLOOP.





ENDFORM.                    "f_get_data
*&-----------------------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_REPROT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_reprot .
  PERFORM build_layout.
  PERFORM build_catalog.
  PERFORM build_event USING gt_events[].

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = gc_repid
      i_callback_user_command = 'USER_COMMAND'
      i_save             = 'A'
*      is_layout          = gt_layout
      it_events          = gt_events[]
      it_fieldcat        = gt_fieldcat

    TABLES
      t_outtab           = GT_RESULT
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " DISPLAY_REPROT

* Add by K 20110215
*---------------------------------------------------------------------*
*       FORM User_command
*---------------------------------------------------------------------*
FORM user_command USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.

  IF r_ucomm = '&IC1'.  "Double click event

*   goto XD03 display Customer Master

*     READ TABLE gt_itab INTO gs_itab INDEX rs_selfield-tabindex.
*
*              SET: PARAMETER ID 'KUN'  FIELD gs_itab-kunnr,
*                      PARAMETER ID 'BUK'  FIELD p_bukrs  .
*              CALL TRANSACTION 'XD03'.

  ENDIF.

  CLEAR r_ucomm.

ENDFORM.                    "user_comman

* End 20110215

*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_layout .
  gt_layout-window_titlebar = sy-title.
  gt_layout-colwidth_optimize = 'X'.
  gt_layout-zebra = 'X'.

ENDFORM.                    "build_layout

*&---------------------------------------------------------------------*
*&      Form  BUILD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_catalog .

  CLEAR : v_pos.


* DeliveryNumber
  PERFORM append_fieldcat USING 'ERP_CUSTOMER_ID'
                                ''
                                ''
                                'BP CODE'
                                 space  space  space
                                 gt_fieldcat[].

* Item
  PERFORM append_fieldcat USING 'PARTNER_ROLE'
                                ''
                                ''
                                'BP ROLE'
                                 space  space  space
                                 gt_fieldcat[].

* QTY
  PERFORM append_fieldcat USING 'PARTNER_CATEGORY'
                                ''
                                ''
                                'BP Category'
                                 space  space  space
                                 gt_fieldcat[].

* MaterialNo
  PERFORM append_fieldcat USING 'ACCOUNT_NAME_LOCAL'
                                ''
                                ''
                                'Name Thai'
                                 space  space  space
                                 gt_fieldcat[].
* SerialNo
  PERFORM append_fieldcat USING 'ERP_NAME1'
                                ''
                                ''
                                'Name1 Local'
                                 space  space  space
                                 gt_fieldcat[].
* CreateDate
  PERFORM append_fieldcat USING 'ERP_NAME2'
                                ''
                                ''
                                'Name2 Local'
                                 space  space  space
                                 gt_fieldcat[].
* ShiptoCode
  PERFORM append_fieldcat USING 'STREET1_LOCAL'
                                ''
                                ''
                                'Street1 Local'
                                 space  space  space
                                 gt_fieldcat[].
* ShiptoName
  PERFORM append_fieldcat USING 'STREET2_LOCAL'
                                ''
                                ''
                                'Street2 Local'
                                 space  space  space
                                 gt_fieldcat[].
* ShiptoAddress
  PERFORM append_fieldcat USING 'STREET3_LOCAL'
                                ''
                                ''
                                'Street3 Local'
                                 space  space  space
                                 gt_fieldcat[].
* ShiptoAddress1
  PERFORM append_fieldcat USING 'DISTRICT_LOCAL'
                                ''
                                ''
                                'District Local'
                                 space  space  space
                                 gt_fieldcat[].
* ShiptoAddress2
  PERFORM append_fieldcat USING 'CITY_LOCAL'
                                ''
                                ''
                                'City Local'
                                 space  space  space
                                 gt_fieldcat[].
* ShiptoPostCode
  PERFORM append_fieldcat USING 'SDS_SUBDISTRICT'
                                ''
                                ''
                                'Subdistrict Local'
                                 space  space  space
                                 gt_fieldcat[].
* ShiptoTell
  PERFORM append_fieldcat USING 'ACCOUNT_NAME_INTER'
                                ''
                                ''
                                'Name1 Inter'
                                 space  space  space
                                 gt_fieldcat[].

* SoldtoCode
  PERFORM append_fieldcat USING 'NAME1'
                                ''
                                ''
                                'Name1 Inter'
                                 space  space  space
                                 gt_fieldcat[].


* SoldtoName
  PERFORM append_fieldcat USING 'NAME2'
                                ''
                                ''
                                'Name2 Inter'
                                 space  space  space
                                 gt_fieldcat[].

* GoodsIssueDate
  PERFORM append_fieldcat USING 'STREET1_INTER'
                                ''
                                ''
                                'Street1 Inter'
                                 space  space  space
                                 gt_fieldcat[].

* ShiptoName
  PERFORM append_fieldcat USING 'STREET2_INTER'
                                ''
                                ''
                                'Street2 Inter'
                                 space  space  space
                                 gt_fieldcat[].
* ShiptoAddress
  PERFORM append_fieldcat USING 'STREET2_INTER'
                                ''
                                ''
                                'Street3 Inter'
                                 space  space  space
                                 gt_fieldcat[].
* ShiptoAddress1
  PERFORM append_fieldcat USING 'DISTRICT_INTER'
                                ''
                                ''
                                'District Inter'
                                 space  space  space
                                 gt_fieldcat[].
* ShiptoAddress2
  PERFORM append_fieldcat USING 'CITY_INTER'
                                ''
                                ''
                                'City Inter'
                                 space  space  space
                                 gt_fieldcat[].
* ShiptoProvince
  PERFORM append_fieldcat USING 'SDS_SUBDISTRICT_INTER'
                                ''
                                ''
                                'Subdistrict Inter'
                                 space  space  space
                                 gt_fieldcat[].
* ShiptoPostCode
  PERFORM append_fieldcat USING 'POSTAL_CODE'
                                ''
                                ''
                                'Post code'
                                 space  space  space
                                 gt_fieldcat[].




* ActualGoodsMovementD
  PERFORM append_fieldcat USING 'TRANSPORTATION_ZONE'
                                ''
                                ''
                                'Transport zone'
                                 space  space  space
                                 gt_fieldcat[].
* SAPQuotationNo
  PERFORM append_fieldcat USING 'SHIPPING_CONDITIONS'
                                ''
                                ''
                                'Shipping con'
                                 space  space  space
                                 gt_fieldcat[].
* WBS
  PERFORM append_fieldcat USING 'COUNTRY'
                                ''
                                ''
                                'Country'
                                 space  space  space
                                 gt_fieldcat[].
* SFQuotationNo
  PERFORM append_fieldcat USING 'REGION'
                                ''
                                ''
                                'Region'
                                 space  space  space
                                 gt_fieldcat[].
* SAPSONo
  PERFORM append_fieldcat USING 'IDENTITYNO'
                                ''
                                ''
                                'Identiry No'
                                 space  space  space
                                 gt_fieldcat[].
*SAPSOItem
  PERFORM append_fieldcat USING 'BRANCH'
                                ''
                                ''
                                'Branch'
                                 space  space  space
                                 gt_fieldcat[].
*DOKey
  PERFORM append_fieldcat USING 'CREDITLIMIT'
                                ''
                                ''
                                'Credit limit'
                                 space  space  space
                                 gt_fieldcat[].
*DOKey
  PERFORM append_fieldcat USING 'ACCOUNT_GROUP'
                                ''
                                ''
                                'Account Group'
                                 space  space  space
                                 gt_fieldcat[].
*DOKey
  PERFORM append_fieldcat USING 'BU_GROUP'
                                ''
                                ''
                                'BU group'
                                 space  space  space
                                 gt_fieldcat[].
*DOKey
  PERFORM append_fieldcat USING 'STATUS'
                                ''
                                ''
                                'Status'
                                 space  space  space
                                 gt_fieldcat[].
*DOKey
  PERFORM append_fieldcat USING 'BLOCK_STATUS'
                                ''
                                ''
                                'Block Status'
                                 space  space  space
                                 gt_fieldcat[].
*DOKey
  PERFORM append_fieldcat USING 'VAT'
                                ''
                                ''
                                'Vat'
                                 space  space  space
                                 gt_fieldcat[].
*DOKey
  PERFORM append_fieldcat USING 'SDS_ARBALANCE'
                                ''
                                ''
                                'AR Balance'
                                 space  space  space
                                 gt_fieldcat[].
*DOKey
  PERFORM append_fieldcat USING 'SDS_YTDCREDITBALANCE'
                                ''
                                ''
                                'YTD credit balance'
                                 space  space  space
                                 gt_fieldcat[].
*DOKey
  PERFORM append_fieldcat USING 'PHONE'
                                ''
                                ''
                                'Phone'
                                 space  space  space
                                 gt_fieldcat[].
*DOKey
  PERFORM append_fieldcat USING 'MOBILE_PHONE'
                                ''
                                ''
                                'Mobile Phone'
                                 space  space  space
                                 gt_fieldcat[].
*DOKey
  PERFORM append_fieldcat USING 'FAX'
                                ''
                                ''
                                'fax'
                                 space  space  space
                                 gt_fieldcat[].
*DOKey
  PERFORM append_fieldcat USING 'EMAIL'
                                ''
                                ''
                                'E-mail'
                                 space  space  space
                                 gt_fieldcat[].
*DOKey
  PERFORM append_fieldcat USING 'PAYMENT_TERM'
                                ''
                                ''
                                'Payment Term Customer'
                                 space  space  space
                                 gt_fieldcat[].
*DOKey
  PERFORM append_fieldcat USING 'PAYMENT_TERMS_VENDOR'
                                ''
                                ''
                                'Payment Term Vendor'
                                 space  space  space
                                 gt_fieldcat[].

*DOKey
  PERFORM append_fieldcat USING 'BANK_KEY'
                                ''
                                ''
                                'Bank Key'
                                 space  space  space
                                 gt_fieldcat[].

*DOKey
  PERFORM append_fieldcat USING 'BANK_NAME'
                                ''
                                ''
                                'Bank Name'
                                 space  space  space
                                 gt_fieldcat[].

*DOKey
  PERFORM append_fieldcat USING 'BANK_BRANCH'
                                ''
                                ''
                                'Bank Branch'
                                 space  space  space
                                 gt_fieldcat[].
*DOKey
  PERFORM append_fieldcat USING 'BANK_ACCOUNT'
                                ''
                                ''
                                'Bank Account'
                                 space  space  space
                                 gt_fieldcat[].
*DOKey
  PERFORM append_fieldcat USING 'BILLING_CYCLE'
                                ''
                                ''
                                'Billing Cycle'
                                 space  space  space
                                 gt_fieldcat[].
*DOKey
  PERFORM append_fieldcat USING 'PAYMENT_CYCLE'
                                ''
                                ''
                                'Payment Cycle'
                                 space  space  space
                                 gt_fieldcat[].
*DOKey
  PERFORM append_fieldcat USING 'SDS_STATUS'
                                ''
                                ''
                                'Status'
                                 space  space  space
                                 gt_fieldcat[].
*DOKey
  PERFORM append_fieldcat USING 'ACCOUNT_ASSIGNMENT_GROUP'
                                ''
                                ''
                                'Account Assignment group'
                                 space  space  space
                                 gt_fieldcat[].
*Previous Account No.
  PERFORM append_fieldcat USING 'PREVIOUS_ACCOUNT_NO'
                                ''
                                ''
                                'Previous Account No.'
                                 space  space  space
                                 gt_fieldcat[].

ENDFORM.             "BUILD_ATALOG

*&---------------------------------------------------------------------*
*&      Form  APPEND_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1453   text
*      -->P_1454   text
*      -->P_1455   text
*      -->P_TEXT_T01  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*

FORM append_fieldcat USING   p_field   "Field name
                             p_reftable"Reference Table name
                             p_reffield"Reference Field name
                             p_coltxt  "Col Text(for specify)
                             p_dosum   "Sum total
                             p_cfieldname  "  currency
                             p_no_zero     " no zero
                             p_it_fieldcat TYPE slis_t_fieldcat_alv.

  DATA: wa_infieldcat TYPE slis_fieldcat_alv,
        v_coltxt_length TYPE i.

  ADD 1 TO v_pos.

  wa_infieldcat-fieldname     = p_field.
  wa_infieldcat-ref_tabname   = p_reftable.
  wa_infieldcat-ref_fieldname = p_reffield.
  wa_infieldcat-col_pos       = v_pos .
  wa_infieldcat-do_sum        = p_dosum.

  IF NOT p_no_zero IS INITIAL .
    wa_infieldcat-no_zero = p_no_zero.
  ENDIF.
  IF NOT p_cfieldname IS INITIAL .
    wa_infieldcat-cfieldname = p_cfieldname .
  ENDIF.


*If we need to specify text ,don't need to derive from data dictionary
*program will check length and define width of the colum
  IF NOT p_coltxt IS INITIAL.
    v_coltxt_length = STRLEN( p_coltxt ).

    IF v_coltxt_length > 20.
      wa_infieldcat-ddictxt = 'L'."Long text
      wa_infieldcat-seltext_l = p_coltxt.
    ELSEIF v_coltxt_length > 10.
      wa_infieldcat-ddictxt = 'M'."Medium Text
      wa_infieldcat-seltext_m = p_coltxt.
    ELSE.
      wa_infieldcat-ddictxt = 'S'."Short Text
      wa_infieldcat-seltext_s = p_coltxt.
    ENDIF.
    wa_infieldcat-reptext_ddic = p_coltxt  .
  ENDIF.
  APPEND wa_infieldcat TO p_it_fieldcat.
ENDFORM.                    " APPEND_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_EVENTS  text
*----------------------------------------------------------------------*
FORM build_event  USING e03_lt_events TYPE slis_t_event.
  DATA: lw_event TYPE slis_alv_event.
* Read Event
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 1
    IMPORTING
      et_events   = e03_lt_events.
  READ TABLE e03_lt_events WITH KEY name = slis_ev_top_of_page
                           INTO lw_event.
*   READ TABLE e03_lt_events WITH KEY name = slis_ev_top_of_list
*                             INTO lw_event.
  IF sy-subrc = 0.
* register top of page event
    MOVE gc_top_of_page TO lw_event-form.
    APPEND lw_event TO e03_lt_events.
  ENDIF.
ENDFORM.                    " BUILD_EVENT

*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM top_of_page.
  PERFORM write_heading.
ENDFORM.                    "top_of_page
*&---------------------------------------------------------------------*
*&      Form  WRITE_HEADING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_heading .
  DATA: t_header      TYPE   slis_t_listheader,
        wa_header     TYPE   slis_listheader.

  wa_header-typ  = 'S'.
  wa_header-key = 'Report Name : '.
  wa_header-info = 'SFDC DO'.
  APPEND wa_header TO t_header.
  CLEAR wa_header.


  wa_header-typ  = 'S'.
  wa_header-key = 'Report Date : '.
  CONCATENATE  sy-datum+6(2) '.'
               sy-datum+4(2) '.'
               sy-datum(4) INTO wa_header-info.   "todays date
  APPEND wa_header TO t_header.
  CLEAR: wa_header.

  wa_header-typ  = 'S'.
  wa_header-key = 'Report Time : '.
  WRITE: sy-uzeit TO wa_header-info.    "todays date
  APPEND wa_header TO t_header.
  CLEAR: wa_header.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_header.

ENDFORM.                    " WRITE_HEADING
*&---------------------------------------------------------------------*
*&      Form  F_EXIT_ALPHA_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_NETWR  text
*----------------------------------------------------------------------*
FORM f_exit_alpha_output  CHANGING lv_netwr.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = lv_netwr
    IMPORTING
      output = lv_netwr.

ENDFORM.                    " F_EXIT_ALPHA_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  F_INSERT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_insert_data.

ENDFORM.                    " F_INSERT_DATA
*&---------------------------------------------------------------------*
*&      Form  F_GET_REANG_PRODH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LR_PRODH  text
*----------------------------------------------------------------------*
FORM f_get_reang_prodh  TABLES fr_prodh.

  RANGES lr_prodh FOR vbrp-prodh.

  CONSTANTS : BEGIN OF lc_trans,
    fg TYPE c LENGTH 18 VALUE 'TRAN SV   FG      ',
    sp TYPE c LENGTH 18 VALUE 'TRAN SV   SP      ',
  END OF lc_trans.

  CLEAR lr_prodh.
  lr_prodh-sign   = 'I'.
  lr_prodh-option = 'EQ'.
  lr_prodh-low    = lc_trans-fg.
  APPEND lr_prodh.

  CLEAR lr_prodh.
  lr_prodh-sign   = 'I'.
  lr_prodh-option = 'EQ'.
  lr_prodh-low    = lc_trans-sp.
  APPEND lr_prodh.

  fr_prodh[] = lr_prodh[].

ENDFORM.                    " F_GET_REANG_PRODH
*&---------------------------------------------------------------------*
*&      Form  F_SET_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_set_selection .
  LOOP AT SCREEN.
    IF screen-name = 'P_PASS'.
      screen-invisible = '1'.
      MODIFY SCREEN.
    ENDIF.


  ENDLOOP.

ENDFORM.                    " F_SET_SELECTION
*&---------------------------------------------------------------------*
*&      Form  F_APHA_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_SERIAL_ITEM  text
*----------------------------------------------------------------------*
FORM f_apha_input USING lv_input.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_input
    IMPORTING
      output = lv_input.

ENDFORM.                    " F_APHA_INPUT

*&---------------------------------------------------------------------*
*&      Form  F_ALPHA_IN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_A  text
*      <--P_B  text
*----------------------------------------------------------------------*
FORM f_alpha_out  USING    lv_in  "TYPE  clike
                 CHANGING lv_out. "TYPE	clike.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = lv_in
    IMPORTING
      output = lv_out.

ENDFORM.                    " F_ALPHA_IN

*&---------------------------------------------------------------------*
*&      Form  F_GET_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_RESULT.

  LS_HEADER-FIELD1 = LC_CON-FIELD1.
  LS_HEADER-FIELD2 = LC_CON-FIELD2.
  LS_HEADER-FIELD3 = LC_CON-FIELD3.
  LS_HEADER-FIELD4 = LC_CON-FIELD4.
  LS_HEADER-FIELD5 = LC_CON-FIELD5.
  LS_HEADER-FIELD6 = LC_CON-FIELD6.
  LS_HEADER-FIELD7 = LC_CON-FIELD7.
  LS_HEADER-FIELD8 = LC_CON-FIELD8.
  LS_HEADER-FIELD9 = LC_CON-FIELD9.
  LS_HEADER-FIELD10 = LC_CON-FIELD10.
  LS_HEADER-FIELD11 = LC_CON-FIELD11.
  LS_HEADER-FIELD12 = LC_CON-FIELD12.
  LS_HEADER-FIELD13 = LC_CON-FIELD13.
  LS_HEADER-FIELD14 = LC_CON-FIELD14.
  LS_HEADER-FIELD15 = LC_CON-FIELD15.
  LS_HEADER-FIELD16 = LC_CON-FIELD16.
  LS_HEADER-FIELD17 = LC_CON-FIELD17.
  LS_HEADER-FIELD18 = LC_CON-FIELD18.
  LS_HEADER-FIELD19 = LC_CON-FIELD19.
  LS_HEADER-FIELD20 = LC_CON-FIELD20.
  LS_HEADER-FIELD21 = LC_CON-FIELD21.
  LS_HEADER-FIELD22 = LC_CON-FIELD22.
  LS_HEADER-FIELD23 = LC_CON-FIELD23.
  LS_HEADER-FIELD24 = LC_CON-FIELD24.
  LS_HEADER-FIELD25   = LC_CON-FIELD25.
  LS_HEADER-FIELD26   = LC_CON-FIELD26.
  LS_HEADER-FIELD27   = LC_CON-FIELD27.
  LS_HEADER-FIELD28   = LC_CON-FIELD28.
  LS_HEADER-FIELD29   = LC_CON-FIELD29.
  LS_HEADER-FIELD30   = LC_CON-FIELD30.
  LS_HEADER-FIELD31   = LC_CON-FIELD31.
  LS_HEADER-FIELD32   = LC_CON-FIELD32.
  LS_HEADER-FIELD33   = LC_CON-FIELD33.
  LS_HEADER-FIELD34   = LC_CON-FIELD34.
  LS_HEADER-FIELD35   = LC_CON-FIELD35.
  LS_HEADER-FIELD36   = LC_CON-FIELD36.
  LS_HEADER-FIELD37   = LC_CON-FIELD37.
  LS_HEADER-FIELD38   = LC_CON-FIELD38.
  LS_HEADER-FIELD39   = LC_CON-FIELD39.
  LS_HEADER-FIELD40   = LC_CON-FIELD40.
  LS_HEADER-FIELD41   = LC_CON-FIELD41.
  LS_HEADER-FIELD42   = LC_CON-FIELD42.
  LS_HEADER-FIELD43   = LC_CON-FIELD43.
  LS_HEADER-FIELD44   = LC_CON-FIELD44.
  LS_HEADER-FIELD45   = LC_CON-FIELD45.
  LS_HEADER-FIELD46   = LC_CON-FIELD46.
  LS_HEADER-FIELD47   = LC_CON-FIELD47.
  LS_HEADER-FIELD48   = LC_CON-FIELD48.
  LS_HEADER-FIELD49   = LC_CON-FIELD49.
  LS_HEADER-FIELD50   = LC_CON-FIELD50.
  LS_HEADER-FIELD51   = LC_CON-FIELD51.

  APPEND LS_HEADER TO LT_HEADER.

  "Z_DEMO_GEN_FILE
  CT_RESULT = ZCL_SDSCA_GEN_DATA_FILE=>GEN_DATA_FIEL( EXPORTING I_HEADER    = LT_HEADER
                                                                I_ITEM      = GT_RESULT
                                                                I_SEPARATOR = '","'
                                                                I_START_END_VALUE = '"').
  PERFORM F_EXPORT_TO_SERVER.
ENDFORM.                    " F_GET_RESULT
*&---------------------------------------------------------------------*
*&      Form  F_export_to_server
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_EXPORT_TO_SERVER.
  DATA : LCL_FTP TYPE REF TO ZCL_SDSCA_GEN_DATA_FILE.

  DATA : LV_STATUS TYPE C.

  DATA : LV_FILE TYPE EVE_TT_STRING.

  DATA : LT_FILE TYPE EREC_T_STRING,
         LS_FILE LIKE LINE OF LT_FILE.
  DATA: LV_PATH(100) TYPE C.
  DATA: LV_PATH_FILE TYPE string.



*ZCL_SDSCA_UTIL_SDS=>GET_CONFIG_001( EXPORTING I_REPID             =  'ZSDSSDR0080'
*                                              I_SINGLE_VALUE_FLAG = ABAP_TRUE "--> need 1 record use ABAP_TRUE need many record Comment this field
*                                              I_PARAM             =  sy-sysid     "LC_CON-SEPARATOR
**                                              I_PARAM_EXT      =
*                                    CHANGING  C_RETURN            = LV_PATH_FILE ).


*  IF sy-sysid = 'F36'.
*     LV_PATH_FILE = '/interface/Z_DS_Dev/SDS_Dev/20_OUTBOUND/MyDaikin_DaikinPro/ZMMI006'.
*  ELSEIF sy-sysid = 'F46'.
*     LV_PATH_FILE = '/interface/Z_DS/SDS/20_OUTBOUND/MyDaikin_DaikinPro/ZMMI006'.
*  ELSE.
**     LV_PATH_FILE = '/interface/Z_DS_Dev/SDS_Dev/20_OUTBOUND/DSS/ZMMI008'.
*  ENDIF.

LV_PATH_FILE = p_path.

  IF LCL_FTP IS NOT BOUND.
    CREATE OBJECT LCL_FTP.
  ENDIF.



  CONCATENATE 'ACCOUNT_' SY-DATUM  sy-timlo '.csv' INTO LV_PATH.
      LV_STATUS = LCL_FTP->FTP_FILE_PUT( "I_WINDOW_PATH = 'DEALER_CLAIM/DEV/OUT'
*                                       I_AL11_PATH   = '/tmp'
                                        I_AL11_PATH   = LV_PATH_FILE
                                        I_FILE_NAME   = LV_PATH "'TEST_FTP_FILE.txt'
                                        I_DATA_SPIDER = 'X'
                                        IT_DATA       = CT_RESULT ).

*  LV_STATUS = LCL_FTP->FTP_FILE_PUT( I_WINDOW_PATH = 'MY_DAIKIN/DEV/OUT'
*                                     I_AL11_PATH   = '/tmp'
*                                     I_FILE_NAME   = LV_PATH "'TEST_FTP_FILE.txt'
*                                     I_USER        = 'ds'
*                                     I_PASS        = 'ds=20240521'
*                                     I_IP          = '172.31.136.250'
*                                     I_PORT        = '21'
*                                     IT_DATA       = CT_RESULT ).
  IF LV_STATUS EQ 'S'.
    " SUCCESS FTP FILE
  ELSE.
    " CANNOT FTP FILE
  ENDIF.
ENDFORM.                 " F_GET_RESULT
