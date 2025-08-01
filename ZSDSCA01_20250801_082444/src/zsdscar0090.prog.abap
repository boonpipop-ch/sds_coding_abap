*-----------------------------------------------------------------------
*  Program ID         : ZSDSCAR0090
*  Creation Date      : 04.11.2024
*  Author             : Zulkiff B.(Eviden)
*  Add-on ID          : N/A
*  Description        : Upload BP master for HR Type
*  Purpose            :
*  Copied from        : ZSDSCAR0050
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*
*-----------------------------------------------------------------------
REPORT ZSDSCAR0090   ##TEXT_USE.


*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:
  SSCRFIELDS ##NEEDED.


*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*

* *****************************
* Types for File Processing
* *****************************
TYPES: GTY_FIELD_NAME TYPE CHAR40.

TYPES: BEGIN OF GTY_RAW,
         TLINE TYPE  STRING,
       END OF GTY_RAW.
TYPES: GTTY_RAW TYPE STANDARD TABLE OF GTY_RAW.

TYPES: GTY_MSGTX TYPE BAPI_MSG.
TYPES: BEGIN OF GTY_MESSG,
         MSGTY TYPE  SY-MSGTY,
         MSGID TYPE  SY-MSGID,
         MSGNO TYPE  SY-MSGNO,
         MSGTX TYPE  GTY_MSGTX,
       END OF GTY_MESSG.
TYPES: GTTY_MESSG TYPE STANDARD TABLE OF GTY_MESSG
                         WITH DEFAULT KEY.

TYPES: BEGIN OF GTY_SUM,
         TOTAL TYPE  I, "Total Entries
         SUCCS TYPE  I, "Success Entries
         ERROR TYPE  I, "Error Entries
       END OF GTY_SUM.

TYPES: GTY_RESULT21 TYPE ZSDSCAS012.
TYPES: GTTY_RESULT21 TYPE STANDARD TABLE OF GTY_RESULT21.

TYPES: GTY_RESULT22 TYPE ZSDSCAS013.
TYPES: GTTY_RESULT22 TYPE STANDARD TABLE OF GTY_RESULT22.

TYPES: GTY_RESULT23 TYPE ZSDSCAS014.
TYPES: GTTY_RESULT23 TYPE STANDARD TABLE OF GTY_RESULT23.

TYPES: GTY_RESULT24 TYPE ZSDSCAS015.
TYPES: GTTY_RESULT24 TYPE STANDARD TABLE OF GTY_RESULT24.

TYPES: BEGIN OF GTY_DEFAULT,
         AWAHR TYPE KNVV-AWAHR,
       END OF GTY_DEFAULT.

TYPES: BEGIN OF GTY_PHONE,
         TELNO1  TYPE  ADR2-TEL_NUMBER,
         TELEXT1 TYPE  ADR2-TEL_EXTENS,
         TELNO2  TYPE  ADR2-TEL_NUMBER,
         TELEXT2 TYPE  ADR2-TEL_EXTENS,
         MOBILE  TYPE  ADR2-TEL_NUMBER,
       END OF GTY_PHONE.

TYPES: BEGIN OF GTY_FAX,
         FAXNO  TYPE  ADR3-FAX_NUMBER,
         FAXEXT TYPE  ADR3-FAX_EXTENS,
       END OF GTY_FAX.

TYPES: BEGIN OF GTY_SMTP,
         EMAIL1 TYPE  ADR6-SMTP_ADDR,
         EMAIL2 TYPE  ADR6-SMTP_ADDR,
         EMAIL3 TYPE  ADR6-SMTP_ADDR,
         EMAIL4 TYPE  ADR6-SMTP_ADDR,
         EMAIL5 TYPE  ADR6-SMTP_ADDR,
         EMAIL6 TYPE  ADR6-SMTP_ADDR,
         EMAIL7 TYPE  ADR6-SMTP_ADDR,
         EMAIL8 TYPE  ADR6-SMTP_ADDR,
         EMAIL9 TYPE  ADR6-SMTP_ADDR,
         EMAIL0 TYPE  ADR6-SMTP_ADDR,
       END OF GTY_SMTP.

TYPES: BEGIN OF GTY_ADDR,
         ADDRNUMBER TYPE  ADRC-ADDRNUMBER, "Address no.
         GUID       TYPE  BUT020-GUID,  "guid
         NAME_FIRST TYPE  BUT000-NAME_FIRST,
         NAME_LAST  TYPE  BUT000-NAME_LAST,
         TITLE      TYPE  BUT000-TITLE,
         NAME1      TYPE  ADRC-NAME1,
         NAME2      TYPE  ADRC-NAME2,
         NAME3      TYPE  ADRC-NAME3,
         NAME4      TYPE  ADRC-NAME4,
         SORT1      TYPE  ADRC-SORT1, "Search Term 1
         SORT2      TYPE  ADRC-SORT2, "Search Term 2
         NAME_CO    TYPE  ADRC-NAME_CO, "c/o name
         HOUSE_NUM1 TYPE  ADRC-HOUSE_NUM1,
         STR_SUPPL1 TYPE  ADRC-STR_SUPPL1,  "Street4
         STR_SUPPL2 TYPE  ADRC-STR_SUPPL2,  "Street4
         STREET     TYPE  ADRC-STREET,      "HouseNo./Street
         STR_SUPPL3 TYPE  ADRC-STR_SUPPL3,  "Street4
         LOCATION   TYPE  ADRC-LOCATION,    "Street5
         CITY2      TYPE  ADRC-CITY2,       "District
         CITY1      TYPE  ADRC-CITY1,       "City
         POST_CODE1 TYPE  ADRC-POST_CODE1,  "Postal Code
         COUNTRY    TYPE  ADRC-COUNTRY,     "Country
         LANGU      TYPE  ADRC-LANGU,       "Language
         PHONE      TYPE  GTY_PHONE,
         FAX        TYPE  GTY_FAX,
         SMTP       TYPE  GTY_SMTP,
         REMARK     TYPE  ADRCT-REMARK,       "Comment
         ADR_KIND   TYPE  BUT021_FS-ADR_KIND,  "Address Type
         ADEXT      TYPE  BUT020-ADEXT,     "External AddressNo.
         DEFLT_COMM TYPE  ADRC-DEFLT_COMM,
         VALID_FROM TYPE  AD_DATE_FR,
         VALID_TO   TYPE  AD_DATE_TO,
         TRANSPZONE TYPE  ADDR1_DATA-TRANSPZONE,  "CH01+
         REGION     TYPE  ADDR1_DATA-REGION,      "CH01+
         HOME_CITY  TYPE  ADDR1_DATA-HOME_CITY,   "CH01+
       END OF GTY_ADDR.

TYPES: BEGIN OF GTY_ADDR_INT,
         ACTIVE     TYPE  FLAG,
         NATION     TYPE  ADRC-NATION,
         TITLE      TYPE  BUT000-TITLE,
         NAME_FIRST TYPE  BUT000-NAME_FIRST,
         NAME_LAST  TYPE  BUT000-NAME_LAST,
         NAME1      TYPE  ADRC-NAME1,
         NAME2      TYPE  ADRC-NAME2,
         NAME3      TYPE  ADRC-NAME3,
         NAME4      TYPE  ADRC-NAME4,
         SORT1      TYPE  ADRC-SORT1,
         SORT2      TYPE  ADRC-SORT2,
         NAME_CO    TYPE  ADRC-NAME_CO, "c/o name
         STR_SUPPL1 TYPE  ADRC-STR_SUPPL1,  "Street4
         STR_SUPPL2 TYPE  ADRC-STR_SUPPL2,  "Street4
         STREET     TYPE  ADRC-STREET,      "HouseNo./Street
         STR_SUPPL3 TYPE  ADRC-STR_SUPPL3,  "Street4
         LOCATION   TYPE  ADRC-LOCATION,    "Street5
         CITY2      TYPE  ADRC-CITY2,       "District
         CITY1      TYPE  ADRC-CITY1,       "City
         POST_CODE1 TYPE  ADRC-POST_CODE1,  "Postal Code
         COUNTRY    TYPE  ADRC-COUNTRY,     "Country
         LANGU      TYPE  ADRC-LANGU,       "Language
         REMARK     TYPE  ADRCT-REMARK,     "Comment
       END OF GTY_ADDR_INT.

TYPES: BEGIN OF GTY_BANK,
         BANK_EXIST TYPE FLAG,
         BKVID      TYPE  BUT0BK-BKVID,          "Bank ID
         BANKS      TYPE  BUT0BK-BANKS,          "Bank Country
         BANKL      TYPE  BUT0BK-BANKL,          "Bank Key
         BANKN      TYPE  BUT0BK-BANKN,          "Bank Account
         KOINH      TYPE  BUT0BK-KOINH,          "Holder name
*         accname TYPE  but0bk-accname,        "Account name
       END OF GTY_BANK.

TYPES: GTY_TXTFIELD TYPE TEXT1000.

TYPES: BEGIN OF GTY_WITHTAX,
         EXIST  TYPE  FLAG,
         WITHT  TYPE  KNBW-WITHT,     "Withhold Type
         WITHCD TYPE  KNBW-WT_WITHCD, "Withhold Code
         AGENT  TYPE  KNBW-WT_AGENT,  "Agent
         WTSTCD TYPE  KNBW-WT_WTSTCD, "WHT Number
         AGTDF  TYPE  KNBW-WT_AGTDF,  "From date
         AGTDT  TYPE  KNBW-WT_AGTDT,  "To date
         QSREC  TYPE  WT_QSREC,       "WHT Form    "CH01
       END OF GTY_WITHTAX.

TYPES: BEGIN OF GTY_VIEW_COMP,
         ACTIVE TYPE  FLAG,
         EXIST  TYPE  FLAG,
         BUKRS  TYPE  KNB1-BUKRS,    "Company code
         AKONT  TYPE  KNB1-AKONT,    "Reconcile Acc.
         ZUAWA  TYPE  KNB1-ZUAWA,    "Sort Key
         ALTKN  TYPE  KNB1-ALTKN,   "Previous Account
         ZTERM  TYPE  KNB1-ZTERM,    "Payment Term
         FDGRV  TYPE  KNB1-FDGRV,
         XZVER  TYPE  KNB1-XZVER,    "Rec Pay History
         XVERR  TYPE  KNB1-XVERR,    "Clearing With Vendor
         KNRZB  TYPE  KNB1-KNRZB,    "Alt Payer

* BOI - CH01
* Tab: Correspondence
         MAHNA  TYPE KNB5-MAHNA,
         BUSAB  TYPE KNB1-BUSAB,
         KVERM  TYPE KNB1-KVERM,
         TLFXS  TYPE KNB1-TLFXS,
         INTAD  TYPE KNB1-INTAD,
         XAUSZ  TYPE KNB1-XAUSZ,
         SPERR  TYPE KNB1-SPERR,
* EOI - CH01

         WTAX01 TYPE  GTY_WITHTAX,
         WTAX02 TYPE  GTY_WITHTAX,
         WTAX03 TYPE  GTY_WITHTAX,
         WTAX04 TYPE  GTY_WITHTAX,
       END OF GTY_VIEW_COMP.

TYPES: BEGIN OF GTY_KEY21,
         RECNO         TYPE  FPM_ROW,            "CH01+
         RLTYP         TYPE  BUT100-RLTYP,
         PARTNER       TYPE  BUT000-PARTNER,
         BU_GROUP      TYPE  BUT000-BU_GROUP,
         TYPE          TYPE  BUT000-TYPE,
         PARTNER_GUID  TYPE  BUT000-PARTNER_GUID,
         PARTNER_EXIST TYPE  FLAG,
         RLTYP_EXIST   TYPE  FLAG,
         EXTEND_COMP   TYPE  FLAG,
       END OF GTY_KEY21.


TYPES: BEGIN OF GTY_TAX,
         NATPERS TYPE  BUT000-NATPERS, "Natural Pers.
         TAXTYPE TYPE  DFKKBPTAXNUM-TAXTYPE,  "Tax Type
         TAXNUM  TYPE  DFKKBPTAXNUM-TAXNUM,   "Tax No.
       END OF GTY_TAX.

TYPES: BEGIN OF GTY_BRANCH,
         J_1TPBUPL      TYPE FITHA_PBUPL_D-J_1TPBUPL,     "Branch
         DESCRIPTION    TYPE FITHA_PBUPL_D_T-DESCRIPTION, "Branch Desc.
         DEFAULT_BRANCH TYPE FITHA_PBUPL_D-DEFAULT_BRANCH , "Default Branch
       END OF GTY_BRANCH.

TYPES: BEGIN OF GTY_LTEXT,
         TXT_C003 TYPE  GTY_TXTFIELD,          "Business Hours
         TXT_FLOG TYPE  GTY_TXTFIELD,          "Note Log
       END OF GTY_LTEXT.

TYPES: BEGIN OF GTY_VIEW_CUST,
         EXIST     TYPE  FLAG,
         KUNNR     TYPE  KNA1-KUNNR,
         VBUND     TYPE  BP001-VBUND,           "Trading Partner
         LIFNR     TYPE  KNA1-LIFNR,    "Supplier
         KUKLA     TYPE  KNA1-KUKLA,    "Cust Class
         BRAN1     TYPE  KNA1-BRAN1,    "Ind Code1
         KATR1     TYPE  KNA1-KATR1,    "Attribute 1
         KDKG1     TYPE  KNA1-KDKG1,    "Cond Group1
         KDKG2     TYPE  KNA1-KDKG2,    "Cond Group2
         CLASS_VAL TYPE  TEXT50,        "****Class value
       END OF GTY_VIEW_CUST.

TYPES: BEGIN OF GTY_MAIN21,
         ADDR       TYPE  GTY_ADDR,
         ADDR_INT   TYPE  GTY_ADDR_INT,
         GENDR      TYPE  BU_SEXID,       "Sex
         DOBDT      TYPE  BUT000-BIRTHDT, "Date of Birth
         FOUND_DAT  TYPE  BUT000-FOUND_DAT,
         LIQUID_DAT TYPE  BUT000-LIQUID_DAT,
         BPEXT      TYPE  BUT000-BPEXT,   "External BPNo.
         BPKIND     TYPE  BUT000-BPKIND,  "BP Type
         TAX        TYPE  GTY_TAX,
         LTEXT      TYPE  GTY_LTEXT,
         BANK01     TYPE  GTY_BANK,
         BANK02     TYPE  GTY_BANK,
         BANK03     TYPE  GTY_BANK,
         BANK04     TYPE  GTY_BANK,
         BANK05     TYPE  GTY_BANK,
         BANK06     TYPE  GTY_BANK,
         VIEW_CUST  TYPE  GTY_VIEW_CUST,
*        Company Code
         VIEW_COMP  TYPE  GTY_VIEW_COMP,
*        Branch
         BRANCH     TYPE  GTY_BRANCH,
*        Indentification
         LEGAL_ENTY TYPE  TB019-LEGAL_ENTY , "Legal form
         LEGAL_ORG  TYPE  TB032-LEGAL_ORG , "Legal entity of org
       END OF GTY_MAIN21.

TYPES: BEGIN OF GTY_PHONEX,
         TELNO1  TYPE  FLAG,
         TELEXT1 TYPE  FLAG,
         TELNO2  TYPE  FLAG,
         TELEXT2 TYPE  FLAG,
         MOBILE  TYPE  FLAG,
       END OF GTY_PHONEX.

TYPES: BEGIN OF GTY_FAXX,
         FAXNO  TYPE  FLAG,
         FAXEXT TYPE  FLAG,
       END OF GTY_FAXX.

TYPES: BEGIN OF GTY_SMTPX,
         EMAIL1 TYPE  FLAG,
         EMAIL2 TYPE  FLAG,
         EMAIL3 TYPE  FLAG,
         EMAIL4 TYPE  FLAG,
         EMAIL5 TYPE  FLAG,
         EMAIL6 TYPE  FLAG,   "CH01+
         EMAIL7 TYPE  FLAG,   "CH01+
       END OF GTY_SMTPX.

TYPES: BEGIN OF GTY_ADDRX,
         TITLE      TYPE  FLAG,
         NAME_FIRST TYPE  FLAG,
         NAME_LAST  TYPE  FLAG,
         NAME1      TYPE  FLAG,
         NAME2      TYPE  FLAG,
         NAME3      TYPE  FLAG,
         NAME4      TYPE  FLAG,
         SORT1      TYPE  FLAG,
         SORT2      TYPE  FLAG,
         NAME_CO    TYPE  FLAG,
         HOUSE_NUM1 TYPE  FLAG,
         STR_SUPPL1 TYPE  FLAG,      "Street4
         STR_SUPPL2 TYPE  FLAG,      "Street4
         STREET     TYPE  FLAG,      "HouseNo./Street
         STR_SUPPL3 TYPE  FLAG,      "Street4
         LOCATION   TYPE  FLAG,      "Street5
         CITY2      TYPE  FLAG,      "District
         CITY1      TYPE  FLAG,      "City
         POST_CODE1 TYPE  FLAG,      "Postal Code
         COUNTRY    TYPE  FLAG,      "Country
         LANGU      TYPE  FLAG,      "Language
         PHONE      TYPE  GTY_PHONEX,
         FAX        TYPE  GTY_FAXX,
         SMTP       TYPE  GTY_SMTPX,
         REMARK     TYPE  FLAG,     "Comment
         ADEXT      TYPE  FLAG,     "External AddressNo.
         DEFLT_COMM TYPE  FLAG,
         VALID_FROM TYPE  FLAG,
         VALID_TO   TYPE  FLAG,
         TRANSPZONE TYPE  FLAG,     "CH01+
         REGION     TYPE  FLAG,     "CH01+
         HOME_CITY  TYPE  FLAG,     "CH01+
       END OF GTY_ADDRX.

TYPES: BEGIN OF GTY_ADDR_INTX,
         TITLE      TYPE  FLAG,
         NAME_FIRST TYPE  FLAG,
         NAME_LAST  TYPE  FLAG,
         NAME1      TYPE  FLAG,
         NAME2      TYPE  FLAG,
         NAME3      TYPE  FLAG,
         NAME4      TYPE  FLAG,
         SORT1      TYPE  FLAG,
         SORT2      TYPE  FLAG,
         NAME_CO    TYPE  FLAG,
         STR_SUPPL1 TYPE  FLAG,
         STR_SUPPL2 TYPE  FLAG,
         STREET     TYPE  FLAG,      "HouseNo./Street
         STR_SUPPL3 TYPE  FLAG,      "Street4
         LOCATION   TYPE  FLAG,      "Street5
         CITY2      TYPE  FLAG,      "District
         CITY1      TYPE  FLAG,      "City
         POST_CODE1 TYPE  FLAG,      "Postal Code
         COUNTRY    TYPE  FLAG,      "Country
         LANGU      TYPE  FLAG,      "Language
         REMARK     TYPE  FLAG,      "Note
       END OF GTY_ADDR_INTX.

TYPES: BEGIN OF GTY_BANKX,
         BKVID TYPE  FLAG,       "Bank ID
         BANKS TYPE  FLAG,       "Bank Country
         BANKL TYPE  FLAG,       "Bank Key
         BANKN TYPE  FLAG,       "Bank Account
         KOINH TYPE  FLAG,       "Holder name
*         accname TYPE  flag,       "Account name
       END OF GTY_BANKX.

TYPES: BEGIN OF GTY_WITHTAXX,
         WITHT  TYPE  FLAG,     "Withhold Type
         WITHCD TYPE  FLAG, "Withhold Code
         AGENT  TYPE  FLAG,  "Agent
         WTSTCD TYPE  FLAG,  "WHT Number
         AGTDF  TYPE  FLAG,  "From date
         AGTDT  TYPE  FLAG,  "To date
         QSREC  TYPE  FLAG,  "WHT form    "CH01+
       END OF GTY_WITHTAXX.

TYPES: BEGIN OF GTY_VIEW_COMPX,
         AKONT  TYPE  FLAG,    "Reconcile Acc.
         KNRZE  TYPE  FLAG,    "Head Office
         ZUAWA  TYPE  FLAG,    "Sort Key
         ZTERM  TYPE  FLAG,    "Payment Term
         ALTKN  TYPE  FLAG,     "Previous Account no.
         XZVER  TYPE  FLAG,    "Rec Pay History
         XVERR  TYPE  FLAG,    "Clearing With Vendor
         KNRZB  TYPE  FLAG,    "Alt Payer

* BOI - CH01
* Tab: Correspondence
         FDGRV  TYPE FLAG,
         MAHNA  TYPE FLAG,
         BUSAB  TYPE FLAG,
         KVERM  TYPE FLAG,
         TLFXS  TYPE FLAG,
         INTAD  TYPE FLAG,
         XAUSZ  TYPE FLAG,
         SPERR  TYPE FLAG,
* EOI - CH01

         WTAX01 TYPE  GTY_WITHTAXX,
         WTAX02 TYPE  GTY_WITHTAXX,
         WTAX03 TYPE  GTY_WITHTAXX,
         WTAX04 TYPE  GTY_WITHTAXX,
       END OF GTY_VIEW_COMPX.

TYPES: BEGIN OF GTY_TAXX,
         NATPERS TYPE  FLAG, "Natural Pers.
         TAXTYPE TYPE  FLAG,  "Tax Type
         TAXNUM  TYPE  FLAG,   "Tax No.
       END OF GTY_TAXX.

TYPES: BEGIN OF GTY_LTEXTX,
         TXT_C003 TYPE  FLAG,          "Business Hours
         TXT_FLOG TYPE  FLAG,          "Note Log
       END OF GTY_LTEXTX.

TYPES: BEGIN OF GTY_VIEW_CUSTX,
         VBUND     TYPE  FLAG,    "Trading Partner
         LIFNR     TYPE  FLAG,    "Supplier
         KUKLA     TYPE  FLAG,    "Cust Class
         BRAN1     TYPE  FLAG,    "Ind Code1
         KATR1     TYPE  FLAG,    "Attribute 1
         KDKG1     TYPE  FLAG,    "Cond Group1
         KDKG2     TYPE  FLAG,    "Cond Group2
         CLASS_VAL TYPE  FLAG,        "****Class value
       END OF GTY_VIEW_CUSTX.

TYPES: BEGIN OF GTY_BRANCHX,
         J_1TPBUPL      TYPE  FLAG, "Branch
         DESCRIPTION    TYPE  FLAG, "Branch Desc.
         DEFAULT_BRANCH TYPE FLAG,  "Default Branch
       END OF GTY_BRANCHX.

TYPES: BEGIN OF GTY_MAIN21X,
         ADDR       TYPE  GTY_ADDRX,
         ADDR_INT   TYPE  GTY_ADDR_INTX,
         GENDR      TYPE  FLAG,   "Sex
         DOBDT      TYPE  FLAG,   "Date of Birth
         FOUND_DAT  TYPE  FLAG,
         LIQUID_DAT TYPE  FLAG,
         BPEXT      TYPE  FLAG,   "External BPNo.
         BPKIND     TYPE  FLAG,   "BP Type
         TAX        TYPE  GTY_TAXX,
         LTEXT      TYPE  GTY_LTEXTX,
         BANK01     TYPE  GTY_BANKX,
         BANK02     TYPE  GTY_BANKX,
         BANK03     TYPE  GTY_BANKX,
         BANK04     TYPE  GTY_BANKX,
         BANK05     TYPE  GTY_BANKX,
         BANK06     TYPE  GTY_BANKX,
         VIEW_CUST  TYPE  GTY_VIEW_CUSTX,
*        Company Code
         VIEW_COMP  TYPE  GTY_VIEW_COMPX,
*        Branch
         BRANCH     TYPE  GTY_BRANCHX,
*        Identification
         LEGAL_ENTY TYPE  FLAG,
         LEGAL_ORG  TYPE  FLAG,
       END OF GTY_MAIN21X.


TYPES: BEGIN OF GTY_MAIN22,
         RLTYP          TYPE  BUT100-RLTYP,
         PARTNER        TYPE  BUT000-PARTNER,
         PARTNER_GUID   TYPE  BUT000-PARTNER_GUID,
         BRANCH_EXIST   TYPE  FLAG,
         KUNNR          TYPE  KNA1-KUNNR,
         J_1TPBUPL      TYPE  FITHA_PBUPL_D-J_1TPBUPL,     "Branch
         DESCRIPTION    TYPE  FITHA_PBUPL_D_T-DESCRIPTION, "Branch Desc.
         DEFAULT_BRANCH TYPE  FITHA_PBUPL_D-DEFAULT_BRANCH, "Default branch
         ADDR           TYPE  GTY_ADDR,
       END OF GTY_MAIN22.


TYPES: BEGIN OF GTY_MAIN22X,
         DESCRIPTION    TYPE  FLAG, "Branch Desc.
         DEFAULT_BRANCH TYPE FLAG, "Default branch
       END OF GTY_MAIN22X.


TYPES: BEGIN OF GTY_MAIN23,
         RLTYP        TYPE  BUT100-RLTYP,
         PARTNER      TYPE  BUT000-PARTNER,
         PARTNER_GUID TYPE  BUT000-PARTNER_GUID,
         KUNNR        TYPE  KNA1-KUNNR,
         ADDR         TYPE  GTY_ADDR,
         ADDR_INT     TYPE  GTY_ADDR_INT,
*        Indentification
         LEGAL_ENTY   TYPE  TB019-LEGAL_ENTY , "Legal form
         LEGAL_ORG    TYPE  TB032-LEGAL_ORG , "Legal entity of org
         BPEXT        TYPE  BUT000-BPEXT,     "External BPNo.
         TAX          TYPE  GTY_TAX,
         BPKIND       TYPE  BUT000-BPKIND,  "BP Type
         BANK01       TYPE  GTY_BANK,
*        Customer View
         VIEW_CUST    TYPE  GTY_VIEW_CUST,
*        Company Code
         VIEW_COMP    TYPE  GTY_VIEW_COMP,
       END OF GTY_MAIN23.


TYPES: BEGIN OF GTY_MAIN23X,
         ADDR       TYPE  GTY_ADDRX,
         ADDR_INT   TYPE  GTY_ADDR_INTX,
*        Indentification
         LEGAL_ENTY TYPE  FLAG, "Legal form
         LEGAL_ORG  TYPE  FLAG , "Legal entity of org
         BPEXT      TYPE  FLAG,   "External BPNo.
         TAX        TYPE  GTY_TAXX,
         BPKIND     TYPE  FLAG,   "BP Type
         BANK01     TYPE  GTY_BANKX,
*        Customer View
         VIEW_CUST  TYPE  GTY_VIEW_CUSTX,
*        Company Code view
         VIEW_COMP  TYPE  GTY_VIEW_COMPX,
       END OF GTY_MAIN23X.

TYPES: BEGIN OF GTY_MAIN24,
         ALTKN           TYPE KNB1-ALTKN,     "Previous account no.
         PARTNER         TYPE BUT000-PARTNER, "Business Partner
         PARTNER_GUID    TYPE BUT000-PARTNER_GUID, "GUID
         RLTYP           TYPE BUT100-RLTYP,   "BP Role
         ADDR_ADR_KIND   TYPE BUT021_FS-ADR_KIND,  "Address Type
         ADDR_ADDRNUMBER TYPE BUT020-ADDRNUMBER, "Address No
         ADDR_EMAIL      TYPE ADR6-SMTP_ADDR,    "Email
         ADDR_GUID       TYPE BUT020-GUID ,  "Address GUID
         ADDR_ADEXT      TYPE BUT020-ADEXT,   "External Address No.
         ADDR_REMARK     TYPE ADRCT-REMARK,   "Address Note
         ADDR_VALID_FROM TYPE AD_DATE_FR, "Valid From
         ADDR_VALID_TO   TYPE AD_DATE_TO,     "Valid To

       END OF GTY_MAIN24.

TYPES: BEGIN OF GTY_MAIN24X,
         ADDR_ADEXT      TYPE FLAG,  "External Address No.
         ADDR_REMARK     TYPE FLAG,  "Address Note
         ADDR_VALID_FROM TYPE FLAG, "Valid From
         ADDR_VALID_TO   TYPE FLAG,  "Valid To
         ADDR_EMAIL      TYPE FLAG,  "Email
       END OF GTY_MAIN24X.

TYPES: BEGIN OF GTY_DATA21,
         ROWNO TYPE  GTY_RESULT21-ROWNO,
         KEY   TYPE  GTY_KEY21,
         MAIN  TYPE  GTY_MAIN21,
         MAINX TYPE  GTY_MAIN21X,
         MESSG TYPE  GTTY_MESSG,
       END OF GTY_DATA21.

TYPES: BEGIN OF GTY_DATA22,
         ROWNO TYPE  GTY_RESULT22-ROWNO,
         MAIN  TYPE  GTY_MAIN22,
         MAINX TYPE  GTY_MAIN22X,
         MESSG TYPE  GTTY_MESSG,
       END OF GTY_DATA22.

TYPES: BEGIN OF GTY_DATA23,
         ROWNO TYPE  GTY_RESULT23-ROWNO,
         MAIN  TYPE  GTY_MAIN23,
         MAINX TYPE  GTY_MAIN23X,
         MESSG TYPE  GTTY_MESSG,
       END OF GTY_DATA23.

TYPES: BEGIN OF GTY_DATA24,
         ROWNO           TYPE  GTY_RESULT24-ROWNO,
         ALTKN           TYPE KNB1-ALTKN,      "Previous account no.
         PARTNER         TYPE BUT000-PARTNER,  "Business Partner
         RLTYP           TYPE BUT100-RLTYP,    "Business Partner Role
         ADDR_ADR_KIND   TYPE BUT021_FS-ADR_KIND, "Address Type
         ADDR_ADEXT      TYPE BUT020-ADEXT,  "External Address No.
         ADDR_REMARK     TYPE ADRCT-REMARK,     "Address Note
         ADDR_VALID_FROM TYPE ADR6-VALID_FROM,  "Valid From
         ADDR_VALID_TO   TYPE ADR6-VALID_TO,    "Valid To
         ADDR_EMAIL      TYPE ADR6-SMTP_ADDR,  "Email
         MAINX           TYPE GTY_MAIN24X,
         MESSG           TYPE GTTY_MESSG,
       END OF GTY_DATA24.

TYPES: BEGIN OF GTYP_LOG_PARTNER ,
         PARTNER   TYPE BUT000-PARTNER,
         GUID      TYPE BAPIBUS1006_HEAD-PARTNGUID,
         GUID_ADDR TYPE  BUT020-GUID,
       END   OF GTYP_LOG_PARTNER ,

       BEGIN OF GTYP_LOG_BANK ,
         PARTNER TYPE BUT000-PARTNER,
         BKVID   TYPE BUT0BK-BKVID,
       END  OF GTYP_LOG_BANK ,

       BEGIN OF GTYP_LOG_COMP ,
         PARTNER TYPE BUT000-PARTNER,
         BUKRS   TYPE LFB1-BUKRS,
         WITHT1  TYPE LFBW-WITHT,
         WITHT2  TYPE LFBW-WITHT,
       END   OF GTYP_LOG_COMP ,

       BEGIN OF GTYP_LOG_PAYEE,
         PARTNER TYPE BUT000-PARTNER,
         BUKRS   TYPE LFZA-BUKRS,
         EMPFK   TYPE LFZA-EMPFK,
       END   OF GTYP_LOG_PAYEE ,

       BEGIN OF GTYP_LOG_EXTEND ,
         PARTNER TYPE BUT000-PARTNER,
         BUKRS   TYPE LFB1-BUKRS,
       END   OF GTYP_LOG_EXTEND .

TYPES: GTTY_DATA21 TYPE STANDARD TABLE OF GTY_DATA21,
       GTTY_DATA22 TYPE  STANDARD TABLE OF GTY_DATA22,
       GTTY_DATA23 TYPE  STANDARD TABLE OF GTY_DATA23,
       GTTY_DATA24 TYPE  STANDARD TABLE OF GTY_DATA24.



*-------------Vendor -------------------------
TYPES: GTY_RESULT11 TYPE ZSDSCAS010.
TYPES: GTTY_RESULT11 TYPE STANDARD TABLE OF GTY_RESULT11.

TYPES: GTY_RESULT12 TYPE ZSDSCAS011.
TYPES: GTTY_RESULT12 TYPE STANDARD TABLE OF GTY_RESULT12.

TYPES: GTY_RESULT13 TYPE ZSDSCAS018.
TYPES: GTTY_RESULT13 TYPE STANDARD TABLE OF GTY_RESULT13.

TYPES: BEGIN OF GTY_V_ROLE,
         RLTYP TYPE  BUT100-RLTYP,
         EXIST TYPE  FLAG,
       END OF GTY_V_ROLE.
TYPES: GTTY_V_ROLE TYPE STANDARD TABLE OF GTY_V_ROLE
                       WITH DEFAULT KEY.

TYPES: BEGIN OF GTY_KEY11,
         PARTNER       TYPE  BUT000-PARTNER,
         BU_GROUP      TYPE  BUT000-BU_GROUP,
         TYPE          TYPE  BUT000-TYPE,
         ROLE          TYPE  GTTY_V_ROLE,
         PARTNER_GUID  TYPE  BUT000-PARTNER_GUID,
         PARTNER_EXIST TYPE  FLAG,
         EXTEND_COMP   TYPE  FLAG,
       END OF GTY_KEY11.

TYPES: BEGIN OF GTY_KEY12,
         PARTNER       TYPE  BUT000-PARTNER,
         BU_GROUP      TYPE  BUT000-BU_GROUP,
         TYPE          TYPE  BUT000-TYPE,
         ROLE          TYPE  GTTY_V_ROLE,
         PARTNER_GUID  TYPE  BUT000-PARTNER_GUID,
         PARTNER_EXIST TYPE  FLAG,
         EXTEND_COMP   TYPE  FLAG,
       END OF GTY_KEY12.

TYPES: BEGIN OF GTY_V_PHONE ##NEEDED,
         TELNO1  TYPE  ADR2-TEL_NUMBER,
         TELEXT1 TYPE  ADR2-TEL_EXTENS,
         TELNO2  TYPE  ADR2-TEL_NUMBER,
         TELEXT2 TYPE  ADR2-TEL_EXTENS,
         TELNO3  TYPE  ADR2-TEL_NUMBER,
         TELEXT3 TYPE  ADR2-TEL_EXTENS,
         TELNO4  TYPE  ADR2-TEL_NUMBER,
         TELEXT4 TYPE  ADR2-TEL_EXTENS,
         TELNO5  TYPE  ADR2-TEL_NUMBER,
         TELEXT5 TYPE  ADR2-TEL_EXTENS,
         MOBILE1 TYPE  ADR2-TEL_NUMBER,
         MOBILE2 TYPE  ADR2-TEL_NUMBER,
         MOBILE3 TYPE  ADR2-TEL_NUMBER,
         MOBILE4 TYPE  ADR2-TEL_NUMBER,
         MOBILE5 TYPE  ADR2-TEL_NUMBER,
       END OF GTY_V_PHONE.

TYPES: BEGIN OF GTY_V_FAX ##NEEDED,
         FAXNO  TYPE  ADR3-FAX_NUMBER,
         FAXEXT TYPE  ADR3-FAX_EXTENS,
       END OF GTY_V_FAX.

TYPES: BEGIN OF GTY_V_SMTP ##NEEDED,
         EMAIL01 TYPE  ADR6-SMTP_ADDR,
         EMAIL02 TYPE  ADR6-SMTP_ADDR,
         EMAIL03 TYPE  ADR6-SMTP_ADDR,
         EMAIL04 TYPE  ADR6-SMTP_ADDR,
         EMAIL05 TYPE  ADR6-SMTP_ADDR,
         EMAIL06 TYPE  ADR6-SMTP_ADDR,
         EMAIL07 TYPE  ADR6-SMTP_ADDR,
         EMAIL08 TYPE  ADR6-SMTP_ADDR,
         EMAIL09 TYPE  ADR6-SMTP_ADDR,
         EMAIL10 TYPE  ADR6-SMTP_ADDR,
       END OF GTY_V_SMTP.

TYPES: BEGIN OF GTY_V_ADDR,
         GUID           TYPE  BUT020-GUID,
         HCM_EXIST      TYPE  FLAG,            "Related to HR Master
         NAME_FIRST     TYPE  BUT000-NAME_FIRST,
         NAME_LAST      TYPE  BUT000-NAME_LAST,
         PREFIX1        TYPE  BUT000-PREFIX1,
         PREFIX2        TYPE  BUT000-PREFIX2,
         TITLE_ACA1     TYPE  BUT000-TITLE_ACA1,
         TITLE_ACA2     TYPE  BUT000-TITLE_ACA2,
         TITLE_SPPL     TYPE  BUT000-TITLE_ROYL,
         PREFIX1_TXT    TYPE TSAD4-PREFIX_TXT,
         PREFIX2_TXT    TYPE TSAD4-PREFIX_TXT,
         TITLE_ACA1_TXT TYPE TSAD2T-TITLE_DSCR,
         TITLE_ACA2_TXT TYPE TSAD2T-TITLE_DSCR,
         TITLE_SPPL_TXT TYPE TSAD5T-TITLE_DSCR,
         FULLNAME       TYPE  BUT000-NAME1_TEXT,
         NAME1          TYPE  ADRC-NAME1,
         NAME2          TYPE  ADRC-NAME2,
         NAME3          TYPE  ADRC-NAME3,
         NAME4          TYPE  ADRC-NAME4,
         SORT1          TYPE  ADRC-SORT1,
         SORT2          TYPE  ADRC-SORT2,
         STREET         TYPE  ADRC-STREET,      "HouseNo./Street
         STR_SUPPL1     TYPE  ADRC-STR_SUPPL1,  "Street2
         STR_SUPPL2     TYPE  ADRC-STR_SUPPL2,  "Street3
         STR_SUPPL3     TYPE  ADRC-STR_SUPPL3,  "Street4
         LOCATION       TYPE  ADRC-LOCATION,    "Street5
         CITY2          TYPE  ADRC-CITY2,       "District
         CITY1          TYPE  ADRC-CITY1,       "City
         POST_CODE1     TYPE  ADRC-POST_CODE1,  "Postal Code
         COUNTRY        TYPE  ADRC-COUNTRY,     "Country
         LANGU          TYPE  ADRC-LANGU,       "Language
         PHONE          TYPE  GTY_PHONE,
         FAX            TYPE  GTY_FAX,
         SMTP           TYPE  GTY_SMTP,
         REMARK         TYPE  ADRCT-REMARK,     "Comment
         ADEXT          TYPE  BUT020-ADEXT,     "External AddressNo.
         DATE_FROM      TYPE  ADRC-DATE_FROM,
         DATE_TO        TYPE  ADRC-DATE_TO,
       END OF GTY_V_ADDR.

TYPES: BEGIN OF GTY_V_ADDR_INT,
         ACTIVE     TYPE  FLAG,
         NATION     TYPE  ADRC-NATION,
         NAME_FIRST TYPE  BUT000-NAME_FIRST,
         NAME_LAST  TYPE  BUT000-NAME_LAST,
         NAME1      TYPE  ADRC-NAME1,
         NAME2      TYPE  ADRC-NAME2,
         NAME3      TYPE  ADRC-NAME3,
         NAME4      TYPE  ADRC-NAME4,
         SORT1      TYPE  ADRC-SORT1,
         SORT2      TYPE  ADRC-SORT2,
         STREET     TYPE  ADRC-STREET,      "HouseNo./Street
         STR_SUPPL1 TYPE  ADRC-STR_SUPPL1,  "Street2
         STR_SUPPL2 TYPE  ADRC-STR_SUPPL2,  "Street3
         STR_SUPPL3 TYPE  ADRC-STR_SUPPL3,  "Street4
         LOCATION   TYPE  ADRC-LOCATION,    "Street5
         CITY2      TYPE  ADRC-CITY2,       "District
         CITY1      TYPE  ADRC-CITY1,       "City
         POST_CODE1 TYPE  ADRC-POST_CODE1,  "Postal Code
         COUNTRY    TYPE  ADRC-COUNTRY,     "Country
         LANGU      TYPE  ADRC-LANGU,
       END OF GTY_V_ADDR_INT.

TYPES: BEGIN OF GTY_V_ADDR_IND,
         TELNO  TYPE  ADR2-TEL_NUMBER,
         MOBILE TYPE  ADR2-TEL_NUMBER,
         EMAIL  TYPE  ADR6-SMTP_ADDR,
         NOTE   TYPE ADRT-REMARK,       "iPS136Oct2021 [Rollout Captain]
       END OF GTY_V_ADDR_IND.

TYPES: BEGIN OF GTY_V_TAX,
         TAXTYPE TYPE  DFKKBPTAXNUM-TAXTYPE,  "Tax Type
         TAXNUM  TYPE  DFKKBPTAXNUM-TAXNUM,   "Tax No.
       END OF GTY_V_TAX.

TYPES: BEGIN OF GTY_V_BANK,
         BANK_EXIST TYPE  FLAG,
         BKVID      TYPE  BUT0BK-BKVID,          "Bank ID
         BANKS      TYPE  BUT0BK-BANKS,          "Bank Country
         BANKL      TYPE  BUT0BK-BANKL,          "Bank Key
         BANKN      TYPE  BUT0BK-BANKN,          "Bank Account
         BKONT      TYPE  BUT0BK-BKONT,          "Control Key
         BKREF      TYPE  BUT0BK-BKREF,          "Ref Detail
         BKEXT      TYPE  BUT0BK-BKEXT,          "Ext ID
         KOINH      TYPE  BUT0BK-KOINH,          "Account holder
       END OF GTY_V_BANK.

TYPES: BEGIN OF GTY_V_VEND,
         EXIST       TYPE  FLAG,
         PAYEE_EXIST TYPE FLAG, "Alt payee
         LIFNR       TYPE  LFA1-LIFNR,
         VBUND       TYPE  LFA1-VBUND,
         KUNNR       TYPE  LFA1-KUNNR,
         XZEMP       TYPE  LFA1-XZEMP,
         EMPFK       TYPE  LFZA-EMPFK,
         NAMEV       TYPE  KNVK-NAMEV,
         NAME1       TYPE  KNVK-NAME1,
       END OF GTY_V_VEND.

TYPES: BEGIN OF GTY_V_WTAX,
         EXIST  TYPE  FLAG,
         WITHT  TYPE  LFBW-WITHT,     "Withhold Type
         WITHCD TYPE  LFBW-WT_WITHCD, "Withhold Code
         SUBJCT TYPE  LFBW-WT_SUBJCT, "Subject
         QSREC  TYPE  LFBW-QSREC,     "Type of Recipient
       END OF GTY_V_WTAX.

TYPES: BEGIN OF GTY_V_BRANCH,
         BCODE TYPE  FITHA_PBUPL_D-J_1TPBUPL,      "Branch
         BDESC TYPE  FITHA_PBUPL_D_T-DESCRIPTION,  "Branch Desc.
         BDEFT TYPE  FITHA_PBUPL_D-DEFAULT_BRANCH, "Default Branch
       END OF GTY_V_BRANCH.

TYPES: BEGIN OF GTY_V_COMP,
         EXIST  TYPE  FLAG,
         BUKRS  TYPE  LFB1-BUKRS,
         AKONT  TYPE  LFB1-AKONT,
         ZUAWA  TYPE  LFB1-ZUAWA,
         FDGRV  TYPE  LFB1-FDGRV,
         ALTKN  TYPE  LFB1-ALTKN,
         ZTERM  TYPE  LFB1-ZTERM,
         REPRF  TYPE  LFB1-REPRF,
         ZWELS  TYPE  LFB1-ZWELS,
         HBKID  TYPE  LFB1-HBKID,
         WTAX1  TYPE  GTY_V_WTAX,
         WTAX2  TYPE  GTY_V_WTAX,
         BRANCH TYPE  GTY_V_BRANCH,
       END OF GTY_V_COMP.

TYPES: BEGIN OF GTY_V_PURCH,
         EXIST   TYPE  FLAG,
*         ekorg TYPE  lfm1-ekorg,
*         waers TYPE  lfm1-waers,
*         zterm TYPE  lfm1-zterm,
*         inco1 TYPE  lfm1-inco1,
*         inco2 TYPE  lfm1-inco2,
*         verkf TYPE  lfm1-verkf,
*         telf1 TYPE  lfm1-telf1,
*         webre TYPE  lfm1-webre,
*         ekgrp TYPE  lfm1-ekgrp,
*         kzaut TYPE  lfm1-kzaut,
         EKORG   TYPE  LFM1-EKORG,
         WAERS   TYPE  LFM1-WAERS,
         ZTERM   TYPE  LFM1-ZTERM,
         MINBW   TYPE  LFM1-MINBW,
         INCO1   TYPE  LFM1-INCO1,
         INCO2_L TYPE  LFM1-INCO2_L,
         INCO3_L TYPE  LFM1-INCO3_L,
         VERKF   TYPE  LFM1-VERKF,
         TELF1   TYPE  LFM1-TELF1,
         WEBRE   TYPE  LFM1-WEBRE,
         LEBRE   TYPE  LFM1-LEBRE,
         BOLRE   TYPE  LFM1-BOLRE,
         UMSAE   TYPE  LFM1-UMSAE,
         BOIND   TYPE  LFM1-BOIND,
         EKGRP   TYPE  LFM1-EKGRP,
         PLIFZ   TYPE  LFM1-PLIFZ,
         AGREL   TYPE  LFM1-EKGRP,
         KZAUT   TYPE  LFM1-KZAUT,
       END OF GTY_V_PURCH.

TYPES: BEGIN OF GTY_MAIN11,
         ADDR     TYPE  GTY_V_ADDR,
         ADDR_INT TYPE  GTY_V_ADDR_INT,
         ADDR_IND TYPE  GTY_V_ADDR_IND,
         TAX      TYPE  GTY_V_TAX,
         BANK01   TYPE  GTY_V_BANK,
         BANK02   TYPE  GTY_V_BANK,
         BANK03   TYPE  GTY_V_BANK,
         BANK04   TYPE  GTY_V_BANK,
         BANK05   TYPE  GTY_V_BANK,
         BANK06   TYPE  GTY_V_BANK,
         VEND     TYPE  GTY_V_VEND,
         COMP     TYPE  GTY_V_COMP,
         PURCH    TYPE  GTY_V_PURCH,
         BPKIND   TYPE  BUT000-BPKIND,  "BP Type
         BPEXT    TYPE  BUT000-BPEXT,   "External BPNo.
       END OF GTY_MAIN11.

TYPES: BEGIN OF GTY_MAIN12,
         ADDR     TYPE  GTY_V_ADDR,
         ADDR_INT TYPE  GTY_V_ADDR_INT,
         ADDR_IND TYPE  GTY_V_ADDR_IND,
         TAX      TYPE  GTY_V_TAX,
         BANK01   TYPE  GTY_V_BANK,
         BANK02   TYPE  GTY_V_BANK,
         VEND     TYPE  GTY_V_VEND,
         COMP     TYPE  GTY_V_COMP,
         PURCH    TYPE  GTY_V_PURCH,
         BPKIND   TYPE  BUT000-BPKIND,  "BP Type
         BPEXT    TYPE  BUT000-BPEXT,   "External BPNo.
       END OF GTY_MAIN12.

TYPES: BEGIN OF GTY_V_PHONEX ##NEEDED,
         TELNO1  TYPE  FLAG,
         TELEXT1 TYPE  FLAG,
         TELNO2  TYPE  FLAG,
         TELEXT2 TYPE  FLAG,
         TELNO3  TYPE  FLAG,
         TELEXT3 TYPE  FLAG,
         TELNO4  TYPE  FLAG,
         TELEXT4 TYPE  FLAG,
         TELNO5  TYPE  FLAG,
         TELEXT5 TYPE  FLAG,
         MOBILE1 TYPE  FLAG,
         MOBILE2 TYPE  FLAG,
         MOBILE3 TYPE  FLAG,
         MOBILE4 TYPE  FLAG,
         MOBILE5 TYPE  FLAG,
       END OF GTY_V_PHONEX.

TYPES: BEGIN OF GTY_V_FAXX ##NEEDED,
         FAXNO  TYPE  FLAG,
         FAXEXT TYPE  FLAG,
       END OF GTY_V_FAXX.

TYPES: BEGIN OF GTY_V_SMTPX ##NEEDED,
         EMAIL01 TYPE  FLAG,
         EMAIL02 TYPE  FLAG,
         EMAIL03 TYPE  FLAG,
         EMAIL04 TYPE  FLAG,
         EMAIL05 TYPE  FLAG,
         NOTE01  TYPE  FLAG,
         NOTE02  TYPE  FLAG,
         NOTE03  TYPE  FLAG,
         NOTE04  TYPE  FLAG,
         NOTE05  TYPE  FLAG,
       END OF GTY_V_SMTPX.

TYPES: BEGIN OF GTY_V_ADDRX,
         GUID       TYPE  FLAG,
         NAME_FIRST TYPE  FLAG,
         NAME_LAST  TYPE  FLAG,
         PREFIX1    TYPE  FLAG,
         PREFIX2    TYPE  FLAG,
         TITLE_ACA1 TYPE  FLAG,
         TITLE_ACA2 TYPE  FLAG,
         TITLE_SPPL TYPE  FLAG,
         FULLNAME   TYPE  FLAG,
         NAME1      TYPE  FLAG,
         NAME2      TYPE  FLAG,
         NAME3      TYPE  FLAG,
         NAME4      TYPE  FLAG,
         SORT1      TYPE  FLAG,
         SORT2      TYPE  FLAG,
         STREET     TYPE  FLAG,  "HouseNo./Street
         STR_SUPPL1 TYPE  FLAG,  "Street2
         STR_SUPPL2 TYPE  FLAG,  "Street3
         STR_SUPPL3 TYPE  FLAG,  "Street4
         LOCATION   TYPE  FLAG,  "Street5
         CITY2      TYPE  FLAG,  "District
         CITY1      TYPE  FLAG,  "City
         POST_CODE1 TYPE  FLAG,  "Postal Code
         COUNTRY    TYPE  FLAG,  "Country
         LANGU      TYPE  FLAG,  "Language
         PHONE      TYPE  GTY_PHONEX,
         FAX        TYPE  GTY_FAXX,
         SMTP       TYPE  GTY_SMTPX,
         REMARK     TYPE  FLAG,     "Comment
         ADEXT      TYPE  FLAG,     "External AddressNo.
         DATE_FROM  TYPE  FLAG,
         DATE_TO    TYPE  FLAG,
       END OF GTY_V_ADDRX.

TYPES: BEGIN OF GTY_V_ADDR_INTX,
         NAME_FIRST TYPE  FLAG,
         NAME_LAST  TYPE  FLAG,
         NAME1      TYPE  FLAG,
         NAME2      TYPE  FLAG,
         NAME3      TYPE  FLAG,
         NAME4      TYPE  FLAG,
         SORT1      TYPE  FLAG,
         SORT2      TYPE  FLAG,
         STREET     TYPE  FLAG,  "HouseNo./Street
         STR_SUPPL1 TYPE  FLAG,  "Street2
         STR_SUPPL2 TYPE  FLAG,  "Street3
         STR_SUPPL3 TYPE  FLAG,  "Street4
         LOCATION   TYPE  FLAG,  "Street5
         CITY2      TYPE  FLAG, "District
         CITY1      TYPE  FLAG,  "City
         POST_CODE1 TYPE  FLAG,  "Postal Code
         COUNTRY    TYPE  FLAG,  "Country
         LANGU      TYPE  FLAG,
       END OF GTY_V_ADDR_INTX.

TYPES: BEGIN OF GTY_V_ADDR_INDX,
         TELNO  TYPE  FLAG,
         MOBILE TYPE  FLAG,
         EMAIL  TYPE  FLAG,
         NOTE   TYPE FLAG,       "iPS136Oct2021 [Rollout Captain]
       END OF GTY_V_ADDR_INDX.

TYPES: BEGIN OF GTY_V_TAXX,
         TAXTYPE TYPE  FLAG,  "Tax Type
         TAXNUM  TYPE  FLAG,   "Tax No.
       END OF GTY_V_TAXX.

TYPES: BEGIN OF GTY_V_BANKX,
         BKVID TYPE  FLAG,          "Bank ID
         BANKS TYPE  FLAG,          "Bank Country
         BANKL TYPE  FLAG,          "Bank Key
         BANKN TYPE  FLAG,          "Bank Account
         BKONT TYPE  FLAG,          "Control Key
         BKREF TYPE  FLAG,          "Ref Detail
         BKEXT TYPE  FLAG,          "Ext ID
         KOINH TYPE  FLAG,          "Account Holder
       END OF GTY_V_BANKX.

TYPES: BEGIN OF GTY_V_VENDX,
         VBUND TYPE  FLAG,
         KUNNR TYPE  FLAG,
         XZEMP TYPE  FLAG,
         EMPFK TYPE  FLAG,
       END OF GTY_V_VENDX.

TYPES: BEGIN OF GTY_V_WTAXX,
         WITHCD TYPE  FLAG, "Withhold Code
         SUBJCT TYPE  FLAG, "Subject
         QSREC  TYPE  FLAG, "Type of Recipient
       END OF GTY_V_WTAXX.

TYPES: BEGIN OF GTY_V_BRANCHX,
         BCODE TYPE  FLAG,  "Branch
         BDESC TYPE  FLAG,  "Branch Desc.
         BDEFT TYPE  FLAG,  "Default Branch
       END OF GTY_V_BRANCHX.

TYPES: BEGIN OF GTY_V_COMPX,
         AKONT  TYPE  FLAG,
         ZUAWA  TYPE  FLAG,
         FDGRV  TYPE  FLAG,
         ALTKN  TYPE  FLAG,
         ZTERM  TYPE  FLAG,
         REPRF  TYPE  FLAG,
         ZWELS  TYPE  FLAG,
         HBKID  TYPE  FLAG,
         WTAX1  TYPE  GTY_V_WTAXX,
         WTAX2  TYPE  GTY_V_WTAXX,
         BRANCH TYPE  GTY_V_BRANCHX,
       END OF GTY_V_COMPX.

TYPES: BEGIN OF GTY_V_PURCHX,
*         waers   TYPE  flag,
*         zterm   TYPE  flag,
*         inco1   TYPE  flag,
*         inco2_l TYPE  flag,
*         inco3_l TYPE  flag,
*         verkf   TYPE  flag,
*         telf1   TYPE  flag,
*         webre   TYPE  flag,
*         ekgrp   TYPE  flag,
*         kzaut   TYPE  flag,
         WAERS   TYPE  FLAG,
         ZTERM   TYPE  FLAG,
         MINBW   TYPE  FLAG,
         INCO1   TYPE  FLAG,
         INCO2_L TYPE  FLAG,
         INCO3_L TYPE  FLAG,
         VERKF   TYPE  FLAG,
         TELF1   TYPE  FLAG,
         WEBRE   TYPE  FLAG,
         LEBRE   TYPE  FLAG,
         BOLRE   TYPE  FLAG,
         UMSAE   TYPE  FLAG,
         BOIND   TYPE  FLAG,
         EKGRP   TYPE  FLAG,
         PLIFZ   TYPE  FLAG,
         AGREL   TYPE  FLAG,
         KZAUT   TYPE  FLAG,
       END OF GTY_V_PURCHX.

TYPES: BEGIN OF GTY_MAIN11X,
         ADDR     TYPE  GTY_V_ADDRX,
         ADDR_INT TYPE  GTY_V_ADDR_INTX,
         ADDR_IND TYPE  GTY_V_ADDR_INDX,
         TAX      TYPE  GTY_V_TAXX,
         BANK01   TYPE  GTY_V_BANKX,
         BANK02   TYPE  GTY_V_BANKX,
         BANK03   TYPE  GTY_V_BANKX,
         BANK04   TYPE  GTY_V_BANKX,
         BANK05   TYPE  GTY_V_BANKX,
         BANK06   TYPE  GTY_V_BANKX,
         VEND     TYPE  GTY_V_VENDX,
         COMP     TYPE  GTY_V_COMPX,
         PURCH    TYPE  GTY_V_PURCHX,
         BPKIND   TYPE  FLAG,
         BPEXT    TYPE  FLAG,
       END OF GTY_MAIN11X.

TYPES: BEGIN OF GTY_MAIN12X,
         ADDR     TYPE  GTY_V_ADDRX,
         ADDR_INT TYPE  GTY_V_ADDR_INTX,
         ADDR_IND TYPE  GTY_V_ADDR_INDX,
         TAX      TYPE  GTY_V_TAXX,
         BANK01   TYPE  GTY_V_BANKX,
         BANK02   TYPE  GTY_V_BANKX,
         VEND     TYPE  GTY_V_VENDX,
         COMP     TYPE  GTY_V_COMPX,
         PURCH    TYPE  GTY_V_PURCHX,
         BPKIND   TYPE  FLAG,
       END OF GTY_MAIN12X.

TYPES: BEGIN OF GTY_DATA11,
         ROWNO TYPE  GTY_RESULT11-ROWNO,
         KEY   TYPE  GTY_KEY11,
         MAIN  TYPE  GTY_MAIN11,
         MAINX TYPE  GTY_MAIN11X,
         MESSG TYPE  GTTY_MESSG,
       END OF GTY_DATA11.
TYPES: GTTY_DATA11 TYPE STANDARD TABLE OF GTY_DATA11.

TYPES: BEGIN OF GTY_DATA12,
         ROWNO TYPE  GTY_RESULT12-ROWNO,
         KEY   TYPE  GTY_KEY12,
         MAIN  TYPE  GTY_MAIN12,
         MAINX TYPE  GTY_MAIN12X,
         MESSG TYPE  GTTY_MESSG,
       END OF GTY_DATA12.
TYPES: GTTY_DATA12 TYPE STANDARD TABLE OF GTY_DATA12.

TYPES: BEGIN OF GTY_MAIN3,
         PARTNER      TYPE  BUT000-PARTNER,
         PARTNER_GUID TYPE  BUT000-PARTNER_GUID,
         PARTNER2     TYPE  BUT050-PARTNER2,
       END OF GTY_MAIN3.

TYPES: BEGIN OF GTY_DATA13,
         ROWNO TYPE  GTY_RESULT13-ROWNO,
         MAIN  TYPE  GTY_MAIN3,
         MESSG TYPE  GTTY_MESSG,
       END OF GTY_DATA13.
TYPES: GTTY_DATA13  TYPE  STANDARD TABLE OF GTY_DATA13.

* *****************************
* Types for Excel Template
* !!!! Changing field name impact to validation logic !!!
* *****************************
*-------Template for Customer-------------------
TYPES: BEGIN OF GTY_TEMPLATE11 ##NEEDED,
         ALTKN              TYPE KNB1-ALTKN,
         PARTNER            TYPE BUT000-PARTNER,
         BU_GROUP           TYPE BUT000-BU_GROUP,
*         type               TYPE but000-type,
         RLTYP              TYPE  BUT100-RLTYP, "BP Role

         ADDR_NAME1         TYPE ADRC-NAME1,
         ADDR_NAME2         TYPE ADRC-NAME2,
         ADDR_NAME3         TYPE ADRC-NAME3,
         ADDR_NAME4         TYPE ADRC-NAME4,
         ADDR_SORT1         TYPE ADRC-SORT1,
         ADDR_SORT2         TYPE ADRC-SORT2,
         ADDR_STREET        TYPE ADRC-STREET,
         ADDR_STR_SUPPL1    TYPE ADRC-STR_SUPPL1,
         ADDR_STR_SUPPL2    TYPE ADRC-STR_SUPPL2,
         ADDR_STR_SUPPL3    TYPE ADRC-STR_SUPPL3,
         ADDR_LOCATION      TYPE ADRC-LOCATION,
         ADDR_CITY2         TYPE ADRC-CITY2,      "Add+ 28/06/2024  Zulkiff B.
         ADDR_CITY1         TYPE ADRC-CITY1,
         ADDR_POST_CODE1    TYPE ADRC-POST_CODE1,
         ADDR_COUNTRY       TYPE ADRC-COUNTRY,
         ADDR_REGION        TYPE ADRC-REGION,    "Add+ 28/06/2024  Zulkiff B.
         ADDR_LANGU         TYPE ADRC-LANGU,
         ADDR_TELNO1        TYPE ADR2-TEL_NUMBER,
         ADDR_TELEXT1       TYPE ADR2-TEL_EXTENS,
         ADDR_MOBILE1       TYPE ADR2-TEL_NUMBER,
         ADDR_FAXNO         TYPE ADR3-FAX_NUMBER,
         ADDR_FAXEXT        TYPE ADR3-FAX_EXTENS,
         ADDR_EMAIL01       TYPE ADR6-SMTP_ADDR,
         ADDRINT_NAME1      TYPE ADRC-NAME1,
         ADDRINT_NAME2      TYPE ADRC-NAME2,
         ADDRINT_NAME3      TYPE ADRC-NAME3,
         ADDRINT_NAME4      TYPE ADRC-NAME4,
         ADDRINT_SORT1      TYPE ADRC-SORT1,
         ADDRINT_SORT2      TYPE ADRC-SORT1,
         ADDRINT_STREET     TYPE ADRC-STREET,
         ADDRINT_STR_SUPPL1 TYPE ADRC-STR_SUPPL1,
         ADDRINT_STR_SUPPL2 TYPE ADRC-STR_SUPPL2,
         ADDRINT_STR_SUPPL3 TYPE ADRC-STR_SUPPL3,
         ADDRINT_LOCATION   TYPE ADRC-LOCATION,
         ADDRINT_CITY2      TYPE ADRC-CITY2,            "Add+ 28/06/2024  Zulkiff B.
         ADDRINT_CITY1      TYPE ADRC-CITY1,
         ADDRINT_POST_CODE1 TYPE ADRC-POST_CODE1,
         ADDRINT_COUNTRY    TYPE ADRC-COUNTRY,
         ADDRINT_LANGU      TYPE ADRC-LANGU,
         BPEXT              TYPE BUT000-BPEXT,
         TAX_TAXTYPE        TYPE DFKKBPTAXNUM-TAXTYPE,
         TAX_TAXNUM         TYPE DFKKBPTAXNUM-TAXNUM,
         BPKIND             TYPE BUT000-BPKIND,
         VEND_VBUND         TYPE LFA1-VBUND,

         BNK01_BKVID        TYPE BUT0BK-BKVID, "Bank ID
         BNK01_BANKS        TYPE BUT0BK-BANKS, "Bank Country
         BNK01_BANKL        TYPE BUT0BK-BANKL, "Bank Key
         BNK01_BANKN        TYPE BUT0BK-BANKN, "Bank Account
         BNK01_KOINH        TYPE BUT0BK-KOINH, "Account holder
         BNK01_BKREF        TYPE BUT0BK-BKREF, "Reference Details for Bank Details "Add+ 28/06/2024  Zulkiff B.

         VEND_XZEMP         TYPE LFA1-XZEMP,
         VEND_LNRZA         TYPE LFA1-LNRZA,
         COMP_BUKRS         TYPE LFB1-BUKRS,
         COMP_AKONT         TYPE LFB1-AKONT,
         COMP_ZUAWA         TYPE LFB1-ZUAWA,
         COMP_FDGRV         TYPE LFB1-FDGRV,
         COMP_MINDK         TYPE LFB1-ALTKN,
         COMP_ALTKN         TYPE LFB1-ALTKN,
         COMP_ZTERM         TYPE LFB1-ZTERM,
         COMP_REPRF         TYPE LFB1-REPRF,
         COMP_ZWELS         TYPE LFB1-ZWELS,
         COMP_HBKID         TYPE LFB1-HBKID,
         VEND_EMPFK         TYPE LFZA-EMPFK,
         WTAX_WITHT1        TYPE LFBW-WITHT,     "Withhold Type
         WTAX_WITHCD1       TYPE LFBW-WT_WITHCD, "Withhold Code
         WTAX_SUBJCT1       TYPE LFBW-WT_SUBJCT, "Subject
         WTAX_QSREC1        TYPE LFBW-QSREC,     "Type of Recipient
         WTAX_WITHT2        TYPE LFBW-WITHT,     "Withhold Type
         WTAX_WITHCD2       TYPE LFBW-WT_WITHCD, "Withhold Code
         WTAX_SUBJCT2       TYPE LFBW-WT_SUBJCT, "Subject
         WTAX_QSREC2        TYPE LFBW-QSREC,     "Type of Recipient
         BCODE              TYPE FITHA_PBUPL_D-J_1TPBUPL,      "Branch
         BDESC              TYPE FITHA_PBUPL_D_T-DESCRIPTION,  "Branch Desc.

         BNK02_BKVID        TYPE BUT0BK-BKVID, "Bank ID
         BNK02_BANKS        TYPE BUT0BK-BANKS, "Bank Country
         BNK02_BANKL        TYPE BUT0BK-BANKL, "Bank Key
         BNK02_BANKN        TYPE BUT0BK-BANKN, "Bank Account
         BNK02_KOINH        TYPE BUT0BK-KOINH, "Account holder

         BNK03_BKVID        TYPE BUT0BK-BKVID, "Bank ID
         BNK03_BANKS        TYPE BUT0BK-BANKS, "Bank Country
         BNK03_BANKL        TYPE BUT0BK-BANKL, "Bank Key
         BNK03_BANKN        TYPE BUT0BK-BANKN, "Bank Account
         BNK03_KOINH        TYPE BUT0BK-KOINH, "Account holder

         BNK04_BKVID        TYPE BUT0BK-BKVID, "Bank ID
         BNK04_BANKS        TYPE BUT0BK-BANKS, "Bank Country
         BNK04_BANKL        TYPE BUT0BK-BANKL, "Bank Key
         BNK04_BANKN        TYPE BUT0BK-BANKN, "Bank Account
         BNK04_KOINH        TYPE BUT0BK-KOINH, "Account holder

         BNK05_BKVID        TYPE BUT0BK-BKVID, "Bank ID
         BNK05_BANKS        TYPE BUT0BK-BANKS, "Bank Country
         BNK05_BANKL        TYPE BUT0BK-BANKL, "Bank Key
         BNK05_BANKN        TYPE BUT0BK-BANKN, "Bank Account
         BNK05_KOINH        TYPE BUT0BK-KOINH, "Account holder

         BNK06_BKVID        TYPE BUT0BK-BKVID, "Bank ID
         BNK06_BANKS        TYPE BUT0BK-BANKS, "Bank Country
         BNK06_BANKL        TYPE BUT0BK-BANKL, "Bank Key
         BNK06_BANKN        TYPE BUT0BK-BANKN, "Bank Account
         BNK06_KOINH        TYPE BUT0BK-KOINH, "Account holder
       END OF GTY_TEMPLATE11.

TYPES: BEGIN OF GTY_TEMPLATE12 ##NEEDED,
         ALTKN              TYPE  KNB1-ALTKN,   "Previous Account
         PARTNER            TYPE  BUT000-PARTNER,
         BU_GROUP           TYPE  BUT000-BU_GROUP,
*         type               TYPE  but000-type,
         RLTYP              TYPE  BUT100-RLTYP, "BP Role
         ADDR_NAME1         TYPE  ADRC-NAME1,
         ADDR_NAME2         TYPE  ADRC-NAME2,
         ADDR_NAME3         TYPE  ADRC-NAME3,
         ADDR_NAME4         TYPE  ADRC-NAME4,
         ADDR_SORT1         TYPE  ADRC-SORT1,
         ADDR_SORT2         TYPE  ADRC-SORT2,
         ADDR_STREET        TYPE  ADRC-STREET,
         ADDR_STR_SUPPL1    TYPE ADRC-STR_SUPPL1,
         ADDR_STR_SUPPL2    TYPE ADRC-STR_SUPPL2,
         ADDR_STR_SUPPL3    TYPE ADRC-STR_SUPPL3,
         ADDR_LOCATION      TYPE ADRC-LOCATION,
         ADDR_CITY2         TYPE ADRC-CITY2,      "Add+ 01/07/2024  Zulkiff B.
         ADDR_CITY1         TYPE ADRC-CITY1,
         ADDR_POST_CODE1    TYPE ADRC-POST_CODE1,
         ADDR_COUNTRY       TYPE ADRC-COUNTRY,
         ADDR_REGION        TYPE ADRC-REGION,    "Add+ 01/07/2024  Zulkiff B.
         ADDR_LANGU         TYPE ADRC-LANGU,
         ADDR_TELNO1        TYPE ADR2-TEL_NUMBER,
         ADDR_TELEXT1       TYPE ADR2-TEL_EXTENS,
         ADDR_MOBILE1       TYPE ADR2-TEL_NUMBER,
         ADDR_FAXNO         TYPE ADR3-FAX_NUMBER,
         ADDR_FAXEXT        TYPE ADR3-FAX_EXTENS,
         ADDR_EMAIL01       TYPE ADR6-SMTP_ADDR,
         ADDRINT_NAME1      TYPE ADRC-NAME1,
         ADDRINT_NAME2      TYPE ADRC-NAME2,
         ADDRINT_NAME3      TYPE ADRC-NAME3,
         ADDRINT_NAME4      TYPE ADRC-NAME4,
         ADDRINT_SORT1      TYPE ADRC-SORT1,
         ADDRINT_SORT2      TYPE ADRC-SORT1,
         ADDRINT_STREET     TYPE ADRC-STREET,
         ADDRINT_STR_SUPPL1 TYPE ADRC-STR_SUPPL1,
         ADDRINT_STR_SUPPL2 TYPE ADRC-STR_SUPPL2,
         ADDRINT_STR_SUPPL3 TYPE ADRC-STR_SUPPL3,
         ADDRINT_LOCATION   TYPE ADRC-LOCATION,
         ADDRINT_CITY2      TYPE ADRC-CITY2,            "Add+ 01/07/2024  Zulkiff B.
         ADDRINT_CITY1      TYPE ADRC-CITY1,
         ADDRINT_POST_CODE1 TYPE ADRC-POST_CODE1,
         ADDRINT_COUNTRY    TYPE ADRC-COUNTRY,
         ADDRINT_LANGU      TYPE ADRC-LANGU,
         BPEXT              TYPE BUT000-BPEXT,
         TAX_TAXTYPE        TYPE DFKKBPTAXNUM-TAXTYPE,
         TAX_TAXNUM         TYPE DFKKBPTAXNUM-TAXNUM,
         BPKIND             TYPE BUT000-BPKIND,  "BP Type
         VEND_VBUND         TYPE LFA1-VBUND,

         BNK01_BKVID        TYPE BUT0BK-BKVID,          "Bank ID
         BNK01_BANKS        TYPE BUT0BK-BANKS,          "Bank Country
         BNK01_BANKL        TYPE BUT0BK-BANKL,          "Bank Key
         BNK01_BANKN        TYPE BUT0BK-BANKN,          "Bank Account
         BNK01_KOINH        TYPE BUT0BK-KOINH,          "Account holder
         BNK01_BKREF        TYPE BUT0BK-BKREF, "Reference Details for Bank Details "Add+01/07/2024  Zulkiff B.

         VEND_XZEMP         TYPE LFA1-XZEMP,
         VEND_LNRZA         TYPE LFA1-LNRZA,
         PURC_EKORG         TYPE LFM1-EKORG,
         PURC_WAERS         TYPE LFM1-WAERS,
         PURC_ZTERM         TYPE LFM1-ZTERM,
         PURC_MINBW         TYPE LFM1-MINBW,
         PURC_INCO1         TYPE LFM1-INCO1,
         PURC_INCO2_L       TYPE LFM1-INCO2_L,
         PURC_INCO3_L       TYPE LFM1-INCO3_L,
         PURC_VERKF         TYPE LFM1-VERKF,
         PURC_TELF1         TYPE LFM1-TELF1,
         PURC_WEBRE         TYPE LFM1-WEBRE,
         PURC_LEBRE         TYPE LFM1-LEBRE,
         PURC_BOLRE         TYPE LFM1-BOLRE,
         PURC_UMSAE         TYPE LFM1-UMSAE,
         PURC_BOIND         TYPE LFM1-BOIND,
         PURC_EKGRP         TYPE LFM1-EKGRP,
         PURC_PLIFZ         TYPE LFM1-PLIFZ,
         PURC_AGREL         TYPE LFM1-EKGRP,
         PURC_KZAUT         TYPE LFM1-KZAUT,

         KNVK_NAMEV         TYPE KNVK-NAMEV,      "Contact Person       "Add+01/07/2024  Zulkiff B.
         KNVK_NAME1         TYPE KNVK-NAME1,      "Contact Person name "Add+01/07/2024  Zulkiff B.

       END OF GTY_TEMPLATE12.

TYPES: BEGIN OF GTY_TEMPLATE13 ##NEEDED,
         PARTNER  TYPE  BUT000-PARTNER,
         PARTNER2 TYPE  BUT050-PARTNER2,
       END OF GTY_TEMPLATE13.

*-------Template for Customer-------------------
TYPES: BEGIN OF GTY_TEMPLATE21 ##NEEDED,
         RECNO                 TYPE  FPM_ROW,            "CH01+                  "1
         ALTKN                 TYPE  KNB1-ALTKN,  "Previous account no.
         PARTNER               TYPE  BUT000-PARTNER, "Business Partner
         BU_GROUP              TYPE  BUT000-BU_GROUP, "Business Partner Group
         RLTYP                 TYPE  BUT100-RLTYP, "Business Partner Role
*         type               TYPE  but000-type, "Business Partner Type
         BPKIND                TYPE  BUT000-BPKIND, "Business Partner Type
         ADDR_TITLE            TYPE  BUT000-TITLE,  "jj
         ADDR_NAME1            TYPE  ADRC-NAME1,
         ADDR_NAME2            TYPE  ADRC-NAME2,
         ADDR_NAME3            TYPE  ADRC-NAME3,                                 "10
         ADDR_NAME4            TYPE  ADRC-NAME4,
         ADDR_SORT1            TYPE  ADRC-SORT1,
         ADDR_SORT2            TYPE  ADRC-SORT2,
         ADDR_NAME_CO          TYPE  ADRC-NAME_CO,
         ADDR_STR_SUPPL1       TYPE  ADRC-STR_SUPPL1,  "Street4
         ADDR_STR_SUPPL2       TYPE  ADRC-STR_SUPPL2,
         ADDR_STREET           TYPE  ADRC-STREET,      "HouseNo./Street
         ADDR_STR_SUPPL3       TYPE  ADRC-STR_SUPPL3,  "Street4
         ADDR_LOCATION         TYPE  ADRC-LOCATION,    "Street5
         ADDR_CITY2            TYPE  ADRC-CITY2,       "District                 "20
         ADDR_CITY1            TYPE  ADRC-CITY1,       "City
         ADDR_POST_CODE1       TYPE  ADRC-POST_CODE1,  "Postal Code
         ADDR_COUNTRY          TYPE  ADRC-COUNTRY,     "Country
         ADDR_LANGU            TYPE  ADRC-LANGU,       "Language
         ADDR_TELNO1           TYPE  ADR2-TEL_NUMBER,
         ADDR_FAXNO            TYPE  ADR3-FAX_NUMBER,
         ADDR_FAXEXT           TYPE  ADR3-FAX_EXTENS,
* BOD - CH01
*         ADDR_DEFLT_COMM       TYPE  ADRC-DEFLT_COMM,  "jj
*         ADDR_REMARK           TYPE  ADRCT-REMARK,     "Comment
*         ADDR_VALID_FROM       TYPE  BUT020-ADDR_VALID_FROM ,  "jj
*         ADDR_VALID_TO         TYPE  BUT020-ADDR_VALID_TO ,  "jj
*         ADDR_ADEXT            TYPE  BUT020-ADEXT,
*         ADDRINT_TITLE         TYPE  BUT000-TITLE, "jj
* EOD - CH01
         ADDRINT_NAME1         TYPE  ADRC-NAME1,
         ADDRINT_NAME2         TYPE  ADRC-NAME2,
         ADDRINT_NAME3         TYPE  ADRC-NAME3,                                "30
         ADDRINT_NAME4         TYPE  ADRC-NAME4,
         ADDRINT_SORT1         TYPE  ADRC-SORT1,
         ADDRINT_SORT2         TYPE  ADRC-SORT2,
         ADDRINT_NAME_CO       TYPE  ADRC-NAME_CO,
         ADDRINT_STR_SUPPL1    TYPE  ADRC-STR_SUPPL1,  "Street4
         ADDRINT_STR_SUPPL2    TYPE  ADRC-STR_SUPPL2,
         ADDRINT_STREET        TYPE  ADRC-STREET,      "HouseNo./Street
         ADDRINT_STR_SUPPL3    TYPE  ADRC-STR_SUPPL3,  "Street4
         ADDRINT_LOCATION      TYPE  ADRC-LOCATION,    "Street5
         ADDRINT_CITY2         TYPE  ADRC-CITY2,       "District                "40
         ADDRINT_CITY1         TYPE  ADRC-CITY1,       "City
         ADDRINT_POST_CODE1    TYPE  ADRC-POST_CODE1,  "Postal Code
         ADDRINT_COUNTRY       TYPE  ADRC-COUNTRY,     "Country
         ADDRINT_LANGU         TYPE  ADRC-LANGU,       "Language
         ADDRINT_REMARK        TYPE  ADRCT-REMARK,     "Comment
         LEGAL_ENTY            TYPE  TP08-LEGAL_ENTY, "Legal Form "jj
         LEGAL_ORG             TYPE  TP09T-LEGAL_ORG, "Legal Entity "jj
         BPEXT                 TYPE  BUT000-BPEXT,   "External BPNo.
         TAXTYPE               TYPE  DFKKBPTAXNUM-TAXTYPE,  "Tax Type
         TAXNUM                TYPE  DFKKBPTAXNUM-TAXNUM,   "Tax No.            "50
         VBUND                 TYPE  BP001-VBUND, "Trading Partner "jj
         LIFNR                 TYPE  KNA1-LIFNR , "Supplier  jj
*         bnk01_banks        TYPE  but0bk-banks,          "Bank Country
*         bnk01_bankn        TYPE  but0bk-bankn,          "Bank Account
*         bnk01_bankl        TYPE  but0bk-bankl,          "Bank Key
*         bnk01_koinh        TYPE  but0bk-koinh,          "Holder name
*         bnk01_accname      TYPE  but0bk-accname,        "Account name

         BNK01_BVTYP           TYPE KNBK-BVTYP,   "Partner bank type
         BNK01_BANKS           TYPE BUT0BK-BANKS, "Bank Country
         BNK01_BANKL           TYPE BUT0BK-BANKL, "Bank Key
         BNK01_BANKN           TYPE BUT0BK-BANKN, "Bank Account
         BNK01_KOINH           TYPE BUT0BK-KOINH, "Account holder

* BOI- CH01
         ADDR_TRANSPZONE       TYPE ADDR1_DATA-TRANSPZONE, "Transporation Zone
         VIEW_CUST_KUKLA       TYPE KNA1-KUKLA,            "Customer Classification
* EOI- CH01

         VIEW_COMP_BUKRS       TYPE  KNB1-BUKRS,    "Company code             "60
         VIEW_COMP_AKONT       TYPE  KNB1-AKONT,    "Reconcile Acc.
         VIEW_COMP_ZUAWA       TYPE  KNB1-ZUAWA,    "Sort Key
         VIEW_COMP_ALTKN       TYPE  KNB1-ALTKN,    "Previous account no. jj
         VIEW_COMP_ZTERM       TYPE  KNB1-ZTERM,    "Payment Term
         VIEW_COMP_XZVER       TYPE  KNB1-XZVER,    "Rec Pay History
         VIEW_COMP_XVERR       TYPE  KNB1-XVERR,    "Clearing with vendor
         WT1_WITHT             TYPE  KNBW-WITHT,     "Withhold Type
         WT1_WITHCD            TYPE  KNBW-WT_WITHCD, "Withhold Code
         WT1_AGENT             TYPE  KNBW-WT_AGENT,  "Agent
         WT1_AGTDF             TYPE  KNBW-WT_AGTDF,  "From date               "70
         WT1_AGTDT             TYPE  KNBW-WT_AGTDT,  "To date
         WT1_QSREC             TYPE  WT_QSREC,       "Withhold form      "CH01+
         WT2_WITHT             TYPE  KNBW-WITHT,     "Withhold Type
         WT2_WITHCD            TYPE  KNBW-WT_WITHCD, "Withhold Code
         WT2_AGENT             TYPE  KNBW-WT_AGENT,  "Agent
         WT2_AGTDF             TYPE  KNBW-WT_AGTDF,  "From date
         WT2_AGTDT             TYPE  KNBW-WT_AGTDT,  "To date
         WT2_QSREC             TYPE  WT_QSREC,       "Withhold form      "CH01+
* BOD - CH01
*         WT3_WITHT             TYPE  KNBW-WITHT,     "Withhold Type
*         WT3_WITHCD            TYPE  KNBW-WT_WITHCD, "Withhold Code
*         WT3_AGENT             TYPE  KNBW-WT_AGENT,  "Agent
*         WT3_AGTDF             TYPE  KNBW-WT_AGTDF,  "From date
*         WT3_AGTDT             TYPE  KNBW-WT_AGTDT,  "To date
*         WT4_WITHT             TYPE  KNBW-WITHT,     "Withhold Type
*         WT4_WITHCD            TYPE  KNBW-WT_WITHCD, "Withhold Code
*         WT4_AGENT             TYPE  KNBW-WT_AGENT,  "Agent
*         WT4_AGTDF             TYPE  KNBW-WT_AGTDF,  "From date
*         WT4_AGTDT             TYPE  KNBW-WT_AGTDT,  "To date
* EOD - CH01
*         view_comp_active   TYPE  flag,  "View company active
         VIEW_COMP_J_1TPBUPL   TYPE  FITHA_PBUPL_D-J_1TPBUPL,     "Branch
         VIEW_COMP_DESCRIPTION TYPE  FITHA_PBUPL_D_T-DESCRIPTION, "Branch Desc.  "80

* BOI - CH01
         VIEW_COMP_FDGRV       TYPE KNB1-FDGRV,   "Planning Group
         VIEW_COMP_MAHNA       TYPE KNB5-MAHNA,   "Dunning Procedure
         VIEW_COMP_BUSAB       TYPE KNB1-BUSAB,   "Account Clerk
         VIEW_COMP_KVERM       TYPE KNB1-KVERM,   "Account Memo
         VIEW_COMP_TLFXS       TYPE KNB1-TLFXS,   "Acctg clerk's fax
         VIEW_COMP_INTAD       TYPE KNB1-INTAD,   "Clerk Internet Add.
         VIEW_COMP_XAUSZ       TYPE KNB1-XAUSZ,   "Account Statement
         VIEW_COMP_SPERR       TYPE KNB1-SPERR,   "Posting Block All Comp

         ADDR_EMAIL1           TYPE ADR6-SMTP_ADDR,
         ADDR_EMAIL2           TYPE ADR6-SMTP_ADDR,
         ADDR_EMAIL3           TYPE ADR6-SMTP_ADDR,
         ADDR_EMAIL4           TYPE ADR6-SMTP_ADDR,
         ADDR_EMAIL5           TYPE ADR6-SMTP_ADDR,
         ADDR_EMAIL6           TYPE ADR6-SMTP_ADDR,
         ADDR_EMAIL7           TYPE ADR6-SMTP_ADDR,
         ADDR_REGION           TYPE ADDR1_DATA-REGION,
         ADDR_HOME_CITY        TYPE ADDR1_DATA-HOME_CITY,
* EOI - CH01

*         BNK02_BVTYP           TYPE KNBK-BVTYP,   "Partner bank type
*         BNK02_BANKS           TYPE BUT0BK-BANKS, "Bank Country
*         BNK02_BANKL           TYPE BUT0BK-BANKL, "Bank Key
*         BNK02_BANKN           TYPE BUT0BK-BANKN, "Bank Account
*         BNK02_KOINH           TYPE BUT0BK-KOINH, "Account holder
*
*         BNK03_BVTYP           TYPE KNBK-BVTYP,   "Partner bank type
*         BNK03_BANKS           TYPE BUT0BK-BANKS, "Bank Country
*         BNK03_BANKL           TYPE BUT0BK-BANKL, "Bank Key
*         BNK03_BANKN           TYPE BUT0BK-BANKN, "Bank Account
*         BNK03_KOINH           TYPE BUT0BK-KOINH, "Account holder
*
*         BNK04_BVTYP           TYPE KNBK-BVTYP,   "Partner bank type
*         BNK04_BANKS           TYPE BUT0BK-BANKS, "Bank Country
*         BNK04_BANKL           TYPE BUT0BK-BANKL, "Bank Key
*         BNK04_BANKN           TYPE BUT0BK-BANKN, "Bank Account
*         BNK04_KOINH           TYPE BUT0BK-KOINH, "Account holder
*
*         BNK05_BVTYP           TYPE KNBK-BVTYP,   "Partner bank type
*         BNK05_BANKS           TYPE BUT0BK-BANKS, "Bank Country
*         BNK05_BANKL           TYPE BUT0BK-BANKL, "Bank Key
*         BNK05_BANKN           TYPE BUT0BK-BANKN, "Bank Account
*         BNK05_KOINH           TYPE BUT0BK-KOINH, "Account holder
*
*         BNK06_BVTYP           TYPE KNBK-BVTYP,   "Partner bank type
*         BNK06_BANKS           TYPE BUT0BK-BANKS, "Bank Country
*         BNK06_BANKL           TYPE BUT0BK-BANKL, "Bank Key
*         BNK06_BANKN           TYPE BUT0BK-BANKN, "Bank Account
*         BNK06_KOINH           TYPE BUT0BK-KOINH, "Account holder
       END OF GTY_TEMPLATE21.


TYPES: BEGIN OF GTY_TEMPLATE22 ##NEEDED,
         ALTKN          TYPE  KNB1-ALTKN,      "Previous account no.
         PARTNER        TYPE  BUT000-PARTNER,  "Business Partner
         RLTYP          TYPE  BUT100-RLTYP,    "Business Partner Role
         DEFAULT_BRANCH TYPE  FITHA_PBUPL_D-DEFAULT_BRANCH, "Def. Branch
         J_1TPBUPL      TYPE  FITHA_PBUPL_D-J_1TPBUPL,     "Branch
         DESCRIPTION    TYPE  FITHA_PBUPL_D_T-DESCRIPTION, "Branch Desc.
       END OF GTY_TEMPLATE22.

TYPES: BEGIN OF GTY_TEMPLATE23 ##NEEDED,
         ALTKN              TYPE  KNB1-ALTKN,  "Previous account no.
         PARTNER            TYPE  BUT000-PARTNER, "Business Partner
         RLTYP              TYPE  BUT100-RLTYP, "Business Partner Role
         ADDR_ADR_KIND      TYPE  BUT021_FS-ADR_KIND, "Address type
         ADDR_ADEXT         TYPE  BUT020-ADEXT,
         ADDR_REMARK        TYPE  ADRCT-REMARK,     "Comment
         ADDR_VALID_FROM    TYPE  BUT020-ADDR_VALID_FROM,
         ADDR_VALID_TO      TYPE  BUT020-ADDR_VALID_TO,
         ADDR_TITLE         TYPE  BUT000-TITLE,
         ADDR_NAME1         TYPE  ADRC-NAME1,
         ADDR_NAME2         TYPE  ADRC-NAME2,
         ADDR_NAME3         TYPE  ADRC-NAME3,
         ADDR_NAME4         TYPE  ADRC-NAME4,
         ADDR_SORT1         TYPE  ADRC-SORT1,
         ADDR_SORT2         TYPE  ADRC-SORT2,
         ADDR_NAME_CO       TYPE  ADRC-NAME_CO,
         ADDR_STR_SUPPL1    TYPE  ADRC-STR_SUPPL1,  "Street4
         ADDR_STR_SUPPL2    TYPE  ADRC-STR_SUPPL2,
         ADDR_STREET        TYPE  ADRC-STREET,      "HouseNo./Street
         ADDR_STR_SUPPL3    TYPE  ADRC-STR_SUPPL3,  "Street4
         ADDR_LOCATION      TYPE  ADRC-LOCATION,    "Street5
         ADDR_CITY2         TYPE  ADRC-CITY2,       "District
         ADDR_CITY1         TYPE  ADRC-CITY1,       "City
         ADDR_POST_CODE1    TYPE  ADRC-POST_CODE1,  "Postal Code
         ADDR_COUNTRY       TYPE  ADRC-COUNTRY,     "Country
         ADDR_LANGU         TYPE  ADRC-LANGU,       "Language
         ADDR_TELNO1        TYPE  ADR2-TEL_NUMBER,
         ADDR_TELEXT1       TYPE  ADR2-TEL_EXTENS,
         ADDR_MOBILE        TYPE  ADR2-TEL_NUMBER,
         ADDR_FAXNO         TYPE  ADR3-FAX_NUMBER,
         ADDR_FAXEXT        TYPE  ADR3-FAX_EXTENS,
         ADDR_EMAIL1        TYPE  ADR6-SMTP_ADDR,
         ADDR_DEFLT_COMM    TYPE  ADRC-DEFLT_COMM,
         ADDRINT_TITLE      TYPE  BUT000-TITLE,
         ADDRINT_NAME1      TYPE  ADRC-NAME1,
         ADDRINT_NAME2      TYPE  ADRC-NAME2,
         ADDRINT_NAME3      TYPE  ADRC-NAME3,
         ADDRINT_NAME4      TYPE  ADRC-NAME4,
         ADDRINT_SORT1      TYPE  ADRC-SORT1,
         ADDRINT_SORT2      TYPE  ADRC-SORT2,
         ADDRINT_NAME_CO    TYPE  ADRC-NAME_CO,
         ADDRINT_STR_SUPPL1 TYPE  ADRC-STR_SUPPL1,  "Street4
         ADDRINT_STR_SUPPL2 TYPE  ADRC-STR_SUPPL2,
         ADDRINT_STREET     TYPE  ADRC-STREET,      "HouseNo./Street
         ADDRINT_STR_SUPPL3 TYPE  ADRC-STR_SUPPL3,  "Street4
         ADDRINT_LOCATION   TYPE  ADRC-LOCATION,    "Street5
         ADDRINT_CITY2      TYPE  ADRC-CITY2,       "District
         ADDRINT_CITY1      TYPE  ADRC-CITY1,       "City
         ADDRINT_POST_CODE1 TYPE  ADRC-POST_CODE1,  "Postal Code
         ADDRINT_COUNTRY    TYPE  ADRC-COUNTRY,     "Country
         ADDRINT_LANGU      TYPE  ADRC-LANGU,       "Language
         ADDRINT_REMARK     TYPE  ADRCT-REMARK,     "Comment
       END OF GTY_TEMPLATE23.


TYPES: BEGIN OF GTY_TEMPLATE24 ##NEEDED,
         ALTKN           TYPE KNB1-ALTKN,      "Previous account no.
         PARTNER         TYPE BUT000-PARTNER,  "Business Partner
         RLTYP           TYPE BUT100-RLTYP,    "Business Partner Role
         ADDR_ADR_KIND   TYPE BUT021_FS-ADR_KIND, "Address Type
         ADDR_ADEXT      TYPE BUT020-ADEXT,  "External Address No.
         ADDR_REMARK     TYPE ADRCT-REMARK,     "Address Note
         ADDR_VALID_FROM TYPE ADR6-VALID_FROM,  "Valid From
         ADDR_VALID_TO   TYPE ADR6-VALID_TO,    "Valid To
         ADDR_EMAIL      TYPE ADR6-SMTP_ADDR,  "Email
       END OF GTY_TEMPLATE24.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TCODE        TYPE  SY-TCODE    VALUE 'ZARC001',

  GC_TRUE         TYPE  CHAR1       VALUE 'X',

* BP Role for Template1 for vendor
  GC_ROLE1_11     TYPE  BUT100-RLTYP VALUE '000000',
  GC_ROLE1_12     TYPE  BUT100-RLTYP VALUE 'FLVN00',
  GC_ROLE1_13     TYPE  BUT100-RLTYP VALUE 'FLVN01',

* BP Role for Template2 for vendor
  GC_ROLE2_11     TYPE  BUT100-RLTYP VALUE '000000' ##NEEDED,
  GC_ROLE2_21     TYPE  BUT100-RLTYP VALUE 'FLVN00' ##NEEDED,
  GC_ROLE2_31     TYPE  BUT100-RLTYP VALUE 'BUP001' ##NEEDED,

* BP Role for Template for customer
  GC_ROLE0_0      TYPE  BUT100-RLTYP VALUE '000000',
*  GC_ROLE2_1      TYPE  BUT100-RLTYP VALUE 'TR0600',  "CH01-
  GC_ROLE2_1      TYPE  BUT100-RLTYP VALUE 'FLCU01',   "CH01+
  GC_ROLE2_2      TYPE  BUT100-RLTYP VALUE 'FLCU00',
  GC_ROLE2_3      TYPE  BUT100-RLTYP VALUE 'CHM003',   "CH01+ "Contact Person

* BP Type
  GC_TYPE_PERSON  TYPE  BUT000-TYPE VALUE '1',
  GC_TYPE_ORG     TYPE  BUT000-TYPE VALUE '2',

* Country / Region
  GC_COUNTRY_TH   TYPE ADRC-COUNTRY VALUE 'TH',      "CH01+

* Processing Mode
  GC_MODE_CREATE  TYPE  CHAR1  VALUE '1',
  GC_MODE_CHANGE  TYPE  CHAR1  VALUE '2',

* Gender
  GC_GENDR_MALE   TYPE  CHAR1  VALUE '2',
  GC_GENDR_FEMALE TYPE  CHAR1  VALUE '1',
  GC_GENDR_UNKNW  TYPE  CHAR1  VALUE '0',

*  gc_limit_rule_1 TYPE  gty_main4-limit_rule  VALUE 'B2B-NEW',

* BP type
  GC_BPKIND_Z002  TYPE  BUT000-BPKIND  VALUE 'Z002' ##NEEDED,

* BU type
  GC_BU_ORG       TYPE  BUT000-TYPE VALUE '2' ##NEEDED, "Organization
  GC_PERSON       TYPE  BUT000-TYPE VALUE '1' ##NEEDED, "Person (HR)

* Group Which has Customer code different from Partner
  GC_BU_GROUP_1   TYPE  BUT000-BU_GROUP VALUE 'Z101' ##NEEDED,

* Splitter used in Raw data
  GC_SPLIT        TYPE  CHAR1
                          VALUE CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB,

* Group Which has Vendor code different from Partner
  GC_BU_GROUP_1_V TYPE  BUT000-BU_GROUP VALUE 'BP31',
  GC_BU_GROUP_2_V TYPE  BUT000-BU_GROUP VALUE 'BP32',


* Field value translated as not filled
  GC_DUMMY_FLD    TYPE  CHAR1       VALUE ' ',
  GC_BLANK_FILL   TYPE  CHAR5       VALUE '" "'.
*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
*DATA: gt_config TYPE SORTED TABLE OF zttec_conversion
*                   WITH UNIQUE KEY seqno.

DATA: GT_RESULT11 TYPE GTTY_RESULT11 ##NEEDED.
DATA: GT_RESULT12 TYPE GTTY_RESULT12 ##NEEDED.
DATA: GT_RESULT13 TYPE GTTY_RESULT13 ##NEEDED.
DATA: GT_RESULT21 TYPE GTTY_RESULT21 ##NEEDED.
DATA: GT_RESULT22 TYPE GTTY_RESULT22 ##NEEDED.
DATA: GT_RESULT23 TYPE GTTY_RESULT23 ##NEEDED.
DATA: GT_RESULT24 TYPE GTTY_RESULT24 ##NEEDED.

DATA: GT_LOG_PARTNER TYPE TABLE OF GTYP_LOG_PARTNER ##NEEDED,
      GT_LOG_BANK    TYPE TABLE OF GTYP_LOG_BANK ##NEEDED,
      GT_LOG_COMP    TYPE TABLE OF GTYP_LOG_COMP ##NEEDED,
      GT_LOG_PAYEE   TYPE TABLE OF GTYP_LOG_PAYEE ##NEEDED,
      GT_LOG_EXTEND  TYPE TABLE OF GTYP_LOG_EXTEND ##NEEDED.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*
DATA:
*  gs_config TYPE  zttec_conversion,
  GS_SUM    TYPE  GTY_SUM ##NEEDED.

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*
DATA:
  GV_MODE    TYPE  C ##NEEDED,            "Processing Mode (1:Create,2:Change)
  GV_ONETIME TYPE FLAG ##NEEDED.

* Context to be able to update HR Business Partner
DATA:
  GV_ZZCONTEXT  TYPE  CHAR10 ##NEEDED.

DATA:
  GS_DEFAULT  TYPE  GTY_DEFAULT ##NEEDED.

DATA GV_VALIDFROM TYPE SY-DATUM ##NEEDED.
*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*
##NEEDED
DATA: GR_RLTYP TYPE RANGE OF BUT100-RLTYP .
*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* GENC VARIABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  GC_STRUCTURE_11     TYPE  TABNAME  VALUE 'ZSDSCAS010',
  GC_HEADER_HEIGHT_11 TYPE  I                VALUE 2,
  GC_ALV_HEIGHT_11    TYPE  I                VALUE 72,

  GC_STRUCTURE_12     TYPE  TABNAME  VALUE 'ZSDSCAS011',
  GC_HEADER_HEIGHT_12 TYPE  I                VALUE 2,
  GC_ALV_HEIGHT_12    TYPE  I                VALUE 72,

  GC_STRUCTURE_13     TYPE  TABNAME  VALUE 'ZSDSCAS018',
  GC_HEADER_HEIGHT_13 TYPE  I                VALUE 2,
  GC_ALV_HEIGHT_13    TYPE  I                VALUE 72,

  GC_STRUCTURE_21     TYPE  TABNAME  VALUE 'ZSDSCAS012',
  GC_HEADER_HEIGHT_21 TYPE  I                VALUE 2,
  GC_ALV_HEIGHT_21    TYPE  I                VALUE 72,

  GC_STRUCTURE_22     TYPE  TABNAME  VALUE 'ZSDSCAS013',
  GC_HEADER_HEIGHT_22 TYPE  I                VALUE 2,
  GC_ALV_HEIGHT_22    TYPE  I                VALUE 72,

  GC_STRUCTURE_23     TYPE  TABNAME  VALUE 'ZSDSCAS014',
  GC_HEADER_HEIGHT_23 TYPE  I                VALUE 2,
  GC_ALV_HEIGHT_23    TYPE  I                VALUE 72,

  GC_STRUCTURE_24     TYPE  TABNAME  VALUE 'ZSDSCAS015',
  GC_HEADER_HEIGHT_24 TYPE  I                VALUE 2,
  GC_ALV_HEIGHT_24    TYPE  I                VALUE 72.


*----------------------------------------------------------------------*
* MACROS
*----------------------------------------------------------------------*
DEFINE MC_SHOW_PROGRESS.
  IF sy-batch IS INITIAL.
*   Show progress online
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = &1 ##NUMBER_OK
        text       = &2.
  ELSE.
*   Show message step in background
    MESSAGE i000(38) WITH &2 space space space.
  ENDIF.
END-OF-DEFINITION.


*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
* Text-s00: Mode
SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME TITLE TEXT-S00.
  PARAMETERS:
    RB_CRE TYPE  FLAG RADIOBUTTON GROUP G0 DEFAULT 'X',
    RB_CHG TYPE  FLAG RADIOBUTTON GROUP G0 ##NEEDED.
SELECTION-SCREEN END OF BLOCK B0.

* Text-s01: Vendor/Customer

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01 .
  PARAMETERS:
    RB_TYPE1 TYPE  FLAG RADIOBUTTON GROUP G2 DEFAULT 'X'
                        USER-COMMAND TPL.
  SELECTION-SCREEN BEGIN OF BLOCK B5 WITH FRAME  NO INTERVALS.
* Text-s02: BP master (General veiw, Comp code View)
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 7(52) TEXT-S02 FOR FIELD RB_TMP11 ##SEL_WRONG
        MODIF ID TP1 .
      SELECTION-SCREEN POSITION 4.
      PARAMETERS: RB_TMP11   TYPE  FLAG RADIOBUTTON GROUP G3 DEFAULT 'X'
                                   MODIF ID TP1 USER-COMMAND U21.
    SELECTION-SCREEN END OF LINE.
* Text-s03: BP Role: FLVN01 (Purchasing View)
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 7(52) TEXT-S03 FOR FIELD RB_TMP12 ##SEL_WRONG
        MODIF ID TP1.
      SELECTION-SCREEN POSITION 4.
      PARAMETERS:   RB_TMP12  TYPE  FLAG RADIOBUTTON GROUP G3
                                     MODIF ID TP1.
    SELECTION-SCREEN END OF LINE.
* Text-s12:
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 7(52) TEXT-S12 FOR FIELD RB_TMP13 ##SEL_WRONG
        MODIF ID TP1.
      SELECTION-SCREEN POSITION 4.
      PARAMETERS:   RB_TMP13  TYPE  FLAG RADIOBUTTON GROUP G3
                                     MODIF ID TP1.
    SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK B5.

  PARAMETERS:
    RB_TYPE2 TYPE  FLAG RADIOBUTTON GROUP G2 ##NEEDED.
  SELECTION-SCREEN BEGIN OF BLOCK B6 WITH FRAME NO INTERVALS.
* BP Master (General view, Comp code View)
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 7(52) TEXT-S04 FOR FIELD RB_TMP21 ##SEL_WRONG
        MODIF ID TP2.
      SELECTION-SCREEN POSITION 4.
      PARAMETERS: RB_TMP21   TYPE  FLAG RADIOBUTTON GROUP G4 DEFAULT 'X'
                                   MODIF ID TP2 USER-COMMAND U22.
    SELECTION-SCREEN END OF LINE.
* Branch code
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 7(52) TEXT-S05 FOR FIELD RB_TMP22 ##SEL_WRONG
        MODIF ID TP2.
      SELECTION-SCREEN POSITION 4.
      PARAMETERS:   RB_TMP22   TYPE  FLAG RADIOBUTTON GROUP G4
                                     MODIF ID TP2 .
    SELECTION-SCREEN END OF LINE.
** Address Type for Tax Address data
*    SELECTION-SCREEN BEGIN OF LINE.
*      SELECTION-SCREEN COMMENT 7(52) TEXT-S06 FOR FIELD RB_TMP23  ##SEL_WRONG
*        MODIF ID TP2.
*      SELECTION-SCREEN POSITION 4.
*      PARAMETERS:  RB_TMP23   TYPE  FLAG   RADIOBUTTON GROUP G4
*                                    MODIF ID TP2 .
*    SELECTION-SCREEN END OF LINE.
*
** E-mail Address data
*    SELECTION-SCREEN BEGIN OF LINE.
*      SELECTION-SCREEN COMMENT 7(52) TEXT-S07 FOR FIELD RB_TMP24 ##SEL_WRONG
*        MODIF ID TP2.
*      SELECTION-SCREEN POSITION 4.
*      PARAMETERS:  RB_TMP24   TYPE  FLAG RADIOBUTTON GROUP G4
*                                    MODIF ID TP2.
*    SELECTION-SCREEN END OF LINE.
    PARAMETERS: RB_TMP23 TYPE FLAG NO-DISPLAY,
                RB_TMP24 TYPE  FLAG NO-DISPLAY.
  SELECTION-SCREEN END OF BLOCK B6.

SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B7 WITH FRAME TITLE TEXT-S09 .
  PARAMETERS:
  P_FILE TYPE  STRING LOWER CASE.

  SELECTION-SCREEN BEGIN OF LINE.
*     Text-s03: Start Row
    SELECTION-SCREEN COMMENT 1(29) TEXT-S08 FOR FIELD P_BEGROW ##SEL_WRONG
      MODIF ID EXC.
    PARAMETERS:
      P_BEGROW TYPE I DEFAULT 13 MODIF ID EXC.
*     Text-s04: Start Column
    SELECTION-SCREEN COMMENT 50(15) TEXT-S09 FOR FIELD P_BEGCOL ##SEL_WRONG
      MODIF ID EXC.
    PARAMETERS:
      P_BEGCOL TYPE I DEFAULT 3 MODIF ID EXC.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
*     Text-s05: End Row
    SELECTION-SCREEN COMMENT 1(29) TEXT-S10 FOR FIELD P_ENDROW ##SEL_WRONG
      MODIF ID EXC.
    PARAMETERS:
      P_ENDROW TYPE I DEFAULT 9999 MODIF ID EXC.
*     Text-s06: End Column
    SELECTION-SCREEN COMMENT 50(15) TEXT-S11 FOR FIELD P_ENDCOL ##SEL_WRONG
      MODIF ID EXC.
    PARAMETERS:
      P_ENDCOL TYPE I DEFAULT 91 MODIF ID EXC.    SELECTION-SCREEN END OF LINE.


  PARAMETERS:
    CB_TEST TYPE  FLAG AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK B7.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.
*  PERFORM F_INITIAL_SELECTION_SCREEN.
*  PERFORM F_GET_GENC.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
* List input File
  PERFORM F_LIST_IFILE CHANGING P_FILE.


AT SELECTION-SCREEN OUTPUT.
*  PERFORM f_template_selection USING space.
*  PERFORM F_TEMPLATE_SELECTION USING GC_TRUE.

AT SELECTION-SCREEN.
  IF  RB_TYPE1 IS NOT INITIAL . "Vendor

    IF RB_CRE IS NOT INITIAL .
      GV_MODE = GC_MODE_CREATE.
    ELSE.
      GV_MODE = GC_MODE_CHANGE.
    ENDIF.

  ELSE.
***** Customer *****
    CASE GC_TRUE.
      WHEN RB_TMP21.
        IF RB_CRE IS NOT INITIAL .
          GV_MODE = GC_MODE_CREATE.
        ELSE.
          GV_MODE = GC_MODE_CHANGE.
        ENDIF.

      WHEN RB_TMP22.
        GV_MODE = GC_MODE_CHANGE.

      WHEN RB_TMP23.
        GV_MODE = GC_MODE_CHANGE.

      WHEN RB_TMP24.
        GV_MODE = GC_MODE_CHANGE.
    ENDCASE.

  ENDIF.


*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  CLEAR: GT_LOG_COMP[], GT_LOG_PAYEE[],
         GT_LOG_PARTNER[], GT_LOG_BANK[], GT_LOG_EXTEND[].

  IF  RB_TYPE1 IS NOT INITIAL .
****** Vendor *****
    CASE GC_TRUE.

      WHEN RB_TMP11.
*       Processing data
        PERFORM F_PROCESS_DATA_TMP11 CHANGING GT_RESULT11
                                              GS_SUM.
        IF GT_RESULT11 IS INITIAL.
*         Message: No data found.
          MESSAGE S829(63).
          RETURN.
        ENDIF.
      WHEN RB_TMP12.
*       Processing data
        PERFORM F_PROCESS_DATA_TMP12 CHANGING GT_RESULT12
                                              GS_SUM.
        IF GT_RESULT12 IS INITIAL.
*         Message: No data found.
          MESSAGE S829(63).
          RETURN.
        ENDIF.

*   ------------------
*   Relationship
*   ------------------
      WHEN RB_TMP13.
*     Processing data
        PERFORM F_PROCESS_DATA_TMP13 CHANGING GT_RESULT13
                                              GS_SUM.
        IF GT_RESULT13 IS INITIAL.
*         Message: No data found.
          MESSAGE S829(63).
          RETURN.
        ENDIF.
    ENDCASE.

  ELSE.

***** Customer *****
    CASE GC_TRUE.
*   ------------------
*   General + Company
*   ------------------
      WHEN RB_TMP21.

        PERFORM F_PROCESS_DATA_TMP21 CHANGING GT_RESULT21
                                              GS_SUM.
        IF GT_RESULT21 IS INITIAL.
*       Message: No data found.
          MESSAGE S829(63).
          RETURN.
        ENDIF.
*   ------------------
*   Branch
*   ------------------
      WHEN RB_TMP22.
*       Processing data
        PERFORM F_PROCESS_DATA_TMP22 CHANGING GT_RESULT22
                                              GS_SUM.
        IF GT_RESULT22 IS INITIAL.
*         Message: No data found.
          MESSAGE S829(63).
          RETURN.
        ENDIF.

*   ------------------
*   Tax address
*   ------------------
      WHEN RB_TMP23.
*       Processing data
        PERFORM F_PROCESS_DATA_TMP23 CHANGING GT_RESULT23
                                              GS_SUM.
        IF GT_RESULT23 IS INITIAL.
*         Message: No data found.
          MESSAGE S829(63).
          RETURN.
        ENDIF.

*   ------------------
*   Email
*   ------------------
      WHEN RB_TMP24.

*       Processing data
        PERFORM F_PROCESS_DATA_TMP24 CHANGING GT_RESULT24
                                              GS_SUM.
        IF GT_RESULT24 IS INITIAL.
*         Message: No data found.
          MESSAGE S829(63).
          RETURN.
        ENDIF.

    ENDCASE.



*   ------------------
*   Branch
*   ------------------

  ENDIF.
*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  IF  RB_TYPE1 IS NOT INITIAL .
    CASE GC_TRUE .
      WHEN RB_TMP11.
        PERFORM F_DISPLAY_RESULT11 USING GT_RESULT11.
      WHEN RB_TMP12.
        PERFORM F_DISPLAY_RESULT12 USING GT_RESULT12.
      WHEN RB_TMP13.
        PERFORM F_DISPLAY_RESULT13 USING GT_RESULT13.
    ENDCASE.
  ELSE.

    CASE GC_TRUE.
*   ------------------
*   General + Company
*   ------------------
      WHEN RB_TMP21.
        PERFORM F_DISPLAY_RESULT21 USING GT_RESULT21.
      WHEN RB_TMP22.
        PERFORM F_DISPLAY_RESULT22 USING GT_RESULT22.
      WHEN RB_TMP23.
        PERFORM F_DISPLAY_RESULT23 USING GT_RESULT23.
      WHEN RB_TMP24.
        PERFORM F_DISPLAY_RESULT24 USING GT_RESULT24.
    ENDCASE.
  ENDIF.
*----------------------------------------------------------------------*
* SUBROUTINE INCLUDES
*----------------------------------------------------------------------*
  INCLUDE ZSDSCAI9990 ##INCL_OK. "ZSDSCARXXX0_TEMPLATE
INCLUDE ZSDSCAR0090_F01.
*  INCLUDE ZSDSCAR0050_F01.
INCLUDE ZSDSCAR0090_F02.
*  INCLUDE ZSDSCAR0050_F02.
INCLUDE ZSDSCAR0090_F03.
*  INCLUDE ZSDSCAR0050_F03.
INCLUDE ZSDSCAR0090_F04.
*  INCLUDE ZSDSCAR0050_F04.


*----------------------------------------------------------------------*
* SUBROUTINES
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  Form F_AUTHORIZE_CHECK
*----------------------------------------------------------------------*
*  Check Authorization on t-code
*----------------------------------------------------------------------*
FORM F_AUTHORIZE_CHECK USING PV_TCODE TYPE SY-TCODE.

  AUTHORITY-CHECK OBJECT 'S_TCODE'     "Transaction Code Check
    ID 'TCD' FIELD PV_TCODE.

  IF SY-SUBRC <> 0.
*   Error You are not authorized to use transaction &
    MESSAGE S172(00) WITH PV_TCODE.
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.

INCLUDE ZSDSCAR0090_F05.
*INCLUDE ZSDSCAR0050_F05.

INCLUDE ZSDSCAR0090_F06.
*INCLUDE ZSDSCAR0050_F06.
