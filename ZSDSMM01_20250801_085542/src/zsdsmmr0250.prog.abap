*-----------------------------------------------------------------------
*  Program ID         : ZDSMMR0250
*  Creation Date      : 07.05.2024
*  Author             : Chanakarn T.(Eviden)
*  Add-on ID          : N/A
*  Description        : Program for read file and create PO
*  Purpose            : Upload outstanding PO
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*& Report ZDSMMR0250
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSDSMMR0250.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
* *****************************
* Types for File Processing
* *****************************
TYPES: TS_FIELD_NAME TYPE CHAR40.

TYPES: BEGIN OF TS_RAW,
         TLINE TYPE STRING,
       END OF TS_RAW.
TYPES: TT_RAW TYPE STANDARD TABLE OF TS_RAW.

TYPES: BEGIN OF TS_SUM,
         TOTAL TYPE I,
         SUCCS TYPE I,
         ERROR TYPE I,
       END OF TS_SUM.

TYPES: TT_STRING_TAB TYPE STANDARD TABLE OF STRING WITH DEFAULT KEY.

TYPES: BEGIN OF TS_TEMPLATE_TEXT,
         FORM_LINE          TYPE TT_STRING_TAB,
         HEADER_REMARK_LINE TYPE TT_STRING_TAB,
         INTERNAL_LINE      TYPE TT_STRING_TAB,
         PROJECT_LINE       TYPE TT_STRING_TAB,
         ATTENTION_LINE     TYPE TT_STRING_TAB,
         PERSON_LINE        TYPE TT_STRING_TAB,
         REMARK_LINE        TYPE TT_STRING_TAB,
         ITEM_LINE          TYPE TT_STRING_TAB,
         MEMO_LINE          TYPE TT_STRING_TAB,
         ADVANCE_LINE       TYPE TT_STRING_TAB,
         RETANTION_LINE     TYPE TT_STRING_TAB,
         WARRANTY_LINE      TYPE TT_STRING_TAB,
         PENALTY_LINE       TYPE TT_STRING_TAB,
         ENGINEER_LINE      TYPE TT_STRING_TAB,
       END OF TS_TEMPLATE_TEXT.

TYPES: BEGIN OF TS_RESULT,
         ROWNO TYPE I,
         MSGTY TYPE SYST_MSGTY,
         MSGTX TYPE ZSDS_DE_MSGTX.
         INCLUDE STRUCTURE ZSDSMMS028.
TYPES:
       END OF TS_RESULT.
TYPES: TT_RESULT TYPE STANDARD TABLE OF TS_RESULT.

TYPES: BEGIN OF TS_FILE_INFO,
         DIRECTORY TYPE STRING,
         FILENAME  TYPE STRING,
         FULLNAME  TYPE STRING,
       END OF TS_FILE_INFO.

TYPES: BEGIN OF TS_LOG,
*        Result
         MSGTY           TYPE TEXT1000,
         MSGTX           TYPE TEXT1000,
*        Header
         BSART           TYPE TEXT1000,  "PO Type
         EBELN           TYPE TEXT1000,  "PO No.
         LIFNR           TYPE TEXT1000,  "Vendor
         NAME1           TYPE TEXT1000,  "Vendor Name
         BEDAT           TYPE TEXT1000,  "Document Date
         EKORG           TYPE TEXT1000,  "Pur.Org.
         EKGRP           TYPE TEXT1000,  "Pur. Group
         BUKRS           TYPE TEXT1000,  "Company code
         ZTERM           TYPE TEXT1000,  "Payment Term
         WAERS           TYPE TEXT1000,  "Currency
         WKURS           TYPE TEXT1000,  "Exchange Rate
         KUFIX           TYPE TEXT1000,  "Fixed Exch. Rate
         INCO1           TYPE TEXT1000,  "Incoterm
         INCO2           TYPE TEXT1000,  "Inco. Location1
         IHREZ           TYPE TEXT1000,  "Your Reference
         UNSEZ           TYPE TEXT1000,  "Our Reference
         VERKF           TYPE TEXT1000,  "Sales Person
*        Header Condition
         HEADER_CON      TYPE TEXT1000,  "ZVAT (%)
*        Payment Processing
         RETTP           TYPE TEXT1000,  "Retention Indicator
*        Header Text
         FROM_LINE1      TYPE TEXT1000,  "From Line 1
         FROM_LINE2      TYPE TEXT1000,  "From Line 2
         FROM_LINE3      TYPE TEXT1000,  "From Line 3
         ZZREMARKS01     TYPE TEXT1000,  "Remarks L1
         ZZREMARKS02     TYPE TEXT1000,  "Remarks L2
         ZZREMARKS03     TYPE TEXT1000,  "Remarks L3
         ZZREMARKS04     TYPE TEXT1000,  "Remarks L4
         ZZREMARKS05     TYPE TEXT1000,  "Remarks L5
*        Header Customer Data
         ZZPRJTYP        TYPE TEXT1000,  "Project Code
         ZZDELIVMODE     TYPE TEXT1000,  "Delivery Mode
         ZZADDRESS       TYPE TEXT1000,  "IGOM Infomation
         ZZPRJTYP_DIL    TYPE TEXT1000,  "Project Type for DIL
         ZZ1_DIVCD_PO    TYPE TEXT1000,  "Division Code
         ZZ_JTEPA        TYPE TEXT1000,  "JTEPA flag
*        Purchase Order Item
         EBELP           TYPE TEXT1000,  "Item Number
         KNTTP           TYPE TEXT1000,  "Acct. Assignment Cat.
         PSTYP           TYPE TEXT1000,  "Item Cat.
         MATNR           TYPE TEXT1000,  "Material No.
         TXZ01           TYPE TEXT1000,  "Short Text
         MATKL           TYPE TEXT1000,  "Material Group
         EKPO_MENGE      TYPE TEXT1000,  "Open PO Quantity
         MEINS           TYPE TEXT1000,  "Order Unit
         EINDT           TYPE TEXT1000,  "Delivery Date
         NETPR           TYPE TEXT1000,  "Net Price
         BRTWR           TYPE TEXT1000,  "Gross Price
         PEINH           TYPE TEXT1000,  "Price Unit
         BPRME           TYPE TEXT1000,  "Order Price Unit
         BPUMN           TYPE TEXT1000,  "Conv. From Qty Order Unit
         BPUMZ           TYPE TEXT1000,  "Conv. To Order Price Unit
         WERKS           TYPE TEXT1000,  "Plant
         LGORT           TYPE TEXT1000,  "Storage Location
         AFNAM           TYPE TEXT1000,  "Requisitioner
         BEDNR           TYPE TEXT1000,  "Tracking Number
         MWSKZ           TYPE TEXT1000,  "Tax Code
         UEBTO           TYPE TEXT1000,  "% Over Tolenrance
         UNTTO           TYPE TEXT1000,  "% Under Tolenrance
         UEBTK           TYPE TEXT1000,  "Unlimited
         WEPOS           TYPE TEXT1000,  "Goods Receipt
         REPOS           TYPE TEXT1000,  "Invoice Receipt
         WEBRE           TYPE TEXT1000,  "GR-Based IV
         XERSY           TYPE TEXT1000,  "ERS
         INSMK           TYPE TEXT1000,  "Stock Type
         MHDRZ           TYPE TEXT1000,  "Rem. Shelf Life
         IPRKZ           TYPE TEXT1000,  "Period Ind.
         SSQSS           TYPE TEXT1000,  "QA Control Key
         BSTAE           TYPE TEXT1000,  "Confirmation Control Key
*        Account Assignment Data
         VRTKZ           TYPE TEXT1000,  "Distribution
         TWRKZ           TYPE TEXT1000,  "Partial Inv.
         ZEKKN           TYPE TEXT1000,  "Seq. No.
         EKKN_MENGE      TYPE TEXT1000,  "Open Quantity
         SAKTO           TYPE TEXT1000,  "G/L Acct.
         KOSTL           TYPE TEXT1000,  "Cost Center
         FISTL           TYPE TEXT1000,  "Fund Center
         ANLN1           TYPE TEXT1000,  "Asset No.
         ANLN2           TYPE TEXT1000,  "Sub No.
         FIPOS           TYPE TEXT1000,  "Commitment Item
         AUFNR           TYPE TEXT1000,  "Order No.
         PRCTR           TYPE TEXT1000,  "Profit Center
         PS_PSP_PNR      TYPE TEXT1000,  "WBS
         VBELN           TYPE TEXT1000,  "Sales Order
         VBELP           TYPE TEXT1000,  "Sales Order Item
*        Item Text
         INTERNAL_LINE1  TYPE TEXT1000,  "Internal Order Line 1
         INTERNAL_LINE2  TYPE TEXT1000,  "Internal Order Line 2
         INTERNAL_LINE3  TYPE TEXT1000,  "Internal Order Line 3
         PROJECT_LINE1   TYPE TEXT1000,  "Project Name Line 1
         PROJECT_LINE2   TYPE TEXT1000,  "Project Name Line 2
         PROJECT_LINE3   TYPE TEXT1000,  "Project Name Line 3
         ATTENTION_LINE1 TYPE TEXT1000,  "Attention Line 1
         ATTENTION_LINE2 TYPE TEXT1000,  "Attention Line 2
         ATTENTION_LINE3 TYPE TEXT1000,  "Attention Line 3
         PERSON_LINE1    TYPE TEXT1000,  "Person In Charge Line 1
         PERSON_LINE2    TYPE TEXT1000,  "Person In Charge Line 2
         PERSON_LINE3    TYPE TEXT1000,  "Person In Charge Line 3
         REMARK_LINE1    TYPE TEXT1000,  "Remarks Line 1
         REMARK_LINE2    TYPE TEXT1000,  "Remarks Line 2
         REMARK_LINE3    TYPE TEXT1000,  "Remarks Line 3
         ITEM_LINE1      TYPE TEXT1000,  "Item Text Line1
         ITEM_LINE2      TYPE TEXT1000,  "Item Text Line2
         ITEM_LINE3      TYPE TEXT1000,  "Item Text Line3
         MEMO_LINE1      TYPE TEXT1000,  "Memo Line1
         MEMO_LINE2      TYPE TEXT1000,  "Memo Line2
         MEMO_LINE3      TYPE TEXT1000,  "Memo Line3
         WARR_LINE1      TYPE TEXT1000,  "warranty Line1
         WARR_LINE2      TYPE TEXT1000,  "warranty Line2
         WARR_LINE3      TYPE TEXT1000,  "warranty Line3
         ENGINEER_LINE1  TYPE TEXT1000,  "Engineer Line1
         ENGINEER_LINE2  TYPE TEXT1000,  "Engineer Line2
         ENGINEER_LINE3  TYPE TEXT1000,  "Engineer Line3
*        Item Customer Data
         ZZ1_LOB_PO_PDI  TYPE TEXT1000,  "LOB
         ZZ1_DES_PT_C_PO TYPE TEXT1000,  "Dest. Port Code
       END OF TS_LOG.


*----------------------------------------------------------------------*
* Types for Outstanding PO
*----------------------------------------------------------------------*
TYPES:
  TS_POHEADER      TYPE BAPIMEPOHEADER,
  TS_POHEADERX     TYPE BAPIMEPOHEADERX,
  TS_EXTENSIONIN   TYPE BAPIPAREX, "Custom field
  TS_POITEM        TYPE BAPIMEPOITEM,
  TS_POITEMX       TYPE BAPIMEPOITEMX,
  TS_POSCHEDULE    TYPE BAPIMEPOSCHEDULE,
  TS_POSCHEDULEX   TYPE BAPIMEPOSCHEDULX,
  TS_POACCOUNT     TYPE BAPIMEPOACCOUNT,
  TS_POACCOUNTX    TYPE BAPIMEPOACCOUNTX,
  TS_POACCOUNTSEG  TYPE BAPIMEPOACCOUNTPROFITSEGMENT,
  TS_POCONDHEADER  TYPE BAPIMEPOCONDHEADER,
  TS_POCONDHEADERX TYPE BAPIMEPOCONDHEADERX,
  TS_POTEXTHEADER  TYPE BAPIMEPOTEXTHEADER,
  TS_POTEXTITEM    TYPE BAPIMEPOTEXT.

TYPES:
  TT_POITEM        TYPE STANDARD TABLE OF TS_POITEM WITH DEFAULT KEY,
  TT_POITEMX       TYPE STANDARD TABLE OF TS_POITEMX WITH DEFAULT KEY,
  TT_POSCHEDULE    TYPE STANDARD TABLE OF TS_POSCHEDULE WITH DEFAULT KEY,
  TT_POSCHEDULEX   TYPE STANDARD TABLE OF TS_POSCHEDULEX WITH DEFAULT KEY,
  TT_POACCOUNT     TYPE STANDARD TABLE OF TS_POACCOUNT,
  TT_POACCOUNTX    TYPE STANDARD TABLE OF TS_POACCOUNTX,
  TT_POCONDHEADER  TYPE STANDARD TABLE OF TS_POCONDHEADER,
  TT_POCONDHEADERX TYPE STANDARD TABLE OF TS_POCONDHEADERX,
  TT_EXTENSIONIN   TYPE STANDARD TABLE OF TS_EXTENSIONIN WITH DEFAULT KEY,
  TT_POTEXTHEADER  TYPE STANDARD TABLE OF TS_POTEXTHEADER WITH DEFAULT KEY,
  TT_POTEXTITEM    TYPE STANDARD TABLE OF TS_POTEXTITEM WITH DEFAULT KEY.

TYPES: BEGIN OF TS_TEXT,
         TDOBJECT TYPE TDOBJECT,
         TDNAME   TYPE TDOBNAME,
         TDID     TYPE TDID,
         TDSPRAS  TYPE SPRAS,
         TDFORMAT TYPE TDTEXTTYPE,
         TDLINE   TYPE TDLINE,
       END OF TS_TEXT.
TYPES: TT_TEXT TYPE STANDARD TABLE OF TS_TEXT
                      WITH DEFAULT KEY.

TYPES: TS_MSGTX  TYPE  BAPI_MSG.

TYPES: BEGIN OF TS_MESSG,
         PO_NO   TYPE EKKO-EBELN,
         PO_ITEM TYPE EKPO-EBELP,
         MSGTY   TYPE  SY-MSGTY,
         MSGID   TYPE  SY-MSGID,
         MSGNO   TYPE  SY-MSGNO,
         MSGTX   TYPE  TS_MSGTX,
       END OF TS_MESSG.
TYPES: TT_MESSG  TYPE  STANDARD TABLE OF TS_MESSG
                         WITH DEFAULT KEY.

TYPES: BEGIN OF TS_DATA,
         ROWNO         TYPE TS_RESULT-ROWNO,
         POHEADER      TYPE TS_POHEADER,
         POHEADERX     TYPE TS_POHEADERX,
         POITEM        TYPE TT_POITEM,
         POITEMX       TYPE TT_POITEMX,
         POSCHEDULE    TYPE TT_POSCHEDULE,
         POSCHEDULEX   TYPE TT_POSCHEDULEX,
         POACCOUNT     TYPE TS_POACCOUNT,
         POACCOUNTX    TYPE TS_POACCOUNTX,
         POACCOUNTSEG  TYPE TS_POACCOUNTSEG,
         POCONDHEADER  TYPE TS_POCONDHEADER,
         POCONDHEADERX TYPE TS_POCONDHEADERX,
         POTEXTHEADER  TYPE TT_POTEXTHEADER,
         POTEXTITEM    TYPE TT_POTEXTITEM,
         EXTENSIONIN   TYPE TT_EXTENSIONIN,
         LTEXT         TYPE TT_TEXT,
         MESSG         TYPE TT_MESSG,
       END OF TS_DATA.
TYPES: TT_DATA TYPE STANDARD TABLE OF TS_DATA.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE            TYPE  CHAR1       VALUE 'X',

* Text ID
  GC_OBJEC_HEADERTXT TYPE  TS_TEXT-TDOBJECT VALUE 'EKKO',
  GC_OBJEC_ITEMTXT   TYPE  TS_TEXT-TDOBJECT VALUE 'EKPO',

* Field value translated as not filled
  GC_DUMMY_FLD       TYPE  CHAR1       VALUE ' ',

* Splitter used in Raw data
  GC_SPLIT           TYPE  CHAR1
           VALUE CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.
*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA:
  GT_RESULT        TYPE  TT_RESULT.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*
DATA:
  GS_LOGFILE TYPE  TS_FILE_INFO,
  GS_SUM     TYPE  TS_SUM.
*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  GC_STRUCTURE_1     TYPE  TABNAME  VALUE 'ZSDSMMS028'.
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
* Text-s01: Upload Outstanding PO
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
  PARAMETERS:
  P_LFILE TYPE STRING LOWER CASE MODIF ID LOC.

  SELECTION-SCREEN BEGIN OF LINE.
*    Text-s02: Start Row.
    SELECTION-SCREEN COMMENT 1(29) TEXT-S02 FOR FIELD P_BEGROW.
    PARAMETERS:
      P_BEGROW TYPE I DEFAULT 13 MODIF ID LOC.
*     Text-s03: Start Column
    SELECTION-SCREEN COMMENT 50(15) TEXT-S03 FOR FIELD P_BEGCOL.
    PARAMETERS:
      P_BEGCOL TYPE I DEFAULT 1 MODIF ID LOC.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
*     Text-s04: End Row
    SELECTION-SCREEN COMMENT 1(29) TEXT-S04 FOR FIELD P_ENDROW.
    PARAMETERS:
      P_ENDROW TYPE I DEFAULT 9999 MODIF ID LOC.
*     Text-s05: End Column
    SELECTION-SCREEN COMMENT 50(15) TEXT-S05 FOR FIELD P_ENDCOL.
    PARAMETERS:
      P_ENDCOL TYPE I DEFAULT 166 MODIF ID LOC.
  SELECTION-SCREEN END OF LINE.
  PARAMETERS:
    P_TEST   TYPE FLAG AS CHECKBOX DEFAULT 'X',
    P_LOG    TYPE FLAG AS CHECKBOX DEFAULT 'X',
    P_LOGFIL TYPE STRING LOWER CASE DEFAULT 'C:\Temp\'.
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_LFILE.
* List Local input File
  PERFORM F_LIST_IFILE CHANGING P_LFILE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_LOGFIL.
  PERFORM F_GET_FOLDER_NAME CHANGING P_LOGFIL.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Processing data
  PERFORM F_PROCESS_DATA CHANGING GT_RESULT
                                  GS_SUM.

  IF GT_RESULT IS INITIAL.
*   Message: No data found.
    MESSAGE S001(ZSDSCA01).
    RETURN.
  ENDIF.

*----------------------------------------------------------------------*
*  Form F_LIST_IFILE
*----------------------------------------------------------------------*
*  Popup for Input file selection
*----------------------------------------------------------------------*
FORM F_LIST_IFILE  CHANGING CF_FILENAME  TYPE  STRING.

  DATA:
      LT_FILE     TYPE  FILETABLE.

  DATA:
    LF_RC     TYPE  I,
    LF_ACTION TYPE  I.

  FIELD-SYMBOLS:
  <L_FILE>  TYPE  FILE_TABLE.

* List Local File
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      DEFAULT_EXTENSION       = '*'
*     File Filter format is '<Text>|<Filtering>;<Text>|<Filtering>'
      FILE_FILTER             = 'Excel File|*.XLSX;*.XLS' ##NO_TEXT
      MULTISELECTION          = SPACE
    CHANGING
      FILE_TABLE              = LT_FILE
      RC                      = LF_RC
      USER_ACTION             = LF_ACTION
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    RETURN.
  ENDIF.

* Read Selected
  IF NOT ( LF_ACTION IS INITIAL AND
           LF_RC     EQ 1 ).
    RETURN.
  ENDIF.

  READ TABLE LT_FILE ASSIGNING <L_FILE>
                     INDEX 1.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Assign Output
  CF_FILENAME = <L_FILE>-FILENAME.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_FOLDER_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_GET_FOLDER_NAME CHANGING CF_FILENAME  TYPE  STRING.

  DATA:
    LF_FOLDER TYPE STRING,
    LF_INIT   TYPE STRING.

  LF_INIT = P_LOGFIL.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_BROWSE
    EXPORTING
      INITIAL_FOLDER       = LF_INIT
    CHANGING
      SELECTED_FOLDER      = LF_FOLDER
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4.
  IF SY-SUBRC EQ 0 AND LF_FOLDER IS NOT INITIAL.
    CF_FILENAME = LF_FOLDER.
  ENDIF.
ENDFORM.
*----------------------------------------------------------------------*
*  Form PROCESS_DATA
*----------------------------------------------------------------------*
*  Process Data
*----------------------------------------------------------------------*
FORM F_PROCESS_DATA CHANGING CT_RESULT TYPE TT_RESULT
                             CS_SUM TYPE TS_SUM.

  DATA:
    LT_RESULT       TYPE TT_RESULT,
    LT_RAW          TYPE TT_RAW,
    LT_DATA         TYPE TT_DATA,
    LT_POITEM       TYPE TT_POITEM,
    LT_POITEMX      TYPE TT_POITEMX,
    LT_POSCHEDULE   TYPE TT_POSCHEDULE,
    LT_POSCHEDULEX  TYPE TT_POSCHEDULEX,
    LT_POACCOUNT    TYPE TT_POACCOUNT,
    LT_POACCOUNTX   TYPE TT_POACCOUNTX,
    LT_POTEXTHEADER TYPE TT_POTEXTHEADER,
    LT_POTEXTITEM   TYPE TT_POTEXTITEM.

* --------------------------------
* Step1: Read Input file data
* --------------------------------
  PERFORM F_READ_INPUT_FILE CHANGING LT_RAW
                                     LT_RESULT.

* --------------------------------
* Step2: Convert data from file into SAP format
* --------------------------------
  PERFORM F_PREPARE_DATA USING  LT_RAW
                          CHANGING LT_DATA.

* --------------------------------
* Step3: Upload data
* --------------------------------
  PERFORM F_UPLOAD_FILE  USING  LT_DATA
                                P_TEST
                         CHANGING CT_RESULT
                                CS_SUM.

* --------------------------------
* Step4: Create Log File
* --------------------------------

  IF P_LOG EQ GC_TRUE .

    PERFORM F_ASSIGN_LOGFILE  USING  P_LOGFIL
                              CHANGING GS_LOGFILE.
*   Generate Log File
    PERFORM F_CREATE_LOGFILE  USING  CS_SUM
                                     GS_LOGFILE
                                     CT_RESULT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_read_input_file
*&---------------------------------------------------------------------*
*& read input file to internal table
*&---------------------------------------------------------------------*
FORM F_READ_INPUT_FILE CHANGING CT_RAW TYPE TT_RAW
                                CT_RESULT TYPE TT_RESULT.
* Show Progress
* Text-p01 : Reading Input file. . .
  MC_SHOW_PROGRESS 10 TEXT-P01.

  PERFORM F_READ_EXCEL USING P_LFILE
                             P_BEGROW
                             P_BEGCOL
                             P_ENDROW
                             P_ENDCOL
                       CHANGING CT_RAW
                                CT_RESULT.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_read_excel
*----------------------------------------------------------------------*
*  Read Excel file into Raw internal table
*----------------------------------------------------------------------*
FORM F_READ_EXCEL  USING  UF_IFILE   TYPE STRING
                          UF_BEGROW  TYPE I
                          UF_BEGCOL  TYPE I
                          UF_ENDROW  TYPE I
                          UF_ENDCOL  TYPE I
                 CHANGING CT_RAW     TYPE TT_RAW
                          CT_RESULT  TYPE TT_RESULT.

  DATA:
   LT_TAB TYPE STANDARD TABLE OF ZSDSCAS016.

  DATA:
  LS_RAW TYPE TS_RAW.

  DATA:
    LF_FILENAME TYPE  STRING,
    LF_ROW      TYPE  I,
    LF_COL      TYPE  I.


  FIELD-SYMBOLS:
  <L_TAB>  TYPE ZSDSCAS016.

* Initialize Output
  REFRESH: CT_RAW,
           CT_RESULT.

* Assign File name
  LF_FILENAME = UF_IFILE.

* Read xls File to Internal Table
  CALL FUNCTION 'Z_SDSCA_EXCEL_TO_ITAB'
    EXPORTING
      FILENAME                = LF_FILENAME
      I_BEGIN_COL             = UF_BEGCOL
      I_BEGIN_ROW             = UF_BEGROW
      I_END_COL               = UF_ENDCOL
      I_END_ROW               = UF_ENDROW
    TABLES
      INTERN                  = LT_TAB
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.
  IF SY-SUBRC <> 0.

  ENDIF.

  SORT LT_TAB BY ROW ASCENDING
                 COL ASCENDING.

  LOOP AT LT_TAB ASSIGNING <L_TAB>.
*   -----------
*   On New Row
*   -----------
    IF <L_TAB>-ROW NE LF_ROW.

      IF LF_ROW IS NOT INITIAL.
        INSERT LS_RAW INTO TABLE CT_RAW.
      ENDIF.

*     Initialize New Line
      LF_ROW = <L_TAB>-ROW.
      LF_COL = 1.
      CLEAR LS_RAW.

    ENDIF.

*   -----------
*   Add Blank Cell
*   -----------
    WHILE LF_COL LT <L_TAB>-COL.
      IF LF_COL GT 1.
        CONCATENATE LS_RAW-TLINE GC_SPLIT
               INTO LS_RAW-TLINE.
      ENDIF.
      LF_COL = LF_COL + 1.
    ENDWHILE.

*   -----------
*   Assign column value
*   -----------
    IF LF_COL EQ 1.
      LS_RAW-TLINE = <L_TAB>-VALUE.
    ELSE.
      CONCATENATE LS_RAW-TLINE <L_TAB>-VALUE
             INTO LS_RAW-TLINE
        SEPARATED BY GC_SPLIT.
    ENDIF.

    LF_COL = LF_COL + 1.

*   Insert last line
    AT LAST.
      INSERT LS_RAW INTO TABLE CT_RAW.
    ENDAT.

  ENDLOOP.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_prepare_data
*----------------------------------------------------------------------*
*  Converrt data from file into SAP format
*----------------------------------------------------------------------*
FORM F_PREPARE_DATA USING  UT_RAW     TYPE  TT_RAW
                     CHANGING CT_DATA  TYPE  TT_DATA.

  DATA:
    LT_LTEXT  TYPE  TT_TEXT.

  DATA:
    LS_POHEADER     TYPE TS_POHEADER,
    LS_POHEADERX    TYPE TS_POHEADERX,
    LS_POITEM       TYPE TS_POITEM,
    LS_POITEMX      TYPE TS_POITEMX,
    LS_POSCHEDULE   TYPE TS_POSCHEDULE,
    LS_POSCHEDULEX  TYPE TS_POSCHEDULEX,
    LS_POACCOUNT    TYPE TS_POACCOUNT,
    LS_POACCOUNTX   TYPE TS_POACCOUNTX,
    LS_POTEXTHEADER TYPE TS_POTEXTHEADER,
    LS_POTEXTITEM   TYPE TS_POTEXTITEM,
    LS_MESSG        TYPE TS_MESSG.

  DATA:
    LF_ROWNO   TYPE  TS_RESULT-ROWNO.

  DATA:
    LS_RESULT TYPE TS_RESULT,
    LT_RESULT TYPE TT_RESULT.

  FIELD-SYMBOLS:
  <L_RAW>   TYPE  TS_RAW.


* Initialize Output
  REFRESH: CT_DATA.

* Show Progress
* Text-p02 : Validating file data. . .
  MC_SHOW_PROGRESS 20 TEXT-P02.

  LOOP AT UT_RAW ASSIGNING <L_RAW>.

    LF_ROWNO = LF_ROWNO + 1.

*   Translate Result into variables
    PERFORM F_TRANSLATE_RAW USING  <L_RAW>
                            CHANGING LS_RESULT
                                     LT_LTEXT
                                     LS_MESSG.

*   Validate and Append data into table
    PERFORM F_VALIDATE_AND_APPEND USING LF_ROWNO
                                        LS_RESULT
                                        LT_LTEXT
                                        LS_MESSG
                                  CHANGING CT_DATA.

    APPEND LS_RESULT TO GT_RESULT.

  ENDLOOP.

  SORT GT_RESULT BY EBELN.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_translate_raw
*----------------------------------------------------------------------*
*  Convert Uploaded data
*----------------------------------------------------------------------*
FORM F_TRANSLATE_RAW  USING US_RAW TYPE TS_RAW
                    CHANGING CS_RESULT  TYPE TS_RESULT
                             CT_TEXT    TYPE TT_TEXT
                             CS_MESSG   TYPE TS_MESSG.

  DATA:
    LT_SPLIT TYPE STANDARD TABLE OF STRING.

  DATA:
    LS_TEXT TYPE TS_TEMPLATE_TEXT.

  DATA:
    LF_INDEX    TYPE I,
    LF_FIELD    TYPE TS_FIELD_NAME,
    LF_STRING   TYPE STRING,
    LF_MSGTX    TYPE TS_MESSG-MSGTX,
    LF_REQUIRED TYPE CHAR1.

  DATA:
        LF_DATE TYPE SY-DATUM.

  DATA: LF_BUKRS TYPE SKB1-BUKRS,
        LF_NETPR TYPE STRING.

* Initialize Output
  CLEAR: CS_RESULT,
         CS_MESSG.
  REFRESH: CT_TEXT.

* Split Into Fields
  SPLIT US_RAW-TLINE AT GC_SPLIT INTO TABLE LT_SPLIT.

* Initialize Variable
  CLEAR: LF_MSGTX.

  LOOP AT LT_SPLIT INTO LF_STRING.
    LF_INDEX = SY-TABIX.

* Initialize Variables
    CLEAR: LF_MSGTX.

*   Ignore Dummy Value
    CHECK LF_STRING NE GC_DUMMY_FLD ##BLANK_OK.

*   Get Target Field name to assign value
    PERFORM F_GET_TARGET_FIELD  USING LF_INDEX
                              CHANGING LF_FIELD.

    CASE LF_FIELD.
*     ---------------------------
*     PO Type
*     ---------------------------
      WHEN 'BSART'.
*       Validate PO Type
        PERFORM F_VALIDATE_BSART USING LF_STRING
                                 CHANGING CS_RESULT-BSART
                                          LF_MSGTX.

*     ---------------------------
*     PO No.
*     ---------------------------
      WHEN 'EBELN'.
*       Validate PO No.
        IF LF_STRING IS INITIAL.
*       Text-e02 : Invalid PO Number:
          CONCATENATE TEXT-E02 LF_STRING
                 INTO LF_MSGTX
            SEPARATED BY SPACE.
        ELSE.
          CS_RESULT-EBELN = LF_STRING.
        ENDIF.

*     ---------------------------
*     Vendor
*     ---------------------------
      WHEN 'LIFNR'.
*       Validate Vendor
        PERFORM F_VALIDATE_LIFNR USING LF_STRING
                                 CHANGING CS_RESULT-LIFNR
                                          LF_MSGTX.

*     ---------------------------
*     Vendor Name
*     ---------------------------
      WHEN 'NAME1'.
*       Validate Vendor Name
        CS_RESULT-NAME1 = LF_STRING.

*     ---------------------------
*     Document Date
*     ---------------------------
      WHEN 'BEDAT'.
*       Validate Document Date
        PERFORM F_VALIDATE_DATE USING LF_STRING
                                CHANGING CS_RESULT-BEDAT
                                         LF_MSGTX.

*     ---------------------------
*     Purchase Org.
*     ---------------------------
      WHEN 'EKORG'.
*       Validate Purchase Org.
        PERFORM F_VALIDATE_EKORG  USING  LF_STRING
                                  CHANGING CS_RESULT-EKORG
                                           LF_MSGTX.

*     ---------------------------
*     Purchase Group
*     ---------------------------
      WHEN 'EKGRP'.
*       Validate Purchase Group
        PERFORM F_VALIDATE_EKGRP  USING  LF_STRING
                                   CHANGING CS_RESULT-EKGRP
                                            LF_MSGTX.

*     ---------------------------
*     Company Code
*     ---------------------------
      WHEN 'BUKRS'.
*       Validate Company Code
        CS_RESULT-BUKRS = LF_STRING.

*     ---------------------------
*     Payment Term
*     ---------------------------
      WHEN 'ZTERM'.
*       Validate Payment Term
        CS_RESULT-ZTERM = LF_STRING.

*     ---------------------------
*     Currency
*     ---------------------------
      WHEN 'WAERS'.
*       Validate Currency
        CS_RESULT-WAERS = LF_STRING.

*     ---------------------------
*     Exchange Rate
*     ---------------------------
      WHEN 'WKURS'.
*       Validate Exchange Rate
        CS_RESULT-WKURS = LF_STRING.

*     ---------------------------
*     Fixed Exch. Rate
*     ---------------------------
      WHEN 'KUFIX'.
*       Validate Fixed Exch. Rate
        CS_RESULT-KUFIX = LF_STRING.

*     ---------------------------
*     Incoterm
*     ---------------------------
      WHEN 'INCO1'.
*       Validate Incoterm
        CS_RESULT-INCO1 = LF_STRING.

*     ---------------------------
*     Inco. Location1
*     ---------------------------
      WHEN 'INCO2'.
*       Validate Inco. Location1
        CS_RESULT-INCO2 = LF_STRING.

*     ---------------------------
*     Your Reference
*     ---------------------------
      WHEN 'IHREZ'.
*       Validate Your Reference
        CS_RESULT-IHREZ = LF_STRING.

*     ---------------------------
*     Our Reference
*     ---------------------------
      WHEN 'UNSEZ'.
*       Validate Our Reference
        CS_RESULT-UNSEZ = LF_STRING.

*     ---------------------------
*     Sales Person
*     ---------------------------
      WHEN 'VERKF'.
*       Validate Sales Person
        CS_RESULT-VERKF = LF_STRING.

*     ---------------------------
*     Header Condition ZVAT
*     ---------------------------
      WHEN 'HEADER_CON'.
*       Validate ZVAT
        CS_RESULT-HEADER_CON = LF_STRING.

*     ---------------------------
*     Retention Indicator
*     ---------------------------
      WHEN 'RETTP'.
*       Validate Retention Indicator
        CS_RESULT-RETTP = LF_STRING.

*     ---------------------------
*     Retention in Percent
*     ---------------------------
      WHEN 'EKKO_RETPC'.
*       Validate Retention in Percent
        CS_RESULT-EKKO_RETPC = LF_STRING.

*     ---------------------------
*     Header text line 1
*     ---------------------------
      WHEN 'FROM_LINE1'.
*       Validate From line 1
        APPEND LF_STRING TO LS_TEXT-FORM_LINE.

*     ---------------------------
*     Header text line 2
*     ---------------------------
      WHEN 'FROM_LINE2'.
*       Validate From line 2
        APPEND LF_STRING TO LS_TEXT-FORM_LINE.

*     ---------------------------
*     Header text line 3
*     ---------------------------
      WHEN 'FROM_LINE3'.
*       Validate From line 3
        APPEND LF_STRING TO LS_TEXT-FORM_LINE.

*     ---------------------------
*     Remark line 1
*     ---------------------------
      WHEN 'ZZREMARKS01'.
*       Validate Remark line 1
        APPEND LF_STRING TO LS_TEXT-HEADER_REMARK_LINE.

*     ---------------------------
*     Remark line 2
*     ---------------------------
      WHEN 'ZZREMARKS02'.
*       Validate Remark line 2
        APPEND LF_STRING TO LS_TEXT-HEADER_REMARK_LINE.

*     ---------------------------
*     Remark line 3
*     ---------------------------
      WHEN 'ZZREMARKS03'.
*       Validate Remark line 3
        APPEND LF_STRING TO LS_TEXT-HEADER_REMARK_LINE.

*     ---------------------------
*     Remark line 4
*     ---------------------------
      WHEN 'ZZREMARKS04'.
*       Validate Remark line 4
        APPEND LF_STRING TO LS_TEXT-HEADER_REMARK_LINE.

*     ---------------------------
*     Remark line 5
*     ---------------------------
      WHEN 'ZZREMARKS05'.
*       Validate Remark line 5
        APPEND LF_STRING TO LS_TEXT-HEADER_REMARK_LINE.

*     ---------------------------
*     Project Code
*     ---------------------------
      WHEN 'ZZPRJTYP'.
*       Validate Project Code
        CS_RESULT-ZZPRJTYP = LF_STRING.

*     ---------------------------
*     Delivery Mode
*     ---------------------------
      WHEN 'ZZDELIVMODE'.
*       Validate Delivery Mode
        PERFORM F_VALIDATE_DLV_MODE USING LF_STRING
                                    CHANGING CS_RESULT-ZZDELIVMODE
                                             LF_MSGTX.

*     ---------------------------
*     Division Code
*     ---------------------------
      WHEN 'ZZ1_DIVCD_PO'.
*       Validate Division Code
        CS_RESULT-ZZ1_DIVCD_PO = LF_STRING.

*     ---------------------------
*     JTEPA flag
*     ---------------------------
      WHEN 'ZZ_JTEPA'.
*       Validate JTEPA flag
        CS_RESULT-ZZ_JTEPA = LF_STRING.

*     ---------------------------
*     Item No.
*     ---------------------------
      WHEN 'EBELP'.
*       Validate Item No.
        CS_RESULT-EBELP = LF_STRING.

*     ---------------------------
*     Acct. Assignment Cat.
*     ---------------------------
      WHEN 'KNTTP'.
*       Validate Acct. Assignment Cat.
        PERFORM F_VALIDATE_KNTTP  USING LF_STRING
                                  CHANGING CS_RESULT-KNTTP
                                           LF_MSGTX.

*     ---------------------------
*     Item Cat.
*     ---------------------------
      WHEN 'PSTYP'.
*       Validate Item Cat.
        PERFORM F_VALIDATE_PSTYP USING LF_STRING
                                 CHANGING CS_RESULT-PSTYP
                                          LF_MSGTX.

*     ---------------------------
*     Material No.
*     ---------------------------
      WHEN 'MATNR'.
*       Validate Material No.
        PERFORM F_VALIDATE_MATNR USING LF_STRING
                                 CHANGING CS_RESULT-MATNR
                                          LF_MSGTX.

*     ---------------------------
*     Short Text
*     ---------------------------
      WHEN 'TXZ01'.
*       Validate Short Text
        CS_RESULT-TXZ01 = LF_STRING.

*     ---------------------------
*     Material Group
*     ---------------------------
      WHEN 'MATKL'.
*       Validate Material Group
        CS_RESULT-MATKL = LF_STRING.

*     ---------------------------
*     Open PO Quantity
*     ---------------------------
      WHEN 'EKPO_MENGE'.
*       Validate Open PO Quantity
        CS_RESULT-EKPO_MENGE = LF_STRING.

*     ---------------------------
*     Order Unit
*     ---------------------------
      WHEN 'MEINS'.
*       Validate Order Unit
        PERFORM F_VALIDATE_UNIT USING LF_STRING
                                CHANGING CS_RESULT-MEINS
                                         LF_MSGTX.

*     ---------------------------
*     Delivery Date
*     ---------------------------
      WHEN 'EINDT'.
*       Validate Delivery Date
        PERFORM F_VALIDATE_DATE  USING  LF_STRING
                                 CHANGING CS_RESULT-EINDT
                                          LF_MSGTX.

*     ---------------------------
*     Net Price
*     ---------------------------
      WHEN 'NETPR'.
*       Validate Net Price
        CS_RESULT-NETPR = LF_STRING.

*     ---------------------------
*     Gross Price
*     ---------------------------
      WHEN 'BRTWR'.
*       Validate Gross Price
        CS_RESULT-BRTWR = LF_STRING.

*     ---------------------------
*     Price Unit
*     ---------------------------
      WHEN 'PEINH'.
*       Validate Price Unit
        CS_RESULT-PEINH = LF_STRING.

*     ---------------------------
*     Order Price Unit
*     ---------------------------
      WHEN 'BPRME'.
*       Validate Order Price Unit
        PERFORM F_VALIDATE_UNIT USING LF_STRING
                                CHANGING CS_RESULT-BPRME
                                         LF_MSGTX.

*     ---------------------------
*     Conv. From Qty Order Unit
*     ---------------------------
      WHEN 'BPUMN'.
*       Validate Conv. From Qty Order Unit
        CS_RESULT-BPUMN = LF_STRING.

*     ---------------------------
*     Conv. To Order Price Unit
*     ---------------------------
      WHEN 'BPUMZ'.
*       Validate Conv. To Order Price Unit
        CS_RESULT-BPUMZ = LF_STRING.

*     ---------------------------
*     Order Plant
*     ---------------------------
      WHEN 'WERKS'.
*       Validate Plant
        CS_RESULT-WERKS = LF_STRING.

*     ---------------------------
*     Storage Location
*     ---------------------------
      WHEN 'LGORT'.
*       Validate Storage Location
        CS_RESULT-LGORT = LF_STRING.

*     ---------------------------
*     Requisitioner
*     ---------------------------
      WHEN 'AFNAM'.
*       Validate Requisitioner
        CS_RESULT-AFNAM = LF_STRING.

*     ---------------------------
*     Tracking Number
*     ---------------------------
      WHEN 'BEDNR'.
*       Validate Tracking Number
        CS_RESULT-BEDNR = LF_STRING.

*     ---------------------------
*     Tax Code
*     ---------------------------
      WHEN 'MWSKZ'.
*       Validate Tax Code
        PERFORM F_VALIDATE_MWSKZ USING LF_STRING
                                CHANGING CS_RESULT-MWSKZ
                                         LF_MSGTX.

*     ---------------------------
*     % Over Tolenrance
*     ---------------------------
      WHEN 'UEBTO'.
*       Validate % Over Tolenrance
        CS_RESULT-UEBTO = LF_STRING.

*     ---------------------------
*     % Under Tolenrance
*     ---------------------------
      WHEN 'UNTTO'.
*       Validate % Under Tolenrance
        CS_RESULT-UNTTO = LF_STRING.

*     ---------------------------
*     Unlimited
*     ---------------------------
      WHEN 'UEBTK'.
*       Validate Unlimited
        CS_RESULT-UEBTK = LF_STRING.

*     ---------------------------
*     Goods Receipt
*     ---------------------------
      WHEN 'WEPOS'.
*       Validate Goods Receipt
        CS_RESULT-WEPOS = LF_STRING.

*     ---------------------------
*     Invoice Receipt
*     ---------------------------
      WHEN 'REPOS'.
*       Validate Invoice Receipt
        CS_RESULT-REPOS = LF_STRING.

*     ---------------------------
*     GR-Based IV
*     ---------------------------
      WHEN 'WEBRE'.
*       Validate GR-Based IV
        CS_RESULT-WEBRE = LF_STRING.

*     ---------------------------
*     Evaluated Receipt Settlement (ERS)
*     ---------------------------
      WHEN 'XERSY'.
*       Validate Evaluated Receipt Settlement (ERS)
        CS_RESULT-XERSY = LF_STRING.

*     ---------------------------
*     Stock Type
*     ---------------------------
      WHEN 'INSMK'.
*       Validate Stock Type
        CS_RESULT-INSMK = LF_STRING.

*     ---------------------------
*     Rem. Shelf Life
*     ---------------------------
      WHEN 'MHDRZ'.
*       Validate Rem. Shelf Life
        CS_RESULT-MHDRZ = LF_STRING.

*     ---------------------------
*     Period Ind.
*     ---------------------------
      WHEN 'IPRKZ'.
*       Validate Period Ind.
        PERFORM F_CONVERSION_EXIT USING 'PERKZ' 'INPUT' LF_STRING
                                  CHANGING CS_RESULT-IPRKZ.

*     ---------------------------
*     QA Control Key
*     ---------------------------
      WHEN 'SSQSS'.
*       Validate QA Control Key
        CS_RESULT-SSQSS = LF_STRING.

*     ---------------------------
*     Confirmation Control Key
*     ---------------------------
      WHEN 'BSTAE'.
*       Validate Confirmation Control Key
        CS_RESULT-BSTAE = LF_STRING.

*     ---------------------------
*     Distribution
*     ---------------------------
      WHEN 'VRTKZ'.
*       Validate Distribution
        CS_RESULT-VRTKZ = LF_STRING.

*     ---------------------------
*     Partial Inv.
*     ---------------------------
      WHEN 'TWRKZ'.
*       Validate Partial Inv.
        CS_RESULT-TWRKZ = LF_STRING.

*     ---------------------------
*     Seq. No.
*     ---------------------------
      WHEN 'ZEKKN'.
*       Validate Seq. No.
        CS_RESULT-ZEKKN = LF_STRING.

*     ---------------------------
*     Open Quantity
*     ---------------------------
      WHEN 'MENGE'.
*       Validate Open Quantity
        CS_RESULT-MENGE = LF_STRING.

*     ---------------------------
*     G/L Acct.
*     ---------------------------
      WHEN 'SAKTO'.
*       Validate G/L Acct.
        PERFORM F_CONVERSION_EXIT USING 'ALPHA' 'INPUT' LF_STRING
                                  CHANGING CS_RESULT-SAKTO.

*     ---------------------------
*     Cost Center
*     ---------------------------
      WHEN 'KOSTL'.
*       Validate Cost Center
        PERFORM F_CONVERSION_EXIT USING 'ALPHA' 'INPUT' LF_STRING
                                  CHANGING CS_RESULT-KOSTL.

*     ---------------------------
*     Fund Center
*     ---------------------------
      WHEN 'FISTL'.
*       Validate Fund Center
        PERFORM F_VALIDATE_FISTL USING LF_STRING
                                 CHANGING CS_RESULT-FISTL
                                         LF_MSGTX.

*     ---------------------------
*     Asset No.
*     ---------------------------
      WHEN 'ANLN1'.
*       Validate Asset No.
        PERFORM F_CONVERSION_EXIT USING 'ALPHA' 'INPUT' LF_STRING
                                  CHANGING CS_RESULT-ANLN1.

*     ---------------------------
*     Sub No.
*     ---------------------------
      WHEN 'ANLN2'.
*       Validate Sub No.
        PERFORM F_CONVERSION_EXIT USING 'ALPHA' 'INPUT' LF_STRING
                                  CHANGING CS_RESULT-ANLN2.

*     ---------------------------
*     Commitment Item
*     ---------------------------
      WHEN 'FIPOS'.
*       Validate Commitment Item
        PERFORM F_CONVERSION_EXIT USING 'FMCIS' 'INPUT' LF_STRING
                                  CHANGING CS_RESULT-FIPOS.

*     ---------------------------
*     Order No.
*     ---------------------------
      WHEN 'AUFNR'.
*       Validate Order No.
        PERFORM F_CONVERSION_EXIT USING 'ALPHA' 'INPUT' LF_STRING
                                  CHANGING CS_RESULT-AUFNR.

*     ---------------------------
*     Profit Center
*     ---------------------------
      WHEN 'PRCTR'.
*       Validate Profit Center
        PERFORM F_CONVERSION_EXIT USING 'ALPHA' 'INPUT' LF_STRING
                                  CHANGING CS_RESULT-PRCTR.

*     ---------------------------
*     Work Breakdown Structure Element(WBS)
*     ---------------------------
      WHEN 'PS_PSP_PNR'.
*       Validate Work Breakdown Structure Element
        PERFORM F_CONVERSION_EXIT USING 'ABPSN' 'INPUT' LF_STRING
                                  CHANGING CS_RESULT-PS_PSP_PNR.

*     ---------------------------
*     Sales Order
*     ---------------------------
      WHEN 'VBELN'.
*       Validate Sales Order
        CS_RESULT-VBELN = LF_STRING.

*     ---------------------------
*     Sales Order item
*     ---------------------------
      WHEN 'VBELP'.
*       Validate Sales Order item
        CS_RESULT-VBELP = LF_STRING.

*     ---------------------------
*     Internal Order Line 1
*     ---------------------------
      WHEN 'INTERNAL_LINE1'.
*       Validate Internal Order Line 1
        APPEND LF_STRING TO LS_TEXT-INTERNAL_LINE.

*     ---------------------------
*     Internal Order Line 2
*     ---------------------------
      WHEN 'INTERNAL_LINE2'.
*       Validate Internal Order Line 2
        APPEND LF_STRING TO LS_TEXT-INTERNAL_LINE.

*     ---------------------------
*     Internal Order Line 3
*     ---------------------------
      WHEN 'INTERNAL_LINE3'.
*       Validate Internal Order Line 3
        APPEND LF_STRING TO LS_TEXT-INTERNAL_LINE.

*     ---------------------------
*     Project Neme Line 1
*     ---------------------------
      WHEN 'PROJECT_LINE1'.
*       Validate Project Neme Line 1
        APPEND LF_STRING TO LS_TEXT-PROJECT_LINE.

*     ---------------------------
*     Project Neme Line 2
*     ---------------------------
      WHEN 'PROJECT_LINE2'.
*       Validate Project Neme Line 2
        APPEND LF_STRING TO LS_TEXT-PROJECT_LINE.

*     ---------------------------
*     Project Neme Line 3
*     ---------------------------
      WHEN 'PROJECT_LINE3'.
*       Validate Project Neme Line 3
        APPEND LF_STRING TO LS_TEXT-PROJECT_LINE.

*     ---------------------------
*     Attention Line 1
*     ---------------------------
      WHEN 'ATTENTION_LINE1'.
*       Validate Attention Line 1
        APPEND LF_STRING TO LS_TEXT-ATTENTION_LINE.

*     ---------------------------
*     Attention Line 2
*     ---------------------------
      WHEN 'ATTENTION_LINE2'.
*       Validate Attention Line 2
        APPEND LF_STRING TO LS_TEXT-ATTENTION_LINE.

*     ---------------------------
*     Attention Line 3
*     ---------------------------
      WHEN 'ATTENTION_LINE3'.
*       Validate Attention Line 3
        APPEND LF_STRING TO LS_TEXT-ATTENTION_LINE.

*     ---------------------------
*     Person In Charge Line1
*     ---------------------------
      WHEN 'PERSON_LINE1'.
*       Validate Person In Charge Line1
        APPEND LF_STRING TO LS_TEXT-PERSON_LINE.

*     ---------------------------
*     Person In Charge Line2
*     ---------------------------
      WHEN 'PERSON_LINE2'.
*       Validate Person In Charge Line2
        APPEND LF_STRING TO LS_TEXT-PERSON_LINE.

*     ---------------------------
*     Person In Charge Line3
*     ---------------------------
      WHEN 'PERSON_LINE3'.
*       Validate Person In Charge Line3
        APPEND LF_STRING TO LS_TEXT-PERSON_LINE.

*     ---------------------------
*     Remark Line1
*     ---------------------------
      WHEN 'REMARK_LINE1'.
*       Validate Remark Line1
        APPEND LF_STRING TO LS_TEXT-REMARK_LINE.

*     ---------------------------
*     Remark Line2
*     ---------------------------
      WHEN 'REMARK_LINE2'.
*       Validate Remark Line2
        APPEND LF_STRING TO LS_TEXT-REMARK_LINE.

*     ---------------------------
*     Remark Line3
*     ---------------------------
      WHEN 'REMARK_LINE3'.
*       Validate Remark Line3
        APPEND LF_STRING TO LS_TEXT-REMARK_LINE.

*     ---------------------------
*     Item Line1
*     ---------------------------
      WHEN 'ITEM_LINE1'.
*       Validate Item Line1
        APPEND LF_STRING TO LS_TEXT-ITEM_LINE.

*     ---------------------------
*     Item Line2
*     ---------------------------
      WHEN 'ITEM_LINE2'.
*       Validate Item Line2
        APPEND LF_STRING TO LS_TEXT-ITEM_LINE.

*     ---------------------------
*     Item Line3
*     ---------------------------
      WHEN 'ITEM_LINE3'.
*       Validate Item Line3
        APPEND LF_STRING TO LS_TEXT-ITEM_LINE.

*     ---------------------------
*     Memo Line1
*     ---------------------------
      WHEN 'MEMO_LINE1'.
*       Validate Memo Line1
        APPEND LF_STRING TO LS_TEXT-MEMO_LINE.

*     ---------------------------
*     Memo Line2
*     ---------------------------
      WHEN 'MEMO_LINE2'.
*       Validate Memo Line2
        APPEND LF_STRING TO LS_TEXT-MEMO_LINE.

*     ---------------------------
*     Memo Line3
*     ---------------------------
      WHEN 'MEMO_LINE3'.
*       Validate Memo Line3
        APPEND LF_STRING TO LS_TEXT-MEMO_LINE.

*     ---------------------------
*     Warranty Line1
*     ---------------------------
      WHEN 'WARR_LINE1'.
*       Validate Warranty Line1
        APPEND LF_STRING TO LS_TEXT-WARRANTY_LINE.

*     ---------------------------
*     Warranty Line2
*     ---------------------------
      WHEN 'WARR_LINE2'.
*       Validate Warranty Line2
        APPEND LF_STRING TO LS_TEXT-WARRANTY_LINE.

*     ---------------------------
*     Warranty Line3
*     ---------------------------
      WHEN 'WARR_LINE3'.
*       Validate Warranty Line3
        APPEND LF_STRING TO LS_TEXT-WARRANTY_LINE.

*     ---------------------------
*     Engineer Line1
*     ---------------------------
      WHEN 'ENGINEER_LINE1'.
*       Validate Engineer Line1
        APPEND LF_STRING TO LS_TEXT-ENGINEER_LINE.

*     ---------------------------
*     Engineer Line2
*     ---------------------------
      WHEN 'ENGINEER_LINE2'.
*       Validate Engineer Line2
        APPEND LF_STRING TO LS_TEXT-ENGINEER_LINE.

*     ---------------------------
*     Engineer Line3
*     ---------------------------
      WHEN 'ENGINEER_LINE3'.
*       Validate Engineer Line3
        APPEND LF_STRING TO LS_TEXT-ENGINEER_LINE.

*     ---------------------------
*     LOB
*     ---------------------------
      WHEN 'ZZ1_LOB_PO_PDI'.
*       Validate LOB
        PERFORM F_CONVERSION_EXIT USING 'ALPHA' 'INPUT' LF_STRING
                                  CHANGING CS_RESULT-ZZ1_LOB_PO_PDI.

*     ---------------------------
*     Dest. Port Code
*     ---------------------------
      WHEN 'ZZ1_DES_PT_C_PO'.
*       Validate Dest. Port Code
        CS_RESULT-ZZ1_DES_PT_C_PO = LF_STRING.

*       This is the last field that must be processed
*       even its previous fields are error
        LF_REQUIRED = GC_TRUE.
    ENDCASE.

*   If error, assign message and ignore the rest
    IF LF_MSGTX IS NOT INITIAL.
*     Do not override previous error
      IF CS_MESSG-MSGTY NE 'E'.
        CS_MESSG-MSGTY = 'E'.
        CS_MESSG-MSGTX = LF_MSGTX.
      ENDIF.
*     Processing until all required fields
      IF LF_REQUIRED EQ GC_TRUE.
        EXIT.
      ENDIF.
    ENDIF.

  ENDLOOP.

* Checking delivery mode
  IF CS_RESULT-LIFNR NE '0000132005'.
    IF CS_RESULT-ZZDELIVMODE IS INITIAL AND ( CS_RESULT-BSART EQ 'ZP1' OR CS_RESULT-BSART EQ 'ZP2' ).
      CS_MESSG-MSGTY = 'E'.
      CS_MESSG-MSGTX = 'Delivery Mode is missing'.
    ENDIF.
  ENDIF.

  IF LS_TEXT IS NOT INITIAL.
    PERFORM F_ASSIGN_TEXT  USING  LS_TEXT
                         CHANGING CT_TEXT.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_get_target_field
*----------------------------------------------------------------------*
*  Get Target Field name based on column sequence
*----------------------------------------------------------------------*
FORM F_GET_TARGET_FIELD  USING  UF_INDEX  TYPE  I
                       CHANGING UF_FIELD  TYPE  TS_FIELD_NAME.

  STATICS:
  LT_FIELDS   TYPE  ABAP_COMPDESCR_TAB.

  DATA:
    LREF_DESCR TYPE  REF TO CL_ABAP_TYPEDESCR,
    LREF_STRUC TYPE  REF TO CL_ABAP_STRUCTDESCR.

  FIELD-SYMBOLS:
  <L_FIELD>  TYPE  ABAP_COMPDESCR.


* Initialize Output
  CLEAR: UF_FIELD.

  IF LT_FIELDS IS INITIAL.
*   Get Template Fields Sequence
    LREF_DESCR = CL_ABAP_TYPEDESCR=>DESCRIBE_BY_NAME( 'ZSDSMMS028' ).
    LREF_STRUC ?= LREF_DESCR.
    LT_FIELDS = LREF_STRUC->COMPONENTS.
  ENDIF.

  READ TABLE LT_FIELDS ASSIGNING <L_FIELD>
                       INDEX UF_INDEX.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Assign Output
  UF_FIELD = <L_FIELD>-NAME.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_validate_bsart
*----------------------------------------------------------------------*
*  text
*----------------------------------------------------------------------*
FORM F_VALIDATE_BSART  USING   UF_STRING TYPE STRING
                      CHANGING CF_BSART TYPE T161-BSART
                               CF_MSGTX TYPE CLIKE.

  TYPES: BEGIN OF LTS_T161,
           BSART TYPE  T161-BSART,
         END OF LTS_T161.
  TYPES: LTT_T161  TYPE  SORTED TABLE OF LTS_T161
  WITH UNIQUE KEY BSART.

  STATICS:
    LT_T161 TYPE  LTT_T161,
    LS_T161 TYPE  LTS_T161.

  DATA: LF_LENGTH TYPE I.

* Initialize Output
  CLEAR: CF_MSGTX.

* Only not initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

  LF_LENGTH = STRLEN( UF_STRING ).
  IF LF_LENGTH GT 4.
*   Text-e01 : Invalid PO Type:
    CONCATENATE TEXT-E01 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.


  IF LS_T161-BSART NE UF_STRING.
*   Validate with Memory
    READ TABLE LT_T161 INTO LS_T161
                       WITH KEY BSART = UF_STRING
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_T161.
*     Validate with Database
      SELECT SINGLE BSART                            "#EC CI_SEL_NESTED
        INTO LS_T161
        FROM T161
       WHERE BSTYP EQ 'F'
       AND BSART  EQ  UF_STRING.
      IF SY-SUBRC NE 0.
*       Text-e01 : Invalid PO Type:
        CONCATENATE TEXT-E01 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_T161 INTO TABLE LT_T161.
    ENDIF.

  ENDIF.

* Assign Output
  CF_BSART = LS_T161-BSART.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_validate_lifnr
*----------------------------------------------------------------------*
*  text
*----------------------------------------------------------------------*
FORM F_VALIDATE_LIFNR  USING  UF_STRING  TYPE  STRING
                      CHANGING CF_LIFNR  TYPE  LFA1-LIFNR
                               CF_MSGTX  TYPE  CLIKE.

  TYPES: BEGIN OF LTS_LFA1,
           LIFNR TYPE  LFA1-LIFNR,
         END OF LTS_LFA1.
  TYPES: LTT_LFA1  TYPE  SORTED TABLE OF LTS_LFA1
  WITH UNIQUE KEY LIFNR.

  STATICS:
    LT_LFA1 TYPE  LTT_LFA1,
    LS_LFA1 TYPE  LTS_LFA1.

  DATA:
    LF_LIFNR  TYPE  LFA1-LIFNR,
    LF_LENGTH TYPE  I.


* Initialize Output
  CLEAR: CF_LIFNR,
         CF_MSGTX.

* Only not initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

  LF_LENGTH = STRLEN( UF_STRING ).
  IF LF_LENGTH GT 10.
*   Text-e03 : Invalid Vendor:
    CONCATENATE TEXT-E03 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Convert to internal format
  PERFORM F_CONVERSION_EXIT USING 'ALPHA' 'INPUT' UF_STRING
                             CHANGING LF_LIFNR.

* Check Buffer
  IF LS_LFA1-LIFNR NE LF_LIFNR.
*   Validate with Memory
    READ TABLE LT_LFA1 INTO LS_LFA1
                       WITH KEY LIFNR = LF_LIFNR
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.

      CLEAR LS_LFA1.
*     Validate with Database
      SELECT SINGLE LIFNR                            "#EC CI_SEL_NESTED
        INTO LS_LFA1
        FROM LFA1
       WHERE LIFNR  EQ  LF_LIFNR.
      IF SY-SUBRC NE 0.
*       Text-e03 : Invalid Vendor:
        CONCATENATE TEXT-E03 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
    ENDIF.
    INSERT LS_LFA1 INTO TABLE LT_LFA1.
  ENDIF.

* Assign Output
  CF_LIFNR = LS_LFA1-LIFNR.

ENDFORM.
*----------------------------------------------------------------------*
*       Form  f_validate_date
*----------------------------------------------------------------------*
*       Validate Date
*----------------------------------------------------------------------*
FORM F_VALIDATE_DATE  USING  UF_STRING  TYPE  STRING
                    CHANGING CF_DATUM   TYPE  ANY
                             CF_MSGTX   TYPE  CLIKE.
  DATA:
    LF_LENGTH  TYPE  I.

* Initialize Output
  CLEAR: CF_DATUM,
         CF_MSGTX.

* Not initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Only number and .?
  IF NOT UF_STRING CO '0123456789.'.
*   Text-e4 : Wrong Date format. Please use format DD.MM.YYYY:
    CONCATENATE TEXT-E04 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Length?
  LF_LENGTH = STRLEN( UF_STRING ).
  IF LF_LENGTH NE 10.
*   Text-e4 : Wrong Date format. Please use format DD.MM.YYYY:
    CONCATENATE TEXT-E04 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* First 2 Digit is day
  IF NOT UF_STRING(2) BETWEEN 00 AND 31.
*   Text-e4 : Wrong Date format. Please use format DD.MM.YYYY:
    CONCATENATE TEXT-E04 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* 4-5th Digit is month
  IF NOT UF_STRING+3(2) BETWEEN 00 AND 12.
*   Text-e4 : Wrong Date format. Please use format DD.MM.YYYY:
    CONCATENATE TEXT-E04 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* 7-8th digit is year
*  IF NOT UF_STRING+6(4) BETWEEN 1900 AND 2200 AND
  IF NOT UF_STRING+6(4) NE '9999'.
*   Text-e4 : Wrong Date format. Please use format DD.MM.YYYY:
    CONCATENATE TEXT-E04 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Assign Output
  CONCATENATE UF_STRING+6(4) UF_STRING+3(2) UF_STRING(2)
         INTO CF_DATUM.
ENDFORM.
*----------------------------------------------------------------------*
*  Form f_validate_ekorg
*----------------------------------------------------------------------*
*  text
*----------------------------------------------------------------------*
FORM F_VALIDATE_EKORG  USING   UF_STRING  TYPE STRING
                        CHANGING CF_EKORG TYPE T024E-EKORG
                                 CF_MSGTX TYPE CLIKE.

  TYPES: BEGIN OF LTS_T024E,
           EKORG TYPE T024E-EKORG,
         END OF LTS_T024E.
  TYPES: LTT_T024E  TYPE  SORTED TABLE OF LTS_T024E
  WITH UNIQUE KEY EKORG.

  STATICS:
    LT_T024E TYPE  LTT_T024E,
    LS_T024E TYPE  LTS_T024E.

  DATA: LF_LENGTH TYPE I.

* Initialize Output
  CLEAR: CF_MSGTX.

* Only not initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

  LF_LENGTH = STRLEN( UF_STRING ).
  IF LF_LENGTH GT 4.
*   Text-e05 : Invalid Purchase Org.:
    CONCATENATE TEXT-E05 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.


  IF LS_T024E-EKORG NE UF_STRING.
*   Validate with Memory
    READ TABLE LT_T024E INTO LS_T024E
                       WITH KEY EKORG = UF_STRING
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_T024E.
*     Validate with Database
      SELECT SINGLE EKORG                            "#EC CI_SEL_NESTED
        INTO LS_T024E
        FROM T024E
       WHERE EKORG EQ  UF_STRING.
      IF SY-SUBRC NE 0.
*       Text-e05 : Invalid Purchase Org.:
        CONCATENATE TEXT-E05 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_T024E INTO TABLE LT_T024E.
    ENDIF.

  ENDIF.

* Assign Output
  CF_EKORG = LS_T024E-EKORG.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_validate_ekgrp
*----------------------------------------------------------------------*
*  text
*----------------------------------------------------------------------*
FORM F_VALIDATE_EKGRP  USING   UF_STRING  TYPE STRING
                        CHANGING CF_EKGRP TYPE T024-EKGRP
                                 CF_MSGTX TYPE CLIKE.

  TYPES: BEGIN OF LTS_T024,
           EKGRP TYPE T024-EKGRP,
         END OF LTS_T024.
  TYPES: LTT_T024  TYPE  SORTED TABLE OF LTS_T024
  WITH UNIQUE KEY EKGRP.

  STATICS:
    LT_T024 TYPE  LTT_T024,
    LS_T024 TYPE  LTS_T024.

  DATA: LF_LENGTH TYPE I.

* Initialize Output
  CLEAR: CF_MSGTX.

* Only not initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

  LF_LENGTH = STRLEN( UF_STRING ).
  IF LF_LENGTH GT 3.
*   Text-e06 : Invalid Purchase Group:
    CONCATENATE TEXT-E06 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.


  IF LS_T024-EKGRP NE UF_STRING.
*   Validate with Memory
    READ TABLE LT_T024 INTO LS_T024
                       WITH KEY EKGRP = UF_STRING
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_T024.
*     Validate with Database
      SELECT SINGLE EKGRP                            "#EC CI_SEL_NESTED
        INTO LS_T024
        FROM T024
       WHERE EKGRP EQ  UF_STRING.
      IF SY-SUBRC NE 0.
*       Text-e06 : Invalid Purchase Group:
        CONCATENATE TEXT-E06 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_T024 INTO TABLE LT_T024.
    ENDIF.

  ENDIF.

* Assign Output
  CF_EKGRP = LS_T024-EKGRP.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_validate_knttp
*----------------------------------------------------------------------*
*  text
*----------------------------------------------------------------------*
FORM F_VALIDATE_KNTTP  USING   UF_STRING  TYPE  STRING
                           CHANGING CF_KNTTP TYPE T163K-KNTTP
                                    CF_MSGTX TYPE CLIKE.

  TYPES: BEGIN OF LTS_T163K,
           KNTTP TYPE T163K-KNTTP,
         END OF LTS_T163K.
  TYPES: LTT_T163K TYPE SORTED TABLE OF LTS_T163K
  WITH UNIQUE KEY KNTTP.

  STATICS:
    LT_T163K TYPE  LTT_T163K,
    LS_T163K TYPE  LTS_T163K.

  DATA: LF_LENGTH TYPE I.

* Initialize Output
  CLEAR: CF_MSGTX.

* Only not initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

  LF_LENGTH = STRLEN( UF_STRING ).
  IF LF_LENGTH GT 1.
*   Text-e07 : Invalid Acct. Assignment Cat:
    CONCATENATE TEXT-E07 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.


  IF LS_T163K-KNTTP NE UF_STRING.
*   Validate with Memory
    READ TABLE LT_T163K INTO LS_T163K
                       WITH KEY KNTTP = UF_STRING
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_T163K.
*     Validate with Database
      SELECT SINGLE KNTTP                            "#EC CI_SEL_NESTED
        INTO LS_T163K
        FROM T163K
       WHERE KNTTP EQ  UF_STRING.
      IF SY-SUBRC NE 0.
*       Text-e07 : Invalid Acct. Assignment Cat:
        CONCATENATE TEXT-E07 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_T163K INTO TABLE LT_T163K.
    ENDIF.

  ENDIF.

* Assign Output
  CF_KNTTP = LS_T163K-KNTTP.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_validate_pstyp
*----------------------------------------------------------------------*
*  text
*----------------------------------------------------------------------*
FORM F_VALIDATE_PSTYP USING UF_STRING TYPE STRING
                           CHANGING CF_EPSTP TYPE T163Y-EPSTP
                                    CF_MSGTX TYPE CLIKE.
  TYPES: BEGIN OF LTS_T163Y,
           EPSTP TYPE T163Y-EPSTP,
         END OF LTS_T163Y.
  TYPES: LTT_T163Y TYPE SORTED TABLE OF LTS_T163Y
  WITH UNIQUE KEY EPSTP.

  STATICS:
    LT_T163Y TYPE LTT_T163Y,
    LS_T163Y TYPE LTS_T163Y.

  DATA: LF_LENGTH TYPE I.

* Initialize Output
  CLEAR: CF_MSGTX.

* Only not initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

  LF_LENGTH = STRLEN( UF_STRING ).
  IF LF_LENGTH GT 1.
*   Text-e08 : Invalid Item Cat.:
    CONCATENATE TEXT-E08 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.


  IF LS_T163Y-EPSTP NE UF_STRING.
*   Validate with Memory
    READ TABLE LT_T163Y INTO LS_T163Y
                       WITH KEY EPSTP = UF_STRING
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_T163Y.
*     Validate with Database
      SELECT SINGLE EPSTP
        INTO LS_T163Y
        FROM T163Y
       WHERE EPSTP EQ  UF_STRING.
      IF SY-SUBRC NE 0.
*       Text-e08 : Invalid Item Cat.:
        CONCATENATE TEXT-E08 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_T163Y INTO TABLE LT_T163Y.
    ENDIF.

  ENDIF.

* Assign Output
  CF_EPSTP = LS_T163Y-EPSTP.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_validate_matnr
*----------------------------------------------------------------------*
*  text
*----------------------------------------------------------------------*
FORM F_VALIDATE_MATNR  USING   UF_STRING  TYPE  STRING
                       CHANGING CF_MATNR TYPE MARA-MATNR
                                CF_MSGTX TYPE CLIKE.
  TYPES: BEGIN OF LTS_MARA,
           MATNR TYPE  MARA-MATNR,
         END OF LTS_MARA.
  TYPES: LTT_MARA  TYPE  SORTED TABLE OF LTS_MARA
  WITH UNIQUE KEY MATNR.

  STATICS:
    LT_MARA TYPE  LTT_MARA,
    LS_MARA TYPE  LTS_MARA.

  DATA:
  LF_MATNR  TYPE  MARA-MATNR.


* Initialize Output
  CLEAR: CF_MATNR,
         CF_MSGTX.

* Only when not initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Convert to internal format
  PERFORM F_CONVERSION_EXIT USING 'MATN1' 'INPUT' UF_STRING
                             CHANGING LF_MATNR.

* Check Buffer
  IF LS_MARA-MATNR NE LF_MATNR.
*   Validate with Memory
    READ TABLE LT_MARA INTO LS_MARA
                       WITH KEY MATNR = LF_MATNR
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.

      CLEAR LS_MARA.
*     Validate with Database
      SELECT SINGLE MATNR                            "#EC CI_SEL_NESTED
        INTO LS_MARA
        FROM MARA
       WHERE MATNR  EQ  LF_MATNR.
      IF SY-SUBRC NE 0.
*       Text-e09 : Invalid Material number:
        CONCATENATE TEXT-E09 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_MARA INTO TABLE LT_MARA.
    ENDIF.

  ENDIF.

* Assign Output
  CF_MATNR = LF_MATNR.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_validate_unit
*----------------------------------------------------------------------*
*  text
*----------------------------------------------------------------------*
FORM F_VALIDATE_UNIT  USING  UF_STRING  TYPE  STRING
                      CHANGING CF_MEINS   TYPE  MEINS
                               CF_MSGTX   TYPE  CLIKE.
  DATA:
    LF_LENGTH  TYPE  I.


* Initialize Output
  CLEAR: CF_MEINS,
         CF_MSGTX.

* Check Not Initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

* Check Length
  LF_LENGTH = STRLEN( UF_STRING ).
  IF LF_LENGTH GT 3.
*   Text-e10 : Invalid Unit:
    CONCATENATE TEXT-E10 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

* Convert to input format
  PERFORM F_CONVERSION_EXIT USING 'CUNIT' 'INPUT' UF_STRING
                             CHANGING CF_MEINS.

* Validate in table
  SELECT SINGLE MSEHI
    INTO CF_MEINS
    FROM T006
   WHERE MSEHI EQ CF_MEINS.
  IF SY-SUBRC NE 0.
    CLEAR CF_MEINS.
*   Text-e10 : Invalid Unit:
    CONCATENATE TEXT-E10 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_validate_mwskz
*----------------------------------------------------------------------*
*  text
*----------------------------------------------------------------------*
FORM F_VALIDATE_MWSKZ  USING   UF_STRING  TYPE STRING
                      CHANGING CF_MWSKZ   TYPE T007A-MWSKZ
                               CF_MSGTX   TYPE CLIKE.

  TYPES: BEGIN OF LTS_T007A,
           MWSKZ TYPE  T007A-MWSKZ,
         END OF LTS_T007A.
  TYPES: LTT_T007A TYPE SORTED TABLE OF LTS_T007A
  WITH UNIQUE KEY MWSKZ.

  STATICS:
    LT_T007A TYPE  LTT_T007A,
    LS_T007A TYPE  LTS_T007A.

  DATA: LF_LENGTH TYPE I.

* Initialize Output
  CLEAR: CF_MSGTX.

* Only not initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

  LF_LENGTH = STRLEN( UF_STRING ).
  IF LF_LENGTH GT 2.
*   Text-e11 : Invalid Tax Code:
    CONCATENATE TEXT-E11 UF_STRING
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.


  IF LS_T007A-MWSKZ NE UF_STRING.
*   Validate with Memory
    READ TABLE LT_T007A INTO LS_T007A
                       WITH KEY MWSKZ = UF_STRING
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_T007A.
*     Validate with Database
      SELECT SINGLE MWSKZ                            "#EC CI_SEL_NESTED
        INTO LS_T007A
        FROM T007A
       WHERE KALSM EQ 'TAXTH'
       AND MWSKZ  EQ  UF_STRING.
      IF SY-SUBRC NE 0.
*       Text-e11 : Invalid Tax Code:
        CONCATENATE TEXT-E11 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_T007A INTO TABLE LT_T007A.
    ENDIF.

  ENDIF.

* Assign Output
  CF_MWSKZ = LS_T007A-MWSKZ.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CONVERSION_EXIT
*&---------------------------------------------------------------------*
*       Conversion Exit
*----------------------------------------------------------------------*
FORM F_CONVERSION_EXIT USING UF_EXIT  TYPE ANY
                             UF_TYPE  TYPE ANY
                             UF_INPUT TYPE ANY
                       CHANGING CF_OUTPUT TYPE ANY.
  DATA:
    LF_CONEXIT TYPE CHAR30.

  CONCATENATE 'CONVERSION_EXIT_' UF_EXIT '_' UF_TYPE INTO LF_CONEXIT.

  CF_OUTPUT = UF_INPUT.
  CALL FUNCTION LF_CONEXIT
    EXPORTING
      INPUT  = CF_OUTPUT
    IMPORTING
      OUTPUT = CF_OUTPUT.
ENDFORM.
*----------------------------------------------------------------------*
*  Form f_validate_fistl
*----------------------------------------------------------------------*
*  text
*----------------------------------------------------------------------*
FORM F_VALIDATE_FISTL USING   UF_STRING TYPE STRING
                      CHANGING CF_FICTR TYPE FMFCTR-FICTR
                               CF_MSGTX TYPE CLIKE.
  TYPES: BEGIN OF LTS_FMFCTR,
           FICTR TYPE FMFCTR-FICTR,
         END OF LTS_FMFCTR.
  TYPES: LTT_FMFCTR TYPE SORTED TABLE OF LTS_FMFCTR
  WITH UNIQUE KEY FICTR.

  STATICS:
    LT_FMFCTR TYPE LTT_FMFCTR,
    LS_FMFCTR TYPE LTS_FMFCTR.

* Initialize Output
  CLEAR: CF_MSGTX.

* Only not initial
  IF UF_STRING IS INITIAL.
    RETURN.
  ENDIF.

  IF LS_FMFCTR-FICTR NE UF_STRING.
*   Validate with Memory
    READ TABLE LT_FMFCTR INTO LS_FMFCTR
                       WITH KEY FICTR = UF_STRING
                       BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_FMFCTR.
*     Validate with Database
      SELECT SINGLE FICTR
        INTO LS_FMFCTR
        FROM FMFCTR
       WHERE FICTR EQ  UF_STRING
        AND DATBIS GE SY-DATUM
        AND DATAB LE SY-DATUM.
      IF SY-SUBRC NE 0.
*       Text-e05 : Invalid Item Cat.:
        CONCATENATE TEXT-E05 UF_STRING
               INTO CF_MSGTX
          SEPARATED BY SPACE.
        RETURN.
      ENDIF.
      INSERT LS_FMFCTR INTO TABLE LT_FMFCTR.
    ENDIF.

  ENDIF.

* Assign Output
  CF_FICTR = LS_FMFCTR-FICTR.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_validate_dlv_mode
*----------------------------------------------------------------------*
*  text
*----------------------------------------------------------------------*
FORM F_VALIDATE_DLV_MODE  USING   UF_STRING  TYPE STRING
                          CHANGING CF_DLV_MODE TYPE EKKO-ZZ1_DLV_MODE
                                 CF_MSGTX TYPE CLIKE.

  DATA: LF_TEMP TYPE STRING.

  IF UF_STRING CA '0123456789'.
    SPLIT UF_STRING AT ' ' INTO CF_DLV_MODE LF_TEMP.

    PERFORM F_CONVERSION_EXIT USING 'ALPHA' 'INPUT' CF_DLV_MODE
                           CHANGING CF_DLV_MODE.
  ELSE.
    CONCATENATE 'Delivery Mode:' UF_STRING 'not contain number'
           INTO CF_MSGTX
      SEPARATED BY SPACE.
    RETURN.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_assign_text
*----------------------------------------------------------------------*
*  Assign Text data from template
*----------------------------------------------------------------------*
FORM F_ASSIGN_TEXT  USING  PS_TEMP_TEXT  TYPE  TS_TEMPLATE_TEXT
                CHANGING PT_TEXT       TYPE  TT_TEXT.

  DATA:
  LS_TEXT  TYPE  TS_TEXT.

  FIELD-SYMBOLS:
  <L_STRING>  TYPE  STRING.

* Initialize Output
  REFRESH: PT_TEXT.

* Form line
  IF PS_TEMP_TEXT-FORM_LINE IS NOT INITIAL.
    CLEAR LS_TEXT.
    LS_TEXT-TDOBJECT = GC_OBJEC_HEADERTXT.
    LS_TEXT-TDID = 'F01'.
    LS_TEXT-TDSPRAS  = 'E'.
    LOOP AT PS_TEMP_TEXT-FORM_LINE ASSIGNING <L_STRING>.
      LS_TEXT-TDFORMAT = '*'.
      LS_TEXT-TDLINE   = <L_STRING>.
      APPEND LS_TEXT TO PT_TEXT.
    ENDLOOP.
  ENDIF.

* Header Remark line
  IF PS_TEMP_TEXT-HEADER_REMARK_LINE IS NOT INITIAL.
    CLEAR LS_TEXT.
    LS_TEXT-TDOBJECT = GC_OBJEC_HEADERTXT.
    LS_TEXT-TDID = 'F00'.
    LS_TEXT-TDSPRAS  = 'E'.
    LOOP AT PS_TEMP_TEXT-HEADER_REMARK_LINE ASSIGNING <L_STRING>.
      LS_TEXT-TDFORMAT = '*'.
      LS_TEXT-TDLINE   = <L_STRING>.
      APPEND LS_TEXT TO PT_TEXT.
    ENDLOOP.
  ENDIF.

* Internal line
  IF PS_TEMP_TEXT-INTERNAL_LINE IS NOT INITIAL.
    CLEAR LS_TEXT.
    LS_TEXT-TDOBJECT = GC_OBJEC_ITEMTXT.
    LS_TEXT-TDID = 'F50'.
    LS_TEXT-TDSPRAS  = 'E'.
    LOOP AT PS_TEMP_TEXT-INTERNAL_LINE ASSIGNING <L_STRING>.
      LS_TEXT-TDFORMAT = '*'.
      LS_TEXT-TDLINE   = <L_STRING>.
      APPEND LS_TEXT TO PT_TEXT.
    ENDLOOP.
  ENDIF.

* Project line
  IF PS_TEMP_TEXT-PROJECT_LINE IS NOT INITIAL.
    CLEAR LS_TEXT.
    LS_TEXT-TDOBJECT = GC_OBJEC_ITEMTXT.
    LS_TEXT-TDID = 'F51'.
    LS_TEXT-TDSPRAS  = 'E'.
    LOOP AT PS_TEMP_TEXT-PROJECT_LINE ASSIGNING <L_STRING>.
      LS_TEXT-TDFORMAT = '*'.
      LS_TEXT-TDLINE   = <L_STRING>.
      APPEND LS_TEXT TO PT_TEXT.
    ENDLOOP.
  ENDIF.

* Attention line
  IF PS_TEMP_TEXT-ATTENTION_LINE IS NOT INITIAL.
    CLEAR LS_TEXT.
    LS_TEXT-TDOBJECT = GC_OBJEC_ITEMTXT.
    LS_TEXT-TDID = 'F52'.
    LS_TEXT-TDSPRAS  = 'E'.
    LOOP AT PS_TEMP_TEXT-ATTENTION_LINE ASSIGNING <L_STRING>.
      LS_TEXT-TDFORMAT = '*'.
      LS_TEXT-TDLINE   = <L_STRING>.
      APPEND LS_TEXT TO PT_TEXT.
    ENDLOOP.
  ENDIF.

* Person line
  IF PS_TEMP_TEXT-PERSON_LINE IS NOT INITIAL.
    CLEAR LS_TEXT.
    LS_TEXT-TDOBJECT = GC_OBJEC_ITEMTXT.
    LS_TEXT-TDID = 'F56'.
    LS_TEXT-TDSPRAS  = 'E'.
    LOOP AT PS_TEMP_TEXT-PERSON_LINE ASSIGNING <L_STRING>.
      LS_TEXT-TDFORMAT = '*'.
      LS_TEXT-TDLINE   = <L_STRING>.
      APPEND LS_TEXT TO PT_TEXT.
    ENDLOOP.
  ENDIF.

* Remark line
  IF PS_TEMP_TEXT-REMARK_LINE IS NOT INITIAL.
    CLEAR LS_TEXT.
    LS_TEXT-TDOBJECT = GC_OBJEC_ITEMTXT.
    LS_TEXT-TDID = 'F00'.
    LS_TEXT-TDSPRAS  = 'E'.
    LOOP AT PS_TEMP_TEXT-REMARK_LINE ASSIGNING <L_STRING>.
      LS_TEXT-TDFORMAT = '*'.
      LS_TEXT-TDLINE   = <L_STRING>.
      APPEND LS_TEXT TO PT_TEXT.
    ENDLOOP.
  ENDIF.

* Item line
  IF PS_TEMP_TEXT-ITEM_LINE IS NOT INITIAL.
    CLEAR LS_TEXT.
    LS_TEXT-TDOBJECT = GC_OBJEC_ITEMTXT.
    LS_TEXT-TDID = 'F01'.
    LS_TEXT-TDSPRAS  = 'E'.
    LOOP AT PS_TEMP_TEXT-ITEM_LINE ASSIGNING <L_STRING>.
      LS_TEXT-TDFORMAT = '*'.
      LS_TEXT-TDLINE   = <L_STRING>.
      APPEND LS_TEXT TO PT_TEXT.
    ENDLOOP.
  ENDIF.

* Memo line
  IF PS_TEMP_TEXT-MEMO_LINE IS NOT INITIAL.
    CLEAR LS_TEXT.
    LS_TEXT-TDOBJECT = GC_OBJEC_ITEMTXT.
    LS_TEXT-TDID = 'F75'.
    LS_TEXT-TDSPRAS  = 'E'.
    LOOP AT PS_TEMP_TEXT-MEMO_LINE ASSIGNING <L_STRING>.
      LS_TEXT-TDFORMAT = '*'.
      LS_TEXT-TDLINE   = <L_STRING>.
      APPEND LS_TEXT TO PT_TEXT.
    ENDLOOP.
  ENDIF.

* Warranty line
  IF PS_TEMP_TEXT-WARRANTY_LINE IS NOT INITIAL.
    CLEAR LS_TEXT.
    LS_TEXT-TDOBJECT = GC_OBJEC_ITEMTXT.
    LS_TEXT-TDID = 'F73'.
    LS_TEXT-TDSPRAS  = 'E'.
    LOOP AT PS_TEMP_TEXT-WARRANTY_LINE ASSIGNING <L_STRING>.
      LS_TEXT-TDFORMAT = '*'.
      LS_TEXT-TDLINE   = <L_STRING>.
      APPEND LS_TEXT TO PT_TEXT.
    ENDLOOP.
  ENDIF.

* Engineer line
  IF PS_TEMP_TEXT-ENGINEER_LINE IS NOT INITIAL.
    CLEAR LS_TEXT.
    LS_TEXT-TDOBJECT = GC_OBJEC_ITEMTXT.
    LS_TEXT-TDID = 'F74'.
    LS_TEXT-TDSPRAS  = 'E'.
    LOOP AT PS_TEMP_TEXT-ENGINEER_LINE ASSIGNING <L_STRING>.
      LS_TEXT-TDFORMAT = '*'.
      LS_TEXT-TDLINE   = <L_STRING>.
      APPEND LS_TEXT TO PT_TEXT.
    ENDLOOP.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_validate_and_append
*----------------------------------------------------------------------*
*  Assign Text data from template
*----------------------------------------------------------------------*
FORM F_VALIDATE_AND_APPEND USING UF_ROWNO TYPE TS_RESULT-ROWNO
                                 US_RESULT TYPE TS_RESULT
                                 UT_LTEXT TYPE TT_TEXT
                                 US_MESSG TYPE TS_MESSG
                           CHANGING CT_DATA TYPE TT_DATA.
  DATA:
    LS_DATA  TYPE TS_DATA,
    LF_INDEX TYPE I,
    LS_MESSG TYPE TS_MESSG.

  DATA:
    LS_POHEADER      TYPE TS_POHEADER,
    LS_POHEADERX     TYPE TS_POHEADERX,
    LS_POITEM        TYPE TS_POITEM,
    LS_POITEMX       TYPE TS_POITEMX,
    LS_POSCHEDULE    TYPE TS_POSCHEDULE,
    LS_POSCHEDULEX   TYPE TS_POSCHEDULEX,
    LS_POACCOUNT     TYPE TS_POACCOUNT,
    LS_POACCOUNTX    TYPE TS_POACCOUNTX,
    LS_POACCOUNTSEG  TYPE TS_POACCOUNTSEG,
    LS_POCONDHEADER  TYPE TS_POCONDHEADER,
    LS_POCONDHEADERX TYPE TS_POCONDHEADERX,
    LT_POTEXTHEADER  TYPE TT_POTEXTHEADER,
    LT_POTEXTITEM    TYPE TT_POTEXTITEM,
    LT_EXTENSIONIN   TYPE TT_EXTENSIONIN.

  PERFORM F_APPEND_HEADER USING US_RESULT
                                UT_LTEXT
                        CHANGING LS_POHEADER
                                 LS_POHEADERX
                                 LS_POCONDHEADER
                                 LS_POCONDHEADERX
                                 LT_POTEXTHEADER
                                 LT_EXTENSIONIN.

  PERFORM F_APPEND_ITEM USING US_RESULT
                              UT_LTEXT
                        CHANGING LS_POITEM
                                 LS_POITEMX
                                 LS_POSCHEDULE
                                 LS_POSCHEDULEX
                                 LT_POTEXTITEM
                                 LT_EXTENSIONIN.

  PERFORM F_APPEND_ACCT USING US_RESULT
                        CHANGING LS_POACCOUNT
                                 LS_POACCOUNTX
                                 LS_POACCOUNTSEG.

  CLEAR: LS_DATA.

  IF CT_DATA IS NOT INITIAL.
    READ TABLE CT_DATA WITH KEY POHEADER-PO_NUMBER = LS_POHEADER-PO_NUMBER INTO LS_DATA.
    IF SY-SUBRC = 0.
      LF_INDEX = SY-TABIX.
      APPEND LS_POITEM TO LS_DATA-POITEM.
      APPEND LS_POITEMX TO LS_DATA-POITEMX.
      APPEND LS_POSCHEDULE TO LS_DATA-POSCHEDULE.
      APPEND LS_POSCHEDULEX TO LS_DATA-POSCHEDULEX.
      APPEND LINES OF LT_POTEXTITEM TO LS_DATA-POTEXTITEM.
      APPEND LINES OF LT_EXTENSIONIN TO LS_DATA-EXTENSIONIN.
*      LS_DATA-EXTENSIONIN = LT_EXTENSIONIN.
*      LS_DATA-POTEXTITEM  = LT_POTEXTITEM.

      IF US_MESSG IS NOT INITIAL.
        LS_MESSG = US_MESSG.
        LS_MESSG-PO_NO = US_RESULT-EBELN.
        LS_MESSG-PO_ITEM = US_RESULT-EBELP.
        APPEND LS_MESSG TO LS_DATA-MESSG.
      ENDIF.
      MODIFY CT_DATA FROM LS_DATA INDEX LF_INDEX.
    ELSE.
      LS_DATA-ROWNO         = UF_ROWNO.
      LS_DATA-POHEADER      = LS_POHEADER.
      LS_DATA-POHEADERX     = LS_POHEADERX.
      LS_DATA-POACCOUNT     = LS_POACCOUNT.
      LS_DATA-POACCOUNTX    = LS_POACCOUNTX.
      LS_DATA-POACCOUNTSEG  = LS_POACCOUNTSEG.
      LS_DATA-POCONDHEADER  = LS_POCONDHEADER.
      LS_DATA-POCONDHEADERX = LS_POCONDHEADERX.
      LS_DATA-POTEXTHEADER  = LT_POTEXTHEADER.
      LS_DATA-POTEXTITEM    = LT_POTEXTITEM.
      LS_DATA-EXTENSIONIN   = LT_EXTENSIONIN.
      LS_DATA-LTEXT         = UT_LTEXT.
      APPEND LS_POITEM      TO LS_DATA-POITEM.
      APPEND LS_POITEMX     TO LS_DATA-POITEMX.
      APPEND LS_POSCHEDULE  TO LS_DATA-POSCHEDULE.
      APPEND LS_POSCHEDULEX TO LS_DATA-POSCHEDULEX.

      IF US_MESSG IS NOT INITIAL.
        LS_MESSG = US_MESSG.
        LS_MESSG-PO_NO = US_RESULT-EBELN.
        LS_MESSG-PO_ITEM = US_RESULT-EBELP.
        APPEND LS_MESSG TO LS_DATA-MESSG.
      ENDIF.
      INSERT LS_DATA INTO TABLE CT_DATA.
    ENDIF.
  ELSE.
    LS_DATA-ROWNO         = UF_ROWNO.
    LS_DATA-POHEADER      = LS_POHEADER.
    LS_DATA-POHEADERX     = LS_POHEADERX.
    LS_DATA-POACCOUNT     = LS_POACCOUNT.
    LS_DATA-POACCOUNTX    = LS_POACCOUNTX.
    LS_DATA-POACCOUNTSEG  = LS_POACCOUNTSEG.
    LS_DATA-POCONDHEADER  = LS_POCONDHEADER.
    LS_DATA-POCONDHEADERX = LS_POCONDHEADERX.
    LS_DATA-POTEXTHEADER  = LT_POTEXTHEADER.
    LS_DATA-POTEXTITEM    = LT_POTEXTITEM.
    LS_DATA-EXTENSIONIN   = LT_EXTENSIONIN.
    LS_DATA-LTEXT         = UT_LTEXT.
    APPEND LS_POITEM      TO LS_DATA-POITEM.
    APPEND LS_POITEMX     TO LS_DATA-POITEMX.
    APPEND LS_POSCHEDULE  TO LS_DATA-POSCHEDULE.
    APPEND LS_POSCHEDULEX TO LS_DATA-POSCHEDULEX.

    IF US_MESSG IS NOT INITIAL.
      LS_MESSG = US_MESSG.
      LS_MESSG-PO_NO = US_RESULT-EBELN.
      LS_MESSG-PO_ITEM = US_RESULT-EBELP.
      APPEND LS_MESSG TO LS_DATA-MESSG.
    ENDIF.
    INSERT LS_DATA INTO TABLE CT_DATA.
  ENDIF.

*  INSERT LS_DATA INTO TABLE CT_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_append_header
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_APPEND_HEADER USING US_RESULT TYPE TS_RESULT
                           UT_TEXT   TYPE TT_TEXT
                     CHANGING CS_POHEADER      TYPE TS_POHEADER
                              CS_POHEADERX     TYPE TS_POHEADERX
                              CS_POCONDHEADER  TYPE TS_POCONDHEADER
                              CS_POCONDHEADERX TYPE TS_POCONDHEADERX
                              CT_POTEXTHEADER  TYPE TT_POTEXTHEADER
                              CT_EXTENSIONIN   TYPE TT_EXTENSIONIN.

  DATA: LF_FIELDNAME TYPE CHAR35,
        LF_NUMC2     TYPE NUMC2.

  DATA: LS_POTEXTHEADER TYPE TS_POTEXTHEADER,
        LS_EXTENSIONIN  TYPE TS_EXTENSIONIN.

  DATA: LS_POHEADER TYPE ZSDSMMS037.

  FIELD-SYMBOLS:
        <L_VALUE>  TYPE ANY.

*--------------------------------------
* PO Header
*--------------------------------------
  IF US_RESULT IS NOT INITIAL.
    CS_POHEADER-PO_NUMBER   = US_RESULT-EBELN.
    CS_POHEADER-COMP_CODE   = US_RESULT-BUKRS.
    CS_POHEADER-DOC_TYPE    = US_RESULT-BSART.
    CS_POHEADER-VENDOR      = US_RESULT-LIFNR.
    CS_POHEADER-PMNTTRMS    = US_RESULT-ZTERM.
    CS_POHEADER-PURCH_ORG   = US_RESULT-EKORG.
    CS_POHEADER-PUR_GROUP   = US_RESULT-EKGRP.
    CS_POHEADER-CURRENCY    = US_RESULT-WAERS.
    CS_POHEADER-EXCH_RATE   = US_RESULT-WKURS.
    CS_POHEADER-EX_RATE_FX  = US_RESULT-KUFIX.
    CS_POHEADER-DOC_DATE    = US_RESULT-BEDAT.
    CS_POHEADER-REF_1       = US_RESULT-IHREZ.
    CS_POHEADER-OUR_REF     = US_RESULT-UNSEZ.
    CS_POHEADER-SALES_PERS  = US_RESULT-VERKF.
    CS_POHEADER-INCOTERMS1  = US_RESULT-INCO1.
    CS_POHEADER-INCOTERMS2  = US_RESULT-INCO2.
    CS_POHEADER-RETENTION_TYPE = US_RESULT-RETTP.
    CS_POHEADER-RETENTION_PERCENTAGE = US_RESULT-EKKO_RETPC.
    CS_POHEADER-ITEM_INTVL  = GC_TRUE.

    IF CS_POHEADER-PO_NUMBER IS NOT INITIAL.
      CS_POHEADERX-PO_NUMBER = GC_TRUE.
    ENDIF.
    CS_POHEADERX-COMP_CODE   = GC_TRUE.
    CS_POHEADERX-DOC_TYPE    = GC_TRUE.
    CS_POHEADERX-VENDOR      = GC_TRUE.
    IF CS_POHEADER-PMNTTRMS IS NOT INITIAL.
      CS_POHEADERX-PMNTTRMS  = GC_TRUE.
    ENDIF.
    CS_POHEADERX-PURCH_ORG   = GC_TRUE.
    CS_POHEADERX-PUR_GROUP   = GC_TRUE.
    CS_POHEADERX-CURRENCY    = GC_TRUE.
    CS_POHEADERX-EXCH_RATE   = GC_TRUE.
    CS_POHEADERX-EX_RATE_FX  = GC_TRUE.
    CS_POHEADERX-DOC_DATE    = GC_TRUE.
    CS_POHEADERX-REF_1       = GC_TRUE.
    CS_POHEADERX-OUR_REF     = GC_TRUE.
    CS_POHEADERX-SALES_PERS  = GC_TRUE.
    CS_POHEADERX-INCOTERMS1  = GC_TRUE.
    CS_POHEADERX-INCOTERMS2  = GC_TRUE.
    CS_POHEADERX-RETENTION_TYPE  = GC_TRUE.
    CS_POHEADERX-RETENTION_PERCENTAGE  = GC_TRUE.
    CS_POHEADERX-ITEM_INTVL  = GC_TRUE.

*--------------------------------------
* PO Header Condition
*--------------------------------------
    IF US_RESULT-HEADER_CON IS NOT INITIAL.
      CS_POCONDHEADER-ITM_NUMBER = '000000'.
      CS_POCONDHEADER-COND_TYPE  = 'ZVAT'.
      CS_POCONDHEADER-COND_VALUE = US_RESULT-HEADER_CON.
      CS_POCONDHEADER-COND_UNIT  = '%'.
      CS_POCONDHEADER-COND_ST_NO = '1'.
      CS_POCONDHEADER-CHANGE_ID  = 'I'.

      CS_POCONDHEADERX-ITM_NUMBER = '000000'.
      CS_POCONDHEADERX-COND_TYPE  = GC_TRUE.
      CS_POCONDHEADERX-COND_VALUE = GC_TRUE.
      CS_POCONDHEADERX-COND_UNIT  = GC_TRUE.
      CS_POCONDHEADERX-COND_ST_NO = '1'.
      CS_POCONDHEADERX-CHANGE_ID  = GC_TRUE.
    ENDIF.

*--------------------------------------
* Texts: Header
*--------------------------------------
    IF UT_TEXT IS NOT INITIAL.
      LOOP AT UT_TEXT INTO DATA(LS_TEXT).
        CLEAR LS_POTEXTHEADER.
        IF LS_TEXT-TDOBJECT EQ GC_OBJEC_HEADERTXT.
          LS_POTEXTHEADER-PO_NUMBER = US_RESULT-EBELN.
          LS_POTEXTHEADER-PO_ITEM   = '000000'.
          LS_POTEXTHEADER-TEXT_ID   = LS_TEXT-TDID.
          LS_POTEXTHEADER-TEXT_FORM = LS_TEXT-TDFORMAT.
          LS_POTEXTHEADER-TEXT_LINE = LS_TEXT-TDLINE.
          APPEND LS_POTEXTHEADER TO CT_POTEXTHEADER.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

*--------------------------------------
* Header Custom Field
*--------------------------------------
  CLEAR: LS_EXTENSIONIN.
  LS_POHEADER-ZZ_JTEPA     = US_RESULT-ZZ_JTEPA.
  LS_POHEADER-ZZ1_DIVCD_PO = US_RESULT-ZZ1_DIVCD_PO.
  LS_POHEADER-ZZ1_PRJ_C_PO = US_RESULT-ZZPRJTYP.
  LS_POHEADER-ZZ1_DLV_MODE = US_RESULT-ZZDELIVMODE.

  LS_EXTENSIONIN-STRUCTURE  = 'PO_Header'.
  LS_EXTENSIONIN-VALUEPART1 = LS_POHEADER.
  APPEND LS_EXTENSIONIN TO CT_EXTENSIONIN.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_append_item
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_APPEND_ITEM USING US_RESULT TYPE TS_RESULT
                         UT_TEXT   TYPE TT_TEXT
                   CHANGING CS_POITEM      TYPE TS_POITEM
                            CS_POITEMX     TYPE TS_POITEMX
                            CS_POSCHEDULE  TYPE TS_POSCHEDULE
                            CS_POSCHEDULEX TYPE TS_POSCHEDULEX
                            CT_POTEXTITEM  TYPE TT_POTEXTITEM
                            CT_EXTENSIONIN TYPE TT_EXTENSIONIN.

  DATA: LF_PCKG_NO     TYPE PACKNO,
        LS_POTEXTITEM  TYPE TS_POTEXTITEM,
        LS_EXTENSIONIN TYPE TS_EXTENSIONIN.

  DATA: LS_POITEM TYPE ZSDSMMS038.

  FIELD-SYMBOLS:
    <L_VALUE>  TYPE ANY.

*--------------------------------------
* PO Item
*--------------------------------------
  IF US_RESULT IS NOT INITIAL.
    CS_POITEM-PO_ITEM     = US_RESULT-EBELP.
    CS_POITEM-SHORT_TEXT  = US_RESULT-TXZ01.
    CS_POITEM-MATERIAL    = US_RESULT-MATNR.
    CS_POITEM-PLANT       = US_RESULT-WERKS.
    CS_POITEM-STGE_LOC    = US_RESULT-LGORT.
    CS_POITEM-TRACKINGNO  = US_RESULT-BEDNR.
    CS_POITEM-MATL_GROUP  = US_RESULT-MATKL.
    MOVE US_RESULT-EKPO_MENGE TO CS_POITEM-QUANTITY.
    CS_POITEM-PO_UNIT     = US_RESULT-MEINS.
    IF US_RESULT-BPRME IS NOT INITIAL.
      CS_POITEM-ORDERPR_UN  = US_RESULT-BPRME.
    ELSE.
      CS_POITEM-ORDERPR_UN  = US_RESULT-MEINS.
    ENDIF.
    CS_POITEM-CONV_NUM1     = US_RESULT-BPUMN.
    CS_POITEM-CONV_DEN1     = US_RESULT-BPUMZ.
    CS_POITEM-NET_PRICE     = US_RESULT-NETPR.
    CS_POITEM-PO_PRICE      = '2'.
    CS_POITEM-PRICE_UNIT    = US_RESULT-PEINH.
    CS_POITEM-TAX_CODE      = US_RESULT-MWSKZ.
    CS_POITEM-QUAL_INSP     = US_RESULT-INSMK.
    CS_POITEM-OVER_DLV_TOL  = US_RESULT-UEBTO.
    CS_POITEM-UNLIMITED_DLV = US_RESULT-UEBTK.
    CS_POITEM-UNDER_DLV_TOL = US_RESULT-UNTTO.
    CS_POITEM-ACCTASSCAT    = US_RESULT-KNTTP.
    CS_POITEM-ITEM_CAT      = US_RESULT-PSTYP.
    CS_POITEM-DISTRIB       = US_RESULT-VRTKZ.
    CS_POITEM-PART_INV      = US_RESULT-TWRKZ.
    CS_POITEM-GR_IND        = US_RESULT-WEPOS.
    CS_POITEM-IR_IND        = US_RESULT-REPOS.
    CS_POITEM-GR_BASEDIV    = US_RESULT-WEBRE.
    CS_POITEM-ERS           = US_RESULT-XERSY.
    CS_POITEM-CTRL_KEY      = US_RESULT-SSQSS.
    CS_POITEM-CONF_CTRL     = US_RESULT-BSTAE.
    CS_POITEM-MINREMLIFE    = US_RESULT-MHDRZ.
    CS_POITEM-PREQ_NAME     = US_RESULT-AFNAM.
    CS_POITEM-PERIOD_IND_EXPIRATION_DATE = US_RESULT-IPRKZ.
    IF  US_RESULT-KNTTP IS INITIAL.
      IF US_RESULT-FIPOS IS NOT INITIAL.
        CS_POITEM-CMMT_ITEM  = US_RESULT-FIPOS.
      ENDIF.
      IF US_RESULT-FISTL IS NOT INITIAL.
        CS_POITEM-FUNDS_CTR  = US_RESULT-FISTL.
      ENDIF.
    ENDIF.

    "For Service - add package number
    CLEAR: LF_PCKG_NO.
    IF CS_POITEM-ITEM_CAT EQ '9'. "Service
      CS_POITEM-PCKG_NO = CS_POITEM-PO_ITEM.
      LF_PCKG_NO = CS_POITEM-PCKG_NO.
    ENDIF.

    CLEAR CS_POITEMX.
    CS_POITEMX-PO_ITEM     = US_RESULT-EBELP.

    IF CS_POITEM-SHORT_TEXT IS NOT INITIAL .
      CS_POITEMX-SHORT_TEXT  = GC_TRUE.
    ENDIF.

    CS_POITEMX-MATERIAL    = GC_TRUE.
    CS_POITEMX-PLANT       = GC_TRUE.
    CS_POITEMX-STGE_LOC    = GC_TRUE.
    CS_POITEMX-TRACKINGNO  = GC_TRUE.
    CS_POITEMX-MATL_GROUP  = GC_TRUE.
    CS_POITEMX-QUANTITY    = GC_TRUE.
    CS_POITEMX-PO_UNIT     = GC_TRUE.
    CS_POITEMX-ORDERPR_UN  = GC_TRUE.
    IF CS_POITEM-CONV_NUM1 IS NOT INITIAL.
      CS_POITEMX-CONV_NUM1   = GC_TRUE.
    ENDIF.
    IF CS_POITEM-CONV_DEN1 IS NOT INITIAL.
      CS_POITEMX-CONV_DEN1   = GC_TRUE.
    ENDIF.
    CS_POITEMX-NET_PRICE     = GC_TRUE.
    CS_POITEMX-PO_PRICE      = '2'.
    CS_POITEMX-PRICE_UNIT    = GC_TRUE.
    CS_POITEMX-TAX_CODE      = GC_TRUE.
    CS_POITEMX-QUAL_INSP     = GC_TRUE.
    CS_POITEMX-OVER_DLV_TOL  = GC_TRUE.
    CS_POITEMX-UNLIMITED_DLV = GC_TRUE.
    CS_POITEMX-UNDER_DLV_TOL = GC_TRUE.
    CS_POITEMX-ACCTASSCAT    = GC_TRUE.
    CS_POITEMX-ITEM_CAT      = GC_TRUE.
    CS_POITEMX-DISTRIB       = GC_TRUE.
    CS_POITEMX-PART_INV      = GC_TRUE.
    CS_POITEMX-GR_IND        = GC_TRUE.
    CS_POITEMX-IR_IND        = GC_TRUE.
    CS_POITEMX-GR_BASEDIV    = GC_TRUE.
    CS_POITEMX-CTRL_KEY      = GC_TRUE.
    CS_POITEMX-MINREMLIFE    = GC_TRUE.
    CS_POITEMX-PREQ_NAME     = GC_TRUE.
    CS_POITEMX-PERIOD_IND_EXPIRATION_DATE = GC_TRUE.
    IF  US_RESULT-KNTTP IS INITIAL.
      IF US_RESULT-FIPOS IS NOT INITIAL.
        CS_POITEMX-CMMT_ITEM  = GC_TRUE.
      ENDIF.
      IF US_RESULT-FISTL IS NOT INITIAL.
        CS_POITEMX-FUNDS_CTR  = GC_TRUE.
      ENDIF.
    ENDIF.

    "For Service - add package number
    CLEAR: LF_PCKG_NO.
    IF CS_POITEM-ITEM_CAT EQ '9'. "Service
      CS_POITEMX-PCKG_NO   = GC_TRUE.
    ENDIF.

*--------------------------------------
* PO Schedule Line
*--------------------------------------
    CLEAR CS_POSCHEDULE.
    CS_POSCHEDULE-PO_ITEM       = US_RESULT-EBELP.
    CS_POSCHEDULE-SCHED_LINE    = '0001'.
    CS_POSCHEDULE-DELIVERY_DATE = US_RESULT-EINDT.
    MOVE US_RESULT-EKPO_MENGE TO CS_POSCHEDULE-QUANTITY.

    CLEAR CS_POSCHEDULEX.
    CS_POSCHEDULEX-PO_ITEM       = US_RESULT-EBELP.
    CS_POSCHEDULEX-SCHED_LINE    = '0001'.
    CS_POSCHEDULEX-DELIVERY_DATE = GC_TRUE.
    CS_POSCHEDULEX-QUANTITY      = GC_TRUE.

*--------------------------------------
* Texts: Items
*--------------------------------------

    LOOP AT UT_TEXT INTO DATA(LS_TEXT).
      CLEAR LS_POTEXTITEM.
      IF LS_TEXT-TDOBJECT = GC_OBJEC_ITEMTXT.
        LS_POTEXTITEM-PO_NUMBER = US_RESULT-EBELN.
        LS_POTEXTITEM-PO_ITEM   = US_RESULT-EBELP.
        LS_POTEXTITEM-TEXT_ID   = LS_TEXT-TDID.
        LS_POTEXTITEM-TEXT_FORM = LS_TEXT-TDFORMAT.
        LS_POTEXTITEM-TEXT_LINE = LS_TEXT-TDLINE.

        APPEND LS_POTEXTITEM TO CT_POTEXTITEM.
      ENDIF.
    ENDLOOP.

*--------------------------------------
* Item Custom Field
*--------------------------------------
    CLEAR: LS_EXTENSIONIN.
    LS_POITEM-ZZ1_LOB_PO_PDI  = US_RESULT-ZZ1_LOB_PO_PDI.
    LS_POITEM-ZZ1_DES_PT_C_PO = US_RESULT-ZZ1_DES_PT_C_PO.

    LS_EXTENSIONIN-STRUCTURE  = 'PO_Item'.  " Structure for header data
    LS_EXTENSIONIN-VALUEPART1 = LS_POITEM.
    APPEND LS_EXTENSIONIN TO CT_EXTENSIONIN.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_append_acct
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_APPEND_ACCT USING US_RESULT TYPE TS_RESULT
                   CHANGING CS_POACCOUNT    TYPE TS_POACCOUNT
                            CS_POACCOUNTX   TYPE TS_POACCOUNTX
                            CS_POACCOUNTSEG TYPE TS_POACCOUNTSEG.

*--------------------------------------
* PO Account Assignment
*--------------------------------------
  CHECK US_RESULT-ZEKKN IS NOT INITIAL.

  CLEAR CS_POACCOUNT.
  CS_POACCOUNT-SD_DOC     = US_RESULT-VBELN.
  CS_POACCOUNT-PO_ITEM    = US_RESULT-VBELP.
  CS_POACCOUNT-SERIAL_NO  = US_RESULT-ZEKKN.
  CS_POACCOUNT-QUANTITY   = US_RESULT-MENGE.
  CS_POACCOUNT-GL_ACCOUNT = US_RESULT-SAKTO.
  CS_POACCOUNT-COSTCENTER = US_RESULT-KOSTL.
  CS_POACCOUNT-ASSET_NO   = US_RESULT-ANLN1.
  CS_POACCOUNT-SUB_NUMBER = US_RESULT-ANLN2.
  CS_POACCOUNT-ORDERID    = US_RESULT-AUFNR.
  CS_POACCOUNT-CMMT_ITEM  = US_RESULT-FIPOS.

  CS_POACCOUNT-FUNDS_CTR  = US_RESULT-FISTL.
  CS_POACCOUNT-PROFIT_CTR = US_RESULT-PRCTR.

  CS_POACCOUNT-WBS_ELEMENT  = US_RESULT-PS_PSP_PNR.

  CLEAR CS_POACCOUNTX.
  CS_POACCOUNTX-SD_DOC     = GC_TRUE.
  CS_POACCOUNTX-PO_ITEM    = US_RESULT-VBELP.
  CS_POACCOUNTX-SERIAL_NO  = US_RESULT-ZEKKN.
  IF CS_POACCOUNT-QUANTITY IS NOT INITIAL .
    CS_POACCOUNTX-QUANTITY   = GC_TRUE.
  ENDIF.

  IF CS_POACCOUNT-GL_ACCOUNT IS NOT INITIAL.
    CS_POACCOUNTX-GL_ACCOUNT = GC_TRUE.
  ENDIF.

  IF CS_POACCOUNT-COSTCENTER IS NOT INITIAL.
    CS_POACCOUNTX-COSTCENTER = GC_TRUE.
  ENDIF.

  IF CS_POACCOUNT-ASSET_NO IS NOT INITIAL.
    CS_POACCOUNTX-ASSET_NO   = GC_TRUE.
  ENDIF.

  IF CS_POACCOUNT-SUB_NUMBER IS NOT INITIAL.
    CS_POACCOUNTX-SUB_NUMBER = GC_TRUE.
  ENDIF.

  IF CS_POACCOUNT-ORDERID IS NOT INITIAL.
    CS_POACCOUNTX-ORDERID    = GC_TRUE.
  ENDIF.

  IF CS_POACCOUNT-CMMT_ITEM IS NOT INITIAL.
    CS_POACCOUNTX-CMMT_ITEM  = GC_TRUE.
  ENDIF.

  IF CS_POACCOUNT-FUNDS_CTR IS NOT INITIAL.
    CS_POACCOUNTX-FUNDS_CTR  = GC_TRUE.
  ENDIF.

  IF CS_POACCOUNT-PROFIT_CTR IS NOT INITIAL.
    CS_POACCOUNTX-PROFIT_CTR = GC_TRUE.
  ENDIF.

  IF CS_POACCOUNT-WBS_ELEMENT IS NOT INITIAL.
    CS_POACCOUNTX-WBS_ELEMENT = GC_TRUE.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_upload_file
*----------------------------------------------------------------------*
*  Upload File
*----------------------------------------------------------------------*
FORM F_UPLOAD_FILE  USING  UT_DATA   TYPE  TT_DATA
                           UF_TEST   TYPE  FLAG
                  CHANGING CT_RESULT TYPE  TT_RESULT
                           CS_SUM    TYPE  TS_SUM.

  DATA: LT_MESSG TYPE TT_MESSG.

  DATA: LS_DATA TYPE TS_DATA.

  DATA: LF_ERROR TYPE FLAG.

  DATA: LF_SUCC TYPE STRING,
        LF_ERR  TYPE STRING.

  DATA: LS_RESULT TYPE TS_RESULT.

* Show Progress
* Text-p03 : Uploading file data. . .
  MC_SHOW_PROGRESS 45 TEXT-P03.

  LOOP AT UT_DATA INTO LS_DATA.

    IF LS_DATA-MESSG[] IS INITIAL.
      PERFORM F_BAPI_CREATE USING LS_DATA
                                  UF_TEST
                            CHANGING LT_MESSG
                                     LF_ERROR.

      LS_DATA-MESSG = LT_MESSG.
    ELSE.
      LF_ERROR = GC_TRUE.
    ENDIF.

    CS_SUM-TOTAL = CS_SUM-TOTAL + 1.

* Collect Result
    PERFORM F_COLLECT_RESULT USING LS_DATA
                             CHANGING CT_RESULT.

  ENDLOOP.

  PERFORM F_COUNT_RESULT  USING CT_RESULT
                          CHANGING CS_SUM.

  LF_SUCC = CS_SUM-SUCCS.
  LF_ERR = CS_SUM-ERROR.
  CONCATENATE 'Processing complete with' LF_SUCC 'success and' LF_ERR 'error' INTO DATA(LF_TEXT) SEPARATED BY SPACE.
*  Show Final Message for processing completed
  MESSAGE LF_TEXT TYPE 'S'.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_bapi_create
*----------------------------------------------------------------------*
*  Create Outstanding PO
*----------------------------------------------------------------------*
FORM F_BAPI_CREATE  USING  US_DATA  TYPE  TS_DATA
                           UF_TEST  TYPE  FLAG
                    CHANGING CT_TEST  TYPE  TT_MESSG
                             CF_ERROR TYPE  FLAG.
  DATA: LT_RETURN TYPE TABLE OF BAPIRET2.

  DATA: LT_POITEM        TYPE TABLE OF BAPIMEPOITEM,
        LT_POITEMX       TYPE TABLE OF BAPIMEPOITEMX,
        LT_POSCHEDULE    TYPE TABLE OF BAPIMEPOSCHEDULE,
        LT_POSCHEDULEX   TYPE TABLE OF BAPIMEPOSCHEDULX,
        LT_POACCOUNT     TYPE TABLE OF BAPIMEPOACCOUNT,
        LT_POACCOUNTX    TYPE TABLE OF BAPIMEPOACCOUNTX,
        LT_POACCOUNTSEG  TYPE TABLE OF BAPIMEPOACCOUNTPROFITSEGMENT,
        LT_POTEXTHEADER  TYPE TABLE OF BAPIMEPOTEXTHEADER,
        LT_POTEXTITEM    TYPE TABLE OF BAPIMEPOTEXT,
        LT_EXTENSIONIN   TYPE TABLE OF BAPIPAREX,
        LT_POCONDHEADER  TYPE TABLE OF BAPIMEPOCONDHEADER,
        LT_POCONDHEADERX TYPE TABLE OF BAPIMEPOCONDHEADERX,
        LS_MESSG         TYPE TS_MESSG.

  FIELD-SYMBOLS:
    <L_RETURN>  TYPE  BAPIRET2.

*  Initial Output
  REFRESH: CT_TEST.
  CLEAR: CF_ERROR.

  APPEND US_DATA-POACCOUNT TO LT_POACCOUNT.
  APPEND US_DATA-POACCOUNTX TO LT_POACCOUNTX.
  APPEND US_DATA-POACCOUNTSEG TO LT_POACCOUNTSEG.
  APPEND US_DATA-POCONDHEADER TO LT_POCONDHEADER.
  APPEND US_DATA-POCONDHEADERX TO LT_POCONDHEADERX.


  CALL FUNCTION 'BAPI_PO_CREATE1'
    EXPORTING
      POHEADER               = US_DATA-POHEADER
      POHEADERX              = US_DATA-POHEADERX
      TESTRUN                = UF_TEST
      NO_PRICE_FROM_PO       = 'X'
    TABLES
      RETURN                 = LT_RETURN
*     POITEM                 = LT_POITEM
      POITEM                 = US_DATA-POITEM
*     POITEMX                = LT_POITEMX
      POITEMX                = US_DATA-POITEMX
      POSCHEDULE             = US_DATA-POSCHEDULE
      POSCHEDULEX            = US_DATA-POSCHEDULEX
      POACCOUNT              = LT_POACCOUNT
      POACCOUNTPROFITSEGMENT = LT_POACCOUNTSEG
      POACCOUNTX             = LT_POACCOUNTX
      POCONDHEADER           = LT_POCONDHEADER
      POCONDHEADERX          = LT_POCONDHEADERX
      POTEXTHEADER           = US_DATA-POTEXTHEADER
      POTEXTITEM             = US_DATA-POTEXTITEM
      EXTENSIONIN            = US_DATA-EXTENSIONIN.

* MEPO 000 Purchase order still contains faulty items
* MEPO 001 Purchase order item & still contains faulty schedule lines
* MEPO 002 PO header data still faulty
  DELETE LT_RETURN WHERE
    ( TYPE EQ 'E' AND ID EQ 'BAPI' AND NUMBER EQ '001' ) OR
    ( TYPE EQ 'E' AND ID EQ 'MEPO' AND NUMBER EQ '000' ) OR
    ( TYPE EQ 'E' AND ID EQ 'MEPO' AND NUMBER EQ '001' ) OR
    ( TYPE EQ 'E' AND ID EQ 'MEPO' AND NUMBER EQ '002' ).

  LOOP AT LT_RETURN ASSIGNING <L_RETURN>
                    WHERE TYPE = 'E'
                    OR    TYPE = 'A'.
    CLEAR: LS_MESSG.
    LS_MESSG-MSGTY = <L_RETURN>-TYPE.
    LS_MESSG-MSGTX = <L_RETURN>-MESSAGE.
    INSERT LS_MESSG INTO TABLE CT_TEST.
    EXIT.
  ENDLOOP.
  IF SY-SUBRC NE 0.
    IF UF_TEST EQ GC_TRUE.
      CLEAR LS_MESSG.
      LS_MESSG-MSGTY = 'S'.
*     Text-i01: Test upload successfully.
      LS_MESSG-MSGTX = TEXT-I01.
      INSERT LS_MESSG INTO TABLE CT_TEST.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ELSE.
      CLEAR LS_MESSG.
      LS_MESSG-MSGTY = 'S'.
*     Text-i02: PO posted successfully.
      LS_MESSG-MSGTX = TEXT-I02.
      INSERT LS_MESSG INTO TABLE CT_TEST.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = GC_TRUE.
    ENDIF.
  ELSE.
    CF_ERROR = GC_TRUE.
*   Rollback Work
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Form f_collect_result
*----------------------------------------------------------------------*
*  Collect Result
*----------------------------------------------------------------------*
FORM F_COLLECT_RESULT  USING  US_DATA   TYPE  TS_DATA
                     CHANGING CT_RESULT TYPE  TT_RESULT.

  DATA:
    LS_RESULT      TYPE TS_RESULT,
    LF_INDEX       TYPE I,
*    LS_ITEM        TYPE TS_POITEM,
    LS_SCHEDULE    TYPE TS_POSCHEDULE,
    LS_EXTENSIONIN TYPE TS_EXTENSIONIN,
    LS_MESSG       TYPE TS_MESSG,
    LS_TEXTITEM    TYPE TS_POTEXTITEM,
    LS_SUM         TYPE TS_SUM.

  CLEAR: LS_RESULT.

  LOOP AT US_DATA-POITEM INTO DATA(LS_ITEM).
    READ TABLE GT_RESULT WITH KEY EBELN = US_DATA-POHEADER-PO_NUMBER
                                  EBELP = LS_ITEM-PO_ITEM
                         INTO LS_RESULT.
    IF SY-SUBRC EQ 0.
      LF_INDEX = SY-TABIX.
      LOOP AT US_DATA-MESSG INTO LS_MESSG.
        IF LS_MESSG-PO_ITEM = LS_ITEM-PO_ITEM.
          LS_RESULT-MSGTY = LS_MESSG-MSGTY.
          LS_RESULT-MSGTX = LS_MESSG-MSGTX.

          MODIFY GT_RESULT FROM LS_RESULT INDEX LF_INDEX.
        ELSEIF LS_MESSG-PO_ITEM IS INITIAL.
          LS_RESULT-MSGTY = LS_MESSG-MSGTY.
          LS_RESULT-MSGTX = LS_MESSG-MSGTX.

          MODIFY GT_RESULT FROM LS_RESULT INDEX LF_INDEX.
        ENDIF.

      ENDLOOP.
      PERFORM F_COLLECT_HEADER_TEXT USING US_DATA-POTEXTHEADER
                                    CHANGING LS_RESULT.

      PERFORM F_COLLECT_ITEM_TEXT USING US_DATA-POTEXTITEM
                                        LS_ITEM-PO_ITEM
                                  CHANGING LS_RESULT.

      MODIFY GT_RESULT FROM LS_RESULT INDEX LF_INDEX.
    ENDIF.
  ENDLOOP.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_collect_item_text
*----------------------------------------------------------------------*
*  collect header text
*----------------------------------------------------------------------*
FORM F_COLLECT_HEADER_TEXT  USING  CS_TEXTHEADER  TYPE  TS_DATA-POTEXTHEADER
                    CHANGING CS_RESULT TYPE  TS_RESULT.

  DATA:
*    LF_OBJEC TYPE TS_TEXT-TDOBJECT,
    LF_TXTID TYPE TS_POTEXTHEADER-TEXT_ID,
    LF_SEQNO TYPE I.


  FIELD-SYMBOLS:
    <L_TEXTHEADER> TYPE TS_POTEXTHEADER.

  LOOP AT CS_TEXTHEADER ASSIGNING <L_TEXTHEADER>.
    IF <L_TEXTHEADER>-TEXT_ID NE LF_TXTID.
      LF_TXTID = <L_TEXTHEADER>-TEXT_ID.
      CLEAR LF_SEQNO.
    ENDIF.
    LF_SEQNO = LF_SEQNO + 1.
    CASE <L_TEXTHEADER>-TEXT_ID.

      WHEN 'F00'.
        CASE LF_SEQNO.
          WHEN 1.
            CS_RESULT-ZZREMARKS01 = <L_TEXTHEADER>-TEXT_LINE.
          WHEN 2.
            CS_RESULT-ZZREMARKS02 = <L_TEXTHEADER>-TEXT_LINE.
          WHEN 3.
            CS_RESULT-ZZREMARKS03 = <L_TEXTHEADER>-TEXT_LINE.
          WHEN 4.
            CS_RESULT-ZZREMARKS04 = <L_TEXTHEADER>-TEXT_LINE.
          WHEN 5.
            CS_RESULT-ZZREMARKS05 = <L_TEXTHEADER>-TEXT_LINE.
        ENDCASE.
      WHEN 'F01'.
        CASE LF_SEQNO.
          WHEN 1.
            CS_RESULT-FROM_LINE1 = <L_TEXTHEADER>-TEXT_LINE.
          WHEN 2.
            CS_RESULT-FROM_LINE2 = <L_TEXTHEADER>-TEXT_LINE.
          WHEN 3.
            CS_RESULT-FROM_LINE3 = <L_TEXTHEADER>-TEXT_LINE.
        ENDCASE.
    ENDCASE.

  ENDLOOP.
ENDFORM.
*----------------------------------------------------------------------*
*  Form f_collect_item_text
*----------------------------------------------------------------------*
*  collect item text
*----------------------------------------------------------------------*
FORM F_COLLECT_ITEM_TEXT  USING  CS_TEXTITEM  TYPE  TS_DATA-POTEXTITEM
                                 CF_POITEM TYPE EKPO-EBELP
                    CHANGING CS_RESULT TYPE  TS_RESULT.
  DATA:
*    LF_OBJEC TYPE TS_TEXT-TDOBJECT,
    LF_TXTID TYPE TS_POTEXTITEM-TEXT_ID,
    LF_SEQNO TYPE I.

  FIELD-SYMBOLS:
    <L_TEXTITEM> TYPE TS_POTEXTITEM.

  LOOP AT CS_TEXTITEM ASSIGNING <L_TEXTITEM> WHERE PO_ITEM = CF_POITEM.
    IF <L_TEXTITEM>-TEXT_ID NE LF_TXTID.
      LF_TXTID = <L_TEXTITEM>-TEXT_ID.
      CLEAR LF_SEQNO.
    ENDIF.

    LF_SEQNO = LF_SEQNO + 1.
    CASE <L_TEXTITEM>-TEXT_ID.
      WHEN 'F00'.
        CASE LF_SEQNO.
          WHEN 1.
            CS_RESULT-REMARK_LINE1 = <L_TEXTITEM>-TEXT_LINE.
          WHEN 2.
            CS_RESULT-REMARK_LINE2 = <L_TEXTITEM>-TEXT_LINE.
          WHEN 3.
            CS_RESULT-REMARK_LINE3 = <L_TEXTITEM>-TEXT_LINE.
        ENDCASE.
      WHEN 'F01'.
        CASE LF_SEQNO.
          WHEN 1.
            CS_RESULT-ITEM_LINE1 = <L_TEXTITEM>-TEXT_LINE.
          WHEN 2.
            CS_RESULT-ITEM_LINE2 = <L_TEXTITEM>-TEXT_LINE.
          WHEN 3.
            CS_RESULT-ITEM_LINE3 = <L_TEXTITEM>-TEXT_LINE.
        ENDCASE.
      WHEN 'F50'.
        CASE LF_SEQNO.
          WHEN 1.
            CS_RESULT-INTERNAL_LINE1 = <L_TEXTITEM>-TEXT_LINE.
          WHEN 2.
            CS_RESULT-INTERNAL_LINE2 = <L_TEXTITEM>-TEXT_LINE.
          WHEN 3.
            CS_RESULT-INTERNAL_LINE3 = <L_TEXTITEM>-TEXT_LINE.
        ENDCASE.
      WHEN 'F51'.
        CASE LF_SEQNO.
          WHEN 1.
            CS_RESULT-PROJECT_LINE1 = <L_TEXTITEM>-TEXT_LINE.
          WHEN 2.
            CS_RESULT-PROJECT_LINE2 = <L_TEXTITEM>-TEXT_LINE.
          WHEN 3.
            CS_RESULT-PROJECT_LINE3 = <L_TEXTITEM>-TEXT_LINE.
        ENDCASE.
      WHEN 'F52'.
        CASE LF_SEQNO.
          WHEN 1.
            CS_RESULT-ATTENTION_LINE1 = <L_TEXTITEM>-TEXT_LINE.
          WHEN 2.
            CS_RESULT-ATTENTION_LINE2 = <L_TEXTITEM>-TEXT_LINE.
          WHEN 3.
            CS_RESULT-ATTENTION_LINE3 = <L_TEXTITEM>-TEXT_LINE.
        ENDCASE.
      WHEN 'F56'.
        CASE LF_SEQNO.
          WHEN 1.
            CS_RESULT-PERSON_LINE1 = <L_TEXTITEM>-TEXT_LINE.
          WHEN 2.
            CS_RESULT-PERSON_LINE2 = <L_TEXTITEM>-TEXT_LINE.
          WHEN 3.
            CS_RESULT-PERSON_LINE3 = <L_TEXTITEM>-TEXT_LINE.
        ENDCASE.
      WHEN 'F73'.
        CASE LF_SEQNO.
          WHEN 1.
            CS_RESULT-WARR_LINE1 = <L_TEXTITEM>-TEXT_LINE.
          WHEN 2.
            CS_RESULT-WARR_LINE2 = <L_TEXTITEM>-TEXT_LINE.
          WHEN 3.
            CS_RESULT-WARR_LINE3 = <L_TEXTITEM>-TEXT_LINE.
        ENDCASE.
      WHEN 'F74'.
        CASE LF_SEQNO.
          WHEN 1.
            CS_RESULT-ENGINEER_LINE1 = <L_TEXTITEM>-TEXT_LINE.
          WHEN 2.
            CS_RESULT-ENGINEER_LINE2 = <L_TEXTITEM>-TEXT_LINE.
          WHEN 3.
            CS_RESULT-ENGINEER_LINE3 = <L_TEXTITEM>-TEXT_LINE.
        ENDCASE.
      WHEN 'F75'.
        CASE LF_SEQNO.
          WHEN 1.
            CS_RESULT-MEMO_LINE1 = <L_TEXTITEM>-TEXT_LINE.
          WHEN 2.
            CS_RESULT-MEMO_LINE2 = <L_TEXTITEM>-TEXT_LINE.
          WHEN 3.
            CS_RESULT-MEMO_LINE3 = <L_TEXTITEM>-TEXT_LINE.
        ENDCASE.
    ENDCASE.
  ENDLOOP.
ENDFORM.
*----------------------------------------------------------------------*
*  Form f_count_result
*----------------------------------------------------------------------*
*  count number of success and error
*----------------------------------------------------------------------*
FORM F_COUNT_RESULT USING CT_RESULT TYPE TT_RESULT
                    CHANGING CS_SUM TYPE TS_SUM.

  DATA: LS_RESULT TYPE TS_RESULT.


  LOOP AT GT_RESULT INTO LS_RESULT.
*    Blank means this item passing validate but others item did not
    IF LS_RESULT-MSGTY IS INITIAL.
      LS_RESULT-MSGTY = 'E'.
      LS_RESULT-MSGTX = 'Validate pass'.
      MODIFY GT_RESULT FROM LS_RESULT INDEX SY-TABIX.
    ENDIF.
    IF LS_RESULT-MSGTY EQ 'S'.
      CS_SUM-SUCCS = CS_SUM-SUCCS + 1.
    ELSE.
      CS_SUM-ERROR = CS_SUM-ERROR + 1.
    ENDIF.
  ENDLOOP.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_assign_logfile
*----------------------------------------------------------------------*
*  Assign Log filename
*----------------------------------------------------------------------*
FORM F_ASSIGN_LOGFILE  USING  CF_DIR  TYPE  CLIKE
                   CHANGING CS_LOGFILE TYPE  TS_FILE_INFO.

* Initialize Output
  CLEAR: CS_LOGFILE.

  CS_LOGFILE-DIRECTORY = CF_DIR.

* File name format:  Upload_Info_Record_Log_YYYYMMDD_HHMMSS.txt
  CONCATENATE 'Upload_Outstanding_PO_Log_'  ##NO_TEXT
               SY-DATLO
               '_'
               SY-TIMLO
               '.txt'
         INTO CS_LOGFILE-FILENAME.

* Assign Fullname
  CONCATENATE CS_LOGFILE-DIRECTORY
              '\'
              CS_LOGFILE-FILENAME
         INTO CS_LOGFILE-FULLNAME.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_create_logfile
*----------------------------------------------------------------------*
*  Generate Log File
*----------------------------------------------------------------------*
FORM F_CREATE_LOGFILE  USING  US_SUM TYPE TS_SUM
                              CS_LOGFILE  TYPE  TS_FILE_INFO
                              UT_RESULT TYPE  TT_RESULT.
  DATA:
        LT_LOG   TYPE  TT_RAW.

  DATA:
        LF_TEXT TYPE STRING.

  DATA:
    LF_SUCC TYPE STRING,
    LF_ERR  TYPE STRING.

* Convert Result into Text table
  PERFORM F_CONVERT_RESULT_TO_TEXT  USING  UT_RESULT
                                    CHANGING LT_LOG.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      FILENAME                = CS_LOGFILE-FULLNAME
      FILETYPE                = 'ASC'
      WRITE_FIELD_SEPARATOR   = 'X'
      TRUNC_TRAILING_BLANKS   = 'X'
      CODEPAGE                = '4103'
      WRITE_BOM               = 'X'
    TABLES
      DATA_TAB                = LT_LOG
    EXCEPTIONS
      FILE_WRITE_ERROR        = 1
      NO_BATCH                = 2
      GUI_REFUSE_FILETRANSFER = 3
      INVALID_TYPE            = 4
      NO_AUTHORITY            = 5
      UNKNOWN_ERROR           = 6
      HEADER_NOT_ALLOWED      = 7
      SEPARATOR_NOT_ALLOWED   = 8
      FILESIZE_NOT_ALLOWED    = 9
      HEADER_TOO_LONG         = 10
      DP_ERROR_CREATE         = 11
      DP_ERROR_SEND           = 12
      DP_ERROR_WRITE          = 13
      UNKNOWN_DP_ERROR        = 14
      ACCESS_DENIED           = 15
      DP_OUT_OF_MEMORY        = 16
      DISK_FULL               = 17
      DP_TIMEOUT              = 18
      FILE_NOT_FOUND          = 19
      DATAPROVIDER_EXCEPTION  = 20
      CONTROL_FLUSH_ERROR     = 21
      OTHERS                  = 22.
  IF SY-SUBRC EQ 0.
    LF_SUCC = US_SUM-SUCCS.
    LF_ERR = US_SUM-ERROR.
    CONCATENATE 'Log file' CS_LOGFILE-FILENAME  'has been created with' LF_SUCC 'success and' LF_ERR 'error'
    INTO LF_TEXT
    SEPARATED BY SPACE.
    MESSAGE LF_TEXT TYPE 'S'.
    RETURN.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_convert_result_to_text
*----------------------------------------------------------------------*
*  Convert Result into Text table
*----------------------------------------------------------------------*
FORM F_CONVERT_RESULT_TO_TEXT  USING  UT_RESULT TYPE TT_RESULT
                               CHANGING CT_RAW  TYPE TT_RAW.

  DATA:
    LS_LOG TYPE  TS_LOG,
    LS_RAW TYPE  TS_RAW.

  DATA:
  LF_INDEX TYPE  I.

  FIELD-SYMBOLS:
    <L_RESULT> TYPE  TS_RESULT,
    <L_FIELD>  TYPE  CLIKE.

* Initialize Output
  REFRESH: CT_RAW.

* Insert Header Section of Log
  PERFORM F_INSERT_LOG_HEADER CHANGING CT_RAW.

  LOOP AT UT_RESULT ASSIGNING <L_RESULT>.



    PERFORM F_ASSIGN_LOG  USING  <L_RESULT>
                          CHANGING LS_LOG.

*   Change date format from YYYYMMDD to DD.MM.YYYY
    CONCATENATE LS_LOG-BEDAT+6(2) LS_LOG-BEDAT+4(2) LS_LOG-BEDAT+0(4) INTO LS_LOG-BEDAT SEPARATED BY '.'.
    CONCATENATE LS_LOG-EINDT+6(2) LS_LOG-EINDT+4(2) LS_LOG-EINDT+0(4) INTO LS_LOG-EINDT SEPARATED BY '.'.

    DO.
      LF_INDEX = SY-INDEX.
      ASSIGN COMPONENT LF_INDEX OF STRUCTURE LS_LOG TO <L_FIELD>.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
      CONDENSE <L_FIELD>.
      IF LF_INDEX EQ 1.
        LS_RAW-TLINE = <L_FIELD>.
      ELSE.
        CONCATENATE LS_RAW-TLINE
                    <L_FIELD>
               INTO LS_RAW-TLINE
          SEPARATED BY GC_SPLIT.
      ENDIF.
    ENDDO.

    INSERT LS_RAW INTO TABLE CT_RAW.

  ENDLOOP.

ENDFORM.
*----------------------------------------------------------------------*
*  Form f_insert_log_header
*----------------------------------------------------------------------*
*  Insert Header Section of Log file
*----------------------------------------------------------------------*
FORM F_INSERT_LOG_HEADER  CHANGING CT_RAW TYPE TT_RAW.
  DATA:
    LT_DESC   TYPE STANDARD TABLE OF  DFIES,
    LT_FIELDS TYPE  ABAP_COMPDESCR_TAB.

  DATA:
    LREF_DESCR TYPE  REF TO CL_ABAP_TYPEDESCR,
    LREF_STRUC TYPE  REF TO CL_ABAP_STRUCTDESCR.

  DATA:
    LS_RAW  TYPE  TS_RAW.

  DATA:
    LF_TEXT  TYPE  TEXT50,
    LF_TEMP1 TYPE  TEXT50,
    LF_TEMP2 TYPE  TEXT50,
    LF_INDEX TYPE  I.

  FIELD-SYMBOLS:
    <L_DESC>  TYPE  DFIES,
    <L_FIELD> TYPE  ABAP_COMPDESCR.


* *********************
* File Header
* *********************
  CONCATENATE SY-REPID SY-TITLE
         INTO LF_TEXT
    SEPARATED BY '-'.
* Text-h01 : Program:
  CONCATENATE TEXT-H01
              LF_TEXT
         INTO LS_RAW-TLINE
    SEPARATED BY GC_SPLIT.
  INSERT LS_RAW INTO TABLE CT_RAW.

  CONCATENATE SY-DATLO+6(2) SY-DATLO+4(2) SY-DATLO+0(4) INTO LF_TEMP1 SEPARATED BY '.'.
  WRITE SY-TIMLO TO LF_TEMP2.
  CONCATENATE LF_TEMP1 LF_TEMP2
         INTO LF_TEXT
    SEPARATED BY SPACE.
* Text-h02 : Processing Date/Time:
  CONCATENATE TEXT-H02
              LF_TEXT
         INTO LS_RAW-TLINE
    SEPARATED BY GC_SPLIT.
  INSERT LS_RAW INTO TABLE CT_RAW.

  CONCATENATE SY-SYSID SY-MANDT
         INTO LF_TEXT
    SEPARATED BY '/'.
* Text-h03 : System/Client:
  CONCATENATE TEXT-H03
              LF_TEXT
         INTO LS_RAW-TLINE
    SEPARATED BY GC_SPLIT.
  INSERT LS_RAW INTO TABLE CT_RAW.

* Text-h04 : Process By:
  CONCATENATE TEXT-H04
              SY-UNAME
         INTO LS_RAW-TLINE
    SEPARATED BY GC_SPLIT.
  INSERT LS_RAW INTO TABLE CT_RAW.

  WRITE GS_SUM-TOTAL TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
* Text-h05 : Total Records:
  CONCATENATE TEXT-H05
              LF_TEXT
         INTO LS_RAW-TLINE
    SEPARATED BY GC_SPLIT.
  INSERT LS_RAW INTO TABLE CT_RAW.

  WRITE GS_SUM-SUCCS TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
* Text-h06 : Success Records:
  CONCATENATE TEXT-H06
              LF_TEXT
         INTO LS_RAW-TLINE
    SEPARATED BY GC_SPLIT.
  INSERT LS_RAW INTO TABLE CT_RAW.

  WRITE GS_SUM-ERROR TO LF_TEXT LEFT-JUSTIFIED.
  CONDENSE LF_TEXT NO-GAPS.
* Text-h07 : Failed Records:
  CONCATENATE TEXT-H07
              LF_TEXT
         INTO LS_RAW-TLINE
    SEPARATED BY GC_SPLIT.
  INSERT LS_RAW INTO TABLE CT_RAW.

* *********************
* Generate Column header
* *********************

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      TABNAME        = GC_STRUCTURE_1
    TABLES
      DFIES_TAB      = LT_DESC
    EXCEPTIONS
      NOT_FOUND      = 1
      INTERNAL_ERROR = 2
      OTHERS         = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
*  SORT LT_FIELDCAT BY FIELDNAME ASCENDING.
  SORT LT_DESC BY FIELDNAME ASCENDING.

* Get List of LOG File fields
  LREF_DESCR = CL_ABAP_TYPEDESCR=>DESCRIBE_BY_NAME( 'TS_LOG' ).
  LREF_STRUC ?= LREF_DESCR.
  LT_FIELDS = LREF_STRUC->COMPONENTS.

  CLEAR LS_RAW.
  CLEAR LF_INDEX.
  LOOP AT LT_FIELDS ASSIGNING <L_FIELD>.

    LF_INDEX = SY-TABIX.

    CLEAR LF_TEXT.
    READ TABLE LT_DESC ASSIGNING <L_DESC>
                           WITH KEY FIELDNAME = <L_FIELD>-NAME
                           BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      LF_TEXT = <L_DESC>-FIELDTEXT.
    ENDIF.

    CASE <L_FIELD>-NAME.
      WHEN 'MSGTY'.
        LF_TEXT = 'Message Type'.
      WHEN 'MSGTX'.
        LF_TEXT = 'Message text'.
    ENDCASE.

    IF LF_INDEX EQ 1.
      LS_RAW-TLINE = LF_TEXT.
    ELSE.
      CONCATENATE LS_RAW-TLINE
                  LF_TEXT
             INTO LS_RAW-TLINE
        SEPARATED BY GC_SPLIT.
    ENDIF.

  ENDLOOP.
  INSERT LS_RAW INTO TABLE CT_RAW.
ENDFORM.
*----------------------------------------------------------------------*
*  Form f_assign_log
*----------------------------------------------------------------------*
*  Assign Data to Log Structure
*----------------------------------------------------------------------*
FORM F_ASSIGN_LOG  USING  US_RESULT  TYPE  TS_RESULT
                 CHANGING CS_LOG     TYPE  TS_LOG.

* Initialize Output
  CLEAR: CS_LOG.

  MOVE-CORRESPONDING US_RESULT TO CS_LOG.

ENDFORM.
