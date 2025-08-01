*&---------------------------------------------------------------------*
*& Report ZSDSFIR0400
*  Creation Date      : 28.07.2024
*  Author             : Wantanee Prateep na thalang
*  Add-on ID          :
*  Description        : Upload,Input,Delete and Lock Data statement bank
*  Purpose            :
*  Copied from        : ZFIAP_STATEMENT_BANK
*  Restriction        :
*&---------------------------------------------------------------------*
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
REPORT ZSDSFIR0400.
*---------------------------------------------------------*
*                       TABLE                        *
*---------------------------------------------------------*
TABLES: BNKA,ZSDSFIT042.
*---------------------------------------------------------*
*                       CONSTANTS                         *
*---------------------------------------------------------*
CONSTANTS: gc_save(1)   TYPE c      VALUE 'A',
           gc_eqtyp     TYPE eqtyp  VALUE 'M'.

************************************************************************
*      D E C L A R E  T A B L E & S T R U C T  U R E & V A R I A B L E *
************************************************************************
TYPE-POOLS: slis,vrm,TRUXS.

TYPES: BEGIN OF ty_ZSDSFIT042,
          mandt             TYPE  ZSDSFIT042-mandt,    "Client
          HBKID             TYPE  ZSDSFIT042-HBKID,    "Bank Keys
          zbank_item        TYPE  ZSDSFIT042-zbank_item,    "Statement bank item
          cheque_date       TYPE  ZSDSFIT042-cheque_date,    "date for payment by cheque
          debit_amt         TYPE  ZSDSFIT042-debit_amt,    "Amount
          received_amount   TYPE  ZSDSFIT042-received_amount,    "Amount
          flag_bank_fee     TYPE  ZSDSFIT042-flag_bank_fee,    "Flag bank fee
          line_map_bank_fe  TYPE  ZSDSFIT042-line_map_bank_fe,    "Statement bank item
          bank_desc         TYPE  ZSDSFIT042-bank_desc,    "Bank description
          bal_amt           TYPE  ZSDSFIT042-bal_amt,    "Amount
          channel           TYPE  ZSDSFIT042-channel,    "Channel
          branch_code       TYPE  ZSDSFIT042-branch_code,    "Branch Code
          branch_name       TYPE  ZSDSFIT042-branch_name,    "Branch Name
          terminal_id       TYPE  ZSDSFIT042-terminal_id,    "Terminal Id
          cheque_no         TYPE  ZSDSFIT042-cheque_no,    "Cheque No
          tran_code         TYPE  ZSDSFIT042-tran_code,    "Tran Code
          sender_bank       TYPE  ZSDSFIT042-sender_bank,    "Sender Bank
          sender_branch     TYPE  ZSDSFIT042-sender_branch,    "Sender Branch
          sender_acc_no     TYPE  ZSDSFIT042-sender_acc_no,    "Sender Account Number
          sender_acc_name   TYPE  ZSDSFIT042-sender_acc_name,    "Sender Account Name
          sds_acc_no        TYPE  ZSDSFIT042-sds_acc_no,    "SDS Account No
          tran_date         TYPE  ZSDSFIT042-tran_date,    "Transection date Or Booking Date
          tran_date_to      TYPE  ZSDSFIT042-tran_date_to,    "Transection date to Or Booking Date to
          smbc_remark       TYPE  ZSDSFIT042-smbc_remark,    "SMBC Remark
          smbc_bal_amt      TYPE  ZSDSFIT042-smbc_bal_amt,    "Amount
          record_type       TYPE  ZSDSFIT042-record_type,    "Record Type
          swift_bic         TYPE  ZSDSFIT042-swift_bic,    "Swift Bic
          acc_type          TYPE  ZSDSFIT042-acc_type,    "Account Type
          currency          TYPE  ZSDSFIT042-currency,    "CURRENCY
          acc_name          TYPE  ZSDSFIT042-acc_name,    "Account Name
          tran_type_name    TYPE  ZSDSFIT042-tran_type_name,    "Trans Type Name
          cust_refer        TYPE  ZSDSFIT042-cust_refer,    "Cust Refer
          bank_refer        TYPE  ZSDSFIT042-bank_refer,    "Bank Refer
          erdat             TYPE  ZSDSFIT042-erdat,    "Date on Which Record Was Created
          erzet             TYPE  ZSDSFIT042-erzet,    "Entry time
          usnam             TYPE  ZSDSFIT042-usnam,    "User Name
          tranf             TYPE  ZSDSFIT042-tranf,    "Transfer No.
          run_id            TYPE  ZSDSFIT042-run_id,    "Running ID
          payin             TYPE  ZSDSFIT042-payin,    "Pay in
          map_status        TYPE  ZSDSFIT042-map_status,    "Mapping Status with collector
          map_date          TYPE  ZSDSFIT042-map_date,    "Mapping Date with collector
          map_time          TYPE  ZSDSFIT042-map_time,    "Mapping Time with collector
          status_lock       TYPE  ZSDSFIT042-status_lock,    "Status lock data
          lock_erdat        TYPE  ZSDSFIT042-lock_erdat,    "Date on Which Record Was Created
          lock_erzet        TYPE  ZSDSFIT042-lock_erzet,    "Entry time
          lock_usnam        TYPE  ZSDSFIT042-lock_usnam,    "User Name
          delete_flag       TYPE  ZSDSFIT042-delete_flag,    "Deleted flag
          del_erdat         TYPE  ZSDSFIT042-del_erdat,    "Date on Which Record Was Created
          del_erzet         TYPE  ZSDSFIT042-del_erzet,    "Entry time
          del_usnam         TYPE  ZSDSFIT042-del_usnam,    "User Name
          cust_name         TYPE  ZSDSFIT042-cust_name,    "Customer Name
          b_value_time      TYPE  ZSDSFIT042-b_value_time,    "Bank Value Time
          map_usnam         TYPE  ZSDSFIT042-map_usnam,    "User Name
          reserve_map       TYPE  ZSDSFIT042-reserve_map,    "Status reserve by user
          reserve_date      TYPE  ZSDSFIT042-reserve_date,    "Date on Which Record Was Created
          reserve_time      TYPE  ZSDSFIT042-reserve_time,    "Entry time
          reserve_usnam     TYPE  ZSDSFIT042-reserve_usnam,    "User Name
          reserve_desc      TYPE  ZSDSFIT042-reserve_desc,    "Reseve detail
          fi_confirm_map    TYPE  ZSDSFIT042-fi_confirm_map,    "FI Confrim
          fi_confirm_date   TYPE  ZSDSFIT042-fi_confirm_date,    "Date on Which Record Was Created
          fi_confirm_time   TYPE  ZSDSFIT042-fi_confirm_time,    "Entry time
          fi_confirm_usnam  TYPE  ZSDSFIT042-fi_confirm_usnam,    "User Name
          status_ad         TYPE  ZSDSFIT042-status_ad,    "Status Clear experiment
          status_acc_recon  TYPE  ZSDSFIT042-status_acc_recon,    "Status Account Reconcile
          recon_date        TYPE  ZSDSFIT042-recon_date,    "Date on Which Record Was Created
          recon_time        TYPE  ZSDSFIT042-recon_time,    "Entry time
          recon_user        TYPE  ZSDSFIT042-recon_user,    "User Name
          KBANK_DETAIL      TYPE  ZSDSFIT042-KBANK_DETAIL,    "KBANK_DETAIL
          TRNFER_NUMBER     TYPE  ZSDSFIT042-TRNFER_NUMBER,    "Tranfer number
          FYEAR_TRNFERNO    TYPE  ZSDSFIT042-FYEAR_TRNFERNO,    "Fiscal year transfer number
          FI_CLEARING_NO    TYPE  ZSDSFIT042-FI_CLEARING_NO,    "FI Clearing number
          FYEAR_CLEARING    TYPE  ZSDSFIT042-FYEAR_CLEARING,    "FY clearing
          DATE_CLEAR        TYPE  ZSDSFIT042-DATE_CLEAR,    "Date Clearing
          TIME_CLEAR        TYPE  ZSDSFIT042-TIME_CLEAR,    "Time clearing
          MT940_DOC         TYPE  ZSDSFIT042-MT940_DOC,    "MT940 Doc.
          FYEAR_MT940       TYPE  ZSDSFIT042-FYEAR_MT940,    "Fiscal year MT940
          CLEARING_USER     TYPE  ZSDSFIT042-CLEARING_USER,    "User Clearing
       END OF ty_ZSDSFIT042.


TYPES: BEGIN OF ty_output,
          check               TYPE c,  "Check deleted
          HBKID               TYPE  ZSDSFIT042-HBKID,    "Bank Keys
          zbank_item          TYPE  ZSDSFIT042-zbank_item,    "Statement bank item
          cheque_date         TYPE  ZSDSFIT042-cheque_date,    "date for payment by cheque
          debit_amt           TYPE  ZSDSFIT042-debit_amt,    "Amount
          received_amount     TYPE  ZSDSFIT042-received_amount,    "Amount
          flag_bank_fee       TYPE  ZSDSFIT042-flag_bank_fee,    "Flag bank fee
          line_map_bank_fe    TYPE  ZSDSFIT042-line_map_bank_fe,    "Statement bank item
          bank_desc           TYPE  ZSDSFIT042-bank_desc,    "Bank description
          bal_amt             TYPE  ZSDSFIT042-bal_amt,    "Amount
          channel             TYPE  ZSDSFIT042-channel,    "Channel
          branch_code         TYPE  ZSDSFIT042-branch_code,    "Branch Code
          branch_name         TYPE  ZSDSFIT042-branch_name,    "Branch Name
          terminal_id         TYPE  ZSDSFIT042-terminal_id,    "Terminal Id
          cheque_no           TYPE  ZSDSFIT042-cheque_no,    "Cheque No
          tran_code           TYPE  ZSDSFIT042-tran_code,    "Tran Code
          sender_bank         TYPE  ZSDSFIT042-sender_bank,    "Sender Bank
          sender_branch       TYPE  ZSDSFIT042-sender_branch,    "Sender Branch
          sender_acc_no       TYPE  ZSDSFIT042-sender_acc_no,    "Sender Account Number
          sender_acc_name     TYPE  ZSDSFIT042-sender_acc_name,    "Sender Account Name
          sds_acc_no          TYPE  ZSDSFIT042-sds_acc_no,    "SDS Account No
          tran_date           TYPE  ZSDSFIT042-tran_date,    "Transection date Or Booking Date
          tran_date_to        TYPE  ZSDSFIT042-tran_date_to,    "Transection date to Or Booking Date to
          smbc_remark         TYPE  ZSDSFIT042-smbc_remark,    "SMBC Remark
          smbc_bal_amt        TYPE  ZSDSFIT042-smbc_bal_amt,    "Amount
          record_type         TYPE  ZSDSFIT042-record_type,    "Record Type
          swift_bic           TYPE  ZSDSFIT042-swift_bic,    "Swift Bic
          acc_type            TYPE  ZSDSFIT042-acc_type,    "Account Type
          currency            TYPE  ZSDSFIT042-currency,    "CURRENCY
          acc_name            TYPE  ZSDSFIT042-acc_name,    "Account Name
          tran_type_name      TYPE  ZSDSFIT042-tran_type_name,    "Trans Type Name
          cust_refer          TYPE  ZSDSFIT042-cust_refer,    "Cust Refer
          bank_refer          TYPE  ZSDSFIT042-bank_refer,    "Bank Refer
          erdat               TYPE  ZSDSFIT042-erdat,    "Date on Which Record Was Created
          erzet               TYPE  ZSDSFIT042-erzet,    "Entry time
          usnam               TYPE  ZSDSFIT042-usnam,    "User Name
          tranf               TYPE  ZSDSFIT042-tranf,    "Transfer No.
          payin               TYPE  ZSDSFIT042-payin,    "Pay in
          map_status          TYPE  ZSDSFIT042-map_status,    "Mapping Status with collector
          map_date            TYPE  ZSDSFIT042-map_date,    "Mapping Date with collector
          map_time            TYPE  ZSDSFIT042-map_time,    "Mapping Time with collector
          b_value_time        TYPE  ZSDSFIT042-b_value_time,    "Bank value time
          map_usnam           TYPE  ZSDSFIT042-map_usnam,    "User Name
          KBANK_DETAIL     TYPE  ZSDSFIT042-KBANK_DETAIL,    "KBANK_DETAIL
          TRNFER_NUMBER     TYPE  ZSDSFIT042-TRNFER_NUMBER,    "Tranfer number
          FYEAR_TRNFERNO    TYPE  ZSDSFIT042-FYEAR_TRNFERNO,    "Fiscal year transfer number
          FI_CLEARING_NO    TYPE  ZSDSFIT042-FI_CLEARING_NO,    "FI Clearing number
          FYEAR_CLEARING    TYPE  ZSDSFIT042-FYEAR_CLEARING,    "FY clearing
          DATE_CLEAR        TYPE  ZSDSFIT042-DATE_CLEAR,    "Date Clearing
          TIME_CLEAR        TYPE  ZSDSFIT042-TIME_CLEAR,    "Time clearing
          MT940_DOC         TYPE  ZSDSFIT042-MT940_DOC,    "MT940 Doc.
          FYEAR_MT940       TYPE  ZSDSFIT042-FYEAR_MT940,    "Fiscal year MT940
          CLEARING_USER     TYPE  ZSDSFIT042-CLEARING_USER,    "User Clearing
       END OF ty_output.

TYPES: BEGIN OF ty_ZSDSFIT042_1,
                HBKID               TYPE  ZSDSFIT042-HBKID,    "Bank Keys
                zbank_item          TYPE  ZSDSFIT042-zbank_item,    "Statement bank item
                cheque_date         TYPE  ZSDSFIT042-cheque_date,    "date for payment by cheque
                debit_amt           TYPE  ZSDSFIT042-debit_amt,    "Amount
                received_amount     TYPE  ZSDSFIT042-received_amount,    "Amount
                flag_bank_fee       TYPE  ZSDSFIT042-flag_bank_fee,    "Flag bank fee
                line_map_bank_fe    TYPE  ZSDSFIT042-line_map_bank_fe,    "Statement bank item
                bank_desc           TYPE  ZSDSFIT042-bank_desc,    "Bank description
                bal_amt             TYPE  ZSDSFIT042-bal_amt,    "Amount
                channel             TYPE  ZSDSFIT042-channel,    "Channel
                branch_code         TYPE  ZSDSFIT042-branch_code,    "Branch Code
                branch_name         TYPE  ZSDSFIT042-branch_name,    "Branch Name
                terminal_id         TYPE  ZSDSFIT042-terminal_id,    "Terminal Id
                cheque_no           TYPE  ZSDSFIT042-cheque_no,    "Cheque No
                tran_code           TYPE  ZSDSFIT042-tran_code,    "Tran Code
                sender_bank         TYPE  ZSDSFIT042-sender_bank,    "Sender Bank
                sender_branch       TYPE  ZSDSFIT042-sender_branch,    "Sender Branch
                sender_acc_no       TYPE  ZSDSFIT042-sender_acc_no,    "Sender Account Number
                sender_acc_name     TYPE  ZSDSFIT042-sender_acc_name,    "Sender Account Name
                sds_acc_no          TYPE  ZSDSFIT042-sds_acc_no,    "SDS Account No
                tran_date           TYPE  ZSDSFIT042-tran_date,    "Transection date Or Booking Date
                tran_date_to        TYPE  ZSDSFIT042-tran_date_to,    "Transection date to Or Booking Date to
                smbc_remark         TYPE  ZSDSFIT042-smbc_remark,    "SMBC Remark
                smbc_bal_amt        TYPE  ZSDSFIT042-smbc_bal_amt,    "Amount
                record_type         TYPE  ZSDSFIT042-record_type,    "Record Type
                swift_bic           TYPE  ZSDSFIT042-swift_bic,    "Swift Bic
                acc_type            TYPE  ZSDSFIT042-acc_type,    "Account Type
                currency            TYPE  ZSDSFIT042-currency,    "CURRENCY
                acc_name            TYPE  ZSDSFIT042-acc_name,    "Account Name
                tran_type_name      TYPE  ZSDSFIT042-tran_type_name,    "Trans Type Name
                cust_refer          TYPE  ZSDSFIT042-cust_refer,    "Cust Refer
                bank_refer          TYPE  ZSDSFIT042-bank_refer,    "Bank Refer
                erdat               TYPE  ZSDSFIT042-erdat,    "Date on Which Record Was Created
                erzet               TYPE  ZSDSFIT042-erzet,    "Entry time
                usnam               TYPE  ZSDSFIT042-usnam,    "User Name
                tranf               TYPE  ZSDSFIT042-tranf,    "Transfer No.
                run_id              TYPE  ZSDSFIT042-run_id,    "Running ID
                payin               TYPE  ZSDSFIT042-payin,    "Pay in
                map_status          TYPE  ZSDSFIT042-map_status,    "Mapping Status with collector
                map_date            TYPE  ZSDSFIT042-map_date,    "Mapping Date with collector
                map_time            TYPE  ZSDSFIT042-map_time,    "Mapping Time with collector
                status_lock         TYPE  ZSDSFIT042-status_lock,    "Status lock data
                lock_erdat          TYPE  ZSDSFIT042-lock_erdat,    "Date on Which Record Was Created
                lock_erzet          TYPE  ZSDSFIT042-lock_erzet,    "Entry time
                lock_usnam          TYPE  ZSDSFIT042-lock_usnam,    "User Name
                delete_flag         TYPE  ZSDSFIT042-delete_flag,    "Deleted flag
                del_erdat           TYPE  ZSDSFIT042-del_erdat,    "Date on Which Record Was Created
                del_erzet           TYPE  ZSDSFIT042-del_erzet,    "Entry time
                del_usnam           TYPE  ZSDSFIT042-del_usnam,    "User Name
                cust_name           TYPE  ZSDSFIT042-cust_name,    "Customer Name
                b_value_time        TYPE  ZSDSFIT042-b_value_time,    "Bank value time
                map_usnam           TYPE  ZSDSFIT042-map_usnam,    "User Name



END OF ty_ZSDSFIT042_1.
TYPES: BEGIN OF typ_input,

          HBKID             TYPE  ZSDSFIT042-HBKID,  "Bank code
          zbank_item        TYPE  ZSDSFIT042-zbank_item,   "Item
          cheque_date       TYPE  ZSDSFIT042-cheque_date,  "date
          received_amount   TYPE  ZSDSFIT042-received_amount, "Amount
          flag_bank_fee     TYPE  ZSDSFIT042-flag_bank_fee,   "Flag status bank fee
          line_map_bank_fe  TYPE  ZSDSFIT042-line_map_bank_fe, "Item line mapping bank free
          bank_desc         TYPE  ZSDSFIT042-bank_desc,       "Description


END OF typ_input.

TYPES: BEGIN OF typ_bank,

         HBKID             TYPE  ZSDSFIT044-HBKID,  "Bank code
         BANK_NAME             TYPE  ZSDSFIT044-BANK_NAME,  "Bank code
END OF typ_bank.

TYPES: BEGIN OF typ_import_file,
         col1(100)       TYPE c,
         col2(100)       TYPE c,
         col3(100)       TYPE c,
         col4(100)       TYPE c,
         col5(100)       TYPE c,
         col6(100)       TYPE c,
         col7(100)       TYPE c,
         col8(100)       TYPE c,
         col9(100)       TYPE c,
         col10(100)       TYPE c,
         col11(100)       TYPE c,
         col12(100)       TYPE c,
         col13(100)       TYPE c,
         col14(100)       TYPE c,
         col15(100)       TYPE c,
         col16(100)       TYPE c,
         col17(100)       TYPE c,
         col18(100)       TYPE c,
         col19(100)       TYPE c,
         col20(100)       TYPE c,
         col21(100)       TYPE c,
         col22(100)       TYPE c,
         col23(100)       TYPE c,
         col24(100)       TYPE c,
         col25(100)       TYPE c,
         col26(100)       TYPE c,
         col27(100)       TYPE c,
         col28(100)       TYPE c,
         col29(100)       TYPE c,
         col30(100)       TYPE c,
         col31(100)       TYPE c,
         col32(100)       TYPE c,
         col33(100)       TYPE c,
         col34(100)       TYPE c,
         col35(100)       TYPE c,
         col36(100)       TYPE c,
         col37(100)       TYPE c,
         col38(100)       TYPE c,
         col39(100)       TYPE c,
         col40(100)       TYPE c,
         col41(100)       TYPE c,
         col42(100)       TYPE c,
         col43(100)       TYPE c,
         col44(100)       TYPE c,
END OF typ_import_file.

DATA: gt_data   TYPE TABLE OF ty_ZSDSFIT042,
      gs_data   TYPE ty_ZSDSFIT042,
      gt_output TYPE TABLE OF ty_output,
      gs_output TYPE ty_output,
      wa_output TYPE ty_output,
      gw_output TYPE ty_output,
      gt_bank   TYPE STANDARD TABLE OF typ_bank,
      gs_bank   TYPE  typ_bank,
      gt_import_file  TYPE STANDARD TABLE OF typ_import_file,
      gs_import_file  TYPE typ_import_file,
      wa_import_file  TYPE typ_import_file,
      gt_input   TYPE STANDARD TABLE OF typ_input,
      gw_input   TYPE typ_input,
      gt_ZSDSFIT042 TYPE STANDARD TABLE OF ty_ZSDSFIT042_1,
      gw_ZSDSFIT042 TYPE ty_ZSDSFIT042_1,
      wa_ZSDSFIT042 TYPE ty_ZSDSFIT042_1.

DATA: lv_check_load_date TYPE sy-datum.
DATA: msg_show(200) TYPE c.


DATA: gt_fieldcat      TYPE slis_t_fieldcat_alv,
      gs_fieldcat      TYPE slis_fieldcat_alv,
      gs_layout        TYPE slis_layout_alv.


DATA: bdcdata TYPE TABLE OF bdcdata WITH HEADER LINE.
DATA:     g_tc_100_lines  like sy-loopc.
DATA: name  TYPE vrm_id,
      list  TYPE vrm_values,
      value LIKE LINE OF list.

DATA: lv_linecount TYPE i.
DATA: fill TYPE i .
controls: tc_100 type tableview using screen 0100.
*&---------------------------------------------------------------------*
*&  M A C R O   C O M M A N D   D E F I N I T I O N                    *
*&---------------------------------------------------------------------*
DEFINE m_fill_cat.
  gs_fieldcat-tabname    = &1.
  gs_fieldcat-fieldname  = &2.
  gs_fieldcat-col_pos    = &3.
  gs_fieldcat-seltext_l  = &4.
  gs_fieldcat-no_out     = &5.
  gs_fieldcat-outputlen  = &6.
*  gs_fieldcat-input       = &7.
  gs_fieldcat-checkbox    = &7.
  gs_fieldcat-edit        = &8.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.

END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&  M A C R O   C O M M A N D   D E F I N I T I O N  ( %_ )            *
*&---------------------------------------------------------------------*
define %show. " show input field
  if screen-group1 = &1.
    screen-invisible = 0.
    screen-active = 1.
  endif.
end-of-definition.
define %hide. " hide input field
  if screen-group1 = &1.
    screen-invisible = 1.
    screen-active = 0.
  endif.
end-of-definition.
************************************************************************
*      S E L E C T I O N  S C R E E N                                  *
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-b01.
PARAMETERS: r_upload RADIOBUTTON GROUP gr1 DEFAULT 'X' user-command exec,
            r_input  RADIOBUTTON GROUP gr1,
            r_del    RADIOBUTTON GROUP gr1,
            r_lock   RADIOBUTTON GROUP gr1,
            r_locka  RADIOBUTTON GROUP gr1.  "CH3 Add by WAntanee

SELECTION-SCREEN END OF BLOCK b2.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b02.
PARAMETERS : p_bank TYPE BNKA-BANKL AS LISTBOX VISIBLE LENGTH 35   DEFAULT space.
PARAMETERS: p_file LIKE rlgrap-filename DEFAULT  'C:\' modif id sc1.
PARAMETERS : p_erdat LIKE ZSDSFIT042-erdat modif id sc2.
SELECT-OPTIONS : s_erdat FOR ZSDSFIT042-erdat modif id sc3.
SELECT-OPTIONS : s_tranf FOR ZSDSFIT042-tranf modif id sc3.
SELECTION-SCREEN END OF BLOCK b1.

************************************************************************
*      A T     S E L E C T I O N     S C R E E N                       *
************************************************************************
INITIALIZATION.
   PERFORM f_add_list_bank.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f_get_path_name CHANGING p_file.

AT selection-screen output.
   PERFORM f_modify_screen.
************************************************************************
*      B E G I N      S E L E C T I O N                                *
************************************************************************
START-OF-SELECTION .

IF NOT r_upload IS INITIAL.
*  CLEAR:name,list,value.
* Get data
  PERFORM f_upload_data.
ELSEIF NOT r_input IS INITIAL.

   fill = 22.  "Set line for screen

   gw_input-HBKID = p_bank.
   call screen 0100.
ELSEIF ( NOT r_del IS INITIAL ) OR ( NOT r_lock IS INITIAL ) OR ( NOT r_locka IS INITIAL ).
   PERFORM f_get_data_viewbank.
   PERFORM f_map_data_viewbank.
ENDIF.


************************************************************************
*      E N D      S E L E C T I O N                                    *
************************************************************************
END-OF-SELECTION .
  IF NOT r_upload IS INITIAL.
*     break wantanee.
     IF lv_check_load_date IS NOT INITIAL.
        MESSAGE i000(38) WITH msg_show DISPLAY LIKE 'E'.
     ELSE.
       IF gt_output[] IS NOT INITIAL.
         PERFORM f_display_report.
       ELSE.
         MESSAGE i000(38) WITH text-e04 DISPLAY LIKE 'E'.
       ENDIF.

     ENDIF.
*   ELSE.
  ELSEIF ( NOT r_del IS INITIAL ) OR ( NOT r_lock IS INITIAL ) OR ( NOT r_locka IS INITIAL ) .
      IF gt_output[] IS NOT INITIAL.
         PERFORM f_display_report.
       ELSE.
         MESSAGE i000(38) WITH text-e04 DISPLAY LIKE 'E'.
       ENDIF.
  ENDIF.

**&---------------------------------------------------------------------*
**&      Form  f_modify_screen
**&---------------------------------------------------------------------*


form f_modify_screen.
  if not r_upload is initial.
   loop at screen.
      %show 'SC1'.
      %hide 'SC2'.
      %hide 'SC3'.
      modify screen.

    endloop.
  elseif not r_input is initial..
    loop at screen.
      %hide 'SC1'.
      %hide 'SC2'.
      %hide 'SC3'.
      modify screen.

    endloop.
   elseif not r_del is initial..
     loop at screen.
      %hide 'SC1'.
      %hide 'SC2'.
      %show 'SC3'.
      modify screen.
      ENDLOOP.
    elseif not r_lock is initial..
     loop at screen.
      %hide 'SC1'.
      %hide 'SC2'.
      %show 'SC3'.
      modify screen.
      ENDLOOP.
    elseif not r_locka is initial..
     loop at screen.
      %hide 'SC1'.
      %hide 'SC2'.
      %show 'SC3'.
      modify screen.
      endloop.
  endif.

endform.
************************************************************************
*      FORM F_GET_PATH_NAME                                              *
*----------------------------------------------------------------------*
*      Description: This form is used for get PC path.                 *
************************************************************************
FORM f_get_path_name  CHANGING p_filepath.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      mask             = ',*.xls.'
      mode             = 'O'
      title            = 'Browsed file'
    IMPORTING
      filename         = p_filepath
    EXCEPTIONS
      inv_winsys       = 01
      no_batch         = 02
      selection_cancel = 03
      selection_error  = 04.

ENDFORM.                    " f_get_path_name
*&---------------------------------------------------------------------*
*&      Form  F_UPLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_upload_data .
  DATA: lt_file TYPE TABLE OF alsmex_tabline WITH HEADER LINE,
        lw_file TYPE alsmex_tabline.

  DATA: lt_raw TYPE TRUXS_T_TEXT_DATA.
  DATA: lv_item TYPE ZSDSFIT042-zbank_item.
  DATA: lv_year(4) TYPE c,
        lv_month(2) TYPE c,
        lv_day(2) TYPE c.
  DATA: lv_date(10) TYPE c.





  SELECT SINGLE erdat
    INTO lv_check_load_date
    FROM ZSDSFIT042
    WHERE HBKID = p_bank
    AND erdat = sy-datum
    AND DELETE_FLAG NE 'X'.

    IF lv_check_load_date IS INITIAL.






                 SELECT max( zbank_item )
                 INTO lv_item
                 FROM ZSDSFIT042.

                 IF lv_item IS INITIAL.
                    lv_item = 0.
                 ENDIF.




*                 CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
*                   EXPORTING
*                     filename                = p_file
*                     i_begin_col             = 1
*                     i_begin_row             = 1
*                     i_end_col               = 9999
*                     i_end_row               = 9999
*                   TABLES
*                     intern                  = lt_file
*                   EXCEPTIONS
*                     inconsistent_parameters = 1
*                     upload_ole              = 2
*                     OTHERS                  = 3.
               CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
                 EXPORTING
                  I_FIELD_SEPERATOR          = 'X'
                  I_LINE_HEADER              = 'X'
                   I_TAB_RAW_DATA             = lt_raw
                   I_FILENAME                 = p_file
                 TABLES
                   I_TAB_CONVERTED_DATA       = gt_import_file
*                EXCEPTIONS
*                  CONVERSION_FAILED          = 1
                         .

                 IF sy-subrc <> 0.
*                   MESSAGE 'Cannot upload file. Please check version excel.' TYPE 'E'.
*                   MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno.
*                       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                 ENDIF.



*               Process Records and get only records into table IT_DATA
                 IF gt_import_file[] IS NOT INITIAL.
                   IF p_bank = 'BAY01'.

                        LOOP AT gt_import_file INTO gs_import_file.
                             CLEAR: gs_output,lv_day, lv_month,  lv_year.
                             lv_item = lv_item + 1.

                             gs_output-HBKID = 'BAY01'.
                             gs_output-zbank_item = lv_item.
*                             SPLIT

                             SPLIT gs_import_file-col1 AT '/' INTO: lv_day lv_month lv_year.
                             CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                               EXPORTING
                                 INPUT         = lv_month
                              IMPORTING
                                OUTPUT        = lv_month
                                       .
                             CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                               EXPORTING
                                 INPUT         = lv_day
                              IMPORTING
                                OUTPUT        = lv_day
                                       .
                             CONCATENATE lv_year lv_month lv_day INTO gs_output-cheque_date.
*                             gs_output-cheque_date = gs_import_file-col1.
                             gs_output-tran_code = gs_import_file-col2.
                             gs_output-bank_desc = gs_import_file-col3.
                             gs_output-debit_amt = gs_import_file-col5.
                             gs_output-received_amount = gs_import_file-col6.
                             gs_output-bal_amt = gs_import_file-col7.
                             gs_output-channel = gs_import_file-col8.
                             gs_output-branch_code = gs_import_file-col9.
                               APPEND gs_output TO gt_output .

                        ENDLOOP.

                   ELSEIF p_bank = 'BBL01'.



                        LOOP AT gt_import_file INTO gs_import_file.
                          CLEAR: gs_output,lv_day, lv_month,  lv_year.
                          IF SY-TABIX > 4.
                                lv_item = lv_item + 1.
                                gs_output-hbkid = 'BBL01'.
                                gs_output-zbank_item = lv_item.

                                 SPLIT gs_import_file-col1 AT '/' INTO: lv_day lv_month  lv_year.
                                   CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                                     EXPORTING
                                       INPUT         = lv_month
                                    IMPORTING
                                      OUTPUT        = lv_month
                                             .
                                   CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                                     EXPORTING
                                       INPUT         = lv_day
                                    IMPORTING
                                      OUTPUT        = lv_day.

                                CONCATENATE lv_year lv_month lv_day INTO gs_output-tran_date.


                                CLEAR: lv_day, lv_month,  lv_year.
                                 SPLIT gs_import_file-col3 AT '/' INTO: lv_day lv_month  lv_year.
                                   CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                                     EXPORTING
                                       INPUT         = lv_month
                                    IMPORTING
                                      OUTPUT        = lv_month
                                             .
                                   CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                                     EXPORTING
                                       INPUT         = lv_day
                                    IMPORTING
                                      OUTPUT        = lv_day
                                             .
                                  CONCATENATE lv_year lv_month lv_day INTO gs_output-cheque_date.
                                  gs_output-bank_desc = gs_import_file-col5.
                                  gs_output-tran_code = gs_import_file-col8.
                                  gs_output-cheque_no = gs_import_file-col10.
                                  REPLACE ALL OCCURRENCES OF ',' IN gs_import_file-col12 WITH ''.
                                  CONDENSE gs_import_file-col12.
                                  gs_output-debit_amt = gs_import_file-col12.
                                  REPLACE ALL OCCURRENCES OF ',' IN gs_import_file-col14 WITH ''.
                                  CONDENSE gs_import_file-col14.
                                  gs_output-received_amount = gs_import_file-col14.
                                  REPLACE ALL OCCURRENCES OF ',' IN gs_import_file-col17 WITH ''.
                                  CONDENSE gs_import_file-col17.
                                  gs_output-bal_amt = gs_import_file-col17.
                                  gs_output-channel = gs_import_file-col20.
                                  gs_output-terminal_id = gs_import_file-col22.
                                  gs_output-branch_name = gs_import_file-col24.
                                  gs_output-sender_bank = gs_import_file-col26.
                                  gs_output-sender_acc_no = gs_import_file-col30.
                                  gs_output-sender_acc_name = gs_import_file-col33.
*                                 IF gs_output-received_amount NE 0.
                                    APPEND gs_output TO gt_output .
*                                 ENDIF.

                           ENDIF.
                         ENDLOOP.
                   ELSEIF p_bank = 'BTMU'.



                        LOOP AT gt_import_file INTO gs_import_file.
                          CLEAR: gs_output,lv_day, lv_month,  lv_year.
                          IF SY-TABIX > 0.
*                                IF NOT gs_import_file-col10 IS INITIAL.
                                     lv_item = lv_item + 1.
                                     gs_output-hbkid = 'BAY01'.
                                     gs_output-zbank_item = lv_item.

                                     gs_output-record_type = gs_import_file-col1.
                                     gs_output-branch_name = gs_import_file-col3.
                                     gs_output-swift_bic = gs_import_file-col4.
                                     gs_output-acc_type = gs_import_file-col5.
                                     gs_output-currency = gs_import_file-col6.
                                     gs_output-sds_acc_no = gs_import_file-col7.
                                     gs_output-acc_name = gs_import_file-col8.
                                     SPLIT gs_import_file-col10 AT '/' INTO: lv_month lv_day lv_year.
                                        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                                          EXPORTING
                                            INPUT         = lv_month
                                         IMPORTING
                                           OUTPUT        = lv_month
                                                  .
                                        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                                          EXPORTING
                                            INPUT         = lv_day
                                         IMPORTING
                                           OUTPUT        = lv_day
                                                  .
                                       CONCATENATE lv_year lv_month lv_day INTO gs_output-tran_date.

                                     CLEAR:lv_day, lv_month,  lv_year.
                                      SPLIT gs_import_file-col11 AT '/' INTO: lv_month lv_day lv_year.
                                        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                                          EXPORTING
                                            INPUT         = lv_month
                                         IMPORTING
                                           OUTPUT        = lv_month
                                                  .
                                        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                                          EXPORTING
                                            INPUT         = lv_day
                                         IMPORTING
                                           OUTPUT        = lv_day
                                                  .
                                       CONCATENATE lv_year lv_month lv_day INTO gs_output-cheque_date.

                                        gs_output-tran_type_name = gs_import_file-col12.

                                        REPLACE ALL OCCURRENCES OF ',' IN gs_import_file-col13 WITH ''.
                                       CONDENSE gs_import_file-col13.
                                        gs_output-debit_amt = gs_import_file-col13.

*                                       REPLACE gs_import_file-col14
*                                       REPLACE ',' WITH space INTO gs_import_file-col14.
                                       REPLACE ALL OCCURRENCES OF ',' IN gs_import_file-col14 WITH ''.
                                       CONDENSE gs_import_file-col14.
                                       gs_output-received_amount = gs_import_file-col14.


                                        REPLACE ALL OCCURRENCES OF ',' IN gs_import_file-col15 WITH ''.
                                       CONDENSE gs_import_file-col15.
                                       gs_output-bal_amt = gs_import_file-col15.

                                       gs_output-cust_refer = gs_import_file-col16.
                                       gs_output-bank_desc = gs_import_file-col17.
                                       gs_output-bank_refer = gs_import_file-col18.
*                                       CONCATENATE 'Bank:' gs_import_file-col24 'Account:' gs_import_file-col30 'Name:' gs_import_file-col33
*                                           INTO gs_output-bank_desc SEPARATED BY SPACE.
                                      APPEND gs_output TO gt_output .
*                                ENDIF.
                           ENDIF.
                         ENDLOOP.
                   ELSEIF p_bank = 'KBK01'.


                        LOOP AT gt_import_file INTO gs_import_file.
                          CLEAR:  gs_output,lv_day, lv_month,  lv_year.
                          IF SY-TABIX > 7.
                                IF gs_import_file-col1+0(1) NE '*'.
*                                IF NOT gs_import_file-col7 IS INITIAL.
                                     lv_item = lv_item + 1.
                                     gs_output-hbkid = 'KBK01'.
                                     gs_output-zbank_item = lv_item.
                                      SPLIT gs_import_file-col1 AT '/' INTO: lv_month lv_day lv_year.
*                                        lv_year = lv_year - 543.
                                        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                                          EXPORTING
                                            INPUT         = lv_month
                                         IMPORTING
                                           OUTPUT        = lv_month
                                                  .
                                        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                                          EXPORTING
                                            INPUT         = lv_day
                                         IMPORTING
                                           OUTPUT        = lv_day
                                                  .
                                       CONCATENATE lv_year lv_month lv_day INTO gs_output-cheque_date.
*                                       REPLACE gs_import_file-col14
*                                       REPLACE ',' WITH space INTO gs_import_file-col14.
*                                       REPLACE ALL OCCURRENCES OF ',' IN gs_import_file-col14 WITH ''.
*                                       CONDENSE gs_import_file-col14.
*                                       gs_output-b_value_time = gs_import_file-col2.
                                       gs_output-bank_desc = gs_import_file-col3.
                                       gs_output-cheque_no = gs_import_file-col4.
                                       gs_output-debit_amt = gs_import_file-col5.
                                       gs_output-received_amount = gs_import_file-col6.
                                       gs_output-bal_amt = gs_import_file-col7.
                                       gs_output-terminal_id = gs_import_file-col8.
                                       gs_output-branch_code = gs_import_file-col9.

                                        CLEAR:  lv_day, lv_month,  lv_year.
                                        SPLIT gs_import_file-col10 AT '/' INTO: lv_month lv_day lv_year.
*                                        lv_year = lv_year - 543.
                                        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                                          EXPORTING
                                            INPUT         = lv_month
                                         IMPORTING
                                           OUTPUT        = lv_month
                                                  .
                                        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                                          EXPORTING
                                            INPUT         = lv_day
                                         IMPORTING
                                           OUTPUT        = lv_day
                                                  .
                                       CONCATENATE lv_year lv_month lv_day INTO gs_output-tran_date.
                                       gs_output-channel = gs_import_file-col11.
                                       gs_output-KBANK_DETAIL = gs_import_file-col12.

                                      APPEND gs_output TO gt_output .
*                                ENDIF.
                                  ENDIF.
                           ENDIF.
                         ENDLOOP.

                   ELSEIF p_bank = 'KTB01'.

*BREAK-POINT.

                        LOOP AT gt_import_file INTO gs_import_file.
                          CLEAR: gs_output,lv_day, lv_month,  lv_year.
                          IF SY-TABIX > 10.
                                IF NOT gs_import_file-col8 IS INITIAL.
                                     lv_item = lv_item + 1.
                                     gs_output-hbkid = 'KTB01'.
                                     gs_output-zbank_item = lv_item.
*                                      SPLIT gs_import_file-col1 AT '/' INTO: lv_month lv_day lv_year.
                                       lv_day = gs_import_file-col1+0(2).
                                       lv_month = gs_import_file-col1+3(2).
                                       lv_year = gs_import_file-col1+6(4).
*                                        lv_year = lv_year - 543.
                                        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                                          EXPORTING
                                            INPUT         = lv_month
                                         IMPORTING
                                           OUTPUT        = lv_month
                                                  .
                                        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                                          EXPORTING
                                            INPUT         = lv_day
                                         IMPORTING
                                           OUTPUT        = lv_day
                                                  .

                                       CONCATENATE lv_year lv_month lv_day INTO gs_output-cheque_date.
                                       gs_output-b_value_time = gs_import_file-col1+12(8).
*                                       gs_output-terminal_id = gs_import_file-col2.
                                       gs_output-tran_code = gs_import_file-col2.
                                       gs_output-bank_desc = gs_import_file-col3.
                                       gs_output-cheque_no = gs_import_file-col4.

*                                       REPLACE gs_import_file-col14
*                                       REPLACE ',' WITH space INTO gs_import_file-col14.
                                       REPLACE ALL OCCURRENCES OF ',' IN gs_import_file-col5 WITH ''.
                                       CONDENSE gs_import_file-col5.
                                       gs_output-received_amount = gs_import_file-col5.
*                                       CONCATENATE 'Bank:' gs_import_file-col24 'Account:' gs_import_file-col30 'Name:' gs_import_file-col33
*                                           INTO gs_output-bank_desc SEPARATED BY SPACE.
                                       REPLACE ALL OCCURRENCES OF ',' IN gs_import_file-col7 WITH ''.
                                       CONDENSE gs_import_file-col7.
                                       gs_output-bal_amt = gs_import_file-col7.
*                                        gs_output-branch_code = gs_import_file-col9.

                                      IF  gs_import_file-col2 EQ 'BSDFE' OR gs_import_file-col2 EQ 'SWFE'.
                                           gs_output-flag_bank_fee   = 'X'.
                                           gs_output-line_map_bank_fe = lv_item - 1.
                                      ENDIF.

                                      gs_output-CHANNEL = gs_import_file-col8.

                                      APPEND gs_output TO gt_output .
                                      CLEAR: wa_import_file.
                                      wa_import_file = gs_import_file.
                                ENDIF.
                           ENDIF.
                         ENDLOOP.

                   ELSEIF p_bank = 'SCB01'.



                        CLEAR: gs_output,lv_day, lv_month,  lv_year.
                        LOOP AT gt_import_file INTO gs_import_file.
                          IF SY-TABIX > 0.
*                                IF NOT gs_import_file-col8 IS INITIAL.
                                     lv_item = lv_item + 1.
                                     gs_output-hbkid = 'SCB01'.
                                     gs_output-zbank_item = lv_item.


                                     gs_output-sds_acc_no = gs_import_file-col1.

                                      SPLIT gs_import_file-col2 AT '/' INTO: lv_day lv_month  lv_year.

*                                        lv_year = lv_year - 543.
                                        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                                          EXPORTING
                                            INPUT         = lv_month
                                         IMPORTING
                                           OUTPUT        = lv_month
                                                  .
                                        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                                          EXPORTING
                                            INPUT         = lv_day
                                         IMPORTING
                                           OUTPUT        = lv_day
                                                  .
                                       CONCATENATE lv_year lv_month lv_day INTO gs_output-cheque_date.
                                       gs_output-b_value_time = gs_import_file-col3.
                                       gs_output-tran_code = gs_import_file-col4.
                                       gs_output-channel = gs_import_file-col5.
                                       gs_output-cheque_no = gs_import_file-col6.

                                       REPLACE ALL OCCURRENCES OF ',' IN gs_import_file-col7 WITH ''.
                                       CONDENSE gs_import_file-col7.
                                       gs_output-debit_amt = gs_import_file-col7.
*                                       REPLACE gs_import_file-col14
*                                       REPLACE ',' WITH space INTO gs_import_file-col14.
                                       REPLACE ALL OCCURRENCES OF ',' IN gs_import_file-col8 WITH ''.
                                       CONDENSE gs_import_file-col8.
                                       gs_output-received_amount = gs_import_file-col8.

                                       REPLACE ALL OCCURRENCES OF ',' IN gs_import_file-col10 WITH ''.
                                       CONDENSE gs_import_file-col10.
                                       gs_output-bal_amt = gs_import_file-col10.
*                                       CONCATENATE 'Bank:' gs_import_file-col24 'Account:' gs_import_file-col30 'Name:' gs_import_file-col33
*                                           INTO gs_output-bank_desc SEPARATED BY SPACE.
                                        REPLACE ALL OCCURRENCES OF '#' IN gs_import_file-col11 WITH ''.
                                       CONDENSE gs_import_file-col11.
                                        gs_output-bank_desc = gs_import_file-col11.
                                      APPEND gs_output TO gt_output .
*                                ENDIF.
                           ENDIF.
                         ENDLOOP.
                   ELSEIF p_bank = 'TMB01'.

                        LOOP AT gt_import_file INTO gs_import_file.
                          CLEAR: gs_output,lv_day, lv_month,  lv_year.
                          IF SY-TABIX > 0.
*                                IF NOT gs_import_file-col8 IS INITIAL.
                                     lv_item = lv_item + 1.
                                     gs_output-hbkid = 'TMB01'.
                                     gs_output-zbank_item = lv_item.

                                      gs_output-sds_acc_no = gs_import_file-col1.

                                     SPLIT gs_import_file-col2 AT '/' INTO: lv_day lv_month  lv_year.

*                                        lv_year = lv_year - 543.
                                        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                                          EXPORTING
                                            INPUT         = lv_month
                                         IMPORTING
                                           OUTPUT        = lv_month
                                                  .
                                        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                                          EXPORTING
                                            INPUT         = lv_day
                                         IMPORTING
                                           OUTPUT        = lv_day
                                                  .
                                       CONCATENATE lv_year lv_month lv_day INTO gs_output-tran_date.

                                      CLEAR: lv_day, lv_month,  lv_year.
                                      SPLIT gs_import_file-col3 AT '/' INTO: lv_day lv_month  lv_year.

*                                        lv_year = lv_year - 543.
                                        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                                          EXPORTING
                                            INPUT         = lv_month
                                         IMPORTING
                                           OUTPUT        = lv_month
                                                  .
                                        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                                          EXPORTING
                                            INPUT         = lv_day
                                         IMPORTING
                                           OUTPUT        = lv_day
                                                  .
                                       CONCATENATE lv_year lv_month lv_day INTO gs_output-cheque_date.


                                        gs_output-tran_code = gs_import_file-col4.
                                       gs_output-cheque_no = gs_import_file-col5.
                                       gs_output-debit_amt = gs_import_file-col6.
                                       gs_output-received_amount = gs_import_file-col7.
                                       gs_output-bal_amt = gs_import_file-col8.
                                       gs_output-terminal_id = gs_import_file-col9.
                                        gs_output-bank_desc = gs_import_file-col10.
                                      APPEND gs_output TO gt_output .
*                                ENDIF.
                           ENDIF.
                         ENDLOOP.
                   ELSEIF p_bank = 'UOB'.

                        LOOP AT gt_import_file INTO gs_import_file.
                          CLEAR: gs_output,lv_day, lv_month,  lv_year.
                          IF SY-TABIX > 3.
*                                IF NOT gs_import_file-col3 IS INITIAL.
                                     lv_item = lv_item + 1.
                                     gs_output-hbkid = 'UOB'.
                                     gs_output-zbank_item = lv_item.


                                     gs_output-sds_acc_no = gs_import_file-col2.
                                      SPLIT gs_import_file-col3+1(10) AT '/' INTO: lv_day lv_month  lv_year.

*                                        lv_year = lv_year - 543.
                                        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                                          EXPORTING
                                            INPUT         = lv_month
                                         IMPORTING
                                           OUTPUT        = lv_month
                                                  .
                                        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                                          EXPORTING
                                            INPUT         = lv_day
                                         IMPORTING
                                           OUTPUT        = lv_day
                                                  .
                                       CONCATENATE lv_year lv_month lv_day INTO gs_output-cheque_date.


                                       CLEAR: lv_day, lv_month,  lv_year.
                                       SPLIT gs_import_file-col4+1(10) AT '/' INTO: lv_day lv_month  lv_year.

*                                        lv_year = lv_year - 543.
                                        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                                          EXPORTING
                                            INPUT         = lv_month
                                         IMPORTING
                                           OUTPUT        = lv_month
                                                  .
                                        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                                          EXPORTING
                                            INPUT         = lv_day
                                         IMPORTING
                                           OUTPUT        = lv_day
                                                  .
                                       CONCATENATE lv_year lv_month lv_day INTO gs_output-tran_date.
                                       gs_output-b_value_time = gs_import_file-col15.
                                       gs_output-bank_desc = gs_import_file-col16.
                                       gs_output-cheque_no = gs_import_file-col19.
                                       REPLACE ALL OCCURRENCES OF ',' IN gs_import_file-col18 WITH ''.
                                       CONDENSE gs_import_file-col18.
                                       gs_output-received_amount = gs_import_file-col18.

                                       REPLACE ALL OCCURRENCES OF ',' IN gs_import_file-col20 WITH ''.
                                       CONDENSE gs_import_file-col20.
                                       gs_output-bal_amt = gs_import_file-col20.

*                                      IF gs_output-received_amount NE 0.
                                         APPEND gs_output TO gt_output .
*                                      ENDIF.
*                                ENDIF.
                           ENDIF.
                         ENDLOOP.
                   ELSEIF p_bank = 'SMB01'.


                        LOOP AT gt_import_file INTO gs_import_file.
                           CLEAR: gs_output,lv_day, lv_month,  lv_year.
                          IF SY-TABIX > 0.
*                                IF NOT gs_import_file-col8 IS INITIAL.
                                     lv_item = lv_item + 1.
                                     gs_output-hbkid = 'SMB01'.
                                     gs_output-zbank_item = lv_item.


                                      gs_output-branch_code = gs_import_file-col2.
                                      gs_output-acc_name = gs_import_file-col4.

                                     SPLIT gs_import_file-col5 AT '/' INTO: lv_day lv_month lv_year.

*                                        lv_year = lv_year - 543.
                                        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                                          EXPORTING
                                            INPUT         = lv_month
                                         IMPORTING
                                           OUTPUT        = lv_month
                                                  .
                                        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                                          EXPORTING
                                            INPUT         = lv_day
                                         IMPORTING
                                           OUTPUT        = lv_day
                                                  .
                                       CONCATENATE lv_year lv_month lv_day INTO gs_output-tran_date.

                                      CLEAR: lv_day, lv_month,  lv_year.

                                       SPLIT gs_import_file-col6 AT '/' INTO: lv_day lv_month   lv_year.

*                                        lv_year = lv_year - 543.
                                        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                                          EXPORTING
                                            INPUT         = lv_month
                                         IMPORTING
                                           OUTPUT        = lv_month
                                                  .
                                        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                                          EXPORTING
                                            INPUT         = lv_day
                                         IMPORTING
                                           OUTPUT        = lv_day
                                                  .
                                       CONCATENATE lv_year lv_month lv_day INTO gs_output-tran_date_to.
                                       gs_output-sds_acc_no = gs_import_file-col7.


                                      CLEAR: lv_day, lv_month,  lv_year.
                                      SPLIT gs_import_file-col8 AT '/' INTO: lv_day lv_month   lv_year.

*                                        lv_year = lv_year - 543.
                                        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                                          EXPORTING
                                            INPUT         = lv_month
                                         IMPORTING
                                           OUTPUT        = lv_month
                                                  .
                                        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                                          EXPORTING
                                            INPUT         = lv_day
                                         IMPORTING
                                           OUTPUT        = lv_day
                                                  .
                                       CONCATENATE lv_year lv_month lv_day INTO gs_output-cheque_date.

                                       REPLACE ALL OCCURRENCES OF ',' IN gs_import_file-col10 WITH ''.
                                       CONDENSE gs_import_file-col10.
                                       gs_output-debit_amt = gs_import_file-col10.


                                       REPLACE ALL OCCURRENCES OF ',' IN gs_import_file-col11 WITH ''.
                                       CONDENSE gs_import_file-col11.
                                       gs_output-received_amount = gs_import_file-col11.

                                       REPLACE ALL OCCURRENCES OF ',' IN gs_import_file-col12 WITH ''.
                                       CONDENSE gs_import_file-col12.
                                       gs_output-smbc_bal_amt = gs_import_file-col12.
*                                       CONCATENATE 'Bank:' gs_import_file-col24 'Account:' gs_import_file-col30 'Name:' gs_import_file-col33
*                                           INTO gs_output-bank_desc SEPARATED BY SPACE.

                                        gs_output-bank_desc = gs_import_file-col13.
                                        gs_output-cheque_no = gs_import_file-col14.
                                        gs_output-smbc_remark = gs_import_file-col16.

                                        REPLACE ALL OCCURRENCES OF ',' IN gs_import_file-col17 WITH ''.
                                       CONDENSE gs_import_file-col17.
                                       gs_output-bal_amt = gs_import_file-col17.
*                                      IF gs_output-received_amount NE 0.
                                         APPEND gs_output TO gt_output .
*                                      ENDIF.
*                                ENDIF.
                           ENDIF.
                         ENDLOOP.
                   ENDIF.

                   LOOP AT gt_output INTO wa_output.

                        gs_data-mandt = sy-mandt.
                        gs_data-hbkid = wa_output-hbkid.
                        gs_data-zbank_item = wa_output-zbank_item.
                        gs_data-cheque_date = wa_output-cheque_date.
                        gs_data-debit_amt = wa_output-debit_amt.
                        gs_data-received_amount = wa_output-received_amount..
                        gs_data-flag_bank_fee = wa_output-flag_bank_fee..
                        gs_data-line_map_bank_fe = wa_output-line_map_bank_fe.
                        gs_data-bank_desc = wa_output-bank_desc.
                        gs_data-bal_amt = wa_output-bal_amt.
                        gs_data-channel = wa_output-channel.
                        gs_data-branch_code = wa_output-branch_code.
                        gs_data-branch_name = wa_output-branch_name.
                        gs_data-terminal_id = wa_output-terminal_id.
                        gs_data-cheque_no = wa_output-cheque_no.
                        gs_data-tran_code = wa_output-tran_code.
                        gs_data-sender_bank = wa_output-sender_bank.
                        gs_data-sender_branch = wa_output-sender_branch.
                        gs_data-sender_acc_no = wa_output-sender_acc_no.
                        gs_data-sender_acc_name = wa_output-sender_acc_name.
                        gs_data-sds_acc_no = wa_output-sds_acc_no.
                        gs_data-tran_date = wa_output-tran_date.
                        gs_data-tran_date_to = wa_output-tran_date_to.
                        gs_data-smbc_remark = wa_output-smbc_remark.
                        gs_data-smbc_bal_amt = wa_output-smbc_bal_amt.
                        gs_data-record_type = wa_output-record_type.
                        gs_data-swift_bic = wa_output-swift_bic.
                        gs_data-acc_type = wa_output-acc_type.
                        gs_data-currency = wa_output-currency.
                        gs_data-acc_name = wa_output-acc_name.
                        gs_data-tran_type_name = wa_output-tran_type_name.
                        gs_data-cust_refer = wa_output-cust_refer.
                        gs_data-bank_refer = wa_output-bank_refer.
                        gs_data-KBANK_DETAIL = wa_output-KBANK_DETAIL.

                        gs_data-erdat = sy-datum.
                        gs_data-erzet = sy-timlo.
                        gs_data-usnam = sy-uname.
                        APPEND gs_data TO gt_data.



                   ENDLOOP.


                 ENDIF.
*
      ELSE.
         CONCATENATE sy-datum+6(2) '/' sy-datum+4(2) '/' sy-datum+0(4) INTO lv_date.
         CONCATENATE 'Data วันที่' lv_date 'ของ' p_bank 'ถูก Upload แล้ว  '  INTO msg_show SEPARATED BY SPACE .
*         MESSAGE i000(38) WITH 'Already upload data of bank ' DISPLAY LIKE 'E'.
      ENDIF.
ENDFORM.                    " F_UPLOAD_DATA

*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA_UNMAP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_data_viewbank .

  DATA: lv_adrnr1 TYPE kna1-adrnr.
  DATA: lv_vbeln TYPE vbrk-vbeln.



    IF r_del IS NOT INITIAL.
        SELECT hbkid zbank_item cheque_date debit_amt
               received_amount flag_bank_fee line_map_bank_fe bank_desc
               bal_amt channel branch_code branch_name terminal_id
               cheque_no tran_code sender_bank sender_branch
               sender_acc_no sender_acc_name sds_acc_no tran_date
               tran_date_to smbc_remark smbc_bal_amt record_type
               swift_bic acc_type currency acc_name tran_type_name
               cust_refer bank_refer erdat erzet usnam tranf run_id payin
               map_status map_date map_time status_lock lock_erdat
               lock_erzet lock_usnam
          INTO TABLE gt_ZSDSFIT042
          FROM ZSDSFIT042
          WHERE hbkid  EQ p_bank
           AND   erdat  IN s_erdat
           AND   tranf IN s_tranf "CH3 Add by WAntanee 20210216
           AND delete_flag NE 'X'.
*          AND   erdat  EQ p_erdat.
     ELSEIF r_lock IS NOT INITIAL OR r_locka IS NOT INITIAL.
          IF p_bank IS INITIAL.
               SELECT hbkid zbank_item cheque_date debit_amt
                    received_amount flag_bank_fee line_map_bank_fe bank_desc
                    bal_amt channel branch_code branch_name terminal_id
                    cheque_no tran_code sender_bank sender_branch
                    sender_acc_no sender_acc_name sds_acc_no tran_date
                    tran_date_to smbc_remark smbc_bal_amt record_type
                    swift_bic acc_type currency acc_name tran_type_name
                    cust_refer bank_refer erdat erzet usnam tranf run_id payin
                    map_status map_date map_time status_lock lock_erdat
                    lock_erzet lock_usnam
               INTO TABLE gt_ZSDSFIT042
               FROM ZSDSFIT042
               WHERE  erdat  IN s_erdat
                AND   tranf IN s_tranf "CH3 Add by WAntanee 20210216
                AND   status_lock  NE 'X' .
           ELSE.
               SELECT hbkid zbank_item cheque_date debit_amt
                      received_amount flag_bank_fee line_map_bank_fe bank_desc
                      bal_amt channel branch_code branch_name terminal_id
                      cheque_no tran_code sender_bank sender_branch
                      sender_acc_no sender_acc_name sds_acc_no tran_date
                      tran_date_to smbc_remark smbc_bal_amt record_type
                      swift_bic acc_type currency acc_name tran_type_name
                      cust_refer bank_refer erdat erzet usnam tranf run_id payin
                      map_status map_date map_time status_lock lock_erdat
                      lock_erzet lock_usnam
                  INTO TABLE gt_ZSDSFIT042
                  FROM ZSDSFIT042
                  WHERE hbkid  EQ p_bank
                  AND  erdat  IN s_erdat
                  AND   tranf IN s_tranf "CH3 Add by WAntanee 20210216
                  AND   status_lock  NE 'X' .
           ENDIF.
     ENDIF.



ENDFORM.                    " F_PROCESS_DATA


*&---------------------------------------------------------------------*
*&      Form  F_MAP_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_map_data_viewbank .
  DATA: lv_receive_amt TYPE ZSDSFIT042-received_amount.
  DATA: lv_check_recive TYPE c.
  DATA: lv_tabix TYPE sy-tabix.

  DATA: lv_adrnr1 TYPE kna1-adrnr.
  DATA: lv_vbeln TYPE vbrk-vbeln.
  DATA: lv_check_collector TYPE c.

     LOOP AT gt_ZSDSFIT042 INTO gw_ZSDSFIT042.
             CLEAR: gw_output.
             gw_output-hbkid = gw_ZSDSFIT042-hbkid.
             gw_output-zbank_item = gw_ZSDSFIT042-zbank_item.
             gw_output-cheque_date = gw_ZSDSFIT042-cheque_date.
             gw_output-debit_amt = gw_ZSDSFIT042-debit_amt.
             gw_output-received_amount = gw_ZSDSFIT042-received_amount.
             gw_output-bank_desc = gw_ZSDSFIT042-bank_desc.
             gw_output-bal_amt = gw_ZSDSFIT042-bal_amt.
             gw_output-channel = gw_ZSDSFIT042-channel.
             gw_output-branch_code = gw_ZSDSFIT042-branch_code.
             gw_output-branch_name = gw_ZSDSFIT042-branch_name.
             gw_output-terminal_id = gw_ZSDSFIT042-terminal_id.
             gw_output-cheque_no = gw_ZSDSFIT042-cheque_no.
             gw_output-tran_code = gw_ZSDSFIT042-tran_code.
             gw_output-sender_bank = gw_ZSDSFIT042-sender_bank.
             gw_output-sender_branch = gw_ZSDSFIT042-sender_branch.
             gw_output-sender_acc_no = gw_ZSDSFIT042-sender_acc_no.
             gw_output-sender_acc_name = gw_ZSDSFIT042-sender_acc_name.
             gw_output-sds_acc_no = gw_ZSDSFIT042-sds_acc_no.
             gw_output-tran_date = gw_ZSDSFIT042-tran_date.
             gw_output-tran_date_to = gw_ZSDSFIT042-tran_date_to.
             gw_output-smbc_remark = gw_ZSDSFIT042-smbc_remark.
             gw_output-smbc_bal_amt = gw_ZSDSFIT042-smbc_bal_amt.
             gw_output-record_type = gw_ZSDSFIT042-record_type.
             gw_output-swift_bic = gw_ZSDSFIT042-swift_bic.
             gw_output-acc_type = gw_ZSDSFIT042-acc_type.
             gw_output-currency = gw_ZSDSFIT042-currency.
             gw_output-acc_name = gw_ZSDSFIT042-acc_name.
             gw_output-tran_type_name = gw_ZSDSFIT042-tran_type_name.
             gw_output-cust_refer = gw_ZSDSFIT042-cust_refer.
             gw_output-bank_refer = gw_ZSDSFIT042-bank_refer.
             gw_output-erdat = gw_ZSDSFIT042-erdat.
             gw_output-erzet = gw_ZSDSFIT042-erzet.
             gw_output-usnam = gw_ZSDSFIT042-usnam.
             gw_output-tranf = gw_ZSDSFIT042-tranf.
             gw_output-payin = gw_ZSDSFIT042-payin.
             gw_output-map_status = gw_ZSDSFIT042-map_status.
             gw_output-map_date = gw_ZSDSFIT042-map_date.
             gw_output-map_time = gw_ZSDSFIT042-map_time.

             IF r_locka EQ 'X'.
               gw_output-check = 'X'.
             ENDIF.

             APPEND gw_output TO gt_output.






     ENDLOOP.





ENDFORM.                    " F_PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  F_PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_process_data .
  LOOP AT gt_data INTO gs_data.
    MOVE-CORRESPONDING gs_data TO gs_output.
    APPEND gs_output TO gt_output.
  ENDLOOP.
ENDFORM.                    " F_PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_display_report .

  PERFORM f_fill_fieldcat.

  PERFORM f_prepare_layout.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_save                   = gc_save
*     is_variant               = g_variant
      i_default                = 'X'
      it_fieldcat              = gt_fieldcat
      is_layout                = gs_layout
      i_callback_pf_status_set = 'STATUS_SET'
      i_callback_user_command  = 'USERCOMMAND'
    TABLES
      t_outtab                 = gt_output
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " F_DISPLAY_REPORT
* ----------------------------------------------------
* Status
* ----------------------------------------------------
FORM status_set USING rt_extab TYPE slis_t_extab.
  IF r_del IS NOT INITIAL.
      SET PF-STATUS 'ZSTANDARD_DEL' EXCLUDING rt_extab.
  ELSEIF r_lock IS NOT INITIAL OR r_locka IS NOT INITIAL.
      SET PF-STATUS 'ZSTANDARD_LOCK' EXCLUDING rt_extab.
  ELSE.
      SET PF-STATUS 'STANDARD' EXCLUDING rt_extab.
  ENDIF.
ENDFORM.                    "status_set
*&---------------------------------------------------------------------*
*&      Form  usercommand
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_UCOMM    text
*      -->I_SELFIELD text
*----------------------------------------------------------------------*
FORM usercommand USING i_ucomm i_selfield TYPE slis_selfield.

  DATA: lt_mess_tab      TYPE tab_bdcmsgcoll,
        lw_mess_tab      TYPE bdcmsgcoll.

  DATA: lv_mode    TYPE c VALUE 'N',
        lv_upd     TYPE c VALUE 'S',
        lv_msgtyp  TYPE c.


*&---------------------------------------------------------------------*
*&for Check = 'X' when tick Check Box
*&---------------------------------------------------------------------*
  data : ref_grid type ref to cl_gui_alv_grid.

  if ref_grid is initial.
    call function 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      importing
        e_grid = ref_grid.
    call method ref_grid->check_changed_data.
  endif.
*&---------------------------------------------------------------------*

   break wantanee.



  CASE i_ucomm.
    WHEN 'UPLOAD'.

      LOOP AT gt_data INTO gs_data.
        INSERT into ZSDSFIT042
         values gs_data.
         COMMIT WORK.
      ENDLOOP.
      message s000(38) with 'Upload Table ZTAP_STATE_BANK Complete'.
      LEAVE TO SCREEN 0.

   WHEN 'DELETE1'.
*           break wantanee.

*            UPDATE ZSDSFIT042
*            SET DELETE_FLAG = 'X'
*                DEL_ERDAT  = sy-datum
*                DEL_ERZET  = sy-timlo
*                DEL_USNAM  = sy-uname
*            WHERE bankl = p_bank
*            AND   erdat = p_erdat.

*           DELETE FROM ZSDSFIT042
*           WHERE bankl = p_bank
*           AND   erdat = p_erdat.
*               COMMIT WORK.
           BREAK wantanee.
           LOOP AT gt_output INTO gw_output WHERE check EQ 'X'..

                 UPDATE ZSDSFIT042
                 SET DELETE_FLAG = 'X'
                     DEL_ERDAT  = sy-datum
                     DEL_ERZET  = sy-timlo
                     DEL_USNAM  = sy-uname
                 WHERE hbkid = gw_output-hbkid
                 AND   zbank_item = gw_output-zbank_item.

*                DELETE FROM ZSDSFIT042
*                WHERE bankl = p_bank
*                AND   erdat = p_erdat.
                    COMMIT WORK.
            ENDLOOP.
        message s000(38) with 'Delete Tabel ZTAP_STATE_BANK Complete'.
        LEAVE TO SCREEN 0.
     WHEN 'LOCK1'.
*           break wantanee.

           LOOP AT gt_output INTO gw_output WHERE check EQ 'X'.. "CH4 Add by Wantanee 20210216

                 UPDATE ZSDSFIT042
                 SET STATUS_LOCK = 'X'
                     LOCK_ERDAT  = sy-datum
                     LOCK_ERZET  = sy-timlo
                     LOCK_USNAM  = sy-uname
                 WHERE hbkid = gw_output-hbkid
                 AND   zbank_item = gw_output-zbank_item.

*                DELETE FROM ZSDSFIT042
*                WHERE bankl = p_bank
*                AND   erdat = p_erdat.
                    COMMIT WORK.
            ENDLOOP.
        message s000(38) with 'Lock Data of Tabel ZTAP_STATE_BANK Complete'.
        LEAVE TO SCREEN 0.
    WHEN OTHERS.

  ENDCASE.
*
*  i_selfield-refresh = 'X'.
*  i_selfield-col_stable = 'X'.
*  i_selfield-row_stable = 'X'.

ENDFORM.                    "USERCOMMAND
*&---------------------------------------------------------------------*
*&      Form  F_FILL_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_fill_fieldcat .
  DATA: lv_column TYPE i.

   IF r_del EQ 'X'.
*       gs_fieldcat-fieldname   = 'CHECK'.
*       gs_fieldcat-seltext_s   = 'Check'.
*       gs_fieldcat-seltext_m   = 'Check'.
*       gs_fieldcat-seltext_l   = 'Check'.
*       gs_fieldcat-checkbox    = 'X'.
*       gs_fieldcat-input       = 'X'.
*       gs_fieldcat-edit        = 'X'.
*      APPEND gs_fieldcat TO gt_fieldcat.
      ADD 1 TO lv_column.
      m_fill_cat 'GT_OUTPUT' 'CHECK' lv_column 'Mark del' space 10 'X' 'X'.
   ELSEIF r_lock EQ 'X'  OR r_locka EQ 'X'.  "CH3 Add by WAntanee 20210216
      ADD 1 TO lv_column.
      m_fill_cat 'GT_OUTPUT' 'CHECK' lv_column 'Lock Data' space 10 'X' 'X'.
    "CH3 End Add by WAntanee 20210216
   ENDIF.


   IF p_bank = 'BAY01'.
*         -Bank code
           ADD 1 TO lv_column.
           m_fill_cat 'GT_OUTPUT' 'HBKID' lv_column 'Bank Code' space 10 '' ''.
*         *-Item
           ADD 1 TO lv_column.
           m_fill_cat 'GT_OUTPUT' 'ZBANK_ITEM' lv_column 'Item' space 10 '' ''.
*         *-date
           ADD 1 TO lv_column.
           m_fill_cat 'GT_OUTPUT' 'CHEQUE_DATE' lv_column 'วันที่' space 10 '' ''.
*         *TRAN_CODE
           ADD 1 TO lv_column.
           m_fill_cat 'GT_OUTPUT' 'TRAN_CODE' lv_column 'รหัส' space 15 '' ''.
*         *-Description
           ADD 1 TO lv_column.
           m_fill_cat 'GT_OUTPUT' 'BANK_DESC' lv_column 'หมายเลขอ้างอิง' space 20 '' ''.
*         *-Debit
           ADD 1 TO lv_column.
           m_fill_cat 'GT_OUTPUT' 'DEBIT_AMT' lv_column 'เดบิต' space 20 '' ''.

*         *-Credit
           ADD 1 TO lv_column.
           m_fill_cat 'GT_OUTPUT' 'RECEIVED_AMOUNT' lv_column 'เครดิต' space 20 '' ''.
*         *-Balance
           ADD 1 TO lv_column.
           m_fill_cat 'GT_OUTPUT' 'BAL_AMT' lv_column 'Balance' space 20 '' ''.
*         *-CHANNEL
           ADD 1 TO lv_column.
           m_fill_cat 'GT_OUTPUT' 'CHANNEL' lv_column 'ช่องทาง' space 20 '' ''.
*         *-Branch Code
           ADD 1 TO lv_column.
           m_fill_cat 'GT_OUTPUT' 'BRANCH_CODE' lv_column 'รหัสสาขา' space 20 '' ''.

    ELSEIF p_bank = 'BBL01'.

*          -Bank code
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'HBKID' lv_column 'Bank Code' space 10 '' ''.
*          *-Item
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'ZBANK_ITEM' lv_column 'Item' space 10 '' ''.
*          *-Tran Date
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'TRAN_DATE' lv_column 'Tran Date' space 10 '' ''.
*          *-Value Date
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'CHEQUE_DATE' lv_column 'Value Date' space 10 '' ''.
*          *-Description
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'BANK_DESC' lv_column 'Description' space 20 '' ''.
*          *TRAN_CODE
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'TRAN_CODE' lv_column 'Tran Code' space 15 '' ''.
*          *Cheque No.
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'CHEQUE_NO' lv_column 'Cheque No.' space 15 '' ''.
*          *-Debit
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'DEBIT_AMT' lv_column 'Debit' space 20 '' ''.

*          *-Credit
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'RECEIVED_AMOUNT' lv_column 'Credit' space 20 '' ''.
*          *-Balance
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'BAL_AMT' lv_column 'Balance' space 20 '' ''.
*          *-Channel
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'CHANNEL' lv_column 'Channel' space 20 '' ''.
*          *-Terminal Id
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'TERMINAL_ID' lv_column 'Terminal Id' space 20 '' ''.
*          *-Branch
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'BRANCH_NAME' lv_column 'Branch' space 20 '' ''.
*          *-Sender Bank
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'SENDER_BANK' lv_column 'Sender Bank' space 20 '' ''.
*          *-Sender Branch
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'SENDER_BRANCH' lv_column 'Sender Branch' space 20 '' ''.
*          *-Sender Account Number
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'SENDER_ACC_NO' lv_column 'Sender Acc.No.' space 20 '' ''.
*          *-Upload date
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'ERDAT' lv_column 'Upload date' space 20 '' ''.


    ELSEIF p_bank = 'BTMU'.

*          -Bank code
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'HBKID' lv_column 'Bank Code' space 10 '' ''.
*          *-Item
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'ZBANK_ITEM' lv_column 'Item' space 10 '' ''.
*          *-Record Type
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'RECORD_TYPE' lv_column 'Record Type' space 10 '' ''.
*          *-Branch
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'BRANCH_NAME' lv_column 'Branch Name' space 20 '' ''.
*          *-SWIFT_BIC
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'SWIFT_BIC' lv_column 'SWIFT BIC' space 20 '' ''.
*          *-Account Type
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'ACC_TYPE' lv_column 'Account Type' space 20 '' ''.
*          *-Currency
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'CURRENCY' lv_column 'Currency' space 20 '' ''.
*          *-Account No.
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'SDS_ACC_NO' lv_column 'SDS Account No.' space 20 '' ''.
*          *-ACC_NAME
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'ACC_NAME' lv_column 'SDS Account Name' space 20 '' ''.
*          *-Booking Date
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'TRAN_DATE' lv_column 'Booking Date' space 10 '' ''.
*          *-Value Date
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'CHEQUE_DATE' lv_column 'Value Date' space 10 '' ''.
*          *-Transaction Type Name
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'TRAN_TYPE_NAME' lv_column 'Trans. Type Name' space 10 '' ''.
*          *-Debit
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'DEBIT_AMT' lv_column 'Debit' space 20 '' ''.
*          *-Credit
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'RECEIVED_AMOUNT' lv_column 'Credit' space 20 '' ''.
*          *-Opening / Closing Balance
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'BAL_AMT' lv_column 'Opening / Closing Balance' space 20 '' ''.

*          *-Customer Reference
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'CUST_REFER' lv_column 'Customer Refer.' space 10 '' ''.
*          *-Detail Information
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'BANK_DESC' lv_column 'Detail Information' space 20 '' ''.
*          *-Bank Reference
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'BANK_REFER' lv_column 'Bank Reference' space 20 '' ''.
*          *-Upload date
            ADD 1 TO lv_column.
            m_fill_cat 'GT_OUTPUT' 'ERDAT' lv_column 'Upload date' space 20 '' ''.



    ELSEIF p_bank = 'KBK01'.

*      -Bank code
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'HBKID' lv_column 'Bank Code' space 10 '' ''.
*      *-Item
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'ZBANK_ITEM' lv_column 'Item' space 10 '' ''.
*      *-Value Date
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'CHEQUE_DATE' lv_column 'วันที่รายการมีผล' space 10 '' ''.
*      *-Bank value time
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'B_VALUE_TIME' lv_column 'Bank value time' space 10 '' ''.

*      *-Detail Information
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'BANK_DESC' lv_column 'รายละเอียดรายการ' space 20 '' ''.
*      *-Cheque No
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'CHEQUE_NO' lv_column 'เลขที่เช็ค' space 10 '' ''.
*      *-Debit
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'DEBIT_AMT' lv_column 'จำนวนเงินหักบัญชี' space 20 '' ''.
*      *-Credit
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'RECEIVED_AMOUNT' lv_column 'จำนวนเงินนำฝากเข้าบัญชี' space 20 '' ''.
*      *-Opening / Closing Balance
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'BAL_AMT' lv_column 'ยอดคงเหลือ' space 20 '' ''.
*                         *-หมายเลข
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'TERMINAL_ID' lv_column 'หมายเลข' space 20 '' ''..
*       *-Branch Code
         ADD 1 TO lv_column.
         m_fill_cat 'GT_OUTPUT' 'BRANCH_CODE' lv_column 'สาขา' space 20 '' ''.
*      *-Booking Date
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'TRAN_DATE' lv_column 'วันที่ทำรายการ' space 10 '' ''.
*      *-Chanel
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'CHANNEL' lv_column 'Channel' space 10 '' ''.
*      *-Booking Date
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'KBANK_DETAIL' lv_column 'Detail' space 10 '' ''.


*       *-Upload date
         ADD 1 TO lv_column.
         m_fill_cat 'GT_OUTPUT' 'ERDAT' lv_column 'Upload date' space 20 '' ''.

    ELSEIF p_bank = 'KTB01'.

*      -Bank code
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'HBKID' lv_column 'Bank Code' space 10 '' ''.
*      *-Item
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'ZBANK_ITEM' lv_column 'Item' space 10 '' ''.
*      *-Date
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'CHEQUE_DATE' lv_column 'Date' space 10 '' ''.
*      *-Bank value time
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'B_VALUE_TIME' lv_column 'Bank value time' space 10 '' ''.
*      *-Teller Id
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'TERMINAL_ID' lv_column 'Teller Id' space 10 '' ''.
*       *TRAN_CODE
         ADD 1 TO lv_column.
         m_fill_cat 'GT_OUTPUT' 'TRAN_CODE' lv_column 'Tran Code' space 15 '' ''.
*      *-Description
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'BANK_DESC' lv_column 'Description' space 20 '' ''.
*      *-Cheque No
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'CHEQUE_NO' lv_column 'Cheque No.' space 10 '' ''.
*      *-Amount
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'RECEIVED_AMOUNT' lv_column 'Amount' space 20 '' ''.
*      *-Balance
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'BAL_AMT' lv_column 'Balance' space 20 '' ''.
*       *-Init Br
         ADD 1 TO lv_column.
         m_fill_cat 'GT_OUTPUT' 'BRANCH_CODE' lv_column 'Init Br' space 20 '' ''.
*       *-Upload date
         ADD 1 TO lv_column.
         m_fill_cat 'GT_OUTPUT' 'ERDAT' lv_column 'Upload date' space 20 '' ''.


    ELSEIF p_bank = 'SCB01'.

*      -Bank code
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'HBKID' lv_column 'Bank Code' space 10 '' ''.
*      *-Item
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'ZBANK_ITEM' lv_column 'Item' space 10 '' ''.
*      *-SDS Acc. No.
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'SDS_ACC_NO' lv_column 'SDS Acc. No.' space 10 '' ''.
*      *-Date
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'CHEQUE_DATE' lv_column 'Date' space 10 '' ''.
*      *-Bank value time
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'B_VALUE_TIME' lv_column 'Bank value time' space 10 '' ''.

*       *TRAN_CODE
         ADD 1 TO lv_column.
         m_fill_cat 'GT_OUTPUT' 'TRAN_CODE' lv_column 'Tran Code' space 10 '' ''.
*       *Channel
         ADD 1 TO lv_column.
         m_fill_cat 'GT_OUTPUT' 'CHANNEL' lv_column 'Channel' space 10 '' ''.
*      *-Cheque No
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'CHEQUE_NO' lv_column 'Cheque No.' space 10 '' ''.
*      *-Debit
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'DEBIT_AMT' lv_column 'Debit' space 20 '' ''.
*      *-Credit
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'RECEIVED_AMOUNT' lv_column 'Credit' space 20 '' ''.
*      *-Balance
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'BAL_AMT' lv_column 'Balance' space 20 '' ''.
*      *-Description
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'BANK_DESC' lv_column 'Description' space 20 '' ''.
*     *-Upload date
       ADD 1 TO lv_column.
       m_fill_cat 'GT_OUTPUT' 'ERDAT' lv_column 'Upload date' space 20 '' ''.


    ELSEIF p_bank = 'TMB01'.

*      -Bank code
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'HBKID' lv_column 'Bank Code' space 10 '' ''.
*      *-Item
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'ZBANK_ITEM' lv_column 'Item' space 10 '' ''.
*      *-SDS Acc. No.
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'SDS_ACC_NO' lv_column 'SDS เลขที่บัญชี' space 10 '' ''.
*      *-Booking Date
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'TRAN_DATE' lv_column 'วันที่ทำรายการ' space 10 '' ''.
*      *-Date
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'CHEQUE_DATE' lv_column 'วันที่มีผล' space 10 '' ''.
*       *TRAN_CODE
         ADD 1 TO lv_column.
         m_fill_cat 'GT_OUTPUT' 'TRAN_CODE' lv_column 'รหัสประเภทรายการ' space 10 '' ''.
*      *-Cheque No
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'CHEQUE_NO' lv_column 'เลขที่เช็ค' space 10 '' ''.

*      *-Debit
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'DEBIT_AMT' lv_column 'ถอน' space 20 '' ''.
*      *-Credit
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'RECEIVED_AMOUNT' lv_column 'ฝาก' space 20 '' ''.
*      *-Balance
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'BAL_AMT' lv_column 'ยอดคงเหลือ' space 20 '' ''.
*      *-Teller Id
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'TERMINAL_ID' lv_column 'ช่องทางทำรายการ' space 10 '' ''.
*      *-Description
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'BANK_DESC' lv_column 'รายละเอียดการทำธุรกรรม' space 20 '' ''.
*      *-Upload date
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'ERDAT' lv_column 'Upload date' space 20 '' ''.



    ELSEIF p_bank = 'UOB'.

*      -Bank code
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'HBKID' lv_column 'Bank Code' space 10 '' ''.
*      *-Item
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'ZBANK_ITEM' lv_column 'Item' space 10 '' ''.
*      *-SDS Acc. No.
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'SDS_ACC_NO' lv_column 'SDS Acc.No.' space 10 '' ''.
*      *-Date
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'CHEQUE_DATE' lv_column 'Value Date' space 10 '' ''.
*      *-Bank value time
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'B_VALUE_TIME' lv_column 'Bank value time' space 10 '' ''.

*      *-Booking Date
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'TRAN_DATE' lv_column 'Date' space 10 '' ''.
*      *-Description
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'BANK_DESC' lv_column 'Description' space 20 '' ''.
*      *-Cheque No
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'CHEQUE_NO' lv_column 'Cheque No' space 10 '' ''.
*      *-Credit
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'RECEIVED_AMOUNT' lv_column 'Deposit' space 20 '' ''.
*      *-Balance
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'BAL_AMT' lv_column 'Ledger Balance' space 20 '' ''.
*       *-Upload date
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'ERDAT' lv_column 'Upload date' space 20 '' ''.


    ELSEIF p_bank = 'SMB01'.

*      -Bank code
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'HBKID' lv_column 'Bank Code' space 10 '' ''.
*      *-Item
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'ZBANK_ITEM' lv_column 'Item' space 10 '' ''.
*       *-Branch Code
         ADD 1 TO lv_column.
         m_fill_cat 'GT_OUTPUT' 'BRANCH_CODE' lv_column 'Branch Code' space 20 '' ''.

*      *-ACC_NAME
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'ACC_NAME' lv_column 'SDS Account Name' space 20 '' ''.
*      *-Date
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'TRAN_DATE' lv_column 'Date From' space 10 '' ''.
*      *-Date
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'TRAN_DATE_TO' lv_column 'Date To' space 10 '' ''.
*      *-SDS Acc. No.
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'SDS_ACC_NO' lv_column 'SDS Acc. No.' space 10 '' ''.
*      *-Date
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'CHEQUE_DATE' lv_column 'Date' space 10 '' ''.
*      *-Debit
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'DEBIT_AMT' lv_column 'Debit' space 20 '' ''.
*      *-Credit
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'RECEIVED_AMOUNT' lv_column 'Credit' space 20 '' ''.
*      *-Balance
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'SMBC_BAL_AMT' lv_column 'Balance' space 20 '' ''.
*      *-Description
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'BANK_DESC' lv_column 'Description' space 20 '' ''.
*      *-Cheque No
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'CHEQUE_NO' lv_column 'Cheque No.' space 10 '' ''.
*      *-Remarks
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'SMBC_REMARK' lv_column 'Remarks' space 20 '' ''.

*      *-AvailableBalance
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'BAL_AMT' lv_column 'AvailableBalance' space 20 '' ''.
*   *  -Upload date
        ADD 1 TO lv_column.
        m_fill_cat 'GT_OUTPUT' 'ERDAT' lv_column 'Upload date' space 20 '' ''.

    ELSEIF p_bank IS INITIAL.
        IF r_lock IS NOT INITIAL OR r_locka IS NOT INITIAL.
*             -Bank code
               ADD 1 TO lv_column.
               m_fill_cat 'GT_OUTPUT' 'HBKID' lv_column 'Bank Code' space 10 '' ''.
*             *-Item
               ADD 1 TO lv_column.
               m_fill_cat 'GT_OUTPUT' 'ZBANK_ITEM' lv_column 'Item' space 10 '' ''.
*             *-Date
               ADD 1 TO lv_column.
               m_fill_cat 'GT_OUTPUT' 'CHEQUE_DATE' lv_column 'Date' space 10 '' ''.
*             *-Credit
               ADD 1 TO lv_column.
               m_fill_cat 'GT_OUTPUT' 'RECEIVED_AMOUNT' lv_column 'Credit' space 20 '' ''.
*             *-Description
               ADD 1 TO lv_column.
               m_fill_cat 'GT_OUTPUT' 'BANK_DESC' lv_column 'Description' space 20 '' ''.
*              *-Upload date
               ADD 1 TO lv_column.
               m_fill_cat 'GT_OUTPUT' 'ERDAT' lv_column 'Upload date' space 20 '' ''.
*              *-Collector map
               ADD 1 TO lv_column.
               m_fill_cat 'GT_OUTPUT' 'TRANF' lv_column 'Collector map' space 20 '' ''.
*              *-Pay in
               ADD 1 TO lv_column.
               m_fill_cat 'GT_OUTPUT' 'PAYIN' lv_column 'Pay in' space 20 '' ''.
*              *-Status Map
               ADD 1 TO lv_column.
               m_fill_cat 'GT_OUTPUT' 'MAP_STATUS' lv_column 'Status Map' space 20 '' ''.
*              *-Map Date
               ADD 1 TO lv_column.
               m_fill_cat 'GT_OUTPUT' 'MAP_DATE' lv_column 'Map Date' space 20 '' ''.
                            gw_output-tranf = gw_ZSDSFIT042-tranf.
*              *-Map Time
               ADD 1 TO lv_column.
               m_fill_cat 'GT_OUTPUT' 'MAP_TIME' lv_column 'Map Time' space 20 '' ''.
                            gw_output-tranf = gw_ZSDSFIT042-tranf.

         ENDIF.
    ENDIF.


*              *-Map Time
               ADD 1 TO lv_column.
               m_fill_cat 'GT_OUTPUT' 'TRANF' lv_column 'Collector No.' space 20 '' ''.
                            gw_output-tranf = gw_ZSDSFIT042-tranf.



*MANDT  Client
*BANKL  Bank Keys
*ZBANK_ITEM	Statement bank item
*CHEQUE_DATE  date for payment by cheque
*DEBIT_AMT  Amount
*RECEIVED_AMOUNT  Amount
*FLAG_BANK_FEE  Flag bank fee
*LINE_MAP_BANK_FE	Statement bank item
*BANK_DESC  Bank description
*BAL_AMT  Amount
*CHANNEL  Channel
*BRANCH_CODE  Branch Code
*BRANCH_NAME  Branch Name
*TERMINAL_ID  Terminal Id
*CHEQUE_NO  Cheque No
*TRAN_CODE  Tran Code
*SENDER_BANK  Sender Bank
*SENDER_BRANCH  Sender Branch
*SENDER_ACC_NO  Sender Account Number
*SENDER_ACC_NAME  Sender Account Name
*SDS_ACC_NO	SDS Account No
*TRAN_DATE  Transection date Or Booking Date
*TRAN_DATE_TO	Transection date to Or Booking Date to
*SMBC_REMARK  SMBC Remark
*SMBC_BAL_AMT	Amount
*RECORD_TYPE  Record Type
*SWIFT_BIC  Swift Bic
*ACC_TYPE	Account Type
*CURRENCY	CURRENCY
*ACC_NAME	Account Name
*TRAN_TYPE_NAME	Trans Type Name
*CUST_REFER	Cust Refer
*BANK_REFER	Bank Refer
*ERDAT  Date on Which Record Was Created
*ERZET  Entry time
*USNAM  User Name
*TRANF  Transfer No.
*MAP_STATUS	Mapping Status with collector
*MAP_DATE	Mapping Date with collector
*MAP_TIME	Mapping Time with collector



ENDFORM.                    " F_FILL_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_prepare_layout .
*  gs_layout-box_fieldname      = 'SEL'.
  gs_layout-zebra              = 'X'.     " striped pattern
ENDFORM.                    " F_PREPARE_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  F_ADD_LIST_BANK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_add_list_bank.
*   bankl             TYPE  ztfi_bank-bankl1,  "Bank code
*         banka             TYPE  bnka-banka,  "Bank code
  break wantanee.
     SELECT HBKID BANK_NAME
       INTO TABLE gt_bank
       FROM ZSDSFIT044 .

  LOOP AT gt_bank INTO gs_bank.. "GM Only
    value-key = gs_bank-HBKID.
    value-text = gs_bank-BANK_NAME.
*    CONCATENATE gs_bank-bankl1 gs_bank-banka  INTO value-text SEPARATED BY '-'.
    APPEND value TO list.
  ENDLOOP.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_BANK'
      values = list.
ENDFORM.                    " F_CHECK_ERROR
*&---------------------------------------------------------------------*
*&      Module  TC_100_CHANGE_TC_ATTR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_100_CHANGE_TC_ATTR OUTPUT.
*    describe table gt_input lines tc_100-lines.
    tc_100-lines = fill.
ENDMODULE.                 " TC_100_CHANGE_TC_ATTR  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_100_GET_LINES  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_100_GET_LINES OUTPUT.
     g_tc_100_lines = sy-loopc.
ENDMODULE.                 " TC_100_GET_LINES  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_TC_100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_TC_100 OUTPUT.

ENDMODULE.                 " MODIFY_TC_100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  set pf-status 'STANDARD'.
  g_tc_100_lines = fill.
*  set titlebar '100' with 'Input Bank detail'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_100_MODIFY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_100_MODIFY INPUT.

*   bankl             TYPE  ZSDSFIT042-bankl,  "Bank code
*          zbank_item        TYPE  ZSDSFIT042-zbank_item,   "Item
*          cheque_date       TYPE  ZSDSFIT042-cheque_date,  "date
*          received_amount   TYPE  ZSDSFIT042-received_amount, "Amount
*          flag_bank_fee     TYPE  ZSDSFIT042-flag_bank_fee,   "Flag status bank fee
*          line_map_bank_fe  TYPE  ZSDSFIT042-line_map_bank_fe, "Item line mapping bank free
*          bank_desc         TYPE  ZSDSFIT042-bank_desc,       "Description
     gw_input-hbkid = p_bank.
     lv_linecount = lines( gt_input ).
  if tc_100-current_line gt lv_linecount.
        fill = fill + 1.
    if gw_input-cheque_date IS NOT INITIAL .

*       perform get_customer using gw_inst_up_cust_bank-kunnr '' changing gw_inst_up_cust_bank-cust_name lv_name_eng.
*       gw_inst_up_cust_bank-bank_key =  gw_inst_up_cust_bank-bank_key.
       append gw_input to gt_input.
    endif.
  else.

*       gw_input-up_flag = 'X'.
       modify gt_input
       from gw_input
       index tc_100-current_line.
  endif.
ENDMODULE.                 " TC_100_MODIFY  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  DATA: lv_item TYPE ZSDSFIT042-zbank_item.
  DATA: lv_year(4) TYPE c,
        lv_month(2) TYPE c,
        lv_day(2) TYPE c.
  case sy-ucomm.
    when '&F15'. "'EXIT'.
      leave to screen 0.
    when '&F03'.  "'BACK'.
      leave to screen 0.

    when '&F12'. "'CANCEL'.
      leave to screen 0.

    when 'UPLOAD'.
      break wantanee.
*
*gt_data   TYPE TABLE OF ty_ZSDSFIT042,
*      gs_data   TYPE ty_ZSDSFIT042,

           SELECT max( zbank_item )
           INTO lv_item
           FROM ZSDSFIT042.
           IF lv_item IS INITIAL.
              lv_item = 0.
           ENDIF.

           LOOP AT gt_input INTO gw_input.
             CLEAR: gs_data.
             lv_item = lv_item + 1.
             gs_data-mandt = sy-mandt.
             gs_data-hbkid = p_bank.
             gs_data-zbank_item = lv_item.
             gs_data-cheque_date = gw_input-cheque_date.
             gs_data-received_amount = gw_input-received_amount.
             gs_data-bank_desc = gw_input-bank_desc.
             gs_data-erdat = sy-datum.
             gs_data-erzet = sy-timlo.
             gs_data-usnam = sy-uname.
             APPEND gs_data TO gt_data.



           ENDLOOP.

            LOOP AT gt_data INTO gs_data.
              INSERT INTO ZSDSFIT042
               VALUES gs_data.
               COMMIT WORK.
            ENDLOOP.
            message s000(38) with 'Upload Table ZTAP_STATE_BANK Complete'.


  endcase.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
