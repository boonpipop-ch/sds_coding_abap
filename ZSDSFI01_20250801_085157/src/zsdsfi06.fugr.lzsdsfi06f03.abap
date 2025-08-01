*----------------------------------------------------------------------*
***INCLUDE LZETAXF03.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form FTP_PDF_ZIP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_TEXT
*&      <-- LV_FILENAME_ZIP
*&      <-- LV_FTPPATH_FILENAME_ZIP
*&      <-- LV_COUNT_PDF_OK
*&---------------------------------------------------------------------*
FORM ftp_pdf_zip  USING pt_txt                  TYPE ZSDSFIS033_TT
               CHANGING ch_filename_zip         TYPE string
                        ch_ftppath_filename_zip TYPE string
                        ch_count_pdf_ok         TYPE i
                        ch_reverse_flag.

  DATA: lo_zip       TYPE REF TO cl_abap_zip,
        lo_exception TYPE REF TO cx_root,
        lv_filename  TYPE string.

  DATA: lwa_msg     TYPE ZSDSFIS034,
        lwa_msg_err TYPE ZSDSFIS034,
        lwa_msg_suc TYPE ZSDSFIS034,
        lwa_return  TYPE bapiret2.

  DATA: lv_path_filename_zip TYPE string,
        lv_ftp_path          TYPE string,
        lv_ftp_fullpath      TYPE string,
        lv_arc_fullpath      TYPE string.

  DATA: lv_content       TYPE xstring,
        lv_message       TYPE string,
        lv_batch_message TYPE string,
        lv_x_fopen       TYPE flag,
        lv_x_fread       TYPE flag,
        lv_x_zip_ok      TYPE flag,
        lv_del_subrc     TYPE sy-subrc.

  FIELD-SYMBOLS: <lfs_txt> LIKE LINE OF pt_txt,
                 <lfs_pdf> TYPE ZSDSFIS025.

  CLEAR: ch_filename_zip,
         ch_ftppath_filename_zip,
         ch_reverse_flag.

  ch_count_pdf_ok = 0.

  PERFORM message_for_batch_d USING 'Start: Create ZIP(PDF)'.

  CREATE OBJECT lo_zip.
  LOOP AT pt_txt ASSIGNING <lfs_txt>.

    ch_reverse_flag = <lfs_txt>-reverse_flag.

    IF <lfs_txt>-x_pdf EQ abap_true.  "This record has PDF file.

      <lfs_txt>-file_status = gc_ftp_pdf.
      LOOP AT <lfs_txt>-etax_pdf ASSIGNING <lfs_pdf>.
*        CLEAR: lwa_return_tf,
*               lv_ftp_fullpath,
*               lv_arc_fullpath.
*
        "---Get 'Filename.ZIP'
        IF ch_filename_zip  IS INITIAL.
*
          PERFORM gen_zip_filename USING <lfs_pdf>-file_name
                                CHANGING ch_filename_zip
                                         lv_path_filename_zip.

          PERFORM message_for_batch_d USING ch_filename_zip.

        ENDIF.


*       ---ADD file.pdf to lo_zip
        CLEAR: lv_content,
               lv_message,
               lv_batch_message,
               lv_x_fopen,
               lv_x_fread.

*>>> EGCO wait for script
        TRY.
            OPEN DATASET <lfs_pdf>-file_path FOR INPUT IN BINARY MODE
                 MESSAGE lv_message.
            IF sy-subrc IS INITIAL.
              lv_x_fopen = abap_true.
              lv_message = space.
            ELSE.
              IF lv_message IS INITIAL.
                lv_message = 'Unable to open file'.
              ENDIF.
            ENDIF.
          CATCH cx_root INTO lo_exception.
*         Gets error message
            lv_message = lo_exception->get_text( ).

            IF lv_message IS INITIAL.
              lv_message = 'Unable to open file'.
            ENDIF.
        ENDTRY.

        IF lv_x_fopen EQ abap_true.
          TRY.
              READ DATASET <lfs_pdf>-file_path INTO lv_content .
              IF sy-subrc IS INITIAL.
                lv_x_fread = abap_true.
              ELSE.
                lv_message = 'Unable to read file'.
              ENDIF.
            CATCH cx_root INTO lo_exception.
*           Gets error message
              CALL METHOD lo_exception->if_message~get_text
                RECEIVING
                  result = lv_message.
              IF lv_message IS INITIAL.
                lv_message = 'Unable to read file'.
              ENDIF.
          ENDTRY.

          CLOSE DATASET <lfs_pdf>-file_path.
          IF lv_x_fread EQ abap_true.
            CLEAR lv_filename.
            lv_filename = <lfs_pdf>-file_name.
            lo_zip->add( name = lv_filename content = lv_content ).
          ENDIF.
        ENDIF.

        IF lv_x_fread EQ abap_false.
          IF lv_message IS INITIAL.
            lv_message = 'Unable to read file'.
          ENDIF.

          CONCATENATE lv_message <lfs_pdf>-file_path
                 INTO lv_message SEPARATED BY space.

          <lfs_txt>-wa_msg-msgty   = gc_error.
          <lfs_txt>-wa_msg-message = lv_message.
          lv_batch_message         = lv_message.

          CONCATENATE '>>' lv_batch_message
                 INTO lv_batch_message SEPARATED BY space.
        ENDIF.

        PERFORM message_for_batch_d USING <lfs_pdf>-file_name.

        IF lv_batch_message IS NOT INITIAL.
          PERFORM message_for_batch_d USING lv_batch_message.
        ENDIF.
*<<< EGCO wait for script

      ENDLOOP.

    ELSE.
      <lfs_txt>-file_status = gc_ftp_pdf_skip.

*      READ TABLE pt_bkpf INTO lwa_bkpf
*        WITH KEY bukrs = <lfs_txt>-comp
*                 belnr = <lfs_txt>-sap_doc_no
*                 gjahr = <lfs_txt>-fyear BINARY SEARCH.
*      IF lwa_bkpf-stblg IS NOT INITIAL OR
*         <lfs_txt>-doc_type = 'T07'.         "Reverse doc ไม่ต้องส่ง pdf
*        <lfs_txt>-wa_msg-msgty    = 'S'.
*        <lfs_txt>-wa_msg-message  = 'PDF is not required'.
*        ADD 1 TO ch_count_pdf_ok.
*
*        CLEAR lwa_pdf_skip.
*        lwa_pdf_skip-bukrs = <lfs_txt>-comp.
*        lwa_pdf_skip-belnr = <lfs_txt>-sap_doc_no.
*        lwa_pdf_skip-gjahr = <lfs_txt>-fyear.
*        APPEND lwa_pdf_skip TO pt_pdf_skip.
      IF <lfs_txt>-reverse_flag = 'Y'.
        <lfs_txt>-wa_msg-msgty    = 'S'.
        <lfs_txt>-wa_msg-message  = 'PDF is not required'.
        ADD 1 TO ch_count_pdf_ok.
      ELSE.
        <lfs_txt>-wa_msg-msgty    = 'E'.
        <lfs_txt>-wa_msg-message  = 'No PDF File'.
      ENDIF.
    ENDIF.

  ENDLOOP.

  PERFORM message_for_batch_d USING 'End: Create ZIP(PDF)'.

*>>> wait for script
* ---SAVE lo_zip to AL11 as ZIP file
  PERFORM upload_zip_to_al11    USING  lv_path_filename_zip
                              CHANGING lo_zip
                                       lwa_msg
                                       lv_x_zip_ok.
*<<< wait for script

* ---Transfer .ZIP to sFTP
  IF lwa_msg-msgty NE gc_error.
    CLEAR : lv_ftp_fullpath, lwa_msg, lv_ftp_path.
    IF lv_path_filename_zip IS NOT INITIAL.
*>>>  wait for script
      PERFORM transfer_to_ftp USING lv_path_filename_zip
                           CHANGING lv_ftp_fullpath
                                    lv_arc_fullpath
                                    lwa_msg.
*<<<  wait for script
      ch_ftppath_filename_zip = lv_ftp_fullpath.  "/EZTAX/DEV/COKE/IN/<filename>.ZIP
      lv_ftp_path             = lv_ftp_fullpath.  "/EZTAX/DEV/COKE/IN/
      REPLACE ch_filename_zip WITH space INTO lv_ftp_path.
      CONDENSE lv_ftp_path.
    ENDIF.
  ENDIF.

* ---RESULT : ถ้าส่ง .ZIP ผ่านแสดงว่า ผ่านทุกไฟล์ ให้ใส่ฟิลด์ msg_type
  LOOP AT pt_txt ASSIGNING <lfs_txt>.
    CLEAR : lwa_msg_err,
            lwa_msg_suc.

    IF <lfs_txt>-x_pdf EQ abap_true.  "This record has PDF file.
      <lfs_txt>-file_status = gc_ftp_pdf.

      LOOP AT <lfs_txt>-etax_pdf ASSIGNING <lfs_pdf>.
        CLEAR: "lwa_return_tf,
               lv_ftp_fullpath,
               lv_arc_fullpath.

        CONCATENATE lv_ftp_path <lfs_pdf>-file_name INTO lv_ftp_fullpath.
        <lfs_pdf>-ftp_fullpath = lv_ftp_fullpath.
        <lfs_pdf>-arc_fullpath = lv_path_filename_zip.
        <lfs_pdf>-msg_from     = 'C'.
        <lfs_pdf>-msg_type     = lwa_msg-msgty = gc_success.
        <lfs_pdf>-msg_text     = lwa_msg-message.

        IF <lfs_pdf>-msg_type EQ gc_success.
          lwa_msg_suc = lwa_msg.
        ELSE.
          lwa_msg_err = lwa_msg.
        ENDIF.
      ENDLOOP.

      IF lwa_msg_suc IS NOT INITIAL AND
         lwa_msg_err IS INITIAL.

        ADD 1 TO ch_count_pdf_ok.

        <lfs_txt>-x_pdf_completed = abap_true.
        <lfs_txt>-wa_msg-msgty    = lwa_msg_suc-msgty.
        <lfs_txt>-wa_msg-message  = lwa_msg_suc-message.

      ELSEIF lwa_msg_err IS NOT INITIAL.
        <lfs_txt>-wa_msg-msgty    = lwa_msg_err-msgty.
        <lfs_txt>-wa_msg-message  = lwa_msg_err-message.
      ENDIF.

    ENDIF.

  ENDLOOP.

  lv_x_zip_ok = abap_false. " wait for script

  IF lv_x_zip_ok EQ abap_true.
    CALL FUNCTION 'ZETX_DELETE_FILE'
      EXPORTING
        im_file_path = lv_path_filename_zip
      IMPORTING
        ex_subrc     = lv_del_subrc
        ex_return    = lwa_return.
  ENDIF.

ENDFORM.                    "ftp_pdf_zip
*&---------------------------------------------------------------------*
*& Form GEN_ZIP_FILENAME
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <LFS_PDF>_FILE_NAME
*&      <-- CH_FILENAME_ZIP
*&      <-- LV_PATH_FILENAME_ZIP
*&---------------------------------------------------------------------*
FORM gen_zip_filename  USING pi_file_name
                    CHANGING ch_filename_zip
                             ch_path_filename_zip.

  DATA: lv_filename      TYPE string,
        lv_path_filename TYPE string,
        lv_extension     TYPE string,
        lv_date_time     TYPE string,
        lv_sap_dir_out   TYPE string.

  PERFORM get_file_path.

  IF gs_file_path-sap_dir_out IS NOT INITIAL.
    lv_sap_dir_out = gs_file_path-sap_dir_out.
  ELSE.
    lv_sap_dir_out = '/sapdata/eztax/OUT/'.
  ENDIF.

  SPLIT pi_file_name AT '.' INTO lv_filename lv_extension.

  GET TIME.
  CONCATENATE sy-datum sy-uzeit
         INTO lv_date_time SEPARATED BY '_'.

  CONCATENATE lv_filename lv_date_time
         INTO lv_filename SEPARATED BY '_'.

  CONCATENATE lv_filename '.ZIP'
         INTO lv_filename.

  CONCATENATE lv_sap_dir_out '/' lv_filename
         INTO lv_path_filename.

  REPLACE ALL OCCURRENCES OF '//' IN lv_path_filename WITH '/'.

  ch_filename_zip      = lv_filename.
  ch_path_filename_zip = lv_path_filename.

ENDFORM.                    "gen_zip_filename
*&---------------------------------------------------------------------*
*&      Form  transfer_to_ftp
*&---------------------------------------------------------------------*
FORM transfer_to_ftp USING pi_source_fullpath
                  CHANGING ch_ftp_fullpath TYPE string
                           ch_arc_fullpath TYPE string
                           ch_wa_msg       TYPE ZSDSFIS034.

  CALL FUNCTION 'ZETX_SFTP'
    EXPORTING
      im_source_fullpath = pi_source_fullpath
    IMPORTING
      ex_ftp_fullpath    = ch_ftp_fullpath
      ex_arc_fullpath    = ch_arc_fullpath
      ex_wa_msg          = ch_wa_msg.

ENDFORM.                    "transfer_to_ftp
*&---------------------------------------------------------------------*
*& Form GET_FILENAME_FROM_FULLPATH
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IM_SOURCE_FULLPATH
*&      <-- LV_FILENAME
*&---------------------------------------------------------------------*
FORM get_filename_from_fullpath     USING pi_fullpath
                                 CHANGING ch_filename.

  DATA: lv_full_path TYPE string,
        lv_ex_path   TYPE string.

  lv_full_path = pi_fullpath.
  ch_filename  = space.

  CALL FUNCTION 'ZETX_GET_FILENAME_FROM_PATH'
    EXPORTING
      im_path_filename = lv_full_path
    IMPORTING
      ex_path          = lv_ex_path
      ex_filename      = ch_filename.

ENDFORM.                    "get_filename_from_fullpath
*&---------------------------------------------------------------------*
*& Form SXPG_COMMAND_EXECUTE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_EXEC_PROTOCOL
*&      --> P_
*&      --> LV_PARAMETERS
*&      --> P_
*&      <-- LV_STATUS
*&      <-- LV_EXITCODE
*&      <-- LV_RET_MESSAGE
*&      <-- LV_RET_TYPE
*&---------------------------------------------------------------------*
FORM sxpg_command_execute
      TABLES pt_exec_protocol
       USING pi_commandname TYPE sxpgcolist-name
             pi_parameters  TYPE sxpgcolist-parameters
             pi_remark      TYPE char10
    CHANGING ch_status      TYPE extcmdexex-status
             ch_exitcode    TYPE extcmdexex-exitcode
             ch_ret_message TYPE char100
             ch_ret_type    TYPE char1.

  CALL FUNCTION 'ZETX_SXPG_COMMAND_EXECUTE'
    EXPORTING
      commandname           = pi_commandname
      additional_parameters = pi_parameters
*     OPERATINGSYSTEM       = SY-OPSYS
*     TARGETSYSTEM          = SY-HOST
*     DESTINATION           =
*     STDOUT                = 'X'
*     STDERR                = 'X'
*     TERMINATIONWAIT       = 'X'
*     TRACE                 =
*     DIALOG                =
      remark                = pi_remark
    IMPORTING
      status                = ch_status
      exitcode              = ch_exitcode
      return_message        = ch_ret_message
      return_type           = ch_ret_type
    TABLES
      exec_protocol         = pt_exec_protocol.

ENDFORM.                    "sxpg_command_execute
*&---------------------------------------------------------------------*
*& Form FTP_TXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IM_WA_INF_FULLPATH_SERVER
*&      --> IM_WA_INF_EXE_TYPE
*&      <-- LT_TEXT
*&      <-- LWA_RETURN_TF
*&---------------------------------------------------------------------*
FORM ftp_txt  USING    pi_fullpath_server
                       pi_exe_type        TYPE ZSDSFIS049-exe_type
             CHANGING  cht_text           TYPE ZSDSFIS033_TT
                       ch_wa_mem_inf01    TYPE ZSDSFIS050
                       ch_wa_return_tf    TYPE ZSDSFIS051.

  DATA: lit_etax_textfile  TYPE ZSDSFIS033_TT.

  DATA: lwa_text           LIKE LINE OF lit_etax_textfile.

  DATA: lv_fullpath_server TYPE string,
        lv_ftp_fullpath    TYPE string,
        lv_arc_fullpath    TYPE string.

  FIELD-SYMBOLS:
    "<lfs_log_result>     LIKE LINE OF gth_log_result,
    <lfs_etax_textfile> LIKE LINE OF lit_etax_textfile,
    <lfs_text>          LIKE LINE OF lit_etax_textfile.

  CONSTANTS:
    lc_del               TYPE string VALUE '#DEL#'.


  PERFORM message_for_batch USING 'Download textfile to AL11.'.

*  CLEAR: ch_wa_mem_inf01-path_and_file_pc,
*         ch_wa_mem_inf01-path_and_file_server,
*         ch_wa_mem_inf01-trans_no.

  lv_fullpath_server = pi_fullpath_server.

  lit_etax_textfile[] = cht_text[].

  LOOP AT lit_etax_textfile ASSIGNING <lfs_etax_textfile>.
    IF <lfs_etax_textfile>-file_status EQ gc_ftp_pdf_skip.
    ELSE.
      IF <lfs_etax_textfile>-x_pdf_completed IS INITIAL     OR
         <lfs_etax_textfile>-file_status     NE gc_ftp_pdf  OR
         <lfs_etax_textfile>-wa_msg-msgty    EQ gc_error.
        <lfs_etax_textfile>-file_status = lc_del.  "mark for delete
      ENDIF.
    ENDIF.
  ENDLOOP.  "lit_etax_textfile

  DELETE lit_etax_textfile WHERE file_status = lc_del.

  IF lit_etax_textfile[] IS NOT INITIAL.

    CALL FUNCTION 'ZETX_DOWNLOAD_TEXTFILE'
      EXPORTING
        im_path_server           = lv_fullpath_server
        im_exe_type              = pi_exe_type
      IMPORTING
        ex_return_pc             = ch_wa_mem_inf01-path_and_file_pc
        ex_return_server         = ch_wa_mem_inf01-path_and_file_server
        ex_filename              = ch_wa_mem_inf01-filename
        ex_trans_no              = ch_wa_mem_inf01-trans_no
        ex_return_tf             = ch_wa_return_tf
        ex_ftp_fullpath          = lv_ftp_fullpath
        ex_arc_fullpath          = lv_arc_fullpath
      TABLES
        it_etax_textfile         = lit_etax_textfile
      EXCEPTIONS
        filepath_not_suppied     = 1
        transfer_to_cloud_failed = 2
        error_message            = 3
        OTHERS                   = 4.

    LOOP AT lit_etax_textfile INTO lwa_text.
      READ TABLE cht_text ASSIGNING <lfs_text>
        WITH KEY bukrs        = lwa_text-bukrs
                 sap_doc_no   = lwa_text-sap_doc_no
                 gjahr        = lwa_text-gjahr
                 rd_doc_type  = lwa_text-rd_doc_type
                 module_etx   = lwa_text-module_etx BINARY SEARCH.
      IF sy-subrc IS INITIAL.
*       Must always be found since lit_etax_textfile is a subset of cht_text
        <lfs_text>-ftp_fullpath   = lv_ftp_fullpath.
        <lfs_text>-arc_fullpath   = lv_arc_fullpath.
        <lfs_text>-file_path      = ch_wa_mem_inf01-path_and_file_server.
        <lfs_text>-file_status    = gc_ftp_txt.
        <lfs_text>-wa_msg-msgty   = ch_wa_return_tf-msgty.
        <lfs_text>-wa_msg-message = ch_wa_return_tf-message.
        <lfs_text>-trans_no       = ch_wa_mem_inf01-trans_no.
        <lfs_text>-trans_item     = lwa_text-trans_item.
      ENDIF.
    ENDLOOP.  "lit_etax_textfile

    IF ch_wa_return_tf-msgty EQ gc_success.

      ch_wa_mem_inf01-cloud_fullpath = ch_wa_return_tf-target_fullpath.

    ENDIF.

  ENDIF.

ENDFORM.                    "ftp_txt
*&---------------------------------------------------------------------*
*& Form BUILD_TEXT_OUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_ETAX_TEXTFILE
*&      <-- LT_TEXT_OUT
*&---------------------------------------------------------------------*
FORM build_text_out  TABLES   pt_etax_textfile TYPE ZSDSFIS033_TT
                     CHANGING ch_text_out      TYPE truxs_t_text_data.

  DATA : lt_head_out  TYPE TABLE OF ZSDSFIS052,
         lt_h_ref_out TYPE TABLE OF ZSDSFIS054,
         lt_h_vat_out TYPE TABLE OF ZSDSFIS055,
         lt_h_dis_out TYPE TABLE OF ZSDSFIS056,
         lt_dis_out   TYPE TABLE OF ZSDSFIS057,
         lt_item_out  TYPE TABLE OF ZSDSFIS058,
         lt_text      TYPE truxs_t_text_data,
         lt_text_tmp  TYPE truxs_t_text_data.

  DATA : lwa_etax_textfile TYPE ZSDSFIS033,
         lwa_h_ref         TYPE ZSDSFIS035,
         lwa_h_vat         TYPE ZSDSFIS036,
         lwa_dischg_h      TYPE ZSDSFIS037,
         lwa_dischg        TYPE ZSDSFIS037,
         lwa_item          TYPE ZSDSFIS038.

  FIELD-SYMBOLS : <lfs_head_out>  TYPE ZSDSFIS052,
                  <lfs_h_ref_out> TYPE ZSDSFIS054,
                  <lfs_h_vat_out> TYPE ZSDSFIS055,
                  <lfs_h_dis_out> TYPE ZSDSFIS056,
                  <lfs_dis_out>   TYPE ZSDSFIS057,
                  <lfs_item_out>  TYPE ZSDSFIS058.

  REFRESH ch_text_out.

  LOOP AT pt_etax_textfile INTO lwa_etax_textfile.

    REFRESH : lt_head_out[],
              lt_h_ref_out[],
              lt_h_vat_out[],
              lt_item_out[].

*<<<<< ZSDSFIS052 - ETax - Header (Out) >>>>>
    APPEND INITIAL LINE TO lt_head_out ASSIGNING <lfs_head_out>.

    PERFORM movedirect_to_head_out USING lwa_etax_textfile
                                CHANGING <lfs_head_out>.

*    PERFORM clear_head_val          CHANGING <lfs_head_out>.
    PERFORM sap_convert_to_tex_format TABLES lt_head_out
                                    CHANGING lt_text_tmp.

    APPEND LINES OF lt_text_tmp TO lt_text.

*<<<<< ZSDSFIS054 - ETax - Additional Referenced Document >>>>>
    LOOP AT lwa_etax_textfile-doc_h_ref[] INTO lwa_h_ref.

      APPEND INITIAL LINE TO lt_h_ref_out ASSIGNING <lfs_h_ref_out> .

      PERFORM movedirect_to_h_ref_out    USING lwa_h_ref
                                      CHANGING <lfs_h_ref_out>.

    ENDLOOP.

    PERFORM sap_convert_to_tex_format TABLES lt_h_ref_out
                                    CHANGING lt_text_tmp.

    APPEND LINES OF lt_text_tmp TO lt_text.

*<<<<< ZSDSFIS055 - ETax - VAT data >>>>>
    LOOP AT lwa_etax_textfile-doc_h_vat[] INTO lwa_h_vat.

      APPEND INITIAL LINE TO lt_h_vat_out ASSIGNING <lfs_h_vat_out>.

      PERFORM movedirect_to_h_vat_out    USING lwa_h_vat
                                      CHANGING <lfs_h_vat_out>.

    ENDLOOP.

    PERFORM sap_convert_to_tex_format TABLES lt_h_vat_out
                                    CHANGING lt_text_tmp.

    APPEND LINES OF lt_text_tmp TO lt_text.

*<<<<< ZSDSFIS056 - ETax - Discharge data >>>>>
    "(HC) Header Disc & Chagre
    REFRESH lt_h_dis_out[].
    LOOP AT lwa_etax_textfile-doc_dischg[] INTO lwa_dischg_h
      WHERE line_type EQ gc_hc.

      APPEND INITIAL LINE TO lt_h_dis_out ASSIGNING <lfs_h_dis_out>.

      PERFORM movedirect_to_doc_dischg_out    USING lwa_dischg_h
                                           CHANGING <lfs_h_dis_out>.

    ENDLOOP.

    PERFORM sap_convert_to_tex_format TABLES lt_h_dis_out
                                    CHANGING lt_text_tmp.

    APPEND LINES OF lt_text_tmp TO lt_text.

*<<<<< ZSDSFIS058 - ETax - Item (Out) >>>>>
    LOOP AT lwa_etax_textfile-doc_item[] INTO lwa_item.

      APPEND INITIAL LINE TO lt_item_out ASSIGNING <lfs_item_out>.

      PERFORM movedirect_to_doc_item_out      USING lwa_item
                                           CHANGING <lfs_item_out>.

    ENDLOOP.

    PERFORM sap_convert_to_tex_format TABLES lt_item_out
                                    CHANGING lt_text_tmp.

    APPEND LINES OF lt_text_tmp TO lt_text.

*<<<<< ZSDSFIS057 - ETax - Discharge (Out) >>>>>
    "(DC) Item Disc & Chagre per unit
    "(DF) Item Disc & Chagre per item
    REFRESH lt_dis_out[].
    LOOP AT lwa_etax_textfile-doc_dischg[] INTO lwa_dischg
      WHERE line_type EQ gc_dc OR line_type EQ gc_df.

      APPEND INITIAL LINE TO lt_dis_out ASSIGNING <lfs_dis_out>.

      PERFORM movedirect_to_doc_dischg_i_out    USING lwa_dischg
                                                CHANGING <lfs_dis_out>.

    ENDLOOP.

    PERFORM sap_convert_to_tex_format TABLES lt_dis_out
                                    CHANGING lt_text_tmp.

    APPEND LINES OF lt_text_tmp TO lt_text.

  ENDLOOP."it_etax_textfile

  ch_text_out[] = lt_text[].

ENDFORM.                    "build_text_out
*&---------------------------------------------------------------------*
*& Form MOVEDIRECT_TO_HEAD_OUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_ETAX_TEXTFILE
*&      <-- <LFS_HEAD_OUT>
*&---------------------------------------------------------------------*
FORM movedirect_to_head_out  USING    pi_wa_textfile TYPE ZSDSFIS033
                             CHANGING ch_wa_head_out TYPE ZSDSFIS052.

  DATA : lt_partner TYPE ZSDSFIS039_TT.

  DATA : lwa_partner LIKE LINE OF lt_partner.

  lt_partner[] = pi_wa_textfile-doc_partner[].

  ch_wa_head_out-line_type            = gc_hh.
  ch_wa_head_out-document_no          = pi_wa_textfile-document_no.
  ch_wa_head_out-rd_doc_type_desc     = pi_wa_textfile-rd_doc_type_desc.
  ch_wa_head_out-rd_doc_type          = pi_wa_textfile-rd_doc_type.
  ch_wa_head_out-module               = pi_wa_textfile-module_etx.

  ch_wa_head_out-sap_po_no            = pi_wa_textfile-sap_po_no.
  ch_wa_head_out-comp_code            = pi_wa_textfile-bukrs.
  ch_wa_head_out-sap_doc_no           = pi_wa_textfile-sap_doc_no.
  ch_wa_head_out-fiscal_year          = pi_wa_textfile-gjahr.
*  ch_wa_head_out-bus_place            = pi_wa_textfile-bupla.
*>>> BEGIN OF MODIFICATION: <ETAX001> on 10.09.2020 <<<
*  CONCATENATE '0' pi_wa_textfile-bupla INTO ch_wa_head_out-bus_place.
  CONCATENATE '0' pi_wa_textfile-bupla_info INTO ch_wa_head_out-bus_place.
*>>> END OF MODIFICATION: <ETAX001> on 10.09.2020 <<<

  PERFORM get_dat_time_txt USING    pi_wa_textfile-sap_po_date
                                    '000000'
                           CHANGING ch_wa_head_out-sap_po_date.

  PERFORM get_dat_time_txt USING    pi_wa_textfile-sap_posting_date
                                    '000000'
                           CHANGING ch_wa_head_out-sap_posting_date.

  ch_wa_head_out-sap_doc_reason_desc  = pi_wa_textfile-rd_doc_resn_desc. "pi_wa_textfile-sap_doc_resn_desc.
  ch_wa_head_out-rd_doc_reason        = pi_wa_textfile-rd_doc_resn.
  ch_wa_head_out-global_doc_no        = pi_wa_textfile-global_doc_no.

  IF pi_wa_textfile-sap_create_date IS NOT INITIAL.
    PERFORM get_dat_time_txt USING    pi_wa_textfile-sap_create_date
                                      '000000'
                             CHANGING ch_wa_head_out-sap_create_date.
  ENDIF.

  ch_wa_head_out-subject              = pi_wa_textfile-subject.
  ch_wa_head_out-content              = pi_wa_textfile-content.
  ch_wa_head_out-kunnr                = pi_wa_textfile-bukrs. "pi_wa_textfile-kunnr.

  PERFORM get_dat_time_txt USING    pi_wa_textfile-req_dev_date
                                    '000000'
                           CHANGING ch_wa_head_out-req_dev_date.

  ch_wa_head_out-rd_curr_code         = pi_wa_textfile-rd_curr_code.
  ch_wa_head_out-pay_term             = pi_wa_textfile-pay_term.
  ch_wa_head_out-pay_term_desc        = pi_wa_textfile-pay_term_desc.

  PERFORM get_dat_time_txt USING    pi_wa_textfile-pay_due_date
                                    '000000'
                           CHANGING ch_wa_head_out-pay_due_date.

  ch_wa_head_out-ref_doc_amt          = pi_wa_textfile-ref_doc_amt.
  ch_wa_head_out-correct_amt          = pi_wa_textfile-correct_amt.
  ch_wa_head_out-diff_amt             = pi_wa_textfile-diff_amt.
  ch_wa_head_out-total_disc_amt       = pi_wa_textfile-total_disc_amt.
  ch_wa_head_out-total_charge_amt     = pi_wa_textfile-total_charge_amt.
  ch_wa_head_out-net_amt_bf_vat       = pi_wa_textfile-net_amt_bf_vat.
  ch_wa_head_out-vat_amt              = pi_wa_textfile-vat_amt.
  ch_wa_head_out-net_amt_aft_vat      = pi_wa_textfile-net_amt_aft_vat.

  ch_wa_head_out-email                = pi_wa_textfile-email.
  ch_wa_head_out-mobile_phone         = pi_wa_textfile-mobile_phone.
  ch_wa_head_out-replace_flag         = pi_wa_textfile-replace_flag.
  ch_wa_head_out-reverse_flag         = pi_wa_textfile-reverse_flag.
  ch_wa_head_out-rd_flag              = pi_wa_textfile-rd_flag.
  ch_wa_head_out-email_flag           = pi_wa_textfile-email_flag.
  ch_wa_head_out-send_mail_req        = pi_wa_textfile-send_mail_req.
  ch_wa_head_out-print_user           = pi_wa_textfile-print_user. "INS <ETAX001> on 08.10.2020

  LOOP AT lt_partner INTO lwa_partner.

    CASE lwa_partner-rd_partner.
      WHEN 'SELL'. "ผู้ขาย

        ch_wa_head_out-sell_global_id       = lwa_partner-global_id.
        ch_wa_head_out-sell_partner_name    = lwa_partner-partner_name.
        ch_wa_head_out-sell_tax_id          = lwa_partner-tax_id.
        ch_wa_head_out-sell_sch_id          = lwa_partner-partner_sch_id.
        ch_wa_head_out-sell_postal          = lwa_partner-postal.
*        ch_wa_head_out-sell_bldg_name       = lwa_partner-.
        ch_wa_head_out-sell_addr_line1      = lwa_partner-addr_line1.
        ch_wa_head_out-sell_addr_line2      = lwa_partner-addr_line2.
*        ch_wa_head_out-sell_sub_street      = = lwa_partner-.
*        ch_wa_head_out-sell_village         = = lwa_partner-.
*        ch_wa_head_out-sell_vill_no         = = lwa_partner-.
*        ch_wa_head_out-sell_street          = lwa_partner-
        ch_wa_head_out-sell_district        = lwa_partner-district.
        ch_wa_head_out-sell_sub_dist        = lwa_partner-sub_dist.
        ch_wa_head_out-sell_country_id      = lwa_partner-country.
        ch_wa_head_out-sell_country_sch_id  = lwa_partner-country_sch_id.
        ch_wa_head_out-sell_province_code   = lwa_partner-province_code.
        ch_wa_head_out-sell_home_no         = lwa_partner-home_no.

      WHEN 'BUYR'.    "ผู้ซื้อ

        ch_wa_head_out-buyr_kunnr             = lwa_partner-kunnr.
        ch_wa_head_out-buyr_global_id         = lwa_partner-global_id.
        ch_wa_head_out-buyr_partner_name      = lwa_partner-partner_name.
        ch_wa_head_out-buyr_tax_id            = lwa_partner-tax_id.
        ch_wa_head_out-buyr_sch_id            = lwa_partner-partner_sch_id.
        ch_wa_head_out-buyr_contact_name      = lwa_partner-contact_name.
        ch_wa_head_out-buyr_contact_dept      = lwa_partner-contact_dept.
*        ch_wa_head_out-buyr_e_mail            = lwa_partner-.
*        ch_wa_head_out-buyr_phone_no          = lwa_partner-.
        ch_wa_head_out-buyr_postal            = lwa_partner-postal.
*        ch_wa_head_out-buyr_bldg_name         = lwa_partner-.
        ch_wa_head_out-buyr_addr_line1        = lwa_partner-addr_line1.
        ch_wa_head_out-buyr_addr_line2        = lwa_partner-addr_line2.
*        ch_wa_head_out-buyr_sub_street        = lwa_partner-.
*        ch_wa_head_out-buyr_village           = lwa_partner-.
*        ch_wa_head_out-buyr_vill_no           = lwa_partner-.
*        ch_wa_head_out-buyr_street            = lwa_partner-.
        ch_wa_head_out-buyr_district_desc     = lwa_partner-district_desc.
        ch_wa_head_out-buyr_sub_dist_desc     = lwa_partner-sub_dist_desc.
        ch_wa_head_out-buyr_country_id        = lwa_partner-country.
        ch_wa_head_out-buyr_country_sch_id    = lwa_partner-country_sch_id.
        ch_wa_head_out-buyr_province_code     = lwa_partner-province_code.
        ch_wa_head_out-buyr_home_no           = lwa_partner-home_no.

      WHEN 'SHTO'.    "ผู้รับ

        ch_wa_head_out-shto_kunnr           = lwa_partner-kunnr.
        ch_wa_head_out-shto_global_id       = lwa_partner-global_id.
        ch_wa_head_out-shto_partner_name    = lwa_partner-partner_name.
        ch_wa_head_out-shto_contact_name    = lwa_partner-contact_name.
        ch_wa_head_out-shto_contact_dept    = lwa_partner-contact_dept.
*        ch_wa_head_out-shto_e_mail
*        ch_wa_head_out-shto_phone_no
        ch_wa_head_out-shto_postal          = lwa_partner-postal.
*        ch_wa_head_out-shto_bldg_name
        ch_wa_head_out-shto_addr_line1      = lwa_partner-addr_line1.
        ch_wa_head_out-shto_addr_line2      = lwa_partner-addr_line2.
*        ch_wa_head_out-shto_sub_street
*        ch_wa_head_out-shto_village
*        ch_wa_head_out-shto_vill_no
*        ch_wa_head_out-shto_street
        ch_wa_head_out-shto_district_desc   = lwa_partner-district_desc.
        ch_wa_head_out-shto_sub_dist_desc   = lwa_partner-sub_dist_desc.
        ch_wa_head_out-shto_country_id      = lwa_partner-country.
        ch_wa_head_out-shto_country_sch_id  = lwa_partner-country_sch_id.
        ch_wa_head_out-shto_province_code   = lwa_partner-province_code.
        ch_wa_head_out-shto_home_no         = lwa_partner-home_no.

      WHEN 'SHFR'.    "ผู้ส่ง

        ch_wa_head_out-shfr_kunnr           = lwa_partner-kunnr.
        ch_wa_head_out-shfr_global_id       = lwa_partner-global_id.
        ch_wa_head_out-shfr_partner_name    = lwa_partner-partner_name.
        ch_wa_head_out-shfr_contact_name    = lwa_partner-contact_name.
        ch_wa_head_out-shfr_contact_dept    = lwa_partner-contact_dept.
*        ch_wa_head_out-shfr_e_mail
*        ch_wa_head_out-shfr_phone_no
        ch_wa_head_out-shfr_postal          = lwa_partner-postal.
*        ch_wa_head_out-shfr_bldg_name
        ch_wa_head_out-shfr_addr_line1      = lwa_partner-addr_line1.
        ch_wa_head_out-shfr_addr_line2      = lwa_partner-addr_line2.
*        ch_wa_head_out-shfr_sub_street
*        ch_wa_head_out-shfr_village
*        ch_wa_head_out-shfr_vill_no
*        ch_wa_head_out-shfr_street
        ch_wa_head_out-shfr_district_desc   = lwa_partner-district_desc.
        ch_wa_head_out-shfr_sub_dist_desc   = lwa_partner-sub_dist_desc.
        ch_wa_head_out-shfr_country_id      = lwa_partner-country.
        ch_wa_head_out-shfr_country_sch_id  = lwa_partner-country_sch_id.
        ch_wa_head_out-shfr_province_code   = lwa_partner-province_code.
        ch_wa_head_out-shfr_home_no         = lwa_partner-home_no.

      WHEN 'ISUE'.    "ผู้ออกเอกสาร

        ch_wa_head_out-isue_kunnr           = lwa_partner-kunnr.
        ch_wa_head_out-isue_global_id       = lwa_partner-global_id.
        ch_wa_head_out-isue_partner_name    = lwa_partner-partner_name.
        ch_wa_head_out-isue_tax_id          = lwa_partner-tax_id.
        ch_wa_head_out-isue_sch_id          = lwa_partner-partner_sch_id.
        ch_wa_head_out-isue_contact_name    = lwa_partner-contact_name.
        ch_wa_head_out-isue_contact_dept    = lwa_partner-contact_dept.
*        ch_wa_head_out-isue_e_mail
*        ch_wa_head_out-isue_phone_no
        ch_wa_head_out-isue_postal          = lwa_partner-postal.
*        ch_wa_head_out-isue_bldg_name
        ch_wa_head_out-isue_addr_line1      = lwa_partner-addr_line1.
        ch_wa_head_out-isue_addr_line2      = lwa_partner-addr_line2.
*        ch_wa_head_out-isue_sub_street
*        ch_wa_head_out-isue_village
*        ch_wa_head_out-isue_vill_no
*        ch_wa_head_out-isue_street
        ch_wa_head_out-isue_district_desc   = lwa_partner-district_desc.
        ch_wa_head_out-isue_sub_dist_desc   = lwa_partner-sub_dist_desc.
        ch_wa_head_out-isue_country_id      = lwa_partner-country.
        ch_wa_head_out-isue_country_sch_id  = lwa_partner-country_sch_id.
        ch_wa_head_out-isue_province_code   = lwa_partner-province_code.
        ch_wa_head_out-isue_home_no         = lwa_partner-home_no.

      WHEN 'INVC'.    "ผู้รับเอกสาร

        ch_wa_head_out-invc_kunnr           = lwa_partner-kunnr.
        ch_wa_head_out-invc_global_id       = lwa_partner-global_id.
        ch_wa_head_out-invc_partner_name    = lwa_partner-partner_name.
        ch_wa_head_out-invc_tax_id          = lwa_partner-tax_id.
        ch_wa_head_out-invc_sch_id          = lwa_partner-partner_sch_id.
        ch_wa_head_out-invc_contact_name    = lwa_partner-contact_name.
        ch_wa_head_out-invc_contact_dept    = lwa_partner-contact_dept.
*        ch_wa_head_out-invc_e_mail
*        ch_wa_head_out-invc_phone_no
        ch_wa_head_out-invc_postal          = lwa_partner-postal.
*        ch_wa_head_out-invc_bldg_name
        ch_wa_head_out-invc_addr_line1      = lwa_partner-addr_line1.
        ch_wa_head_out-invc_addr_line2      = lwa_partner-addr_line2.
*        ch_wa_head_out-invc_sub_street
*        ch_wa_head_out-invc_village
*        ch_wa_head_out-invc_vill_no
*        ch_wa_head_out-invc_street
        ch_wa_head_out-invc_district_desc   = lwa_partner-district_desc.
        ch_wa_head_out-invc_sub_dist_desc   = lwa_partner-sub_dist_desc.
        ch_wa_head_out-invc_country_id      = lwa_partner-country.
        ch_wa_head_out-invc_country_sch_id  = lwa_partner-country_sch_id.
        ch_wa_head_out-invc_province_code   = lwa_partner-province_code.
        ch_wa_head_out-invc_home_no         = lwa_partner-home_no.

      WHEN 'PAYR'.    "ผู้ชำระเงิน

        ch_wa_head_out-payr_kunnr           = lwa_partner-kunnr.
        ch_wa_head_out-payr_global_id       = lwa_partner-global_id.
        ch_wa_head_out-payr_partner_name    = lwa_partner-partner_name.
        ch_wa_head_out-payr_tax_id          = lwa_partner-tax_id.
        ch_wa_head_out-payr_sch_id          = lwa_partner-partner_sch_id.
        ch_wa_head_out-payr_contact_name    = lwa_partner-contact_name.
        ch_wa_head_out-payr_contact_dept    = lwa_partner-contact_dept.
*        ch_wa_head_out-payr_e_mail
*        ch_wa_head_out-payr_phone_no
        ch_wa_head_out-payr_postal          = lwa_partner-postal.
*        ch_wa_head_out-payr_bldg_name
        ch_wa_head_out-payr_addr_line1      = lwa_partner-addr_line1.
        ch_wa_head_out-payr_addr_line2      = lwa_partner-addr_line2.
*        ch_wa_head_out-payr_sub_street
*        ch_wa_head_out-payr_village
*        ch_wa_head_out-payr_vill_no
*        ch_wa_head_out-payr_street
        ch_wa_head_out-payr_district_desc   = lwa_partner-district_desc.
        ch_wa_head_out-payr_sub_dist_desc   = lwa_partner-sub_dist_desc.
        ch_wa_head_out-payr_country_id      = lwa_partner-country.
        ch_wa_head_out-payr_country_sch_id  = lwa_partner-country_sch_id.
        ch_wa_head_out-payr_province_code   = lwa_partner-province_code.
        ch_wa_head_out-payr_home_no         = lwa_partner-home_no.

      WHEN 'PAYE'.    "ผู้รับชำระเงิน

        ch_wa_head_out-paye_kunnr           = lwa_partner-kunnr.
        ch_wa_head_out-paye_global_id       = lwa_partner-global_id.
        ch_wa_head_out-paye_partner_name    = lwa_partner-partner_name.
        ch_wa_head_out-paye_tax_id          = lwa_partner-tax_id.
        ch_wa_head_out-paye_sch_id          = lwa_partner-partner_sch_id.
        ch_wa_head_out-paye_contact_name    = lwa_partner-contact_name.
        ch_wa_head_out-paye_contact_dept    = lwa_partner-contact_dept.
*        ch_wa_head_out-paye_e_mail
*        ch_wa_head_out-paye_phone_no
        ch_wa_head_out-paye_postal          = lwa_partner-postal.
*        ch_wa_head_out-paye_bldg_name
        ch_wa_head_out-paye_addr_line1      = lwa_partner-addr_line1.
        ch_wa_head_out-paye_addr_line2      = lwa_partner-addr_line2.
*        ch_wa_head_out-paye_sub_street
*        ch_wa_head_out-paye_village
*        ch_wa_head_out-paye_vill_no
*        ch_wa_head_out-paye_street
        ch_wa_head_out-paye_district_desc   = lwa_partner-district_desc.
        ch_wa_head_out-paye_sub_dist_desc   = lwa_partner-sub_dist_desc.
        ch_wa_head_out-paye_country_id      = lwa_partner-country.
        ch_wa_head_out-paye_country_sch_id  = lwa_partner-country_sch_id.
        ch_wa_head_out-paye_province_code   = lwa_partner-province_code.
        ch_wa_head_out-paye_home_no         = lwa_partner-home_no.

      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    "movedirect_to_head_out
*&---------------------------------------------------------------------*
*& Form GET_DAT_TIME_TXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PI_WA_TEXTFILE_SAP_POSTING_DAT
*&      --> P_
*&      <-- CH_WA_HEAD_OUT_SAP_POSTING_DAT
*&---------------------------------------------------------------------*
FORM get_dat_time_txt  USING    p_dat   TYPE vbrk-erdat
                                p_time  TYPE vbrk-erzet
                       CHANGING ch_dat_time.

  DATA: lv_dat_txt  TYPE char10,
        lv_time_txt TYPE char10.

  CONCATENATE p_dat+0(4) '-'
              p_dat+4(2) '-'
              p_dat+6(2)
         INTO lv_dat_txt.

  WRITE p_time TO lv_time_txt USING EDIT MASK '__:__:__'.

  CONCATENATE lv_dat_txt
              'T'
              lv_time_txt
         INTO ch_dat_time.

ENDFORM.                    "get_dat_time_txt
*&---------------------------------------------------------------------*
*& Form UPLOAD_ZIP_TO_AL11
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_PATH_FILENAME_ZIP
*&      <-- LO_ZIP
*&      <-- LWA_MSG
*&      <-- LV_X_ZIP_OK
*&---------------------------------------------------------------------*
FORM upload_zip_to_al11  USING    pv_path_filezip
                         CHANGING ch_lo_zip  TYPE REF TO cl_abap_zip
                                  ch_wa_msg  TYPE ZSDSFIS034
                                  ch_x_done  TYPE flag.

  DATA: lt_zipbindata TYPE TABLE OF tbl1024,
        ls_zipbindata TYPE tbl1024.
  DATA: lv_xstr_zip TYPE xstring,
        lv_len      TYPE i.
  DATA: lo_exception     TYPE REF TO cx_root,
        lv_message       TYPE string,
        lv_batch_message TYPE string,
        lv_x_fopen       TYPE flag,
        lv_x_done        TYPE flag,
        lv_path          TYPE string,
        lv_filename      TYPE string.

  CHECK pv_path_filezip IS NOT INITIAL.  "ถ้า เกิด ไฟล์ .ZIP

  IF ch_lo_zip IS NOT INITIAL.
    "---นำ object lo_zip ==> เก็บลง Xstring
    lv_xstr_zip = ch_lo_zip->save( ).

    "---แปล Xstring ให้เป็น BIN DATA
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_xstr_zip
*       APPEND_TO_TABLE = ' '
      IMPORTING
        output_length = lv_len
      TABLES
        binary_tab    = lt_zipbindata.

    "---Transfer BIN DATA to AL11
*<<<DEL: TIWA 21-Dec-2019 (to fix exception CX_SY_FILE_OPEN_MODE)
*    OPEN DATASET pv_path_filezip FOR OUTPUT IN BINARY MODE.
*    LOOP AT lt_zipbindata INTO ls_zipbindata.
*      TRANSFER ls_zipbindata TO pv_path_filezip.
*    ENDLOOP.
*    CLOSE DATASET pv_path_filezip.
*    FREE ch_lo_zip.
*>>>DEL: TIWA 21-Dec-2019

*<<<INS: TIWA 21-Dec-2019 (to fix exception CX_SY_FILE_OPEN_MODE)
    PERFORM message_for_batch_d USING 'Start: Download ZIP to AL11'.

    PERFORM get_filename_from_path USING pv_path_filezip
                                CHANGING lv_path
                                         lv_filename.

    TRY.
        OPEN DATASET pv_path_filezip FOR OUTPUT IN BINARY MODE
             MESSAGE lv_message.
        IF sy-subrc IS INITIAL.
          lv_x_fopen = abap_true.

          LOOP AT lt_zipbindata INTO ls_zipbindata.
            TRANSFER ls_zipbindata TO pv_path_filezip.
          ENDLOOP.

          lv_x_done = abap_true.

        ENDIF.
      CATCH cx_root INTO lo_exception.
*     Gets error message
        CALL METHOD lo_exception->if_message~get_text
          RECEIVING
            result = lv_message.
    ENDTRY.

    IF lv_x_fopen EQ abap_true.
      CLOSE DATASET pv_path_filezip.
    ENDIF.

    IF lv_x_done EQ abap_false.

      IF lv_message IS INITIAL.
        lv_message = 'Unable to write ZIP file: '.
      ENDIF.

      CONCATENATE lv_message pv_path_filezip
             INTO lv_message SEPARATED BY space.

      ch_wa_msg-msgty   = gc_error.
      ch_wa_msg-message = lv_message.
      lv_batch_message  = lv_message.
    ELSE.
      lv_batch_message  = lv_filename.
    ENDIF.

    ch_x_done = lv_x_done.

    FREE ch_lo_zip.

    PERFORM message_for_batch_d USING lv_batch_message.
    PERFORM message_for_batch_d USING 'End: Download ZIP to AL11'.
*>>>INS: TIWA 21-Dec-2019

  ENDIF.

ENDFORM.                    " UPLOAD_ZIP_TO_AL11
*&---------------------------------------------------------------------*
*& Form GET_FILENAME_FROM_PATH
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PV_PATH_FILEZIP
*&      <-- LV_PATH
*&      <-- LV_FILENAME
*&---------------------------------------------------------------------*
FORM get_filename_from_path  USING    pi_v_path_filename
                         CHANGING ch_v_path
                                  ch_v_filename.

  DATA: lv_path_filename TYPE string,
        lv_path          TYPE string,
        lv_filename      TYPE string.

  CLEAR: ch_v_path,
         ch_v_filename.

  lv_path_filename = pi_v_path_filename.

  CALL FUNCTION 'ZETX_GET_FILENAME_FROM_PATH'
    EXPORTING
      im_path_filename = lv_path_filename
    IMPORTING
      ex_path          = lv_path
      ex_filename      = lv_filename.

  ch_v_path     = lv_path.
  ch_v_filename = lv_filename.

ENDFORM.                    "get_filename_from_path
*&---------------------------------------------------------------------*
*& Form SAP_CONVERT_TO_TEX_FORMAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_HEAD_OUT
*&      <-- LT_TEXT_TMP
*&---------------------------------------------------------------------*
FORM sap_convert_to_tex_format  TABLES pt_data    TYPE table
                              CHANGING ch_it_text TYPE truxs_t_text_data.

  REFRESH ch_it_text.

  CALL FUNCTION 'SAP_CONVERT_TO_TEX_FORMAT'
    EXPORTING
      i_field_seperator    = '|'
*     I_LINE_HEADER        =
*     I_FILENAME           =
*     I_APPL_KEEP          = ' '
    TABLES
      i_tab_sap_data       = pt_data
    CHANGING
      i_tab_converted_data = ch_it_text
*       EXCEPTIONS
*     CONVERSION_FAILED    = 1
*     OTHERS               = 2
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    "sap_convert_to_tex_format
*&---------------------------------------------------------------------*
*& Form MOVEDIRECT_TO_H_REF_OUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_H_REF
*&      <-- <LFS_H_REF_OUT>
*&---------------------------------------------------------------------*
FORM movedirect_to_h_ref_out  USING    pi_wa_h_ref     TYPE ZSDSFIS035
                              CHANGING ch_wa_h_ref_out TYPE ZSDSFIS054.

  ch_wa_h_ref_out-line_type         = gc_hr.
  ch_wa_h_ref_out-ref_doc_no        = pi_wa_h_ref-ref_doc_no.

  PERFORM get_dat_time_txt USING   pi_wa_h_ref-ref_sap_post_date
                                  '000000'
                         CHANGING ch_wa_h_ref_out-ref_sap_post_date.

  ch_wa_h_ref_out-ref_rd_doc_type   = pi_wa_h_ref-ref_rd_doc_type.

ENDFORM.                    "movedirect_to_h_ref_out
*&---------------------------------------------------------------------*
*& Form MOVEDIRECT_TO_H_VAT_OUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_H_VAT
*&      <-- <LFS_H_VAT_OUT>
*&---------------------------------------------------------------------*
FORM movedirect_to_h_vat_out  USING    pi_wa_h_vat     TYPE ZSDSFIS036
                              CHANGING ch_wa_h_vat_out TYPE ZSDSFIS055.

  DATA lv_vat_rate TYPE i.

  ch_wa_h_vat_out-line_type       = gc_hv.
  ch_wa_h_vat_out-rd_vat_type     = pi_wa_h_vat-rd_vat_type.

  lv_vat_rate                     = pi_wa_h_vat-vat_rate.
  ch_wa_h_vat_out-vat_rate        = lv_vat_rate.
  CONDENSE ch_wa_h_vat_out-vat_rate.

  ch_wa_h_vat_out-net_amt_bf_vat  = pi_wa_h_vat-net_amt_bf_vat.
  ch_wa_h_vat_out-vat_amt         = pi_wa_h_vat-vat_amt.

ENDFORM.                    "movedirect_to_h_vat_out
*&---------------------------------------------------------------------*
*& Form MOVEDIRECT_TO_DOC_DISCHG_OUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_DISCHG
*&      <-- <LFS_DIS_OUT>
*&---------------------------------------------------------------------*
FORM movedirect_to_doc_dischg_out  USING    pi_wa_dischg    TYPE ZSDSFIS037
                                   CHANGING ch_wa_h_dis_out TYPE ZSDSFIS056.

  ch_wa_h_dis_out-line_type       = pi_wa_dischg-line_type.
  ch_wa_h_dis_out-rd_charge_flag  = pi_wa_dischg-rd_charge_flag.
  ch_wa_h_dis_out-charge_amt      = pi_wa_dischg-charge_amt.
  ch_wa_h_dis_out-rd_charge_code  = pi_wa_dischg-rd_charge_code.
  ch_wa_h_dis_out-charge_reason   = pi_wa_dischg-charge_resn.

ENDFORM.                    "movedirect_to_doc_dischg_out
*&---------------------------------------------------------------------*
*& Form MOVEDIRECT_TO_DOC_DISCHG_OUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_DISCHG
*&      <-- <LFS_DIS_OUT>
*&---------------------------------------------------------------------*
FORM movedirect_to_doc_dischg_i_out  USING    pi_wa_dischg    TYPE ZSDSFIS037
                                     CHANGING ch_wa_h_dis_out TYPE ZSDSFIS057.


  ch_wa_h_dis_out-line_type       = pi_wa_dischg-line_type.
  ch_wa_h_dis_out-item_no         = pi_wa_dischg-item_no.
  ch_wa_h_dis_out-rd_charge_flag  = pi_wa_dischg-rd_charge_flag.
  ch_wa_h_dis_out-charge_amt      = pi_wa_dischg-charge_amt.
  ch_wa_h_dis_out-rd_charge_code  = pi_wa_dischg-rd_charge_code.
  ch_wa_h_dis_out-charge_reason   = pi_wa_dischg-charge_resn.

ENDFORM.                    "movedirect_to_doc_dischg_i_out
*&---------------------------------------------------------------------*
*& Form MOVEDIRECT_TO_DOC_ITEM_OUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_ITEM
*&      <-- <LFS_ITEM_OUT>
*&---------------------------------------------------------------------*
FORM movedirect_to_doc_item_out  USING    pi_wa_item     TYPE ZSDSFIS038
                                 CHANGING ch_wa_item_out TYPE ZSDSFIS058.

  DATA lv_vat_rate TYPE i.

  ch_wa_item_out-line_type      = gc_dd.
  ch_wa_item_out-item_no        = pi_wa_item-item_no.
  ch_wa_item_out-item_code      = pi_wa_item-item_code.
  ch_wa_item_out-item_name      = pi_wa_item-item_name.
  ch_wa_item_out-item_desc      = pi_wa_item-item_desc.
  ch_wa_item_out-batch          = pi_wa_item-batch.

  PERFORM get_dat_time_txt USING    pi_wa_item-exp_date
                                    '000000'
                           CHANGING ch_wa_item_out-exp_date.

  ch_wa_item_out-mat_grp        = pi_wa_item-mat_grp.
  ch_wa_item_out-mat_grp_name   = pi_wa_item-mat_grp_name.
  ch_wa_item_out-unit_price     = pi_wa_item-unit_price.
  ch_wa_item_out-qty            = pi_wa_item-qty.
  ch_wa_item_out-rd_vat_type    = pi_wa_item-rd_vat_type.

  lv_vat_rate                   = pi_wa_item-vat_rate.
  ch_wa_item_out-vat_rate       = lv_vat_rate.
  CONDENSE ch_wa_item_out-vat_rate.

  ch_wa_item_out-net_amt_bf_vat     = pi_wa_item-net_amt_bf_vat.
  ch_wa_item_out-vat_amt            = pi_wa_item-vat_amt.
  ch_wa_item_out-it_vat_amt         = pi_wa_item-vat_amt.
  ch_wa_item_out-it_net_amt_bf_vat  = pi_wa_item-net_amt_bf_vat.
  ch_wa_item_out-it_net_amt_aft_vat = pi_wa_item-net_amt_aft_vat.

  ch_wa_item_out-sap_po_no = pi_wa_item-sap_po_no.

  DATA: lv_po_item LIKE pi_wa_item-sap_po_item .
  lv_po_item =  pi_wa_item-sap_po_item .
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_po_item
    IMPORTING
      output = lv_po_item.

  ch_wa_item_out-sap_po_item = lv_po_item+1(5).

ENDFORM.                    "movedirect_to_doc_item_out
*&---------------------------------------------------------------------*
*& Form DOWNLOAD_TO_SERVER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IM_PATH_SERVER
*&      --> IM_EXE_TYPE
*&      --> LT_TEXT_OUT
*&      <-- LV_RETURN_SERVER
*&      <-- LV_FILENAME
*&      <-- EX_TRANS_NO
*&      <-- EX_RETURN_TF
*&---------------------------------------------------------------------*
FORM download_to_server  USING    pi_server
                                  pi_exe_type TYPE ZSDSFIS049-exe_type
                                  pt_text     TYPE truxs_t_text_data
                         CHANGING ch_sap_fullpath
                                  ch_filename
                                  ch_trans_no
                                  ch_wa_return TYPE ZSDSFIS051.

  DATA : lv_fullpath    TYPE string,
         lv_filename    TYPE text255,
         lwa_text(4096) TYPE c,
         lv_trans_no    TYPE zsdsde_trans_no.

  DATA : lref_cx_root   TYPE REF TO cx_root.

  CALL FUNCTION 'ZETX_GEN_FILENAME'
    EXPORTING
      im_module    = 'SD'
      im_extension = 'txt'
    IMPORTING
      ex_filename  = lv_filename
      ex_trans_no  = lv_trans_no.

  CONCATENATE pi_server
              lv_filename
         INTO lv_fullpath SEPARATED BY '/'.

  REPLACE ALL OCCURRENCES OF '//' IN lv_fullpath WITH '/'.

  ch_sap_fullpath   = lv_fullpath.
  ch_filename       = lv_filename.
  ch_trans_no       = lv_trans_no.

  TRY.
      OPEN DATASET lv_fullpath FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
      IF sy-subrc = 0.

        LOOP AT pt_text INTO lwa_text.
          TRANSFER lwa_text TO lv_fullpath.
        ENDLOOP.

        CLOSE DATASET lv_fullpath.

      ELSE.

        ch_wa_return-msgty   = 'E'.
        ch_wa_return-message = 'Could not write file(AL11) &1'.
        REPLACE '&1' IN ch_wa_return-message WITH lv_fullpath.

      ENDIF.
    CATCH cx_root INTO lref_cx_root.

      ch_wa_return-msgty   = 'E'.
      ch_wa_return-message = lref_cx_root->get_text( ).

  ENDTRY.

ENDFORM.                    "download_to_server
*&---------------------------------------------------------------------*
*& Form ARCHIVE_FILE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_RETURN_SERVER
*&---------------------------------------------------------------------*
FORM archive_file  USING pi_source.

  DATA: lv_source TYPE text255,
        lv_target TYPE text255.

  PERFORM get_file_path.

  lv_source = pi_source.

  lv_target = gs_file_path-sap_dir_arch.

  PERFORM move_file USING lv_source
                          lv_target.

ENDFORM.                    "archive_file
*&---------------------------------------------------------------------*
*& Form MOVE_FILE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_SOURCE
*&      --> LV_TARGET
*&---------------------------------------------------------------------*
FORM move_file  USING    pi_source
                         pi_target.

  DATA: lv_subrc   TYPE sy-subrc,
        lv_command TYPE string,
        lwa_return TYPE bapiret2.

  CALL FUNCTION 'ZETX_MOVE_FILE'
    EXPORTING
      im_source  = pi_source
      im_target  = pi_target
    IMPORTING
      ex_subrc   = lv_subrc
      ex_command = lv_command
      ex_return  = lwa_return
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

ENDFORM.                    "move_file
*&---------------------------------------------------------------------*
*& Form RUN_OS_COMMAND
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_COMMAND
*&      <-- LV_SUBRC
*&      <-- LV_MSG
*&---------------------------------------------------------------------*
FORM run_os_command  USING    pi_command
                     CHANGING ch_subrc
                              ch_msg.

  CALL FUNCTION 'ZETX_RUN_OS_COMMAND'
    EXPORTING
      im_command = pi_command
    IMPORTING
      ex_subrc   = ch_subrc
      ex_msg     = ch_msg
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

ENDFORM.                    "run_os_command
*&---------------------------------------------------------------------*
*& Form CALL_WS_SIGN_DOCUMENT_ZIP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_MEM_INF01
*&      --> LV_FILENAME_ZIP
*&      --> LV_FTPPATH_FILENAME_ZIP
*&      <-- LT_TEXT
*&      <-- LWA_RETURN_TF
*&---------------------------------------------------------------------*
FORM call_ws_sign_document_zip
        USING pi_wa_mem_inf01      TYPE ZSDSFIS050
              pi_filename_zip
              pi_ftppath_filename_zip
     CHANGING cht_etax_textfile    TYPE ZSDSFIS033_TT
              ch_wa_text_tf        TYPE ZSDSFIS051.

  DATA: lit_request  TYPE ZSDSFIS059_TT,
        lit_response TYPE ZSDSFIS048_TT,
        lit_textfile TYPE ZSDSFIS033_TT.

  DATA: lwa_return_tf TYPE ZSDSFIS051,
        lwa_bapiret2  TYPE ZSDSFIS060.

  lit_textfile[] = cht_etax_textfile[].

  DELETE lit_textfile
   WHERE file_status  NE gc_ftp_txt
      OR wa_msg-msgty NE 'S'.

  IF lit_textfile[] IS NOT INITIAL.

    PERFORM build_request_sign_zip USING pi_wa_mem_inf01
                                         ch_wa_text_tf
                                         pi_filename_zip
                                         pi_ftppath_filename_zip
                                CHANGING lit_textfile
                                         lit_request.


    PERFORM ws_sign_document USING lit_request
                          CHANGING lit_textfile
                                   lit_response
                                   lwa_bapiret2
                                   lwa_return_tf.

    PERFORM update_text_response USING lit_textfile
                              CHANGING cht_etax_textfile.

  ENDIF.

  ch_wa_text_tf = lwa_return_tf.

ENDFORM.                    "call_ws_sign_document_zip
*&---------------------------------------------------------------------*
*& Form BUILD_REQUEST_SIGN_ZIP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PI_WA_MEM_INF01
*&      --> CH_WA_TEXT_TF
*&      --> PI_FILENAME_ZIP
*&      --> PI_FTPPATH_FILENAME_ZIP
*&      <-- LIT_TEXTFILE
*&---------------------------------------------------------------------*
FORM build_request_sign_zip
        USING pi_wa_mem_inf01   TYPE ZSDSFIS050
              pi_wa_text_tf     TYPE ZSDSFIS051
              pi_filename_zip
              pi_ftppath_filename_zip
     CHANGING cht_etax_textfile TYPE ZSDSFIS033_TT
              pt_request        TYPE ZSDSFIS059_TT.


  DATA: lwa_request LIKE LINE OF pt_request,
        "lwa_head    TYPE zsetax_log_key,
        lwa_text    LIKE LINE OF cht_etax_textfile,
        lwa_pdf     TYPE ZSDSFIS025.

*---.txt
  READ TABLE cht_etax_textfile INTO lwa_text INDEX 1.
  IF sy-subrc IS INITIAL.
    CLEAR lwa_request.
    lwa_request-company           = lwa_text-bukrs.
    lwa_request-document_no       = ''.
    lwa_request-document_type     = ''.
    lwa_request-email_address     = ''.
    lwa_request-filetype          = 'XML'.
    lwa_request-fisical_year      = lwa_text-gjahr.
    lwa_request-fullpath          = lwa_text-ftp_fullpath.
    lwa_request-module            = lwa_text-module_etx.
    lwa_request-ref_document_type = ''.
    lwa_request-sap_document_no   = ''.
    lwa_request-storage_type      = '2'.
    lwa_request-trans_no          = lwa_text-trans_no.
    lwa_request-trans_item        = gc_trans_item1.
    APPEND lwa_request TO pt_request.

*---.ZIP
    IF pi_ftppath_filename_zip IS NOT INITIAL.
      CLEAR lwa_request.
      lwa_request-company           = lwa_text-bukrs.
      lwa_request-document_no       = ''.
      lwa_request-document_type     = ''.
      lwa_request-email_address     = ''.
      lwa_request-filetype          = 'ZIP'.
      lwa_request-fisical_year      = lwa_text-gjahr.
      lwa_request-fullpath          = pi_ftppath_filename_zip.
      lwa_request-module            = lwa_text-module_etx.
      lwa_request-ref_document_type = ''.
      lwa_request-sap_document_no   = ''.
      lwa_request-storage_type      = '2'.
      lwa_request-trans_no          = lwa_text-trans_no.
      lwa_request-trans_item        = gc_trans_item1.
      APPEND lwa_request TO pt_request.
    ENDIF.
  ENDIF.

*---.pdf
  LOOP AT cht_etax_textfile INTO lwa_text
    WHERE x_pdf_completed = abap_true.

    LOOP AT lwa_text-etax_pdf INTO lwa_pdf.
      CLEAR lwa_request.
      lwa_request-company           = lwa_text-bukrs.         "lwa_text-comp.
      lwa_request-document_no       = lwa_text-document_no.   "lwa_text-doc_no.
      lwa_request-document_type     = lwa_text-rd_doc_type.   "lwa_text-doc_type.
      lwa_request-email_address     = lwa_text-email.
      lwa_request-filetype          = 'PDF'.
      lwa_request-fisical_year      = lwa_text-gjahr.         "lwa_text-fyear.
      lwa_request-fullpath          = lwa_pdf-ftp_fullpath.
      lwa_request-module            = lwa_text-module_etx.
      lwa_request-ref_document_type = lwa_text-sap_doc_type.
      lwa_request-sap_document_no   = lwa_text-sap_doc_no.
      lwa_request-storage_type      = '2'.
      lwa_request-trans_no          = lwa_pdf-trans_no.       "lwa_text-ref_type.
      lwa_request-trans_item        = lwa_pdf-trans_item.
      APPEND lwa_request TO pt_request.
    ENDLOOP.  "lwa_text-it_pdf

  ENDLOOP.  "cht_etax_textfile

ENDFORM.                    "build_request_sign_zip
*&---------------------------------------------------------------------*
*& Form ARCHIVE_PDF_FILES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_TEXT
*&---------------------------------------------------------------------*
FORM archive_pdf_files  USING pt_text TYPE ZSDSFIS033_TT.

  TYPES: BEGIN OF lty_file,
           name TYPE string,
         END OF lty_file.
*
  DATA: lit_file TYPE TABLE OF lty_file.
  DATA: lwa_text LIKE LINE OF pt_text,
        lwa_pdf  LIKE LINE OF lwa_text-etax_pdf,
        lwa_file LIKE LINE OF lit_file.

  DEFINE append_file.
    clear lwa_file.
    lwa_file-name = &1.
    perform set_mv_filename changing lwa_file-name.
    append lwa_file to lit_file.
  END-OF-DEFINITION.

* Prepare file name
  LOOP AT pt_text INTO lwa_text
    WHERE wa_msg-msgty EQ gc_success "NE gc_success
      AND x_pdf        EQ abap_true.

    append_file: lwa_text-file_path.

    LOOP AT lwa_text-etax_pdf INTO lwa_pdf.
      append_file: lwa_pdf-file_path.
    ENDLOOP.

  ENDLOOP.  "pt_text

  SORT lit_file BY name.
  DELETE ADJACENT DUPLICATES FROM lit_file COMPARING name.

* start moving file from OUT to ARCHIVE
  LOOP AT lit_file INTO lwa_file.

    PERFORM archive_file USING lwa_file-name.

  ENDLOOP.  "lit_file

ENDFORM.                    "archive_pdf_files
*&---------------------------------------------------------------------*
*& Form set_mv_filename
*&---------------------------------------------------------------------*
*  Input:   EZTAX_20200114_152646_100920200000830003.pdf
*  Output:  EZTAX_*_100920200000830003.pdf
*&---------------------------------------------------------------------*
FORM set_mv_filename CHANGING ch_name.

  CALL FUNCTION 'ZETX_MV_SET_FILENAME'
    EXPORTING
      im_filename = ch_name
    IMPORTING
      ex_filename = ch_name.

ENDFORM.                    "set_mv_filename
*&---------------------------------------------------------------------*
*& Form WS_SIGN_DOCUMENT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LIT_REQUEST
*&      <-- LIT_TEXTFILE
*&      <-- LIT_RESPONSE
*&      <-- LWA_BAPIRET2
*&      <-- LWA_RETURN_TF
*&---------------------------------------------------------------------*
FORM ws_sign_document
        USING pt_request      TYPE ZSDSFIS059_TT
     CHANGING cht_textfile    TYPE ZSDSFIS033_TT
              cht_response    TYPE ZSDSFIS048_TT
              ch_wa_bapiret2  TYPE ZSDSFIS060
              ch_wa_return_tf TYPE ZSDSFIS051.

  DATA: lo_service     TYPE REF TO ZCL_SDSFI_CO_IETAX_SERVICE,
        lref_sys_fault TYPE REF TO cx_ai_system_fault,
        lref_app_fault TYPE REF TO cx_ai_application_fault,
        lref_cx_root   TYPE REF TO cx_root.

  DATA: lit_response   TYPE ZSDSFIS048_TT.

  DATA: ls_input    TYPE ZSDSFIS065,
        ls_output   TYPE ZSDSFIS067,
        ls_response LIKE LINE OF cht_response,
        ls_bapiret2 TYPE ZSDSFIS060.

  DATA: lwa_msg    TYPE ZSDSFIS034.

  ls_input-requests-sap_request_sign[] = pt_request[].

  TRY.
      CREATE OBJECT lo_service
        EXPORTING
          logical_port_name = gc_logical_port.
    CATCH cx_root INTO lref_cx_root.
      lwa_msg-msgty   = gc_error.
      lwa_msg-message = lref_cx_root->get_text( ).
  ENDTRY.

  IF lwa_msg-msgty NE gc_error.
    TRY.
        CALL METHOD lo_service->sign_document
          EXPORTING
            input  = ls_input
          IMPORTING
            output = ls_output.

      CATCH cx_ai_system_fault INTO lref_sys_fault.
        lwa_msg-msgty   = gc_error.
        lwa_msg-message = lref_sys_fault->get_text( ).

      CATCH cx_ai_application_fault INTO lref_app_fault.
        lwa_msg-msgty   = gc_error.
        lwa_msg-message = lref_app_fault->get_text( ).

    ENDTRY.
  ENDIF.

  lit_response[] = ls_output-sign_document_result-result-sap_response[].
  ls_bapiret2    = ls_output-sign_document_result-bapiret2.

  IF lwa_msg-msgty EQ gc_error.
    ch_wa_return_tf-msgty   = lwa_msg-msgty.
    ch_wa_return_tf-message = lwa_msg-message.
  ELSEIF ls_bapiret2-type EQ gc_error.
    ch_wa_return_tf-msgty   = ls_bapiret2-type.
    ch_wa_return_tf-message = ls_bapiret2-message.
  ENDIF.

  IF ls_bapiret2   IS INITIAL AND
     lwa_msg-msgty EQ gc_error.

    ls_bapiret2-type    = lwa_msg-msgty.
    ls_bapiret2-message = lwa_msg-message.

  ENDIF.

  PERFORM rebuild_response USING pt_request
                                 ls_bapiret2
                        CHANGING lit_response.

  PERFORM response_to_textfile USING lit_response
                            CHANGING cht_textfile.

  cht_response[] = lit_response[].
  ch_wa_bapiret2 = ls_bapiret2.

ENDFORM.                    "ws_sign_document
*&---------------------------------------------------------------------*
*& Form REBUILD_RESPONSE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PT_REQUEST
*&      --> LS_BAPIRET2
*&      <-- LIT_RESPONSE
*&---------------------------------------------------------------------*
FORM rebuild_response  USING pt_request     TYPE ZSDSFIS059_TT
                             pi_wa_bapiret2 TYPE ZSDSFIS060
                    CHANGING cht_response   TYPE ZSDSFIS048_TT.

  DATA: lit_response  TYPE ZSDSFIS048_TT.
  DATA: lwa_request  LIKE LINE OF pt_request,
        lwa_response LIKE LINE OF cht_response.

  SORT cht_response BY trans_no
                       trans_item.

  LOOP AT pt_request INTO lwa_request.
    CLEAR lwa_response.

    READ TABLE cht_response INTO lwa_response
      WITH KEY trans_no   = lwa_request-trans_no
               trans_item = lwa_request-trans_item BINARY SEARCH.
    IF sy-subrc IS INITIAL.
    ELSE.
      CLEAR lwa_response.
      MOVE-CORRESPONDING lwa_request TO lwa_response.
      IF pi_wa_bapiret2-type = gc_error.
        lwa_response-msgty   = pi_wa_bapiret2-type.
        lwa_response-message = pi_wa_bapiret2-message.
      ENDIF.
      IF lwa_response-msgty IS INITIAL.
        lwa_response-msgty   = gc_error.
        lwa_response-message = 'No response return from Cloud.'.
      ENDIF.
      APPEND lwa_response TO lit_response.
    ENDIF.

  ENDLOOP.  "pt_response

  APPEND LINES OF lit_response TO cht_response.

  SORT cht_response BY trans_no
                       trans_item.

ENDFORM.                    "rebuild_response
*&---------------------------------------------------------------------*
*& Form RESPONSE_TO_TEXTFILE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LIT_RESPONSE
*&      <-- CHT_TEXTFILE
*&---------------------------------------------------------------------*
FORM response_to_textfile  USING pt_response TYPE ZSDSFIS048_TT
                       CHANGING cht_textfile TYPE ZSDSFIS033_TT.

  DATA: lt_response LIKE pt_response[].

  DATA: lwa_response      LIKE LINE OF pt_response,
        lwa_response_text LIKE LINE OF pt_response.

  FIELD-SYMBOLS: <lfs_text> LIKE LINE OF cht_textfile,
                 <lfs_pdf>  TYPE ZSDSFIS025.

  lt_response[] = pt_response[].

  SORT lt_response BY trans_no trans_item.

  READ TABLE lt_response INTO lwa_response_text
    WITH KEY trans_item = gc_trans_item1.

  LOOP AT cht_textfile ASSIGNING <lfs_text>.
    <lfs_text>-file_status = gc_call_ws.

    IF lwa_response_text-msgty IS NOT INITIAL.
      <lfs_text>-wa_msg-msgty   = lwa_response_text-msgty.
      <lfs_text>-wa_msg-message = lwa_response_text-message.
    ELSE.
      <lfs_text>-wa_msg-msgty   = gc_error.
      <lfs_text>-wa_msg-message = 'No response from Cloud.'.
    ENDIF.

    LOOP AT <lfs_text>-etax_pdf ASSIGNING <lfs_pdf>
      WHERE trans_no   IS NOT INITIAL
        AND trans_item IS NOT INITIAL.
      READ TABLE pt_response INTO lwa_response
        WITH KEY trans_no   = <lfs_pdf>-trans_no
                 trans_item = <lfs_pdf>-trans_item BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <lfs_text>-wa_msg-msgty   = lwa_response-msgty.
        <lfs_text>-wa_msg-message = lwa_response-message.
      ELSE.
        <lfs_text>-wa_msg-msgty   = gc_error.
        <lfs_text>-wa_msg-message = 'No response from Cloud.'.
      ENDIF.
    ENDLOOP.

  ENDLOOP.  "cht_textfile

ENDFORM.                    "response_to_textfile
*&---------------------------------------------------------------------*
*& Form UPDATE_TEXT_RESPONSE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LIT_TEXTFILE
*&      <-- CHT_ETAX_TEXTFILE
*&---------------------------------------------------------------------*
FORM update_text_response
        USING pt_textfile       TYPE ZSDSFIS033_TT
     CHANGING cht_etax_textfile TYPE ZSDSFIS033_TT.

  DATA: lt_textfile  LIKE pt_textfile[].

  DATA: lwa_textfile LIKE LINE OF pt_textfile.
  FIELD-SYMBOLS: <lfs_textfile> LIKE LINE OF cht_etax_textfile.

  lt_textfile[] = pt_textfile[].

  SORT lt_textfile BY trans_no trans_item.

  LOOP AT cht_etax_textfile ASSIGNING <lfs_textfile>.

    CLEAR lwa_textfile.
    READ TABLE lt_textfile INTO lwa_textfile
      WITH KEY trans_no   = <lfs_textfile>-trans_no
               trans_item = <lfs_textfile>-trans_item BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <lfs_textfile> = lwa_textfile.
      IF lwa_textfile-wa_msg-msgty EQ gc_success.
        <lfs_textfile>-wa_msg-message = 'Done'.
      ELSE.
      ENDIF.
    ENDIF.

  ENDLOOP.  "cht_etax_textfile

ENDFORM.                    "update_text_response
*&---------------------------------------------------------------------*
*& Form GEN_LOG_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_TEXT
*&      --> LT_TAX_LOG
*&      --> IM_WA_INF
*&---------------------------------------------------------------------*
FORM gen_log_data    TABLES pt_text      TYPE ZSDSFIS033_TT
                            pt_log       STRUCTURE ZSDSFIT019
                      USING pi_wa_inf    TYPE ZSDSFIS049
                            pi_wa_return_tf    TYPE ZSDSFIS051.

  DATA: lwa_text LIKE LINE OF pt_text.

  FIELD-SYMBOLS: <lfs_tax_log> LIKE LINE OF pt_log.

  PERFORM message_for_batch USING 'Generate log data.'.

  LOOP AT pt_text INTO lwa_text.

    APPEND INITIAL LINE TO pt_log ASSIGNING <lfs_tax_log>.
    MOVE-CORRESPONDING lwa_text TO <lfs_tax_log>.
    <lfs_tax_log>-mandt        = sy-datum.
    <lfs_tax_log>-s_flag       = abap_on.
    <lfs_tax_log>-r_flag       = abap_off.

    IF pi_wa_return_tf-msgty EQ gc_success.
      <lfs_tax_log>-xml_completed = 'X'.
    ENDIF.

    IF lwa_text-reverse_flag = 'Y'. "กรณไม่ส่งกรม ไม่สร้าง PDF
      <lfs_tax_log>-pdf_completed  = 'X'. "แต่ mark 'X' หลอกไว้กัน error
    ELSE.
      <lfs_tax_log>-pdf_completed  = lwa_text-x_pdf_completed.
    ENDIF.
    <lfs_tax_log>-updated_date = sy-datum.
    <lfs_tax_log>-updated_time = sy-uzeit.
    <lfs_tax_log>-updated_by   = sy-uname.

  ENDLOOP.  "im_it_text
ENDFORM.                    "gen_log_data
*&---------------------------------------------------------------------*
*& Form GEN_TRANSACTION_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_TAX_LOG
*&      --> LT_TEXT
*&      --> LWA_MEM_INF01
*&      --> IM_WA_INF
*&      <-- LWA_TRANS
*&---------------------------------------------------------------------*
FORM gen_transaction_data  TABLES   pt_log        STRUCTURE ZSDSFIT019
                                    pt_text       TYPE ZSDSFIS033_TT
                           USING    pwa_mem_inf01 TYPE ZSDSFIS050
                                    pwa_inf       TYPE ZSDSFIS049
                                    pwa_return_tf TYPE ZSDSFIS051
                           CHANGING ch_trans      TYPE ZSDSFIS041.

  DATA: lwa_header  LIKE LINE OF ch_trans-it_header,
        lwa_partner LIKE LINE OF ch_trans-it_partner,
        lwa_h_ref   LIKE LINE OF ch_trans-it_h_ref,
        lwa_h_vat   LIKE LINE OF ch_trans-it_h_vat,
        lwa_item    LIKE LINE OF ch_trans-it_item,
        lwa_dischg  LIKE LINE OF ch_trans-it_dischg,
        lwa_pdf     LIKE LINE OF pt_text-etax_pdf.

  DATA: lv_date TYPE d,
        lv_time TYPE t.

  FIELD-SYMBOLS: <lfs_log>     LIKE LINE OF pt_log,
                 <lfs_text>    LIKE LINE OF pt_text,
                 <lfs_partner> LIKE LINE OF pt_text-doc_partner,
                 <lfs_item>    LIKE LINE OF pt_text-doc_item,
                 <lfs_h_ref>   LIKE LINE OF pt_text-doc_h_ref,
                 <lfs_h_vat>   LIKE LINE OF pt_text-doc_h_vat,
                 <lfs_dischg>  LIKE LINE OF pt_text-doc_dischg.

  PERFORM message_for_batch USING 'Generate transaction data.'.

  lv_date = sy-datum.
  lv_time = sy-uzeit.

  LOOP AT pt_text ASSIGNING <lfs_text>.

    MOVE-CORRESPONDING:
    <lfs_text> TO lwa_header.

    READ TABLE <lfs_text>-etax_pdf INTO lwa_pdf INDEX 1.
    <lfs_text>-arc_fullpath = lwa_pdf-arc_fullpath.

    lwa_header-mandt     = sy-mandt.
    lwa_header-etax_date = lv_date.
    lwa_header-etax_time = lv_time.
    lwa_header-etax_by   = sy-uname.

    IF pwa_return_tf-msgty EQ gc_success.
      lwa_header-status  = '2'.          "2 - Transferred Complete
    ELSE.
      lwa_header-status  = '4'.          "4 - Transferred Incomplete
    ENDIF.

    IF lwa_header-reverse_flag EQ space.  "Normal Document

    ELSE.                                 "Cancelled Document

    ENDIF.

*    Update ZSDSFIT014
    APPEND lwa_header TO ch_trans-it_header.

    LOOP AT <lfs_text>-doc_partner ASSIGNING <lfs_partner>.
      MOVE-CORRESPONDING: <lfs_partner> TO lwa_partner.

*    Update ZSDSFIT018
      lwa_partner-mandt = sy-mandt.
      APPEND lwa_partner TO ch_trans-it_partner.
    ENDLOOP.

    LOOP AT <lfs_text>-doc_item ASSIGNING <lfs_item>.
      MOVE-CORRESPONDING: <lfs_item> TO lwa_item.

*    Update ZSDSFIT015
      lwa_item-mandt = sy-mandt.
      APPEND lwa_item TO ch_trans-it_item.
    ENDLOOP.

    LOOP AT <lfs_text>-doc_h_ref ASSIGNING <lfs_h_ref>.
      MOVE-CORRESPONDING: <lfs_h_ref> TO lwa_h_ref.

*    Update ZSDSFIT016
      lwa_h_ref-mandt = sy-mandt.
      lwa_h_ref-ref_sap_post_dat = <lfs_h_ref>-ref_sap_post_date.
      APPEND lwa_h_ref TO ch_trans-it_h_ref.
    ENDLOOP.

    LOOP AT <lfs_text>-doc_h_vat ASSIGNING <lfs_h_vat>.
      MOVE-CORRESPONDING: <lfs_h_vat> TO lwa_h_vat.

*    Update ZSDSFIT017
      lwa_h_vat-mandt = sy-mandt.
      APPEND lwa_h_vat TO ch_trans-it_h_vat.
    ENDLOOP.

    LOOP AT <lfs_text>-doc_dischg ASSIGNING <lfs_dischg>.
      MOVE-CORRESPONDING: <lfs_dischg> TO lwa_dischg.

*    Update ZSDSFIT013
      lwa_dischg-mandt = sy-mandt.
      APPEND lwa_dischg TO ch_trans-it_dischg.
    ENDLOOP.

  ENDLOOP.

  LOOP AT pt_log ASSIGNING <lfs_log>.
    READ TABLE pt_text ASSIGNING <lfs_text>
      WITH KEY bukrs        = <lfs_log>-bukrs
               sap_doc_no   = <lfs_log>-sap_doc_no
               gjahr        = <lfs_log>-gjahr
               rd_doc_type  = <lfs_log>-rd_doc_type
               module_etx   = <lfs_log>-module_etx BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <lfs_log>-trans_no   = <lfs_text>-trans_no.
      <lfs_log>-trans_item = <lfs_text>-trans_item.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "gen_transaction_data
*&---------------------------------------------------------------------*
*& Form UPDATE_STATUS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_TAX_LOG
*&      --> LT_TEXT
*&---------------------------------------------------------------------*
FORM update_status  TABLES   pt_log         STRUCTURE ZSDSFIT019
                             pt_text        TYPE ZSDSFIS033_TT
                       USING pwa_text_tf    TYPE ZSDSFIS051
                    CHANGING ch_trans       TYPE ZSDSFIS041.

  DATA: lwa_text LIKE LINE OF pt_text.

  FIELD-SYMBOLS: <lfs_log>    LIKE LINE OF pt_log,
                 <lfs_header> LIKE LINE OF ch_trans-it_header.

  PERFORM message_for_batch USING 'Update processing status.'.

  LOOP AT pt_text INTO lwa_text.
    READ TABLE pt_log ASSIGNING <lfs_log>
    WITH KEY bukrs       = lwa_text-bukrs
             sap_doc_no  = lwa_text-sap_doc_no
             gjahr       = lwa_text-gjahr
             rd_doc_type = lwa_text-rd_doc_type
             module_etx  = lwa_text-module_etx BINARY SEARCH.
    IF sy-subrc EQ 0.

      IF lwa_text-file_status EQ gc_call_ws AND
         lwa_text-wa_msg-msgty EQ gc_success.
        <lfs_log>-xml_downloaded = 'X'.
      ENDIF.
    ENDIF.

    READ TABLE ch_trans-it_header ASSIGNING <lfs_header>
    WITH KEY bukrs       = lwa_text-bukrs
             sap_doc_no  = lwa_text-sap_doc_no
             gjahr       = lwa_text-gjahr
             rd_doc_type = lwa_text-rd_doc_type
             module_etx  = lwa_text-module_etx BINARY SEARCH.
    IF sy-subrc EQ 0.
      IF lwa_text-wa_msg-msgty EQ gc_error.
        <lfs_header>-status = '4'.      "4 - Transferred Incomplete
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "update_status
*&---------------------------------------------------------------------*
*& Form DOWNLOAD_TO_PC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IM_WA_INF_FULLPATH_SERVER
*&      --> LV_FILENAME_ZIP
*&---------------------------------------------------------------------*
FORM download_to_pc  USING    pi_server
                              pi_file
                              pi_pc_dir.

  CONSTANTS : blocksize   TYPE i VALUE 65535,               "524287,
              packagesize TYPE i VALUE 8.

  TYPES ty_datablock(blocksize) TYPE x .

  DATA lt_data TYPE STANDARD TABLE OF ty_datablock.

  DATA ls_data TYPE ty_datablock.

  DATA: lv_cl_fn TYPE string, "VALUE 'C:\Users\Etax'.
        lv_as_fn TYPE text255. "sapb-sappfad.

  DATA: lv_fil              TYPE text255, ".epsf-epsfilnam.
        lv_dir              TYPE epsf-epsdirnam,
        lv_block_len        TYPE i,
        lv_package_len      TYPE i,
        lv_subrc            TYPE sy-subrc,
        lv_msgv1            LIKE sy-msgv1,
        lv_processed_so_far TYPE p,
        lv_append           TYPE c,
        lv_status           TYPE string,
        lv_filesize         TYPE p.

  CHECK pi_pc_dir IS NOT INITIAL.

  lv_cl_fn = pi_pc_dir.
  lv_dir   = pi_server.
  lv_fil   = pi_file.

  CONCATENATE : lv_dir lv_fil INTO lv_as_fn,
                lv_cl_fn '\' lv_fil INTO lv_cl_fn.

  "Open the file on application server
  OPEN DATASET lv_as_fn FOR INPUT IN BINARY MODE MESSAGE lv_msgv1.
  CHECK sy-subrc EQ 0.
*  IF sy-subrc <> 0.
*    MESSAGE e048(cms) WITH lv_as_fn lv_msgv1 RAISING file_read_error.
*    EXIT.
*  ENDIF.

  lv_processed_so_far = 0.
  DO.

    REFRESH lt_data.
    lv_package_len = 0.
    DO packagesize TIMES.
      CLEAR ls_data.
      CLEAR lv_block_len.
      READ DATASET lv_as_fn INTO ls_data MAXIMUM LENGTH blocksize LENGTH lv_block_len.
      lv_subrc = sy-subrc.
      IF lv_block_len > 0.
        lv_package_len = lv_package_len + lv_block_len.
        APPEND ls_data TO lt_data.
      ENDIF.
      "End of file
      IF lv_subrc <> 0.
        EXIT.
      ENDIF.
    ENDDO.

    IF lv_package_len > 0.
      "Put file to client
      IF lv_processed_so_far = 0.
        lv_append = ' '.
      ELSE.
        lv_append = 'X'.
      ENDIF.
      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          bin_filesize         = lv_package_len
          filename             = lv_cl_fn
          filetype             = 'BIN'
          append               = lv_append
          show_transfer_status = abap_false
        TABLES
          data_tab             = lt_data.

    ENDIF.

    "End of file
    IF lv_subrc <> 0.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.                    "download_to_pc
*&---------------------------------------------------------------------*
*& Form UPDATE_LOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_TAX_LOG
*&      --> EX_IT_BAPIRET2
*&      --> LWA_TRANS
*&---------------------------------------------------------------------*
FORM update_log  TABLES   pt_log           STRUCTURE ZSDSFIT019
                          pt_bapi_ret2     TYPE bapiret2_t
                 USING    pi_trans         TYPE ZSDSFIS041.

  TYPES: BEGIN OF lty_count.
  INCLUDE TYPE ZSDSFIS020.
  TYPES:
    success TYPE i,
    error   TYPE i.
  TYPES: END OF lty_count.

  DATA: lit_count     TYPE TABLE OF lty_count.

  DATA : lwa_bapi_ret2 LIKE LINE OF pt_bapi_ret2,
         lwa_count     LIKE LINE OF lit_count,
         lwa_log       LIKE LINE OF pt_log.

  DATA : lv_record      TYPE i,
         lv_msg         TYPE string,
         lv_msgty       TYPE sy-msgty,
         lv_success     TYPE i,
         lv_error       TYPE i,
         lv_success_txt TYPE c LENGTH 20,
         lv_error_txt   TYPE c LENGTH 20.

  PERFORM message_for_batch USING 'Update log data to database.'.

  LOOP AT pt_log INTO lwa_log.

    IF lwa_log-xml_completed  EQ 'X' AND
       lwa_log-xml_downloaded EQ 'X' AND
       lwa_log-pdf_completed  EQ 'X'.
      ADD 1 TO lv_success.
    ELSE.
      ADD 1 TO lv_error.
    ENDIF.
  ENDLOOP.

  WRITE lv_success TO lv_success_txt.
  WRITE lv_error   TO lv_error_txt.

  CONDENSE lv_success_txt NO-GAPS.
  CONDENSE lv_error_txt   NO-GAPS.

  MESSAGE s000(38) WITH 'Completed: ' lv_success_txt
                        'Error: '     lv_error_txt INTO lv_msg.
  CONCATENATE lv_msg 'record(s)'
         INTO lv_msg SEPARATED BY space.

  IF lv_error > 0.
    IF lv_success > 0.
      lv_msgty = 'W'.
    ELSE.
      lv_msgty = 'E'.
    ENDIF.
  ELSE.
    lv_msgty = 'S'.
  ENDIF.

  CLEAR lwa_bapi_ret2.
  lwa_bapi_ret2-type    = lv_msgty.
  lwa_bapi_ret2-message = lv_msg.
  APPEND lwa_bapi_ret2 TO pt_bapi_ret2.

  MODIFY ZSDSFIT019       FROM TABLE pt_log[].

  MODIFY ZSDSFIT014  FROM TABLE pi_trans-it_header[].
  MODIFY ZSDSFIT018 FROM TABLE pi_trans-it_partner[].
  MODIFY ZSDSFIT015    FROM TABLE pi_trans-it_item[].
  MODIFY ZSDSFIT016   FROM TABLE pi_trans-it_h_ref[].
  MODIFY ZSDSFIT017   FROM TABLE pi_trans-it_h_vat[].
  MODIFY ZSDSFIT013  FROM TABLE pi_trans-it_dischg[].
  COMMIT WORK AND WAIT.

ENDFORM.                    "update_log
