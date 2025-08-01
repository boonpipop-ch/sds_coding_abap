FUNCTION Z_SDSFI_GET_FILE_PATH.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(EX_WA_FILE_PATH) TYPE  ZSDSFIS028
*"----------------------------------------------------------------------

  DATA: lt_param TYPE tty_param.

  DATA: ls_param LIKE LINE OF lt_param.

  RANGES: lr_name FOR ZSDSFIC015-name.

  FIELD-SYMBOLS: <lfs_name> LIKE LINE OF lr_name.

  APPEND INITIAL LINE TO lr_name ASSIGNING <lfs_name>.
  <lfs_name>     = 'IEQ'.
  <lfs_name>-low = 'FILE_PATH'.

  APPEND INITIAL LINE TO lr_name ASSIGNING <lfs_name>.
  <lfs_name>     = 'IEQ'.
  <lfs_name>-low = 'FTP_HOST'.

  APPEND INITIAL LINE TO lr_name ASSIGNING <lfs_name>.
  <lfs_name>     = 'IEQ'.
  <lfs_name>-low = 'FTP_USER'.

  APPEND INITIAL LINE TO lr_name ASSIGNING <lfs_name>.
  <lfs_name>     = 'IEQ'.
  <lfs_name>-low = 'FTP_DIR_IN'.

  SELECT id name param_ext sequence
         endda begda param_sign param_option
         low_value high_value comments
    FROM ZSDSFIC015
    INTO TABLE lt_param
    WHERE id    = 'ZETX001'
    AND   name  IN lr_name
    AND   endda >= sy-datum
    AND   begda <= sy-datum.

  LOOP AT lt_param INTO ls_param.

    CASE ls_param-name .
      WHEN 'FILE_PATH'.
        IF ls_param-param_ext EQ 'SAP_DIR_ARCH'.
          "SAP_DIR_ARCH
          ex_wa_file_path-sap_dir_arch = ls_param-low_value.
        ELSEIF ls_param-param_ext EQ 'SAP_DIR_IN'.
          "SAP_DIR_IN
          ex_wa_file_path-sap_dir_in   = ls_param-low_value.
        ELSEIF ls_param-param_ext EQ 'SAP_DIR_OUT'.
          "SAP_DIR_OUT
          ex_wa_file_path-sap_dir_out  = ls_param-low_value.
        ENDIF.
      WHEN 'FTP_HOST'.
        "FTP_HOST
        IF sy-sysid EQ ls_param-param_ext.    "Wait for user systems
          ex_wa_file_path-ftp_host = ls_param-low_value.
        ENDIF.
      WHEN 'FTP_USER'.
        "FTP_USER
        IF sy-sysid EQ ls_param-param_ext.    "Wait for user systems
          ex_wa_file_path-ftp_user = ls_param-low_value.
        ENDIF.
      WHEN 'FTP_DIR_IN'.
        "FTP_DIR_IN
        IF sy-sysid EQ ls_param-param_ext.    "Wait for user systems
          ex_wa_file_path-ftp_dir_in   = ls_param-low_value.
        ENDIF.
    ENDCASE.
  ENDLOOP.
ENDFUNCTION.
