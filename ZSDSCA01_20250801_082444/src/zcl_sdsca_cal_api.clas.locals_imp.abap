*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS LCL_ZGG_API DEFINITION.

  PUBLIC SECTION.
*--------------------------------------------------------------------*
* Type
*--------------------------------------------------------------------*
    TYPES GY_URL TYPE C LENGTH 2500.
*--------------------------------------------------------------------*
* Data
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
* Constants
*--------------------------------------------------------------------*
    CONSTANTS : GC_X TYPE C VALUE 'X',
                GC_U TYPE C VALUE 'U'.

    CONSTANTS : GC_CALL_BACK TYPE C LENGTH 11 VALUE 'WEBCALLBACK'.
*--------------------------------------------------------------------*
* Methods
*--------------------------------------------------------------------*
*    METHODS check_token
*      IMPORTING
*        i_token TYPE ztgoogletoken
*      CHANGING
*        e_check TYPE c.
*
*    METHODS get_url
*      IMPORTING
*        i_mandt   TYPE sy-mandt
*        i_process TYPE ztgoogleprocess-process
*        i_uname   TYPE sy-uname OPTIONAL
*      CHANGING
*        e_url     TYPE gy_url.

    METHODS CALL_URL
      IMPORTING
        I_URL TYPE GY_URL.

*    METHODS refresh_token
*      IMPORTING
*        i_proc  TYPE ztgoogleprocess-process
*        i_user  TYPE sy-uname
*        i_token TYPE ztgoogletoken
*      EXPORTING
*        e_token TYPE ztgoogletoken.
*
*    METHODS refresh_token_update
*      IMPORTING
*        i_data  TYPE string
*      EXPORTING
*        e_token TYPE ztgoogletoken.
*
*    METHODS get_token
*      IMPORTING
*        i_proc  TYPE ztgoogleprocess-process
*        i_user  TYPE sy-uname
*        i_token TYPE ztgoogletoken
*      EXPORTING
*        e_token TYPE ztgoogletoken.
*
*    METHODS token_update
*      IMPORTING
*        i_data  TYPE string
*      EXPORTING
*        e_token TYPE ztgoogletoken.

    METHODS CONVERT_JSON
      IMPORTING
        I_JSON TYPE STRING
      EXPORTING
        E_DATA TYPE ANY.

ENDCLASS.

CLASS LCL_ZGG_API IMPLEMENTATION.

*  METHOD check_token.
*    DATA : i_add_seconds TYPE p,
*           i_uzeit       TYPE sy-uzeit,
*           i_datum       TYPE sy-datum.
*
*    DATA : e_datum TYPE sy-datum,
*           e_uzeit TYPE sy-uzeit.
*
*    DATA : lv_time_chk TYPE sy-uzeit.
*
*    i_add_seconds = i_token-expires_in - 600.
*    i_uzeit       = i_token-updated_time.
*    i_datum       = i_token-updated_on.
*
*    CALL FUNCTION 'C14Z_CALC_DATE_TIME'
*      EXPORTING
*        i_add_seconds = i_add_seconds
*        i_uzeit       = i_uzeit
*        i_datum       = i_datum
*      IMPORTING
*        e_datum       = e_datum
*        e_uzeit       = e_uzeit.
*    IF i_token-token IS INITIAL.
*      e_check = gc_x.
*    ELSE.
*      IF     sy-datum GT e_datum.
*        e_check = gc_u.
*      ELSEIF sy-uzeit GT e_uzeit.
*        e_check = gc_u.
*      ELSE.
*        CLEAR : e_check.
*      ENDIF.
*    ENDIF.
*  ENDMETHOD.
*
*  METHOD get_url.
*
*    DATA : ls_scope TYPE ztgooglescope,
*           ls_webcb TYPE ztgooglescope.
*
*    DATA : lt_scope TYPE TABLE OF ztgooglescope.
*
*    DATA : ls_tmp LIKE ls_scope.
*
*    DATA : BEGIN OF ls_ztgooglekey,
**             process TYPE ztgooglekey-process,
**             keyid   TYPE ztgooglekey-keyid,
*             cliid TYPE ztgooglekey-cliid,
**             clisc   TYPE ztgooglekey-clisc,
*           END OF ls_ztgooglekey.
*
*    DATA : lv_uname TYPE sy-uname.
*
*    SELECT *
*      FROM ztgooglescope
*      INTO TABLE lt_scope
*      WHERE process EQ i_process.
*
*    SELECT SINGLE "process
*                  "keyid
*                  cliid
**                  clisc
*      FROM ztgooglekey
*      INTO ls_ztgooglekey
*      WHERE process EQ i_process.
*
*    READ TABLE lt_scope INTO ls_webcb
*    WITH KEY sname = gc_call_back
*             runng = i_mandt.
*
*    DELETE lt_scope WHERE sname = gc_call_back.
*
*    SORT lt_scope BY runng.
*
*    IF i_uname IS NOT INITIAL.
*      lv_uname = i_uname.
*    ELSE.
*      lv_uname = sy-uname.
*    ENDIF.
*
*    LOOP AT lt_scope INTO ls_tmp.
*      MOVE-CORRESPONDING ls_tmp TO ls_scope.
*
*      IF ls_tmp-runng EQ 1.
*        CONCATENATE ls_scope-scope 'scope=' INTO e_url.
*      ELSE.
*        CONCATENATE e_url ls_scope-scope INTO e_url SEPARATED BY '%20'.
*      ENDIF.
*
*      AT LAST.
*        CONCATENATE e_url '&' 'state=' lv_uname '|' i_process INTO e_url.
*        CONCATENATE e_url '&access_type=offline'              INTO e_url.
*        CONCATENATE e_url '&include_granted_scopes=true'      INTO e_url.
*        CONCATENATE e_url '&response_type=code'               INTO e_url.
*        CONCATENATE e_url '&redirect_uri='                    INTO e_url.
*
*        IF ls_webcb IS NOT INITIAL.
*          CONCATENATE e_url ls_webcb-scope INTO e_url.
*        ENDIF.
*        CONCATENATE e_url '&client_id=' ls_ztgooglekey-cliid INTO e_url.
*        CONCATENATE e_url '&prompt=consent' INTO e_url.
*      ENDAT.
*
*    ENDLOOP.
*
*
*  ENDMETHOD.

  METHOD CALL_URL.
    CALL FUNCTION 'ZFMGG001_CALL_URL'
      EXPORTING
        I_URL = I_URL.
  ENDMETHOD.
*
*  METHOD refresh_token.
*
*    DATA : i_url              TYPE  string,
*           i_method           TYPE  string,
*           i_header           TYPE  ztsgg001,
*           i_body_text        TYPE  string,
*           i_body_bin         TYPE  xstring,
*           i_len              TYPE  i,
*           i_len_bin          TYPE  i,
*           e_return_body_text TYPE  string,
*           e_return_body_bin  TYPE  xstring,
*           e_message          TYPE  string,
*           e_status           TYPE  c.
*
*    DATA lcl_gg_api TYPE REF TO zcl_gg_api.
*
*    DATA : BEGIN OF ls_ztgooglekey,
**             keyid TYPE ztgooglekey-keyid,
*             cliid TYPE ztgooglekey-cliid,
*             clisc TYPE ztgooglekey-clisc,
*           END OF ls_ztgooglekey.
**    DATA lt_ztgooglekey LIKE TABLE OF ls_ztgooglekey.
*
*    DATA : BEGIN OF ls_googleurl,
*             url    TYPE ztgoogleurl-url,
*             method TYPE ztgoogleurl-method,
*           END OF ls_googleurl.
**    DATA lt_googleurl LIKE table OF ls_googleurl.
*
*    DATA : BEGIN OF ls_googleparam,
*             runng TYPE ztgoogleparam-runng,
*             param TYPE ztgoogleparam-param,
*             value TYPE ztgoogleparam-value,
*           END OF ls_googleparam.
*    DATA lt_googleparam LIKE TABLE OF ls_googleparam.
*
*    DATA : lt_param TYPE ztsgg002,
*           ls_param LIKE LINE OF lt_param.
*
*    DATA : ls_refresh_token TYPE ztgoogletoken-refresh_token.
*
*    SELECT SINGLE cliid
*                  clisc
*      FROM ztgooglekey
*      INTO ls_ztgooglekey
*      WHERE process EQ i_proc.
*
*    SELECT SINGLE refresh_token
*      FROM ztgoogletoken
*      INTO ls_refresh_token
*      WHERE uname   = i_user
*        AND process = i_proc.
*
*    SELECT SINGLE url
*                  method
*      FROM ztgoogleurl
*      INTO ls_googleurl
*      WHERE aname EQ 'TOKEN_RE'.
*
*    SELECT runng
*           param
*           value
*      FROM ztgoogleparam
*      INTO TABLE lt_googleparam
*      WHERE aname EQ 'TOKEN_RE'.
*
*    LOOP AT lt_googleparam INTO ls_googleparam.
*      ls_param-name = ls_googleparam-param.
*      IF  ls_googleparam-runng EQ 1.
*        ls_param-value = ls_ztgooglekey-cliid.
*      ELSEIF ls_googleparam-runng EQ 2.
*        ls_param-value = ls_ztgooglekey-clisc.
*      ELSEIF ls_googleparam-runng EQ 3.
*        ls_param-value = ls_refresh_token.
*      ELSEIF ls_googleparam-runng EQ 4.
*        ls_param-value = ls_googleparam-value.
*      ELSE.
*        ls_param-value = ls_googleparam-value.
*      ENDIF.
*
*      APPEND ls_param TO lt_param.
*    ENDLOOP.
*
*    i_url     = ls_googleurl-url.
*    i_method  = ls_googleurl-method.
*
*    IF lcl_gg_api IS INITIAL.
*      CREATE OBJECT lcl_gg_api.
*    ENDIF.
*
*    CALL METHOD lcl_gg_api->gen_url
*      EXPORTING
*        i_url       = i_url
*        i_parameter = lt_param
*      RECEIVING
*        r_url       = i_url.
*
*    CALL METHOD lcl_gg_api->call_api
*      EXPORTING
*        i_url              = i_url
*        i_method           = i_method
*        i_header           = i_header
*        i_body_text        = i_body_text
*        i_body_bin         = i_body_bin
*        i_len              = i_len
*        i_len_bin          = i_len_bin
*      IMPORTING
*        e_return_body_text = e_return_body_text
*        e_return_body_bin  = e_return_body_bin
*        e_message          = e_message
*        e_status           = e_status.
*
*    CALL METHOD refresh_token_update
*      EXPORTING
*        i_data  = e_return_body_text
*      IMPORTING
*        e_token = e_token.
*  ENDMETHOD.
*
*  METHOD get_token.
*    DATA : i_url              TYPE  string,
*           i_method           TYPE  string,
*           i_header           TYPE  ztsgg001,
*           i_body_text        TYPE  string,
*           i_body_bin         TYPE  xstring,
*           i_len              TYPE  i,
*           i_len_bin          TYPE  i,
*           e_return_body_text TYPE  string,
*           e_return_body_bin  TYPE  xstring,
*           e_message          TYPE  string,
*           e_status           TYPE  c.
*
*    DATA lcl_gg_api TYPE REF TO zcl_gg_api.
*
*    DATA : BEGIN OF ls_ztgooglekey,
**             keyid TYPE ztgooglekey-keyid,
*             cliid TYPE ztgooglekey-cliid,
*             clisc TYPE ztgooglekey-clisc,
*           END OF ls_ztgooglekey.
**    DATA lt_ztgooglekey LIKE TABLE OF ls_ztgooglekey.
*
*    DATA : BEGIN OF ls_googleurl,
*             url    TYPE ztgoogleurl-url,
*             method TYPE ztgoogleurl-method,
*           END OF ls_googleurl.
**    DATA lt_googleurl LIKE table OF ls_googleurl.
*
*    DATA : BEGIN OF ls_googleparam,
*             runng TYPE ztgoogleparam-runng,
*             param TYPE ztgoogleparam-param,
*             value TYPE ztgoogleparam-value,
*           END OF ls_googleparam.
*    DATA lt_googleparam LIKE TABLE OF ls_googleparam.
*
*    DATA : lt_param TYPE ztsgg002,
*           ls_param LIKE LINE OF lt_param.
*
*    DATA : lv_code  TYPE ztgoogletoken-code,
*           lv_scope TYPE ztgooglescope-scope.
*
*    SELECT SINGLE cliid
*                  clisc
*      FROM ztgooglekey
*      INTO ls_ztgooglekey
*      WHERE process EQ i_proc.
*
*    SELECT SINGLE code
*      FROM ztgoogletoken
*      INTO lv_code
*      WHERE uname   = i_user
*        AND process = i_proc.
*
*    SELECT SINGLE url
*                  method
*      FROM ztgoogleurl
*      INTO ls_googleurl
*      WHERE aname EQ 'TOKEN'.
*
*    SELECT runng
*           param
*           value
*      FROM ztgoogleparam
*      INTO TABLE lt_googleparam
*      WHERE aname EQ 'TOKEN'.
*
*    SELECT SINGLE scope
*      FROM ztgooglescope
*      INTO lv_scope
*      WHERE sname   EQ 'WEBCALLBACK'
*        AND runng   EQ sy-mandt
*        AND process EQ i_proc.
*
*    LOOP AT lt_googleparam INTO ls_googleparam.
*      ls_param-name = ls_googleparam-param.
*      IF  ls_googleparam-runng    EQ 1.
*        ls_param-value = lv_code.
*      ELSEIF ls_googleparam-runng EQ 2.
*        ls_param-value = ls_ztgooglekey-cliid.
*      ELSEIF ls_googleparam-runng EQ 3.
*        ls_param-value = ls_ztgooglekey-clisc.
*      ELSEIF ls_googleparam-runng EQ 4.
*        ls_param-value = lv_scope.
*      ELSEIF ls_googleparam-runng EQ 5.
*        ls_param-value = ls_googleparam-value.
*      ELSE.
*        ls_param-value = ls_googleparam-value.
*      ENDIF.
*
*      APPEND ls_param TO lt_param.
*    ENDLOOP.
*
*    i_url     = ls_googleurl-url.
*    i_method  = ls_googleurl-method.
*
*    IF lcl_gg_api IS INITIAL.
*      CREATE OBJECT lcl_gg_api.
*    ENDIF.
*
*    CALL METHOD lcl_gg_api->gen_url
*      EXPORTING
*        i_url       = i_url
*        i_parameter = lt_param
*      RECEIVING
*        r_url       = i_url.
*
*    CALL METHOD lcl_gg_api->call_api
*      EXPORTING
*        i_url              = i_url
*        i_method           = i_method
*        i_header           = i_header
*        i_body_text        = i_body_text
*        i_body_bin         = i_body_bin
*        i_len              = i_len
*        i_len_bin          = i_len_bin
*      IMPORTING
*        e_return_body_text = e_return_body_text
*        e_return_body_bin  = e_return_body_bin
*        e_message          = e_message
*        e_status           = e_status.
*
*    CALL METHOD token_update
*      EXPORTING
*        i_data  = e_return_body_text
*      IMPORTING
*        e_token = e_token.
*  ENDMETHOD.

  METHOD CONVERT_JSON.
    CALL METHOD /UI2/CL_JSON=>DESERIALIZE
      EXPORTING
        JSON        = I_JSON
        PRETTY_NAME = /UI2/CL_JSON=>PRETTY_MODE-CAMEL_CASE
*       assoc_arrays = abap_true
      CHANGING
        DATA        = E_DATA
      EXCEPTIONS
        OTHERS      = 1.
  ENDMETHOD.

*  METHOD refresh_token_update.
*    DATA: BEGIN OF ls_data,
*            access_token TYPE string,
*            expires_in   TYPE string,
*            scope        TYPE string,
*            token_type   TYPE string,
**          refresh_token TYPE string,
*          END OF ls_data.
*
*    CALL METHOD convert_json
*      EXPORTING
*        i_json = i_data
*      IMPORTING
*        e_data = ls_data.
*
*    e_token-token         = ls_data-access_token.
*    e_token-expires_in    = ls_data-expires_in.
*    e_token-scope         = ls_data-scope.
*    e_token-token_type    = ls_data-token_type.
*    e_token-updated_by    = sy-uname.
*    e_token-updated_on    = sy-datum.
*    e_token-updated_time  = sy-uzeit.
*
*    MODIFY ztgoogletoken FROM e_token.
*    COMMIT WORK AND WAIT.
*  ENDMETHOD.
*
*  METHOD token_update.
*    DATA: BEGIN OF ls_data,
*            access_token  TYPE string,
*            expires_in    TYPE string,
*            scope         TYPE string,
*            token_type    TYPE string,
*            refresh_token TYPE string,
*          END OF ls_data.
*
*    CALL METHOD convert_json
*      EXPORTING
*        i_json = i_data
*      IMPORTING
*        e_data = ls_data.
*
*    e_token-token         = ls_data-access_token.
*    e_token-refresh_token = ls_data-refresh_token.
*    e_token-expires_in    = ls_data-expires_in.
*    e_token-scope         = ls_data-scope.
*    e_token-token_type    = ls_data-token_type.
*    e_token-updated_by    = sy-uname.
*    e_token-updated_on    = sy-datum.
*    e_token-updated_time  = sy-uzeit.
*
*    MODIFY ztgoogletoken FROM e_token.
*    COMMIT WORK AND WAIT.
*  ENDMETHOD.
ENDCLASS.
CLASS LCL_DATA DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS :
      GET_ENDPOINT IMPORTING I_PROCESS TYPE ZSDSDE_PROCESS_NAME
                             I_ENV     TYPE ZSDSDE_ENVIRONMENT
                   RETURNING VALUE(R)  TYPE ZSDSDE_END_POINT.

ENDCLASS.
CLASS lcl_Data IMPLEMENTATION.
  METHOD GET_ENDPOINT.
    SELECT SINGLE END_POINT
      FROM ZSDSCAC010
      WHERE PROCESS_NAME EQ @I_PROCESS
        AND ENVIRONMENT  EQ @I_ENV
       INTO @R.
  ENDMETHOD.
ENDCLASS.
