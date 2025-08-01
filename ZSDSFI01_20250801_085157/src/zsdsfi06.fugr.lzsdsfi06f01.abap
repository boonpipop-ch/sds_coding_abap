*----------------------------------------------------------------------*
***INCLUDE LZSDSFI06F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form GEN_TRANSACTION_NO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_TRANS_NO
*&---------------------------------------------------------------------*
FORM gen_transaction_no  CHANGING ch_trans_no TYPE zsdsde_trans_no.

  CLEAR ch_trans_no.
  CALL FUNCTION 'ZETX_GEN_TRANSACTION_NO'
    IMPORTING
      ex_transaction_no = ch_trans_no.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZFILL_FILE_LIST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IM_DIRECTORY
*&      <-- LIT_FILE
*&---------------------------------------------------------------------*
FORM zfill_file_list  USING pi_dir_name
                  CHANGING cht_file_list TYPE tty_file.

  DATA: lv_dir_name     TYPE dirname_al11,
        lv_generic_name TYPE filename_al11.

  lv_dir_name     = pi_dir_name.
  lv_generic_name = 'EZTAX*'.

  PERFORM fill_file_list USING lv_dir_name
                               lv_generic_name
                               space
                               c_file_list_create
                      CHANGING cht_file_list.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_FILE_LIST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_DIR_NAME
*&      --> LV_GENERIC_NAME
*&      --> SPACE
*&      --> C_FILE_LIST_CREATE
*&      <-- CHT_FILE_LIST
*&---------------------------------------------------------------------*
FORM fill_file_list  USING    a_dir_name      TYPE dirname_al11
                             a_generic_name  TYPE filename_al11
                             a_must_cs       TYPE c
                             a_operation     TYPE c
                 CHANGING    cht_file        TYPE tty_file.

  DATA:
    errcnt(2)       TYPE p VALUE 0.

  DATA: file_list TYPE STANDARD TABLE OF ts_file WITH HEADER LINE." WITH NON-UNIQUE SORTED KEY k1 COMPONENTS name.

  REFRESH cht_file.

  IF a_dir_name IS INITIAL.
    EXIT.
  ENDIF.

  CALL 'C_DIR_READ_FINISH'             " just to be sure
      ID 'ERRNO'  FIELD file_list-errno
      ID 'ERRMSG' FIELD file_list-errmsg.

  CALL 'C_DIR_READ_START' ID 'DIR'    FIELD a_dir_name
                          ID 'FILE'   FIELD a_generic_name
                          ID 'ERRNO'  FIELD file-errno
                          ID 'ERRMSG' FIELD file-errmsg.
  IF sy-subrc <> 0.
*   check if the directory was already pushed to the stack; if
*   yes: remove from stack
    DATA: l_nr       TYPE i,
          l_dirstack LIKE LINE OF directory_stack.
    l_nr = lines( directory_stack ).
    IF l_nr > 0.
      READ TABLE directory_stack INTO l_dirstack INDEX l_nr.
      IF l_dirstack-name = a_dir_name.
        DELETE directory_stack INDEX l_nr.
      ENDIF.
    ENDIF.
    "message e204 with file_list-errmsg file-errmsg.
  ENDIF.

  DO.
    CLEAR file.
    CALL 'C_DIR_READ_NEXT'
      ID 'TYPE'   FIELD file-type
      ID 'NAME'   FIELD file-name
      ID 'LEN'    FIELD file-len
      ID 'OWNER'  FIELD file-owner
      ID 'MTIME'  FIELD file-mtime
      ID 'MODE'   FIELD file-mode
      ID 'ERRNO'  FIELD file-errno
      ID 'ERRMSG' FIELD file-errmsg.
    file-dirname = a_dir_name.
    MOVE sy-subrc TO file-subrc.
    CASE sy-subrc.
      WHEN 0.
        CLEAR: file-errno, file-errmsg.
        CASE file-type(1).
          WHEN 'F'.                    " normal file.
            PERFORM filename_useable USING file-name CHANGING file-useable.
          WHEN 'f'.                    " normal file.
            PERFORM filename_useable USING file-name CHANGING file-useable.
          WHEN OTHERS. " directory, device, fifo, socket,...
            MOVE abap_false  TO file-useable.
        ENDCASE.
        IF file-len = 0.
          MOVE abap_false TO file-useable.
        ENDIF.
      WHEN 1.
        "No more slots available.
        EXIT.
      WHEN 5.
        "Only NAME is valid due to internal error.
        CLEAR: file-type, file-len, file-owner, file-mtime, file-mode,
               file-errno, file-errmsg.
        file-useable = abap_false.
      WHEN OTHERS.
        "possible other return codes (sapaci2.c)
        "3 ... Internal error.
        "4 ... NAME is truncated (Warning only)
        ADD 1 TO errcnt.
        IF errcnt > 10.
          EXIT.
        ENDIF.
        "don't list files with error
        IF file-subrc = 3.
          CONTINUE.
        ENDIF.
    ENDCASE.
    PERFORM p6_to_date_time_tz(rstr0400) USING file-mtime
                                               file-mod_time
                                               file-mod_date.


*    IF a_operation EQ C_FILE_LIST_UPDATE.
*      READ TABLE file_list WITH KEY k1 COMPONENTS NAME = file-NAME ASSIGNING <File>.
*
*      IF sy-subrc EQ 0.
*        "In case file is found in the list means it already exists, otherwise, file does not exist anymore
*
*
*        IF <File>-type  <> file-type  or
*           <File>-len   <> file-len   or
*           <File>-owner <> file-owner or
*           <File>-mtime <> file-mtime or
*           <File>-mode  <> file-mode  or
*           <File>-errno <> file-errno.
*
*           file-CHANGED = ABAP_TRUE.
*        ENDIF.
*        file-STATUS = ABAP_TRUE.
*
*        MOVE-CORRESPONDING file TO <File>.
*
*      ELSE.
*        "New File
*        file-STATUS = ABAP_TRUE.
*        MOVE-CORRESPONDING file TO file_list.
*
**       * Does the filename contains the requested pattern?
**       * Then store it, else forget it.
*        if a_must_cs = no_cs.
*          append file_list.
*        else.
*          if file-name cs a_must_cs.
*            append file_list.
*          endif.
*        endif.
*
*      ENDIF.
*    ELSE.

    "Build a new list
    file-status = abap_true.
    MOVE-CORRESPONDING file TO file_list.

*     * Does the filename contains the requested pattern?
*     * Then store it, else forget it.
    IF a_must_cs = no_cs.
      APPEND file_list.
    ELSE.
      IF file-name CS a_must_cs.
        APPEND file_list.
      ENDIF.
    ENDIF.

*    ENDIF.

  ENDDO.

  "Remove all files which does not exist anymore
  IF a_operation EQ c_file_list_update.
    DELETE file_list WHERE status IS INITIAL.

    LOOP AT file_list ASSIGNING <file>.
      <file>-status = abap_false.
    ENDLOOP.
  ENDIF.

  CALL 'C_DIR_READ_FINISH'
      ID 'ERRNO'  FIELD file_list-errno
      ID 'ERRMSG' FIELD file_list-errmsg.
*  if sy-subrc <> 0.
*    write: / 'C_DIR_READ_FINISH', 'SUBRC', sy-subrc.
*  endif.

  DELETE file_list WHERE type = 'directory'.

  IF srt = 'T'.
    SORT file_list BY mtime DESCENDING name ASCENDING.
  ELSE.
    SORT file_list BY name ASCENDING mtime DESCENDING.
  ENDIF.

  cht_file[] = file_list[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILENAME_USEABLE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> FILE_NAME
*&      <-- FILE_USEABLE
*&---------------------------------------------------------------------*
FORM filename_useable  USING    p_name    TYPE filename_al11
                      CHANGING p_useable TYPE abap_bool.

  IF p_name(4) = 'core'.
    p_useable = abap_false.
  ELSE.
    p_useable = abap_true.
  ENDIF.

ENDFORM.
