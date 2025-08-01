FUNCTION ZSDS_FI_MAINTAIN_LONG_TEXT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_THEAD) TYPE  THEAD
*"  EXPORTING
*"     REFERENCE(EX_TDLINE) TYPE  TDLINE
*"     REFERENCE(EX_DATA_CHANGED) TYPE  FLAG
*"----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
* 09.04.2025 F36K915466  Boontip R.   CH01(IMS 420000252)
* fix function ZSDS_FI_MAINTAIN_LONG_TEXT
* issue : long text is shift everytime
*-----------------------------------------------------------------------

  DATA: LT_TLINE TYPE TLINE_T.
  CLEAR: EX_TDLINE,
         EX_DATA_CHANGED.

  PERFORM F_INIT_GLOBAL.
  IF IM_THEAD IS INITIAL.
    RETURN.
  ENDIF.
* -- read text
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      ID                      = IM_THEAD-TDID
      LANGUAGE                = IM_THEAD-TDSPRAS
      NAME                    = IM_THEAD-TDNAME
      OBJECT                  = IM_THEAD-TDOBJECT
    TABLES
      LINES                   = LT_TLINE
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.
  IF SY-SUBRC <> 0
  AND SY-SUBRC <> 4.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 .
    RETURN.
  ENDIF.
  IF LT_TLINE[] IS NOT INITIAL.
* -- convert text from tline -> stream text table
    CALL FUNCTION 'CONVERT_ITF_TO_STREAM_TEXT'
      EXPORTING
        LANGUAGE    = IM_THEAD-TDSPRAS
      TABLES
        ITF_TEXT    = LT_TLINE
        TEXT_STREAM = GT_TEXT.
    GT_OLD_TEXT = GT_TEXT.
  ENDIF.

* -- call screen
  CALL SCREEN 100 STARTING AT 5 5
                  ENDING AT 114 30.

* -- if changed
  IF GT_OLD_TEXT[] <> GT_TEXT[] .
    CLEAR LT_TLINE.
* --- conver stream text to tline
    CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
      EXPORTING
        LANGUAGE    = IM_THEAD-TDSPRAS
      TABLES
        TEXT_STREAM = GT_TEXT
        ITF_TEXT    = LT_TLINE.
* ---if found text save text ( to buffer ) , it needs call function 'commit_text' in program
    IF LT_TLINE[] IS NOT INITIAL.
      CALL FUNCTION 'SAVE_TEXT'
        EXPORTING
          HEADER   = IM_THEAD
        TABLES
          LINES    = LT_TLINE
        EXCEPTIONS
          ID       = 1
          LANGUAGE = 2
          NAME     = 3
          OBJECT   = 4
          OTHERS   = 5.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 .
      ENDIF.
* -- if not found then -> delete text ( to buffer),it needs call function 'commit_text' in program
    ELSE.
      CALL FUNCTION 'DELETE_TEXT'
        EXPORTING
          ID        = IM_THEAD-TDID
          LANGUAGE  = IM_THEAD-TDSPRAS
          NAME      = IM_THEAD-TDNAME
          OBJECT    = IM_THEAD-TDOBJECT
        EXCEPTIONS
          NOT_FOUND = 1
          OTHERS    = 2.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 .
      ENDIF.
    ENDIF.
    READ TABLE LT_TLINE INTO DATA(LS_TLINE) INDEX 1.
    EX_TDLINE = LS_TLINE-TDLINE.
    EX_DATA_CHANGED = ABAP_TRUE.
  ENDIF.

  PERFORM F_INIT_GLOBAL.

ENDFUNCTION.
