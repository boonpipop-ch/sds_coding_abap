FUNCTION Z_SDSFI_DOWNLOAD_TEXTFILE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_PATH_PC) TYPE  STRING OPTIONAL
*"     REFERENCE(IM_PATH_SERVER) TYPE  STRING OPTIONAL
*"     REFERENCE(IM_TRANSFER_TO) TYPE  FLAG DEFAULT 'F'
*"     REFERENCE(IM_EXE_TYPE) TYPE  CHAR1
*"  EXPORTING
*"     REFERENCE(EX_RETURN_PC) TYPE  CHAR300
*"     REFERENCE(EX_RETURN_SERVER) TYPE  CHAR300
*"     REFERENCE(EX_FILENAME) TYPE  CHAR100
*"     REFERENCE(EX_TRANS_NO) TYPE  CHAR17
*"     REFERENCE(EX_RETURN_TF) TYPE  ZSDSFIS051
*"     REFERENCE(EX_FTP_FULLPATH)
*"     REFERENCE(EX_ARC_FULLPATH)
*"  TABLES
*"      IT_ETAX_TEXTFILE TYPE  ZSDSFIS033_TT
*"----------------------------------------------------------------------
  DATA : LT_TEXT_OUT    TYPE TRUXS_T_TEXT_DATA.

  DATA : LV_PATH_PC      TYPE STRING,
         LV_PATH_SERVER  TYPE STRING,
         LV_FTP_FULLPATH TYPE STRING,
         LV_ARC_FULLPATH TYPE STRING.

  DATA:  LWA_MSG           TYPE ZSDSFIS034.

  DEFINE M_SET_RETURN_TF.
    EX_RETURN_TF-MSGTY   = &1.
    EX_RETURN_TF-MESSAGE = &2.
  END-OF-DEFINITION.

  LV_PATH_PC     = IM_PATH_PC.
  LV_PATH_SERVER = IM_PATH_SERVER.

  CONDENSE: LV_PATH_PC     NO-GAPS,
            LV_PATH_SERVER NO-GAPS.

  IF LV_PATH_SERVER IS INITIAL.
    M_SET_RETURN_TF: 'E' 'File path is required'.
    RAISE FILEPATH_NOT_SUPPIED.
  ENDIF.

  IF IM_TRANSFER_TO NE 'F'.
    M_SET_RETURN_TF: 'E' 'Transfer to must be ''F'''.
    RAISE ERROR.
  ENDIF.

  PERFORM BUILD_TEXT_OUT TABLES IT_ETAX_TEXTFILE
                       CHANGING LT_TEXT_OUT.

  IF LT_TEXT_OUT[] IS INITIAL.
    M_SET_RETURN_TF: 'E' 'Internal error, unable to build text out.'.
    EXIT.
  ENDIF.

  IF LV_PATH_SERVER IS NOT INITIAL.

    PERFORM DOWNLOAD_TO_SERVER USING IM_PATH_SERVER
                                     IM_EXE_TYPE
                                     LT_TEXT_OUT
                            CHANGING EX_RETURN_SERVER
                                     EX_FILENAME
                                     EX_TRANS_NO
                                     EX_RETURN_TF.

    IF EX_RETURN_TF-MSGTY EQ 'E'.
*     Could not write file(AL11)
      EXIT.
    ENDIF.

*   Transfer file to FTP Server
    CASE IM_TRANSFER_TO.
      WHEN 'F'. "FTP server
        PERFORM TRANSFER_TO_FTP USING EX_RETURN_SERVER
                             CHANGING LV_FTP_FULLPATH
                                      LV_ARC_FULLPATH
                                      LWA_MSG.
        IF LWA_MSG-MSGTY EQ 'S'.
*         OK
          EX_FTP_FULLPATH      = LV_FTP_FULLPATH.
          EX_ARC_FULLPATH      = LV_ARC_FULLPATH.
          EX_RETURN_TF-MSGTY   = LWA_MSG-MSGTY.
          EX_RETURN_TF-MESSAGE = LWA_MSG-MESSAGE.

        ELSE.
          EX_RETURN_TF-MSGTY   = LWA_MSG-MSGTY.
          EX_RETURN_TF-MESSAGE = LWA_MSG-MESSAGE.
          RAISE TRANSFER_TO_FTP_FAILED.

        ENDIF.

      WHEN OTHERS.

    ENDCASE.

*   start moving text file from OUT to ARCHIVE
    PERFORM ARCHIVE_FILE USING EX_RETURN_SERVER.

  ENDIF.




ENDFUNCTION.
