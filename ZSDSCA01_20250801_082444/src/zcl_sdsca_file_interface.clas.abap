class ZCL_SDSCA_FILE_INTERFACE definition
  public
  final
  create public .

public section.

  types TS_DATABIN type SOLIX .
  types:
    TT_DATABIN TYPE STANDARD TABLE OF TS_DATABIN .
  types TS_DATATXT type STRING .
  types:
    TT_DATATXT TYPE STANDARD TABLE OF TS_DATATXT .

  class-methods SPLIT_PATH_AND_NAME
    importing
      !IF_FULLNAME type STRING
    exporting
      !EF_FILEPATH type STRING
      !EF_FILENAME type STRING .
  class-methods CREATE_INTERFACE_FILE
    importing
      !IF_INTFNO type ZSDSDE_INTFNO
      !IF_FILENAME type STRING
      !IF_PARAM1 type STRING optional
      !IF_PARAM2 type STRING optional
      !IF_PARAM3 type STRING optional
      !IT_DATATXT type TT_DATATXT optional
      !IT_DATABIN type TT_DATABIN optional
    exporting
      !ES_RETURN type BAPIRET2
      !EF_FILEPATH type STRING
      !EF_FILENAME type STRING
      !EF_FULLNAME type STRING .
  class-methods GET_FILE_PATH
    importing
      !IF_INTFNO type ZSDSDE_INTFNO
      !IF_FILENAME type STRING default 'DUMMY.TXT'
      !IF_PARAM1 type STRING optional
      !IF_PARAM2 type STRING optional
      !IF_PARAM3 type STRING optional
    exporting
      !EF_FILEPATH type STRING
      !EF_FILENAME type STRING
      !EF_FULLNAME type STRING
      !ES_RETURN type BAPIRET2 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SDSCA_FILE_INTERFACE IMPLEMENTATION.


METHOD CREATE_INTERFACE_FILE.

  DATA:
    LF_FILEPATH   TYPE  STRING,
    LF_FILENAME   TYPE  STRING,
    LF_FULLNAME   TYPE  STRING,
    LF_MODE       TYPE  CHAR5,
    LF_ERROR      TYPE  XFLAG.


* Initialize Output
  CLEAR: ES_RETURN,
         EF_FILEPATH,
         EF_FILENAME,
         EF_FULLNAME.

* Get Interface File Info
  GET_FILE_PATH(
    EXPORTING
      IF_INTFNO   = IF_INTFNO
      IF_FILENAME = IF_FILENAME
      IF_PARAM1   = IF_PARAM1
      IF_PARAM2   = IF_PARAM2
      IF_PARAM3   = IF_PARAM3
    IMPORTING
      ES_RETURN   = ES_RETURN
      EF_FILEPATH = LF_FILEPATH
      EF_FILENAME = LF_FILENAME
      EF_FULLNAME = LF_FULLNAME ).
  IF ES_RETURN IS NOT INITIAL.
    RETURN.
  ENDIF.

* Determine Mode
  IF IT_DATATXT IS SUPPLIED.
    LF_MODE = 'TXT'.
  ELSEIF IT_DATABIN IS SUPPLIED.
    LF_MODE = 'BIN'.
  ELSE.
*   Error: Cannot find input data for file creation.
    ES_RETURN-TYPE       = 'E'.
    ES_RETURN-ID         = 'ZSDSCA01'.
    ES_RETURN-NUMBER     = '008'.
    MESSAGE ID ES_RETURN-ID TYPE ES_RETURN-TYPE
            NUMBER ES_RETURN-NUMBER
            WITH ES_RETURN-MESSAGE_V1 ES_RETURN-MESSAGE_V2
                 ES_RETURN-MESSAGE_V3 ES_RETURN-MESSAGE_V4
            INTO ES_RETURN-MESSAGE.
    RETURN.
  ENDIF.

* ---------------------------------
* Create Application Server file
* ---------------------------------
  DO 1 TIMES.
    CLEAR: LF_ERROR.
    TRY.
*       --------------------
*       Create Text File
*       --------------------
        IF LF_MODE EQ 'TXT'.
          OPEN DATASET LF_FULLNAME FOR OUTPUT IN TEXT MODE ENCODING DEFAULT
                                   IGNORING CONVERSION ERRORS.
          IF SY-SUBRC NE 0.
            LF_ERROR = 'X'.
            EXIT.
          ENDIF.
          LOOP AT IT_DATATXT ASSIGNING FIELD-SYMBOL(<L_DATATXT>).
            TRANSFER <L_DATATXT> TO LF_FULLNAME.
          ENDLOOP.
          CLOSE DATASET LF_FULLNAME.
          IF SY-SUBRC NE 0.
            LF_ERROR = 'X'.
            EXIT.
          ENDIF.

*       --------------------
*       Create Binary File
*       --------------------
        ELSEIF LF_MODE EQ 'BIN'.
          OPEN DATASET LF_FULLNAME FOR OUTPUT IN BINARY MODE.
          IF SY-SUBRC NE 0.
            LF_ERROR = 'X'.
            EXIT.
          ENDIF.
          LOOP AT IT_DATABIN ASSIGNING FIELD-SYMBOL(<L_DATABIN>).
            TRANSFER <L_DATABIN> TO LF_FULLNAME.
          ENDLOOP.
          CLOSE DATASET LF_FULLNAME.
          IF SY-SUBRC NE 0.
            LF_ERROR = 'X'.
            EXIT.
          ENDIF.

        ENDIF.

      CATCH CX_ROOT INTO DATA(LREF_ERROR) ##NEEDED ##CATCH_ALL.
        LF_ERROR = 'X'.
        EXIT.
    ENDTRY.

  ENDDO.
* Error During File creation
  IF LF_ERROR EQ 'X'.
*   Error: Error during creating file on application server.
    ES_RETURN-TYPE       = 'E'.
    ES_RETURN-ID         = 'ZSDSCA01'.
    ES_RETURN-NUMBER     = '009'.
    MESSAGE ID ES_RETURN-ID TYPE ES_RETURN-TYPE
            NUMBER ES_RETURN-NUMBER
            WITH ES_RETURN-MESSAGE_V1 ES_RETURN-MESSAGE_V2
                 ES_RETURN-MESSAGE_V3 ES_RETURN-MESSAGE_V4
            INTO ES_RETURN-MESSAGE.
    RETURN.
  ENDIF.

* Assign Result
* Message: Interface File is created successfully.
  ES_RETURN-TYPE       = 'S'.
  ES_RETURN-ID         = 'ZSDSCA01'.
  ES_RETURN-NUMBER     = '010'.
  MESSAGE ID ES_RETURN-ID TYPE ES_RETURN-TYPE
          NUMBER ES_RETURN-NUMBER
          WITH ES_RETURN-MESSAGE_V1 ES_RETURN-MESSAGE_V2
               ES_RETURN-MESSAGE_V3 ES_RETURN-MESSAGE_V4
          INTO ES_RETURN-MESSAGE.

* Assign Output
  EF_FULLNAME = LF_FULLNAME.
  EF_FILEPATH = LF_FILEPATH.
  EF_FILENAME = LF_FILENAME.

ENDMETHOD.


METHOD GET_FILE_PATH.

  CONSTANTS:
    LC_INBOUND  TYPE  TEXT20 VALUE '10_INBOUND',
    LC_OUTBOUND TYPE  TEXT20 VALUE '20_OUTBOUND'.

  DATA:
    LF_FULLNAME   TYPE  STRING,
    LF_FILENAME_I TYPE  STRING.


* Initialize Output
  CLEAR: ES_RETURN,
         EF_FILEPATH,
         EF_FILENAME,
         EF_FULLNAME.

* Get Interface Setting
  SELECT SINGLE *
    FROM ZSDSCAC004
   WHERE INTFNO   EQ @IF_INTFNO
     AND SYSID    EQ @SY-SYSID
     AND ZDEL_FLG EQ @SPACE
    INTO @DATA(LS_SETTING).
  IF SY-SUBRC NE 0.
    SELECT SINGLE *
      FROM ZSDSCAC004
     WHERE INTFNO   EQ @IF_INTFNO
       AND SYSID    EQ @SPACE
       AND ZDEL_FLG EQ @SPACE
      INTO @LS_SETTING.
  ENDIF.
  IF SY-SUBRC NE 0.
*   Error: Invalid Interface ID &1. Please check setting in table ZSDSCAC004.
    ES_RETURN-TYPE       = 'E'.
    ES_RETURN-ID         = 'ZSDSCA01'.
    ES_RETURN-NUMBER     = '006'.
    ES_RETURN-MESSAGE_V1 = IF_INTFNO.
    MESSAGE ID ES_RETURN-ID TYPE ES_RETURN-TYPE
            NUMBER ES_RETURN-NUMBER
            WITH ES_RETURN-MESSAGE_V1 ES_RETURN-MESSAGE_V2
                 ES_RETURN-MESSAGE_V3 ES_RETURN-MESSAGE_V4
            INTO ES_RETURN-MESSAGE.
    RETURN.
  ENDIF.

* Get File path from Setting
  CLEAR LF_FULLNAME.
  IF LS_SETTING-AUTOSUBFOLDER IS INITIAL.
    LF_FILENAME_I = IF_FILENAME.
  ELSE.
    IF LS_SETTING-INOUT_FLAG IS INITIAL OR
       LS_SETTING-SYSNAME    IS INITIAL OR
       LS_SETTING-WRICEF     IS INITIAL.
*     Error: Invalid setting for auto subfolder in Interface ID &1.
      ES_RETURN-TYPE       = 'E'.
      ES_RETURN-ID         = 'ZSDSCA01'.
      ES_RETURN-NUMBER     = '011'.
      ES_RETURN-MESSAGE_V1 = IF_INTFNO.
      MESSAGE ID ES_RETURN-ID TYPE ES_RETURN-TYPE
              NUMBER ES_RETURN-NUMBER
              WITH ES_RETURN-MESSAGE_V1 ES_RETURN-MESSAGE_V2
                   ES_RETURN-MESSAGE_V3 ES_RETURN-MESSAGE_V4
              INTO ES_RETURN-MESSAGE.
      RETURN.
    ENDIF.
    CASE LS_SETTING-INOUT_FLAG.
      WHEN 'I'.
        LF_FILENAME_I = LC_INBOUND.
      WHEN 'O'.
        LF_FILENAME_I = LC_OUTBOUND.
    ENDCASE.
    LF_FILENAME_I = LF_FILENAME_I      && '/' &&
                    LS_SETTING-SYSNAME && '/' &&
                    LS_SETTING-WRICEF  && '/' &&
                    IF_FILENAME.
  ENDIF.

  CALL FUNCTION 'FILE_GET_NAME_USING_PATH'
    EXPORTING
      LOGICAL_PATH               = LS_SETTING-PATHINTERN
      OPERATING_SYSTEM           = SY-OPSYS
      PARAMETER_1                = IF_PARAM1
      PARAMETER_2                = IF_PARAM2
      PARAMETER_3                = IF_PARAM3
      FILE_NAME                  = LF_FILENAME_I
      ELEMINATE_BLANKS           = 'X'
    IMPORTING
      FILE_NAME_WITH_PATH        = LF_FULLNAME
    EXCEPTIONS
      PATH_NOT_FOUND             = 1
      MISSING_PARAMETER          = 2
      OPERATING_SYSTEM_NOT_FOUND = 3
      FILE_SYSTEM_NOT_FOUND      = 4
      OTHERS                     = 5.
  IF SY-SUBRC <> 0.
*   Error: Error during get interface file path from Logical path &1.
    ES_RETURN-TYPE       = 'E'.
    ES_RETURN-ID         = 'ZSDSCA01'.
    ES_RETURN-NUMBER     = '007'.
    ES_RETURN-MESSAGE_V1 = LS_SETTING-PATHINTERN.
    MESSAGE ID ES_RETURN-ID TYPE ES_RETURN-TYPE
            NUMBER ES_RETURN-NUMBER
            WITH ES_RETURN-MESSAGE_V1 ES_RETURN-MESSAGE_V2
                 ES_RETURN-MESSAGE_V3 ES_RETURN-MESSAGE_V4
            INTO ES_RETURN-MESSAGE.
    RETURN.
  ENDIF.

* Assign Output
  EF_FULLNAME = LF_FULLNAME.
  IF EF_FILEPATH IS SUPPLIED OR
     EF_FILENAME IS SUPPLIED.
    SPLIT_PATH_AND_NAME( EXPORTING IF_FULLNAME = EF_FULLNAME
                         IMPORTING EF_FILEPATH = EF_FILEPATH
                                   EF_FILENAME = EF_FILENAME ).
  ENDIF.

ENDMETHOD.


METHOD SPLIT_PATH_AND_NAME.

* Initialize Output
  CLEAR: EF_FILEPATH,
         EF_FILENAME.

  CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
    EXPORTING
      FULL_NAME     = IF_FULLNAME
    IMPORTING
      STRIPPED_NAME = EF_FILENAME
      FILE_PATH     = EF_FILEPATH
    EXCEPTIONS
      X_ERROR       = 1
      OTHERS        = 2.
  IF SY-SUBRC <> 0.
    RETURN.
  ENDIF.

*  DATA:
*    LT_TXT  TYPE  STANDARD TABLE OF STRING.
*
*  DATA:
*    LF_SEP  TYPE  CHAR1.
*
*
** get path/file separator
*  CL_GUI_FRONTEND_SERVICES=>GET_FILE_SEPARATOR(
*    CHANGING
*      FILE_SEPARATOR       = LF_SEP
*    EXCEPTIONS
*      CNTL_ERROR           = 1
*      ERROR_NO_GUI         = 2
*      NOT_SUPPORTED_BY_GUI = 3
*      OTHERS               = 99
*      ).
*  IF SY-SUBRC NE 0.
*    RETURN.
*  ENDIF.
*
*  BREAK-POINT.
*  SPLIT IF_FULLNAME AT LF_SEP INTO TABLE LT_TXT.
*
*  LOOP AT LT_TXT ASSIGNING FIELD-SYMBOL(<L_TXT>).
*    AT LAST.
*      EF_FILENAME = <L_TXT>.
*    ENDAT.
*
*  ENDLOOP.

ENDMETHOD.
ENDCLASS.
