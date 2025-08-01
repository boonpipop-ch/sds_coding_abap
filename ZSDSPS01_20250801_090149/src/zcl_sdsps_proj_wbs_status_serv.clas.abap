class ZCL_SDSPS_PROJ_WBS_STATUS_SERV definition
  public
  inheriting from ZCL_SDSCA_REST_SERVICE
  final
  create public .

public section.

  methods GET_STATUS
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SDSPS_PROJ_WBS_STATUS_SERV IMPLEMENTATION.


METHOD GET_STATUS.
*-----------------------------------------------------------------------
*  Program ID         : ZCL_SDSPS_PROJ_WBS_STATUS_SERV/ GET_STATUS
*  Creation Date      : 26.06.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : PSI010
*  Description        : Redefine method GET_STATUS
*                       to read processing status from Response
*                       and update it to EF_STATUS and EF_MESSAGE
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
  DATA: LF_OBJECT TYPE CHAR50 VALUE 'OBJECT'.
  DATA: LT_RESPONSE TYPE ZSDSPSS010_TT.

  DATA:
    LF_FNAME  TYPE  CHAR50.

  FIELD-SYMBOLS:
    <L_FIELD>    TYPE  ANY,
    <L_RESPONSE> TYPE ZSDSPSS010.

* Initialize Output
  CLEAR: EF_STATUS,
         EF_MESSAGE.

* Assign Result
  CONCATENATE 'IS_DATA-' LF_OBJECT
         INTO LF_FNAME.
  ASSIGN (LF_FNAME) TO <L_FIELD>.
  IF SY-SUBRC EQ 0.
    LT_RESPONSE = <L_FIELD>.
  ENDIF.

* Capture the first error from response data
  READ TABLE LT_RESPONSE ASSIGNING <L_RESPONSE>
                         WITH KEY RESP_STATUS ='E'.
  IF SY-SUBRC EQ 0.
    EF_STATUS = <L_RESPONSE>-RESP_STATUS.
    EF_MESSAGE = <L_RESPONSE>-RESP_MESSAGE.
  ELSE.
*   If no any record in error, then capture the first success message
    READ TABLE LT_RESPONSE ASSIGNING <L_RESPONSE>
                          WITH KEY RESP_STATUS ='S'.
    IF SY-SUBRC EQ 0.
      EF_STATUS = <L_RESPONSE>-RESP_STATUS.
      EF_MESSAGE = <L_RESPONSE>-RESP_MESSAGE.
    ENDIF.
  ENDIF.

ENDMETHOD.
ENDCLASS.
