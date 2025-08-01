*&---------------------------------------------------------------------*
*& Report ZDEMO_UNRELEASE_TR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZDEMO_UNRELEASE_TR.

tables: e070,vbrp,sscrfields.



selection-screen begin of block main with frame.
*  SELECT-options s_ss for vbrp-vkbur.
  parameter s_trkorr like e070-trkorr .
selection-screen end of block main.

*INITIALIZATION.
*CONCATENATE '@39@' ' SIMULATE' INTO SSCRFIELDS-FUNCTXT_01.

update e070 set trstatus = 'D'
where trkorr = s_trkorr.
