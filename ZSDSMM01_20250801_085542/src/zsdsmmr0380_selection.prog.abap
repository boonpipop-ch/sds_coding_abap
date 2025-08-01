*&---------------------------------------------------------------------*
*& Include          ZSDSMMR0380_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : S_MATKL FOR MARA-MATKL MEMORY ID MKL,
                   S_MATNR FOR MARA-MATNR MEMORY ID MAT,
                   S_IDNRK FOR STPO-IDNRK,
                   S_ERNAM FOR MARA-ERNAM,
                   S_ERSDA FOR MARA-ERSDA,
                   S_MTART FOR MARA-MTART MEMORY ID MTA,
                   S_AENAM FOR MAST-AENAM,
                   S_AEDAT FOR MAST-AEDAT,
                   S_STLAL FOR MAST-STLAL,
                   S_STLNR FOR MAST-STLNR,
                   S_STLAN FOR MAST-STLAN MEMORY ID CSV,
                   S_STLKN FOR STPO-STLKN.
SELECTION-SCREEN END OF BLOCK BLOCK1.
SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-002.
  PARAMETERS P_LAYOUT TYPE SLIS_VARI MODIF ID LAY.
SELECTION-SCREEN END OF BLOCK BLOCK2.
