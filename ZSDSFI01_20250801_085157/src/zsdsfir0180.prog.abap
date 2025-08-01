*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0180
*  Creation Date      : 26.04.2024
*  Author             : Jakarin S
*  Add-on ID          :
*  Description        : Electronic Tax Invoice Program
*  Purpose            :
*  Copied from        : ZETX001
*  Restriction        :
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

REPORT ZSDSFIR0180.
INCLUDE: ZSDSFIR0180_topz,
         ZSDSFIR0180_s01,
         ZSDSFIR0180_f01,
         ZSDSFIR0180_f02,
         ZSDSFIR0180_f03,
         ZSDSFIR0180_f04.

*&--------------------------------------------------------------------*
*& INITIALIZATION
*&--------------------------------------------------------------------*
INITIALIZATION.
  PERFORM initialization.

*&--------------------------------------------------------------------*
*& S T A R T - O F - S E L E C T I O N
*&--------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM start.

*&--------------------------------------------------------------------*
*& E N D - O F - S E L E C T I O N
*&--------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM end.
