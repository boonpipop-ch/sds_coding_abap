*-----------------------------------------------------------------------
*  Program ID         : <<Program Name>>
*  Creation Date      : <<DD.MM.YYYY>>
*  Author             : <<Author Name>>
*  Add-on ID          : <<Refer WRICEF List)
*  Description        : <<Description of program>>
*  Purpose            :
*  Copied from        : <<Reference Program>>
*  Restriction        :
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
*REPORT XXXXX
*&---------------------------------------------------------------------*
*& Include          ZXTRKU07
*&---------------------------------------------------------------------*
DATA: l_idoc_data TYPE edidd,
      l_e1edl20   TYPE e1edl20,
      l_e1txth8   TYPE e1txth8,
      l_e1txtp8   TYPE e1txtp8,
      l_delivery_com TYPE komdlgn.

LOOP AT idoc_data INTO l_idoc_data.
  CASE l_idoc_data-segnam.
    WHEN 'E1EDL20'.
      l_e1edl20 = l_idoc_data-sdata.
    WHEN 'E1TXTH8'.
      l_e1txth8 = l_idoc_data-sdata.
    WHEN 'E1TXTP8'.
      IF l_e1txth8-tdid = 'X010'.      " Truck No
        l_e1txtp8 = l_idoc_data-sdata.
      ENDIF.
  ENDCASE.

ENDLOOP.


LOOP AT delivery_com INTO l_delivery_com.
  IF NOT l_e1edl20-lifex IS INITIAL.
    l_delivery_com-lifex = l_e1edl20-lifex.
  ENDIF.

  l_delivery_com-zwadat_ist = l_e1edl20-podat.
  l_delivery_com-zwatim_ist = l_e1edl20-potim.
  l_delivery_com-ztruckno   = l_e1txtp8-tdline.

  MODIFY delivery_com FROM l_delivery_com.

ENDLOOP.
