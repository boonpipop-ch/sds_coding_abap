*-----------------------------------------------------------------------
*  Program ID         : ZSDSMMR0320
*  Creation Date      : 22.05.2024
*  Author             : Boonpipop Ch.
*  Add-on ID          : ZMMF009
*  Description        : Goods receipt slip from
*  Purpose            : N/A
*  Copied from        : ZSAPM07DR_GR (ECC)
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------

INCLUDE: M07DRTOP.                   "Tabellen und Datendeklarationen

*INCLUDE: M07DRTOP_PDF.        -> moved to M07DRTOP             "1098706

INCLUDE: M07DRMTA.                   "Zugriffsroutinen für ATAB-Tabellen

INCLUDE: M07DRMMA.                   "Zugriffsroutinen Material

INCLUDE: M07DRMBE.                   "Zugriffsroutine Bestellung

INCLUDE: M07DRMFA.                   "Zugriffsroutine FauftrKopf

INCLUDE: M07DRKON.                   "Zugriffsroutine Kontierung

INCLUDE: M07DRF01.                   "Druck WE Schein  FertAuftr. Vers1

INCLUDE: M07DRF01_PDF.               "Druck WE Schein  FertAuftr. Vers1 (PDF)

INCLUDE: M07DRF02.                   "Druck WE Schein  FertAuftr. Vers2

INCLUDE: M07DRF02_PDF.               "Druck WE Schein  FertAuftr. Vers2 (PDF)

INCLUDE: M07DRE01.                   "Druck Wareneingangsschein Vers.1

INCLUDE: M07DRE01_PDF.               "Druck Wareneingangsschein Vers.1 (PDF)

INCLUDE: M07DRE02.                   "Druck Wareneingangsschein Vers.2

INCLUDE: M07DRE02_PDF.               "Druck Wareneingangsschein Vers.1 (PDF)

*INCLUDE: M07DRE03.                   "Druck Wareneingangsschein Vers.3
INCLUDE: ZSDSMMI0010.

INCLUDE: M07DRA01.                   "Druck Warenausgangsschein Vers.1

INCLUDE: M07DRA01_PDF.               "Druck Warenausgangsschein Vers.1 (PDF)

INCLUDE: M07DRA02.                   "Druck Warenausgangsschein Vers.2

INCLUDE: M07DRA02_PDF.               "Druck Warenausgangsschein Vers.1 (PDF)

INCLUDE: M07DRA03.                   "Druck Warenausgangsschein Vers.3

INCLUDE: M07DRLB3.                   "Druck Warenausg.schein LB Vers.3

INCLUDE: M07DRETI.                   "Druck Etiketten Warenausgang

INCLUDE: M07DRETI_PDF.               "Druck Etiketten Warenausgang (PDF)

INCLUDE: M07DRKTO.                   "Druck Mehrfachkontierungsblatt

INCLUDE: M07DRKTO_PDF.               "Druck Mehrfachkontierungsblatt (PDF)

INCLUDE: M07DRENT.                   "Entries für Druck

INCLUDE: M07DRENT_PDF.               "Entries für Druck (PDF)

INCLUDE: M07DRLOB.                   "Leseroutine Adresse LB-Lieferant

INCLUDE: M07DRAUS.                   "Ausgaberoutinen

INCLUDE: M07DRAUS_PDF.               "Ausgaberoutinen (PDF)

INCLUDE: M07DRSON.                   "sonstige Routinen

INCLUDE: M07DRSON_PDF.               "sonstige Routinen (PDF)

* eject
