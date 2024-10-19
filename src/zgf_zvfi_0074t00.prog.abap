*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 26.07.2018 at 16:39:55 by user NO_OTOCHON
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZVFI_0074.......................................*
TABLES: ZVFI_0074, *ZVFI_0074. "view work areas
CONTROLS: TCTRL_ZVFI_0074
TYPE TABLEVIEW USING SCREEN '9000'.
DATA: BEGIN OF STATUS_ZVFI_0074. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZVFI_0074.
* Table for entries selected to show on screen
DATA: BEGIN OF ZVFI_0074_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZVFI_0074.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVFI_0074_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZVFI_0074_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZVFI_0074.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVFI_0074_TOTAL.

*.........table declarations:.................................*
TABLES: ZTFI_0074                      .
