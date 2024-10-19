*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 13.08.2018 at 14:20:41 by user NO_OTOCHON
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZVFI_0075.......................................*
TABLES: zvfi_0075, *zvfi_0075. "view work areas
CONTROLS: tctrl_zvfi_0075
TYPE TABLEVIEW USING SCREEN '9000'.
DATA: BEGIN OF status_zvfi_0075. "state vector
    INCLUDE STRUCTURE vimstatus.
DATA: END OF status_zvfi_0075.
* Table for entries selected to show on screen
DATA: BEGIN OF zvfi_0075_extract OCCURS 0010.
    INCLUDE STRUCTURE zvfi_0075.
    INCLUDE STRUCTURE vimflagtab.
DATA: END OF zvfi_0075_extract.
* Table for all entries loaded from database
DATA: BEGIN OF zvfi_0075_total OCCURS 0010.
    INCLUDE STRUCTURE zvfi_0075.
    INCLUDE STRUCTURE vimflagtab.
DATA: END OF zvfi_0075_total.

*.........table declarations:.................................*
TABLES: ztfi_0075                      .
