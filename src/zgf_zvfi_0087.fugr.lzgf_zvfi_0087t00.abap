*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZVFI_0087.......................................*
TABLES: ZVFI_0087, *ZVFI_0087. "view work areas
CONTROLS: TCTRL_ZVFI_0087
TYPE TABLEVIEW USING SCREEN '9000'.
DATA: BEGIN OF STATUS_ZVFI_0087. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZVFI_0087.
* Table for entries selected to show on screen
DATA: BEGIN OF ZVFI_0087_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZVFI_0087.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVFI_0087_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZVFI_0087_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZVFI_0087.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVFI_0087_TOTAL.

*.........table declarations:.................................*
TABLES: ZTFI_0086                      .
TABLES: ZTFI_0087                      .
