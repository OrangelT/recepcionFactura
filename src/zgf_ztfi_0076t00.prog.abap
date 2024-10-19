*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 26.07.2018 at 15:39:36 by user NO_OTOCHON
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTFI_0076.......................................*
DATA:  BEGIN OF status_ztfi_0076                     .   "state vector
    INCLUDE STRUCTURE vimstatus.
DATA:  END OF status_ztfi_0076                     .
CONTROLS: tctrl_ztfi_0076
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ztfi_0076                     .
TABLES: ztfi_0076                      .

* general table data declarations..............
INCLUDE lsvimtdt                                .
