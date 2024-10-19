*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 29.08.2018 at 11:29:16 by user NO_OTOCHON
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTFI_0074.......................................*
DATA:  BEGIN OF status_ztfi_0074                     .   "state vector
    INCLUDE STRUCTURE vimstatus.
DATA:  END OF status_ztfi_0074                     .
CONTROLS: tctrl_ztfi_0074
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ztfi_0074                     .
TABLES: ztfi_0074                      .

* general table data declarations..............
INCLUDE lsvimtdt                                .
