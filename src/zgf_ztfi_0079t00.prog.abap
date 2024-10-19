*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 26.07.2018 at 15:48:37 by user NO_OTOCHON
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTFI_0079.......................................*
DATA:  BEGIN OF status_ztfi_0079                     .   "state vector
    INCLUDE STRUCTURE vimstatus.
DATA:  END OF status_ztfi_0079                     .
CONTROLS: tctrl_ztfi_0079
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ztfi_0079                     .
TABLES: ztfi_0079                      .

* general table data declarations..............
INCLUDE lsvimtdt                                .
