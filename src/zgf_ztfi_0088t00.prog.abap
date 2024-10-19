*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 21.08.2018 at 17:57:16 by user NO_OTOCHON
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTFI_0088.......................................*
DATA:  BEGIN OF status_ztfi_0088                     .   "state vector
    INCLUDE STRUCTURE vimstatus.
DATA:  END OF status_ztfi_0088                     .
CONTROLS: tctrl_ztfi_0088
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ztfi_0088                     .
TABLES: ztfi_0088                      .

* general table data declarations..............
INCLUDE lsvimtdt                                .
