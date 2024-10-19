*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTFI_0085.......................................*
DATA:  BEGIN OF STATUS_ZTFI_0085                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTFI_0085                     .
CONTROLS: TCTRL_ZTFI_0085
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ZTFI_0085                     .
TABLES: ZTFI_0085                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
