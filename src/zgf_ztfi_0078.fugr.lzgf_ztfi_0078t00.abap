*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTFI_0078.......................................*
DATA:  BEGIN OF STATUS_ZTFI_0078                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTFI_0078                     .
CONTROLS: TCTRL_ZTFI_0078
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ZTFI_0078                     .
TABLES: ZTFI_0078                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
