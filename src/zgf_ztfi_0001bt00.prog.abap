*&---------------------------------------------------------------------*
*&  Include           ZGF_ZTFI_0001BT00
*&---------------------------------------------------------------------*
*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 16.08.2018 at 14:33:03 by user NO_OTOCHON
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTFI_0001B......................................*
DATA:  BEGIN OF STATUS_ZTFI_0001B                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTFI_0001B                    .
CONTROLS: TCTRL_ZTFI_0001B
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ZTFI_0001B                    .
TABLES: ZTFI_0001B                     .
