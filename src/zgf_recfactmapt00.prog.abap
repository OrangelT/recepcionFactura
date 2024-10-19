*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 27.07.2018 at 12:02:22 by user NO_OTOCHON
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZCB_RECFACTMAP..................................*
DATA:  BEGIN OF status_zcb_recfactmap                .   "state vector
    INCLUDE STRUCTURE vimstatus.
DATA:  END OF status_zcb_recfactmap                .
CONTROLS: tctrl_zcb_recfactmap TYPE TABLEVIEW USING SCREEN '9000'.

*...processing: ZCB_RECFACTMAP2.................................*
DATA:  BEGIN OF status_zcb_recfactmap2               .   "state vector
    INCLUDE STRUCTURE vimstatus.
DATA:  END OF status_zcb_recfactmap2               .
CONTROLS: tctrl_zcb_recfactmap2 TYPE TABLEVIEW USING SCREEN '9100'.

*...processing: ZDT_RECFACTMAP..................................*
DATA:  BEGIN OF status_zdt_recfactmap                .   "state vector
    INCLUDE STRUCTURE vimstatus.
DATA:  END OF status_zdt_recfactmap                .
CONTROLS: tctrl_zdt_recfactmap TYPE TABLEVIEW USING SCREEN '9200'.

*...processing: ZRF_RECFACTMAP..................................*
DATA:  BEGIN OF status_zrf_recfactmap                .   "state vector
    INCLUDE STRUCTURE vimstatus.
DATA:  END OF status_zrf_recfactmap                .
CONTROLS: tctrl_zrf_recfactmap TYPE TABLEVIEW USING SCREEN '9300'.

*...processing: ZTD_RECFACTMAP..................................*
DATA:  BEGIN OF status_ztd_recfactmap                .   "state vector
    INCLUDE STRUCTURE vimstatus.
DATA:  END OF status_ztd_recfactmap                .
CONTROLS: tctrl_ztd_recfactmap TYPE TABLEVIEW USING SCREEN '9400'.

*...processing: ZDT_RECFACTMAP2..................................*
DATA:  BEGIN OF status_zdt_recfactmap2             .   "state vector
    INCLUDE STRUCTURE vimstatus.
DATA:  END OF status_zdt_recfactmap2               .
CONTROLS: tctrl_zdt_recfactmap2 TYPE TABLEVIEW USING SCREEN '9500'.

*...processing: ZIM_RECFACTMAP..................................*
DATA:  BEGIN OF status_zim_recfactmap                .   "state vector
    INCLUDE STRUCTURE vimstatus.
DATA:  END OF status_zim_recfactmap                .
CONTROLS: tctrl_zim_recfactmap TYPE TABLEVIEW USING SCREEN '9600'.

*.........table declarations:.................................*
TABLES: *zcb_recfactmap                .
TABLES: *zcb_recfactmap2               .
TABLES: *zdt_recfactmap                .
TABLES: *zrf_recfactmap                .
TABLES: *ztd_recfactmap                .
TABLES: *zdt_recfactmap2               .
TABLES: *zim_recfactmap                .
TABLES: zcb_recfactmap                 .
TABLES: zcb_recfactmap2                .
TABLES: zdt_recfactmap                 .
TABLES: zrf_recfactmap                 .
TABLES: ztd_recfactmap                 .
TABLES: zdt_recfactmap2                .
TABLES: zim_recfactmap                 .

* general table data declarations..............
INCLUDE lsvimtdt                                .
