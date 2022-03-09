*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 04/28/2020 at 07:23:57
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTINT_MONITOR...................................*
DATA:  BEGIN OF STATUS_ZTINT_MONITOR                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTINT_MONITOR                 .
CONTROLS: TCTRL_ZTINT_MONITOR
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTINT_MONITOR                 .
TABLES: ZTINT_MONITOR                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
