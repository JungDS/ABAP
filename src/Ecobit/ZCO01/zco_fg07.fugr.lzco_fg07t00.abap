*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2021.08.31 at 16:45:41
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZCOV000.........................................*
TABLES: ZCOV000, *ZCOV000. "view work areas
CONTROLS: TCTRL_ZCOV000
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZCOV000. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZCOV000.
* Table for entries selected to show on screen
DATA: BEGIN OF ZCOV000_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZCOV000.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZCOV000_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZCOV000_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZCOV000.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZCOV000_TOTAL.

*.........table declarations:.................................*
TABLES: T001                           .
TABLES: ZCOT000                        .
