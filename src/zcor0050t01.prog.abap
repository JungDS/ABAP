*&---------------------------------------------------------------------*
*& Include          ZCOR0050T01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES : sscrfields. "선택화면의 필드(Function Key)

TYPE-POOLS: icon, abap.
TABLES : zcot0030log. " ADD BSGSM_FCM
*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS gc_ktopl TYPE ktopl VALUE '1000'.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* VARIABLE
*----------------------------------------------------------------------*

DATA: gv_exit   TYPE xfeld,
      gv_answer TYPE c,
      ok_code   TYPE sy-ucomm,   "예외
      save_ok   TYPE sy-ucomm.   "예외

DATA: gv_change TYPE xfeld.      "변경체크

DATA gv_mode.
DATA gv_valid.

*----------------------------------------------------------------------*
* STRUCTURE
*----------------------------------------------------------------------*
DATA : gs_funtxt TYPE smp_dyntxt.
*DATA: GS_DISPLAY TYPE ZCOS0030.

"__ 리스트 조회
DATA: BEGIN OF gs_display .
        INCLUDE TYPE zcos0030b.
        DATA: msg     TYPE c LENGTH 100,
        mark(1).
DATA :    style TYPE lvc_t_styl.
DATA : cellcolor TYPE lvc_t_scol.
DATA: END OF gs_display.
INCLUDE <color>.

DATA: gv_totcnt TYPE i,    "총 아이템 수
      gv_succnt TYPE i,    "성공 건수
      gv_falcnt TYPE i,    "실패 건수
      gv_execnt TYPE i,    "실행 건수
      gv_per(4) TYPE p  DECIMALS 2.


*----------------------------------------------------------------------*
* INTERNAL TABLE
*----------------------------------------------------------------------*

DATA: gt_display     LIKE TABLE OF gs_display. "DISPLAY DATA
DATA: gt_display_log LIKE TABLE OF gs_display. "

*----------------------------------------------------------------------*
* RANGES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*
* EX) FIELD-SYMBOLS <FS_SFLIGHT> TYPE SFLIGHT.
FIELD-SYMBOLS <fs_disp> LIKE gs_display.


*----------------------------------------------------------------------*
* MACRO (Define)
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. _ : '_' 1개로 시작; Global, Local 구분 없음.

DEFINE _style_disabled.
  gs_style-fieldname = &1.
  gs_style-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT gs_style INTO TABLE gt_style.
END-OF-DEFINITION.

DEFINE _style_enabled.
  gs_style-fieldname = &1.
  gs_style-style     = cl_gui_alv_grid=>mc_style_enabled.
  INSERT gs_style INTO TABLE gt_style.
END-OF-DEFINITION.

DEFINE _conversion_in.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
  EXPORTING
  input = &1
  IMPORTING
  output = &1.
END-OF-DEFINITION.

DEFINE _conversion_out.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
  EXPORTING
  input = &1
  IMPORTING
  output = &1.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
* Table Controls
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. TC_ : Table Controls

*----------------------------------------------------------------------*
* Custom Controls
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. CC_ : Custom Controls

*----------------------------------------------------------------------*
* Tabstrip Controls
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. TS_ : Tabstrip Controls
