*&---------------------------------------------------------------------*
*& Include          ZCOR0090T01
*&---------------------------------------------------------------------*
TYPE-POOLS : icon,abap.

TABLES : sscrfields, "선택화면의 필드(Function Key)
         csks,
         t001,
         tka01,
         tka02,
         tka05,
         tcurc,
         cepc,
         zcot0040.

*---------------------------------------------------------------------*
* CONSTANTS
*---------------------------------------------------------------------*
CONSTANTS : gc_lzone TYPE lzone    VALUE '0000000001',
            gc_a     TYPE char01   VALUE 'A',
            gc_s     TYPE char01   VALUE 'S',
            gc_e     TYPE char01   VALUE 'E',
            gc_x     TYPE char01   VALUE 'X',
            gc_n     TYPE char01   VALUE 'N',
            gc_tcode TYPE sy-tcode VALUE 'KS01'.

CONSTANTS gc_ktopl TYPE ktopl VALUE '1000'.

*---------------------------------------------------------------------*
* TYPES
*---------------------------------------------------------------------*

TYPES: BEGIN OF ty_cosp,
         versn  TYPE versn,
         objnr  TYPE j_objnr,
         kstar  TYPE kstar,
         kstxt  TYPE ktext,
         wkg001 TYPE wkgxxx,
         wkg002 TYPE wkgxxx,
         wkg003 TYPE wkgxxx,
         wkg004 TYPE wkgxxx,
         wkg005 TYPE wkgxxx,
         wkg006 TYPE wkgxxx,
         wkg007 TYPE wkgxxx,
         wkg008 TYPE wkgxxx,
         wkg009 TYPE wkgxxx,
         wkg010 TYPE wkgxxx,
         wkg011 TYPE wkgxxx,
         wkg012 TYPE wkgxxx,
       END OF ty_cosp.


*---------------------------------------------------------------------*
* VARIABLE
*---------------------------------------------------------------------*
* - Prefix 정의
*   1. GV_  : Global Variable
*   2. LV_  : Local Variable

*EX) DATA: GV_EXIT   TYPE C,
*          GV_ANSWER TYPE C,
*          OK_CODE   TYPE SY-UCOMM,   "예외
*          SAVE_OK   TYPE SY-UCOMM.   "예외

DATA: gv_exit TYPE c,
      ok_code TYPE sy-ucomm,   "예외
      save_ok TYPE sy-ucomm.   "예외

DATA: gv_total  TYPE c LENGTH 30,
      gv_sucess TYPE c LENGTH 30,
      gv_error  TYPE c LENGTH 30.

DATA : gv_super(1).
DATA : gv_bukrs_auth(1).


DATA gv_answer.

DATA gv_message TYPE c LENGTH 100.

DATA: g_valid TYPE c.
DATA: ls_toolbar TYPE stb_button.

TYPES: BEGIN OF ty_layout,
         repid    TYPE syrepid,
         restrict TYPE salv_de_layout_restriction,
         default  TYPE sap_bool,
         layout   TYPE disvariant-variant,
       END OF ty_layout.

DATA: go_alv        TYPE REF TO cl_salv_table,
      go_functions  TYPE REF TO cl_salv_functions_list,
      go_layout     TYPE REF TO cl_salv_layout,
      go_dspset     TYPE REF TO cl_salv_display_settings,
      gs_key        TYPE salv_s_layout_key,
      gt_column_ref TYPE salv_t_column_ref.


DATA gs_color  TYPE lvc_s_colo.

DATA gr_column TYPE REF TO cl_salv_column_table.

DATA: gt_rows TYPE salv_t_row,
      gs_row  TYPE int4.  "ALV position 기억

DATA: gv_column_text TYPE string,
      gv_scrtext_s   TYPE scrtext_s,
      gv_scrtext_m   TYPE scrtext_m,
      gv_scrtext_l   TYPE scrtext_l.

DATA: gs_salv_layout TYPE ty_layout.
DATA  gv_repid TYPE syrepid.

DATA gt_return TYPE TABLE OF bapiret2 WITH HEADER LINE.

DATA gv_count TYPE i.

DATA gv_ktopl TYPE ktopl VALUE '1000'.

DATA gv_mode.

DATA: gr_data TYPE REF TO data.

DATA: gt_fieldcat TYPE lvc_t_fcat,
      gs_fieldcat TYPE lvc_s_fcat.

*---------------------------------------------------------------------*
* STRUCTURE
*---------------------------------------------------------------------*
DATA : gs_funtxt TYPE smp_dyntxt. "Excel 양식 Download(펑션키)

*---------------------------------------------------------------------*
* INTERNAL TABLE
*---------------------------------------------------------------------*

DATA gt_tkvs TYPE TABLE OF tkvs WITH HEADER LINE.

DATA gt_cosp TYPE TABLE OF ty_cosp.

DATA: BEGIN OF gt_month OCCURS 0,
        v1 TYPE n LENGTH 2,
      END OF gt_month.

*---------------------------------------------------------------------*
* RANGES
*---------------------------------------------------------------------*
RANGES r_verns FOR tkvs-versi.

*---------------------------------------------------------------------*
* FIELD-SYMBOLS
*---------------------------------------------------------------------*
FIELD-SYMBOLS: <gt_table> TYPE table,
               <gs_line>  TYPE any.


*---------------------------------------------------------------------*
* MACRO (Define)
*---------------------------------------------------------------------*
* - Prefix 정의
*   1. _ : '_' 1개로 시작; Global, Local 구분 없음.
DEFINE _initial_chk.
  IF &1 IS INITIAL.
  gs_outtab-status    = 'E'.
  gs_outtab-message   = &2.
  ENDIF.
END-OF-DEFINITION.

DEFINE _conversion_in.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
  EXPORTING
  input = &1
  IMPORTING
  output = &1.
END-OF-DEFINITION.

DEFINE _conversion_wbs_in.
  CALL FUNCTION 'CONVERSION_EXIT_ABPSN_INPUT'
  EXPORTING
  input = &1
  IMPORTING
  output = &1.
END-OF-DEFINITION.

DEFINE _conversion_wbs_out.
  CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
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

DEFINE _clear.
  CLEAR &1. REFRESH &1.
END-OF-DEFINITION.

DEFINE _makeicon.

  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name                  = &1
      info                  = &2
    IMPORTING
      result                = &3
    EXCEPTIONS
      icon_not_found        = 1
      outputfield_too_short = 2
      OTHERS                = 3.

END-OF-DEFINITION.

DEFINE _set_color.

  gs_color-col = &1.
  gs_color-int = &2.
  gs_color-inv = &3.

END-OF-DEFINITION.

DEFINE _string_replace.

  IF &1 IS NOT INITIAL.

  CALL FUNCTION 'STRING_REPLACE'
    EXPORTING
      pattern    = ','
      substitute = ''
    CHANGING
      text       = &1.

 CONDENSE  &1.

  TRY.

    MOVE &1 TO a.

    CATCH cx_sy_conversion_no_number INTO DATA(err).

      gs_outtab-status =  'E'.
      gs_outtab-message = TEXT-e01.

      CLEAR  &1.

  ENDTRY.

 ENDIF.

END-OF-DEFINITION.
DEFINE _curr_input.
  WRITE &1 CURRENCY 'KRW' TO &2.
  CONDENSE &2.
END-OF-DEFINITION.

DEFINE _conv_krw.
  IF &1 IS NOT INITIAL .
    &1 = &1 / 100.
  ENDIF.
END-OF-DEFINITION.

*---------------------------------------------------------------------*
* Table Controls
*---------------------------------------------------------------------*
* - Prefix 정의
*   1. TC_ : Table Controls

*---------------------------------------------------------------------*
* Custom Controls
*---------------------------------------------------------------------*
* - Prefix 정의
*   1. CC_ : Custom Controls

*---------------------------------------------------------------------*
* Tabstrip Controls
*---------------------------------------------------------------------*
* - Prefix 정의
*   1. TS_ : Tabstrip Controls
