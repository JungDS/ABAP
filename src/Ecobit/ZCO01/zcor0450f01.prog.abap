*&---------------------------------------------------------------------*
*& Include          ZCOR0450F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM set_initvalue .

  CLEAR gv_datum.
  "전월
  _get_last_date sy-datum '00' '01' '-' '00' gv_datum.
  pa_month  = gv_datum(6).


  gv_repid = sy-repid.
  SELECT SINGLE bezei, waers INTO (@pa_ktxt, @gv_waers)
    FROM tka01
   WHERE kokrs = '1000'.
  SET PARAMETER ID 'CAC' FIELD pa_kokrs.


  SELECT SINGLE butxt INTO @pa_butxt
    FROM t001
   WHERE bukrs = @pa_bukrs.


  gs_funtxt-icon_id   = icon_create.
  gs_funtxt-quickinfo = '매출원가 계정지정'.
  gs_funtxt-icon_text = '매출원가 계정지정'.

  sscrfields-functxt_02 = gs_funtxt.




ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM modify_screen .
  LOOP AT SCREEN.
    IF p_ra1 = 'X'.
      IF screen-group1 = 'D' .
        screen-active = '0'.
      ENDIF.

    ELSEIF p_ra2 = 'X'.
*      IF SCREEN-GROUP1 = 'C' .
*        SCREEN-ACTIVE = '0'.
*      ENDIF.

      IF screen-name CP '*P_MODE*'.
        screen-active = '0'.
      ENDIF.


    ENDIF .

    IF screen-name = 'PA_KOKRS' OR
     screen-name = 'PA_MTART'  OR
     screen-name = 'PA_BUKRS'.

      screen-input = 0.


    ENDIF.

* 제품 체크 제외는 옵션은 숨기고 DEFAULT 제품만 조회됨

    IF screen-name CP '*P_CHK*'.
      screen-active = '0'.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECTED_DATA_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM selected_data_rtn .
  CLEAR : gv_lines , gv_exit ,
          gt_display , gt_display[],
          gt_acdoca, gt_acdoca[].

  PERFORM set_range_data.


* SD 플래트 별 판매수량 금액 가져오기
  PERFORM get_sales_qty_netwr.


*   ZMMT0610 [MM] 생산 투입/입고 처리 가져오기
*Mm 테이블에서 생산수량이 있는 제품 + 기말재고수량이 있는 제품이 화면에 출력되는 대상 제품코드 임.

*
  SELECT  werks ,
          rmblnr ,  " 원자재의 mvt 903으로 생성된 자재 문서번호
          fmatnr ,   "제품
          fmenge ,  "
          fmeins ,
          'KRW'  AS twaer ,
          rmatnr  ,  " 원자재
          rmenge  ,
          rmeins  ,
          rdmbtr AS rwrbtr      " mseg-dmbtr
    FROM zmmt0610
   WHERE werks =  @pa_werks
     AND budat IN @gr_budat
     AND fmenge > 0    " 제품 수량 있는  항목
     AND lvorm <> 'X'  "삭제 지시자
    INTO CORRESPONDING FIELDS OF TABLE @gt_raw.


*재료비

  CLEAR : gt_rate3.
  CLEAR : gt_rate3[].
  REFRESH : gt_rate3.


  LOOP AT gt_raw ASSIGNING FIELD-SYMBOL(<$fs>).
    <$fs>-twaer = 'KRW'.

    CLEAR gs_display.
    MOVE-CORRESPONDING <$fs> TO gs_display.

    gs_display-bukrs = pa_bukrs.
    gs_display-werks = pa_werks.
    gs_display-spmon = pa_month.


    READ TABLE gt_zmmt600 ASSIGNING FIELD-SYMBOL(<$zz2>) WITH KEY matnr = <$fs>-fmatnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_display-rmatnr =  <$zz2>-matnr2. " 매핑 원자재 넣기  COLLECT  목적...

      PERFORM get_lifnr USING gs_display.

    ENDIF.

* 제품 생산량 기준 배부 비율 계산용
    CLEAR:  gs_rate3.
    CLEAR:  gs_rate5.
    CLEAR:  gs_rate4.
    MOVE-CORRESPONDING gs_display TO gs_rate3.
    MOVE-CORRESPONDING gs_display TO gs_rate5.
    MOVE-CORRESPONDING gs_display TO gs_rate4.

    COLLECT gs_rate3 INTO gt_rate3.
    COLLECT gs_rate5 INTO gt_rate5.
    COLLECT gs_rate4 INTO gt_rate4.


    COLLECT gs_display INTO gt_display.


  ENDLOOP.

ENDFORM.                    " SELECTED_DATA_RTN


*&---------------------------------------------------------------------*
*&      Form  EVENT_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM event_toolbar
       USING pr_object     TYPE REF TO cl_alv_event_toolbar_set
             pv_interactive TYPE char01
             pr_sender     TYPE REF TO cl_gui_alv_grid.

* - Prefix 정의 - Form Parameter
*   1. PT_  : Internal Table or Ranges
*   2. PS_  : Structure
*   3. PV_  : Variables
*   4. PR_ : Reference Variables

* - BUTTON TYPE - BTYPE.
*  0 버튼(일반)
*  1 메뉴 및 기본 버튼
*  2 메뉴
*  3 분리자
*  4 라디오 버튼
*  5 체크박스
*  6 메뉴 엔트리

  CASE pr_sender.
    WHEN gr_grid1.

      IF p_ra1 IS NOT INITIAL. " 기표 후에조회 불필요...


        "ADD_BUTTON : OBJECT, BTYPE, FUNC, ICON, INFO, TEXT, DISABLE
        PERFORM add_button
          USING: pr_object '3' space space space space space, "분리자
                 pr_object '0' '&RAW' space TEXT-bt0 TEXT-bt0 space,   " MM 재료비


                 pr_object '0' '&DIFF1' space TEXT-bt1 TEXT-bt1 space, " FI/MM 비교
                 pr_object '0' '&DIFF2' space TEXT-bt2 TEXT-bt2 space,  " 불일치 전표리스트
                 pr_object '0' '&DIFFA' space TEXT-bta TEXT-bta space,  " FI 재료비 전체 전표리스트

                 pr_object '0' '&DIFF3' space TEXT-bt3 TEXT-bt3 space,  "구매가격차차이 전표리스트
                 pr_object '0' '&DIFF4' space TEXT-bt4 TEXT-bt4 space,   "  재고조정전표 리스트


                 pr_object '0' '&LABOR' space TEXT-bt5 TEXT-bt5 space,  "노무비 전표리스트
                 pr_object '0' '&COST' space TEXT-bt6 TEXT-bt6 space.  " 경비 전표리스트


        PERFORM add_button
        USING :  pr_object '3' space space space space space, "분리자
               pr_object '0' '&RATE3' space TEXT-bt7 TEXT-bt7 space,
               pr_object '0' '&RATE4' space TEXT-bt9 TEXT-bt9 space,
               pr_object '0' '&RATE5' space TEXT-bt8 TEXT-bt8 space.




      ENDIF.



    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " EVENT_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form ADD_BUTTON
*&---------------------------------------------------------------------*
FORM add_button USING pr_object TYPE REF TO cl_alv_event_toolbar_set
                    pv_btype
                    pv_func
                    pv_icon
                    pv_info
                    pv_text
                    pv_disa.

  DATA: ls_button TYPE stb_button,
        ls_btnmnu TYPE stb_btnmnu,

        lt_button TYPE ttb_button,
        lt_btnmnu TYPE ttb_btnmnu.

  CLEAR ls_button.
  ls_button-butn_type = pv_btype.
  ls_button-function  = pv_func.
  ls_button-icon      = pv_icon.
  ls_button-quickinfo = pv_info.

  ls_button-text      = pv_text.
  ls_button-disabled  = pv_disa.

  APPEND ls_button TO pr_object->mt_toolbar.

ENDFORM.                   " ADD_BUTTON

*&---------------------------------------------------------------------*
*&      Form  EVENT_USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM event_user_command  USING pv_ucomm   TYPE sy-ucomm
                               pr_sender TYPE REF TO cl_gui_alv_grid.

* - Prefix 정의 - Form Parameter
*   1. PT_  : Internal Table or Ranges
*   2. PS_  : Structure
*   3. PV_  : Variables
*   4. PR_  : Reference Variables

*  CLEAR: GT_ROWS, GT_ROWS[].
*  "선택 ROW가져오기
*  CALL METHOD PR_SENDER->GET_SELECTED_ROWS
*    IMPORTING
*      ET_INDEX_ROWS = GT_ROWS[].

  CASE pr_sender.
    WHEN gr_grid1.
      CASE pv_ucomm.
        WHEN '&RAW'.   "-- 재료비 상세
          PERFORM salv_call USING 0.    " GT_RAW...

        WHEN '&DIFF1'.   "-- FI  MM 비교   GT_RAW2...    FI/MM 비교
          PERFORM salv_call USING 1.
        WHEN '&DIFF2'.   "-- " ZMMR4050 미처리 전표리스트
          PERFORM salv_call USING 2.   "GT_DIFF_001B

        WHEN '&DIFFA'.   "-- "  FI 전체 전표리스트
          PERFORM salv_call USING ''.   "GT_DIFF_001

        WHEN '&DIFF3'.   "-- "구매가격차차이 전표리스트
          PERFORM salv_call USING 3.   "GT_DIFF_002  504101003   구매가격차이
        WHEN '&DIFF4'.   "-- 재고조정전표 리스트
          PERFORM salv_call USING 4.   "GT_DIFF_003   05101002  재고조정
        WHEN '&LABOR'.   "-- 노무비  GT_LABOR
          PERFORM salv_call USING 5.
        WHEN '&COST'.   "-- 경비  GT_MCOST
          PERFORM salv_call USING 6.


        WHEN '&RATE3'.
          PERFORM salv_call USING 7.

        WHEN '&RATE5'.
          PERFORM salv_call USING 8.

        WHEN '&RATE4'.
          PERFORM salv_call USING 9.






      ENDCASE.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " EVENT_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  EVENT_DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM event_data_changed
       USING pr_data_changed TYPE REF TO cl_alv_changed_data_protocol
             pv_onf4          TYPE char01
             pv_onf4_before   TYPE char01
             pv_onf4_after    TYPE char01
             pv_ucomm         TYPE sy-ucomm
             pr_sender       TYPE REF TO cl_gui_alv_grid.


* - Prefix 정의 - Form Parameter
*   1. PT_  : Internal Table or Ranges
*   2. PS_  : Structure
*   3. PV_  : Variables
*   4. PR_  : Reference Variables

*--- Begin or Example
*  DATA: LS_MOD_CELLS TYPE LVC_S_MODI,
*        LS_INS_CELLS TYPE LVC_S_MOCE,
*        LS_DEL_CELLS TYPE LVC_S_MOCE.
*
*  DEFINE _MODIFY_CELL.
*
*    CALL METHOD PR_DATA_CHANGED->MODIFY_CELL
*      EXPORTING
*        I_FIELDNAME = &1
*        I_ROW_ID    = &2
*        I_VALUE     = &3.
*
*  END-OF-DEFINITION.
*
*  DEFINE _GET_CELL_VALUE.
*    CALL METHOD PR_DATA_CHANGED->GET_CELL_VALUE
*      EXPORTING
*        I_FIELDNAME = &1
*        I_ROW_ID    = &2
*      IMPORTING
*        E_VALUE     = &3.
*  END-OF-DEFINITION.
*
*  DEFINE _ADD_PROTOCOL.
*    CALL METHOD PR_DATA_CHANGED->ADD_PROTOCOL_ENTRY
*      EXPORTING
*        I_FIELDNAME = &1
*        I_ROW_ID    = &2
*        I_MSGID     = 'ZMSTD'
*        I_MSGTY     = &3
*        I_MSGNO     = &4
*        I_MSGV1     = &5
*        I_MSGV2     = &6
*        I_MSGV3     = &7
*        I_MSGV4     = &8.
*  END-OF-DEFINITION.
*
*--- End of Example


*  CASE PR_SENDER.
*    WHEN GR_GRID1.
*      LOOP AT PR_DATA_CHANGED->MT_MOD_CELLS INTO LS_MOD_CELLS.
*        CASE LS_MOD_CELLS-FIELDNAME.
*          WHEN 'VBELN'.
*          WHEN OTHERS.
*        ENDCASE.
*      ENDLOOP.
*    WHEN OTHERS.
*  ENDCASE.


ENDFORM.                    " EVENT_DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  EVENT_DATA_CHANGED_FINISHED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM event_data_changed_finished
       USING p_modified    TYPE char01
             pt_good_cells TYPE lvc_t_modi
             pr_sender    TYPE REF TO cl_gui_alv_grid.


* - Prefix 정의 - Form Parameter
*   1. PT_  : Internal Table or Ranges
*   2. PS_  : Structure
*   3. PV_  : Variables
*   4. PR_ : Reference Variables


ENDFORM.                    " EVENT_DATA_CHANGED_FINISHED
*&---------------------------------------------------------------------*
*&      Form  EVENT_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM event_hotspot_click USING ps_row_id    TYPE lvc_s_row
                               ps_column_id TYPE lvc_s_col
                               ps_row_no    TYPE lvc_s_roid
                               pr_sender   TYPE REF TO cl_gui_alv_grid.


* - Prefix 정의 - Form Parameter
*   1. PT_  : Internal Table or Ranges
*   2. PS_  : Structure
*   3. PV_  : Variables
*   4. PR_ : Reference Variables


  CASE pr_sender.
    WHEN gr_grid1.

      CASE ps_column_id-fieldname.
        WHEN 'MATNR'.
*          READ TABLE GT_DISPLAY INTO GS_DISPLAY INDEX PS_ROW_NO-ROW_ID.
*          CHECK SY-SUBRC EQ 0.
*
*          "Call Transaction
*          SET PARAMETER ID 'MAT' FIELD GS_DISPLAY-MATNR .
*          SET PARAMETER ID 'MXX' FIELD 'K' .              "Directly Display Basic Data
*          CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN .

      ENDCASE.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " EVENT_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*&      Form  EVENT_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM event_double_click  USING ps_row     TYPE lvc_s_row
                               ps_column  TYPE lvc_s_col
                               ps_row_no  TYPE lvc_s_roid
                               pr_sender TYPE REF TO cl_gui_alv_grid.


* - Prefix 정의 - Form Parameter
*   1. PT_  : Internal Table or Ranges
*   2. PS_  : Structure
*   3. PV_  : Variables
*   4. PR_ : Reference Variables


  CASE pr_sender.
    WHEN gr_grid1.
*-- 전표번호를 클릭하면 전표조회 화면으로 이동
      CASE ps_column .
        WHEN 'BELNR1'.
          CLEAR gs_display.
          READ TABLE gt_display INTO gs_display INDEX ps_row_no-row_id.

          CHECK gs_display-belnr1 IS NOT INITIAL.

          SET PARAMETER ID 'BLN' FIELD gs_display-belnr1.
          SET PARAMETER ID 'BUK' FIELD pa_bukrs.
          SET PARAMETER ID 'GJR' FIELD pa_month+0(4).

          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        WHEN 'BELNR3'.
          CLEAR gs_display.
          READ TABLE gt_display INTO gs_display INDEX ps_row_no-row_id.

          CHECK gs_display-belnr3 IS NOT INITIAL.
*
          SET PARAMETER ID 'BLN' FIELD gs_display-belnr3.
          SET PARAMETER ID 'BUK' FIELD pa_bukrs.
          SET PARAMETER ID 'GJR' FIELD pa_month+0(4).


          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.


        WHEN 'FMATNR' OR 'FMATNR_MAKTX'.

          CLEAR gs_display.
          READ TABLE gt_display INTO gs_display INDEX ps_row_no-row_id.

          CHECK gs_display-fmatnr IS NOT INITIAL.
*
          SET PARAMETER ID 'MAT' FIELD gs_display-fmatnr.
          SET PARAMETER ID 'MXX' FIELD 'K'. .
          SET PARAMETER ID 'WRK' FIELD pa_werks.
          CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

        WHEN 'RMATNR' OR 'RMATNR_MAKTX'.

          CLEAR gs_display.
          READ TABLE gt_display INTO gs_display INDEX ps_row_no-row_id.

          CHECK gs_display-rmatnr IS NOT INITIAL.
*
          SET PARAMETER ID 'MAT' FIELD gs_display-rmatnr.
          SET PARAMETER ID 'MXX' FIELD 'K'. .
          SET PARAMETER ID 'WRK' FIELD pa_werks.
          CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

        WHEN OTHERS.
      ENDCASE.
  ENDCASE.




ENDFORM.                    " EVENT_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*&      Form  EVENT_HELP_ON_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM event_help_on_f4
       USING pv_fieldname   TYPE lvc_fname
             pv_fieldvalue  TYPE lvc_value
             ps_row_no      TYPE lvc_s_roid
             pr_event_data TYPE REF TO cl_alv_event_data
             pt_bad_cells   TYPE lvc_t_modi
             pv_display     TYPE char01
             pr_sender     TYPE REF TO cl_gui_alv_grid.

* - Prefix 정의 - Form Parameter
*   1. PT_  : Internal Table or Ranges
*   2. PS_  : Structure
*   3. PV_  : Variables
*   4. PR_ : Reference Variables


*  CASE PR_SENDER.
*    WHEN GR_GRID1.
*    WHEN OTHERS.
*  ENDCASE.






ENDFORM.                    " EVENT_HELP_ON_F4
*&---------------------------------------------------------------------*
*&      Form  CREATE_INSTANCE_0100
*&---------------------------------------------------------------------*
*       Postfix : _xxxx ( Screen No ).
*         부가적으로 더 필요한 경우 : _xx ( Seq No )를 추가로 붙임.
*----------------------------------------------------------------------*
FORM create_instance_0100 .
*-- 1. customer container

*  CREATE OBJECT GR_CON1
*    EXPORTING
*      CONTAINER_NAME = GV_CONTAINER. "USER가 정의한 CONTAINER
*
*  CREATE OBJECT GR_GRID1
*    EXPORTING
*      I_PARENT = GR_CON1.



*-- 2. full screen

  CREATE OBJECT gr_grid1
    EXPORTING
      i_parent = cl_gui_container=>screen0.

  "CL_GUI_CONTAINER=>SCREEN0 : Dummy for Top Level 0 Screen Container


ENDFORM.                    " CREATE_INSTANCE_0100
*&---------------------------------------------------------------------*
*&      Form  INIT_LAYOUT_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM init_layout_0100.

  gs_layout-edit_mode  = abap_true.
  gs_layout-zebra      = abap_false.
  gs_layout-cwidth_opt = abap_true.
* GS_LAYOUT-STYLEFNAME = 'STYLE'.
  gs_layout-sel_mode   = space.     "B:단일,C:복수,D:셀,A:행/열
  gs_layout-box_fname  = space.
  gs_layout-no_rowmark = space.

*  GS_LAYOUT-CTAB_FNAME = 'COLOR'.
*  GS_LAYOUT-INFO_FNAME = 'INFO'.

**  "alv title
**  GS_LAYOUT-GRID_TITLE = TEXT-GT1.

ENDFORM.                    " INIT_LAYOUT_0100
*&---------------------------------------------------------------------*
*&      Form  SET_GRID_EXCLUDE_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_grid_exclude_0100 .

  DATA: ls_exclude LIKE LINE OF gt_exclude.
  REFRESH: gt_exclude.

  "-- DEFINE _SET_EX
  DEFINE _set_ex.
    CLEAR: ls_exclude.
    ls_exclude = &1.
    APPEND ls_exclude TO gt_exclude.
  END-OF-DEFINITION.


  _set_ex:
*   CL_GUI_ALV_GRID=>MC_FC_FIND,
*   CL_GUI_ALV_GRID=>MC_FC_SORT_ASC,
*   CL_GUI_ALV_GRID=>MC_FC_SORT_DSC,
*   CL_GUI_ALV_GRID=>MC_MB_SUBTOT,
*   CL_GUI_ALV_GRID=>MC_MB_SUM,

    cl_gui_alv_grid=>mc_fc_loc_copy_row,
    cl_gui_alv_grid=>mc_fc_loc_append_row,
    cl_gui_alv_grid=>mc_fc_loc_insert_row,
    cl_gui_alv_grid=>mc_fc_loc_move_row,
    cl_gui_alv_grid=>mc_fc_loc_delete_row,
    cl_gui_alv_grid=>mc_fc_loc_copy,
    cl_gui_alv_grid=>mc_fc_loc_cut,
    cl_gui_alv_grid=>mc_fc_loc_paste,
    cl_gui_alv_grid=>mc_fc_loc_paste_new_row,
    cl_gui_alv_grid=>mc_fc_loc_undo,
    cl_gui_alv_grid=>mc_fc_check,

*   CL_GUI_ALV_GRID=>MC_FC_DETAIL,
*   CL_GUI_ALV_GRID=>MC_FC_FILTER,
    cl_gui_alv_grid=>mc_fc_graph,
    cl_gui_alv_grid=>mc_fc_html,
    cl_gui_alv_grid=>mc_fc_info,
    cl_gui_alv_grid=>mc_fc_refresh,

*   CL_GUI_ALV_GRID=>MC_FC_VIEWS,
*   CL_GUI_ALV_GRID=>MC_FC_LOAD_VARIANT,
*   CL_GUI_ALV_GRID=>MC_FC_PRINT,
*   CL_GUI_ALV_GRID=>MC_MB_VARIANT,
*   CL_GUI_ALV_GRID=>MC_MB_EXPORT,

    cl_gui_alv_grid=>mc_fc_view_crystal,
    cl_gui_alv_grid=>mc_fc_view_excel,
    cl_gui_alv_grid=>mc_fc_view_grid,
    cl_gui_alv_grid=>mc_fc_view_lotus,
    cl_gui_alv_grid=>mc_fc_expcrdata,
    cl_gui_alv_grid=>mc_fc_expcrdesig,
    cl_gui_alv_grid=>mc_fc_expcrtempl,
    cl_gui_alv_grid=>mc_fc_call_abc,
    cl_gui_alv_grid=>mc_fc_call_crbatch.

ENDFORM. " SET_GRID_EXCLUDE_0100
*&---------------------------------------------------------------------*
*&      Form  ALV_SORT_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_sort_0100 .

  CLEAR: gs_sort, gt_sort.
  REFRESH: gt_sort.


ENDFORM.                    " ALV_SORT_0100
*&---------------------------------------------------------------------*
*&      Form  APPEND_FIELDCAT_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM append_fieldcat_0100 .

  "-- field catalog data
  "   field catalog merge or set fieldcatalog를 사용할 수 있음.

  "{ FIELDCATLOG MERGE 사용
  PERFORM get_fieldcatlog_data(zcar9000) TABLES gt_fieldcat
                                          USING 'ZCOS0450'
                                             IF FOUND.

  PERFORM modify_fieldcatlog_data.
  "}

  "{ SET FIELDCATLOG 사용
*  PERFORM SET_FIELDCATLOG_DATA.
  "}


ENDFORM.                    " APPEND_FIELDCAT_0100
*&---------------------------------------------------------------------*
*&      Form  MODIFY_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM modify_fieldcatlog_data .

  DATA:  lv_text(50).

  "--- Change Fieldcat.
  LOOP AT gt_fieldcat INTO gs_fieldcat.
    CLEAR: lv_text.

    "-- Change fieldcat Attribute
    CASE gs_fieldcat-fieldname.
      WHEN 'STATS'.
        gs_fieldcat-just = gc_c.
        gs_fieldcat-outputlen = 4.
        lv_text = TEXT-f77.
        gs_fieldcat-fix_column = 'X'.
      WHEN 'ICON'.
        gs_fieldcat-just = gc_c.
        gs_fieldcat-outputlen = 4.
        gs_fieldcat-fix_column = 'X'.
        lv_text = TEXT-f01.
      WHEN 'BELNR1'.
        lv_text = TEXT-f02.
      WHEN 'BELNR2'.
        lv_text = TEXT-f13.
        gs_fieldcat-no_out = abap_true.

      WHEN 'BELNR3'.
        lv_text = TEXT-f03.

      WHEN 'BUKRS'.
        gs_fieldcat-no_out = abap_true.
      WHEN 'D1WRBTR' OR  'D2WRBTR' OR 'D3WRBTR' OR 'D4WRBTR' OR 'EUWRBTR' " 기말단가(매출단가)
           OR 'EWRBTR' OR 'LWRBTR' OR 'MWRBTR' OR  'RWRBTR'
           OR 'SWRBTR' OR 'ZCOGM' OR 'ZCOGS' OR 'DMBTR'.

        gs_fieldcat-just = gc_r.
        gs_fieldcat-no_zero = gc_x.
        gs_fieldcat-outputlen = '22'.
        gs_fieldcat-cfieldname = 'TWAER' .
        gs_fieldcat-emphasize = 'C411'.
        gs_fieldcat-do_sum = 'X'.
      WHEN 'EMEINS'.
        lv_text = TEXT-g02.  "단위
        gs_fieldcat-decimals = 3.
      WHEN 'EMENGE'. "기말수량
        lv_text = TEXT-g01.
        gs_fieldcat-qfieldname = 'EMEINS' .
      WHEN 'FMEINS'.
        gs_fieldcat-decimals = 3.
        lv_text = TEXT-g02.  "단위
      WHEN 'FMENGE'. "생산수량
        gs_fieldcat-qfieldname = 'FMEINS' .
        lv_text = TEXT-f11.
      WHEN 'FMATNR'."제품코드
        gs_fieldcat-lzero = 'X'.
*        GS_FIELDCAT-OUTPUTLEN = 10.
        lv_text = TEXT-f06.
*        GS_FIELDCAT-FIX_COLUMN = 'X'.
*        GS_FIELDCAT-NO_OUT = ABAP_TRUE.
        gs_fieldcat-emphasize = 'C711'.
      WHEN 'FMATNR_MAKTX' ."제품코드명 .
        lv_text = TEXT-f10.
        gs_fieldcat-outputlen = 10.
        gs_fieldcat-fix_column = 'X'.
        gs_fieldcat-emphasize = 'C711'.
      WHEN 'LIFNR'.
        gs_fieldcat-outputlen = 10.
        gs_fieldcat-no_out = abap_true.
        gs_fieldcat-fix_column = 'X'.
      WHEN 'NAME1'.
        lv_text = TEXT-f04.
        gs_fieldcat-outputlen = 8.
        gs_fieldcat-fix_column = 'X'.
      WHEN 'MATKL'.
        gs_fieldcat-outputlen = 10.
        gs_fieldcat-no_out = abap_true.
        gs_fieldcat-fix_column = 'X'.
      WHEN 'WGBEZ'.
        lv_text = TEXT-f07. "자재그룹명
        gs_fieldcat-outputlen = 10.
        gs_fieldcat-fix_column = 'X'.
      WHEN 'RMATNR'.  "원자재
        gs_fieldcat-lzero = 'X'.
        lv_text = TEXT-f08.
        gs_fieldcat-no_out = abap_true.
*        GS_FIELDCAT-OUTPUTLEN = 10.
*        GS_FIELDCAT-FIX_COLUMN = 'X'.
        gs_fieldcat-emphasize = 'C310'.
      WHEN 'RMATNR_MAKTX'.
        lv_text = TEXT-f18.
        gs_fieldcat-emphasize = 'C310'.
        gs_fieldcat-fix_column = 'X'.
        gs_fieldcat-outputlen = 8.
      WHEN 'RMEINS'.
        gs_fieldcat-decimals = 3.
        lv_text = TEXT-g02.  "단위
      WHEN 'RMENGE'.
        gs_fieldcat-qfieldname = 'RMEINS' .
        lv_text = TEXT-f12.
      WHEN 'SMEINS'.
        gs_fieldcat-decimals = 3.
        lv_text = TEXT-g02.  "단위
      WHEN 'SMENGE'.
        gs_fieldcat-qfieldname = 'SMEINS' .

      WHEN 'BWRBTR'.
*        GS_FIELDCAT-DECIMALS = 3.
        lv_text = TEXT-g10.  " 기초이월 금액
*        GS_FIELDCAT-NO_OUT = ABAP_TRUE.

        gs_fieldcat-just = gc_r.
        gs_fieldcat-no_zero = gc_x.
        gs_fieldcat-outputlen = '22'.
        gs_fieldcat-cfieldname = 'TWAER' .
        gs_fieldcat-emphasize = 'C411'.


      WHEN 'BMEINS'.
        gs_fieldcat-decimals = 3.
        lv_text = TEXT-g11.  " 기초이월 단위
        gs_fieldcat-no_out = abap_true.
      WHEN 'BMENGE'.
        gs_fieldcat-qfieldname = 'BMEINS' .
*        GS_FIELDCAT-NO_OUT = ABAP_TRUE.
        lv_text = TEXT-g12.  " 기초이월 수량
      WHEN 'SPMON'.
        gs_fieldcat-outputlen = 10.
        gs_fieldcat-no_out = abap_true.
      WHEN 'TWAER'.
        gs_fieldcat-no_out = abap_true.
      WHEN 'WERKS'.
        gs_fieldcat-no_out = abap_true.

      WHEN 'WERKS_NAME'.
        lv_text = TEXT-f05.
        gs_fieldcat-no_out = abap_true.
        gs_fieldcat-outputlen = 10.

      WHEN 'DMBTR'.
        lv_text =  TEXT-f66.
*        LV_TEXT = '제조매출(FI)'.
      WHEN  'MSG'.
        lv_text = 'ICON 설명'.

*                GS_FIELDCAT-FIX_COLUMN = 'X'.
      WHEN OTHERS.
*      WHEN 'AEDAT'.
*      WHEN 'AENAM'.
*      WHEN 'AEZET'.
*      WHEN 'ERDAT'.
*      WHEN 'ERNAM'.
*      WHEN 'ERZET'.

        gs_fieldcat-no_out = abap_true.
    ENDCASE.

    "-- fix column
    IF gs_fieldcat-col_pos LE 4.
      gs_fieldcat-fix_column = abap_true.
    ENDIF.

    "-- 최적화

    "-- Common attribute
    IF lv_text IS NOT INITIAL.
      gs_fieldcat-coltext   = lv_text.
      gs_fieldcat-scrtext_l = lv_text.
      gs_fieldcat-scrtext_m = lv_text.
      gs_fieldcat-scrtext_s = lv_text.
    ENDIF.

    MODIFY gt_fieldcat FROM gs_fieldcat.
  ENDLOOP.

ENDFORM.                    " MODIFY_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
*&      Form  SET_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_fieldcatlog_data.

  CLEAR gt_fieldcat[].

*  PERFORM FILL_FIELD_CATEGORY USING :
*        'S' 'FIELDNAME'   'ICON',
*        ' ' 'OUTPUTLEN'   '4',
*        ' ' 'FIX_COLUMN'  'X',
*        'E' 'COLTEXT'     'ID'.

ENDFORM.                    " SET_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
*&      Form  fill_field_category
*&---------------------------------------------------------------------*
FORM fill_field_category USING pv_gub pv_fname pv_con.

  IF pv_gub = 'S'.
    CLEAR gs_fieldcat.
  ENDIF.

* 속성 MOVE
  DATA lv_col(40).
  FIELD-SYMBOLS <fs>.
  CONCATENATE 'GS_FIELDCAT-' pv_fname  INTO lv_col.
  ASSIGN      (lv_col)       TO        <fs>.
  MOVE         pv_con        TO        <fs>.

  IF pv_gub = 'E'.
    APPEND gs_fieldcat TO gt_fieldcat.
  ENDIF.
ENDFORM. " fill_field_category
*&---------------------------------------------------------------------*
*&      Form  REGIST_ALV_EVENT_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM regist_alv_event_0100 USING pr_grid TYPE REF TO cl_gui_alv_grid.

* REGISTER EVENT
  CALL METHOD pr_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*
  CALL METHOD pr_grid->set_ready_for_input
    EXPORTING
      i_ready_for_input = 1.

*-- GR_EVENT_RECEIVER
  IF gr_event_receiver IS INITIAL.
    CREATE OBJECT gr_event_receiver.
  ENDIF.

* Handler Event
  SET HANDLER:
    gr_event_receiver->handle_toolbar       FOR ALL INSTANCES,
    gr_event_receiver->handle_data_changed  FOR ALL INSTANCES,
    gr_event_receiver->handle_user_command  FOR ALL INSTANCES,
    gr_event_receiver->handle_hotspot_click FOR ALL INSTANCES,
    gr_event_receiver->handle_double_click  FOR ALL INSTANCES,
    gr_event_receiver->handle_onf4          FOR ALL INSTANCES.

ENDFORM.                    " REGIST_ALV_EVENT_0100
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_TITLE_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_alv_title_0100 .

  DATA: lv_title TYPE lvc_title.

  lv_title = TEXT-gt1.

  CALL METHOD gr_grid1->set_gridtitle
    EXPORTING
      i_gridtitle = lv_title.

ENDFORM.                    " DISPLAY_ALV_TITLE_0100
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_GRID_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_alv_grid_0100 .

  gs_variant-report = sy-repid.
  gs_variant-handle = sy-dynnr.

  gv_save = 'A'.

  "*-- Build field catalog for the alv control
  CALL METHOD gr_grid1->set_table_for_first_display
    EXPORTING
      i_default                     = abap_true
      is_layout                     = gs_layout
      is_variant                    = gs_variant
      i_save                        = gv_save
      it_toolbar_excluding          = gt_exclude
    CHANGING
      it_fieldcatalog               = gt_fieldcat
      it_sort                       = gt_sort
      it_outtab                     = gt_display[]
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3.

  IF sy-subrc NE 0.
    MESSAGE e000(0k) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " DISPLAY_ALV_GRID_0100
*&---------------------------------------------------------------------*
*&      Form  REFRESH_GRID_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM refresh_grid_0100 .

  gs_stable-row = abap_true. "Row
  gs_stable-col = abap_true. "column

  CALL METHOD gr_grid1->refresh_table_display
    EXPORTING
      is_stable      = gs_stable
      i_soft_refresh = space.

ENDFORM.                    " REFRESH_GRID_0100
*&---------------------------------------------------------------------*
*&      Form  CHECKED_SAVED_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM checked_saved_data .

*  CLEAR: GT_ROWS[].
*
*  CALL METHOD GR_GRID1->GET_SELECTED_ROWS
*    IMPORTING
*      ET_INDEX_ROWS = GT_ROWS[].

ENDFORM.                    " CHECKED_SAVED_DATA
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM save_data_rtn .


ENDFORM.                    " SAVE_DATA_RTN
*&---------------------------------------------------------------------*
*& Form SELECT_MAIN_DATA_RTN_1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM select_main_data_rtn_1 .

  "
ENDFORM.
*
*&---------------------------------------------------------------------*
*& Form SET_RANGE_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_range_data .
  CLEAR : gr_gjahr[],
          gr_budat[],
          gr_racct[].

  DATA : lv_date     TYPE sy-datum,
         lv_last_day TYPE sy-datum.

  DATA : lv_yy(4),
         lv_ym(6).


  CLEAR gr_budat. REFRESH gr_budat.

  lv_date = pa_month && '01'.
  PERFORM rp_last_day_of_months(zcar9000) USING lv_date
                                       CHANGING lv_last_day
                                             IF FOUND.

  _set_ranges : gr_gjahr 'I' 'EQ' pa_month(4) '',  "연도
                gr_budat 'I' 'BT' lv_date lv_last_day .   "전기월


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_DATA_RTN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_data_rtn .
  PERFORM clear_itab.

*  LOOP AT GT_ROWS INTO GS_ROWS.

  MESSAGE i000(zco01) WITH '매출원가,제조원가 0원자료는 제외후 일괄 기표됩니다..' .

  CLEAR: gv_error.  "각 단계 ERROR 체크.
*
*  LOOP AT GT_DISPLAY ASSIGNING  <FS_DISP>  WHERE  ZCOGM IS NOT INITIAL
*                                              OR ZCOGS IS NOT INITIAL.


  LOOP AT gt_display ASSIGNING  <fs_disp>   .

**  기표대상
    IF   <fs_disp>-zcogm IS NOT INITIAL  OR <fs_disp>-zcogs IS NOT INITIAL.

*    " 선택 항목으로 기표
*    READ TABLE GT_DISPLAY ASSIGNING <FS_DISP> INDEX GS_ROWS-INDEX.

*****    PERFORM BUDAT_SET.

      IF <fs_disp>-belnr1 IS INITIAL AND <fs_disp>-zcogm IS NOT INITIAL.
        PERFORM make_data USING 'YY'.
        PERFORM bdc_fb01 USING 'YY'.
      ENDIF.


      IF <fs_disp>-belnr3 IS INITIAL  AND  <fs_disp>-zcogs IS NOT INITIAL .

        PERFORM make_data USING 'ZZ'.
        PERFORM bdc_fb01 USING 'ZZ'.
      ENDIF.

    ELSE.  " 생산, 매출없지만 기말재고 있는 자재 저장
      " 기말 수량이 있는 제품 자재만
      IF <fs_disp>-ewrbtr IS NOT INITIAL.
        <fs_disp>-icon = '@08@'." 성공
        <fs_disp>-msg = '저장완료' ."

        PERFORM save_1260.
      ENDIF.

    ENDIF.

  ENDLOOP.

*  ENDLOOP.


**제조원가 0, 매출원가 0 은 저장 불필요  20200316  메일 답변

**  PERFORM SAVE_COT1260_ZER0.


ENDFORM.

*&---------------------------------------------------------------------*
*& Form MAKE_DATA
*&---------------------------------------------------------------------*
*& 현재 사용되는 ZCOR0380 프로그램의 매출원가 전기 로직 참조  SPEC  매출원가전표

*&---------------------------------------------------------------------*
*&      --> GC_A
*&---------------------------------------------------------------------*
FORM make_data  USING    pv_val.

  DATA: lv_seq TYPE vvnlfdnum.
  CLEAR : lv_seq , gt_bk, gt_bs, gv_spmon,
  gt_bk[], gt_bs[].



  DATA: lv_wrbtr   LIKE gs_display-zcogm,
        lv_wrbtr_c TYPE c LENGTH 20.

  gv_spmon    =  pa_month && '01'.


  "입력기간 말일
  PERFORM rp_last_day_of_months(zcar9000) USING gv_spmon
                                       CHANGING gv_spmon
                                             IF FOUND.

  CASE pv_val.

    WHEN 'YY' . " "<<<< 제조원가

      "Header
      CLEAR gt_bk.
      gt_bk-serial =  '0001'.   " 순번
      gt_bk-tcode = 'FB01'.   " 트랜잭션 코드
      gt_bk-blart = gc_zm. " 전표유형
      gt_bk-bukrs = pa_bukrs. "회사코드
      gt_bk-bldat = gv_spmon. " 증빙일자
      gt_bk-budat = gv_spmon. "전기일자
      gt_bk-waers = gc_krw.  "<FS_DISP>-WAERS.  "거래통화
      gt_bk-bktxt = gv_spmon+(6) && TEXT-h06.  "전표 헤더텍스트_ 제품원가대체
      gt_bk-xblnr = ''.

      APPEND gt_bk. CLEAR gt_bk.




      DO 2 TIMES.
        lv_seq = lv_seq + 1.
        gt_bs-serial = '0001'. "일련번호
        gt_bs-itmno = lv_seq . "라인 일련번호

        CASE lv_seq.
          WHEN 1. "첫번째라인
            IF  <fs_disp>-zcogm >  0 .
              gt_bs-newbs = '40'. "전기키  "차변
              gt_bs-newko = '0101302001 '.  " 제품
            ELSE .
              gt_bs-newbs = '50'.
              gt_bs-newko = '0101302001'.
            ENDIF.

          WHEN 2. "두번째라인
            gt_bs-newko = '0505501001'.
            IF  <fs_disp>-zcogm >  0 .
              gt_bs-newbs = '50'.
            ELSE .
              gt_bs-newbs = '40'.
            ENDIF.


            READ TABLE gr_posid INDEX 1.
            gt_bs-projk  =  gr_posid-low.



        ENDCASE.

        lv_wrbtr =   abs( <fs_disp>-zcogm ).
        lv_wrbtr_c =  lv_wrbtr. CONDENSE lv_wrbtr_c.
        PERFORM curr_sap_to_idoc(zcar9000) USING gc_krw lv_wrbtr_c
                                        IF FOUND.

        gt_bs-wrbtr = lv_wrbtr_c.

*19AA = 1900 인데   제조원가 발생안한다고 함
        IF pa_werks = '19AA'.
          gt_bs-gsber  = '1900'.
        ELSE.
          gt_bs-gsber = <fs_disp>-werks.       "사업영역 = 플랜트
        ENDIF.

        gt_bs-wrbtr = lv_wrbtr_c.
        gt_bs-sgtxt =  gv_spmon+(6) && TEXT-h06 .

*제품 자재 코드 넣기
        gt_bs-matnr = <fs_disp>-fmatnr.


        APPEND gt_bs.  CLEAR: gt_bs.

      ENDDO.

    WHEN 'ZZ'.  "<<<< 매출원가

      "Header
      CLEAR gt_bk.
      gt_bk-serial =  '0001'.   " 순번
      gt_bk-tcode = 'FB01'.   " 트랜잭션 코드
      gt_bk-blart = gc_wz. " 전표유형
      gt_bk-bukrs = pa_bukrs. "회사코드
      gt_bk-bldat = gv_spmon. " 증빙일자
      gt_bk-budat = gv_spmon. "전기일자
      gt_bk-waers = gc_krw.  "<FS_DISP>-WAERS.  "거래통화
      gt_bk-bktxt = gv_spmon+(6) && TEXT-h07.  "전표 헤더텍스트_ 매출원가
      gt_bk-xblnr = ''.

      APPEND gt_bk. CLEAR gt_bk.


      READ TABLE gt_zcot1230 INTO gs_zcot1230 INDEX 1.
      READ TABLE gt_wbs_cogs INTO gs_wbs_cogs INDEX 1.

      DO 2 TIMES.
        lv_seq = lv_seq + 1.
        gt_bs-serial = '0001'. "일련번호
        gt_bs-itmno = lv_seq . "라인 일련번호

        CASE lv_seq.
          WHEN 1. "첫번째라인
            IF  <fs_disp>-zcogs >  0 .
              gt_bs-newbs = '40'. "전기키  "차변
              gt_bs-newko =  gs_zcot1230-saknr.  " 제품매출원가
*              GT_BS-NEWKO = '502101001 '.  " 제품매출원가
            ELSE .
              gt_bs-newbs = '50'.
              gt_bs-newko = gs_zcot1230-saknr..
*              GT_BS-NEWKO = '502101001'.
            ENDIF.


            gt_bs-projk  =  gs_wbs_cogs-posid.
*            GT_BS-PROJK  =  GS_ZCOT1230-POSID.


          WHEN 2. "두번째라인
            IF  <fs_disp>-zcogs >  0 .
              gt_bs-newbs = '50'. "전기키  "차변
              gt_bs-newko = '0101302001 '.  " 제품
            ELSE .
              gt_bs-newbs = '40'.
              gt_bs-newko = '0101302001'.
            ENDIF.


        ENDCASE.

        lv_wrbtr =   abs( <fs_disp>-zcogs ).
        lv_wrbtr_c =  lv_wrbtr. CONDENSE lv_wrbtr_c.
        PERFORM curr_sap_to_idoc(zcar9000) USING gc_krw lv_wrbtr_c
                                           IF FOUND.



        gt_bs-wrbtr = lv_wrbtr_c.

*19AA = 1900 인데   제조원가 발생안한다고 함
        IF pa_werks = '19AA'.
          gt_bs-gsber  = '1900'.
        ELSE.
          gt_bs-gsber = <fs_disp>-werks.       "사업영역 = 플랜트
        ENDIF.

        gt_bs-wrbtr = lv_wrbtr_c.
        gt_bs-sgtxt =  '[CO] 제품별 매출원가 전기'.


        gt_bs-matnr = <fs_disp>-fmatnr.

        APPEND gt_bs.  CLEAR: gt_bs.

      ENDDO.


  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BDC_FB01
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GC_A
*&---------------------------------------------------------------------*
FORM bdc_fb01  USING    pv_val.
  DATA : lv_lines TYPE sy-tabix.

  CLEAR: gv_message.

  CALL FUNCTION 'Z_FI_CREATE_DOC'
    EXPORTING
      rfbifunct       = 'C'
      cmode           = p_mode
    TABLES
      ibkpf           = gt_bk
      ibseg           = gt_bs
      t_fimsg         = gt_fimsg
      iwith           = gt_wi
      ittax           = gt_ta
    EXCEPTIONS
      no_posting_data = 1
      error_mode      = 2
      posting_error   = 3
      OTHERS          = 4.


  " 결과 화면에 출력 및 db update
  " 성공건이 있는지 확인
  READ TABLE gt_fimsg WITH KEY msgid = 'F5' msgty = 'S' msgno = '312'.
  IF sy-subrc = 0.
    " 성공 건
    PERFORM message_text_build USING gt_fimsg-msgid
                                 gt_fimsg-msgno
                                 gt_fimsg-msgv1
                                 gt_fimsg-msgv2
                                 gt_fimsg-msgv3
                                 gt_fimsg-msgv4
                               CHANGING gv_message.       "결과 메시지
    CASE pv_val.
      WHEN 'YY'.
        <fs_disp>-belnr1 = gt_fimsg-msgv1. "전표번호
      WHEN 'ZZ'.
        <fs_disp>-belnr3 = gt_fimsg-msgv1. "전표번호 매출원가

    ENDCASE.

    <fs_disp>-icon = '@08@'." 성공
    <fs_disp>-msg =  gv_message ."

    PERFORM save_1260.


  ELSE.
    READ TABLE gt_fimsg WITH KEY msgty = gc_e.
    IF sy-subrc NE 0 .
      CLEAR lv_lines.
      DESCRIBE TABLE gt_fimsg LINES lv_lines.
      READ TABLE gt_fimsg INDEX lv_lines.
    ENDIF.
*    READ TABLE GT_FIMSG INDEX 1.
    " 실패건
    PERFORM message_text_build USING gt_fimsg-msgid
                                 gt_fimsg-msgno
                                 gt_fimsg-msgv1
                                 gt_fimsg-msgv2
                                 gt_fimsg-msgv3
                                 gt_fimsg-msgv4
                               CHANGING gv_message.       "결과 메시지
    <fs_disp>-icon = '@0A@'."에러
    <fs_disp>-msg =  gv_message ."에러

    gv_error = abap_true. "각 단계 ERROR체크
  ENDIF.

  CLEAR: gt_bk,gt_bk[],
      gt_bs, gt_bs[],
      gt_fimsg, gt_fimsg[].
ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPDATE_DATA_RTN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PV_VAL
*&---------------------------------------------------------------------*
FORM update_data_rtn  USING    pv_val.
  DATA: ls_t1260 LIKE zcot1260.

  CASE pv_val .
    WHEN 'YY'.

      UPDATE  zcot1260 SET belnr1 = ''
        WHERE bukrs  = <fs_disp>-bukrs
          AND spmon  = <fs_disp>-spmon
          AND fmatnr = <fs_disp>-fmatnr
          AND werks  = <fs_disp>-werks.

      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.
      ELSE.
        ROLLBACK WORK.
      ENDIF.

    WHEN 'RR'.
      UPDATE  zcot1260 SET belnr3 = ''
      WHERE bukrs  = <fs_disp>-bukrs
        AND spmon  = <fs_disp>-spmon
        AND fmatnr = <fs_disp>-fmatnr
        AND werks  = <fs_disp>-werks.

      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form MESSAGE_TEXT_BUILD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_FIMSG_MSGID
*&      --> GT_FIMSG_MSGNO
*&      --> GT_FIMSG_MSGV1
*&      --> GT_FIMSG_MSGV2
*&      --> GT_FIMSG_MSGV3
*&      --> GT_FIMSG_MSGV4
*&      <-- GV_MESSAGE
*&---------------------------------------------------------------------*
FORM message_text_build  USING   pv_msgid
                        pv_msgnr
                        pv_msgv1
                        pv_msgv2
                        pv_msgv3
                        pv_msgv4
               CHANGING pv_mesg.

  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      msgid               = pv_msgid
      msgnr               = pv_msgnr
      msgv1               = pv_msgv1
      msgv2               = pv_msgv2
      msgv3               = pv_msgv3
      msgv4               = pv_msgv4
    IMPORTING
      message_text_output = pv_mesg.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CLEAR_ITAB
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM clear_itab .
  CLEAR: gt_bk,gt_bk[],
         gt_bs, gt_bs[],
         gs_ibkpf,gt_ibkpf[],
         gs_ibseg,gt_ibseg[],
         gs_iselk,gt_iselk[],
         gs_ibselp, gt_ibselp[],
         gs_selp, gt_selp[],
         gs_display , gv_exit , gs_rows .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_SELECT_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_select_data  USING pv_val.
  CLEAR : gs_rows , gs_display .
  LOOP AT gt_rows INTO gs_rows.
    READ TABLE gt_display ASSIGNING <fs_disp> INDEX gs_rows-index.
    CASE pv_val .
      WHEN 'RR' ."역분개시  로직 넣기  """"""
        "전표 처리 상태 체크
        IF <fs_disp>-belnr1 IS NOT INITIAL AND <fs_disp>-belnr3  IS NOT INITIAL.

          gv_exit = gc_x.
*          MESSAGE S000 WITH <FS_DISP>-PSPNR TEXT-M02 DISPLAY LIKE 'E' .EXIT .
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

  ENDLOOP .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CANCLE_DATA_RTN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM cancle_data_rtn .
  DATA  ls_bseg  LIKE bseg.

  LOOP AT gt_rows INTO gs_rows.

    CLEAR : gs_info .
    READ TABLE gt_display ASSIGNING <fs_disp> INDEX gs_rows-index.


    IF <fs_disp>-belnr3 IS NOT INITIAL.
      gs_info-belnr = <fs_disp>-belnr3.
      gs_info-gjahr = <fs_disp>-spmon+0(4).
      PERFORM cancel_doc USING 'RR'.
    ENDIF.

    IF <fs_disp>-belnr1 IS NOT INITIAL.
      CLEAR : gs_info.
      gs_info-belnr = <fs_disp>-belnr1.
      gs_info-gjahr = <fs_disp>-spmon+0(4).
      PERFORM cancel_doc USING 'YY'.

    ENDIF.
    CLEAR : gs_info , gv_exit.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CANCEL_DOC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM cancel_doc USING pv_val.

* 역분개 여부 체크
  SELECT SINGLE *
  INTO @DATA(lv_bkpf)
   FROM bkpf
   WHERE bukrs = @pa_bukrs
     AND belnr = @gs_info-belnr
     AND gjahr = @gs_info-gjahr.

  IF lv_bkpf-awref_rev IS NOT  INITIAL. " 이미역분개됨
    gs_info-result =  'S'.
    gs_info-rbelnr = lv_bkpf-stblg.  " 역분개 전표번호
  ELSE.

    CLEAR :  gv_exit.
    CALL FUNCTION 'Z_FI_CANCEL_DOC_BDC'
      EXPORTING
        i_bukrs   = pa_bukrs
        i_belnr   = gs_info-belnr
        i_gjahr   = gs_info-gjahr
*       I_BUDAT   = LS_T0250-DSDAT
        i_stgrd   = '03'
      IMPORTING
        e_result  = gs_info-result
        e_rbelnr  = gs_info-rbelnr
        e_err_txt = gs_info-err_txt
      EXCEPTIONS
        no_belnr  = 1
        OTHERS    = 2.

  ENDIF.


  IF gs_info-result EQ 'S'.
    <fs_disp>-icon = '@08@'." 성공
    CASE pv_val .
      WHEN  'YY'. "
        <fs_disp>-belnr1 = space." 성공

      WHEN  'RR'. "
        <fs_disp>-belnr3 = space." 성공

    ENDCASE.

    "LOG TABLE UPDATE
    PERFORM  update_data_rtn USING pv_val.

  ELSE.
    gv_exit = abap_true. "ERROR FLAG.
    <fs_disp>-icon = '@0A@'." 에러
    <fs_disp>-msg = gs_info-err_txt." 에러
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MONAT_F4
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM monat_f4 USING pv_spmon.
  DATA : lv_retcode      LIKE sy-subrc.
  DATA : lv_spmon LIKE  isellist-month,
         lv_subrc LIKE  sy-subrc.
  CALL FUNCTION 'CONVERSION_EXIT_PERI_INPUT'
    EXPORTING
      input  = pv_spmon
    IMPORTING
      output = lv_spmon.

  IF lv_spmon IS INITIAL.
    lv_spmon = gv_datum(6).
  ENDIF.

  CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
    EXPORTING
      actual_month               = lv_spmon
*     LANGUAGE                   = 'E'
    IMPORTING
      selected_month             = lv_spmon
      return_code                = lv_subrc
    EXCEPTIONS
      factory_calendar_not_found = 01
      holiday_calendar_not_found = 02
      month_not_found            = 03.

  IF sy-subrc = 0 AND lv_subrc = 0.
    pv_spmon = lv_spmon.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_AUTH_COND
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_auth_cond .
  DATA : lv_type(1) .
  CLEAR : gv_exit , gv_message  .

  CALL FUNCTION 'ZCA_PRG_AUTH_CHECK'
    EXPORTING
      i_bname    = sy-uname
      i_module   = 'CO'
      i_bukrs_fi = pa_bukrs
    IMPORTING
      e_type     = lv_type
      e_message  = gv_message
      e_exit     = gv_exit.

  IF lv_type EQ gc_e.
    MESSAGE s000 WITH gv_message DISPLAY LIKE gc_e.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F4_P_MONTH
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f4_p_month .
  DATA: BEGIN OF mf_dynpfields OCCURS 1.
          INCLUDE STRUCTURE dynpread.
        DATA: END   OF mf_dynpfields.
  DATA: mf_returncode LIKE sy-subrc,
        mf_monat      LIKE isellist-month,
        mf_hlp_repid  LIKE sy-repid.
  FIELD-SYMBOLS: <mf_feld>.

  GET CURSOR FIELD mf_dynpfields-fieldname.
  APPEND mf_dynpfields.
  mf_hlp_repid = sy-repid.
  DO 2 TIMES.
    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname               = mf_hlp_repid
        dynumb               = sy-dynnr
      TABLES
        dynpfields           = mf_dynpfields
      EXCEPTIONS
        invalid_abapworkarea = 01
        invalid_dynprofield  = 02
        invalid_dynproname   = 03
        invalid_dynpronummer = 04
        invalid_request      = 05
        no_fielddescription  = 06
        undefind_error       = 07.
    IF sy-subrc = 3.
      mf_hlp_repid = 'SAPLALDB'.
    ELSE.
      READ TABLE mf_dynpfields INDEX 1.
      TRANSLATE mf_dynpfields-fieldvalue USING '_ '.
      EXIT.
    ENDIF.
  ENDDO.
  IF sy-subrc = 0.
    CALL FUNCTION 'CONVERSION_EXIT_PERI_INPUT'
      EXPORTING
        input  = mf_dynpfields-fieldvalue
      IMPORTING
        output = mf_monat.
    IF mf_monat IS INITIAL.
      mf_monat = sy-datlo(6).
    ENDIF.
    CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
      EXPORTING
        actual_month               = mf_monat
      IMPORTING
        selected_month             = mf_monat
        return_code                = mf_returncode
      EXCEPTIONS
        factory_calendar_not_found = 01
        holiday_calendar_not_found = 02
        month_not_found            = 03.
    IF sy-subrc = 0 AND mf_returncode = 0.
      CALL FUNCTION 'CONVERSION_EXIT_PERI_OUTPUT'
        EXPORTING
          input  = mf_monat
        IMPORTING
          output = mf_dynpfields-fieldvalue.
**** 날짜 . 없애기 추가
*      REPLACE '.' IN MF_DYNPFIELDS-FIELDVALUE  WITH ''.

      CONDENSE mf_dynpfields-fieldvalue.
******

      COLLECT mf_dynpfields.
      CALL FUNCTION 'DYNP_VALUES_UPDATE'
        EXPORTING
          dyname               = mf_hlp_repid
          dynumb               = sy-dynnr
        TABLES
          dynpfields           = mf_dynpfields
        EXCEPTIONS
          invalid_abapworkarea = 01
          invalid_dynprofield  = 02
          invalid_dynproname   = 03
          invalid_dynpronummer = 04
          invalid_request      = 05
          no_fielddescription  = 06
          undefind_error       = 07.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_INPUT_VALID
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_input_valid .

  SELECT SINGLE werks
    FROM t001w AS a INNER JOIN  t024e  AS b
      ON a~ekorg = b~ekorg
  WHERE b~bukrs  = @pa_bukrs
    AND a~werks =  @pa_werks
    INTO @DATA(lv_werks).

  IF sy-subrc EQ 0.
    CLEAR gv_exit .
  ELSE.
    gv_exit = abap_true .
    MESSAGE s000(zco01) WITH TEXT-e01  DISPLAY LIKE gc_e.
    STOP.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHK_POSTED
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_FLAG
*&---------------------------------------------------------------------*
FORM chk_posted  USING    pc.

  SELECT SINGLE *
   FROM zcot1260
  WHERE bukrs  = @pa_bukrs
   AND spmon = @pa_month
   AND werks = @pa_werks
   AND ( belnr1 IS NOT INITIAL OR
       belnr3 IS NOT INITIAL )
  INTO @DATA(ls_1260).

  IF sy-subrc EQ 0.


    pc = 'X'.

  ELSE.

**    DELETE FROM ZCOT1260 WHERE  BUKRS = PA_BUKRS
**                          AND  SPMON = PA_MONTH
**                          AND  WERKS = PA_WERKS.
**
**    IF SY-SUBRC EQ 0.
**      COMMIT WORK AND WAIT.
**    ELSE.
**      ROLLBACK WORK.
**    ENDIF.

    CLEAR pc.
  ENDIF.




ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_DATA_RTN_1260
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM selected_data_rtn_1260 .

* FI  제품매출액  1000  402101001
*      제품매출액_수출 1000  402101003    추가로 표시

  DATA : lv_lifnr TYPE lfa1-lifnr.


  PERFORM get_fi_rev_gl.


  SELECT *
    FROM zcot1260
    WHERE bukrs = @pa_bukrs
      AND spmon = @pa_month
      AND werks = @pa_werks
    INTO TABLE @DATA(lt_1260).


  LOOP AT lt_1260 ASSIGNING FIELD-SYMBOL(<$fs>) .

    MOVE-CORRESPONDING <$fs> TO gs_display.

* *자재그룹명
    SELECT SINGLE wgbez INTO gs_display-wgbez
      FROM t023t
     WHERE matkl = gs_display-matkl.

* 제품자재명
    SELECT SINGLE maktx INTO gs_display-fmatnr_maktx
      FROM makt
     WHERE spras = sy-langu
      AND matnr = gs_display-fmatnr.

* 원자재  자재명
    SELECT SINGLE maktx INTO gs_display-rmatnr_maktx
      FROM makt
     WHERE spras = sy-langu
      AND matnr = gs_display-rmatnr.

*  구매처명
    SELECT SINGLE name1 INTO gs_display-name1
      FROM lfa1
    WHERE lifnr = gs_display-lifnr.



    IF gs_display-belnr1 IS NOT INITIAL OR
      gs_display-belnr3 IS NOT INITIAL.

      gs_display-icon = icon_led_green.
    ELSE.
      gs_display-icon = icon_led_yellow.

    ENDIF.

*역분개 여부 추가
    IF gs_display-belnr1 IS NOT INITIAL OR
        gs_display-belnr3 IS NOT INITIAL.
      PERFORM check_reverse USING gs_display.
    ENDIF.


    SELECT SINGLE mtart
          INTO gs_display-mtart
          FROM mara
        WHERE matnr = gs_display-fmatnr.



    APPEND gs_display TO gt_display.
    CLEAR gs_display.
  ENDLOOP.


*  fi 매출 보이게 추가  20200903...


  SORT gt_display BY fmatnr.

  CLEAR gs_fi_rev.
  LOOP AT gt_fi_rev INTO gs_fi_rev.

    READ TABLE gt_display ASSIGNING FIELD-SYMBOL(<$fs2>)
                       WITH KEY fmatnr = gs_fi_rev-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      <$fs2>-dmbtr =  gs_fi_rev-hsl * ( -1 ).

    ELSE.

      CLEAR  gs_display.
      SELECT SINGLE mtart
             INTO gs_display-mtart
             FROM mara
           WHERE matnr = gs_fi_rev-matnr.


      gs_display-fmatnr = gs_fi_rev-matnr.
      gs_display-dmbtr =  gs_fi_rev-hsl * ( -1 ).
      gs_display-twaer = 'KRW'.


*MODIFY 2021 02.23   REQ  BY 강현수K..

      CLEAR gs_t023t.
      SELECT SINGLE
              b~matkl AS matkl
              b~wgbez AS wgbez
              a~wrkst  AS wrkst  " 제품의 구매처
              c~maktx AS fmatnr_maktx " 자재명
       INTO CORRESPONDING FIELDS OF  gs_display
         FROM mara AS a INNER JOIN  t023t AS b
         ON a~matkl = b~matkl INNER JOIN makt AS c ON
          a~matnr = c~matnr
         WHERE a~matnr =  gs_display-fmatnr " 제품의 자재그룹
         AND b~spras = sy-langu.

**    제품명
*      SELECT SINGLE MAKTX  MATKL
*        INTO GS_DISPLAY-FMATNR_MAKTX
*        FROM MAKT
*      WHERE MATNR =  GS_DISPLAY-FMATNR
*        AND SPRAS = SY-LANGU.


*공급업체

      SELECT SINGLE matnr2  INTO gs_display-rmatnr
        FROM zmmt0600
       WHERE matnr = gs_display-fmatnr
        AND lvorm = ''.

      IF sy-subrc EQ 0.
        PERFORM get_lifnr USING gs_display.



        lv_lifnr = |{ gs_display-lifnr  ALPHA = IN }|.



*  구매처명
        SELECT SINGLE name1 INTO gs_display-name1
          FROM lfa1
        WHERE lifnr = lv_lifnr .
*        WHERE lifnr = gs_display-lifnr.

      ELSE.
        gs_display-msg =  gs_display-msg && 'ZMMT0610 매핑 원재료 없음,'.
      ENDIF.

**END BY  BSGSM_FCM.....

      gs_display-msg  =    gs_display-msg && '수불 테이블에 존재하지 않는 FI 자재별  매출추가 '.

      APPEND gs_display TO  gt_display.

      SORT gt_display BY fmatnr.


    ENDIF.

  ENDLOOP.


**기표시 UNB3 만 기표로 변경되어 아래 로직 필요없음 ..!! 20200911..

**  IF P_CHK  IS NOT INITIAL.
**
**    DELETE GT_DISPLAY WHERE MTART  NE 'UNB3'.
**  ENDIF.
***



ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_DATA_RTN_ACDOCA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM selected_data_rtn_acdoca  USING pv.

****      원재료비차이

**  위에서 집계한 원재료비의 금액이
**제)원재료비  1900  505101001
**시산표상(ACDOCA테이블 )의 금액과 차이가 발생하면
* 이는 <배부방법1: 아래설명> 에 의하여 배부함
**<배부방법1>	재료비 계산 버튼 클릭시 화면에 제공

  DATA : lv_acct1 TYPE acdoca-racct.
  DATA : lv_acct2 TYPE acdoca-racct.
  DATA : lv_acct3 TYPE acdoca-racct.



  lv_acct1 =  '0505101001'. " 원재료비  전표 와 cbo의 차이

  REFRESH gr_racct. CLEAR gr_racct.
  _set_ranges :   gr_racct 'I' 'EQ'  lv_acct1 ''.


  """    @GT_RAW2.  " 원자재별 금액 합계,


  PERFORM get_raw.

  "## 원재료비 차이  total    GV_DIFF_ACDOCA_SUM

  PERFORM compute_diff_1.


  "## 가격차이 전표 가져오기  직과   ==> 배부방법 --> 2

  CLEAR gr_racct. REFRESH gr_racct.

  lv_acct2 =  '0504101003'. " 원재료  구매가격차이
  _set_ranges :   gr_racct  'I' 'EQ'  lv_acct2 ''.

  PERFORM compute_diff_2 .


  "## 재고조정 전표 가져오기


* 505101002   재고조정계정
  CLEAR gr_racct. REFRESH gr_racct.
  lv_acct3 =  '0505101002  '. "  재고조정계정
  _set_ranges :   gr_racct  'I' 'EQ'  lv_acct3 ''.

  PERFORM compute_diff_3 .



  "## 노무비  전표 가져오기

*  노무비  ACDOCA테이블에서 계정~ 사이에 있는 계정의 금액을 합산하여 생산 수량 비율로 금액을 배부함
*   (단수차이 주의)505201001 ~ 505432001
*   <배부방법2>
*   단수차이가 발생시는 금액이 가장 큰 항목에 반영  노무비,경비 배부 버튼 클릭시 계산
*  CLEAR GR_RACCT. REFRESH GR_RACCT.
**
**  LV_ACCT4 =  '0505201001' .
**  LV_ACCT5 =  '0505432001' .
**  _SET_RANGES :   GR_RACCT  'I' 'BT'  LV_ACCT4   LV_ACCT5.
**  spec 있는 계정 적용시 노무비와 경비 겹침.. 재무제표버전트리 활용



  IF gr_gkont1[] IS NOT INITIAL.

    PERFORM compute_diff_4 .
  ELSE.
    pv = 'X'.
  ENDIF.

  "##경비  전표 가져오기
**ACDOCA테이블에서 계정505301001~505432001 사이에 있는 계정의 금액을 합산하여
** 생산 수량 비율로 금액을 배부함 (단수차이 주의)
**<배부방법2>
**단수차이가 발생시는 금액이 가장 큰 항목에 반영  노무비,경비 배부 버튼 클릭시 계산


*  CLEAR GR_RACCT. REFRESH GR_RACCT.
**
**  LV_ACCT6 =  '0505301001' .
**  LV_ACCT7 =  '0505432001' .
**  _SET_RANGES :   GR_RACCT  'I' 'BT'  LV_ACCT6   LV_ACCT7.


  IF gr_gkont2[] IS NOT INITIAL.
    PERFORM compute_diff_5 .
  ELSE.
    pv = 'X'.
  ENDIF.






ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_MASTER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_master .

  PERFORM get_werks_name.

  PERFORM get_mmt0600.

  PERFORM get_gsber.



**  재무제표 버전트리  노무, 경비

*
*  CLEAR : GR_GKONT1, GR_GKONT2.
*  REFRESH : GR_GKONT1, GR_GKONT2.

*개발 200 1000 재무제표버전의 손익의 위치가 달라져서  수정
*  개발시점의 위치과 20200624 위치가 다름
* 300 CTS  되면서 발생 프리텍은 200 에서 프로젝 수행
**  LOOP AT NODE_TAB .
**
**    IF NODE_TAB-TYPE = 'P' AND NODE_TAB-TLEVEL = '05'.
**      GUBUN = NODE_TAB-TEXT.
**
**    ENDIF.
**
**    IF NODE_TAB-TYPE = 'A'.
**      CASE GUBUN.
**
**        WHEN  C_LEVEL4_L.   "노무비
**          GR_GKONT1-LOW    = NODE_TAB-NAME+4(10).
**          GR_GKONT1-HIGH   = NODE_TAB-NAME+17(10).
**          GR_GKONT1-SIGN   = 'I'.
**          GR_GKONT1-OPTION = 'BT'.
**          APPEND GR_GKONT1.CLEAR GR_GKONT1.
**
**        WHEN  C_LEVEL4_M.   "경비
**
**          GR_GKONT2-LOW    = NODE_TAB-NAME+4(10).
**          GR_GKONT2-HIGH   = NODE_TAB-NAME+17(10).
**          GR_GKONT2-SIGN   = 'I'.
**          GR_GKONT2-OPTION = 'BT'.
**          APPEND GR_GKONT2.CLEAR GR_GKONT2.
**
**      ENDCASE.
**
**    ENDIF.
**
**  ENDLOOP.

  PERFORM get_range_gl.  " BSGSM_FCM 20200624


  CLEAR : gv_exe_flag_raw.
  CLEAR : gv_exe_flag_labor.
  CLEAR : gv_exe_flag_cogs.

*CONVERSION_EXIT_ABPSN_INPUT
*CONVERSION_EXIT_ABPSN_OUTPUT


  CLEAR  gr_posid.
  REFRESH gr_posid.

  gr_posid-sign = 'I'.
  gr_posid-option = 'CP'.
  gr_posid-low = 'M*'.

  APPEND gr_posid.

* 플랜트별  wbs 설정  m  로 시작하는거만...

**  SELECT POSID
**    FROM PRPS
**   WHERE PGSBR  IN @GR_GSBER
**     AND POSID IN @GR_POSID
*    INTO TABLE @GT_WBS.

**  20200318  변경 SPEC

  PERFORM get_wbs_zsdt0120.
** END BY  20200318



  CLEAR  gr_posid.
  REFRESH gr_posid.

  gr_posid-sign = 'I'.
  gr_posid-option = 'EQ'.

  DATA : lv_wbs LIKE prps-posid.

  LOOP AT gt_wbs ASSIGNING FIELD-SYMBOL(<wbs>).

    gr_posid-low =  <wbs>-posid.
    COLLECT gr_posid.

  ENDLOOP.


* 매출원가 계정 WBS

  SELECT a~posid, a~saknr, b~txt20
    FROM zcot1230 AS a
   INNER JOIN skat AS b
      ON a~saknr = b~saknr
     AND b~spras = @sy-langu
     AND b~ktopl = '1000'
    INTO TABLE @gt_zcot1230
   WHERE a~bukrs = @pa_bukrs.







ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_WERKS_NAME
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_werks_name .
  SELECT SINGLE  a~name1
    FROM t001w AS a
   WHERE a~werks = @pa_werks
    INTO  @DATA(gv_we_nm).



ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_BL1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_bl1 .


  SELECT SINGLE name1
     FROM t001w AS a INNER JOIN  t024e  AS b
       ON a~ekorg = b~ekorg
   WHERE b~bukrs  = @pa_bukrs
     AND a~werks =  @pa_werks
     INTO @pa_wenm.

  IF sy-subrc EQ 0.
    CLEAR gv_exit .

    gv_we_nm = pa_wenm.

  ELSE.
    SET CURSOR FIELD 'PA_WERKS'.
    MESSAGE e027  WITH TEXT-e02.

  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_SALES_QTY_NETWR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_sales_qty_netwr .


  e_spmon = pa_month.
  e_werks = pa_werks.


  CLEAR : gt_sd_result, gt_sd_result[].
  CLEAR : gt_sd_result2, gt_sd_result2[].

  TRY.

      CALL FUNCTION 'ZSD_SALES_QTY_NETWR_CALCULATE'
        EXPORTING
          i_spmon   = e_spmon
          i_werks   = e_werks
*         I_MATNR   =
        TABLES
          t_result  = gt_sd_result
          t_result2 = gt_sd_result2.

  ENDTRY.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_MMT0610
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_mmt0600 .

  SELECT *
   FROM zmmt0600
   WHERE lvorm <> 'X'
    INTO TABLE @gt_zmmt600.


*제품 순으로 SORT
  SORT gt_zmmt600 BY matnr matnr2.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SALES_QTY_NETWR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_DISPLAY
*&---------------------------------------------------------------------*
FORM get_matnr_name  USING    ps_display STRUCTURE gs_display.



* 자재그룹명

  SELECT SINGLE b~matkl b~wgbez INTO ( ps_display-matkl, ps_display-wgbez )
      FROM mara AS a INNER JOIN  t023t AS b
       ON a~matkl = b~matkl
    WHERE a~matnr = ps_display-rmatnr
      AND b~spras = sy-langu.



* 제품에 매핑된 원자재...
  READ TABLE gt_zmmt600 ASSIGNING FIELD-SYMBOL(<$t600>) WITH KEY matnr =   ps_display-fmatnr BINARY SEARCH.
  IF sy-subrc EQ 0.
    gs_display-rmatnr = <$t600>-matnr2. " 원재료
*     원자재명
    SELECT SINGLE maktx
      INTO ps_display-rmatnr_maktx
      FROM makt
    WHERE matnr = ps_display-rmatnr
      AND spras = sy-langu.


    ps_display-icon =  icon_dummy.

  ELSE.

***
***ROH1	[T] 1.원자재
***HAW1	[T] 2.상품
***UNB1	[T] 3.비평가 원자재
***UNB2	[T] 4.비평가 반제품
***UNB3	[T] 5.비평가 제품
***SER1	[T] 6.서비스
***NLG1	[T] 폐기물
**최초 실행시 '제품'에 대해서만 check , 상품과 서비스는 화면에서 조회는 하지만 체크대상에서는 제외 )
*20200318  MAIL 수신 수정 SEPC 추가

    ps_display-msg = 'ZMMT600 매핑 자재 누락'.
    ps_display-icon =  icon_resource  .


  ENDIF.

*    제품명
  SELECT SINGLE maktx
    INTO ps_display-fmatnr_maktx
    FROM makt
  WHERE matnr = ps_display-fmatnr
    AND spras = sy-langu.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SALES_AMONT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_DISPLAY
*&---------------------------------------------------------------------*
FORM set_sales_amont  USING    ps_display STRUCTURE gs_display.


  SORT gt_sd_result2 BY matnr.

  READ TABLE  gt_sd_result2 ASSIGNING FIELD-SYMBOL(<$sd>)
        WITH  KEY  matnr = ps_display-fmatnr BINARY SEARCH.

  CHECK sy-subrc EQ 0.

  ps_display-werks   = <$sd>-werks.
  ps_display-twaer   = <$sd>-waers. " 항상 KRW

  ps_display-fmatnr  = <$sd>-matnr.  "제품
  ps_display-smeins  = <$sd>-vrkme. " KG
  ps_display-smenge  = <$sd>-fkimg. " 수량
  ps_display-swrbtr  = <$sd>-netwr_l. " 매출액  원화


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_WGBEZ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_DISPLAY
*&---------------------------------------------------------------------*
FORM get_wgbez  USING    ps_display STRUCTURE gs_display.

*제품의 자재그룹....

  READ TABLE gt_t023t ASSIGNING FIELD-SYMBOL(<$t023t>) WITH KEY  fmatnr =  ps_display-fmatnr BINARY SEARCH.

  IF sy-subrc EQ 0.

    ps_display-matkl = <$t023t>-matkl.
    ps_display-wgbez  = <$t023t>-wgbez.
    ps_display-mtart  = <$t023t>-mtart.

  ELSE.

    CLEAR gs_t023t.
    SELECT SINGLE a~matnr AS fmatnr
            b~matkl AS matkl
            b~wgbez AS wgbez
            a~wrkst  AS wrkst  " 제품의 구매처
            a~mtart  AS mtart
     INTO CORRESPONDING FIELDS OF  gs_t023t
       FROM mara AS a INNER JOIN  t023t AS b
       ON a~matkl = b~matkl
       WHERE a~matnr =  ps_display-fmatnr " 제품의 자재그룹
       AND b~spras = sy-langu.



    IF sy-subrc EQ 0.
      APPEND gs_t023t TO gt_t023t.

      ps_display-fmatnr = gs_t023t-fmatnr.
      ps_display-matkl = gs_t023t-matkl.
      ps_display-wgbez = gs_t023t-wgbez.
      ps_display-mtart  = gs_t023t-mtart.

    ELSE.

      CLEAR :    ps_display-matkl,
                 ps_display-wgbez,
                 ps_display-mtart.

    ENDIF.






  ENDIF.




ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_GSBER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_gsber .


  SELECT * INTO TABLE gt_t134g
    FROM t134g
  WHERE werks = pa_werks.

**19AA 플랜트는 발생안한다   김호준위원  20200306

  REFRESH gr_gsber.
  CLEAR gr_gsber.

  gr_gsber-sign = 'I'.
  gr_gsber-option = 'EQ'.

  LOOP AT gt_t134g ASSIGNING FIELD-SYMBOL(<$gg>).

    gr_gsber-low = <$gg>-gsber.
    COLLECT  gr_gsber.

  ENDLOOP.


ENDFORM.

*&---------------------------------------------------------------------*
*& Form COMPUTE_DIFF_1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM compute_diff_1 .

  CLEAR gv_diff_acdoca_sum.     " mm - fi  차이
  CLEAR gv_tot_mm0610_sum.      "  MM  당월 투입원재료 SUM
  CLEAR gv_505101001_sum .      "  "  전표 원재료비 계정 SUM


*전표의 원자재별 SUM.


  SELECT
           "A~MATNR AS MATNR,  " 원자재
          SUM( a~hsl )  AS fi_hsl
     FROM acdoca   AS a
    WHERE a~rldnr = '0L'
      AND a~rbukrs = @pa_bukrs
      AND a~werks = @pa_werks  " 플랜트
      AND a~budat IN @gr_budat
      AND a~racct IN @gr_racct
      AND a~ps_posid IN @gr_posid
*      AND A~XREVERSING IS INITIAL   @  역분개 포함으로 수정
*      AND A~XREVERSED IS INITIAL
*      AND A~AWREF_REV = @SPACE
      INTO CORRESPONDING FIELDS OF TABLE @gt_505101001_acdoca.


  SELECT   SUM( a~hsl )  AS hsl
     FROM acdoca   AS a
    WHERE a~rldnr = '0L'
      AND a~rbukrs = @pa_bukrs
      AND a~werks = @pa_werks  " 플랜트
      AND a~budat IN @gr_budat
      AND a~racct IN @gr_racct
      AND a~ps_posid IN @gr_posid
*      AND A~XREVERSING IS INITIAL
*      AND A~XREVERSED IS INITIAL
*      AND A~AWREF_REV = @SPACE
       INTO  @DATA(gv_505101001_sum_fi)   .


  CLEAR : gt_diff_001[], gt_diff_001.


  SELECT
          a~werks  AS werks,
          a~ps_posid AS ps_posid,
          a~gjahr  AS gjahr,
          a~belnr  AS belnr,
          a~buzei  AS buzei,
          a~racct  AS racct,
          b~txt20  AS txt20,

         'KRW'  AS twaer,
          a~hsl  AS hsl,
          a~matnr AS matnr,
          c~maktx AS maktx,

          a~awref AS awref,
          a~awref_rev AS awref_rev
     FROM acdoca   AS a  INNER JOIN skat AS b
        ON a~racct = b~saknr
                         INNER JOIN makt AS c
        ON a~matnr = c~matnr
        AND b~spras = c~spras
    WHERE a~rldnr = '0L'
      AND a~rbukrs = @pa_bukrs
      AND a~werks = @pa_werks
      AND a~budat IN @gr_budat
      AND a~racct IN @gr_racct
      AND a~ps_posid IN @gr_posid
      AND b~spras = @sy-langu
      AND b~ktopl = '1000'
      AND c~spras =  @sy-langu
*      AND A~AWREF_REV = @SPACE
*      AND A~XREVERSING IS INITIAL
*      AND A~XREVERSED IS INITIAL
     ORDER BY awref
      INTO CORRESPONDING FIELDS OF TABLE @gt_diff_001.
*
  DATA : lv_tabix TYPE sytabix.

  SORT gt_raw BY rmblnr . " 자재문서 번호

  CLEAR : gt_diff_001b, gt_diff_001b[]. " 재료비 직접  귀속 대상 전표리스트


  CLEAR gv_505101001_sum  .

  LOOP AT gt_diff_001  ASSIGNING FIELD-SYMBOL(<d001>).
    lv_tabix = sy-tabix.

    <d001>-twaer = 'KRW'.
    READ TABLE gt_raw ASSIGNING FIELD-SYMBOL(<c1>) WITH KEY rmblnr = <d001>-awref BINARY SEARCH.
    IF sy-subrc EQ 0.
*          DELETE GT_DIFF_001 INDEX LV_TABIX.

      <d001>-msg_text = 'ZMM0610 존재'.
    ELSE.

*    변경 SPEC..  20200311
*"재료비 차이
*mmt0610에서 처리안한거 "
*505101001..
*"ACDOCA에서 계정이 50501001로 기표된 전표들증  ACDOCA-AWREF <>ZMMT0610-RMBLNR 인 것들이 대상임
*"
*
      gv_505101001_sum  = gv_505101001_sum  +  <d001>-hsl. "  불일치 재료비 누적

      APPEND <d001> TO gt_diff_001b.  " 재료비 직접 귀속 대상 테이블..

    ENDIF.

  ENDLOOP.


  READ TABLE gt_raw2 ASSIGNING FIELD-SYMBOL(<raw>) INDEX 1.

  IF sy-subrc EQ 0.

    <raw>-twaer = 'KRW'.
    <raw>-fi_hsl =  gv_505101001_sum_fi .  "FI 총기표 금액
    <raw>-diff =   gv_505101001_sum. .
  ENDIF.




ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_LFA1_NAME
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_DISPLAY
*&---------------------------------------------------------------------*
FORM get_lfa1_name  USING    ps_display STRUCTURE gs_display.

  DATA : lv_lifnr LIKE lfa1-lifnr.


*  MM SPEC에서 가져옴  펑션 개발이 안되어서..  20200308.
* 기본 자재 필드인데  구매처를 넣어준다고..???  <질문하기>>


  CLEAR gs_lfa1.

  READ TABLE gt_lfa1 INTO gs_lfa1 WITH KEY rmatnr = ps_display-rmatnr BINARY SEARCH.


  IF sy-subrc EQ 0.


    ps_display-lifnr = gs_lfa1-lifnr.
    ps_display-name1 = gs_lfa1-name1.


  ELSE.


    CLEAR gs_lfa1.


    SELECT SINGLE   a~matkl   " 자재그룹
   FROM mara AS a
  WHERE a~matnr =  @ps_display-rmatnr
    AND a~lvorm = ' '
   INTO  @gs_lfa1-matkl .

    CLEAR lv_lifnr.
    SELECT SINGLE a~wrkst AS lifnr  " 기본뷰 2 의  기본자재
   FROM mara AS a
  WHERE a~matnr =  @ps_display-rmatnr
    AND a~lvorm = ' '
   INTO  @lv_lifnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_lifnr
      IMPORTING
        output = lv_lifnr.

    SELECT SINGLE  b~name1 AS name1
      FROM lfa1 AS b
     WHERE b~lifnr =  @lv_lifnr
      INTO @gs_lfa1-name1.


    IF sy-subrc EQ 0.

      gs_lfa1-lifnr = lv_lifnr.

      APPEND gs_lfa1 TO gt_lfa1.


      ps_display-lifnr = gs_lfa1-lifnr.
      ps_display-name1 = gs_lfa1-name1.


      SORT gt_lfa1 BY rmatnr.

    ELSE.
      CLEAR ps_display-lifnr.
      CLEAR ps_display-name1.

    ENDIF.


  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form COMPUTE_RATE345
*&---------------------------------------------------------------------*
*&해당원재료의 구매처와 자재그룹이 동일한 제품의 생산수량 비율로 배부
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM compute_rate3 .


  CLEAR: gt_rate3a, gt_rate3a[].
  CLEAR gv_fmatnr_menge_tot. "전체 생산총량

*    구매처별 자재그룹 제품별 비율
  LOOP AT gt_rate3 INTO gs_rate3.

    CLEAR gs_rate3a.
    MOVE-CORRESPONDING gs_rate3 TO gs_rate3a.
    COLLECT gs_rate3a INTO gt_rate3a.

    gv_fmatnr_menge_tot =    gv_fmatnr_menge_tot + gs_rate3-fmenge.  " 총생산수량..

  ENDLOOP.


  DATA : r3_tot_menge LIKE zmmt0610-fmenge.

  SORT gt_rate3a BY lifnr matkl.
  SORT gt_rate3 BY lifnr matkl fmatnr fmenge.


  READ TABLE gt_raw2 ASSIGNING FIELD-SYMBOL(<$raw>) INDEX 1.

  CHECK sy-subrc EQ 0.


  DATA : lv_rate(16) TYPE p   DECIMALS 9 .
  DATA : lv_rate_1(16) TYPE p   DECIMALS 9 .
  DATA : lv_rate_d(16) TYPE p   DECIMALS 9 .
  DATA : lv_rate_0(16) TYPE p   DECIMALS 9 .


  DATA : lv_texte(16).

  lv_text = '1.000000000'.

  CALL FUNCTION 'CHAR_FLTP_CONVERSION'
    EXPORTING
      string = lv_text "CHAR 변수
    IMPORTING
      flstr  = lv_rate_1. "DEC 변수





  LOOP AT gt_rate3a ASSIGNING FIELD-SYMBOL(<$3a>).

*
** 전체 총량에서  구매처별/자재그룹별 비율
*    IF <$3A>-FMENGE IS NOT INITIAL AND    GV_FMATNR_MENGE_TOT  IS NOT INITIAL.
*      <$3A>-RATE =    <$3A>-FMENGE  /   GV_FMATNR_MENGE_TOT.
*
*    ENDIF.

    CLEAR r3_tot_menge.

    CLEAR  lv_rate. " 1에서 시작
    lv_rate = lv_rate_1.

    LOOP AT gt_rate3 INTO gs_rate3 WHERE lifnr = <$3a>-lifnr AND matkl = <$3a>-matkl.

      r3_tot_menge  =  r3_tot_menge  +  gs_rate3-fmenge  .  " 구매처별 자재그룹별 총수량

    ENDLOOP.


    IF r3_tot_menge IS NOT INITIAL.

      LOOP AT gt_rate3 ASSIGNING FIELD-SYMBOL(<$r3>) WHERE lifnr = <$3a>-lifnr AND matkl =  <$3a>-matkl.

        CHECK   <$r3>-fmenge  IS NOT INITIAL.

        <$r3>-rate3 =   <$r3>-fmenge  /    r3_tot_menge .
        <$r3>-sub_menge  =      r3_tot_menge. "   부분 총수량
*      <$R3>-TOT_MENGE  =      GV_FMATNR_MENGE_TOT. "  총수량


        lv_rate = lv_rate -    <$r3>-rate3 .

      ENDLOOP.
    ENDIF.

    SORT gt_rate3  BY lifnr matkl fmatnr fmenge DESCENDING.

    IF lv_rate NE  lv_rate_0.  " zero 가 아니면

      LOOP AT  gt_rate3 ASSIGNING FIELD-SYMBOL(<$rr>)   WHERE  lifnr = <$3a>-lifnr
                                                          AND  matkl =  <$3a>-matkl.
        lv_rate_d   =  lv_rate_d + <$r3>-rate3 .

      ENDLOOP.


*비율합이 1이 아니면   큰수량에 더한다.

      READ TABLE gt_rate3  ASSIGNING FIELD-SYMBOL(<$rrr>) WITH KEY   lifnr = <$3a>-lifnr
                                                                    matkl =  <$3a>-matkl BINARY SEARCH.
      IF sy-subrc EQ 0.

**   차이 보정
        <$rrr>-rate3 =   <$rrr>-rate3  +       lv_rate.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form COMPUTE_DIFF_2
*&---------------------------------------------------------------------*
*& t
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM compute_diff_2.

  CLEAR gv_504101003_sum .      "  "  전표 가격차이 계정 SUM


*전표의 원자재별 SUM.


  CLEAR : gt_diff_002[], gt_diff_002.


  SELECT
          a~werks  AS werks,
          a~gjahr  AS gjahr,
          a~belnr  AS belnr,
          a~buzei  AS buzei,
          a~racct  AS racct,
          b~txt20  AS txt20,
         'KRW'  AS twaer,
          a~hsl  AS hsl,
          a~matnr AS matnr,
          c~maktx AS maktx,
          a~awref AS awref,
          a~ps_posid AS ps_posid,
          a~awref_rev AS awref_rev
     FROM acdoca   AS a  INNER JOIN skat AS b
        ON a~racct = b~saknr
                         INNER JOIN makt AS c
        ON a~matnr = c~matnr
        AND b~spras = c~spras
    WHERE a~rldnr = '0L'
      AND a~rbukrs = @pa_bukrs
      AND  a~werks = @pa_werks
      AND a~budat IN @gr_budat
      AND a~racct IN @gr_racct
      AND a~ps_posid IN @gr_posid
      AND b~spras = @sy-langu
      AND b~ktopl = '1000'
      AND c~spras =  @sy-langu
*      AND A~AWREF_REV = @SPACE
*      AND A~XREVERSING IS INITIAL
*      AND A~XREVERSED IS INITIAL
      INTO  CORRESPONDING FIELDS OF TABLE @gt_diff_002.


*BKPF  FI 직접 기표한 전표  자재가 없음

  SELECT    a~werks  AS werks,
            a~gjahr  AS gjahr,
            a~belnr  AS belnr,
            a~buzei  AS buzei,
            a~rbusa  AS gsber,
            a~racct  AS racct,
            a~hsl  AS hsl,
            a~awref AS awref,
            a~ps_posid AS ps_posid,
            b~txt20  AS txt20,
            'KRW'  AS twaer
 FROM acdoca   AS a    INNER JOIN skat AS b
        ON a~racct = b~saknr
WHERE a~rldnr = '0L'
  AND a~rbukrs = @pa_bukrs
  AND a~budat IN @gr_budat
  AND a~racct IN @gr_racct
  AND a~awtyp LIKE '%BKPF%'
  AND a~ps_posid IN @gr_posid
*  AND A~AWREF_REV = @SPACE
  AND b~spras = @sy-langu
  AND b~ktopl = '1000'
    INTO  TABLE @DATA(gt_002_bkpf).


  LOOP  AT gt_002_bkpf ASSIGNING FIELD-SYMBOL(<bkpf>).
    IF <bkpf>-gsber IN gr_gsber.
      CLEAR gs_acdoca.
      MOVE-CORRESPONDING <bkpf> TO gs_acdoca.

      APPEND gs_acdoca TO  gt_diff_002.
    ENDIF.
  ENDLOOP.


  SORT gt_diff_002 BY bukrs belnr gjahr buzei.
  DELETE ADJACENT DUPLICATES FROM gt_diff_002  COMPARING  bukrs belnr gjahr buzei.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_RAW
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_raw .

*
*원재료비차이
*	CBO의 원재료비와 시산(표준테이블)상의 재료비 금액과 차이가 날경우
*505101001   제품 있으면 직접 부과  없으면 배부방법1..


  SELECT  SUM( rdmbtr ) AS mm_rwrbtr  " 원자재 투입비
    FROM zmmt0610
   WHERE werks =  @pa_werks
     AND budat IN @gr_budat
     AND fmenge > 0    " 제품 수량 있는  항목
     AND lvorm <> 'X'  "삭제 지시자
    INTO CORRESPONDING FIELDS OF TABLE @gt_raw2.  " 원자재별 금액 합계


  SELECT  SUM( rdmbtr )  " 총 원자재 투입비
    FROM zmmt0610
    INTO   @gv_tot_mm0610_sum  " 원자재 SUM
    WHERE werks =  @pa_werks
    AND budat IN @gr_budat
    AND fmenge > 0    " 제품 수량 있는  항목
    AND lvorm <> 'X'.  "삭제 지시자


ENDFORM.
*&---------------------------------------------------------------------*
*& Form COMPUTE_DIFF_3
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM compute_diff_3 .


  CLEAR gv_505101002_sum .   " 재고조정차이계정


  CLEAR : gt_diff_003[], gt_diff_003.

  TRY.
      SELECT
              a~werks  AS werks,
              a~gjahr  AS gjahr,
              a~belnr  AS belnr,
              a~buzei  AS buzei,
              a~racct  AS racct,
              b~txt20  AS txt20,
              a~hsl  AS hsl,
              a~ps_posid AS ps_posid,
            'KRW'   AS twaer,
              a~matnr AS matnr,
              c~maktx AS maktx,
              a~awref AS awref
         FROM acdoca   AS a  INNER JOIN skat AS b
            ON a~racct = b~saknr
                             INNER JOIN makt AS c
            ON a~matnr = c~matnr
                AND b~spras = c~spras
        WHERE a~rldnr = '0L'
          AND a~rbukrs = @pa_bukrs
           AND a~rbusa IN @gr_gsber
          AND a~budat IN @gr_budat
          AND a~racct IN @gr_racct
          AND a~ps_posid IN @gr_posid
          AND b~spras = @sy-langu
          AND b~ktopl = '1000'
*          AND A~XREVERSING IS INITIAL
*          AND A~XREVERSED IS INITIAL
*          AND A~AWREF_REV = @SPACE
          INTO  CORRESPONDING FIELDS OF TABLE @gt_diff_003.  " 재고조정  발생전표 조회용...

  ENDTRY.


  SELECT    a~werks  AS werks,
            a~gjahr  AS gjahr,
            a~belnr  AS belnr,
            a~buzei  AS buzei,
            a~rbusa  AS gsber,
            a~racct  AS racct,
            a~hsl  AS hsl,
            a~awref AS awref,
            a~ps_posid AS ps_posid,
            b~txt20  AS txt20,
            'KRW'  AS twaer
 FROM acdoca   AS a    INNER JOIN skat AS b
        ON a~racct = b~saknr
WHERE a~rldnr = '0L'
  AND a~rbukrs = @pa_bukrs
  AND a~budat IN @gr_budat
  AND a~racct IN @gr_racct
  AND a~awtyp LIKE '%BKPF%'
*  AND A~PS_POSID IN @GR_POSID
*  AND A~AWREF_REV = @SPACE
  AND b~spras = @sy-langu
  AND b~ktopl = '1000'
    INTO  TABLE @DATA(gt_003_bkpf).


  LOOP  AT gt_003_bkpf ASSIGNING FIELD-SYMBOL(<bkpf>).
    IF <bkpf>-gsber IN gr_gsber.
      CLEAR gs_acdoca.
      MOVE-CORRESPONDING <bkpf> TO gs_acdoca.

      APPEND gs_acdoca TO  gt_diff_003.
    ENDIF.
  ENDLOOP.


  SORT gt_diff_003 BY bukrs belnr gjahr buzei.
  DELETE ADJACENT DUPLICATES FROM gt_diff_003  COMPARING  bukrs belnr gjahr buzei.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form COMPUTE_DIFF_4
*&---------------------------------------------------------------------*
*& 노무비
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM compute_diff_4 .

  CLEAR   gv_labor_amt_sum   .


  SELECT
      SUM( a~hsl )

     FROM acdoca   AS a
    WHERE a~rldnr = '0L'
      AND a~rbukrs = @pa_bukrs
       AND a~rbusa IN @gr_gsber
      AND a~budat IN @gr_budat
      AND a~racct IN @gr_gkont1
      INTO  @gv_labor_amt_sum   .


  CLEAR : gt_labor[], gt_labor.


  SELECT
          a~werks  AS werks,
          a~gjahr  AS gjahr,
          a~belnr  AS belnr,
          a~buzei  AS buzei,
          a~racct  AS racct,
              a~ps_posid AS ps_posid,
          b~txt20  AS txt20,
          'KRW'   AS twaer,
          a~hsl  AS hsl
     FROM acdoca   AS a  INNER JOIN skat AS b
        ON a~racct = b~saknr
    WHERE a~rldnr = '0L'
      AND a~rbukrs = @pa_bukrs
       AND a~rbusa IN @gr_gsber
      AND a~budat IN @gr_budat
      AND a~racct IN @gr_gkont1
              AND a~ps_posid IN @gr_posid
      AND b~spras = @sy-langu
      AND b~ktopl = '1000'
      INTO CORRESPONDING FIELDS OF TABLE @gt_labor.  " 노무비 개별 전표

ENDFORM.
*&---------------------------------------------------------------------*
*& Form COMPUTE_DIFF_5
*&---------------------------------------------------------------------*
*& 경비
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM compute_diff_5 .
  CLEAR gv_cost_amt_sum    .


  SELECT
      SUM( a~hsl )

     FROM acdoca   AS a
    WHERE a~rldnr = '0L'
      AND a~rbukrs = @pa_bukrs
      AND a~rbusa IN @gr_gsber
      "AND A~WERKS = @PA_WERKS,
      AND a~budat IN @gr_budat
      AND a~racct IN @gr_gkont2
      INTO  @gv_cost_amt_sum  .


  CLEAR : gt_mcost[], gt_mcost.


  SELECT
          a~werks  AS werks,
          a~gjahr  AS gjahr,
          a~belnr  AS belnr,
          a~buzei  AS buzei,
          a~racct  AS racct,
              a~ps_posid AS ps_posid,
          b~txt20  AS txt20,
         'KRW'   AS twaer,
          a~hsl  AS hsl
     FROM acdoca   AS a  INNER JOIN skat AS b
        ON a~racct = b~saknr
    WHERE a~rldnr = '0L'
      AND a~rbukrs = @pa_bukrs
      AND a~rbusa IN @gr_gsber
      "AND A~WERKS = @PA_WERKS,
      AND a~budat IN @gr_budat
      AND a~racct IN @gr_gkont2
       AND a~ps_posid IN @gr_posid
      AND b~spras = @sy-langu
      AND b~ktopl = '1000'
      INTO CORRESPONDING FIELDS OF TABLE @gt_mcost.  " 경비

ENDFORM.
*&---------------------------------------------------------------------*
*& Form RAW_DISTRIBUTION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM raw_distribution .

* GV_DIFF_001B  MMT0610에 없는 전표중  직접귀속


  LOOP AT gt_display ASSIGNING FIELD-SYMBOL(<clr>) .

    CLEAR <clr>-d1wrbtr.

  ENDLOOP.



  CHECK gt_diff_001b[] IS  NOT INITIAL.
  DATA : lv_matkl  TYPE mara-matkl,
         lv_lifnr  TYPE lfa1-lifnr,
         lv_fmatnr TYPE zcos0450-fmatnr.


  SORT gt_rate3 BY lifnr matkl fmatnr.
  SORT gt_zmmt600  BY matnr2.

*           투입된 원재료의 매핑된 제품코드에 직접귀속..

  LOOP AT gt_diff_001b ASSIGNING FIELD-SYMBOL(<001b>).
    CLEAR <001b>-msg_text .

* 배부방법 1 : 직접귀속

    READ TABLE gt_display ASSIGNING FIELD-SYMBOL(<aaa>) WITH   KEY  rmatnr =  <001b>-matnr .


    IF sy-subrc EQ 0 AND <aaa>-fmenge IS NOT INITIAL. ""직접귀속 원자재 MMT0610에 있다

      <aaa>-d1wrbtr = <aaa>-d1wrbtr  +  <001b>-hsl.

      <001b>-msg_text = 'S: 배부방법1 직접귀속   우선순위1'.

      CONTINUE.



    ENDIF.



*전표의 원재료와 매칭되는 MMT0610의 원재료가 없을때

    CHECK  <001b>-msg_text IS INITIAL.
*    CHECK SY-SUBRC NE 0 AND  <001B>-MSG_TEXT IS INITIAL.. ""  직접귀속 못할때...



    CLEAR :  lv_matkl,
             lv_lifnr ,
             lv_fmatnr.

* 전표의 자재로  매핑된 제품 코드 찾기
    READ TABLE gt_zmmt600 INTO gs_zmmt600 WITH KEY  matnr2 = <001b>-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.

      lv_fmatnr = gs_zmmt600-matnr.


**   찾은 제품의 자재그룹
      READ TABLE gt_t023t ASSIGNING FIELD-SYMBOL(<023t>)  WITH KEY fmatnr =    lv_fmatnr   BINARY SEARCH.
      IF sy-subrc EQ 0.
        lv_fmatnr = <023t>-fmatnr.

      ENDIF.

*** 원재료의 구매처
      READ TABLE gt_lfa1 INTO gs_lfa1 WITH KEY   rmatnr = <001b>-matnr BINARY SEARCH.


      IF sy-subrc EQ 0.
        lv_lifnr = gs_lfa1-lifnr.

      ENDIF.


    ELSE.
      <001b>-msg_text = 'E0: 원재료에 매핑되는 제품이 cbo 마스터에 없음. 전체 생산수량비율로 배부  '.

    ENDIF.






    READ TABLE gt_rate3 ASSIGNING FIELD-SYMBOL(<r3>) WITH KEY  lifnr  = lv_lifnr
                                                               matkl = lv_matkl
                                                               fmatnr = lv_fmatnr BINARY SEARCH.


    IF sy-subrc EQ 0.


      READ TABLE gt_display ASSIGNING FIELD-SYMBOL(<$disp>) WITH   KEY     lifnr  = lv_lifnr
                                                                      matkl = lv_matkl
                                                                      fmatnr = lv_fmatnr BINARY SEARCH.


      IF sy-subrc EQ 0 AND <$disp>-fmenge IS NOT INITIAL.
        " 재료비차이  전표번호별 같은 원자재 있을수 있기때문에 누적
        <$disp>-d1wrbtr = <$disp>-d1wrbtr  +  <001b>-hsl.

        <001b>-msg_text = 'S: 배부방법1  우선순위2'.


        CONTINUE.


      ELSE.

        <001b>-msg_text = 'E0: 우선순위2까지 매핑 비율 미존재 '.
      ENDIF.


    ELSE.

      READ TABLE gt_rate4 ASSIGNING FIELD-SYMBOL(<r4>) WITH KEY  lifnr  = lv_lifnr
                                                                fmatnr = lv_fmatnr BINARY SEARCH.

      IF sy-subrc EQ 0.

        READ TABLE gt_display ASSIGNING FIELD-SYMBOL(<$disp2>) WITH   KEY     lifnr  = lv_lifnr
                                                                    matkl = lv_matkl
                                                                    fmatnr = lv_fmatnr BINARY SEARCH.

        IF sy-subrc EQ 0 AND <$disp2>-fmenge IS NOT INITIAL.
          <001b>-msg_text = 'S: 배부방법1  우선순위3 '.

          " 재료비차이  전표번호별 같은 원자재 있을수 있기때문에 누적
          <$disp2>-d1wrbtr = <$disp2>-d1wrbtr  +  <001b>-hsl.
          CONTINUE.
        ELSE.
          <001b>-msg_text = 'E0: 우선순위3까지 매핑 비율 미존재 '.
        ENDIF.
      ELSE.
        <001b>-msg_text = 'E0: 우선순위3까지 매핑 비율 미존재 '.

      ENDIF.

    ENDIF.




  ENDLOOP.



***
*****spec if문 별   비율  미리 계산
***        PERFORM COMPUTE_RATE3.  " 구매처별 자재그룹별 제품별
***
***        PERFORM COMPUTE_RATE4.  " 구매처별 제품별
***        PERFORM COMPUTE_RATE5.  "  제품별  비율 .. 노무, 경비배부 활용


  DATA : lv_error(2).
  DATA : lv_error_result(2).
  DATA : no_distri_amt TYPE acdoca-hsl.
  DATA : lv_diff_acdoca_sum  TYPE acdoca-hsl.
  DATA : lv_d1wrbtr  TYPE acdoca-hsl.

  lv_error = 'E0'.
  CLEAR lv_error_result.


  CLEAR no_distri_amt.

  LOOP AT gt_diff_001b ASSIGNING FIELD-SYMBOL(<001b$>) WHERE msg_text+0(2) EQ lv_error.

    lv_error_result = 'YY'.
    no_distri_amt = no_distri_amt +   <001b$>-hsl.
    <001b$>-msg_text = '배부율 우선순위 4 적용'.

  ENDLOOP.



  CHECK  lv_error_result  EQ 'YY'.
  CHECK no_distri_amt IS NOT INITIAL.

  lv_diff_acdoca_sum  =   no_distri_amt.

  SORT gt_rate5 BY fmatnr.

  LOOP AT gt_display ASSIGNING FIELD-SYMBOL(<data>) WHERE fmenge  IS NOT INITIAL.


    READ TABLE gt_rate5 ASSIGNING FIELD-SYMBOL(<$r5>) WITH KEY fmatnr = <data>-fmatnr BINARY SEARCH.
    CHECK sy-subrc EQ 0 AND <$r5>-rate5 IS NOT INITIAL.


    CLEAR lv_d1wrbtr .
    lv_d1wrbtr =   no_distri_amt  *  <$r5>-rate5 .


    <data>-d1wrbtr =  <data>-d1wrbtr +   lv_d1wrbtr.

    lv_diff_acdoca_sum   = lv_diff_acdoca_sum   -   lv_d1wrbtr.


  ENDLOOP.


**차이보정

  IF lv_diff_acdoca_sum  IS NOT INITIAL AND gt_display[] IS NOT  INITIAL.

    SORT gt_display BY fmenge DESCENDING.

    LOOP AT gt_display ASSIGNING FIELD-SYMBOL(<$fs>) WHERE fmatnr IS NOT INITIAL AND fmenge IS NOT INITIAL.

      <$fs>-d1wrbtr  =   <$fs>-d1wrbtr  +  lv_diff_acdoca_sum  .

      EXIT.
    ENDLOOP.



  ENDIF.


  LOOP AT gt_display ASSIGNING FIELD-SYMBOL(<st>) WHERE d1wrbtr IS NOT INITIAL.
    <st>-icon =  icon_led_yellow.
    <st>-stats =  <st>-stats && '재료비차이 배부/'.

  ENDLOOP.




  SORT gt_display BY lifnr matkl fmatnr rmatnr.

  SORT gt_zmmt600  BY matnr.


  gv_distrl_error1 = '1E'.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form COMPUTE_RATE5
*&---------------------------------------------------------------------*
*& 당월생산된 전제 제품에 제품수량기준으로 배부
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM compute_rate5 .

  CHECK gv_fmatnr_menge_tot  IS NOT INITIAL.

  SORT gt_rate5 BY fmatnr fmenge.


  DATA : lv_rate(16) TYPE p   DECIMALS 9 .
  DATA : lv_texte(16).

  lv_text = '1.000000000'.


  CALL FUNCTION 'CHAR_FLTP_CONVERSION'
    EXPORTING
      string = lv_text "CHAR 변수
    IMPORTING
      flstr  = lv_rate. "DEC 변수




  LOOP AT gt_rate5 ASSIGNING FIELD-SYMBOL(<$r5>) .

    IF  <$r5>-fmenge IS NOT INITIAL .

      <$r5>-rate5     =   <$r5>-fmenge  /   gv_fmatnr_menge_tot .

      lv_rate = lv_rate -   <$r5>-rate5 .

      <$r5>-tot_menge  =      gv_fmatnr_menge_tot. "  총수량

    ENDIF.

  ENDLOOP.


*비율합이 1이 아니면   큰수량에 더한다.

  SORT gt_rate5  BY fmenge DESCENDING.

  IF lv_rate IS NOT INITIAL.

    READ TABLE gt_rate5  ASSIGNING FIELD-SYMBOL(<$rrrr>) INDEX 1.

    IF sy-subrc EQ 0.

**   차이 보정

      <$rrrr>-rate5 =   <$rrrr>-rate5  +       lv_rate.

    ENDIF.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form COST_DISTRIBUTION
*&---------------------------------------------------------------------*
*& 노무비 배부
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM lcost_distribution .

*
  DATA :         lv_labor_amt_sum   LIKE gs_acdoca-hsl.

  lv_labor_amt_sum  =  gv_labor_amt_sum  .
  CHECK  gv_labor_amt_sum    IS NOT INITIAL.



  SORT gt_rate5 BY fmatnr.

  LOOP AT gt_display ASSIGNING FIELD-SYMBOL(<data>) WHERE fmenge  IS NOT INITIAL.


    READ TABLE gt_rate5 ASSIGNING FIELD-SYMBOL(<$r5>) WITH KEY fmatnr = <data>-fmatnr BINARY SEARCH.
    CHECK sy-subrc EQ 0 AND <$r5>-rate5 IS NOT INITIAL.



    <data>-lwrbtr = gv_labor_amt_sum   *  <$r5>-rate5.

    lv_labor_amt_sum   =  lv_labor_amt_sum   -   <data>-lwrbtr.


  ENDLOOP.



  IF  lv_labor_amt_sum  IS NOT INITIAL AND gt_display[] IS NOT  INITIAL.

    SORT gt_display BY fmenge DESCENDING.

    LOOP AT gt_display ASSIGNING FIELD-SYMBOL(<$fs>) WHERE fmatnr IS NOT INITIAL.

      <$fs>-lwrbtr  =   <$fs>-lwrbtr  +   lv_labor_amt_sum  .

      EXIT.
    ENDLOOP.

    SORT gt_display BY lifnr matkl fmatnr rmatnr.

  ENDIF.





ENDFORM.
*&---------------------------------------------------------------------*
*& Form MCOST_DISTRIBUTION
*&---------------------------------------------------------------------*
*& 경비 배부
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM mcost_distribution .


*
  DATA:    lv_cost_amt_sum    LIKE gs_acdoca-hsl.

  lv_cost_amt_sum  =  gv_cost_amt_sum  .
  CHECK  gv_cost_amt_sum    IS NOT INITIAL.



  SORT gt_rate5 BY  fmatnr.

  LOOP AT gt_display ASSIGNING FIELD-SYMBOL(<data>) WHERE fmenge  IS NOT INITIAL.


    READ TABLE gt_rate5 ASSIGNING FIELD-SYMBOL(<$r5>) WITH KEY    fmatnr = <data>-fmatnr BINARY SEARCH.
    CHECK sy-subrc EQ 0 AND <$r5>-rate5 IS NOT INITIAL.

    <data>-mwrbtr = gv_cost_amt_sum   *  <$r5>-rate5.

    lv_cost_amt_sum   =  lv_cost_amt_sum   -   <data>-mwrbtr.


  ENDLOOP.



  IF  lv_cost_amt_sum  IS NOT INITIAL AND gt_display[] IS NOT  INITIAL.

    SORT gt_display BY fmenge DESCENDING.

    LOOP AT  gt_display ASSIGNING FIELD-SYMBOL(<$fs>) WHERE fmenge IS NOT INITIAL.

      <$fs>-mwrbtr  =   <$fs>-mwrbtr  +   lv_cost_amt_sum  .
      EXIT.
    ENDLOOP.
    SORT gt_display BY lifnr matkl fmatnr rmatnr.
  ENDIF.


  LOOP AT gt_display ASSIGNING FIELD-SYMBOL(<st>) WHERE mwrbtr IS NOT INITIAL.
    <st>-icon =  icon_led_yellow.
    <st>-stats =   <st>-stats  && '/노무비, 경비  배부'.

  ENDLOOP.





ENDFORM.


*&---------------------------------------------------------------------*
*& Form GET_BEFORE_STOCK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_before_stock .

**  기초재고수량, 금액
  DATA  lv_date  TYPE sydatum.
  DATA : lv_ym(6).


  DATA : lv_matnr(18).

  CLEAR : lv_date, lv_ym.



  CASE sy-sysid.
    WHEN 'TCD'.
      CASE pa_month.
        WHEN '202002'.
*  if  조회월이 2020년 2월이면  zcot1260 에 서  1월의 기말 수량과 금액을 가져오다
*          운영은 2020년 2월은 마이그레이션한다..
          lv_ym =  '202001'.

        WHEN OTHERS.

*  조회월이 2020년 3월이 아니고 크면  zcot1260의  조회월 -1 개월의  기말 수량과 금액이 기초가 된다

          lv_date = pa_month &&  '01'.

          CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL_SG'
            EXPORTING
              date      = lv_date
              days      = 0
              months    = 1
              signum    = '-'
              years     = 0
            IMPORTING
              calc_date = lv_date.

          lv_ym =  lv_date+0(6).

      ENDCASE.

    WHEN 'TCP' OR 'TCQ'.

      CASE pa_month.
        WHEN '202003'.
*  if  조회월이 2020년 3월이면  zcot1260 에 서  2월의 기말 수량과 금액을 가져오다  2월은 마이그레이션한다..
          lv_ym =  '202002'.

        WHEN OTHERS.

*  조회월이 2020년 3월이 아니고 크면  zcot1260의  조회월 -1 개월의  기말 수량과 금액이 기초가 된다

          lv_date = pa_month &&  '01'.

          CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL_SG'
            EXPORTING
              date      = lv_date
              days      = 0
              months    = 1
              signum    = '-'
              years     = 0
            IMPORTING
              calc_date = lv_date.

          lv_ym =  lv_date+0(6).

      ENDCASE.


  ENDCASE.


  SELECT bukrs,
         spmon,
         fmatnr,
         werks,
         emenge,
         emeins,
         ewrbtr,
         twaer
    FROM zcot1260
    WHERE bukrs = @pa_bukrs
      AND werks = @pa_werks
      AND spmon = @lv_ym
    INTO TABLE @DATA(lt_temp).


  SORT      gt_display   BY   bukrs
                              fmatnr
                              spmon
                              werks .



  LOOP AT lt_temp ASSIGNING FIELD-SYMBOL(<ts>) .

    CLEAR lv_matnr.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <ts>-fmatnr
      IMPORTING
        output = lv_matnr.



    READ TABLE gt_display ASSIGNING FIELD-SYMBOL(<disp>) WITH KEY bukrs  = <ts>-bukrs
                                                                  fmatnr = lv_matnr
                                                                  spmon  = <ts>-spmon
                                                                  werks   = <ts>-werks  BINARY SEARCH.

    IF sy-subrc EQ 0.

      <disp>-spmon   =  pa_month.
      <disp>-bukrs   =  pa_bukrs.
      <disp>-werks   =  pa_werks.

      <disp>-bmenge  =    <ts>-emenge .
      <disp>-bmeins  =    <ts>-emeins .
      <disp>-bwrbtr  =   <ts>-ewrbtr .



      READ TABLE gt_zmmt600 ASSIGNING FIELD-SYMBOL(<$zz>) WITH KEY matnr =   <disp>-fmatnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        <disp>-rmatnr =  <$zz>-matnr2. " 매핑 원자재 넣기  COLLECT  목적...

        PERFORM get_lifnr USING <disp>.


      ENDIF.





    ELSE.

      CLEAR gs_display.
      gs_display-fmatnr = <ts>-fmatnr.

      gs_display-spmon      =  pa_month.
      gs_display-werks      =  pa_werks.
      gs_display-bukrs      =  pa_bukrs.
      gs_display-bmenge     =  <ts>-emenge .
      gs_display-bmeins     =  <ts>-emeins .
      gs_display-bwrbtr     =  <ts>-ewrbtr .
      gs_display-twaer      =   'KRW'.





      READ TABLE gt_zmmt600 ASSIGNING FIELD-SYMBOL(<$zz2>) WITH KEY matnr =   gs_display-fmatnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        gs_display-rmatnr =  <$zz2>-matnr2. " 매핑 원자재 넣기  COLLECT  목적...

        PERFORM get_lifnr USING gs_display.

      ENDIF.

      COLLECT gs_display         INTO     gt_display.

      SORT  gt_display   BY   bukrs
                              fmatnr
                              spmon
                              werks .

    ENDIF.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form COMPUTE_RATE4
*&---------------------------------------------------------------------*
*& 해당 원재료의 구매처가 동일한 제품의 생산수량비율로 배부
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM compute_rate4 .


  CLEAR: gt_rate4a, gt_rate4a[].

*    구매처별 자재그룹 제품별 비율
  LOOP AT gt_rate4 INTO gs_rate4.

    CLEAR gs_rate4a.
    MOVE-CORRESPONDING gs_rate4 TO gs_rate4a.
    COLLECT gs_rate4a INTO gt_rate4a.


  ENDLOOP.


  DATA : r4_tot_menge LIKE zmmt0610-fmenge.

  SORT gt_rate4a BY lifnr .
  SORT gt_rate4 BY lifnr .


  READ TABLE gt_raw2 ASSIGNING FIELD-SYMBOL(<$raw>) INDEX 1.

  CHECK sy-subrc EQ 0.


  DATA : lv_rate(16) TYPE p   DECIMALS 9 .
  DATA : lv_rate_1(16) TYPE p   DECIMALS 9 .
  DATA : lv_rate_d(16) TYPE p   DECIMALS 9 .
  DATA : lv_rate_0(16) TYPE p   DECIMALS 9 .


  DATA : lv_texte(16).

  lv_text = '1.000000000'.

  CALL FUNCTION 'CHAR_FLTP_CONVERSION'
    EXPORTING
      string = lv_text "CHAR 변수
    IMPORTING
      flstr  = lv_rate_1. "DEC 변수


  LOOP AT gt_rate4a ASSIGNING FIELD-SYMBOL(<$4a>).

    CLEAR r4_tot_menge.

    CLEAR  lv_rate. " 1에서 시작
    lv_rate = lv_rate_1.

    LOOP AT gt_rate4 INTO gs_rate4 WHERE lifnr = <$4a>-lifnr .

      r4_tot_menge  =  r4_tot_menge  +  gs_rate4-fmenge  .  " 구매처별 총수량

    ENDLOOP.

    IF  r4_tot_menge  IS NOT INITIAL.
      LOOP AT gt_rate4 ASSIGNING FIELD-SYMBOL(<$r4>) WHERE lifnr = <$4a>-lifnr  AND fmenge IS NOT INITIAL.

        <$r4>-rate4      =   <$r4>-fmenge  /    r4_tot_menge .
        <$r4>-sub_menge  =      r4_tot_menge. "   부분 총수량

        lv_rate = lv_rate -    <$r4>-rate4 .

      ENDLOOP.
    ENDIF.

    SORT gt_rate4  BY lifnr  fmenge DESCENDING.


    IF lv_rate NE  lv_rate_0.  " zero 가 아니면


      LOOP AT  gt_rate4 ASSIGNING FIELD-SYMBOL(<$rr>)   WHERE  lifnr = <$4a>-lifnr.

        lv_rate_d   =  lv_rate_d + <$r4>-rate4 .

      ENDLOOP.


*비율합이 1이 아니면   큰수량에 더한다.

      READ TABLE gt_rate4  ASSIGNING FIELD-SYMBOL(<$rrr>) WITH KEY   lifnr = <$4a>-lifnr
                                                                    BINARY SEARCH.
      IF sy-subrc EQ 0.

**   차이 보정
        <$rrr>-rate4 =   <$rrr>-rate4  +       lv_rate.
      ENDIF.

    ENDIF.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> TEXT_PT1
*&      --> TEXT_QT1
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM popup_to_confirm USING pv_pt
                            pv_qt.

  "-- call popup
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = pv_pt
*     DIAGNOSE_OBJECT       = ' '
      text_question         = pv_qt
*     TEXT_BUTTON_1         = 'Ja'(001)
*     ICON_BUTTON_1         = ' '
*     TEXT_BUTTON_2         = 'Nein'(002)
*     ICON_BUTTON_2         = ' '
*     DEFAULT_BUTTON        = '1'
      display_cancel_button = ' '
*     USERDEFINED_F1_HELP   = ' '
*     START_COLUMN          = 25
*     START_ROW             = 6
*     POPUP_TYPE            =
*     IV_QUICKINFO_BUTTON_1 = ' '
*     IV_QUICKINFO_BUTTON_2 = ' '
    IMPORTING
      answer                = gv_answer
*   TABLES
*     PARAMETER             =
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form RAW_DISTRIBUTION2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM raw_distribution2 .


* 구매가격차이 1  직접귀속


  LOOP AT gt_display ASSIGNING FIELD-SYMBOL(<clr>) .

    CLEAR <clr>-d2wrbtr.

  ENDLOOP.



  CHECK gt_diff_002[] IS  NOT INITIAL.
  DATA : lv_matkl  TYPE mara-matkl,
         lv_lifnr  TYPE lfa1-lifnr,
         lv_fmatnr TYPE zcos0450-fmatnr.


  SORT gt_rate3 BY lifnr matkl fmatnr.
  SORT gt_zmmt600  BY matnr2.

*           투입된 원재료의 매핑된 제품코드에 직접귀속..

  LOOP AT gt_diff_002 ASSIGNING FIELD-SYMBOL(<d002>).
    CLEAR <d002>-msg_text .

* 배부방법 1 : 직접귀속

    IF <d002>-matnr IS NOT INITIAL.  " 자재가 있으면...
      READ TABLE gt_display ASSIGNING FIELD-SYMBOL(<aaa>) WITH   KEY  rmatnr =  <d002>-matnr .


      IF sy-subrc EQ 0 AND <aaa>-fmenge IS NOT INITIAL. ""직접귀속 원자재 MMT0610에 있다

        <aaa>-d2wrbtr = <aaa>-d2wrbtr  +  <d002>-hsl.

        <d002>-msg_text = 'S: 배부방법1 직접귀속   우선순위1'.

        CONTINUE.

      ENDIF.

    ENDIF.



*전표의 가격차이와 매칭되는 MMT0610의 원재료가 없을때

    CHECK sy-subrc NE 0 AND  <d002>-msg_text IS INITIAL.. ""  직접귀속 못할때...



    CLEAR :  lv_matkl,
             lv_lifnr ,
             lv_fmatnr.

* 전표의 자재로  매핑된 제품 코드 찾기
    READ TABLE gt_zmmt600 INTO gs_zmmt600 WITH KEY  matnr2 = <d002>-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.

      lv_fmatnr = gs_zmmt600-matnr.


**   찾은 제품의 자재그룹
      READ TABLE gt_t023t ASSIGNING FIELD-SYMBOL(<023t>)  WITH KEY fmatnr =    lv_fmatnr   BINARY SEARCH.
      IF sy-subrc EQ 0.
        lv_fmatnr = <023t>-fmatnr.

      ENDIF.

*** 원재료의 구매처
      READ TABLE gt_lfa1 INTO gs_lfa1 WITH KEY   rmatnr = <d002>-matnr BINARY SEARCH.


      IF sy-subrc EQ 0.
        lv_lifnr = gs_lfa1-lifnr.

      ENDIF.


    ELSE.
      <d002>-msg_text = 'E0: 원재료에 매핑되는 제품이 cbo 마스터에 없음. 전체 생산수량비율로 배부  '.

    ENDIF.


    READ TABLE gt_rate3 ASSIGNING FIELD-SYMBOL(<r3>) WITH KEY  lifnr  = lv_lifnr
                                                               matkl = lv_matkl
                                                               fmatnr = lv_fmatnr BINARY SEARCH.


    IF sy-subrc EQ 0.


      READ TABLE gt_display ASSIGNING FIELD-SYMBOL(<$disp>) WITH   KEY     lifnr  = lv_lifnr
                                                                      matkl = lv_matkl
                                                                      fmatnr = lv_fmatnr BINARY SEARCH.


      IF sy-subrc EQ 0 AND <$disp>-fmenge IS NOT INITIAL.
        " 가격 차이  전표번호별 같은 원자재 있을수 있기때문에 누적
        <$disp>-d3wrbtr = <$disp>-d3wrbtr  +  <d002>-hsl.

        <d002>-msg_text = 'S: 배부방법1  우선순위2'.


        CONTINUE.


      ELSE.

        <d002>-msg_text = 'E0: 우선순위2까지 매핑 비율 미존재 '.
      ENDIF.


    ELSE.

      READ TABLE gt_rate4 ASSIGNING FIELD-SYMBOL(<r4>) WITH KEY  lifnr  = lv_lifnr
                                                                fmatnr = lv_fmatnr BINARY SEARCH.

      IF sy-subrc EQ 0.

        READ TABLE gt_display ASSIGNING FIELD-SYMBOL(<$disp2>) WITH   KEY     lifnr  = lv_lifnr
                                                                    matkl = lv_matkl
                                                                    fmatnr = lv_fmatnr BINARY SEARCH.

        IF sy-subrc EQ 0  AND <$disp2>-fmenge IS NOT INITIAL.
          <d002>-msg_text = 'S: 배부방법1  우선순위3 '.

          " 가격 차이  전표번호별 같은 원자재 있을수 있기때문에 누적
          <$disp2>-d3wrbtr = <$disp2>-d3wrbtr  +  <d002>-hsl.
          CONTINUE.
        ELSE.
          <d002>-msg_text = 'E0: 우선순위3까지 매핑 비율 미존재 '.
        ENDIF.
      ELSE.
        <d002>-msg_text = 'E0: 우선순위3까지 매핑 비율 미존재 '.

      ENDIF.

    ENDIF.




  ENDLOOP.


  DATA : lv_error(2).
  DATA : lv_error_result(2).
  DATA : no_distri_amt TYPE acdoca-hsl.
  DATA : lv_diff_acdoca_sum  TYPE acdoca-hsl.
  DATA : lv_d3wrbtr  TYPE acdoca-hsl.

  lv_error = 'E0'.
  CLEAR lv_error_result.


  CLEAR no_distri_amt.

  LOOP AT gt_diff_002 ASSIGNING FIELD-SYMBOL(<001b$>) WHERE msg_text+0(2) EQ lv_error  OR
                                                          msg_text IS INITIAL.


    lv_error_result = 'YY'.
    <001b$>-msg_text =   '배부율 우선순위 4 적용 '.
    no_distri_amt = no_distri_amt +   <001b$>-hsl.

  ENDLOOP.

  LOOP AT gt_display ASSIGNING FIELD-SYMBOL(<st>) WHERE d2wrbtr IS NOT INITIAL.
    <st>-icon =  icon_led_yellow.
    <st>-stats =  <st>-stats && '/가격차이 배부/'.

  ENDLOOP.



  CHECK  lv_error_result  EQ 'YY'.
  CHECK no_distri_amt IS NOT INITIAL.

  lv_diff_acdoca_sum  =   no_distri_amt.

  SORT gt_rate5 BY fmatnr.

  LOOP AT gt_display ASSIGNING FIELD-SYMBOL(<data>) WHERE fmenge  IS NOT INITIAL.


    READ TABLE gt_rate5 ASSIGNING FIELD-SYMBOL(<$r5>) WITH KEY fmatnr = <data>-fmatnr BINARY SEARCH.
    CHECK sy-subrc EQ 0 AND <$r5>-rate5 IS NOT INITIAL.


    CLEAR lv_d3wrbtr .
    lv_d3wrbtr =   no_distri_amt  *  <$r5>-rate5 .


    <data>-d3wrbtr =  <data>-d3wrbtr +   lv_d3wrbtr.

    lv_diff_acdoca_sum   = lv_diff_acdoca_sum   -   lv_d3wrbtr..

  ENDLOOP.


**차이보정

  IF lv_diff_acdoca_sum  IS NOT INITIAL AND gt_display[] IS NOT  INITIAL.

    SORT gt_display BY fmenge DESCENDING.

    LOOP AT gt_display ASSIGNING FIELD-SYMBOL(<$fs>) WHERE fmatnr IS NOT INITIAL AND fmenge IS NOT INITIAL.

      <$fs>-d3wrbtr  =   <$fs>-d3wrbtr  +  lv_diff_acdoca_sum  .

      EXIT.
    ENDLOOP.



  ENDIF.


  LOOP AT gt_display ASSIGNING FIELD-SYMBOL(<st2>) WHERE d3wrbtr IS NOT INITIAL.
    <st2>-icon =  icon_led_yellow.
    <st2>-stats =  <st2>-stats && '/전체 생산량 비율,가격차이 배부/'.

  ENDLOOP.


  SORT gt_display BY lifnr matkl fmatnr rmatnr.

  SORT gt_zmmt600  BY matnr.


  gv_distrl_error2 = '2E'.




ENDFORM.
*&---------------------------------------------------------------------*
*& Form COMPUTE_COGS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM compute_cogs .

  LOOP AT gt_display  ASSIGNING FIELD-SYMBOL(<ee>).

*재고단가 = 매출단가  ( 당월제조원가 + 전월 기말재고 ) / (당월생산수량 + 전월기말재고수량)

    IF  ( <ee>-zcogm  IS NOT  INITIAL OR  <ee>-bwrbtr  IS NOT INITIAL ) AND
        ( <ee>-fmenge  IS NOT  INITIAL OR  <ee>-bmenge IS NOT INITIAL ).

      <ee>-euwrbtr = ( <ee>-zcogm + <ee>-bwrbtr ) / ( <ee>-fmenge + <ee>-bmenge ).

    ENDIF.


*   매출원가 = 당월 매출수량 * 매출단가(재고단가)
*   단, 당월 기말 재고금액이 없으면 계산하지 말고 (전월기말재고금액 + 당월제조원가) 로 금액 대체

    IF <ee>-smenge IS NOT INITIAL AND  <ee>-euwrbtr IS NOT INITIAL.
      <ee>-zcogs =  <ee>-smenge  *   <ee>-euwrbtr.

      <ee>-stats = '매출원가 계산'.

    ELSE.

      IF <ee>-euwrbtr IS INITIAL.
        <ee>-zcogs  =    <ee>-bwrbtr   + <ee>-zcogm.
        <ee>-stats = '당월 기말재고금액 0원(전월기말재고금액 + 당월제조원가)로 금액 대체 '.
        .
      ENDIF.

    ENDIF.


* 당월기말재고 = 	( 당월제조원가 + 전월기말재고 ) – 매출원가

    <ee>-ewrbtr =  ( <ee>-zcogm + <ee>-bwrbtr ) - <ee>-zcogs.


* 기말 재고 수량이 0인 경우  기말 재고 금액은  매출원가로 가감산
*  20200902  김호준위원 통화

    IF <ee>-emenge IS INITIAL AND <ee>-ewrbtr IS NOT INITIAL AND
        <ee>-smenge IS NOT INITIAL.  " 판매수량이 있을때 매출원가로 보정  20200910...
      <ee>-zcogs =  <ee>-zcogs +   <ee>-ewrbtr.
      CLEAR <ee>-ewrbtr.  " 기말 재고는 ZERO..

    ELSE.

* 당월 생산 , 당월 판매의 예외 발생..  20200910...
* 당월 생산없고, 판매 없고, 전월이월 수량과 금액이 있는 경우
**기초를 기말로 MOVEG 해야됨   마이그레이션 6월 자재가  7월 생산없이
** 8월에 판매 된 경우 발생함

      IF <ee>-emenge IS INITIAL AND  <ee>-smenge IS INITIAL AND <ee>-fmenge IS INITIAL
       AND <ee>-bmenge IS NOT INITIAL AND <ee>-bwrbtr IS NOT INITIAL.

        <ee>-emenge  =  <ee>-bmenge.
        <ee>-emeins  =  <ee>-bmeins.
        <ee>-ewrbtr  =  <ee>-bwrbtr.

      ENDIF.

    ENDIF.


  ENDLOOP.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form COMPUTE_COMS
*&---------------------------------------------------------------------*
*& 제조원가 계산
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM compute_coms .

*제조원가 원재료비 + 차이1 + 차이2 + 차이3+ 노무비 + 경비  실행 및 재료비, 노무비, 경비 등의 금액이 변경될 때 실시간 합계


  LOOP AT gt_display  ASSIGNING FIELD-SYMBOL(<coms>) WHERE fmenge IS NOT INITIAL.


    <coms>-zcogm =          "RWRBTR [CO]재료비 금액
                                 <coms>-rwrbtr +         "D1WRBTR  [CO] 재료비차이1
                                 <coms>-d1wrbtr +        "D2WRBTR  [CO] 구매가격차이 1
                                 <coms>-d2wrbtr +       " D3WRBTR  [CO] 구매가격차이 2
                                 <coms>-d3wrbtr +       " D4WRBTR  [CO] 재고조정금액
                                 <coms>-d4wrbtr +       " LWRBTR [CO] 노무비
                                 <coms>-lwrbtr +        " MWRBTR [CO] 경비
                                 <coms>-mwrbtr .
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAVE_1260
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM save_1260 .

  DATA : ls_1260 LIKE zcot1260.

  <fs_disp>-erdat = sy-datum.
  <fs_disp>-erzet = sy-uzeit.
  <fs_disp>-ernam = sy-uname.

  CLEAR  ls_1260.



  ls_1260-bukrs       =     pa_bukrs..
  ls_1260-spmon       =     pa_month.
  ls_1260-fmatnr      =     <fs_disp>-fmatnr.
  ls_1260-werks       =    pa_werks.
  ls_1260-lifnr       =     <fs_disp>-lifnr .
  ls_1260-matkl       =     <fs_disp>-matkl .
  ls_1260-bwrbtr      =     <fs_disp>-bwrbtr.
  ls_1260-bmenge      =     <fs_disp>-bmenge.
  ls_1260-bmeins      =     <fs_disp>-bmeins.
  ls_1260-fmenge      =     <fs_disp>-fmenge.
  ls_1260-fmeins      =     <fs_disp>-fmeins.
  ls_1260-rmatnr      =     <fs_disp>-rmatnr.
  ls_1260-rmenge      =     <fs_disp>-rmenge.
  ls_1260-rmeins      =     <fs_disp>-rmeins.
  ls_1260-rwrbtr      =     <fs_disp>-rwrbtr.
  ls_1260-d1wrbtr     =     <fs_disp>-d1wrbtr.
  ls_1260-d2wrbtr     =     <fs_disp>-d2wrbtr.
  ls_1260-d3wrbtr     =     <fs_disp>-d3wrbtr.
  ls_1260-d4wrbtr     =     <fs_disp>-d4wrbtr.
  ls_1260-lwrbtr      =     <fs_disp>-lwrbtr.
  ls_1260-mwrbtr      =     <fs_disp>-mwrbtr.
  ls_1260-zcogm       =     <fs_disp>-zcogm.
  ls_1260-smenge      =     <fs_disp>-smenge.
  ls_1260-smeins      =     <fs_disp>-smeins.
  ls_1260-swrbtr      =     <fs_disp>-swrbtr.
  ls_1260-zcogs       =     <fs_disp>-zcogs.
  ls_1260-emenge      =     <fs_disp>-emenge.
  ls_1260-emeins      =     <fs_disp>-emeins.
  ls_1260-ewrbtr      =     <fs_disp>-ewrbtr.
  ls_1260-euwrbtr     =     <fs_disp>-euwrbtr.
  ls_1260-belnr1      =     <fs_disp>-belnr1.
  ls_1260-belnr3      =     <fs_disp>-belnr3.
  ls_1260-twaer       =     <fs_disp>-twaer.
  ls_1260-erdat       =     <fs_disp>-erdat.
  ls_1260-erzet       =     <fs_disp>-erzet.
  ls_1260-ernam       =     <fs_disp>-ernam.

  MODIFY zcot1260 FROM ls_1260.
  COMMIT WORK.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DELETE_COT1260
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM delete_cot1260 .
  DATA : lv_flag.
  CLEAR lv_flag.
  LOOP AT gt_display INTO gs_display WHERE belnr1 IS NOT INITIAL  OR  belnr3 IS NOT INITIAL.
    lv_flag =  'X'.
  ENDLOOP.
  IF lv_flag IS INITIAL.
    DELETE FROM zcot1260 WHERE bukrs = pa_bukrs AND spmon = pa_month AND werks = pa_werks.
    IF sy-subrc EQ 0.
      COMMIT WORK AND  WAIT.

      CLEAR:  gt_display[], gt_display[].

    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ELSE.
    MESSAGE s000 WITH TEXT-e88  DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAVE_COT1260
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM save_cot1260 .

**   SPEC 에 없는  테이블 저장로직 ...
****전표생성 없는 ROW 저장 기능 필요유무 문의하기..


  IF gt_rows[] IS INITIAL.
    MESSAGE s021.
    gv_exit = abap_true.
  ENDIF.
  CHECK gv_exit EQ space.
  PERFORM check_select_data  USING gc_c.
  CHECK gv_exit = space.



  LOOP AT gt_rows INTO gs_rows.


    READ TABLE gt_display ASSIGNING <fs_disp> INDEX gs_rows-index.

    IF sy-subrc EQ 0.

      <fs_disp>-icon = icon_system_save  .
      CHECK  <fs_disp>-zcogs IS NOT INITIAL  OR <fs_disp>-zcogm IS NOT INITIAL.

      PERFORM save_1260.

      MESSAGE s007.
    ENDIF.


  ENDLOOP.







ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAVE_COT1260_ZER0
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM save_cot1260_zer0 .

  LOOP AT gt_display ASSIGNING <fs_disp>   WHERE zcogm IS INITIAL
                                               OR zcogs IS  INITIAL.
    <fs_disp>-icon = icon_system_save  .

    PERFORM save_1260.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_LASTDAY_STOCK_MM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_lastday_stock_mm .

  DATA : gt_stock LIKE zmms4020 OCCURS 0,

         i_matnr  TYPE  matnr    ,              "     자재 번호
         i_lfgja  TYPE  lfgja    ,              "    현재 기간의 회계연도
         i_lfmon  TYPE  lfmon    .              "    현재 기간 (전기기간)


  DATA : e_lfgja TYPE lfgja,
         e_lfmon TYPE lfmon,
         e_werks TYPE werks.    "    플랜트

****ZMMS4020...
**MATNR 1 Types MATNR CHAR  40  0 자재 번호
**LABST 1 Types LABST QUAN  13  3 평가된 가용 재고
**MEINS 1 Types MEINS UNIT  3 0 기본 단위
**LFGJA 1 Types LFGJA NUMC  4 0 현재 기간의 회계연도
**LFMON 1 Types LFMON NUMC  2 0 현재 기간 (전기기간)
**SPMON 1 Types SPMON NUMC  6 0 분석기간 - 월

  e_lfgja = pa_month+0(4).
  e_lfmon = pa_month+4(2).
  e_werks = pa_werks.

  CLEAR : gt_stock, gt_stock[].

  TRY.

      CALL FUNCTION 'ZMM_PERIOD_END_STOCK_QUANTITY'
        EXPORTING
*         I_MATNR =
          i_lfgja = e_lfgja
          i_lfmon = e_lfmon
          i_werks = e_werks
        TABLES
          et_list = gt_stock.

*CATCH

  ENDTRY.

  CHECK gt_stock[] IS NOT INITIAL.


  SORT gt_stock BY matnr.
  DELETE gt_stock WHERE labst IS INITIAL.

  LOOP AT gt_stock ASSIGNING FIELD-SYMBOL(<stock>).

    READ TABLE gt_display ASSIGNING FIELD-SYMBOL(<disp>) WITH KEY fmatnr = <stock>-matnr      BINARY SEARCH.

    IF sy-subrc EQ 0.

      <disp>-emenge = <stock>-labst. " 수량
      <disp>-emeins = <stock>-meins.  " 단위


    ELSE.

      CLEAR gs_display.
      gs_display-fmatnr = <stock>-matnr.

      gs_display-spmon      =  pa_month.
      gs_display-werks      =  pa_werks.
      gs_display-bukrs      =  pa_bukrs.
      gs_display-emenge     =  <stock>-labst .
      gs_display-emeins     =  <stock>-meins .

      gs_display-twaer      =   'KRW'.


      READ TABLE gt_zmmt600 ASSIGNING FIELD-SYMBOL(<$zz2>) WITH KEY matnr =   gs_display-fmatnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        gs_display-rmatnr =  <$zz2>-matnr2. " 매핑 원자재 넣기  COLLECT  목적...

        PERFORM get_lifnr USING gs_display.

      ENDIF.

      COLLECT gs_display         INTO     gt_display.

      SORT  gt_display   BY   bukrs
                              fmatnr
                              spmon
                              werks .

    ENDIF.



  ENDLOOP.






ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_LIFNR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <$FS>
*&---------------------------------------------------------------------*
FORM get_lifnr  USING   ps_display STRUCTURE gs_display.

  CLEAR :  gs_lfa1-matkl,
           gs_lfa1-lifnr.

  SELECT SINGLE   a~matkl   a~wrkst AS lifnr    "
 FROM mara AS a
    INTO (ps_display-matkl,  ps_display-lifnr )
  WHERE a~matnr =  ps_display-rmatnr
    AND a~lvorm = ' '.




ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_DATA_RTN2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM selected_data_rtn2 .



******* SD
*MATNR  자재 번호   " 제품 번호
*WERKS  플랜트
*VRKME  판매 단위
*FKIMG  실제 대금청구 수량
*MEINS  기본 단위
*WAERS  SD 문서 통화  KRW
*NETWR_L  문서통화의 청구품목 정가


  SORT  gt_display BY  werks fmatnr.

  LOOP AT gt_sd_result2 ASSIGNING FIELD-SYMBOL(<$sd>).

    CLEAR gs_display.
    READ TABLE gt_display ASSIGNING FIELD-SYMBOL(<$fs>) WITH KEY werks =  <$sd>-werks
                                                                 fmatnr =   <$sd>-matnr
                                                                 BINARY SEARCH.

    IF sy-subrc EQ 0.

      <$fs>-bukrs   =  pa_bukrs.
      <$fs>-werks   =  pa_werks.
      <$fs>-twaer   = <$sd>-waers. " 항상 KRW
      <$fs>-spmon   = pa_month. " 년도 월

      <$fs>-fmatnr  = <$sd>-matnr.  "제품
      <$fs>-smeins  = <$sd>-vrkme. "  판매단위
      <$fs>-smenge  = <$sd>-fkimg. " 수량


      <$fs>-swrbtr  = <$sd>-netwr_l. " 매출액

      READ TABLE gt_zmmt600 ASSIGNING FIELD-SYMBOL(<$zz>) WITH KEY matnr =   <$sd>-matnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        <$fs>-rmatnr =  <$zz>-matnr2. " 매핑 원자재 넣기  COLLECT  목적...

        PERFORM get_lifnr USING <$fs>.

      ELSE.


      ENDIF.





    ELSE.

      gs_display-bukrs   = pa_bukrs.
      gs_display-werks   = pa_werks.

      gs_display-twaer   = <$sd>-waers. " 항상 KRW
      gs_display-spmon   =  pa_month.

      gs_display-fmatnr  = <$sd>-matnr.  "제품
      gs_display-smeins  = <$sd>-vrkme. " KG
      gs_display-smenge  = <$sd>-fkimg. " 수량
      gs_display-swrbtr  = <$sd>-netwr_l. " 매출액


      READ TABLE gt_zmmt600 ASSIGNING FIELD-SYMBOL(<$zx>) WITH KEY matnr =   <$sd>-matnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        gs_display-rmatnr =  <$zx>-matnr2. " 매핑 원자재 넣기  COLLECT  목적...

        PERFORM get_lifnr USING gs_display.

      ENDIF.

      COLLECT gs_display         INTO     gt_display.


    ENDIF.



  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form LAST_COLLECT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM last_collect .

  DATA(lt_disp)      = gt_display[].
  DATA(lt_disp_save) = gt_display[].

  CLEAR : gt_display, gt_display[].

  LOOP AT lt_disp ASSIGNING FIELD-SYMBOL(<fs>).
    CLEAR <fs>-fmeins.
    CLEAR <fs>-rmeins.
    CLEAR <fs>-emeins.
    CLEAR <fs>-bmeins.
    CLEAR <fs>-smeins.


    COLLECT <fs> INTO gt_display.


  ENDLOOP.



*  이월  판매   생산투입  기말  단위가  같지 않으면
* 제품별 제조원가, 매출단가  계산 못함

  LOOP AT gt_display ASSIGNING FIELD-SYMBOL(<zz>).

    SELECT SINGLE meins INTO <zz>-fmeins
     FROM mara
      WHERE matnr = <zz>-fmatnr.


    SELECT SINGLE meins INTO <zz>-rmeins
     FROM mara
      WHERE matnr = <zz>-rmatnr.


    <zz>-emeins =  <zz>-fmeins.
    <zz>-smeins =  <zz>-fmeins.
    <zz>-bmeins =  <zz>-fmeins.

** 명칭 찾아 넣기..

* 자재그룹명 : 제품의
    PERFORM get_wgbez USING <zz>.

*벤더명 :  원자재의
    PERFORM get_lfa1_name USING <zz>.

*제품 원자재명 넣기
    PERFORM  get_matnr_name  USING   <zz>  .

    <zz>-werks_name =  gv_we_nm .

  ENDLOOP.

* FI  제품매출액  1000  402101001
*      제품매출액_수출 1000  402101003    추가로 표시


  SORT gt_display BY fmatnr.


  PERFORM get_fi_rev_gl.

  CLEAR gs_fi_rev.
  LOOP AT gt_fi_rev INTO gs_fi_rev.

    READ TABLE gt_display ASSIGNING FIELD-SYMBOL(<$fs>)
                       WITH KEY fmatnr = gs_fi_rev-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      <$fs>-dmbtr =  gs_fi_rev-hsl * ( -1 ).


    ELSE.



      CLEAR  gs_display.
      gs_display-fmatnr = gs_fi_rev-matnr.
      gs_display-dmbtr =  gs_fi_rev-hsl * ( -1 ).

      gs_display-twaer = 'KRW'.


*MODIFY 2021 02.23   REQ  BY 강현수K

      CLEAR gs_t023t.
      SELECT SINGLE
              b~matkl AS matkl
              b~wgbez AS wgbez
              a~wrkst  AS wrkst  " 제품의 구매처
              c~maktx AS fmatnr_maktx " 자재명
       INTO CORRESPONDING FIELDS OF  gs_display
         FROM mara AS a INNER JOIN  t023t AS b
         ON a~matkl = b~matkl INNER JOIN makt AS c ON
          a~matnr = c~matnr
         WHERE a~matnr =  gs_display-fmatnr " 제품의 자재그룹
         AND b~spras = sy-langu.



*공급업체

      SELECT SINGLE matnr2  INTO gs_display-rmatnr
        FROM zmmt0600
       WHERE matnr = gs_display-fmatnr
        AND lvorm = ''.

      IF sy-subrc EQ 0.
        PERFORM get_lifnr USING gs_display.


*  구매처명
        SELECT SINGLE name1 INTO gs_display-name1
          FROM lfa1
        WHERE lifnr =  gs_display-lifnr.

      ENDIF.


**    제품명
*      SELECT SINGLE MAKTX  MATKL
*        INTO GS_DISPLAY-FMATNR_MAKTX
*        FROM MAKT
*      WHERE MATNR =  GS_DISPLAY-FMATNR
*        AND SPRAS = SY-LANGU.

**END BY  BSGSM_FCM.....

      gs_display-msg  = '수불 테이블에 존재하지 않는 FI 자재별  매출추가 '.

      APPEND gs_display TO  gt_display.

      SORT gt_display BY fmatnr.

    ENDIF.

    CLEAR gs_fi_rev.


  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_REVERSE
*&---------------------------------------------------------------------*
*& 이프로그램에서 역분개하지 않아서 CBO 남아 잇는 전표 화면에서 소거
*   전체 역분개되면 삭제버튼으로  처리 해야 조회에 나타나게..
*&---------------------------------------------------------------------*
*&      --> GS_DISPLAY
*&---------------------------------------------------------------------*
FORM check_reverse  USING    ps STRUCTURE gs_display.

  DATA : lv_stblg LIKE bkpf-stblg.
  CLEAR lv_stblg.
  SELECT SINGLE stblg
   INTO lv_stblg
    FROM bkpf
   WHERE bukrs = ps-bukrs
     AND belnr = ps-belnr1
     AND gjahr = pa_month+0(4).

  IF sy-subrc EQ 0 AND lv_stblg IS NOT INITIAL.

    CLEAR : ps-belnr1.

    PERFORM  update_data_rtn1 USING    ps
                                      'YY'.


  ENDIF.

  CLEAR lv_stblg.
  SELECT SINGLE stblg
  INTO lv_stblg
   FROM bkpf
  WHERE bukrs = ps-bukrs
    AND belnr = ps-belnr3
     AND gjahr = pa_month+0(4).

  IF sy-subrc EQ 0 AND lv_stblg IS NOT INITIAL.

    CLEAR : ps-belnr3.
    PERFORM  update_data_rtn1 USING    ps
                                      'YY'.


  ENDIF.




ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_WBS_SDT0120
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <ZZ>
*&---------------------------------------------------------------------*
FORM get_wbs_zsdt0120 .

**     기존에 플랜트에서 대상 wbs를 찾는 로직을 wbs 마스터에서 가지고 오거나 했는데..
**    이부분은 sd에서 별도의 CBO 테이블을 가지고 간다
**    -> ZSDT0120 테이블에서 해당 WERKS = 화면입력 플랜트 ,
**      BUKRS = 화면입력 회사코드 ,
**      MODUL = 'M'  인 WBSNR 필드를 WBS로
**    ( 이 때 WBSNR 는 숫자로되어있는 WBS 번호라 PRPS-PSPNR을 읽어서.
**     POSID를 읽어와야 WBS 코드가 됩니다. )


*ZSDT0120  PK
*MODUL  모듈(S:SD, M:MM)
*BUKRS  입고 회사 코드
*WERKS  플랜트
*VKGRP  영업 그룹
*KTGRM  자재의 계정 지정 그룹
*WBSNR  WBS 요소
*LVORM  삭제 지시자

  CLEAR : gt_wbs, gt_wbs[].

  SELECT posid
      FROM prps AS a INNER JOIN zsdt0120 AS b
                       ON b~wbsnr EQ a~pspnr
     WHERE b~modul = 'M'
       AND b~bukrs = @pa_bukrs
       AND b~werks EQ @pa_werks
    INTO  TABLE @gt_wbs.

**
**김호준입니다.
**
** 1. 플랜트에 해당 하는 wbs를 찾는 모든 로직에 적용입니다. (재수정)
**   1.1  ZSDT0120 테이블에서 해당 WERKS = 화면입력 플랜트 , BUKRS = 화면입력 회사코드 ,  MODUL = 'M'  인 WBSNR 필드를 WBS로 인식
**    - acdoca에서 금액집계할 때의 wbs를 선택하는 부분도 해당
**    - 제조원가 전표처리하는 로직에도 해당
**  1.2  ZSDT0120 테이블에서 해당 WERKS = 화면입력 플랜트 , BUKRS = 화면입력 회사코드 ,  MODUL = 'S'  인 WBSNR 필드를 WBS로 인식
**    - 매출원가 기표할 때 매출원가 계정에 대한 wbs 코드를 결정하는 로직  (==> 이부분은 제가 zcor0380 한번 더 보고 말씀드리겠습니다. )
*



  CLEAR : gt_wbs_cogs, gt_wbs_cogs[].

  SELECT posid
      FROM prps AS a INNER JOIN zsdt0120 AS b
                       ON b~wbsnr EQ a~pspnr
     WHERE b~modul = 'S'
       AND b~bukrs = @pa_bukrs
       AND b~werks EQ @pa_werks
    INTO  TABLE @gt_wbs_cogs.





ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_RANGE_GL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_range_gl .

  DATA et_011z TYPE STANDARD TABLE OF fagl_011zc.
  DATA et_011p TYPE STANDARD TABLE OF fagl_011pc.
  DATA et_011s TYPE STANDARD TABLE OF fagl_011sc.
  DATA et_011v TYPE STANDARD TABLE OF fagl_011vc.
  DATA et_011f TYPE STANDARD TABLE OF fagl_011fc.

  CLEAR :    et_011z,
             et_011p,
             et_011s,
             et_011v,
             et_011f,

             et_011z[],
             et_011p[],
             et_011s[],
             et_011v[],
             et_011f[].

  CALL FUNCTION 'FAGL_FSV_POS_READ'
    EXPORTING
      i_versn       = p_versn
    TABLES
      et_011z       = et_011z
      et_011p       = et_011p
      et_011s       = et_011s
      et_011v       = et_011v
      et_011f       = et_011f
    EXCEPTIONS
      fsv_not_found = 1.

*1011110 미수수익



  CLEAR : gr_gkont1, gr_gkont2.
  REFRESH : gr_gkont1, gr_gkont2.


**          노무비

  LOOP AT  et_011z ASSIGNING FIELD-SYMBOL(<fs>)
                              WHERE ergsl CP '*5052*' .

    CLEAR gr_gkont1.

    gr_gkont1-sign = 'I'.
    gr_gkont1-option = 'BT'.
    gr_gkont1-low = <fs>-vonkt.
    gr_gkont1-high = <fs>-biskt.
    APPEND  gr_gkont1.

  ENDLOOP.


**             경비

  LOOP AT  et_011z ASSIGNING FIELD-SYMBOL(<fs2>)
                              WHERE ergsl CP '*5054*' .

    CLEAR gr_gkont1.

    gr_gkont2-sign = 'I'.
    gr_gkont2-option = 'BT'.
    gr_gkont2-low = <fs2>-vonkt.
    gr_gkont2-high = <fs2>-biskt.
    APPEND  gr_gkont2.

  ENDLOOP.





ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_FI_REV_GL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_fi_rev_gl .

  RANGES : lr_racct FOR acdoca-racct.

  REFRESH lr_racct. CLEAR lr_racct.

  lr_racct = 'IEQ'.
  lr_racct-low = '0402101001' .
  APPEND lr_racct.
  lr_racct-low = '0402101003' .
  APPEND lr_racct.

  CLEAR : gt_fi_rev, gt_fi_rev[].

  PERFORM set_range_data.  "add  bsgsm_fcm 20200910

  SELECT  a~matnr AS matnr,
          SUM( a~hsl  ) AS hsl
     FROM acdoca AS a
    WHERE a~rldnr = '0L'
      AND a~rbukrs = @pa_bukrs
      AND a~werks = @pa_werks
      AND a~budat IN @gr_budat
      AND a~racct IN @lr_racct
*      AND A~RACCT IN ( '0402101001' ,'0402101003' )
     GROUP BY matnr
     ORDER BY matnr
      INTO  CORRESPONDING FIELDS OF TABLE @gt_fi_rev.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPDATE_DATA_RTN1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> PS
*&---------------------------------------------------------------------*
FORM update_data_rtn1  USING   ps STRUCTURE gs_display
                                pv_val.

  DATA: ls_t1260 LIKE zcot1260.

  CASE pv_val .
    WHEN 'YY'.

      UPDATE  zcot1260 SET belnr1 = ''
        WHERE bukrs  = pa_bukrs
          AND spmon  = pa_month
          AND fmatnr = ps-fmatnr
          AND werks  = ps-werks.

      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.
      ELSE.
        ROLLBACK WORK.
      ENDIF.

    WHEN 'RR'.
      UPDATE  zcot1260 SET belnr3 = ''
         WHERE bukrs  = pa_bukrs
          AND spmon  = pa_month
          AND fmatnr = ps-fmatnr
          AND werks  = ps-werks.

      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
  ENDCASE.


ENDFORM.
