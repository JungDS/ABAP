*&---------------------------------------------------------------------*
*& Include          ZCOR0390F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form SET_SCREEN
*&---------------------------------------------------------------------*
FORM set_screen .
*강현수과장  실적만 나오는 리포트임으로 버전을 비활성화요청 20200521

  LOOP AT SCREEN.
    IF screen-name = 'PA_KOKRS' OR screen-name = 'PA_VERSN'.
*    IF SCREEN-NAME = 'PA_KOKRS'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F4_PDGR
*&---------------------------------------------------------------------*
FORM f4_pdgr  CHANGING p_pdgr.

  DATA: help_setnr     LIKE rgsmh-setnr,
        help_searchfld LIKE rgsmh-searchfld,
        help_set       LIKE rgsbs-setnr,
        help_setclass  LIKE rgsmh-class.

  MOVE pa_kokrs TO help_searchfld.

  CALL FUNCTION 'K_GROUP_SELECT'
    EXPORTING
      class           = '0110'
      field_name      = 'POSID'
      searchfld       = help_searchfld
      searchfld_input = ' '
      set             = help_set
    IMPORTING
      set_name        = help_setnr
    EXCEPTIONS
      no_set_picked   = 1.

  IF sy-subrc = 0.
    p_pdgr = help_setnr.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_BL1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM check_bl1 .

*  IF  PA_PERBL < 1 OR PA_PERBL > 12.
  IF  pa_perbl  >  pa_eperl.
    SET CURSOR FIELD 'PA_PERBL'.
    MESSAGE e000  WITH TEXT-e88.
  ELSEIF ( pa_perbl < 1 OR pa_perbl > 12 ) OR
     (  pa_eperl < 1 OR pa_eperl > 12 ).
    SET CURSOR FIELD 'PA_PERBL'.
    MESSAGE e023  WITH TEXT-003.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_BL2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM check_bl2 .
  DATA ls_return TYPE bapiret2.

  IF so_posid[] IS NOT INITIAL AND
     pa_pdgr IS NOT INITIAL.
    SET CURSOR FIELD 'SO_POSID-LOW'.
    MESSAGE e026  WITH TEXT-e01.
  ENDIF.

  IF so_posid[] IS NOT INITIAL.

    SELECT SINGLE * FROM prps
      INTO @DATA(ls_prps)
     WHERE posid IN @so_posid.

    IF sy-subrc <> 0.
      SET CURSOR FIELD 'SO_POSID-LOW'.
      MESSAGE e027  WITH TEXT-e02.
    ENDIF.

  ENDIF.

  IF pa_pdgr IS NOT INITIAL.

    PERFORM get_check_group USING '0110'
                                  pa_pdgr
                            CHANGING ls_return.

    IF ls_return-type = 'E'.
      SET CURSOR FIELD 'PA_PDGR'.
      MESSAGE e027  WITH TEXT-e03.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_CHECK_GROUP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM get_check_group  USING    VALUE(p_setclass)
                                        p_group
*                                        P_CHRT_ACCTS
                               CHANGING ps_return STRUCTURE bapiret2.

  CLEAR ps_return.

  DATA lv_groupname TYPE grpname.
  DATA lv_setclass  TYPE setclass.
  DATA lt_sethier TYPE TABLE OF sethier_co WITH HEADER LINE.

  MOVE: p_group     TO lv_groupname,
        p_setclass  TO lv_setclass.

  CALL FUNCTION 'K_GROUP_REMOTE_READ'
    EXPORTING
      setclass   = lv_setclass
      co_area    = pa_kokrs
      chrt_accts = gc_ktopl
      groupname  = lv_groupname
    IMPORTING
      return     = ps_return
    TABLES
      et_sethier = lt_sethier.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_DATA_RTN
*&---------------------------------------------------------------------*
FORM selected_data_rtn .
  CLEAR : gt_display, gt_display[].

**  PERFORM SET_RANGES_VALUE.
  PERFORM set_ranges_objnr.

  PERFORM set_kstar_ranges_value USING: '0401101001' '0499999999' 'A01',
                                        '0500101001' '0502999999' 'B01',
                                        '0504101001' '0504999999' 'B01',
                                        '0503101001' '0503999999' 'C01',
                                        '0505101001' '0505999999' 'E01',
                                        '0601101001' '0699999999' 'G01',
                                        '0701101001' '0701999999' 'H01',
                                        '0703101001' '0703999999' 'H01',
                                        '0705101001' '0705999999' 'H01',
                                        '0702101001' '0702999999' 'I01',
                                        '0704101001' '0704999999' 'I01',
                                        '0706101001' '0706999999' 'I01',
                                        '0811101001' '0819999999' 'J01',
                                        '0984000100' '0984000999' 'K01'.

  PERFORM get_kagru_ranges_value USING: 'V01R20030' 'D01',
                                        'V01R20040' 'D02',
                                        'V01R20050' 'D03',
                                        'V01R20060' 'D04',
                                        'V01R20070' 'D05',
                                        'V01R20080' 'D06',
                                        'V01R20090' 'D07',
                                        'V01R20100' 'D08',
                                        'V01R20110' 'D09',
                                        'V01R20120' 'D10',
                                        'V01R20130' 'D11',
                                        'V01R20140' 'D12',
                                        'V01R20150' 'D13',
                                        'V01R20160' 'D14',
                                        'V01R20170' 'D15',
                                        'V01R20180' 'D16',
                                        'V01R20190' 'D17',
                                        'V01R20200' 'D18',
                                        'V01R20210' 'D19',
                                        'V01R20220' 'D20',
                                        'V01R20230' 'D21',
                                        'V01R20240' 'D22',
                                        'V01R20250' 'D23',
                                        'V01R20260' 'D24',
                                        'V01R20270' 'D25',
                                        'V01R20280' 'D26',
                                        'V01R20281' 'D27',
                                        'V01R20290' 'D28',
                                        'V01R20300' 'D29',
                                        'V01R20310' 'D30',
                                        'V01R20320' 'D31',
                                        'V01R20330' 'D32',
                                        'V01R20340' 'D33',
                                        'V01R20350' 'D34',
                                        'V01R20360' 'D35',
                                        'V01R20370' 'D36',
                                        'V01R20380' 'D37',
                                        'V01R20390' 'D38',
                                        'V01R20400' 'D39',
                                        'V01R20410' 'D40',
                                        'V01R20420' 'D41',
                                        'V01R20430' 'D42',
                                        'V01R20440' 'D43',
                                        'V01R20450' 'D44',
                                        'V01R20460' 'D45',
                                        'V01R20470' 'D46',
                                        'V01R20475' 'D47',
                                        'V01R20480' 'D48'.



*ADD BSGSM_FCM   이병기K REQ  20210309
* BSGSM_FCM  20210309 아래 주석

**
**  LOOP AT gt_display.
**    gt_display-f01 = gt_display-b01 + gt_display-c01 + gt_display-e01.
**    MODIFY gt_display.
**    CLEAR gt_display.
**  ENDLOOP.


**START
*
  IF gt_display[] IS NOT INITIAL.

    lt_display[] = gt_display[].
    SORT lt_display BY posid.

    SELECT a~posid,  b~zzsct, b~zzscttx
      FROM prps AS a INNER JOIN zcot1010t AS b
       ON a~zzsct =  b~zzsct
**********************************************************************
      JOIN PROJ AS C ON C~PSPNR EQ A~PSPHI
**********************************************************************
      FOR ALL ENTRIES IN @lt_display
     WHERE a~posid = @lt_display-posid
**********************************************************************
       AND C~PROFL IN @R_PROFL
**********************************************************************
      INTO TABLE @DATA(lt_zzsct).


    SORT lt_zzsct BY posid.

  ENDIF.
*


  LOOP AT gt_display ASSIGNING FIELD-SYMBOL(<$fs>).
    <$fs>-f01 = <$fs>-b01 + <$fs>-c01 + <$fs>-e01.

    READ TABLE lt_zzsct ASSIGNING FIELD-SYMBOL(<zz>) WITH KEY posid = <$fs>-posid BINARY SEARCH.

    IF sy-subrc EQ 0.

      <$fs>-zzsct = <zz>-zzsct.
      <$fs>-zzscttx = <zz>-zzscttx.

    ENDIF.

  ENDLOOP.

**END BY  BSGSM_FCM   20210309...!!

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_RANGES_VALUE
*&---------------------------------------------------------------------*
FORM set_ranges_value .
  CLEAR: r_sale, r_sale[], r_cost, r_cost[].

  MOVE: 'I'          TO r_sale-sign,
        'BT'         TO r_sale-option,
        '0401101001' TO r_sale-low,
        '0409999999' TO r_sale-high.

  APPEND r_sale.
  CLEAR  r_sale.

  MOVE: 'E'          TO r_sale-sign,
        'EQ'         TO r_sale-option,
        '0405101004' TO r_sale-low.

  APPEND r_sale.
  CLEAR  r_sale.

  MOVE: 'I'          TO r_cost-sign,
        'BT'         TO r_cost-option,
*        '0501101001' TO R_COST-LOW,
        '0500101001' TO r_cost-low,
        '0503599999' TO r_cost-high.

  APPEND r_cost.
  CLEAR  r_cost.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_RANGES_OBJNR
*&---------------------------------------------------------------------*
FORM set_ranges_objnr .
  CLEAR: gt_values, gt_values[],
         r_objnr,   r_objnr[].

  IF so_posid[] IS NOT INITIAL.

    LOOP AT so_posid.

      MOVE: so_posid-sign   TO r_objnr-sign,
            so_posid-option TO r_objnr-option.

      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
        EXPORTING
          input     = so_posid-low
        IMPORTING
          output    = r_objnr-low
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.

      r_objnr-low  = 'PR' && r_objnr-low.

      IF so_posid-high IS NOT INITIAL.

        CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
          EXPORTING
            input     = so_posid-high
          IMPORTING
            output    = r_objnr-high
          EXCEPTIONS
            not_found = 1
            OTHERS    = 2.

        r_objnr-high  = 'PR' && r_objnr-high.

      ENDIF.

      COLLECT r_objnr.
      CLEAR   r_objnr.

    ENDLOOP.

  ENDIF.

  IF pa_pdgr IS NOT INITIAL.

    PERFORM read_hierarchy_tables TABLES gt_values
                                  USING '0110'
                                        pa_pdgr.   "WBS 요소 그룹

    LOOP AT gt_values.

      MOVE: 'I'   TO r_objnr-sign,
            'BT'  TO r_objnr-option.

      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
        EXPORTING
          input     = gt_values-vfrom
        IMPORTING
          output    = r_objnr-low
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.

      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
        EXPORTING
          input     = gt_values-vto
        IMPORTING
          output    = r_objnr-high
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.

      r_objnr-low   = 'PR' && r_objnr-low.
      r_objnr-high  = 'PR' && r_objnr-high.

      COLLECT r_objnr.
      CLEAR   r_objnr.

    ENDLOOP.

  ENDIF.



*--------------------------------------------------------------------*
* [CO] ESG Pjt. 설비WBS 검색제외 - 2021.11.10 14:25:25, MDP_06
*--------------------------------------------------------------------*
  IF PA_EQWBS IS INITIAL.
    " 설비WBS 제외
    R_PROFL[] = VALUE #( ( CONV #( 'EEQZ000003' ) ) ).
  ELSE.
    " 설비WBS 포함
    CLEAR: R_PROFL, R_PROFL[].
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form READ_HIERARCHY_TABLES
*&---------------------------------------------------------------------*
FORM read_hierarchy_tables TABLES pt_values STRUCTURE grpvalues
                            USING pv_class TYPE setclass
                                  pv_setid.

  DATA: lv_setid     LIKE sethier-setid,
        lv_overwrite LIKE sy-datar,
        lt_info      LIKE grphinfo OCCURS 0 WITH HEADER LINE,
        lt_nodes     LIKE grpobjects OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'G_SET_ENCRYPT_SETID'
    EXPORTING
      setclass  = pv_class
      shortname = pv_setid  "코스트센터그룹
    IMPORTING
      setid     = lv_setid.

  IF sy-subrc = 0.

    CALL FUNCTION 'K_HIERARCHY_TABLES_READ'
      EXPORTING
        e_class                     = pv_class
        e_setid                     = lv_setid
        e_kokrs                     = pa_kokrs
      TABLES
        t_nodes                     = lt_nodes
        t_values                    = pt_values
      CHANGING
        c_info                      = lt_info
        c_overwrite                 = lv_overwrite
      EXCEPTIONS
        no_controlling_area         = 1
        no_chart_of_account         = 2
        different_controlling_areas = 3
        different_chart_of_accounts = 4
        set_not_found               = 5
        illegal_field_replacement   = 6
        illegal_table_replacement   = 7
        fm_raise                    = 8
        convert_error               = 9
        no_overwrite_standard_hier  = 10
        no_bukrs_for_kokrs          = 11
        OTHERS                      = 12.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_INSTANCE_0100
*&---------------------------------------------------------------------*
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
  CREATE OBJECT gr_splitter1
    EXPORTING
      rows    = 2
      columns = 1
      parent  = cl_gui_splitter_container=>screen0.

*== get container instance
*-- 1. top of page
  gr_parent_html = gr_splitter1->get_container(
      row       = 1
      column    = 1 ).

  gr_data_container = gr_splitter1->get_container(
      row       = 2
      column    = 1 ).

  CALL METHOD gr_splitter1->set_row_height
    EXPORTING
      id     = 1
      height = 7.

  CALL METHOD gr_splitter1->set_row_height
    EXPORTING
      id     = 2
      height = 50.

  CREATE OBJECT gr_grid1
    EXPORTING
      i_parent = gr_data_container.

ENDFORM.                    " CREATE_INSTANCE_0100
*&---------------------------------------------------------------------*
*&      Form  INIT_LAYOUT_0100
*&---------------------------------------------------------------------*
FORM init_layout_0100.

  CLEAR gs_layout.

*  GS_LAYOUT-EDIT_MODE  = ABAP_TRUE.
  gs_layout-zebra      = abap_true.
  GS_LAYOUT-CWIDTH_OPT = ABAP_TRUE."  주석해제 2021 0317  강현수k 요청
  gs_layout-sel_mode   = space.     "B:단일,C:복수,D:셀,A:행/열
  gs_layout-box_fname  = space.
  gs_layout-no_rowmark = space.

*  GS_LAYOUT-STYLEFNAME = 'STYLE'.
*  GS_LAYOUT-CTAB_FNAME = 'COLOR'.
*  GS_LAYOUT-INFO_FNAME = 'INFO'.

**  "alv title
**  GS_LAYOUT-GRID_TITLE = TEXT-GT1.

ENDFORM.                    " INIT_LAYOUT_0100
*&---------------------------------------------------------------------*
*&      Form  SET_GRID_EXCLUDE_0100
*&---------------------------------------------------------------------*
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
FORM alv_sort_0100 .

  CLEAR: gs_sort, gt_sort.
  REFRESH: gt_sort.

ENDFORM.                    " ALV_SORT_0100
*&---------------------------------------------------------------------*
*&      Form  APPEND_FIELDCAT_0100
*&---------------------------------------------------------------------*
FORM append_fieldcat_0100 .

  "-- field catalog data
  "   field catalog merge or set fieldcatalog를 사용할 수 있음.

  "{ FIELDCATLOG MERGE 사용
  PERFORM get_fieldcatlog_data.

  PERFORM modify_fieldcatlog_data.
  "}

  "{ SET FIELDCATLOG 사용
*  PERFORM SET_FIELDCATLOG_DATA.
  "}

ENDFORM.                    " APPEND_FIELDCAT_0100
*&---------------------------------------------------------------------*
*&      Form  GET_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
FORM get_fieldcatlog_data .

  DATA: lt_fieldcat TYPE kkblo_t_fieldcat.

  CALL FUNCTION 'K_KKB_FIELDCAT_MERGE'
    EXPORTING
      i_callback_program     = sy-repid
      i_strucname            = 'ZCOS0370' "ABAP DIC. 정의된 STRUCTURE
      i_bypassing_buffer     = abap_true
      i_inclname             = sy-repid
    CHANGING
      ct_fieldcat            = lt_fieldcat[]
    EXCEPTIONS
      inconsistent_interface = 1
      OTHERS                 = 2.

  IF sy-subrc EQ 0.

    "-- Trasnfer LVC.
    CALL FUNCTION 'LVC_TRANSFER_FROM_KKBLO'
      EXPORTING
        it_fieldcat_kkblo = lt_fieldcat[]
      IMPORTING
        et_fieldcat_lvc   = gt_fieldcat[]
      EXCEPTIONS
        it_data_missing   = 1.
  ELSE.

    MESSAGE e020.

  ENDIF.

ENDFORM.                    " GET_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
FORM modify_fieldcatlog_data .

  DATA:  lv_text(50).

  LOOP AT gt_fieldcat INTO gs_fieldcat.

    CLEAR: lv_text.

    CASE gs_fieldcat-fieldname.
      WHEN 'POSID'.
        lv_text               = TEXT-c01.
        gs_fieldcat-emphasize = 'C112'.

      WHEN 'POST1'.
        lv_text = TEXT-c02.

      WHEN 'TWAER'.
        gs_fieldcat-no_out = abap_true.

      WHEN 'A01'.
        lv_text = TEXT-f01.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'B01'.
        lv_text = TEXT-f02.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'C01'.
        lv_text = TEXT-f03.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'E01'.
        lv_text = TEXT-f04.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'F01'.
        lv_text = TEXT-f05.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'G01'.
        lv_text = TEXT-f06.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'H01'.
        lv_text = TEXT-f07.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'I01'.
        lv_text = TEXT-f08.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'J01'.
        lv_text = TEXT-f09.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'K01'.
        lv_text = TEXT-f10.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D01'.
        lv_text = TEXT-d01.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D02'.
        lv_text = TEXT-d02.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D03'.
        lv_text = TEXT-d03.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D04'.
        lv_text = TEXT-d04.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D05'.
        lv_text = TEXT-d05.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D06'.
        lv_text = TEXT-d06.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D07'.
        lv_text = TEXT-d07.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D08'.
        lv_text = TEXT-d08.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D09'.
        lv_text = TEXT-d09.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D10'.
        lv_text = TEXT-d10.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D11'.
        lv_text = TEXT-d11.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D12'.
        lv_text = TEXT-d12.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D13'.
        lv_text = TEXT-d13.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D14'.
        lv_text = TEXT-d14.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D15'.
        lv_text = TEXT-d15.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D16'.
        lv_text = TEXT-d16.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D17'.
        lv_text = TEXT-d17.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D18'.
        lv_text = TEXT-d18.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D19'.
        lv_text = TEXT-d19.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D20'.
        lv_text = TEXT-d20.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D21'.
        lv_text = TEXT-d21.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D22'.
        lv_text = TEXT-d22.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D23'.
        lv_text = TEXT-d23.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D24'.
        lv_text = TEXT-d24.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D25'.
        lv_text = TEXT-d25.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D26'.
        lv_text = TEXT-d26.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D27'.
        lv_text = TEXT-d27.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D28'.
        lv_text = TEXT-d28.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D29'.
        lv_text = TEXT-d29.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D30'.
        lv_text = TEXT-d30.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D31'.
        lv_text = TEXT-d31.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D32'.
        lv_text = TEXT-d32.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D33'.
        lv_text = TEXT-d33.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D34'.
        lv_text = TEXT-d34.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D35'.
        lv_text = TEXT-d35.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D36'.
        lv_text = TEXT-d36.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D37'.
        lv_text = TEXT-d37.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D38'.
        lv_text = TEXT-d38.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D39'.
        lv_text = TEXT-d39.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D40'.
        lv_text = TEXT-d40.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D41'.
        lv_text = TEXT-d41.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D42'.
        lv_text = TEXT-d42.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D43'.
        lv_text = TEXT-d43.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D44'.
        lv_text = TEXT-d44.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D45'.
        lv_text = TEXT-d45.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D46'.
        lv_text = TEXT-d46.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D47'.
        lv_text = TEXT-d47.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

      WHEN 'D48'.
        lv_text = TEXT-d48.
        gs_fieldcat-outputlen = 15.
        gs_fieldcat-do_sum = abap_true.

    ENDCASE.

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
FORM set_fieldcatlog_data.

  CLEAR gt_fieldcat[].

  PERFORM fill_field_category USING :
        'S' 'FIELDNAME'   'GJAHR',
        ' ' 'OUTPUTLEN'   '4',
        'E' 'COLTEXT'     '적용연도',

        'S' 'FIELDNAME'   'CARRID',
        ' ' 'OUTPUTLEN'   '3',
        ' ' 'EDIT'        'X',
        'E' 'COLTEXT'     '항공사 ID'.

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
*& Form TOP_OF_PAGE_CREATE_OBJECT_0100
*&---------------------------------------------------------------------*
FORM top_of_page_create_object_0100 .

* Create TOP-Document
  CREATE OBJECT gr_top_document
    EXPORTING
      style = 'ALV_GRID'.

* Initialize
  CALL METHOD gr_top_document->initialize_document( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_TOP_OF_PAGE_DATA_0100
*&---------------------------------------------------------------------*
FORM make_top_of_page_data_0100 .

  DATA: lt_texts TYPE sdydo_text_table,
        lv_text  TYPE sdydo_text_element.

  DATA : lv_ss TYPE n LENGTH 2.
  DATA : lv_ee TYPE n LENGTH 2.


  CONCATENATE TEXT-001 ':' pa_kokrs
        INTO lv_text SEPARATED BY space.

  CALL METHOD gr_top_document->add_text
    EXPORTING
      text         = lv_text
      sap_color    = cl_dd_document=>list_heading_int
      sap_emphasis = cl_dd_area=>key
      style_class  = space.

  CALL METHOD gr_top_document->new_line.
*
  CONCATENATE TEXT-002 ':' pa_gjahr
        INTO lv_text SEPARATED BY space.

  CALL METHOD gr_top_document->add_text
    EXPORTING
      text         = lv_text
      sap_color    = cl_dd_document=>list_heading_int
      sap_emphasis = cl_dd_area=>key
      style_class  = space.

  CALL METHOD gr_top_document->new_line.

  lv_ss = pa_perbl.
  lv_ee = pa_eperl.

  CONCATENATE TEXT-003 ':' lv_ss '~' lv_ee
                            INTO lv_text SEPARATED BY space.

  CALL METHOD gr_top_document->add_text
    EXPORTING
      text         = lv_text
      sap_color    = cl_dd_document=>list_heading_int
      sap_emphasis = cl_dd_area=>key
      style_class  = space.

  CALL METHOD gr_top_document->new_line.

  CONCATENATE TEXT-004 ':' pa_versn
        INTO lv_text SEPARATED BY space.

  CALL METHOD gr_top_document->add_text
    EXPORTING
      text         = lv_text
      sap_color    = cl_dd_document=>list_heading_int
      sap_emphasis = cl_dd_area=>key
      style_class  = space.

  CALL METHOD gr_top_document->new_line.

*  CONCATENATE TEXT-008 ` / ` TEXT-009 ` / ` TEXT-010
*        INTO LV_TEXT SEPARATED BY SPACE.
*
*  CALL METHOD GR_TOP_DOCUMENT->ADD_TEXT
*    EXPORTING
*      TEXT         = LV_TEXT
*      SAP_COLOR    = CL_DD_DOCUMENT=>LIST_HEADING_INT
*      SAP_EMPHASIS = CL_DD_AREA=>KEY
*      STYLE_CLASS  = SPACE.
*
*  CALL METHOD GR_TOP_DOCUMENT->NEW_LINE
*    EXPORTING
*      REPEAT = 1.

*  CALL METHOD GR_TOP_DOCUMENT->ADD_GAP
*    EXPORTING
*      WIDTH = 20.

  " Get Ready
  CALL METHOD gr_top_document->merge_document.

*" Display TOP document
  CALL METHOD gr_top_document->display_document
    EXPORTING
      reuse_control      = 'X'
      parent             = gr_parent_html
    EXCEPTIONS
      html_display_error = 1.

ENDFORM.
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
*  SET HANDLER:
*    GR_EVENT_RECEIVER->HANDLE_TOOLBAR       FOR ALL INSTANCES,
*    GR_EVENT_RECEIVER->HANDLE_DATA_CHANGED  FOR ALL INSTANCES,
*    GR_EVENT_RECEIVER->HANDLE_DATA_CHANGED_FINISHED
*      FOR ALL INSTANCES,
*    GR_EVENT_RECEIVER->HANDLE_USER_COMMAND  FOR ALL INSTANCES,
*    GR_EVENT_RECEIVER->HANDLE_HOTSPOT_CLICK FOR ALL INSTANCES,
*    GR_EVENT_RECEIVER->HANDLE_DOUBLE_CLICK  FOR ALL INSTANCES,
*    GR_EVENT_RECEIVER->HANDLE_ONF4          FOR ALL INSTANCES.

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

  CALL METHOD cl_gui_cfw=>flush.

ENDFORM.                    " REFRESH_GRID_0100
*&---------------------------------------------------------------------*
*& Form SET_INIT_HELP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM set_init_help .

  "__ Function Key
  DATA: ls_funtxt TYPE smp_dyntxt.

  ls_funtxt-icon_id   = icon_information.
  ls_funtxt-quickinfo = 'Program Help'.

  sscrfields-functxt_01 = ls_funtxt.

  "__ 20191223 BSGSM_FCM ADD default cac
  SET PARAMETER ID 'CAC' FIELD pa_kokrs.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCR_USER_COMMAND_HELP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM scr_user_command_help .

  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      PERFORM call_popup_help(zcar9000) USING sy-repid sy-dynnr sy-langu ''.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_KSTAR_RANGES_VALUE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM set_kstar_ranges_value  USING    pv_from
                                      pv_to
                                      pv_field.


  CLEAR: r_kstar, r_kstar[], gt_itab, gt_itab[].

  MOVE: 'I'          TO r_kstar-sign,
        'BT'         TO r_kstar-option,
        pv_from      TO r_kstar-low,
        pv_to        TO r_kstar-high.

  APPEND r_kstar.
  CLEAR  r_kstar.

  SELECT b~posid, b~post1, 'KRW' AS twaer,
         SUM( a~wkg001 ) AS wkg001,
         SUM( a~wkg002 ) AS wkg002,
         SUM( a~wkg003 ) AS wkg003,
         SUM( a~wkg004 ) AS wkg004,
         SUM( a~wkg005 ) AS wkg005,
         SUM( a~wkg006 ) AS wkg006,
         SUM( a~wkg007 ) AS wkg007,
         SUM( a~wkg008 ) AS wkg008,
         SUM( a~wkg009 ) AS wkg009,
         SUM( a~wkg010 ) AS wkg010,
         SUM( a~wkg011 ) AS wkg011,
         SUM( a~wkg012 ) AS wkg012
    FROM cosp AS a JOIN prps AS b
                     ON a~objnr = b~objnr
**********************************************************************
                   JOIN PROJ AS C ON C~PSPNR EQ B~PSPHI
**********************************************************************
   WHERE a~lednr  = '00'
     AND a~gjahr  = @pa_gjahr
     AND a~versn  = @pa_versn
     AND a~wrttp IN ('04', '60')
     AND a~objnr IN @r_objnr
     AND a~kstar IN @r_kstar
     AND b~pbukr IN @so_bukrs
**********************************************************************
     AND C~PROFL IN @R_PROFL
**********************************************************************
   GROUP BY b~posid, b~post1
  UNION ALL
  SELECT b~posid, b~post1, 'KRW' AS twaer,
         SUM( a~wkg001 ) AS wkg001,
         SUM( a~wkg002 ) AS wkg002,
         SUM( a~wkg003 ) AS wkg003,
         SUM( a~wkg004 ) AS wkg004,
         SUM( a~wkg005 ) AS wkg005,
         SUM( a~wkg006 ) AS wkg006,
         SUM( a~wkg007 ) AS wkg007,
         SUM( a~wkg008 ) AS wkg008,
         SUM( a~wkg009 ) AS wkg009,
         SUM( a~wkg010 ) AS wkg010,
         SUM( a~wkg011 ) AS wkg011,
         SUM( a~wkg012 ) AS wkg012
    FROM coss AS a JOIN prps AS b
                     ON a~objnr = b~objnr
**********************************************************************
                   JOIN PROJ AS C ON C~PSPNR EQ B~PSPHI
**********************************************************************
   WHERE a~lednr  = '00'
     AND a~gjahr  = @pa_gjahr
     AND a~versn  = @pa_versn
     AND a~wrttp IN ('04', '60')
     AND a~objnr IN @r_objnr
     AND a~kstar IN @r_kstar
     AND b~pbukr IN @so_bukrs
**********************************************************************
     AND C~PROFL IN @R_PROFL
**********************************************************************
   GROUP BY b~posid, b~post1
    INTO TABLE @gt_itab.

  DATA: lv_fieldname1 TYPE fieldname,
        lv_fieldname2 TYPE fieldname.

  FIELD-SYMBOLS: <fs_tot1> TYPE any,
                 <fs_tot2> TYPE any.

  " ADD  BY BSGSM_fCM
  DATA lv_cnt TYPE n LENGTH 2.
  DATA lv_do_mm TYPE numc3.

  lv_cnt = pa_eperl - pa_perbl.
  lv_cnt = lv_cnt  + 1.
  lv_do_mm = pa_perbl.

  LOOP AT gt_itab ASSIGNING FIELD-SYMBOL(<fs_data>).
    gt_display-posid = <fs_data>-posid.
    gt_display-post1 = <fs_data>-post1.
    gt_display-twaer = <fs_data>-twaer.


    DO lv_cnt TIMES.

      lv_fieldname1 = '<FS_DATA>-WKG' && lv_do_mm.
      ASSIGN (lv_fieldname1) TO <fs_tot1>.

      IF <fs_tot1> IS NOT INITIAL.

        IF pv_field EQ 'A01' OR pv_field EQ 'H01'.
          <fs_tot1> = <fs_tot1> * -1.
        ENDIF.

        lv_fieldname2 = 'GT_DISPLAY-' && pv_field.
        ASSIGN (lv_fieldname2) TO <fs_tot2>.

        <fs_tot2> = <fs_tot1>.

        COLLECT gt_display.

      ENDIF.

      lv_do_mm  = lv_do_mm + 1.

    ENDDO.
    lv_do_mm = pa_perbl. "다시 시작
**end by bsgsm_fcm

    CLEAR: gt_display, <fs_data>.
  ENDLOOP.

***아래 주석후 위 수정  20200521

***    LV_FIELDNAME1 = '<FS_DATA>-WKG' && PA_PERBL.
***    ASSIGN (LV_FIELDNAME1) TO <FS_TOT1>.
***
***    IF <FS_TOT1> IS INITIAL.
***      CONTINUE.
***    ENDIF.
***
***    IF PV_FIELD EQ 'A01' OR PV_FIELD EQ 'H01'.
***      <FS_TOT1> = <FS_TOT1> * -1.
***    ENDIF.
***
***    LV_FIELDNAME2 = 'GT_DISPLAY-' && PV_FIELD.
***    ASSIGN (LV_FIELDNAME2) TO <FS_TOT2>.
***
***    <FS_TOT2> = <FS_TOT1>.
***
***    COLLECT GT_DISPLAY.

***    CLEAR: GT_DISPLAY, <FS_DATA>.
*  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_KAGRU_RANGES_VALUE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM get_kagru_ranges_value  USING    pv_kagru
                                      pv_field.

  CLEAR: r_kstar, r_kstar[], gt_itab, gt_itab[].

  PERFORM read_hierarchy_tables TABLES gt_values
                                USING '0102'
                                      pv_kagru.  "원가요소 그룹

  LOOP AT gt_values.

    MOVE: 'I'               TO r_kstar-sign,
           'BT'             TO r_kstar-option,
           gt_values-vfrom  TO r_kstar-low,
           gt_values-vto    TO r_kstar-high.

    COLLECT r_kstar.
    CLEAR   r_kstar.
  ENDLOOP.

  SELECT b~posid, b~post1, 'KRW' AS twaer,
         SUM( a~wkg001 ) AS wkg001,
         SUM( a~wkg002 ) AS wkg002,
         SUM( a~wkg003 ) AS wkg003,
         SUM( a~wkg004 ) AS wkg004,
         SUM( a~wkg005 ) AS wkg005,
         SUM( a~wkg006 ) AS wkg006,
         SUM( a~wkg007 ) AS wkg007,
         SUM( a~wkg008 ) AS wkg008,
         SUM( a~wkg009 ) AS wkg009,
         SUM( a~wkg010 ) AS wkg010,
         SUM( a~wkg011 ) AS wkg011,
         SUM( a~wkg012 ) AS wkg012
    FROM cosp AS a JOIN prps AS b
                     ON a~objnr = b~objnr
**********************************************************************
                   JOIN PROJ AS C ON C~PSPNR EQ B~PSPHI
**********************************************************************
   WHERE a~lednr  = '00'
     AND a~gjahr  = @pa_gjahr
     AND a~versn  = @pa_versn
     AND a~wrttp IN ('04', '60')
     AND a~objnr IN @r_objnr
     AND a~kstar IN @r_kstar
     AND b~pbukr IN @so_bukrs
**********************************************************************
     AND C~PROFL IN @R_PROFL
**********************************************************************
   GROUP BY b~posid, b~post1
  UNION ALL
  SELECT b~posid, b~post1, 'KRW' AS twaer,
         SUM( a~wkg001 ) AS wkg001,
         SUM( a~wkg002 ) AS wkg002,
         SUM( a~wkg003 ) AS wkg003,
         SUM( a~wkg004 ) AS wkg004,
         SUM( a~wkg005 ) AS wkg005,
         SUM( a~wkg006 ) AS wkg006,
         SUM( a~wkg007 ) AS wkg007,
         SUM( a~wkg008 ) AS wkg008,
         SUM( a~wkg009 ) AS wkg009,
         SUM( a~wkg010 ) AS wkg010,
         SUM( a~wkg011 ) AS wkg011,
         SUM( a~wkg012 ) AS wkg012
    FROM coss AS a JOIN prps AS b
                     ON a~objnr = b~objnr
**********************************************************************
                   JOIN PROJ AS C ON C~PSPNR EQ B~PSPHI
**********************************************************************
   WHERE a~lednr  = '00'
     AND a~gjahr  = @pa_gjahr
     AND a~versn  = @pa_versn
     AND a~wrttp IN ('04', '60')
     AND a~objnr IN @r_objnr
     AND a~kstar IN @r_kstar
     AND b~pbukr IN @so_bukrs
**********************************************************************
     AND C~PROFL IN @R_PROFL
**********************************************************************
   GROUP BY b~posid, b~post1
    INTO TABLE @gt_itab.

  DATA: lv_fieldname1 TYPE fieldname,
        lv_fieldname2 TYPE fieldname.

  FIELD-SYMBOLS: <fs_tot1> TYPE any,
                 <fs_tot2> TYPE any.

  DATA lv_cnt TYPE n LENGTH 2.
  DATA lv_do_mm TYPE numc3.
  CLEAR : lv_cnt, lv_do_mm.

  lv_cnt = pa_eperl - pa_perbl.
  lv_cnt = lv_cnt + 1.
  lv_do_mm = pa_perbl.

  LOOP AT gt_itab ASSIGNING FIELD-SYMBOL(<fs_data>).
    gt_display-posid = <fs_data>-posid.
    gt_display-post1 = <fs_data>-post1.
    gt_display-twaer = <fs_data>-twaer.


    DO  lv_cnt TIMES.

      lv_fieldname1 = '<FS_DATA>-WKG' &&  lv_do_mm.

      ASSIGN (lv_fieldname1) TO <fs_tot1>.

      IF <fs_tot1> IS NOT INITIAL.

        lv_fieldname2 = 'GT_DISPLAY-' && pv_field.
        ASSIGN (lv_fieldname2) TO <fs_tot2>.

        <fs_tot2> = <fs_tot1>.

        COLLECT gt_display.

      ENDIF.


      lv_do_mm  = lv_do_mm + 1.

    ENDDO.
    lv_do_mm = pa_perbl. "다시 시작

    CLEAR: gt_display, <fs_data>.
  ENDLOOP.



****  아래 주석처리후  위 추가  by bsgsm_fcm  20200521
****  LOOP AT GT_ITAB ASSIGNING FIELD-SYMBOL(<FS_DATA>).
****    GT_DISPLAY-POSID = <FS_DATA>-POSID.
****    GT_DISPLAY-POST1 = <FS_DATA>-POST1.
****    GT_DISPLAY-TWAER = <FS_DATA>-TWAER.
****
****    LV_FIELDNAME1 = '<FS_DATA>-WKG' && PA_PERBL.
****    ASSIGN (LV_FIELDNAME1) TO <FS_TOT1>.
****
****    IF <FS_TOT1> IS INITIAL.
****      CONTINUE.
****    ENDIF.
****
****    LV_FIELDNAME2 = 'GT_DISPLAY-' && PV_FIELD.
****    ASSIGN (LV_FIELDNAME2) TO <FS_TOT2>.
****
****    <FS_TOT2> = <FS_TOT1>.
****
****    COLLECT GT_DISPLAY.
****
****    CLEAR: GT_DISPLAY, <FS_DATA>.
****  ENDLOOP.

ENDFORM.
