*&---------------------------------------------------------------------*
*& Include          ZCOR0270F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITAIL
*&---------------------------------------------------------------------*
FORM initail .
  gv_repid = sy-repid.

  SELECT SINGLE bezei INTO @pa_ktxt
    FROM tka01
   WHERE kokrs = @pa_kokrs.

  SET CURSOR FIELD 'PA_POSID'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SCREEN
*&---------------------------------------------------------------------*
FORM set_screen .

  LOOP AT SCREEN.
    IF screen-name = 'PA_KOKRS'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F4_KSGRU
*&---------------------------------------------------------------------*
FORM f4_ksgru  CHANGING p_ksgru.

  DATA: help_setnr     LIKE rgsmh-setnr,
        help_searchfld LIKE rgsmh-searchfld,
        help_set       LIKE rgsbs-setnr,
        help_setclass  LIKE rgsmh-class.

  MOVE pa_kokrs TO help_searchfld.

  CALL FUNCTION 'K_GROUP_SELECT'
    EXPORTING
      class           = '0101'
      field_name      = 'KOSTL'
      searchfld       = help_searchfld
      searchfld_input = ' '
      set             = help_set
    IMPORTING
      set_name        = help_setnr
    EXCEPTIONS
      no_set_picked   = 1.

  IF sy-subrc = 0.
    p_ksgru = help_setnr.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_BL1
*&---------------------------------------------------------------------*
FORM check_bl1 .
  DATA ls_return TYPE bapiret2.

  IF so_kostl[] IS NOT INITIAL AND
     pa_ksgru IS NOT INITIAL.
    SET CURSOR FIELD 'SO_KOSTL-LOW'.
    MESSAGE e000  WITH TEXT-e02.
  ENDIF.

  IF so_kostl[] IS NOT INITIAL.

    SELECT SINGLE * FROM csks
      INTO @DATA(ls_csks)
     WHERE kokrs = @pa_kokrs
       AND kostl IN @so_kostl
       AND datbi >= @sy-datum
       AND datab <= @sy-datum.

    IF sy-subrc <> 0.
      SET CURSOR FIELD 'SO_KOSTL-LOW'.
      MESSAGE e027  WITH TEXT-e03.
    ENDIF.

  ENDIF.

  IF pa_ksgru IS NOT INITIAL.

    PERFORM get_check_group USING '0101'
                                  pa_ksgru
                            CHANGING ls_return.
    IF ls_return-type = 'E'.
      SET CURSOR FIELD 'PA_KSGRU'.
      MESSAGE e027  WITH TEXT-e04.
    ENDIF.

  ENDIF.

  SELECT SINGLE versi INTO @DATA(lv_tkvs)
    FROM tkvs
   WHERE versi = @pa_versn.

  IF sy-subrc <> 0.
    SET CURSOR FIELD 'PA_VERSN'.
    MESSAGE e027  WITH TEXT-e08.

  ELSEIF sy-subrc = 0 AND ( pa_versn = 'B0' OR
                            pa_versn = 'B1' ).

    SET CURSOR FIELD 'PA_VERSN'.
    MESSAGE e000  WITH TEXT-e09.
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

  PERFORM set_ranges_objnr.
*  PERFORM selected_main_data.

  IF gv_error IS INITIAL AND ( r_objnr[] IS NOT INITIAL OR gv_super EQ 'X' ).

    PERFORM selected_main_data.

  ELSE.
    IF gv_error IS NOT INITIAL.

      MESSAGE s000(zco01) WITH gv_error DISPLAY LIKE  gc_e.

    ELSE.
      MESSAGE s004(zco01).

    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_RANGES_OBJNR
*&---------------------------------------------------------------------*
FORM set_ranges_objnr .
  CLEAR: gt_values, gt_values[],
         r_objnr,   r_objnr[].

*  IF so_kostl[] IS NOT INITIAL.
*
*    LOOP AT so_kostl.
*
*      MOVE: so_kostl-sign   TO r_objnr-sign,
*            so_kostl-option TO r_objnr-option.
*
*      r_objnr-low  = 'KS' && pa_kokrs && so_kostl-low.
*
*      IF so_kostl-high IS NOT INITIAL.
*        r_objnr-high = 'KS' && pa_kokrs && so_kostl-high.
*      ENDIF.
*
*      COLLECT r_objnr.
*      CLEAR   r_objnr.
*
*    ENDLOOP.
*
*  ENDIF.
*
*  IF pa_ksgru IS NOT INITIAL.
*
*    PERFORM read_hierarchy_tables TABLES gt_values
*                                  USING '0101'
*                                        pa_ksgru.  "코스트센터 그룹
*
*    LOOP AT gt_values.
*
*      MOVE: 'I'   TO r_objnr-sign,
*             'BT' TO r_objnr-option.
*
*      r_objnr-low  = 'KS' && pa_kokrs && gt_values-vfrom.
*      r_objnr-high = 'KS' && pa_kokrs &&  gt_values-vto.
*
*      COLLECT r_objnr.
*      CLEAR   r_objnr.
*
*    ENDLOOP.
*
*  ENDIF.


*>>>>>>>>>>>> 위 로직  주석 후  아래  추가 ....
**  super, co 슈퍼이면 기존 로직
** >>>>>>>>>>>>>>>>>> 2021.03.24 CBO 권한 체크 추가         BSGSM_FCM  START..~~~~~~~~~

** IF   ZCOT0320-bukrs2, prctr2  권한 있으면 해당 회사 CO OBJECT 으로 제한후  input  파라메터로 제한
**  ELSEIF . ZCOT0320   " 회사 코드 등록은 없고   손익센터가 있으면    해당 손익센터 CO OBJECT 만


  DATA lv_type TYPE c.
  DATA lv_message TYPE bapi_msg.

  DATA: lt_0070  LIKE TABLE OF zcas0070,
        ls_0070  LIKE zcas0070,
        lv_class TYPE zcat0031-cd_class,
        lv_code  TYPE zcat0031-cd_code.

  DATA : lv_super(1).
  DATA : lv_super_co(1).


  CLEAR lv_super.
  CLEAR lv_super_co.
  CLEAR gv_super.

  CLEAR : gv_error .

  RANGES : lr_prctr FOR csks-prctr.
  RANGES : lr_bukrs FOR zcot0320-bukrs.

  CLEAR : lr_prctr,  lr_bukrs .
  REFRESH:  lr_prctr, lr_bukrs.


**---------------

  lv_class = 'CASUSR'.
  lv_code  = sy-uname.

  "__ SUPER USER ID 체크
  PERFORM call_f4_values(zcar9000) TABLES lt_0070
                                    USING lv_class lv_code ls_0070.
  IF lt_0070[] IS NOT INITIAL.
    lv_super = abap_true.
  ELSE.
    lv_class = 'CASUCO'.
    lv_code  = sy-uname.

    "__ SUPER USER ID 체크
    PERFORM call_f4_values(zcar9000) TABLES lt_0070
                                      USING lv_class lv_code ls_0070.
    IF lt_0070[] IS NOT INITIAL.
      lv_super_co = abap_true.
    ENDIF.
  ENDIF.

**-----------------

  IF lv_super IS NOT INITIAL OR   lv_super_co IS NOT INITIAL. "  CO OBJECT 권한 체크

    gv_super = abap_true.

  ELSE.


    SELECT * FROM zcot0320
      INTO TABLE @DATA(lt_zcot0320)
      WHERE bname = @sy-uname
        AND ( prctr2 NE @space OR bukrs2 NE @space ).



    IF sy-subrc EQ 0.


      LOOP AT lt_zcot0320 ASSIGNING FIELD-SYMBOL(<fs>).


        IF <fs>-prctr2 IS NOT INITIAL.

          lr_prctr-sign = 'I'.   lr_prctr-option = 'EQ'.
          lr_prctr-low = <fs>-prctr2. COLLECT lr_prctr.
        ENDIF.


        IF  <fs>-bukrs2 IS NOT INITIAL.
          lr_bukrs-sign = 'I'.           lr_bukrs-option = 'EQ'.
          lr_bukrs-low = <fs>-bukrs2.   COLLECT lr_bukrs.

        ENDIF.

      ENDLOOP.

    ELSE.

*조회 권한이 없습니다.(CO 모듈 관리자에게 문의하세요..
      gv_error = TEXT-e77.  " ZCOR0320  미등록 ID.


    ENDIF.


  ENDIF.





  DELETE lr_prctr WHERE low IS INITIAL.
  DELETE lr_bukrs WHERE low IS INITIAL.


  IF lv_super IS NOT INITIAL OR lv_super IS NOT INITIAL.  " 기존 로직...


    IF so_kostl[] IS NOT INITIAL.

      LOOP AT so_kostl.

        MOVE: so_kostl-sign   TO r_objnr-sign,
              so_kostl-option TO r_objnr-option.

        r_objnr-low  = 'KS' && pa_kokrs && so_kostl-low.

        IF so_kostl-high IS NOT INITIAL.
          r_objnr-high = 'KS' && pa_kokrs && so_kostl-high.
        ENDIF.

        COLLECT r_objnr.
        CLEAR   r_objnr.

      ENDLOOP.

    ENDIF.

    IF pa_ksgru IS NOT INITIAL.

      PERFORM read_hierarchy_tables TABLES gt_values
                                    USING '0101'
                                          pa_ksgru.  "코스트센터 그룹

      LOOP AT gt_values.

        MOVE: 'I'   TO r_objnr-sign,
               'BT' TO r_objnr-option.

        r_objnr-low  = 'KS' && pa_kokrs && gt_values-vfrom.
        r_objnr-high = 'KS' && pa_kokrs &&  gt_values-vto.

        COLLECT r_objnr.
        CLEAR   r_objnr.

      ENDLOOP.

    ENDIF.

  ELSE.  " 권한 있는 회사와  손익센터 기준으로 CCTR..

    DATA : lt_csks TYPE TABLE OF csks.

    CLEAR : lt_csks, lt_csks[].

****    CCTR 그룹에 대한  권한 체크 추가


    IF pa_ksgru IS NOT INITIAL.

      PERFORM read_hierarchy_tables TABLES gt_values
                                    USING '0101'
                                          pa_ksgru.  "코스트센터 그룹
      LOOP AT gt_values.


        IF lr_bukrs[] IS NOT INITIAL.

          SELECT kostl  AS kostl,
                 prctr  AS prctr
            FROM csks
            WHERE bukrs IN @lr_bukrs  " 권한 있는 회사
              AND kostl BETWEEN @gt_values-vfrom AND @gt_values-vto "<<<<<<<<<
              AND kostl IN @so_kostl
                    INTO CORRESPONDING FIELDS OF TABLE @lt_csks.

        ELSE.

          IF lr_prctr[] IS NOT INITIAL.

            SELECT kostl  AS kostl,
                   prctr  AS prctr
                   FROM csks
                   WHERE  prctr IN @lr_prctr   " 권한 있는 손익센터
                     AND kostl BETWEEN @gt_values-vfrom AND @gt_values-vto "<<<<<<<<<
                     AND kostl IN @so_kostl
                    INTO CORRESPONDING FIELDS OF TABLE @lt_csks.
          ENDIF.

        ENDIF.




        LOOP AT lt_csks ASSIGNING FIELD-SYMBOL(<fs3>).


          r_objnr-sign = 'I'.
          r_objnr-option = 'EQ'.

          r_objnr-low  = 'KS' && pa_kokrs && <fs3>-kostl.

          COLLECT r_objnr.
          CLEAR   r_objnr.

        ENDLOOP.

      ENDLOOP.  " 그룹 CCTR  ENDLOOP...




    ELSE.  " CCTR 그룹  미입력 인경우..

      IF lr_bukrs[] IS NOT INITIAL. " 우선 순위 1.


        SELECT kostl  AS kostl,
               prctr  AS prctr
          FROM csks
          WHERE   bukrs IN @lr_bukrs  " 권한 있는 회사
            AND kostl IN @so_kostl
          INTO  CORRESPONDING FIELDS OF TABLE @lt_csks.

      ELSE.

        IF lr_prctr[] IS NOT INITIAL.  " 우선순위 2..

          SELECT kostl  AS kostl,
                 prctr  AS prctr
            FROM csks
            WHERE  prctr IN @lr_prctr   " 권한 있는 손익센터
              AND kostl IN @so_kostl
            INTO  CORRESPONDING FIELDS OF TABLE @lt_csks.

        ENDIF.

      ENDIF.


      LOOP AT lt_csks ASSIGNING FIELD-SYMBOL(<fs2>).


        r_objnr-sign = 'I'.
        r_objnr-option = 'EQ'.

        r_objnr-low  = 'KS' && pa_kokrs && <fs2>-kostl.

        COLLECT r_objnr.
        CLEAR   r_objnr.

      ENDLOOP.


    ENDIF.


  ENDIF.  " SUPER  END IF. 문...


**-- <<<<<<<<<<<<<<<<<<   END BY BSGSM_FCM   20210324...

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
*& Form SELECTED_MAIN_DATA
*&---------------------------------------------------------------------*
FORM selected_main_data .
  CLEAR: gt_display, gt_display[],
         gt_display_log, gt_display_log[].

  DATA: lv_index     TYPE i,
        lv_month     TYPE n LENGTH 2,
        lv_fieldname TYPE fieldname.

  DATA: ls_color TYPE lvc_s_scol.

  FIELD-SYMBOLS: <fs_wkf>   TYPE any,
                 <fs_wkg>   TYPE any,
                 <fs_total> TYPE any.

  SELECT b~kostl,
         d~ktext AS kotxt,
         a~kstar,
         c~ktext AS kstxt,
         a~twaer,
         SUM( a~wkg001 ) AS wkg01,
         SUM( a~wkg002 ) AS wkg02,
         SUM( a~wkg003 ) AS wkg03,
         SUM( a~wkg004 ) AS wkg04,
         SUM( a~wkg005 ) AS wkg05,
         SUM( a~wkg006 ) AS wkg06,
         SUM( a~wkg007 ) AS wkg07,
         SUM( a~wkg008 ) AS wkg08,
         SUM( a~wkg009 ) AS wkg09,
         SUM( a~wkg010 ) AS wkg10,
         SUM( a~wkg011 ) AS wkg11,
         SUM( a~wkg012 ) AS wkg12
    INTO TABLE @DATA(lt_sum)
    FROM cosp AS a INNER JOIN csks AS b
                           ON a~objnr = b~objnr
                          AND b~datbi >= @sy-datum
                          AND b~datab <= @sy-datum
                    LEFT JOIN csku AS c
                           ON c~spras = @sy-langu
                          AND c~ktopl = @gc_ktopl
                          AND a~kstar = c~kstar
                    LEFT JOIN cskt AS d
                           ON b~kokrs = d~kokrs
                          AND b~datbi = d~datbi
                          AND b~kostl = d~kostl
                          AND d~spras = @sy-langu
   WHERE a~lednr    = '00'
     AND a~gjahr    = @pa_gjahr
     AND a~versn    = @pa_versn
     AND a~wrttp    = '01'
     AND a~objnr   IN @r_objnr
     AND a~objnr LIKE 'KS%'
   GROUP BY b~kostl, d~ktext, a~kstar, c~ktext, a~twaer.



** BSGSm_FCM  20210324.

  SELECT a~kostl, b~ktext, a~prctr, c~ltext, a~bukrs
    INTO TABLE @DATA(lt_kostl)
    FROM csks AS a
    LEFT JOIN cskt AS b
      ON a~kokrs = b~kokrs
     AND a~kostl = b~kostl
     AND a~datbi = b~datbi
     AND b~spras = @sy-langu
    LEFT JOIN cepct AS c
      ON a~prctr = c~prctr
      AND a~kokrs = c~kokrs
      AND c~spras = @sy-langu
   WHERE a~kokrs = @pa_kokrs
     AND a~bukrs IN @so_bukrs
    ORDER BY a~kostl.

** END BY BSGSM-FCM 20210324


  LOOP AT lt_sum INTO DATA(ls_sum).


    MOVE: ls_sum-kostl TO gs_display-kostl,
          ls_sum-kotxt TO gs_display-kotxt,
          ls_sum-kstar TO gs_display-kstar,
          ls_sum-kstxt TO gs_display-kstxt,
          ls_sum-twaer TO gs_display-waers.

    IF ls_sum-kstar CP '04*'   OR ls_sum-kstar CP '0701*' OR
       ls_sum-kstar CP '0703*' OR ls_sum-kstar CP '0705*'.
      MOVE TEXT-006 TO gs_display-ktext.
    ENDIF.

    lv_month = '01'.

    DO 12 TIMES.

      lv_fieldname = 'LS_SUM-WKG' && lv_month.

      ASSIGN (lv_fieldname) TO <fs_wkg>.

      lv_fieldname = 'GS_DISPLAY-WKF' && lv_month.
      ASSIGN (lv_fieldname) TO <fs_wkf>.

      ASSIGN COMPONENT 'TOTAL' OF STRUCTURE gs_display TO <fs_total>.

      <fs_wkf> = <fs_wkg>.

      <fs_total> = <fs_total> + <fs_wkf>.

      ADD 1 TO lv_month.

    ENDDO.

*    IF GS_DISPLAY-TOTAL < 0 AND
*       ( LS_SUM-KSTAR CP '04*'   OR LS_SUM-KSTAR CP '0701*' OR
*         LS_SUM-KSTAR CP '0703*' OR LS_SUM-KSTAR CP '0705*' ).
*
*      LS_COLOR-FNAME = 'TOTAL'.
*      LS_COLOR-COLOR-COL = 6.  "color code 1-7, if outside rage defaults to 7
*      LS_COLOR-COLOR-INT = '0'.  "1 = Intensified on, 0 = Intensified off
*      LS_COLOR-COLOR-INV = '1'.  "1 = text colour, 0 = background colour
*      APPEND LS_COLOR TO GS_DISPLAY-COLOR.
*
*      LV_MONTH = '01'.
*
*      DO 12 TIMES.
*
*        LS_COLOR-FNAME = 'WKF' && LV_MONTH.
*        APPEND LS_COLOR TO GS_DISPLAY-COLOR.
*        ADD 1 TO LV_MONTH.
*
*      ENDDO.
*
*    ENDIF.

    READ TABLE lt_kostl ASSIGNING FIELD-SYMBOL(<fs>) WITH KEY kostl = gs_display-kostl BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_display-prctr = <fs>-prctr.
      gs_display-ltext = <fs>-ltext.

      APPEND gs_display TO gt_display.

    ENDIF.


*    APPEND gs_display TO gt_display.
    CLEAR: gs_display, ls_sum.

  ENDLOOP.

  SORT gt_display BY kostl kstar.

  gt_display_log[] = gt_display[].


  DATA(lv_lines) = lines( gt_display[] ).

  MESSAGE s039(zco01) WITH lv_lines.


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
*  GS_LAYOUT-CWIDTH_OPT = ABAP_TRUE.
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
      i_strucname            = 'ZCOS0290' "ABAP DIC. 정의된 STRUCTURE
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

  DATA: lv_text(50),
        lv_pos      TYPE i VALUE 7.

  LOOP AT gt_fieldcat INTO gs_fieldcat.

    CLEAR: lv_text.

    CASE gs_fieldcat-fieldname.

      WHEN 'KOSTL'.
        lv_text               = TEXT-c01.
        gs_fieldcat-col_pos   = 1.
        gs_fieldcat-emphasize = 'C112'.
        gs_fieldcat-outputlen = 10.

      WHEN 'KOTXT'.
        lv_text               = TEXT-c02.
        gs_fieldcat-col_pos   = 2.
        gs_fieldcat-emphasize = 'C112'.
        gs_fieldcat-outputlen = 20.

      WHEN 'KSTAR'.
        lv_text               = TEXT-c03.
        gs_fieldcat-col_pos   = 3.
        gs_fieldcat-emphasize = 'C112'.
        gs_fieldcat-outputlen = 10.

      WHEN 'KSTXT'.
        lv_text               = TEXT-c04.
        gs_fieldcat-col_pos   = 4.
        gs_fieldcat-emphasize = 'C112'.
        gs_fieldcat-outputlen = 20.

      WHEN 'KTEXT'.
        lv_text                = TEXT-c07.
        gs_fieldcat-col_pos    = 5.
        gs_fieldcat-outputlen  = 10.

      WHEN 'TOTAL'.
        lv_text                = TEXT-c05.
        gs_fieldcat-col_pos    = 6.
        gs_fieldcat-outputlen  = 10.

      WHEN 'WAERS'.
        gs_fieldcat-no_out = abap_true.

      WHEN 'MESSAGE'.
        lv_text               = TEXT-c06.
        gs_fieldcat-outputlen = 50.

      WHEN 'PRCTR' OR 'LTEXT'.  " ADD BSGSM_FCM  권한 검증용 추가
        gs_fieldcat-no_out  = abap_true.

    ENDCASE.

    IF gs_fieldcat-fieldname CP 'WKF*'.

      CONCATENATE gs_fieldcat-fieldname+3(2) '월' INTO lv_text.
      gs_fieldcat-col_pos    = lv_pos.
      gs_fieldcat-outputlen  = 10.

      IF pa_rad1 = 'X'.
        gs_fieldcat-edit     = abap_true.
      ELSE.
        gs_fieldcat-edit     = abap_false.
      ENDIF.

      ADD 1 TO lv_pos.

    ENDIF.

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

  CONCATENATE TEXT-001 ':' pa_kokrs
        INTO lv_text SEPARATED BY space.

  CALL METHOD gr_top_document->add_text
    EXPORTING
      text         = lv_text
      sap_color    = cl_dd_document=>list_heading_int
      sap_emphasis = cl_dd_area=>key
      style_class  = space.

  CALL METHOD gr_top_document->new_line.

  CONCATENATE TEXT-004 ':' pa_gjahr
        INTO lv_text SEPARATED BY space.

  CALL METHOD gr_top_document->add_text
    EXPORTING
      text         = lv_text
      sap_color    = cl_dd_document=>list_heading_int
      sap_emphasis = cl_dd_area=>key
      style_class  = space.

  CALL METHOD gr_top_document->new_line.

  CONCATENATE TEXT-005 ':' pa_versn
        INTO lv_text SEPARATED BY space.

  CALL METHOD gr_top_document->add_text
    EXPORTING
      text         = lv_text
      sap_color    = cl_dd_document=>list_heading_int
      sap_emphasis = cl_dd_area=>key
      style_class  = space.

  CALL METHOD gr_top_document->new_line
    EXPORTING
      repeat = 1.

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
  SET HANDLER:
*    GR_EVENT_RECEIVER->HANDLE_TOOLBAR       FOR ALL INSTANCES,
    gr_event_receiver->handle_data_changed  FOR ALL INSTANCES.
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
*& Form CHECK_CHANGE
*&---------------------------------------------------------------------*
FORM check_change  CHANGING p_gv_valid.

  IF gt_display_log[] = gt_display[].
    CLEAR p_gv_valid.
  ELSE.
    p_gv_valid = abap_true.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
FORM popup_to_confirm USING pv_title
                            pv_quest.

  "-- call popup
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar       = pv_title                "TEXT-PT1
*     DIAGNOSE_OBJECT             = ' '
      text_question  = pv_quest                "TEXT-QT1
*     TEXT_BUTTON_1  = 'Ja'(001)
*     ICON_BUTTON_1  = ' '
*     TEXT_BUTTON_2  = 'Nein'(002)
*     ICON_BUTTON_2  = ' '
*     DEFAULT_BUTTON = '1'
*     DISPLAY_CANCEL_BUTTON       = 'X'
*     USERDEFINED_F1_HELP         = ' '
*     START_COLUMN   = 25
*     START_ROW      = 6
*     POPUP_TYPE     =
*     IV_QUICKINFO_BUTTON_1       = ' '
*     IV_QUICKINFO_BUTTON_2       = ' '
    IMPORTING
      answer         = gv_answer
*   TABLES
*     PARAMETER      =
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*& Form BAPI_COSTACTPLN_POSTPRIMCOST
*&---------------------------------------------------------------------*
FORM bapi_costactpln_postprimcost .
  DATA: lr_error   TYPE REF TO cx_sy_sql_error,
        lv_message TYPE string.

  DATA lv_fname TYPE stfna.
  DATA lv_month TYPE n LENGTH 2.

  FIELD-SYMBOLS: <fs1> TYPE any,
                 <fs2> TYPE any.

  DATA : ls_headerinfo  LIKE  bapiplnhdr.

  DATA : lt_indexstructure LIKE bapiacpstru OCCURS 0 WITH HEADER LINE,
         lt_coobject       LIKE bapipcpobj OCCURS 0 WITH HEADER LINE,
         lt_pervalue       LIKE bapipcpval OCCURS 0 WITH HEADER LINE.

  LOOP AT gt_display INTO gs_display.

    CLEAR: gt_return, gt_return[].

    CLEAR: ls_headerinfo, lt_indexstructure,
                          lt_indexstructure[],
                          lt_coobject,
                          lt_coobject[],
                          lt_pervalue,
                          lt_pervalue[].
    "Planning ( KP06 )
*-- Header Data
    ls_headerinfo-co_area     = pa_kokrs.     "관리 회계영역
    ls_headerinfo-fisc_year   = pa_gjahr.     "회계연도
    ls_headerinfo-period_from = 1.            "기간 시작
    ls_headerinfo-period_to   = 12.           "기간 종료
    ls_headerinfo-version     = pa_versn.     "버전

*  CONCATENATE PA_GJAHR '년 사업계획' INTO LS_HEADERINFO-DOC_HDR_TX.

*-- 전표 헤더 텍스트
    ls_headerinfo-plan_currtype = 'C'. "통화

*-- CO-계획: 액티비티투입 & 주요지표 계획 BAPIs
    lt_indexstructure-object_index = 1.
    lt_indexstructure-value_index  = 1.
    APPEND lt_indexstructure.

*-- CO 계획: 1차 원가 BAPI에 대한 오브젝트
    lt_coobject-object_index = 1.
    lt_coobject-costcenter   = gs_display-kostl.
    APPEND lt_coobject.

*-- CO 계획: 1차 원가 BAPI에 대한 값
    lt_pervalue-value_index  = 1.
    lt_pervalue-cost_elem    = gs_display-kstar.   "원가요소
    lt_pervalue-trans_curr   = gs_display-waers.

    lv_month = '01'.

    DO 12 TIMES.

      lv_fname = 'GS_DISPLAY-WKF' && lv_month.
      ASSIGN (lv_fname) TO <fs1>.

      lv_fname = 'LT_PERVALUE-FIX_VAL_PER' && lv_month.
      ASSIGN (lv_fname) TO <fs2>.

      IF gs_display-waers = 'KRW'.
        <fs2> = <fs1> * 100.
      ELSE.
        <fs2> = <fs1>.
      ENDIF.

*      IF GS_DISPLAY-KSTAR(2) = '04'   OR GS_DISPLAY-KSTAR(4) = '0701' OR
*         GS_DISPLAY-KSTAR(4) = '0703' OR GS_DISPLAY-KSTAR(4) = '0705'.
*        <FS2> = <FS2> * -1.
*      ENDIF.

      ADD 1 TO lv_month.

    ENDDO.

    APPEND lt_pervalue.

    CALL FUNCTION 'BAPI_COSTACTPLN_POSTPRIMCOST'
      EXPORTING
        headerinfo     = ls_headerinfo
      TABLES
        indexstructure = lt_indexstructure
        coobject       = lt_coobject
        pervalue       = lt_pervalue
        return         = gt_return.

    READ TABLE gt_return WITH KEY type = 'E'.
    IF sy-subrc EQ 0 .

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.


      PERFORM build_message USING    gt_return
                            CHANGING lv_message.

      gs_display-message = lv_message.

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      gs_display-message = TEXT-s01.
    ENDIF.

    MODIFY gt_display FROM gs_display.
    CLEAR gs_display.

  ENDLOOP.

  CLEAR: gt_display_log, gt_display_log[].

  gt_display_log[] = gt_display[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_MESSAGE
*&---------------------------------------------------------------------*
FORM build_message  USING    ps_message STRUCTURE bapiret2
                     CHANGING pv_text.

  CLEAR pv_text.

  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      msgid               = ps_message-id
      msgnr               = ps_message-number
      msgv1               = ps_message-message_v1
      msgv2               = ps_message-message_v2
      msgv3               = ps_message-message_v3
      msgv4               = ps_message-message_v4
    IMPORTING
      message_text_output = pv_text.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXCEL_DOWNLOAD
*&---------------------------------------------------------------------*
FORM excel_download .
  DATA: li_data TYPE REF TO data.

  DATA: lv_xml          TYPE xstring,
        lv_gui_type     TYPE i VALUE 1,
        lv_display_mode TYPE i,
        lv_version      TYPE string,
        lv_flavour      TYPE string.

  DATA t_sort TYPE lvc_t_sort.

  DATA t_fieldcat TYPE lvc_t_fcat.

  DATA: ls_xml_choice TYPE if_salv_bs_xml=>s_type_xml_choice.

  DATA: lt_xml_choice TYPE if_salv_bs_xml=>t_type_xml_choice.

  DATA: li_result_data TYPE REF TO cl_salv_ex_result_data_table,
        li_controller  TYPE REF TO cl_salv_export_c8r.


  DATA: lv_default_extension TYPE string,
        lv_initial_directory TYPE string,
        lv_default_file_name TYPE string,
        lv_mask              TYPE char255,
        lv_mask1             TYPE string,
        lv_application       TYPE string.


  FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE.

  CREATE DATA li_data LIKE gt_display.

  ASSIGN li_data->* TO <lt_data>.

  <lt_data> = gt_display[].

  CLEAR: t_fieldcat[].

  t_fieldcat[] =  gt_fieldcat[].

  LOOP AT t_fieldcat INTO gs_fieldcat.

    CASE gs_fieldcat-fieldname.

      WHEN 'KOTXT' OR 'TOTAL' OR 'MESSAGE'.
        gs_fieldcat-no_out = abap_true.

      WHEN OTHERS.
        gs_fieldcat-no_out = abap_false.

    ENDCASE.

    MODIFY t_fieldcat FROM gs_fieldcat.

  ENDLOOP.

  CALL METHOD cl_salv_export_xml_dialog=>execute
    EXPORTING
      gui_type     = lv_gui_type
      display_mode = lv_display_mode
    RECEIVING
      value        = lt_xml_choice.

  READ TABLE lt_xml_choice INTO ls_xml_choice INDEX 1.

  IF sy-subrc NE 0.
    MESSAGE 'FORMAT Not Choice!'
    TYPE 'S' RAISING format_not_choice.
  ENDIF.

  CREATE OBJECT li_controller
    EXPORTING
      t_choice = lt_xml_choice.

  CASE cl_salv_bs_a_xml_base=>get_version( ).
    WHEN if_salv_bs_xml=>version_25.
      lv_version = if_salv_bs_xml=>version_25.
    WHEN if_salv_bs_xml=>version_26.
      lv_version = if_salv_bs_xml=>version_26.
  ENDCASE.

  lv_flavour = if_salv_bs_c_tt=>c_tt_xml_flavour_export.

  CALL METHOD cl_salv_ex_util=>factory_result_data_table
    EXPORTING
      r_data              = li_data
*     S_LAYOUT            = IS_LAYOUT
      t_fieldcatalog      = t_fieldcat[]
      t_sort              = t_sort[]
    RECEIVING
      r_result_data_table = li_result_data.

  CALL METHOD cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform
    EXPORTING
      xml_type      = ls_xml_choice-xml_type
      xml_version   = lv_version
      r_result_data = li_result_data
      xml_flavour   = lv_flavour
      gui_type      = if_salv_bs_xml=>c_gui_type_gui
    IMPORTING
      xml           = lv_xml.


  CALL METHOD cl_alv_bds=>create_mask_for_filefilter
    EXPORTING
      i_frontend          = ls_xml_choice-frontend
    IMPORTING
      e_default_extension = lv_default_extension
    CHANGING
      c_mask              = lv_mask.

  lv_mask1 = lv_mask.

  IF ls_xml_choice-frontend EQ cl_alv_bds=>mc_mhtml_frontend.
    lv_default_extension = cl_alv_bds=>mc_excel_extension.
  ENDIF.

  CONCATENATE 'export.' lv_default_extension
        INTO lv_default_file_name.

  DATA: lv_length      TYPE i,
        lv_filename    TYPE string,
        lv_appl_para   TYPE string,
        lv_xml_stream  TYPE etxml_xline_tabtype,
        lv_title       TYPE string,
        lv_loc_fn      TYPE string,
        lv_loc_dir     TYPE string,
        lv_user_action TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title         = lv_title
      default_extension    = lv_default_extension
      default_file_name    = lv_default_file_name
      file_filter          = lv_mask1
      initial_directory    = lv_initial_directory
    CHANGING
      filename             = lv_loc_fn
      path                 = lv_loc_dir
      fullpath             = lv_loc_dir
      user_action          = lv_user_action
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

  IF sy-subrc <> 0.
    MESSAGE e162(alvht).
    EXIT.
  ENDIF.

  IF lv_user_action = cl_gui_frontend_services=>action_cancel .
    MESSAGE s161(alvht).
    EXIT.
  ENDIF.

  CONCATENATE lv_loc_dir lv_loc_fn INTO lv_filename.

  IF NOT lv_filename IS INITIAL.
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_xml
      IMPORTING
        output_length = lv_length
      TABLES
        binary_tab    = lv_xml_stream.

    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        bin_filesize = lv_length
        filetype     = 'BIN'
        filename     = lv_filename
      CHANGING
        data_tab     = lv_xml_stream
      EXCEPTIONS
        OTHERS       = 1.
  ENDIF.

ENDFORM.
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
  DATA: ls_mod_cells TYPE lvc_s_modi.

  DEFINE _modify_cell.

    CALL METHOD pr_data_changed->modify_cell
      EXPORTING
        i_fieldname = &1
        i_row_id    = &2
        i_value     = &3.

  END-OF-DEFINITION.
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

  DATA: lv_index     TYPE i,
        lv_month     TYPE n LENGTH 2,
        lv_fieldname TYPE fieldname,
        lv_field     TYPE fieldname.

  FIELD-SYMBOLS: <fs_cnt>   TYPE any,
                 <fs_cnt_t> TYPE any..

  CASE pr_sender.
    WHEN gr_grid1.
*      LOOP AT PR_DATA_CHANGED->MT_MOD_CELLS INTO LS_MOD_CELLS.
      LOOP AT pr_data_changed->mt_good_cells INTO ls_mod_cells.

        IF ls_mod_cells-fieldname(3) CP 'WKF'.

          READ TABLE gt_display INDEX ls_mod_cells-row_id ASSIGNING FIELD-SYMBOL(<ls_display>).

          lv_month = '01'.

          DO 12 TIMES.

            lv_fieldname = '<LS_DISPLAY>-WKF' && lv_month.
            ASSIGN (lv_fieldname) TO <fs_cnt>.

            lv_field = 'WKF' && lv_month.

            IF lv_field = ls_mod_cells-fieldname.

              <fs_cnt> = ls_mod_cells-value.

            ENDIF.

            IF lv_month = '01'.
              CLEAR <ls_display>-total.
            ENDIF.

            ASSIGN COMPONENT 'TOTAL' OF STRUCTURE <ls_display> TO <fs_cnt_t>.
            <fs_cnt_t> = <fs_cnt_t> + <fs_cnt>.

            ADD 1 TO lv_month.

          ENDDO.

        ENDIF.
      ENDLOOP.

      "-- ALV Refresh
      PERFORM refresh_grid_0100.

    WHEN OTHERS.

  ENDCASE.

ENDFORM.                    " EVENT_DATA_CHANGED
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
