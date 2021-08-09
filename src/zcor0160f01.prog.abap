*&---------------------------------------------------------------------*
*& Include          ZCOR0160F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITIAL_SET
*&---------------------------------------------------------------------*
FORM initial_set .

  SELECT SINGLE bezei, waers INTO (@pa_ktxt, @gv_waers)
    FROM tka01
   WHERE kokrs = '1000'.
*
*  SELECT SINGLE VTEXT INTO @PA_VTXT
*    FROM TKVS AS A
*    LEFT JOIN TKVST AS B
*      ON A~VERSI = B~VERSI
*     AND B~SPRAS = @SY-LANGU
*   WHERE A~VERSI = 'P0'.

  SELECT SINGLE b~txt INTO @pa_vtxt
    FROM tka09 AS a INNER JOIN tkt09 AS b
                       ON b~kokrs EQ a~kokrs
                      AND b~versn EQ a~versn
                      AND b~langu EQ @sy-langu
   WHERE a~kokrs EQ @pa_kokrs
     AND a~versn EQ @pa_versn.


  SELECT SINGLE versn_t INTO @pa_pvert
    FROM zcot0150
     WHERE versn = @pa_pver.

  "__ 20191223 BSGSM_FCM ADD default cac
  SET PARAMETER ID 'CAC' FIELD pa_kokrs.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_SELECTION_SCREEN
*&---------------------------------------------------------------------*
FORM check_selection_screen .

  DATA ls_setleaf TYPE setleaf.

  IF pa_sperl > pa_eperl AND
      pa_eperl IS NOT INITIAL.
    SET CURSOR FIELD 'PA_SPERL'.
    MESSAGE e018  WITH TEXT-e03.
  ENDIF.

*  SELECT SINGLE VTEXT INTO @PA_VTXT
*    FROM TKVS AS A
*    LEFT JOIN TKVST AS B
*      ON A~VERSI = B~VERSI
*     AND B~SPRAS = @SY-LANGU
*   WHERE A~VERSI = @PA_VERSN.

  SELECT SINGLE b~txt INTO @pa_vtxt
    FROM tka09 AS a INNER JOIN tkt09 AS b
                       ON b~kokrs EQ a~kokrs
                      AND b~versn EQ a~versn
                      AND b~langu EQ @sy-langu
   WHERE a~kokrs EQ @pa_kokrs
     AND a~versn EQ @pa_versn.

  IF sy-subrc <> 0.
    SET CURSOR FIELD 'PA_VERSN'.
    MESSAGE e027  WITH TEXT-002.
  ENDIF.

  SELECT SINGLE versn_t INTO @pa_pvert
    FROM zcot0150
   WHERE kokrs  = @pa_kokrs
     AND versn  = @pa_pver.

  SELECT SINGLE versn INTO @DATA(lv_versn)
    FROM zcot0150
   WHERE kokrs  = @pa_kokrs
     AND versn  = @pa_pver.

  IF sy-subrc <> 0.
    SET CURSOR FIELD 'PA_PVER'.
    MESSAGE e027  WITH TEXT-e02.
  ENDIF.

  IF so_posid[] IS NOT INITIAL AND
     pa_pdgr IS NOT INITIAL.
    SET CURSOR FIELD 'SO_POSID-LOW'.
    MESSAGE e026  WITH TEXT-e04.
  ENDIF.

  IF so_posid[] IS NOT INITIAL.

    SELECT SINGLE * FROM prps
      INTO @DATA(ls_prps)
     WHERE posid IN @so_posid.

    IF sy-subrc <> 0.
      SET CURSOR FIELD 'SO_POSID-LOW'.
      MESSAGE e027  WITH TEXT-e07.
    ENDIF.

  ENDIF.

  IF pa_pdgr IS NOT INITIAL.

    SELECT SINGLE * FROM setleaf
      INTO @ls_setleaf
     WHERE setclass = '0110'
       AND setname  = @pa_pdgr.

    IF sy-subrc <> 0.
      SET CURSOR FIELD 'PA_PDGR'.
      MESSAGE e027  WITH TEXT-e08.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_DATA_RTN
*&---------------------------------------------------------------------*
FORM selected_data_rtn .

  DATA lt_data   TYPE TABLE OF ty_data   WITH HEADER LINE.
  DATA lt_acdoca TYPE TABLE OF ty_acdoca WITH HEADER LINE.

  SELECT * FROM zcot0150
    INTO TABLE @gt_zcot0150
   WHERE kokrs = @pa_kokrs
     AND versn = @pa_pver.

  IF sy-subrc <> 0.
    MESSAGE s004 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  SELECT a~kstar, b~ktext
    INTO TABLE @gt_cska
    FROM cska AS a
    LEFT JOIN csku AS b
      ON a~ktopl = b~ktopl
     AND a~kstar = b~kstar
     AND b~spras = @sy-langu
   WHERE a~ktopl = @gc_ktopl.

  SELECT * FROM setnode
    INTO TABLE @gt_setnode
   WHERE setclass = '0102'
     AND subclass = @pa_kokrs.

  CLEAR: r_kstar, r_kstar[].

  LOOP AT gt_zcot0150 WHERE gubun = '2'.
    PERFORM set_kstar_ranges TABLES r_kstar
                             USING gt_zcot0150-fkagru
                                   gt_zcot0150-tkagru.
  ENDLOOP.

  SELECT a~versn, a~wrttp, a~gjahr, a~objnr, a~kstar, b~ktext,
         SUM( a~wkg001 ) AS wkg001, SUM( a~wkg002 ) AS wkg002,
         SUM( a~wkg003 ) AS wkg003, SUM( a~wkg004 ) AS wkg004,
         SUM( a~wkg005 ) AS wkg005, SUM( a~wkg006 ) AS wkg006,
         SUM( a~wkg007 ) AS wkg007, SUM( a~wkg008 ) AS wkg008,
         SUM( a~wkg009 ) AS wkg009, SUM( a~wkg010 ) AS wkg010,
         SUM( a~wkg011 ) AS wkg011, SUM( a~wkg012 ) AS wkg012
    FROM cosp AS a
    LEFT JOIN csku AS b
      ON a~kstar = b~kstar
     AND b~ktopl = @gv_ktopl
     AND b~spras = @sy-langu
   WHERE a~lednr = '00'
     AND ( ( a~versn = '000'      AND a~wrttp = '04' ) OR
           ( a~versn = @pa_versn  AND a~wrttp = '01' ) OR
           ( a~versn = 'PZ'       AND a~wrttp = '01' )  OR
           ( a~versn = '000'      AND a~wrttp = '01'  AND
               a~kstar = '0984000010'  ) )
     AND a~gjahr = @pa_gjahr
     AND a~kstar IN @r_kstar
     AND a~objnr IN @r_objnr
   GROUP BY a~versn, a~wrttp, a~gjahr, a~objnr, a~kstar, b~ktext
   UNION ALL
  SELECT a~versn, a~wrttp, a~gjahr, a~objnr, a~kstar, b~ktext,
         SUM( a~wkg001 ) AS wkg001, SUM( a~wkg002 ) AS wkg002,
         SUM( a~wkg003 ) AS wkg003, SUM( a~wkg004 ) AS wkg004,
         SUM( a~wkg005 ) AS wkg005, SUM( a~wkg006 ) AS wkg006,
         SUM( a~wkg007 ) AS wkg007, SUM( a~wkg008 ) AS wkg008,
         SUM( a~wkg009 ) AS wkg009, SUM( a~wkg010 ) AS wkg010,
         SUM( a~wkg011 ) AS wkg011, SUM( a~wkg012 ) AS wkg012
    FROM coss AS a
    LEFT JOIN csku AS b
      ON a~kstar = b~kstar
     AND b~ktopl = @gv_ktopl
     AND b~spras = @sy-langu
   WHERE a~lednr = '00'
     AND ( ( a~versn = '000'      AND a~wrttp = '04' ) OR
           ( a~versn = @pa_versn  AND a~wrttp = '01' ) OR
           ( a~versn = 'PZ'       AND a~wrttp = '01' )  OR
           ( a~versn = '000'      AND a~wrttp = '01'  AND
             a~kstar = '0984000010'  ) )
     AND a~gjahr = @pa_gjahr
     AND a~kstar IN @r_kstar
     AND a~objnr IN @r_objnr
   GROUP BY a~versn, a~wrttp, a~gjahr, a~objnr, a~kstar, b~ktext
    UNION ALL
  SELECT a~rvers  AS versn, a~wrttp, a~ryear AS gjahr,
         a~robjnr AS objnr, a~rkstar AS kstar, b~ktext,
        SUM( a~hsl01 ) AS wkg001, SUM( a~hsl02 ) AS wkg002,
        SUM( a~hsl03 ) AS wkg003, SUM( a~hsl04 ) AS wkg004,
        SUM( a~hsl05 ) AS wkg005, SUM( a~hsl06 ) AS wkg006,
        SUM( a~hsl07 ) AS wkg007, SUM( a~hsl08 ) AS wkg008,
        SUM( a~hsl09 ) AS wkg009, SUM( a~hsl10 ) AS wkg010,
        SUM( a~hsl11 ) AS wkg011, SUM( a~hsl12 ) AS wkg012
    FROM zcot1180 AS a
    LEFT JOIN csku AS b
      ON a~rkstar = b~kstar
     AND b~ktopl  = @gv_ktopl
     AND b~spras  = @sy-langu
   WHERE a~rvers  = '000'
     AND a~wrttp  = '04'
     AND a~rkokrs = @pa_kokrs
     AND a~ryear  = @pa_gjahr
     AND a~rkstar IN @r_kstar
     AND a~robjnr IN @r_objnr
   GROUP BY a~rvers, a~wrttp, a~ryear, a~robjnr, a~rkstar, b~ktext
   UNION ALL
  SELECT a~rvers , a~wrttp, a~ryear AS gjahr,
         a~objnr,  a~saknr AS kstar, b~txt20 AS ktext,
        SUM( a~hsl01 ) AS wkg001, SUM( a~hsl02 ) AS wkg002,
        SUM( a~hsl03 ) AS wkg003, SUM( a~hsl04 ) AS wkg004,
        SUM( a~hsl05 ) AS wkg005, SUM( a~hsl06 ) AS wkg006,
        SUM( a~hsl07 ) AS wkg007, SUM( a~hsl08 ) AS wkg008,
        SUM( a~hsl09 ) AS wkg009, SUM( a~hsl10 ) AS wkg010,
        SUM( a~hsl11 ) AS wkg011, SUM( a~hsl12 ) AS wkg012
    FROM zfit0621 AS a        "관계사간 계획
    LEFT JOIN skat AS b
      ON a~saknr  = b~saknr
     AND b~ktopl  = @gv_ktopl
     AND b~spras  = @sy-langu
   WHERE a~rvers  = @pa_versn
     AND a~wrttp  = '01'
     AND a~kokrs  = @pa_kokrs
     AND a~ryear  = @pa_gjahr
     AND a~saknr  IN @r_kstar
     AND a~objnr  IN @r_objnr
   GROUP BY a~rvers, a~wrttp, a~ryear, a~objnr, a~saknr, b~txt20
    INTO TABLE @gt_data.

*-- 관계사간 거래 실적
  SELECT a~spmon, a~saknr, a~txt20, b~objnr,
         SUM( a~dmbtr ) AS dmbtr
    INTO TABLE @DATA(lt_zfit0620)
    FROM zfit0620 AS a
    LEFT JOIN prps AS b
      ON a~posid = b~posid
   WHERE a~posid IN @r_posid2
     AND a~spmon IN @r_spmon
   GROUP BY a~spmon, a~saknr, a~txt20, b~objnr.

  LOOP AT lt_zfit0620 INTO DATA(ls_zfit0620).

    MOVE: '000'              TO gt_data-versn,
          '04'               TO gt_data-wrttp,
          pa_gjahr           TO gt_data-gjahr,
          ls_zfit0620-objnr  TO gt_data-objnr,
          ls_zfit0620-saknr  TO gt_data-kstar,
          ls_zfit0620-txt20  TO gt_data-ktext.

    DATA(lv_fieldname) = 'WKG0' && ls_zfit0620-spmon+4(2).

    ASSIGN COMPONENT lv_fieldname
      OF STRUCTURE gt_data TO FIELD-SYMBOL(<fs_wkg>).

    MOVE ls_zfit0620-dmbtr TO <fs_wkg>.

    COLLECT gt_data.
    CLEAR   gt_data.

  ENDLOOP.

  DELETE gt_data WHERE wkg001 = 0
                   AND wkg002 = 0
                   AND wkg003 = 0
                   AND wkg004 = 0
                   AND wkg005 = 0
                   AND wkg006 = 0
                   AND wkg007 = 0
                   AND wkg008 = 0
                   AND wkg009 = 0
                   AND wkg010 = 0
                   AND wkg011 = 0
                   AND wkg012 = 0.

  SELECT a~racct, b~ktext, a~objnr,
         SUM( a~hsl ) AS hsl
    FROM acdoca AS a
    LEFT JOIN csku AS b
      ON a~racct = b~kstar
     AND b~ktopl = @gv_ktopl
     AND b~spras = @sy-langu
   WHERE a~rldnr = '0L'
     AND a~gjahr = @pa_gjahr
     AND a~kokrs = @pa_kokrs
     AND a~racct IN @r_kstar
     AND a~objnr IN @r_objnr
     AND a~blart = 'DD'
     AND a~fiscyearper IN @r_fyear
   GROUP BY a~racct, b~ktext, a~objnr
    INTO TABLE @gt_acdoca.

  DELETE gt_acdoca WHERE hsl = 0.

*-- 금액이 0인 WBS 삭제 처리
  LOOP AT gt_prps.

    CLEAR: lt_data[], lt_acdoca[].

    LOOP AT gt_data WHERE objnr = gt_prps-objnr.
      MOVE gt_data TO lt_data.
      CLEAR: lt_data-versn, lt_data-wrttp,
             lt_data-gjahr, lt_data-kstar,
             lt_data-ktext.
      COLLECT lt_data.
    ENDLOOP.

    LOOP AT gt_acdoca WHERE objnr = gt_prps-objnr.
      MOVE gt_acdoca TO lt_acdoca.
      CLEAR: lt_acdoca-racct, lt_acdoca-ktext.
      COLLECT lt_acdoca.
    ENDLOOP.

    IF lt_data[] IS INITIAL AND
       lt_acdoca[] IS INITIAL.
      DELETE gt_prps WHERE objnr = gt_prps-objnr.
    ENDIF.

  ENDLOOP.

  IF gt_prps[] IS INITIAL.
    MESSAGE s004 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  PERFORM set_tree_data TABLES r_posid.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SCREEN
*&---------------------------------------------------------------------*
FORM set_screen .

  LOOP AT SCREEN.
    IF screen-group1 = 'MG1'.
      screen-input = 0 .
    ENDIF.
    MODIFY SCREEN.
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
*& Form SET_RANGES_OBJNR
*&---------------------------------------------------------------------*
FORM set_ranges_objnr .

  CLEAR: r_objnr,   r_objnr[],
         r_kstar,   r_kstar[],
         r_spmon,   r_spmon[].

  RANGES r_posid FOR prps-posid.

  PERFORM authority_check.

  IF so_posid[] IS NOT INITIAL.
    r_posid[] = so_posid[].
  ENDIF.

  IF pa_pdgr IS NOT INITIAL.

    PERFORM read_hierarchy_tables TABLES gt_values
                                  USING '0110'
                                        pa_pdgr.   "WBS 요소 그룹

    LOOP AT gt_values.

      MOVE: 'I'   TO r_posid-sign,
            'BT'  TO r_posid-option.

      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
        EXPORTING
          input     = gt_values-vfrom
        IMPORTING
          output    = r_posid-low
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.

      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
        EXPORTING
          input     = gt_values-vto
        IMPORTING
          output    = r_posid-high
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.

      COLLECT r_posid.
      CLEAR   r_posid.

    ENDLOOP.

  ENDIF.

  IF pa_eperl IS NOT INITIAL.

    MOVE: 'I'   TO r_fyear-sign,
          'BT'  TO r_fyear-option.

    r_fyear-low  = pa_gjahr && pa_sperl.
    r_fyear-high = pa_gjahr && pa_eperl.
    APPEND r_fyear.

    MOVE: 'I'   TO r_spmon-sign,
          'BT'  TO r_spmon-option.

    r_spmon-low  = pa_gjahr && pa_sperl+1(2).
    r_spmon-high = pa_gjahr && pa_eperl+1(2).
    APPEND r_spmon.

  ELSE.

    MOVE: 'I'   TO r_fyear-sign,
          'EQ'  TO r_fyear-option.

    r_fyear-low  = pa_gjahr && pa_sperl.
    APPEND r_fyear.

    MOVE: 'I'   TO r_spmon-sign,
          'EQ'  TO r_spmon-option.

    r_spmon-low  = pa_gjahr && pa_sperl+1(2).
    APPEND r_spmon.

  ENDIF.

  SELECT b~pspid, b~post1 AS psptx,
         a~objnr, a~pspnr, a~posid, a~post1
    FROM prps AS a
   INNER JOIN proj AS b
      ON a~psphi = b~pspnr
    INTO TABLE @gt_prps
   WHERE a~objnr IN @r_objnr
     AND a~pbukr IN @so_bukrs
     AND a~posid IN @r_posid
     AND b~pspid IN @so_pspid
     AND a~prctr IN @so_prctr
     AND a~loevm = @space
     AND b~loevm = @space
     AND a~pkokr = @pa_kokrs
     AND a~pbukr IN @r_bukrs
     AND a~prctr IN @r_prctr1
     AND b~pspid IN @r_pspid
    ORDER BY b~pspid, a~posid.

  IF sy-subrc <> 0 .
    MESSAGE s004 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  CLEAR: r_objnr,  r_objnr[],
         r_posid2, r_posid2[].

  LOOP AT gt_prps.

    MOVE: 'I'           TO r_objnr-sign,
          'EQ'          TO r_objnr-option,
          gt_prps-objnr TO r_objnr-low.

    COLLECT r_objnr.
    CLEAR   r_objnr.

    MOVE: 'I'           TO r_posid2-sign,
          'EQ'          TO r_posid2-option,
          gt_prps-posid TO r_posid2-low.

    COLLECT r_posid2.
    CLEAR   r_posid2.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form READ_HIERARCHY_TABLES
*&---------------------------------------------------------------------*
FORM read_hierarchy_tables  TABLES pt_values STRUCTURE grpvalues
                            USING pv_class TYPE setclass
                                  pv_setid.

  DATA: lv_setid     LIKE sethier-setid,
        lv_overwrite LIKE sy-datar,
        lt_info      LIKE grphinfo OCCURS 0 WITH HEADER LINE,
        lt_nodes     LIKE grpobjects OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'G_SET_ENCRYPT_SETID'
    EXPORTING
      setclass  = pv_class
      shortname = pv_setid
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
*& Form CREATE_INSTANCE_0100
*&---------------------------------------------------------------------*
FORM create_instance_0100 .

  CREATE OBJECT gr_splitter1
    EXPORTING
      rows    = 1
      columns = 2
      parent  = cl_gui_splitter_container=>screen0.

*-- 1. LEFT data
  gr_left_container = gr_splitter1->get_container(
      row       = 1
      column    = 1 ).

*-- 2. data
  gr_data_container = gr_splitter1->get_container(
      row       = 1
      column    = 2 ).

*-- 1. Left data
  CALL METHOD gr_splitter1->set_column_width
    EXPORTING
      id    = 1
      width = 20.

*-- 2. Left data
  CREATE OBJECT gr_tree1
    EXPORTING
      parent                      = gr_left_container
      node_selection_mode         = 0
      item_selection              = abap_true
      no_html_header              = abap_true
      no_toolbar                  = space
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      illegal_node_selection_mode = 5
      failed                      = 6
      illegal_column_name         = 7.

  CREATE OBJECT gr_tree_data
    EXPORTING
      parent                      = gr_data_container
      node_selection_mode         = 0
      item_selection              = abap_true
      no_html_header              = abap_false
      no_toolbar                  = space
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      illegal_node_selection_mode = 5
      failed                      = 6
      illegal_column_name         = 7.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REGIST_ALV_EVENT_0100
*&---------------------------------------------------------------------*
FORM regist_alv_event_0100.

  DATA: lt_events TYPE cntl_simple_events,
        ls_event  TYPE cntl_simple_event.

* REGISTER EVENT
  CALL METHOD gr_tree1->get_registered_events
    IMPORTING
      events = lt_events.

  ls_event-eventid = cl_gui_column_tree=>eventid_node_double_click.
  APPEND ls_event TO lt_events.

  ls_event-eventid = cl_gui_column_tree=>eventid_item_double_click.
  APPEND ls_event TO lt_events.

  ls_event-eventid = cl_gui_column_tree=>eventid_header_click.
  APPEND ls_event TO lt_events.

  CALL METHOD gr_tree1->set_registered_events
    EXPORTING
      events                    = lt_events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.

  IF sy-subrc <> 0.
    MESSAGE x000 WITH 'ERROR'.
  ENDIF.

*-- GR_EVENT_RECEIVER
  IF gr_tree_event_receiver IS INITIAL.
    CREATE OBJECT gr_tree_event_receiver.
  ENDIF.

* Handler Event
  SET HANDLER:
    gr_tree_event_receiver->handle_node_double_click
        FOR gr_tree1,
    gr_tree_event_receiver->handle_item_double_click
        FOR gr_tree1,
    gr_tree_event_receiver->handle_header_click
        FOR gr_tree1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_NODE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
FORM event_node_double_click
                  USING p_node_key TYPE lvc_nkey
                        p_sender TYPE REF TO cl_gui_alv_tree.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_NODE_ITEM_DOUBLE_CLICK
*&---------------------------------------------------------------------*
FORM event_node_item_double_click  USING p_node_key TYPE lvc_nkey
                                         p_fieldname TYPE lvc_fname
                                         sender.

  RANGES: lr_budat  FOR cobk-budat,
          lr_kstar  FOR cskb-kstar,
          lr_posid  FOR prps_r-posid.

  DATA lv_koagr TYPE kagru.

  DATA ls_posid TYPE curto_pspnr_range.

  CASE sender.

    WHEN gr_tree1.

      CLEAR gs_select.

      CLEAR: r_posid, r_posid[].

      READ TABLE gt_prps_display INTO gs_select INDEX p_node_key.

      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF gs_select-pspid IS NOT INITIAL AND      "Project
         gs_select-posid IS INITIAL.

        LOOP AT gt_prps WHERE pspid = gs_select-pspid.

          MOVE: gt_prps-objnr TO ls_posid-low,
                'I'           TO ls_posid-sign,
                'EQ'          TO ls_posid-option.

          APPEND ls_posid TO r_posid.

        ENDLOOP.

      ELSEIF gs_select-pspid IS NOT INITIAL AND  "WBS
              gs_select-posid IS NOT INITIAL.

        LOOP AT gt_prps WHERE pspid = gs_select-pspid
                          AND posid = gs_select-posid.

          MOVE: gt_prps-objnr TO ls_posid-low,
                'I'           TO ls_posid-sign,
                'EQ'          TO ls_posid-option.

          APPEND ls_posid TO r_posid.

        ENDLOOP.

      ELSE.       "ALL



      ENDIF.

      CLEAR: gt_outtab_display[].

      PERFORM set_tree_data TABLES r_posid.
      PERFORM free_data_tree.
      PERFORM display_alv_tree_0100_01.
      PERFORM regist_alv_event_0100_01.    "TREE EVENT

      MESSAGE s005 .

    WHEN gr_tree_data.

      CASE p_fieldname.

        WHEN 'F1HSL'.

          SELECT DISTINCT
                 'I' AS sign, 'EQ' AS option,
                 posid AS low
           INTO CORRESPONDING FIELDS OF TABLE @lr_posid
            FROM prps
           WHERE objnr IN @r_objnr. "modi bsgsm_fcm  20210706
*           WHERE OBJNR IN @R_POSID.

          READ TABLE gt_outtab_display INTO DATA(ls_display)
                 INDEX p_node_key.

          IF ls_display-setid IS NOT INITIAL AND
             ls_display-kstar IS NOT INITIAL.

            CONCATENATE 'IEQ' ls_display-kstar INTO lr_kstar.
            APPEND lr_kstar.

          ELSEIF ls_display-setid IS NOT INITIAL AND
                  ls_display-kstar IS INITIAL.

            MOVE ls_display-setid TO lv_koagr.

          ELSE.

            READ TABLE gt_zcot0150 WITH KEY zcode = ls_display-zcode.

            IF sy-subrc = 0.

              PERFORM set_kstar_ranges TABLES lr_kstar
                                       USING gt_zcot0150-fkagru
                                             gt_zcot0150-tkagru.

            ENDIF.

          ENDIF.

          MOVE: 'I'  TO lr_budat-sign,
                'BT' TO lr_budat-option.

          CONCATENATE pa_gjahr pa_sperl+1(2) '01' INTO lr_budat-low.
          CONCATENATE pa_gjahr pa_eperl+1(2) '01' INTO lr_budat-high.

          CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
            EXPORTING
              day_in            = lr_budat-high
            IMPORTING
              last_day_of_month = lr_budat-high
            EXCEPTIONS
              day_in_no_date    = 1
              OTHERS            = 2.

          APPEND lr_budat.

          SUBMIT rkpep003  USING SELECTION-SCREEN '1000'
               WITH cn_pspnr    IN lr_posid
               WITH r_kstar     IN lr_kstar
               WITH koagr       = lv_koagr
               WITH r_budat     IN lr_budat
               AND RETURN.
      ENDCASE.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_NODE_HANDLE_HEADER_CLICK
*&---------------------------------------------------------------------*
FORM event_node_handle_header_click  USING p_fieldname TYPE lvc_fname
                                           sender.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_HIERARCHY_HEADER_0100
*&---------------------------------------------------------------------*
FORM build_hierarchy_header_0100 .

  gs_hierarchy_header-heading = TEXT-h01.
  gs_hierarchy_header-width = 30.
  gs_hierarchy_header-width_pix = ''.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV_TREE_0100
*&---------------------------------------------------------------------*
FORM display_alv_tree_0100 .

  CALL METHOD gr_tree1->set_table_for_first_display
    EXPORTING
      is_hierarchy_header  = gs_hierarchy_header
      it_toolbar_excluding = gt_exclude[]
    CHANGING
      it_outtab            = gt_prps_display[]
      it_fieldcatalog      = gt_fieldcat[].

  PERFORM build_hierarchy.

* calculate totals
  CALL METHOD gr_tree1->update_calculations.

* this method must be called to send the data to the frontend
  CALL METHOD gr_tree1->frontend_update.

  CALL METHOD cl_gui_cfw=>flush
    EXCEPTIONS
      cntl_system_error = 1
      cntl_error        = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form APPEND_FIELDCAT_0100
*&---------------------------------------------------------------------*
FORM append_fieldcat_0100 .

  PERFORM get_fieldcatlog_data.
  PERFORM modify_fieldcatlog_data.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
FORM get_fieldcatlog_data .

  DATA: lt_fieldcat TYPE kkblo_t_fieldcat.

  CALL FUNCTION 'K_KKB_FIELDCAT_MERGE'
    EXPORTING
      i_callback_program     = sy-repid
      i_strucname            = 'ZCOS0180' "ABAP DIC. 정의된 STRUCTURE
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

    " Error Fieldcatalog merge!!
    MESSAGE e020(zmstda).

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
FORM modify_fieldcatlog_data .

  DATA:  lv_text(50).

  LOOP AT gt_fieldcat INTO gs_fieldcat.

    CLEAR: lv_text.

    CASE gs_fieldcat-fieldname.

      WHEN 'DESCR'.
        lv_text = TEXT-c01.

      WHEN OTHERS.
        gs_fieldcat-no_out = abap_true.

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

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADD_NODEKEY
*&---------------------------------------------------------------------*
FORM add_nodekey  USING   pv_relat_key TYPE lvc_nkey
                           pr_tree       TYPE REF TO cl_gui_alv_tree
                           ps_outtab     TYPE zcos0180
                           p_gubun
                  CHANGING pv_node_key  TYPE lvc_nkey.

  DATA: lv_node_text TYPE lvc_value.

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  DATA ls_display TYPE zcos0180.
  MOVE ps_outtab TO ls_display.

  DATA ls_node_layout TYPE lvc_s_layn.

  DATA: lv_pspid TYPE ps_pspid,
        lv_posid TYPE ps_posid.

  CASE p_gubun.

    WHEN 'ALL'.
      CLEAR ls_display.
      lv_node_text = TEXT-t06.

    WHEN 'PSPID'.

      CLEAR: ls_display-posid,
             ls_display-objnr,
             ls_display-pspnr,
             ls_display-posid,
             ls_display-post1.

      ls_node_layout-n_image   = '@EC@'.
      ls_node_layout-exp_image = '@EC@'.

      CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
        EXPORTING
          input  = ps_outtab-pspid
        IMPORTING
          output = lv_pspid.

*      CONCATENATE LV_PSPID '(' PS_OUTTAB-PSPTX ')'
*            INTO LV_NODE_TEXT SEPARATED BY SPACE.

      lv_node_text = lv_pspid.

    WHEN 'POSID'.

      CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
        EXPORTING
          input  = ps_outtab-posid
        IMPORTING
          output = lv_posid.

      ls_node_layout-isfolder  = ' '.

      ls_node_layout-n_image   = '@ED@'.
      ls_node_layout-exp_image = '@ED@'.
      ls_item_layout-fieldname = pr_tree->c_hierarchy_column_name.
      ls_item_layout-style =
       cl_gui_column_tree=>style_emphasized.

      APPEND ls_item_layout TO lt_item_layout.

      lv_node_text = lv_posid.

  ENDCASE.

  CALL METHOD pr_tree->add_node
    EXPORTING
      i_relat_node_key = pv_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = lv_node_text
      is_outtab_line   = ls_display
      is_node_layout   = ls_node_layout
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = pv_node_key.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_HIERARCHY
*&---------------------------------------------------------------------*
FORM build_hierarchy .

  DATA: lv_key1 TYPE lvc_nkey,
        lv_key2 TYPE lvc_nkey,
        lv_key3 TYPE lvc_nkey,
        lv_root TYPE lvc_nkey.

  DATA ls_prps TYPE zcos0180.

  PERFORM add_nodekey     USING    ''
                                   gr_tree1
                                   ls_prps
                                   'ALL'
                          CHANGING lv_root.

  LOOP AT gt_prps.

    MOVE gt_prps TO ls_prps.

    AT NEW pspid.

      MOVE ls_prps-psptx TO ls_prps-descr.

      PERFORM add_nodekey     USING    lv_root
                                       gr_tree1
                                       ls_prps
                                       'PSPID'
                              CHANGING lv_key2.
    ENDAT.

    MOVE ls_prps-post1 TO ls_prps-descr.

    PERFORM add_nodekey     USING    lv_key2
                                     gr_tree1
                                     ls_prps
                                     'POSID'
                            CHANGING lv_key3.

    MODIFY gt_prps FROM ls_prps.

  ENDLOOP.

  CALL METHOD gr_tree1->expand_node
    EXPORTING
      i_node_key = lv_root.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form APPEND_FIELDCAT_0100_01
*&---------------------------------------------------------------------*
FORM append_fieldcat_0100_01 .

  PERFORM get_fieldcatlog_data2.
  PERFORM modify_fieldcatlog_data2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_FIELDCATLOG_DATA2
*&---------------------------------------------------------------------*
FORM get_fieldcatlog_data2 .

  DATA: lt_fieldcat TYPE kkblo_t_fieldcat.

  CALL FUNCTION 'K_KKB_FIELDCAT_MERGE'
    EXPORTING
      i_callback_program     = sy-repid
      i_strucname            = 'ZCOS0110' "ABAP DIC. 정의된 STRUCTURE
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
        et_fieldcat_lvc   = gt_fieldcat2[]
      EXCEPTIONS
        it_data_missing   = 1.
  ELSE.

    " Error Fieldcatalog merge!!
    MESSAGE e020(zmstda).

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_FIELDCATLOG_DATA2
*&---------------------------------------------------------------------*
FORM modify_fieldcatlog_data2 .

  DATA:  lv_text(50).

  LOOP AT gt_fieldcat2 INTO gs_fieldcat.

    CLEAR: lv_text.

    CASE gs_fieldcat-fieldname.

      WHEN 'DESCR'.
        lv_text = TEXT-c01.
        gs_fieldcat-outputlen = 30.

      WHEN 'F1HSL' .
        lv_text = TEXT-c04.
        gs_fieldcat-do_sum = abap_true.

        gs_fieldcat-outputlen = 25.

      WHEN 'F2HSL'.
        lv_text = TEXT-c05.
        gs_fieldcat-do_sum = abap_true.

        gs_fieldcat-outputlen = 25.

      WHEN 'F3HSL'.
        lv_text = TEXT-c06.
        gs_fieldcat-do_sum = abap_true.

        gs_fieldcat-outputlen = 25.

      WHEN 'F4HSL'.

        CONCATENATE '(' pa_versn ' 버전)' INTO DATA(lv_versn).

        lv_text = TEXT-c07 && lv_versn.

        gs_fieldcat-do_sum = abap_true.

        gs_fieldcat-outputlen = 25.

      WHEN 'F5HSL'.
        lv_text = TEXT-c08.
        gs_fieldcat-do_sum = abap_true.

        gs_fieldcat-outputlen = 25.

      WHEN 'F6HSL'.
        lv_text = TEXT-c09.
        gs_fieldcat-do_sum = abap_true.

        gs_fieldcat-outputlen = 34.

      WHEN OTHERS.
        gs_fieldcat-no_out = abap_true.

    ENDCASE.



    IF lv_text IS NOT INITIAL.
      gs_fieldcat-coltext   = lv_text.
      gs_fieldcat-scrtext_l = lv_text.
      gs_fieldcat-scrtext_m = lv_text.
      gs_fieldcat-scrtext_s = lv_text.
    ENDIF.

    MODIFY gt_fieldcat2 FROM gs_fieldcat.

  ENDLOOP.

*  DATA: L_FIELDCATALOG_WA TYPE LVC_S_FCAT.
*  DATA L_COL_NAME TYPE TV_ITMNAME.
*
** set col_key in fieldcatalog
*  LOOP AT GT_FIELDCAT2 INTO GS_FIELDCAT.
*    CONCATENATE 'C' M_COLUMN_KEY_COUNT INTO L_COL_NAME.
*    L_FIELDCATALOG_WA-H_COL_KEY = L_COL_NAME.
*    MODIFY MT_FIELDCATALOG FROM L_FIELDCATALOG_WA
*           TRANSPORTING H_COL_KEY.
*    ADD 1 TO M_COLUMN_KEY_COUNT.
*  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_KSTAR_RANGES
*&---------------------------------------------------------------------*
FORM set_kstar_ranges TABLES pr_kstar STRUCTURE r_kstar
                             USING p_fkagru p_tkagru.

  RANGES r_setname FOR setnode-setname.

  IF p_tkagru IS NOT INITIAL.

    MOVE: 'I'      TO r_setname-sign,
          'BT'     TO r_setname-option,
          p_fkagru TO r_setname-low,
          p_tkagru TO r_setname-high.

    APPEND r_setname.

  ELSE.

    MOVE: 'I'      TO r_setname-sign,
          'EQ'     TO r_setname-option,
          p_fkagru TO r_setname-low.

    APPEND r_setname.

  ENDIF.

  SELECT * FROM setheader
    INTO TABLE @DATA(lt_setheader)
   WHERE setclass = '0102'
     AND subclass = @pa_kokrs
     AND setname  IN @r_setname.

  LOOP AT lt_setheader INTO DATA(ls_setheader).

    PERFORM hierarchy_read USING ls_setheader-setname.

    LOOP  AT gt_values.

      IF gt_values-vfrom = gt_values-vto.

        MOVE: gt_values-vfrom   TO pr_kstar-low,
              'I'               TO pr_kstar-sign,
              'EQ'              TO pr_kstar-option.
      ELSE.

        MOVE: gt_values-vfrom   TO pr_kstar-low,
              gt_values-vto     TO pr_kstar-high,
              'I'               TO pr_kstar-sign,
              'BT'              TO pr_kstar-option.
      ENDIF.

      COLLECT pr_kstar.
      CLEAR   pr_kstar.

    ENDLOOP.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_COMMENT
*&---------------------------------------------------------------------*
FORM build_comment USING
      pt_list_commentary TYPE slis_t_listheader
      p_logo             TYPE sdydo_value.

  DATA: ls_line TYPE slis_listheader.


  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = TEXT-t02.

  DATA(lv_gjahr) = pa_gjahr && TEXT-t04.
  DATA(lv_smon)  = pa_sperl+1(2) && TEXT-t05.
  DATA(lv_emon)  = pa_eperl+1(2) && TEXT-t05.

  CONCATENATE lv_gjahr lv_smon '~' lv_emon
         INTO ls_line-info SEPARATED BY space.

  APPEND ls_line TO pt_list_commentary.

  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = TEXT-t07.

  DATA(lv_vtxt) = '(' && pa_vtxt && ')'.

  CONCATENATE pa_versn lv_vtxt
         INTO ls_line-info SEPARATED BY space.

  APPEND ls_line TO pt_list_commentary.

  ls_line-typ  = 'A'.
  ls_line-info = TEXT-t03.
  APPEND ls_line TO pt_list_commentary.

*  P_LOGO = 'ENJOYSAP_LOGO'.
  p_logo = 'TSKLOGO'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_HIERARCHY_HEADER_0100_01
*&---------------------------------------------------------------------*
FORM build_hierarchy_header_0100_01 .

  gs_hierarchy_header2-heading = TEXT-h02.
  gs_hierarchy_header2-width = 30.
  gs_hierarchy_header2-width_pix = ''.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV_TREE_0100_01
*&---------------------------------------------------------------------*
FORM display_alv_tree_0100_01 .

* create info-table for html-header
  DATA: lt_list_commentary TYPE slis_t_listheader,
        l_logo             TYPE sdydo_value.

  PERFORM build_comment USING
                 lt_list_commentary
                 l_logo.

  CALL METHOD gr_tree_data->set_table_for_first_display
    EXPORTING
      is_hierarchy_header  = gs_hierarchy_header2
      it_toolbar_excluding = gt_exclude[]
      it_list_commentary   = lt_list_commentary
      i_logo               = l_logo
    CHANGING
      it_outtab            = gt_outtab_display[]
      it_fieldcatalog      = gt_fieldcat2[].

  PERFORM build_hierarchy2.

* calculate totals
  CALL METHOD gr_tree_data->update_calculations.

* this method must be called to send the data to the frontend
  CALL METHOD gr_tree_data->frontend_update.

  CALL METHOD cl_gui_cfw=>flush
    EXCEPTIONS
      cntl_system_error = 1
      cntl_error        = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_HIERARCHY2
*&---------------------------------------------------------------------*
FORM build_hierarchy2 .

  DATA: lv_key1 TYPE lvc_nkey,
        lv_key2 TYPE lvc_nkey,
        lv_key3 TYPE lvc_nkey,
        lv_key4 TYPE lvc_nkey,
        lv_key5 TYPE lvc_nkey.

  DATA ls_outtab TYPE zcos0110.

  DATA: BEGIN OF lt_key OCCURS 0,
          lv_key TYPE lvc_nkey,
        END OF lt_key.

  CLEAR: gt_topnodes, gt_topnodes[].

  DATA lt_outtab LIKE TABLE OF gs_outtab WITH HEADER LINE.

  DATA lv_setid TYPE setid.

  LOOP AT gt_outtab INTO gs_outtab.

    CASE gs_outtab-type.

      WHEN 'H'.

        PERFORM add_nodekey2     USING   lv_key1
                                         gr_tree_data
                                         gs_outtab
                                         'ZCODE'
                                CHANGING lv_key2.

        MOVE lv_key2 TO gs_topnodes-nodekey.
        APPEND gs_topnodes TO gt_topnodes.
        CLEAR gs_topnodes.

      WHEN 'R'.

        MOVE gs_outtab-shortname TO gs_outtab-descr.

        PERFORM add_nodekey2     USING   lv_key2
                                         gr_tree_data
                                         gs_outtab
                                         'SETID'
                                CHANGING lv_key3.

    ENDCASE.

    MODIFY gt_outtab FROM gs_outtab.

  ENDLOOP.

  SORT gt_topnodes DESCENDING.

  LOOP AT gt_topnodes INTO gs_topnodes.
    CALL METHOD gr_tree_data->expand_node
      EXPORTING
        i_node_key = gs_topnodes-nodekey.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TREE_DATA
*&---------------------------------------------------------------------*
FORM set_tree_data TABLES pt_objnr STRUCTURE ftr_s_objnr.

  DATA lv_month TYPE n LENGTH 3.
  DATA lv_fieldname TYPE fieldname.

  DATA: result_tab TYPE match_result_tab.

  DATA lv_operand TYPE string.

  DATA ls_outtab TYPE zcos0110.

  CLEAR gt_outtab[].
  CLEAR gs_outtab.

  FIELD-SYMBOLS: <fs_hsl>   TYPE any,
                 <fs_sum>   TYPE any,
                 <fs_all>   TYPE any,
                 <fs_wkg>   TYPE any,
                 <fs_value> TYPE any.

  FIELD-SYMBOLS: <fs>  TYPE any.
  FIELD-SYMBOLS: <fs1> TYPE any.

  DATA: lv_res    TYPE i,
        lv_ope    TYPE c,
        lv_off    TYPE i,
        lv_length TYPE i.

  DATA lv_index TYPE syindex.
  DATA lv_lines TYPE sytfill.

  DATA lv_zcode TYPE zcode.

  RANGES r_setname FOR setnode-setname.

  LOOP AT gt_zcot0150.

    MOVE: gt_zcot0150-zcode TO gs_outtab-zcode,
          gt_zcot0150-descr TO gs_outtab-settext,
          gt_zcot0150-gubun TO gs_outtab-gubun,
          gv_waers          TO gs_outtab-waers,
          'H'               TO gs_outtab-type.

    COLLECT gs_outtab INTO gt_outtab.

    CASE gt_zcot0150-gubun.

      WHEN '2'.

        CLEAR: r_setname, r_setname[].

        IF gt_zcot0150-tkagru IS NOT INITIAL.

          MOVE: 'I'                TO r_setname-sign,
                'BT'               TO r_setname-option,
                gt_zcot0150-fkagru TO r_setname-low,
                gt_zcot0150-tkagru TO r_setname-high.

          APPEND r_setname.

        ELSE.

          MOVE: 'I'      TO r_setname-sign,
                'EQ'     TO r_setname-option,
               gt_zcot0150-fkagru TO r_setname-low.

          APPEND r_setname.

        ENDIF.

        SELECT * FROM setheader
          INTO TABLE @DATA(lt_setheader)
         WHERE setclass = '0102'
           AND subclass = @pa_kokrs
           AND setname  IN @r_setname
          ORDER BY setclass, subclass, setname.

        IF sy-subrc = 0 .

          LOOP AT lt_setheader INTO DATA(ls_setheader).

            PERFORM hierarchy_read USING ls_setheader-setname.

            LOOP AT gt_nodes.

              MOVE: gt_nodes-shortname TO gs_outtab-setid,
                    gt_nodes-descript  TO gs_outtab-shortname,
                    gt_nodes-type      TO gs_outtab-type,
                    gt_nodes-hlevel    TO gs_outtab-hlevel.

*-- 상위노드 GET
              CASE gt_nodes-hlevel.

                WHEN 0.
                  MOVE: 'R' TO gs_outtab-type.
                  MOVE: gs_outtab-setid TO gs_outtab-subname.

                  CLEAR: gs_outtab-kstar, gs_outtab-ktext,
                         gs_outtab-descr.

                  COLLECT gs_outtab INTO gt_outtab.

                  READ TABLE gt_values WITH KEY setid = gt_nodes-setid.  "계정 X
                  IF sy-subrc <> 0.
                    CONTINUE.
                  ENDIF.

                WHEN OTHERS.

                  READ TABLE gt_setnode
                     WITH KEY subsetname = gs_outtab-setid.

                  IF sy-subrc = 0.
                    MOVE: gt_setnode-setname TO gs_outtab-subname.
                  ELSE.
                    MOVE: gs_outtab-setid TO gs_outtab-subname.
                  ENDIF.

                  READ TABLE gt_values WITH KEY setid = gt_nodes-setid.  "계정 X

                  IF sy-subrc <> 0.

                    CLEAR: gs_outtab-kstar,  gs_outtab-ktext,
                           gs_outtab-f1hsl,  gs_outtab-f2hsl,
                           gs_outtab-f3hsl,  gs_outtab-f4hsl,
                           gs_outtab-f5hsl,  gs_outtab-f5hsl,
                           gs_outtab-f6hsl,  gs_outtab-descr.

                    COLLECT gs_outtab INTO gt_outtab.
                    CONTINUE.

                  ENDIF.

              ENDCASE.

              MOVE gt_nodes-type TO gs_outtab-type.

*-- 그룹에 등록되어 있는 계정만큼

              LOOP AT gt_values WHERE setid = gt_nodes-setid.

                LOOP AT gt_cska WHERE kstar >= gt_values-vfrom
                                   AND kstar <= gt_values-vto.

                  MOVE: gt_cska-kstar TO gs_outtab-kstar,
                        gt_cska-ktext TO gs_outtab-ktext,
                        gt_cska-ktext TO gs_outtab-descr.

                  COLLECT gs_outtab INTO gt_outtab.

                  LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<ls_data>)
                                   WHERE kstar = gs_outtab-kstar
                                     AND objnr IN pt_objnr.

                    CLEAR lv_month.

                    DO 12 TIMES.

                      ADD 1 TO lv_month.

                      IF lv_month >= pa_sperl AND
                         lv_month <= pa_eperl.

                        lv_fieldname = '<LS_DATA>-WKG' && lv_month.

                        ASSIGN (lv_fieldname) TO <fs_wkg>.

                        IF <ls_data>-versn = '000' AND
                           <ls_data>-wrttp  = '04'.

                          ASSIGN COMPONENT 'F1HSL'
                           OF STRUCTURE gs_outtab TO <fs_all>.
                          <fs_all> = <fs_all> + <fs_wkg>.

                        ENDIF.

*-- 수주계획은 실적으로 표기
                        IF <ls_data>-versn    = '000' AND
                                   <ls_data>-wrttp  = '01' AND
                                   gs_outtab-kstar = '0984000010'.

                          ASSIGN COMPONENT 'F1HSL'
                           OF STRUCTURE gs_outtab TO <fs_all>.
                          <fs_all> = <fs_all> + <fs_wkg>.

                        ENDIF.

                        IF <ls_data>-versn = pa_versn AND  "계획
                              <ls_data>-wrttp   = '01'.

                          ASSIGN COMPONENT 'F4HSL'
                           OF STRUCTURE gs_outtab TO <fs_all>.
                          <fs_all> = <fs_all> + <fs_wkg>.

                        ENDIF.

                        IF <ls_data>-versn = 'PZ' AND  "계획 원가 배분
                              <ls_data>-wrttp   = '01'.

                          ASSIGN COMPONENT 'F6HSL'
                            OF STRUCTURE gs_outtab TO <fs_all>.
                          <fs_all> = <fs_all> + <fs_wkg>.

                        ENDIF.

                      ENDIF.

                    ENDDO.

                    IF    gs_outtab-kstar CP '04*' OR    "매출 계정은 음수로 표시
                          gs_outtab-kstar CP '0701*' OR
                          gs_outtab-kstar CP '0703*' OR
                          gs_outtab-kstar CP '0705*'.
                      gs_outtab-f1hsl =  gs_outtab-f1hsl * '-1'.
                      gs_outtab-f4hsl =  gs_outtab-f4hsl * '-1'.
                      gs_outtab-f6hsl =  gs_outtab-f6hsl * '-1'.
                    ENDIF.

                    gs_outtab-f3hsl = gs_outtab-f1hsl - gs_outtab-f2hsl.
                    gs_outtab-f5hsl = gs_outtab-f6hsl - gs_outtab-f4hsl.

                    COLLECT gs_outtab INTO gt_outtab.

                    MOVE gs_outtab TO ls_outtab.
                    MOVE 'H'       TO ls_outtab-type.

                    CLEAR: ls_outtab-setid,   ls_outtab-shortname,
                           ls_outtab-kstar,   ls_outtab-ktext,
                           ls_outtab-subname, ls_outtab-hlevel,
                           ls_outtab-descr.

                    COLLECT ls_outtab INTO gt_outtab.

                    CLEAR: gs_outtab-f1hsl, gs_outtab-f2hsl,
                           gs_outtab-f3hsl, gs_outtab-f4hsl,
                           gs_outtab-f5hsl, gs_outtab-f5hsl,
                           gs_outtab-f6hsl.

                  ENDLOOP.

                  LOOP AT gt_acdoca ASSIGNING FIELD-SYMBOL(<ls_acdoca>)
                                     WHERE racct = gs_outtab-kstar
                                       AND objnr IN pt_objnr.

                    gs_outtab-f2hsl = gs_outtab-f2hsl +  <ls_acdoca>-hsl.

                    IF gs_outtab-kstar CP '04*' OR  "매출 계정은 음수로 표시
                        gs_outtab-kstar CP '0701*' OR
                        gs_outtab-kstar CP '0703*' OR
                        gs_outtab-kstar CP '0705*'.
                      gs_outtab-f2hsl =  gs_outtab-f2hsl * '-1'.
                    ENDIF.

                    gs_outtab-f3hsl = gs_outtab-f1hsl - gs_outtab-f2hsl.
                    gs_outtab-f5hsl = gs_outtab-f6hsl - gs_outtab-f4hsl.

                    COLLECT gs_outtab INTO gt_outtab.

                    MOVE gs_outtab TO ls_outtab.
                    MOVE 'H'       TO ls_outtab-type.

                    CLEAR: ls_outtab-setid,   ls_outtab-shortname,
                           ls_outtab-kstar,   ls_outtab-ktext,
                           ls_outtab-subname, ls_outtab-hlevel,
                            ls_outtab-descr.

                    COLLECT ls_outtab INTO gt_outtab.

                    CLEAR: gs_outtab-f1hsl, gs_outtab-f2hsl,
                           gs_outtab-f3hsl, gs_outtab-f4hsl,
                           gs_outtab-f5hsl, gs_outtab-f5hsl,
                           gs_outtab-f6hsl.

                  ENDLOOP.

                ENDLOOP.

              ENDLOOP.

            ENDLOOP.

          ENDLOOP.

        ENDIF.

    ENDCASE.

    CLEAR gs_outtab.

  ENDLOOP.

  LOOP AT gt_outtab INTO gs_outtab WHERE gubun = '1'.  "수식 계산

    CLEAR: lv_index, lv_lines.

    READ TABLE gt_zcot0150 WITH KEY zcode = gs_outtab-zcode.

    IF sy-subrc = 0.

      REFRESH result_tab.

      FIND ALL OCCURRENCES OF REGEX '[\+\-]'
           IN gt_zcot0150-calcu
           RESPECTING CASE
           RESULTS result_tab.

      IF sy-subrc = 0.

        lv_lines = lines( result_tab ).

        ADD 1 TO lv_lines.

        DO lv_lines TIMES.

          ADD 1 TO lv_index.

          READ TABLE result_tab INTO DATA(ls_result) INDEX lv_index.

          IF lv_index = 1.

            MOVE: gt_zcot0150-calcu(4)   TO lv_zcode,
                  gt_zcot0150-calcu+4(1) TO lv_ope.

            lv_off = ls_result-offset + ls_result-length.

            READ TABLE gt_outtab INTO DATA(ls_outtab1)
                                     WITH KEY zcode = lv_zcode
                                              setid = space.
            IF sy-subrc = 0.

              MOVE: ls_outtab1-f1hsl TO gs_outtab-f1hsl,
                    ls_outtab1-f2hsl TO gs_outtab-f2hsl,
                    ls_outtab1-f3hsl TO gs_outtab-f3hsl,
                    ls_outtab1-f4hsl TO gs_outtab-f4hsl,
                    ls_outtab1-f5hsl TO gs_outtab-f5hsl,
                    ls_outtab1-f6hsl TO gs_outtab-f6hsl.

            ENDIF.

          ELSE.

            MOVE gt_zcot0150-calcu+lv_off(4) TO lv_zcode.

            lv_off = ls_result-offset + ls_result-length.

            READ TABLE gt_outtab INTO DATA(ls_outtab2)
                                     WITH KEY zcode = lv_zcode
                                              setid = space.

            IF sy-subrc = 0.

              CASE lv_ope.

                WHEN '+'.

                  gs_outtab-f1hsl = gs_outtab-f1hsl + ls_outtab2-f1hsl.
                  gs_outtab-f2hsl = gs_outtab-f2hsl + ls_outtab2-f2hsl.
                  gs_outtab-f3hsl = gs_outtab-f3hsl + ls_outtab2-f3hsl.
                  gs_outtab-f4hsl = gs_outtab-f4hsl + ls_outtab2-f4hsl.
                  gs_outtab-f5hsl = gs_outtab-f5hsl + ls_outtab2-f5hsl.
                  gs_outtab-f6hsl = gs_outtab-f6hsl + ls_outtab2-f6hsl.

                WHEN '-'.

                  gs_outtab-f1hsl = gs_outtab-f1hsl - ls_outtab2-f1hsl.
                  gs_outtab-f2hsl = gs_outtab-f2hsl - ls_outtab2-f2hsl.
                  gs_outtab-f3hsl = gs_outtab-f3hsl - ls_outtab2-f3hsl.
                  gs_outtab-f4hsl = gs_outtab-f4hsl - ls_outtab2-f4hsl.
                  gs_outtab-f5hsl = gs_outtab-f5hsl - ls_outtab2-f5hsl.
                  gs_outtab-f6hsl = gs_outtab-f6hsl - ls_outtab2-f6hsl.

              ENDCASE.

            ENDIF.

            MOVE gt_zcot0150-calcu+ls_result-offset(1) TO lv_ope.

          ENDIF.

        ENDDO.

      ENDIF.

    ENDIF.

    MODIFY gt_outtab FROM gs_outtab.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HIERARCHY_READ
*&---------------------------------------------------------------------*
FORM hierarchy_read  USING   p_setid TYPE setnamenew.

  CLEAR: gt_nodes,  gt_nodes[],
         gt_values, gt_values[],
         gt_info,   gt_info[].

  DATA: lv_setid     LIKE sethier-setid,
        lv_overwrite LIKE sy-datar.

  CALL FUNCTION 'G_SET_ENCRYPT_SETID'
    EXPORTING
      setclass  = '0102'
      shortname = p_setid
    IMPORTING
      setid     = lv_setid.

  IF sy-subrc = 0.

    CALL FUNCTION 'K_HIERARCHY_TABLES_READ'
      EXPORTING
        e_class                     = '0102'
        e_setid                     = lv_setid
        e_kokrs                     = pa_kokrs
      TABLES
        t_nodes                     = gt_nodes
        t_values                    = gt_values
      CHANGING
        c_info                      = gt_info
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
*& Form ADD_NODEKEY
*&---------------------------------------------------------------------*
FORM add_nodekey2  USING   pv_relat_key TYPE lvc_nkey
                           pr_tree       TYPE REF TO cl_gui_alv_tree
                           ps_outtab     TYPE zcos0110
                           p_gubun
                  CHANGING pv_node_key  TYPE lvc_nkey.

  DATA: lv_node_text TYPE lvc_value.

  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  DATA ls_display TYPE zcos0110.
  MOVE ps_outtab TO ls_display.

  DATA lv_key TYPE lvc_nkey.

  DATA ls_node_layout TYPE lvc_s_layn.

  CASE p_gubun.

    WHEN 'ZCODE'.
      CLEAR: ls_display-setid, ls_display-shortname,
             ls_display-kstar, ls_display-ktext.

      lv_node_text = ls_display-settext.

      ls_node_layout-isfolder  = 'X'.
      CALL METHOD pr_tree->add_node
        EXPORTING
          i_relat_node_key = pv_relat_key
          i_relationship   = cl_gui_column_tree=>relat_last_child
          i_node_text      = lv_node_text
          is_outtab_line   = ls_display
          is_node_layout   = ls_node_layout
          it_item_layout   = lt_item_layout
        IMPORTING
          e_new_node_key   = pv_node_key.

    WHEN 'SETID'.

      CLEAR: ls_display-kstar, ls_display-ktext.

      lv_node_text = ls_display-setid.
      ls_node_layout-isfolder  = 'X'.

      CALL METHOD pr_tree->add_node
        EXPORTING
          i_relat_node_key = pv_relat_key
          i_relationship   = cl_gui_column_tree=>relat_last_child
          i_node_text      = lv_node_text
          is_outtab_line   = ls_display
          is_node_layout   = ls_node_layout
          it_item_layout   = lt_item_layout
        IMPORTING
          e_new_node_key   = pv_node_key.

*      PERFORM ADD_NODEKEY3     USING   PV_NODE_KEY
*                                       GR_TREE_DATA
*                                       LS_DISPLAY-ZCODE
*                                       LS_DISPLAY-SETID
*                              CHANGING LV_KEY.

      PERFORM add_line USING ls_display-setid
                             ls_display-zcode
                             pv_node_key.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADD_LINE
*&---------------------------------------------------------------------*
FORM add_line  USING   p_setid
                       p_zcode
                       p_key TYPE lvc_nkey.

  DATA ls_node_layout TYPE lvc_s_layn.
  DATA lv_node_text  TYPE lvc_value.
  DATA lv_key        TYPE lvc_nkey.

  DATA ls_outtab TYPE zcos0110.

  DATA lv_setid TYPE setid.

  LOOP AT gt_outtab INTO DATA(ls_out)
                         WHERE zcode   = p_zcode
                           AND subname = p_setid
                           AND  type <> 'H' AND type <> 'R'.

    MOVE ls_out TO ls_outtab.

    IF ls_outtab-setid = ls_outtab-subname.

      lv_node_text = ls_outtab-kstar.
      MOVE ls_outtab-ktext TO ls_outtab-descr.
      MODIFY gt_outtab FROM ls_outtab.

    ELSEIF ls_outtab-setid <> ls_outtab-subname.

      IF lv_setid = ls_outtab-setid.
        CONTINUE.
      ENDIF.

      lv_setid = ls_outtab-setid.

      lv_node_text = ls_outtab-setid.
      MOVE ls_outtab-shortname TO ls_outtab-descr.
      MODIFY gt_outtab FROM ls_outtab.

      CLEAR: ls_outtab-kstar, ls_outtab-ktext.

      ls_node_layout-isfolder  = 'X'.

    ENDIF.

    CALL METHOD gr_tree_data->add_node
      EXPORTING
        i_relat_node_key = p_key
        i_relationship   = cl_gui_column_tree=>relat_last_child
        i_node_text      = lv_node_text
        is_outtab_line   = ls_outtab
        is_node_layout   = ls_node_layout
      IMPORTING
        e_new_node_key   = lv_key.

    IF ls_outtab-setid <> ls_outtab-subname.

      PERFORM set_kstar USING  ls_outtab-setid
                               ls_outtab-subname
                               p_zcode
                               lv_key.

      READ TABLE gt_setnode WITH KEY setname = ls_outtab-setid
        TRANSPORTING NO FIELDS.

      IF sy-subrc = 0 .
        PERFORM add_line USING ls_outtab-setid
                               ls_outtab-zcode
                               lv_key.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_KSTAR
*&---------------------------------------------------------------------*
FORM set_kstar  USING   p_setid
                        p_subname
                        p_zcode
                        p_key TYPE lvc_nkey.

  DATA lv_node_text  TYPE lvc_value.
  DATA lv_key        TYPE lvc_nkey.

  LOOP AT gt_outtab INTO DATA(ls_temp)
                      WHERE zcode = p_zcode
                        AND setid = p_setid
                        AND subname <> p_setid
                        AND kstar IS NOT INITIAL
                        AND type <> 'H' AND type <> 'R'.

    lv_node_text = ls_temp-kstar.
    MOVE ls_temp-ktext TO ls_temp-descr.
    MODIFY gt_outtab FROM ls_temp.

    CALL METHOD gr_tree_data->add_node
      EXPORTING
        i_relat_node_key = p_key
        i_relationship   = cl_gui_column_tree=>relat_last_child
        i_node_text      = lv_node_text
        is_outtab_line   = ls_temp
      IMPORTING
        e_new_node_key   = lv_key.

  ENDLOOP.

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

  CREATE DATA li_data LIKE gt_outtab.

  ASSIGN li_data->* TO <lt_data>.

  <lt_data> = gt_outtab[].

  CLEAR: t_fieldcat[].

  t_fieldcat[] =  gt_fieldcat2[].

  LOOP AT t_fieldcat INTO gs_fieldcat.

    CASE gs_fieldcat-fieldname.

      WHEN 'HLEVEL' OR 'TYPE'.
        gs_fieldcat-no_out = abap_true.
      WHEN OTHERS.
        gs_fieldcat-no_out = abap_false.
        MODIFY t_fieldcat FROM gs_fieldcat.
    ENDCASE.

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
*& Form REGIST_ALV_EVENT_0100_01
*&---------------------------------------------------------------------*
FORM regist_alv_event_0100_01 .

  DATA: lt_events TYPE cntl_simple_events,
        ls_event  TYPE cntl_simple_event.

* REGISTER EVENT
  CALL METHOD gr_tree_data->get_registered_events
    IMPORTING
      events = lt_events.

  ls_event-eventid = cl_gui_column_tree=>eventid_node_double_click.
  APPEND ls_event TO lt_events.

  ls_event-eventid = cl_gui_column_tree=>eventid_item_double_click.
  APPEND ls_event TO lt_events.

  ls_event-eventid = cl_gui_column_tree=>eventid_header_click.
  APPEND ls_event TO lt_events.

  CALL METHOD gr_tree_data->set_registered_events
    EXPORTING
      events                    = lt_events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.

  IF sy-subrc <> 0.
    MESSAGE x000 WITH 'ERROR'.
  ENDIF.

*-- GR_EVENT_RECEIVER
  IF gr_tree_event_receiver IS INITIAL.
    CREATE OBJECT gr_tree_event_receiver.
  ENDIF.

* Handler Event
  SET HANDLER:
    gr_tree_event_receiver->handle_node_double_click
        FOR gr_tree_data,
    gr_tree_event_receiver->handle_item_double_click
        FOR gr_tree_data,
    gr_tree_event_receiver->handle_header_click
        FOR gr_tree_data.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FREE_DATA_TREE
*&---------------------------------------------------------------------*
FORM free_data_tree .

  CALL METHOD gr_tree_data->free.
  CLEAR gr_tree_data.

  CREATE OBJECT gr_tree_data
    EXPORTING
      parent                      = gr_data_container
      node_selection_mode         = 0
      item_selection              = abap_true
      no_html_header              = abap_false
      no_toolbar                  = space
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      illegal_node_selection_mode = 5
      failed                      = 6
      illegal_column_name         = 7.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADD_NODEKEY3
*&---------------------------------------------------------------------*
FORM add_nodekey3  USING   pv_relat_key TYPE lvc_nkey
                           pr_tree       TYPE REF TO cl_gui_alv_tree
                           p_code
                           p_root
                  CHANGING pv_node_key  TYPE lvc_nkey.

  DATA: lv_node_text TYPE lvc_value.

  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  DATA lv_key  TYPE lvc_nkey.
  DATA lv_key2 TYPE lvc_nkey.
  DATA ls_outtab TYPE zcos0110.

  DATA ls_value TYPE zcos0110.

  DATA ls_node_layout TYPE lvc_s_layn.

  LOOP AT  gt_outtab INTO ls_outtab
                 WHERE zcode       = p_code
                   AND subname = p_root.

    MOVE ls_outtab TO ls_value.

    CASE ls_outtab-type.

      WHEN 'B'.
        lv_node_text =  ls_outtab-kstar.

      WHEN 'S'.
        lv_node_text =  ls_outtab-shortname.
        CLEAR: ls_value-kstar, ls_value-ktext.

    ENDCASE.

    CALL METHOD pr_tree->add_node
      EXPORTING
        i_relat_node_key = pv_relat_key
        i_relationship   = cl_gui_column_tree=>relat_last_child
        i_node_text      = lv_node_text
        is_outtab_line   = ls_value
        is_node_layout   = ls_node_layout
        it_item_layout   = lt_item_layout
      IMPORTING
        e_new_node_key   = pv_node_key.

  ENDLOOP.

  PERFORM add_nodekey3     USING   pv_node_key
                                   gr_tree_data
                                   ls_outtab-zcode
                                   ls_outtab-setid
                          CHANGING lv_key.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM authority_check .

  DATA lv_type TYPE c.
  DATA lv_message TYPE bapi_msg.

  DATA: lt_0070  LIKE TABLE OF zcas0070,
        ls_0070  LIKE zcas0070,
        lv_class TYPE zcat0031-cd_class,
        lv_code  TYPE zcat0031-cd_code.

  CLEAR: r_prctr1, r_prctr1[],
         r_bukrs,  r_bukrs[],
         r_pspid,  r_pspid[].

  lv_class = 'CASUSR'.
  lv_code  = sy-uname.

  "__ SUPER USER ID 체크
  PERFORM call_f4_values(zcar9000) TABLES lt_0070
                                    USING lv_class lv_code ls_0070.
  IF lt_0070[] IS NOT INITIAL.
    EXIT.
  ELSE.
    lv_class = 'CASUCO'.
    lv_code  = sy-uname.

    "__ SUPER USER ID 체크
    PERFORM call_f4_values(zcar9000) TABLES lt_0070
                                      USING lv_class lv_code ls_0070.
    IF lt_0070[] IS NOT INITIAL.
      EXIT.
    ENDIF.
  ENDIF.

  IF so_bukrs[] IS INITIAL AND
     so_prctr[] IS INITIAL AND
     so_pspid[] IS INITIAL AND
     ( so_posid[] IS INITIAL AND pa_pdgr IS INITIAL ).

    MESSAGE s000 WITH TEXT-e12 DISPLAY LIKE 'E'.
    STOP.

  ENDIF.

  CALL FUNCTION 'ZCA_PRG_AUTH_CHECK'
    EXPORTING
      i_module     = 'CO'
      i_posidgr_co = pa_pdgr
    IMPORTING
      e_type       = lv_type
      e_message    = lv_message
    TABLES
      it_prctr_co  = so_prctr[]
      it_bukrs_co  = so_bukrs[]
      it_pspid_co  = so_pspid[]
      it_posid_co  = so_posid[].

  IF lv_type = 'E'.
    MESSAGE s000 WITH lv_message DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  SELECT * FROM zcot0320
    INTO TABLE @DATA(lt_zcot0320)
    WHERE bname = @sy-uname.

  LOOP AT lt_zcot0320 INTO DATA(ls_zcot0320).

    IF ls_zcot0320-prctr1 IS NOT INITIAL.

      MOVE: ls_zcot0320-prctr1 TO r_prctr1-low,
            'I'                TO r_prctr1-sign,
            'EQ'               TO r_prctr1-option.

      COLLECT r_prctr1.
      CLEAR   r_prctr1.

    ENDIF.

    IF ls_zcot0320-bukrs IS NOT INITIAL.

      MOVE: ls_zcot0320-bukrs TO r_bukrs-low,
            'I'                TO r_bukrs-sign,
            'EQ'               TO r_bukrs-option.

      COLLECT r_bukrs.
      CLEAR   r_bukrs.

    ENDIF.

    IF ls_zcot0320-pspid IS NOT INITIAL.

      MOVE: ls_zcot0320-pspid  TO r_pspid-low,
            'I'                TO r_pspid-sign,
            'EQ'               TO r_pspid-option.

      COLLECT r_pspid.
      CLEAR   r_pspid.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCRFIELDS_FUNCTXT
*&---------------------------------------------------------------------*
FORM scrfields_functxt .

  gs_funtxt-icon_id   = icon_information.
  gs_funtxt-quickinfo = 'Program Help'.

  sscrfields-functxt_01 = gs_funtxt.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCR_USER_COMMAND
*&---------------------------------------------------------------------*
FORM scr_user_command .

  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      PERFORM call_popup_help(zcar9000) USING sy-repid
                                              sy-dynnr
                                              sy-langu ''.

    WHEN OTHERS.

  ENDCASE.

ENDFORM.
