*&---------------------------------------------------------------------*
*& Include          ZCOR0090F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITAIL
*&---------------------------------------------------------------------*
FORM initail .

  gv_repid = sy-repid.
  GET PARAMETER ID 'CAC' FIELD pa_kokrs.

  IF pa_kokrs IS INITIAL.
    pa_kokrs = '1000'.
  ENDIF.

  SELECT SINGLE bezei INTO @pa_ktxt
    FROM tka01
   WHERE kokrs = @pa_kokrs.

  CASE sy-tcode.

    WHEN 'ZCOR0091'.
      gv_mode = 'S'.
      sy-title = TEXT-t05.
      pa_versn = 'P0'.
      pv_tvers = 'PZ'.

    WHEN 'ZCOR0092'.
      gv_mode = 'C'.
      sy-title = TEXT-t06.
      pa_versn = '000'.
      pv_tvers = 'R1'.

    WHEN  OTHERS.
      gv_mode = 'H'.
      pa_versn = 'PZ'.
      pv_tvers = 'B0'.
      pv_tvere = 'B1'.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SCREEN
*&---------------------------------------------------------------------*
FORM set_screen .

  LOOP AT SCREEN.

    IF screen-name = 'PA_KOKRS' OR
       screen-name = 'P_TGJAHR' OR
       screen-name = 'P_HGJAHR' OR
       screen-name = 'P_HSPERL' OR
       screen-name = 'P_HEPERL' OR
       screen-name = 'PA_CHK'   OR
       screen-name = 'P_HVERSN' OR
       screen-name = 'P_TSPERL' OR
       screen-name = 'P_TEPERL'.
      screen-input = 0.
    ENDIF.

    CASE gv_mode.

      WHEN 'S'.
        IF screen-group1 = 'MG1'.
          screen-active = 0.
        ENDIF.

      WHEN 'C'.
        IF screen-name = 'PA_VERSN'.
          screen-input = 0.
        ENDIF.

        IF screen-group1 = 'MG1'.
          screen-active = 0.
        ENDIF.

      WHEN 'H'.
        IF screen-name = 'PA_SPERL' OR
           screen-name = 'PA_EPERL'.
          screen-input = 0.
        ENDIF.

    ENDCASE.

    MODIFY SCREEN.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_B1
*&---------------------------------------------------------------------*
FORM check_b1.

  IF pa_sperl > pa_eperl.
    SET CURSOR FIELD 'PA_SPERL'.
    MESSAGE e018  WITH TEXT-e03.
  ENDIF.

  IF pa_sperl > 12 OR
     pa_eperl > 12.
    SET CURSOR FIELD 'PA_SPERL'.
    MESSAGE e023  WITH TEXT-e03.
  ENDIF.

  SELECT SINGLE vtext INTO @pa_vtxt
    FROM tkvs AS a
    LEFT JOIN tkvst AS b
      ON a~versi = b~versi
     AND b~spras = @sy-langu
   WHERE a~versi = @pa_versn.

  IF sy-subrc <> 0.
    SET CURSOR FIELD 'PA_VERSN'.
    MESSAGE e027  WITH TEXT-005.
  ENDIF.

  p_hgjahr = p_tgjahr = pa_gjahr.
  p_tsperl = pa_sperl.
  p_teperl = pa_eperl.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_B3
*&---------------------------------------------------------------------*
FORM check_b3 .

  SELECT SINGLE vtext INTO @p_hvtxt
  FROM tkvs AS a
  LEFT JOIN tkvst AS b
    ON a~versi = b~versi
   AND b~spras = @sy-langu
 WHERE a~versi = @p_hversn.

  IF sy-subrc <> 0.
    SET CURSOR FIELD 'P_HVERSN'.
    MESSAGE e027  WITH TEXT-005.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_B2
*&---------------------------------------------------------------------*
FORM check_b2 .


  CLEAR: r_verns, r_verns[].

  IF pv_tvers IS INITIAL AND
     pv_tvere IS INITIAL.
    MESSAGE e026 WITH TEXT-e01.
  ENDIF.

  IF pv_tvere < pv_tvers AND
     pv_tvere IS NOT INITIAL.
    SET CURSOR FIELD 'PV_TVERS'.
    MESSAGE e018  WITH TEXT-e01.
  ENDIF.

  SELECT SINGLE vtext INTO @DATA(lv_t1)
    FROM tkvs AS a
    LEFT JOIN tkvst AS b
      ON a~versi = b~versi
     AND b~spras = @sy-langu
   WHERE a~versi = @pv_tvers.

  IF sy-subrc <> 0.
    SET CURSOR FIELD 'PV_TVERS'.
    MESSAGE e027  WITH TEXT-005.
  ENDIF.

  IF gv_mode = 'H' AND pv_tvers NP 'B*'.
    SET CURSOR FIELD 'PV_TVERS'.
    MESSAGE e000  WITH TEXT-e06.

  ELSEIF ( gv_mode = 'S' OR gv_mode = 'C' ) AND
            pv_tvers CP 'B*'.

    SET CURSOR FIELD 'PV_TVERS'.
    MESSAGE e000  WITH TEXT-e07.

  ENDIF.

  IF pv_tvere IS NOT INITIAL.

    SELECT SINGLE vtext INTO @DATA(lv_t2)
      FROM tkvs AS a
      LEFT JOIN tkvst AS b
        ON a~versi = b~versi
       AND b~spras = @sy-langu
     WHERE a~versi = @pv_tvere.

    IF sy-subrc <> 0.
      SET CURSOR FIELD 'PV_TVERE'.
      MESSAGE e027  WITH TEXT-005.
    ENDIF.

    IF gv_mode = 'H' AND pv_tvere NP 'B*'.
      SET CURSOR FIELD 'PV_TVERE'.
      MESSAGE e000  WITH TEXT-e06.

    ELSEIF ( gv_mode = 'S' OR gv_mode = 'C' ) AND
              pv_tvere CP 'B*'.

      SET CURSOR FIELD 'PV_TVERE'.
      MESSAGE e000  WITH TEXT-e07.

    ENDIF.

  ENDIF.

  IF pv_tvere IS INITIAL.

    MOVE: 'I'      TO r_verns-sign,
          'EQ'     TO r_verns-option,
          pv_tvers TO r_verns-low.

    APPEND r_verns.

  ELSE.

    MOVE: 'I'      TO r_verns-sign,
          'BT'     TO r_verns-option,
          pv_tvers TO r_verns-low,
          pv_tvere TO r_verns-high.

    APPEND r_verns.

  ENDIF.

  SELECT * FROM tkvs
    INTO TABLE @gt_tkvs
   WHERE versi IN @r_verns.

  CASE gv_mode.

    WHEN 'S' OR 'C'.

      SELECT SINGLE versi INTO @DATA(lv_versn)
        FROM tkvs
        WHERE versi IN @r_verns
          AND versi LIKE 'B%'.

      IF sy-subrc = 0.
        SET CURSOR FIELD 'PV_TVERES'.
        MESSAGE e000  WITH TEXT-e07.
      ENDIF.

    WHEN 'H'.

      SELECT SINGLE versi INTO @lv_versn
        FROM tkvs
        WHERE versi IN @r_verns
          AND versi NOT LIKE 'B%'.

      IF sy-subrc = 0.
        SET CURSOR FIELD 'PV_TVERES'.
        MESSAGE e000  WITH TEXT-e06.
      ENDIF.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DATA_GET
*&---------------------------------------------------------------------*
FORM data_get .

  DATA lv_fieldname TYPE fieldname.
  DATA lv_fyear     TYPE n LENGTH 7.

  DATA lv_rfield    TYPE fieldname.
  DATA lv_diff.

  DATA otab TYPE abap_sortorder_tab .

  FIELD-SYMBOLS: <fs_s>     TYPE any,
                 <fs_r>     TYPE any,
                 <fs_d>     TYPE any,
                 <fs_d2>    TYPE any,
                 <fs_d3>    TYPE any,
                 <fs_d33>   TYPE any,
                 <fs_objnr> TYPE j_objnr,
                 <fs_kstar> TYPE kstar,
                 <fs_hsl>   TYPE any,
                 <fs_his>   TYPE any.

  CLEAR: <gt_table> .
  CLEAR: gt_cosp, gt_cosp[].

  SELECT SINGLE waers INTO @DATA(lv_waers)
    FROM tka01
   WHERE kokrs = @pa_kokrs.

  CASE gv_mode.

    WHEN 'C'.

      SELECT a~versn, a~objnr, a~kstar, b~ktext AS kstxt,
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

        FROM cosp AS a
        LEFT JOIN csku AS b
          ON a~kstar = b~kstar
         AND b~ktopl = @gc_ktopl
         AND b~spras = @sy-langu
       WHERE a~lednr = '00'
         AND a~gjahr = @pa_gjahr
         AND ( ( a~wrttp = '04' AND a~versn = @pa_versn ) OR "실적
               ( a~wrttp = '01' AND a~versn IN @r_verns ) )  "계획
         AND a~kstar IN ( SELECT DISTINCT saknr
                            FROM ska1
                           WHERE ktopl = @gc_ktopl
                             AND ( ktoks = 'PL' OR
                                   ktoks = 'SECC' ) )

        GROUP BY a~versn, a~objnr, a~kstar, b~ktext

        UNION ALL

      SELECT a~versn, a~objnr, a~kstar, b~ktext AS kstxt,
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

        FROM coss AS a
        LEFT JOIN csku AS b
          ON a~kstar = b~kstar
         AND b~ktopl = @gc_ktopl
         AND b~spras = @sy-langu
       WHERE a~lednr = '00'
         AND a~gjahr = @pa_gjahr
         AND a~wrttp = '04'             "실적
         AND a~versn = @pa_versn
         AND a~kstar IN ( SELECT DISTINCT saknr
                            FROM ska1
                           WHERE ktopl = @gc_ktopl
                             AND ( ktoks = 'PL' OR
                                   ktoks = 'SECC' ) )

        GROUP BY a~versn, a~objnr, a~kstar, b~ktext

         INTO TABLE @gt_cosp.

    WHEN OTHERS.

      SELECT a~versn, a~objnr, a~kstar, b~ktext AS kstxt,
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
        INTO TABLE @gt_cosp
        FROM cosp AS a
        LEFT JOIN csku AS b
          ON a~kstar = b~kstar
         AND b~ktopl = @gc_ktopl
         AND b~spras = @sy-langu
       WHERE a~lednr = '00'
         AND a~gjahr = @pa_gjahr
         AND a~wrttp = '01'                 "계획
         AND ( a~versn IN @r_verns OR
               a~versn = @pa_versn )
         AND a~kstar IN ( SELECT DISTINCT saknr
                            FROM ska1
                           WHERE ktopl = @gc_ktopl
                             AND ( ktoks = 'PL' OR
                                   ktoks = 'SECC' ) )

        GROUP BY a~versn, a~objnr, a~kstar, b~ktext.

  ENDCASE.




** >>>>>>>>>>>>>>>> modi by bsgsm_fcm   20210324

  SELECT a~kostl, b~ktext, a~prctr, c~ltext
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
     AND a~bukrs = @pa_bukrs.




  SELECT a~pspnr, a~posid, a~post1, a~prctr , c~ltext
     INTO TABLE @DATA(lt_prps)
    FROM prps AS a INNER JOIN  cepct AS c
     ON a~prctr = c~prctr
    WHERE a~pbukr = @pa_bukrs
      AND c~spras = @sy-langu
      AND c~kokrs = @pa_kokrs
      AND c~datbi = '99991231'.


* cbo 권한 추가로 아래 주석
*  SELECT A~KOSTL, B~KTEXT
*    INTO TABLE @DATA(LT_KOSTL)
*    FROM CSKS AS A
*    LEFT JOIN CSKT AS B
*      ON A~KOKRS = B~KOKRS
*     AND A~KOSTL = B~KOSTL
*     AND A~DATBI = B~DATBI
*     AND B~SPRAS = @SY-LANGU
*   WHERE A~KOKRS = @PA_KOKRS.
*
*  SELECT PSPNR, POSID, POST1 INTO TABLE @DATA(LT_PRPS)
*    FROM PRPS.

*<<<<<<<<<<<<<<<<<<<<<    20210324   bsgsm_fcm .........






  IF gv_mode = 'H'.

    SELECT rvers, robjnr, rkstar,
           SUM( hsl01 ) AS hsl01, SUM( hsl02 ) AS hsl02,
           SUM( hsl03 ) AS hsl03, SUM( hsl04 ) AS hsl04,
           SUM( hsl05 ) AS hsl05, SUM( hsl06 ) AS hsl06,
           SUM( hsl07 ) AS hsl07, SUM( hsl08 ) AS hsl08,
           SUM( hsl09 ) AS hsl09, SUM( hsl10 ) AS hsl10,
           SUM( hsl11 ) AS hsl11, SUM( hsl12 ) AS hsl12
      INTO TABLE @DATA(lt_zcot0040)
      FROM zcot0040
     WHERE rldnr = '00'
       AND rrcty = '1'
       AND rvers = @p_hversn
       AND ryear = @p_hgjahr
     GROUP BY rvers, robjnr, rkstar.

  ENDIF.



*add bsgsm_fcm  2021.01.04.
**  선택 회사별  objnr..

  CLEAR :  gv_super, gv_bukrs_auth.

  PERFORM set_ranges_objnr .

  IF ( r_objnr[] IS NOT INITIAL ) OR
     (   gv_bukrs_auth IS NOT INITIAL ).


    LOOP AT gt_cosp ASSIGNING FIELD-SYMBOL(<ls_cosp>) WHERE objnr IN r_objnr.


* end by bsgsm_fcm.....

*  LOOP AT GT_COSP ASSIGNING FIELD-SYMBOL(<LS_COSP>).

      MOVE-CORRESPONDING <ls_cosp> TO <gs_line>.

      CASE <ls_cosp>-objnr(2).

        WHEN 'KS'.

          ASSIGN COMPONENT 'KOSTL' OF STRUCTURE <gs_line> TO <fs_d>.
          MOVE <ls_cosp>-objnr+6 TO <fs_d>.

          READ TABLE lt_kostl ASSIGNING FIELD-SYMBOL(<ls_kostl>)
                              WITH KEY kostl = <fs_d>.

          IF sy-subrc = 0.

            ASSIGN COMPONENT 'KTEXT'
                 OF STRUCTURE <gs_line> TO <fs_d2>.

            MOVE <ls_kostl>-ktext TO <fs_d2>.

**          ADD BSGSM_FCM   20210324
            ASSIGN COMPONENT 'PRCTR'
                OF STRUCTURE <gs_line> TO <fs_d3>.
            ASSIGN COMPONENT 'LTEXT'
        OF STRUCTURE <gs_line> TO <fs_d33>.

            MOVE <ls_kostl>-prctr TO <fs_d3>.
            MOVE <ls_kostl>-ltext TO <fs_d33>.
**          END BY BSGSM_FCM



          ENDIF.

        WHEN 'PR'.
          ASSIGN COMPONENT 'POSID' OF STRUCTURE <gs_line> TO <fs_d>.
          ASSIGN COMPONENT 'POST1' OF STRUCTURE <gs_line> TO <fs_d2>.


          READ TABLE lt_prps ASSIGNING FIELD-SYMBOL(<ls_prps>)
               WITH KEY pspnr = <ls_cosp>-objnr+2.

          IF sy-subrc EQ 0.

            MOVE: <ls_prps>-posid TO <fs_d>,
                  <ls_prps>-post1 TO <fs_d2>.

**               ADD BSGSM_FCM   20210324
            ASSIGN COMPONENT 'PRCTR' OF STRUCTURE <gs_line> TO <fs_d3>.
            ASSIGN COMPONENT 'LTEXT' OF STRUCTURE <gs_line> TO <fs_d33>.

            MOVE :
                  <ls_prps>-prctr TO <fs_d3>,
                  <ls_prps>-ltext TO <fs_d33>.


          ENDIF.



      ENDCASE.

      CASE <ls_cosp>-versn.

        WHEN pa_versn.
          lv_rfield = 'SDATA'.

        WHEN OTHERS.
          lv_rfield = 'T_' && <ls_cosp>-versn.

      ENDCASE.

      ASSIGN COMPONENT lv_rfield OF STRUCTURE <gs_line>
            TO <fs_r>.

      LOOP AT gt_month.

*-- COSP
        lv_fieldname = 'WKG0' && gt_month-v1.

        ASSIGN COMPONENT lv_fieldname OF STRUCTURE <ls_cosp>
              TO <fs_s>.

        <fs_r> = <fs_r> + <fs_s>.

      ENDLOOP.

      ASSIGN COMPONENT 'WAERS' OF STRUCTURE <gs_line> TO <fs_d>.
      MOVE lv_waers TO <fs_d>.

      COLLECT <gs_line> INTO <gt_table>.
      CLEAR   <gs_line>.

    ENDLOOP.

    LOOP AT <gt_table> ASSIGNING <gs_line>.

      CLEAR lv_diff.

      ASSIGN COMPONENT 'SDATA' OF STRUCTURE <gs_line>
         TO <fs_s>.

      ASSIGN COMPONENT 'HDATA' OF STRUCTURE <gs_line>
            TO <fs_his>.

      ASSIGN COMPONENT 'OBJNR' OF STRUCTURE <gs_line>
            TO <fs_objnr>.

      ASSIGN COMPONENT 'KSTAR' OF STRUCTURE <gs_line>
            TO <fs_kstar>.

      IF <fs_s> IS INITIAL.
        DELETE <gt_table> INDEX sy-tabix.
        CONTINUE.
      ENDIF.

*-- History
      READ TABLE lt_zcot0040 ASSIGNING FIELD-SYMBOL(<ls_zcot0040>)
                            WITH KEY robjnr = <fs_objnr>
                                     rkstar = <fs_kstar>.

      IF sy-subrc = 0.

        LOOP AT gt_month.

          lv_fieldname = 'HSL' && gt_month-v1.

          ASSIGN COMPONENT lv_fieldname OF STRUCTURE <ls_zcot0040>
                TO <fs_hsl>.

          <fs_his> = <fs_his> + <fs_hsl>.

        ENDLOOP.

      ENDIF.

*-- Value check
      LOOP AT gt_tkvs.

        lv_fieldname = 'T_' && gt_tkvs-versi.

        ASSIGN COMPONENT lv_fieldname OF STRUCTURE <gs_line>
           TO <fs_r>.

        IF gv_mode = 'H'.

          IF NOT ( <fs_s> = <fs_r> AND
                   <fs_s> = <fs_his> ).
            lv_diff = abap_true.
          ENDIF.

        ELSE.

          IF NOT (  <fs_s> = <fs_r> ).
            lv_diff = abap_true.
          ENDIF.

        ENDIF.

      ENDLOOP.

      ASSIGN COMPONENT 'ICON' OF STRUCTURE <gs_line>
        TO <fs_d2>.

      IF lv_diff = abap_true.
        <fs_d2> = '@4A@'.  "Copy

      ELSE.
        <fs_d2> = '@00@'.
      ENDIF.

    ENDLOOP.

    otab = VALUE #( ( name = 'KOSTL'  )
                    ( name = 'POSID'  )
                    ( name = 'KSTAR'  ) ).

    SORT <gt_table> BY (otab).


  ELSE.


  ENDIF.


  IF gv_error IS NOT INITIAL.
    MESSAGE s000(zco01) WITH gv_error DISPLAY LIKE gc_e.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SALV_CALL
*&---------------------------------------------------------------------*
FORM salv_call .

  TRY.
      cl_salv_table=>factory(
                IMPORTING
                  r_salv_table = go_alv
                CHANGING
                  t_table      = <gt_table> ).
    CATCH cx_salv_msg.
  ENDTRY.

  PERFORM set_pf_status.
  PERFORM set_layout.

  PERFORM set_top_of_page.

  PERFORM set_event.
  PERFORM set_table_settings.

  go_alv->display( ).




ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_PF_STATUS
*&---------------------------------------------------------------------*
FORM set_pf_status .

  DATA: lt_func_list TYPE salv_t_ui_func,
        ls_func_list TYPE salv_s_ui_func.

  DATA: l_title TYPE lvc_title,
        l_lines TYPE c LENGTH 100,
        l_count TYPE i.

  DATA lv_text TYPE char100.

  go_alv->set_screen_status(
    pfstatus      =  'STANDARD'
    report        =  gv_repid
    set_functions = go_alv->c_functions_all ).

  l_count = lines( <gt_table> ).

  CASE gv_mode.
    WHEN 'S'.
      lv_text = TEXT-t05.
    WHEN 'C'.
      lv_text = TEXT-t06.
    WHEN OTHERS.
      lv_text = TEXT-t00.
  ENDCASE.

  WRITE l_count TO l_lines.
  CONDENSE l_lines.

  CONCATENATE lv_text '(' 'Selected entries :' l_lines ')'
        INTO l_title SEPARATED BY space.

* set output control : ZEBRA
  go_dspset = go_alv->get_display_settings( ).
  go_dspset->set_striped_pattern( abap_true ).
  go_dspset->set_list_header( l_title ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LAYOUT
*&---------------------------------------------------------------------*
FORM set_layout .

  gs_salv_layout-repid = sy-repid.
  gs_salv_layout-default = 'X'.
  gs_salv_layout-layout = '/STANDARD'.
  gs_key-report = gv_repid.

  go_layout = go_alv->get_layout( ).
  go_layout->set_key( gs_key ).

  gs_salv_layout-restrict = if_salv_c_layout=>restrict_none.
  go_layout->set_default( gs_salv_layout-default ).
  go_layout->set_save_restriction( gs_salv_layout-restrict ).
  go_layout->set_initial_layout( gs_salv_layout-layout ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_EVENT
*&---------------------------------------------------------------------*
FORM set_event .

*-- EVENT
  DATA: lr_events TYPE REF TO cl_salv_events_table.

  go_functions = go_alv->get_functions( ).
  go_functions->set_all( ).

  lr_events = go_alv->get_event( ).

  CREATE OBJECT g_event_receiver.
  SET HANDLER g_event_receiver->on_user_command FOR lr_events.
  SET HANDLER g_event_receiver->top_of_page     FOR lr_events.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TABLE_SETTINGS
*&---------------------------------------------------------------------*
FORM set_table_settings .

  DATA: lr_columns  TYPE REF TO cl_salv_columns_table.
  DATA: lo_aggrs    TYPE REF TO cl_salv_aggregations.

*-- set column
  TRY.
      lr_columns = go_alv->get_columns( ).
      lo_aggrs   = go_alv->get_aggregations( ).
*      LR_COLUMNS->SET_OPTIMIZE( ABAP_TRUE ).
      lr_columns->set_cell_type_column( 'CELLTYPE' ).
*      LR_COLUMNS->SET_KEY_FIXATION( ABAP_TRUE ).
    CATCH cx_salv_data_error.
  ENDTRY.

  PERFORM set_columns_technical USING lr_columns
                                      lo_aggrs.

  TRY.
      lr_columns->set_key_fixation( abap_true ).
    CATCH cx_salv_data_error.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_COLUMNS_TECHNICAL
*&---------------------------------------------------------------------*
FORM set_columns_technical USING ir_columns TYPE REF TO
                                            cl_salv_columns_table
                                  ir_agg TYPE REF TO
                                            cl_salv_aggregations.

  DATA lr_column  TYPE REF TO cl_salv_column_table.

  DATA: lr_selections TYPE REF TO cl_salv_selections,
        lt_column     TYPE salv_t_column.

  DATA lv_ltext TYPE scrtext_l.
  DATA lv_mtext TYPE scrtext_m.
  DATA lv_stext TYPE scrtext_s.

  FIELD-SYMBOLS:
    <column_ref> LIKE LINE OF gt_column_ref.

  gt_column_ref = ir_columns->get( ).

  TRY.
      LOOP AT gt_column_ref
        ASSIGNING <column_ref>.
        gr_column ?= ir_columns->get_column( <column_ref>-columnname ).

        PERFORM set_column_text USING    <column_ref>-columnname
                                CHANGING gv_column_text.

        IF gv_column_text IS NOT INITIAL.

          gv_scrtext_s = gv_column_text.
          gv_scrtext_m = gv_column_text.
          gv_scrtext_l = gv_column_text.

          gr_column->set_short_text( gv_scrtext_s ).
          gr_column->set_medium_text( gv_scrtext_m ).
          gr_column->set_long_text( gv_scrtext_l ).

        ENDIF.

        IF ( <column_ref>-columnname CP 'T_*' OR
            <column_ref>-columnname = 'HDATA'  OR
            <column_ref>-columnname = 'SDATA' ).

          CALL METHOD ir_agg->add_aggregation
            EXPORTING
              columnname  = <column_ref>-columnname
              aggregation = if_salv_c_aggregation=>total.

        ENDIF.

      ENDLOOP.

    CATCH cx_salv_not_found.
    CATCH cx_salv_data_error.
    CATCH cx_salv_existing.

  ENDTRY.

  CASE gv_mode.

    WHEN 'S' OR 'C'.

      TRY.
          lr_column ?= ir_columns->get_column( 'HDATA' ).
          lr_column->set_visible( if_salv_c_bool_sap=>false ).
        CATCH cx_salv_not_found.
      ENDTRY.

  ENDCASE.

  TRY.
      lr_column ?= ir_columns->get_column( 'OBJNR' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found.
  ENDTRY.

*-- SELECT FIELD 추가
  lr_selections = go_alv->get_selections( ).
  lr_selections->set_selection_mode( if_salv_c_selection_mode=>cell ).

  ir_agg->set_aggregation_before_items( ).

ENDFORM. " SET_COLUMNS_TECHNICAL
*&---------------------------------------------------------------------*
*& Form HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
FORM handle_user_command USING p_ucomm TYPE salv_de_function.

  DATA l_dummy TYPE c LENGTH 100.

  CASE p_ucomm.

    WHEN 'CREATE'.

      PERFORM error_check.
      CHECK gv_exit IS INITIAL.
      PERFORM popup_to_confirm USING TEXT-pt1
                                     TEXT-qt1.
      CHECK gv_answer EQ '1'.
      PERFORM create_execute.

    WHEN '&HELP'.
      PERFORM call_popup_help(zcar9000)
               USING sy-repid sy-dynnr sy-langu ''.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
FORM popup_to_confirm USING pv_title
                            pv_quest.
  "-- call popup
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar       = pv_title
      text_question  = pv_quest
    IMPORTING
      answer         = gv_answer
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_COLUMN_TEXT
*&---------------------------------------------------------------------*
FORM set_column_text USING p_columnname
                      CHANGING p_column_text.

  DATA l_field TYPE lvc_cfname VALUE 'WAERS'.

  TRY.

      CLEAR p_column_text.

      CASE p_columnname.

        WHEN 'ICON'.
          p_column_text = TEXT-c01.

          gr_column->set_icon( if_salv_c_bool_sap=>true ).
          gr_column->set_key( if_salv_c_bool_sap=>true ).
          gr_column->set_output_length( value = 4 ).
*          GR_COLUMN->SET_KEY_FIXATION( IF_SALV_C_BOOL_SAP=>TRUE ).

        WHEN 'KOSTL'.
          p_column_text = TEXT-c02.
          gr_column->set_key( if_salv_c_bool_sap=>true ).
          gr_column->set_output_length( value = 10 ).

        WHEN 'KTEXT'.
          p_column_text = TEXT-c03.
          gr_column->set_key( if_salv_c_bool_sap=>true ).

        WHEN 'POSID'.
          p_column_text = TEXT-c04.
          gr_column->set_key( if_salv_c_bool_sap=>true ).
          gr_column->set_output_length( value = 10 ).

        WHEN 'POST1'.
          p_column_text = TEXT-c05.
          gr_column->set_key( if_salv_c_bool_sap=>true ).
          gr_column->set_output_length( value = 20 ).

        WHEN 'KSTAR'.
          p_column_text = TEXT-c06.
          gr_column->set_key( if_salv_c_bool_sap=>true ).

        WHEN 'KSTXT'.
          p_column_text = TEXT-c07.
          gr_column->set_key( if_salv_c_bool_sap=>true ).

        WHEN 'WAERS'.
          p_column_text = TEXT-c09.

        WHEN 'SDATA'.
          p_column_text = TEXT-t01.

          gr_column->set_currency_column( l_field ).

          _set_color '3' '0' '0'.
          gr_column->set_color( gs_color ).
          gr_column->set_output_length( value = 18 ).

        WHEN 'HDATA'.
          p_column_text = TEXT-t03.

          gr_column->set_currency_column( l_field ).

          _set_color '6' '0' '0'.
          gr_column->set_color( gs_color ).
          gr_column->set_output_length( value = 18 ).

        WHEN 'LOG'.
          p_column_text = TEXT-c08.
          gr_column->set_output_length( value = 50 ).

      ENDCASE.

      IF p_columnname CP 'T_*'.

        p_column_text = TEXT-t02 && '(' && p_columnname+2(3) &&
                        ')'.

        gr_column->set_currency_column( l_field ).

        _set_color '5' '0' '0'.
        gr_column->set_color( gs_color ).
        gr_column->set_output_length( value = 18 ).

      ENDIF.

    CATCH cx_salv_not_found.
    CATCH cx_salv_data_error.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_EXECUTE
*&---------------------------------------------------------------------*
FORM create_execute .

  DATA lv_rfield    TYPE fieldname.
  DATA lv_last.

  FIELD-SYMBOLS: <fs_message> TYPE any,
                 <fs_icon>    TYPE any,
                 <fs_sour>    TYPE any,
                 <fs_recv>    TYPE any,
                 <fs_hist>    TYPE any.

  DATA lv_error.

  LOOP AT gt_rows INTO gs_row.

    READ TABLE <gt_table> ASSIGNING <gs_line> INDEX gs_row.

    IF sy-subrc = 0.

      ASSIGN COMPONENT 'LOG' OF STRUCTURE <gs_line>
         TO <fs_message>.

      ASSIGN COMPONENT 'ICON' OF STRUCTURE <gs_line>
         TO <fs_icon>.

      IF <fs_icon> = '@4A@'.      "복사가능 한것만

        ASSIGN COMPONENT 'SDATA' OF STRUCTURE <gs_line>
           TO <fs_sour>.

        LOOP AT gt_tkvs.

          CLEAR lv_last.

          AT LAST.
            lv_last = abap_true.
          ENDAT.

          PERFORM copy_version USING    gt_tkvs-versi
                                        lv_last
                               CHANGING <fs_message>
                                        lv_error.
          IF lv_error IS NOT INITIAL.
            EXIT.
          ENDIF.

          lv_rfield = 'T_' && gt_tkvs-versi.
          ASSIGN COMPONENT lv_rfield OF STRUCTURE <gs_line>
             TO <fs_recv>.

          MOVE <fs_sour> TO <fs_recv>.

          IF lv_last = abap_true.

            MOVE: '@00@'   TO <fs_icon>,
                  TEXT-s01 TO <fs_message>.

            IF gv_mode = 'H'.

              ASSIGN COMPONENT 'HDATA' OF STRUCTURE <gs_line>
                 TO <fs_hist>.

              MOVE <fs_sour> TO <fs_hist>.

            ENDIF.

          ENDIF.

        ENDLOOP.

      ELSE.
        <fs_message> = TEXT-s03.

      ENDIF.

    ENDIF.

  ENDLOOP.

  MESSAGE s000 WITH TEXT-s02 .

  CALL METHOD go_alv->refresh
    EXPORTING
      refresh_mode = if_salv_c_refresh=>full.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ERROR_CHECK
*&---------------------------------------------------------------------*
FORM error_check .

  DATA lr_selections TYPE REF TO cl_salv_selections.

  FIELD-SYMBOLS <fs_icon> TYPE any.

  CLEAR: gt_rows, gv_exit.

  lr_selections = go_alv->get_selections( ).
  gt_rows = lr_selections->get_selected_rows( ).

  IF gt_rows IS INITIAL.
    MESSAGE s015 DISPLAY LIKE 'E'.
    gv_exit = abap_true.
    EXIT.
  ENDIF.

*  LOOP AT GT_ROWS INTO GS_ROW.
*
*    READ TABLE <GT_TABLE> ASSIGNING <GS_LINE> INDEX GS_ROW.
*
*    IF SY-SUBRC = 0.
*
*      ASSIGN COMPONENT 'ICON' OF STRUCTURE <GS_LINE>
*         TO  <FS_ICON>.
*
*      IF <FS_ICON> = '@00@'.
*        MESSAGE S000   WITH TEXT-E05 DISPLAY LIKE 'E'.
*        GV_EXIT = ABAP_TRUE.
*        EXIT.
*      ENDIF.
*    ENDIF.
*
*  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM set_top_of_page .

  DATA: lr_content TYPE REF TO cl_salv_form_element.
  PERFORM built_header CHANGING lr_content.
  go_alv->set_top_of_list( lr_content ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_TABLE
*&---------------------------------------------------------------------*
FORM make_table .

  DATA lv_fieldname TYPE fieldname.
  DATA lv_title     TYPE lvc_txtcol.
  DATA lv_month     TYPE n LENGTH 2.

  CLEAR gt_fieldcat[].

  CLEAR: gt_month, gt_month[].

  lv_month = pa_sperl+1(2).

  DO .
    MOVE lv_month TO gt_month-v1.
    APPEND gt_month.

    ADD 1 TO lv_month.
    IF lv_month > pa_eperl+1(2).
      EXIT.
    ENDIF.
  ENDDO.

  SELECT * FROM tkvs
    INTO TABLE @gt_tkvs
   WHERE versi IN @r_verns.

  IF <gt_table> IS ASSIGNED.
    UNASSIGN <gt_table>.
    UNASSIGN <gs_line>.
    CLEAR gr_data.
  ENDIF.

  PERFORM fill_field_category USING :

        'S' 'FIELDNAME'   'ICON',
        ' ' 'OUTPUTLEN'   '8',
        ' ' 'ICON'        'X',
        ' ' 'REF_TABLE'   'ICON',
        ' ' 'REF_FIELD'   'ID',
        'E' 'COLTEXT'     TEXT-c01,

        'S' 'FIELDNAME'   'OBJNR',
        ' ' 'OUTPUTLEN'   '22',
        ' ' 'REF_TABLE'   'COSP',
        ' ' 'REF_FIELD'   'OBJNR',
        'E' 'COLTEXT'     TEXT-c10,

*ADD BSGSM_FCM

        'S' 'FIELDNAME'   'PRCTR',
        ' ' 'OUTPUTLEN'   '10',
*        ' ' 'NO_OUT'       'X',
        ' ' 'REF_TABLE'   'CEPCT',
        ' ' 'REF_FIELD'   'PRCTR',
        'E' 'COLTEXT'     TEXT-c11,

        'S' 'FIELDNAME'   'LTEXT',
        ' ' 'OUTPUTLEN'   '10',
*        ' ' 'NO_OUT'       'X',
        ' ' 'REF_TABLE'   'CEPCT',
        ' ' 'REF_FIELD'   'LTEXT',
        'E' 'COLTEXT'     TEXT-c12,

*END BSGSM_FCM   손익센터 필드 추가



        'S' 'FIELDNAME'   'KOSTL',
        ' ' 'OUTPUTLEN'   '10',
        ' ' 'REF_TABLE'   'CSKS',
        ' ' 'REF_FIELD'   'KOSTL',
        'E' 'COLTEXT'     TEXT-c02,

        'S' 'FIELDNAME'   'KTEXT',
        ' ' 'OUTPUTLEN'   '20',
        ' ' 'REF_TABLE'   'CSKT',
        ' ' 'REF_FIELD'   'KTEXT',
        'E' 'COLTEXT'     TEXT-c03,

        'S' 'FIELDNAME'   'POSID',
        ' ' 'OUTPUTLEN'   '25',
        ' ' 'REF_TABLE'   'PRPS',
        ' ' 'REF_FIELD'   'POSID',
        'E' 'COLTEXT'     TEXT-c04,

        'S' 'FIELDNAME'   'POST1',
        ' ' 'OUTPUTLEN'   '30',
        ' ' 'REF_TABLE'   'PRPS',
        ' ' 'REF_FIELD'   'POST1',
        'E' 'COLTEXT'     TEXT-c05,

        'S' 'FIELDNAME'   'KSTAR',
        ' ' 'OUTPUTLEN'   '10',
        ' ' 'REF_TABLE'   'CSKA',
        ' ' 'REF_FIELD'   'KSTAR',
        'E' 'COLTEXT'     TEXT-c06,

        'S' 'FIELDNAME'   'KSTXT',
        ' ' 'OUTPUTLEN'   '20',
        ' ' 'REF_TABLE'   'CSKU',
        ' ' 'REF_FIELD'   'KTEXT',
        'E' 'COLTEXT'     TEXT-c07,

        'S' 'FIELDNAME'   'WAERS',
        ' ' 'OUTPUTLEN'   '4',
        ' ' 'REF_TABLE'   'TKA01',
        ' ' 'REF_FIELD'   'WAERS',
        'E' 'COLTEXT'     TEXT-c09,

         'S' 'FIELDNAME'   'SDATA',
         ''  'OUTPUTLEN'    '23',
         ' ' 'CFIELDNAME'  'WAERS',
         ' ' 'REF_TABLE'   'ZCOT0040',
         ' ' 'REF_FIELD'   'HSLVT',
         'E' 'COLTEXT'     TEXT-t01.

  LOOP AT gt_tkvs.

    lv_fieldname = 'T_' && gt_tkvs-versi.
    lv_title     = TEXT-t02 && '(' && gt_tkvs-versi && ')'.

    PERFORM fill_field_category USING :
          'S' 'FIELDNAME'   lv_fieldname,
          ''  'OUTPUTLEN'    '23',
          ' ' 'CFIELDNAME'  'WAERS',
          ' ' 'REF_TABLE'   'ZCOT0040',
          ' ' 'REF_FIELD'   'HSLVT',
          'E' 'COLTEXT'     lv_title.

  ENDLOOP.

  PERFORM fill_field_category USING :

         'S' 'FIELDNAME'   'HDATA',
         ''  'OUTPUTLEN'    '23',
         ' ' 'CFIELDNAME'  'WAERS',
         ' ' 'REF_TABLE'   'ZCOT0040',
         ' ' 'REF_FIELD'   'HSLVT',
         'E' 'COLTEXT'     TEXT-t03,

        'S' 'FIELDNAME'   'LOG',
        ''  'OUTPUTLEN'   '80',
        'E' 'COLTEXT'     TEXT-c08.

  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = gt_fieldcat
    IMPORTING
      ep_table        = gr_data.

  ASSIGN  gr_data->* TO <gt_table>.
  CREATE DATA gr_data LIKE LINE OF <gt_table>.
  ASSIGN  gr_data->* TO <gs_line>.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_FIELD_CATEGORY
*&---------------------------------------------------------------------*
FORM fill_field_category  USING pv_gub pv_fname pv_con.

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

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILT_HEADER
*&---------------------------------------------------------------------*
FORM built_header  CHANGING cr_content TYPE REF TO cl_salv_form_element.

  DATA: lr_grid   TYPE REF TO cl_salv_form_layout_grid,
        lr_grid_1 TYPE REF TO cl_salv_form_layout_grid,
        lr_label  TYPE REF TO cl_salv_form_label,
        lr_text   TYPE REF TO cl_salv_form_text,
        lv_text   TYPE string,
        l_total   TYPE string,
        l_sucess  TYPE string,
        l_error   TYPE string.

  CREATE OBJECT lr_grid.

  lr_grid_1 = lr_grid->create_grid(
                row    = 4
                column = 1 ).

  lr_label = lr_grid_1->create_label(
    row     = 1
    column  = 1
    text    = TEXT-t04
    tooltip = TEXT-t04 ).

  lr_text = lr_grid_1->create_text(
    row     = 1
    column  = 2
    text    = pa_gjahr
    tooltip = pa_gjahr ).

  lr_label->set_label_for( lr_text ).

  lr_label = lr_grid_1->create_label(
    row    = 2
    column = 1
    text    = TEXT-t01
    tooltip = TEXT-t01 ).

  SELECT SINGLE vtext INTO @DATA(lv_t1)
    FROM tkvs AS a
    LEFT JOIN tkvst AS b
      ON a~versi = b~versi
     AND b~spras = @sy-langu
   WHERE a~versi = @pa_versn.

  CONCATENATE pa_versn '(' lv_t1 ')'
        INTO lv_text SEPARATED BY space.

  lr_text = lr_grid_1->create_text(
    row    = 2
    column = 2
    text    = lv_text
    tooltip = lv_text ).

  lr_label->set_label_for( lr_text ).

  lr_label = lr_grid_1->create_label(
    row    = 3
    column = 1
    text    = TEXT-t02
    tooltip = TEXT-t02 ).

  IF pv_tvere IS NOT INITIAL.

    SELECT SINGLE vtext INTO @DATA(lv_t2)
      FROM tkvs AS a
      LEFT JOIN tkvst AS b
        ON a~versi = b~versi
       AND b~spras = @sy-langu
     WHERE a~versi = @pv_tvers.

    SELECT SINGLE vtext INTO @DATA(lv_t3)
      FROM tkvs AS a
      LEFT JOIN tkvst AS b
        ON a~versi = b~versi
       AND b~spras = @sy-langu
     WHERE a~versi = @pv_tvere.

    CONCATENATE pv_tvers '(' lv_t2 ')'
            '~' pv_tvere '(' lv_t3 ')'
           INTO lv_text SEPARATED BY space.

  ELSE.

    SELECT SINGLE vtext INTO @lv_t2
      FROM tkvs AS a
      LEFT JOIN tkvst AS b
        ON a~versi = b~versi
       AND b~spras = @sy-langu
     WHERE a~versi = @pv_tvers.

    CONCATENATE pv_tvers '(' lv_t2 ')'
          INTO lv_text SEPARATED BY space.

  ENDIF.

  lr_text = lr_grid_1->create_text(
    row    = 3
    column = 2
    text    = lv_text
    tooltip = lv_text ).

  lr_label->set_label_for( lr_text ).

  IF gv_mode = 'H'.

    lr_label = lr_grid_1->create_label(
      row    = 4
      column = 1
      text    = TEXT-t03
      tooltip = TEXT-t03 ).

    SELECT SINGLE vtext INTO @DATA(lv_t4)
      FROM tkvs AS a
      LEFT JOIN tkvst AS b
        ON a~versi = b~versi
       AND b~spras = @sy-langu
     WHERE a~versi = @p_hversn.

    CONCATENATE p_hversn '(' lv_t4 ')'
          INTO lv_text SEPARATED BY space.

    lr_text = lr_grid_1->create_text(
      row    = 4
      column = 2
      text    = lv_text
      tooltip = lv_text ).

  ENDIF.

  cr_content = lr_grid.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form COPY_VERSION
*&---------------------------------------------------------------------*
FORM copy_version  USING    p_versn
                            p_last
                   CHANGING p_message
                            p_error.

  DATA lv_message TYPE string.

  DATA ls_headerinfo LIKE bapiplnhdr.
  DATA ls_return     TYPE bapiret2.

  DATA lv_fname TYPE fieldname.

  DATA lv_fieldname TYPE fieldname.

  DATA : lt_indexstructure LIKE bapiacpstru OCCURS 0 WITH HEADER LINE,
         lt_coobject       LIKE bapipcpobj  OCCURS 0 WITH HEADER LINE,
         lt_pervalue       LIKE bapipcpval  OCCURS 0 WITH HEADER LINE.

  DATA lt_zcot0040 TYPE TABLE OF zcot0040 WITH HEADER LINE.

  FIELD-SYMBOLS: <fs_kostl> TYPE kostl,
                 <fs_posid> TYPE ps_posid,
                 <fs_objnr> TYPE j_objnr,
                 <fs_kstar> TYPE kstar,
                 <fs_waers> TYPE waers,
                 <fs_sour>  TYPE any,
                 <fs_recv>  TYPE any,
                 <fs_hist>  TYPE any.

  DATA lv_belnr TYPE belnr_d.
  DATA lv_seq   TYPE zerseq.

  CLEAR: gt_return, gt_return[].
  CLEAR p_message.
  CLEAR p_error.

  IF p_last = abap_true.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZCO_DOCNR'
        toyear                  = pa_gjahr
      IMPORTING
        number                  = lv_belnr
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    IF sy-subrc <> 0.
      MESSAGE s001 WITH TEXT-e04 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

  ENDIF.

*-- Header Data
  ls_headerinfo-co_area     = pa_kokrs.     "관리 회계영역
  ls_headerinfo-fisc_year   = pa_gjahr.     "회계연도
  ls_headerinfo-period_from = pa_sperl.      "기간 시작
  ls_headerinfo-period_to   = pa_eperl.      "기간 종료
  ls_headerinfo-version     = p_versn.       "버전

*-- 전표 헤더 텍스트
  ls_headerinfo-plan_currtype = 'C'. "통화

*-- CO-계획: 액티비티투입 & 주요지표 계획 BAPIs
  lt_indexstructure-object_index = 1.
  lt_indexstructure-value_index  = 1.
  APPEND lt_indexstructure.

*-- CO 계획: 1차 원가 BAPI에 대한 오브젝트
  lt_coobject-object_index  = 1.

  ASSIGN COMPONENT 'KOSTL' OF STRUCTURE <gs_line> TO <fs_kostl>.
  ASSIGN COMPONENT 'POSID' OF STRUCTURE <gs_line> TO <fs_posid>.
  ASSIGN COMPONENT 'OBJNR' OF STRUCTURE <gs_line> TO <fs_objnr>.
  ASSIGN COMPONENT 'KSTAR' OF STRUCTURE <gs_line> TO <fs_kstar>.
  ASSIGN COMPONENT 'WAERS' OF STRUCTURE <gs_line> TO <fs_waers>.

  IF <fs_kostl> IS NOT INITIAL.
    lt_coobject-costcenter    = <fs_kostl>.
  ELSE.
    lt_coobject-wbs_element   = <fs_posid>.
  ENDIF.

  APPEND lt_coobject.

  lt_pervalue-value_index  = 1.
  lt_pervalue-cost_elem    = <fs_kstar>.
  lt_pervalue-trans_curr   = <fs_waers>.

  READ TABLE gt_cosp ASSIGNING FIELD-SYMBOL(<ls_cosp>)
                 WITH KEY versn = pa_versn
                          objnr = <fs_objnr>
                          kstar = <fs_kstar>.

  LOOP AT gt_month.

    lv_fname = 'LT_PERVALUE-FIX_VAL_PER' && gt_month-v1.

    ASSIGN (lv_fname) TO <fs_recv>.

    lv_fname = 'WKG0' && gt_month-v1.

    IF <ls_cosp> IS ASSIGNED.

      ASSIGN COMPONENT lv_fname OF STRUCTURE <ls_cosp>
            TO <fs_sour>.

      IF <fs_waers> = 'KRW'.
        <fs_recv> = <fs_sour>  * 100.
      ELSE.
        <fs_recv> = <fs_sour>.
      ENDIF.

    ENDIF.

    IF gv_mode = 'H' AND p_last = abap_true.

*-- CBO 저장 데이터
      MOVE: '00'           TO lt_zcot0040-rldnr,
            '1'            TO lt_zcot0040-rrcty,
            '1'            TO lt_zcot0040-rseq,
            p_hversn       TO lt_zcot0040-rvers,
            pa_gjahr       TO lt_zcot0040-ryear,
            <fs_objnr>     TO lt_zcot0040-robjnr,
            lv_belnr       TO lt_zcot0040-rdocnr,
            'BT01'         TO lt_zcot0040-budtype_9,
            pa_kokrs       TO lt_zcot0040-rkokrs,
            sy-uzeit       TO lt_zcot0040-cputm,
            sy-datum       TO lt_zcot0040-cpudt,
            sy-uname       TO lt_zcot0040-usnam,
            <fs_waers>     TO lt_zcot0040-rtcur,
            <fs_kstar>     TO lt_zcot0040-rkstar,
            'ENTR'         TO lt_zcot0040-process_9.

      lv_fname = 'HSL' && gt_month-v1.

      ASSIGN COMPONENT lv_fname OF STRUCTURE lt_zcot0040
         TO <fs_hist>.

      MOVE <fs_sour> TO <fs_hist>.

      COLLECT lt_zcot0040.
      CLEAR   lt_zcot0040.

    ENDIF.

  ENDLOOP.

  APPEND lt_pervalue.

  CALL FUNCTION 'BAPI_COSTACTPLN_POSTPRIMCOST'
    EXPORTING
      headerinfo     = ls_headerinfo
      delta          = abap_false
    TABLES
      indexstructure = lt_indexstructure
      coobject       = lt_coobject
      pervalue       = lt_pervalue
      return         = gt_return.

  READ TABLE gt_return WITH KEY type = 'E'.

  IF sy-subrc EQ 0 .

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    PERFORM build_message USING    gt_return
                          CHANGING p_message.

    p_error = abap_true.

  ELSE.

    TRY .

        IF gv_mode = 'H' AND p_last = abap_true.

          DELETE FROM zcot0040 WHERE rvers  = p_hversn
                                 AND ryear  = pa_gjahr
                                 AND robjnr = <fs_objnr>
                                 AND rkstar = <fs_kstar>.

          INSERT  zcot0040 FROM TABLE lt_zcot0040.

        ENDIF.

        COMMIT WORK.

      CATCH cx_sy_sql_error INTO DATA(lr_error).

        ROLLBACK WORK.

        lv_message = lr_error->get_text( ).
        MESSAGE s001 WITH lv_message INTO p_message.

        p_error = abap_true.

    ENDTRY.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_MESSAGE
*&---------------------------------------------------------------------*
FORM build_message USING    ps_message STRUCTURE bapiret2
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
*& Form CHECK_ANSWER
*&---------------------------------------------------------------------*
FORM check_answer .

  CHECK gv_mode = 'H'.

  SELECT SINGLE * FROM zcot0040 INTO @DATA(ls_zcot0040)
   WHERE rvers     = @p_hversn
     AND ryear     = @p_hgjahr
     AND process_9 = 'ENTR'.

  IF sy-subrc = 0.

    CLEAR gv_answer.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar       = TEXT-pt1
        icon_button_1  = 'ICON_OKAY'
        text_question  = TEXT-i01
        popup_type     = 'ICON_MESSAGE_WARNING'
      IMPORTING
        answer         = gv_answer
      EXCEPTIONS
        text_not_found = 1
        OTHERS         = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF gv_answer <> '1'.
      MESSAGE s052 DISPLAY LIKE 'E'.
      STOP.
    ENDIF.

  ENDIF.

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
*&---------------------------------------------------------------------*
*& Form SET_RANGES_OBJNR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_ranges_objnr .

** >>>>>>>>>>>>>>>>>> 2021.03.24 CBO 권한 체크 추가   BSGSM_FCM  START..~~~~~~~~~
** IF INPUT 회사코드로 ZCOT0320  권한 있으면 해당 회사 CO OBJECT
**  ELSEIF . ZCOT0320   " 회사 코드 등록은 없고 손익센터가 있으면 INPUT 회사코드의 해당 손익센터 CO OBJECT 만
** ELSEIF  회사코드, 손익센터 없고  프로젝트 있으며 해당 프로젝트만  조회되도록
** CO 슈퍼나 모듈 슈퍼는 INPUT  회사코드의  전체 WBS CCTR 조회 가능하도록 수정
** REQ  BY  강현수K


  DATA lv_type TYPE c.
  DATA lv_message TYPE bapi_msg.

  DATA: lt_0070  LIKE TABLE OF zcas0070,
        ls_0070  LIKE zcas0070,
        lv_class TYPE zcat0031-cd_class,
        lv_code  TYPE zcat0031-cd_code.

  CLEAR : gv_error .

  RANGES : lr_prctr FOR csks-prctr.
  RANGES : lr_pspnr FOR zcot0320-pspid.
  RANGES : lr_bukrs FOR zcot0320-bukrs.

  CLEAR : lr_prctr,  lr_pspnr, lr_bukrs .
  REFRESH:  lr_prctr, lr_pspnr, lr_bukrs.


**---------------

  lv_class = 'CASUSR'.
  lv_code  = sy-uname.

  "__ SUPER USER ID 체크
  PERFORM call_f4_values(zcar9000) TABLES lt_0070
                                    USING lv_class lv_code ls_0070.
  IF lt_0070[] IS NOT INITIAL.
    gv_super = abap_true.
  ELSE.
    lv_class = 'CASUCO'.
    lv_code  = sy-uname.

    "__ SUPER USER ID 체크
    PERFORM call_f4_values(zcar9000) TABLES lt_0070
                                      USING lv_class lv_code ls_0070.
    IF lt_0070[] IS NOT INITIAL.
      gv_super = abap_true.
    ENDIF.
  ENDIF.

**-----------------

  IF gv_super IS NOT INITIAL.  "  CO OBJECT 권한 체크

    " 회사 코드 오브젝트 모두 조회

    lr_bukrs-sign = 'I'.         lr_bukrs-option = 'EQ'.
    lr_bukrs-low = pa_bukrs.    COLLECT lr_bukrs.
    gv_bukrs_auth = abap_true.

  ELSE.

    SELECT * FROM zcot0320
      INTO TABLE @DATA(lt_zcot0320)
      WHERE bname = @sy-uname
        AND ( prctr1 NE @space OR
              bukrs NE @space OR
              pspid NE @space ).


    IF sy-subrc EQ 0.


      LOOP AT lt_zcot0320 ASSIGNING FIELD-SYMBOL(<fs>).


        IF <fs>-pspid IS NOT INITIAL.
          lr_pspnr-sign = 'I'.    lr_pspnr-option = 'EQ'.
          lr_pspnr-low = <fs>-pspid. COLLECT lr_pspnr.
        ENDIF.

        IF <fs>-prctr1 IS NOT INITIAL.

          lr_prctr-sign = 'I'.   lr_prctr-option = 'EQ'.
          lr_prctr-low = <fs>-prctr1. COLLECT lr_prctr.
        ENDIF.


        IF  <fs>-bukrs IS NOT INITIAL AND pa_bukrs  = <fs>-bukrs.  " cbo 권한 과 input 회사 같으면...
          lr_bukrs-sign = 'I'.           lr_bukrs-option = 'EQ'.
          lr_bukrs-low = <fs>-bukrs.    COLLECT lr_bukrs.

          gv_bukrs_auth = abap_true.


        ENDIF.


      ENDLOOP.


    ELSE.

      gv_error = TEXT-e77.   " ZCOR0320  미등록 ID.
*      gv_error = '조회 권한이 없습니다.(CO 모듈 관리자에게 문의하세요..)'.  " ZCOR0320  미등록 ID.

      CLEAR:  gt_cosp, gt_cosp[].


    ENDIF.


  ENDIF.

  DELETE lr_pspnr WHERE low IS INITIAL.
  DELETE lr_prctr WHERE low IS INITIAL.
  DELETE lr_bukrs WHERE low IS INITIAL.



  IF lr_pspnr IS INITIAL AND
     lr_prctr IS INITIAL AND
     lr_bukrs IS INITIAL.

    CLEAR:  gt_cosp, gt_cosp[].

    gv_error =  text-e88.
*    gv_error = '선택한 회사코드 조회권한이 없습니다.'.

  ENDIF.


**-- <<<<<<<<<<<<<<<<<<   END BY BSGSM_FCM   20210324


  CLEAR:  r_objnr,   r_objnr[].


*권한 있는  WBS, CCTR

*우선 순위  1 . 회사 코드권한  있을 때

  IF lr_bukrs[] IS NOT INITIAL .

*cctr..

    SELECT kostl FROM csks
           INTO TABLE @DATA(lt_csks)
           WHERE bukrs EQ @pa_bukrs
            AND datbi >= @sy-datum
            AND datab <= @sy-datum.


    LOOP AT lt_csks INTO DATA(ls_csks).

      MOVE: 'I'  TO r_objnr-sign,
            'EQ' TO r_objnr-option.

      r_objnr-low  = 'KS' && pa_kokrs && ls_csks-kostl.

      COLLECT r_objnr.
      CLEAR   r_objnr.

    ENDLOOP.


**wbs..

    SELECT a~objnr, a~pspnr, a~posid,  a~post1, a~zzcyp
          INTO TABLE @gt_prps
          FROM prps AS a
         INNER JOIN proj AS b
            ON a~psphi = b~pspnr
         WHERE a~pkokr = @pa_kokrs
           AND a~pbukr EQ  @pa_bukrs
           AND a~posid IN @lr_pspnr.


    LOOP AT gt_prps.
      MOVE: 'I'            TO r_objnr-sign,
            'EQ'           TO r_objnr-option,
            gt_prps-objnr  TO r_objnr-low.
      COLLECT r_objnr.
      CLEAR   r_objnr.
    ENDLOOP.

    EXIT.


  ENDIF.


* 우선순위 2.  손익 센터..

  IF lr_prctr[] IS NOT INITIAL.

**cctr..

    SELECT kostl FROM csks
           INTO TABLE @DATA(lt_csks2)
           WHERE bukrs EQ @pa_bukrs
            AND prctr IN @lr_prctr  " ADD BSGSM_FCM 20210324
            AND datbi >= @sy-datum
            AND datab <= @sy-datum.


    LOOP AT lt_csks2 INTO DATA(ls_csks2).

      MOVE: 'I'  TO r_objnr-sign,
            'EQ' TO r_objnr-option.

      r_objnr-low  = 'KS' && pa_kokrs && ls_csks2-kostl.

      COLLECT r_objnr.
      CLEAR   r_objnr.

    ENDLOOP.


**wbs..

    SELECT a~objnr, a~pspnr, a~posid,  a~post1, a~zzcyp
          INTO TABLE @gt_prps
          FROM prps AS a
         INNER JOIN proj AS b
            ON a~psphi = b~pspnr
         WHERE a~pkokr = @pa_kokrs
           AND a~pbukr EQ  @pa_bukrs
           AND a~prctr IN @lr_prctr
           AND a~posid IN @lr_pspnr.

    LOOP AT gt_prps ASSIGNING FIELD-SYMBOL(<wbs1>).
      MOVE: 'I'            TO r_objnr-sign,
            'EQ'           TO r_objnr-option,
            <wbs1>-objnr    TO r_objnr-low.
      COLLECT r_objnr.
      CLEAR   r_objnr.
    ENDLOOP.



    EXIT.

  ENDIF.


*우선순위 3  WBS 권한 있을 때..

  IF lr_pspnr[] IS NOT INITIAL.

**wbs..

    SELECT a~objnr, a~pspnr, a~posid,  a~post1, a~zzcyp
          INTO TABLE @gt_prps
          FROM prps AS a
         INNER JOIN proj AS b
            ON a~psphi = b~pspnr
         WHERE a~pkokr = @pa_kokrs
           AND a~pbukr EQ  @pa_bukrs
           AND a~prctr IN @lr_prctr
           AND a~posid IN @lr_pspnr.


    LOOP AT gt_prps ASSIGNING FIELD-SYMBOL(<wbs>).
      MOVE: 'I'            TO r_objnr-sign,
            'EQ'           TO r_objnr-option,
            <wbs>-objnr    TO r_objnr-low.
      COLLECT r_objnr.
      CLEAR   r_objnr.
    ENDLOOP.




  ENDIF.



**cctr..
**
**  SELECT kostl FROM csks
**         INTO TABLE @DATA(lt_csks)
**         WHERE bukrs EQ @pa_bukrs
**          AND prctr IN @lr_prctr  " ADD BSGSM_FCM 20210324
**          AND datbi >= @sy-datum
**          AND datab <= @sy-datum.
**
**
**  LOOP AT lt_csks INTO DATA(ls_csks).
**
**    MOVE: 'I'  TO r_objnr-sign,
**          'EQ' TO r_objnr-option.
**
**    r_objnr-low  = 'KS' && pa_kokrs && ls_csks-kostl.
**
**    COLLECT r_objnr.
**    CLEAR   r_objnr.
**
**  ENDLOOP.
**
**
****wbs..
**
**  SELECT a~objnr, a~pspnr, a~posid,  a~post1, a~zzcyp
**        INTO TABLE @gt_prps
**        FROM prps AS a
**       INNER JOIN proj AS b
**          ON a~psphi = b~pspnr
**       WHERE a~pkokr = @pa_kokrs
**         AND a~pbukr EQ  @pa_bukrs
**         AND a~prctr IN @lr_prctr
**         AND a~posid IN @lr_pspnr.
**
**
**  LOOP AT gt_prps.
**    MOVE: 'I'            TO r_objnr-sign,
**          'EQ'           TO r_objnr-option,
**          gt_prps-objnr  TO r_objnr-low.
**    COLLECT r_objnr.
**    CLEAR   r_objnr.
**  ENDLOOP.




ENDFORM.
