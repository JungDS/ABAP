*&---------------------------------------------------------------------*
*& Include          ZCOR0400F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITIAL_SET
*&---------------------------------------------------------------------*
FORM initial_set .

  SELECT SINGLE bezei, waers INTO (@pa_ktxt,@gv_waers)
    FROM tka01
   WHERE kokrs = '1000'.

  SELECT SINGLE vtext INTO @pa_vtxt
    FROM tkvs AS a
    LEFT JOIN tkvst AS b
      ON a~versi = b~versi
     AND b~spras = @sy-langu
   WHERE a~versi = 'B1'.

  "__ 20191223 BSGSM_FCM ADD default cac
  SET PARAMETER ID 'CAC' FIELD pa_kokrs.

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

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SELSCREEN
*&---------------------------------------------------------------------*
FORM set_selscreen .

  LOOP AT SCREEN.

    IF screen-group1 = 'MG1'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_CONTROLLING_AREA
*&---------------------------------------------------------------------*
FORM check_controlling_area .

  SELECT SINGLE b~ktext INTO @pa_ktext
    FROM cska AS a
    LEFT JOIN csku AS b
      ON a~ktopl = b~ktopl
     AND a~kstar = b~kstar
     AND b~spras = @sy-langu
   WHERE a~ktopl = @gc_ktopl
     AND a~kstar = @pa_kstar.

  IF sy-subrc <> 0.
    SET CURSOR FIELD 'PA_KSTAR'.
    MESSAGE e027  WITH TEXT-005.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_DATA_RTN
*&---------------------------------------------------------------------*
FORM selected_data_rtn .

  DATA lv_month     TYPE n LENGTH 2.
  DATA lv_fieldname TYPE fieldname.
  DATA lv_fyear     TYPE n LENGTH 7.

  DATA lv_value TYPE c LENGTH 6.

  FIELD-SYMBOLS: <fs1>      TYPE any,
                 <fs_value> TYPE any.

  RANGES r_objnr FOR zcot0040-robjnr.

  CLEAR: gt_outtab, gt_outtab[].

  CASE abap_true.

    WHEN pa_rad1.

      lv_value = 'KS' && pa_kokrs.

      IF r_bukrs[] IS NOT INITIAL OR
         so_prctr[] is not INITIAL or  gv_super EQ abap_true.

        SELECT a~kostl, b~ktext,
               concat( @lv_value , a~kostl ) AS objnr,
***        <<<<< bsgsm_fcm  20210414
               a~bukrs AS bukrs  ,
               a~prctr AS prctr
***           end by bsgsm_fcm
          FROM csks AS a
          LEFT JOIN cskt AS b
            ON a~kostl = b~kostl
           AND a~kokrs = b~kokrs
           AND a~datbi = b~datbi
         WHERE a~kokrs = @pa_kokrs
           AND a~datab <= @sy-datum
           AND a~datbi >= @sy-datum
           AND a~bukrs IN @so_bukrs
           AND a~bukrs IN @r_bukrs
           AND a~prctr  IN @so_prctr " add bsgsm_fcm  20210414
          INTO TABLE @gt_kostl.

        LOOP AT gt_kostl.

          MOVE: 'I'            TO r_objnr-sign,
                'EQ'           TO r_objnr-option,
                gt_kostl-objnr TO r_objnr-low.

          COLLECT r_objnr.
          CLEAR   r_objnr.

        ENDLOOP.

      ENDIF.

    WHEN pa_rad2.

**      SELECT posid, post1, objnr
**        FROM prps
**        INTO TABLE @gt_prps
**       WHERE pbukr IN @so_bukrs
**         AND pbukr IN @r_bukrs
**         AND loevm = @space.

**MODI BY BSGSM_FCM
      SELECT  a~posid,  a~post1,  a~objnr,
              a~pbukr, a~prctr
           INTO TABLE @gt_prps
           FROM prps AS a
          INNER JOIN proj AS b
             ON a~psphi = b~pspnr
          WHERE a~pkokr = @pa_kokrs
            AND a~pbukr IN @so_bukrs
            AND a~prctr IN @so_prctr
            AND b~pspid IN @so_pspid
            AND a~pbukr IN @r_bukrs
            AND a~loevm = @space.

** END BY BSGSM_FCM  20210414..

      LOOP AT gt_prps.

        MOVE: 'I'           TO r_objnr-sign,
              'EQ'          TO r_objnr-option,
              gt_prps-objnr TO r_objnr-low.

        COLLECT r_objnr.
        CLEAR   r_objnr.

      ENDLOOP.

  ENDCASE.

  IF r_objnr[] IS NOT INITIAL OR gv_super EQ abap_true.

    SELECT robjnr,
           SUM( hsl01 ) AS hsl01, SUM( hsl02 ) AS hsl02,
           SUM( hsl03 ) AS hsl03, SUM( hsl04 ) AS hsl04,
           SUM( hsl05 ) AS hsl05, SUM( hsl06 ) AS hsl06,
           SUM( hsl07 ) AS hsl07, SUM( hsl08 ) AS hsl08,
           SUM( hsl09 ) AS hsl09, SUM( hsl10 ) AS hsl10,
           SUM( hsl11 ) AS hsl11, SUM( hsl12 ) AS hsl12
      INTO TABLE @DATA(lt_zcot0040)
      FROM zcot0040
     WHERE rldnr  = '00'
       AND rrcty  = '1'
       AND rvers  = @pa_versn
       AND ryear  = @pa_gjahr
       AND robjnr IN @r_objnr
       AND rkstar = @pa_kstar
    GROUP BY robjnr.

  ENDIF.

  CASE 'X'.

    WHEN pa_rad1.

      LOOP AT gt_kostl.

        READ TABLE lt_zcot0040 INTO DATA(ls_zcot0040)
                       WITH KEY robjnr = gt_kostl-objnr.

        IF sy-subrc = 0.

          MOVE-CORRESPONDING ls_zcot0040 TO gt_outtab.
          MOVE: gv_waers                 TO gt_outtab-rtcur,
                gt_kostl-kostl           TO gt_outtab-kostl,
                gt_kostl-ktext           TO gt_outtab-ktext.

          gt_outtab-sum = gt_outtab-hsl01 + gt_outtab-hsl02 +
                          gt_outtab-hsl03 + gt_outtab-hsl04 +
                          gt_outtab-hsl05 + gt_outtab-hsl06 +
                          gt_outtab-hsl07 + gt_outtab-hsl08 +
                          gt_outtab-hsl09 + gt_outtab-hsl10 +
                          gt_outtab-hsl11 + gt_outtab-hsl12.

        ELSE.

          MOVE: gv_waers                 TO gt_outtab-rtcur,
                '1'                      TO gt_outtab-sortkey,
                gt_kostl-kostl           TO gt_outtab-kostl,
                gt_kostl-ktext           TO gt_outtab-ktext,
                gt_kostl-objnr           TO gt_outtab-robjnr.

        ENDIF.


**          add bsgsm_fcm   20210414
        gt_outtab-bukrs  = gt_kostl-bukrs.
        gt_outtab-prctr  = gt_kostl-prctr.
**  end by bsgsm_fcm

        MOVE '@77@' TO gt_outtab-icon.

        APPEND gt_outtab.
        CLEAR  gt_outtab.

      ENDLOOP.

      SORT gt_outtab BY sortkey kostl.

    WHEN pa_rad2.

      LOOP AT gt_prps.

        READ TABLE lt_zcot0040 INTO DATA(ls_zcot0040_prps)
                       WITH KEY robjnr = gt_prps-objnr.

        IF sy-subrc = 0.

          MOVE-CORRESPONDING ls_zcot0040_prps TO gt_outtab.
          MOVE: gv_waers                 TO gt_outtab-rtcur,
                gt_prps-posid            TO gt_outtab-posid,
                gt_prps-post1            TO gt_outtab-post1.

          gt_outtab-sum = gt_outtab-hsl01 + gt_outtab-hsl02 +
                          gt_outtab-hsl03 + gt_outtab-hsl04 +
                          gt_outtab-hsl05 + gt_outtab-hsl06 +
                          gt_outtab-hsl07 + gt_outtab-hsl08 +
                          gt_outtab-hsl09 + gt_outtab-hsl10 +
                          gt_outtab-hsl11 + gt_outtab-hsl12.
        ELSE.

          MOVE: gv_waers                 TO gt_outtab-rtcur,
                '1'                      TO gt_outtab-sortkey,
                gt_prps-posid            TO gt_outtab-posid,
                gt_prps-post1            TO gt_outtab-post1,
                gt_prps-objnr            TO gt_outtab-robjnr.

        ENDIF.

        MOVE '@77@' TO gt_outtab-icon.

**          add bsgsm_fcm   20210414
        gt_outtab-bukrs =  gt_prps-pbukr  .
        gt_outtab-prctr =   gt_prps-prctr .
**  end by bsgsm_fcm



        APPEND gt_outtab.
        CLEAR  gt_outtab.

      ENDLOOP.

      SORT gt_outtab BY sortkey posid.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECKED_SAVED_DATA
*&---------------------------------------------------------------------*
FORM checked_saved_data .

  CLEAR gv_exit.

  DATA lv_month TYPE n LENGTH 2.
  DATA lv_fieldname TYPE fieldname.

  CALL METHOD gr_grid1->check_changed_data.

  LOOP AT gt_outtab.

    CLEAR lv_month.
    CLEAR gt_outtab-changed.

    DO 12 TIMES.

      ADD 1 TO lv_month.
      lv_fieldname = 'SH' && lv_month.

      ASSIGN COMPONENT lv_fieldname OF STRUCTURE gt_outtab
        TO FIELD-SYMBOL(<fs1>).

      IF <fs1> IS NOT INITIAL.
        gt_outtab-changed = abap_true.
      ENDIF.

    ENDDO.

    MODIFY gt_outtab.

  ENDLOOP.

  READ TABLE gt_outtab WITH KEY changed = abap_true
             TRANSPORTING NO FIELDS.

  IF sy-subrc <> 0.
    MESSAGE s000 WITH TEXT-e01 DISPLAY LIKE 'E'.
    gv_exit = abap_true.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
FORM popup_to_confirm  USING pv_title
                            pv_quest.

  "-- call popup
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar       = pv_title                "TEXT-PT1
      text_question  = pv_quest                "TEXT-QT1
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

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAVE_DATA_RTN
*&---------------------------------------------------------------------*
FORM save_data_rtn .

  DATA: lv_message TYPE string.

  FIELD-SYMBOLS: <fs1> TYPE any,
                 <fs2> TYPE any.

  DATA lv_valid TYPE c.

  DATA ls_headerinfo  LIKE  bapiplnhdr.
  DATA ls_return TYPE bapiret2.

  DATA lv_fname TYPE fieldname.
  DATA lv_month TYPE n LENGTH 2.

  DATA : lt_indexstructure LIKE bapiacpstru OCCURS 0 WITH HEADER LINE,
         lt_coobject       LIKE bapipcpobj  OCCURS 0 WITH HEADER LINE,
         lt_pervalue       LIKE bapipcpval  OCCURS 0 WITH HEADER LINE.

  DATA lt_zcot0040 TYPE TABLE OF zcot0040 WITH HEADER LINE.

  DATA lv_belnr TYPE belnr_d.
  DATA lv_seq   TYPE zerseq.

  LOOP AT gt_outtab WHERE changed = abap_true.

    CLEAR: gt_return, gt_return[].

    CLEAR: lt_zcot0040, lt_zcot0040[].
    CLEAR  lv_belnr.
    CLEAR  lv_valid.
    CLEAR  lv_seq.

    CLEAR: ls_headerinfo, lt_indexstructure,
                          lt_indexstructure[],
                          lt_coobject,
                          lt_coobject[],
                          lt_pervalue,
                          lt_pervalue[].

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
      MESSAGE s001 WITH TEXT-e03 INTO gt_outtab-message.
      MOVE '@0A@' TO gt_outtab-icon.
      MODIFY gt_outtab.
      CONTINUE.
    ENDIF.

    _conversion_in lv_belnr.

*-- Header Data
    ls_headerinfo-co_area     = pa_kokrs.     "관리 회계영역
    ls_headerinfo-fisc_year   = pa_gjahr.     "회계연도
    ls_headerinfo-period_from = '001'.        "기간 시작
    ls_headerinfo-period_to   = '012'.        "기간 종료
    ls_headerinfo-version     = pa_versn.     "버전

*-- 전표 헤더 텍스트
    ls_headerinfo-plan_currtype = 'C'. "통화

*-- CO-계획: 액티비티투입 & 주요지표 계획 BAPIs
    lt_indexstructure-object_index = 1.
    lt_indexstructure-value_index  = 1.
    APPEND lt_indexstructure.

*-- CO 계획: 1차 원가 BAPI에 대한 오브젝트
    lt_coobject-object_index  = 1.

    CASE abap_true.
      WHEN pa_rad1.
        lt_coobject-costcenter    = gt_outtab-kostl.
      WHEN pa_rad2.
        lt_coobject-wbs_element   = gt_outtab-posid.
    ENDCASE.

    APPEND lt_coobject.

*-- CO 계획: 1차 원가 BAPI에 대한 값
    lt_pervalue-value_index  = 1.
    lt_pervalue-cost_elem    = pa_kstar.   "원가요소
    lt_pervalue-trans_curr   = gv_waers.

    CLEAR lv_month.

    DO 12 TIMES.

      ADD 1 TO lv_month.

      lv_fname = 'SH' && lv_month.

      ASSIGN COMPONENT lv_fname OF STRUCTURE gt_outtab
         TO <fs1>.

      IF <fs1> IS INITIAL.
        CONTINUE.
      ELSE.
        lv_valid = abap_true.
      ENDIF.

      lv_fname = 'LT_PERVALUE-FIX_VAL_PER' && lv_month.

      ASSIGN (lv_fname) TO <fs2>.

      IF gv_waers = 'KRW'.
        <fs2> = <fs1>  * 100.
      ELSE.
        <fs2> = <fs1>.
      ENDIF.

*-- CBO 저장 데이터
      MOVE: '00'             TO lt_zcot0040-rldnr,
            '1'              TO lt_zcot0040-rrcty,
            pa_versn         TO lt_zcot0040-rvers,
            pa_gjahr         TO lt_zcot0040-ryear,
            gt_outtab-robjnr TO lt_zcot0040-robjnr,
            lv_belnr         TO lt_zcot0040-rdocnr,
            'BT01'           TO lt_zcot0040-budtype_9,
            pa_kokrs         TO lt_zcot0040-rkokrs,
            sy-uzeit         TO lt_zcot0040-cputm,
            sy-datum         TO lt_zcot0040-cpudt,
            sy-uname         TO lt_zcot0040-usnam,
            gv_waers         TO lt_zcot0040-rtcur,
            pa_kstar         TO lt_zcot0040-rkstar.

      lv_fname = 'HSL' && lv_month.

      ASSIGN COMPONENT lv_fname OF STRUCTURE lt_zcot0040
         TO <fs2>.

      MOVE <fs1> TO <fs2>.

      IF <fs1> > 0.

        MOVE: 'SUPL' TO lt_zcot0040-process_9.   "증액

      ELSEIF <fs1> < 0.

        MOVE: 'RENT' TO lt_zcot0040-process_9.   "감액

      ENDIF.

      COLLECT lt_zcot0040.
      CLEAR   lt_zcot0040.

    ENDDO.

    APPEND lt_pervalue.

    IF lv_valid IS INITIAL.

      MOVE '@09@'   TO gt_outtab-icon.
      MOVE TEXT-i01 TO gt_outtab-message.
      MODIFY gt_outtab.
      CONTINUE.

    ENDIF.

    LOOP AT lt_zcot0040.
      ADD 1 TO lv_seq.
      MOVE lv_seq TO lt_zcot0040-rseq.
      MODIFY lt_zcot0040.
    ENDLOOP.

    CALL FUNCTION 'BAPI_COSTACTPLN_POSTPRIMCOST'
      EXPORTING
        headerinfo     = ls_headerinfo
        delta          = abap_true
      TABLES
        indexstructure = lt_indexstructure
        coobject       = lt_coobject
        pervalue       = lt_pervalue
        return         = gt_return.

    READ TABLE gt_return WITH KEY type = 'E'.

    IF sy-subrc EQ 0 .

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      PERFORM build_message USING    gt_return
                            CHANGING gt_outtab-message.
      MOVE '@0A@' TO gt_outtab-icon.

    ELSE.

      TRY .

          INSERT  zcot0040 FROM TABLE lt_zcot0040.
          COMMIT WORK.

          MOVE '@08@' TO gt_outtab-icon.

          gt_outtab-sum = gt_outtab-sum + gt_outtab-sumsh.
          CLEAR gt_outtab-sumsh.
          CLEAR lv_month.

          DO 12 TIMES.

            ADD 1 TO lv_month.

            lv_fname = 'SH' && lv_month.

            ASSIGN COMPONENT lv_fname OF STRUCTURE gt_outtab
               TO <fs1>.

            lv_fname = 'HSL' && lv_month.

            ASSIGN COMPONENT lv_fname OF STRUCTURE gt_outtab
               TO <fs2>.

            <fs2> = <fs2> + <fs1>.

            CLEAR <fs1>.

          ENDDO.

        CATCH cx_sy_sql_error INTO DATA(lr_error).

          ROLLBACK WORK.

          lv_message = lr_error->get_text( ).
          MESSAGE s001 WITH lv_message INTO gt_outtab-message.
          MOVE '@0A@' TO gt_outtab-icon.

      ENDTRY.

    ENDIF.

    MODIFY gt_outtab.

  ENDLOOP.

  MESSAGE s000 WITH TEXT-s01.

  PERFORM refresh_grid_0100.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_INSTANCE_0100
*&---------------------------------------------------------------------*
FORM create_instance_0100 .

  CREATE OBJECT gr_splitter1
    EXPORTING
      rows    = 2
      columns = 1
      parent  = cl_gui_splitter_container=>screen0.

  gr_parent_html = gr_splitter1->get_container(
      row       = 1
      column    = 1 ).

  gr_data_container = gr_splitter1->get_container(
      row       = 2
      column    = 1 ).

  CALL METHOD gr_splitter1->set_row_height
    EXPORTING
      id     = 1
      height = 8.

  CALL METHOD gr_splitter1->set_row_height
    EXPORTING
      id     = 2
      height = 50.

  CREATE OBJECT gr_grid1
    EXPORTING
      i_parent = gr_data_container.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form INIT_LAYOUT_0100
*&---------------------------------------------------------------------*
FORM init_layout_0100 .

  CLEAR gs_layout.

  gs_layout-zebra      = abap_true.
  gs_layout-sel_mode   = space.     "B:단일,C:복수,D:셀,A:행/열
  gs_layout-box_fname  = space.
  gs_layout-no_rowmark = space.
  gs_layout-totals_bef = abap_true.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_GRID_EXCLUDE_0100
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

*
  _set_ex:
*   CL_GUI_ALV_GRID=>MC_FC_FIND,
*
*    "-- begin 기능버튼활성화시 제외
*    CL_GUI_ALV_GRID=>MC_FC_SORT_ASC,
*    CL_GUI_ALV_GRID=>MC_FC_SORT_DSC,
*    CL_GUI_ALV_GRID=>MC_MB_SUBTOT,
*    CL_GUI_ALV_GRID=>MC_MB_SUM,
*    "-- end
*
    cl_gui_alv_grid=>mc_fc_loc_copy_row,
    cl_gui_alv_grid=>mc_fc_loc_append_row,
    cl_gui_alv_grid=>mc_fc_loc_insert_row,
    cl_gui_alv_grid=>mc_fc_loc_move_row,
    cl_gui_alv_grid=>mc_fc_loc_delete_row,
*
*    "-- begin 기능버튼활성화
    cl_gui_alv_grid=>mc_fc_loc_copy,
    cl_gui_alv_grid=>mc_fc_loc_cut,
    cl_gui_alv_grid=>mc_fc_loc_paste,
    cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
*    "-- end
*
**    CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO,
**    CL_GUI_ALV_GRID=>MC_FC_CHECK,
**
***   CL_GUI_ALV_GRID=>MC_FC_DETAIL,
***   CL_GUI_ALV_GRID=>MC_FC_FILTER,
**    CL_GUI_ALV_GRID=>MC_FC_GRAPH,
**    CL_GUI_ALV_GRID=>MC_FC_HTML,
**    CL_GUI_ALV_GRID=>MC_FC_INFO,
**    CL_GUI_ALV_GRID=>MC_FC_REFRESH,
**
***   CL_GUI_ALV_GRID=>MC_FC_VIEWS,
***   CL_GUI_ALV_GRID=>MC_FC_LOAD_VARIANT,
***   CL_GUI_ALV_GRID=>MC_FC_PRINT,
***   CL_GUI_ALV_GRID=>MC_MB_VARIANT,
***   CL_GUI_ALV_GRID=>MC_MB_EXPORT,
**
**    CL_GUI_ALV_GRID=>MC_FC_VIEW_CRYSTAL,
**    CL_GUI_ALV_GRID=>MC_FC_VIEW_EXCEL,
**    CL_GUI_ALV_GRID=>MC_FC_VIEW_GRID,
**    CL_GUI_ALV_GRID=>MC_FC_VIEW_LOTUS,
**    CL_GUI_ALV_GRID=>MC_FC_EXPCRDATA,
**    CL_GUI_ALV_GRID=>MC_FC_EXPCRDESIG,
**    CL_GUI_ALV_GRID=>MC_FC_EXPCRTEMPL,
**    CL_GUI_ALV_GRID=>MC_FC_CALL_ABC,
**    CL_GUI_ALV_GRID=>MC_FC_CALL_CRBATCH.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form APPEND_FIELDCAT_0100
*&---------------------------------------------------------------------*
FORM append_fieldcat_0100 .

  PERFORM get_fieldcatlog_data.
  PERFORM modify_fieldcatlog_data.

ENDFORM.
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
*& Form GET_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
FORM get_fieldcatlog_data .

  DATA: lt_fieldcat TYPE kkblo_t_fieldcat.

  CALL FUNCTION 'K_KKB_FIELDCAT_MERGE'
    EXPORTING
      i_callback_program     = sy-repid
      i_strucname            = 'ZCOS0390'
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

    MESSAGE e014.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MODIFY_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
FORM modify_fieldcatlog_data .

  DATA:  lv_text(50).

  LOOP AT gt_fieldcat INTO gs_fieldcat.

    CLEAR: lv_text.

    CASE gs_fieldcat-fieldname.

      WHEN 'SORTKEY' OR 'CHANGED' OR 'ROBJNR'.
        gs_fieldcat-no_out = abap_true.

      WHEN 'ICON'.
        gs_fieldcat-icon = abap_true.
        lv_text = TEXT-c10.
        gs_fieldcat-key       = abap_true.

      WHEN 'KOSTL'.
        IF pa_rad2 = abap_true.
          gs_fieldcat-no_out = abap_true.
        ENDIF.

        lv_text = TEXT-c01.
        gs_fieldcat-outputlen = '10'.
        gs_fieldcat-key       = abap_true.

      WHEN 'KTEXT'.
        IF pa_rad2 = abap_true.
          gs_fieldcat-no_out = abap_true.
        ENDIF.

        lv_text = TEXT-c05.
        gs_fieldcat-outputlen = '25'.
        gs_fieldcat-key       = abap_true.

      WHEN 'POSID'.
        IF pa_rad1 = abap_true.
          gs_fieldcat-no_out = abap_true.
        ENDIF.
        lv_text = TEXT-c03.
        gs_fieldcat-outputlen = '16'.
        gs_fieldcat-key       = abap_true.

      WHEN 'POST1'.
        IF pa_rad1 = abap_true.
          gs_fieldcat-no_out = abap_true.
        ENDIF.
        lv_text = TEXT-c12.
        gs_fieldcat-outputlen = '25'.
        gs_fieldcat-key       = abap_true.

      WHEN 'SUM'.
        lv_text = TEXT-c06.
        gs_fieldcat-outputlen = '15'.
        gs_fieldcat-no_zero   = abap_true.
        gs_fieldcat-do_sum    = abap_true.

      WHEN 'SUMSH'.
        lv_text = TEXT-c07.
        gs_fieldcat-outputlen = '15'.
        gs_fieldcat-no_zero   = abap_true.
        gs_fieldcat-do_sum    = abap_true.

      WHEN 'MESSAGE'.
        lv_text = TEXT-c11.
        gs_fieldcat-outputlen = '100'.

      WHEN 'BUKRS' OR 'PRCTR'.
        gs_fieldcat-no_out = abap_true.

      WHEN OTHERS.

    ENDCASE.

    IF gs_fieldcat-fieldname CP 'HSL*'.

      lv_text = gs_fieldcat-fieldname+3(2) && TEXT-c08.
      gs_fieldcat-outputlen = '15'.
      gs_fieldcat-do_sum    = abap_true.
      gs_fieldcat-no_zero   = abap_true.
      gs_fieldcat-emphasize  = 'C500'.

    ELSEIF gs_fieldcat-fieldname CP 'SH*'.

      lv_text = gs_fieldcat-fieldname+2(2) && TEXT-c09.
      gs_fieldcat-edit = abap_true.
      gs_fieldcat-outputlen = '15'.
      gs_fieldcat-no_zero   = abap_true.
      gs_fieldcat-do_sum    = abap_true.

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
*& Form MAKE_TOP_OF_PAGE_DATA_0100
*&---------------------------------------------------------------------*
FORM make_top_of_page_data_0100 .

  DATA: lt_texts TYPE sdydo_text_table,
        lv_text  TYPE sdydo_text_element.

  DATA lv_kstar TYPE kstar.

  CONCATENATE TEXT-001 ':' pa_kokrs
        INTO lv_text SEPARATED BY space.

  CALL METHOD gr_top_document->add_text
    EXPORTING
      text         = lv_text
      sap_color    = cl_dd_document=>list_heading_int
      sap_emphasis = cl_dd_area=>key
      style_class  = space.

  CALL METHOD gr_top_document->new_line.

  CONCATENATE TEXT-c02 ':' pa_gjahr
        INTO lv_text SEPARATED BY space.

  CALL METHOD gr_top_document->add_text
    EXPORTING
      text         = lv_text
      sap_color    = cl_dd_document=>list_heading_int
      sap_emphasis = cl_dd_area=>key
      style_class  = space.

  CALL METHOD gr_top_document->new_line.

  CONCATENATE TEXT-002 ':' pa_versn  pa_vtxt
        INTO lv_text SEPARATED BY space.

  CALL METHOD gr_top_document->add_text
    EXPORTING
      text         = lv_text
      sap_color    = cl_dd_document=>list_heading_int
      sap_emphasis = cl_dd_area=>key
      style_class  = space.

  CALL METHOD gr_top_document->new_line.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = pa_kstar
    IMPORTING
      output = lv_kstar.

  CONCATENATE TEXT-005 ':' lv_kstar pa_ktext
        INTO lv_text SEPARATED BY space.

  CALL METHOD gr_top_document->add_text
    EXPORTING
      text         = lv_text
      sap_color    = cl_dd_document=>list_heading_int
      sap_emphasis = cl_dd_area=>key
      style_class  = space.

  CASE 'X'.

    WHEN pa_rad1.

      CONCATENATE TEXT-006 ':' TEXT-c01
            INTO lv_text SEPARATED BY space.

    WHEN pa_rad2.

      CONCATENATE TEXT-006 ':' TEXT-c03
            INTO lv_text SEPARATED BY space.

  ENDCASE.

  CALL METHOD gr_top_document->new_line.

  CALL METHOD gr_top_document->add_text
    EXPORTING
      text         = lv_text
      sap_color    = cl_dd_document=>list_heading_int
      sap_emphasis = cl_dd_area=>key
      style_class  = space.

  CALL METHOD gr_top_document->new_line
    EXPORTING
      repeat = 1.

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
*& Form DISPLAY_ALV_GRID_0100
*&---------------------------------------------------------------------*
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
      it_outtab                     = gt_outtab[]
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3.

  IF sy-subrc NE 0.
    MESSAGE e000(0k) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  DATA(lv_lines) = lines( gt_outtab ).
  MESSAGE s039 WITH lv_lines.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFRESH_GRID_0100
*&---------------------------------------------------------------------*
FORM refresh_grid_0100 .

  gs_stable-row = abap_true. "Row
  gs_stable-col = abap_true. "column

  CALL METHOD gr_grid1->refresh_table_display
    EXPORTING
      is_stable      = gs_stable
      i_soft_refresh = space.

  CALL METHOD cl_gui_cfw=>flush.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_TOOLBAR
*&---------------------------------------------------------------------*
FORM event_toolbar  USING    p_e_object
                             p_e_interactive
                             p_sender.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_USER_COMMAND
*&---------------------------------------------------------------------*
FORM event_user_command  USING    p_e_ucomm
                                  p_sender.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_DATA_CHANGED
*&---------------------------------------------------------------------*
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
  DATA: ls_mod_cells TYPE lvc_s_modi,
        ls_ins_cells TYPE lvc_s_moce,
        ls_del_cells TYPE lvc_s_moce.

  DATA lv_month TYPE n LENGTH 2.
  DATA lv_fieldname TYPE fieldname.

  DATA lv_tabix TYPE sy-tabix.
  DATA lv_value TYPE hslxx9_cs.
  DATA lv_sum   TYPE hslxx9_cs.

  DEFINE _modify_cell.
    CALL METHOD pr_data_changed->modify_cell
      EXPORTING
        i_fieldname = &1
        i_row_id    = &2
        i_value     = &3.

  END-OF-DEFINITION.

  DEFINE _get_cell_value.
    CALL METHOD pr_data_changed->get_cell_value
      EXPORTING
        i_fieldname = &1
        i_row_id    = &2
      IMPORTING
        e_value     = &3.
  END-OF-DEFINITION.

  DEFINE _add_protocol.
    CALL METHOD pr_data_changed->add_protocol_entry
      EXPORTING
        i_fieldname = &1
        i_row_id    = &2
        i_msgid     = 'ZCO01'
        i_msgty     = &3
        i_msgno     = &4
        i_msgv1     = &5
        i_msgv2     = &6
        i_msgv3     = &7
        i_msgv4     = &8.
  END-OF-DEFINITION.

  DEFINE _modify_style.
    CALL METHOD pr_data_changed->modify_style
      EXPORTING
        i_fieldname = &1
        i_row_id    = &2
        i_style     = &3.
  END-OF-DEFINITION.
*--- End of Example

  CASE pr_sender.

    WHEN gr_grid1.

      LOOP AT pr_data_changed->mt_mod_cells INTO ls_mod_cells.

        IF ls_mod_cells-fieldname CP 'SH*'.

          CLEAR lv_month.

          DO 12 TIMES.

            ADD 1 TO lv_month.

            lv_fieldname = 'SH' && lv_month.

            _get_cell_value lv_fieldname ls_mod_cells-row_id
                                         lv_value.

            lv_sum = lv_sum + lv_value.

          ENDDO.

          _modify_cell 'SUMSH'   ls_mod_cells-row_id lv_sum.

        ENDIF.

      ENDLOOP.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_DATA_CHANGED_FINISHED
*&---------------------------------------------------------------------*
FORM event_data_changed_finished  USING    p_e_modified
                                           p_et_good_cells
                                           p_sender.
  IF p_e_modified IS NOT INITIAL.
    PERFORM refresh_grid_0100.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM event_hotspot_click  USING    p_e_row_id
                                   p_e_column_id
                                   p_es_row_no
                                   p_sender.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_DOUBLE_CLICK
*&---------------------------------------------------------------------*
FORM event_double_click  USING    p_e_row
                                  p_e_column
                                  p_es_row_no
                                  p_sender.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_HELP_ON_F4
*&---------------------------------------------------------------------*
FORM event_help_on_f4  USING    p_e_fieldname
                                p_e_fieldvalue
                                p_es_row_no
                                p_er_event_data
                                p_et_bad_cells
                                p_e_display
                                p_sender.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM event_top_of_page  USING    p_e_dyndoc_id
                                 p_table_index
                                 p_sender.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_END_OF_LIST
*&---------------------------------------------------------------------*
FORM event_end_of_list  USING    p_e_dyndoc_id
                                 p_sender.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REGIST_ALV_EVENT_0100
*&---------------------------------------------------------------------*
FORM regist_alv_event_0100 USING pr_grid TYPE REF TO cl_gui_alv_grid.

* REGISTER EVENT
  CALL METHOD pr_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

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
    gr_event_receiver->handle_data_changed_finished
      FOR ALL INSTANCES,
    gr_event_receiver->handle_user_command  FOR ALL INSTANCES,
    gr_event_receiver->handle_hotspot_click FOR ALL INSTANCES,
    gr_event_receiver->handle_double_click  FOR ALL INSTANCES.


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
*& Form ENQUE_LOCK
*&---------------------------------------------------------------------*
FORM enque_lock .

  DATA ls_rstable TYPE rstable.

  ls_rstable-tabname = 'ZCOT0040'.
  ls_rstable-varkey  = sy-mandt && pa_versn && pa_gjahr.

  CALL FUNCTION 'ENQUEUE_E_TABLE'
    EXPORTING
      mode_rstable   = 'E'
      tabname        = ls_rstable-tabname
      varkey         = ls_rstable-varkey
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DEQUE_LOCK
*&---------------------------------------------------------------------*
FORM deque_lock .

  DATA ls_rstable TYPE rstable.

  ls_rstable-tabname = 'ZCOT0040'.
  ls_rstable-varkey  = sy-mandt && pa_versn && pa_gjahr.

  CALL FUNCTION 'DEQUEUE_E_TABLE'
    EXPORTING
      mode_rstable   = 'E'
      tabname        = ls_rstable-tabname
      varkey         = ls_rstable-varkey
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*

FORM authority_check.

  DATA lv_type TYPE c.
  DATA lv_message TYPE bapi_msg.

  DATA: lt_0070  LIKE TABLE OF zcas0070,
        ls_0070  LIKE zcas0070,
        lv_class TYPE zcat0031-cd_class,
        lv_code  TYPE zcat0031-cd_code.

  lv_class = 'CASUSR'.
  lv_code  = sy-uname.



  DATA : lv_super(1).
  DATA : lv_super_co(1).


  "__ SUPER USER ID 체크
  PERFORM call_f4_values(zcar9000) TABLES lt_0070
                                    USING lv_class lv_code ls_0070.
  IF lt_0070[] IS NOT INITIAL.
*    EXIT.

    lv_super = abap_true.
  ELSE.
    lv_class = 'CASUCO'.
    lv_code  = sy-uname.

    "__ SUPER USER ID 체크
    PERFORM call_f4_values(zcar9000) TABLES lt_0070
                                      USING lv_class lv_code ls_0070.
    IF lt_0070[] IS NOT INITIAL.
*      EXIT.

      lv_super_co = abap_true.
    ENDIF.
  ENDIF.

**  CHECK GV_MODE = 'S'. 처리자 강현수 21.04.08

**
**  READ TABLE so_bukrs INDEX 1.
**  CALL FUNCTION 'ZCA_PRG_AUTH_CHECK'
**    EXPORTING
**      i_module   = 'CO'
***      i_bukrs_co = so_bukrs-low
**    IMPORTING
**      e_type     = lv_type
**      e_message  = lv_message
**      TABLES
**      IT_BUKRS_CO  = SO_BUKRS[].
**
**
**  IF LV_TYPE = 'E'.
**    MESSAGE S000 WITH LV_MESSAGE DISPLAY LIKE 'E'.
**    STOP.
**  ENDIF.
**
**  SELECT * FROM ZCOT0320
**    INTO TABLE @DATA(LT_ZCOT0320)
**    WHERE BNAME = @SY-UNAME.
**
**  LOOP AT LT_ZCOT0320 INTO DATA(LS_ZCOT0320).
**
**
**    IF LS_ZCOT0320-BUKRS IS NOT INITIAL.
**
**      MOVE: LS_ZCOT0320-BUKRS TO R_BUKRS-LOW,
**            'I'                TO R_BUKRS-SIGN,
**            'EQ'               TO R_BUKRS-OPTION.
**
**      COLLECT R_BUKRS.
**      CLEAR   R_BUKRS.
**
**    ENDIF.
**
**
**  ENDLOOP.



*****<<<<<<<<<<<<   modi by bsgsm_fcm    20210414..

**--- Original Message ---
**From : "강현수"<balla2000@tsk.co.kr>
**To : "정수연(BSG_FCM)/위원/수행사(ERP)"<bsg_fcm1@tsk.co.kr>
**Date : 2021/04/14 수요일 오전 11:25:25
**Subject : 예산증/감액 관리 계정별 일괄처리(ZCOR0410) 수정 요청의 건
**
**프로그램  예산증/감액 관리 계정별 일괄처리(ZCOR0410)에 대하여 권한 로직 수정을 요청합니다.
**1) ZCOR0320에 좌측(손익센터~프로젝트)의 권한을 기준으로
**2) 해당 오브젝트에 해당되는 속성 조회되도록 수정을 요청합니다.



  IF lv_super IS NOT INITIAL OR   lv_super_co IS NOT INITIAL. "  CO OBJECT 권한 체크

    gv_super = abap_true.

  ELSE.

    READ TABLE so_bukrs INDEX 1.
    CALL FUNCTION 'ZCA_PRG_AUTH_CHECK'
      EXPORTING
        i_module    = 'CO'
*       i_bukrs_co  = so_bukrs-low
      IMPORTING
        e_type      = lv_type
        e_message   = lv_message
      TABLES
        it_bukrs_co = so_bukrs[].

    IF lv_type = 'E'.
      MESSAGE s000 WITH lv_message DISPLAY LIKE 'E'.
      STOP.
    ENDIF.

    SELECT * FROM zcot0320
      INTO TABLE @DATA(lt_zcot0320)
      WHERE bname = @sy-uname.

    CLEAR : so_prctr, so_pspid.
    REFRESH : so_prctr, so_pspid.


    LOOP AT lt_zcot0320 ASSIGNING FIELD-SYMBOL(<fs>).


      IF <fs>-bukrs IS NOT INITIAL.

        MOVE: <fs>-bukrs TO r_bukrs-low,
              'I'        TO r_bukrs-sign,
              'EQ'       TO r_bukrs-option.

        COLLECT r_bukrs.
        CLEAR   r_bukrs.

      ENDIF.



      IF <fs>-prctr1 IS NOT INITIAL.


        MOVE: <fs>-prctr1 TO so_prctr-low,
              'I'         TO so_prctr-sign,
              'EQ'        TO so_prctr-option.

        COLLECT so_prctr.
        CLEAR   so_prctr.



      ENDIF.


      IF <fs>-pspid IS NOT INITIAL.

        MOVE: <fs>-pspid TO so_pspid-low,
                  'I'    TO so_pspid-sign,
                  'EQ'   TO so_pspid-option.

        COLLECT so_pspid.
        CLEAR   so_pspid.

      ENDIF.

    ENDLOOP.


  ENDIF.

***  end by bsgsm_fcm  >>>>>>>>>>>>>>>>>
ENDFORM.
