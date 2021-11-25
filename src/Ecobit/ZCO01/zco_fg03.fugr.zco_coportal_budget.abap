FUNCTION zco_coportal_budget .
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(E_RESULT) TYPE  CHAR1
*"     VALUE(E_MSG) TYPE  CHAR255
*"  TABLES
*"      ET_BUDGET STRUCTURE  ZCOS0250
*"----------------------------------------------------------------------

  DATA: lv_posid TYPE ps_posid,
        lv_kstar TYPE kstar.

  DATA lv_ctype TYPE zectype.

  DATA: lv_ntime     TYPE i,
        lv_cpperl    TYPE pperl,
        lv_fieldname TYPE fieldname.

  DATA lv_month TYPE n LENGTH 2.
  DATA lv_objnr TYPE j_objnr.

  DATA: BEGIN OF lt_month OCCURS 0,
          v1 TYPE n LENGTH 2,
        END OF lt_month.

  FIELD-SYMBOLS <fs_wkg> TYPE any.

  DATA: lv_dsum TYPE hslvt9_cs,
        lv_fsum TYPE hslvt9_cs.

  RANGES r_budat FOR acdoca-budat.

* ADD BSGSM_FCM   2021.09.01
  DATA : lv_bukrs TYPE zcot0010-bukrs.
  DATA : lv_budget_flag TYPE zcot000-flag.

  READ TABLE et_budget ASSIGNING FIELD-SYMBOL(<fs>) INDEX 1.

  IF <fs>-posid IS ASSIGNED AND <fs>-posid IS NOT  INITIAL.
    CLEAR lv_posid.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSN_INPUT'
      EXPORTING
        input  = <fs>-posid
      IMPORTING
        output = lv_posid.

    CLEAR lv_bukrs.
    SELECT SINGLE pbukr FROM prps
      INTO @lv_bukrs
     WHERE posid = @lv_posid.

    CLEAR lv_budget_flag.
    SELECT SINGLE flag INTO @lv_budget_flag
      FROM zcot000
      WHERE bukrs = @lv_bukrs
        AND flag = 'X'.

  ENDIF.

**END BY BSGSM_FCM.




  LOOP AT et_budget.

    CLEAR: r_budat, r_budat[], lv_ntime, lv_month.

    CLEAR: lt_month, lt_month[].

    CLEAR: et_budget-hslyear,
           et_budget-hslperio,
           et_budget-hslspend.

    CLEAR: lv_dsum, lv_fsum.

    MOVE: et_budget-eindt(4) TO et_budget-gjahr,
          'KRW'              TO et_budget-waers.

    CALL FUNCTION 'CONVERSION_EXIT_ABPSN_INPUT'
      EXPORTING
        input  = et_budget-posid
      IMPORTING
        output = lv_posid.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = et_budget-saknr
      IMPORTING
        output = lv_kstar.

    SELECT SINGLE * FROM prps
      INTO @DATA(ls_prps)
     WHERE posid = @lv_posid.

    IF sy-subrc = 0.

      lv_objnr = ls_prps-objnr.

      CASE ls_prps-zzcyp.   " WBS 통제유형

        WHEN '1' OR '2' OR '3'.
          lv_ctype = ls_prps-zzcyp.

        WHEN OTHERS.   "비통제

          et_budget-cperd = '5'.       "예산통제 제외
          MODIFY et_budget.
          CONTINUE.

      ENDCASE.

*-- 기간별 예산통제 제외
      SELECT SINGLE bexcl FROM zcot0030
        INTO @DATA(lv_bexcl)
       WHERE gjahr = @et_budget-gjahr
         AND kokrs = @gc_kokrs
         AND posid = @lv_posid.

      IF sy-subrc = 0  AND lv_bexcl = abap_true.

        et_budget-cperd = '5'.       "예산통제 제외
        MODIFY et_budget.
        CONTINUE.

      ENDIF.

**>>>>>>>>>>>>>    MODIFY BSGSM_FCM 2021.09.01
      IF lv_budget_flag = 'X'.

        SELECT SINGLE cperd INTO @et_budget-cperd
                  FROM zcot0010
                 WHERE bukrs = @lv_bukrs
                   AND ctype  = @ls_prps-zzcyp
                   AND fkstar = @lv_kstar
                   AND kokrs  = @gc_kokrs
                   AND gjahr  = @et_budget-gjahr.
      ELSE.
        SELECT SINGLE cperd INTO @et_budget-cperd
          FROM zcot0010
         WHERE ctype  = @ls_prps-zzcyp
           AND fkstar = @lv_kstar
           AND kokrs  = @gc_kokrs
           AND gjahr  = @et_budget-gjahr.
      ENDIF.
*
*           SELECT SINGLE cperd INTO @et_budget-cperd
*          FROM zcot0010
*         WHERE ctype  = @ls_prps-zzcyp
*           AND fkstar = @lv_kstar
*           AND kokrs  = @gc_kokrs
*           AND gjahr  = @et_budget-gjahr.
**END BY BSGSM_FCM  >>>>>>>>>>>>>

      CASE et_budget-cperd.

        WHEN '1'.

          lv_ntime = 1.
          lv_month = et_budget-eindt+4(2).

          r_budat-low    = et_budget-eindt(4) && '0101'.

          CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
            EXPORTING
              day_in            = r_budat-low
            IMPORTING
              last_day_of_month = r_budat-high
            EXCEPTIONS
              day_in_no_date    = 1
              OTHERS            = 2.

          r_budat-sign   = 'I'.
          r_budat-option = 'BT'.
          APPEND r_budat.

        WHEN '2'.

          lv_ntime = 3.

          IF et_budget-eindt+4(4) >= '0101' AND
             et_budget-eindt+4(4) <= '0331'.

            lv_month = '01'.

            r_budat-low    = et_budget-gjahr && '0101'.
            r_budat-high   = et_budget-gjahr && '0331'.
            r_budat-sign   = 'I'.
            r_budat-option = 'BT'.
            APPEND r_budat.

          ELSEIF et_budget-eindt+4(4) >= '0401' AND
                  et_budget-eindt+4(4) <= '0630'.

            lv_month = '04'.

            r_budat-low    = et_budget-gjahr && '0401'.
            r_budat-high   = et_budget-gjahr && '0630'.
            r_budat-sign   = 'I'.
            r_budat-option = 'BT'.
            APPEND r_budat.

          ELSEIF et_budget-eindt+4(4) >= '0701' AND
                   et_budget-eindt+4(4) <= '0930'.

            lv_month = '07'.

            r_budat-low    = et_budget-gjahr && '0701'.
            r_budat-high   = et_budget-gjahr && '0930'.
            r_budat-sign   = 'I'.
            r_budat-option = 'BT'.
            APPEND r_budat.

          ELSEIF et_budget-eindt+4(4) >= '1001' AND
                   et_budget-eindt+4(4) <= '1231'.

            lv_month = '10'.

            r_budat-low    = et_budget-gjahr && '1001'.
            r_budat-high   = et_budget-gjahr && '1231'.
            r_budat-sign   = 'I'.
            r_budat-option = 'BT'.
            APPEND r_budat.

          ENDIF.

        WHEN '3'.

          lv_ntime = 6.

          IF et_budget-eindt+4(4) >= '0101' AND
             et_budget-eindt+4(4) <= '0630'.

            lv_month = '01'.

            r_budat-low    = et_budget-gjahr && '0101'.
            r_budat-high   = et_budget-gjahr && '0630'.
            r_budat-sign   = 'I'.
            r_budat-option = 'BT'.
            APPEND r_budat.

          ELSEIF et_budget-eindt+4(4) >= '0701' AND
                  et_budget-eindt+4(4) <= '1231'.

            lv_month = '07'.

            r_budat-low    = et_budget-gjahr && '0701'.
            r_budat-high   = et_budget-gjahr && '1231'.
            r_budat-sign   = 'I'.
            r_budat-option = 'BT'.
            APPEND r_budat.

          ENDIF.

        WHEN '4'.

          lv_ntime = 12.
          lv_month = '01'.

          r_budat-low    = et_budget-gjahr && '0101'.
          r_budat-high   = et_budget-gjahr && '1231'.
          r_budat-sign   = 'I'.
          r_budat-option = 'BT'.
          APPEND r_budat.

        WHEN '5'.

          MODIFY et_budget.
          CONTINUE.

      ENDCASE.

    ELSE.
      e_result = 'E'.
      MESSAGE s000(zco01) WITH TEXT-e01 et_budget-posid
      INTO e_msg.
      EXIT.
    ENDIF.

    DO lv_ntime TIMES.
      lt_month-v1 = lv_month.
      APPEND lt_month.
      ADD 1 TO lv_month.
    ENDDO.

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
     AND rvers  = 'B1'
     AND ryear  = @et_budget-gjahr
     AND robjnr = @lv_objnr
     AND rkokrs = @gc_kokrs
     AND rkstar = @lv_kstar
    GROUP BY robjnr.

*-- 실적
    SELECT objnr,
           SUM( wkg001 ) AS wkg001, SUM( wkg002 ) AS wkg002,
           SUM( wkg003 ) AS wkg003, SUM( wkg004 ) AS wkg004,
           SUM( wkg005 ) AS wkg005, SUM( wkg006 ) AS wkg006,
           SUM( wkg007 ) AS wkg007, SUM( wkg008 ) AS wkg008,
           SUM( wkg009 ) AS wkg009, SUM( wkg010 ) AS wkg010,
           SUM( wkg011 ) AS wkg011, SUM( wkg012 ) AS wkg012
      INTO TABLE @DATA(lt_cosp)
      FROM cosp
     WHERE lednr = '00'
       AND versn = '000'
       AND wrttp IN ('04', '60', '21', '22')
       AND gjahr = @et_budget-gjahr
       AND kstar = @lv_kstar
       AND objnr = @lv_objnr
       AND vrgng <> 'SDOR'
     GROUP BY objnr.

    SELECT objnr,
           SUM( wkg001 ) AS wkg001, SUM( wkg002 ) AS wkg002,
           SUM( wkg003 ) AS wkg003, SUM( wkg004 ) AS wkg004,
           SUM( wkg005 ) AS wkg005, SUM( wkg006 ) AS wkg006,
           SUM( wkg007 ) AS wkg007, SUM( wkg008 ) AS wkg008,
           SUM( wkg009 ) AS wkg009, SUM( wkg010 ) AS wkg010,
           SUM( wkg011 ) AS wkg011, SUM( wkg012 ) AS wkg012
      INTO TABLE @DATA(lt_cosp_rku)
      FROM cosp
     WHERE lednr = '00'
       AND versn = '000'
       AND wrttp IN ('04', '60', '21', '22')
       AND gjahr = @et_budget-gjahr
       AND kstar = @lv_kstar
       AND objnr = @lv_objnr
       AND vrgng = 'RKU1'
     GROUP BY objnr.

    SELECT objnr,
           SUM( hsl ) AS hsl
      INTO TABLE @DATA(lt_acdoca)
      FROM acdoca
     WHERE rldnr = '0L'
       AND gjahr = @et_budget-gjahr
       AND kokrs = @gc_kokrs
       AND racct = @lv_kstar
       AND objnr = @lv_objnr
       AND budat IN @r_budat
       AND blart IN ('DD', 'SS')
     GROUP BY objnr.

*-- 1년 예산
    CLEAR lv_month.

    READ TABLE lt_zcot0040
     ASSIGNING FIELD-SYMBOL(<ls_zcot0040>) INDEX 1.

    IF sy-subrc = 0.
      DO 12 TIMES.
        ADD 1 TO lv_month.
        lv_fieldname = '<LS_ZCOT0040>-HSL' && lv_month.
        ASSIGN (lv_fieldname) TO FIELD-SYMBOL(<fs_hsl>).
        et_budget-hslyear = et_budget-hslyear + <fs_hsl>.
      ENDDO.
    ENDIF.

    READ TABLE lt_zcot0040
     ASSIGNING FIELD-SYMBOL(<ls_zcot0040_perio>) INDEX 1.

    READ TABLE lt_cosp
     ASSIGNING FIELD-SYMBOL(<ls_cosp>) INDEX 1.

    READ TABLE lt_cosp_rku
         ASSIGNING FIELD-SYMBOL(<ls_cosp_rku>) INDEX 1.

    READ TABLE lt_acdoca ASSIGNING FIELD-SYMBOL(<ls_acdoca>)
                   INDEX 1.

*-- 기간 실적및 예산
    LOOP AT lt_month.

      IF <ls_zcot0040_perio> IS ASSIGNED.
        lv_fieldname = '<LS_ZCOT0040_PERIO>-HSL' && lt_month-v1.
        ASSIGN (lv_fieldname) TO FIELD-SYMBOL(<fs_hslperio>).
        et_budget-hslperio = et_budget-hslperio + <fs_hslperio>.

      ENDIF.

*-- COSP
      IF <ls_cosp> IS ASSIGNED.
        lv_fieldname = '<LS_COSP>-WKG0' && lt_month-v1.

        ASSIGN (lv_fieldname) TO <fs_wkg>.
        lv_dsum = lv_dsum + <fs_wkg>.
      ENDIF.

*-- RKU
      IF <ls_cosp_rku> IS ASSIGNED.
        lv_fieldname = '<LS_COSP_RKU>-WKG0' && lt_month-v1.

        ASSIGN (lv_fieldname) TO <fs_wkg>.
        lv_fsum = lv_fsum + <fs_wkg>.
      ENDIF.

    ENDLOOP.

    IF <ls_acdoca> IS ASSIGNED.
      lv_fsum = lv_fsum + <ls_acdoca>-hsl.
    ENDIF.

    et_budget-hslspend = lv_dsum - lv_fsum.

    MODIFY et_budget.

    UNASSIGN: <ls_zcot0040_perio>, <ls_cosp>,
              <ls_cosp_rku>, <ls_acdoca>.

    CLEAR: lt_zcot0040[], lt_cosp[], lt_cosp_rku[], lt_acdoca[].

  ENDLOOP.

  IF e_result IS INITIAL.
    e_result = 'S'.
    MESSAGE s005(zco01) INTO e_msg.
  ENDIF.

ENDFUNCTION.
