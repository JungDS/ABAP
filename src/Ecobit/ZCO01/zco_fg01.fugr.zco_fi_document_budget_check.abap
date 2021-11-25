FUNCTION zco_fi_document_budget_check.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(IS_BKPF) TYPE  BKPF
*"  EXPORTING
*"     REFERENCE(E_TYPE) TYPE  CHAR1
*"     REFERENCE(E_MESSAGE) TYPE  BAPI_MSG
*"  TABLES
*"      T_BSEG STRUCTURE  ZFIS600
*"      T_BSEG_OLD STRUCTURE  ZFIS600
*"  EXCEPTIONS
*"      NOT_VALID_BUKRS
*"      EXCEEDING_BUDGET
*"      NOT_VALID_CODE
*"----------------------------------------------------------------------

  DATA lv_ctype TYPE zectype.
  DATA lv_cperd TYPE zecperd.

  DATA lv_month TYPE n LENGTH 2.
  DATA lv_objnr TYPE j_objnr.

  DATA: lv_ntime     TYPE i,
        lv_cpperl    TYPE pperl,
        lv_fieldname TYPE fieldname.

  DATA lv_fyear TYPE jahrperbl.

  RANGES r_budat FOR acdoca-budat.

  DATA: lv_dsum TYPE hslvt9_cs,
        lv_fsum TYPE hslvt9_cs,
        lv_asum TYPE hslvt9_cs.

  DATA: lv_dsum_char TYPE char255,
        lv_fsum_char TYPE char255,
        lv_asum_char TYPE char255.

  FIELD-SYMBOLS: <fs_hsl> TYPE any,
                 <fs_wkg> TYPE any.

*  DATA LV_DMBTR_OLD TYPE DMBTR.
*  DATA LV_DMBTR_NEW TYPE DMBTR.

  DATA lv_dmbtr_old TYPE fins_vhcur12.  " MODI BY JSY
  DATA lv_dmbtr_new TYPE fins_vhcur12.

  RANGES r_kstar FOR cska-kstar.

  DATA: BEGIN OF lt_month OCCURS 0,
          v1 TYPE n LENGTH 2,
        END OF lt_month.

  SELECT SINGLE kokrs INTO @DATA(lv_kokrs)
    FROM tka02
   WHERE bukrs = @is_bkpf-bukrs.

  IF sy-subrc <> 0.
    RAISE not_valid_bukrs.
    EXIT.
  ENDIF.


  DATA lt_bseg_new TYPE TABLE OF zfis600  WITH HEADER LINE.
*  DATA LT_BSEG_NEW TYPE TABLE OF BSEG WITH HEADER LINE.

*-- 예산유형별 통제 원가요소
*  SELECT * FROM ZCOT0010
*    INTO TABLE @DATA(LT_ZCOT0010)
*    WHERE GJAHR  = @IS_BKPF-GJAHR
*      AND KOKRS  = @LV_KOKRS.

  SELECT * FROM zcot0010
    INTO TABLE @DATA(lt_zcot0010)
    WHERE bukrs =  @is_bkpf-bukrs  " ADD BSGSM_FCM 2021.09.01
      AND gjahr  = @is_bkpf-gjahr
      AND kokrs  = @lv_kokrs.





  lt_bseg_new[] = t_bseg[].

  LOOP AT t_bseg.

    CLEAR lv_objnr.
    CLEAR: lv_dsum, lv_fsum, lv_asum,
           r_budat, r_budat[],
           r_kstar, r_kstar[],
           lv_dmbtr_new, lv_dmbtr_old,
           lv_cperd.

    CLEAR: lt_month, lt_month[].

    IF t_bseg-kostl IS NOT INITIAL.

      lv_ctype = '1'.   "부서예산

*-- 예산통제 제외
      SELECT SINGLE kostl INTO @DATA(lv_kostl)
        FROM zcot0030
       WHERE gjahr  = @is_bkpf-gjahr  " ADD BY JSY  200108
         AND kokrs = @lv_kokrs
         AND kostl = @t_bseg-kostl
         AND bexcl = @abap_true.


* 20200108919_1
*  강현수K REQ 년초에  예산통제가 안되고 있음..
*  원인은 2019년 통제로 등록 100045 로  2020년이 통제 안되었음.
**            SELECT SINGLE KOSTL INTO @DATA(LV_KOSTL)
**        FROM ZCOT0030
**       WHERE KOKRS = @LV_KOKRS
**         AND KOSTL = @T_BSEG-KOSTL
**         AND BEXCL = @ABAP_TRUE.
**


      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      lv_objnr = 'KS' && lv_kokrs && t_bseg-kostl.

      MOVE: 'I'          TO r_kstar-sign,
            'EQ'         TO r_kstar-option,
            t_bseg-hkont TO r_kstar-low.

      APPEND r_kstar.

    ELSEIF t_bseg-projk IS NOT INITIAL.

      SELECT SINGLE * INTO @DATA(ls_prps)
        FROM prps
       WHERE pspnr = @t_bseg-projk.

      IF sy-subrc <> 0.
        RAISE not_valid_code.
        EXIT.
      ENDIF.

      lv_objnr = ls_prps-objnr.

*-- 예산통제 제외
      SELECT SINGLE posid INTO @DATA(lv_posid)
        FROM zcot0030
       WHERE kokrs = @lv_kokrs
         AND posid = @ls_prps-posid
         AND bexcl = @abap_true.

      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      CASE ls_prps-zzcyp.   "통제유형

        WHEN '1' OR '2'.

          lv_ctype = ls_prps-zzcyp.

          MOVE: 'I'          TO r_kstar-sign,
                'EQ'         TO r_kstar-option,
                t_bseg-hkont TO r_kstar-low.

          APPEND r_kstar.

        WHEN '3'.   "공사통제 유형

          lv_ctype = ls_prps-zzcyp.

          LOOP AT lt_zcot0010 INTO DATA(ls_zcot0010)
                             WHERE ctype = lv_ctype
                               AND cperd = '4'.      "공사는 항상 연기준

            MOVE: 'I'                TO r_kstar-sign,
                  'EQ'               TO r_kstar-option,
                  ls_zcot0010-fkstar TO r_kstar-low.

            APPEND r_kstar.

          ENDLOOP.

        WHEN OTHERS.   "비통제
          CONTINUE.
      ENDCASE.

    ENDIF.

*-- 예산점검 마스터(ZCOT0010)
    CASE lv_ctype.

      WHEN '3'.  "공사통제유형
        IF r_kstar[] IS INITIAL.
          CONTINUE.
        ENDIF.

        lv_cperd = '4'.

      WHEN  OTHERS.
        READ TABLE lt_zcot0010 INTO ls_zcot0010
             WITH KEY ctype  = lv_ctype
                      fkstar = t_bseg-hkont.

        IF sy-subrc = 0 .
          lv_cperd = ls_zcot0010-cperd.
        ELSE.
          CONTINUE.
        ENDIF.

    ENDCASE.

    CASE lv_cperd.

      WHEN '1'.   "월기준

        lv_ntime = 1.
        lv_month = is_bkpf-budat+4(2).

        r_budat-low    = is_bkpf-budat(4) && '0101'.

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

      WHEN '2'.   "분기 기준

        lv_ntime = 3.

        IF is_bkpf-budat+4(4) >= '0101' AND
           is_bkpf-budat+4(4) <= '0331'.

          lv_month = '01'.

          r_budat-low    = is_bkpf-gjahr && '0101'.
          r_budat-high   = is_bkpf-gjahr && '0331'.
          r_budat-sign   = 'I'.
          r_budat-option = 'BT'.
          APPEND r_budat.

        ELSEIF is_bkpf-budat+4(4) >= '0401' AND
                is_bkpf-budat+4(4) <= '0630'.

          lv_month = '04'.

          r_budat-low    = is_bkpf-gjahr && '0401'.
          r_budat-high   = is_bkpf-gjahr && '0630'.
          r_budat-sign   = 'I'.
          r_budat-option = 'BT'.
          APPEND r_budat.

        ELSEIF is_bkpf-budat+4(4) >= '0701' AND
                 is_bkpf-budat+4(4) <= '0930'.

          lv_month = '07'.

          r_budat-low    = is_bkpf-gjahr && '0701'.
          r_budat-high   = is_bkpf-gjahr && '0930'.
          r_budat-sign   = 'I'.
          r_budat-option = 'BT'.
          APPEND r_budat.

        ELSEIF is_bkpf-budat+4(4) >= '1001' AND
                 is_bkpf-budat+4(4) <= '1231'.

          lv_month = '10'.

          r_budat-low    = is_bkpf-gjahr && '1001'.
          r_budat-high   = is_bkpf-gjahr && '1231'.
          r_budat-sign   = 'I'.
          r_budat-option = 'BT'.
          APPEND r_budat.

        ENDIF.

      WHEN '3'.   "반기 기준

        lv_ntime = 6.

        IF is_bkpf-budat+4(4) >= '0101' AND
           is_bkpf-budat+4(4) <= '0630'.

          lv_month = '01'.

          r_budat-low    = is_bkpf-gjahr && '0101'.
          r_budat-high   = is_bkpf-gjahr && '0630'.
          r_budat-sign   = 'I'.
          r_budat-option = 'BT'.
          APPEND r_budat.

        ELSEIF is_bkpf-budat+4(4) >= '0701' AND
                is_bkpf-budat+4(4) <= '1231'.

          lv_month = '07'.

          r_budat-low    = is_bkpf-gjahr && '0701'.
          r_budat-high   = is_bkpf-gjahr && '1231'.
          r_budat-sign   = 'I'.
          r_budat-option = 'BT'.
          APPEND r_budat.

        ENDIF.

      WHEN '4'.   "연 기준

        lv_ntime = 12.
        lv_month = '01'.

        r_budat-low    = is_bkpf-gjahr && '0101'.
        r_budat-high   = is_bkpf-gjahr && '1231'.
        r_budat-sign   = 'I'.
        r_budat-option = 'BT'.
        APPEND r_budat.

      WHEN '5'.   "기준 없음(비통제 대상)
        CONTINUE.

    ENDCASE.

    DO lv_ntime TIMES.
      lt_month-v1 = lv_month.
      APPEND lt_month.
      ADD 1 TO lv_month.
    ENDDO.

*-- 계획
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
     AND ryear  =   @is_bkpf-gjahr
     AND robjnr =   @lv_objnr
     AND rkokrs =   @lv_kokrs
     AND rkstar IN  @r_kstar
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
       AND gjahr = @is_bkpf-gjahr
       AND kstar IN  @r_kstar
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
       AND gjahr = @is_bkpf-gjahr
       AND kstar IN  @r_kstar
       AND objnr = @lv_objnr
       AND vrgng = 'RKU1'
     GROUP BY objnr, kstar.

    SELECT objnr,
           SUM( hsl ) AS hsl
      INTO TABLE @DATA(lt_acdoca)
      FROM acdoca
     WHERE rldnr = '0L'
       AND rbukrs = @is_bkpf-bukrs "ADD BSGSM_FCM  2021.09.01
       AND gjahr = @is_bkpf-gjahr
       AND kokrs = @lv_kokrs
       AND racct IN  @r_kstar
       AND objnr = @lv_objnr
       AND budat IN @r_budat
       AND blart IN ('DD', 'SS')
     GROUP BY objnr.

    READ TABLE lt_zcot0040 ASSIGNING FIELD-SYMBOL(<ls_zcot0040>)
                           INDEX 1.
    IF sy-subrc = 0.

      LOOP AT lt_month.

        lv_fieldname = '<LS_ZCOT0040>-HSL' && lt_month-v1.

        ASSIGN (lv_fieldname) TO <fs_hsl>.
        lv_dsum = lv_dsum + <fs_hsl>.

*-- COSP
        READ TABLE lt_cosp ASSIGNING FIELD-SYMBOL(<ls_cosp>)
                    WITH KEY objnr = <ls_zcot0040>-robjnr.

        IF sy-subrc = 0.
          lv_fieldname = '<LS_COSP>-WKG0' && lt_month-v1.

          ASSIGN (lv_fieldname) TO <fs_wkg>.
          lv_fsum = lv_fsum + <fs_wkg>.
        ENDIF.

*-- RKU
        READ TABLE lt_cosp_rku ASSIGNING FIELD-SYMBOL(<ls_cosp_rku>)
                    WITH KEY objnr = <ls_zcot0040>-robjnr.

        IF sy-subrc = 0.
          lv_fieldname = '<LS_COSP_RKU>-WKG0' && lt_month-v1.

          ASSIGN (lv_fieldname) TO <fs_wkg>.
          lv_fsum = lv_fsum - <fs_wkg>.
        ENDIF.

      ENDLOOP.

      LOOP AT lt_bseg_new WHERE kostl = t_bseg-kostl
                             AND projk = t_bseg-projk
                             AND hkont IN r_kstar.

        lv_dmbtr_new =  lv_dmbtr_new + lt_bseg_new-dmbtr.

      ENDLOOP.

      LOOP AT t_bseg_old WHERE kostl = t_bseg-kostl
                            AND projk = t_bseg-projk
                            AND hkont IN r_kstar.

        lv_dmbtr_old =  lv_dmbtr_old + t_bseg_old-dmbtr.

      ENDLOOP.

      READ TABLE lt_acdoca ASSIGNING FIELD-SYMBOL(<ls_acdoca>)
                    WITH KEY objnr = <ls_zcot0040>-robjnr.

      IF sy-subrc = 0.
        lv_fsum = lv_fsum - <ls_acdoca>-hsl + lv_dmbtr_new
                                            - lv_dmbtr_old.
        lv_asum = lv_dsum - lv_fsum.

      ELSE.
        lv_fsum = lv_fsum + lv_dmbtr_new - lv_dmbtr_old.
        lv_asum = lv_dsum - lv_fsum.

      ENDIF.

      IF lv_asum < 0.

        e_type = 'E'.

        lv_asum = abs( lv_asum ).
        WRITE lv_asum TO lv_asum_char
                 CURRENCY is_bkpf-waers LEFT-JUSTIFIED NO-GAP.

        MESSAGE s043(zco01) WITH lv_asum_char
                            INTO e_message.
        EXIT.

      ENDIF.

    ELSE.

      e_type = 'E'.
      MESSAGE s045(zco01) INTO e_message.
      EXIT.

    ENDIF.

  ENDLOOP.

  IF e_type IS INITIAL.
    e_type = 'S'.
  ENDIF.

ENDFUNCTION.
