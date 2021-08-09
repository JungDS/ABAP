FUNCTION zco_gw_budget_data_send.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_GWKEY) TYPE  ZEGWKEY
*"     VALUE(I_GUBUN) TYPE  ZEGWTYP
*"  EXPORTING
*"     VALUE(E_RESULT) TYPE  CHAR1
*"     VALUE(E_MSG) TYPE  CHAR255
*"  TABLES
*"      T_PERIOD STRUCTURE  ZCOS0310
*"      T_ALIENATION STRUCTURE  ZCOS0311
*"      T_INCRAMOUNT STRUCTURE  ZCOS0312
*"----------------------------------------------------------------------

  DATA lv_fieldname TYPE fieldname.
  DATA lv_month TYPE n LENGTH 2.

*add bsgsm_fcm 20210512
*  100 단위 미만 절사되어 누적 오류 수정 프리텍 이은지d 요청
  DATA : ls_period      TYPE zcos0310b.
  DATA : ls_alienation  TYPE zcos0311b.
  DATA : ls_incramount  TYPE zcos0312b.
*end by  bsgsm_fcm 20210512


  SELECT b~gwkey, b~rseq, b~process_9,
         b~rldnr, b~rrcty, b~rvers, b~ryear, rkokrs,
         b~robjnr, b~rkstar, b~famount,
         hsl01, hsl02, hsl03, hsl04, hsl05, hsl06,
         hsl07, hsl08, hsl09, hsl10, hsl11, hsl12
    INTO TABLE @DATA(lt_zcot1200)
    FROM zcot1190 AS a
   INNER JOIN zcot1200 AS b
      ON a~gwkey = b~gwkey
   WHERE b~rldnr = '00'
     AND b~rrcty = '1'
     AND b~rvers = 'B1'
     AND a~gwkey = @i_gwkey
     AND a~gwtyp = @i_gubun
     AND a~gwsts <> 'Z9'.
  IF sy-subrc <> 0.
    e_result = 'E'.
    MESSAGE s004(zco01) INTO e_msg.
    EXIT.
  ENDIF.

  CASE i_gubun.

    WHEN 'P'.   "예산전용 기간

      LOOP AT lt_zcot1200 INTO DATA(ls_zcot1200)
            WHERE process_9 = 'RECV'.

        MOVE ls_zcot1200-rkstar TO t_period-kstar.

        PERFORM get_kstar_text USING t_period-kstar
                               CHANGING t_period-kstxt.

        CASE ls_zcot1200-robjnr(2).

          WHEN 'PR'.
            PERFORM get_post1_code_txt USING ls_zcot1200-robjnr
                                       CHANGING t_period-posid
                                                t_period-post1.
          WHEN 'KS'.

            MOVE ls_zcot1200-robjnr+6 TO t_period-kostl.

            PERFORM get_kostl_text USING      t_period-kostl
                                              ls_zcot1200-rkokrs
                                   CHANGING   t_period-ktext.
        ENDCASE.

        CLEAR lv_month.  CLEAR  ls_period.
        DO 12 TIMES.
          ADD 1 TO lv_month.
          lv_fieldname = 'LS_ZCOT1200-HSL' && lv_month.
          ASSIGN (lv_fieldname) TO FIELD-SYMBOL(<fs_hsl>).
*          t_period-amount = t_period-amount + <fs_hsl>.
          ls_period-amount  = ls_period-amount  +  <fs_hsl>.

        ENDDO.

*-- 예산은 KRW 만 입력하므로
        t_period-amount =  ls_period-amount * '100'.
*        t_period-amount = t_period-amount * '100'.

        APPEND t_period.
        CLEAR  t_period.

      ENDLOOP.

      IF sy-subrc = 0 .
        e_result = 'S'.
        MESSAGE s005(zco01) INTO e_msg.
        EXIT.

      ELSE.
        e_result = 'E'.
        MESSAGE s004(zco01) INTO e_msg.
        EXIT.

      ENDIF.

    WHEN 'A'.    "예산전용 계정

      LOOP AT lt_zcot1200 INTO ls_zcot1200.

        CASE ls_zcot1200-robjnr(2).

          WHEN 'PR'.
            PERFORM get_post1_code_txt USING     ls_zcot1200-robjnr
                                       CHANGING t_alienation-posid
                                                t_alienation-post1.
          WHEN 'KS'.

            MOVE ls_zcot1200-robjnr+6 TO t_alienation-kostl.

            PERFORM get_kostl_text USING      t_alienation-kostl
                                              ls_zcot1200-rkokrs
                                   CHANGING   t_alienation-ktext.
        ENDCASE.

        CASE ls_zcot1200-process_9.

          WHEN 'SEND'.

            MOVE ls_zcot1200-rkstar TO t_alienation-s_kstar.

            PERFORM get_kstar_text USING    t_alienation-s_kstar
                                   CHANGING t_alienation-s_kstxt.

          WHEN 'RECV'.

            MOVE ls_zcot1200-rkstar TO t_alienation-r_kstar.

            PERFORM get_kstar_text USING    t_alienation-r_kstar
                                   CHANGING t_alienation-r_kstxt.

            CLEAR lv_month.  CLEAR   ls_alienation.

            DO 12 TIMES.
              ADD 1 TO lv_month.
              lv_fieldname = 'LS_ZCOT1200-HSL' && lv_month.
              ASSIGN (lv_fieldname) TO <fs_hsl>.
              ls_alienation-amount = ls_alienation-amount + <fs_hsl>.
*              t_alienation-amount = t_alienation-amount + <fs_hsl>.
            ENDDO.

*-- 예산은 KRW 만 입력하므로
            t_alienation-amount = ls_alienation-amount * '100'.
*            t_alienation-amount = t_alienation-amount * '100'.

        ENDCASE.

        AT LAST.
          APPEND t_alienation.
          CLEAR  t_alienation.
        ENDAT.

      ENDLOOP.

      IF sy-subrc = 0 .
        e_result = 'S'.
        MESSAGE s005(zco01) INTO e_msg.
        EXIT.

      ELSE.
        e_result = 'E'.
        MESSAGE s004(zco01) INTO e_msg.
        EXIT.

      ENDIF.

    WHEN 'C'.

      LOOP AT lt_zcot1200 INTO ls_zcot1200.

        CASE ls_zcot1200-robjnr(2).

          WHEN 'PR'.
            PERFORM get_post1_code_txt USING    ls_zcot1200-robjnr
                                       CHANGING t_incramount-posid
                                                t_incramount-post1.
          WHEN 'KS'.

            MOVE ls_zcot1200-robjnr+6 TO t_incramount-kostl.

            PERFORM get_kostl_text USING      t_incramount-kostl
                                              ls_zcot1200-rkokrs
                                   CHANGING   t_incramount-ktext.
        ENDCASE.
*
*        MOVE: ls_zcot1200-rkstar  TO t_incramount-c_kstar,
*              ls_zcot1200-famount TO t_incramount-famount.

        MOVE: ls_zcot1200-rkstar  TO t_incramount-c_kstar.
        t_incramount-famount = ls_zcot1200-famount   * 100.


        PERFORM get_kstar_text USING    t_incramount-c_kstar
                               CHANGING t_incramount-c_kstxt.

        CLEAR lv_month.      CLEAR ls_incramount.

        DO 12 TIMES.
          ADD 1 TO lv_month.
          lv_fieldname = 'LS_ZCOT1200-HSL' && lv_month.
          ASSIGN (lv_fieldname) TO <fs_hsl>.
*          t_incramount-iamount = t_incramount-iamount + <fs_hsl>.
          ls_incramount-iamount = ls_incramount-iamount + <fs_hsl>.
        ENDDO.


        t_incramount-iamount =   ls_incramount-iamount * 100.



        AT LAST.
          APPEND t_incramount.
          CLEAR  t_incramount.
        ENDAT.

      ENDLOOP.

*-- 반영 후 합계
      LOOP AT t_incramount.

*-- 예산은 KRW 만 입력하므로
*        t_incramount-famount = t_incramount-famount * '100'.
*        t_incramount-iamount = t_incramount-iamount * '100'.

        t_incramount-tamount =  t_incramount-famount +
                                t_incramount-iamount.
        MODIFY t_incramount.
      ENDLOOP.

      IF sy-subrc = 0 .
        e_result = 'S'.
        MESSAGE s005(zco01) INTO e_msg.
        EXIT.

      ELSE.
        e_result = 'E'.
        MESSAGE s004(zco01) INTO e_msg.
        EXIT.

      ENDIF.

  ENDCASE.

ENDFUNCTION.
