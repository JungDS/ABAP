*----------------------------------------------------------------------*
***INCLUDE ZCOR0040MBF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form BACK_ZCOT0040B
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM back_zcot0010b .




  SELECT *
    FROM zcot0010
    INTO TABLE @DATA(lt_0010).

  DATA : lt_0010b TYPE STANDARD TABLE OF zcot0010b,
         ls_0010b LIKE LINE OF lt_0010b.


  LOOP AT lt_0010 ASSIGNING FIELD-SYMBOL(<fs>).
    CLEAR ls_0010b.
    MOVE-CORRESPONDING <fs> TO ls_0010b.

    APPEND ls_0010b TO lt_0010b.

  ENDLOOP.

  IF lt_0010b[] IS NOT INITIAL.
    MODIFY zcot0010b FROM TABLE lt_0010b.
    COMMIT WORK AND WAIT.

    MESSAGE s000(zco01) WITH 'ZCOT0010 -> ZCOT0010B COPY 완료'.


  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_COT0040B_TO_0040
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_cot0010b_to_0010 .



  SELECT *
    FROM zcot0010b
    INTO TABLE @DATA(lt_0010b).

  DATA : lt_0010 TYPE STANDARD TABLE OF zcot0010,
         ls_0010 LIKE LINE OF lt_0010.


  LOOP AT lt_0010b ASSIGNING FIELD-SYMBOL(<fs>).
    CLEAR ls_0010.
    MOVE-CORRESPONDING <fs> TO ls_0010.

    APPEND ls_0010 TO lt_0010.

  ENDLOOP.

  IF lt_0010[] IS NOT INITIAL.
    MODIFY zcot0010 FROM TABLE lt_0010.
    COMMIT WORK AND WAIT.

    MESSAGE s000(zco01) WITH 'ZCOT0010B -> ZCOT0010 COPY 완료'.


  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM modify_screen .


  LOOP AT SCREEN.

    CASE 'X'.
      WHEN pa_crea OR  pa_dele OR pa_1100.

        IF   screen-group1  = 'KK'.
          screen-active = 1.

        ENDIF.
      WHEN OTHERS.
        IF   screen-group1  = 'KK'.
          screen-active = 0.

        ENDIF.
    ENDCASE.


    MODIFY SCREEN.


  ENDLOOP.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_BUKRS_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_bukrs_data .

  SELECT *
        FROM zcot0010
       WHERE kokrs = '1000'
        AND bukrs = ''
       INTO TABLE @DATA(lt_0010).

  IF  lt_0010[] IS NOT INITIAL.


    STATICS  : gt_0010 TYPE STANDARD TABLE OF zcot0010,
               gs_0010 LIKE LINE OF gt_0010.


    SELECT *
      FROM t001
      WHERE bukrs IN @so_bukrs
      INTO TABLE @DATA(lt_t001).

    CLEAR : gt_0010, gt_0010[], gs_0010.

    LOOP AT lt_t001 ASSIGNING FIELD-SYMBOL(<zz>). "회사코드
      LOOP AT lt_0010 ASSIGNING FIELD-SYMBOL(<fs>).
        CLEAR gs_0010.
        MOVE-CORRESPONDING <fs> TO gs_0010.
        gs_0010-bukrs = <zz>-bukrs.

        gs_0010-ernam = sy-uname.
        gs_0010-erzet = sy-uzeit.
        gs_0010-erdat = sy-datum.

        CLEAR :  gs_0010-aedat,
                 gs_0010-aezet,
                 gs_0010-aenam.

        APPEND gs_0010 TO gt_0010.

      ENDLOOP.
    ENDLOOP.


    IF gt_0010[] IS NOT INITIAL.

      MODIFY zcot0010 FROM TABLE gt_0010[].
      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.
        MESSAGE s000 WITH '복사 성공'.

      ELSE.
        ROLLBACK WORK.
      ENDIF.


    ENDIF.

  ELSE.
    MESSAGE s000 WITH '복사대상 자료가 없습니다. '.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form DELETE_BUKRS_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM delete_bukrs_data .

  RANGES : lr_bukrs  FOR zcot0010-bukrs.
  RANGES : lr_kokrs  FOR zcot0010-kokrs.


  REFRESH lr_bukrs.
  CLEAR lr_bukrs.


  lr_bukrs-sign = 'E'.
  lr_bukrs-option = 'EQ'.
  lr_bukrs-low = ''.
  APPEND lr_bukrs.


  IF so_bukrs IS NOT INITIAL.

    LOOP AT so_bukrs ASSIGNING FIELD-SYMBOL(<so>).

      CLEAR lr_bukrs.
      lr_bukrs-sign = 'I'.
      lr_bukrs-option = 'EQ'.
      lr_bukrs-low = <so>-low.
      lr_bukrs-high = <so>-high.
      APPEND lr_bukrs.

    ENDLOOP.
  ENDIF.


  SELECT SINGLE *
  FROM zcot0010
  WHERE kokrs = '1000'
    AND bukrs IN @lr_bukrs  " 회사코드 공백 자료 삭제 불가
    AND bukrs IN @so_bukrs
    AND gjahr = @pa_gjahr
    INTO @DATA(ls_sele).


  IF sy-subrc EQ 0.

    DELETE FROM zcot0010 WHERE  kokrs = '1000'
                           AND bukrs IN lr_bukrs
                           AND gjahr = pa_gjahr.

    IF sy-subrc EQ 0.

      COMMIT WORK AND WAIT.

      MESSAGE s000 WITH '삭제완료'.

    ELSE.

      ROLLBACK WORK .
      MESSAGE s000 WITH '삭제실패' DISPLAY LIKE 'E'..
    ENDIF.

  ELSE.

    MESSAGE s000 WITH '삭제 대상 자료가 없습니다..' DISPLAY LIKE 'E'..
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form ROLE_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GV_FLAG
*&---------------------------------------------------------------------*
FORM role_check  USING  pv.


  CLEAR gv_flag.

  SELECT  SINGLE *
   FROM agr_users
   WHERE agr_name IN ('Z_FCM_0001')  " FCM 롤
   AND uname = @sy-uname
    INTO  @DATA(ls_users).

  IF sy-subrc = 0.
    gv_flag = 'X'.
  ELSE.
    CLEAR gv_flag.
  ENDIF.




ENDFORM.
*&---------------------------------------------------------------------*
*& Form 1100_COPY&PASTE_BUKRS_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM 1100_copy$$paste_bukrs_data .
  SELECT *
          FROM zcot0010
         WHERE kokrs = '1000'
          AND bukrs = '1100'
         INTO TABLE @DATA(lt_0010).

  IF  lt_0010[] IS NOT INITIAL.

    STATICS  : gt_0010 TYPE STANDARD TABLE OF zcot0010,
               gs_0010 LIKE LINE OF gt_0010.


    SELECT *
      FROM t001
      WHERE bukrs IN @so_bukrs
      INTO TABLE @DATA(lt_t001).

    CLEAR : gt_0010, gt_0010[], gs_0010.

    LOOP AT lt_t001 ASSIGNING FIELD-SYMBOL(<zz>). "회사코드
      LOOP AT lt_0010 ASSIGNING FIELD-SYMBOL(<fs>).
        CLEAR gs_0010.
        MOVE-CORRESPONDING <fs> TO gs_0010.
        gs_0010-bukrs = <zz>-bukrs.

        gs_0010-ernam = sy-uname.
        gs_0010-erzet = sy-uzeit.
        gs_0010-erdat = sy-datum.

        CLEAR :  gs_0010-aedat,
                 gs_0010-aezet,
                 gs_0010-aenam.

        APPEND gs_0010 TO gt_0010.

      ENDLOOP.
    ENDLOOP.


    IF gt_0010[] IS NOT INITIAL.

      MODIFY zcot0010 FROM TABLE gt_0010[].
      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.
        MESSAGE s000 WITH '복사 성공'.

      ELSE.
        ROLLBACK WORK.
      ENDIF.


    ENDIF.

  ELSE.
    MESSAGE s000 WITH '복사대상 자료가 없습니다. '.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form INITIAL_SET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM initial_set .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCRFIELDS_FUNCTXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM scrfields_functxt .
  gs_funtxt-icon_id   = icon_information.
  gs_funtxt-quickinfo = 'Program Help'.

  sscrfields-functxt_01 = gs_funtxt.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCR_USER_COMMAND
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
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
