class ZCL_IM_CO_WBS_UPDATE definition
  public
  final
  create public .

public section.

  interfaces IF_EX_WORKBREAKDOWN_UPDATE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_CO_WBS_UPDATE IMPLEMENTATION.


  METHOD IF_EX_WORKBREAKDOWN_UPDATE~AT_SAVE.


**********************************************************************
* WBS POSID 가 오픈되어 사용자에 의해 변경될 가능성이 존재한다.
* 본 점검로직은 해당 이슈 발생 시 실적이 발생한 WBS에 한하여
* POSID 가 변경되지 않도록 점검하는데 목적이 있다.
**********************************************************************

    DATA: LR_OBJNR    TYPE RANGE OF PRPS-OBJNR,
          LR_OBJNR_2  TYPE RANGE OF PRPS-OBJNR,
          LS_OBJNR    LIKE LINE OF LR_OBJNR.


    DATA(LT_WBS) = IT_WBS_ELEMENT[].

    LOOP AT LT_WBS INTO DATA(LS_WBS).
      LS_OBJNR = VALUE #( SIGN = 'I'
                          OPTION = 'EQ'
                          LOW = LS_WBS-OBJNR ).
      APPEND LS_OBJNR TO LR_OBJNR.
    ENDLOOP.

    SORT LR_OBJNR BY LOW.
    DELETE ADJACENT DUPLICATES FROM LR_OBJNR COMPARING LOW.
    DELETE LR_OBJNR WHERE LOW IS INITIAL.

    CHECK  LR_OBJNR[] IS NOT INITIAL.

    SELECT OBJNR, POSID
      FROM PRPS
     WHERE OBJNR IN @LR_OBJNR
      INTO TABLE @DATA(LT_PRPS).

    CHECK SY-SUBRC EQ 0.


    LOOP AT LT_WBS INTO LS_WBS.

      READ TABLE LT_PRPS INTO DATA(LS_PRPS)
                         WITH KEY OBJNR = LS_WBS-OBJNR
                                  BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        IF LS_WBS-POSID NE LS_PRPS-POSID.
          " POSID 를 변경한 경우
          LS_OBJNR = VALUE #( SIGN = 'I'
                              OPTION = 'EQ'
                              LOW = LS_WBS-OBJNR ).
          APPEND LS_OBJNR TO LR_OBJNR_2.
        ENDIF.
      ELSE.
        "-- 신규
      ENDIF.
    ENDLOOP.

    " POSID 를 변경한 OBJNR 가 있으면,
    CHECK LR_OBJNR_2[] IS NOT INITIAL.

    " 해당 OBJNR 로 COSP 기표 이력이 있는지 점검한다.
    SELECT COUNT(*)
      FROM COSP
     WHERE OBJNR IN @LR_OBJNR_2.

    " COSP 에 실적이 존재
    CHECK SY-DBCNT GT 0.

    SY-MSGID = 'ZCO01'.
    SY-MSGNO = '063'.
    SY-MSGTY = 'E'.

    RAISE ERROR_WITH_MESSAGE.

  ENDMETHOD.


  method IF_EX_WORKBREAKDOWN_UPDATE~BEFORE_UPDATE.
  endmethod.
ENDCLASS.
