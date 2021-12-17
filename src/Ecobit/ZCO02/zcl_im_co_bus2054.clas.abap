class ZCL_IM_CO_BUS2054 definition
  public
  final
  create public .

public section.

  interfaces IF_EX_BAPIEXT_BUS2054 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_CO_BUS2054 IMPLEMENTATION.


  method IF_EX_BAPIEXT_BUS2054~CHANGE_EXIT1.
  endmethod.


  method IF_EX_BAPIEXT_BUS2054~CHANGE_EXIT2.
  endmethod.


  METHOD IF_EX_BAPIEXT_BUS2054~CREATE_EXIT1.

DEFINE __APPEND_ERROR_MESSAGE.

  APPEND INITIAL LINE TO RETURN ASSIGNING <FS_RETURN>.

  IF SY-SUBRC EQ 0.

    " & 필드는 필수입니다.
    <FS_RETURN> = VALUE #(
      TYPE        = 'E'
      ID          = 'ZCO01'
      NUMBER      = '026'
      MESSAGE_V1  = &2
      FIELD       = &1
    ).

    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        MSGID               = <FS_RETURN>-ID
        MSGNR               = <FS_RETURN>-NUMBER
        MSGV1               = <FS_RETURN>-MESSAGE_V1
      IMPORTING
        MESSAGE_TEXT_OUTPUT = <FS_RETURN>-MESSAGE.

    UNASSIGN <FS_RETURN>.
  ENDIF.

END-OF-DEFINITION.
DEFINE __CHECK_CUSTOM_FIELD.

  ASSIGN COMPONENT &1 OF STRUCTURE <FS_CUSTOM_FIELDS> TO <FS_VALUE>.

  IF SY-SUBRC EQ 0.
    IF <FS_VALUE> IS INITIAL.
      __APPEND_ERROR_MESSAGE &1 &2.
    ENDIF.

    UNASSIGN <FS_VALUE>.
  ENDIF.

END-OF-DEFINITION.

    DATA LS_PROJECT             TYPE BAPI_BUS2001_NEW.
    DATA LS_PROJECT_DETAIL      TYPE BAPI_BUS2001_DETAIL.
    DATA LR_CUSTOM_FIELDS       TYPE REF TO DATA.
    DATA LV_FIELD               TYPE FIELDNAME.

    FIELD-SYMBOLS <FS_RETURN>   TYPE BAPIRET2.
    FIELD-SYMBOLS <FS_VALUE>    TYPE ANY.

    " 시작일자 ~ 종료일자 필수입력 점검
    " T-마켓에서 시작일자~종료일자 사이에 현재일자일 경우만 사용되어
    " 공란일 경우에 사용할 수가 업어서 필수체크한다.

    LOOP AT I_WBS_ELEMENT_TABLE INTO DATA(LS_WBS).
      IF LS_WBS-WBS_BASIC_FINISH_DATE IS INITIAL.
        __APPEND_ERROR_MESSAGE 'PSTRT' '시작일자'.
      ENDIF.
    ENDLOOP.


    " 설비 WBS 만 필수 여부 점검
    " 이미 생성된 프로젝트인지 점검
    CALL FUNCTION 'BAPI_BUS2001_GETDATA'
      EXPORTING
        I_PROJECT_DEFINITION = I_PROJECT_DEFINITION
      IMPORTING
        E_PROJECT_DEFINITION = LS_PROJECT_DETAIL.

    IF LS_PROJECT_DETAIL IS INITIAL.
      " 생성된 프로젝트가 아니라면 BAPI_BUS2001_CREATE 로 생성하면서
      " BUS2054 도 생성할 수 있으므로, ABAP 메모리 데이터 점검
      IMPORT BAPI_BUS2001_NEW = LS_PROJECT FROM MEMORY ID 'ZCO_BUS2001'.
    ELSE.
      LS_PROJECT = CORRESPONDING #( LS_PROJECT_DETAIL ).
    ENDIF.

    " 가져온 데이터가 현재 WBS 요소의 프로젝트 정의와 동일한지 확인
    " 설비 WBS 만 수행하도록 조건
    CHECK LS_PROJECT-PROJECT_DEFINITION EQ I_PROJECT_DEFINITION
      AND LS_PROJECT-PROJECT_PROFILE    EQ 'Z000003'
      AND EXTENSIONIN[] IS NOT INITIAL.

    " 확장필드 정보
    LOOP AT EXTENSIONIN INTO DATA(LS_IN).

      IF LR_CUSTOM_FIELDS IS NOT INITIAL.
        FREE LR_CUSTOM_FIELDS.
      ENDIF.

      CREATE DATA LR_CUSTOM_FIELDS TYPE (LS_IN-STRUCTURE).
      ASSIGN LR_CUSTOM_FIELDS->* TO FIELD-SYMBOL(<FS_CUSTOM_FIELDS>).

      CHECK SY-SUBRC EQ 0.

      " 확장필드 구조에 값 전달
      <FS_CUSTOM_FIELDS> = LS_IN-VALUEPART1.


      " 필수필드 체크
      __CHECK_CUSTOM_FIELD : 'ZZIZW' '투자사유',
                             'ZZCD1' '설비분류코드(대분류)',
                             'ZZWBT' 'WBS 유형',
                             'ZZCYP' '통제유형'.
    ENDLOOP.

  ENDMETHOD.


  method IF_EX_BAPIEXT_BUS2054~CREATE_EXIT2.
  endmethod.


  method IF_EX_BAPIEXT_BUS2054~DELETE_EXIT1.
  endmethod.


  method IF_EX_BAPIEXT_BUS2054~DELETE_EXIT2.
  endmethod.


  method IF_EX_BAPIEXT_BUS2054~GETDATA_EXIT1.
  endmethod.


  method IF_EX_BAPIEXT_BUS2054~GETDATA_EXIT2.
  endmethod.
ENDCLASS.
