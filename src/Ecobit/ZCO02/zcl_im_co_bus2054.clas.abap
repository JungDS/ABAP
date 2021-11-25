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

    DATA LS_PROJECT             TYPE BAPI_BUS2001_NEW.
    DATA LS_PROJECT_DETAIL      TYPE BAPI_BUS2001_DETAIL.
    DATA LR_CUSTOM_FIELDS       TYPE REF TO DATA.
    DATA LV_FIELD               TYPE FIELDNAME.

    FIELD-SYMBOLS <FS_RETURN>   TYPE BAPIRET2.
    FIELD-SYMBOLS <FS_VALUE>    TYPE ANY.


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
    READ TABLE EXTENSIONIN INTO DATA(LS_IN) INDEX 1.

    IF LR_CUSTOM_FIELDS IS NOT INITIAL.
      FREE LR_CUSTOM_FIELDS.
    ENDIF.

    CREATE DATA LR_CUSTOM_FIELDS TYPE (LS_IN-STRUCTURE).
    ASSIGN LR_CUSTOM_FIELDS->* TO FIELD-SYMBOL(<FS_CUSTOM_FIELDS>).

    CHECK SY-SUBRC EQ 0.

    " 확장필드 구조에 값 전달
    <FS_CUSTOM_FIELDS> = LS_IN-VALUEPART1.

DEFINE __CHECK_VALUE.

  ASSIGN COMPONENT &1 OF STRUCTURE <FS_CUSTOM_FIELDS> TO <FS_VALUE>.

  IF SY-SUBRC EQ 0.
    IF <FS_VALUE> IS INITIAL.

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
    ENDIF.

    UNASSIGN <FS_VALUE>.
  ENDIF.

END-OF-DEFINITION.

    " 필수필드 체크
    __CHECK_VALUE : 'ZZIZW' '투자사유',
                    'ZZCD1' '설비분류코드(대분류)',
                    'ZZWBT' 'WBS 유형',
                    'ZZCYP' '통제유형'.

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
