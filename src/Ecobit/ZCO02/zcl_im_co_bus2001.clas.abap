class ZCL_IM_CO_BUS2001 definition
  public
  final
  create public .

public section.

  interfaces IF_EX_BAPIEXT_BUS2001 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_CO_BUS2001 IMPLEMENTATION.


  method IF_EX_BAPIEXT_BUS2001~CHANGE_EXIT1.
  endmethod.


  method IF_EX_BAPIEXT_BUS2001~CHANGE_EXIT2.
  endmethod.


  method IF_EX_BAPIEXT_BUS2001~CREATE_EXIT1.
    EXPORT BAPI_BUS2001_NEW = I_PROJECT_DEFINITION
        TO MEMORY ID 'ZCO_BUS2001'.
  endmethod.


  method IF_EX_BAPIEXT_BUS2001~CREATE_EXIT2.
  endmethod.


  method IF_EX_BAPIEXT_BUS2001~DELETE_EXIT1.
  endmethod.


  method IF_EX_BAPIEXT_BUS2001~DELETE_EXIT2.
  endmethod.


  method IF_EX_BAPIEXT_BUS2001~GETDATA_EXIT1.
  endmethod.


  method IF_EX_BAPIEXT_BUS2001~GETDATA_EXIT2.
  endmethod.
ENDCLASS.
