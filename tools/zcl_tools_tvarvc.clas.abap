CLASS zcl_tools_tvarvc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: tyt_tvarvc TYPE STANDARD TABLE OF tvarvc.
    CONSTANTS:
      BEGIN OF c_s_para,
        dummy TYPE rvari_vnam VALUE '',
      END OF c_s_para .
    CONSTANTS:
      BEGIN OF c_s_sele,
        dummy                   TYPE rvari_vnam VALUE '',
      END OF c_s_sele .

    CLASS-METHODS read_para
      IMPORTING
        !iv_name TYPE rvari_vnam
        !iv_numb TYPE tvarv_numb DEFAULT 0000
      EXPORTING
        !ev_val  TYPE rvari_val_255 .
    CLASS-METHODS read_sele
      IMPORTING
        !iv_name  TYPE rvari_vnam
      EXPORTING
        !er_range TYPE STANDARD TABLE .
    CLASS-METHODS read_tvarvc
      IMPORTING
        !iv_name  TYPE rvari_vnam DEFAULT '*'
        !iv_type  TYPE rsscr_kind DEFAULT '*'
      EXPORTING
        !et_table TYPE tyt_tvarvc .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TOOLS_TVARVC IMPLEMENTATION.


  METHOD read_para.
    CLEAR ev_val.

    SELECT SINGLE low FROM tvarvc INTO ev_val
           WHERE name EQ iv_name
           AND   type = 'P'
           AND   numb EQ iv_numb.
  ENDMETHOD.


  METHOD read_sele.
    CLEAR er_range.

    DATA lt_tvarvc TYPE STANDARD TABLE OF tvarvc.
    SELECT * FROM tvarvc INTO TABLE lt_tvarvc
             WHERE name EQ iv_name AND
                   type EQ 'S'.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    FIELD-SYMBOLS <ls_line> TYPE any.
    FIELD-SYMBOLS <l_comp> TYPE any.

    DATA lo_data TYPE REF TO data.
    CREATE DATA lo_data LIKE LINE OF er_range.
    ASSIGN lo_data->* TO <ls_line>.

    LOOP AT lt_tvarvc ASSIGNING FIELD-SYMBOL(<ls_t>).
      ASSIGN COMPONENT 'OPTION' OF STRUCTURE <ls_line> TO <l_comp>.
      IF sy-subrc EQ 0. <l_comp> = <ls_t>-opti. ENDIF.

      ASSIGN COMPONENT 'SIGN' OF STRUCTURE <ls_line> TO <l_comp>.
      IF sy-subrc EQ 0. <l_comp> = <ls_t>-sign. ENDIF.

      ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_line> TO <l_comp>.
      IF sy-subrc EQ 0. <l_comp> = <ls_t>-low. ENDIF.

      ASSIGN COMPONENT 'HIGH' OF STRUCTURE <ls_line> TO <l_comp>.
      IF sy-subrc EQ 0. <l_comp> = <ls_t>-high. ENDIF.

      IF <ls_line> IS NOT INITIAL.
        APPEND <ls_line> TO er_range.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD read_tvarvc.
    CLEAR et_table.

    DATA:
      lr_name TYPE RANGE OF rvari_vnam,
      lr_type TYPE RANGE OF rsscr_kind.

    APPEND INITIAL LINE TO lr_name ASSIGNING FIELD-SYMBOL(<n>).
    <n> = 'IEQ'.
    IF iv_name CA '*'. <n> = 'ICP'. ENDIF.
    <n>-low = iv_name.

    APPEND INITIAL LINE TO lr_type ASSIGNING FIELD-SYMBOL(<t>).
    <t> = 'IEQ'.
    IF iv_type CA '*'. <t> = 'ICP'. ENDIF.
    <t>-low = iv_type.

    SELECT * FROM tvarvc INTO TABLE et_table
             WHERE name IN lr_name
             AND   type IN lr_type.
  ENDMETHOD.
ENDCLASS.
