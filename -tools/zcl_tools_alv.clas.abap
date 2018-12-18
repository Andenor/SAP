CLASS zcl_tools_alv DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

*"* public components of class ZCL_TOOLS_ALV
*"* do not include other source files here!!!
  PUBLIC SECTION.
    TYPE-POOLS col .
    TYPE-POOLS slis .

    DATA grid TYPE REF TO cl_salv_table .

    INTERFACE if_salv_c_selection_mode LOAD .
    INTERFACE if_salv_c_bool_sap LOAD .
    CLASS-METHODS display
      IMPORTING
        VALUE(imp_title)                    TYPE string OPTIONAL
        VALUE(imp_selection_mode)           TYPE salv_de_constant DEFAULT if_salv_c_selection_mode=>none
        VALUE(imp_double_click_form)        TYPE char32 DEFAULT 'ON_DOUBLE_CLICK'
        VALUE(imp_hotspot_click_form)       TYPE char32 DEFAULT 'ON_HOTSPOT_CLICK'
        VALUE(imp_before_user_command_form) TYPE char32 DEFAULT 'ON_BEFORE_USER_COMMAND'
        VALUE(imp_after_user_command_form)  TYPE char32 DEFAULT 'ON_AFTER_USER_COMMAND'
        VALUE(imp_user_command_form)        TYPE char32 DEFAULT 'ON_USER_COMMAND'
        VALUE(imp_top_of_list_form)         TYPE char32 DEFAULT 'TOP_OF_LIST'
        VALUE(imp_variant)                  TYPE slis_vari OPTIONAL
        VALUE(imp_default_top_of_list)      TYPE sap_bool DEFAULT if_salv_c_bool_sap=>true
        VALUE(imp_header_col_visible)       TYPE sap_bool DEFAULT if_salv_c_bool_sap=>false
        VALUE(imp_bukrs)                    TYPE bukrs DEFAULT '9999'
      CHANGING
        !chg_table                          TYPE table
      EXCEPTIONS
        casting_error .
    METHODS f4_layouts
      CHANGING
        !chg_variant TYPE slis_vari .
    CLASS-METHODS customize_alv
      IMPORTING
        VALUE(imp_selection_mode)           TYPE char32 OPTIONAL
        VALUE(imp_double_click_form)        TYPE char32 DEFAULT 'ON_DOUBLE_CLICK'
        VALUE(imp_hotspot_click_form)       TYPE char32 DEFAULT 'ON_HOTSPOT_CLICK'
        VALUE(imp_before_user_command_form) TYPE char32 DEFAULT 'ON_BEFORE_USER_COMMAND'
        VALUE(imp_after_user_command_form)  TYPE char32 DEFAULT 'ON_AFTER_USER_COMMAND'
        VALUE(imp_user_command_form)        TYPE char32 DEFAULT 'ON_USER_COMMAND'
        VALUE(imp_variant)                  TYPE slis_vari OPTIONAL
      EXPORTING
        VALUE(ret_object)                   TYPE REF TO zcl_tools_alv
      CHANGING
        !chg_table                          TYPE table
      EXCEPTIONS
        casting_error .
    METHODS get_default_layout
      CHANGING
        !chg_variant TYPE slis_vari .
    METHODS set_line_color
      IMPORTING
        VALUE(imp_row)       TYPE salv_de_row
        VALUE(imp_color)     TYPE c DEFAULT col_positive
        VALUE(imp_color_int) TYPE i DEFAULT 0
        VALUE(imp_color_inv) TYPE i DEFAULT 0 .
    METHODS set_column_color
      IMPORTING
        VALUE(imp_column)    TYPE salv_de_column
        VALUE(imp_color)     TYPE c
        VALUE(imp_color_int) TYPE i DEFAULT 0
        VALUE(imp_color_inv) TYPE i DEFAULT 0 .
    METHODS set_cell_color
      IMPORTING
        VALUE(imp_row)       TYPE salv_de_row
        VALUE(imp_column)    TYPE salv_de_column
        VALUE(imp_color)     TYPE c
        VALUE(imp_color_int) TYPE i DEFAULT 0
        VALUE(imp_color_inv) TYPE i DEFAULT 0 .
    METHODS set_variant
      IMPORTING
        VALUE(imp_variant) TYPE slis_vari .
    METHODS set_selection_mode
      IMPORTING
        VALUE(imp_mode) TYPE salv_de_constant .
    METHODS set_double_click_form
      IMPORTING
        VALUE(imp_double_click_form) TYPE char32 .
    METHODS set_hotspot_click_form
      IMPORTING
        VALUE(imp_hotspot_click_form) TYPE char32 .
    METHODS set_before_user_command_form
      IMPORTING
        VALUE(imp_before_user_command_form) TYPE char32 .
    METHODS set_after_user_command_form
      IMPORTING
        VALUE(imp_after_user_command_form) TYPE char32 .
    METHODS set_user_command_form
      IMPORTING
        VALUE(imp_user_command_form) TYPE char32 .
    INTERFACE if_salv_c_alignment LOAD .
    METHODS set_column_attributes
      IMPORTING
        VALUE(imp_column_name)         TYPE lvc_fname
        VALUE(imp_short_text)          TYPE scrtext_s OPTIONAL
        VALUE(imp_medium_text)         TYPE scrtext_m OPTIONAL
        VALUE(imp_long_text)           TYPE scrtext_l OPTIONAL
        VALUE(imp_visible)             TYPE sap_bool DEFAULT if_salv_c_bool_sap=>false
        VALUE(imp_technical)           TYPE sap_bool DEFAULT if_salv_c_bool_sap=>true
        VALUE(imp_as_key)              TYPE sap_bool DEFAULT if_salv_c_bool_sap=>true
        VALUE(imp_as_checkbox)         TYPE sap_bool DEFAULT if_salv_c_bool_sap=>true
        VALUE(imp_as_icon)             TYPE sap_bool DEFAULT if_salv_c_bool_sap=>true
        VALUE(imp_as_hotspot)          TYPE sap_bool DEFAULT if_salv_c_bool_sap=>true
        VALUE(imp_as_exception)        TYPE sap_bool DEFAULT if_salv_c_bool_sap=>true
        VALUE(imp_exception_group)     TYPE char1 DEFAULT space
        VALUE(imp_exception_condensed) TYPE sap_bool DEFAULT if_salv_c_bool_sap=>false
        VALUE(imp_as_symbol)           TYPE sap_bool DEFAULT if_salv_c_bool_sap=>true
        VALUE(imp_alignment)           TYPE salv_de_alignment DEFAULT if_salv_c_alignment=>left
        VALUE(imp_ddic_reference)      TYPE salv_s_ddic_reference OPTIONAL
        VALUE(imp_edit_mask)           TYPE lvc_edtmsk OPTIONAL
        VALUE(imp_currency)            TYPE lvc_curr OPTIONAL
        VALUE(imp_currency_column)     TYPE lvc_cfname OPTIONAL
        VALUE(imp_decimals)            TYPE lvc_decmls OPTIONAL
        VALUE(imp_decimals_column)     TYPE lvc_dfname OPTIONAL
        VALUE(imp_f1_rollname)         TYPE lvc_roll OPTIONAL
        VALUE(imp_leading_zero)        TYPE sap_bool DEFAULT if_salv_c_bool_sap=>false
        VALUE(imp_lowercase)           TYPE sap_bool DEFAULT if_salv_c_bool_sap=>true
        VALUE(imp_optimized)           TYPE sap_bool DEFAULT if_salv_c_bool_sap=>true
        VALUE(imp_output_length)       TYPE lvc_outlen OPTIONAL
        VALUE(imp_quantity)            TYPE lvc_quan OPTIONAL
        VALUE(imp_quantity_column)     TYPE lvc_qfname OPTIONAL
        VALUE(imp_round)               TYPE lvc_round OPTIONAL
        VALUE(imp_round_column)        TYPE lvc_rndfn OPTIONAL
        VALUE(imp_sign)                TYPE sap_bool DEFAULT if_salv_c_bool_sap=>true
        VALUE(imp_tooltip)             TYPE lvc_tip OPTIONAL
        VALUE(imp_zero)                TYPE sap_bool DEFAULT if_salv_c_bool_sap=>true .
    CLASS cl_salv_tooltip DEFINITION LOAD .
    METHODS set_tooltip
      IMPORTING
        !imp_type    TYPE salv_de_constant DEFAULT cl_salv_tooltip=>c_type_exception
        !imp_value   TYPE lvc_value
        !imp_tooltip TYPE lvc_tip OPTIONAL .
    METHODS set_alv_popup
      IMPORTING
        !imp_start_column TYPE i
        !imp_end_column   TYPE i OPTIONAL
        !imp_start_line   TYPE i
        !imp_end_line     TYPE i OPTIONAL .
    INTERFACE if_salv_c_sort LOAD .
    METHODS add_sort
      IMPORTING
        !imp_columnname          TYPE lvc_fname
        !imp_position            TYPE i OPTIONAL
        !imp_sequence            TYPE salv_de_sort_sequence DEFAULT if_salv_c_sort=>sort_up
        !imp_subtotal            TYPE sap_bool DEFAULT if_salv_c_bool_sap=>false
        !imp_compressed_subtotal TYPE sap_bool DEFAULT if_salv_c_bool_sap=>true
        !imp_group               TYPE salv_de_sort_group DEFAULT if_salv_c_sort=>group_none .
    METHODS add_maximum
      IMPORTING
        !imp_columnname TYPE lvc_fname .
    METHODS add_minimum
      IMPORTING
        !imp_columnname TYPE lvc_fname .
    METHODS add_average
      IMPORTING
        !imp_columnname TYPE lvc_fname .
    METHODS add_sum
      IMPORTING
        !imp_columnname TYPE lvc_fname .
    METHODS get_current_cell
      RETURNING
        VALUE(ret_cell) TYPE salv_s_cell .
    METHODS get_selected_columns
      RETURNING
        VALUE(ret_t_cols) TYPE salv_t_column .
    METHODS get_selected_rows
      RETURNING
        VALUE(ret_t_rows) TYPE salv_t_row .
    METHODS set_screen_status
      IMPORTING
        !imp_pfstatus      TYPE sypfkey
        !imp_set_functions TYPE salv_de_constant DEFAULT 2 .
    INTERFACE if_salv_c_refresh LOAD .
    METHODS refresh
      IMPORTING
        !imp_s_stable     TYPE lvc_s_stbl OPTIONAL
        !imp_refresh_mode TYPE salv_de_constant DEFAULT if_salv_c_refresh=>soft .
    METHODS clear_sorts .
    CLASS-METHODS set_block_mode
      IMPORTING
        !imp_block_mode     TYPE sap_bool DEFAULT if_salv_c_bool_sap=>true
        !imp_header_visible TYPE sap_bool DEFAULT if_salv_c_bool_sap=>true
          PREFERRED PARAMETER imp_block_mode .
    CLASS-METHODS end_block_list .
    METHODS delete_customizing
      IMPORTING
        !imp_table TYPE table .
    METHODS set_selected_rows
      IMPORTING
        !imp_t_rows TYPE salv_t_row .
  PROTECTED SECTION.
*"* protected components of class ZCL_TOOLS_ALV
*"* do not include other source files here!!!

    METHODS constructor
      EXCEPTIONS
        casting_error .
    CLASS-METHODS get_table_desc
      IMPORTING
        !imp_table     TYPE table
      CHANGING
        !chg_report    TYPE repid
        !chg_structure TYPE tabname
      EXCEPTIONS
        casting_error .
    METHODS process_functions .
    METHODS process_layout
      IMPORTING
        VALUE(imp_variant) TYPE slis_vari OPTIONAL .
    METHODS process_display_settings .
    METHODS process_functional_settings .
    METHODS process_columns .
    METHODS process_events
      IMPORTING
        VALUE(imp_double_click_form)        TYPE char32 OPTIONAL
        VALUE(imp_hotspot_click_form)       TYPE char32
        VALUE(imp_before_user_command_form) TYPE char32
        VALUE(imp_after_user_command_form)  TYPE char32
        VALUE(imp_user_command_form)        TYPE char32 .
    METHODS process_aggregations .
    METHODS process_sorts .
    METHODS process_filters .
    METHODS process_print .
    METHODS process_top_of_list
      IMPORTING
        !imp_title TYPE string
        !imp_bukrs TYPE bukrs DEFAULT '9999' .
    METHODS process_selections .
  PRIVATE SECTION.
*"* private components of class ZCL_TOOLS_ALV
*"* do not include other source files here!!!

    CLASS-DATA:
      t_table_obj TYPE TABLE OF REF TO zcl_tools_alv .
    DATA repid TYPE repid .
    DATA structure TYPE tabname .
    DATA t_table TYPE REF TO data .
    DATA on_double_click_form TYPE char32 .
    DATA on_hotspot_click_form TYPE char32 .
    DATA on_before_user_command_form TYPE char32 .
    DATA on_after_user_command_form TYPE char32 .
    DATA on_user_command_form TYPE char32 .
    CLASS-DATA block_mode TYPE sap_bool .
    CLASS-DATA:
      t_prog_attr TYPE TABLE OF lvc_s_deta .

    CLASS-METHODS get_existing_customizing
      IMPORTING
        !imp_table        TYPE table
      RETURNING
        VALUE(ret_object) TYPE REF TO zcl_tools_alv
      EXCEPTIONS
        not_found
        casting_error .
    METHODS instanciate_alv_if_needed
      CHANGING
        !chg_table TYPE table .
    METHODS on_double_click
          FOR EVENT double_click OF cl_salv_events_table
      IMPORTING
          !row
          !column .
    METHODS on_hotspot_click
          FOR EVENT link_click OF if_salv_events_actions_table
      IMPORTING
          !row
          !column .
    METHODS on_before_user_command
          FOR EVENT before_salv_function OF cl_salv_events_table
      IMPORTING
          !e_salv_function .
    METHODS on_after_user_command
          FOR EVENT after_salv_function OF cl_salv_events_table
      IMPORTING
          !e_salv_function .
    METHODS on_user_command
          FOR EVENT added_function OF cl_salv_events_table
      IMPORTING
          !e_salv_function .
ENDCLASS.



CLASS ZCL_TOOLS_ALV IMPLEMENTATION.


  METHOD add_average.
*----------------------------------------------------------------------*
* Method: ADD_AVERAGE
* Title:  Permet de definir une aggregation : moyenne
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    DATA: lo_aggregations TYPE REF TO cl_salv_aggregations.

    lo_aggregations = grid->get_aggregations( ).

    TRY.
        CALL METHOD lo_aggregations->add_aggregation
          EXPORTING
            columnname  = imp_columnname
            aggregation = if_salv_c_aggregation=>average.
      CATCH cx_salv_data_error .
      CATCH cx_salv_not_found .
      CATCH cx_salv_existing .
    ENDTRY.

  ENDMETHOD.


  METHOD add_maximum.
*----------------------------------------------------------------------*
* Method: ADD_MAXIMUM
* Title:  Permet de definir une aggregation : maximum
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    DATA: lo_aggregations TYPE REF TO cl_salv_aggregations.

    lo_aggregations = grid->get_aggregations( ).

    TRY.
        CALL METHOD lo_aggregations->add_aggregation
          EXPORTING
            columnname  = imp_columnname
            aggregation = if_salv_c_aggregation=>maximum.
      CATCH cx_salv_data_error .
      CATCH cx_salv_not_found .
      CATCH cx_salv_existing .
    ENDTRY.

  ENDMETHOD.


  METHOD add_minimum.
*----------------------------------------------------------------------*
* Method: ADD_MINIMUM
* Title:  Permet de definir une aggregation : minimum
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    DATA: lo_aggregations TYPE REF TO cl_salv_aggregations.

    lo_aggregations = grid->get_aggregations( ).

    TRY.
        CALL METHOD lo_aggregations->add_aggregation
          EXPORTING
            columnname  = imp_columnname
            aggregation = if_salv_c_aggregation=>minimum.
      CATCH cx_salv_data_error .
      CATCH cx_salv_not_found .
      CATCH cx_salv_existing .
    ENDTRY.

  ENDMETHOD.


  METHOD add_sort.
*----------------------------------------------------------------------*
* Method: ADD_SORT
* Title:  Permet de definir un critere de tri
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    DATA: lo_sorts   TYPE REF TO cl_salv_sorts.

    lo_sorts = grid->get_sorts( ).

    TRY.
        CALL METHOD lo_sorts->add_sort
          EXPORTING
            columnname = imp_columnname
            position   = imp_position
            sequence   = imp_sequence
            subtotal   = imp_subtotal
            group      = imp_group.
**      obligatory = IF_SALV_C_BOOL_SAP=>FALSE
*    receiving
*      value      =
*      .

        IF imp_compressed_subtotal IS SUPPLIED.
          IF imp_compressed_subtotal = if_salv_c_bool_sap=>true.
            CALL METHOD lo_sorts->set_compressed_subtotal
              EXPORTING
                value = imp_columnname.
          ENDIF.
        ENDIF.


      CATCH cx_salv_not_found .
      CATCH cx_salv_existing .
      CATCH cx_salv_data_error .
    ENDTRY.

  ENDMETHOD.


  METHOD add_sum.
*----------------------------------------------------------------------*
* Method: ADD_SUM
* Title:  Permet de definir une aggregation : somme
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    DATA: lo_aggregations TYPE REF TO cl_salv_aggregations.

    lo_aggregations = grid->get_aggregations( ).

    TRY.
        CALL METHOD lo_aggregations->add_aggregation
          EXPORTING
            columnname  = imp_columnname
            aggregation = if_salv_c_aggregation=>total.
      CATCH cx_salv_data_error .
      CATCH cx_salv_not_found .
      CATCH cx_salv_existing .
    ENDTRY.

  ENDMETHOD.


  METHOD clear_sorts.
*----------------------------------------------------------------------*
* Method: CLEAR_SORTS
* Title:  (Re)initialise les tri sur l'ALV
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    DATA: lo_sorts   TYPE REF TO cl_salv_sorts.

    lo_sorts = grid->get_sorts( ).

    CALL METHOD lo_sorts->clear.

  ENDMETHOD.


  METHOD constructor.
*----------------------------------------------------------------------*
* Method: CONSTRUCTOR
* Title:  Constructeur (Instanciation privée) voir meth : customize_alv
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    APPEND me TO t_table_obj.

  ENDMETHOD.


  METHOD customize_alv.
*----------------------------------------------------------------------*
* Method: CUSTOMIZE_ALV
* Title:  Permet d'instancier un objet ZCL_TOOLS_ALV afin de le
*         parametrer
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    CREATE OBJECT ret_object.

* On recupere les infos sur la table interne
    CALL METHOD zcl_tools_alv=>get_table_desc
      EXPORTING
        imp_table     = chg_table
      CHANGING
        chg_report    = ret_object->repid
        chg_structure = ret_object->structure
      EXCEPTIONS
        casting_error = 1
        OTHERS        = 2.
    IF sy-subrc = 1.
      RAISE casting_error.
    ENDIF.

    ret_object->instanciate_alv_if_needed( CHANGING chg_table = chg_table ).

* On garde une reference dans la classe, si on veut manipuler la table
    GET REFERENCE OF chg_table INTO ret_object->t_table.

* Parametrage par defauit de l'ALV.
    ret_object->process_functions( ).
    ret_object->process_layout( imp_variant ).
    ret_object->process_display_settings( ).
    ret_object->process_functional_settings( ).
    ret_object->process_columns( ).

    ret_object->process_events( imp_double_click_form        = imp_double_click_form
                                imp_hotspot_click_form       = imp_hotspot_click_form
                                imp_before_user_command_form = imp_before_user_command_form
                                imp_after_user_command_form  = imp_after_user_command_form
                                imp_user_command_form        = imp_user_command_form ).

    ret_object->process_aggregations( ).
    ret_object->process_sorts( ).
    ret_object->process_filters( ).
    ret_object->process_print( ).
    ret_object->process_selections( ).

  ENDMETHOD.


  METHOD delete_customizing.

    DATA: lo_object  TYPE REF TO zcl_tools_alv.

    DATA: l_repid     TYPE syrepid,
          l_structure TYPE tabname.


    CALL METHOD get_table_desc
      EXPORTING
        imp_table     = imp_table
      CHANGING
        chg_report    = l_repid
        chg_structure = l_structure
      EXCEPTIONS
        casting_error = 1
        OTHERS        = 2.


    LOOP AT t_table_obj INTO lo_object.
      IF lo_object->repid = l_repid AND
         lo_object->structure = l_structure.
        DELETE t_table_obj INDEX sy-tabix.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD display.
*----------------------------------------------------------------------*
* Method: display
* Title:  Permet d'afficher sous forme ALV la table interne passé
*         en paramètre.
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* JZ15052012 - New Branding - Ajout parametre société pour Titre
*----------------------------------------------------------------------*

*
    DATA: lo_object  TYPE REF TO zcl_tools_alv.

    CALL METHOD get_existing_customizing
      EXPORTING
        imp_table     = chg_table
      RECEIVING
        ret_object    = lo_object
      EXCEPTIONS
        not_found     = 1
        casting_error = 2
        OTHERS        = 3.

    IF sy-subrc = 1.

      CALL METHOD zcl_tools_alv=>customize_alv
        EXPORTING
*         imp_selection_mode           =
*         imp_double_click_form        = 'ON_DOUBLE_CLICK'
*         imp_hotspot_click_form       = 'ON_HOTSPOT_CLICK'
*         imp_before_user_command_form = 'ON_BEFORE_USER_COMMAND'
*         imp_after_user_command_form  = 'ON_AFTER_USER_COMMAND'
*         imp_user_command_form        = 'ON_USER_COMMAND'
          imp_variant   = imp_variant
        IMPORTING
          ret_object    = lo_object
        CHANGING
          chg_table     = chg_table
        EXCEPTIONS
          casting_error = 1
          OTHERS        = 2.

    ENDIF.

    IF imp_selection_mode IS SUPPLIED.
      lo_object->set_selection_mode( imp_selection_mode ).
    ENDIF.
    IF imp_variant IS NOT INITIAL.
      lo_object->set_variant( imp_variant ).
    ENDIF.
    IF imp_default_top_of_list IS NOT INITIAL.
      lo_object->process_top_of_list( EXPORTING imp_title = imp_title
                                                imp_bukrs = imp_bukrs ). "JZ15052012
    ENDIF.
    IF imp_double_click_form IS SUPPLIED.
      lo_object->set_double_click_form( imp_double_click_form ).
    ENDIF.
    IF imp_hotspot_click_form IS SUPPLIED.
      lo_object->set_hotspot_click_form( imp_hotspot_click_form ).
    ENDIF.
    IF imp_before_user_command_form IS SUPPLIED.
      lo_object->set_before_user_command_form( imp_before_user_command_form ).
    ENDIF.
    IF imp_after_user_command_form IS SUPPLIED.
      lo_object->set_after_user_command_form( imp_after_user_command_form ).
    ENDIF.
    IF imp_user_command_form IS SUPPLIED.
      lo_object->set_user_command_form( imp_user_command_form ).
    ENDIF.

    IF imp_header_col_visible IS SUPPLIED.
      DATA: lo_columns TYPE REF TO cl_salv_columns_table.
      lo_columns = lo_object->grid->get_columns( ).

      CALL METHOD lo_columns->set_headers_visible
        EXPORTING
          value = imp_header_col_visible.
    ENDIF.


    IF block_mode = if_salv_c_bool_sap=>false.
      lo_object->grid->display( ).
    ELSE.
*  CL_SALV_CONTROLLER_METADATA

      DATA : is_layout   TYPE  slis_layout_alv,
             it_fieldcat TYPE  slis_t_fieldcat_alv,
             it_events   TYPE  slis_t_event,
             ls_events   TYPE  slis_alv_event,
             it_sort     TYPE  slis_t_sortinfo_alv,
             i_text      TYPE  slis_text40.

      DATA: lo_functional_settings TYPE REF TO cl_salv_functional_settings,
            lo_display_settings    TYPE REF TO cl_salv_display_settings,
            lo_columns_list        TYPE REF TO cl_salv_columns_list,
            lo_aggregations        TYPE REF TO cl_salv_aggregations,
            lo_sorts               TYPE REF TO cl_salv_sorts,
            lo_print               TYPE REF TO cl_salv_print,
            lo_header              TYPE REF TO cl_salv_header,
            lo_footer              TYPE REF TO cl_salv_footer.

      CALL METHOD lo_object->grid->get_functional_settings
        RECEIVING
          value = lo_functional_settings.

      CALL METHOD lo_object->grid->get_display_settings
        RECEIVING
          value = lo_display_settings.

      CALL METHOD lo_object->grid->get_columns
        RECEIVING
          value = lo_columns_list.

      CALL METHOD lo_object->grid->get_aggregations
        RECEIVING
          value = lo_aggregations.

      CALL METHOD lo_object->grid->get_sorts
        RECEIVING
          value = lo_sorts.

      CALL METHOD lo_object->grid->get_print
        RECEIVING
          value = lo_print.

      CALL METHOD cl_salv_controller_metadata=>get_slis_layout
        EXPORTING
          r_functional_settings = lo_functional_settings
          r_display_settings    = lo_display_settings
          r_columns             = lo_columns_list
          r_aggregations        = lo_aggregations
          r_sorts               = lo_sorts
          r_print               = lo_print
        CHANGING
          s_layout              = is_layout.

      CALL METHOD cl_salv_controller_metadata=>get_slis_fieldcatalog
        EXPORTING
          r_columns      = lo_columns_list
          r_aggregations = lo_aggregations
        RECEIVING
          t_fieldcatalog = it_fieldcat.

*    CALL METHOD cl_salv_controller_metadata=>get_slis_events
*      EXPORTING
*        r_header = lo_header
*        r_footer = lo_footer
*      RECEIVING
*        t_event  = it_events.

      IF imp_top_of_list_form IS SUPPLIED.
        ls_events-form = imp_top_of_list_form.
        ls_events-name = 'TOP_OF_LIST'.
        APPEND ls_events TO it_events.
      ENDIF.


      CALL METHOD cl_salv_controller_metadata=>get_slis_sort
        EXPORTING
          r_sorts = lo_sorts
*         r_sorts_slave =
        RECEIVING
          t_sort  = it_sort.


*if imp_title is SUPPLIED.
* i_text = imp_title.
*endif.


      CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_APPEND'
        EXPORTING
          is_layout                  = is_layout
          it_fieldcat                = it_fieldcat
          i_tabname                  = lo_object->structure
          it_events                  = it_events
          it_sort                    = it_sort
          i_text                     = i_text
        TABLES
          t_outtab                   = chg_table
        EXCEPTIONS
          program_error              = 1
          maximum_of_appends_reached = 2
          OTHERS                     = 3.

    ENDIF.

  ENDMETHOD.


  METHOD end_block_list.
*----------------------------------------------------------------------*
* Method: END_BLOCK_LIST
* Title:  Termine un bloc de liste ALV
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* MALAQU0P  - 23.10.2015 - IN_15_028537
*             Dump lors de l'exécution en arrière plan :
*             Accessing using a 'ZERO' object reference is not possible
*----------------------------------------------------------------------*

    CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_DISPLAY'
*   EXPORTING
*     I_INTERFACE_CHECK             = ' '
*     IS_PRINT                      =
*     I_SCREEN_START_COLUMN         = 0
*     I_SCREEN_START_LINE           = 0
*     I_SCREEN_END_COLUMN           = 0
*     I_SCREEN_END_LINE             = 0
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER       =
*     ES_EXIT_CAUSED_BY_USER        =
      EXCEPTIONS
        program_error = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


  ENDMETHOD.


  METHOD f4_layouts.
*----------------------------------------------------------------------*
* Method: F4_LAYOUTS
* Title:  Permet de proposer un matchcode des mises en forme propres
*         a l'alv en cours
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    DATA: ls_layout TYPE salv_s_layout_info,
          ls_key    TYPE salv_s_layout_key.

    ls_key-report = me->repid.
    ls_layout = cl_salv_layout_service=>f4_layouts( s_key = ls_key
                                                    restrict = if_salv_c_layout=>restrict_none ).

    chg_variant = ls_layout-layout.


  ENDMETHOD.


  METHOD get_current_cell.
*----------------------------------------------------------------------*
* Method: GET_CURRENT_CELL
* Title:  Permet de recuperer la valeur de la cellule en cours
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    DATA: lo_selections TYPE REF TO cl_salv_selections.

    lo_selections = grid->get_selections( ).
    ret_cell = lo_selections->get_current_cell( ).

  ENDMETHOD.


  METHOD get_default_layout.
*----------------------------------------------------------------------*
* Method: GET_DEFAULT_LAYOUT
* Title:  Permet de recuperer la mise en forme par défaut
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    DATA: ls_layout TYPE salv_s_layout_info,
          ls_key    TYPE salv_s_layout_key.

    ls_key-report = me->repid.
*  ls_key-handle = 'VARI'.

*  AUTHORITY-CHECK OBJECT 'S_ALV_LAYO'
*           ID 'ACTVT' FIELD '23'.
*  IF sy-subrc = 0.
    ls_layout = cl_salv_layout_service=>get_default_layout( s_key = ls_key
                                                          restrict = if_salv_c_layout=>restrict_user_independant ).
*  ELSE.
*    ls_layout = cl_salv_layout_service=>get_default_layout( s_key = ls_key
*                                                            restrict = if_salv_c_layout=>restrict_user_dependant ).
*  ENDIF.

    chg_variant = ls_layout-layout.

  ENDMETHOD.


  METHOD get_existing_customizing.
*----------------------------------------------------------------------*
* Method: GET_EXISTING_CUSTOMIZING
* Title:  Permet de recuperer le parametrage eventuellement existant
*         de l'ALV
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    DATA: lo_object  TYPE REF TO zcl_tools_alv.

    DATA: l_repid     TYPE syrepid,
          l_structure TYPE tabname,
          l_found     TYPE sap_bool.

    CALL METHOD get_table_desc
      EXPORTING
        imp_table     = imp_table
      CHANGING
        chg_report    = l_repid
        chg_structure = l_structure
      EXCEPTIONS
        casting_error = 1
        OTHERS        = 2.
    IF sy-subrc = 1.
      RAISE casting_error.
    ENDIF.

    LOOP AT t_table_obj INTO lo_object.
      IF lo_object->repid = l_repid AND
         lo_object->structure = l_structure.
        l_found = if_salv_c_bool_sap=>true.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF l_found = if_salv_c_bool_sap=>true.
      ret_object = lo_object.
    ELSE.
      RAISE not_found.
    ENDIF.


  ENDMETHOD.


  METHOD get_selected_columns.
*----------------------------------------------------------------------*
* Method: GET_SELECTED_COLUMNS
* Title:  Permet de recuperer les colonnes selectionnées
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    DATA: lo_selections TYPE REF TO cl_salv_selections.

    lo_selections = grid->get_selections( ).
    ret_t_cols = lo_selections->get_selected_columns( ).

  ENDMETHOD.


  METHOD get_selected_rows.
*----------------------------------------------------------------------*
* Method: GET_SELECTED_ROWS
* Title:  Permet de recuperer les lignes selectionnées
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    DATA: lo_selections TYPE REF TO cl_salv_selections.

    lo_selections = grid->get_selections( ).
    ret_t_rows = lo_selections->get_selected_rows( ).


  ENDMETHOD.


  METHOD get_table_desc.
*----------------------------------------------------------------------*
* Method: GET_TABLE_DESC
* Title:  Permet de recupérer la definition de la table passé en param
*         nom programme + nom structure
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    DATA: lo_desc  TYPE REF TO cl_abap_structdescr.

    DATA: l_absol TYPE char200,
          l_repid TYPE repid,
          l_struc TYPE tabname.

    DATA: ls_data  TYPE REF TO data.
    FIELD-SYMBOLS <struc> TYPE any.

    CREATE DATA ls_data LIKE LINE OF imp_table.
    ASSIGN ls_data->* TO <struc>.

    CATCH SYSTEM-EXCEPTIONS assign_casting_illegal_cast = 1.
      lo_desc ?= cl_abap_typedescr=>describe_by_data( <struc> ).
    ENDCATCH.
    IF sy-subrc = 1.
      RAISE casting_error.
    ENDIF.

*   Get program name and main type used to define table
    l_absol = lo_desc->absolute_name.
    SPLIT l_absol AT '\TYPE=' INTO l_repid l_struc.
    SHIFT l_repid UP TO '='.
    SHIFT l_repid.

    IF l_absol CP '*FUNCTION-POOL*'.
      CONCATENATE 'SAPL' l_repid INTO l_repid.
    ENDIF.

    IF l_repid CP '*\FORM*'.
      MOVE l_repid(sy-fdpos) TO l_repid.
    ENDIF.

* Program with local class (\CLAS is at end of program name)
    IF l_repid CP '*\CLA*'.                 "RFS_14_010510 (+)
      MOVE l_repid(sy-fdpos) TO l_repid.    "RFS_14_010510 (+)
    ENDIF.                                  "RFS_14_010510 (+)

*  CHECK l_struc NP '%_*'.

    IF l_repid IS INITIAL.
      l_repid = sy-cprog.
    ENDIF.

*   Set attributes
    chg_report = l_repid.
    chg_structure = l_struc.

  ENDMETHOD.


  METHOD instanciate_alv_if_needed.
*----------------------------------------------------------------------*
* Method: INSTANCIATE_ALV_IF_NEEDED
* Title:  Instancie l'objet CL_SALV_TABLE si besoin
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    DATA : lo_msg  TYPE REF TO cx_salv_msg.

    IF NOT grid IS BOUND.
* On instancie notre objet grid (type CL_SALV_TABLE)
      TRY.
          cl_salv_table=>factory(
                                  EXPORTING
                                    list_display = if_salv_c_bool_sap=>false
                                  IMPORTING
                                    r_salv_table = grid
                                  CHANGING
                                    t_table = chg_table ).
        CATCH cx_salv_msg INTO lo_msg.
          MESSAGE lo_msg TYPE 'I'.
          EXIT.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD on_after_user_command.
*----------------------------------------------------------------------*
* Method: ON_AFTER_USER_COMMAND
* Title:  Event handler pour after user command
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    IF me->on_after_user_command_form IS NOT INITIAL.
      PERFORM (me->on_after_user_command_form) IN PROGRAM (repid) USING e_salv_function IF FOUND.
    ENDIF.

  ENDMETHOD.


  METHOD on_before_user_command.
*----------------------------------------------------------------------*
* Method: ON_BEFORE_USER_COMMAND
* Title:  Event handler pour before user command
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    IF me->on_before_user_command_form IS NOT INITIAL.
      PERFORM (me->on_before_user_command_form) IN PROGRAM (repid) USING e_salv_function IF FOUND.
    ENDIF.

  ENDMETHOD.


  METHOD on_double_click.
*----------------------------------------------------------------------*
* Method: ON_DOUBLE_CLICK
* Title:  Event handler pour double clic
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

*  DATA: l_text TYPE string.
*  l_text = row.
*  CONCATENATE '(Doubleclick)' l_text column INTO l_text SEPARATED BY space.
*  MESSAGE l_text TYPE 'I'.

    IF me->on_double_click_form IS NOT INITIAL.
      PERFORM (me->on_double_click_form) IN PROGRAM (repid) USING row column IF FOUND.
    ENDIF.

  ENDMETHOD.


  METHOD on_hotspot_click.
*----------------------------------------------------------------------*
* Method: ON_HOTSPOT_CLICK
* Title:  Event handler pour hotspot clic
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    IF me->on_hotspot_click_form IS NOT INITIAL.
      PERFORM (me->on_hotspot_click_form) IN PROGRAM (repid) USING row column IF FOUND.
    ENDIF.

  ENDMETHOD.


  METHOD on_user_command.
*----------------------------------------------------------------------*
* Method: ON_USER_COMMAND
* Title:  Event handler pour user command
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    IF me->on_user_command_form IS NOT INITIAL.
      PERFORM (me->on_user_command_form) IN PROGRAM (repid) USING e_salv_function IF FOUND.
    ENDIF.

  ENDMETHOD.


  METHOD process_aggregations.
*----------------------------------------------------------------------*
* Method: PROCESS_AGGREGATIONS
* Title:  Gestion des aggregations par défaut
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*



  ENDMETHOD.


  METHOD process_columns.
*----------------------------------------------------------------------*
* Method: PROCESS_COLUMNS
* Title:  Gestion des colonnes par défaut
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    DATA: lo_columns TYPE REF TO cl_salv_columns_table,
          lo_column  TYPE REF TO cl_salv_column.

    lo_columns = grid->get_columns( ).

* Optimisation des colonnes
    lo_columns->set_optimize( if_salv_c_bool_sap=>true ).

*... set cell type column for manipulating cells
    TRY.
        lo_columns->set_cell_type_column( 'T_CELLTYPE' ).
      CATCH cx_salv_data_error.                         "#EC NO_HANDLER
    ENDTRY.

*... set the color of a complete row
*     register the column in which the color information
*        for the row is held
    TRY.
        lo_columns->set_color_column( 'T_COLOR' ).
      CATCH cx_salv_data_error.                         "#EC NO_HANDLER
    ENDTRY.

* Champs techniques à ne pas afficher
    TRY.
        lo_column = lo_columns->get_column( 'MANDT' ).
        lo_column->set_technical( if_salv_c_bool_sap=>true ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.


  METHOD process_display_settings.
*----------------------------------------------------------------------*
* Method: PROCESS_DISPLAY_SETTINGS
* Title:  Display settings par défaut
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    DATA: lo_display_settings TYPE REF TO cl_salv_display_settings.

    lo_display_settings = grid->get_display_settings( ).

* Set the striped pattern
    CALL METHOD lo_display_settings->set_striped_pattern
      EXPORTING
        value = if_salv_c_bool_sap=>true.

  ENDMETHOD.


  METHOD process_events.
*----------------------------------------------------------------------*
* Method: PROCESS_EVENTS
* Title:  Gestion des events par défaut
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    CHECK block_mode = abap_false.

*... register to the events of cl_salv_table
    DATA: lo_events TYPE REF TO cl_salv_events_table.

    lo_events = grid->get_event( ).

    IF NOT imp_double_click_form IS INITIAL.
      set_double_click_form( imp_double_click_form ).
    ENDIF.
    IF NOT imp_hotspot_click_form IS INITIAL.
      set_hotspot_click_form( imp_hotspot_click_form ).
    ENDIF.
    IF NOT imp_before_user_command_form IS INITIAL.
      set_before_user_command_form( imp_before_user_command_form ).
    ENDIF.
    IF NOT imp_after_user_command_form IS INITIAL.
      set_after_user_command_form( imp_after_user_command_form ).
    ENDIF.
    IF NOT imp_user_command_form IS INITIAL.
      set_user_command_form( imp_user_command_form ).
    ENDIF.

*... register to the events
    IF NOT me->on_before_user_command_form IS INITIAL.
      SET HANDLER me->on_before_user_command FOR lo_events.
    ENDIF.
    IF NOT me->on_after_user_command_form IS INITIAL.
      SET HANDLER me->on_after_user_command FOR lo_events.
    ENDIF.
    IF NOT me->on_user_command_form IS INITIAL.
      SET HANDLER me->on_user_command FOR lo_events.
    ENDIF.
    IF NOT me->on_double_click_form IS INITIAL.
      SET HANDLER me->on_double_click FOR lo_events.
    ENDIF.
    IF NOT me->on_hotspot_click_form IS INITIAL.
      SET HANDLER me->on_hotspot_click FOR lo_events.
    ENDIF.


  ENDMETHOD.


  METHOD process_filters.
*----------------------------------------------------------------------*
* Method: PROCESS_FILTERS
* Title:  Gestion des filtres par défaut
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

  ENDMETHOD.


  METHOD process_functional_settings.
*----------------------------------------------------------------------*
* Method: PROCESS_FUNCTIONAL_SETTINGS
* Title:  functionnal settings par défaut
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*



  ENDMETHOD.


  METHOD process_functions.
*----------------------------------------------------------------------*
* Method: PROCESS_FUNCTIONS
* Title:  Fonctions par défaut
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    DATA: lo_functions TYPE REF TO cl_salv_functions_list.

    CHECK block_mode = abap_false.

    lo_functions = grid->get_functions( ).
    lo_functions->set_all( abap_true ).
    lo_functions->set_export_xml( abap_true ).
*  lo_functions->set_export_spreadsheet( abap_true ).
    lo_functions->set_view_lotus( abap_false ).


  ENDMETHOD.


  METHOD process_layout.
*----------------------------------------------------------------------*
* Method: PROCESS_LAYOUT
* Title:  Layout par défaut
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

*... set layout
    DATA: lo_layout TYPE REF TO cl_salv_layout,
          ls_key    TYPE salv_s_layout_key.

    ls_key-report = me->repid.

    lo_layout = grid->get_layout( ).

    lo_layout->set_key( ls_key ).

*... set usage of default Layouts
    lo_layout->set_default( if_salv_c_bool_sap=>true ).


*... set Layout save restriction
    AUTHORITY-CHECK OBJECT 'S_ALV_LAYO'
             ID 'ACTVT' FIELD '23'.
    IF sy-subrc = 0.
      lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
    ELSE.
      lo_layout->set_save_restriction( if_salv_c_layout=>restrict_user_dependant ).
    ENDIF.

*... set initial Layout
    IF imp_variant IS NOT INITIAL.
      lo_layout->set_initial_layout( imp_variant ).
    ENDIF.

  ENDMETHOD.


  METHOD process_print.
*----------------------------------------------------------------------*
* Method: PROCESS_PRINT
* Title:  Gestion des impressions par défaut
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

  ENDMETHOD.


  METHOD process_selections.
*----------------------------------------------------------------------*
* Method: PROCESS_SELECTIONS
* Title:  Mode de selection des lignes dans l'ALV par défaut
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

*  set_selection_mode( if_salv_c_selection_mode=>single ).
    set_selection_mode( if_salv_c_selection_mode=>none ).

  ENDMETHOD.


  METHOD process_sorts.
*----------------------------------------------------------------------*
* Method: PROCESS_SORTS
* Title:  Gestion des tris par défaut
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*


  ENDMETHOD.


  METHOD process_top_of_list.
*----------------------------------------------------------------------*
* Method: PROCESS_TOP_OF_LIST
* Title:  Header de l'ALV (HTML) par défaut
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaaaa - Blablabla...
* JZ15052012  - New Branding
*----------------------------------------------------------------------*

    CHECK block_mode = abap_false.

    DATA: lo_grid    TYPE REF TO cl_salv_form_layout_grid,
          lo_grid_1  TYPE REF TO cl_salv_form_layout_grid,
          lo_flow    TYPE REF TO cl_salv_form_layout_flow,
          lo_label   TYPE REF TO cl_salv_form_label,
          lo_picture TYPE REF TO cl_salv_form_picture,
          lo_text    TYPE REF TO cl_salv_form_text,
          l_text     TYPE string,
          l_title    TYPE string.
** Début de modification JZ15052012
*    DATA: st_t880 TYPE t880,
*          w_comp  TYPE rcomp_d.
*
*    CONCATENATE '00' imp_bukrs INTO w_comp.
*    SELECT SINGLE * FROM t880 INTO st_t880
*                   WHERE rcomp  = w_comp.
*    IF sy-subrc NE 0.
*      CLEAR st_t880.
*    ENDIF.
** Fin de modification JZ150052012

    CREATE OBJECT lo_grid.
    IF imp_title IS INITIAL.
      l_title = sy-title.
    ELSE.
      l_title = imp_title.
    ENDIF.


    lo_grid->create_header_information( row     = 1
                                        column  = 1
                                        text    = l_title
                                        tooltip = l_title ).

*... in the cell [2,1] create a grid
    lo_grid_1 = lo_grid->create_grid( row    = 2
                                      column = 1 ).
*
*
*... in the cell [1,1] of the second grid create a label
    lo_text = lo_grid_1->create_text( row      = 1
                                      column   = 1
                                      colspan = 2
*                                    text     = 'Bobst Mex SA'(bi1)
                                      text     = 'ALV'
*                                    tooltip  = ''(ki1)
  ).
*


    lo_flow = lo_grid_1->create_flow( row    = 2
                                      column = 1 ).

    lo_label = lo_flow->create_label( text    = 'Program:'(t02)
                                      tooltip = 'Program: '(t02) ).

    lo_text = lo_flow->create_text( text    = me->repid
                                    tooltip = me->repid ).
*
    lo_flow = lo_grid_1->create_flow( row    = 2
                                      column = 2 ).

    lo_label = lo_flow->create_label( text    = 'Transaction:'(t09)
                                      tooltip = 'Transaction: '(t09) ).

    lo_text = lo_flow->create_text( text    = sy-tcode
                                    tooltip = sy-tcode ).
*
*
    lo_flow = lo_grid_1->create_flow( row    = 3
                                      column = 1 ).

    lo_label = lo_flow->create_label( text    = 'System:'(t03)
                                      tooltip = 'System:'(t03) ).

    lo_text = lo_flow->create_text( text    = sy-sysid
                                    tooltip = sy-sysid ).

    lo_flow = lo_grid_1->create_flow( row    = 3
                                      column = 2 ).

    lo_label = lo_flow->create_label( text    = 'Client:'(t04)
                                      tooltip = 'Client:'(t04) ).

    lo_text = lo_flow->create_text( text    = sy-mandt
                                    tooltip = sy-mandt ).

    lo_flow = lo_grid_1->create_flow( row = 4
                                      column = 1 ).

    DATA: date1(12) TYPE c.
    DATA: time1(8) TYPE c.
    DATA: tzonesys TYPE tznzonesys.

    WRITE sy-datum TO date1.
    WRITE sy-uzeit TO time1.

    SELECT SINGLE tzonesys FROM ttzcu INTO tzonesys.

    lo_label = lo_flow->create_label( text    = 'Date:'(t05)
                                      tooltip = 'Date:'(t05) ).
    lo_text = lo_flow->create_text( text = date1
                                    tooltip = date1 ).

    lo_flow = lo_grid_1->create_flow( row = 4
                                      column = 2 ).

    lo_label = lo_flow->create_label( text    = 'Time:'(t06)
                                      tooltip = 'Time:'(t06) ).

    lo_text = lo_flow->create_text( text = time1
                                    tooltip = time1 ).

    lo_text = lo_flow->create_text( text = tzonesys
                                    tooltip = tzonesys ).

    IF sy-timlo NE sy-uzeit.
      WRITE sy-datlo TO date1.
      WRITE sy-timlo TO time1.

      lo_flow = lo_grid_1->create_flow( row    = 5
                                        column = 1 ).
      lo_label = lo_flow->create_label( text    = 'Local Date:'(t07)
                                        tooltip = 'Local Date:'(t07) ).

      lo_text = lo_flow->create_text( text    = date1
                                      tooltip = date1 ).

      lo_flow = lo_grid_1->create_flow( row = 5
                                        column = 2 ).

      lo_label = lo_flow->create_label( text = 'Local Time:'(t08)
                                        tooltip = 'Local Time:'(t08) ).

      lo_text = lo_flow->create_text( text = time1
                                      tooltip = time1 ).

      lo_text = lo_flow->create_text( text    = sy-zonlo
                                      tooltip = sy-zonlo ).

    ENDIF.
*
    grid->set_top_of_list( lo_grid ).
*


  ENDMETHOD.


  METHOD refresh.
*----------------------------------------------------------------------*
* Method: REFRESH
* Title:  Raffraichit l'affichage de l'ALV
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    CALL METHOD grid->refresh
      EXPORTING
        s_stable     = imp_s_stable
        refresh_mode = imp_refresh_mode.


  ENDMETHOD.


  METHOD set_after_user_command_form.
*----------------------------------------------------------------------*
* Method: SET_AFTER_USER_COMMAND_FORM
* Title:  Permet de specifier la routine appelé sur l'evenement
*         on_after_user_command
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    me->on_after_user_command_form = imp_after_user_command_form.

  ENDMETHOD.


  METHOD set_alv_popup.
*----------------------------------------------------------------------*
* Method: SET_ALV_POPUP
* Title:  Permet d'afficher, grace aux coordonnées, l'ALV dans une popup
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    CALL METHOD grid->set_screen_popup
      EXPORTING
        start_column = imp_start_column
        end_column   = imp_end_column
        start_line   = imp_start_line
        end_line     = imp_end_line.

  ENDMETHOD.


  METHOD set_before_user_command_form.
*----------------------------------------------------------------------*
* Method: SET_BEFORE_USER_COMMAND_FORM
* Title:  Permet de specifier la routine appelé sur l'evenement
*         on_before_user_command
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    me->on_before_user_command_form = imp_before_user_command_form.

  ENDMETHOD.


  METHOD set_block_mode.
*----------------------------------------------------------------------*
* Method: SET_BLOCK_MODE
* Title:  Initialise un bloc de liste ALV
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    block_mode = imp_block_mode.

    CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_INIT'
      EXPORTING
        i_callback_program = sy-cprog
*       I_CALLBACK_PF_STATUS_SET       = ' '
*       I_CALLBACK_USER_COMMAND        = ' '
*       IT_EXCLUDING       =
      .

    CHECK NOT imp_header_visible IS INITIAL.

* On initialise le bloc de liste par une header
*  DATA: s_prog_attr TYPE zca_st_tools_alv.
    DATA: s_prog_attr TYPE lvc_s_deta.

    s_prog_attr-columntext = 'Program:'(t02).
    s_prog_attr-value = sy-cprog.
    APPEND s_prog_attr TO t_prog_attr.

    s_prog_attr-columntext = 'Transaction:'(t09).
    s_prog_attr-value = sy-tcode.
    APPEND s_prog_attr TO t_prog_attr.

    s_prog_attr-columntext = 'System:'(t03).
    s_prog_attr-value = sy-sysid.
    APPEND s_prog_attr TO t_prog_attr.

    s_prog_attr-columntext = 'Client:'(t04).
    s_prog_attr-value = sy-mandt.
    APPEND s_prog_attr TO t_prog_attr.

    s_prog_attr-columntext = 'User:'(t10).
    s_prog_attr-value = sy-uname.
    APPEND s_prog_attr TO t_prog_attr.

    s_prog_attr-columntext = 'Date:'(t05).
    WRITE sy-datum TO s_prog_attr-value.
    APPEND s_prog_attr TO t_prog_attr.

    s_prog_attr-columntext = 'Time:'(t06).
    WRITE sy-uzeit TO s_prog_attr-value.
    APPEND s_prog_attr TO t_prog_attr.


    IF sy-timlo NE sy-uzeit.
      s_prog_attr-columntext = 'Local Date:'(t07).
      WRITE sy-datlo TO s_prog_attr-value.
      APPEND s_prog_attr TO t_prog_attr.

      s_prog_attr-columntext = 'Local Time:'(t08).
      WRITE sy-timlo TO s_prog_attr-value.
      APPEND s_prog_attr TO t_prog_attr.

    ENDIF.


    CALL METHOD zcl_tools_alv=>display
      EXPORTING
*       imp_title              =
*       imp_selection_mode     = IF_SALV_C_SELECTION_MODE=>NONE
*       imp_double_click_form  = 'ON_DOUBLE_CLICK'
*       imp_hotspot_click_form = 'ON_HOTSPOT_CLICK'
*       imp_before_user_command_form = 'ON_BEFORE_USER_COMMAND'
*       imp_after_user_command_form  = 'ON_AFTER_USER_COMMAND'
*       imp_user_command_form  = 'ON_USER_COMMAND'
*       imp_variant            =
*       imp_default_top_of_list      = IF_SALV_C_BOOL_SAP=>TRUE
        imp_header_col_visible = if_salv_c_bool_sap=>false
      CHANGING
        chg_table              = t_prog_attr
*  EXCEPTIONS
*       casting_error          = 1
*       others                 = 2
      .


  ENDMETHOD.


  METHOD set_cell_color.
*----------------------------------------------------------------------*
* Method: SET_CELL_COLOR
* Title:  Permet de définir la couleur d'une cellule
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    DATA: lo_columns TYPE REF TO cl_salv_columns_table,
          lo_column  TYPE REF TO cl_salv_column.

    FIELD-SYMBOLS: <table> TYPE STANDARD TABLE,
                   <row>   TYPE any,
                   <field> TYPE STANDARD TABLE.

    lo_columns = grid->get_columns( ).

    TRY.
        DATA: color_column TYPE lvc_fname.
        color_column = lo_columns->get_color_column( ).

        ASSIGN t_table->* TO <table>.

        READ TABLE <table> INDEX imp_row ASSIGNING <row>.

        ASSIGN COMPONENT color_column OF STRUCTURE <row> TO <field>.
*      REFRESH <field>.

        DATA: lt_color TYPE lvc_t_scol,
              ls_color TYPE lvc_s_scol.

        ls_color-fname     = imp_column.
        ls_color-color-col = imp_color.
        ls_color-color-int = imp_color_int.
        ls_color-color-inv = imp_color_inv.
        APPEND ls_color TO lt_color.

        APPEND LINES OF lt_color TO <field>.

        CALL METHOD grid->refresh
*        EXPORTING
*          s_stable     =
*          refresh_mode = if_salv_c_refresh=>SOFT
          .
*if_salv_c_refresh=>NONE
*if_salv_c_refresh=>FULL
*if_salv_c_refresh=>SOFT

      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.


  METHOD set_column_attributes.
*----------------------------------------------------------------------*
* Method: SET_COLUMN_ATTRIBUTES
* Title:  Permet de specifier les attributs d'une colonne de l'ALV
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    DATA: lo_columns TYPE REF TO cl_salv_columns_table,
          lo_column  TYPE REF TO cl_salv_column_table.

    lo_columns = grid->get_columns( ).

    TRY.
        lo_column ?= lo_columns->get_column( imp_column_name ).

        IF imp_as_checkbox IS SUPPLIED.
          IF imp_as_checkbox = if_salv_c_bool_sap=>true.
            lo_column->set_cell_type( if_salv_c_cell_type=>checkbox ).
          ENDIF.
        ENDIF.
        IF imp_as_icon IS SUPPLIED.
          lo_column->set_icon( imp_as_icon ).
        ENDIF.

        IF imp_as_key IS SUPPLIED.
*        IF imp_as_key = if_salv_c_bool_sap=>true.
          lo_column->set_key( imp_as_key ).
*        ENDIF.
        ENDIF.

        IF imp_as_hotspot IS SUPPLIED.
          IF imp_as_hotspot = if_salv_c_bool_sap=>true.
            lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
          ENDIF.
        ENDIF.
        IF imp_as_symbol IS SUPPLIED.
          lo_column->set_symbol( imp_as_symbol ).
        ENDIF.

        IF imp_as_exception IS SUPPLIED.
          IF imp_as_exception = if_salv_c_bool_sap=>true.

            CALL METHOD lo_columns->set_exception_column
              EXPORTING
                value     = imp_column_name
                group     = imp_exception_group
                condensed = imp_exception_condensed.

          ENDIF.
        ENDIF.

        IF imp_alignment IS SUPPLIED.
          lo_column->set_alignment( imp_alignment ).
        ENDIF.
        IF imp_currency IS SUPPLIED.
          lo_column->set_currency( imp_currency ).
        ENDIF.
        IF imp_currency_column IS SUPPLIED.
          lo_column->set_currency_column( imp_currency_column ).
        ENDIF.
        IF imp_ddic_reference IS SUPPLIED.
          lo_column->set_ddic_reference( imp_ddic_reference ).
        ENDIF.
        IF imp_decimals_column IS SUPPLIED.
          lo_column->set_decimals_column( imp_decimals_column ).
        ENDIF.
        IF imp_decimals IS SUPPLIED.
          lo_column->set_decimals( imp_decimals ).
        ENDIF.
        IF imp_edit_mask IS SUPPLIED.
          lo_column->set_edit_mask( imp_edit_mask ).
        ENDIF.
        IF imp_f1_rollname IS SUPPLIED.
          lo_column->set_f1_rollname( imp_f1_rollname ).
        ENDIF.
        IF imp_leading_zero IS SUPPLIED.
          lo_column->set_leading_zero( imp_leading_zero ).
        ENDIF.
        IF imp_long_text IS SUPPLIED.
          lo_column->set_long_text( imp_long_text ).
        ENDIF.
        IF imp_lowercase IS SUPPLIED.
          lo_column->set_lowercase( imp_lowercase ).
        ENDIF.
        IF imp_medium_text IS SUPPLIED.
          lo_column->set_medium_text( imp_medium_text ).
        ENDIF.
        IF imp_optimized IS SUPPLIED.
          lo_column->set_optimized( imp_optimized ).
        ENDIF.
        IF imp_output_length IS SUPPLIED.
          lo_column->set_output_length( imp_output_length ).
        ENDIF.
        IF imp_quantity IS SUPPLIED.
          lo_column->set_quantity( imp_quantity ).
        ENDIF.
        IF imp_quantity_column IS SUPPLIED.
          lo_column->set_quantity_column( imp_quantity_column ).
        ENDIF.
        IF imp_round IS SUPPLIED.
          lo_column->set_round( imp_round ).
        ENDIF.
        IF imp_round_column IS SUPPLIED.
          lo_column->set_round_column( imp_round_column ).
        ENDIF.
        IF imp_short_text IS SUPPLIED.
          lo_column->set_short_text( imp_short_text ).
        ENDIF.
        IF imp_sign IS SUPPLIED.
          lo_column->set_sign( imp_sign ).
        ENDIF.
        IF imp_technical IS SUPPLIED.
          lo_column->set_technical( imp_technical ).
        ENDIF.
        IF imp_tooltip IS SUPPLIED.
          lo_column->set_tooltip( imp_tooltip ).
        ENDIF.
        IF imp_visible IS SUPPLIED.
          lo_column->set_visible( imp_visible ).
        ENDIF.
        IF imp_zero IS SUPPLIED.
          lo_column->set_zero( imp_zero ).
        ENDIF.

      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.


  METHOD set_column_color.
*----------------------------------------------------------------------*
* Method: SET_COLUMN_COLOR
* Title:  Permet de définir la couleur d'une colonne
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    DATA: lo_columns TYPE REF TO cl_salv_columns_table,
          lo_column  TYPE REF TO cl_salv_column_table,
          ls_color   TYPE lvc_s_colo.

    lo_columns = grid->get_columns( ).

    TRY.
        lo_column ?= lo_columns->get_column( imp_column ).


        ls_color-col = imp_color.
        ls_color-int = imp_color_int.
        ls_color-inv = imp_color_inv.

        lo_column->set_color( ls_color ).

      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.


  ENDMETHOD.


  METHOD set_double_click_form.
*----------------------------------------------------------------------*
* Method: SET_DOUBLE_CLICK_FORM
* Title:  Permet de specifier la routine appelé lors d'un double clic
*         dans l'ALV
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    me->on_double_click_form = imp_double_click_form.

  ENDMETHOD.


  METHOD set_hotspot_click_form.
*----------------------------------------------------------------------*
* Method: SET_HOTSPOT_CLICK_FORM
* Title:  Permet de specifier la routine appelé lors d'un clic sur un
*         lien
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    me->on_hotspot_click_form = imp_hotspot_click_form.

  ENDMETHOD.


  METHOD set_line_color.
*----------------------------------------------------------------------*
* Method: SET_LINE_COLOR
* Title:  Permet de définir la couleur d'une ligne
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    DATA: lo_columns TYPE REF TO cl_salv_columns_table,
          lo_column  TYPE REF TO cl_salv_column.

*  DATA : lt_columns TYPE salv_t_column_ref,
*         ls_columns TYPE salv_s_column_ref.

    FIELD-SYMBOLS: <table> TYPE STANDARD TABLE,
                   <row>   TYPE any,
                   <field> TYPE STANDARD TABLE.

    lo_columns = grid->get_columns( ).

    TRY.
        DATA: color_column TYPE lvc_fname.
        color_column = lo_columns->get_color_column( ).

        ASSIGN t_table->* TO <table>.

        READ TABLE <table> INDEX imp_row ASSIGNING <row>.

        ASSIGN COMPONENT color_column OF STRUCTURE <row> TO <field>.
        CHECK sy-subrc = 0.
        REFRESH <field>.

        DATA: lt_color TYPE lvc_t_scol,
              ls_color TYPE lvc_s_scol.

*      lt_columns = lo_columns->get( ).

*      LOOP AT lt_columns INTO ls_columns .
*        ls_color-fname     = ls_columns-columnname.
*        ls_color-color-col = imp_color.
*        ls_color-color-int = imp_color_int.
*        ls_color-color-inv = imp_color_inv.
*        APPEND ls_color TO lt_color.
*      ENDLOOP.

        ls_color-fname     = ''.
        ls_color-color-col = imp_color.
        ls_color-color-int = 0.
        ls_color-color-inv = 0.
        ls_color-nokeycol = 'X'.
        APPEND ls_color TO lt_color.

        APPEND LINES OF lt_color TO <field>.

        CALL METHOD grid->refresh
*        EXPORTING
*          s_stable     =
*          refresh_mode = IF_SALV_C_REFRESH=>SOFT
          .


      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.



  ENDMETHOD.


  METHOD set_screen_status.
*----------------------------------------------------------------------*
* Method: SET_SCREEN_STATUS
* Title:  Permet de specifier un screen status
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

* Dans le cas d'un affichage en plein ecran, copier le Gui status 'STANDARD' du programme 'SAPLSALV'
* Dans le cas d'un affichage en popup, copier le Gui status 'STDPOPUP' du programme 'SAPLSALV'
* Ajouter ensuite vos functions personaliser.
* Il est inutile de supprimer les functions standards, car elle sont manipulable gràce au parametre imp_set_functions

    DATA: lo_functions TYPE REF TO cl_salv_functions.

    lo_functions = grid->get_functions( ).
    CLEAR lo_functions.

    grid->set_screen_status( pfstatus      = imp_pfstatus
                             report        = me->repid
                             set_functions = imp_set_functions ).
*                          set_functions : 0 None
*                                          1 Default
*                                          2 All

  ENDMETHOD.


  METHOD set_selected_rows.

    DATA: lo_selections TYPE REF TO cl_salv_selections.

    lo_selections = grid->get_selections( ).
    lo_selections->set_selected_rows( imp_t_rows ).

  ENDMETHOD.


  METHOD set_selection_mode.
*----------------------------------------------------------------------*
* Method: SET_SELECTION_MODE
* Title:  Permet de specifier le mode de selection des lignes dans
*         l'ALV
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    DATA: lr_selections TYPE REF TO cl_salv_selections,
          lt_rows       TYPE salv_t_row,
          lt_column     TYPE salv_t_column,
          ls_cell       TYPE salv_s_cell.

    lr_selections = grid->get_selections( ).

*... §7.1 set selection mode
    lr_selections->set_selection_mode( imp_mode ).
    " if_salv_c_selection_mode=>single          1
    " if_salv_c_selection_mode=>multiple        2
    " if_salv_c_selection_mode=>cell            3
    " if_salv_c_selection_mode=>row_column      4
    " if_salv_c_selection_mode=>none            0


  ENDMETHOD.


  METHOD set_tooltip.
*----------------------------------------------------------------------*
* Method: SET_TOOLTIP
* Title:  Permet de définir des tooltips (info bulle)
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    DATA: lo_functional_settings TYPE REF TO cl_salv_functional_settings,
          lo_tooltips            TYPE REF TO cl_salv_tooltips.

    lo_functional_settings = grid->get_functional_settings( ).
    lo_tooltips = lo_functional_settings->get_tooltips( ).

    TRY.
        lo_tooltips->add_tooltip(
          type       = imp_type
          value      = imp_value
          tooltip    = imp_tooltip ).
      CATCH cx_salv_existing.                           "#EC NO_HANDLER

    ENDTRY.


  ENDMETHOD.


  METHOD set_user_command_form.
*----------------------------------------------------------------------*
* Method: SET_USER_COMMAND_FORM
* Title:  Permet de specifier la routine appelé sur l'evenement
*         on_user_command
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

    me->on_user_command_form = imp_user_command_form.

  ENDMETHOD.


  METHOD set_variant.
*----------------------------------------------------------------------*
* Method: SET_VARIANT
* Title:  Permet de specifier une mise en forme
*----------------------------------------------------------------------*
* Author: S. Collomb
* Date:   19.08.2008
*----------------------------------------------------------------------*
* M O D I F I C A T I O N S
* XXXjjmmaa - Blablabla...
*----------------------------------------------------------------------*

*... set layout
    DATA: lo_layout TYPE REF TO cl_salv_layout,
          ls_key    TYPE salv_s_layout_key.

    ls_key-report = me->repid.

    lo_layout = grid->get_layout( ).

    lo_layout->set_key( ls_key ).

*... set initial Layout
    IF imp_variant IS NOT INITIAL.
      lo_layout->set_initial_layout( imp_variant ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
