CLASS lcl_data_comparator DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_s_diff_report,
             field   TYPE string,
             value_a TYPE string,
             value_b TYPE string,
           END OF ty_s_diff_report.
    TYPES: ty_t_diff_report TYPE TABLE OF ty_s_diff_report WITH DEFAULT KEY.
    TYPES: ty_t_field_names TYPE TABLE OF char100.

    METHODS: set_ignored_fields IMPORTING it_ignored_fields TYPE ty_t_field_names.

    METHODS: compare_data IMPORTING i_data_a              TYPE any
                                    i_data_b              TYPE any
                                    iv_field_name         TYPE string OPTIONAL
                          RETURNING VALUE(rt_diff_report) TYPE ty_t_diff_report.

  PROTECTED SECTION.
    METHODS: compare_structures IMPORTING is_struct_a   TYPE any
                                          is_struct_b   TYPE any
                                          iv_field_name TYPE string.

    METHODS: compare_tables IMPORTING it_table_a    TYPE any TABLE
                                      it_table_b    TYPE any TABLE
                                      iv_field_name TYPE string.

    METHODS: compare_scalars IMPORTING iv_scalar_a   TYPE any
                                       iv_scalar_b   TYPE any
                                       iv_field_name TYPE string.

  PRIVATE SECTION.
    DATA: mv_diff           TYPE abap_bool.
    DATA: mt_ignored_fields TYPE ty_t_field_names.
    DATA: mt_diff_report    TYPE ty_t_diff_report.
ENDCLASS.

CLASS lcl_data_comparator IMPLEMENTATION.
**********************************************************************
  METHOD compare_data.
    DATA: lv_var_type TYPE c.

    DESCRIBE FIELD i_data_a TYPE lv_var_type.

    CASE lv_var_type.
      WHEN 'u' OR 'v'.    "Flat & deep structure
        me->compare_structures( EXPORTING is_struct_a   = i_data_a
                                          is_struct_b   = i_data_b
                                          iv_field_name = iv_field_name ).
      WHEN 'h'.           "Table type
        me->compare_tables( EXPORTING it_table_a    = i_data_a
                                      it_table_b    = i_data_b
                                      iv_field_name = iv_field_name ).
      WHEN OTHERS.        "Scalars
        me->compare_scalars( EXPORTING iv_scalar_a   = i_data_a
                                       iv_scalar_b   = i_data_b
                                       iv_field_name = iv_field_name ).
    ENDCASE.

    rt_diff_report = mt_diff_report.
  ENDMETHOD.

**********************************************************************
  METHOD set_ignored_fields.
    me->mt_ignored_fields = it_ignored_fields.
  ENDMETHOD.

**********************************************************************
  METHOD compare_structures.
    DATA: lo_struc_descr          TYPE REF TO cl_abap_structdescr.
    DATA: lv_new_field_name       TYPE string.
    FIELD-SYMBOLS: <fs_field_a>   TYPE any.
    FIELD-SYMBOLS: <fs_field_b>   TYPE any.
    FIELD-SYMBOLS: <ls_component> TYPE abap_compdescr.

    lo_struc_descr ?= cl_abap_structdescr=>describe_by_data( is_struct_a ).

    LOOP AT lo_struc_descr->components ASSIGNING <ls_component>.
      ASSIGN COMPONENT <ls_component>-name OF STRUCTURE is_struct_a TO <fs_field_a>.
      ASSIGN COMPONENT <ls_component>-name OF STRUCTURE is_struct_b TO <fs_field_b>.

      IF iv_field_name IS INITIAL.
        MOVE <ls_component>-name TO lv_new_field_name.
      ELSE.
        CONCATENATE iv_field_name '-' <ls_component>-name INTO lv_new_field_name.
      ENDIF.

      me->compare_data( i_data_a      = <fs_field_a>
                        i_data_b      = <fs_field_b>
                        iv_field_name = lv_new_field_name ).
    ENDLOOP.
  ENDMETHOD.

**********************************************************************
  METHOD compare_tables.
    DATA: lv_match_result        TYPE match_result.
    DATA: lv_lines_a             TYPE n.
    DATA: lv_lines_b             TYPE n.
    DATA: lv_tabix               TYPE string.
    DATA: lv_value_a             TYPE string.
    DATA: lv_value_b             TYPE string.
    DATA: lv_new_field_name      TYPE string.
    DATA: lv_table_name          TYPE string.
    DATA: lo_table_descr         TYPE REF TO cl_abap_tabledescr.
    DATA: ls_diff                TYPE lcl_data_comparator=>ty_s_diff_report.
    FIELD-SYMBOLS: <fs_record_a> TYPE any.
    FIELD-SYMBOLS: <fs_record_b> TYPE any.
    FIELD-SYMBOLS: <fs_table_b>  TYPE INDEX TABLE.

    lo_table_descr ?= cl_abap_tabledescr=>describe_by_data( it_table_a ).

    lv_table_name = lo_table_descr->absolute_name.
    FIND '=' IN lv_table_name RESULTS lv_match_result.
    WHILE sy-subrc = 0.
      ADD 1 TO lv_match_result-offset.
      MOVE lv_table_name+lv_match_result-offset TO lv_table_name.
      FIND '=' IN lv_table_name RESULTS lv_match_result.
    ENDWHILE.

    ASSIGN it_table_b TO <fs_table_b>. "Cast to indexed table

    lv_lines_a = lines( it_table_a ).
    lv_lines_b = lines( it_table_b ).

    IF lv_lines_a <> lv_lines_b.
      CONCATENATE 'Records:' lv_lines_a INTO lv_value_a SEPARATED BY space.
      CONCATENATE 'Records:' lv_lines_b INTO lv_value_b SEPARATED BY space.

      IF iv_field_name IS INITIAL.
        CONCATENATE lv_table_name '[]' INTO lv_new_field_name.
      ELSE.
        CONCATENATE iv_field_name '-' lv_table_name '[]' INTO lv_new_field_name.
      ENDIF.

      MOVE abap_true TO mv_diff.
      MOVE lv_value_a TO ls_diff-value_a.
      MOVE lv_value_b TO ls_diff-value_b.
      MOVE lv_new_field_name TO ls_diff-field.
      APPEND ls_diff TO mt_diff_report.
      EXIT.
    ENDIF.

    LOOP AT it_table_a ASSIGNING <fs_record_a>.
      lv_tabix = sy-tabix.
      CONDENSE lv_tabix NO-GAPS.

      IF iv_field_name IS INITIAL.
        CONCATENATE lv_table_name '[' lv_tabix ']' INTO lv_new_field_name.
      ELSE.
        CONCATENATE iv_field_name '-' lv_table_name '[' lv_tabix ']' INTO lv_new_field_name.
      ENDIF.

      READ TABLE <fs_table_b> INDEX sy-tabix ASSIGNING <fs_record_b>.

      IF sy-subrc = 0.
        me->compare_data( i_data_a      = <fs_record_a>
                          i_data_b      = <fs_record_b>
                          iv_field_name = lv_new_field_name ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

**********************************************************************
  METHOD compare_scalars.
    DATA: ls_diff TYPE lcl_data_comparator=>ty_s_diff_report.

    LOOP AT mt_ignored_fields TRANSPORTING NO FIELDS WHERE table_line = iv_field_name.
    ENDLOOP.

    IF sy-subrc <> 0.
      IF iv_scalar_a <> iv_scalar_b.
        MOVE abap_true TO mv_diff.
        MOVE iv_scalar_a TO ls_diff-value_a.
        MOVE iv_scalar_b TO ls_diff-value_b.
        MOVE iv_field_name TO ls_diff-field.
        APPEND ls_diff TO mt_diff_report.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
