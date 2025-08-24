" Basic ABAP Functions - Translated from Python Examples
" Demonstrates various ABAP constructs corresponding to the original Python code

CLASS lcl_calculator DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor,
      add
        IMPORTING iv_a TYPE i iv_b TYPE i
        RETURNING VALUE(rv_result) TYPE i,
      subtract
        IMPORTING iv_a TYPE i iv_b TYPE i
        RETURNING VALUE(rv_result) TYPE i,
      multiply
        IMPORTING iv_a TYPE i iv_b TYPE i
        RETURNING VALUE(rv_result) TYPE i,
      divide
        IMPORTING iv_a TYPE i iv_b TYPE i
        RETURNING VALUE(rv_result) TYPE f
        RAISING cx_sy_arithmetic_error,
      get_history
        RETURNING VALUE(rt_history) TYPE STANDARD TABLE OF string,
      clear_history.

  PRIVATE SECTION.
    DATA mt_history TYPE STANDARD TABLE OF string.
ENDCLASS.

CLASS lcl_calculator IMPLEMENTATION.

  METHOD constructor.
    CLEAR mt_history.
  ENDMETHOD.

  METHOD add.
    rv_result = iv_a + iv_b.
    APPEND |{ iv_a } + { iv_b } = { rv_result }| TO mt_history.
  ENDMETHOD.

  METHOD subtract.
    rv_result = iv_a - iv_b.
    APPEND |{ iv_a } - { iv_b } = { rv_result }| TO mt_history.
  ENDMETHOD.

  METHOD multiply.
    rv_result = iv_a * iv_b.
    APPEND |{ iv_a } * { iv_b } = { rv_result }| TO mt_history.
  ENDMETHOD.

  METHOD divide.
    IF iv_b = 0.
      RAISE EXCEPTION TYPE cx_sy_arithmetic_error
        EXPORTING textid = cx_sy_arithmetic_error=>division_by_zero.
    ENDIF.
    rv_result = iv_a / iv_b.
    APPEND |{ iv_a } / { iv_b } = { rv_result }| TO mt_history.
  ENDMETHOD.

  METHOD get_history.
    rt_history = mt_history.
  ENDMETHOD.

  METHOD clear_history.
    CLEAR mt_history.
  ENDMETHOD.

ENDCLASS.

"----------------------------------------------------------
" User structure for create_user
TYPES: BEGIN OF ty_user,
         name      TYPE string,
         email     TYPE string,
         age       TYPE i,
         is_active TYPE abap_bool,
       END OF ty_user.

"----------------------------------------------------------
" Function: greet_user
CLASS lcl_functions DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      greet_user
        IMPORTING iv_name TYPE string
        RETURNING VALUE(rv_greeting) TYPE string,
      calculate_area
        IMPORTING iv_length TYPE i iv_width TYPE i
        RETURNING VALUE(rv_area) TYPE i,
      check_age
        IMPORTING iv_age TYPE i
        RETURNING VALUE(rv_group) TYPE string,
      sum_numbers
        IMPORTING it_numbers TYPE STANDARD TABLE OF i
        RETURNING VALUE(rv_total) TYPE i,
      create_user
        IMPORTING iv_name TYPE string iv_email TYPE string iv_age TYPE i
        RETURNING VALUE(rs_user) TYPE ty_user,
      filter_even_numbers
        IMPORTING it_numbers TYPE STANDARD TABLE OF i
        RETURNING VALUE(rt_even) TYPE STANDARD TABLE OF i,
      format_name
        IMPORTING iv_first TYPE string iv_last TYPE string
        RETURNING VALUE(rv_formatted) TYPE string,
      divide_numbers
        IMPORTING iv_a TYPE f iv_b TYPE f
        RETURNING VALUE(rv_result) TYPE f
        RAISING cx_sy_arithmetic_error,
      fibonacci
        IMPORTING iv_n TYPE i
        RETURNING VALUE(rv_fib) TYPE i,
      process_text
        IMPORTING iv_text TYPE string iv_operation TYPE string
        RETURNING VALUE(rv_result) TYPE string,
      analyze_list
        IMPORTING it_data TYPE STANDARD TABLE OF i
        RETURNING VALUE(rs_analysis) TYPE ty_analysis,
      process_kwargs
        IMPORTING it_kwargs TYPE STANDARD TABLE OF ty_key_value
        RETURNING VALUE(rt_result) TYPE STANDARD TABLE OF ty_key_value,
      validate_email
        IMPORTING iv_email TYPE string
        RETURNING VALUE(rs_validation) TYPE ty_validation,
      batch_process
        IMPORTING it_items TYPE STANDARD TABLE OF i iv_batch_size TYPE i
        RETURNING VALUE(rt_batches) TYPE STANDARD TABLE OF STANDARD TABLE OF i.

  PRIVATE SECTION.
    CLASS-METHODS:
      is_string
        IMPORTING iv_value TYPE any
        RETURNING VALUE(rv_is_string) TYPE abap_bool,
      is_number
        IMPORTING iv_value TYPE any
        RETURNING VALUE(rv_is_number) TYPE abap_bool.
ENDCLASS.

" Structures for multiple returns
TYPES: BEGIN OF ty_analysis,
         total   TYPE i,
         average TYPE f,
         maximum TYPE i,
       END OF ty_analysis.

TYPES: BEGIN OF ty_key_value,
         key   TYPE string,
         value TYPE any,
       END OF ty_key_value.

TYPES: BEGIN OF ty_validation,
         is_valid TYPE abap_bool,
         message  TYPE string,
       END OF ty_validation.

CLASS lcl_functions IMPLEMENTATION.

  METHOD greet_user.
    rv_greeting = |Hello, { iv_name }!|.
  ENDMETHOD.

  METHOD calculate_area.
    rv_area = iv_length * iv_width.
  ENDMETHOD.

  METHOD check_age.
    rv_group = COND string(
      WHEN iv_age >= 18 THEN 'Adult'
      ELSE 'Minor'
    ).
  ENDMETHOD.

  METHOD sum_numbers.
    rv_total = REDUCE i( INIT x = 0 FOR <num> IN it_numbers NEXT x = x + <num> ).
  ENDMETHOD.

  METHOD create_user.
    rs_user = VALUE ty_user(
      name      = iv_name
      email     = iv_email
      age       = iv_age
      is_active = abap_true
    ).
  ENDMETHOD.

  METHOD filter_even_numbers.
    rt_even = VALUE #( FOR <num> IN it_numbers WHERE ( <num> MOD 2 = 0 ) ( <num> ) ).
  ENDMETHOD.

  METHOD format_name.
    rv_formatted = |{ iv_first && ',' && iv_last }|.
    rv_formatted = |{ iv_first->to_upper( ) }, { iv_last->to_upper( ) }|.
  ENDMETHOD.

  METHOD divide_numbers.
    IF iv_b = 0.
      RAISE EXCEPTION TYPE cx_sy_arithmetic_error
        EXPORTING textid = cx_sy_arithmetic_error=>division_by_zero.
    ENDIF.
    rv_result = iv_a / iv_b.
  ENDMETHOD.

  METHOD fibonacci.
    IF iv_n <= 1.
      rv_fib = iv_n.
    ELSE.
      rv_fib = lcl_functions=>fibonacci( iv_n = iv_n - 1 ) + lcl_functions=>fibonacci( iv_n = iv_n - 2 ).
    ENDIF.
  ENDMETHOD.

  METHOD process_text.
    DATA(lt_operations) = VALUE string_table(
      ( 'upper' ) ( 'lower' ) ( 'reverse' ) ( 'length' )
    ).
    CASE iv_operation.
      WHEN 'upper'.
        rv_result = iv_text.
        rv_result = rv_result->to_upper( ).
      WHEN 'lower'.
        rv_result = iv_text.
        rv_result = rv_result->to_lower( ).
      WHEN 'reverse'.
        rv_result = ''.
        DO strlen( iv_text ) TIMES.
          rv_result = rv_result && iv_text+strlen( iv_text )-sy-index(1)(1).
        ENDDO.
      WHEN 'length'.
        rv_result = |{ strlen( iv_text ) }|.
      WHEN OTHERS.
        rv_result = iv_text.
    ENDCASE.
  ENDMETHOD.

  METHOD analyze_list.
    IF it_data IS INITIAL.
      rs_analysis = VALUE #( total = 0 average = 0 maximum = 0 ).
    ELSE.
      rs_analysis-total = REDUCE i( INIT x = 0 FOR <n> IN it_data NEXT x = x + <n> ).
      rs_analysis-average = rs_analysis-total / lines( it_data ).
      rs_analysis-maximum = REDUCE i( INIT x = -999999 FOR <n> IN it_data NEXT x = COND #( WHEN <n> > x THEN <n> ELSE x ) ).
    ENDIF.
  ENDMETHOD.

  METHOD process_kwargs.
    rt_result = VALUE #( ).
    LOOP AT it_kwargs ASSIGNING FIELD-SYMBOL(<kv>).
      DATA(lv_key) = <kv>-key.
      DATA(lv_value) = <kv>-value.
      DATA(lv_new_value) TYPE any.
      IF lcl_functions=>is_string( lv_value ) = abap_true.
        lv_new_value = lv_value.
        lv_new_value = lv_new_value->to_title( ).
      ELSEIF lcl_functions=>is_number( lv_value ) = abap_true.
        lv_new_value = lv_value * 2.
      ELSE.
        lv_new_value = lv_value.
      ENDIF.
      APPEND VALUE ty_key_value( key = lv_key value = lv_new_value ) TO rt_result.
    ENDLOOP.
  ENDMETHOD.

  METHOD validate_email.
    IF iv_email IS INITIAL.
      rs_validation = VALUE #( is_valid = abap_false message = 'Email is empty' ).
      RETURN.
    ENDIF.
    IF iv_email CP '*@*' = abap_false.
      rs_validation = VALUE #( is_valid = abap_false message = 'Email must contain @' ).
      RETURN.
    ENDIF.
    DATA(lt_parts) = VALUE string_table( ).
    SPLIT iv_email AT '@' INTO TABLE lt_parts.
    IF lines( lt_parts ) <> 2.
      rs_validation = VALUE #( is_valid = abap_false message = 'Email format is invalid' ).
      RETURN.
    ENDIF.
    DATA(lv_username) = lt_parts[ 1 ].
    DATA(lv_domain)   = lt_parts[ 2 ].
    IF lv_username IS INITIAL OR lv_domain IS INITIAL.
      rs_validation = VALUE #( is_valid = abap_false message = 'Username or domain is empty' ).
      RETURN.
    ENDIF.
    IF lv_domain CP '*.*' = abap_false.
      rs_validation = VALUE #( is_valid = abap_false message = 'Domain must contain a dot' ).
      RETURN.
    ENDIF.
    rs_validation = VALUE #( is_valid = abap_true message = 'Email is valid' ).
  ENDMETHOD.

  METHOD batch_process.
    rt_batches = VALUE #( ).
    DATA(lv_count) = lines( it_items ).
    DATA(lv_idx) = 1.
    WHILE lv_idx <= lv_count.
      DATA(lt_batch) = VALUE #( FOR i = lv_idx TO lv_idx + iv_batch_size - 1 WHERE ( i <= lv_count ) ( it_items[ i ] ) ).
      APPEND lt_batch TO rt_batches.
      lv_idx = lv_idx + iv_batch_size.
    ENDWHILE.
  ENDMETHOD.

  METHOD is_string.
    rv_is_string = abap_false.
    IF cl_abap_typedescr=>describe_by_data( iv_value )->type_kind = cl_abap_typedescr=>typekind_string.
      rv_is_string = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD is_number.
    rv_is_number = abap_false.
    DATA(lo_desc) = cl_abap_typedescr=>describe_by_data( iv_value ).
    IF lo_desc->type_kind = cl_abap_typedescr=>typekind_int OR
       lo_desc->type_kind = cl_abap_typedescr=>typekind_p OR
       lo_desc->type_kind = cl_abap_typedescr=>typekind_f.
      rv_is_number = abap_true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

"----------------------------------------------------------
" Main execution block (test/demo)
START-OF-SELECTION.

  " Test basic functions
  DATA(lv_user_name) = 'Alice'.
  DATA(lv_greeting) = lcl_functions=>greet_user( iv_name = lv_user_name ).
  WRITE: / lv_greeting.

  DATA(lv_area) = lcl_functions=>calculate_area( iv_length = 10 iv_width = 5 ).
  WRITE: / |Area: { lv_area }|.

  DATA(lv_age_group) = lcl_functions=>check_age( iv_age = 25 ).
  WRITE: / |Age group: { lv_age_group }|.

  DATA(lt_numbers) = VALUE i_tab( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) ).
  DATA(lv_total) = lcl_functions=>sum_numbers( it_numbers = lt_numbers ).
  WRITE: / |Sum: { lv_total }|.

  DATA(ls_user) = lcl_functions=>create_user( iv_name = 'John Doe' iv_email = 'john@example.com' iv_age = 30 ).
  WRITE: / |User: { ls_user-name }, { ls_user-email }, { ls_user-age }, Active: { ls_user-is_active }|.

  DATA(lt_even_nums) = lcl_functions=>filter_even_numbers( it_numbers = VALUE i_tab( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) ( 6 ) ( 7 ) ( 8 ) ( 9 ) ( 10 ) ) ).
  WRITE: / |Even numbers: { lt_even_nums }|.

  DATA(lv_formatted_name) = lcl_functions=>format_name( iv_first = 'John' iv_last = 'Doe' ).
  WRITE: / |Formatted name: { lv_formatted_name }|.

  TRY.
      DATA(lv_div_result) = lcl_functions=>divide_numbers( iv_a = 10 iv_b = 2 ).
      WRITE: / |Division result: { lv_div_result }|.
    CATCH cx_sy_arithmetic_error INTO DATA(lo_arith).
      WRITE: / |Error: { lo_arith->get_text( ) }|.
  ENDTRY.

  DATA(lv_fib_result) = lcl_functions=>fibonacci( iv_n = 8 ).
  WRITE