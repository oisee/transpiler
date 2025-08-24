Certainly! Below is an idiomatic ABAP (7.40+) translation of your Go code, preserving structure, business logic, and using modern ABAP constructs and naming conventions.  
**Comments** are added for clarity and Go-to-ABAP mapping.

CLASS lcl_main DEFINITION FINAL.
  PUBLIC SECTION.

    " User structure as ABAP class
    CLASS lcl_user DEFINITION.
      PUBLIC SECTION.
        DATA name     TYPE string.
        DATA email    TYPE string.
        DATA age      TYPE i.
        DATA is_active TYPE abap_bool.

        METHODS constructor
          IMPORTING
            iv_name     TYPE string
            iv_email    TYPE string
            iv_age      TYPE i.

        METHODS get_full_info
          RETURNING VALUE(rv_info) TYPE string.
    ENDCLASS.

    " Stats structure as ABAP class
    CLASS lcl_stats DEFINITION.
      PUBLIC SECTION.
        DATA min     TYPE f.
        DATA max     TYPE f.
        DATA sum     TYPE f.
        DATA count   TYPE i.
        DATA average TYPE f.

        METHODS constructor.
    ENDCLASS.

    " Main runner
    CLASS-METHODS main.

    " Function translations
    CLASS-METHODS:
      greet_user
        IMPORTING iv_name TYPE string
        RETURNING VALUE(rv_greeting) TYPE string,

      calculate_area_perimeter
        IMPORTING iv_length TYPE f
                  iv_width  TYPE f
        EXPORTING ev_area   TYPE f
                  ev_perimeter TYPE f,

      divide_numbers
        IMPORTING iv_a TYPE f
                  iv_b TYPE f
        EXPORTING ev_result TYPE f
        RAISING   cx_sy_arithmetic_error,

      check_age
        IMPORTING iv_age TYPE i
        RETURNING VALUE(rv_group) TYPE string,

      sum_numbers
        IMPORTING it_numbers TYPE STANDARD TABLE OF i
        RETURNING VALUE(rv_sum) TYPE i,

      filter_even_numbers
        IMPORTING it_numbers TYPE STANDARD TABLE OF i
        RETURNING VALUE(rt_even) TYPE STANDARD TABLE OF i,

      count_words
        IMPORTING iv_text TYPE string
        RETURNING VALUE(rt_wordcount) TYPE HASHED TABLE OF i WITH UNIQUE KEY table_line,

      create_user
        IMPORTING iv_name TYPE string
                  iv_email TYPE string
                  iv_age TYPE i
        RETURNING VALUE(ro_user) TYPE REF TO lcl_user,

      update_user_age
        CHANGING co_user TYPE REF TO lcl_user
        IMPORTING iv_new_age TYPE i,

      calculate_average
        IMPORTING it_numbers TYPE STANDARD TABLE OF f
        RETURNING VALUE(rv_avg) TYPE f,

      process_file
        IMPORTING iv_filename TYPE string
        RAISING   cx_sy_file_open_mode,

      factorial
        IMPORTING iv_n TYPE i
        RETURNING VALUE(rv_fact) TYPE i,

      get_grade
        IMPORTING iv_score TYPE i
        RETURNING VALUE(rv_grade) TYPE string,

      process_interface
        IMPORTING iv_data TYPE any
        RETURNING VALUE(rv_result) TYPE string,

      validate_email
        IMPORTING iv_email TYPE string
        RAISING   cx_sy_conversion_error,

      calculate_stats
        IMPORTING it_numbers TYPE STANDARD TABLE OF f
        RETURNING VALUE(ro_stats) TYPE REF TO lcl_stats.

ENDCLASS.

"------------------- IMPLEMENTATION -------------------

CLASS lcl_main IMPLEMENTATION.

  " User class implementation
  CLASS lcl_user IMPLEMENTATION.
    METHOD constructor.
      name = iv_name.
      email = iv_email.
      age = iv_age.
      is_active = abap_true.
    ENDMETHOD.

    METHOD get_full_info.
      DATA(lv_status) = COND string( WHEN is_active = abap_true THEN 'active' ELSE 'inactive' ).
      rv_info = |Name: { name }, Email: { email }, Age: { age }, Status: { lv_status }|.
    ENDMETHOD.
  ENDCLASS.

  " Stats class implementation
  CLASS lcl_stats IMPLEMENTATION.
    METHOD constructor.
      min = 0.
      max = 0.
      sum = 0.
      count = 0.
      average = 0.
    ENDMETHOD.
  ENDCLASS.

  " Greet user
  METHOD greet_user.
    rv_greeting = |Hello, { iv_name }!|.
  ENDMETHOD.

  " Area and perimeter
  METHOD calculate_area_perimeter.
    ev_area = iv_length * iv_width.
    ev_perimeter = 2 * ( iv_length + iv_width ).
  ENDMETHOD.

  " Divide numbers with error
  METHOD divide_numbers.
    IF iv_b = 0.
      RAISE EXCEPTION TYPE cx_sy_arithmetic_error
        EXPORTING textid = cx_sy_arithmetic_error=>division_by_zero.
    ENDIF.
    ev_result = iv_a / iv_b.
  ENDMETHOD.

  " Check age
  METHOD check_age.
    rv_group = COND string( WHEN iv_age >= 18 THEN 'Adult' ELSE 'Minor' ).
  ENDMETHOD.

  " Sum numbers
  METHOD sum_numbers.
    rv_sum = REDUCE i( INIT x = 0 FOR n IN it_numbers NEXT x = x + n ).
  ENDMETHOD.

  " Filter even numbers
  METHOD filter_even_numbers.
    rt_even = VALUE #( FOR n IN it_numbers WHERE ( n MOD 2 = 0 ) ( n ) ).
  ENDMETHOD.

  " Count words
  METHOD count_words.
    DATA(lt_words) = VALUE string_table( ).
    SPLIT iv_text AT space INTO TABLE lt_words.
    DATA(lt_lower) = VALUE string_table( FOR w IN lt_words ( to_lower( w ) ) ).
    DATA lt_wordcount TYPE HASHED TABLE OF i WITH UNIQUE KEY table_line.
    LOOP AT lt_lower INTO DATA(lv_word).
      READ TABLE lt_wordcount WITH TABLE KEY table_line = lv_word TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        lt_wordcount[ table_line = lv_word ] += 1.
      ELSE.
        INSERT lv_word INTO TABLE lt_wordcount.
        lt_wordcount[ table_line = lv_word ] = 1.
      ENDIF.
    ENDLOOP.
    rt_wordcount = lt_wordcount.
  ENDMETHOD.

  " Create user
  METHOD create_user.
    ro_user = NEW lcl_user( iv_name = iv_name iv_email = iv_email iv_age = iv_age ).
  ENDMETHOD.

  " Update user age
  METHOD update_user_age.
    co_user->age = iv_new_age.
  ENDMETHOD.

  " Calculate average
  METHOD calculate_average.
    IF lines( it_numbers ) = 0.
      rv_avg = 0.
      RETURN.
    ENDIF.
    DATA(lv_sum) = REDUCE f( INIT x = 0 FOR n IN it_numbers NEXT x = x + n ).
    rv_avg = lv_sum / lines( it_numbers ).
  ENDMETHOD.

  " Process file with CLEANUP (defer)
  METHOD process_file.
    TRY.
        IF iv_filename IS INITIAL.
          RAISE EXCEPTION TYPE cx_sy_file_open_mode
            EXPORTING textid = cx_sy_file_open_mode=>file_not_found.
        ENDIF.
        WRITE: |Opening file: { iv_filename }|.
        CLEANUP.
          WRITE: |Closing file: { iv_filename }|.
        ENDTRY.
        WRITE: |Processing file: { iv_filename }|.
      CATCH cx_sy_file_open_mode INTO DATA(lo_ex).
        RAISE lo_ex.
    ENDTRY.
  ENDMETHOD.

  " Factorial (recursive)
  METHOD factorial.
    rv_fact = COND #( WHEN iv_n <= 1 THEN 1 ELSE iv_n * me->factorial( iv_n - 1 ) ).
  ENDMETHOD.

  " Get grade (switch)
  METHOD get_grade.
    rv_grade = SWITCH string(
      WHEN iv_score >= 90 THEN 'A'
      WHEN iv_score >= 80 THEN 'B'
      WHEN iv_score >= 70 THEN 'C'
      WHEN iv_score >= 60 THEN 'D'
      ELSE 'F'
    ).
  ENDMETHOD.

  " Process interface (type assertion)
  METHOD process_interface.
    CASE type of iv_data.
      WHEN string.
        rv_result = |String: { iv_data }|.
      WHEN i.
        rv_result = |Integer: { iv_data }|.
      WHEN f.
        rv_result = |Float: { iv_data }|.
      WHEN abap_bool.
        rv_result = |Boolean: { iv_data }|.
      WHEN OTHERS.
        rv_result = 'Unknown type'.
    ENDCASE.
  ENDMETHOD.

  " Validate email (error handling)
  METHOD validate_email.
    IF iv_email IS INITIAL.
      RAISE EXCEPTION TYPE cx_sy_conversion_error
        EXPORTING textid = cx_sy_conversion_error=>input_invalid.
    ENDIF.
    IF iv_email CP '*@*'.
      SPLIT iv_email AT '@' INTO DATA(lv_user) DATA(lv_domain).
      IF lv_user IS INITIAL OR lv_domain IS INITIAL.
        RAISE EXCEPTION TYPE cx_sy_conversion_error
          EXPORTING textid = cx_sy_conversion_error=>input_invalid.
      ENDIF.
      IF lv_domain CP '*.*'.
        RETURN.
      ELSE.
        RAISE EXCEPTION TYPE cx_sy_conversion_error
          EXPORTING textid = cx_sy_conversion_error=>input_invalid.
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE cx_sy_conversion_error
        EXPORTING textid = cx_sy_conversion_error=>input_invalid.
    ENDIF.
  ENDMETHOD.

  " Calculate stats
  METHOD calculate_stats.
    ro_stats = NEW lcl_stats( ).
    IF lines( it_numbers ) = 0.
      RETURN.
    ENDIF.
    ro_stats->min = it_numbers[ 1 ].
    ro_stats->max = it_numbers[ 1 ].
    ro_stats->count = lines( it_numbers ).
    LOOP AT it_numbers INTO DATA(lv_num).
      ro_stats->sum += lv_num.
      IF lv_num < ro_stats->min.
        ro_stats->min = lv_num.
      ENDIF.
      IF lv_num > ro_stats->max.
        ro_stats->max = lv_num.
      ENDIF.
    ENDLOOP.
    ro_stats->average = ro_stats->sum / ro_stats->count.
  ENDMETHOD.

  " Main method
  METHOD main.
    WRITE: / '=== Basic Function Tests ==='.

    DATA(lv_user_name) = 'Alice'.
    DATA(lv_greeting) = greet_user( lv_user_name ).
    WRITE: / lv_greeting.

    DATA(lv_area) TYPE f.
    DATA(lv_perimeter) TYPE f.
    calculate_area_perimeter( iv_length = 10 iv_width = 5
      IMPORTING ev_area = lv_area ev_perimeter = lv_perimeter ).
    WRITE: / |Area: { lv_area }, Perimeter: { lv_perimeter }|.

    TRY.
        DATA(lv_result) TYPE f.
        divide_numbers( iv_a = 10 iv_b = 2 EXPORTING ev_result = lv_result ).
        WRITE: / |Division result: { lv_result }|.
      CATCH cx_sy_arithmetic_error INTO DATA(lo_div_err).
        WRITE: / |Error: { lo_div_err->get_text( ) }|.
    ENDTRY.

    DATA(lv_age_group) = check_age( 25 ).
    WRITE: / |Age group: { lv_age_group }|.

    DATA(lt_numbers) = VALUE i_tab( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) ).
    DATA(lv_sum) = sum_numbers( lt_numbers ).
    WRITE: / |Sum: { lv_sum }|.

    DATA(lt_even_nums) = filter_even_numbers( VALUE i_tab( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) ( 6 ) ( 7 ) ( 8 ) ( 9 ) ( 10 ) ) ).
    WRITE: / |Even numbers: { lt_even_nums }|.

    DATA(lt_wordcount) = count_words( 'hello world hello go' ).
    LOOP AT lt_wordcount INTO DATA(lv_word).
      WRITE: / |Word: { lv_word }, Count: { lt_wordcount[ lv_word ] }|.
    ENDLOOP.

    WRITE: / '=== Struct and Method Tests ==='.

    DATA(lo_user) = create_user( 'John Doe' 'john@example.com' 30 ).
    WRITE: / lo_user->get_full_info( ).

    update_user_age( CHANGING co_user = lo_user iv_new_age = 31 ).
    WRITE: / |Updated age: { lo_user->age }|.

    DATA(lv_avg) = calculate_average( VALUE f_tab( ( 1.5 ) ( 2.5 ) ( 3.5 ) ( 4.5 ) ( 5.5 ) ) ).
    WRITE: / |Average: { lv_avg }|.

    WRITE: / '=== File Processing Test ==='.
    TRY.
        process_file( 'test.txt' ).
      CATCH cx_sy_file_open_mode INTO DATA(lo_file_err).
        WRITE: / |File processing error: { lo_file_err->get_text( ) }|.
    ENDTRY.

    WRITE: / '=== Recursive Function Test ==='.
    DATA(lv_fact) = factorial( 5 ).
    WRITE: / |Factorial of 5: { lv_fact }|.

    WRITE: / '=== Switch Statement Test ==='.
    DATA(lt_scores) = VALUE i_tab( ( 95 ) ( 87 ) ( 72 ) ( 64 ) ( 58 ) ).
    LOOP AT lt_scores INTO DATA(lv_score).
      DATA(lv_grade) = get_grade( lv_score ).
      WRITE: / |Score { lv_score }: Grade { lv_grade }|.
    ENDLOOP.

    WRITE: / '=== Interface Processing Test ==='.
    " ABAP doesn't have true interface{}; simulate with different types
    DATA(lt_test_data) = VALUE string_tab( ( 'hello' ) ( '42' ) ( '3.14' ) ( 'X' ) ).