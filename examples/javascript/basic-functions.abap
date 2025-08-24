"----------------------------------------------------------------------  
" Basic ABAP Functions - Translated from JavaScript Examples  
"----------------------------------------------------------------------  
CLASS lcl_js_examples DEFINITION FINAL.
  PUBLIC SECTION.

    " Simple function declaration
    METHODS greet_user
      IMPORTING iv_name TYPE string
      RETURNING VALUE(rv_greeting) TYPE string.

    " Function with multiple parameters and calculations
    METHODS calculate_area
      IMPORTING iv_length TYPE i
                iv_width TYPE i
      RETURNING VALUE(rv_area) TYPE i.

    " Function with conditional logic
    METHODS check_age
      IMPORTING iv_age TYPE i
      RETURNING VALUE(rv_group) TYPE string.

    " Function with loops
    METHODS sum_numbers
      IMPORTING it_numbers TYPE STANDARD TABLE OF i WITH EMPTY KEY
      RETURNING VALUE(rv_sum) TYPE i.

    " Function with object manipulation
    METHODS create_user
      IMPORTING iv_name TYPE string
                iv_email TYPE string
                iv_age TYPE i
      RETURNING VALUE(rs_user) TYPE ty_user.

    " Function with array operations
    METHODS filter_even_numbers
      IMPORTING it_numbers TYPE STANDARD TABLE OF i WITH EMPTY KEY
      RETURNING VALUE(rt_even_numbers) TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    " Function with string operations
    METHODS format_name
      IMPORTING iv_first_name TYPE string
                iv_last_name TYPE string
      RETURNING VALUE(rv_formatted) TYPE string.

    " Function with error handling
    METHODS divide_numbers
      IMPORTING iv_a TYPE p DECIMALS 2
                iv_b TYPE p DECIMALS 2
      RETURNING VALUE(rv_result) TYPE p DECIMALS 2.

  PRIVATE SECTION.
    " User structure definition
    TYPES: BEGIN OF ty_user,
             name     TYPE string,
             email    TYPE string,
             age      TYPE i,
             is_active TYPE abap_bool,
           END OF ty_user.

ENDCLASS.

CLASS lcl_js_examples IMPLEMENTATION.

  METHOD greet_user.
    " Concatenate greeting
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
    rv_sum = REDUCE i( INIT x = 0 FOR n IN it_numbers NEXT x = x + n ).
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
    rt_even_numbers = VALUE #( FOR n IN it_numbers WHERE ( n MOD 2 = 0 ) ( n ) ).
  ENDMETHOD.

  METHOD format_name.
    rv_formatted = |{ to_upper( iv_first_name ) }, { to_upper( iv_last_name ) }|.
  ENDMETHOD.

  METHOD divide_numbers.
    TRY.
        IF iv_b = 0.
          RAISE EXCEPTION TYPE cx_sy_arithmetic_error
            EXPORTING textid = cx_sy_arithmetic_error=>division_by_zero.
        ENDIF.
        rv_result = iv_a / iv_b.
      CATCH cx_sy_arithmetic_error INTO DATA(lo_error).
        WRITE: / |Error: { lo_error->get_text( ) }|.
        CLEAR rv_result.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

"----------------------------------------------------------------------  
" Main Execution  
"----------------------------------------------------------------------  
START-OF-SELECTION.

DATA(lo_js) = NEW lcl_js_examples( ).

" Greet user
DATA(lv_user_name) = 'Alice'.
DATA(lv_greeting) = lo_js->greet_user( iv_name = lv_user_name ).
WRITE: / lv_greeting.

" Calculate area
DATA(lv_area) = lo_js->calculate_area( iv_length = 10 iv_width = 5 ).
WRITE: / |Area: { lv_area }|.

" Check age group
DATA(lv_age_group) = lo_js->check_age( iv_age = 25 ).
WRITE: / |Age group: { lv_age_group }|.

" Sum numbers
DATA(lt_numbers) = VALUE #( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) ).
DATA(lv_sum) = lo_js->sum_numbers( it_numbers = lt_numbers ).
WRITE: / |Sum: { lv_sum }|.

" Create user
DATA(ls_user) = lo_js->create_user(
  iv_name  = 'John Doe'
  iv_email = 'john@example.com'
  iv_age   = 30
).
WRITE: / |User: Name={ ls_user-name }, Email={ ls_user-email }, Age={ ls_user-age }, Active={ ls_user-is_active }|.

" Filter even numbers
DATA(lt_even_nums) = lo_js->filter_even_numbers(
  it_numbers = VALUE #( FOR i = 1 UNTIL i > 10 ( i ) )
).
WRITE: / |Even numbers: { lt_even_nums }|.

" Format name
DATA(lv_formatted_name) = lo_js->format_name(
  iv_first_name = 'John'
  iv_last_name  = 'Doe'
).
WRITE: / |Formatted name: { lv_formatted_name }|.

" Divide numbers
DATA(lv_result) = lo_js->divide_numbers( iv_a = 10 iv_b = 2 ).
WRITE: / |Division result: { lv_result }|.

**Notes:**
- All functions are implemented as methods in a local class (`lcl_js_examples`).
- The user object is mapped to a structure (`ty_user`).
- Array and object operations use ABAP table and structure constructs.
- Error handling uses ABAP exception classes and `TRY...CATCH`.
- String operations use ABAP string templates and helper function `to_upper`.
- Main execution is in `START-OF-SELECTION`, with output via `WRITE`.
- Functional style (RETURNING parameters, expressions) is used throughout.