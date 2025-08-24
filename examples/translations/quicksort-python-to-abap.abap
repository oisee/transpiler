*&---------------------------------------------------------------------*
*& Modern ABAP translation from Python quicksort function
*& Original Python:
*& def quicksort(arr):
*&     if len(arr) <= 1:
*&         return arr
*&     pivot = arr[len(arr) // 2]
*&     left = [x for x in arr if x < pivot]
*&     middle = [x for x in arr if x == pivot]  
*&     right = [x for x in arr if x > pivot]
*&     return quicksort(left) + middle + quicksort(right)
*&---------------------------------------------------------------------*

" Type definitions
TYPES: tt_int_table TYPE TABLE OF i.

CLASS lcl_sorting_algorithms DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: quicksort
      IMPORTING
        it_array TYPE tt_int_table
      RETURNING
        VALUE(rt_sorted) TYPE tt_int_table.
ENDCLASS.

CLASS lcl_sorting_algorithms IMPLEMENTATION.
  METHOD quicksort.
    " Base case: array with 0 or 1 element
    DATA(lv_size) = lines( it_array ).
    IF lv_size <= 1.
      rt_sorted = it_array.
      RETURN.
    ENDIF.
    
    " Choose pivot (middle element)
    DATA(lv_pivot_index) = lv_size DIV 2.
    DATA(lv_pivot) = it_array[ lv_pivot_index ].
    
    " Partition using modern ABAP VALUE expressions (Python list comprehensions equivalent)
    DATA(lt_left) = VALUE tt_int_table(
      FOR lv_value IN it_array
      WHERE ( lv_value < lv_pivot )
      ( lv_value )
    ).
    
    DATA(lt_middle) = VALUE tt_int_table(
      FOR lv_value IN it_array
      WHERE ( lv_value = lv_pivot )
      ( lv_value )
    ).
    
    DATA(lt_right) = VALUE tt_int_table(
      FOR lv_value IN it_array
      WHERE ( lv_value > lv_pivot )
      ( lv_value )
    ).
    
    " Recursively sort and combine (modern ABAP equivalent of Python's concatenation)
    DATA(lt_sorted_left) = quicksort( lt_left ).
    DATA(lt_sorted_right) = quicksort( lt_right ).
    
    " Combine results: sorted_left + middle + sorted_right
    rt_sorted = VALUE #( BASE lt_sorted_left
                         ( LINES OF lt_middle )
                         ( LINES OF lt_sorted_right ) ).
  ENDMETHOD.
ENDCLASS.

" Test program
START-OF-SELECTION.
  " Initialize test array using VALUE expression: [3, 1, 4, 1, 5, 9, 2, 6, 5]
  DATA(lt_numbers) = VALUE tt_int_table( ( 3 ) ( 1 ) ( 4 ) ( 1 ) ( 5 ) ( 9 ) ( 2 ) ( 6 ) ( 5 ) ).
  
  " Sort the array
  DATA(lt_sorted) = lcl_sorting_algorithms=>quicksort( lt_numbers ).
  
  " Display results using string templates
  WRITE: / 'Original array:'.
  LOOP AT lt_numbers INTO DATA(lv_number).
    WRITE: lv_number, space.
  ENDLOOP.
  
  WRITE: / 'Sorted array:'.
  LOOP AT lt_sorted INTO lv_number.
    WRITE: lv_number, space.
  ENDLOOP.