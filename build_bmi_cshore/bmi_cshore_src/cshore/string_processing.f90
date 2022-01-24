module string_processing
  implicit none

  contains

  function FindFirstInSet(str, set, idx) result (status)
    character(len=*), intent(in) :: str
    character(len=*), intent(in) :: set
    integer, intent(out) :: idx
    logical :: status

    idx = scan(str, set)

    if (0 < idx) then
      status = .true.
    else
      status = .false.
    end if

  end function

  function FindFirstNotInSet(str, set, idx) result (status)
    character(len=*), intent(in) :: str
    character(len=*), intent(in) :: set
    integer, intent(out) :: idx
    logical :: status
    integer :: i

    idx = 0
    do i = 1, len(str), 1
      if (scan(str(i:i), set) == 0) then
        idx = i
        exit
      end if
    end do

    if (0 < idx) then
      status = .true.
    else
      status = .false.
    end if

  end function

  function ParseAKeyValuePair( &
    str, &
    key_char_set, &
    separator_char_set, &
    value_char_set, &
    key_begin_idx, key_end_idx, &
    value_begin_idx, value_end_idx &
    ) result (status)

    character(len=*), intent(in) :: str
    character(len=*), intent(in) :: key_char_set
    character(len=*), intent(in) :: separator_char_set
    character(len=*), intent(in) :: value_char_set
    integer, intent(out) :: key_begin_idx, key_end_idx
    integer, intent(out) :: value_begin_idx, value_end_idx
    logical :: status
    integer :: separator_idx, temp_idx

    ! strip space
    status = FindFirstNotInSet( &
      str(1: len(str)), " "//char(9), key_begin_idx)
    if (.not. status) then
      status = .false.
      return
    end if

    key_begin_idx = 1 - 1 + key_begin_idx
    ! print *, "kb ", key_begin_idx

    ! find key
    status = FindFirstInSet( &
      str(key_begin_idx: len(str)), " :"//char(9), key_end_idx)
    if (.not. status) then
      status = .false.
      return
    end if

    key_end_idx = key_begin_idx - 1 + key_end_idx - 1
    ! print *, "ke ", key_end_idx

    ! validate key
    status = FindFirstNotInSet( &
      str(key_begin_idx: key_end_idx), key_char_set, temp_idx)
    if (status) then
      status = .false.
      return
    end if

    ! strip space
    status = FindFirstNotInSet( &
      str(key_end_idx + 1: len(str)), " "//char(9), temp_idx)
    if (.not. status) then
      status = .false.
      return
    end if

    temp_idx = key_end_idx + temp_idx
    ! print *, "t ", temp_idx

    ! find separator
    status = FindFirstInSet( &
      str(temp_idx: len(str)), separator_char_set, separator_idx)
    if (.not. status) then
      status = .false.
      return
    end if

    separator_idx = temp_idx - 1 + separator_idx
    ! print *, "s ", separator_idx

    ! strip space
    status = FindFirstNotInSet( &
      str(separator_idx + 1: len(str)), " "//char(9), value_begin_idx)
    if (.not. status) then
      status = .false.
      return
    end if

    value_begin_idx = separator_idx + value_begin_idx
    ! print *, "vb ", value_begin_idx

    ! find value
    status = FindFirstInSet( &
      str(value_begin_idx: len(str)), " "//char(9), value_end_idx)
    if (.not. status) then
      value_end_idx = len(str)
    else
      value_end_idx = value_begin_idx - 1 + value_end_idx - 1
    end if

    ! print *, "ve ", value_end_idx

    ! validate value
    status = FindFirstNotInSet( &
      str(value_begin_idx: value_end_idx), value_char_set, temp_idx)
    if (status) then
      status = .false.
      return
    end if

    status = .true.

  end function

end module
