module module_model_coupling_ipc

  use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer, c_sizeof
  implicit none

  enum, bind(c)
    enumerator::field_idx_var_id = 1
    enumerator::field_idx_var_unit_size
    enumerator::field_idx_var_total_units
    enumerator::total_fields_plus_1
  end enum

  enum, bind(c)
    enumerator::var_id_bathymetry = 1
    enumerator::var_id_max_mean_water_level
    enumerator::var_id_test_ls
    enumerator::var_id_test_tab
    enumerator::total_vars_plus_1
  end enum

  type :: type_var_info
    character (len = 64) :: name
    character (len = 32) :: type
    integer :: unit_size
    integer, allocatable :: shape(:)
    integer, allocatable :: column_effective_length(:)
  end type

  type :: type_header_field
    character (len = 64) :: name
    character (len = 32) :: type
    integer :: unit_size
  end type

  type :: type_model_coupling_ipc

    type (type_header_field), dimension(total_fields_plus_1 - 1) :: &
      table_protocol_definition
    type (type_var_info), dimension(total_vars_plus_1 - 1) :: &
      table_var_info

    contains

    procedure :: InitializeProtocolDefinition => InitializeProtocolDefinition
    procedure :: InitializeVarInfo => InitializeVarInfo
    !procedure :: FinalizeVarInfo => FinalizeVarInfo
    procedure :: SendVar => SendVar
    procedure :: RecvVar => RecvVar
    procedure :: WriteNBytes => WriteNBytes
    procedure :: ReadNBytes => ReadNBytes

  end type

  contains

  function InitializeProtocolDefinition(this) result (status)
    implicit none

    class (type_model_coupling_ipc), intent(inout) :: this
    integer :: status
    integer :: var_integer

    this%table_protocol_definition(field_idx_var_id)%name = &
      "var_id"
    this%table_protocol_definition(field_idx_var_id)%type = &
      "integer"
    this%table_protocol_definition(field_idx_var_id)%unit_size = &
      int(c_sizeof(var_integer))

    this%table_protocol_definition(field_idx_var_unit_size)%name = &
      "unit_size"
    this%table_protocol_definition(field_idx_var_unit_size)%type = &
      "integer"
    this%table_protocol_definition(field_idx_var_unit_size)%unit_size = &
      int(c_sizeof(var_integer))

    this%table_protocol_definition(field_idx_var_total_units)%name = &
      "total_units"
    this%table_protocol_definition(field_idx_var_total_units)%type = &
      "integer"
    this%table_protocol_definition(field_idx_var_total_units)%unit_size = &
      int(c_sizeof(var_integer))

    status = 0
  end function InitializeProtocolDefinition

  function InitializeVarInfo(this, &
    var_id, name, var_type, unit_size, var_shape, column_effective_length) &
    result (status)
    implicit none

    class (type_model_coupling_ipc), intent(inout) :: this
    integer, intent(in) :: var_id
    character (len = *), intent(in) :: name
    character (len = *), intent(in) :: var_type
    integer, intent(in) :: unit_size
    integer, intent(in) :: var_shape(:)
    integer, intent(in) :: column_effective_length(:)
    integer :: status

    this%table_var_info(var_id)%name = name
    this%table_var_info(var_id)%type = var_type
    this%table_var_info(var_id)%unit_size = unit_size
    allocate(this%table_var_info(var_id)%shape(size(var_shape)), stat = status)
    if (status /= 0) then
      status = 1
      return
    end if
    this%table_var_info(var_id)%shape = var_shape
    print *, "var_info var shape: ", this%table_var_info(var_id)%shape
    allocate(this%table_var_info(var_id)%column_effective_length( &
      size(column_effective_length)), stat = status)
    if (status /= 0) then
      status = 2
      return
    end if
    this%table_var_info(var_id)%column_effective_length = &
      column_effective_length
    print *, "var_info col_effective_len: ", column_effective_length

    status = 0
  end function InitializeVarInfo

  function SendVar(this, stream, var_id, buffer) result (status)
    implicit none

    class (type_model_coupling_ipc), intent(inout) :: this
    integer, intent(in) :: stream
    integer, intent(in) :: var_id
    type (c_ptr), intent(in) :: buffer
    integer :: status
    integer :: i, j, array_dimension, buffer_size, begin_index
    integer, target :: int_buffer(1)
    integer, pointer :: int_ptr(:)
    type (c_ptr) :: generic_ptr

!    print *, "SendVar var id: ", var_id
    ! send Field var_id
    int_buffer(1) = var_id
    int_ptr => int_buffer
    generic_ptr = c_loc(int_ptr(1))
    status = this%WriteNBytes(stream, generic_ptr, &
        this%table_protocol_definition(field_idx_var_id)%unit_size, &
        1, &
        this%table_protocol_definition(field_idx_var_id)%unit_size)
    if (status /= 0) then
      status = 1
      return
    end if
!    print *, "SendVar send ", &
!      this%table_protocol_definition(field_idx_var_id)%unit_size

    ! send Field unit size
    ! int_buffer(1) = this%table_var_info(var_id)%unit_size
    ! int_ptr => int_buffer
    ! generic_ptr = c_loc(int_ptr(1))
    ! status = this%WriteNBytes(stream, generic_ptr, &
    !     this%table_protocol_definition(field_idx_var_total_units)%unit_size, &
    !     1, &
    !     this%table_protocol_definition(field_idx_var_total_units)%unit_size)
    ! if (status /= 0) then
    !   status = 2
    !   return
    ! end if

    ! send Field total values
    ! array_dimension = size(this%table_var_info(var_id)%shape)
    ! if (array_dimension == 0) then
    !   int_buffer(1) = 1
    ! else if (array_dimension == 1) then
    !   int_buffer(1) = this%table_var_info(var_id)%column_effective_length(1)
    ! else if (array_dimension == 2) then
    !   int_buffer(1) = 0
    !   do i = 1, size(this%table_var_info(var_id)%column_effective_length), 1
    !     int_buffer(1) = int_buffer(1) + &
    !       this%table_var_info(var_id)%column_effective_length(i)
    !   end do
    ! else
    !   ! higher-dimension arrays are not supported
    !   status = 3
    !   return
    ! end if
    ! int_ptr => int_buffer
    ! generic_ptr = c_loc(int_ptr(1))
    ! status = this%WriteNBytes(stream, generic_ptr, &
    !     this%table_protocol_definition(field_idx_var_total_units)%unit_size, &
    !     1, &
    !     this%table_protocol_definition(field_idx_var_total_units)%unit_size)
    ! if (status /= 0) then
    !   status = 4
    !   return
    ! end if

    ! send payload
    array_dimension = size(this%table_var_info(var_id)%shape)
    if (array_dimension == 0) then
      ! send 1 * unit size
      status = this%WriteNBytes(stream, buffer, &
        this%table_var_info(var_id)%unit_size, &
        1, &
        this%table_var_info(var_id)%unit_size)
      if (status /= 0) then
        status = 5
        return
      end if
    else if (array_dimension == 1) then
      ! send column_effective_length(1) * unit size
      status = this%WriteNBytes(stream, buffer, &
        this%table_var_info(var_id)%shape(1) * &
          this%table_var_info(var_id)%unit_size, &
        1, &
        this%table_var_info(var_id)%column_effective_length(1) * &
          this%table_var_info(var_id)%unit_size)
      if (status /= 0) then
        status = 6
        return
      end if
    else if (array_dimension == 2) then
      ! for every shape(1) values, send column_effective_length * unit size
      buffer_size = &
        this%table_var_info(var_id)%shape(1) * &
          this%table_var_info(var_id)%shape(2)
      begin_index = 1
      do i = 1, size(this%table_var_info(var_id)%column_effective_length), 1
        status = this%WriteNBytes(stream, buffer, &
          buffer_size * this%table_var_info(var_id)%unit_size, &
          begin_index, &
          this%table_var_info(var_id)%column_effective_length(i) * &
            this%table_var_info(var_id)%unit_size)
        if (status /= 0) then
          status = 7
          return
        end if

!        print *, "SendVar send ", &
!          this%table_var_info(var_id)%column_effective_length(i) * &
!            this%table_var_info(var_id)%unit_size

        begin_index = begin_index + &
          this%table_var_info(var_id)%shape(1) * &
          this%table_var_info(var_id)%unit_size
      end do
    end if

    ! flush buffered write operations
    flush(stream, iostat=status)
    if (status /= 0) then
      status = 8
      return
    end if

    status = 0
  end function SendVar

  function RecvVar(this, stream, var_id, buffer) result (status)
    implicit none

    class (type_model_coupling_ipc), intent(inout) :: this
    integer, intent(in) :: stream
    integer, intent(in) :: var_id
    type (c_ptr), intent(in) :: buffer
    integer :: status
    integer :: i, j, array_dimension, buffer_size, begin_index
    integer, target :: int_buffer(1)
    integer, pointer :: int_ptr(:)
    type (c_ptr) :: generic_ptr

    ! receive Field var_id
    int_ptr => int_buffer
    generic_ptr = c_loc(int_ptr(1))
    status = this%ReadNBytes(stream, generic_ptr, &
      this%table_protocol_definition(field_idx_var_id)%unit_size, &
      1, &
      this%table_protocol_definition(field_idx_var_id)%unit_size)
    if (status /= 0) then
      status = 1
      return
    end if

    if (int_buffer(1) /= var_id) then
      status = 2
      return
    end if

!    print *, "RecvVar var id: ", int_buffer(1)
!    print *, "RecvVar var shape: ", this%table_var_info(var_id)%shape

    ! receive payload
    array_dimension = size(this%table_var_info(var_id)%shape)
    if (array_dimension == 0) then
      ! receive 1 * unit size
      status = this%ReadNBytes(stream, buffer, &
        this%table_var_info(var_id)%unit_size, &
        1, &
        this%table_var_info(var_id)%unit_size)
      if (status /= 0) then
        status = 3
        return
      end if
    else if (array_dimension == 1) then
      ! receive column_effective_length(1) * unit size
      status = this%ReadNBytes(stream, buffer, &
        this%table_var_info(var_id)%shape(1) * &
          this%table_var_info(var_id)%unit_size, &
        1, &
        this%table_var_info(var_id)%column_effective_length(1) * &
          this%table_var_info(var_id)%unit_size)
      if (status /= 0) then
        status = 4
        return
      end if
    else if (array_dimension == 2) then
      ! for every shape(1) values, receive column_effective_length * unit size
      buffer_size = &
        this%table_var_info(var_id)%shape(1) * &
          this%table_var_info(var_id)%shape(2)
      begin_index = 1
      do i = 1, size(this%table_var_info(var_id)%column_effective_length), 1
        status = this%ReadNBytes(stream, buffer, &
          buffer_size * this%table_var_info(var_id)%unit_size, &
          begin_index, &
          this%table_var_info(var_id)%column_effective_length(i) * &
            this%table_var_info(var_id)%unit_size)
        if (status /= 0) then
          status = 5
          return
        end if
        begin_index = begin_index + &
          this%table_var_info(var_id)%shape(1) * &
          this%table_var_info(var_id)%unit_size
      end do
    end if

    status = 0
  end function RecvVar

  function ReadNBytes(this, &
    stream, buffer, buffer_size, begin_index, n) result (status)
    implicit none

    class (type_model_coupling_ipc), intent(inout) :: this
    integer, intent(in) :: stream
    type (c_ptr), intent(in) :: buffer
    integer, intent(in) :: buffer_size
    integer, intent(in) :: begin_index
    integer, intent(in) :: n
    integer :: status, i
    character, pointer :: char_ptr(:)

    call c_f_pointer(buffer, char_ptr, [buffer_size])
    ! TO DO: accelerate with integer(kind = 4) and real(kind = 8) buffers
    ! modulo() for the remainder
    read (stream, iostat = status) (char_ptr(i), &
      i = begin_index, begin_index + n - 1)
    if (status /= 0) then
      status = 1
      return
    end if
    !print *, (char_ptr(i), i = begin_index, begin_index + n - 1)

    status = 0
  end function ReadNBytes

  function WriteNBytes(this, &
    stream, buffer, buffer_size, begin_index, n) result (status)
    implicit none

    class (type_model_coupling_ipc), intent(inout) :: this
    integer, intent(in) :: stream
    type (c_ptr), intent(in) :: buffer
    integer, intent(in) :: buffer_size
    integer, intent(in) :: begin_index
    integer, intent(in) :: n
    integer :: status, i
    character, pointer :: char_ptr(:)
    integer, pointer :: int_ptr(:)
    double precision :: dp
    double precision, pointer :: dp_ptr(:)

    call c_f_pointer(buffer, char_ptr, [buffer_size])
    ! TO DO: accelerate with integer(kind = 4) and real(kind = 8) buffers
    ! modulo() for the remainder
    write (stream, iostat = status) (char_ptr(i), &
      i = begin_index, begin_index + n - 1)
    if (status /= 0) then
      status = 1
      return
    end if
    ! print *, "as char:"
    ! print *, (char_ptr(i), i = begin_index, begin_index + n - 1)
    ! call c_f_pointer(buffer, int_ptr, [buffer_size / int(c_sizeof(i))])
    ! print *, "as int:"
    ! print *, (int_ptr(i), i = begin_index / int(c_sizeof(i)) + 1, (begin_index + n - 1) / int(c_sizeof(i)))
    ! call c_f_pointer(buffer, dp_ptr, [buffer_size / int(c_sizeof(dp))])
    ! print *, "as dp:"
    ! print *, (dp_ptr(i), i = begin_index / int(c_sizeof(dp)) + 1, (begin_index + n - 1) / int(c_sizeof(dp)))
    ! print *, ""

    status = 0
  end function WriteNBytes

end module module_model_coupling_ipc
