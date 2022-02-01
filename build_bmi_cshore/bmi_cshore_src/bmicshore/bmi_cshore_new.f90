module bmi_cshore_module

  use cshore_module
  use bmif_2_0
  use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer
  implicit none

  type :: exp_var_info_type
    character (len = BMI_MAX_VAR_NAME) :: name
    character (len = 64) :: cshore_var_name
    integer :: grid_id
    integer :: grid_rank
    integer, pointer :: grid_shape(:)
    character (len = BMI_MAX_UNITS_NAME) :: unit
    character (len = BMI_MAX_TYPE_NAME) :: type
  end type

  enum, bind(c)
      enumerator::var_io_idx_bathymetry = 1
      enumerator::total_exp_var_io_plus_1
  end enum

  enum, bind(c)
      enumerator::var_i_idx_mainloop_itime=1
      enumerator::total_exp_var_i_plus_1
  end enum

  enum, bind(c)
      enumerator::var_o_idx_grid_per_transect = 1
      enumerator::var_o_idx_total_transects
      enumerator::var_o_idx_ntime
      enumerator::var_o_idx_maxmwl
      enumerator::var_o_idx_setup
      enumerator::total_exp_var_o_plus_1
  end enum

  type, extends (bmi) :: bmi_cshore_type
    private
      type (cshore_vars_type) :: cshore_vars
      type (exp_var_info_type), dimension(total_exp_var_io_plus_1 - 1) :: &
        exp_var_info_table_io
      type (exp_var_info_type), dimension(total_exp_var_i_plus_1 - 1) :: &
        exp_var_info_table_i
      type (exp_var_info_type), dimension(total_exp_var_o_plus_1 - 1) :: &
        exp_var_info_table_o

    contains
     procedure :: get_component_name => cshore_component_name
     procedure :: get_input_item_count => cshore_input_item_count
     procedure :: get_output_item_count => cshore_output_item_count
     procedure :: get_input_var_names => cshore_input_var_names
     procedure :: get_output_var_names => cshore_output_var_names
     procedure :: initialize => cshore_initialize
     procedure :: finalize => cshore_finalize
     procedure :: update => cshore_update
     procedure :: get_start_time => heat_start_time
     procedure :: get_end_time => heat_end_time
     procedure :: get_current_time => heat_current_time
     procedure :: get_time_step => heat_time_step
     procedure :: get_time_units => heat_time_units
     procedure :: update_until => heat_update_until
     procedure :: get_var_grid => cshore_var_grid
     procedure :: get_grid_type => heat_grid_type
     procedure :: get_grid_rank => heat_grid_rank
     procedure :: get_grid_shape => cshore_grid_shape
     procedure :: get_grid_size => cshore_grid_size
     procedure :: get_grid_spacing => heat_grid_spacing
     procedure :: get_grid_origin => heat_grid_origin
     procedure :: get_grid_x => heat_grid_x
     procedure :: get_grid_y => heat_grid_y
     procedure :: get_grid_z => heat_grid_z
     procedure :: get_grid_node_count => heat_grid_node_count
     procedure :: get_grid_edge_count => heat_grid_edge_count
     procedure :: get_grid_face_count => heat_grid_face_count
     procedure :: get_grid_edge_nodes => heat_grid_edge_nodes
     procedure :: get_grid_face_edges => heat_grid_face_edges
     procedure :: get_grid_face_nodes => heat_grid_face_nodes
     procedure :: get_grid_nodes_per_face => heat_grid_nodes_per_face
     procedure :: get_var_type => cshore_var_type
     procedure :: get_var_units => cshore_var_units
     procedure :: get_var_itemsize => heat_var_itemsize
     procedure :: get_var_nbytes => heat_var_nbytes
     procedure :: get_var_location => heat_var_location
     procedure :: get_value_int => cshore_get_int
     procedure :: get_value_float => heat_get_float
     procedure :: get_value_double => cshore_get_double
     generic :: get_value => &
          get_value_int, &
          get_value_float, &
          get_value_double
     procedure :: get_value_ptr_int => heat_get_ptr_int
     procedure :: get_value_ptr_float => heat_get_ptr_float
     procedure :: get_value_ptr_double => cshore_get_ptr_double
     generic :: get_value_ptr => &
          get_value_ptr_int, &
          get_value_ptr_float, &
          get_value_ptr_double
     procedure :: get_value_at_indices_int => heat_get_at_indices_int
     procedure :: get_value_at_indices_float => heat_get_at_indices_float
     procedure :: get_value_at_indices_double => heat_get_at_indices_double
     generic :: get_value_at_indices => &
          get_value_at_indices_int, &
          get_value_at_indices_float, &
          get_value_at_indices_double
     procedure :: set_value_int => cshore_set_int
     procedure :: set_value_float => heat_set_float
     procedure :: set_value_double => cshore_set_double
     generic :: set_value => &
          set_value_int, &
          set_value_float, &
          set_value_double
     procedure :: set_value_at_indices_int => heat_set_at_indices_int
     procedure :: set_value_at_indices_float => heat_set_at_indices_float
     procedure :: set_value_at_indices_double => heat_set_at_indices_double
     generic :: set_value_at_indices => &
          set_value_at_indices_int, &
          set_value_at_indices_float, &
          set_value_at_indices_double
     procedure :: print_model_info
  end type bmi_cshore_type

  private
  public :: bmi_cshore_type

  character (len=BMI_MAX_COMPONENT_NAME), target :: &
       component_name = "CSHORE-VEG-BMI"

  ! Exchange items
  !integer, parameter :: input_item_count = 3
  !integer, parameter :: output_item_count = 1
  character (len=BMI_MAX_VAR_NAME), target, &
      dimension( &
        total_exp_var_io_plus_1 - 1 + total_exp_var_i_plus_1 - 1) :: &
        input_items
  character (len=BMI_MAX_VAR_NAME), target, &
      dimension( &
        total_exp_var_io_plus_1 - 1 + total_exp_var_o_plus_1 - 1) :: &
        output_items
       !output_items = (/'plate_surface__temperature'/)

contains

  ! Get the name of the model.
  function cshore_component_name(this, name) result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    character (len=*), pointer, intent(out) :: name
    integer :: bmi_status

    name => component_name
    bmi_status = BMI_SUCCESS
  end function cshore_component_name

  ! Count the input variables.
  function cshore_input_item_count(this, count) result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    integer, intent(out) :: count
    integer :: bmi_status

    count = total_exp_var_io_plus_1 - 1 + total_exp_var_i_plus_1 - 1
    bmi_status = BMI_SUCCESS
  end function cshore_input_item_count

  ! Count the output variables.
  function cshore_output_item_count(this, count) result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    integer, intent(out) :: count
    integer :: bmi_status

    count = total_exp_var_io_plus_1 - 1 + total_exp_var_o_plus_1 - 1
    bmi_status = BMI_SUCCESS
  end function cshore_output_item_count

  ! List input variables.
  function cshore_input_var_names(this, names) result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    character (*), pointer, intent(out) :: names(:)
    integer :: bmi_status

    !input_items(1) = 'plate_surface__temperature'
    !input_items(2) = 'plate_surface__thermal_diffusivity'
    !input_items(3) = 'model__identification_number'

    names => input_items
    bmi_status = BMI_SUCCESS
  end function cshore_input_var_names

  ! List output variables.
  function cshore_output_var_names(this, names) result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    character (*), pointer, intent(out) :: names(:)
    integer :: bmi_status

    names => output_items
    bmi_status = BMI_SUCCESS
  end function cshore_output_var_names

  ! BMI initializer.
  function cshore_initialize(this, config_file) result (bmi_status)
    class (bmi_cshore_type), intent(out) :: this
    character (len=*), intent(in) :: config_file
    integer :: i, i_acc, bmi_status
    integer :: io_stat, status
    integer, allocatable :: grid_shape(:)
    integer :: grid_rank

    !if (len(config_file) > 0) then
    !   call initialize_from_file(this%cshore_vars, config_file)
    !else
    !   call initialize_from_defaults(this%cshore_vars)
    !end if
    ! call OPENER(this%cshore_vars, config_file)
    ! call INPUT(this%cshore_vars, config_file)

    status = INIT(this%cshore_vars, config_file)

    if (status /= 0) then
      bmi_status = BMI_FAILURE
      return
    end if

    ! initialize exposed var info tables
    this%exp_var_info_table_io(var_io_idx_bathymetry)%name = &
      "bathymetry"
    this%exp_var_info_table_io(var_io_idx_bathymetry)%cshore_var_name = &
      "ZB"
    this%exp_var_info_table_io(var_io_idx_bathymetry)%unit = &
      "m"
    this%exp_var_info_table_io(var_io_idx_bathymetry)%type = &
      "double precision"
    this%exp_var_info_table_io(var_io_idx_bathymetry)%grid_id = &
      var_io_idx_bathymetry
    grid_shape = shape(this%cshore_vars%ZB)
    grid_rank = size(grid_shape)
    allocate(this%exp_var_info_table_io(var_io_idx_bathymetry)%grid_shape( &
      grid_rank), stat = status)
    if (status /= 0) then
      bmi_status = BMI_FAILURE
      return
    end if
    this%exp_var_info_table_io(var_io_idx_bathymetry)%grid_shape = &
      grid_shape
    this%exp_var_info_table_io(var_io_idx_bathymetry)%grid_rank = &
      grid_rank

    this%exp_var_info_table_i(var_i_idx_mainloop_itime)%name = &
      "mainloop_itime"
    this%exp_var_info_table_i(var_i_idx_mainloop_itime)%cshore_var_name = &
      "mainloop_itime"
    this%exp_var_info_table_i(var_i_idx_mainloop_itime)%unit = &
      "time step"
    this%exp_var_info_table_i(var_i_idx_mainloop_itime)%type = &
      "integer"
    this%exp_var_info_table_i(var_i_idx_mainloop_itime)%grid_id = &
      total_exp_var_io_plus_1 - 1 + var_i_idx_mainloop_itime
    ! set rank and shape

    this%exp_var_info_table_o(var_o_idx_grid_per_transect)%name = &
      "grid_per_transect"
    this%exp_var_info_table_o(var_o_idx_grid_per_transect)%cshore_var_name = &
      "NBINP"
    this%exp_var_info_table_o(var_o_idx_grid_per_transect)%unit = &
      "grid"
    this%exp_var_info_table_o(var_o_idx_grid_per_transect)%type = &
      "integer"
    this%exp_var_info_table_o(var_o_idx_grid_per_transect)%grid_id = &
      total_exp_var_io_plus_1 - 1 + total_exp_var_i_plus_1 - 1 + &
      var_o_idx_grid_per_transect
    ! set rank and shape

    this%exp_var_info_table_o(var_o_idx_total_transects)%name = &
      "total_transects"
    this%exp_var_info_table_o(var_o_idx_total_transects)%cshore_var_name = &
      "ILINE"
    this%exp_var_info_table_o(var_o_idx_total_transects)%unit = &
      "transect"
    this%exp_var_info_table_o(var_o_idx_total_transects)%type = &
      "integer"
    this%exp_var_info_table_o(var_o_idx_total_transects)%grid_id = &
      total_exp_var_io_plus_1 - 1 + total_exp_var_i_plus_1 - 1 + &
      var_o_idx_total_transects
    ! set rank and shape

    this%exp_var_info_table_o(var_o_idx_ntime)%name = &
      "ntime"
    this%exp_var_info_table_o(var_o_idx_ntime)%cshore_var_name = &
      "ntime"
    this%exp_var_info_table_o(var_o_idx_ntime)%unit = &
      "time step"
    this%exp_var_info_table_o(var_o_idx_ntime)%type = &
      "integer"
    this%exp_var_info_table_o(var_o_idx_ntime)%grid_id = &
      total_exp_var_io_plus_1 - 1 + total_exp_var_i_plus_1 - 1 + &
      var_o_idx_ntime
    ! set rank and shape

    this%exp_var_info_table_o(var_o_idx_maxmwl)%name = &
      "max_mean_water_level"
    this%exp_var_info_table_o(var_o_idx_maxmwl)%cshore_var_name = &
      "MAXMWL"
    this%exp_var_info_table_o(var_o_idx_maxmwl)%unit = &
      "m"
    this%exp_var_info_table_o(var_o_idx_maxmwl)%type = &
      "double precision"
    this%exp_var_info_table_o(var_o_idx_maxmwl)%grid_id = &
      total_exp_var_io_plus_1 - 1 + total_exp_var_i_plus_1 - 1 + &
      var_o_idx_maxmwl
    grid_shape = shape(this%cshore_vars%MAXMWL)
    grid_rank = size(grid_shape)
    allocate(this%exp_var_info_table_o(var_o_idx_maxmwl)%grid_shape( &
      grid_rank), stat = status)
    if (status /= 0) then
      bmi_status = BMI_FAILURE
      return
    end if
    this%exp_var_info_table_o(var_o_idx_maxmwl)%grid_shape = grid_shape
    this%exp_var_info_table_o(var_o_idx_maxmwl)%grid_rank = grid_rank

    this%exp_var_info_table_o(var_o_idx_setup)%name = &
      "setup"
    this%exp_var_info_table_o(var_o_idx_setup)%cshore_var_name = &
      "WSETUP"
    this%exp_var_info_table_o(var_o_idx_setup)%unit = &
      "m"
    this%exp_var_info_table_o(var_o_idx_setup)%type = &
      "double precision"
    this%exp_var_info_table_o(var_o_idx_setup)%grid_id = &
      total_exp_var_io_plus_1 - 1 + total_exp_var_i_plus_1 - 1 + &
      var_o_idx_setup
    ! set rank and shape

    !initialize input/output items
    do i = 1, total_exp_var_io_plus_1 - 1, 1
      input_items(i) = this%exp_var_info_table_io(i)%name
    end do
    i_acc = total_exp_var_io_plus_1 - 1

    do i = 1, total_exp_var_i_plus_1 - 1, 1
      input_items(i_acc + i) = this%exp_var_info_table_i(i)%name
    end do

    do i = 1, total_exp_var_io_plus_1 - 1, 1
      output_items(i) = this%exp_var_info_table_io(i)%name
    end do
    i_acc = total_exp_var_io_plus_1 - 1

    do i = 1, total_exp_var_o_plus_1 - 1, 1
      output_items(i_acc + i) = this%exp_var_info_table_o(i)%name
    end do

    bmi_status = BMI_SUCCESS
  end function cshore_initialize

  ! BMI finalizer.
  function cshore_finalize(this) result (bmi_status)
    class (bmi_cshore_type), intent(inout) :: this
    integer :: bmi_status, i, status
    logical :: finish_with_error = .false.

    do i = 1, total_exp_var_io_plus_1 - 1, 1
      if (associated(this%exp_var_info_table_io(i)%grid_shape)) then
        deallocate(this%exp_var_info_table_io(i)%grid_shape, stat = status)
        if (status /= 0) then
          finish_with_error = .true.
        end if
      end if
    end do

    do i = 1, total_exp_var_i_plus_1 - 1, 1
      if (associated(this%exp_var_info_table_i(i)%grid_shape)) then
        deallocate(this%exp_var_info_table_i(i)%grid_shape, stat = status)
        if (status /= 0) then
          finish_with_error = .true.
        end if
      end if
    end do

    do i = 1, total_exp_var_o_plus_1 - 1, 1
      if (associated(this%exp_var_info_table_o(i)%grid_shape)) then
        deallocate(this%exp_var_info_table_o(i)%grid_shape, stat = status)
        if (status /= 0) then
          finish_with_error = .true.
        end if
      end if
    end do

    call cleanup(this%cshore_vars)

    if (finish_with_error) then
      bmi_status = BMI_FAILURE
    else
      bmi_status = BMI_SUCCESS
    end if
  end function cshore_finalize

  ! Model start time.
  function heat_start_time(this, time) result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    time = 0.d0
    bmi_status = BMI_SUCCESS
  end function heat_start_time

  ! Model end time.
  function heat_end_time(this, time) result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    time = dble(this%cshore_vars%t_end)
    bmi_status = BMI_SUCCESS
  end function heat_end_time

  ! Model current time.
  function heat_current_time(this, time) result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    time = dble(this%cshore_vars%t)
    bmi_status = BMI_SUCCESS
  end function heat_current_time

  ! Model time step.
  function heat_time_step(this, time_step) result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    double precision, intent(out) :: time_step
    integer :: bmi_status

    time_step = dble(this%cshore_vars%dt)
    bmi_status = BMI_SUCCESS
  end function heat_time_step

  ! Model time units.
  function heat_time_units(this, units) result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    character (len=*), intent(out) :: units
    integer :: bmi_status

    units = "s"
    bmi_status = BMI_SUCCESS
  end function heat_time_units

  ! ! Advance model by one time step.
  ! function heat_update(this) result (bmi_status)
  !   class (bmi_cshore_type), intent(inout) :: this
  !   integer :: bmi_status
  !
  !   call advance_in_time(this%cshore_vars)
  !   bmi_status = BMI_SUCCESS
  ! end function heat_update

  ! Advance model by one time step.
  function cshore_update(this) result (bmi_status)
    class (bmi_cshore_type), intent(inout) :: this
    integer :: bmi_status

    call compute(this%cshore_vars)
    bmi_status = BMI_SUCCESS
  end function cshore_update

  ! Advance the model until the given time.
  function heat_update_until(this, time) result (bmi_status)
    class (bmi_cshore_type), intent(inout) :: this
    double precision, intent(in) :: time
    integer :: bmi_status
    double precision :: n_steps_real
    integer :: n_steps, i, s

    if (time < this%cshore_vars%t) then
       bmi_status = BMI_FAILURE
       return
    end if

    n_steps_real = (time - this%cshore_vars%t) / this%cshore_vars%dt
    n_steps = floor(n_steps_real)
    do i = 1, n_steps
       s = this%update()
    end do
    call update_frac(this, n_steps_real - dble(n_steps)) ! See near bottom of file
    bmi_status = BMI_SUCCESS
  end function heat_update_until

  ! Get the grid id for a particular variable.
  function cshore_var_grid(this, name, grid) result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(out) :: grid
    integer :: bmi_status, i

    ! select case(name)
    ! case('plate_surface__temperature')
    !    grid = 0
    !    bmi_status = BMI_SUCCESS
    ! case('plate_surface__thermal_diffusivity')
    !    grid = 1
    !    bmi_status = BMI_SUCCESS
    ! case('model__identification_number')
    !    grid = 1
    !    bmi_status = BMI_SUCCESS
    ! case default
    !    grid = -1
    !    bmi_status = BMI_FAILURE
    ! end select

    do i = 1, total_exp_var_io_plus_1 - 1, 1
      if (name == this%exp_var_info_table_io(i)%name) then
        grid = this%exp_var_info_table_io(i)%grid_id
        bmi_status = BMI_SUCCESS
        return
      end if
    end do

    do i = 1, total_exp_var_i_plus_1 - 1, 1
      if (name == this%exp_var_info_table_i(i)%name) then
        grid = this%exp_var_info_table_i(i)%grid_id
        bmi_status = BMI_SUCCESS
        return
      end if
    end do

    do i = 1, total_exp_var_o_plus_1 - 1, 1
      if (name == this%exp_var_info_table_o(i)%name) then
        grid = this%exp_var_info_table_o(i)%grid_id
        bmi_status = BMI_SUCCESS
        return
      end if
    end do

    bmi_status = BMI_FAILURE
  end function cshore_var_grid

  ! The type of a variable's grid.
  function heat_grid_type(this, grid, type) result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    integer, intent(in) :: grid
    character (len=*), intent(out) :: type
    integer :: bmi_status

    select case(grid)
    case(0)
       type = "uniform_rectilinear"
       bmi_status = BMI_SUCCESS
    case(1)
       type = "scalar"
       bmi_status = BMI_SUCCESS
    case default
       type = "-"
       bmi_status = BMI_FAILURE
    end select
  end function heat_grid_type

  ! The number of dimensions of a grid.
  function heat_grid_rank(this, grid, rank) result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: rank
    integer :: bmi_status, i

    ! select case(grid)
    ! case(0)
    !    rank = 2
    !    bmi_status = BMI_SUCCESS
    ! case(1)
    !    rank = 0
    !    bmi_status = BMI_SUCCESS
    ! case default
    !    rank = -1
    !    bmi_status = BMI_FAILURE
    ! end select

    do i = 1, total_exp_var_io_plus_1 - 1, 1
      if (grid == this%exp_var_info_table_io(i)%grid_id) then
        rank = this%exp_var_info_table_io(i)%grid_rank
        bmi_status = BMI_SUCCESS
        return
      end if
    end do

    do i = 1, total_exp_var_i_plus_1 - 1, 1
      if (grid == this%exp_var_info_table_i(i)%grid_id) then
        rank = this%exp_var_info_table_i(i)%grid_rank
        bmi_status = BMI_SUCCESS
        return
      end if
    end do

    do i = 1, total_exp_var_o_plus_1 - 1, 1
      if (grid == this%exp_var_info_table_o(i)%grid_id) then
        rank = this%exp_var_info_table_o(i)%grid_rank
        bmi_status = BMI_SUCCESS
        return
      end if
    end do

    bmi_status = BMI_FAILURE
  end function heat_grid_rank

  ! The dimensions of a grid.
  function cshore_grid_shape(this, grid, shape) result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: shape
    integer :: bmi_status, i

    ! select case(grid)
    ! case(0)
    !    shape(:) = [this%cshore_vars%n_y, this%cshore_vars%n_x]
    !    bmi_status = BMI_SUCCESS
    ! case default
    !    shape(:) = -1
    !    bmi_status = BMI_FAILURE
    ! end select

    do i = 1, total_exp_var_io_plus_1 - 1, 1
      if (grid == this%exp_var_info_table_io(i)%grid_id) then
        shape = this%exp_var_info_table_io(i)%grid_shape
        bmi_status = BMI_SUCCESS
        return
      end if
    end do

    do i = 1, total_exp_var_i_plus_1 - 1, 1
      if (grid == this%exp_var_info_table_i(i)%grid_id) then
        shape = this%exp_var_info_table_i(i)%grid_shape
        bmi_status = BMI_SUCCESS
        return
      end if
    end do

    do i = 1, total_exp_var_o_plus_1 - 1, 1
      if (grid == this%exp_var_info_table_o(i)%grid_id) then
        shape = this%exp_var_info_table_o(i)%grid_shape
        bmi_status = BMI_SUCCESS
        return
      end if
    end do

    bmi_status = BMI_FAILURE
  end function cshore_grid_shape

  ! The total number of elements in a grid.
  function cshore_grid_size(this, grid, size) result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: size
    integer :: bmi_status, i, index_shape

    ! select case(grid)
    ! case(0)
    !    size = this%cshore_vars%n_y * this%cshore_vars%n_x
    !    bmi_status = BMI_SUCCESS
    ! case(1)
    !    size = 1
    !    bmi_status = BMI_SUCCESS
    ! case default
    !    size = -1
    !    bmi_status = BMI_FAILURE
    ! end select

    do i = 1, total_exp_var_io_plus_1 - 1, 1
      if (grid == this%exp_var_info_table_io(i)%grid_id) then
        size = 1
        do index_shape = 1, this%exp_var_info_table_io(i)%grid_rank, 1
          size = size * this%exp_var_info_table_io(i)%grid_shape(index_shape)
        end do
        bmi_status = BMI_SUCCESS
        return
      end if
    end do

    do i = 1, total_exp_var_i_plus_1 - 1, 1
      if (grid == this%exp_var_info_table_i(i)%grid_id) then
        size = 1
        do index_shape = 1, this%exp_var_info_table_i(i)%grid_rank, 1
          size = size * this%exp_var_info_table_i(i)%grid_shape(index_shape)
        end do
        bmi_status = BMI_SUCCESS
        return
      end if
    end do

    do i = 1, total_exp_var_o_plus_1 - 1, 1
      if (grid == this%exp_var_info_table_o(i)%grid_id) then
        size = 1
        do index_shape = 1, this%exp_var_info_table_o(i)%grid_rank, 1
          size = size * this%exp_var_info_table_o(i)%grid_shape(index_shape)
        end do
        bmi_status = BMI_SUCCESS
        return
      end if
    end do

    bmi_status = BMI_FAILURE
  end function cshore_grid_size

  ! The distance between nodes of a grid.
  function heat_grid_spacing(this, grid, spacing) result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: spacing
    integer :: bmi_status

    select case(grid)
    case(0)
       spacing(:) = [double precision:: this%cshore_vars%dy, this%cshore_vars%dx]
       bmi_status = BMI_SUCCESS
    case default
       spacing(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function heat_grid_spacing

  ! Coordinates of grid origin.
  function heat_grid_origin(this, grid, origin) result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: origin
    integer :: bmi_status

    select case(grid)
    case(0)
       origin(:) = [0.d0, 0.d0]
       bmi_status = BMI_SUCCESS
    case default
       origin(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function heat_grid_origin

  ! X-coordinates of grid nodes.
  function heat_grid_x(this, grid, x) result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: x
    integer :: bmi_status

    select case(grid)
    case(1)
       x(:) = [0.d0]
       bmi_status = BMI_SUCCESS
    case default
       x(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function heat_grid_x

  ! Y-coordinates of grid nodes.
  function heat_grid_y(this, grid, y) result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: y
    integer :: bmi_status

    select case(grid)
    case(1)
       y(:) = [0.d0]
       bmi_status = BMI_SUCCESS
    case default
       y(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function heat_grid_y

  ! Z-coordinates of grid nodes.
  function heat_grid_z(this, grid, z) result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: z
    integer :: bmi_status

    select case(grid)
    case(1)
       z(:) = [0.d0]
       bmi_status = BMI_SUCCESS
    case default
       z(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function heat_grid_z

  ! Get the number of nodes in an unstructured grid.
  function heat_grid_node_count(this, grid, count) result(bmi_status)
    class(bmi_cshore_type), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: count
    integer :: bmi_status

    select case(grid)
    case(0:1)
       bmi_status = this%get_grid_size(grid, count)
    case default
       count = -1
       bmi_status = BMI_FAILURE
    end select
  end function heat_grid_node_count

  ! Get the number of edges in an unstructured grid.
  function heat_grid_edge_count(this, grid, count) result(bmi_status)
    class(bmi_cshore_type), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: count
    integer :: bmi_status

    count = -1
    bmi_status = BMI_FAILURE
  end function heat_grid_edge_count

  ! Get the number of faces in an unstructured grid.
  function heat_grid_face_count(this, grid, count) result(bmi_status)
    class(bmi_cshore_type), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: count
    integer :: bmi_status

    count = -1
    bmi_status = BMI_FAILURE
  end function heat_grid_face_count

  ! Get the edge-node connectivity.
  function heat_grid_edge_nodes(this, grid, edge_nodes) result(bmi_status)
    class(bmi_cshore_type), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: edge_nodes
    integer :: bmi_status

    edge_nodes(:) = -1
    bmi_status = BMI_FAILURE
  end function heat_grid_edge_nodes

  ! Get the face-edge connectivity.
  function heat_grid_face_edges(this, grid, face_edges) result(bmi_status)
    class(bmi_cshore_type), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: face_edges
    integer :: bmi_status

    face_edges(:) = -1
    bmi_status = BMI_FAILURE
  end function heat_grid_face_edges

  ! Get the face-node connectivity.
  function heat_grid_face_nodes(this, grid, face_nodes) result(bmi_status)
    class(bmi_cshore_type), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: face_nodes
    integer :: bmi_status

    face_nodes(:) = -1
    bmi_status = BMI_FAILURE
  end function heat_grid_face_nodes

  ! Get the number of nodes for each face.
  function heat_grid_nodes_per_face(this, grid, nodes_per_face) result(bmi_status)
    class(bmi_cshore_type), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: nodes_per_face
    integer :: bmi_status

    nodes_per_face(:) = -1
    bmi_status = BMI_FAILURE
  end function heat_grid_nodes_per_face

  ! The data type of the variable, as a string.
  function cshore_var_type(this, name, type) result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: type
    integer :: i, bmi_status

    !select case(name)
    !case("plate_surface__temperature")
    !   type = "real"
    !   bmi_status = BMI_SUCCESS
    !case("plate_surface__thermal_diffusivity")
    !   type = "real"
    !   bmi_status = BMI_SUCCESS
    !case("model__identification_number")
    !   type = "integer"
    !   bmi_status = BMI_SUCCESS
    !case default
    !   type = "-"
    !   bmi_status = BMI_FAILURE
    !end select

    do i = 1, total_exp_var_io_plus_1 - 1, 1
      if (name == this%exp_var_info_table_io(i)%name) then
        type = this%exp_var_info_table_io(i)%type
        bmi_status = BMI_SUCCESS
        return
      end if
    end do

    do i = 1, total_exp_var_i_plus_1 - 1, 1
      if (name == this%exp_var_info_table_i(i)%name) then
        type = this%exp_var_info_table_i(i)%type
        bmi_status = BMI_SUCCESS
        return
      end if
    end do

    do i = 1, total_exp_var_o_plus_1 - 1, 1
      if (name == this%exp_var_info_table_o(i)%name) then
        type = this%exp_var_info_table_o(i)%type
        bmi_status = BMI_SUCCESS
        return
      end if
    end do

    bmi_status = BMI_FAILURE

  end function cshore_var_type

  ! The units of the given variable.
  function cshore_var_units(this, name, units) result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: units
    integer :: i, bmi_status

    !select case(name)
    !case("plate_surface__temperature")
    !   units = "K"
    !   bmi_status = BMI_SUCCESS
    !case("plate_surface__thermal_diffusivity")
    !   units = "m2 s-1"
    !   bmi_status = BMI_SUCCESS
    !case("model__identification_number")
    !   units = "1"
    !   bmi_status = BMI_SUCCESS
    !case default
    !   units = "-"
    !   bmi_status = BMI_FAILURE
    !end select

    do i = 1, total_exp_var_io_plus_1 - 1, 1
      if (name == this%exp_var_info_table_io(i)%name) then
        units = this%exp_var_info_table_io(i)%unit
        bmi_status = BMI_SUCCESS
        return
      end if
    end do

    do i = 1, total_exp_var_i_plus_1 - 1, 1
      if (name == this%exp_var_info_table_i(i)%name) then
        units = this%exp_var_info_table_i(i)%unit
        bmi_status = BMI_SUCCESS
        return
      end if
    end do

    do i = 1, total_exp_var_o_plus_1 - 1, 1
      if (name == this%exp_var_info_table_o(i)%name) then
        units = this%exp_var_info_table_o(i)%unit
        bmi_status = BMI_SUCCESS
        return
      end if
    end do

    bmi_status = BMI_FAILURE

  end function cshore_var_units

  ! Memory use per array element.
  function heat_var_itemsize(this, name, size) result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(out) :: size
    integer :: bmi_status

    select case(name)
    case("plate_surface__temperature")
       !size = sizeof(this%cshore_vars%temperature(1,1))  ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("plate_surface__thermal_diffusivity")
       !size = sizeof(this%cshore_vars%alpha)             ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("model__identification_number")
       !size = sizeof(this%cshore_vars%id)                ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case default
       size = -1
       bmi_status = BMI_FAILURE
    end select
  end function heat_var_itemsize

  ! The size of the given variable.
  function heat_var_nbytes(this, name, nbytes) result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(out) :: nbytes
    integer :: bmi_status
    integer :: s1, s2, s3, grid, grid_size, item_size

    s1 = this%get_var_grid(name, grid)
    s2 = this%get_grid_size(grid, grid_size)
    s3 = this%get_var_itemsize(name, item_size)

    if ((s1 == BMI_SUCCESS).and.(s2 == BMI_SUCCESS).and.(s3 == BMI_SUCCESS)) then
       nbytes = item_size * grid_size
       bmi_status = BMI_SUCCESS
    else
       nbytes = -1
       bmi_status = BMI_FAILURE
    end if
  end function heat_var_nbytes

  ! The location (node, face, edge) of the given variable.
  function heat_var_location(this, name, location) result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: location
    integer :: bmi_status

    select case(name)
    case default
       location = "node"
       bmi_status = BMI_SUCCESS
    end select
  end function heat_var_location

  ! Get a copy of a integer variable's values, flattened.
  function cshore_get_int(this, name, dest) result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(inout) :: dest(:)
    integer :: i, bmi_status

    !select case(name)
    !case("model__identification_number")
    !   dest = [this%cshore_vars%id]
    !   bmi_status = BMI_SUCCESS
    !case("enable_cshore_outputfiles")
    !  if (this%cshore_vars%enable_cshore_outputfiles) then
    !    dest = 1
    !  elseif (.NOT.this%cshore_vars%enable_cshore_outputfiles) then
    !    dest = 0
    !  endif
    !case default
    !   dest(:) = -1
    !   bmi_status = BMI_FAILURE
    !end select

    do i = 1, total_exp_var_io_plus_1 - 1, 1
      if (name == this%exp_var_info_table_io(i)%name) then

        bmi_status = BMI_SUCCESS
        return
      end if
    end do

    do i = 1, total_exp_var_o_plus_1 - 1, 1
      if (name == this%exp_var_info_table_o(i)%name) then

        select case (name)
          case ("grid_per_transect")
            ! print *, "get_int, exp_var_name: grid_per_transect, cshore_var_name: ", this%exp_var_info_table_o(i)%cshore_var_name
            dest = this%cshore_vars%NBINP(1:this%cshore_vars%ILINE)
          case ("total_transects")
            ! print *, "get_int, exp_var_name: total_transects, cshore_var_name: ", this%exp_var_info_table_o(i)%cshore_var_name
            dest = [this%cshore_vars%ILINE]
          case ("ntime")
            ! print *, "get_int, exp_var_name: ntime, cshore_var_name: ", this%exp_var_info_table_o(i)%cshore_var_name
            dest = [this%cshore_vars%NTIME]
        end select

        bmi_status = BMI_SUCCESS
        return
      end if
    end do

    bmi_status = BMI_FAILURE

  end function cshore_get_int

  ! Get a copy of a real variable's values, flattened.
  function heat_get_float(this, name, dest) result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    character (len=*), intent(in) :: name
    real, intent(inout) :: dest(:)
    integer :: bmi_status

    select case(name)
    case("plate_surface__temperature")
       ! This would be safe, but subject to indexing errors.
       ! do j = 1, this%cshore_vars%n_y
       !    do i = 1, this%cshore_vars%n_x
       !       k = j + this%cshore_vars%n_y*(i-1)
       !       dest(k) = this%cshore_vars%temperature(j,i)
       !    end do
       ! end do

       ! This is an equivalent, elementwise copy into `dest`.
       ! See https://stackoverflow.com/a/11800068/1563298
       dest = reshape(this%cshore_vars%temperature, [this%cshore_vars%n_x*this%cshore_vars%n_y])
       bmi_status = BMI_SUCCESS
    case("plate_surface__thermal_diffusivity")
       dest = [this%cshore_vars%alpha]
       bmi_status = BMI_SUCCESS
    case default
       dest(:) = -1.0
       bmi_status = BMI_FAILURE
    end select
  end function heat_get_float

  ! Get a copy of a double variable's values, flattened.
  function cshore_get_double(this, name, dest) result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    character (len=*), intent(in) :: name
    double precision, intent(inout) :: dest(:)
    integer :: i, t, c, bmi_status
    ! integer :: ii

    !select case(name)
    !case default
    !   dest(:) = -1.d0
    !   bmi_status = BMI_FAILURE
    !end select

    do i = 1, total_exp_var_io_plus_1 - 1, 1
      if (name == this%exp_var_info_table_io(i)%name) then

        select case (name)
          case ("bathymetry")
            ! print *, "get_double, exp_var_name: bathymetry, cshore_var_name: ", this%exp_var_info_table_io(i)%cshore_var_name

            c = 0
            do t = 1, this%cshore_vars%ILINE, 1
              dest(c + 1: c + this%cshore_vars%NBINP(t)) = &
                this%cshore_vars%ZB(1: this%cshore_vars%NBINP(t), t)
              c = c + this%cshore_vars%NBINP(t)
            end do

            !write(*,*) 'dest after get in bmi_cshore.f03'
            !WRITE(*,*) (dest(ii),ii=1,10)

        end select

        bmi_status = BMI_SUCCESS
        return
      end if
    end do

    do i = 1, total_exp_var_o_plus_1 - 1, 1
      if (name == this%exp_var_info_table_o(i)%name) then

        select case (name)

          case ("max_mean_water_level")
            dest(1) = this%cshore_vars%MAXMWL(1)

        end select

        bmi_status = BMI_SUCCESS
        return
      end if
    end do

    bmi_status = BMI_FAILURE

  end function cshore_get_double

  ! Get a reference to an integer-valued variable, flattened.
  function heat_get_ptr_int(this, name, dest_ptr) result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, pointer, intent(inout) :: dest_ptr(:)
    integer :: bmi_status
    type (c_ptr) :: src
    integer :: n_elements

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function heat_get_ptr_int

  ! Get a reference to a real-valued variable, flattened.
  function heat_get_ptr_float(this, name, dest_ptr) result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    character (len=*), intent(in) :: name
    real, pointer, intent(inout) :: dest_ptr(:)
    integer :: bmi_status
    type (c_ptr) :: src
    integer :: n_elements

    select case(name)
    case("plate_surface__temperature")
       src = c_loc(this%cshore_vars%temperature(1,1))
       n_elements = this%cshore_vars%n_y * this%cshore_vars%n_x
       call c_f_pointer(src, dest_ptr, [n_elements])
       bmi_status = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select
  end function heat_get_ptr_float

  ! Get a reference to an double-valued variable, flattened.
  function cshore_get_ptr_double(this, name, dest_ptr) result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    character (len=*), intent(in) :: name
    double precision, pointer, intent(inout) :: dest_ptr(:)
    integer :: bmi_status
    type (c_ptr) :: src
    integer :: n_elements, i

    ! select case(name)
    ! case("plate_surface__temperature")
    !    src = c_loc(this%cshore_vars%temperature(1,1))
    !    n_elements = this%cshore_vars%n_y * this%cshore_vars%n_x
    !    call c_f_pointer(src, dest_ptr, [n_elements])
    !    bmi_status = BMI_SUCCESS
    ! case default
    !    bmi_status = BMI_FAILURE
    ! end select

!    do i = 1, total_exp_var_io_plus_1 - 1, 1
!      if (name == this%exp_var_info_table_io(i)%name) then
!
!        select case (name)
!          case ("bathymetry")
!            ! print *, "get_ptr_double, exp_var_name: bathymetry, cshore_var_name: ", this%exp_var_info_table_io(i)%cshore_var_name
!
!            src = c_loc(this%cshore_vars%ZB(1,1))
!            n_elements = NN * NL
!            call c_f_pointer(src, dest_ptr, [n_elements])
!
!         end select
!
!        bmi_status = BMI_SUCCESS
!        return
!      end if
!    end do

    do i = 1, total_exp_var_o_plus_1 - 1, 1
      if (name == this%exp_var_info_table_o(i)%name) then

        select case (name)

          case ("max_mean_water_level")
        ! print *, "get_ptr_double, exp_var_name: bathymetry, cshore_var_name: ", this%exp_var_info_table_io(i)%cshore_var_name

            src = c_loc(this%cshore_vars%MAXMWL(1))
            call c_f_pointer(src, dest_ptr, [1])

        end select

        bmi_status = BMI_SUCCESS
        return
      end if
    end do

    bmi_status = BMI_FAILURE

  end function cshore_get_ptr_double

  ! Get values of an integer variable at the given locations.
  function heat_get_at_indices_int(this, name, dest, inds) &
       result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(inout) :: dest(:)
    integer, intent(in) :: inds(:)
    integer :: bmi_status
    type (c_ptr) src
    integer, pointer :: src_flattened(:)
    integer :: i, n_elements

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function heat_get_at_indices_int

  ! Get values of a real variable at the given locations.
  function heat_get_at_indices_float(this, name, dest, inds) &
       result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    character (len=*), intent(in) :: name
    real, intent(inout) :: dest(:)
    integer, intent(in) :: inds(:)
    integer :: bmi_status
    type (c_ptr) src
    real, pointer :: src_flattened(:)
    integer :: i, n_elements

    select case(name)
    case("plate_surface__temperature")
       src = c_loc(this%cshore_vars%temperature(1,1))
       call c_f_pointer(src, src_flattened, [this%cshore_vars%n_y * this%cshore_vars%n_x])
       n_elements = size(inds)
       do i = 1, n_elements
          dest(i) = src_flattened(inds(i))
       end do
       bmi_status = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select
  end function heat_get_at_indices_float

  ! Get values of a double variable at the given locations.
  function heat_get_at_indices_double(this, name, dest, inds) &
       result (bmi_status)
    class (bmi_cshore_type), intent(in) :: this
    character (len=*), intent(in) :: name
    double precision, intent(inout) :: dest(:)
    integer, intent(in) :: inds(:)
    integer :: bmi_status
    type (c_ptr) src
    double precision, pointer :: src_flattened(:)
    integer :: i, n_elements

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function heat_get_at_indices_double

  ! Set new integer values.
  function cshore_set_int(this, name, src) result (bmi_status)
    class (bmi_cshore_type), intent(inout) :: this
    character (len=*), intent(in) :: name
    integer, intent(in) :: src(:)
    integer :: i, bmi_status

    !select case(name)
    !case("model__identification_number")
    !   this%cshore_vars%id = src(1)
    !   bmi_status = BMI_SUCCESS
    !case("enable_cshore_outputfiles")
    !   if(src(1).eq.1) then
    !       this%cshore_vars%enable_cshore_outputfiles = .true.
    !   elseif (src(1).eq.0) then
    !       this%cshore_vars%enable_cshore_outputfiles = .false.
    !   endif
    !   bmi_status = BMI_SUCCESS
    !case default
    !   bmi_status = BMI_FAILURE
    !end select

    do i = 1, total_exp_var_io_plus_1 - 1, 1
      if (name == this%exp_var_info_table_io(i)%name) then

        bmi_status = BMI_SUCCESS
        return
      end if
    end do

    do i = 1, total_exp_var_i_plus_1 - 1, 1
      if (name == this%exp_var_info_table_i(i)%name) then

        select case(name)
          case("mainloop_itime")
            this%cshore_vars%mainloop_itime = src(1)
            bmi_status = BMI_SUCCESS
          case default
            bmi_status = BMI_FAILURE
        end select

        return
      end if

    end do

  end function cshore_set_int

  ! Set new real values.
  function heat_set_float(this, name, src) result (bmi_status)
    class (bmi_cshore_type), intent(inout) :: this
    character (len=*), intent(in) :: name
    real, intent(in) :: src(:)
    integer :: bmi_status

    select case(name)
    case("plate_surface__temperature")
       this%cshore_vars%temperature = reshape(src, [this%cshore_vars%n_y, this%cshore_vars%n_x])
       bmi_status = BMI_SUCCESS
    case("plate_surface__thermal_diffusivity")
       this%cshore_vars%alpha = src(1)
       bmi_status = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select
  end function heat_set_float

  ! Set new double values.
  function cshore_set_double(this, name, src) result (bmi_status)
    class (bmi_cshore_type), intent(inout) :: this
    character (len=*), intent(in) :: name
    double precision, intent(in) :: src(:)
    integer :: i, t, c, bmi_status
    ! integer :: ii, jj

    !select case(name)
    !case default
    !   bmi_status = BMI_FAILURE
    !end select

    do i = 1, total_exp_var_io_plus_1 - 1, 1
      if (name == this%exp_var_info_table_io(i)%name) then

        select case (name)
          case ("bathymetry")
            !print *, "set_double, exp_var_name: bathymetry, cshore_var_name: ", this%exp_var_info_table_io(i)%cshore_var_name

            ! write(*,*) 'ZB before set in bmi_cshore.f03'
            ! WRITE(*,'(a,d10.4)') '1,1 ', this%cshore_vars%ZB(1,1)
            ! WRITE(*,'(a,d10.4)') '1,2701 ', this%cshore_vars%ZB(1,2701)
            ! WRITE(*,'(a,d10.4)') '1,2702 ', this%cshore_vars%ZB(1,2702)
            ! WRITE(*,'(a,d10.4)') '2,1 ', this%cshore_vars%ZB(2,1)
            ! WRITE(*,'(a,d10.4)') '2701,1 ', this%cshore_vars%ZB(2701,1)
            ! WRITE(*,'(a,d10.4)') '2702,1 ', this%cshore_vars%ZB(2702,1)

            c = 0
            do t = 1, this%cshore_vars%ILINE, 1
              this%cshore_vars%ZB(1: this%cshore_vars%NBINP(t), t) = &
                src(c + 1: c + this%cshore_vars%NBINP(t))
              c = c + this%cshore_vars%NBINP(t)
            end do

            ! write(*,*) 'ZB after set in bmi_cshore.f03'
            ! WRITE(*,*) ((this%cshore_vars%ZB(ii,jj),jj=1,10),ii=1,2)
            ! WRITE(*,'(a,d10.4)') '1,1 ', this%cshore_vars%ZB(1,1)
            ! WRITE(*,'(a,d10.4)') '1,2701 ', this%cshore_vars%ZB(1,2701)
            ! WRITE(*,'(a,d10.4)') '1,2702 ', this%cshore_vars%ZB(1,2702)
            ! WRITE(*,'(a,d10.4)') '2,1 ', this%cshore_vars%ZB(2,1)
            ! WRITE(*,'(a,d10.4)') '2701,1 ', this%cshore_vars%ZB(2701,1)
            ! WRITE(*,'(a,d10.4)') '2702,1 ', this%cshore_vars%ZB(2702,1)

        end select

        bmi_status = BMI_SUCCESS
        return
      end if
    end do

    do i = 1, total_exp_var_i_plus_1 - 1, 1
      if (name == this%exp_var_info_table_o(i)%name) then

        bmi_status = BMI_SUCCESS
        return
      end if
    end do

    bmi_status = BMI_FAILURE

  end function cshore_set_double

  ! Set integer values at particular locations.
  function heat_set_at_indices_int(this, name, inds, src) &
       result (bmi_status)
    class (bmi_cshore_type), intent(inout) :: this
    character (len=*), intent(in) :: name
    integer, intent(in) :: inds(:)
    integer, intent(in) :: src(:)
    integer :: bmi_status
    type (c_ptr) dest
    integer, pointer :: dest_flattened(:)
    integer :: i

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function heat_set_at_indices_int

  ! Set real values at particular locations.
  function heat_set_at_indices_float(this, name, inds, src) &
       result (bmi_status)
    class (bmi_cshore_type), intent(inout) :: this
    character (len=*), intent(in) :: name
    integer, intent(in) :: inds(:)
    real, intent(in) :: src(:)
    integer :: bmi_status
    type (c_ptr) dest
    real, pointer :: dest_flattened(:)
    integer :: i

    select case(name)
    case("plate_surface__temperature")
       dest = c_loc(this%cshore_vars%temperature(1,1))
       call c_f_pointer(dest, dest_flattened, [this%cshore_vars%n_y * this%cshore_vars%n_x])
       do i = 1, size(inds)
          dest_flattened(inds(i)) = src(i)
       end do
       bmi_status = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select
  end function heat_set_at_indices_float

  ! Set double values at particular locations.
  function heat_set_at_indices_double(this, name, inds, src) &
       result (bmi_status)
    class (bmi_cshore_type), intent(inout) :: this
    character (len=*), intent(in) :: name
    integer, intent(in) :: inds(:)
    double precision, intent(in) :: src(:)
    integer :: bmi_status
    type (c_ptr) dest
    double precision, pointer :: dest_flattened(:)
    integer :: i

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function heat_set_at_indices_double

  ! A non-BMI helper routine to advance the model by a fractional time step.
  subroutine update_frac(this, time_frac)
    class (bmi_cshore_type), intent(inout) :: this
    double precision, intent(in) :: time_frac
    real :: time_step

    if (time_frac > 0.0) then
       time_step = this%cshore_vars%dt
       this%cshore_vars%dt = time_step*real(time_frac)
       call advance_in_time(this%cshore_vars)
       this%cshore_vars%dt = time_step
    end if
  end subroutine update_frac

  ! A non-BMI procedure for cshore_vars introspection.
  subroutine print_model_info(this)
    class (bmi_cshore_type), intent(in) :: this

    call print_info(this%cshore_vars)
  end subroutine print_model_info

end module bmi_cshore_module
