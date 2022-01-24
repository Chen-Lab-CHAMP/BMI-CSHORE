! Run the cshore model through its BMI.
program bmi_main

  use bmi_cshore_module
  use bmif_2_0
  use, intrinsic :: iso_fortran_env, only : file_unit=>input_unit
  implicit none

  character (len=*), parameter :: output_file = "bmiheatf.out"
  character (len=*), parameter :: var_name = "plate_surface__temperature"
  integer, parameter :: ndims = 2

  type (bmi_cshore_type) :: model
  integer :: i, j, grid_id, grid_size, grid_shape(ndims),itime,ntime,mainloop_itime
  double precision :: current_time, end_time
  real, allocatable :: temperature(:)
  integer, allocatable :: tmp_int(:)
  character (len=BMI_MAX_VAR_NAME), pointer :: model_name
  character (len=BMI_MAX_TYPE_NAME) :: var_type
  character (len=BMI_MAX_TYPE_NAME) :: var_unit
  character (len=BMI_MAX_VAR_NAME), pointer :: var_names(:)
  character (len=90):: program_path, string
  integer :: count, length, status, total_transects, total_grids
  integer, allocatable :: integer_buffer(:)
  double precision, allocatable :: double_buffer(:)

  character (len=*), parameter:: usage_doc = &
    " <file system path prefix for input/output files>"

  status = model%get_component_name(model_name)
  print *, trim(model_name)

  count = command_argument_count()
  call get_command_argument(0, program_path, length, status)
  if (status /= 0) then
    write (*, FMT = '(a)', ADVANCE = 'YES') &
      "Cannot retrieve program path from command line."
    stop 11
  end if

  ! "Initialize model."
  select case (count)
    case (0)
      status = model%initialize("")

    case (1)
      call get_command_argument(1, string, length, status)
      if (status /= 0) then
        write (*, FMT = '(a,a)', ADVANCE = 'YES') &
          trim(program_path), trim(usage_doc)
        stop 12
      end if

      status = model%initialize(trim(string))

    case default
      write (*, FMT = '(a,a)', ADVANCE = 'YES') &
        trim(program_path), trim(usage_doc)
      stop 13

  end select

  print *, 'set items count:'
  status = model%get_input_item_count(count)
  print *, count

  status = model%get_input_var_names(var_names)
  do i = 1, size(var_names), 1
    print *, trim(var_names(i))
  end do

  print *, 'get items count:'
  status = model%get_output_item_count(count)
  print *, count

  status = model%get_output_var_names(var_names)
  do i = 1, size(var_names), 1
    print *, trim(var_names(i))
  end do
  !
  ! status = model%get_var_type("bathymetry", var_type)
  ! print *, trim(var_type)
  !
  ! status = model%get_var_type("grid_per_transect", var_type)
  ! print *, trim(var_type)
  !
  ! status = model%get_var_type("total_transects", var_type)
  ! print *, trim(var_type)
  !
  ! status = model%get_var_units("bathymetry", var_unit)
  ! print *, trim(var_unit)
  !
  ! status = model%get_var_units("grid_per_transect", var_unit)
  ! print *, trim(var_unit)
  !
  ! status = model%get_var_units("total_transects", var_unit)
  ! print *, trim(var_unit)
  !
  ! allocate(integer_buffer(1))
  ! status = model%get_value("total_transects", integer_buffer)
  ! print *, integer_buffer
  ! total_transects = integer_buffer(1)
  ! deallocate(integer_buffer)
  !
  ! allocate(integer_buffer(total_transects))
  ! status = model%get_value("grid_per_transect", integer_buffer)
  ! print *, integer_buffer
  !
  ! total_grids = 0
  ! do i = 1, total_transects, 1
  !   total_grids = total_grids + integer_buffer(i)
  ! end do
  ! allocate(double_buffer(total_grids))
  !
  ! !status = model%get_value("bathymetry", double_buffer)
  !
  ! count = 0
  ! do i = 1, total_transects, 1
  !   do j = 1, integer_buffer(i), 1
  !     count = count + 1
  !     !write (*, '(d10.4,a)', ADVANCE = 'NO') double_buffer(count), ','
  !   end do
  !   !write (*, '(a)') ''
  ! end do
  !
  ! count = 0
  ! do i = 1, total_transects, 1
  !   do j = 1, integer_buffer(i), 1
  !     count = count + 1
  !     double_buffer(count) = dble(count)
  !   end do
  ! end do
  ! status = model%set_value("bathymetry", double_buffer)
  !
  ! count = 0
  ! do i = 1, total_transects, 1
  !   do j = 1, integer_buffer(i), 1
  !     count = count + 1
  !     double_buffer(count) = dble(0)
  !   end do
  ! end do
  ! status = model%get_value("bathymetry", double_buffer)
  !
  ! ! write(*,*) 'dble_buff after get in bmi_main.f03'
  ! ! WRITE(*,*) (double_buffer(i),i=1,10)
  !
  ! deallocate(integer_buffer)
  ! deallocate(double_buffer)

  allocate(integer_buffer(1))
  status = model%get_value("ntime", integer_buffer)
  print *, integer_buffer
  ntime = integer_buffer(1)

  do itime=1,ntime
    integer_buffer(1)=itime
    status = model%set_value_int("mainloop_itime", integer_buffer)
    status = model%update()
  end do
  status = model%finalize()

  deallocate(integer_buffer)

end program bmi_main
