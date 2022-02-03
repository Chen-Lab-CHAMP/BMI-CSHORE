! Run the cshore model through its BMI.
program bmi_main

  use module_model_coupling_ipc
  use bmi_cshore_module
  use bmif_2_0
  use iso_c_binding
  use, intrinsic :: iso_fortran_env, only : file_unit=>input_unit
  implicit none

  character (len=*), parameter :: output_file = "bmiheatf.out"
  character (len=*), parameter :: var_name = "plate_surface__temperature"

  type (bmi_cshore_type) :: model
  integer :: i, j, itime, ntime, mainloop_itime
  double precision :: current_time, end_time
  real, allocatable :: temperature(:)
  integer, allocatable :: tmp_int(:)
  character (len=BMI_MAX_VAR_NAME), pointer :: model_name
  character (len=BMI_MAX_TYPE_NAME) :: var_type
  character (len=BMI_MAX_TYPE_NAME) :: var_unit
  character (len=BMI_MAX_VAR_NAME), pointer :: var_names(:)
  character (len=90):: program_path, string
  character (len=1024) :: pipe_in_path, pipe_out_path
  integer :: count, length, status, total_transects, total_grids
  integer, allocatable :: integer_buffer(:), grid_per_transect(:)
  integer :: io_stat
  double precision, allocatable :: double_buffer(:), zb_buffer(:)
  double precision, pointer :: bathymetry_ptr(:), maxmwl_ptr(:)
  type (c_ptr) :: bathymetry_generic_ptr, maxmwl_generic_ptr

  integer :: var_id, total_elements
  double precision, pointer :: bathymetry(:)

  type (type_model_coupling_ipc) :: ipc
  integer, pointer :: int_ls(:), int_tab(:,:)
  double precision :: dp
  double precision, pointer :: dp_ls(:), dp_tab(:,:)
  type (c_ptr) :: generic_ptr

  integer :: grid_id, grid_rank
  integer, pointer :: grid_shape(:)

  integer :: pipe_out = 10, pipe_in = 11

  character (len=*), parameter:: usage_doc = &
    " <var-in pipe> <var-out pipe> <path prefix for in/out files>"

  logical :: debug_mode = .true.

  status = model%get_component_name(model_name)
  if (status /= BMI_SUCCESS) then
    stop 1
  end if
  print *, trim(model_name)

  count = command_argument_count()
  call get_command_argument(0, program_path, length, status)
  if (status /= 0) then
    write (*, FMT = '(a)', ADVANCE = 'YES') &
      "Cannot retrieve program path from command line."
    stop 2
  end if

  if (1 < count .and. count < 4) then
    call get_command_argument(1, pipe_in_path, length, status)
    if (status /= 0) then
      write (*, FMT = '(a,a)', ADVANCE = 'YES') &
        trim(program_path), trim(usage_doc)
      stop 3
    end if
    call get_command_argument(2, pipe_out_path, length, status)
    if (status /= 0) then
      write (*, FMT = '(a,a)', ADVANCE = 'YES') &
        trim(program_path), trim(usage_doc)
      stop 4
    end if

    ! "Initialize model."
    select case (count)
      case (2)
        status = model%initialize("")
        if (status /= BMI_SUCCESS) then
          stop 6
        end if

      case (3)
        call get_command_argument(3, string, length, status)
        if (status /= 0) then
          write (*, FMT = '(a,a)', ADVANCE = 'YES') &
            trim(program_path), trim(usage_doc)
          stop 7
        end if

        status = model%initialize(trim(string))
        if (status /= BMI_SUCCESS) then
          stop 8
        end if

    end select

  else
    write (*, FMT = '(a,a)', ADVANCE = 'YES') &
      trim(program_path), trim(usage_doc)
    stop 5
  end if

  ! get number of time steps
  allocate(integer_buffer(1), stat = status)
  if (status /= 0) then
    stop 9
  end if
  status = model%get_value("ntime", integer_buffer)
  if (status /= BMI_SUCCESS) then
    stop 10
  end if
  ntime = integer_buffer(1)
  if (debug_mode) print *, "total time steps: ", ntime

  ! get number of transects, grid per transect, and total grid size (=transect# x grid per transect)
  status = model%get_value("total_transects", integer_buffer)
  if (status /= BMI_SUCCESS) then
    stop 11
  end if
  total_transects = integer_buffer(1)
  if (debug_mode) print *, "bathymetry total transects: ", total_transects
  deallocate(integer_buffer, stat = status)
  if (status /= 0) then
    stop 12
  end if

  allocate(grid_per_transect(total_transects), stat = status)
  if (status /= 0) then
    stop 13
  end if
  status = model%get_value("grid_per_transect", grid_per_transect)
  if (status /= BMI_SUCCESS) then
    stop 14
  end if
  if (debug_mode) print *, "bathymetry grid per transect: ", grid_per_transect

  total_grids = 0
  do i = 1, total_transects, 1
    total_grids = total_grids + grid_per_transect(i)
  end do
  if (debug_mode) print *, "bathymetry total grids: ", total_grids

  ! initialize ipc protocol
  status = ipc%InitializeProtocolDefinition()
  if (status /= 0) then
    stop 15
  end if

  ! set bathymetry var info
  status = model%get_var_grid("bathymetry", grid_id)
  if (status /= 0) then
    stop 16
  end if
  if (debug_mode) print *, "bathymetry grid id: ", grid_id

  status = model%get_grid_rank(grid_id, grid_rank)
  if (status /= 0) then
    stop 17
  end if
  if (debug_mode) print *, "bathymetry grid rank: ", grid_rank

  allocate(grid_shape(grid_rank), stat = status)
  if (status /= 0) then
    stop 18
  end if

  status = model%get_grid_shape(grid_id, grid_shape)
  if (status /= 0) then
    stop 19
  end if
  if (debug_mode) print *, "bathymetry grid shape: ", grid_shape

  allocate(int_ls(total_transects), stat = status)
  if (status /= 0) then
    stop 20
  end if

  int_ls = grid_per_transect

  status = ipc%InitializeVarInfo( &
    var_id_bathymetry, &
    "bathymetry", &
    "double_precision", &
    int(c_sizeof(dp)), &
    grid_shape, &
    int_ls)
  if (status /= 0) then
    stop 21
  end if

  deallocate(grid_shape, stat = status)
  if (status /= 0) then
    stop 22
  end if
  deallocate(int_ls, stat = status)
  if (status /= 0) then
    stop 23
  end if

  ! set max_mean_water_level var info
  status = model%get_var_grid("max_mean_water_level", grid_id)
  if (status /= 0) then
    stop 24
  end if
  if (debug_mode) print *, "max_mean_water_level grid id: ", grid_id

  status = model%get_grid_rank(grid_id, grid_rank)
  if (status /= 0) then
    stop 25
  end if
  if (debug_mode) print *, "max_mean_water_level grid rank: ", grid_rank

  allocate(grid_shape(grid_rank), stat = status)
  if (status /= 0) then
    stop 26
  end if

  status = model%get_grid_shape(grid_id, grid_shape)
  if (status /= 0) then
    stop 27
  end if
  if (debug_mode) print *, "max_mean_water_level grid shape: ", grid_shape

  allocate(int_ls(1), stat = status)
  if (status /= 0) then
    stop 28
  end if

  ! shape: [1]
  ! column_effective_length: [1]
  int_ls(1) = 1

  status = ipc%InitializeVarInfo( &
    var_id_max_mean_water_level, &
    "max_mean_water_level", &
    "double_precision", &
    int(c_sizeof(dp)), &
    int_ls, &
    int_ls)
  if (status /= 0) then
    stop 29
  end if

  deallocate(grid_shape, stat = status)
  if (status /= 0) then
    stop 30
  end if
  deallocate(int_ls, stat = status)
  if (status /= 0) then
    stop 31
  end if

  ! prepare buffers
  allocate(integer_buffer(1), stat = status)
  if (status /= 0) then
    stop 32
  end if

  ! acquire pointers to vars
  status = model%get_value_ptr("bathymetry", bathymetry_ptr)
  if (status /= BMI_SUCCESS) then
    stop 33
  end if
  bathymetry_generic_ptr = c_loc(bathymetry_ptr(1))

  status = model%get_value_ptr("max_mean_water_level", maxmwl_ptr)
  if (status /= BMI_SUCCESS) then
    stop 34
  end if
  maxmwl_generic_ptr = c_loc(maxmwl_ptr(1))

  ! start time marching
  !ntime = 1
  do itime = 1, ntime, 1
    if (debug_mode) print *, 'itime = ', itime, 'ntime = ', ntime

    integer_buffer(1)=itime
    status = model%set_value_int("mainloop_itime", integer_buffer)
    if (status /= BMI_SUCCESS) then
      stop 35
    end if

    status = model%update()
    if (status /= BMI_SUCCESS) then
      stop 36
    end if

!    ! send bathymetry and max_mean_water_level to aeolis through pipe
!    ! open pipes if itime==1
!    if (itime==1) then
!      ! open pipe for writing
!      open(pipe_out, file=pipe_out_path, status="old", &
!        iostat=io_stat, access="stream", action="write")
!
!      if (io_stat /= 0) then
!        print *, "cannot open pipe cshore-to-aeolis"
!        stop 37
!      end if
!    end if
!
!    ! send zb
!    if (debug_mode) print *, "send zb [", bathymetry_ptr(1)
!    status = ipc%SendVar( &
!      pipe_out, &
!      var_id_bathymetry, &
!      bathymetry_generic_ptr)
!    if (status /= 0) then
!      stop 38
!    end if
!
!    ! send max mwl
!    if (debug_mode) print *,  'send max mwl ', maxmwl_ptr(1)
!    status = ipc%SendVar( &
!      pipe_out, &
!      var_id_max_mean_water_level, &
!      maxmwl_generic_ptr)
!    if (status /= 0) then
!      stop 39
!    end if
!
!    ! wait for AeoLiS to finish the current time step
!
!    ! receive bathymetry from aeolis through pipe
!    ! open pipes if itime==1
!    if (itime==1) then
!      open(pipe_in, file=pipe_in_path, status="old", &
!        iostat=io_stat, access="stream", action="read")
!
!      if (io_stat /= 0) then
!        print *, "cannot open pipe aeolis-to-cshore"
!        stop 40
!      end if
!    end if
!
!    ! receive zb
!    status = ipc%RecvVar( &
!      pipe_in, &
!      var_id_bathymetry, &
!      bathymetry_generic_ptr)
!    if (status /= 0) then
!      stop 41
!    end if
!    if (debug_mode) print *,  "received zb [", bathymetry_ptr(1)
!
  end do
  ! end time marching

  allocate(zb_buffer(total_grids))
  status = model%get_value("bathymetry", zb_buffer)

  OPEN(UNIT=112,FILE='final_zb', STATUS='UNKNOWN',ACCESS='SEQUENTIAL')

  ! copy double_buffer to bathymetry (a pointer)
  count=1
  do i = 1, total_transects, 1
    do j = 1, grid_per_transect(i), 1
      write(112, *) zb_buffer(count)
      count = count + 1 
    end do
  end do
  deallocate(zb_buffer)


  deallocate(integer_buffer, stat = status)
  if (status /= 0) then
    stop 42
  end if

  close(pipe_out, iostat = io_stat)
  if (io_stat /= 0) then
    stop 43
  end if
  close(pipe_in, iostat = io_stat)
  if (io_stat /= 0) then
    stop 44
  end if

  status = model%finalize()
  if (status /= BMI_SUCCESS) then
    stop 45
  end if

end program bmi_main
