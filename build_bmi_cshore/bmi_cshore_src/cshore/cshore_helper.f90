module cshore_helper_module
  use string_processing

  implicit none

  contains

  function InitializeIoConfiguration( &
    config_file_prefix, &
    enable_cshore_stdout, &
    enable_cshore_diskout, &
    enable_varscheck_beginend, &
    enable_varscheck_allstep) &
    result ( &
      status)
    character (len=*), intent(in) :: config_file_prefix
    logical, intent(out) :: &
      enable_cshore_stdout, enable_cshore_diskout, &
      enable_varscheck_beginend, enable_varscheck_allstep
    logical :: status
    character (len=1024) :: line
    integer :: i, file_unit_no, io_stat, err_no
    integer :: key_begin_idx, key_end_idx, value_begin_idx, value_end_idx

    enable_cshore_stdout = .false.
    enable_cshore_diskout = .false.
    enable_varscheck_beginend = .false.
    enable_varscheck_allstep = .false.

    inquire(FILE = (config_file_prefix)//'infile_io_control', EXIST = status)
    if (.not. status) then
      status = .true.
      return
    end if

    file_unit_no = 99
    err_no = 0

    if (err_no == 0) then
      open(UNIT = file_unit_no, &
        FILE = (config_file_prefix)//'infile_io_control', &
        STATUS = 'OLD', ACCESS = 'SEQUENTIAL', IOSTAT = io_stat)
      if (io_stat /= 0) then
        err_no = 1
      end if
    end if

    if (err_no == 0) then
      read(file_unit_no, '(a)', iostat = io_stat) line
      if (0 < io_stat) then
        err_no = 2
      end if

      do while (io_stat == 0)
        if (err_no == 0) then
          status = ParseAKeyValuePair(line, &
            "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_1234567890", &
            ":", &
            "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_1234567890", &
            key_begin_idx, key_end_idx, value_begin_idx, value_end_idx)
          if (.not.status) then
            ! invalid line
            err_no = 3
            exit
          end if

          select case (line(key_begin_idx: key_end_idx))

            case ("enable_terminal_out")
              select case (line(value_begin_idx: value_end_idx))
                case ("on")
                  enable_cshore_stdout = .true.
                case ("off")
                  enable_cshore_stdout = .false.
                case default
                  ! unsupported value
                  err_no = 4
              end select

            case ("enable_disk_out")
              select case (line(value_begin_idx: value_end_idx))
                case ("on")
                  enable_cshore_diskout = .true.
                case ("off")
                  enable_cshore_diskout = .false.
                case default
                  err_no = 4
              end select

            case ("enable_vars_check")
              select case (line(value_begin_idx: value_end_idx))
                case ("t0tn")
                  enable_varscheck_beginend = .true.
                  enable_varscheck_allstep = .false.
                case ("tall")
                  enable_varscheck_beginend = .false.
                  enable_varscheck_allstep = .true.
                case ("off")
                  enable_varscheck_beginend = .false.
                  enable_varscheck_allstep = .false.
                case default
                  err_no = 4
              end select

            case default
              ! unsupported key
              err_no = 4
          end select

        end if

        read(file_unit_no, '(a)', iostat = io_stat) line
        if (0 < io_stat) then
          err_no = 2
        end if

      end do
    end if

    if (err_no == 0) then
      close(UNIT = file_unit_no, IOSTAT = io_stat)
      if (io_stat /= 0) then
        err_no = 5
      end if
    end if

    if (err_no == 0) then
      ! print *, "stdout: ", enable_cshore_stdout
      ! print *, "diskout: ", enable_cshore_diskout
      ! print *, "check_be: ", enable_varscheck_beginend
      ! print *, "check_as: ", enable_varscheck_allstep

      status = .true.
      return
    end if

    ! exception handling begin

    if (2 <= err_no) then
      status = .false.

      close(UNIT = file_unit_no, IOSTAT = io_stat)
    end if

    status = .false.

  end function InitializeIoConfiguration

end module
