program add_3vars_2d
    use netcdf
    implicit none

    integer :: ncid
    integer :: sheleg_id, snwdph_id, zorl_id
    integer :: xdimid, ydimid
    integer :: retval

    integer :: nx, ny, i
    integer, parameter :: dp = selected_real_kind(15, 300)

    character(len=17) :: fname

    real(dp), allocatable :: sheleg(:,:), snwdph(:,:), zorl(:,:)

  do i=1,6 ! loop over 6 cube faces

    !--------------------------------------------------
    ! Create name from loop index
    !--------------------------------------------------
    write(fname, '(A,I0,A)') 'sfc_data.tile',i,'.nc'
    print*,' Processing: ',fname,' file'

    !--------------------------------------------------
    ! Open existing NetCDF file for writing
    !--------------------------------------------------
    retval = nf90_open(fname, nf90_write, ncid)
    if (retval /= nf90_noerr) stop nf90_strerror(retval)

    !--------------------------------------------------
    ! Inquire dimension IDs
    !--------------------------------------------------
    retval = nf90_inq_dimid(ncid, "xaxis_1", xdimid)
    if (retval /= nf90_noerr) stop nf90_strerror(retval)

    retval = nf90_inq_dimid(ncid, "yaxis_1", ydimid)
    if (retval /= nf90_noerr) stop nf90_strerror(retval)

    !--------------------------------------------------
    ! Inquire dimension lengths
    !--------------------------------------------------
    retval = nf90_inquire_dimension(ncid, xdimid, len=nx)
    if (retval /= nf90_noerr) stop nf90_strerror(retval)

    retval = nf90_inquire_dimension(ncid, ydimid, len=ny)
    if (retval /= nf90_noerr) stop nf90_strerror(retval)

    !--------------------------------------------------
    ! Allocate arrays dynamically
    !--------------------------------------------------
    allocate(sheleg(nx, ny), snwdph(nx, ny), zorl(nx, ny))

    !--------------------------------------------------
    ! Enter define mode
    !--------------------------------------------------
    retval = nf90_redef(ncid)
    if (retval /= nf90_noerr) stop nf90_strerror(retval)

    !--------------------------------------------------
    ! Define variables (DOUBLE PRECISION)
    !--------------------------------------------------

    if (nf90_inq_varid(ncid, "sheleg", sheleg_id) /= nf90_noerr) then
      retval = nf90_def_var(ncid, "sheleg",  nf90_double, (/ xdimid, ydimid /), sheleg_id)
      if (retval /= nf90_noerr) stop nf90_strerror(retval)
    else
      print*, 'Variable sheleg already exist!!!'
      stop
    endif

    if (nf90_inq_varid(ncid, "snwdph",snwdph_id) /= nf90_noerr) then
      retval = nf90_def_var(ncid, "snwdph",  nf90_double, (/ xdimid, ydimid /), snwdph_id)
      if (retval /= nf90_noerr) stop nf90_strerror(retval)
    else
      print*, 'Variable snwdph already exist!!!'
      stop
    endif

    if (nf90_inq_varid(ncid, "zorl", zorl_id) /= nf90_noerr) then
      retval = nf90_def_var(ncid, "zorl",  nf90_double, (/ xdimid, ydimid /), zorl_id)
      if (retval /= nf90_noerr) stop nf90_strerror(retval)
    else
      print*, 'Variable zorl already exist!!!'
      stop
    endif

    !--------------------------------------------------
    ! Optional attributes
    !--------------------------------------------------
    retval = nf90_put_att(ncid, sheleg_id,  "long_name", "sheleg")
    retval = nf90_put_att(ncid, snwdph_id, "long_name", "snwdph")
    retval = nf90_put_att(ncid, zorl_id, "long_name", "zorl")

    retval = nf90_put_att(ncid, sheleg_id, "units", "none")
    retval = nf90_put_att(ncid, snwdph_id, "units", "none")
    retval = nf90_put_att(ncid, zorl_id, "units", "none")

    retval = nf90_put_att(ncid, sheleg_id, "coordinates", "geolon geolat")
    retval = nf90_put_att(ncid, snwdph_id, "coordinates", "geolon geolat")
    retval = nf90_put_att(ncid, zorl_id, "coordinates", "geolon geolat")

    !--------------------------------------------------
    ! Exit define mode
    !--------------------------------------------------
    retval = nf90_enddef(ncid)
    if (retval /= nf90_noerr) stop nf90_strerror(retval)

    !--------------------------------------------------
    ! Fill example data
    !--------------------------------------------------
    sheleg = 0.0_dp
    snwdph = 0.0_dp
    zorl   = 0.0_dp

    !--------------------------------------------------
    ! Write variables
    !--------------------------------------------------
    retval = nf90_put_var(ncid, sheleg_id,  sheleg)
    if (retval /= nf90_noerr) stop nf90_strerror(retval)

    retval = nf90_put_var(ncid, snwdph_id, snwdph)
    if (retval /= nf90_noerr) stop nf90_strerror(retval)

    retval = nf90_put_var(ncid, zorl_id, zorl)
    if (retval /= nf90_noerr) stop nf90_strerror(retval)

    !--------------------------------------------------
    ! Close file
    !--------------------------------------------------
    retval = nf90_close(ncid)
    if (retval /= nf90_noerr) stop nf90_strerror(retval)

    !--------------------------------------------------
    ! Deallocate arrays
    !--------------------------------------------------
    deallocate(sheleg, snwdph, zorl)

  enddo ! i loop over 6 cube faces

end program add_3vars_2d
