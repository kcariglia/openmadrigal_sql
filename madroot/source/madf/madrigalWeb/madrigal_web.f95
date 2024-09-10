      module madrigal_web

          implicit none

          ! The madrigal_web module is meant as a complete read Fortran 95 API into Madrigal
          ! See file testMadrigalWeb.f95 for example usage of all calls
          ! Written by Bill Rideout
          ! $Id: madrigal_web.f95 6099 2017-05-31 14:08:57Z brideout $

          ! data structures

          ! connection - created by get_connection
          ! passed into all other methods
          type :: connection
              character (len=1024) :: main_url
              character (len=1024) :: cgi_url
              character (len=512)  :: user_fullname
              character (len=512)  :: user_email
              character (len=512)  :: user_affiliation
              integer              :: site_id ! site id of site connected to
              character (len=4096) :: version ! version of local Madrigal site in form I.I[.I]
          end type connection


          ! instrument - created by get_instruments
          type :: instrument
              character (len=1024)  :: name ! instrument name
              integer               :: code ! instrument id code (kinst)
              character (len=12)    :: mnemonic ! 3 letter code
              double precision      :: latitude
              double precision      :: longitude
              double precision      :: altitude ! in km
              character (len=1024)  :: category ! Example: 'Incoherent Scatter Radars'
              character (len=1024)  :: pi ! Instrument PI.  Will be 'Unknown' for Madrigal 2.X sites
              character (len=1024)  :: pi_email ! Instrument PI email.  Will be 'Unknown' for Madrigal 2.X sites
          end type instrument


          ! site - created by get_sites
          type :: site
              integer               :: site_id
              character (len=1024)  :: site_name ! site name
              character (len=1024)  :: site_url ! url to site home page
              character (len=1024)  :: contact_name ! site contact name
              character (len=1024)  :: contact_email ! site contact email
          end type site


          ! experiment - created by get_experiments
          type :: experiment
              integer               :: id ! unique id that identifies experiment
              character (len=1024)  :: url ! Example: 'http://madrigal.haystack.mit.edu/cgi-bin/madtoc/1997/mlh/03dec97'
              character (len=1024)  :: name ! experiment name. Example: 'Wide Latitude Substorm Study'
              integer               :: site_id ! site id of Madrigal site where data located
              character (len=1024)  :: site_name ! site name of site where data is. eg: 'Millstone Hill'
              integer               :: kinst ! id code of instrument
              character (len=1024)  :: inst_name ! Example: 'Millstone Hill Zenith Radar'
              integer               :: syear, smonth, sday, shour, sminute, ssecond ! start time
              integer               :: eyear, emonth, eday, ehour, eminute, esecond ! end time
              logical               :: is_local ! True if local data to this Madrigal site, False if remote data
              character (len=1024)  :: madrigal_url ! url to Madrigal site where data is. If local, same as init
              character (len=1024)  :: pi ! Instrument PI.  Will be 'Unknown' for Madrigal 2.X sites
              character (len=1024)  :: pi_email ! Instrument PI email.  Will be 'Unknown' for Madrigal 2.X sites
              character (len=1024)  :: real_url ! real url to Madrigal experiment. url string is only historical format
              integer               :: uttimestamp ! st_mtime of expDir. -999 if not supported by the Madrigal site
              integer               :: access ! access code of the experiment (0 if public, 2 if public, -999 if n/a).
              character (len=1024)  :: version ! version of Madrigal site where data is in form I.I[.I]
          end type experiment


          ! experiment_file - created by get_experiment_files
          type :: experiment_file
              character (len=1024)  :: fullname ! file fullname
              integer               :: kindat ! kind of data code of this file
              character (len=2048)  :: kindat_desc ! string describing kind of data
              integer               :: category ! (1=default, 2=variant, 3=history, 4=real-time)
              character (len=1024)  :: status ! status description
              integer               :: permission ! 0 for public, 1 for private
              integer               :: exp_id ! experiment id this file belongs to
          end type experiment_file


          ! cedar_parameter - created by get_cedar_parameters
          type :: cedar_parameter
              character (len=128)   :: mnemonic ! parameter mnemonic
              character (len=2048)  :: description ! parameter description
              integer               :: is_error ! 1 if error parameter, 0 if not
              character (len=128)   :: units ! units description
              integer               :: is_measured ! 1 if parm in file, 0 if derived
              character (len=1028)  :: category ! parameter category description
          end type cedar_parameter


      contains


          function get_connection(madrigal_url, user_fullname, &
          user_email, user_affiliation)
          ! get_connection returns the connection type needed by all other methods
          ! always the first method called for each session
          ! arguments:
          !  madrigal_url - url of particular Madrigal site's home page
          !  user_fullname - your full name
          !  user_email - your email
          !  user_affiliation - your affiliation. Use 'None' if none
          ! returns a connection structure which is the first argument of all other calls
              character (len=*), intent(in) :: madrigal_url
              character (len=*), intent(in) :: user_fullname
              character (len=*), intent(in) :: user_email
              character (len=*), intent(in) :: user_affiliation
              type(connection)              :: get_connection

              ! local variables
              character (len=4096):: tmp_file
              character (len=2048):: tmp_line
              character (len=1024):: tmp_url
              integer             :: io, cgi_index, quote_index, left, right
              integer             :: slash_index, url_len

              get_connection%main_url = trim(madrigal_url)
              get_connection%user_fullname = user_fullname
              get_connection%user_email = user_email
              get_connection%user_affiliation = user_affiliation
              url_len = len_trim(madrigal_url)

              ! find cgi_url
              tmp_file = get_url(madrigal_url)
              open(100, file=tmp_file)
              do
                read(100,'(A)',IOSTAT=io) tmp_line
                if (io<0) exit
                cgi_index = index(tmp_line, 'accessData.cgi')
                if (cgi_index <= 0) cycle
                quote_index = index(tmp_line(:cgi_index-1), '"', back=.true.)
                if (cgi_index-quote_index > 2) then
                  ! see if we need to strip off some of madrigal_url
                  slash_index = index(madrigal_url(8:), '/')
                  if ((slash_index < len_trim(madrigal_url(8:)) -1) .and. (slash_index > 0)) then
                      get_connection%cgi_url = madrigal_url(:slash_index + 6) // tmp_line(quote_index+1:cgi_index-1)
                  else
                      get_connection%cgi_url = madrigal_url // tmp_line(quote_index+1:cgi_index-1)
                  end if
                else if (madrigal_url(url_len:url_len) .ne. '/') then
                    get_connection%cgi_url = madrigal_url // '/'
                else
                    get_connection%cgi_url = madrigal_url
                end if
                ! no need to read further
                exit
              end do
              close(100)

              ! find site_id
              tmp_url = trim(get_connection%cgi_url) // 'getMetadata?fileType=0'
              tmp_file = get_url(tmp_url)
              open(100, file=tmp_file)
              do
                read(100,'(A)',IOSTAT=io) tmp_line
                if (io<0) exit
                tmp_line = trim(tmp_line)
                ! site id id fourth item
                left = 1
                right = index(tmp_line(left:), ',')
                if (right .le. 0) cycle
                ! second item - skip
                left = right+1
                right = index(tmp_line(left:), ',') + left - 1
                ! third item - skip
                left = right+1
                right = index(tmp_line(left:), ',') + left - 1
                ! fourth item - site id
                left = right+1
                right = index(tmp_line(left:), ',')
                read(tmp_line(left:left+right-2), *) get_connection%site_id
                ! no need to read further
                exit
              end do
              close(100)

              ! find version
               tmp_url = trim(get_connection%cgi_url) // 'getVersionService.py'
              tmp_file = get_url(tmp_url)
              open(100, file=tmp_file)
              do
                read(100,'(A)',IOSTAT=io) tmp_line
                if (io<0) exit
                tmp_line = trim(tmp_line)
                if (len(tmp_line) > 1) then
                  get_connection%version = tmp_line
                else
                  cycle
                end if
                exit
              end do
              close(100)


          end function get_connection



          subroutine get_instruments(conn, inst_arr, inst_count)
          ! get_instruments allocates an array of instrument structures
          ! describing all available Madrigal instruments
          !
          ! arguments:
          !  conn - the connection structure created by get_connection call
          !  inst_arr - a pointer to an instrument array to be allocated and populated.
          !     user is responsible for calling deallocate and nullify when done
          !  inst_count - set to the number of instruments in inst_arr
            type(connection), intent(in)                           :: conn
            type(instrument), dimension(:), pointer, intent(inout) :: inst_arr
            integer, intent(inout)                                 :: inst_count

            ! local variables
            character (*), parameter :: url = 'getInstrumentsService.py'
            character (len=4096)     :: tmp_file
            integer                  :: io, left, right, i
            character (len=2048)     :: tmp_line

            tmp_file = get_url(trim(conn%cgi_url) // url)

            ! first pass is just to get count
            inst_count = 0
            open(100, file=tmp_file)
            do
              read(100,'(A)',IOSTAT=io) tmp_line
              if (io<0) exit
              left = 1
              right = index(tmp_line(left:), ',')
              if (right .le. 0) cycle
              inst_count = inst_count + 1

            end do
            close(100)

            ! allocate return array of instruments
            allocate(inst_arr(inst_count))

            ! second pass is to fill out inst_arr
            open(100, file=tmp_file)
            i = 1 ! index into inst_arr
            do
              read(100,'(A)',IOSTAT=io) tmp_line
              if (io<0) exit

              ! set defaults that may be overwritten
              inst_arr(i)%category = 'Unknown'
              inst_arr(i)%pi = 'Unknown'
              inst_arr(i)%pi_email = 'Unknown'

              ! read name
              left = 1
              right = index(tmp_line(left:), ',')
              if (right .le. 0) cycle
              inst_arr(i)%name = tmp_line(left:right-1)
              ! read code
              left = right+1
              right = index(tmp_line(left:), ',')
              read(tmp_line(left:left+right-2), *) inst_arr(i)%code
              ! read mnemonic
              left = left + right
              right = index(tmp_line(left:), ',')
              inst_arr(i)%mnemonic = tmp_line(left:(left + right)-2)
              ! read latitude
              left = left + right
              right = index(tmp_line(left:), ',')
              read(tmp_line(left:left+right-2), *) inst_arr(i)%latitude
              ! read longitude
              left = left + right
              right = index(tmp_line(left:), ',')
              read(tmp_line(left:left+right-2), *) inst_arr(i)%longitude
              ! read altitude
              left = left + right
              right = index(tmp_line(left:), ',')
              if (right > 0) then
                read(tmp_line(left:left+right-2), *) inst_arr(i)%altitude
              else
                read(tmp_line(left:), *) inst_arr(i)%altitude
                i = i + 1
                cycle
              end if
              ! read category
              left = left + right
              right = index(tmp_line(left:), ',')
              if (right > 0) then
                inst_arr(i)%category = tmp_line(left:(left + right)-2)
              else
                inst_arr(i)%category = tmp_line(left:)
                i = i + 1
                cycle
              end if
              ! try to read pi
              left = left + right
              right = index(tmp_line(left:), ',')
              if (right > 0) then
                  inst_arr(i)%pi = tmp_line(left:(left + right)-2)
                  ! read pi_email
                  left = left + right
                  right = index(tmp_line(left:), ',')
                  if (right < 1) then
                      inst_arr(i)%pi_email = tmp_line(left:)
                  else
                      inst_arr(i)%pi_email = tmp_line(left:(left + right)-2)
                  end if
              end if

              i = i + 1

            end do
            close(100)

          end subroutine get_instruments


          subroutine get_sites(conn, site_arr, site_count)
          ! get_sites allocates an array of site structures
          ! describing all available Madrigal sites
          !
          ! arguments:
          !  conn - the connection structure created by get_connection call
          !  site_arr - a pointer to a site array to be allocated and populated.
          !     user is responsible for calling deallocate and nullify when done
          !  site_count - set to the number of sites in site_arr
            type(connection), intent(in)                           :: conn
            type(site), dimension(:), pointer, intent(inout)       :: site_arr
            integer, intent(inout)                                 :: site_count

            ! local variables
            character (*), parameter :: url = 'getMetadata?fileType=5'
            character (len=4096)     :: tmp_file
            integer                  :: io, left, right, i, j
            character (len=2048)     :: tmp_line

            tmp_file = get_url(trim(conn%cgi_url) // url)

            ! first pass is just to get count
            site_count = 0
            open(100, file=tmp_file)
            do
              read(100,'(A)',IOSTAT=io) tmp_line
              if (io<0) exit
              left = 1
              right = index(tmp_line(left:), ',')
              if (right .le. 0) cycle
              site_count = site_count + 1

            end do
            close(100)

            ! allocate return array of sites
            allocate(site_arr(site_count))

            ! second pass is to fill out site_arr
            open(100, file=tmp_file)
            i = 1 ! index into inst_arr
            do
              read(100,'(A)',IOSTAT=io) tmp_line
              if (io<0) exit
              ! read site_id
              left = 1
              right = index(tmp_line(left:), ',')
              if (right .le. 0) cycle
              read(tmp_line(left:left+right-1), *) site_arr(i)%site_id
              ! read site_name
              left = right+1
              right = index(tmp_line(left:), ',')
              site_arr(i)%site_name = tmp_line(left:(left + right)-2)
              ! read site_url (part 1)
              left = left + right
              right = index(tmp_line(left:), ',')
              site_arr(i)%site_url = 'http://' // tmp_line(left:(left + right)-2)
              ! append trailing slash if needed
              j = len(trim(site_arr(i)%site_url))
              if (site_arr(i)%site_url(j:j) .ne. '/') then
                site_arr(i)%site_url = trim(site_arr(i)%site_url) // "/"
              end if
              ! append rest of url if any
              left = left + right
              right = index(tmp_line(left:), ',')
              site_arr(i)%site_url = trim(site_arr(i)%site_url) // tmp_line(left:(left + right)-2)
              ! skip next two fields
              left = left + right
              right = index(tmp_line(left:), ',')
              left = left + right
              right = index(tmp_line(left:), ',')
              ! contact_name
              left = left + right
              right = index(tmp_line(left:), ',')
              site_arr(i)%contact_name = tmp_line(left:(left + right)-2)
              ! skip next eight fields
              left = left + right
              right = index(tmp_line(left:), ',')
              left = left + right
              right = index(tmp_line(left:), ',')
              left = left + right
              right = index(tmp_line(left:), ',')
              left = left + right
              right = index(tmp_line(left:), ',')
              left = left + right
              right = index(tmp_line(left:), ',')
              left = left + right
              right = index(tmp_line(left:), ',')
              left = left + right
              right = index(tmp_line(left:), ',')
              left = left + right
              right = index(tmp_line(left:), ',')
              ! contact_email
              left = left + right
              right = index(tmp_line(left:), ',')
              site_arr(i)%contact_email = tmp_line(left:(left + right)-2)

              i = i + 1

            end do
            close(100)

          end subroutine get_sites



          subroutine get_experiments(conn, kinst, syear, smonth, sday, shour, sminute, ssecond,  &
               eyear, emonth, eday, ehour, eminute, esecond, local_only, exp_arr, exp_count)
          ! get_experiments allocates an array of exteriment structures
          ! describing all available Madrigal experiments with the given kinst and time range
          !
          ! arguments:
          !  conn - the connection structure created by get_connection call
          !  syear, smonth, sday, shour, sminute, ssecond - exclude exps before this time
          !  eyear, emonth, eday, ehour, eminute, esecond - exclude exps after this time
          !  local_only - if True, only return experiments local to this server.
          !               if False, return local and remote experiments
          !  exp_arr - a pointer to an experiment array to be allocated and populated.
          !     user is responsible for calling deallocate and nullify when done
          !  exp_count - set to the number of experiments in exp_arr
            type(connection), intent(in)                           :: conn
            integer, intent(in)  :: kinst
            integer, intent(in)  :: syear, smonth, sday, shour, sminute, ssecond
            integer, intent(in)  :: eyear, emonth, eday, ehour, eminute, esecond
            logical, intent(in)  :: local_only
            type(experiment), dimension(:), pointer, intent(inout) :: exp_arr
            integer, intent(inout)                                 :: exp_count

            ! local variables
            character (len=2048)     :: url
            character (len=1024)     :: args
            character (len=4096)      :: tmp_file, tmp_url
            integer                  :: io, left, right, i, j
            character (len=2048)     :: tmp_line
            integer                  :: local, url_index
            ! arguments needed by get_sites call, called only to fill in info for non-local exps
            type(site), dimension(:), pointer :: site_arr ! returns an allocated array of site structures
            integer :: site_count ! returns the number of site returned

            if (local_only) then
              local = 1
            else
              local = 0
              call get_sites(conn, site_arr, site_count) ! needed to get urls of remote sites
            end if

            ! build url from arguments
            url = trim(conn%cgi_url) // 'getExperimentsService.py?'
            write(args, 200) kinst, syear, smonth, sday, shour, sminute, ssecond, &
                eyear, emonth, eday, ehour, eminute, esecond, local

        200 format('code=', I0, '&startyear=', I0, '&startmonth=', I0, '&startday=', I0, &
                '&starthour=', I0, '&startmin=', I0, '&startsec=', I0, &
                '&endyear=', I0, '&endmonth=', I0, '&endday=', I0, &
                '&endhour=', I0, '&endmin=', I0, '&endsec=', I0, '&local=', I0)

            url = trim(url) // trim(args)

            tmp_file = get_url(url)

            ! first pass is just to get count
            exp_count = 0
            open(100, file=tmp_file)
            do
              read(100,'(A)',IOSTAT=io) tmp_line
              if (io<0) exit
              left = 1
              right = index(tmp_line(left:), ',')
              if (right .le. 0) cycle
              exp_count = exp_count + 1

            end do
            close(100)

            ! allocate return array of instruments
            allocate(exp_arr(exp_count))

            ! second pass is to fill out exp_arr
            open(100, file=tmp_file)
            i = 1 ! index into exp_arr
            do
              read(100,'(A)',IOSTAT=io) tmp_line
              if (io<0) exit

              ! set defaults that may be overwritten
              exp_arr(i)%pi = 'Unknown'
              exp_arr(i)%pi_email = 'Unknown'
              exp_arr(i)%uttimestamp = -999
              exp_arr(i)%access = -999

              ! read id
              left = 1
              right = index(tmp_line(left:), ',')
              if (right .le. 0) cycle
              read(tmp_line(left:left+right-2), *) exp_arr(i)%id
              ! read url
              left = right+1
              right = index(tmp_line(left:), ',')
              exp_arr(i)%url = tmp_line(left:(left + right)-2)
              ! read name
              left = left + right
              right = index(tmp_line(left:), ',')
              exp_arr(i)%name = tmp_line(left:(left + right)-2)
              ! read site_id
              left = left + right
              right = index(tmp_line(left:), ',')
              read(tmp_line(left:left+right-2), *) exp_arr(i)%site_id
              ! read site_name
              left = left + right
              right = index(tmp_line(left:), ',')
              exp_arr(i)%site_name = tmp_line(left:(left + right)-2)
              ! read kinst
              left = left + right
              right = index(tmp_line(left:), ',')
              read(tmp_line(left:left+right-2), *) exp_arr(i)%kinst
              ! read inst_name
              left = left + right
              right = index(tmp_line(left:), ',')
              exp_arr(i)%inst_name = tmp_line(left:(left + right)-2)

              ! read syear, smonth, sday, shour, sminute, ssecond
              left = left + right
              right = index(tmp_line(left:), ',')
              read(tmp_line(left:left+right-2), *) exp_arr(i)%syear
              left = left + right
              right = index(tmp_line(left:), ',')
              read(tmp_line(left:left+right-2), *) exp_arr(i)%smonth
              left = left + right
              right = index(tmp_line(left:), ',')
              read(tmp_line(left:left+right-2), *) exp_arr(i)%sday
              left = left + right
              right = index(tmp_line(left:), ',')
              read(tmp_line(left:left+right-2), *) exp_arr(i)%shour
              left = left + right
              right = index(tmp_line(left:), ',')
              read(tmp_line(left:left+right-2), *) exp_arr(i)%sminute
              left = left + right
              right = index(tmp_line(left:), ',')
              read(tmp_line(left:left+right-2), *) exp_arr(i)%ssecond

              ! read eyear, emonth, eday, ehour, eminute, esecond
              left = left + right
              right = index(tmp_line(left:), ',')
              read(tmp_line(left:left+right-2), *) exp_arr(i)%eyear
              left = left + right
              right = index(tmp_line(left:), ',')
              read(tmp_line(left:left+right-2), *) exp_arr(i)%emonth
              left = left + right
              right = index(tmp_line(left:), ',')
              read(tmp_line(left:left+right-2), *) exp_arr(i)%eday
              left = left + right
              right = index(tmp_line(left:), ',')
              read(tmp_line(left:left+right-2), *) exp_arr(i)%ehour
              left = left + right
              right = index(tmp_line(left:), ',')
              read(tmp_line(left:left+right-2), *) exp_arr(i)%eminute
              left = left + right
              right = index(tmp_line(left:), ',')
              read(tmp_line(left:left+right-2), *) exp_arr(i)%esecond
              ! set is_local
              exp_arr(i)%is_local = (conn%site_id == exp_arr(i)%site_id)
              ! set madrigal_url
              if (local .eq. 1) then
                exp_arr(i)%madrigal_url = trim(conn%main_url)
              else if (exp_arr(i)%is_local) then
                exp_arr(i)%madrigal_url = trim(conn%main_url)
              else
                exp_arr(i)%madrigal_url = 'unknown' ! default if not found
                do j=1,site_count
                  if (site_arr(j)%site_id .eq. exp_arr(i)%site_id) then
                    exp_arr(i)%madrigal_url = site_arr(j)%site_url
                    exit
                  end if
                end do
              end if

              ! real url
              if (exp_arr(i)%is_local) then
                url_index = index(exp_arr(i)%url, '/madtoc/')
                if (compare_versions(conn%version, '3.0') .gt. -1) then
                  write(tmp_url, 220) exp_arr(i)%id
        220       format('showExperiment/?experiment_list=', I0)
                  exp_arr(i)%real_url = trim(exp_arr(i)%url(:url_index)) // trim(tmp_url)
                else
                  exp_arr(i)%real_url = trim(exp_arr(i)%url(:url_index)) // 'madExperiment.cgi?exp='
                  exp_arr(i)%real_url = trim(exp_arr(i)%real_url) // trim(exp_arr(i)%url(url_index+8:))
                  exp_arr(i)%real_url = trim(exp_arr(i)%real_url) // '&displayLevel=0&expTitle='
                end if
              else
                exp_arr(i)%real_url = 'Cannot_get_real_url_for_remote_site'
              end if

              ! skip security code
              left = left + right
              right = index(tmp_line(left:), ',')
              if (right .eq. 0) then
                i = i + 1
                cycle
              end if

              ! try to read pi
              left = left + right
              right = index(tmp_line(left:), ',')
              if (right > 0) then
                  exp_arr(i)%pi = tmp_line(left:(left + right)-2)
                  ! read pi_email
                  left = left + right
                  right = index(tmp_line(left:), ',')
                  if (right < 1) then
                      exp_arr(i)%pi_email = tmp_line(left:)
                      i = i + 1
                      cycle
                  else
                      exp_arr(i)%pi_email = tmp_line(left:(left + right)-2)
                  end if
              end if

              ! try to read uttimestamp
              left = left + right
              right = index(tmp_line(left:), ',')
              if (right < 1) then
                  read(tmp_line(left:), *) exp_arr(i)%uttimestamp
                  i = i + 1
                  cycle
              else
                  read(tmp_line(left:left+right-2), *) exp_arr(i)%uttimestamp
              end if

              ! try to read access
              left = left + right
              right = index(tmp_line(left:), ',')
              if (right < 1) then
                  read(tmp_line(left:), *) exp_arr(i)%access
                  i = i + 1
                  cycle
              else
                  read(tmp_line(left:left+right-2), *) exp_arr(i)%access
              end if

              i = i + 1

            end do
            close(100)

            ! free site_arr if needed to avoid memory leak
            if (local .eq. 0) then
              deallocate(site_arr)
              nullify(site_arr)
            end if

          end subroutine get_experiments


          subroutine get_experiment_files(conn, exp_id, file_arr, file_count)
          ! get_experiment_files allocates an array of experiment_file structures
          ! describing all available Madrigal files for the given experiment id
          !
          ! arguments:
          !  conn - the connection structure created by get_connection call
          !  exp_id - experiment id as returned by get_experiments
          !  file_arr - a pointer to ae experiment_file array to be allocated and populated.
          !     user is responsible for calling deallocate and nullify when done
          !  file_count - set to the number of file in file_arr
            type(connection), intent(in)                                :: conn
            integer, intent(in)                                         :: exp_id
            type(experiment_file), dimension(:), pointer, intent(inout) :: file_arr
            integer, intent(inout)                                      :: file_count

            ! local variables
            character (len=2048)     :: url
            character (len=1024)     :: args
            character (len=4096)     :: tmp_file
            integer                  :: io, left, right, i
            character (len=2048)     :: tmp_line

            ! build url from arguments
            url = trim(conn%cgi_url) // 'getExperimentFilesService.py?'
            write(args, 300) exp_id

      300   format('id=', I0)

            url = trim(url) // trim(args)

            tmp_file = get_url(url)

            ! first pass is just to get count
            file_count = 0
            open(100, file=tmp_file)
            do
              read(100,'(A)',IOSTAT=io) tmp_line
              if (io<0) exit
              left = 1
              right = index(tmp_line(left:), ',')
              if (right .le. 0) cycle
              file_count = file_count + 1

            end do
            close(100)

            ! allocate return array of experiment_files
            allocate(file_arr(file_count))

            ! second pass is to fill out file_arr
            open(100, file=tmp_file)
            i = 1 ! index into file_arr
            do
              read(100,'(A)',IOSTAT=io) tmp_line
              if (io<0) exit

              ! read fullname
              left = 1
              right = index(tmp_line(left:), ',')
              if (right .le. 0) cycle
              file_arr(i)%fullname = tmp_line(left:(left + right)-2)
              ! read kindat
              left = left + right
              right = index(tmp_line(left:), ',')
              read(tmp_line(left:left+right-2), *) file_arr(i)%kindat
              ! read kindat_desc
              left = left + right
              right = index(tmp_line(left:), ',')
              file_arr(i)%kindat_desc = tmp_line(left:(left + right)-2)
              ! read category
              left = left + right
              right = index(tmp_line(left:), ',')
              read(tmp_line(left:left+right-2), *) file_arr(i)%category
              ! read status
              left = left + right
              right = index(tmp_line(left:), ',')
              file_arr(i)%status = tmp_line(left:(left + right)-2)
              ! read permission
              left = left + right
              right = index(tmp_line(left:), ',')
              if (right > 0) then
                read(tmp_line(left:left+right-2), *) file_arr(i)%permission
              else
                read(tmp_line(left:), *) file_arr(i)%permission
              end if
              file_arr(i)%exp_id = exp_id

              i = i + 1

            end do
            close(100)

          end subroutine get_experiment_files


          subroutine get_cedar_parameters(conn, fullname, parm_arr, parm_count)
          ! get_cedar_parameters allocates an array of cedar_parameter structures
          ! describing all available Madrigal parameters for the given fullname of file
          ! as returned by get_experiment_files
          !
          ! arguments:
          !  conn - the connection structure created by get_connection call
          !  fullname - fullname of madrigal file as returned by get_experiment_files
          !  parm_arr - a pointer to an cedar_parameter array to be allocated and populated.
          !     user is responsible for calling deallocate and nullify when done
          !  parm_count - set to the number of cedar_parameters in parm_arr
          !
          ! delimiter is \, not ,
          !
            type(connection), intent(in)                                :: conn
            character (len=*) , intent(in)                              :: fullname
            type(cedar_parameter), dimension(:), pointer, intent(inout) :: parm_arr
            integer, intent(inout)                                      :: parm_count

            ! local variables
            character (len=2048)     :: url
            character (len=4096)     :: tmp_file
            integer                  :: io, left, right, i
            character (len=2048)     :: tmp_line

            ! build url from arguments
            url = trim(conn%cgi_url) // 'getParametersService.py?filename='

            url = trim(url) // trim(fullname)

            tmp_file = get_url(url)

            ! first pass is just to get count
            parm_count = 0
            open(100, file=tmp_file)
            do
              read(100,'(A)',IOSTAT=io) tmp_line
              if (io<0) exit
              left = 1
              right = index(tmp_line(left:), '\')
              if (right .le. 0) cycle
              parm_count = parm_count + 1

            end do
            close(100)

            ! allocate return array of cedar_parameters
            allocate(parm_arr(parm_count))

            ! second pass is to fill out parm_arr
            open(100, file=tmp_file)
            i = 1 ! index into parm_arr
            do
              read(100,'(A)',IOSTAT=io) tmp_line
              if (io<0) exit

              ! read mnemonic
              left = 1
              right = index(tmp_line(left:), '\')
              if (right .le. 0) cycle
              parm_arr(i)%mnemonic = tmp_line(left:(left + right)-2)
              ! read description
              left = left + right
              right = index(tmp_line(left:), '\')
              parm_arr(i)%description = tmp_line(left:(left + right)-2)
              ! read is_error
              left = left + right
              right = index(tmp_line(left:), '\')
              read(tmp_line(left:left+right-2), *) parm_arr(i)%is_error
              ! read units
              left = left + right
              right = index(tmp_line(left:), '\')
              parm_arr(i)%units = tmp_line(left:(left + right)-2)
              ! read is_measured
              left = left + right
              right = index(tmp_line(left:), '\')
              read(tmp_line(left:left+right-2), *) parm_arr(i)%is_measured
              ! read category
              left = left + right
              right = index(tmp_line(left:), '\')
              if (right > 0) then
                parm_arr(i)%category = tmp_line(left:(left + right)-2)
              else
                parm_arr(i)%category = tmp_line(left:)
              end if

              i = i + 1

            end do
            close(100)

          end subroutine get_cedar_parameters


          function download_file(conn, fullname, destination, user_fullname, user_email, user_affiliation, file_format)
          ! download_file downloads fullname as returned by get_experiment_files to destination is specified
          !    file_format
          !
          ! arguments:
          !  conn - the connection structure created by get_connection call
          !  fullname - fullname of madrigal file as returned by get_experiment_files
          !  destination - full path to save file as locally
          !  user_fullname, user_email, user_affiliation - three string describing requester
          !  file_format - must be 'ascii', 'hdf5', or 'netCDF4'.  'netCDF4' requires
          !    a madrigal 3.0 site of higher
          !
          ! Returns 0 is success, -1 if not
          !
            type(connection), intent(in)                    :: conn
            character (len=*) , intent(in)                  :: fullname
            character (len=*) , intent(in)                  :: destination
            character (len=*) , intent(in)                  :: user_fullname
            character (len=*) , intent(in)                  :: user_email
            character (len=*) , intent(in)                  :: user_affiliation
            character (len=*) , intent(in)                  :: file_format
            integer                                         :: download_file

            ! local variables
            character (len=2048)     :: url
            character (len=1024)     :: args
            character (len=4096)     :: tmp_file
            integer                  :: fileType
            character (len=1)        :: blank
            character (len=1)        :: plus
            character (len=2048)     :: cmd

            blank = ' '
            plus = '+'

            if (file_format .eq. 'ascii') then
              fileType = -1
            else if (file_format .eq. 'hdf5') then
              fileType = -2
            else if (file_format .eq. 'netCDF4') then
              fileType = -3
              ! verify site at least 3.0
              if (compare_versions(conn%version, '3.0') < 0) then
                print*, 'Cannot download netCDF4 files when Madrigal version only ', conn%version
                download_file = -1
                return
              end if
            else
              print*, 'Illegal file_format ', file_format
              download_file = -1
              return
            end if

            ! build url from arguments
            url = trim(conn%cgi_url) // 'getMadfile.cgi?fileName='

            url = trim(url) // trim(fullname)

            write(args, 400), fileType
     400    format('&fileType=', I0, '&user_fullname=')

            url = trim(url) // trim(args)
            url = trim(url) // trim(Replace_Text(user_fullname, blank, plus))
            url = trim(url) // '&user_email=' // trim(user_email) // '&user_affiliation='
            url = trim(url) // trim(Replace_Text(user_affiliation, blank, plus))

            tmp_file = get_url(url)
            cmd = 'cp ' // trim(tmp_file) // ' ' // trim(destination)
            call system(cmd)

          end function download_file


          function isprint(conn, fullname, parms, filters, destination, user_fullname, user_email, user_affiliation)
          ! isprint downloads fullname to destination with requested parms and filters applied.  parms
          !     may include both measured and derived parameters.  Format of output file is hdf5, netCDF4, or
          !     ascii as determined by destination extension
          !
          ! arguments:
          !  conn - the connection structure created by get_connection call
          !  fullname - fullname of madrigal file as returned by get_experiment_files
          !  parms - Comma delimited string listing requested parameters (no spaces allowed).
          !  filters - Space delimited string listing filters desired, as in isprint command All filters
          !      begin with ==filter=. Details: --filter=<[mnemonic] or [mnemonic1,[+-*/]mnemonic2]>,<lower limit1>,
          !      <upper limit1>[or<lower limit2>,<upper limit2>...] a filter using any measured or derived Madrigal
          !      parameter, or two Madrigal parameters either added, subtracted, multiplied or divided. Each filter
          !      has one or more allowed ranges. The filter accepts data that is in any allowed range. If the Madrigal
          !      parameter value is missing, the filter will always reject that data. Multiple filter arguments are
          !      allowed. To skip either a lower limit or an upper limit, leave it blank. Examples: (--filter=ti,500,1000
          !      (Accept when 500 <= Ti <= 1000) or --filter=gdalt,-,sdwht,0, (Accept when gdalt > shadowheight - that is,
          !      point in direct sunlight) or --filter=gdalt,200,300or1000,1200
          !      (Accept when 200 <= gdalt <= 300 OR 1000 <= gdalt <= 1200))
          !  destination - full path to save file as locally. If extention is .h5, .hdf, or .hdf5,
          !     will download in Madrigal Hdf5 format. If it has a .nc extension, will
          !     download as netCDF4. Otherwise, it will download as column delimited ascii.
          !     Trying to save as Hdf5 or netCDF4 with a Madrigal 2 site will raise an exception
          !  user_fullname, user_email, user_affiliation - three string describing requester
          !
          ! Returns 0 is success, -1 if not
          !
            type(connection), intent(in)                    :: conn
            character (len=*) , intent(in)                  :: fullname
            character (len=*) , intent(in)                  :: parms
            character (len=*) , intent(in)                  :: filters
            character (len=*) , intent(in)                  :: destination
            character (len=*) , intent(in)                  :: user_fullname
            character (len=*) , intent(in)                  :: user_email
            character (len=*) , intent(in)                  :: user_affiliation
            integer                                         :: isprint

            ! local variables
            character (len=2048)     :: url
            character (len=4096)     :: tmp_file
            character (len=1)        :: blank
            character (len=1)        :: plus
            character (len=1)        :: comma
            character (len=2048)     :: cmd

            blank = ' '
            plus = '+'
            comma = ','

            ! build url from arguments
            url = trim(conn%cgi_url) // 'isprintService.py?file='

            url = trim(url) // trim(fullname) // '&parms='
            url = trim(url) // trim(Replace_Text(parms, comma, plus)) // '&filters='
            url = trim(url) // trim(filters) // '&output='
            url = trim(url) // trim(get_basename(destination)) // '&user_fullname='
            url = trim(url) // trim(Replace_Text(user_fullname, blank, plus))
            url = trim(url) // '&user_email=' // trim(user_email) // '&user_affiliation='
            url = trim(url) // trim(Replace_Text(user_affiliation, blank, plus))

            tmp_file = get_url(url)
            cmd = 'cp ' // trim(tmp_file) // ' ' // trim(destination)
            call system(cmd)

            isprint = 0

          end function isprint


          function mad_calculator(conn, parms, destination, year, month, day, hour, minute, second,   &
              start_lat, stop_lat, step_lat, start_lon, stop_lon, step_lon, start_alt, stop_alt, step_alt, &
              one_d_parms, one_d_values)
          ! mad_calculator runs the Madrigal derivation engine to derive the requested parameters for the specified time
          !     and range of geodetic locations. Writes ascii output to destination.  Optional arguments
          !     are one_d_parms and one_d_values, where if given these set additional one d values for the given one d parms.
          !
          ! arguments:
          !  conn - the connection structure created by get_connection call
          !  parms - Comma delimited string listing requested parameters (no spaces allowed).
          !  destination - full path to save ascii file as locally.
          !  year, month, day, hour. minute. second - six integers that set input time
          !  start_lat, stop_lat, step_lat - three real dp that set latitude range and step in degrees (-90 to 90)
          !  start_lon, stop_lon, step_lon - three real dp that set longitude range and step in degrees (-180 to 180)
          !  start_alt, stop_alt, step_alt - three real dp that set altitude range and step in km
          !  one_d_parms - a string with comma separated mnemonics representing the one D parameters to specify (no spaces)
          !     (optional argument). Example: 'azm,elm'
          !  one_d_values - a string with comma separated float strings representing the one D values to specify (no spaces)
          !     (optional argument) Number of values must match number of parms in one_d_parms. Example: '27.5,-165'
          !
          ! Returns 0 is success, -1 if not
          !
            type(connection), intent(in)                    :: conn
            character (len=*) , intent(in)                  :: parms
            character (len=*) , intent(in)                  :: destination
            integer , intent(in)                            :: year, month, day, hour, minute, second
            real*8 , intent(in)                             :: start_lat, stop_lat, step_lat
            real*8 , intent(in)                             :: start_lon, stop_lon, step_lon
            real*8 , intent(in)                             :: start_alt, stop_alt, step_alt
            character (len=*), optional, intent(in)         :: one_d_parms
            character (len=*), optional, intent(in)         :: one_d_values
            integer                                         :: mad_calculator

            ! local variables
            character (len=2048)     :: url
            character (len=1024)     :: args
            character (len=4096)     :: tmp_file
            character (len=2048)     :: cmd
            integer                  :: st_parms, st_values, ind_parms, ind_values

            ! build url from arguments
            url = trim(conn%cgi_url) // 'madCalculatorService.py?'

            write(args, 600), year, month, day, hour, minute, second
     600    format('year=',  I0, '&month=', I0, '&day=', I0, '&hour=', I0, '&min=', I0, '&sec=', I0)
            url = trim(url) // trim(args)

            write(args, 610), start_lat, stop_lat, step_lat
     610    format('&startLat=',  F0.3, '&endLat=',F0.3, '&stepLat=', F0.3)
            url = trim(url) // trim(args)

            write(args, 620), start_lon, stop_lon, step_lon
     620    format('&startLong=',  F0.3, '&endLong=',F0.3, '&stepLong=', F0.3)
            url = trim(url) // trim(args)

            write(args, 630), start_alt, stop_alt, step_alt
     630    format('&startAlt=',  F0.3, '&endAlt=',F0.3, '&stepAlt=', F0.3)
            url = trim(url) // trim(args)

            url = trim(url) // '&parms=' // trim(parms)

            ! code up one d parms and values
            if (present(one_d_parms) .and. present(one_d_values)) then
              st_parms = 1
              st_values = 1
              do
                ind_parms = index(one_d_parms(st_parms:), ',')
                ind_values = index(one_d_values(st_values:), ',')
                ! raise error if one is zero but not the other
                if ((ind_parms .eq. 0 .and. ind_values .ne. 0) .or. (ind_parms .ne. 0 .and. ind_values .eq. 0)) then
                  print*, 'Length of oneD parms not length of oneD values'
                  mad_calculator = -1
                  return
                end if
                if (ind_parms .eq. 0) then
                  write(args, 640), one_d_parms(st_parms:), one_d_values(st_values:)
                  url = trim(url) // trim(args)
                  exit
                else
                  write(args, 640), one_d_parms(st_parms:ind_parms-1), one_d_values(st_values:ind_values-1)
                  url = trim(url) // trim(args)
                  st_parms = ind_parms + 1
                  st_values = ind_values + 1
                end if

      640     format('&oneD=', A, ',', A)
              end do

            end if

            ! make sure there are no pluses
            url = trim(Replace_Text(url, '+',  '%2B'))

            tmp_file = get_url(url)
            cmd = 'cp ' // trim(tmp_file) // ' ' // trim(destination)
            call system(cmd)

            mad_calculator = 0

          end function mad_calculator



          function get_url(url)
          ! get_url uses wget to retrieve url, and store in in tmp file.
          ! returns name of temp file
          ! Private method used internally only
              character (len=2048)          :: get_url
              character (len=*), intent(in) :: url

              integer :: status
              character (len=512) :: tmp_file
              character (len=1024) :: cmd

              tmp_file = '/tmp/get_url_output.txt'
              cmd = 'wget -q -O ' // trim(tmp_file) // ' "' // trim(url) // '"'
              status = system(cmd)

              if (status .eq. 0) then
                  get_url = tmp_file
              else
                  get_url = 'Error - cmd failed: ' // cmd
              end if

          end function get_url


          !  The following methods are not meant to be called externally
          function get_basename(full_filename)
          ! get_basename returns the basename of full_filename
              character (len=*), intent(in) :: full_filename
              character (len=1024)          :: get_basename

              ! local variables
              integer                        :: slash_index

              slash_index = index(full_filename, '/', .true.)
              if (slash_index .eq. 0) then
                get_basename = full_filename
              else
                get_basename = full_filename(slash_index+1:)
              end if

          end function get_basename


          function compare_versions(ver1, ver2)
          ! compare_versions returns -1 if ver1 < ver2, 0 if equal,
          ! and 1 if ver1 > ver2. Reads three levels only
          ! Private method used internally only
              character (len=*), intent(in) :: ver1
              character (len=*), intent(in) :: ver2
              integer                       :: compare_versions

              integer :: i1, i2, left1, right1, left2, right2

              ! default
              compare_versions = 0

              ! read first numbers
              left1 = 1
              right1 = index(ver1(left1:), '.')
              if (right1 > 0) then
                read(ver1(left1:left1+right1-2), *) i1
              else
                read(ver1(left1:), *) i1
              end if
              left2 = 1
              right2 = index(ver2(left2:), '.')
              if (right2 > 0) then
                read(ver2(left2:left2+right2-2), *) i2
              else
                read(ver2(left2:), *) i2
              end if

              ! compare first numbers
              if (i1 < i2) then
                compare_versions = -1
                return
              else if (i1 > i2) then
                compare_versions = 1
                return
              end if

              ! check for more
              if ((right1 .eq. 0) .and. (right2 .eq. 0)) then
                return
              else if (right1 .eq. 0) then
                compare_versions = -1
                return
              else if (right2 .eq. 0) then
                compare_versions = 1
                return
              end if

              ! read second numbers
              left1 = left1 + right1
              right1 = index(ver1(left1:), '.')
              if (right1 > 0) then
                read(ver1(left1:left1+right1-2), *) i1
              else
                read(ver1(left1:), *) i1
              end if
              left2 = left2 + right2
              right2 = index(ver2(left2:), '.')
              if (right2 > 0) then
                read(ver2(left2:left2+right2-2), *) i2
              else
                read(ver2(left2:), *) i2
              end if

              ! compare second numbers
              if (i1 < i2) then
                compare_versions = -1
                return
              else if (i1 > i2) then
                compare_versions = 1
                return
              end if

              ! check for more
              if ((right1 .eq. 0) .and. (right2 .eq. 0)) then
                return
              else if (right1 .eq. 0) then
                compare_versions = -1
                return
              else if (right2 .eq. 0) then
                compare_versions = 1
                return
              end if

              ! read third numbers
              left1 = left1 + right1
              right1 = index(ver1(left1:), '.')
              if (right1 > 0) then
                read(ver1(left1:left1+right1-2), *) i1
              else
                read(ver1(left1:), *) i1
              end if
              left2 = left2 + right2
              right2 = index(ver2(left2:), '.')
              if (right2 > 0) then
                read(ver2(left2:left2+right2-2), *) i2
              else
                read(ver2(left2:), *) i2
              end if

              ! compare third numbers
              if (i1 < i2) then
                compare_versions = -1
                return
              else if (i1 > i2) then
                compare_versions = 1
                return
              end if

              ! all equal - return default

          end function compare_versions


          ! function taken from the internet to replace text (typically blanks with '+')
          FUNCTION Replace_Text (s,text,rep)  RESULT(outs)
            ! harcoded so that len of text must be 1
            CHARACTER(*)        :: s,text,rep
            CHARACTER(LEN(s)+100) :: outs     ! provide outs with extra 100 char len
            INTEGER             :: i, nt, in_len

            in_len = len(s)
            outs = s
            nt = 1
            DO
               i = INDEX(outs,text(:nt))
               IF (i == 0) EXIT
               if (i > in_len) exit
               outs = outs(:i-1) // trim(rep) // outs(i+nt:)
            END DO
          END FUNCTION Replace_Text





      end module madrigal_web
