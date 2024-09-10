      program testMadrigalWeb
          use madrigal_web

          ! testMadrigalWeb is menat as an example of how to use madrigal_web fortran api
          ! Written by Bill Rideout
          ! $Id: testMadrigalWeb.f95 5690 2016-06-01 16:17:33Z brideout $

          ! mad_connection is the main object to be used by all calls after get_connection
          type(connection) mad_connection

          ! arguments needed by get_instruments call
          type(instrument), dimension(:), pointer :: inst_arr ! returns an allocated array of instrument structures
          integer :: inst_count ! returns the number of instruments returned

          ! arguments needed by get_sites call
          type(site), dimension(:), pointer :: site_arr ! returns an allocated array of site structures
          integer :: site_count ! returns the number of site returned

          ! arguments needed by get_experiments call
          type(experiment), dimension(:), pointer :: exp_arr ! returns an allocated array of experiment structures
          integer :: exp_count ! returns the number of experiments returned

          ! arguments needed by get_experiment_files call
          type(experiment_file), dimension(:), pointer :: file_arr ! returns an allocated array of experiment file structures
          integer :: file_count ! returns the number of experiment files returned

          ! arguments needed by get_cedar_parameters call
          type(cedar_parameter), dimension(:), pointer :: parm_arr ! returns an allocated array of cedar parameter structures
          integer :: parm_count ! returns the number of parameters returned

          ! other variables
          integer :: exp_id, result
          character (len=1024)  :: fullname

          ! to be done - declare arugments for all other api calls

          ! first call is always get_connection
          mad_connection = get_connection('http://madrigal3.haystack.mit.edu', "Bill Rideout", &
          "brideout@haystack.mit.edu", "MIT")
          !mad_connection = get_connection('http://isr.sri.com/madrigal/', "Bill Rideout", &
          !"brideout@haystack.mit.edu", "MIT")
          !mad_connection = get_connection('http://madrigal.haystack.mit.edu/madrigal/', "Bill Rideout", &
          !"brideout@haystack.mit.edu", "MIT")

          ! prove that first call succeeded
          print *, trim(mad_connection%user_fullname)
          print *, mad_connection%site_id
          print *, mad_connection%version


          ! get information on all available Madrigal instruments
          call get_instruments(mad_connection, inst_arr, inst_count)


          ! prove success by simply printing instrument data
          print *, 'inst_count is ', inst_count
          do i=1,3 ! shorten - could be inst_count
              print *, 'line ', i
              print *, trim(inst_arr(i)%name)
              print *, inst_arr(i)%code
              print *, trim(inst_arr(i)%mnemonic)
              print *, inst_arr(i)%latitude
              print *, inst_arr(i)%longitude
              print *, inst_arr(i)%altitude
              print *, trim(inst_arr(i)%category)
              print *, trim(inst_arr(i)%pi)
              print *, trim(inst_arr(i)%pi_email)
          end do

          deallocate(inst_arr) ! should be done only after you are done with it
          nullify(inst_arr)

          call get_sites(mad_connection, site_arr, site_count)
          ! prove success by simply printing site data
          print *, 'site_count is ', site_count
          do i=1,3 ! shorten - could be site_count
              print *, 'line ', i
              print *, site_arr(i)%site_id
              print *, trim(site_arr(i)%site_name)
              print *, trim(site_arr(i)%site_url)
              print *, trim(site_arr(i)%contact_name)
              print *, trim(site_arr(i)%contact_email)
          end do
          deallocate(site_arr) ! should be done only after you are done with it
          nullify(site_arr)

          call get_experiments(mad_connection, 30, 1998,1,1,0,0,0, 1998,2,1,0,0,0, .true., exp_arr, exp_count)
          ! do something with exp_arr
          print *, 'exp_count is ', exp_count
          do i=1,exp_count
              print *, exp_arr(i)%id, trim(exp_arr(i)%url), ' ', trim(exp_arr(i)%name)
              print *, exp_arr(i)%site_id, trim(exp_arr(i)%site_name), exp_arr(i)%kinst
              print *, trim(exp_arr(i)%inst_name)
              print *, exp_arr(i)%syear, exp_arr(i)%smonth, exp_arr(i)%sday
              print *, exp_arr(i)%shour, exp_arr(i)%sminute, exp_arr(i)%ssecond
              print *, exp_arr(i)%eyear, exp_arr(i)%emonth, exp_arr(i)%eday
              print *, exp_arr(i)%ehour, exp_arr(i)%eminute, exp_arr(i)%esecond
              print *, exp_arr(i)%is_local, trim(exp_arr(i)%madrigal_url)
              print *, trim(exp_arr(i)%pi), ' ', trim(exp_arr(i)%pi_email)
              print *, trim(exp_arr(i)%real_url), exp_arr(i)%uttimestamp, exp_arr(i)%access
              print *, trim(exp_arr(i)%version)
          end do
          exp_id =  exp_arr(1)%id
          deallocate(exp_arr) ! should be done only after you are done with it
          nullify(exp_arr)

          call get_experiment_files(mad_connection, exp_id, file_arr, file_count)
          ! do something with file_arr
          print *, 'file_count is ', file_count
          do i=1,file_count
              print *, trim(file_arr(i)%fullname), file_arr(i)%kindat
              print *, trim(file_arr(i)%kindat_desc)
              print *, file_arr(i)%category, trim(file_arr(i)%status)
              print *, file_arr(i)%permission, file_arr(i)%exp_id
          end do
          fullname = file_arr(1)%fullname
          deallocate(file_arr) ! should be done only after you are done with it
          nullify(file_arr)


          call get_cedar_parameters(mad_connection, fullname, parm_arr, parm_count)
          ! do something with parm_arr
          print *, 'parm_count is ', parm_count
          do i=1,3 ! parm_count (to shorten output)
              print *, trim(parm_arr(i)%mnemonic), ' ', trim(parm_arr(i)%description)
              print *, parm_arr(i)%is_error, trim(parm_arr(i)%units)
              print *, parm_arr(i)%is_measured, trim(parm_arr(i)%category)
          end do
          deallocate(parm_arr) ! should be done only after you are done with it
          nullify(parm_arr)

          ! download_file
          print*, 'about to call download file'
          result = download_file(mad_connection, fullname, '/tmp/junk.h5', 'Bill Rideout', &
            'brideout@haystack.mit.edu', 'MIT Haystack', 'hdf5')
          print*, 'download_file complete'

          ! isprint
          print*, 'about to call isprint'
          result = isprint(mad_connection, fullname, 'gdalt,elm,azm,nel', 'filter=recno,,5', '/tmp/junk.h5', &
            'Bill Rideout', 'brideout@haystack.mit.edu', 'MIT Haystack')
          print*, 'isprint complete'

          ! mad_calculator
          print*, 'about to call mad_calculator'
          result = mad_calculator(mad_connection, 'kp,ap3', '/tmp/junk.txt', 2001, 1, 1, 13, 0, 0,   &
              20D+0, 60D+0, 5D+0, -90D+0, -60D+0, 5D+0, 100D+0, 1000D+0, 100D+0, 'ph+,elm', '0.03,-160')



          ! test of private methods - users should not need to call these methods

          ! test of compare_versions
          print*, 'comparing 3.0 and 2.6'
          print*, compare_versions('3.0', '2.6')
          print*, 'comparing 2.6 and 3.0'
          print*, compare_versions('2.6', '3.0')
          print*, 'comparing 3.1 and 3.0'
          print*, compare_versions('3.1', '3.0')
          print*, 'comparing 3.0 and 3.1'
          print*, compare_versions('3.0', '3.1')
          print*, 'comparing 3 and 3.1'
          print*, compare_versions('3', '3.1')
          print*, 'comparing 3 and 2.6'
          print*, compare_versions('3', '2.6')
          print*, 'comparing 3.0 and 3.0'
          print*, compare_versions('3.0', '3.0')
          print*, 'comparing 3.0.1 and 3.0'
          print*, compare_versions('3.0.1', '3.0')
          print*, 'comparing 3.0 and 3.0.1'
          print*, compare_versions('3.0', '3.0.1')
          print*, 'comparing 3.0.1 and 3.0.1'
          print*, compare_versions('3.0.1', '3.0.1')



      end program testMadrigalWeb
