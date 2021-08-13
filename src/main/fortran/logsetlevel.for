c logsetlevel - set log level variable
c_________________________________________________________________NoticeStart_
c StateMod Water Allocation Model
c StateMod is a part of Colorado's Decision Support Systems (CDSS)
c Copyright (C) 1994-2021 Colorado Department of Natural Resources
c
c StateMod is free software:  you can redistribute it and/or modify
c     it under the terms of the GNU General Public License as published by
c     the Free Software Foundation, either version 3 of the License, or
c     (at your option) any later version.
c
c StateMod is distributed in the hope that it will be useful,
c     but WITHOUT ANY WARRANTY; without even the implied warranty of
c     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c     GNU General Public License for more details.
c
c     You should have received a copy of the GNU General Public License
c     along with StateMod.  If not, see <https://www.gnu.org/licenses/>.
c_________________________________________________________________NoticeEnd___

      subroutine logsetlevel(loginfo, ierror)

      include 'common.inc'

      ! Subroutine parameters.
      character(len=*) loginfo ! Log level to set, for example 'ioutp=1', can be any case.
      integer nlog             ! Unit number of log file.
      integer ierror           ! Error code to return, 0=success, 1=error

      ! Local variables.
      integer ipos                 ! Used to parse the string.
      integer loglevel             ! Logging level to set.
      character(len=16) logvar     ! Logging variable.
      character(len=8) cloglevel   ! Logging level as a string.
      logical logvarSet            ! Indicates that log variable was set.

      ! For example, the command line option is:  --loginfo=IOUTP=1
      ! loginfo passed into this routine should be syntax:
      !   'IOUTP=1'

      logvarSet = .TRUE. ! Indicates that log variable was set (will be set to .FALSE. if not handled)

      ! Trim whitespace.
      loginfo=trim(loginfo)

      ! Split the string using equal sign:
      ! - convert the variable to uppercase for comparisons below
      ! - the level must be an integer
      ipos = index(loginfo, '=')
      if ( ipos == 0 ) then
        write(nlog,*)
     1  'No logging level set in:  ', loginfo
        ierror=1
        return
      endif
      logvar = trim(loginfo(1:(ipos-1)))
      ! Log level starts after the equal sign.
      cloglevel = trim(loginfo((ipos + 1):len_trim(loginfo)))
      ! Convert the logging variable to uppercase so it can be checked below.
      call AdjCase(nlog, logvar, logvar, len(logvar), 2)

      ! Convert the character log level to an integer.
      read(cloglevel,*,iostat=ios) loglevel
      if ( ios .ne. 0 ) then
        write(nlog,*)
     1  'Logging level for ', logvar, ' is not an integer: ', cloglevel
        ierror=1
        return
      endif

      ! Set a logging common block variables to the requested level.
      ! This is brute force because logging is not very sophisticated.
      ! TODO smalers 2021-07-31 this is only enabled for a few routines
      ! such as bomsec.for logging variables, to help with troubleshooting.
      ! Print a log message at the end indicating that a variable is being set.
      if ( logvar .eq. 'IOUT01' ) then
        log_IOUT01=loglevel
      else if ( logvar .eq. 'IOUT02' ) then
        log_IOUT02=loglevel
      else if ( logvar .eq. 'IOUT03' ) then
        log_IOUT03=loglevel
      else if ( logvar .eq. 'IOUT04' ) then
        log_IOUT04=loglevel
      else if ( logvar .eq. 'IOUT05' ) then
        log_IOUT05=loglevel
      else if ( logvar .eq. 'IOUT06' ) then
        log_IOUT06=loglevel
      else if (logvar .eq. 'IOUT6' .or. logvar .eq. 'IOUT06') then
        ! Note no leading zero in routines.
        log_IOUT6=loglevel
      else if ( logvar .eq. 'IOUT07' ) then
        log_IOUT07=loglevel
      else if (logvar .eq. 'IOUT8' .or. logvar .eq. 'IOUT08') then
        ! Note no leading zero in routines.
        log_IOUT8=loglevel
      else if ( logvar .eq. 'IOUT09' ) then
        log_IOUT09=loglevel
      else if ( logvar .eq. 'IOUT10' ) then
        log_IOUT10=loglevel
      else if ( logvar .eq. 'IOUT11' ) then
        log_IOUT11=loglevel
      else if ( logvar .eq. 'IOUT12' ) then
        log_IOUT12=loglevel
      else if ( logvar .eq. 'IOUT13' ) then
        log_IOUT13=loglevel
      else if ( logvar .eq. 'IOUT14' ) then
        log_IOUT14=loglevel
      else if ( logvar .eq. 'IOUT15' ) then
        log_IOUT15=loglevel
      else if ( logvar .eq. 'IOUT16' ) then
        log_IOUT16=loglevel
      else if ( logvar .eq. 'IOUT17' ) then
        log_IOUT17=loglevel
      else if ( logvar .eq. 'IOUT18' ) then
        log_IOUT18=loglevel
      else if ( logvar .eq. 'IOUT19' ) then
        log_IOUT19=loglevel
      else if ( logvar .eq. 'IOUT20' ) then
        log_IOUT20=loglevel
      else if ( logvar .eq. 'IOUT21' ) then
        log_IOUT21=loglevel
      else if ( logvar .eq. 'IOUT22' ) then
        log_IOUT22=loglevel
      else if ( logvar .eq. 'IOUT23' ) then
        log_IOUT23=loglevel
      else if ( logvar .eq. 'IOUT24' ) then
        log_IOUT24=loglevel
      else if ( logvar .eq. 'IOUT25' ) then
        log_IOUT25=loglevel
      else if ( logvar .eq. 'IOUT26' ) then
        log_IOUT26=loglevel
      else if ( logvar .eq. 'IOUT27' ) then
        log_IOUT27=loglevel
      else if ( logvar .eq. 'IOUT28' ) then
        log_IOUT28=loglevel
      else if ( logvar .eq. 'IOUT29' ) then
        log_IOUT29=loglevel
      else if ( logvar .eq. 'IOUT30' ) then
        log_IOUT30=loglevel
      else if ( logvar .eq. 'IOUT31' ) then
        log_IOUT31=loglevel
      else if ( logvar .eq. 'IOUT32' ) then
        log_IOUT32=loglevel
      else if ( logvar .eq. 'IOUT33' ) then
        log_IOUT33=loglevel
      else if ( logvar .eq. 'IOUT34' ) then
        log_IOUT34=loglevel
      else if ( logvar .eq. 'IOUT35' ) then
        log_IOUT35=loglevel
      else if ( logvar .eq. 'IOUT36' ) then
        log_IOUT36=loglevel
      else if ( logvar .eq. 'IOUT37' ) then
        log_IOUT37=loglevel
      else if ( logvar .eq. 'IOUT38' ) then
        log_IOUT38=loglevel
      else if ( logvar .eq. 'IOUT39' ) then
        log_IOUT39=loglevel
      else if ( logvar .eq. 'IOUT40' ) then
        log_IOUT40=loglevel
      else if ( logvar .eq. 'IOUT41' ) then
        log_IOUT41=loglevel
      else if ( logvar .eq. 'IOUT42' ) then
        log_IOUT42=loglevel
      else if ( logvar .eq. 'IOUT43' ) then
        log_IOUT43=loglevel
      else if ( logvar .eq. 'IOUT45' ) then
        log_IOUT45=loglevel
      else if ( logvar .eq. 'IOUT45X' ) then
        log_IOUT45X=loglevel
      else if ( logvar .eq. 'IOUT46' ) then
        log_IOUT46=loglevel
      else if ( logvar .eq. 'IOUT47' ) then
        log_IOUT47=loglevel
      else if ( logvar .eq. 'IOUT48' ) then
        log_IOUT48=loglevel
      else if ( logvar .eq. 'IOUT49' ) then
        log_IOUT49=loglevel
      else if ( logvar .eq. 'IOUT50' ) then
        log_IOUT50=loglevel
      else if ( logvar .eq. 'IOUT51' ) then
        log_IOUT51=loglevel
      else if ( logvar .eq. 'IOUT52' ) then
        log_IOUT52=loglevel
      else if ( logvar .eq. 'IOUT54' ) then
        log_IOUT54=loglevel
      else if ( logvar .eq. 'IOUT' ) then
        log_IOUT=loglevel
      else if ( logvar .eq. 'IOUTA' ) then
        log_IOUTA=loglevel
      else if ( logvar .eq. 'IOUTADJ' ) then
        log_IOUTADJ=loglevel
      else if ( logvar .eq. 'IOUTB' ) then
        log_IOUTB=loglevel
      else if ( logvar .eq. 'IOUTC' ) then
        log_IOUTC=loglevel
      else if ( logvar .eq. 'IOUTCR' ) then
        log_IOUTCR=loglevel
      else if ( logvar .eq. 'IOUTCS' ) then
        log_IOUTCS=loglevel
      else if ( logvar .eq. 'IOUTCU' ) then
        log_IOUTCU=loglevel
      else if ( logvar .eq. 'IOUTCX' ) then
        log_IOUTCX=loglevel
      else if ( logvar .eq. 'IOUTD' ) then
        log_IOUTD=loglevel
      else if ( logvar .eq. 'IOUTE' ) then
        log_IOUTE=loglevel
      else if ( logvar .eq. 'IOUTEF' ) then
        log_IOUTEF=loglevel
      else if ( logvar .eq. 'IOUTEV' ) then
        log_IOUTEV=loglevel
      else if ( logvar .eq. 'IOUTF' ) then
        log_IOUTF=loglevel
      else if ( logvar .eq. 'IOUTG1' ) then
        log_IOUTG1=loglevel
      else if ( logvar .eq. 'IOUTG' ) then
        log_IOUTG=loglevel
      else if ( logvar .eq. 'IOUTGVC' ) then
        log_IOUTGVC=loglevel
      else if ( logvar .eq. 'IOUTGX' ) then
        log_IOUTGX=loglevel
      else if ( logvar .eq. 'IOUTHG1' ) then
        log_IOUTHG1=loglevel
      else if ( logvar .eq. 'IOUTHG' ) then
        log_IOUTHG=loglevel
      else if ( logvar .eq. 'IOUTI' ) then
        log_IOUTI=loglevel
      else if ( logvar .eq. 'IOUTIN' ) then
        log_IOUTIN=loglevel
      else if ( logvar .eq. 'IOUTIR' ) then
        log_IOUTIR=loglevel
      else if ( logvar .eq. 'IOUTIW' ) then
        log_IOUTIW=loglevel
      else if ( logvar .eq. 'IOUTJM' ) then
        log_IOUTJM=loglevel
      else if ( logvar .eq. 'IOUTL' ) then
        log_IOUTL=loglevel
      else if ( logvar .eq. 'IOUTLIM' ) then
        log_IOUTLIM=loglevel
      else if ( logvar .eq. 'IOUTN' ) then
        log_IOUTN=loglevel
      else if ( logvar .eq. 'IOUTNG' ) then
        log_IOUTNG=loglevel
      else if ( logvar .eq. 'IOUTOUT' ) then
        log_IOUTOUT=loglevel
      else if ( logvar .eq. 'IOUTP' ) then
        log_IOUTP=loglevel
      else if ( logvar .eq. 'IOUTP1' ) then
        log_IOUTP1=loglevel
      else if ( logvar .eq. 'IOUTPPT' ) then
        log_IOUTPPT=loglevel
      else if ( logvar .eq. 'IOUTPRF' ) then
        log_IOUTPRF=loglevel
      else if ( logvar .eq. 'IOUTPU' ) then
        log_IOUTPU=loglevel
      else if ( logvar .eq. 'IOUTPUC' ) then
        log_IOUTPUC=loglevel
      else if ( logvar .eq. 'IOUTQ' ) then
        log_IOUTQ=loglevel
      else if ( logvar .eq. 'IOUTR' ) then
        log_IOUTR=loglevel
      else if ( logvar .eq. 'IOUTRE' ) then
        log_IOUTRE=loglevel
      else if ( logvar .eq. 'IOUTRE2' ) then
        log_IOUTRE2=loglevel
      else if ( logvar .eq. 'IOUTREP' ) then
        log_IOUTREP=loglevel
      else if ( logvar .eq. 'IOUTRF' ) then
        log_IOUTRF=loglevel
      else if ( logvar .eq. 'IOUTRGF' ) then
        log_IOUTRGF=loglevel
      else if ( logvar .eq. 'IOUTRGS' ) then
        log_IOUTRGS=loglevel
      else if ( logvar .eq. 'IOUTRO' ) then
        log_IOUTRO=loglevel
      else if ( logvar .eq. 'IOUTRTN' ) then
        log_IOUTRTN=loglevel
      else if ( logvar .eq. 'IOUTS' ) then
        log_IOUTS=loglevel
      else if ( logvar .eq. 'IOUTSEP' ) then
        log_IOUTSEP=loglevel
      else if ( logvar .eq. 'IOUTSM' ) then
        log_IOUTSM=loglevel
      else if ( logvar .eq. 'IOUTSO' ) then
        log_IOUTSO=loglevel
      else if ( logvar .eq. 'IOUTSP' ) then
        log_IOUTSP=loglevel
      else if ( logvar .eq. 'IOUTT' ) then
        log_IOUTT=loglevel
      else if ( logvar .eq. 'IOUTTAR' ) then
        log_IOUTTAR=loglevel
      else if ( logvar .eq. 'IOUTURM' ) then
        log_IOUTURM=loglevel
      else if ( logvar .eq. 'IOUTW' ) then
        log_IOUTW=loglevel
      else if ( logvar .eq. 'IOUTW1' ) then
        log_IOUTW1=loglevel
      else if ( logvar .eq. 'IOUTWR' ) then
        log_IOUTWR=loglevel
      ! TODO smalers 2021-07-31 evaluate how to handle the following, which is used in several routines:
      ! - sometimes is an array, sometimes not?
      !log_IOUTX(IX)=loglevel
      else if ( logvar .eq. 'IOUTX' ) then
        log_IOUTX=loglevel
      else if ( logvar .eq. 'IOUTY' ) then
        log_IOUTY=loglevel
      else if ( logvar .eq. 'IOUTZ' ) then
        log_IOUTZ=loglevel
      else
        write(nlog,*) '--log= variable is not handled: ', logvar
        ! Indicate that log variable was not set:
        ! - the warning was printed above so don't print message below about being set
        logvarSet = .FALSE.
      endif

      ! Print a log message indicating which variable was set:
      ! - TODO smalers 2021-07-31 need to fix the formatting so extra spaces are not included
      if ( logvarSet .eqv. .TRUE. ) then
        write(nlog,*) '     Set log_', logvar, '=', loglevel
      endif

      return
      end
