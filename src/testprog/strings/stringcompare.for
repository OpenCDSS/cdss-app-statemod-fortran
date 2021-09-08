! Test program to evaluate gfortran compiler options
! Test simple initialization of variable with automatic variables.

      program testprog1

      implicit none

      ! Declare variables.
      character (len=80) string80
      character (len=80) string80spaces
      character (len=80) string80read
      character (len=32) string32
      character (len=32) string32spaces
      character (len=32) string32read

      character (len=80) stringinput

      string80 = 'A longer string                 x'
      string32 = 'A short string x'

      ! Call subroutines to do the comparison.

      call compare1(string80,string32)

      call compare2(string80,string32)

      ! Local string comparisons.

      ! Compare strings initialized to different values.
      if ( string80 .eq. string32 ) then
        write(*,*) 'main: Strings of different length are equal.'
      else
        write(*,*) 'main: Strings of different length are not equal.'
      endif

      string80spaces = 'ALFALFA'
      string32spaces = 'ALFALFA     '

      ! Compare strings that have extra spaces on end.
      if ( string80 .eq. string32 ) then
        write(*,*) 'main: Strings with extra spaces are equal.'
      else
        write(*,*) 'main: Strings with extra spaces are not equal.'
      endif

      ! Compare strings read from input.
      ! - won't be equal because strings have extra spaces on the ends
      stringinput = 'ALFALFA      ALFALFA'
      read(stringinput,'(a12,1x,a80)') string32read, string80read
      if ( string80read .eq. string32read ) then
        write(*,*) 'main: Strings read from input are equal'
        write(*,*) '  string32read=|', string32read, "|"
        write(*,*) '    len(string32read)=', len(string32read)
        write(*,*) '  string80read=|', string80read, "|"
        write(*,*) '    len(string80read)=', len(string80read)
      else
        write(*,*) 'main: Strings read from input are not equal.'
        write(*,*) '  string32read=|', string32read, "|"
        write(*,*) '    len(string32read)=', len(string32read)
        write(*,*) '  string80read=|', string80read, "|"
        write(*,*) '    len(string80read)=', len(string80read)
      endif

      string32read = trim(string32read)
      string80read = trim(string80read)
      if ( string80read .eq. string32read ) then
        write(*,*) 'main: Trimmed strings read from input are equal'
        write(*,*) '  string32read=|', string32read, "|"
        write(*,*) '  string80read=|', string80read, "|"
      else
        write(*,*)'main: Trimmed strings read from input are not equal.'
        write(*,*) '  string32read=|', string32read, "|"
        write(*,*) '  string80read=|', string80read, "|"
      endif

      end

      ! ==================================

      ! Version that uses hard-coded string sizes:
      ! - will not compile if declared with comments below

      subroutine compare1(string1, string2)

      ! The following does not compile.
      !character string1(80)
      !character string2(32)

      ! Either of the following compile:
      ! - however, see compare2 for preferred approach
      !character string2*80
      !character string2*32
      character (len=80) string1
      character (len=32) string2

      if ( string1 .eq. string2 ) then
        write(*,*) 'compare1: Strings are equal.'
      else
        write(*,*) 'compare1: Strings are not equal.'
      endif

      return
      end

      ! Version that uses variable string sizes:
      subroutine compare2(string1, string2)
      character (len=*) string1
      character (len=*) string2

      if ( string1 .eq. string2 ) then
        write(*,*) 'compare2: Strings are equal.'
      else
        write(*,*) 'compare2: Strings are not equal.'
      endif

      return
      end
