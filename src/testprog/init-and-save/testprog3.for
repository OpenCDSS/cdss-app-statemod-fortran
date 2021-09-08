! Test program to evaluate gfortran compiler options.
! Test simple initialization of variable with automatic variables.
! Values are set to local variables in subroutine.

      program testprog1

      implicit none

      ! Declare variables.
      character progdesc*80
      character progname*32
      integer int1, intarray1(2)
      real real1, realarray1(2)

      ! Initialize variables.
      ! - do not initialize any of the numerical variables
      progname = 'testprog3'
      progdesc = 'No initialization, variables set in called routine.'

      ! Output initial values.
      write(*,*) progname, progdesc
      write(*,*) 'If no -finit-local-zero is used, expect garbage.'
      write(*,*) 'If -finit-local-zero is used, expect 0, 1, 1.'
      write(*,*) 'If -fno-automatic -finit-local-zero is used,'
      write(*,*) '  expect 0, 1, 2.'
      write(*,'(/,A)') 'Initial values:'
      call output(int1, intarray1, real1, realarray1)

      ! Increment values by 1 and output.
      call set_using_local(int1, intarray1, real1, realarray1)
      write(*,'(/,A)') 'After calling set1:'
      call output(int1, intarray1, real1, realarray1)

      ! Increment values by 1 and output.
      call set_using_local(int1, intarray1, real1, realarray1)
      write(*,'(/,A)') 'After calling set1:'
      call output(int1, intarray1, real1, realarray1)

      end
