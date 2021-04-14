! Test program to evaluate gfortran compiler options
! Test simple initialization of variable with automatic variables.

      program testprog1

      implicit none

      ! Declare variables.
      character progdesc*80
      character progname*32
      integer int1, intarray1(2)
      real real1, realarray1(2)

      ! Initialize variables.
      ! - do not initialize any of the numerical variables
      progname = 'testprog1'
      progdesc = 'No initialization.'

      ! Output initial values.
      write(*,*) progname, progdesc
      write(*,*) 'If no -finit-local-zero is used, expect garbage.'
      write(*,*) 'If -finit-local-zero is used, expect 0, 1, 2.'
      write(*,'(/,A)') 'Initial values:'
      call output(int1, intarray1, real1, realarray1)

      ! Increment values by 1 and output.
      call set1(int1, intarray1, real1, realarray1)
      write(*,'(/,A)') 'After calling set1:'
      call output(int1, intarray1, real1, realarray1)

      ! Increment values by 1 and output.
      call set1(int1, intarray1, real1, realarray1)
      write(*,'(/,A)') 'After calling set1:'
      call output(int1, intarray1, real1, realarray1)

      end
