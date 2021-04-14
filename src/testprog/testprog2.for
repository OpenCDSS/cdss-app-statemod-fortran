! Test program to evaluate gfortran compiler options
! Test simple initialization of variable with automatic variables.
! Use common block for variables

      program testprog1

      implicit none

      ! Common block has the data
      include 'common1.inc'

      ! Declarations
      ! - do not initialize any of the numerical variables in the common block
      character progname*32
      character progdesc*80

      ! Initialize variables.
      progname = 'testprog2'
      progdesc = 'No initialization with common block.'

      ! Output initial values.
      write(*,*) progname, progdesc
      write(*,*) 'If no -finit-local-zero is used, expect 0, 1, 2'
      write(*,*) '  because common block is always initialized.'
      write(*,*) 'If -finit-local-zero is used, also expect 0, 1, 2.'
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
