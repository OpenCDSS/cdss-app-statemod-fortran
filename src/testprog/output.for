! Subroutine to output test variables>

      subroutine output(int1, intarray1, real1, realarray1 )

      implicit none

      ! Declarations
      integer int1, intarray1(2)
      real real1, realarray1(2)

      ! Write the output to stdout
      write(*,*) 'int1=', int1
      write(*,*) 'intarray1(1)=', intarray1(1)
      write(*,*) 'intarray1(2)=', intarray1(2)
      write(*,*) 'real1=', real1
      write(*,*) 'realarray1(1)=', realarray1(1)
      write(*,*) 'realarray1(2)=', realarray1(2)

      return
      end
