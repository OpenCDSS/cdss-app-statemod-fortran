! Subroutine to set test variables

      subroutine set1(int1, intarray1, real1, realarray1 )

      implicit none

      ! Declarations
      integer int1, intarray1(2)
      real real1, realarray1(2)

      ! Increment all variables by 1
      int1 = int1 + 1
      intarray1(1) = intarray1(1) + 1
      intarray1(2) = intarray1(2) + 1

      real1 = real1 + 1.0
      realarray1(1) = realarray1(1) + 1.0
      realarray1(2) = realarray1(2) + 1.0

      return
      end
