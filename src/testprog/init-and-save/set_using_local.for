! Subroutine to set test variables.
! Local variables are maintained.

      subroutine set_using_local(int1, intarray1, real1, realarray1 )

      implicit none

      ! Declarations for passed in arguments.
      integer int1, intarray1(2)
      real real1, realarray1(2)

      ! Declarations for local variables.
      ! - do not initialize
      integer local_int1, local_intarray1(2)
      real local_real1, local_realarray1(2)

      ! Increment all local variables by 1
      ! - if not initialized may have garbage
      ! - if -finit-local-zero should have zeros assigned after declaration
      !   - if only -no-automatic is used the output will always be 1.0
      ! - if -no-automatic is used then the local variables will increment for each call
      local_int1 = local_int1 + 1
      local_intarray1(1) = local_intarray1(1) + 1
      local_intarray1(2) = local_intarray1(2) + 1

      ! This should always result in 1.0 if the initial values are zero.
      local_real1 = local_real1 + 1.0
      local_realarray1(1) = local_realarray1(1) + 1.0
      local_realarray1(2) = local_realarray1(2) + 1.0

      ! Assign returned values to local values
      int1 = local_int1
      intarray1(1) = local_intarray1(1)
      intarray1(2) = local_intarray1(2)

      real1 = local_real1
      realarray1(1) = local_realarray1(1)
      realarray1(2) = local_realarray1(2)

      return
      end
