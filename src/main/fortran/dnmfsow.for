C

      SUBROUTINE DNMFSOw(maxsta, AVAIL ,numsta, IMCD)
c
c _________________________________________________________
c	Program Description
c
c       Dnmfsow; It is similar to dnmfso but it searches
c               all array elements, not just downstream
c               Used for wells since depletions are often
c               not downstream
c
      dimension avail(maxsta)
C
C-------------------------------------------------------------------
C
C------  FIND THE MINIMUM FLOW IN THE NETWORK
C
C-------------------------------------------------------------------
C
      IMCD=1
      
      DO ND=1,numsta
        IF(AVAIL(IMCD).GT.AVAIL(nd)) IMCD=nd
      end do
C
      RETURN
      END
