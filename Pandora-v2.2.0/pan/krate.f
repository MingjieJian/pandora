      subroutine KRATE
     $(NVSB,N,VSB,VM,VXS,KVSB)
C
C     Rudolf Loeser, 1990 Apr 26
C---- Checks "Sobolev velocity" and aborts on error.
C     (This is version 2 of KRATE.)
C     !DASH
      save
C     !DASH
      real*8 VM, VSB, VXS
      integer KVSB, LUEO, N, NVSB
      logical VZERO
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external NAUGHTD, MESHED, VECOUT, ABORT, HI, BYE
C
C               VSB(N), VM(N), VXS(N)
      dimension VSB(*), VM(*), VXS(*)
C
C
      call HI ('KRATE')
C     !BEG
      call NAUGHTD  (VSB, 1, N, VZERO)
C
      if(VZERO) then
        call MESHED ('KRATE', 1)
        write (LUEO,100) NVSB,KVSB
  100   format(' ','Expansion velocity for Sobolev solution = 0.'//
     $         ' ','NVSB=',I2,5X,'KVSB=',I1)
        call VECOUT (LUEO, VSB, N, 'VSB')
        call VECOUT (LUEO, VXS, N, 'VXS')
        call VECOUT (LUEO, VM , N, 'VM' )
        call ABORT
      end if
C     !END
      call BYE ('KRATE')
C
      return
      end
