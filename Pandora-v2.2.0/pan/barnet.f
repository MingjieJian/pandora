      subroutine BARNET
     $(N,VM,VMI)
C
C     Rudolf Loeser, 1998 Feb 13
C---- Checks whether the table VMI (the mass motion velocity as used
C     in the diffusion calculation) differs from the VM table
C     (the PANDORA-wide values of mass motion velocity); differences
C     can arise if RHEAB was recomputed.
C     If the tables are different, then
C        1) update VM;
C        2) set signal for the Continuum Calculations.
C     !DASH
      save
C     !DASH
      real*8 DELTA, VM, VMI
      integer KODE, N
C     !DASH
      external ARISOD, MOVE1, WENDY, HI, BYE
C
C               VM(N), VMI(N)
      dimension VM(*), VMI(*)
C
      data DELTA /1.D-13/
C
      call HI ('BARNET')
C     !BEG
      call ARISOD  (VM,N,VMI,N,DELTA,KODE)
      if(KODE.eq.0) then
        call MOVE1 (VMI,N,VM)
        call WENDY (VM,1,N,39,'BARNET')
      end if
C     !END
      call BYE ('BARNET')
C
      return
      end
