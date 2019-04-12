      subroutine PORREX
     $(KOUNT,TAU,INT1,TAU1)
C
C     Rudolf Loeser, 1983 May 19
C---- Finds the TAU ivalue closest to 1, and its index, for Disk Rays.
C     !DASH
      save
C     !DASH
      real*8 ONE, TAU, TAU1, TMIN, ZMIN
      integer I, INT1, KOUNT
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  HI, BYE
      intrinsic abs
C
C               TAU(KOUNT)
      dimension TAU(*)
C
      call HI ('PORREX')
C     !BEG
      INT1 = 1
      TAU1 = TAU(1)
      TMIN = abs(TAU(1)-ONE)
C
      do 100 I = 2,KOUNT
        ZMIN = abs(TAU(I)-ONE)
        if(ZMIN.lt.TMIN) then
          INT1 = I
          TAU1 = TAU(I)
          TMIN = ZMIN
        end if
  100 continue
      INT1 = INT1+1
C     !END
      call BYE ('PORREX')
C
      return
      end
