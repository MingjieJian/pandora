      subroutine ENRICO
     $(KOUNT,N,TAU,INT1,TAU1)
C
C     Rudolf Loeser, 1983 May 19
C---- Finds TAU closest to 1, and its index, for Shell Rays.
C     !DASH
      save
C     !DASH
      real*8 DMIN, ONE, TAU, TAU1, TMIN
      integer I, INT1, KOUNT, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  HELOT, HI, BYE
      intrinsic abs
C
C               TAU(KOUNT)
      dimension TAU(*)
C
      call HI ('ENRICO')
C     !BEG
      INT1 = 1
      TMIN = abs(TAU(1)-ONE)
C
      do 100 I = 2,KOUNT
        DMIN = abs(TAU(I)-ONE)
        if(DMIN.lt.TMIN) then
          INT1 = I
          TMIN = DMIN
        end if
  100 continue
C
      call HELOT  (N,INT1,TAU1,TAU)
C     !END
      call BYE ('ENRICO')
C
      return
      end
