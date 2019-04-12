      subroutine BATTI
     $(KK,N,XJBAR,EP,BS,S)
C
C     Rudolf Loeser, 2004 Mar 30
C           revised, 2004 May 07
C
C---- Computes updated line source function for the DIRECT solution.
C     !DASH
      save
C     !DASH
      real*8 BS, EP, ONE, S, XDEN, XJBAR, XNUM
      integer I, KK, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIVIDE, HI, BYE
C
C               XJBAR(N), EP(N), BS(N), S(N)
      dimension XJBAR(*), EP(*), BS(*), S(*)
C
      call HI ('BATTI')
C     !BEG
      do 100 I = KK,N
        XNUM = XJBAR(I)+EP(I)*BS(I)
        XDEN = ONE+EP(I)
        call DIVIDE (XNUM, XDEN, S(I))
  100 continue
C     !END
      call BYE ('BATTI')
C
      return
      end
