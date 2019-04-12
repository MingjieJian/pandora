      subroutine TURKANA
     $(N,XN,XD,A,AU)
C
C     Rudolf Loeser, 1983 Dec 12
C---- Computes AU from A, based on the ratio (XN/XD).
C     (This is version 2 of TURKANA.)
C     !DASH
      save
C     !DASH
      real*8 A, AU, XD, XN
      integer N
C     !DASH
      external ARRDIV, ARRMUL, HI, BYE
C
      dimension XN(*), XD(*), A(*), AU(*)
C
      call HI ('TURKANA')
C     !BEG
      call ARRDIV (XN,XD,AU,N)
      call ARRMUL (A ,AU,AU,N)
C     !END
      call BYE ('TURKANA')
C
      return
      end
