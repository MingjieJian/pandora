      subroutine GAZA
     $(TAU,N,XLO,XHI,ILO,IHI)
C
C     Rudolf Loeser, 1980 Dec 04
C---- Finds limits for TE-plots.
C     (This is version 2 of GAZA.)
C     !DASH
      save
C     !DASH
      real*8 TAU, XHI, XLO
      integer I, IHI, ILO, N
C     !DASH
      external NOTMORE, NOTLESS, HI, BYE
C
C               TAU(N)
      dimension TAU(*)
C
      call HI ('GAZA')
C     !BEG
      ILO = 2
      call NOTMORE (TAU,N,XLO,I)
      if(I.gt.0) then
        ILO = I
      end if
C
      IHI = N
      call NOTLESS (TAU,N,XHI,I)
      if(I.gt.0) then
        IHI = I
      end if
C     !END
      call BYE ('GAZA')
C
      return
      end
