      subroutine GAMBIT
     $(N,NSL,P,TE,XNU,GMI,NO)
C
C     Rudolf Loeser, 1978 Jan 26
C---- Computes and prints Gamma (GMI), for SETTUP.
C     !DASH
      save
C     !DASH
      real*8 GMI, P, TE, XNU
      integer J, N, NO, NSL
      logical PRNTZ
C     !DASH
      external  RYE, ABJECT, OMAR, HI, BYE
C
C               P(NSL), XNU(NSL), GMI(N,NSL), TE(N)
      dimension P(*),   XNU(*),   GMI(N,*),   TE(*)
C
      data PRNTZ /.false./
C
      call HI ('GAMBIT')
C     !BEG
      do 100 J = 1,NSL
        call RYE    (N,P(J),XNU(J),TE,GMI(1,J))
  100 continue
C
      if(NO.gt.0) then
        call ABJECT (NO)
        write (NO,101)
  101   format(' ','GM - statistical weights times Boltzmann factor')
        call OMAR   (NO,N,NSL,GMI,'Level ',PRNTZ)
      end if
C     !END
      call BYE ('GAMBIT')
C
      return
      end
