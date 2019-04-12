      subroutine SALOME
     $(N,XNE,SA,GMI,BDI,PKS,ESG,QOUT,NO)
C
C     Rudolf Loeser, 2001 Dec 12
C---- Computes QOUT, for K-Shell ionization calculations.
C     (This is version 4 of SALOME.)
C     !DASH
      save
C     !DASH
      real*8 BDI, ESG, GMI, PKS, QOUT, SA, XNE
      integer I, N, NO
      logical PRINT
C     !DASH
      external ABJECT, LINER, URBINO, ARRMUL, HI, BYE
C
C               GMI(N,NSL), BDI(N,NL), XNE(N), PKS(N), QOUT(N), SA(N),
      dimension GMI(*),     BDI(N,*),  XNE(*), PKS(*), QOUT(*), SA(*),
C
C               ESG(N)
     $          ESG(*)
C
      call HI ('SALOME')
C     !BEG
      PRINT = NO.gt.0
      if(PRINT) then
        call ABJECT (NO)
        write (NO,100)
  100   format(' ','QOUT, for K-Shell ionization calculations.'//
     $         ' ',18X,'PKS',13X,'ESG',13X,'BD1',12X,'QOUT')
        call LINER  (1,NO)
      end if
C
      call URBINO   (N,1,XNE,SA,GMI,ESG)
      call ARRMUL   (ESG,BDI(1,1),QOUT,N)
      call ARRMUL   (QOUT,PKS,QOUT,N)
C
      if(PRINT) then
        write (NO,101) (I,PKS(I),ESG(I),BDI(I,1),QOUT(I),I=1,N)
  101   format(5(' ',I5,1P4E16.8/))
      end if
C     !END
      call BYE ('SALOME')
C
      return
      end
