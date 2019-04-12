      subroutine CONCH
     $(IU,IL,P,XNU,FROSC,A)
C
C     Rudolf Loeser, 1992 Feb 04
C---- Computes an estimate of the Einstein A coefficient for the
C     "forbidden" transition (IU/IL).
C     (This is version 2 of CONCH.)
C     !DASH
      save
C     !DASH
      real*8 A, DNU2, FAC, FROSC, P, XNU
      integer IL, IU
C     !DASH
      external HI, BYE
C
C               P(NSL), XNU(NSL)
      dimension P(*),   XNU(*)
C
      data FAC /7.42166D8/
C
      call HI ('CONCH')
C     !BEG
      DNU2 = (XNU(IU)-XNU(IL))**2
C
      A = (FAC*(P(IL)/P(IU))*DNU2)*FROSC
C     !END
      call BYE ('CONCH')
C
      return
      end
