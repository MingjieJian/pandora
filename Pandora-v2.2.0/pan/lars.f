      subroutine LARS
     $(X,W,N,P5,TAU5,IMG)
C
C     Rudolf Loeser, 1994 May 17
C---- Computes continuum optical depth at 5000 Angstroms, for DEAD.
C     (This is version 2 of LARS.)
C     !DASH
      save
C     !DASH
      real*8 P5, TAU5, W, X
      integer IMG, N, jummy
      logical lummy1, lummy2
      character LABEL*100
C     !DASH
      external DISMAL, HI, BYE
C
      dimension X(*), W(*)
C
C               P5(N), TAU5(N), IMG(N)
      dimension P5(*), TAU5(*), IMG(*)
C
      data LABEL /'Tau500 for Z-from-TAUKIN'/
C
      call HI ('LARS')
C     !BEG
C
      call DISMAL (X,W,1,N,P5,TAU5,LABEL,jummy,lummy1,lummy2,IMG)
C
C     !END
      call BYE ('LARS')
C
      return
      end
