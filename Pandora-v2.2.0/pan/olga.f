      subroutine OLGA
     $(XNE,XNE0,W1,W2,N,SYM,CPR,CHI,ETA)
C
C     Rudolf Loeser, 1978 Aug 12
C---- Applies a correction factor to ETA, when it pertains to
C     a Population Update Ion, for GIZMO.
C
C     SYM is a population-ion symbol other than "H  ".
C     (This is version 2 of OLGA.)
C     !DASH
      save
C     !DASH
      real*8 CHI, CPR, ETA, ETA0, ETAC, RAT, W1, W2, XNE, XNE0
      integer I, KODE, N
      character SYM*3
C     !DASH
      external DALLIS, TAMARA, DIVIDE, HI, BYE
C
C               ETA(N), CPR(N), W1(N), W2(N), XNE(N), XNE0(N)
      dimension ETA(*), CPR(*), W1(*), W2(*), XNE(*), XNE0(*)
C
      call HI ('OLGA')
C     !BEG
      call DALLIS     (SYM, KODE)
C
      if(KODE.eq.1) then
        do 100 I = 1,N
          call TAMARA (XNE (I), W1(I), W2(I), CPR(I), CHI, ETAC)
          call TAMARA (XNE0(I), W1(I), W2(I), CPR(I), CHI, ETA0)
C
          call DIVIDE (ETAC, ETA0, RAT)
          ETA(I) = RAT*ETA(I)
  100   continue
      end if
C     !END
      call BYE ('OLGA')
C
      return
      end
