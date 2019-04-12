      subroutine FLURRY
     $(N,MRR,K,NRAD,R1N,Z,FRR,RADI,DPTH,SI,DI,XINT)
C
C     Rudolf Loeser, 1992 Sep 23
C---- Makes data arrays, for TOTILA.
C     !DASH
      save
C     !DASH
      real*8 DI, DPTH, FRR, R1N, RADI, SI, XINT, Z
      integer K, MRR, N, NRAD
C     !DASH
      external FLOP, RULFRY, HI, BYE
C
C               FRR(MRR), RADI(NRAD), DI(MRR,KM), SI(N,KM), DPTH(NRAD),
      dimension FRR(*),   RADI(*),    DI(*),      SI(*),    DPTH(*),
C
C               Z(N), XINT(NRAD,KM)
     $          Z(*), XINT(*)
C
      call HI ('FLURRY')
C     !BEG
C---- Make NRAD, and also RADI, DPTH
      call FLOP   (N,MRR,R1N,Z,FRR,NRAD,RADI,DPTH)
C---- Set up consolidated intensity array
      call RULFRY (N,MRR,K,NRAD,SI,DI,XINT)
C     !END
      call BYE ('FLURRY')
C
      return
      end
