      subroutine DEVIL
     $(N,K,FMULT,T1,TR,CAPPAR,SIGMA,BHSR,XJNU,XKKA,XKKB,XLB,SP,OPAC,
     $ XLM,IADRS,DUMP)
C
C     Rudolf Loeser, 1979 Nov 30
C---- Retrieves and processes data from the K'th Continuum Data Block,
C     for TEUFEL.
C     !DASH
      save
C     !DASH
      real*8 BHSR, CAPABR, CAPPAR, FMULT, FSIGMA, OPAC, SIGMA, SP, T1,
     $       TR, XJNU, XKKA, XKKB, XLB, XLM
      integer I, IADRS, K, N
      logical DUMP
C     !DASH
      external VOLE, DIVIDE, HI, BYE
C
C               XKKA(N), XKKB(N), CAPPAR(N), SIGMA(N), BHSR(N), XLB(N),
      dimension XKKA(*), XKKB(*), CAPPAR(*), SIGMA(*), BHSR(*), XLB(*),
C
C               T1(N), TR(N), XJNU(N), SP(N), OPAC(N)
     $          T1(*), TR(*), XJNU(*), SP(*), OPAC(*)
C
      call HI ('DEVIL')
C     !BEG
      do 100 I = 1,N
        CAPABR  = FMULT*(CAPPAR(I)+TR(I))
        FSIGMA  = FMULT*SIGMA(I)
C
        XKKA(I) = FMULT*T1(I)
        XKKB(I) = CAPABR+FSIGMA
        OPAC(I) = (XKKA(I)+XKKB(I))
C
        call DIVIDE (XKKB(I), OPAC(I), XLB(I))
        call DIVIDE ((CAPABR*BHSR(I)+FSIGMA*XJNU(I)), XKKB(I), SP(I))
  100 continue
C
      call VOLE     (XKKA, N, 'XKKA', K, XLM, IADRS, DUMP)
      call VOLE     (XKKB, N, 'XKKB', K, XLM, IADRS, DUMP)
      call VOLE     (XLB,  N, 'XLB' , K, XLM, IADRS, DUMP)
C     !END
      call BYE ('DEVIL')
C
      return
      end
