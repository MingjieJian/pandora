      subroutine RITA
     $(NO,N,IU,IL,BDJ,BDR,BDQ,BDS,RHOS,RHOJ,RHOW,BDIJ,RHO,WEIGHT,
     $ CHI,S,YBAR,ABDR,ARHO)
C
C     Rudolf Loeser, 2004 Mar 17
C---- Prints with analysis, for CROCUS.
C     !DASH
      save
C     !DASH
      real*8 ABDR, ARHO, BDIJ, BDIJIT, BDJ, BDQ, BDR, BDS, CHI, RHO,
     $       RHOJ, RHOS, RHOW, S, WEIGHT, YBAR
      integer I, IL, IU, N, NO
      character BTIT*4, RTIT*4
C---- TULLY       as of 2004 Mar 17
      integer     MBD,MRHO
      logical     KRHOJ,KRHOW,KRBDS,KRBDJ,KRBDR,KRBDQ
C
      common      /TULLY1/ MBD,MRHO
      common      /TULLY2/ KRHOJ,KRHOW,KRBDS,KRBDJ,KRBDR,KRBDQ
C     Intermediates for TULIP: RHO & RBD calculation.
C     .
C     !DASH
      external FLAX, BRAT, KRAKEN, SHIM, REKKAN, KANKER, HI, BYE
C
C               BDR(N,NL), ARHO(N,3), WEIGHT(N), BDIJ(N,NL), BDQ(N,NL),
      dimension BDR(*),    ARHO(N,*), WEIGHT(*), BDIJ(*),    BDQ(*),
C
C               YBAR(N), BDJ(N,NL), RHOS(N), RHO(N), BDS(N), ABDR(N,3),
     $          YBAR(*), BDJ(*),    RHOS(*), RHO(*), BDS(*), ABDR(N,*),
C
C               RHOJ(N), S(N), RHOW(N), CHI(N)
     $          RHOJ(*), S(*), RHOW(*), CHI(*)
C
      dimension BTIT(4), RTIT(4)
C
      call HI ('RITA')
C     !BEG
C---- Set up RBDs for printing
      call REKKAN   (N, IU, IL, MBD, BDJ, BDR, BDQ, BDS, BTIT, ABDR)
C---- Set up RHOs for printing
      call KANKER   (N, MRHO, RHOS, RHOJ, RHOW, RTIT, ARHO)
C---- Print transition heading
      call FLAX     (NO, IU, IL, BTIT, RTIT)
C---- Loop over all depths
      do 100 I = 1,N
        call BRAT   (I, IU, IL, BDIJ, BDIJIT)
        call KRAKEN (NO, I, ABDR(I,1), ABDR(I,2), ABDR(I,3), BDIJIT,
     $               WEIGHT(I), ARHO(I,1), ARHO(I,2), ARHO(I,3),
     $               RHO(I), CHI(I), S(I), YBAR(I))
        call SHIM   (I, 5, NO)
  100 continue
C     !END
      call BYE ('RITA')
C
      return
      end
