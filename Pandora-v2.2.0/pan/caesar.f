      subroutine CAESAR
     $(NO,IU,IL,Z,N,WTAB,WVL,K,FRR,MRR,SI,INT1S,TAU1S,EPCS,EPSS,
     $ EPCD,DI,INT1D,TAU1D,SF,SFC,DF,DFC,TF,ICE,R1N,DISK,TOT,KILROY,
     $ SII,DII)
C
C     Rudolf Loeser, 1981 Aug 26
C---- Prints eclipse calculation results.
C     !DASH
      save
C     !DASH
      real*8 DF, DFC, DI, DII, EPCD, EPCS, EPSS, FRR, R1N, SF, SFC, SI,
     $       SII, TAU1D, TAU1S, TF, WTAB, WVL, Z
      integer ICE, IL, INT1D, INT1S, IU, K, MRR, N, NO
      logical DISK, KILROY
      character REM*56, TOT*(*)
C     !DASH
      external  HILDE, VENI, VIDI, VICI, ODI, AMO, CREASE, ANOR, LINER,
     $          BOSO, KARTIR, CRASSUS, HI, BYE
C
C               WTAB(KM), FRR(MRR), SI(N,KM), INT1S(N,KM), TAU1S(N,KM),
      dimension WTAB(*),  FRR(*),   SI(*),    INT1S(*),    TAU1S(*),
C
C               EPCD(N), EPSS(N), TAU1D(MRR,KM), INT1D(MRR,KM), DF(KM),
     $          EPCD(*), EPSS(*), TAU1D(*),      INT1D(*),      DF(*),
C
C               EPCS(KM), SFC(N,KM), DFC(MRR,KM), DII(N,KM), SII(N,KM),
     $          EPCS(*),  SFC(*),    DFC(*),      DII(*),    SII(*),
C
C               TF(KM), DI(MRR,KM), SF(KM), Z(N)
     $          TF(*),  DI(*),      SF(*),  Z(*)
C     !EJECT
C
      call HI ('CAESAR')
C     !BEG
      if(KILROY) then
C----   Heading
        KILROY = .false.
        call HILDE  (NO,DISK,IU,IL,WVL,ICE,R1N,Z,N,FRR,MRR)
      end if
      if(DISK) then
C----   Disk intensity
        call LINER  (3,NO)
        call VIDI   (NO,WTAB,K,MRR,DI,EPCD,TOT)
      end if
C---- Shell intensity
      call LINER    (3,NO)
      call VENI     (NO,WTAB,K,N,SI,EPCS,EPSS,TOT)
      if(DISK) then
C----   Disk flux contributions
        call LINER  (3,NO)
        call ODI    (NO,WTAB,K,MRR,DFC,TOT)
      end IF
C---- Shell flux contributions
      call LINER    (3,NO)
      call AMO      (NO,WTAB,K,N,SFC,TOT)
C---- Flux profiles
      call LINER    (3,NO)
      call VICI     (NO,WTAB,K,SF,DF,TF,R1N,TOT)
      if(DISK) then
C----   Disk depths of formation
        call LINER  (3,NO)
        call ANOR   (NO,WTAB,K,MRR,INT1D,TAU1D,TOT)
      end if
C---- Shell depths of formation
      call LINER    (3,NO)
      call BOSO     (NO,WTAB,K,N,INT1S,TAU1S,TOT)
C----
      call CREASE   (REM)
      if(DISK) then
C----   Disk integrated ray intensities
        call LINER  (3,NO)
        call KARTIR (NO,WTAB,K,MRR,DII,TOT,REM)
      end if
C---- Shell integrated ray intensities
      call LINER    (3,NO)
      call CRASSUS  (NO,WTAB,K,N,SII,TOT,REM)
C     !END
      call BYE ('CAESAR')
C
      return
      end
