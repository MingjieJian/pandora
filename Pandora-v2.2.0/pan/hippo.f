      subroutine HIPPO
     $(N,NSHL,XKLSHL,MRR,XKLDSK,ZAXA,ZAYA,SIGMAS,BHSNMS,FMULT,XASHL,
     $ XADSK,YASHL,YADSK,OPACSHL,OPACDSK,KODEXA,INDSHL,INDDSK,DUMP)
C
C     Rudolf Loeser, 1982 Feb 05
C---- Computes XA and YA, both Shell and Disk parts,
C     for angle-dependent Continuum Calculations.
C     On return, KODEXA = 1 if all abs(XA).le.0.5, = 0 otherwise.
C     (See also AGORA.)
C     !DASH
      save
C     !DASH
      real*8 BHSNMS, FMULT, OPACDSK, OPACSHL, SIGMAS, XADSK, XASHL,
     $       XKLDSK, XKLSHL, YADSK, YASHL, ZAXA, ZAYA
      integer I, INDDSK, INDSHL, KHLF, KODEXA, M, MRR, N, NSHL
      logical DMPM, DUMP
      character LABEL*100
C     !DASH
      external THASOS, YUKUM, TATAR, HI, BYE
C
C               OPACSHL(N,N,NSHL), XASHL(N,N,NSHL), ZAYA(N), BHSNMS(N),
      dimension OPACSHL(N,N,*),    XASHL(N,N,*),    ZAYA(*), BHSNMS(*),
C
C               OPACDSK(N,N,MRR), ZAXA(N), SIGMAS(N), XKLSHL(N,N,NSHL),
     $          OPACDSK(N,N,*),   ZAXA(*), SIGMAS(*), XKLSHL(N,N,*),
C
C               YASHL(N,N,NSHL), YADSK(N,N,MRR), XKLDSK(N,N,MRR),
     $          YASHL(N,N,*),    YADSK(N,N,*),   XKLDSK(N,N,*),
C
C               XADSK(N,N,MRR)
     $          XADSK(N,N,*)
C     !EJECT
C
      call HI ('HIPPO')
C     !BEG
      KODEXA = 1
C---- Loop over all Shell rays
      I = 0
      do 100 M = 1,NSHL
        call TATAR  (I)
        call YUKUM  (M, NSHL, INDSHL, 1, I, LABEL, DUMP, DMPM)
        call THASOS (I, XKLSHL(1,1,M), ZAXA, ZAYA, SIGMAS, BHSNMS,
     $               FMULT, XASHL(1,1,M), YASHL(1,1,M), OPACSHL(1,1,M),
     $               KHLF, DMPM)
        if(KHLF.ne.(N*N)) then
          KODEXA = 0
        end if
  100 continue
C---- Loop over all Disk rays
      do 101 M = 1,MRR
        call YUKUM  (M, MRR, INDDSK, 2, N, LABEL, DUMP, DMPM)
        call THASOS (N, XKLDSK(1,1,M), ZAXA, ZAYA, SIGMAS, BHSNMS,
     $               FMULT, XADSK(1,1,M), YADSK(1,1,M), OPACDSK(1,1,M),
     $               KHLF, DMPM)
        if(KHLF.ne.(N*N)) then
          KODEXA = 0
        end if
  101 continue
C     !END
      call BYE ('HIPPO')
C
      return
      end
