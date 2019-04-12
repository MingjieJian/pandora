      subroutine AGORA
     $(N,LG,XKL,ZAXA,ZAYA,SIGMAS,BHSNMS,F,XA,YA,OPAC,KODEXA,INDEX,
     $ DUMP)
C
C     Rudolf Loeser, 1982 Feb 01
C---- Computes XA and YA, for all look angles,
C     for angle-dependent Continuum Calculations.
C     On return, KODEXA = 1 if all abs(XA).le.0.5, = 0 otherwise.
C     (See also HIPPO.)
C     !DASH
      save
C     !DASH
      real*8 BHSNMS, F, OPAC, SIGMAS, XA, XKL, YA, ZAXA, ZAYA
      integer INDEX, KHLF, KODEXA, LG, M, N
      logical DMPM, DUMP
      character LABEL*100
C     !DASH
      external THASOS, YUKUM, HI, BYE
C
C               XKL(N,N,LG), BHSNMS(N), YA(N,N,LG), ZAYA(N), SIGMAS(N),
      dimension XKL(N,N,*),  BHSNMS(*), YA(N,N,*),  ZAYA(*), SIGMAS(N),
C
C               ZAXA(N), OPAC(N,N,LG), XA(N,N,LG)
     $          ZAXA(*), OPAC(N,N,*),  XA(N,N,*)
C
      call HI ('AGORA')
C     !BEG
      KODEXA = 1
C---- Loop over all angles
      do 100 M = 1,LG
        call YUKUM  (M, LG, INDEX, 3, N, LABEL, DUMP, DMPM)
        call THASOS (N, XKL(1,1,M), ZAXA, ZAYA, SIGMAS, BHSNMS, F,
     $               XA(1,1,M), YA(1,1,M), OPAC(1,1,M), KHLF, DMPM)
        if(KHLF.ne.(N*N)) then
          KODEXA = 0
        end if
  100 continue
C     !END
      call BYE ('AGORA')
C
      return
      end
