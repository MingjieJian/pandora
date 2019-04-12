      subroutine SHIRE
     $(LU,LUG,FMULT,BMULT,LYM,KISLV,TPOP,COPAC,CSO,OPAC,CAPPA,SIGSTR,
     $ ALBD,T1,TR,TS1,TSR,HEAD,LINE,ISW,PLTID,NOPAC,N,GRAF)
C
C     Rudolf Loeser, 1978 Apr 08
C---- Sets up Absorber printout, for SHARON.
C     !DASH
      save
C     !DASH
      real*8 ALBD, BMULT, CAPPA, COPAC, CSO, FMULT, OPAC, SIGSTR, T1,
     $       TR, TS1, TSR
      integer ISW, KISLV, KNFRM, KONFRM, LU, LUG, N, NOPAC
      logical GRAF, LINE, LYM, ZALBD
      character HEAD*12, PLTID*1, TPOP*3
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 26),KNFRM)
C     !DASH
C     !EJECT
      external NAUGHTD, HAVGAN, MOVE1, KRAAL, CRAWL, POOBLE, HI, BYE
C
C               T1(N), TR(N), COPAC(Nopac,N), CSO(Nopac,N), ISW(Nopac),
      dimension T1(*), TR(*), COPAC(*),       CSO(*),       ISW(*),
C
C               OPAC(N), ALBD(N), TS1(N), TSR(N), SIGSTR(N), CAPPA(N)
     $          OPAC(*), ALBD(*), TS1(*), TSR(*), SIGSTR(*), CAPPA(*)
C
      dimension PLTID(4)
C
      call HI ('SHIRE')
C     !BEG
      GRAF = .false.
      if(LU.gt.0) then
        call HAVGAN   (OPAC, N, KNFRM, KONFRM)
        call MOVE1    (COPAC, (N*NOPAC), CSO)
        call NAUGHTD  (ALBD, 1, N, ZALBD)
C
        if(KONFRM.eq.1) then
          call POOBLE (N, NOPAC, HEAD, OPAC, FMULT, BMULT, CAPPA,
     $                 SIGSTR, ALBD, ZALBD, CSO, KONFRM, LU, LUG,
     $                 ISW, PLTID, T1, TR, LYM, TPOP, KISLV, LINE)
C
        else if(KONFRM.eq.2) then
          call KRAAL  (CSO, OPAC, FMULT, N, NOPAC, KONFRM)
          call CRAWL  (LYM, T1, TR, TS1, TSR, OPAC, FMULT, N)
          call POOBLE (N, NOPAC, HEAD, OPAC, FMULT, BMULT, CAPPA,
     $                 SIGSTR, ALBD, ZALBD, CSO, KONFRM, LU, LUG,
     $                 ISW, PLTID, TS1, TSR, LYM, TPOP, KISLV, LINE)
          GRAF = .true.
        end if
      end if
C     !END
      call BYE ('SHIRE')
C
      return
      end
