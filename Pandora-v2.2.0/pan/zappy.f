      subroutine ZAPPY
     $(N,Z,TKIN,HNEU,HND,XNE,PGS,PTU,PTO,TAU5000,GMASS,TE,VT,VM,DGM,
     $ PMG,HNDO,XNEO,GR,HP,HNDNW,XNENW,GD,T5,F,RIND,HELABD,XCBL,
     $ NTITER,NEITER,MEITER,NHITER,KASE,NLH,LIMP,XNP,XNH,ISWA,VEC,
     $ VET,IPTO,PEX,EMN,EF,XK,DD,EM,FEM)
C
C     Rudolf Loeser, 1980 Jul 22
C---- Prints, for ZIPPY.
C     (This is version 2 of ZAPPY.)
C     !DASH
      save
C     !DASH
      real*8 CGR, DD, DGM, EF, EM, EMN, F, FEM, GD, GMASS, GR, HEL,
     $       HELABD, HND, HNDNW, HNDO, HNEU, HP, HSEC, HTAU, PEX, PGS,
     $       PMG, PTO, PTU, RIND, T5, TAU5000, TE, TKIN, VEC, VET, VM,
     $       VT, WVL, XCBL, XK, XNE, XNENW, XNEO, XNH, XNP, YH, Z,
     $       dummy
      integer IPTO, IQAHS, ISWA, KASE, LHHSE, LIMP, MEITER, MO, N,
     $        NEITER, NHITER, NLH, NTITER, jummy
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ( 71),HTAU )
      equivalence (RZQ( 16),CGR  )
      equivalence (RZQ( 15),YH   )
      equivalence (RZQ( 14),HSEC )
      equivalence (RZQ( 31),HEL  )
      equivalence (KZQ(129),LHHSE)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !EJECT
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ(257),IQAHS)
C     !DASH
      external DICK, HE, SERF, PONG, GENOA, HARRY, HALT, AGONE, PLINTH,
     $         TOM, PAZYP, ONEGA, LINER, HI, BYE
C
C               Z(N), TKIN(N), HND(N), VEC(N), XCBL(Miklen), HELABD(N),
      dimension Z(*), TKIN(*), HND(*), VEC(*), XCBL(*),      HELABD(*),
C
C               PTU(N), PTO(N), TAU5000(N), GMASS(N), PGS(N), HNDNW(N),
     $          PTU(*), PTO(*), TAU5000(*), GMASS(*), PGS(*), HNDNW(*),
C
C               HNDO(N), XNEO(N), GR(N), HP(N), VT(N), XNENW(N), GD(N),
     $          HNDO(*), XNEO(*), GR(*), HP(*), VT(*), XNENW(*), GD(*),
C
C               T5(12), F(12), TE(N), XNP(N), XNH(N,Limp), ISWA(Nopac),
     $          T5(*),  F(*),  TE(*), XNP(*), XNH(*),      ISWA(*),
C
C               VM(N), XNE(N), PEX(N), HNEU(N), DGM(N), EMN(N), PMG(N),
     $          VM(*), XNE(*), PEX(*), HNEU(*), DGM(*), EMN(*), PMG(*),
C
C               VET(N), IPTO(N), RIND(N), EF(N), XK(N), DD(N), EM(N)
     $          VET(*), IPTO(*), RIND(*), EF(*), XK(*), DD(*), EM(*)
C
      data WVL /5.D3/
C     !EJECT
C
      call HI ('ZAPPY')
C     !BEG
      if(MO.gt.0) then
C----   Header
        call PAZYP     (MO, CGR, YH, LHHSE, KASE)
C
        if(IQAHS.gt.0) then
C----     Abbreviated version
          call LINER   (1, MO)
          write (MO,100) HEL,HSEC
  100     format(' ',1PE13.5,3X,'HEL',10X,E13.5,3X,'HSEC')
          call PONG    (MO, N, Z, HND, XNE, PGS, HNEU, PTO, GD,
     $                  TAU5000, GMASS, TE, VT, VM, DGM, PMG, TKIN,
     $                  HELABD, VEC, VET, KASE, 1, IPTO)
        else
C----     Normal, full version
          if((KASE.lt.1).or.(KASE.gt.3)) then
            write (MSSLIN(1),101) KASE
  101       format('KASE =',I12,', which is not 1, 2, or 3.')
            call HALT  ('ZAPPY', 1)
          end if
C
          if(KASE.eq.1) then
            call TOM   (MO, N, HNDO, XNEO, XNP, GR, HP, HNDNW, XNENW,
     $                  HND, XNE, HSEC, NEITER, TAU5000, MEITER, HEL,
     $                  TKIN, NHITER)
          else if(KASE.eq.2) then
            call DICK  (MO, N, Z, HNDO, XNEO, XNP, GR, HP, HNDNW,
     $                  XNENW, HND, XNE, HSEC, NEITER, TAU5000,
     $                  NTITER, MEITER, T5, F, NTITER, HEL, HTAU)
          else
            call HARRY (MO, N, Z, HNDO, XNEO, XNP, GR, HP, HNDNW,
     $                  XNENW, HND, XNE, HSEC, NEITER, MEITER, HEL)
          end if
C
          call GENOA   (MO, N, HNDO, HNDNW, XNEO, XNENW, VEC)
          call ONEGA   (MO, N, EF, XK, DD, EM, FEM)
          call AGONE   (MO, N, Z, TE, VM, PEX, PGS, EMN)
          call PONG    (MO, N, Z, HND, XNE, PGS, HNEU, PTO, GD,
     $                  TAU5000, GMASS, TE, VT, VM, DGM, PMG, TKIN,
     $                  HELABD, VEC, VET, KASE, 0, IPTO)
C----     Print Absorbers at 5000 Angstroms
          call PLINTH  (WVL, XCBL, ISWA, MO)
C----     Print Hydrogen level populations
          call SERF    (MO, NLH, N, HND, XNE, XNH, XNP, 1, 'HYDROGEN',
     $                  8, LIMP, 1, 1, dummy, jummy, 0, .true., .true.)
C----     Print populations adjustment factor
          call HE      (MO, RIND, N)
        end if
      end if
C     !END
      call BYE ('ZAPPY')
C
      return
      end
