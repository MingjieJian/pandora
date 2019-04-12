      subroutine GHASTLY
     $(N,PEL,PGS,PTU,PEX,PMG,PNH,PTO,XNE,TE,HND,HELABD,H2N,VT,VM,
     $ GDN,EMN)
C
C     Rudolf Loeser, 1970 Dec 01
C---- Computes:
C     GDN - gas density;
C     PEL - electron pressure;
C     PGS - gas pressure;
C     PTU - turbulent pressure;
C     PEX - expansion pressure;
C     PNH - total NH-related pressure terms;
C     PMG - magnetic pressure;
C     PTO - total pressure, and
C     EMN - Mach number.
C     !DASH
      save
C     !DASH
      real*8 CON18, EMN, FAC, GDN, H2N, HELABD, HND, PEL, PEX, PGS, PMG,
     $       PNH, PTO, PTU, TE, TWO, VM, VT, XNE, ZERO
      integer I, IQHSV, LGT, N
C     !COM
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
      equivalence (IQQ(270),IQHSV)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 3),TWO   )
C     !DASH
C     !EJECT
      external RIGEL, POLYP, GASP, PLUSD, LUST, GUSH, DIVIDE, HI, BYE
C
C               PGS(N), PTU(N), PTO(N), VT(N), TE(N), VM(N), HELABD(N),
      dimension PGS(*), PTU(*), PTO(*), VT(*), TE(*), VM(*), HELABD(*),
C
C               PMG(N), XNE(N), HND(N), H2N(*), PEX(*), GDN(N), EMN(N),
     $          PMG(*), XNE(*), HND(*), H2N(*), PEX(*), GDN(*), EMN(*),
C
C               PEL(N), PNH(N)
     $          PEL(*), PNH(*)
C
      call HI ('GHASTLY')
C     !BEG
C---- Get gas density
      call GUSH       (N, HND, HELABD, GDN)
C---- Get electron pressure
      call POLYP      (TE, XNE, PEL, N)
C---- Get gas pressure
      call GASP       (TE, HND, HELABD, H2N, PEL, PGS, N)
C
      call RIGEL      (18, CON18)
C---- Compute the rest
      do 100 I = 1,N
        FAC    = GDN(I)*CON18
        PTU(I) = FAC*(VT(I)**2)
C
        if(IQHSV.gt.0) then
          PEX(I) = FAC*TWO*(VM(I)**2)
          call DIVIDE (PEX(I), PGS(I), EMN(I))
        else
          PEX(I) = ZERO
          EMN(I) = ZERO
        end if
C
        PNH(I) = PGS(I)+PTU(I)+PEX(I)
        PTO(I) = PNH(I)+PMG(I)
  100 continue
C
      call PLUSD      (PTO, 1, N, LGT)
      if(LGT.lt.N) then
C----   Total Pressure is not all > 0: print message and abort
        call LUST     (N, H2N, HND, HELABD, XNE, TE, GDN, PEL, PGS,
     $                 PTU, PEX, PTO, PMG)
      end if
C     !END
      call BYE ('GHASTLY')
C
      return
      end
