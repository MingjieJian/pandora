      subroutine PRATON
     $(DUMP,N,NL,KODE,HND,H1,HK,RHAB,HEND,HE1,HEK,BETA,HE21,HE2K,RHEAB,
     $ PNF,RNDU,RND,PLK,PKL,SPKL,BETAR,DZB,SHE,SHE2)
C
C     Rudolf Loeser, 1998 Jul 21
C---- Dumps for PONTAR.
C     !DASH
      save
C     !DASH
      real*8 BETA, BETAR, DZB, H1, HE1, HE21, HE2K, HEK, HEND, HK, HND,
     $       PKL, PLK, PNF, RHAB, RHEAB, RND, RNDU, SHE, SHE2, SPKL
      integer IQAN1, KODE, N, NL
      logical DUMP
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
      equivalence (IQQ(272),IQAN1)
C     !DASH
C     !EJECT
      external MEULAN, LAUTU, MESHED, MASHED, HI, BYE
C
C               RNDU(N,NL), RND(N,NL), PLK(N,NL), HE1(N), H1(N), HK(N),
      dimension RNDU(N,*),  RND(N,*),  PLK(N,*),  HE1(*), H1(*), HK(*),
C
C               PKL(N,NL), RHEAB(N), BETA(N), HE21(N), DZB(N), RHAB(N),
     $          PKL(N,*),  RHEAB(*), BETA(*), HE21(*), DZB(*), RHAB(*),
C
C               HEND(N), SPKL(N), HE2K(N), HEK(N), SHE2(N), BETAR(N),
     $          HEND(*), SPKL(*), HE2K(*), HEK(*), SHE2(*), BETAR(*),
C
C               SHE(N), PNF(N), HND(N)
     $          SHE(*), PNF(*), HND(*)
C
      call HI ('PRATON')
C     !BEG
      if(DUMP.and.(IQAN1.gt.0)) then
        call MESHED ('PRATON', 2)
C
C----   Populations
        call MEULAN (N, 0, HND, H1, HK, RHAB, HEND, HE1, HEK, BETA,
     $               HE21, HE2K, RHEAB, PNF, SHE, SHE2, BETAR, DZB)
C----   Population ratios and transition rates
        call LAUTU  (N, NL, RNDU, RND, KODE, PLK, PKL, SPKL)
C
        call MASHED ('PRATON')
      end if
C     !END
      call BYE ('PRATON')
C
      return
      end
