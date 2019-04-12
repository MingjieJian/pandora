      subroutine PIE
     $(N,NL,M,EP1,EP2,RS,CKI,PKL,PL1,PLK,P1L,CQI,BDI,MQT,CQTAIL,TAUK,
     $ XLMT,EPCBR,SPKL,PS1,AL,KDGV,GVL,DUMP)
C
C     Rudolf Loeser, 1976 Nov 26
C---- Computes and prints EP1 and EP2, for COCOS.
C     (This is version 2 of PIE.)
C     !DASH
      save
C     !DASH
      real*8 AL, BDI, CKI, CQI, CQTAIL, EP1, EP2, EPCBR, GVL, P1L, PKL,
     $       PL1, PLK, PS1, RS, SPKL, TAUK, XLMT
      integer IQEPS, KDGV, KODE, M, MQT, N, NL
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
      equivalence (IQQ(122),IQEPS)
C     !DASH
      external ZERO1, SAND, GRAVEL, PISTAKA, HI, BYE
C
C               EP1(N), EP2(N), RS(N), CKI(N,NSL), PKL(N), CQTAIL(MQT),
      dimension EP1(*), EP2(*), RS(*), CKI(*),     PKL(*), CQTAIL(*),
C
C               PLK(N), P1L(N), SPKL(N), BDI(N,NL), GVL(N,NL), TAUK(N),
     $          PLK(*), P1L(*), SPKL(*), BDI(*),    GVL(*),    TAUK(*),
C
C               PS1(N), AL(NL), PL1(N), CQI(N)
     $          PS1(*), AL(*),  PL1(*), CQI(*)
C     !EJECT
C
      call HI ('PIE')
C     !BEG
      call ZERO1    (CQI, N)
      call SAND     (N, NL, M, EP1, EP2, RS, CKI, PKL, PL1, PLK, P1L,
     $               CQI, BDI, GVL, KDGV, EPCBR, SPKL, PS1, AL)
C
      if(IQEPS.gt.0) then
        call GRAVEL (N, XLMT, TAUK, EP1, CQI, CQTAIL, MQT, KODE)
        if(KODE.gt.0) then
          call SAND (N, NL, M, EP1, EP2, RS, CKI, PKL, PL1, PLK, P1L,
     $               CQI, BDI, GVL, KDGV, EPCBR, SPKL, PS1, AL)
        end if
      end if
 
      call PISTAKA  (N, M, RS, CKI, PKL, PLK, PL1, P1L, BDI, CQI, EP1,
     $               EP2, DUMP)
C     !END
      call BYE ('PIE')
C
      return
      end
