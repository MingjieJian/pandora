      subroutine PYROLA
     $(X,KAMB,N,NL,XNK,XN1,HND,HEK,HE1,HE2K,HE21,SHE,SHE2,RHEAB,ABDEL,
     $ F1C,F1AC,F2C,PNF,BETAP,DUMP)
C
C     Rudolf Loeser, 1990 Sep 06
C---- "Additional" normalization, for the 'special' N1 calculation
C     of the ambipolar diffusion calculation.
C     (This is version 2 of PYROLA.)
C     !DASH
      save
C     !DASH
      real*8 ABDEL, BETAP, F1AC, F1C, F2C, HE1, HE21, HE2K, HEK, HND,
     $       PNF, RHEAB, SHE, SHE2, X, XN1, XNK
      integer IQHNM, KAMB, LUEO, N, NL
      logical DUMP
      character NAMES*4
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
      equivalence (IQQ(230),IQHNM)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external PROLAY, RAPLOY, PALORY, ARRMUL, LINER, PRIVET, HI, BYE
C
      dimension X(*)
C
C               HE21(N), ABDEL(N), F1AC(N), RHEAB(N), HE2K(N), SHE2(N),
      dimension HE21(*), ABDEL(*), F1AC(*), RHEAB(*), HE2K(*), SHE2(*),
C
C               XNK(N), PNF(N), HND(N), HEK(N), HE1(N), F2C(N), SHE(N),
     $          XNK(*), PNF(*), HND(*), HEK(*), HE1(*), F2C(*), SHE(*),
C
C               F1C(N), XN1(N), BETAP(N)
     $          F1C(*), XN1(*), BETAP(*)
C
      dimension NAMES(2)
C
      data NAMES /'HeI ', 'HeII'/
C
      call HI ('PYROLA')
C     !BEG
      if((IQHNM.gt.0).and.((KAMB.eq.2).or.(KAMB.eq.3))) then
C
        call PROLAY (KAMB, N, F1C, F1AC, XNK, XN1, HE21, HEK, BETAP)
        call ARRMUL (XNK, F1C, XNK, N)
        call ARRMUL (XN1, F1C, XN1, N)
        call PALORY (KAMB, N, F1AC, HE21, HE2K, HE1, HEK, SHE, SHE2,
     $               PNF)
C
        call RAPLOY (KAMB, N, F2C, RHEAB, ABDEL, HND, XNK, XN1, HE2K,
     $               HE1)
        call ARRMUL (XNK, F2C, XNK, N)
        call ARRMUL (XN1, F2C, XN1, N)
        call PALORY (KAMB, N, F2C, HE21, HE2K, HE1, HEK, SHE, SHE2,
     $               PNF)
C     !EJECT
        if(DUMP) then
          call LINER  (3, LUEO)
          write (LUEO,100) NAMES(KAMB-1)
  100     format(' ','"Additional" normalization of N1 and NK, ',
     $               '(option HENORM)'//
     $           ' ','First factor for ',A4)
          call PRIVET (LUEO, F1C, N)
C
          call LINER  (2, LUEO)
          write (LUEO,101) NAMES(4-KAMB)
  101     format(' ','First factor for ',A4)
          call PRIVET (LUEO, F1AC, N)
C
          call LINER  (2, LUEO)
          write (LUEO,102)
  102     format(' ','Second factor for HeI and HeII')
          call PRIVET (LUEO, F2C, N)
C
          call LINER  (2, LUEO)
          write (LUEO,103) NAMES(4-KAMB)
  103     format(' ','These particular ',A4,' changes'/
     $           ' ','affect   O N L Y   the diffusion calculations, ',
     $               'and   N O T H I N G   else.')
        end if
C
      end if
C     !END
      call BYE ('PYROLA')
C
      return
      end
