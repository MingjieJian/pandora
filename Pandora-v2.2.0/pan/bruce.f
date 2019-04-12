      subroutine BRUCE
     $(EMU,L,AVCON,AVCONA,WVNUM,EMINT,EMINTA,NW,LFB)
C
C     Rudolf Loeser, 1993 Feb 19
C---- Computes average intensities.
C     !DASH
      save
C     !DASH
      real*8 AVCON, AVCONA, EMINT, EMINTA, EMU, WVNUM, ZERO
      integer IQAVK, IQWNM, J, L, LFB, NW
      logical INCRAD
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
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
      equivalence (IQQ(303),IQAVK)
      equivalence (IQQ(290),IQWNM)
C     !DASH
      external ZERO1, BARRY, DOWNY, HI, BYE
C
C               WVNUM(Nmkuse), EMINT(Nmkuse,L), EMINTA(Nmkuse), EMU(L),
      dimension WVNUM(*),      EMINT(NW,*),     EMINTA(*),      EMU(*),
C
C               AVCON(L)
     $          AVCON(*)
C     !EJECT
C
      call HI ('BRUCE')
C     !BEG
      call ZERO1       (AVCON,L)
      AVCONA = ZERO
C
      if((IQAVK.gt.0).and.(IQWNM.gt.0)) then
        do 100 J = 1,L
          call DOWNY   (LFB,EMU(J),INCRAD)
          call BARRY   (1,WVNUM,EMINT(1,J),NW,AVCON(J))
          if(INCRAD) then
            call BARRY (1,WVNUM,EMINTA    ,NW,AVCONA  )
          end if
  100   continue
      end if
C     !END
      call BYE ('BRUCE')
C
      return
      end
