      subroutine DIAMOND
     $(X,IX,W,IW,LU)
C
C     Rudolf Loeser, 1970 Feb 18
C---- Supervises Departure Coefficients plots.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IN, IPOIN, IQBDG, IQTEG, IS, ITAU, IW, IWS, IX, IXLB1,
     $        IZLOG, JJBDI, JJBIJ, JJTE, JJZ, JN, LAB, LU, MOX, MUX, N,
     $        NL, NRAD, NT
      logical BDG, TEG
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ( 5),NT )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ( 44),JJBDI)
      equivalence (IZOQ( 23),JJBIJ)
      equivalence (IZOQ(  7),JJTE )
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
      equivalence (IQQ(252),IQBDG)
      equivalence (IQQ(253),IQTEG)
C     !EJECT
C---- IMAGE       as of 1997 Aug 21
      integer     IMALEN
      parameter   (IMALEN=65535)
      character   IMAGE*(IMALEN)
      common      /IMAGE/ IMAGE
C     Character string to hold plot images constructed by the
C     K-type line printer plotting routines;
C     but used also as a general scratch character array.
C     .
C     !DASH
      external IDOL, BLAND, ZAMORIN, GILLIE, GAHOL, IGIVE, CARAT, WGIVE,
     $         HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),ITAU ),(IN( 2),IPOIN),(IN( 3),IZLOG),(IN( 4),IXLB1)
C
      dimension JN(1)
      equivalence
     $(JN( 1),LAB  )
C
      call HI ('DIAMOND')
C     !BEG
      BDG = IQBDG.gt.0
      TEG = IQTEG.gt.0
      if(BDG.or.TEG) then
C       (Get, and allocate, W & IW allotments)
        call IDOL      (IN, IS,  MOX, 'DIAMOND')
        call CARAT     (JN, IWS, MUX, 'DIAMOND')
C
C----   Gather TAU scales and transition labels, for all
C       radiative transitions
        call BLAND     (W(IXLB1), W(ITAU), NRAD, N, NT, IW(LAB))
        if(BDG) then
C----     Plot Departure Coefficients and TAU scales vs. Z
          call ZAMORIN (X(JJZ), X(JJBDI), W(ITAU), IW(LAB), NRAD,
     $                  IMAGE, W(IPOIN), W(IZLOG), N, NL, LU)
C----     Plot ratios of Departure Coefficients and TAU scales vs. Z
          call GILLIE  (LU, N, NRAD, X(JJZ), X(JJBIJ), W(ITAU),
     $                  IW(LAB), W(IPOIN), W(IZLOG), IMAGE)
        end if
        if(TEG) then
C----     Plot TE vs. TAU
          call GAHOL   (LU, N, NRAD, IMAGE, W(ITAU), IW(LAB),
     $                  X(JJTE))
        end if
C
C       (Give back W & IW allotments)
        call WGIVE     (W,  'DIAMOND')
        call IGIVE     (IW, 'DIAMOND')
      end if
C     !END
      call BYE ('DIAMOND')
C
      return
      end
