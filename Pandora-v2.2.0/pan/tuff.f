      subroutine TUFF
     $(X,IX,W,IW,RKI,IQRK,RLI,IQRL,LU,XPBL,ESG,KILROY)
C
C     Rudolf Loeser, 1978 Feb 14
C---- Supervises Lower-Level Charge Exchange calculation.
C     !DASH
      save
C     !DASH
      real*8 ESG, RKI, RLI, W, X, XPBL, dummy
      integer IALFH, IALFHE, IALL, ICFH, ICFHE, IH1, IHE1, IHEK, IHK,
     $        IN, INDEX, IQLXL, IQRK, IQRL, IS, IW, IX, J, JJPF, JJTE,
     $        LCEX, LU, MOX, N, NL
      logical DORK, DORL, KILROY
      character INAME*3
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ(141),JJPF )
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
      equivalence (IQQ(333),IQLXL)
C     !DASH
C     !EJECT
      external LOKI, BURION, KALT, POPUTIL, SEDUM, CINDER, WGIVE,
     $         HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XPBL(Lenpbl), ESG(N,NSL), RKI(N,NSL), RLI(N,NSL),
      dimension XPBL(*),      ESG(N,*),   RKI(N,*),   RLI(N,*),
C
C               IQRK(NSL), IQRL(NSL)
     $          IQRK(*),   IQRL(*)
C
      dimension IN(11)
      equivalence
     $(IN( 1),IH1   ),(IN( 2),IHK   ),(IN( 3),IHE1  ),(IN( 4),IHEK  ),
     $(IN( 5),IALFH ),(IN( 6),IALFHE),(IN( 7),ICFH  ),(IN( 8),ICFHE ),
     $(IN( 9),IALL  ),(IN(10),ISO   ),(IN(11),ISUM  )
C
      call HI ('TUFF')
C     !BEG
      call SEDUM        (INAME, INDEX, LCEX)
C
      if(INDEX.ne.0) then
C       (Get and allocate W allotment)
        call LOKI       (IN, IS, MOX, 'TUFF')
C
        if(KILROY) then
          KILROY = .false.
          call BURION   (X, 1, NL, ESG)
        end if
        if(IQLXL.gt.0) then
          call KALT     (X, W, IW, N, W(IH1), W(IHK), LU)
        else
          call POPUTIL  (XPBL, 1, 1, W(IH1),  1, W(IHK),  0, dummy,
     $                            0, dummy)
        end if
        call POPUTIL    (XPBL, 4, 1, W(IHE1), 1, W(IHEK), 0, dummy,
     $                            0, dummy)
C
        do 100 J = 1,LCEX
          DORK = IQRK(J).gt.0
          DORL = IQRL(J).gt.0
          if(DORK.or.DORL) then
            call CINDER (N, LU, INDEX, INAME, J, ESG(1,J), X(JJTE),
     $                   X(JJPF), W(IH1), W(IHK), W(IHE1), W(IHEK),
     $                   W(IALFH), W(IALFHE), W(IALL), W(ICFH),
     $                   W(ICFHE), RKI(1,J), DORK, RLI(1,J), DORL)
          end if
  100   continue
C
C       (Give back W allotment)
        call WGIVE      (W, 'TUFF')
      end if
C     !END
      call BYE ('TUFF')
C
      return
      end
