      subroutine GOONEY
     $(X,W,XPBL,FCJ,FCK,LU)
C
C     Rudolf Loeser, 1984 Jul 21
C---- Drives GREYLAG, to deal with fast electrons.
C     !DASH
      save
C     !DASH
      real*8 FCJ, FCK, W, X, XPBL
      integer IAA, IEE, IEV, IFF, IFJN, IFJNL, IFNJ, IFNJL, IGG, IN,
     $        IQFLD, IQFLP, IS, ISS, IVL, IVV, IXPB2, IXPB3, IXQ, JJTE,
     $        JJZ, KFELE, LU, MOX, N, NL, NO, jummy
      logical DUMP, FELE
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
      equivalence (IZOQ( 37),JJZ  )
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
      equivalence (IQQ( 81),IQFLP)
      equivalence (IQQ( 22),IQFLD)
C     !EJECT
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(70),KFELE)
C     !DASH
      external ZEUS, HINNY, POPIO, MOOSE, GREYLAG, WGIVE, ZERO1,
     $         HI, BYE
C
      dimension X(*), W(*)
C
C               FCJ(N,NL), FCK(N), XPBL(Lenpbl)
      dimension FCJ(*),    FCK(*), XPBL(*)
C
      dimension IN(15)
      equivalence
     $(IN( 1),IFJNL ),(IN( 2),IXPB2 ),(IN( 3),IXPB3 ),(IN( 4),ISS   ),
     $(IN( 5),IEV   ),(IN( 6),IEE   ),(IN( 7),IVV   ),(IN( 8),IVL   ),
     $(IN( 9),IFNJ  ),(IN(10),IFNJL ),(IN(11),IXQ   ),(IN(12),IFF   ),
     $(IN(13),IAA   ),(IN(14),IGG   ),(IN(15),IFJN  )
C
      call HI ('GOONEY')
C     !BEG
      call ZERO1     (FCK, N     )
      call ZERO1     (FCJ, (N*NL))
C
      call HINNY     (FELE, KFELE)
      if(FELE) then
C       (Get, and allocate, W allotment)
        call MOOSE   (IN, IS, MOX, 'GOONEY')
C       (Initialize populations buffers)
        call POPIO   ('INIT', jummy, W(IXPB2))
        call POPIO   ('INIT', jummy, W(IXPB3))
C
        call ZEUS    (LU, IQFLP, NO)
        DUMP = (IQFLP.gt.0).and.(IQFLD.gt.0)
        call GREYLAG (X, W, XPBL, W(IXPB2), W(IXPB3), W(ISS), W(IEV),
     $                W(IVV), W(IVL), W(IGG), W(IFNJ), W(IFNJL),
     $                W(IXQ), W(IFF), W(IAA), W(IFJN), W(IFJNL),
     $                X(JJZ), X(JJTE), W(IEE), NO, DUMP, FCK, FCJ)
C
C       (Give back W allotment)
        call WGIVE   (W, 'GOONEY')
      end if
C     !END
      call BYE ('GOONEY')
C
      return
      end
