      subroutine CURVY
     $(N,NL,XND,XNK)
C
C     Rudolf Loeser, 1975 Aug 01
C---- Saves recomputed XND and XNK for iterative summary
C     (omitting the updates occurring in the Lyman iterations).
C     !DASH
      save
C     !DASH
      real*8 XND, XNK
      integer IQINN, J, KXND, KXNK, LITER, N, NL
      logical KOUNT
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(24),LITER)
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
      equivalence (IQQ(113),IQINN)
C     !DASH
      external BERWYN, HI, BYE
C
C               XND(N,NL), XNK(N)
      dimension XND(N,*),  XNK(*)
C
      data KXND,KXNK /5, 14/
C     !EJECT
C
      call HI ('CURVY')
C     !BEG
      if((IQINN.gt.0).and.(LITER.le.0)) then
        KOUNT = .true.
C
        do 100 J = 1,NL
          call BERWYN (KXND,'Curvy','ND',J,0,XND(1,J),N,KOUNT )
          KOUNT = .false.
  100   continue
C
        call BERWYN   (KXNK,'Curvy','NK',0,0,XNK     ,N,.true.)
      end if
C     !END
      call BYE ('CURVY')
C
      return
      end
