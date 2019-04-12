      subroutine JUICE
     $(NO,IMAGE,J,KSHL,XH,XNU,XNUC,NH,II,LINE)
C
C     Rudolf Loeser, 2001 Dec 26
C---- Prints the graph, for HARP.
C     (This is version 4 of JUICE.)
C     !DASH
      save
C     !DASH
      real*8 ONE, XH, XNU, XNUC
      integer I, II, J, NH, NO
      logical KSHL
      character IMAGE*(*), LINE*(*), QTIT*8
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
C     !DASH
C     !EJECT
      external ABJECT, KPRINT, ALBERIC, LINER, HI, BYE
C
C               II(5), XNU(NSL), XNUC(NSL)
      dimension II(*), XNU(*),   XNUC(*)
C
      call HI ('JUICE')
C     !BEG
C---- Write header
      if(KSHL) then
        QTIT = 'K-shell'
      else
        write (QTIT,100) J
  100   format('Level',I3)
      end if
      call ABJECT   (NO)
      write (NO,101) QTIT
  101 format(' ','Plot of log(Jnu) vs. RNU for ',A8)
C
C---- Print graph
      call KPRINT   (IMAGE, NO)
C
C---- Write labels
      call ALBERIC  (NO, LINE, NH, ONE, XH, XNU, XNUC, J)
      call LINER    (1, NO)
      write (NO,102) (ALPHS(I),II(I),I=1,5)
  102 format(' ','The curves correspond to the following depths: ',
     $           4(3X,A1,'-',I4,','),3X,A1,'-',I4,'.')
C     !END
      call BYE ('JUICE')
C
      return
      end
