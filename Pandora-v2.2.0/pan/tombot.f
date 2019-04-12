      subroutine TOMBOT
     $(REAL,JTRANS,X,XLB1)
C
C     Rudolf Loeser, 1998 Aug 13
C---- Final processing for the current transition, for BOTTOM.
C     !DASH
      save
C     !DASH
      real*8 X, XLB1
      integer JJ304, JTRANS, MMJBR, MMRHO, MMS, MMTAU, N
      logical REAL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(  5),JJ304)
C
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML(16),MMTAU)
      equivalence (MML(26),MMS  )
      equivalence (MML(28),MMRHO)
      equivalence (MML(27),MMJBR)
C     !DASH
      external KRIS, ZULU, DOUR, HI, BYE
C
      dimension X(*)
C
C               XLB1(Li1len)
      dimension XLB1(*)
C     !EJECT
C
      call HI ('TOMBOT')
C     !BEG
      if(REAL) then
C----   Save Mean Intensity in special slot, if necessary
        call KRIS (XLB1(MMJBR),X(JJ304),N)
C----   Save data for iterative summary
        call ZULU (XLB1(MMTAU),XLB1(MMS),XLB1(MMRHO),JTRANS)
C----   Save debug checksums
        call DOUR (XLB1(MMTAU),XLB1(MMS),XLB1(MMRHO),XLB1(MMJBR))
      end if
C     !END
      call BYE ('TOMBOT')
C
      return
      end
