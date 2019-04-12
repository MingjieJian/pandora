      subroutine GOBY
     $(XLB1,JPROM)
C
C     Rudolf Loeser, 1992 Mar 11
C---- Adjusts the provisional value of the Hydrogen Stark broadening
C     switch so that it applies to the current transition
C     (i.e. the transition whose Line Intensity Data block is in XLB1).
C
C     (The provisional value of this switch, MPROM, is set by OAK.)
C
C     (This is version 2 of GOBY.)
C     !DASH
      save
C     !DASH
      real*8 XLB1, ZERO
      integer JPROM, MMSTK, MPROM
C     !COM
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML(62),MMSTK)
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(63),MPROM)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external HI, BYE
C
C               XLB1(Li1len)
      dimension XLB1(*)
C
      call HI ('GOBY')
C     !BEG
      JPROM = 0
      if(MPROM.gt.0) then
        if(XLB1(MMSTK).gt.ZERO) then
          JPROM = 1
        end if
      end if
C     !END
      call BYE ('GOBY')
C
      return
      end
