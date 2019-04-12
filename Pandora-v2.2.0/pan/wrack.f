      subroutine WRACK
     $(X,XLB1,TZERO,VM,KILROY)
C
C     Rudolf Loeser, 1980 Aug 13
C---- Computes table of Doppler Widths, and
C     Reference Value of Doppler Width,
C     for all rediative and passive transitions.
C     !DASH
      save
C     !DASH
      real*8 TZERO, VM, X, XLB1
      integer JJTE, JJV, JJVR, MMCDW, MMDW, MMLAM, N, NDW
      logical KILROY
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
      equivalence (IZOQ( 12),JJV  )
      equivalence (IZOQ(165),JJVR )
      equivalence (IZOQ(  7),JJTE )
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(  1),NDW  )
C
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML( 2),MMLAM)
      equivalence (MML(13),MMDW )
      equivalence (MML( 4),MMCDW)
C     !DASH
      external TALUS, ZERO1, SCREE, ELVA, HI, BYE
C
      dimension X(*)
C
C               XLB1(Li1len), VM(N), TZERO(N)
      dimension XLB1(*),      VM(*), TZERO(*)
C     !EJECT
C
      call HI ('WRACK')
C     !BEG
      if(KILROY) then
        KILROY = .false.
C----   Set up table of zeroes
        call ZERO1 (TZERO,N)
C----   Compute VM
        call ELVA  (N,X(JJV),X(JJVR),VM)
      end if
C
C---- Compute Doppler Width
      call TALUS   (X(JJTE),VM,TZERO,TZERO,N,XLB1(MMLAM),0,XLB1(MMDW))
C---- Set up Reference Doppler Width
      call SCREE   (XLB1(MMDW),XLB1(MMCDW))
C     !END
      call BYE ('WRACK')
C
      return
      end
