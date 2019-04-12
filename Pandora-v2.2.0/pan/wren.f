      subroutine WREN
     $(X,IX,W,IW,XLB1,MPROM,XKL,XKT)
C
C     Rudolf Loeser, 1980 May 09
C---- Drives TAU-calculations, for MINUET.
C     !DASH
      save
C     !DASH
      real*8 W, X, XKL, XKT, XLB1
      integer IFDDL, IN, IS, IW, IX, JJBDI, JJVXS, JJXND, JJXNE, MMCDL,
     $        MMCOP, MMDDL, MMDP, MMDW, MMFXI, MMGTN, MMGTO, MMLAM,
     $        MMSTE, MMTAM, MMTAU, MOX, MPROM, N
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
      equivalence (IZOQ( 59),JJXND)
      equivalence (IZOQ( 44),JJBDI)
      equivalence (IZOQ(129),JJVXS)
      equivalence (IZOQ(  9),JJXNE)
C
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML(15),MMGTN)
      equivalence (MML(17),MMCOP)
      equivalence (MML(12),MMDP )
      equivalence (MML(13),MMDW )
      equivalence (MML( 2),MMLAM)
      equivalence (MML(16),MMTAU)
      equivalence (MML(34),MMDDL)
      equivalence (MML(31),MMCDL)
      equivalence (MML(48),MMFXI)
      equivalence (MML(51),MMTAM)
      equivalence (MML(61),MMSTE)
      equivalence (MML(45),MMGTO)
C     !DASH
C     !EJECT
      external VULTURE, NOSSEN, NIMBLE, WGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XKL(N), XKT(N), XLB1(Lizlen)
      dimension XKL(*), XKT(*), XLB1(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IFDDL )
C
      call HI ('WREN')
C     !BEG
C     (Get W allotment)
      call NOSSEN  (IN, IS, MOX, 'WREN')
C
      call NIMBLE  (XLB1(MMSTE), X(JJXNE), W(IFDDL), N)
      call VULTURE (X, IX, W, IW, X(JJXND), X(JJBDI), XLB1(MMGTO),
     $              XLB1(MMGTN), XLB1(MMFXI), XLB1(MMCOP), XLB1(MMDP),
     $              XLB1(MMDW), MPROM, X(JJXNE), XLB1(MMDDL), W(IFDDL),
     $              XLB1(MMCDL), X(JJVXS), XLB1(MMLAM), XLB1(MMTAU),
     $              XLB1(MMTAM), XKL, XKT, 1)
C
C     (Give back W allotment)
      call WGIVE   (W, 'WREN')
C     !END
      call BYE ('WREN')
C
      return
      end
