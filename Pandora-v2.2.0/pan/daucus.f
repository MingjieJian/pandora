      subroutine DAUCUS
     $(X,W)
C
C     Rudolf Loeser, 1988 Aug 10
C---- Recomputes fluid velocities,
C     now that HND, and perhaps Z, have been recomputed.
C     !DASH
      save
C     !DASH
      real*8 CVSB, CVXS, W, X
      integer IFR, IHEND, IHNDL, IHNVL, IN, IS, IVNH, JJFMV, JJHEA,
     $        JJHND, JJHNV, JJV, JJVBM, JJVM, JJVNH, JJVSB, JJVT, JJVXS,
     $        JJZ, MO, MOX, N, NVH, NVSB
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(54),NVH)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ(129),JJVXS)
      equivalence (IZOQ(181),JJVM )
      equivalence (IZOQ(172),JJVSB)
      equivalence (IZOQ(219),JJHEA)
      equivalence (IZOQ(206),JJHNV)
      equivalence (IZOQ( 41),JJVNH)
      equivalence (IZOQ( 12),JJV  )
      equivalence (IZOQ( 49),JJVT )
      equivalence (IZOQ(225),JJFMV)
      equivalence (IZOQ(196),JJVBM)
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
      equivalence (RZQ(104),CVXS )
      equivalence (RZQ(114),CVSB )
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
      equivalence (LEST(36),NVSB )
      equivalence (LEST(42),IVNH )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external GAURA, IMI, WGIVE, HI, BYE
C
      dimension X(*), W(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IFR   ),(IN( 2),IHNVL ),(IN( 3),IHNDL ),(IN( 4),IHEND )
C
      call HI ('DAUCUS')
C     !BEG
C     (Get, and allocate, W allotment)
      call IMI   (IN, IS, MOX, 'DAUCUS')
C
      call GAURA (X(JJZ), X(JJHND), X(JJHEA), X(JJFMV), N, X(JJVM),
     $            CVXS, X(JJVXS), NVSB, CVSB, X(JJVSB), X(JJV),
     $            X(JJHNV), X(JJVNH), NVH, IVNH, X(JJVT), MO, W(IFR),
     $            W(IHNVL), W(IHNDL), X(JJVBM), W(IHEND))
C
C     (Give back W allotment)
      call WGIVE (W, 'DAUCUS')
C     !END
      call BYE ('DAUCUS')
C
      return
      end
