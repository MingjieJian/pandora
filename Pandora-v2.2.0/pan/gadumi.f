      subroutine GADUMI
     $(X,IX,W,VEL,KVEL,NO)
C
C     Rudolf Loeser, 2003 Mar 25
C---- Supervises set-up of velocity set, for LIGHT.
C     !DASH
      save
C     !DASH
      real*8 VEL, W, X
      integer IHRQ, IN, IRVL, IRVS, IS, IVEC, IVRQ, IX, JJISV, JJVAD,
     $        JJVSB, JJVXN, JJVXS, JJWTP, KVEL, MOX, N, NO, NVX
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(42),NVX)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(164),JJVXN)
      equivalence (IZOQ(177),JJVAD)
      equivalence (IZOQ(129),JJVXS)
      equivalence (IZOQ(172),JJVSB)
      equivalence (IZOQ(120),JJWTP)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ( 13),JJISV)
C     !DASH
C     !EJECT
      external DRALENA, MUGADI, WGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*)
C
C               VEL(N,NVX+3), KVEL(NVX+3)
      dimension VEL(*),       KVEL(*)
C
      dimension IN(5)
      equivalence
     $(IN( 1),IRVL  ),(IN( 2),IRVS  ),(IN( 3),IVEC  ),(IN( 4),IVRQ  ),
     $(IN( 5),IHRQ  )
C
      call HI ('GADUMI')
C     !BEG
C     (Get, and allocate, W allotment)
      call DRALENA (IN, IS, MOX, 'GADUMI')
C
      call MUGADI  (X, N, VEL, KVEL, X(JJVXS), X(JJVSB), X(JJVXN),
     $              X(JJWTP), IX(JJISV), NVX, X(JJVAD), W(IRVL),
     $              W(IVRQ), W(IHRQ), W(IRVS), W(IVEC), NO)
C
C     (Give back W allotment)
      call WGIVE   (W, 'GADUMI')
C     !END
      call BYE ('GADUMI')
C
      return
      end
