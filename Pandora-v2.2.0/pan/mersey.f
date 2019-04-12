      subroutine MERSEY
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1983 Jul 21
C---- Controls the line-related processing of an overall iteration.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer ISUB, ITER, IW, IX, MO, NOION, NT
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 5),NT )
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
      equivalence (KZQ(  4),ISUB )
      equivalence (KZQ( 94),NOION)
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
      equivalence (LEST( 3),ITER )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external ORRIN, URE, REDE, EPHOR, HISS, TAMAR, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C     !EJECT
C
      call HI ('MERSEY')
C     !BEG
      if((NT.gt.0).and.(NOION.le.0)) then
C
C----   Passive Jnu shuffling
        call URE     (X, IX, W, IW)
C----   Initialize Bs, NDs & SETs
        call REDE    (X, IX, W, IW)
C----   Damping Parameter
        call ORRIN   (X, IX, W, IW)
C
C----   Sub-iterations: Line Source Function, RHOs, and B-ratios
        do 100 ITER = 1,ISUB
          call EPHOR (MO)
          call HISS  (X, IX)
          call TAMAR (X, IX, W, IW)
  100   continue
        ITER = 0
C
      end if
C     !END
      call BYE ('MERSEY')
C
      return
      end
