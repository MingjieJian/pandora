      subroutine RUM
     $(X,IX,FR,VEC,WT)
C
C     Rudolf Loeser, 1980 Oct 23
C---- Post-read defaults for the third batch of input data.
C     (This is version 2 of RUM.)
C     !DASH
      save
C     !DASH
      real*8 COMU, FR, VEC, WT, X
      integer IX, JJCVX, JJFIW, JJHND, JJISV, JJMU, JJMUF, JJVXI, JJVXN,
     $        JJVXS, JJWTP, KMUS, L, LF, N
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(  4),JJMU )
      equivalence (IZOQ(115),JJMUF)
      equivalence (IZOQ(229),JJFIW)
      equivalence (IZOQ(164),JJVXN)
      equivalence (IZOQ(186),JJCVX)
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ(129),JJVXS)
      equivalence (IZOQ(202),JJVXI)
      equivalence (IZOQ(120),JJWTP)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ( 13),JJISV)
C     !EJECT
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 7),L  )
      equivalence (JZQ(19),LF )
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
      equivalence (RZQ(139),COMU )
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
      equivalence (LEST(31),KMUS )
C     !DASH
C     !EJECT
      external PRATE, FRATE, UMBER, ABNER, HI, BYE
C
      dimension X(*), IX(*)
C
C               VEC(NFH), FR(N), WT(NVX)
      dimension VEC(*),   FR(*), WT(*)
C
      call HI ('RUM')
C     !BEG
C---- Set up EMU as a subset of EMUF
C     (MUST precede ABNER)
      call PRATE (X(JJMUF), LF, X(JJMU), L, KMUS)
      if((KMUS.le.0).and.(LF.ge.1)) then
        L = 1
        X(JJMU) = X(JJMUF)
      end if
C
C---- Check COMU amd mu tables
      call ABNER (COMU, L, X(JJMU), LF, X(JJMUF))
C
C---- Flux integration weights
      call UMBER (LF, X(JJMUF), X(JJFIW))
C
C---- Generate "additional velocities" (and VXS, if needed)
      call FRATE (X, X(JJVXN), N, X(JJCVX), IX(JJISV), VEC, FR,
     $            X(JJVXI), X(JJVXS), WT, X(JJWTP))
C     !END
      call BYE ('RUM')
C
      return
      end
