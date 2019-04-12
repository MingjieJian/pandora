      subroutine BRAVO
     $(X,W,IW,XCBL,DUMP)
C
C     Rudolf Loeser, 1980 Sep 23
C---- Computes PREF, the continuum opacity at REFLM Angstroms.
C     (This is version 4 of BRAVO.)
C     !DASH
      save
C     !DASH
      real*8 REFLM, W, X, XCBL
      integer IW, JJPRF, LGT, LUEO, N
      logical DMPR, DUMP, KILROY, PRNTZ
      character BLANK*1
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
      equivalence (IZOQ(126),JJPRF)
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
      equivalence (RZQ( 91),REFLM)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
C     !EJECT
      external RIPPLE, MASHED, OMAR, PLUSD, MESHED, PRIVET, ABORT,
     $         HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               XCBL(Miklen)
      dimension XCBL(*)
C
      data PRNTZ,DMPR /.false., .false./
C
      call HI ('BRAVO')
C     !BEG
      KILROY = .true.
      call RIPPLE   (X, W, IW, REFLM, 14, XCBL, KILROY, X(JJPRF), DMPR)
C
      if(DUMP) then
C----   Print
        call MESHED ('BRAVO', 2)
        write (LUEO,100) REFLM
  100   format(' ','Calculation of PREF, the opacity at REFLM =',
     $             1PE20.12,' Angstroms.')
        call OMAR   (LUEO, N, 1, X(JJPRF), BLANK, PRNTZ)
        call MASHED ('BRAVO')
      end if
C
C---- Check whether result is OK (all positive)
      call PLUSD    (X(JJPRF), 1, N, LGT)
      if(LGT.ne.N) then
        call MESHED ('BRAVO', 1)
        write (LUEO,101)
  101   format(' ','PREF has negative values.')
        call PRIVET (LUEO, X(JJPRF), N)
        call ABORT
      end if
C     !END
      call BYE ('BRAVO')
C
      return
      end
