      subroutine CHURN
     $(X,IX)
C
C     Rudolf Loeser, 1997 Jun 13
C---- Sets up    real*8 general and scratch storage in  X, and
C             integer*4                                IX.
C     !DASH
      save
C     !DASH
      real*8 X
      integer IPEX, IX, LUEO
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
C
C---- WORLD       as of 2002 Jun 04
C
      integer     LISTK
      parameter   (LISTK = 100)
      integer     ISTCK,INEXT,ILMIT,IUMAX,IUKNT
      dimension   ISTCK(LISTK)
      common      /WORLD/ ISTCK,INEXT,ILMIT,IUMAX,IUKNT
C     Management of floating point working/scratch storage in X
C     - ISTCK is the allocation stack
C     - INEXT is the stack index for the next allocation
C     - ILMIT is the length of X
C     - IUMAX and IUKNT are cumulative usage statistics.
C     .
C---- WSAFE       as of 1997 Oct 31
      real*8      XSIGNAL
      common      /WSAFE/ XSIGNAL
C     .
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
C
C---- IWORLD      as of 2002 Jun 04
C
      integer     LJSTK
      parameter   (LJSTK = 100)
      integer     JSTCK,JNEXT,JLMIT,JUMAX,JUKNT
      dimension   JSTCK(LJSTK)
      common      /IWORLD/ JSTCK,JNEXT,JLMIT,JUMAX,JUKNT
C     Management of integer working/scratch storage in IX
C     - JSTCK is the allocation stack
C     - JNEXT is the stack index for the next allocation
C     - JLMIT is the length of IX
C     - JUMAX and JUKNT are cumulative usage statistics.
C     .
C---- IWSAFE      as of 1997 Oct 31
      integer     IXSIGNL
      common      /IWSAFE/ IXSIGNL
C     .
C     !EJECT
C---- SALLOC      as of 1997 Oct 31
      integer     ISALLOC
      common      /SALLOC/ ISALLOC
C     Index of first allocatable cell in WORLD and IWORLD
C     (To reserve initial cells for error checking).
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
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
      equivalence (KZQ( 18),IPEX )
C     !DASH
      external MANAGE, MINIGE, ZEROD, SETD, ZEROI, SETI, MESHED, LINER,
     $         MASHED, HI, BYE
C
      dimension X(*), IX(*)
C     !EJECT
C
      call HI ('CHURN')
C     !BEG
      call MANAGE   (IZOQ, ISALLOC, IBSCR)
      call ZEROD    (X , 1, IBSCR)
      call SETD     (X , 1, (ISALLOC-1), XSIGNAL)
      IUMAX = 0
      IUKNT = 0
      INEXT = 1
      ISTCK(INEXT) = IBSCR+1
C
      call MINIGE   (JZOQ, ISALLOC, JBSCR)
      call ZEROI    (IX, 1, JBSCR)
      call SETI     (IX, 1, (ISALLOC-1), IXSIGNL)
      JUMAX = 0
      JUKNT = 0
      JNEXT = 1
      JSTCK(JNEXT) = JBSCR+1
C
      if((IPEX.lt.0).or.(IPEX.eq.23)) then
        call MESHED ('CHURN', 2)
        write (LUEO,100) IBSCR
  100   format(' ','manager: IBSCR =',I12)
        call LINER  (1, LUEO)
        write (LUEO,101) IZOQ
  101   format(10(' ',10I12/))
        call LINER  (2, LUEO)
        write (LUEO,102) JBSCR
  102   format(' ','miniger: JBSCR =',I12)
        call LINER  (1, LUEO)
        write (LUEO,101) JZOQ
        call MASHED ('CHURN')
      end if
C     !END
      call BYE ('CHURN')
C
      return
      end
