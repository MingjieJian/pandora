      subroutine SIGH
     $(X,W,XND,XNK,PRNT)
C
C     Rudolf Loeser, 1980 Aug 13
C---- Computes starting default number densities.
C     (This is version 2 of SIGH.)
C     !DASH
      save
C     !DASH
      real*8 W, X, XND, XNK
      integer IABDEL, IABDI, IN, IQLND, IS, ISO, ISUM, J, JJBDI, MOX, N,
     $        NL, NO
      logical DUMP, KILROY, PRNT, ULN, ZND, ZNK
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 44),JJBDI)
C
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ(129),IQLND)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C     !DASH
C     !EJECT
      external NAUGHTD, JASON, KULLOCH, ICECLAN, PRIAM, MUSHED, MASHED,
     $         ULAN, LINER, WGIVE, HI, BYE
C
      dimension X(*), W(*)
C
C               XND(N,NL), XNK(N)
      dimension XND(N,*),  XNK(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IABDEL),(IN( 2),IABDI ),(IN( 3),ISUM  ),(IN( 4),ISO   )
C
      call HI ('SIGH')
C     !BEG
C     (Get, and allocate, W allotment)
      call ICECLAN    (IN, IS, MOX, 'SIGH')
C
      ULN    = .false.
      PRNT   = .false.
      DUMP   = IQLND.gt.0
      KILROY = .true.
C---- Continuum
      call NAUGHTD    (XNK, 1, N, ZNK)
      if(ZNK) then
C----   Compute
        call ULAN     (X, N, W(IABDEL), W(IABDI))
        ULN = .true.
        if(DUMP) then
          call MUSHED ('SIGH', 2, KILROY)
        end if
        call JASON    (X, 1, DUMP, X(JJBDI), W(IABDI), XNK, W(ISUM),
     $                 W(ISO))
        PRNT = .true.
      end if
C     !EJECT
C---- Loop over all levels
      do 100 J = 1,NL
C----   Check whether ND for this level exists
        call NAUGHTD    (XND(1,J), 1, N, ZND)
        if(ZND) then
C----     Compute
          if(.not.ULN) then
            call ULAN   (X, N, W(IABDEL), W(IABDI))
            ULN = .true.
          end if
          if(DUMP) then
            call MUSHED ('SIGH', 2, KILROY)
          end if
          call KULLOCH  (X, J, 1, DUMP, X(JJBDI), W(IABDI), XND(1,J),
     $                   W(ISUM), W(ISO))
          PRNT = .true.
        end if
  100 continue
      if(.not.KILROY) then
        call MASHED     ('SIGH')
      end if
C
C     (Give back W allotment)
      call WGIVE        (W, 'SIGH')
C     !END
      call BYE ('SIGH')
C
      return
      end
