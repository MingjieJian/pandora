      subroutine RAVI
     $(X,IX,W,IW,XNDO,XND,EDITED,KFCE,OFCE)
C
C     Rudolf Loeser, 2003 Apr 25
C---- Examines, and perhaps edits, number densities.
C     !DASH
      save
C     !DASH
      real*8 ONE, W, X, XND, XNDO
      integer I, IN, IND, IQ, IQEND, IS, ITND, IW, IX, JEDIT, JJFCE,
     $        JJKIJ, JJMIJ, JJP, JJPCE, JJSET, JJZ, JNEDP, KFCE, LUEO,
     $        MO, MOX, N, NL, NT
      logical DUMP, EDITED, KILROY, MDFD, OFCE
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ( 5),NT )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 27),JJP  )
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(248),JJFCE)
      equivalence (IZOQ(249),JJPCE)
      equivalence (IZOQ(253),JJSET)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  2),JJKIJ)
      equivalence (JZOQ( 16),JJMIJ)
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
      equivalence (KZQ(193),JNEDP)
      equivalence (KZQ(194),JEDIT)
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
      equivalence (LUNITS( 6),LUEO )
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
      equivalence (IQQ(306),IQEND)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external MASHED, MOVED, MOVE1, LINER, OLEG, WGIVE, KONSTD, PETER,
     $         SOD, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XNDO(N,NL), XND(N,NL)
      dimension XNDO(*),    XND(N,*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),IQ    ),(IN( 2),IND   ),(IN( 3),ITND  )
C     !EJECT
C
      call HI ('RAVI')
C     !BEG
C     (Get, and allocate, W allotment)
      call OLEG        (IN, IS , MOX, 'RAVI')
C
      call MOVE1       (XNDO, (N*NL), XND)
      EDITED = .false.
C
      if(KFCE.gt.0) then
C----   Examine number densities, and perhaps adjust CE-enhancements
        call PETER     (N, NL, NT, X(JJSET), IX(JJKIJ), IX(JJMIJ),
     $                  X(JJPCE), X(JJZ), X(JJFCE), W, IW)
      end if
C
C---- Check status of FCE
      call KONSTD      (X(JJFCE), 1, (N*NT), ONE, OFCE)
C
      if((IQEND.gt.0).and.(JEDIT.le.N)) then
C----   Edit number densities, to prevent line source functions < 0
        DUMP   = (JNEDP.gt.0).and.(MO.gt.0)
        KILROY = .true.
        do 100 I = JEDIT,N
          call MOVED   (XND(I,1), N, NL, W(IND), 1, NL)
          call SOD     (NL, IX(JJKIJ), W(IND), X(JJSET), W(IQ), X(JJP),
     $                  W(ITND), MDFD, I, DUMP, KILROY, 'RAVI')
          if(MDFD) then
            EDITED = .true.
            call MOVED (W(IND), 1, NL, XND(I,1), N, NL)
          end if
  100   continue
        if(.not.KILROY) then
          call MASHED  ('RAVI')
        end if
      end if
C
C     (Give back W allotment)
      call WGIVE       (W, 'RAVI')
C     !END
      call BYE ('RAVI')
C
      return
      end
