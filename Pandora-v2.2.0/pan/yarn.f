      subroutine YARN
     $(X,W,IW,XLB1,HN1,POPK,KHED)
C
C     Rudolf Loeser, 1980 Aug 20
C---- Controls final broadening calculation.
C     !DASH
      save
C     !DASH
      real*8 HN1, POPK, W, X, XLB1
      integer IFBRSW, IW, JJTE, JJXND, JJXNE, JJXNU, JJZ, JPROM, KHED,
     $        LUA, LUD
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ( 59),JJXND)
      equivalence (IZOQ( 26),JJXNU)
C     !DASH
      external CAROL, ABJECT, CHIMNEY, NUT, GOBY, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               XLB1(Li1len), HN1(N), POPK(N,NPOPS)
      dimension XLB1(*),      HN1(*), POPK(*)
C
      data IFBRSW /1/
C
      call HI ('YARN')
C     !BEG
C---- Set up output
      call CAROL    (KHED, LUD, LUA)
C---- Adjust Hydrogen Stark broadening switch
      call GOBY     (XLB1, JPROM)
C---- Compute and print final damping parameter
      call CHIMNEY  (X(JJZ), X(JJTE), X(JJXNE), JPROM, X(JJXND),
     $               X(JJXNU), HN1, IFBRSW, LUD, XLB1, POPK, W)
      if(LUA.gt.0) then
C----   Print profile analysis
        call ABJECT (LUA)
        call NUT    (X, W, IW, XLB1, JPROM, LUA)
      end if
C     !END
      call BYE ('YARN')
C
      return
      end
