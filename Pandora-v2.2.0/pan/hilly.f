      subroutine HILLY
     $(N,XLM,TE,H1,HN,SHL,WLIN,PFAC,EX,XNUM,XDEN)
C
C     Rudolf Loeser, 2002 Sep 24
C---- Computes a value of Hydrogen Lyman N/1 line background source
C     function.
C     !DASH
      save
C     !DASH
      real*8 CON6, EL, EM, ENN2, EX, H1, HN, ONE, PFAC, R, SHL, TE,
     $       WLIN, XDEN, XLM, XNUM
      integer N
      logical KILROY, OLD
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external RIGEL, KITH, DIVIDE, HUNK, HI, BYE
C
      data KILROY,OLD /.true., .false./
C
      call HI ('HILLY')
C     !BEG
      if(KILROY) then
        KILROY = .false.
        call RIGEL  (6, CON6)
      end if
C
      call DIVIDE   (H1, HN, R)
      ENN2 = N**2
      PFAC = ENN2*R
      call KITH     (N, WLIN)
C
      if(OLD) then
        call HUNK   (TE, XLM,  2, EM)
        call HUNK   (TE, WLIN, 2, EL)
        EX   = exp(EM-EL)
C
        XDEN = PFAC*EX-ONE
        call DIVIDE (CON6, (XLM**3), XNUM)
      else
        XDEN = PFAC-ONE
        call DIVIDE (CON6, (WLIN**3), XNUM)
      end if
C
      call DIVIDE   (XNUM, XDEN, SHL)
C     !END
      call BYE ('HILLY')
C
      return
      end
