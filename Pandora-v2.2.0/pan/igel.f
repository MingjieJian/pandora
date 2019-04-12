      subroutine IGEL
     $(CHECK,J,N,NCK,ITMX,JMIN,JMAX,PLOT)
C
C     Rudolf Loeser, 1984 May 04
C---- Checks whether to plot the current set of CHECKs, and
C     sets range indices.
C     !DASH
      save
C     !DASH
      real*8 CHECK, CRIT, ONE
      integer I, IFLG, ITER, ITMX, J, JMAX, JMIN, N, NCK
      logical PLOT
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  COMPD, HI, BYE
      intrinsic max
C
C               CHECK(N,NCK,ITMX)
      dimension CHECK(N,NCK,*)
C
      data      CRIT /1.D-3/
C
      call HI ('IGEL')
C     !BEG
      PLOT = .false.
C
      do 101 ITER = 1,ITMX
        do 100 I = 1,N
          call COMPD (CHECK(I,J,ITER),ONE,CRIT,IFLG)
          if(IFLG.ne.0) then
            PLOT = .true.
            goto 102
          end if
  100   continue
  101 continue
C
  102 continue
      if(PLOT) then
        JMAX = ITMX
        JMIN = max((JMAX-25),1)
      end if
C     !END
      call BYE ('IGEL')
C
      return
      end
