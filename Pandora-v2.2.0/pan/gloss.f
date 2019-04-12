      subroutine GLOSS
     $(ZOLD,ZNEW,ZWAT,N,W,WEIT)
C
C     Rudolf Loeser, 1980 Feb 06
C---- Weighting of recalculated Z.
C     (This is version 2 of GLOSS.)
C     !DASH
      save
C     !DASH
      real*8 W, WEIT, ZERO, ZNEW, ZOLD, ZWAT, dummy
      integer J, KLOG, KMSS, M, MODE, N
      character qummy*8
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external WEITER, HI, BYE
C
C               ZOLD(N), ZNEW(N), ZWAT(N), WEIT(N)
      dimension ZOLD(*), ZNEW(*), ZWAT(*), WEIT(*)
C
      data KLOG,MODE,KMSS /1, 0, 0/
C
      call HI ('GLOSS')
C     !BEG
      if(ZNEW(1).eq.ZERO) then
C----   Skip Z = 0
        ZWAT(1) = ZERO
        M = N-1
        J = 2
      else
C----   Do it all
        M = N
        J = 1
      end if
C
      call WEITER (ZWAT(J),ZNEW(J),ZOLD(J),dummy,W,M,KLOG,MODE,KMSS,
     $             qummy,WEIT)
C     !END
      call BYE ('GLOSS')
C
      return
      end
