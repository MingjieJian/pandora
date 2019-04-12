      subroutine STYRAL
     $(I,J,ITAU,ASTAR,CHI,Y)
C
C     Rudolf Loeser, 2003 Mar 14
C---- Computes Y, for LYRA.
C     !DASH
      save
C     !DASH
      real*8 ASTAR, CHI, Y, ZERO
      integer I, IJ, ITAU, J, N
      logical OK
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external INDXNT, HI, BYE
C
C               ASTAR(N,NT), CHI(N,NT)
      dimension ASTAR(N,*),  CHI(N,*)
C
      call HI ('STYRAL')
C     !BEG
      Y = ZERO
C
      call INDXNT (I, J, OK, IJ)
      if(OK) then
        Y = ASTAR(ITAU,IJ)*CHI(ITAU,IJ)
      end if
C     !END
      call BYE ('STYRAL')
C
      return
      end
