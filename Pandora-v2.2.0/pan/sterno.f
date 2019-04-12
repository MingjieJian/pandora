      subroutine STERNO
     $(I,J,ITAU,SA,SAV)
C
C     Rudolf Loeser, 2003 Apr 08
C---- Gets SAV: a specific value of SA, for LYRA.
C     !DASH
      save
C     !DASH
      real*8 SA, SAV, ZERO
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
C               SA(N,NT)
      dimension SA(N,*)
C
      call HI ('STERNO')
C     !BEG
      SAV = ZERO
C
      call INDXNT (I, J, OK, IJ)
      if(OK) then
        SAV = SA(ITAU,IJ)
      end if
C     !END
      call BYE ('STERNO')
C
      return
      end
