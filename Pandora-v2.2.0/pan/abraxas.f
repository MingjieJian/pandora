      subroutine ABRAXAS
     $(XIX,KX)
C
C     Rudolf Loeser, 1990 Feb 06
C---- Makes sure that XIX is a proper full-profile frequency-values set.
C     (This is version 3 of ABRAXAS.)
C     !DASH
      save
C     !DASH
      real*8 ONE, XIX, ZERO
      integer J, KX
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external HALT, HI, BYE
C
C               XIX(KM)
      dimension XIX(*)
C
      call HI ('ABRAXAS')
C     !BEG
      if(XIX(1).gt.ZERO) then
        write (MSSLIN(1),100) 1,XIX(1)
  100   format('XIX(',I5,') =',1PE16.8,', which "can''t happen".')
        call HALT ('ABRAXAS', 1)
C
      else if(XIX(1).eq.ZERO) then
        KX = KX+1
        do 101 J = KX,2,-1
          XIX(J) = XIX(J-1)
  101   continue
        XIX(1) = -ONE
      end if
C
      if(XIX(KX).lt.ZERO) then
        write (MSSLIN(1),100) KX,XIX(KX)
        call HALT ('ABRAXAS', 1)
C
      else if(XIX(KX).eq.ZERO) then
        KX = KX+1
        XIX(KX) = ONE
      end if
C     !END
      call BYE ('ABRAXAS')
C
      return
      end
