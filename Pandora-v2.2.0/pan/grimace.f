      subroutine GRIMACE
     $(I,N,J,K,PHI,DL,A,XNE,IU,IL,R,OFF)
C
C     Rudolf Loeser, 2006 Jun 29
C---- Checks and fixes a PHI value, for FASAN.
C     !DASH
      save
C     !DASH
      real*8 A, DL, OFF, PHI, R, XNE, ZERO
      integer I, IL, IU, J, K, LUEO, N
      logical YES
C     !COM  or  !DASH
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external IS_BAD, MESHED, MASHED, HI, BYE
C
      call HI ('GRIMACE')
C     !BEG
      call IS_BAD   (PHI, YES)
      if(YES) then
        call MESHED ('GRIMACE', 1)
        write (LUEO,100) IU,IL,PHI,I,N,J,K
  100   format(' ','Bad convolved PHI value for transition (',I2,
     $             '/',I2,')'/
     $         ' ','PHI =',1PE16.9,5X,'N =',I5,' (',I5,')     J =',
     $             I4,' (',I4,')')
        write (LUEO,101) DL,A,XNE,R,OFF
  101   format(' ','DL =',1PE12.4,5X,'A =',E12.4,5X,'NE =',E12.4,5X,
     $             'R =',E12.4,5X,'OFF =',E12.4)
        call MASHED ('GRIMACE')
C
        PHI = ZERO
      end if
C     !END
      call BYE ('GRIMACE')
C
      return
      end
