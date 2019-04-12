      subroutine HODRU
     $(Z,KZXST,R1N,N,R1GD)
C
C     Rudolf Loeser, 1980 Feb 20
C---- Sets up default R1GD value.
C     !DASH
      save
C     !DASH
      real*8 R1GD, R1N, Z, ZERO
      integer KZXST, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external HI, BYE
C
C               Z(N)
      dimension Z(*)
C
      call HI ('HODRU')
C     !BEG
      R1GD = R1N
      if(KZXST.gt.0) then
        if(R1GD.eq.ZERO) then
          R1GD = Z(N)
        end if
        R1GD = R1GD+Z(N)
      end if
C     !END
      call BYE ('HODRU')
C
      return
      end
