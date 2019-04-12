      subroutine ARABIA
     $(I,J,XM,XR,NL,NLM,D)
C
C     Rudolf Loeser, 1987 Nov 06
C---- Prints, for ASIA.
C     !DASH
      save
C     !DASH
      real*8 D, XM, XR
      integer I, J, LUEO, NL, NLM
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, DPRAY, DARROUT, HI, BYE
C
C               XM(NL,NL), XR(NL-1,NL-1)
      dimension XM(*),     XR(*)
C
      call HI ('ARABIA')
C     !BEG
      if((I.eq.1).and.(I.eq.J)) then
        call DARROUT (LUEO, XM, NL, NL, 'The full matrix M')
      end if
C
      call LINER     (1, LUEO)
      write (LUEO,100) I,J
  100 format(' ','The reduced matrix, after eliminating row ',I3,
     $           ' and column ',I3)
      call DPRAY     (LUEO, XR, NLM, NLM)
C
      call LINER     (1, LUEO)
      write (LUEO,101) D
  101 format(' ','Determinant =',1PE24.16)
C     !END
      call BYE ('ARABIA')
C
      return
      end
