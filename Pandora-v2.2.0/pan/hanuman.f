      subroutine HANUMAN
     $(XNE,N,CRIT,INDEX)
C
C     Rudolf Loeser, 1992 Feb 06
C---- Finds a default value of the Stark splitting NE-index, for CHALK.
C     (This is version 3 of HANUMAN.)
C     !DASH
      save
C     !DASH
      real*8 CRIT, DLEFT, DRITE, ONE, XNE
      integer I, INDEX, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  HI, BYE
      intrinsic sign
C
C               XNE(N)
      dimension XNE(*)
C
      call HI ('HANUMAN')
C     !BEG
      if((XNE(N).ge.XNE(1)).and.(XNE(N).le.CRIT)) then
        INDEX = N
      else
C
        do 100 I = (N-1),1,-1
          INDEX = I
          DLEFT = XNE(INDEX  )-CRIT
          DRITE = XNE(INDEX+1)-CRIT
          if(sign(ONE,DLEFT).ne.sign(ONE,DRITE)) then
            go to 101
          end if
  100   continue
      end if
C
  101 continue
C     !END
      call BYE ('HANUMAN')
C
      return
      end
