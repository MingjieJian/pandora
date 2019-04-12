      subroutine IDOL
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 18 Feb 70
C---- Allocates scratch storage for DIAMOND.
C     !DASH
      save
C     !DASH
      integer IN, IS, MEX, MIX, MUX, N, NL, NNT, NT
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ( 5),NT )
C
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
C     !DASH
      external  WGET, WLCK, HI, BYE
      intrinsic max
C
      dimension IN(*)
C
      call HI ('IDOL')
C     !BEG
      call WGET (IS ,CALLER)
C
      MIX = max(NT,NL)
      MEX = MIX*N
      NNT = NT*N
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NNT
      IN( 3) = IN( 2)+MEX
      IN( 4) = IN( 3)+N
      MUX    = IN( 4)+LI1LEN
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('IDOL')
C
      return
      end
