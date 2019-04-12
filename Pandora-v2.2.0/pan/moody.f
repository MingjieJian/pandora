      subroutine MOODY
C
C     Rudolf Loeser, 1999 Dec 07
C---- Allocates the parts of the Line Intensity Data Block.
C     !DASH
      save
C     !COM
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
C     !DASH
      external MOOD1, MOOD2, MOOD3, HI, BYE
C
      call HI ('MOODY')
C     !BEG
      call MOOD1 (MML, LI1LEN)
      call MOOD2 (MMP, LI2LEN)
      call MOOD3 (MMT, LI3LEN)
C     !END
      call BYE ('MOODY')
C
      return
      end
