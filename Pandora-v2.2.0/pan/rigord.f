      subroutine RIGORD
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1995 Aug 14
C---- Allocates integer scratch storage for GRIS.
C     !DASH
      save
C     !DASH
      integer IN, IS, KM, MRR, MUX, N
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(49),KM )
      equivalence (JZQ(15),MRR)
C     !DASH
      external IGET, ILCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('RIGORD')
C     !BEG
      call IGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+KM*N
      MUX    = IN( 2)+KM*MRR
C
      call ILCK (MUX,CALLER)
C     !END
      call BYE ('RIGORD')
C
      return
      end
