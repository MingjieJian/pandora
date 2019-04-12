      subroutine HIMILCO
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1982 Feb 02
C---- Allocates scratch storage for SALAMIS.
C     !DASH
      save
C     !DASH
      integer IN, IS, LG, MUX, N, NNLG
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(34),LG )
C
C---- ORIBLOC     as of 2005 Jan 21
      integer     LODLEN,LOD
      dimension   LOD(17)
      common      /ORIBLOC/ LODLEN,LOD
C     This is the "ORION" Data Block index, for the calculation
C     of line source functions in an expanding atmosphere.
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('HIMILCO')
C     !BEG
      call WGET (IS,  CALLER)
C
      NNLG = (N**2)*LG
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+NNLG
      IN( 3) = IN( 2)+NNLG
      IN( 4) = IN( 3)+NNLG
      IN( 5) = IN( 4)+NNLG
      IN( 6) = IN( 5)+N
      IN( 7) = IN( 6)+LODLEN
      MUX    = IN( 7)+NNLG
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('HIMILCO')
C
      return
      end
