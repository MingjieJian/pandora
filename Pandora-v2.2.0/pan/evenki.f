      subroutine EVENKI
     $(N,WZ,C,LABEL)
C
C     Rudolf Loeser, 1981 Nov 03
C---- Prints, for GINA.
C     !DASH
      save
C     !DASH
      real*8 C, WZ
      integer LUEO, N
      character LABEL*27
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external VECOUT, ARROUT, HI, BYE
C
C               WZ(N,N), C(N)
      dimension WZ(*),   C(*)
C
      call HI ('EVENKI')
C     !BEG
      call VECOUT (LUEO, C , N,    ('C for'//LABEL(1:3)))
      call ARROUT (LUEO, WZ, N, N, LABEL                )
C     !END
      call BYE ('EVENKI')
C
      return
      end
