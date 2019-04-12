      subroutine CARDIFF
     $(IVLSW)
C
C     Rudolf Loeser, 1998 Mar 05
C---- Prints the heading, for SWASH.
C     !DASH
      save
C     !DASH
      integer IVLSW, K, LUEO
      character LAB*7
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external  LINER, HI, BYE
      intrinsic min, max
C
      dimension LAB(3)
C
      data LAB /'outward', '       ', 'inward '/
C
      call HI ('CARDIFF')
C     !BEG
      K = min(max((IVLSW+2),1),3)
C
      call LINER (2, LUEO)
      write (LUEO,100) LAB(K)
  100 format(' ','Exponential solution: ',A7,60X,'("New method for ',
     $           'treating mass flows")')
C     !END
      call BYE ('CARDIFF')
C
      return
      end
