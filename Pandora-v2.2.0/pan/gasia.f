      subroutine GASIA
     $(XLM,ITAU)
C
C     Rudolf Loeser, 2003 Jan 08
C---- Prints dump header, for SAIGA.
C     !DASH
      save
C     !DASH
      real*8 XLM
      integer ITAU, LUEO
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, DASHER, HI, BYE
C
      call HI ('GASIA')
C     !BEG
      call LINER    (2, LUEO)
      call DASHER   (LUEO)
      call LINER    (1, LUEO)
      write (LUEO,100) XLM,ITAU
  100 format(' ','Calculation of highest H Ly lines emission at LM =',
     $           1PE20.12,' and i =',I5)
C     !END
      call BYE ('GASIA')
C
      return
      end
