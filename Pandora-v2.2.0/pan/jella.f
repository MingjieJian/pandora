      subroutine JELLA
     $(XLM,ITAU,T,V,H1,NLY,LYLINO)
C
C     Rudolf Loeser, 2002 Sep 25
C---- Prints details, for AMETH.
C     (This is version 4 of JELLA.)
C     !DASH
      save
C     !DASH
      real*8 H1, T, V, XLM
      integer ITAU, LUEO, LYLINO, NLY
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external DASHER, LINER, HI, BYE
C
      call HI ('JELLA')
C     !BEG
      call LINER    (2, LUEO)
      call DASHER   (LUEO)
      call LINER    (1, LUEO)
      write (LUEO,100) XLM,ITAU
  100 format(' ','Calculation of higher H Ly lines opacities at ',
     $           'LM =',1PE19.12,' and i =',I5)
      call LINER    (1, LUEO)
C
      write (LUEO,101) T,V,H1,NLY,LYLINO
  101 format(' ','TE =',1PE15.8,5X,'VM =',E16.8,5X,'H1 =',E15.8,5X,
     $           'NLY =',I3,' (',I2,')')
C     !END
      call BYE ('JELLA')
C
      return
      end
