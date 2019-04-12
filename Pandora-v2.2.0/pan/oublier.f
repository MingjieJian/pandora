      subroutine OUBLIER
     $(XLM,ITAU,T,V,H1,XNU2,ROOT,CL2,A21,A21S,XLIM,X)
C
C     Rudolf Loeser, 2002 Sep 20
C---- Debug printout for BIE.
C     (This is version 2 of OUBLIER.)
C     !DASH
      save
C     !DASH
      real*8 A21, A21S, CL2, H1, ROOT, T, V, X, XLIM, XLM, XNU2
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
      call HI ('OUBLIER')
C     !BEG
      call LINER    (1, LUEO)
      call DASHER   (LUEO)
      call LINER    (1, LUEO)
      write (LUEO,100) XLM,ITAU,T,V,H1
  100 format(' ','Details of H Lyman alpha opacity at LM =',1PE19.12,
     $           ' and i =',I5//
     $       ' ','TE =',E15.8,5X,'VM =',E15.7,5X,'N1 =',E15.8)
      call LINER    (1, LUEO)
      write (LUEO,101) XNU2,ROOT,CL2,A21,A21S,X,XLIM
  101 format(' ','NU2 =',1PE15.8,5X,'ROOT =',E15.8,5X,'L2 =',E15.8,
     $           5X,'A21 =',E15.8,5X,'sA2k =',E15.8//
     $       ' ','x =',E15.8,5X,'(core size =',E15.8,')')
      call LINER    (1, LUEO)
      write (LUEO,102)
  102 format(' ','(Note: DR-calculation parameters were printed ',
     $           'earlier in the printout section INPUT.)')
C     !END
      call BYE ('OUBLIER')
C
      return
      end
