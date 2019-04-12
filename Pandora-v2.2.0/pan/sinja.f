      subroutine SINJA
     $(STKFN,A,PHI,OPAC,SCAT)
C
C     Rudolf Loeser, 2002 Sep 20
C---- Debug printout for BIE.
C     !DASH
      save
C     !DASH
      real*8 A, OPAC, PHI, SCAT, STKFN
      integer LUEO
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, HI, BYE
C
      call HI ('SINJA')
C     !BEG
      call LINER (1, LUEO)
      write (LUEO,100) STKFN,A,PHI,OPAC,SCAT
  100 format(' ','core',10X,'STKFN =',1PE15.7,5X,'a =',E15.7,5X,
     $           'phi(a,x) =',E15.8,5X,'OPAC =',E15.8,5X,'SCAT =',
     $           E10.2)
C     !END
      call BYE ('SINJA')
C
      return
      end
