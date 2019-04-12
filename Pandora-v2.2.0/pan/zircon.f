      subroutine ZIRCON
     $(I,BDI,BDIRECT,W,Z)
C
C     Rudolf Loeser, 2003 Jun 19
C---- Prints for MULIAK.
C     (This is version 5 of ZIRCON.)
C     !DASH
      save
C     !DASH
      real*8 BDI, BDIRECT, W, Z, ZERO
      integer I, MO
      character BL*2, CA*14, CB*14, CD*14, MA*2, MB*2, SG*2
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external NOTICE, SHIM, HI, BYE
C
      data BL,SG /'  ', '**'/
C
      call HI ('ZIRCON')
C     !BEG
      call NOTICE (14, BDI, BDIRECT, CA, CB, CD)
C
      MA = BL
      if(BDI.lt.ZERO) then
        MA = SG
      end if
      MB = BL
      if(BDIRECT.lt.ZERO) then
        MB = SG
      end if
C
      write (MO,100) I,CA,MA,CB,MB,CD,W,Z
  100 format(' ',I4,2X,2(A14,A2),A14,1P2E16.6)
C
      call SHIM   (I, 5, MO)
C     !END
      call BYE ('ZIRCON')
C
      return
      end
