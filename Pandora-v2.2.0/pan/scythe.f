      subroutine SCYTHE
     $(J,N,NL,XDEN,BDI,BRJ)
C
C     Rudolf Loeser, 1988 Jan 05
C---- Dumps, for "b from b-ratios" calculation.
C     (This is version 2 of SCYTHE.)
C     !DASH
      save
C     !DASH
      real*8 BDI, BRJ, XDEN
      integer J, LUEO, N, NL
      logical PRNTZ
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, OMAR, VECOUT, HI, BYE
C
C               XDEN(N), BDI(N), BRJ(N,NL)
      dimension XDEN(*), BDI(*), BRJ(*)
C
      data PRNTZ /.false./
C
      call HI ('SCYTHE')
C     !BEG
      call LINER  (5, LUEO)
      write (LUEO,100) J
  100 format(' ','********************',' For level',I3)
C
      call LINER  (1, LUEO)
      write (LUEO,101) J
  101 format(' ','Set of b-ratios (L/',I3,')')
      call OMAR   (LUEO, N, NL, BRJ, 'Level ',PRNTZ)
C
      call VECOUT (LUEO, XDEN, N, 'Denominator')
      call VECOUT (LUEO, BDI , N, 'BD'         )
C     !END
      call BYE ('SCYTHE')
C
      return
      end
