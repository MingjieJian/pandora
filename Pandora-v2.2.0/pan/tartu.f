      subroutine TARTU
     $(IDIR,XLMB,NB)
C
C     Rudolf Loeser, 1996 Feb 29
C---- Dumps, for WAGRIN.
C     !DASH
      save
C     !DASH
      real*8 XLMB
      integer IDIR, LUEO, NB
      character DIR*4
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, DPRIVE, HI, BYE
C
C               XLMB(NB)
      dimension XLMB(*)
C
      dimension DIR(3)
C
      data DIR /'blue', ' ', 'red'/
C
      call HI ('TARTU')
C     !BEG
      call LINER  (2, LUEO)
      write (LUEO,100) DIR(IDIR+2)
  100 format(' ','Batch of wavelengths, ',A,' direction')
      call DPRIVE (LUEO, XLMB, NB)
C     !END
      call BYE ('TARTU')
C
      return
      end
