      subroutine HUPA
     $(QELSM,ABD,IN,KODE)
C
C     Rudolf Loeser, 1981 May 19
C---- Identifies the element of the run, and computes abundances,
C     for HODRE.
C     Returns with KODE=1 if abundances were determined, =0 if not.
C     !DASH
      save
C     !DASH
      real*8 ABD
      integer IN, KODE
      logical GO
      character ABREV*2, QELSM*8
C     !DASH
      external ELIMA, HI, BYE
C
      dimension ABD(3)
C
      data GO /.false./
C
      call HI ('HUPA')
C     !BEG
      KODE = 0
C
C---- Check element symbol
      IN = 0
      if(QELSM(1:2).ne.'ZZ') then
C----   Element symbol is not bad - proceed
        ABREV = QELSM(1:2)
C----   Which element is it?
        if(ABREV.eq.'C ') then
C----     It is Carbon
          IN = 1
        else if(ABREV.eq.'O ') then
C----     It is Oxygen
          IN = 2
        else if(ABREV.eq.'H ') then
C----     It is Hydrogen
          IN = 3
        end if
      end if
C
C---- Get abundances
      call ELIMA (ABD, KODE, GO)
C     !END
      call BYE ('HUPA')
C
      return
      end
