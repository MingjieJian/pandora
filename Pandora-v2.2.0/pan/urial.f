      subroutine URIAL
     $(DUMP,YO,YA,YN,I4DFM,M)
C
C     Rudolf Loeser, 1997 Oct 22
C---- Selects and compares new sets of y.
C     (This is version 2 of URIAL.)
C     !DASH
      save
C     !DASH
      real*8 YA, YN, YO
      integer I, I4DFM, KODE, LUEO, M
      logical DUMP
      character CA*15, CD*15, CO*15, LAB*9
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external  LINER, NOTICE, SHIM, MOVE1, HI, BYE
      intrinsic min, max
C
C               YO(M), YA(M), YN(M)
      dimension YO(*), YA(*), YN(*)
C
      dimension LAB(2)
C
      data LAB /' original', 'alternate'/
C
      call HI ('URIAL')
C     !BEG
      KODE = max(min(I4DFM,2),1)
      if(DUMP) then
        call LINER    (3, LUEO)
        write (LUEO,100) LAB(KODE), I4DFM, LAB(1), LAB(2)
  100   format(' ','Comparison of "y"s obtained from the fourdiagonal ',
     $             'equations; "',A,'" selected (I4DFM =',I2,').'//
     $         ' ',10X,A9,29X,A9)
        call LINER    (1, LUEO)
        do 102 I = 1,M
          call NOTICE (15,YO(I),YA(I),CO,CA,CD)
          write (LUEO,101) I,CO,CD,CA
  101     format(' ',I4,A15,2(4X,A15))
          call SHIM   (I, 5, LUEO)
  102   continue
      end if
      if(KODE.eq.1) then
        call MOVE1    (YO, M, YN)
      else
        call MOVE1    (YA, M, YN)
      end if
C     !END
      call BYE ('URIAL')
C
      return
      end
