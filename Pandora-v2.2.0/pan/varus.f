      subroutine VARUS
     $(TITLE,VALUE,LINE,IZZ,NO,KODE)
C
C     Rudolf Loeser, 1982 Mar 02
C---- ATOM data printing utility.
C     !DASH
      save
C     !DASH
      real*8 ARR, VALUE
      integer IZZ, KODE, NO
      character LINE*120, TITLE*(*)
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external FROG, FROCK, HI, BYE
C
      dimension ARR(2)
C
      call HI ('VARUS')
C     !BEG
      ARR(1) = ZERO
      ARR(2) = VALUE
      if(KODE.eq.1) then
        call FROCK (TITLE, ARR, 2, LINE, IZZ, NO)
      else
        call FROG  (TITLE, ARR, 2, LINE, IZZ, NO)
      end if
C     !END
      call BYE ('VARUS')
C
      return
      end
