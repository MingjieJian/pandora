      subroutine URRACA
     $(NL,QNAME,LIJ)
C
C     Rudolf Loeser, 1996 Feb 21
C---- Reads single rate switch for a given transition
C     (the value may only be =1 or =2).
C     !DASH
      save
C     !DASH
      real*8 dummy
      integer IL, IU, IUL, LIJ, LRIJ, LUEO, NL
      character QNAME*8, qummy*8
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MINT, MUSTARD, MESHED, ABORT, INDXUL, HI, BYE
C
C               LIJ(MUL)
      dimension LIJ(*)
C
      call HI ('URRACA')
C     !BEG
      call MINT     (QNAME, IU)
      call MINT     (QNAME, IL)
      call MUSTARD  (QNAME, dummy, LRIJ, qummy, 1, 3)
C
      if((LRIJ.lt.1).or.(LRIJ.gt.2)) then
        call MESHED ('URRACA', 1)
        write (LUEO,100) IU,IL,LRIJ
  100   format(' ','Trouble reading single rate switch KRATE(',I2,
     $             '/',I2,') =',I5/
     $         ' ','The value may only be either 1 (= use net rate) ',
     $             'or 2 (= use single rate).')
        call ABORT
      end if
C
      call INDXUL   (IU, IL, IUL)
      LIJ(IUL) = LRIJ
C     !END
      call BYE ('URRACA')
C
      return
      end
