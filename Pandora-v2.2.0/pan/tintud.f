      subroutine TINTUD
     $(XLM,CORE,ITYPE)
C
C     Rudolf Loeser, 2002 Nov 20
C---- Modifies ITYPE for TRUDE, if necessary.
C     !DASH
      save
C     !DASH
      real*8 CORE, XLM
      integer IFLG, ITYPE
      logical PRNT
C     !COM
C---- ISOLA       as of 1997 Nov 19
      real*8      WAVEDEL
      common      /ISOLA/ WAVEDEL
C     Two Continuum Wavelength values are "equal" if their
C     relative difference is less than WAVEDEL.
C     .
C     !DASH
      external COMPD, DINAR, HI, BYE
C
      call HI ('TINTUD')
C     !BEG
      call COMPD (XLM, CORE, WAVEDEL, IFLG)
      if(IFLG.eq.0) then
        call DINAR (PRNT)
C
        if(ITYPE.eq.4) then
          if(PRNT) then
            ITYPE = 24
          else
            ITYPE = 25
          end if
C
        else if(ITYPE.eq.16) then
          if(PRNT) then
            ITYPE = 18
          else
            ITYPE = 26
          end if
C
        end if
      end if
C     !END
      call BYE ('TINTUD')
C
      return
      end
