      subroutine MASSAGE
     $(KODE,IPACK,IOVR,IHSE,ILYM)
C
C     Rudolf Loeser, 1975 Apr 29
C---- Packs (KODE=1) or unpacks (KODE=2) iteration counters.
C     (This is version 2 of MASSAGE.)
C     !DASH
      save
C     !DASH
      integer IHSE, ILYM, IOVR, IPACK, ISIG, KODE, LCONS
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external  HALT, HI, BYE
      intrinsic mod
C
      data LCONS /512/
C
      call HI ('MASSAGE')
C     !BEG
      if(KODE.eq.1) then
        IPACK = (ILYM+(IHSE*LCONS))*LCONS+IOVR
      else if(KODE.eq.2) then
        ISIG = IPACK
        IOVR = mod(ISIG,LCONS)
        ISIG = ISIG/LCONS
        ILYM = mod(ISIG,LCONS)
        IHSE = ISIG/LCONS
      else
        write (MSSLIN(1),100) KODE
  100   format('KODE =',I12,', which is neither 1 nor 2.')
        call HALT ('MASSAGE', 1)
      end if
C     !END
      call BYE ('MASSAGE')
C
      return
      end
