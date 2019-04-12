      subroutine ETHEL
     $(KODE,LINE,I,K,Z,TE,Y,XLAM,BRIGHT,YNT,XLTIT,ISTAR,MODE,MYX)
C
C     Rudolf Loeser, 1980 Nov 14
C---- Encodes a print line, for LEDGER.
C     !DASH
      save
C     !DASH
      real*8 BRIGHT, TE, XLAM, XLTIT, Y, YNT, Z
      integer I, ISTAR, K, KODE, MODE, MYX
      character BLANK*1, LABEL*48, LCON*13, LINE*127
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external NABOB, TISRIT, HALT, HI, BYE
C
      call HI ('ETHEL')
C     !BEG
      if((KODE.lt.1).or.(KODE.gt.3)) then
        write (MSSLIN(1),100) KODE
  100   format('KODE =',I12,', which is not 1, 2, or 3.')
        call HALT   ('ETHEL', 1)
      end if
C
      LINE = BLANK
      if(KODE.eq.1) then
        write (LINE,101) Z,I,TE
  101   format(1PE13.5,I9,0PF31.0)
      else
        call NABOB  (XLTIT,ISTAR,MODE,LABEL)
        call TISRIT (I,Y,MYX,LCON)
        if(KODE.eq.2) then
          write (LINE,102)   LCON,XLAM,   SYMBS(K),BRIGHT,YNT,LABEL
  102     format(13X    ,' ',A13,1PE16.8,0P,10X ,A3,
     $           F9.0,1PE12.3,2X,A48)
        else if(KODE.eq.3) then
          write (LINE,103) Z,LCON,XLAM,TE,SYMBS(K),BRIGHT,YNT,LABEL
  103     format(1PE13.5,' ',A13,1PE16.8,0PF10.0,A3,
     $           F9.0,1PE12.3,2X,A48)
        end if
      end if
C     !END
      call BYE ('ETHEL')
C
      return
      end
