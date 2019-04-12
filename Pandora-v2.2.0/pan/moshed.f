      subroutine MOSHED
     $(CALLER,KODE)
C
C     Rudolf Loeser, 2003 Dec 11
C---- Administers the MESHED/MASHED caller stack.
C     KODE = 1 means: pop;   KODE = 2 means: put.
C     !DASH
      save
C     !DASH
      integer KODE
      character BLANK*1, CALLER*(*)
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C
C---- MISHED      as of 2003 Dec 11
      parameter   (MSHLNG=50)
      integer     MSHCNO,MSHLNG
      character   MSHCLR*40
      dimension   MSHCLR(MSHLNG)
      common      /MISHED1/ MSHCNO
      common      /MISHED2/ MSHCLR
C     Caller stack for MESHED/MASHED messaging system.
C     .
C     !DASH
      external HI, BYE
C
      call HI ('MOSHED')
C     !BEG
      if(KODE.eq.2) then
C
        MSHCNO = MSHCNO+1
        if(MSHCNO.le.MSHLNG) then
          MSHCLR(MSHCNO) = CALLER
        end if
C
      else if(KODE.eq.1) then
C
        CALLER = BLANK
        if(MSHCNO.gt.0) then
          if(MSHCNO.le.MSHLNG) then
            CALLER = MSHCLR(MSHCNO)
          end if
          MSHCNO = MSHCNO-1
        end if
C
      end if
C     !END
      call BYE ('MOSHED')
C
      return
      end
