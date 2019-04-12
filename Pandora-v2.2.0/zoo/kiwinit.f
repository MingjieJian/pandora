      subroutine KIWINIT
     $(NI,FILE)
C     Rudolf Loeser, 1985 May 17
C---- Initialization routine for KIWI in an interactive environment.
C     !DASH
      save
C     !DASH
      integer IPR, IRD, KARD, KART, KIWILFN, KIWIOUT, LAST, LUINP,
     $        LUOUT, NI
      character BLANK*1, BUFFER*80, FILE*(*), NAME*24
C     !COM
      common /DATAFIL/ KIWILFN
      common /CARDFIL/ KIWIOUT
      common /MAORI/   BUFFER
      common /KAURI/   LAST
      common /NUDEEL/  LUINP,IRD,IPR,LUOUT,KARD,KART
C     !DASH
      data BLANK /' '/
C
C     !BEG
      KIWILFN = NI
      KIWIOUT = 0
      LUINP = 0
      IRD   = 0
      IPR   = 0
      LUOUT = 0
      KARD  = 0
      KART  = 0
      NAME  = BLANK
C
      write (*,100) FILE
  100 format('$','Type file spec of "',A,'" input file: ')
      read  (*,101) NAME
  101 format(A24)
      write (*,102) NAME
  102 format(' ',A)
C
      open(unit=KIWILFN, file=NAME, status='OLD')
C     !END
C
      return
      end
