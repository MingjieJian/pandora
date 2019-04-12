      subroutine NIT
     $(KILROY,KODE,LINE)
C
C     Rudolf Loeser, 1978 May 28
C---- Sets an error diagnostic flag.
C     !DASH
      save
C     !DASH
      integer KODE
      logical KILROY
      character BLANK*1, KOKE*1, LINE*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C
C---- NICNAC      as of 1988 Feb 10
      integer     KFS,KFN
      dimension   KFN(3)
      common      /NICNAC/ KFS,KFN
C     Error counts, for Continuous Emergent Intensity printouts.
C     .
C     !DASH
      external ZEROI, HI, BYE
C
      dimension KOKE(3)
C
      data KOKE /'$', '*', '%'/
C
      call HI ('NIT')
C     !BEG
      if(KILROY) then
        KILROY = .false.
        call ZEROI (KFN,1,3)
        KFS = 0
      end if
C
      if((KODE.ge.1).and.(KODE.le.3)) then
        KFN(KODE) = KFN(KODE)+1
        KFS  = KFS+1
        LINE = KOKE(KODE)
      else
        LINE = BLANK
      end if
C     !END
      call BYE ('NIT')
C
      return
      end
