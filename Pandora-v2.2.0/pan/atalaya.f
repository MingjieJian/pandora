      subroutine ATALAYA
     $(NO,MARK,CTAU,NPNT)
C
C     Rudolf Loeser, 1982 May 11
C---- Prints scales, for PICTURE.
C     !DASH
      save
C     !DASH
      real*8 CTAU
      integer I, IT, KAR, MARK, NO, NPNT
      character BLANK*1, LINE*120
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C
C---- ALVIN       as of 1995 Aug 08
      integer     LIMSCL
      parameter   (LIMSCL=11)
C     The number of TAU-scales to be printed fancily.
C     (Used in PICTURE, etc.)
C     .
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, HI, BYE
C
C               MARK(NPNT), CTAU(NPNT)
      dimension MARK(*),    CTAU(*)
C     !EJECT
C
      call HI ('ATALAYA')
C     !BEG
      do 103 I = 1,NPNT
        IT = MARK(I)
        if((IT.lt.0).or.(IT.gt.LIMSCL)) then
          write (MSSLIN(1),100) IT,LIMSCL
  100     format('IT =',I12,' is inappropriate (LIMSCL =',I12,').')
          call HALT ('ATALAYA',1)
        end if
C
        KAR  = 10*IT
        LINE = BLANK
        write (LINE(KAR+1:KAR+10),101) CTAU(I)
  101   format(1PE10.2)
C
        write (NO,102) LINE
  102   format(' ',A120)
  103 continue
C     !END
      call BYE ('ATALAYA')
C
      return
      end
