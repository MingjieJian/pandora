      subroutine ANKOLE
     $(LU,IU,IL,METSE,METOLD,METNAM,KNT)
C
C     Rudolf Loeser, 1984 Oct 26
C---- Prints, for SWALLOW.
C     !DASH
      save
C     !DASH
      integer IL, IU, KNT, LU, METOLD, METSE
      character METNAM*15, STAR*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(45),STAR  )
C     !DASH
      external LINER, RULER, HI, BYE
C
C               METNAM(KNT)
      dimension METNAM(*)
C
      call HI ('ANKOLE')
C     !BEG
      if(LU.gt.0) then
        call LINER (2, LU)
        call RULER (LU, STAR, 127)
C
        write (LU,100) IU,IL,METNAM(METOLD+1),METNAM(METSE+1)
  100   format(' ','Automatic selection of STATISTICAL EQUILIBRIUM ',
     $             'METHODS was activated.'/
     $         ' ','Starting method (',I2,'/',I2,'): ',A/
     $         ' ','   Final method        : ',A)
C
        call RULER (LU, STAR, 127)
        call LINER (2, LU)
      end if
C     !END
      call BYE ('ANKOLE')
C
      return
      end
