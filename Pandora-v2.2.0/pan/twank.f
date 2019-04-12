      subroutine TWANK
     $(NO,N,Z,SFLX,SDER,SDIF,ROSSK,ROSST,TEFF,LFB)
C     Rudolf Loeser, 1984 Jun 22
C---- Prints integrated quantities, for BASHKIR.
C     (This is version 2 of TWANK.)
C     !DASH
      save
C     !DASH
      real*8 ROSSK, ROSST, SDER, SDIF, SFLX, TEFF, Z
      integer I, LFB, N, NO
      character BLANK*1, FACELAB*10
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
C     !EJECT
      external ABJECT, LINER, DOUBLER, TUMBLE, HI, BYE
C
C               TEFF(N), SFLX(N), SDIF(N), SDER(N), ROSSK(N), ROSST(N),
      dimension TEFF(*), SFLX(*), SDIF(*), SDER(*), ROSSK(*), ROSST(*),
C
C               Z(N)
     $          Z(*)
C
      call HI ('TWANK')
C     !BEG
      if(NO.gt.0) then
        call ABJECT  (NO)
        call TUMBLE  (LFB, FACELAB)
        if(FACELAB(10:10).eq.BLANK) then
          write (NO,100)
  100     format(' ',51X,'Continuum flux integrals.')
        else
          write (NO,101) FACELAB
  101     format(' ',45X,'Continuum flux integrals.',2X,A10)
        end if
        call LINER   (1, NO)
        call DOUBLER (NO)
        call LINER   (3, NO)
C
        write (NO,102)
  102   format(' ',3X,2(9X,'Rosseland'),16X,3(8X,'Integrated'),
     $             9X,'Effective'/
     $         ' ',14X,'Opacity',15X,'Tau',15X,'Z',14X,'Flux',7X,
     $             'Differences',7X,'Derivatives',7X,'Temperature')
        call LINER   (1, NO)
        write (NO,103) (I,ROSSK(I),ROSST(I),Z(I),SFLX(I),SDIF(I),
     $                    SDER(I),TEFF(I),I=1,N)
  103   format(5(' ',I3,1P2E18.10,E16.8,4E18.10/))
      end if
C     !END
      call BYE ('TWANK')
C
      return
      end
