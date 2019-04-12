      subroutine POOBLE
     $(N,NOPAC,HEAD,OPAC,FMULT,BMULT,CAPPA,SIGSTR,ALBD,ZALBD,C,KONFORM,
     $ NO,LUG,ISW,PLTID,T1,TR,LYM,TPOP,KISLV,LINE)
C
C     Rudolf Loeser, 1969 Jul 01
C---- Prints a set of Opacities.
C     !DASH
      save
C     !DASH
      real*8 ALBD, BMULT, C, CAPPA, FMULT, OPAC, SIGSTR, T1, TR
      integer I, IE, IS, ISW, KISLV, KN, KONFORM, LUG, N, NO, NOPAC
      logical LINE, LYM, ZALBD
      character HEAD*12, LABA*27, LABK*27, LABO*27, LABS*27, PLTID*1,
     $          SEP*10, TPOP*3
C     !DASH
      external  LINER, FULK, PING, TWANG, ROOSTER, HI, BYE
      intrinsic min
C
C               OPAC(N), C(Nopac,N), ISW(Nopac), T1(N), TR(N), ALBD(N),
      dimension OPAC(*), C(*),       ISW(*),     T1(*), TR(*), ALBD(*),
C
C               CAPPA(N), SIGSTR(N)
     $          CAPPA(*), SIGSTR(*)
C
      dimension PLTID(4)
C
      data LABO /'      Total Opacity (/cm)  '/
      data LABK /'         Absorption (/cm)  '/
      data LABS /'         Scattering (/cm)  '/
      data LABA /'  Albedo, Composite Lines  '/
      data SEP  /' ---------'/
C
      call HI ('POOBLE')
C     !BEG
      if(NO.gt.0) then
        call LINER (4, NO)
C----   Header
        if(LINE) then
          write (NO,100) HEAD,FMULT,BMULT
        else
          write (NO,100) HEAD,FMULT
        end if
  100   format(' ','Continuous Opacity at wavelength ',A12//
     $         ' ','Overall multiplier [ MLC(wavelength) ] =',F10.3,:,
     $             ', Background Line Opacity multiplier ',
     $             '[ OML(u/l) ] =',F10.3)
C----   Explanations
        call FULK  (NO, LUG, 'absorber')
C     !EJECT
C
        IE = 0
  101   continue
          IS = IE+1
          IE = min(IE+10,N)
          KN = (IE-IS)+1
C
          call LINER   (2, NO)
          write (NO,102) (I,I=IS,IE)
  102     format(' ',20X,'Depth',2X,10I10)
          call LINER   (1, NO)
C
C----     Opacity
          call PING    (NO, 1, LABO, OPAC(IS),   KN)
          call LINER   (1, NO)
C----     Components: absorption and scattering
          call PING    (NO, 1, LABK, CAPPA(IS),  KN)
          call PING    (NO, 1, LABS, SIGSTR(IS), KN)
C
          call LINER   (1, NO)
          write (NO,103) (SEP,I=IS,IE)
  103     format(' ','     Breakdown of opacity',2X,10A10)
C
C----     Reserved contributions (? )
          call TWANG   (T1, TR, IS, IE, LYM, TPOP, KISLV, KONFORM, NO,
     $                  PLTID)
C----     Contributors to opacity
          call ROOSTER (IS, IE, C, KONFORM, NO, ISW)
C
          write (NO,104) (SEP,I=IS,IE)
  104     format(' ',27X,10A10)
C
          if(.not.ZALBD) then
C----       "Composite Lines" albedo (? )
            call LINER (1, NO)
            call PING  (NO, 1, LABA, ALBD(IS), KN)
          end if
C
        if(IE.lt.N) goto 101
C
      end if
C     !END
      call BYE ('POOBLE')
C
      return
      end
