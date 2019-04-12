      subroutine TWEET
     $(N,NOPAC,HEAD,BHS,BHSNUM,BHSDEN,B,C,KONFORM,NO,LUG,PLTID,S1,SR,
     $ LYM,TPOP,KISLV,ISW)
C
C     Rudolf Loeser, 1973 Feb 06
C---- Prints a set of Emitters.
C     (This is version 3 of TWEET.)
C     !DASH
      save
C     !DASH
      real*8 B, BHS, BHSDEN, BHSNUM, C, S1, SR
      integer I, IE, IS, ISW, KISLV, KN, KONFORM, LUG, N, NO, NOPAC
      logical LYM
      character HEAD*12, LABA*27, LABD*27, LABN*27, LABP*27, PLTID*1,
     $          SEP*10, TPOP*3
C     !DASH
      external  LINER, FULK, PING, TWANG, ROOSTER, NESTLE, HI, BYE
      intrinsic min
C
C               BHS(N), BHSNUM(N), BHSDEN(N), S1(N), SR(N), ISW(Nopac),
      dimension BHS(*), BHSNUM(*), BHSDEN(*), S1(*), SR(*), ISW(*),
C
C               B(N), C(Nopac,N)
     $          B(*), C(*)
C
      dimension PLTID(4)
C
      data LABP /'          Planck Function  '/
      data LABA /'Absrption Source Function  '/
      data LABN /'                Numerator  '/
      data LABD /'              Denominator  '/
      data SEP  /' ---------'/
C
      call HI ('TWEET')
C     !BEG
      if(NO.gt.0) then
        call LINER  (4, NO)
C----   Header
        write (NO,100) HEAD
  100   format(' ','Absorption Source Function at wavelength ',A12)
C----   H Ly lines normalization statement
        call NESTLE (NO)
C----   Explanation
        call FULK   (NO, LUG, 'emitter')
C     !EJECT
C
        IE = 0
  101   continue
          IS = IE+1
          IE = min((IE+10),N)
          KN = (IE-IS)+1
C
          call LINER   (2, NO)
          write (NO,102) (I,I=IS,IE)
  102     format(' ',20X,'Depth',2X,10I10)
          call LINER   (1, NO)
C
C----     Planck function
          call PING    (NO, 1, LABP, B(IS)     , KN)
          call LINER   (1, NO)
C----     Absorption source function, and components
          call PING    (NO, 1, LABA, BHS(IS)   , KN)
          call PING    (NO, 1, LABN, BHSNUM(IS), KN)
          call PING    (NO, 1, LABD, BHSDEN(IS), KN)
C
          call LINER   (1,NO)
          write (NO,103) (SEP,I=IS,IE)
  103     format(' ','   Breakdown of Numerator',2X,10A10)
C
C----     Reserved contributions (? )
          call TWANG   (S1, SR, IS, IE, LYM, TPOP, KISLV, KONFORM, NO,
     $                  PLTID)
C----     Contributors to emission
          call ROOSTER (IS, IE, C, KONFORM, NO, ISW)
C
          write (NO,104) (SEP,I=IS,IE)
  104     format(' ',27X,10A10)
C
        if(IE.lt.N) goto 101
C
      end if
C     !END
      call BYE ('TWEET')
C
      return
      end
