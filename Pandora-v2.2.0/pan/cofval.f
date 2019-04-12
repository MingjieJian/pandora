      subroutine COFVAL
     $(J,JP,K,KP,ISO,WN,RC1213,CF,CA,PRNT,METH,FVAL)
C
C     Rudolf Loeser, 1993 Mar 18
C---- Computes the f-value for the CO line (J -> JP, K -> KP),
C     for Carbon isotope C(12) or C(13) as specified by ISO,
C     by one of two alternative methods.
C     !DASH
      save
C     !DASH
      real*8 CA, CF, FVAL, RC1213, WN
      integer ISO, J, JD, JP, K, KD, KMX, KP, METH
      logical PRNT
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external  HALT, FOLD, FRESH, HI, BYE
      intrinsic abs
C
      data KMX /20/
C
      call HI ('COFVAL')
C     !BEG
      KD = KP-K
      JD = JP-J
C
      if((KD.lt.0).or.(KD.gt.2)) then
        write (MSSLIN(1),100) K,KP
  100   format('K =',I12,', KP =',I12,'; KP-K does not = 1 or 2.')
        call HALT ('COFVAL',1)
      end if
      if(abs(JD).ne.1) then
        write (MSSLIN(1),101) J,JP
  101   format('J =',I12,', JP =',I12,'; |JP-J| does not = 1.')
        call HALT ('COFVAL',1)
      end if
      if((ISO.lt.12).or.(ISO.gt.13)) then
        write (MSSLIN(1),102) ISO
  102   format('ISO =',I12,' (C isotope #) does not = 12 or 13.')
        call HALT ('COFVAL',1)
      end if
C     !EJECT
      if(METH.eq.1) then
        if(KP.gt.KMX) then
          write (MSSLIN(1),103) KP,(KMX+1)
  103     format('KP =',I12,', which is not less than',I3,'.')
          call HALT ('COFVAL',1)
        end if
C
        call FOLD   (J,JP,K,KP,ISO,WN,RC1213,CF,CA,PRNT,FVAL)
C
      else if(METH.eq.2) then
        if((K.lt.0).or.(K.gt.KMX)) then
          write (MSSLIN(1),104) K,(KMX+1)
  104     format('K =',I12,', which is not greater than -1 and ',
     $           'less than',I3,'.')
          call HALT ('COFVAL',1)
        end if
C
        call FRESH  (J,JP,K,KP,ISO,WN,RC1213,CF,CA,PRNT,FVAL)
      end if
C     !END
      call BYE ('COFVAL')
C
      return
      end
