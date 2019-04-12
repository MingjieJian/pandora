      subroutine SIPRFX
     $(X,F,P,KODE,TIT)
C     Rudolf Loeser, 1986 Feb 27
C---- Determines the proper scaling prefix
C     for quantities measured in S.I. units.
C
C---- Computes F and P such that X=F*P,
C     where P is a power of 1000, and
C     where TIT is the name of the SI scaling prefix
C     corresponding to P.
C     X, F and P are R*8 variables, and TIT is Type Character,
C     at least 5 characters long.
C     The word in TIT will be right-justified.
C     (If the input parameter KODE .eq. 2, then the word in
C     TIT will consist of lower case characters; otherwise,
C     it will consist of upper case characters.)
C---- If  1.D-24 .le. abs(X) .lt. 1.D+27,  then
C     SIPRFX delivers  1 .le. F .lt. 1000.
C     If, however, the magnitude of X falls outside this range,
C     then SIPRFX delivers F=X, P=1 and TIT=blanks.
C
C     (This is version 2 of SIPRFX.)
C     !DASH
      save
C     !DASH
      real*8 F, ONE, P, PT, THSND, X, ZERO
      integer I, K, KODE
      character QL*5, QU*5, TIT*(*)
C     !DASH
      external    COMPD
      intrinsic   abs, sign
C
      dimension   QU(17), QL(17)
      dimension   PT(17)
      equivalence (PT(9),ONE), (PT(10),THSND)
C
      data ZERO /0.D0/
C
      data QU /
     $ 'YOCTO', 'ZEPTO', 'ATTO ', 'FEMTO', 'PICO ', 'NANO ',
     $ 'MICRO', 'MILLI', '     ', 'KILO ', 'MEGA ',
     $ 'GIGA ', 'TERA ', 'PETA ', 'EXA  ', 'ZETTA', 'YOTTA'/
      data QL /
     $ 'yocto', 'zepto', 'atto ', 'femto', 'pico ', 'nano ',
     $ 'micro', 'milli', '     ', 'kilo ', 'mega ',
     $ 'giga ', 'tera ', 'peta ', 'exa  ', 'zetta', 'yotta'/
      data PT /
     $ 1.D-24,  1.D-21,  1.D-18,  1.D-15,  1.D-12,  1.D-09,
     $ 1.D-06,  1.D-03,  1.D+00,  1.D+03,  1.D+06,
     $ 1.D+09,  1.D+12,  1.D+15,  1.D+18,  1.D+21,  1.D+24/
C     !EJECT
C
C     !BEG
C---- Initialize
      F = abs(X)
      K = 9
  100 continue
        call COMPD (F,THSND,ZERO,I)
        if((I.ge.0).and.(K.lt.18)) then
C----     Try next larger prefix
          F = F/THSND
          K = K+1
          goto 100
        end if
  101 continue
        call COMPD (F,ONE,ZERO,I)
        if((I.lt.0).and.(K.ge.1)) then
C----     Try next smaller prefix
          F = F*THSND
          K = K-1
          goto 101
        end if
C---- Adjust for prefix out-of-range, if necessary
      if((K.lt.1).or.(K.gt.17)) then
        K = 9
        F = X
      end if
C
C---- Adjust sign of F
      F = sign(F,X)
C---- Set up proper prefix value
      P = PT(K)
C---- Set up proper prefix name
      if(KODE.eq.2) then
        TIT = QL(K)
      else
        TIT = QU(K)
      end if
C     !END
C
      return
      end
