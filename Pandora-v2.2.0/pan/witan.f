      subroutine WITAN
     $(DL,K,DLSMIN,DLSMAX,IMIN,IMAX)
C
C     Rudolf Loeser, 1983 Sep 01
C---- Finds indices of minimum bracketing DL-subset, for the purpose
C     of parabolic interpolation.
C     !DASH
      save
C     !DASH
      real*8 DL, DLSMAX, DLSMIN
      integer I, IMAX, IMIN, IN, IPEX, IX, K, LUEO
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 18),IPEX )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external  NOTMORE, NOTLESS, MESHED, MASHED, HI, BYE
      intrinsic min, max
C
C               DL(K)
      dimension DL(*)
C
      call HI ('WITAN')
C     !BEG
      call NOTMORE  (DL, K, DLSMIN, IN)
      if(IN.gt.0) then
        IMIN = max(1,(IN-1))
      else
        IMIN = 1
      end if
C
      call NOTLESS  (DL, K, DLSMAX, IX)
      if(IX.gt.0) then
        IMAX = min(K,(IX+1))
      else
        IMAX = K
      end if
C
      if((IPEX.lt.0).or.(IPEX.eq.5)) then
        call MESHED ('WITAN', 2)
        write (LUEO,100) DLSMIN,IN,IMIN, DLSMAX,IX,IMAX, K
  100   format(' ','DLSMIN =',1PE12.4,2I10,5X,'DLSMAX =',E12.4,2I10,I15/
     $         ' ',5X,'DL')
        write (LUEO,101) (DL(I),I=1,K)
  101   format(' ',1P12E10.2)
        call MASHED ('WITAN')
      end if
C     !END
      call BYE ('WITAN')
C
      return
      end
