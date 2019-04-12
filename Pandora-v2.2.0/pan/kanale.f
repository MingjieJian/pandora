      subroutine KANALE
     $(LU,KODE,LAB,N,NL,MN1,Z,JQ,JX,RGVL)
C
C     Rudolf Loeser, 1998 Jun 22
C---- Plots diffusion term ratios.
C     !DASH
      save
C     !DASH
      real*8 GMAX, GMIN, RGVL, Z
      integer J, JQ, JX, KODE, LU, MN1, N, NL
      logical OK
      character LAB*(*)
C     !COM
C---- IMAGE       as of 1997 Aug 21
      integer     IMALEN
      parameter   (IMALEN=65535)
      character   IMAGE*(IMALEN)
      common      /IMAGE/ IMAGE
C     Character string to hold plot images constructed by the
C     K-type line printer plotting routines;
C     but used also as a general scratch character array.
C     .
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
C     !DASH
      external SAPA, BORLEY, NORI, LAPHE, HI, BYE
C
C               Z(N), RGVL(N,NL)
      dimension Z(*), RGVL(N,*)
C
      call HI ('KANALE')
C     !BEG
C---- Get function limits
      call SAPA     (RGVL,N,NL,JQ,JX,GMIN,GMAX)
C---- Initialize plot image
      call BORLEY   (IMAGE,GMIN,GMAX,Z,JQ,JX,KODE,OK)
      if(OK) then
C----   Enter data
        do 100 J = NL,1,-1
          call NORI (IMAGE,JQ,JX,Z,RGVL(1,J),ALPHS(J),KODE)
  100   continue
C----   Print plot
        call LAPHE  (LU,IMAGE,NL,LAB)
      end if
C     !END
      call BYE ('KANALE')
C
      return
      end
